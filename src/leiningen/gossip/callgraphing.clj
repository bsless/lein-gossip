(ns leiningen.gossip.callgraphing
  (:require [clojure.set :refer [map-invert]]
            [clojure.string :refer [join]]
            [clojure.java.io :as io])
  (:import java.io.PushbackReader))

(defn flatten-maps [x]
  (if (coll? x)
    (mapcat flatten-maps x)
    [x]))

(defn read-all-forgivingly
  "read clojure code, tolerating invalid tokens"
  [pbr]
  (let [form (try (read pbr)
                  (catch Exception e
                    (let [message (.getMessage e)]
                      (if (= message "EOF while reading")
                        ::eof
                        (do (println (str "[reader error] " message))
                            ::unreadable-form)))))]
    (if (= form ::eof) nil
        (lazy-seq (cons form (read-all-forgivingly pbr))))))

(defn clj-to-data
  "Specify the full path of the .clj file and it will slurp it up, wrap it
   in parentheses and apply read-string to it."
  [file]
  (binding [*data-readers* (conj *data-readers* {'js identity})]
    (with-open [rdr (java.io.PushbackReader. (io/reader file))]
      (vec (read-all-forgivingly rdr)))))

(defn select-ns
  "code is the .clj file that has been passed through the reader. This
   function returns the (ns ...) list."
  [code]
  (first (drop-while #(not= 'ns (first %)) code))
  #_(first (filter #(and (coll? %) (= 'ns (first %))) code))
  )

(defn requires->map
  [code]
  (map (fn [[lib & {:keys [refer as]}]] {:lib lib :refer refer :as as}) code))

(defn uses->map
  [code]
  (let [collected (map (fn [[lib & {:keys [only] :as args}]]
                         {:lib lib :only only}) code)]
    (into {} (mapcat (fn [{lib :lib only :only}] (map (fn [f] {f lib}) only)) collected))))

(defn ns->map
  "Parse namespace code into map of format:
  {:ns name
   :require [requires]
   :use [uses]}"
  [code]
  (let [name (second code)
        body (drop 2 code)
        ns-map (reduce (fn [accum [k & vs]] (assoc accum k vs))
                       {}
                       body)
        ns-map (assoc ns-map :ns name)
        ns-map (update ns-map :require requires->map)
        ns-map (update ns-map :use uses->map)]
    ns-map))

(defn select-defs
  "code is a .clj file that has been passed through the reader. This
   function returns all of the (defn ...) lists. Right now it only finds
   defns."
  [code]
  (filter #(and (coll? %)
                (contains? #{'defn 'def 'defn- 'defmulti 'defmethod} (first %)))
          code))

(defn extract-def-names
  "With a list of defn lists, it returns what the defs name."
  [defs]
  (map #(nth % 1) defs))

(defn ns-map->required-lookup
  "
  Given a ns map as returned by `ns-map`, return a mapping
  of a name space abbreviation or usage to the namespace, i.e.:
    {abbrev1 ns1, abbrev2 ns2, ns3 ns3, ... }
  "
  [ns-map]
  (reduce (fn [accum {lib :lib ref :refer as :as}]
            (assoc accum (or as lib) lib)) {}
          (get ns-map :require ())))

(defn ns-map->used-lookup
  "
  Given a ns map as returned by `ns-map`, return a map
  Containing the :use and :refer statements of the form:
    {f1 ns1, f2 ns1, f3 ns2, ... }

  This assumes that all used namespaces use the [namespace :only [f1 f2]] form,
  as the original author believes they should be.
  This author tends to agree.
  "
  [{reqs :require used :use
    :or {reqs ()
         used {}}}]
  (reduce (fn [accum {lib :lib referred :refer}]
            (if referred
              (reduce (fn [accum r] (assoc accum r lib))
                      accum
                      referred)
              accum)) used reqs))

(defn parse-namespace-qualified-function [required-ns-lookup symbol-name]
  (let [string-name (str symbol-name)
        slash-idx (.indexOf string-name "/")]
    [(str (required-ns-lookup (symbol (.substring string-name 0 slash-idx)))) (.substring string-name (inc slash-idx))]))

(defn select-calls-in-def [def-names used-ns-lookup required-ns-lookup def-expression]
  (let [head (str (first (rest def-expression)))]
    (loop [body (flatten-maps (rest (rest def-expression)))
           so-far []]
      (if (empty? body)
        [{:type :defn :name head} (distinct (filter identity so-far))]
        (let [current (first body)
              result (cond
                       (some #{current} def-names) {:type :defn :name (str current)}
                       (some #{current} (keys used-ns-lookup)) {:type :use :calls current :name (str (used-ns-lookup current))}
                       (not (empty? (filter #(.startsWith (str current) (str % "/")) (keys required-ns-lookup))))
                       (let [[namespace func] (parse-namespace-qualified-function required-ns-lookup current)]
                         {:type :require :calls func :name namespace})
                       :else nil)]
          (recur (rest body) (conj so-far result)))))))

(defn select-calls-for-each-def [def-names used-ns-lookup required-ns-lookup defs]
  (into {} (map (partial select-calls-in-def def-names used-ns-lookup required-ns-lookup) defs)))

(defn select-defs-and-calls [code]
  (let [namespace (select-ns code)
        defs (select-defs code)
        def-names (into #{} (extract-def-names defs))
        ns-map (ns->map namespace)
        used-ns-lookup (ns-map->used-lookup ns-map)
        required-ns-lookup (ns-map->required-lookup ns-map)]
    (select-calls-for-each-def def-names used-ns-lookup required-ns-lookup defs)))

(def ^:dynamic *formatting*
  {:defn
   {:shape "ellipse", :style "bold"}
   :use
   {:shape "box"}
   :require
   {:shape "box"}
   [:defn :use]
   {:penwidth 3, :style "dashed"}
   [:defn :require]
   {:penwidth 3, :style "dotted"}
   [:defn :defn]
   {:penwidth 3}})

(defn style
  ([key] (style key {}))
  ([key base-styling]
   (let [styling (merge base-styling (*formatting* key))]
     (if (nil? styling)
       ""
       (str "[" (join "," (for [[k v] styling] (str (name k) "=" v))) "]")))))

(defn q [st]
  (str "\"" st "\""))

;; this is a hack which enables multimethods to be redefined in the REPL
(def node-to-string nil)
(defmulti node-to-string
  (fn [nodes>codes name referenced-names]
    (let [current (first referenced-names)]
      (current :type))))

(defmethod node-to-string :defn [nodes>codes name referenced-names]
  (str (nodes>codes name) " " (style :defn {:label (q name)}) ";\n"))
(defmethod node-to-string :use [nodes>codes name referenced-names]
  (let [label (q (str name "\\n\\n" (join "\\n" (map :calls referenced-names))))]
    (str (nodes>codes name) " " (style :use {:label label}) ";\n")))
(defmethod node-to-string :require [nodes>codes name referenced-names]
  (let [label (q (str name "\\n\\n" (join "\\n" (map :calls referenced-names))))]
    (str (nodes>codes name) " " (style :require {:label label}) ";\n")))

;; this is a hack which enables multimethods to be redefined in the REPL
(def edge-to-string nil)
(defmulti edge-to-string
  (fn [nodes>codes func call]
    [(func :type) (call :type)]))

(defmethod edge-to-string [:defn :use] [nodes>codes func call]
  (str (nodes>codes (func :name)) "->" (nodes>codes (call :name)) " " (style [:defn :use]) ";\n"))
(defmethod edge-to-string [:defn :require] [nodes>codes func call]
  (str (nodes>codes (func :name)) "->" (nodes>codes (call :name)) " " (style [:defn :require]) ";\n"))
(defmethod edge-to-string [:defn :defn] [nodes>codes func call]
  (str (nodes>codes (func :name)) "->" (nodes>codes (call :name)) " " (style [:defn :defn]) ";\n"))

(defn identify-distinct-names-called
  "A referenced name can be a value in the current namespace, a name used from another namespace or a name referenced
   by a required namespace. In the last case, because the edge is from the name to the namespace, we only want
   one edge even if there are multiple names called in the namespace. This function removes duplicate names
   since for edges, only the namespace matters.

   Essentially, we are reducing the metadata about calls from :name, :type, and :calls to just :name and :type
   distincting."
  [calls]
  (distinct (map #(select-keys % [:name :type]) calls)))

(defn clj-to-dot [filename]
  (let [code (clj-to-data filename)
        namespace (second (select-ns code))
        defs-and-calls (select-defs-and-calls code)
        nodes (group-by :name (distinct (concat (keys defs-and-calls) (flatten (vals defs-and-calls)))))
        nodes>codes (into {} (map (fn [name num] [name (str "G" num)]) (keys nodes) (range)))
        codes>nodes  (map-invert nodes>codes)
        dot (StringBuffer.)]
    (.append dot "digraph g {\n")
    (.append dot "subgraph cluster1 {\n")
    (.append dot (str "label=\"" namespace "\"\n"))
    (doseq [[name referenced-names] nodes]
      (.append dot (node-to-string nodes>codes name referenced-names)))
    (.append dot "}\n")
    (doseq [[adef calls] defs-and-calls]
      (doseq [call (identify-distinct-names-called calls)]
        (.append dot (edge-to-string nodes>codes adef call))))
    (.append dot "}\n")
    [(str namespace) (str dot)]))

;; http://rosettacode.org/wiki/Walk_a_directory/Recursively#Clojure
(defn walk-directory [dirpath pattern]
  (doall (filter #(re-matches pattern (.getName %))
                 (file-seq (io/file dirpath)))))

(defn generate-dot-files-from-clj-files [src-dir tar-dir]
  (doseq [file (walk-directory src-dir #".*\.cljs?")]
    (let [_ (println (.getPath file))
          [namespace dot] (clj-to-dot file)
          filename (str tar-dir "/" (.replace namespace "." "_") ".dot")]
      (spit filename dot))))

(comment
  (#'clojure.core/load-data-readers)
  (set! *data-readers* (.getRawRoot #'*data-readers*)))

