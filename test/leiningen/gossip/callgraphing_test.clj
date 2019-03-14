(ns leiningen.gossip.callgraphing-test
  (:require [leiningen.gossip.callgraphing :as sut]
            [clojure.test :refer :all]))

(deftest namespace-processing
  (testing "select ns"
    (let [code
          ['(ns foo.bar
              (:require
               [clojure.core.match :refer [match]]
               [cheshire.core :as json]
               [taoensso.timbre :as timbre :refer (trace debug info warn error fatal spy)]
               [clojure.java.io :refer (writer)]
               [com.stuartsierra.component :as component]
               [clj-time.format :as tf]
               [clj-time.core :as tc])
              (:use [clojure.core.async :only [chan >!! <!!]]))]
          namespace
          '(ns foo.bar
             (:require
              [clojure.core.match :refer [match]]
              [cheshire.core :as json]
              [taoensso.timbre :as timbre :refer (trace debug info warn error fatal spy)]
              [clojure.java.io :refer (writer)]
              [com.stuartsierra.component :as component]
              [clj-time.format :as tf]
              [clj-time.core :as tc])
             (:use [clojure.core.async :only [chan >!! <!!]]))
          ns-map
          '{:require
            ({:lib clojure.core.match, :refer [match], :as nil}
             {:lib cheshire.core, :refer nil, :as json}
             {:lib taoensso.timbre,
              :refer (trace debug info warn error fatal spy),
              :as timbre}
             {:lib clojure.java.io, :refer (writer), :as nil}
             {:lib com.stuartsierra.component, :refer nil, :as component}
             {:lib clj-time.format, :refer nil, :as tf}
             {:lib clj-time.core, :refer nil, :as tc}),
            :use
            {chan clojure.core.async, >!! clojure.core.async, <!! clojure.core.async},
            :ns foo.bar}
          used-lookup
          '{match clojure.core.match,
            chan clojure.core.async,
            >!! clojure.core.async,
            spy taoensso.timbre,
            warn taoensso.timbre,
            trace taoensso.timbre,
            debug taoensso.timbre,
            <!! clojure.core.async,
            fatal taoensso.timbre,
            writer clojure.java.io,
            info taoensso.timbre,
            error taoensso.timbre}
          required-lookup
          '{clojure.core.match clojure.core.match,
            json cheshire.core,
            timbre taoensso.timbre,
            clojure.java.io clojure.java.io,
            component com.stuartsierra.component,
            tf clj-time.format,
            tc clj-time.core}]
      (is (= (sut/select-ns code) namespace))
      (is (= '{chan clojure.core.async, >!! clojure.core.async, <!! clojure.core.async}
             (sut/uses->map '[[clojure.core.async :only [chan >!! <!!]]])))
      (is (= '({:lib clojure.core.match, :refer [match], :as nil}
               {:lib cheshire.core, :refer nil, :as json}
               {:lib taoensso.timbre,
                :refer (trace debug info warn error fatal spy),
                :as timbre}
               {:lib clojure.java.io, :refer (writer), :as nil}
               {:lib com.stuartsierra.component, :refer nil, :as component}
               {:lib clj-time.format, :refer nil, :as tf}
               {:lib clj-time.core, :refer nil, :as tc})
             (sut/requires->map
              '([clojure.core.match :refer [match]]
                [cheshire.core :as json]
                [taoensso.timbre :as timbre :refer (trace debug info warn error fatal spy)]
                [clojure.java.io :refer (writer)]
                [com.stuartsierra.component :as component]
                [clj-time.format :as tf]
                [clj-time.core :as tc]))))
      (is (= (sut/ns->map namespace) ns-map))
      (is (= (sut/ns-map->used-lookup ns-map) used-lookup))
      (is (= (sut/ns-map->required-lookup ns-map) required-lookup)))))
