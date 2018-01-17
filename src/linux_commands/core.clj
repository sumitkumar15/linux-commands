(ns linux-commands.core
  (:require [clojure.java.io :as io])
  (:gen-class))


(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println (send-request "192.168.0.103"))
  (println "Hello, World!"))
