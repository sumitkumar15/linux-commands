(ns linux-commands.core
  (:require [clojure.java.io :as io]
            [clojure.tools.cli :refer [parse-opts]]
            [ping.ping :as ping]
            [linux-commands.ls :refer [ls]])
  (:gen-class))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (let [command (first args)]
    (case command
      "ping" (ping/ping (second args))
      "ls" (ls (rest args)))))
