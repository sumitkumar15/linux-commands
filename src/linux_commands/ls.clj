(ns linux-commands.ls
  (:require [clojure.java.io :as io]
            [clojure.tools.cli :refer [parse-opts]]))

(defn print-out
  [x]
  (println x))

(def cli-opts
  [["-a" "--all" "All"]])

(defn ls-base
  [dir]
  (let [files (seq (.list (io/file dir)))]
    files))
(. ())
(defn process
  [^:list data ^:list opts]
  (loop [[opt & remm] opts out data]
    (if (nil? opt)
      out
      (cond
        (= "-r" opt) (recur remm (reverse out))
        (= "-s" opt) (recur remm (map (fn [x] [(.length (io/file x)) x]) out))
        (or (= opt "-p") (= "-F" opt)) (recur remm (map (fn [x]
                                        (if (.isDirectory (io/file x))
                                                (str x "/") x))))

        (= "-m" opt) (recur remm (clojure.string/join ", " out))
        (= "-Q" opt) (recur remm (map (fn [x] (str "\"" x "\"")) out))
        (= "-S" opt) (recur remm (-> (map (fn [x] [(.length (io/file x)) x]) out)
                                     #(sort-by first %)
                                     reverse
                                     #(map second %)))

        ;(= "-R" opt)                                        ;todo implement recursive

        ))))

(defn ls
  [args]
  (let [
        ;opts (clojure.tools.cli/parse-opts args cli-opts)
        path (System/getProperty "user.dir")]
    (println args)
    (if (empty? args)
      (println (sort (ls-base path)))
      (println (process (sort (ls-base path)) args))
      )))
