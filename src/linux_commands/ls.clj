(ns linux-commands.ls
  (:require [clojure.java.io :as io]
            [clojure.tools.cli :refer [parse-opts]])
  (:import (java.nio.file Files LinkOption)
           (java.nio.file Paths Path)
           (java.net.URI)))

(defn file-info
  [fpath pattern]
  (let [uri (java.net.URI. (str "file://" (.getAbsolutePath (io/file fpath))))
        p (Paths/get uri)
        info (into {}
                   (Files/readAttributes
                     p
                     pattern
                     (into-array LinkOption [LinkOption/NOFOLLOW_LINKS])))
        final (reduce merge {} (map (fn [x] {(first x) (str (second x))}) info))
        ]
    final))

(defn print-out
  [x]
  (println x))

(def cli-opts
  [["-a" "--all" "All"]])

(defn ls-base
  [dir]
  (let [files (seq (.list (io/file dir)))]
    files))

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

        ;(= "-S" opt) (recur remm (-> (map (fn [x] [(.length (io/file x)) x]) out)
        ;                             #(sort-by first %)
        ;                             reverse
        ;                             #(map second %)))

        (= "-l" opt) (recur remm
                            (-> (map (fn [x]
                                       (str (clojure.string/join
                                              "  "
                                              (file-info x "posix:owner,size,lastModifiedTime")) x))
                                     (filter #(not (clojure.string/starts-with? % ".")) out))))

        ;(= "-R" opt)                                        ;todo implement recursive
        ))))

(defn ls
  [args]
  (let [path (System/getProperty "user.dir")]
    (if (empty? args)
      (println (sort (ls-base path)))
      (println (process (sort (ls-base path)) args))
      )))
