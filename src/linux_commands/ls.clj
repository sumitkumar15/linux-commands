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
        final (clojure.string/join "  " (map (fn [x] (str (second x))) info))
        ]
    final))

(defn print-out
  [x]
  (println x))

(def cli-opts
  [["-a" "--all"]
   ["-r" "--reverse"]
   [nil "-m"
    :id :fill]
   ["-Q" "--quote-name"]
   ["-s" "--size"]
   ["-l"
    :id :long-list]
   ["-R" "--recursive"]])

(defn ls-base
  [dir]
  (let [files (seq (.list (io/file dir)))]
    files))

(defn process-data
  [data opt]
  (cond
    (= "-r" opt) (reverse data)

    (= "-s" opt) (map (fn [x] [(.length (io/file x)) x]) data)

    (or (= opt "-p") (= "-F" opt)) (map (fn [x]
                                          (if (.isDirectory (io/file x))
                                            (str x "/") x)))

    (= "-m" opt) (clojure.string/join ", " data)

    (= "-Q" opt) (map (fn [x] (str "\"" x "\"")) data)

    ;(= "-S" opt) (recur remm (-> (map (fn [x] [(.length (io/file x)) x]) data)
    ;                             #(sort-by first %)
    ;                             reverse
    ;                             #(map second %)))

    (= :long-list opt) (-> (map (fn [x]
                            (str (file-info x "posix:owner,size,lastModifiedTime") x))
                          (filter #(not (clojure.string/starts-with? % ".")) data)))

    ;(= "-R" opt)                                        ;todo implement recursive
    ))

;(defn process
;  [^:list data ^:list opts]
;  (loop [[opt & remm] opts out data]
;    (if (nil? opt)
;      out
;      (cond
;        (= "-r" opt) (recur remm (reverse out))
;
;        (= "-s" opt) (recur remm (map (fn [x] [(.length (io/file x)) x]) out))
;
;        (or (= opt "-p") (= "-F" opt)) (recur remm (map (fn [x]
;                                                          (if (.isDirectory (io/file x))
;                                                            (str x "/") x))))
;
;        (= "-m" opt) (recur remm (clojure.string/join ", " out))
;
;        (= "-Q" opt) (recur remm (map (fn [x] (str "\"" x "\"")) out))
;
;        ;(= "-S" opt) (recur remm (-> (map (fn [x] [(.length (io/file x)) x]) out)
;        ;                             #(sort-by first %)
;        ;                             reverse
;        ;                             #(map second %)))
;
;        (= "-l" opt) (recur remm
;                            (-> (map (fn [x]
;                                       (str (clojure.string/join
;                                              "  "
;                                              (file-info x "posix:owner,size,lastModifiedTime")) x))
;                                     (filter #(not (clojure.string/starts-with? % ".")) out))))
;
;        ;(= "-R" opt)                                        ;todo implement recursive
;        ))))

(defn error-msg [errors]
  (str "The following errors occurred while parsing your command:\n\n"
       (clojure.string/join \newline errors)))

(defn process-opts
  [path options]
  (if (:long-list options)
    (println (process-data (ls-base path) :long-list))))

(defn ls
  [args]
  (println (clojure.tools.cli/parse-opts args cli-opts))
  (let [{:keys [options arguments errors summary]}
        (clojure.tools.cli/parse-opts args cli-opts)]
    (cond
      (some? errors) (do (println (error-msg errors))
                         (println "Available options are:\n" summary))
      (empty? arguments) (let [path (System/getProperty "user.dir")]
                         (process-opts path options))
      (some? arguments) (let [path (first arguments)]
                          (process-opts path options))
      )))