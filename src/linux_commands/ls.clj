(ns linux-commands.ls
  (:require [clojure.java.io :as io]
            [clojure.tools.cli :refer [parse-opts]])
  (:import (java.nio.file Files LinkOption)
           (java.nio.file Paths Path)
           (java.net.URI)
           (java.nio.file.attribute PosixFilePermissions)))

(def cli-opts
  [["-a" "--all"]
   ["-r" "--reverse"]
   [nil "-m"
    :id :fill]
   ["-Q" "--quote-name"]
   ["-s" "--size"]
   ["-l"
    :id :long-list]
   ["-R" "--recursive"]
   ["-p" "--indicator-style"]])

(defn file-info
  [fpath pattern]
  (let [uri (java.net.URI. (str "file://" (.getAbsolutePath (io/file fpath))))
        p (Paths/get uri)
        info (into {}
                   (Files/readAttributes
                     p
                     pattern
                     (into-array LinkOption [LinkOption/NOFOLLOW_LINKS])))
        pre (apply merge {:name fpath} (map (fn [x] {(first x) (str (second x))}) info))
        final (assoc pre :permission
                         (-> (Files/getPosixFilePermissions
                               p
                               (into-array LinkOption [LinkOption/NOFOLLOW_LINKS]))
                             (PosixFilePermissions/toString)))
        ]
    final))

(defn ls-base
  [dir]
  (let [files (seq (.list (io/file dir)))]
    files))

(defn ls-base-info
  [dir]
  (let [files (seq (.list (io/file dir)))]
    (map (fn [x] (file-info x "posix:owner,size,lastModifiedTime")) files)))

(defn process-data
  [data opt]
  (cond
    (nil? opt) (filter (fn [x] (not (clojure.string/starts-with? (:name x) "."))) data)

    (= :reverse opt) (reverse data)

    (= :size opt) (map (fn [x]
                         (assoc x :b-size (.length (io/file x))))
                       data)

    (or (= opt :indicator-style)) (map (fn [x]
                                          (if (.isDirectory (io/file x))
                                            (update x :name #(str % "/")))))

    (= :fill opt) data

    (= :quote-name opt) (map (fn [x] (assoc x :name (str "\"" x "\""))) data)

    ;(= "-S" opt) (recur remm (-> (map (fn [x] [(.length (io/file x)) x]) data)
    ;                             #(sort-by first %)
    ;                             reverse
    ;                             #(map second %)))

    (= :long-list opt) data
    ;(-> (map (fn [x]
    ;           (str (file-info x "posix:owner,size,lastModifiedTime") "  " x))
    ;         (filter #(not (clojure.string/starts-with? % ".")) data)))

    ;(= "-R" opt)                                        ;todo implement recursive
    ))

(defn error-msg [errors]
  (str "The following errors occurred while parsing your command:\n\n"
       (clojure.string/join \newline errors)))

;(defn process-opts
;  [path options]
;  (let [dirdata (ls-base-info path)]
;    (cond
;      (empty? options) (process-data dirdata nil)
;      :default ()
;      )))


(defn print-out
  [xs option-map]
  (doseq [x xs]
    (let [res (loop [mx (keys option-map) d x]
                (if (nil? mx)
                  d
                  (recur (rest mx) (process-data d (first mx)))))]
      (println (-> (list (:b-size res)
                         (:permission res)
                         (:owner res)
                         (:size res)
                         (:lastModifiedTime res)
                         (:name res))
                   (fn [x] (filter some? x))
                   #(clojure.string/join "  " %)))
      )))

(defn ls
  [args]
  (println (clojure.tools.cli/parse-opts args cli-opts))
  (println (file-info "LICENSE" "posix:owner,size,lastModifiedTime"))
  (let [{:keys [options arguments errors summary]}
        (clojure.tools.cli/parse-opts args cli-opts)]
    (cond
      (some? errors) (do (println (error-msg errors))
                         (println "Available options are:\n" summary))
      (empty? arguments) (let [path (System/getProperty "user.dir")]
                         (print-out (ls-base-info path) options))
      (some? arguments) (if (= (count arguments) 1)
                          (let [path (str (System/getProperty "user.dir")
                                          "/"
                                          (first arguments))]
                            (print-out (ls-base-info path) options))
                          (doseq [p arguments]
                            (do
                              (println p ":")
                              (print-out (ls-base-info p) options))))
      )))