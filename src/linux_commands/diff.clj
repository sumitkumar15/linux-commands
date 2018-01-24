(ns linux-commands.diff
  (:require [clojure.string :as cstr]
            [clojure.term.colors :as color]
            [clojure.tools.cli :refer [parse-opts]]))

(defn lcs
  ([x y]
   (reverse (lcs x y '())))
  ([[x & xs] [y & ys] l]
   (if (not (and x y))
     l
     (if (= x y)
       (recur xs ys (cons x l))
       (let [a (lcs (cons x xs) ys l)
             b (lcs xs (cons y ys) l)]
         (if (> (count a) (count b)) a b))))))

;;From a longest common subsequence it is only a small step to get diff-like output:
;;
;; if an item is absent in the subsequence but present in the first original sequence,
;; it must have been deleted (as indicated by the '-' marks, below).
;;
;; If it is absent in the subsequence but present in the second original sequence,
;; it must have been inserted (as indicated by the '+' marks).

(defn generate-diff
  ([l1 l2]
   (let [ls (lcs l1 l2)]
     (generate-diff ls l1 l2 '())))
  ([[l & ls] [x & l1] [y & l2] final]
   (if (nil? l)
     (let [rlines (map (fn [x] [\- x]) (filter some? (cons x l1)))
           alines (map (fn [y] [\+ y]) (filter some? (cons y l2)))]
       (-> final
           (into rlines)
           (into alines)
           reverse))
     (cond
       (and (= l x) (= l y)) (recur ls l1 l2 (cons [nil x] final))
       (and (some? x) (not= l x)) (recur (cons l ls) l1 (cons y l2) (cons [\- x] final))
       (and (some? y) (not= l y)) (recur (cons l ls) (cons x l1) l2 (cons [\+ y] final))))))

(defn print-diff
  [inp]
  (doseq [[x strr] inp]
    (if (nil? x)
      (println (color/yellow (str "     " strr)))
      (println (if (= x \-)
                 (color/red x "   " strr)
                 (color/green x "   " strr))))))

(defn diff-files
  [file1 file2]
  (let [t1 (-> file1 slurp (cstr/split-lines))
        t2 (-> file2 slurp (cstr/split-lines))]
    (generate-diff t1 t2)))

(def curr-dir (str (System/getProperty "user.dir") "/"))
(def cli-opts
  [nil "--color"])

(defn error-msg
  [msg]
  (str "The following errors occurred while parsing your command:\n\n"
       (clojure.string/join \newline msg)))

(defn diff
  [args]
  (let [{:keys [options arguments errors summary]}
        (clojure.tools.cli/parse-opts args cli-opts)]
    (cond
      (some? errors) (do (println (error-msg errors) "\n")
                         (println "Available options are:\n" summary))
      (empty? arguments) (println "missing arguments: enter file names to compare")
      (= 1 (count arguments)) (println "missing argument after " (first arguments))
      (> (count arguments) 2) (println "extra operand " (nth arguments 2))
      :default (let [path1 (str curr-dir (first arguments))
                     path2 (str curr-dir (second arguments))]
                 (print-diff (diff-files path1 path2))))))
