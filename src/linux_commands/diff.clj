(ns linux-commands.diff
  ;(:import [clojure.tools.cli :refer [parse-opts]])
  )

;(def cli-opts
;  [["-a" "--all"]
;   ["-r" "--reverse"]
;   [nil "-m"
;    :id :fill]
;   ["-Q" "--quote-name"]
;   ["-s" "--size"]
;   ["-l"
;    :id :long-list]
;   ["-R" "--recursive"]
;   ["-p" "--indicator-style"]])

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

;(println (lcs
;           (clojure.string/split "a b c d f g h j q z" #"\s+")
;           (clojure.string/split "a b c d e f g i j k r x y z" #"\s+")))