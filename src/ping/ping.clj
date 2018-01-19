(ns ping.ping
  (:require [clojure.core.async])
  (:import (java.net InetAddress)))

(defn send-icmp
  "Sends an icmp request to given host return true if packet received
  requires root access to make request to external ips"
  [host timeout]
  (.isReachable (aget (InetAddress/getAllByName host) 0) timeout))

(defn make-icmp-req
  [host timeout]
  (let [start (. System (nanoTime))
        result (send-icmp host timeout)
        total (/ (double (- (. System (nanoTime)) start)) 1000000.0)]
    [result total]))

(defn ping-loop
  ([host timeout n]
   (loop [k n xs (list)]
     (if (<= k 0)
       xs
       (do
         (Thread/sleep 1000)
         (let [r (make-icmp-req host timeout)]
           (if (true? (first r))
           (println (str "received from" host ":icmp time= " (second r) " ms"))
             (println "packet dropped"))
           (recur (dec k) (cons r xs))))))))

(defn ping
  "ping an ip address"
  ([host]
    (ping host 4000))
  ([host timeout]
    (let [output (ping-loop host timeout 5)
          tpack (filter #(true? (first %)) output)
          len (count output)
          received (count tpack)
          lost (- len received)
          avg (/ (reduce + (map second tpack)) len)]
      (do
        (println (str "-- " host " ping statistics --"))
        (println (str len " packets transmitted, "
                      received " packets received, "
                      (* 100 (/ (- len received) len)) "% packet loss"))))))
