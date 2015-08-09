 (ns lleo.utils
  (:require [clojure.pprint :as p]) 
  (:import (java.util.regex Pattern Matcher)))


(defn substr [^String s ^long beg ^long end]
  (.substring s beg end))


(defn split-fixed [^String s ^String fe]
  (loop [beg 0 end (.indexOf s fe beg) result []]
    (if (< end 0)
      (conj result (substr s beg (count s)))
      (recur (+ end (count fe))
             (.indexOf s fe (+ end (count fe)))
             (conj result (substr s beg end))))))


(defn split-fixed-limit [^String s ^String fe ^Long lim]
  (if (< 0 lim)
    (split-fixed s fe)
    (loop [beg 0 end (.indexOf s fe beg) result []]
      (if (< (count result) lim)
        (if (< end 0)
          (conj result (substr s beg (count s)))
          (recur (+ end (count fe))
                 (.indexOf s fe (+ end (count fe)))
                 (conj result (substr s beg end))))
        result))))


(defmulti split (fn [s re & rest] (mapv class (into [s re] rest))))

(defmethod split [String String] [s fe]
  (let [^Matcher m (re-matcher (Pattern/compile (str "\\Q" fe "\\E")) s)]
    (loop [beg 0 result []]
      (if (.find m)
        (recur (.end m) (conj result (substr s beg (.start m))))
        (conj result (substr s beg (count s)))))))

(defmethod split [String String Long] [s fe lim]
  (if (< 0 lim)
    (split s fe)
    (let [^Matcher m (re-matcher (Pattern/compile (str "\\Q" fe "\\E")) s)]
      (loop [beg 0 result []]
        (if (< (count result) lim)
          (if (.find m)
            (recur (.end m) (conj result (substr s beg (.start m))))
            (conj result (substr s beg (count s))))
          result)))))

(defmethod split [String Pattern] [s rx]
  (let [^Matcher m (re-matcher rx s)]
    (loop [beg 0 result []]
      (if (.find m)
        (recur (.end m) (conj result (substr s beg (.start m))))
        (conj result (substr s beg (count s)))))))

(defmethod split [String Pattern Long] [s rx lim]
  (if (< 0 lim)
    (split s rx)
    (let [^Matcher m (re-matcher rx s)]
      (loop [beg 0 result []]
        (if (< (count result) lim)
          (if (.find m)
            (recur (.end m) (conj result (substr s beg (.start m))))
            (conj result (substr s beg (count s))))
          result)))))

(defn foo
  "I don't do a whole lot."
  [x]
  (println x "Hello, World!"))
