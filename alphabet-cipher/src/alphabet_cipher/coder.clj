(ns alphabet-cipher.coder
  (:require [clojure.string :as string]))

(def alphabet "abcdefghijklmnopqrstuvwxyz")

(defn idx [character]
  (string/index-of alphabet character))

(defn rotate [char]
  (str (subs alphabet (idx char)) (subs alphabet 0 (idx char))))

(defn make-grid [memo char] (assoc memo char (rotate char)))

(def grid (reduce make-grid {} alphabet))

(defn encode-single-char [column-char row-char]
  (nth (get grid column-char) (idx row-char)))

(defn decode-single-char [column-char row-char]
  (nth alphabet
       (string/index-of
         (get grid column-char) row-char)))

(defn encode [keyword message]
  (apply str
    (map encode-single-char
         (cycle keyword)
         message)))

(defn decode [keyword encrypted-message]
  (apply str
    (map decode-single-char
         (cycle keyword)
         encrypted-message)))

(defn decipher-single-char [message-char encrypted-char]
  (first (filter
           (fn [key] (= (string/index-of (get grid key) encrypted-char) (idx message-char)))
           (keys grid))))

(defn cycled-keyword [message encrypted-message]
  (apply str (map decipher-single-char (vec message) (vec encrypted-message))))

(defn decipher [message encrypted-message]
  (let [cycled (cycled-keyword message encrypted-message)]
    (subs cycled 0 (nth
                     (filter
                       (fn [x] (= cycled (string/join (take (count cycled) (cycle (subs cycled 0 x))))))
                       (range (count cycled)))
                     0))))
