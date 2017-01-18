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

(defn take-cycle [benchmark word]
  (take (count benchmark) (cycle word)))

(defn encode [keyword message]
  (string/join
    (map encode-single-char
      (vec (take (count message) (cycle keyword)))
      (vec message))))

(defn decode [keyword encrypted-message] (string/join
  (map decode-single-char
    (vec (take-cycle encrypted-message keyword))
    (vec encrypted-message))))

(defn decipher [cipher message]
  "decypherme")

