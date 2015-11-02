(ns fallout-hacking.core
  (:require [clojure.string :as s])
  (:gen-class))

(def rchrs (map char (concat (range 58 64) (range 123 125) (range 32 47))))
(def dict (s/split (slurp "enable1.txt") #"\s"))
(def max-cols 65)
(def max-rows 30)
(def gridn (* max-cols max-rows) )
(def RANDS (int (+ (rand 0xFFF) 0xFFF)))

(defn get-random-word [len]
  (let [word (s/upper-case (nth dict (rand (count dict))))]
    (if (= (count word) len)
      word
      (get-random-word len))))

(defn irand [m]
  (int (rand m)))

(defn hacker-noise []
  (str (nth rchrs (irand (count rchrs)))))

(defn cmp-str [f s1 s2]
  (if (f (count s1) (count s2))
    s1 
    s2))

(defn letters-correct [g answer]
  (let [bigger (cmp-str > g answer)
        smaller (cmp-str <= g answer)] 
  (count 
    (filter true? 
            (keep-indexed #(= %2 (nth bigger %1)) smaller)))))

(defn guess [gcount answer]
  (println gcount "ATTEMPTS LEFT")
  (print ">")
  (flush)
  (let [guess (s/upper-case (read-line))]
    (println (str (letters-correct guess answer) "/" (count answer)) "correct")
    (if (= guess answer) 
      true
      false)))

(defn next-term-line [pos] 
  (print (str " 0x" (Integer/toHexString (+ RANDS pos))) " "))

(defn make-board-list 
  ([spots words] (make-board-list spots words 0 '()))
  ([spots words p lst]
  (if (= p (inc gridn))
    lst
    (let [spot (first spots) 
          word (first words)]
      (if (= p spot)
        (make-board-list 
          (rest spots) 
          (rest words)
          (+ p (count word))
          (concat lst word))
      (make-board-list
        spots
        words
        (inc p)
        (concat lst (hacker-noise))))))))

(defn display-board [boardlist] 
  (loop [lst boardlist p 0]
    (when (not (empty? lst))
      (print (first lst))
      (when (and 
              (= (mod p max-cols) 0) 
              (not (empty? (rest lst))))
        (println)
        (next-term-line p))
      (recur (rest lst) (inc p))))
  (println)
  (println)
  (flush))

(defn overlap? [lst1 lst2]
  (not (every? nil? (map #(some #{%1} lst1) lst2))))

(defn get-random-spots 
  ([wordlen wordcount] (get-random-spots wordlen wordcount '() '()))
  ([wlen wc spots heads]
  (if (<= (count heads) wc)
    (let [spot (irand (- gridn wlen))
          maybe-spots (range spot (+ spot wlen))]
      (if (overlap? maybe-spots spots)
        (get-random-spots wlen wc spots heads)
        (get-random-spots wlen wc (concat spots maybe-spots) (conj heads spot))))
    heads)))

(defn -main [& args]
  (print "Difficulty: ")
  (flush)
  (let [difficulty-mod (try (Integer/parseInt (read-line)) (catch Exception e 1))
        wordlen (+ 4 difficulty-mod)
        wordcount (+ 6 difficulty-mod)
        words (repeatedly wordcount #(get-random-word wordlen))
        answer (nth words (rand (count words)))] 

    ;(println "words:" words)
    ;(println "answer:" answer)
    (let [spots (sort (get-random-spots wordlen wordcount))
          boardlist (make-board-list spots words)] 
      ;(println spots)
      (display-board boardlist))

    (if (or 
          (guess 4 answer)
          (guess 3 answer)
          (guess 2 answer)
          (guess 1 answer))
      (println "Access Granted")
      (println "Access Denied")))
  
  (print "Play again? ")
  (flush)
  (when (= (try 
           (s/lower-case (first (read-line)))
           (catch Exception e "no")) 
        "y")
    (-main args)))
