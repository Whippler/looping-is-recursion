(ns looping-is-recursion)

(defn power [base exp]
  (let [helper (fn [acc base exp]
                 (if (zero? exp)
                   acc
                   (recur (* acc base) base (dec exp))))]
    (helper 1 base exp)))

(defn last-element [a-seq]
  (let [helper (fn [s]
                 (if (empty? (rest s))
                   (first s)
                   (recur (rest s))))]
    (helper a-seq)))

(defn seq= [seq1 seq2]
    (loop [seq1 seq1
           seq2 seq2]
      (cond
        (and (empty? seq1)(empty? seq2))
          true
        (or (empty? seq1)(empty? seq2))
          false
        (= (first seq1)(first seq2))
          (recur (rest seq1) (rest seq2))
        :else
          false)))

(defn find-first-index [pred a-seq]
  (loop [x 0
         a-seq a-seq]
    (cond
      (empty? a-seq)
        nil
      (pred (first a-seq))
        x
      :else
        (recur (inc x) (rest a-seq)))))

(defn avg [a-seq]
  (loop [acc (first a-seq)
         a-seq (rest a-seq)
         c 1]
    (cond 
      (empty? a-seq)
        (/ acc c) 
      :else
        (recur (+ acc (first a-seq)) (rest a-seq) (inc c)))))

(defn parity [a-seq]
  (loop[k (keys(frequencies a-seq))
        v (vals(frequencies a-seq)) 
        acc '()]
    (cond
      (empty? v)
        acc
      (== 1(mod (first v) 2))
        (recur (rest k) (rest v) (cons (first k) acc))
      :else
        (recur (rest k) (rest v) acc))))

(defn fast-fibo [n]
  (loop [n n
         f0 0
         f1 1]
     (cond
       (= n 0) 0
       (> 2 n) f1
       :else   (recur (dec n) f1 (+ f0 f1)))))

(defn cut-at-repetition [a-seq]
  (loop [acc   []
         a-seq a-seq]
    (cond 
      (or (contains? (set acc) (first a-seq))(empty? a-seq))
        acc 
      :else
        (recur (conj acc (first a-seq)) (rest a-seq)))))