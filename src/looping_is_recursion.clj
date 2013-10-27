(ns looping-is-recursion)

(defn power [b exp]
  (let [asd(fn [acc, t]
    (if (== t 0)
        acc
        (recur (* acc b) (dec t))
    )
    )]
  (if (== 0 exp)
    1
    (asd b (dec exp)))
    )
)

(defn last-element [a-seq]
  (let [asd(fn [acc]
    (if (empty? (rest acc))
        (first acc)
        (recur (rest acc))
    )
    )]
  (asd a-seq)))

(defn seq= [seq1 seq2]
  (let [asd(fn [a b]
    (cond
        (and (empty? a) (empty? b)) true
        (or (empty? a) (empty? b)) false
        (not= (first a) (first b)) false
        :else (recur (rest a) (rest b))
    )
    )]
  (asd seq1 seq2)
))

(defn find-first-index [pred a-seq]
  (loop [pred pred a a-seq i 0]
    (cond
        (empty? a) nil
        (pred (first a)) i
        :else (recur pred (rest a) (inc i))
        )))

(defn avg [a-seq]
  (loop [acc 0 s a-seq]
      (if (empty? s)
      (/ acc (count a-seq))
      (recur (+ acc (first s)) (rest s))
      )
   )
)

(defn toggle [s k]
  (if (contains? s k)
      (disj s k)
      (conj s k))
)
  
(defn parity [a-seq]
  (loop [acc #{} s a-seq]
    (if (empty? s)
        acc
        (recur (toggle acc (first s)) (rest s)))
))

(defn fast-fibo [n]
  (loop [a 0 b 1 c 1 counter 2]
    (cond
      (== n 0) 0
      (== n 1) 1
      (== n counter) c
      :else (recur b c (+ b c) (inc counter))
)))

(defn invec? [v i]
  (contains? (set v) i)
)

(defn cut-at-repetition [a-seq]
  (loop [seen [] items a-seq]
    (cond 
      (empty? items) seen
      (invec? seen (first items)) seen
      :else (recur (conj seen (first items)) (rest items) )
      )
    ))

