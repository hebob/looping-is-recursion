(ns looping-is-recursion)

(defn power [base exp]
  (let [mul base
        helper (fn [a b] 
                 (if (zero? b) 
                   a 
                   (recur (* a mul)(dec b))))]
    (if (zero? exp)
      1
      (helper base (dec exp)))))

(defn last-element [a-seq]
  (let [helper (fn [b-seq]
                 (if (empty? (rest b-seq))
                   (first b-seq)
                   (recur (rest b-seq))))]
    (helper a-seq)))

(defn seq= [seq1 seq2]
  (let [helper (fn [a-seq b-seq]
                 (cond
                   (and (empty? a-seq) (empty? b-seq))
                      true
                   (or (empty? a-seq) (empty? b-seq))
                     false
                   (= (first a-seq) (first b-seq))
                     (recur (rest a-seq) (rest b-seq))
                   :else
                     false))]
    (helper seq1 seq2)))

(defn find-first-index [pred a-seq]
  (loop [index 0
         seq1 a-seq]
    (cond
       (empty? seq1)
         nil
       (pred (first seq1))
         index
       :else
         (recur (inc index) (rest seq1)))))

(defn avg [a-seq]
  (loop [iter 1
         sum 0 
         seq1 a-seq]
         (cond
           (empty? seq1)
            nil
           (empty? (rest seq1))
            (/ (+ sum (first seq1)) iter)
           :else
           (recur (inc iter) (+ sum (first seq1)) (rest seq1)))))

; first count items, then toggle unpaired
(defn parity [a-seq]
  (let [toggle (fn [a-set elem]
                 (if (contains? a-set elem)
                    (disj a-set elem)
                   (conj a-set elem)))]
 (loop [elems a-seq 
        counts #{}
        ]
        (cond 
          (empty? elems)
           nil
          (empty? (rest elems))
           (toggle counts (first elems))
          :else
           (recur (rest elems) (toggle counts (first elems)))))))

(defn fast-fibo [n]
  (loop [fibcount n
         cur 1
         prev 0]
    (cond
      (zero? fibcount)
       prev
      (== 1 fibcount)
       cur
      :else
       (recur (dec fibcount) (+ cur prev) cur) )))

(defn cut-at-repetition [a-seq]
  (loop [contained [] 
         my-seq a-seq]
      (cond
        (or (contains? (set contained) (first my-seq))
            (empty? my-seq))
          contained
        :else 
          (recur (conj contained (first my-seq)) (rest my-seq)))))
