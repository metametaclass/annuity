(ns annuity
 (:import [java.lang Math])
)


(defn round-cent
 [total]
 (/ (Math/round (* total 100.0)) 100.0))


(defn next-month
  [[y m]]
  (if (= m 12)
    [(inc y) 1]
    [y (inc m)]))


(defn print-payment
 [[y m] debt-payment interest-payment]
 (println y " " m " " debt-payment " " interest-payment 
          (round-cent (+ debt-payment interest-payment))))


(defn calc-annuity
  "Calculates and prints annuity payments.
   Usage: (calc-annuity total months interest-rate begin)
     total - loan total
     months - loan term in months
     interest-rate - year percent/interest rate
     begin - month of first payment, vector [y m]
   
   Example (calc-annuity 100500 180 12 [2008 8])
   "
  [total months interest-rate begin ]
  (let [month-perc (/ interest-rate 12 100)
        sum-of-perc (reduce + (take months (iterate (partial * (+ 1 month-perc)) 1.0)))
        perc-step (+ 1 month-perc) 
        start-payment (/ total sum-of-perc)]
    (loop [curr-debt total
           curr-payment-perc 1
           month begin]
      (let [curr-debt-payment (round-cent (* curr-payment-perc start-payment)) 
            curr-interest-payment (round-cent (* curr-debt month-perc))]                  
        (if (> curr-debt curr-debt-payment)
          (do (print-payment month curr-debt-payment curr-interest-payment)
              (recur (- curr-debt curr-debt-payment) 
                     (* curr-payment-perc perc-step) 
                     (next-month month)))
          (print-payment month curr-debt curr-interest-payment))))))





;(calc-annuity 100500 180 12 [2008 8])
