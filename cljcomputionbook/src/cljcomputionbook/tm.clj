(ns cljcomputionbook.tm
  (:require [clojure.string :as cs]))

(defrecord Tape [left middle right blank]
  Object
  (toString [tape]
    (pr-str tape)))

(defmethod print-method Tape [tape writer]
  (.write writer (format "#<Tape %s(%s)%s>"
                         (cs/join (:left tape))
                         (:middle tape)
                         (cs/join (:right tape)))))

(defn write [{:keys [left right blank]} ch]
  (Tape. left ch right blank))

(defmulti move-head (fn [tape direction] direction))

(defmethod move-head :left [{:keys [left middle right blank]} _]
  (Tape.
   (butlast left)
   (or (last left)
       blank)
   (concat [middle] right)
   blank))

(defmethod move-head :right [{:keys [left middle right blank]} _]
  (Tape.
   (concat left [middle])
   (or (first right)
       blank)
   (next right)
   blank))

(defrecord TMConfiguration [state tape])

(defprotocol Rule
  (applies-rule? [this conf])
  (follow-rule [this conf]))

(defn- next-tape [tape write_character direction]
  (->
   tape
   (write write_character)
   (move-head direction)))

(defrecord TMRule [state character next_state write_character direction]
  Rule
  (applies-rule? [this conf]
    (when (and
           (= state (:state conf))
           (= character (-> conf :tape :middle)))
      this))
  (follow-rule [this conf]
    (TMConfiguration.
     next_state
     (next-tape (:tape conf) write_character direction))))


(defprotocol Rulebook
  (next-configuration [this conf])
  (rule-for [this conf])
  (applies-to? [this conf]))

(defrecord DTMRulebook [rules]
  Rulebook
  (next-configuration [this conf]
    (follow-rule
     (rule-for this conf)
     conf))
  (rule-for [this conf]
    (some
     #(applies-rule? % conf)
     rules))
  (applies-to? [this conf]
    ((comp not nil?)
     (rule-for this conf))))

(defrecord DTM [current_configuration accept_states rulebook debug])

(defn accepting? [{:keys [accept_states current_configuration]}]
  (boolean
   (some (partial = (:state current_configuration))
         accept_states)))

(defn stuck? [{:keys [rulebook current_configuration] :as tm}]
  (and
   (not (accepting? tm))
   (not
    (applies-to? rulebook
                 current_configuration))))

(defn- debug-tm [{:keys [current_configuration debug] :as tm}]
  (when debug
    (println "DEBUG: "
             (merge
              (select-keys current_configuration [:state :tape])
              {:accepting? (accepting? tm)
               :stuck? (stuck? tm)}))))

(defn step [{:keys [current_configuration accept_states rulebook debug]
             :as tm}]
  (debug-tm tm)
  (DTM.
   (next-configuration rulebook current_configuration)
   accept_states
   rulebook
   debug))

(defn run [tm]
  (if (or (accepting? tm)
          (stuck? tm))
    (do
      (when (:debug tm)
        (debug-tm tm))
      tm)
    (recur
     (step tm))))

(def rulebook
  (DTMRulebook.
   [(TMRule. 1 0 2 1 :right)
    (TMRule. 1 1 1 0 :left)
    (TMRule. 1 '_ 2 1 :right)
    (TMRule. 2 0 2 0 :right)
    (TMRule. 2 1 2 1 :right)
    (TMRule. 2 '_ 3 '_ :left)]))

(def tape (Tape. [1 0 1] 1 [] '_))

(let [dtm (DTM. (TMConfiguration. 1 tape)
                [3]
                rulebook
                true)
      ran-dtm (run dtm)]
  (println (accepting? ran-dtm)))
