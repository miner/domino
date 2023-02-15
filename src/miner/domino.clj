(ns miner.domino)


;;; probably faster to use single long with high byte and low byte for two sides

;;; natural sort already works, good indexing, identity, equality, etc.

;;; Note lower (or equal) pips is byte 1 (shift 8 bits).  Bigger pips in byte 0.
;;; 0x0102 or 0x0A0B
;;; never 0x0901 -- canonical is 0x0109

;;; Using a domino in reverse canonical order shouldn't be an issue.  Just keep track of
;;; tail number for matching and maybe unordered bag of used tiles for stats.

(defn pips [d]
  (bit-and d 0xFF))

(defn pips2 [d]
  (bit-shift-right d 8))

(defn dub-pips [d]
  (let [p (pips d)]
    (when (= (pips2 d) p)
      p)))

(defn total-pips [d]
  (+ (pips d) (pips2 d)))

;; double-blank is worth 50 
(defn score-pips [d]
  (let [tot (total-pips d)]
    (if (pos? tot)
      tot
      50)))

;; high is normally 6 or 12 pips
(defn gen-domino-ints [high]
  (for [i (range 0 (inc high))
        :let [hi (bit-shift-left i 8)]
        j (range i (inc high))]
    (bit-or hi j)))


(defn dom-str [dom]
  (str (pips2 dom) ":" (pips dom)))

(defn hand-str [doms]
  (clojure.string/join ", " (map dom-str (sort doms))))

(defn bone-str [doms]
  (str "[" (clojure.string/join ", " (map dom-str doms)) "]"))

(defn count-dominoes [high]
  (/ (* (inc (inc high)) (inc high)) 2))


;;; returns canonical lo <= hi
(defn mkdom [a b]
  (assert (and (<= 0 a 12) (<= 0 b 12)))
  (bit-or (bit-shift-left (min a b) 8)
          (max a b)))

(defn pipv [d]
  [(pips2 d) (pips d)])


;;; Game state

;;; Mexican train = 0, other players are 1 to N in rotational order
;;; player turn: one of 1 ... N
;;; next-starter: player-id
;;; winner: player-id
;;; hands array [0...N] of hand [doms...]
;;; bone-yard: vector of shuffled doms in (hands 0).  Players 1...N

;;; :tail [0..N] int
;;; :public [0..N] bool
;;; :train [0..N]  [doms...]

;;; when center C:C is established all trains are initialized with tail C



(defn init-game
  ([] (init-game 4))
  ([num-players]
  (assert (> num-players 1))
  (let [high 6 ;;later 12
        hand-size 4 ;;later 15
        all-doms (gen-domino-ints high)
        count-doms (count all-doms)
        shuffled (shuffle all-doms)
        to-be-dealt (* num-players hand-size)
        _ (assert (< to-be-dealt count-doms))
        bone-yard (vec (take (- count-doms to-be-dealt) shuffled))
        hands (into [nil] (comp (drop (count bone-yard)) (partition-all hand-size))
                    shuffled)]
    {:high-pips high
     :start nil
     :next-player 1
     :unsatisfied nil
     :winner nil
     :num-players num-players
     :bone-yard bone-yard
     :tail (into [] (repeat (inc num-players) nil))
     :public (into [] (repeat (inc num-players) false))
     :train (into [] (repeat (inc num-players) []))
     :hand hands})))

;;; Not sure we need :start as tails will be set to it
;;; Maybe submap of :initial-data that never changes ???
;;; hands are just seqs for now.  Probably will need indexes later for searching.

(defn next-player [game]
  (let [nxt (inc (:next-player game))]
    (assoc game :next-player (if (> nxt (:num-players game)) 1 nxt))))

(defn remove-dom [hand dom]
  (remove #(= % dom) hand))

(defn find-start [game]
  (let [player (:next-player game)
        ;; tricky -- double pips sort same as single pips, not true in general as lo-pips dominate
        hi-dub (reduce max -1 (filter dub-pips (nth (:hand game) player)))]
    (if (neg? hi-dub)
      (let [draw (peek (:bone-yard game))]
        (if-let [start (dub-pips draw)]
          (-> game (update :bone-yard pop)
              (assoc :start start))
          ;; failed to start, let next player try
          (-> game (next-player game)
              (update :bone-yard pop)
              (update-in [:hand player] conj draw))))
      (let [start (pips hi-dub)]
        (-> game
            (update-in [:hand player] remove-dom hi-dub)
            (assoc :start start))))))

(defn print-game [game]
  (println "Next" (:next-player game) " start:" (:start game) " unsat:" (:unsatisfied game))
  (dotimes [i (:num-players game)]
    (print i (nth (:tail game) i))
    (when (nth (:public game) i) (print "*"))
    (when (pos? i) (print "; hand:" (hand-str (nth (:hand game) i))))
    (println))
  (println "Bone yard:" (bone-str (:bone-yard game)))
  (println))


(defn initial-train [game player]
  ;; UNFINISHED
  game)

(defn satisfy [game player unsat-train]
  (let [targ (nth (:tail game) unsat-train)]
    ;; find a tile matching targ
    ;; play it on that train
    ;; or draw, maybe play or give up
    ;; on to next player
    ;; UNFINISHED
    game))

(defn normal-play [game player]
  (let [possible-trains (into #{player} (filter (:public game))
                              (range (inc (:num-players game))))
        possible-tails nil]
    ;; UNFINISHED
    ;; pick best tile to play on best train
    ;; or draw if no

    ))
    


(defn run-game []
  (loop [game (loop [g (init-game 4)] (if (:start g) g (find-start g)))]
    (if (:winner game)
      game
      (let [p (:next-player game)]
        (if-let [unsat (:unsatisfied game)]
          (satisfy game p unsat)
          (if (nil? (nth (:tail game) p))
            (initial-train game p)
            (normal-play game p)))))))
            


(defn make-train [train hand]
  ;; use alpha-beta search through space of hand
  ;; return vector of unused hand in final position
  )





;;; stolen and hacked from hack/bitset.clj
(defn hexstr
  ([n] (hexstr 4 n))
  ([width n]
   {:pre [(<= 0 width 16)]}
   (let [hs (clojure.string/upper-case (Long/toHexString n))
         len (count hs)]
     (if (> width len)
       (str (subs "0000000000000000" 0 (- width len)) hs)
       hs))))


;;; ----------------------------------------------------------------------
;;; JUNK

;;; Dropped idea -- put canonical a <= b in low bits for standard hash
;;; high bits for swapped/re-ordered dom

;;; decided it's not important to keep order/reverse of domino -- better to stay canonical
;;; and keep separate tail value -- that's the only thing you have to match.

;;; A B H L
;;; A B is the creation order
;;; H=(min a b)
;;; L=(max a b)


;;; obsolete

;;; Represent as [Low High]
;;; 0 = blank
;;; High is typically either 6 or 12.

#_ (defn flip [[a b]]
  [b a])

#_ (defn canonical [[a b :as dom]]
  (if (<= a b)
    dom
    [b a]))


#_ (defn same? [[a b] [c d]]
  (or (and (= a c) (= b d))
      (and (= a d) (= b c))))

#_ (defn total [[a b]]
  (+ a b))

#_ (defn score [[a b]]
  (let [tot (+ a b)]
    (if (zero? tot)
      50
      tot)))


;;; train must be a vector of vectors
#_ (defn tail [train]
  (peek (peek train)))

;;; train is vector of dominoes [[a b] [b c] [c d] [d d] [d e]]

#_ (defn gen-dominoes [high]
  (for [i (range 0 (inc high))
        j (range i (inc high))]
    [i j]))
