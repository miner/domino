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

(defn opips [d n]
  (condp = n
    (pips d) (pips2 d)
    (pips2 d) (pips d)
    nil))
            
#_ slower
(defn opp-pips [d n]
  (cond (= (pips d) n) (pips2 d)
        (= (pips2 d) n) (pips d)
        :else nil))


(defn total-pips [d]
  (+ (pips d) (pips2 d)))

;; double-blank is worth 50 
(defn score-pips [d]
  (let [tot (total-pips d)]
    (if (pos? tot)
      tot
      50)))


(defn count-dominoes [high]
  (/ (* (inc (inc high)) (inc high)) 2))

;; high is normally 6 or 12 pips
(defn gen-domino-ints [high]
  (for [i (range 0 (inc high))
        :let [hi (bit-shift-left i 8)]
        j (range i (inc high))]
    (bit-or hi j)))


(defn dom-str [dom]
  (str (pips2 dom) ":" (pips dom)))

;;; FIXME inefficient
(defn hand-str [hand]
  (let [doms (into #{} (mapcat #(get hand %)) (range 13))]
    (clojure.string/join ", " (map dom-str (sort doms)))))

(defn bone-str [doms]
  (str "[" (clojure.string/join ", " (map dom-str doms)) "]"))



;;; returns canonical lo <= hi
(defn dom
  ([[a b]] (dom a b))
  ([a b] 
   (assert (and (<= 0 a 12) (<= 0 b 12)))
   (bit-or (bit-shift-left (min a b) 8)
           (max a b))))

(defn pipv [d]
  [(pips2 d) (pips d)])


;;; Game state

;;; Mexican train = :mexican, other players are 1 to N in rotational order
;;; player turn: one of 1 ... N
;;; next-player: player-id
;;; winner: player-id
;;; bone-yard: vector of shuffled doms  (peek/pop to draw)
;;; player states are keys 1 .. N => maps of players
;;; :mexican key for mexican train (like player state without map of doms, just train)


;;; player-state is map
;;; :player N
;;; :public T/F
;;; :unsatisfied T/F
;;; :train  [ENGINE]   later, [ENGINE engine pips pips x x y ...]
;;;    vector, ENGINE comes from game initial DOUBLE
;;;    peek is pips to match
;;;    every two pips is from a domino played
;;;    test for doubles as necessary
;;; plus N keys mapping to set of dominoes that have that pip number -- so every domino (except
;;; doubles) maps to two keys N and M for the two pips.

;;; :train is a vector of pips,  start with initial double or "engine", then add two
;;; pips per domino so peek always is the next to match.  Double ending is easy to check.
;;;
;;; also better to keep all player info within hand map of 0..N to doms (double entry map).
;;; Just add non-int keys for :player N, :public true/false, :unsatisfied true/false within
;;; same map.
;;;
;;; Finding initial train is loop of trying to add dom, by end, adding one each to train
;;; into working set and keep adding until you can't, then move stat to `finished` list.  At
;;; end take longest train (non-double if tied)

;;; Need to check rules on initial train with unsatisfied double

;;; Exactly one Mexican train is allowed.


;;; For initial multi-tile turn, we should encode by leaving it nil until you have a chance
;;; to start.  Then at that point, copy init train from :mexican.  So init assignment is
;;; only for :mexican as well.  This will let a player who is forced to satisfy someone's
;;; double to still have an initial turn.

;;; FIXME question:  if you can't start a train, you have to draw.  Then can you still start
;;; multi-tile train or can you just play that one tile?  If you draw and still stuck, can
;;; you later do a multi-tile train or is that chance lost?

;;; For now, I'm saying if you draw, you can only play that one tile.  And you never get another
;;; chance to start multi-train -- except if you're forced to satisfy someone else's double.


;;; when center "engine" C:C is established all trains are initialized with tail C
;;; We use the :mexican :train as the standard

(defn engine [game]
  (get-in game [:mexican :train 0]))


;; "hand" is my old term for "player"

(defn empty-hand [high]
  (assoc (zipmap (range (inc high))
                 (repeat #{}))
         :train nil
         :public false
         :unsatisfied false))

(defn dom-conj [hand domino]
  (-> hand
      (update (pips domino) conj domino)
      (update (pips2 domino) conj domino)))

(defn dom-disj [hand domino]
  (-> hand
      (update (pips domino) disj domino)
      (update (pips2 domino) disj domino)))
      

(defn init-game
  ([] (init-game 4))
  ([num-players]
   (assert (> num-players 1))
   (let [high 12
         all-doms (gen-domino-ints high)
         count-doms (count all-doms)
         hand-size (long (/ (* 0.7 count-doms) num-players))
         shuffled (shuffle all-doms)
         to-be-dealt (* num-players hand-size)
         _ (assert (< to-be-dealt count-doms))
         bone-yard (vec (drop to-be-dealt shuffled))
         hands (sequence (comp (take to-be-dealt)
                               (partition-all hand-size))
                         shuffled)
         players (mapv #(reduce dom-conj (empty-hand high) %) hands)]

     (reduce (fn [st n] (assoc-in st [n :player] n))
             (merge (zipmap (range 1 (inc num-players)) players)
                    {:mexican {:player :mexican :public true :unsatisifed false :train nil}
                     :high-pips high
                     :next-player 1
                     :unsatisfied nil
                     :winner nil
                     :num-players num-players
                     :bone-yard bone-yard})
             (range 1 (inc num-players))))))



(defn hand-doms [player-state]
  (sort (map pipv (into #{} (mapcat player-state) (range 13)))))
  
(defn max-double [hand high]
  (first (keep dub-pips (mapcat hand (range high -1 -1)))))

;;; player 0 is the mexican train, 1-N are normal players
(defn inc-next-player [game]
  (let [nxt (inc (:next-player game))]
    (assoc game :next-player (if (<= nxt (:num-players game)) nxt 1))))

;; "The engine" is the starting doubles
(defn assign-engine [game]
  (let [player (:next-player game)]
    (if-let [max-dub (max-double (get game player) (:high-pips game))]
      (-> game
          (update player dom-disj (dom max-dub max-dub))
          (assoc-in [:mexican :train] [max-dub]))
      (let [draw (peek (:bone-yard game))]
        (if (dub-pips draw)
          (-> game
              (update :bone-yard pop)
              (assoc-in [:mexican :train] [(pips draw)]))
          (recur (-> game
                     (update :bone-yard pop)
                     (update player dom-conj draw)
                     (inc-next-player))))))))


(defn print-game [game]
  (println "Next" (:next-player game) " engine:" (get-in game [:mexican :train 0]) " unsat:" (:unsatisfied game))
  (doseq [i (conj (range 1 (inc (:num-players game))) :mexican)]
    (print i (get-in game [i :train]))
    (when (get-in game [i :public]) (print "*"))
    (when (and (not= i :mexican) (pos? i)) (print "; hand:" (hand-str (get game i))))
    (println))
  (println "Bone yard:" (bone-str (:bone-yard game)))
  (println))


;;; maybe start should have been pre-inserted in :train so you don't have to do it here?
;;; maybe doms need to be sorted so you get most pips in initial train?
;;; probably shouldn't end in a double, or maybe that's a strategy?  Would have to draw
;;; immediately!

(defn grow-initial-train [player-state engine]
  (let [fin? (fn [st] (empty? (get st (peek (:train st)))))
        expand-st (fn [st]
                    (let [end (peek (:train st))
                          doms (get st end)]
                      (map (fn [dom]
                             (-> st
                                 (update (pips dom) disj dom)
                                 (update (pips2 dom) disj dom)
                                 (update :train conj end (opips dom end))))
                           doms)))]
    (loop [working [(assoc player-state :train [engine])] finished nil]
      (let [finished1 (into finished (filter fin?) working)
            working1 (mapcat expand-st (remove fin? working))]
        (if (empty? working1)
          (apply max-key #(count (:train %)) finished1)
          (recur working1 finished1))))))

  

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
  (loop [game (loop [g (init-game 4)] (if (engine g) g (find-start g)))]
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
