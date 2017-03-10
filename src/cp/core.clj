(ns cp.core
  (:gen-class))


(def +hand-count+ 5)
(def +bot-count-max+ 4)

;; suit one of 'diamond 'spade 'heart 'club
;; value 2-14
(defrecord Card [suit value])

;; hand list of Cards
(defrecord Player [name hand bot])

(defrecord Board [deck hands rounds])


(defn card-str [c]
  (str "\t" (:suit c)
       "\t"
       (let [v (:value c)]
         (cond
           (<= v 10) v
           (= v 11) "jack "
           (= v 12) "queen"
           (= v 13) "king "
           (= v 14) "ace  "
           true "Somethings fucked up"
       )
  )))

;; hand list of cards
(defn hand-value [hand]
  0
  )

;; shuffled deck of 52 Card
(defn deck-new []
  (let [nums '(2 3 4 5 6 7 8 9 10 11 12 13 14)
        suits '(diamond space heart club)]
    (shuffle
     (apply concat 
            (map (fn [suit]
                   (map (fn [num]
                          (Card. suit num))
                        nums))
                 suits)))))




(defn print-help []
  (println "no help")
  )

;; return true if command was general 
(defn general-command [txt]
  (case txt
    "help" (do (print-help) true)
    "quit" (do (println "Exiting") (System/exit 0))
    nil))

;; parse command
(defn command [msg fn-in]
  (println msg)
  (let [txt (read-line)]
    (if (general-command txt) ; general command
      (command msg fn-in) ; ask command again
      (fn-in msg txt)))) ; otherwise call fn on input


;; 1 - 4
(defn bot-count-parse [cmd txt]
    4)

(defn player-name-parse [cmd txt]
  (str "[player] " txt))


(defn lpop [lst]
  [(first lst) 	(rest lst)]
  )

(defn deck-hand [deck n]
  [(take n deck)
   (drop n deck)])



(defn deck-hands [deck n ret]
  (cond
    (= n 0) [deck ret]
    true (let [[h ndeck] (deck-hand deck +hand-count+)]
           (deck-hands ndeck (- n 1) (cons h ret))
           )))
  
(defn board-new []
  (println "New game")
  (let [player-name (command "insert player name"
                             player-name-parse)
        player-count (command "insert bot count[1-4]"
                              bot-count-parse)
        deck (deck-new)
        [d hnd] (deck-hands deck player-count nil)
        ]
    (Board. d
            ;; make players from hands
            (cons (Player. player-name (first hnd) nil)
                  (map (fn [bot-hand]
                         (Player. (str (gensym))
                                  bot-hand
                                  true))
                       (rest hnd)))
            0
            )))



(defn hand-print [h]
  (apply println (map (fn [t] (str (card-str t) "\n")) h)))

(defn score-print [hands]
  (if (or (not hands) (empty? hands))
    nil
    (let [p (first hands)]
      (if (:bot p)
        (println (str "[bot] " (:name p)))
        (println (:name p)))
      (hand-print (:hand p))
      (println "")
      (score-print (rest hands)))))

(defn game-win [board]
  (println "game-win")
  (if (= (:rounds board) 1)
    (do
      (score-print (:hands board))
      (command "Start new game?" (fn [cmd txt]
                                   (board-new))))
    nil))


(defn hands-find-player [hands]
  (if (not (:bot (first hands)))
    (first hands)
    (hands-find-player (rest hands))))

(defn player-hand-print [board]
  (let [ph (hands-find-player (:hands board))]
    (hand-print (:hand ph))
  (flush)))

(defn drop-nth-help [cont removed n len ret0 ret1]
  (if (not cont)
    [ret0 ret1]
    (if (= n len)
      [ret0 ret1]
      (if (some (partial = n) removed)
        (drop-nth-help (rest cont) removed (+ n 1) len ret0 (cons (first cont) ret1))
        (drop-nth-help (rest cont)
                       removed
                       (+ n 1)
                       len
                       (cons (first cont) ret0)
                       ret1)))))

(defn drop-vals [cont removed]
  (if (not removed)
    [cont '()]
    (drop-nth-help cont removed 0 (count cont) '() '())))
    

(defn board-swap-cards [board vals]
  (let [[ncards deck-tmp] (deck-hand (:deck board) (count vals))
        hands (:hands board)
        ph (hands-find-player hands)
        [keep back-to-deck] (drop-vals (:hand ph) vals)]
    (Board.
     (shuffle (concat back-to-deck deck-tmp))
     (map (fn [p]
            (if (:bot p)
              p
              (Player. (:name p)
                       (concat ncards keep)
                       nil)))
          hands)
     (+ 1 (:rounds board)))))

(defn and-fn [lst]
  (cond
   (empty? lst) true
   (first lst) (and-fn (rest lst))
   true nil))

(defn hand-swap-ask [board]
  (command "Select cards to swap, numbers from 0-4 delimited by space"
           (fn [cmd txt]
             (let [vals (map read-string (re-seq #"[\d.]+" txt))
                   bools (map (fn [num]
                                (if (and
                                     (number? num)
                                     (>= num 0)
                                     (<= num (- +hand-count+ 1)))
                                  true
                                  nil))
                              vals)]
               (if 
                   (and (and-fn bools)                    
                        (<= (count vals) +hand-count+))
                 (board-swap-cards board vals)
                 (do
                   (println "ERROR IN INPUT")
                   (hand-swap-ask board)))))))
           


;; execute state
;; show state
;; ask command
(defn next-round [board]
  (player-hand-print board)
  (cond
   (= (:rounds board) 0) (hand-swap-ask board)
   ;(= (:round board) 1) 
   true nil
   ))

;;; game = recursive function that gets the board and keeps track of game state
(defn game [& board-in]  
  (let [b (first board-in)
        board (or
               (game-win b)
               b
               (board-new))]
      (game (next-round board))))

(defn -main
  [& args]
  (println "At any time you can type help to get help message or quit to exit the game")
  (game))

