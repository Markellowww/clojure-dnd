(ns mire.commands
  (:require [clojure.string :as str]
            [mire.rooms :as rooms]
            [mire.player :as player]))

(def event-stats (ref {}))

(def event-settings (ref {:move-event-chance 0.3
                          :grab-event-chance 0.25
                          :event-cooldown 5}))

(def last-event-time (ref {}))

(defn- can-trigger-event? [player-name event-type]
  "Checks if an event can be triggered for the player."
  (let [now (System/currentTimeMillis)
        last-time (get @last-event-time [player-name event-type] 0)
        cooldown (* (:event-cooldown @event-settings) 1000)]
    (> (- now last-time) cooldown)))

(defn- update-event-time [player-name event-type]
  "Updates the last event time and statistics."
  (dosync
   (alter last-event-time assoc [player-name event-type] (System/currentTimeMillis))
   (alter event-stats update-in [player-name event-type] (fnil inc 0))))

(defn- random-move-event []
  (let [events [{:message "You heard a strange noise behind you..."
                 :type :sound}
                {:message "A drop of water fell from the ceiling onto your head."
                 :type :effect}
                {:message "You felt a slight draft."
                 :type :ambience}
                {:message "Something fell in the distance with a crash."
                 :type :sound}
                {:message "You noticed a strange shadow in the corner of the room."
                 :type :ambience}
                {:message "Your foot brushed against something metallic."
                 :type :sound}
                {:message "There's a strange smell in the air."
                 :type :ambience}
                {:message "You stepped on a slippery rock and almost fell!"
                 :type :effect
                 :action (fn []
                           (Thread/sleep 1000)
                           "You lost your balance for a second.")}
                {:message "A mysterious voice whispers in your ear..."
                 :type :mystery
                 :action (fn []
                           (str "\"Be careful, " player/*name* "...\""))}
                {:message "You found an old coin on the floor!"
                 :type :loot
                 :action (fn []
                           (dosync
                            (when (< (rand) 0.5)
                              (alter player/*inventory* conj :old-coin)))
                           "Coin added to inventory.")}]
        event (rand-nth events)]
    (if (:action event)
      (str (:message event) "\n" ((:action event)))
      (:message event))))

(defn- random-grab-event [item]
  (let [events [{:message (str "When you picked up " item ", it glowed slightly.")
                 :type :magic}
                {:message (str item " was unexpectedly heavy.")
                 :type :effect}
                {:message (str "You felt a strange energy from " item ".")
                 :type :mystery}
                {:message (str "You noticed strange symbols on " item ".")
                 :type :lore}
                {:message (str item " suddenly slipped out of your hands and fell to the floor!")
                 :type :cursed
                 :action (fn []
                           (dosync
                            (alter player/*inventory* disj (keyword item))
                            (alter (:items @player/*current-room*) conj (keyword item)))
                           (str item " fell to the floor."))}
                {:message (str "Along with " item ", you also found something!")
                 :type :bonus
                 :action (fn []
                           (let [bonus-items ["small-key" "package" "strange-piece-of-paper"]
                                 bonus (rand-nth bonus-items)]
                             (dosync
                              (alter player/*inventory* conj (keyword bonus)))
                             (str "You also received: " bonus)))}]
        event (rand-nth events)]
    (if (:action event)
      (str (:message event) "\n" ((:action event)))
      (:message event))))

(defn- move-between-refs
  "Move one instance of obj between from and to. Must call in a transaction."
  [obj from to]
  (alter from disj obj)
  (alter to conj obj))

(defn look
  "Get a description of the surrounding environs and its contents."
  []
  (str (:desc @player/*current-room*)
       "\nExits: " (keys @(:exits @player/*current-room*)) "\n"
       (str/join "\n" (map #(str "There is " % " here.\n")
                           @(:items @player/*current-room*)))))

(defn move
  "\"♬ We gotta get out of this place... ♪\" Give a direction."
  [direction]
  (dosync
   (let [target-name ((:exits @player/*current-room*) (keyword direction))
         target (@rooms/rooms target-name)]
     (if target
       (do
         (move-between-refs player/*name*
                            (:inhabitants @player/*current-room*)
                            (:inhabitants target))
         (ref-set player/*current-room* target)

         (let [result (look)
               event-result (if (and (< (rand) (:move-event-chance @event-settings))
                                     (can-trigger-event? player/*name* :move))
                              (do
                                (update-event-time player/*name* :move)
                                (str "[Event] " (random-move-event)))
                              "")]
           (str result event-result)))
       "You can't go that way."))))

(defn grab
  "Pick something up."
  [thing]
  (dosync
   (if (rooms/room-contains? @player/*current-room* thing)
     (do (move-between-refs (keyword thing)
                            (:items @player/*current-room*)
                            player/*inventory*)
       (let [base-result (str "You picked up the " thing ".")
             event-result (if (and (< (rand) (:grab-event-chance @event-settings))
                                   (can-trigger-event? player/*name* :grab))
                            (do
                              (update-event-time player/*name* :grab)
                              (str "[Event] " (random-grab-event thing)))
                            "")]
         (str base-result event-result)))
     (str "There isn't any " thing " here."))))

(defn discard
  "Put something down that you're carrying."
  [thing]
  (dosync
   (if (player/carrying? thing)
     (do (move-between-refs (keyword thing)
                            player/*inventory*
                            (:items @player/*current-room*))
         (str "You dropped the " thing "."))
     (str "You're not carrying a " thing "."))))

(defn inventory
  "See what you've got."
  []
  (str "You are carrying:\n"
       (str/join "\n" (seq @player/*inventory*))))

(defn detect
  "If you have the detector, you can see which room an item is in."
  [item]
  (if (@player/*inventory* :detector)
    (if-let [room (first (filter #((:items %) (keyword item))
                                 (vals @rooms/rooms)))]
      (str item " is in " (:name room))
      (str item " is not in any room."))
    "You need to be carrying the detector for that."))

(defn say
  "Say something out loud so everyone in the room can hear."
  [& words]
  (let [message (str/join " " words)]
    (doseq [inhabitant (disj @(:inhabitants @player/*current-room*)
                             player/*name*)]
      (binding [*out* (player/streams inhabitant)]
        (println player/*name* "said:" message)
        (print player/prompt) (flush)))
    (str "You said " message)))

(defn shout
  "Say something so everyone in the current and adjacent rooms can hear."
  [& words]
  (let [message (str/join " " words)
        current-room @player/*current-room*
        neighbor-names (vals @(:exits current-room))
        neighbor-rooms (keep #(get @rooms/rooms %) neighbor-names)
        all-rooms (cons current-room neighbor-rooms)
        all-inhabitants (set (mapcat #(seq @(:inhabitants %)) all-rooms))]

    (doseq [inhabitant (disj all-inhabitants player/*name*)]
      (binding [*out* (player/streams inhabitant)]
        (println player/*name* "shouted:" message)
        (print player/prompt) (flush)))

    (str "You shouted: \"" message "\" (heard in "
         (count all-rooms) " rooms)")))

(defn whisper
      "Send a private message to a specific player."
      [target & words]
      (let [message (str/join " " words)]
           (cond
             (= target player/*name*)
             "You can't whisper to yourself."
             (not (player/streams target))
             (str "Player \"" target "\" doesn't exist.")
             :else
             (do
               (binding [*out* (player/streams target)]
                        (println "Whisper from" player/*name* ":" message)
                        (print player/prompt) (flush))
               (str "You whispered to " target ": \"" message "\"")))))

(defn help
  "Show available commands and what they do."
  []
  (str/join "\n" (map #(str (key %) ": " (:doc (meta (val %))))
                      (dissoc (ns-publics 'mire.commands)
                              'execute 'commands))))

(defn stats
  "Show statistics of your random events."
  []
  (let [player-name player/*name*
        stats (get @event-stats player-name {})]
    (if (empty? stats)
      "You haven't had any random events yet."
      (str "Random event statistics for " player-name ":\n"
           (str/join "\n" (map (fn [[k v]] (str (name k) ": " v " times"))
                               stats))))))

(def commands {"move" move,
               "north" (fn [] (move :north)),
               "south" (fn [] (move :south)),
               "east" (fn [] (move :east)),
               "west" (fn [] (move :west)),
               "grab" grab
               "discard" discard
               "inventory" inventory
               "detect" detect
               "look" look
               "say" say
               "shout" shout
               "whisper" whisper
               "help" help
               "stats" stats})

(defn execute
  "Execute a command that is passed to us."
  [input]
  (try (let [[command & args] (.split input " +")]
         (apply (commands command) args))
       (catch Exception e
         (.printStackTrace e (new java.io.PrintWriter *err*))
         "You can't do that!")))