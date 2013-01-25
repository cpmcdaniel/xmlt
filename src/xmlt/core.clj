(ns xmlt.core
  (:import [javax.xml.stream
            XMLInputFactory XMLOutputFactory]
           [javax.xml.stream.events
            StartElement EndElement Characters]))

(def input-factory ^XMLInputFactory (XMLInputFactory/newFactory))
(def output-factory ^XMLOutputFactory (XMLOutputFactory/newFactory))

(defn xml->events [reader-or-stream]
  ;; TODO encoding?
  (let [event-reader (. input-factory (createXMLEventReader reader-or-stream))]
    (iterator-seq event-reader)))

(defn events->xml [events writer-or-stream]
  ;; TODO again, encoding...
  (with-open [event-writer (. output-factory (createXMLEventWriter writer-or-stream))]
    (doseq [event events]
      (. event-writer (add event)))))

(defn- handle-events [handlers {:keys [ctx events path] :as m}]
  (->> handlers

       ;; try each handler in turn
       (reductions
        (fn [_ handler]
          (handler m))
        nil)

       (drop-while nil?) ;; drop while the handlers aren't interested
       first))           ;; get the first one that is

(defn traverse [events & {:keys [ctx handlers]}]
  (mapcat :out-events ;; TODO how to get ctx out?
          (take-while #(or (:out-events %) (seq (:events %)))
                      (rest (iterate (fn [{:keys [events path ctx] :as m}]
                                       (when (seq events)
                                         (if-let [handled-m (handle-events handlers m)]
                                           handled-m
                                           ;; TODO handle with default
                                           (do))))

                                     {:events events
                                      :path nil
                                      :ctx ctx})))))

#_(time (let [events (lazy-cat ["test" "world hello"] ["diff"])
              ctx {}
              handlers [(fn [{[e & more] :events
                              :keys [ctx path]
                              :as m}]
                          (when-let [c (:count ctx)]
                            (-> m
                                (assoc :events more)
                                (assoc :out-events (repeat c e)))))
                        (fn [{[e & more] :events
                              :keys [ctx path]
                              :as m}]
                          (-> m
                              (assoc :events more)
                              (assoc :out-events [e])))]]
          (mapcat :out-events
                  (take-while #(or (:out-events %) (seq (:events %)))
                              (rest (iterate (fn [{:keys [events path ctx] :as m}]
                                               (when (seq events)
                                                 (if-let [handled-m (handle-events handlers m)]
                                                   (merge m (dissoc handled-m :path))
                                                   ;; TODO handle with default
                                                   (do))))

                                             {:events events
                                              :path nil
                                              :ctx ctx}))))))



#_(traverse (xml->events (java.io.StringReader. "<foo><bar>Hello world</bar></foo>"))
            :ctx {:key :v
                  :evs []}
            :handlers [(fn [events {:keys [ctx] :as handler-info}]
                         (let [[e & more] events]
                           [e (update-in ctx [:evs] conj {:ev e}) more]))])