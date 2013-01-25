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

#_(let [w (java.io.StringWriter.)]
    (events->xml (xml->events (java.io.StringReader. "<foo><bar>Hello world</bar></foo>")) w)
    (str w))

(defn start-element-handler [handler & {:keys [path tag]}]
  (fn [events {:keys [event-path event-tag attrs ctx] :as handler-info}]
    (when (and (instance? (first events) StartElement)
               (or (not path) (= event-path path))
               (or (not tag) (= event-tag tag)))
      (handler events handler-info))))

(defn split-to-matching-element [events]
  (letfn [(split-to-matching-element* [depth before-matching-event after-matching-event]
            (lazy-seq
             (if (seq after-matching-event)
               (let [e (first after-matching-event)
                     more (lazy-seq (rest after-matching-event))]
                 (condp instance? e
                   StartElement (split-to-matching-element* (inc depth)
                                                            (lazy-cat before-matching-event [e]) more)
                   EndElement (if (zero? depth)
                                ;; TODO should 'after' include the end
                                ;; element? Possibly to help with popping off the path
                                [before-matching-event after-matching-event]
                                (split-to-matching-element* (dec depth)
                                                            (lazy-cat before-matching-event [e]) more))
                   (split-to-matching-element* depth (lazy-cat before-matching-event [e]) more)))
               [before-matching-event nil])))]
    (split-to-matching-element* 0 nil events)))


(defn handle-events [events path {:keys [ctx handlers]}]
  (->> {:handlers handlers}

       ;; try each handler in turn
       (iterate 
        (fn [{[handler & more] :handlers}]
          {:handler-result (handler events :ctx ctx :path path)
           :handlers more}))

       ;; drop nil results if there are still handlers.
       (drop-while (fn [{:keys [handler-result handlers]}]
                     (and (not handler-result)
                          (seq handlers))))

       ;; return the first handler result
       first
       :handler-result))

{:path [:doc :book :trade] :handle (fn [events & {:keys [ctx path]}]
                                     events)}

(handle-events 'eventsy 'pathy {:ctx 'ctx
                :handlers [(fn [e & {:keys [ctx path]}]
                             path)
                           (fn [e & {:keys [ctx path]}]
                             nil)]})



(defn traverse [events & {:keys [ctx handlers] :as traversal}]
  (letfn [(get-new-path [event current-path]
            )
          (traverse* [events path {:keys [ctx handlers] :as traversal}]
            (lazy-seq
             (if-not (seq events)
               [nil ctx]
               
               (if-let [[handled-events rest-events ctx] (handle-events events path traversal)]
                 ;; Handler's handled - pass off the events and recurse through rest-events
                 (if (seq rest-events)
                   (lazy-seq
                    (let [[sub-handled-events ctx] (traverse* rest-events path (assoc traversal
                                                                                 :ctx ctx))]
                      [(lazy-cat handled-events sub-handled-events) ctx]))
                   [handled-events ctx])

                 ;; Nobody's interested - default handler
                 ;; (handle top event, modify path and recurse)
                 (let [[e & more] events])))))]
    
    (traverse* events [] traversal)))

#_(traverse (xml->events (java.io.StringReader. "<foo><bar>Hello world</bar></foo>"))
            :ctx {:key :v
                  :evs []}
            :handlers [(fn [events {:keys [ctx] :as handler-info}]
                         (let [[e & more] events]
                           [e (update-in ctx [:evs] conj {:ev e}) more]))])