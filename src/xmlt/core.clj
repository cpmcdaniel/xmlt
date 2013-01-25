(ns xmlt.core
  (:import [javax.xml.stream
            XMLInputFactory XMLOutputFactory]
           [javax.xml.stream.events
            StartElement]))

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

(defn traverse [events & {:keys [ctx handlers]}]
  )

(traverse (xml->events (java.io.StringReader. "<foo><bar>Hello world</bar></foo>"))
          :ctx {:key :v
                :evs []}
          :handlers [(fn [events {:keys [ctx] :as handler-info}]
                       (let [[e & more] events]
                         [e (update-in ctx [:evs] conj {:ev e}) more]))])