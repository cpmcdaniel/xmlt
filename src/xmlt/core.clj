(ns xmlt.core
  (:import [javax.xml.stream
            XMLInputFactory XMLOutputFactory]))

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