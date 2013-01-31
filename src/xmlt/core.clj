(ns xmlt.core
  (:require [lamina
             [core :as l]
             [executor :as lex]])
  (:import [javax.xml.stream
            XMLInputFactory XMLOutputFactory XMLEventFactory]
           [javax.xml.stream.events
            StartElement EndElement Characters]))


(def input-factory ^XMLInputFactory (XMLInputFactory/newFactory))
(def output-factory ^XMLOutputFactory (XMLOutputFactory/newFactory))
(def event-factory ^XMLEventFactory (XMLEventFactory/newFactory))

(defn xml->events [reader-or-stream]
  ;; TODO encoding?
  (let [event-reader (. input-factory (createXMLEventReader reader-or-stream))]
    {:reader event-reader
     :events (iterator-seq event-reader)}))

(defn events->xml [events writer-or-stream]
  ;; TODO again, encoding...
  (with-open [event-writer (. output-factory (createXMLEventWriter writer-or-stream))]
    (doseq [event events]
      (. event-writer (add event)))))

(defn transform-tag-content [ch current-tag & {:keys [ctx path-transformers]}]
  (l/enqueue ch current-tag)
  (l/run-pipeline {:ctx ctx :path []}
                  (fn [{:keys [ctx path] :as m}]
                    (l/run-pipeline (l/read-channel ch)
                                    (fn [ev] (assoc m :ev ev))))

                  (fn [{:keys [ctx path ev] :as m}]
                    (condp instance? ev
                      StartElement (let [start-el-name (keyword (.. ev getName getLocalPart))
                                         new-path (conj path {:tag start-el-name})]
                                     (if-let [transformer (get path-transformers (map :tag new-path))]
                                       (let [after-ctx @(l/run-pipeline (transformer :ch ch :current-tag ev :ctx ctx))]
                                         (l/restart {:ctx after-ctx :path path}))
                                       (do (l/enqueue ch ev)
                                           (l/restart {:ctx ctx :path new-path}))))

                      Characters (let [text (.. ev getData)
                                       new-path (conj path {:tag 'text})]
                                   (if-let [transformer (get path-transformers (map :tag new-path))]
                                     (let [after-ctx @(l/run-pipeline (transformer :ch ch :text text :ctx ctx))]
                                       (l/restart {:ctx after-ctx :path path}))
                                     (do (l/enqueue ch ev)
                                         (l/restart m))))

                      EndElement (if-let [new-path (seq (butlast path))]
                                   (do (l/enqueue ch ev)
                                       (l/restart {:ctx ctx :path (vec new-path)}))

                                   (let [after-ctx (if-let [after-transformer (get path-transformers :after)]
                                                     (after-transformer :ctx ctx :ch ch)
                                                     ctx)]
                                     (l/enqueue ch ev)
                                     (l/complete after-ctx)))))))

(defn add-str [ch s]
  (l/enqueue ch (. event-factory (createCharacters s))))

(defn add-tag [ch & elements]
  (doseq [element elements]
    ))

(defn transform-file [in-stream out-writer transformer]
  (let [[our-ch their-ch] (l/channel-pair)
        {:keys [events reader]} (xml->events in-stream)
        [start-doc start-root-tag & more] events
        renderer-task (lex/task
                       (events->xml (l/channel->lazy-seq our-ch)
                                    out-writer))]

    (l/siphon (l/lazy-seq->channel more) our-ch)

    (l/enqueue their-ch start-doc)
            
    (l/run-pipeline nil
                    (fn [_]
                      (transformer their-ch start-root-tag))

                    (fn [ctx]
                      ;; This redirects any unused events (ideally
                      ;; just the end-root and end-doc) back to us
                      (l/siphon their-ch their-ch)
                      (l/close their-ch)
                      (.close reader)
                              
                      (l/run-pipeline nil
                                      ;; await the renderer task
                                      (constantly renderer-task)

                                      ;; return the context
                                      (constantly ctx))))))

#_(let [sw (java.io.StringWriter.)
      sr (java.io.StringReader. "<root><hello><world>Text</world><world>More text</world></hello></root>")]
  @(transform-file sr sw
                   (fn [ch current-tag]
                     (transform-tag-content ch current-tag
                                            :path-transformers
                                            {[:hello]
                                             (fn [& {:keys [ch current-tag ctx]}]
                                               (transform-tag-content ch current-tag
                                                                      :path-transformers {[:world]
                                                                                          (fn [& {:keys [ch current-tag ctx]}]
                                                                                            (transform-tag-content ch current-tag
                                                                                                                   :ctx ctx
                                                                                                                   :path-transformers {['text]
                                                                                                                                       (fn [& {:keys [ch text ctx]}]
                                                                                                                                         (add-str ch (apply str (reverse text)))
                                                                                                                                         (update-in ctx [:worlds] conj 1))}))

                                                                                          :after
                                                                                          (fn [& {:keys [ctx]}]
                                                                                            (add-str ch (format "There were %s :world tags"
                                                                                                                (count (:worlds ctx)))))}))})))
  (str sw))
