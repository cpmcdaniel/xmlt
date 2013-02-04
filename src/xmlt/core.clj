(ns xmlt.core
  (:import [javax.xml.stream
            XMLInputFactory XMLOutputFactory XMLEventFactory XMLEventReader XMLEventWriter]
           [javax.xml.stream.events
            XMLEvent StartElement EndElement Characters]))


(def ^XMLInputFactory input-factory (XMLInputFactory/newFactory))
(def ^XMLOutputFactory output-factory (XMLOutputFactory/newFactory))
(def ^XMLEventFactory event-factory (XMLEventFactory/newFactory))

(defn ^XMLEventReader open-xml-reader [reader-or-stream]
  ;; TODO encoding?
  (.createXMLEventReader input-factory reader-or-stream))

(defn ^XMLEventWriter open-xml-writer [writer-or-stream]
  ;; TODO again, encoding...
  (.createXMLEventWriter output-factory writer-or-stream))

(def ^:dynamic ^XMLEventReader *r* nil)
(def ^:dynamic ^XMLEventWriter *w* nil)

(defn write-event* [^XMLEvent event]
  (when *w*
    (.add *w* event)))

(defn transform-tag-content [ctx path-transformers]
  (write-event* (.nextEvent *r*))

  (loop [{:keys [ctx path] :as m} {:ctx ctx :path []}]
    (let [^XMLEvent ev (.peek *r*)]
      (condp instance? ev
        StartElement (let [start-el-name (keyword (.. ^StartElement ev getName getLocalPart))
                           new-path (conj path {:tag start-el-name})]
                       (if-let [transformer (or (get path-transformers (map :tag new-path))
                                                (get path-transformers (conj (mapv :tag path) :<*>))
                                                (get path-transformers [:<*>]))]
                         (recur {:ctx (transformer ctx :path new-path) :path path})
                         (do (write-event* (.nextEvent *r*))
                             (recur {:ctx ctx :path new-path}))))

        Characters (let [ev (.nextEvent *r*)
                         text (.. ^Characters ev getData)]
                     (if-let [transformer (or (get path-transformers (conj (mapv :tag path) :-text))
                                              (get path-transformers [:<*> :-text]))]
                       (recur {:ctx (transformer ctx :text text :path path) :path path})
                       (do (write-event* ev)
                           (recur m))))

        EndElement (let [ev (.nextEvent *r*)]
                     (if (seq path)
                       (do (write-event* ev)
                           (recur {:ctx ctx :path (vec (butlast path))}))

                       (let [after-ctx (if-let [after-transformer (get path-transformers :after)]
                                         (after-transformer ctx)
                                         ctx)]
                         (write-event* ev)
                         after-ctx)))

        (if (.hasNext *r*)
          (do (write-event* (.nextEvent *r*))
              (recur m))

          ctx)))))

(defn add-str [s]
  (write-event* (. event-factory (createCharacters s))))

(defn add-tag [element]
  ;; TODO doesn't handle attributes yet.
  (cond
   (vector? element) (let [[tag & content] element]
                       (write-event* (. event-factory (createStartElement "" "" (name tag))))
                       (add-tag content)
                       (write-event* (. event-factory (createEndElement "" "" (name tag)))))
   (string? element) (write-event* (. event-factory (createCharacters element)))
   (seq? element) (doseq [e element] (add-tag e))
   :otherwise (add-tag (str element))))

(defn transform-file [in-stream out-writer transformer]
  (with-open [r (open-xml-reader in-stream)
              w (open-xml-writer out-writer)]
    (binding [*r* r
              *w* w]
      (transformer))))

(let [sw (java.io.StringWriter.)
      sr (java.io.StringReader. "<root><hello><test><test2>desreveR</test2><test4>drawkcaB</test4></test><test3>Kept</test3><world>doubled</world><world>doubled again</world></hello></root>")]
  (transform-file sr sw
                  (fn []
                    (transform-tag-content
                     {}
                     {[:root :hello]
                      (fn [ctx & _]
                        (transform-tag-content
                         {}
                         {[:test :<*>]
                          (fn [ctx & {:keys [path]}]
                            (transform-tag-content
                             ctx
                             {[:-text]
                              (fn [ctx & {:keys [text]}]
                                (add-str (apply str "In '" (apply str (interpose "," (map :tag path))) "' tag: " (reverse text))))}))
                          [:world]
                          (fn [ctx & _]
                            (transform-tag-content
                             ctx
                             {[:-text]
                              (fn [ctx & {:keys [text]}]
                                (add-str (apply str (repeat 2 text)))
                                (update-in ctx [:worlds] conj 1))}))

                          :after
                          (fn [ctx & _]
                            (add-tag [:world-count (count (:worlds ctx))]))}))})))
  (str sw))
