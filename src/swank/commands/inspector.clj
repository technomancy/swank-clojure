(ns swank.commands.inspector
  (:use (swank util core commands)
        (swank.core connection))
  (:import (java.lang.reflect Field)))

;;;; Inspector for basic clojure data structures

;; This a mess, I'll clean up this code after I figure out exactly
;; what I need for debugging support.

(def inspectee (ref nil))
(def inspectee-content (ref nil))
(def inspectee-parts (ref nil))
(def inspectee-actions (ref nil))
(def inspector-stack (ref nil))
(def inspector-history (ref nil))

(defn reset-inspector []
  (dosync
   (ref-set inspectee nil)
   (ref-set inspectee-content nil)
   (ref-set inspectee-parts [])
   (ref-set inspectee-actions [])
   (ref-set inspector-stack nil)
   (ref-set inspector-history [])))

(defn indexed-values [obj]
  (apply concat
         (map-indexed (fn [idx val]
                        `(~(str "  " idx ". ") (:value ~val) (:newline)))
                      obj)))

(defn named-values [obj]
  (apply concat
         (for [[key val] obj]
           `("  " (:value ~key) " = " (:value ~val) (:newline)))))

(defn inspectee-title [obj]
  (cond
   (instance? clojure.lang.LazySeq obj) (str "clojure.lang.LazySeq@...")
   :else (str obj)))

(defn print-part-to-string [value]
  (let [s (inspectee-title value)
        pos (position #{value} @inspector-history)]
    (if pos
      (str "#" pos "=" s)
      s)))

(defn assign-index [o dest]
  (dosync
   (let [index (count @dest)]
     (alter dest conj o)
     index)))

(defn value-part [obj s]
  (list :value (or s (print-part-to-string obj))
        (assign-index obj inspectee-parts)))

(defn action-part [label lambda refresh?]
  (list :action label
        (assign-index (list lambda refresh?)
                      inspectee-actions)))

(defn label-value-line
  ([label value] (label-value-line label value true))
  ([label value newline?]
     (list* (str label) ": " (list :value value)
            (if newline? '((:newline)) nil))))

(defmacro label-value-line* [& label-values]
  `(concat ~@(map (fn [[label value]]
                    `(label-value-line ~label ~value))
                  label-values)))

;; Inspection

;; This is the simple version that only knows about clojure stuff.
;; Many of these will probably be redefined by swank-clojure-debug
(defmulti emacs-inspect
  (fn known-types [obj]
    (cond
     (map? obj) :map
     (vector? obj) :vector
     (var? obj) :var
     (string? obj) :string
     (seq? obj) :seq
     (instance? Class obj) :class
     (instance? clojure.lang.Namespace obj) :namespace
     (instance? clojure.lang.ARef obj) :aref
     (.isArray (class obj)) :array)))

(defn inspect-meta-information [obj]
  (when (seq (meta obj))
    (concat
     '("Meta Information: " (:newline))
     (named-values (meta obj)))))

(defmethod emacs-inspect :map [obj]
  (concat
   (label-value-line*
    ("Class" (class obj))
    ("Count" (count obj)))
   (inspect-meta-information obj)
   '("Contents: " (:newline))
   (named-values obj)))

(defmethod emacs-inspect :vector [obj]
  (concat
   (label-value-line*
    ("Class" (class obj))
    ("Count" (count obj)))
   (inspect-meta-information obj)
   '("Contents: " (:newline))
   (indexed-values obj)))

(defmethod emacs-inspect :array [#^"[Ljava.lang.Object;" obj]
  (concat
   (label-value-line*
    ("Class" (class obj))
    ("Count" (alength obj))
    ("Component Type" (.getComponentType (class obj))))
   '("Contents: " (:newline))
   (indexed-values obj)))

(defmethod emacs-inspect :var [#^clojure.lang.Var obj]
  (concat
   (label-value-line*
    ("Class" (class obj)))
   (inspect-meta-information obj)
   (when (.isBound obj)
     `("Value: " (:value ~(var-get obj))))))

(defmethod emacs-inspect :string [obj]
  (concat
   (label-value-line*
    ("Class" (class obj)))
   (list (str "Value: " (pr-str obj)))))

(defmethod emacs-inspect :seq [obj]
  (concat
   (label-value-line*
    ("Class" (class obj)))
   (inspect-meta-information obj)
   '("Contents: " (:newline))
   (indexed-values obj)))


(defmethod emacs-inspect :default [obj]
  (let [#^"[Ljava.lang.reflect.Field;" fields (. (class obj) getDeclaredFields)
        names (map #(.getName #^Field %) fields)
        get (fn [#^Field f]
              (try (.setAccessible f true)
                   (catch java.lang.SecurityException e))
              (try (.get f obj)
                   (catch java.lang.IllegalAccessException e
                     "Access denied.")))
        vals (map get fields)]
    (concat
     `("Type: " (:value ~(class obj)) (:newline)
       "Value: " (:value ~obj) (:newline)
       "---" (:newline)
       "Fields: " (:newline))
     (mapcat
      (fn [name val]
        `(~(str "  " name ": ") (:value ~val) (:newline))) names vals))))

(defn- inspect-class-section [obj section]
  (let [method (symbol (str ".get" (name section)))
        elements (eval (list method obj))]
    (if (seq elements)
      `(~(name section) ": " (:newline)
        ~@(mapcat (fn [f] `("  " (:value ~f) (:newline))) elements)))))

(defmethod emacs-inspect :class [#^Class obj]
  (apply concat (interpose ['(:newline) "--- "]
                           (cons `("Type: " (:value ~(class obj)) (:newline))
                                 (for [section [:Interfaces :Constructors
                                                :Fields :Methods]
                                       :let [elements (inspect-class-section
                                                       obj section)]
                                       :when (seq elements)]
                                   elements)))))

(defmethod emacs-inspect :aref [#^clojure.lang.ARef obj]
  `("Type: " (:value ~(class obj)) (:newline)
    "Value: " (:value ~(deref obj)) (:newline)))

(defn ns-refers-by-ns [#^clojure.lang.Namespace ns]
  (group-by (fn [#^clojure.lang.Var v] (. v ns))
            (map val (ns-refers ns))))

(defmethod emacs-inspect :namespace [#^clojure.lang.Namespace obj]
  (concat
   (label-value-line*
    ("Class" (class obj))
    ("Count" (count (ns-map obj))))
   '("---" (:newline)
     "Refer from: " (:newline))
   (mapcat (fn [[ns refers]]
             `("  "(:value ~ns) " = " (:value ~refers) (:newline)))
           (ns-refers-by-ns obj))
   (label-value-line*
    ("Imports" (ns-imports obj))
    ("Interns" (ns-interns obj)))))

(defn inspector-content [specs]
  (letfn [(spec-seq [seq]
            (let [[f & args] seq]
              (cond
                (= f :newline) (str \newline)

                (= f :value)
                (let [[obj & [str]] args]
                  (value-part obj str))

                (= f :action)
                (let [[label lambda & options] args
                      {:keys [refresh?]} (apply hash-map options)]
                  (action-part label lambda refresh?)))))
          (spec-value [val]
            (cond
              (string? val) val
              (seq? val) (spec-seq val)))]
    (map spec-value specs)))

;; Works for infinite sequences, but it lies about length. Luckily, emacs doesn't
;; care.
(defn content-range [lst start end]
    (let [amount-wanted (- end start)
          shifted (drop start lst)
          taken (take amount-wanted shifted)
          amount-taken (count taken)]
      (if (< amount-taken amount-wanted)
        (list taken (+ amount-taken start) start end)
        ;; There's always more until we know there isn't
        (list taken (+ end 500) start end))))

(defn inspect-object [o]
  (dosync
   (ref-set inspectee o)
   (alter inspector-stack conj o)
   (when-not (filter #(identical? o %) @inspector-history)
     (alter inspector-history conj o))
   (ref-set inspectee-content (inspector-content (emacs-inspect o)))
   (list :title (inspectee-title o)
         :id (assign-index o inspectee-parts)
         :content (content-range @inspectee-content 0 500))))

(defslimefn init-inspector [string]
  (with-emacs-package
    (reset-inspector)
    (inspect-object (eval (read-string string)))))

(defn inspect-in-emacs [what]
  (letfn [(send-it []
            (with-emacs-package
              (reset-inspector)
              (send-to-emacs `(:inspect ~(inspect-object what)))))]
    (cond
      *current-connection* (send-it)
      (comment (first @connections))
      ;; TODO: take a second look at this, will probably need garbage collection on connections
      (comment
        (binding [*current-connection* (first @connections)]
          (send-it))))))

(defslimefn inspect-frame-var [frame index]
  (if (and (zero? frame) *current-env*)
    (let [locals *current-env*
          object (locals (nth (keys locals) index))]
      (with-emacs-package
        (reset-inspector)
        (inspect-object object)))))

(defslimefn inspector-nth-part [index]
  (get @inspectee-parts index))

(defslimefn inspect-nth-part [index]
  (with-emacs-package
   (inspect-object ((slime-fn 'inspector-nth-part) index))))

(defslimefn inspector-range [from to]
  (content-range @inspectee-content from to))

(defn ref-pop [ref]
  (let [[f & r] @ref]
    (ref-set ref r)
    f))

(defslimefn inspector-call-nth-action [index & args]
  (let [[fn refresh?] (get @inspectee-actions index)]
    (apply fn args)
    (if refresh?
      (inspect-object (dosync (ref-pop inspector-stack)))
      nil)))

(defslimefn inspector-pop []
  (with-emacs-package
   (cond
    (rest @inspector-stack)
    (inspect-object
     (dosync
      (ref-pop inspector-stack)
      (ref-pop inspector-stack)))
    :else nil)))

(defslimefn inspector-next []
  (with-emacs-package
    (let [pos (position #{@inspectee} @inspector-history)]
      (cond
       (= (inc pos) (count @inspector-history)) nil
       :else (inspect-object (get @inspector-history (inc pos)))))))

(defslimefn inspector-reinspect []
  (inspect-object @inspectee))

(defslimefn quit-inspector []
  (reset-inspector)
  nil)

(defslimefn describe-inspectee []
  (with-emacs-package
   (str @inspectee)))
