(ns swank.commands)

(defonce slime-fn-map {})

(defmacro defslimefn
  ([fname & body]
     `(alter-var-root #'slime-fn-map
                      assoc
                      (symbol (str "swank:" ~(name fname)))
		      (defn ~fname ~@body)))
  {:indent 'defun})

(defn fq-symbol [sym]
  (symbol (str "swank:" (name sym))))

(defn slime-fn [sym]
  (slime-fn-map (fq-symbol sym)))

(defn slime-fqfn [sym]
  (slime-fn-map sym))
