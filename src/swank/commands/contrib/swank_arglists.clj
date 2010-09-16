(ns swank.commands.contrib.swank-arglists
  (:use (swank util core commands)))

((slime-fn 'swank-require) :swank-c-p-c)

(defn position-in-params? [params pos]
  (or (some #(= '& %) params)
      (<= pos (count params))))

;; (position-in-params? '[x y] 2)

(defn highlight-position [params pos]
  (if (<= pos (count (take-while #(not= % '&) params)))
    (into [] (concat (take (dec pos) params)
                     '(===>)
                     (list (nth params (dec pos)))
                     '(<===)
                     (drop pos params)))
    (if (some #(= % '&) params)
      (into [] (concat (take-while #(not= % '&) params)
                       '(===>)
                       '(&)
                       (list (last params))
                       '(<===))))))

;; (highlight-position '[x y] 1)

(defn highlight-param-lists [params-list pos]
  (loop [checked []
         current (first params-list)
         remaining (rest params-list)]
    (if (position-in-params? current pos)
      (concat checked
              [(highlight-position current pos)]
              remaining)
      (when (seq remaining)
        (recur (conj checked current)
               (first remaining)
               (rest remaining))))))

;; (highlight-param-lists '([fname & body]) 1)

(defslimefn arglist-for-echo-area [raw-specs & options]
  (let [{:keys [arg-indices
                print-right-margin
                print-lines]} (apply hash-map options)]
    ;; Yeah, I'm lazy -- I'll flesh this out later
    (if (and raw-specs
             (seq? raw-specs)
             (seq? (first raw-specs)))
      (let [result ((slime-fn 'operator-arglist) (ffirst raw-specs) *current-package*)
            pos (first (second options))
            cmd (ffirst raw-specs)]
        (str cmd ": "
             (if (or (zero? pos) (nil? result))
               nil
               (apply list (highlight-param-lists (read-string result) pos)))))
      nil)))

(defslimefn variable-desc-for-echo-area [variable-name]
  (with-emacs-package
   (or 
    (try
     (when-let [sym (read-string variable-name)]
       (when-let [var (resolve sym)]
         (when (.isBound #^clojure.lang.Var var)
           (str variable-name " => " (var-get var)))))
     (catch Exception e nil))
    "")))
