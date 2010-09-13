;;; This code has been placed in the Public Domain.  All warranties are disclaimed.

(ns #^{:doc "Pass remote calls and responses between lisp systems using the swank-rpc protocol."
       :author "Terje Norderhaug <terje@in-progress.com>"}
  swank.rpc
  (:use (swank util)
        (swank.util io))
  (:import (java.io Writer Reader PushbackReader StringReader OutputStreamWriter FileOutputStream)))

;; ERROR HANDLING

(def swank-protocol-error (Exception. "Swank protocol error."))

;; LOGGING

(def *log-events* false)

(def *log-output* (OutputStreamWriter. (FileOutputStream. "swank-log" true)))

(defn log-event [format-string & args]
  (when *log-events* 
    (.write (or *log-output* *out*) (apply format format-string args))
    (.flush (or *log-output* *out*))
    nil))

;; INPUT
(defn- read-form)

(defn- rpc-read-string [#^Reader rdr]
  (let [sb (StringBuilder.)]
    (loop [] 
      (let [c (.read rdr)]
	(if (= c -1)
	  (throw (java.io.EOFException. "Incomplete reading of quoted string."))
	  (condp = (char c)
	      \" (str sb)
	      \\ (do (.append sb (char (.read rdr)))
		     (recur))
	      (do (.append sb (char c)) 
		  (recur))))))))

(defn- read-list [#^Reader rdr]
  (loop [result []]
    (let [form (read-form rdr)]
      (let [c (.read rdr)]
	(if (= c -1)
	  (throw (java.io.EOFException. "Incomplete reading of list."))
	  (condp = (char c)
	      \) (sequence (conj result form))
	      \space (recur (conj result form))))))))

(defn- read-form
  "Read a form that conforms to the swank rpc protocol"
  ([#^Reader rdr] (read-form rdr :base))
  ([#^Reader rdr state]
     (let [form (let [c (.read rdr)]    
		  (condp = (char c)
		      \" (rpc-read-string rdr)
		      \( (read-list rdr)
		      \' (list 'quote (read-form rdr :quote))
		      (let [sb (StringBuilder.)]
			(loop [c c]
			  (if (not= c -1)
			    (condp = (char c)
				\\ (do (.append sb (char (.read rdr))) 
				       (recur (.read rdr)))
				\space (.unread rdr c)
				\) (.unread rdr c)
				(do (.append sb (char c)) 
				    (recur (.read rdr))))))
			(let [str (str sb)]
			  (cond
			   (. Character isDigit c) (Integer/parseInt str)
			   (= "nil" str)
			   nil
			   (= "t" str)
			   true
			   (and (not (= state :quote)) (= "swank::%cursor-marker%" str))
			   ::cursor-marker
			   (and (not (= state :quote)) (.startsWith str ":"))
			   (keyword (.substring str 1 (count str)))
			   :else
			   (symbol str))))))]
       form)))

(defn- read-packet
  ([#^Reader reader]
     (let [len (Integer/parseInt (read-chars reader 6 swank-protocol-error) 16)]
       (read-chars reader len swank-protocol-error))))
 
(defn decode-message
   "Read an rpc message encoded using the swank rpc protocol." 
  ([#^Reader rdr]
    (let [packet (read-packet rdr)]
       (log-event "READ: %s\n" packet)
       (try
         (with-open [rdr (PushbackReader. (StringReader. packet))]
           (read-form rdr))
         (catch Exception e
           (list :reader-error packet e))))))

; (with-open [rdr (StringReader. "00001f(swank:a 123 (%b% (t nil) \"c\"))")] (decode-message rdr)) 
       

;; OUTPUT

(defmulti print-object (fn [x writer] (type x)))

(defmethod print-object :default [o, #^Writer w]
  (print-method o w))

(defmethod print-object Boolean [o, #^Writer w]
  (.write w (if o "t" "nil")))

(defmethod print-object String [#^String s, #^Writer w]
  (let [char-escape-string {\" "\\\""
                            \\  "\\\\"}]
    (do (.append w \")
      (dotimes [n (count s)]
        (let [c (.charAt s n)
              e (char-escape-string c)]
          (if e (.write w e) (.append w c))))
      (.append w \"))
  nil))

(defmethod print-object clojure.lang.ISeq [o, #^Writer w]
  (.write w "(")
  (print-object (first o) w)
  (doseq [item (rest o)]
    (.write w " ")
    (print-object item w))
  (.write w ")"))

(defn- write-form
  ([#^Writer writer message]
    (print-object message writer)))

(defn- write-packet
  ([#^Writer writer str]
   (let [len (.length str)]
    (doto writer
          (.write (format "%06x" len))
          (.write str)
          (.flush)))))

(defn encode-message
  "Write an rpc message encoded using the swank rpc protocol."
  ([#^Writer writer message]
     (let [str (with-out-str
                  (write-form *out* message)) ]
       (log-event "WRITE: %s\n" str)
       (write-packet writer str))))

; (with-out-str (encode-message *out* "hello"))
; (with-out-str (encode-message *out* '(a 123 (swank:b (true false) "c"))))


;; DISPATCH

(defonce rpc-fn-map {})

(defn- keyword-to-symbol [kw]
  (symbol (str ":" (name kw))))

(defn register-dispatch 
  ([name fn]
    (register-dispatch name fn #'rpc-fn-map))
  ([name fn fn-map]
     (assert (keyword? name))
     (alter-var-root fn-map assoc name fn)))

(defn dispatch-message
  ([conn message fn-map]
     (let [[operation & _] message
	   message-fn (fn-map operation)]
       (assert message-fn)
       (message-fn conn message)))
  ([conn message]
     (dispatch-message conn message rpc-fn-map)))

(defn test-decoder [msg]
  (decode-message (StringReader. (format "%06x%s" (count msg) msg))))
