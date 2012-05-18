;; Requires clojure 1.1 (currently in alpha)
(ns swank.test-swank.util.net.sockets
  (:import (java.net ServerSocket Socket InetSocketAddress))
  (:use clojure.test
        swank.util.net.sockets))

(deftest making-server
  (are [x] (with-open [#^ServerSocket socket x]
             (instance? ServerSocket x))
       (make-server-socket)
       (make-server-socket {:backlog 10})
       (make-server-socket {:host "localhost"})))

;; Testing of connection (ought to do object mocks)
