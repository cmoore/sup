
(ql:quickload '(:sup :swank))
(in-package :sup)
(swank:create-server :port 5000 :dont-close t)
(start-server)
(start-refresh-threads)
(start-ws-server)
(start-ws-reader-thread)


;; 253-248-5168

