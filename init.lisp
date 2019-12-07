
(ql:quickload '(:usocket :slynk))
(slynk:create-server :port 5000 :dont-close t)
(ql:quickload :sup)
(in-package :sup)
(start-server)
(start-refresh-threads)
(start-ws-server)
(start-ws-reader-thread)


