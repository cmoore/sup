
(ql:quickload '(:usocket :slynk :sup))
(slynk:create-server :port 5000 :dont-close t)
(in-package :sup)
(start)
