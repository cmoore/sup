
(ql:quickload '(:usocket :swank :sup))
(swank:create-server :port 5000 :dont-close t)
;;(slynk:create-server :port 5000 :dont-close t)
(in-package :sup)
(start)


