
(ql:quickload '(:usocket :slynk))
(slynk:create-server :port 4008 :dont-close t)
(ql:quickload :sup)
(sup:start)
