
(ql:quickload '(:sup :swank))
(in-package :sup)
(swank:create-server :port 5000 :dont-close t)
(start-server :port 8087)
(start-refresh-threads)


