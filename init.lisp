
(ql:quickload '(:sup :swank))
(setf chunga::+output-buffer-size+ 2048)
(in-package :sup)
(swank:create-server :port 5000 :dont-close t)
(start-server :port 8087)
(start-refresh-threads)

