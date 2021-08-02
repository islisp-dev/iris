(defclass A () ())
(for ((i 0 (+ i 1)))
     ((> i 1))
     (create (class A)))
