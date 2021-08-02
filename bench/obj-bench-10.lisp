(defclass A () ())
(for ((i 0 (+ i 1)))
     ((> i 10))
     (create (class A)))
