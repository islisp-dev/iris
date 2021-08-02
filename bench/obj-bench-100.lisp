(defclass A () ())
(for ((i 0 (+ i 1)))
     ((> i 100))
     (create (class A)))
