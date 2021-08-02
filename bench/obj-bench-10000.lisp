(defclass A () ())
(for ((i 0 (+ i 1)))
     ((> i 10000))
     (create (class A)))
