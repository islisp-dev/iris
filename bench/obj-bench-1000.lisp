(defclass A () ())
(for ((i 0 (+ i 1)))
     ((> i 1000))
     (create (class A)))
