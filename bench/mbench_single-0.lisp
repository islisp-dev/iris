(progn (defgeneric f (x y z) (:method-combination nil))
(defclass C () ())
(defmethod f ((x C) (y C) (z C))  
                 (format (standard-output) "call with ~A ~A ~A~%" 'C 'C 'C))
(f (create (class c)) (create (class c)) (create (class c))))
