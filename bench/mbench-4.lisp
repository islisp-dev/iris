(defgeneric f (x y) (:method-combination nil))
(defclass C00 () ())
(defmethod f ((x C00) (y C00)) nil)
(defclass C01 () ())
(defmethod f ((x C01) (y C01)) nil)
(defclass C0 (C00 C01) ())
(defmethod f ((x C01) (y C00))
                 (format (standard-output) "call with ~A ~A~%" 'C01 'C00)
                 (if (next-method-p) (call-next-method)))
(defmethod f ((x C00) (y C01))
                 (format (standard-output) "call with ~A ~A~%" 'C00 'C01)
                 (f (create (class C01)) (create (class C0))))
(defclass C10 () ())
(defmethod f ((x C10) (y C10)) nil)
(defclass C11 () ())
(defmethod f ((x C11) (y C11)) nil)
(defclass C1 (C10 C11) ())
(defmethod f ((x C11) (y C10))
                 (format (standard-output) "call with ~A ~A~%" 'C11 'C10)
                 (if (next-method-p) (call-next-method)))
(defmethod f ((x C10) (y C11))
                 (format (standard-output) "call with ~A ~A~%" 'C10 'C11)
                 (f (create (class C11)) (create (class C1))))
(defclass C (C0 C1) ())
(defmethod f ((x C1) (y C0))
                 (format (standard-output) "call with ~A ~A~%" 'C1 'C0)
                 (if (next-method-p) (call-next-method)))
(defmethod f ((x C0) (y C1))
                 (format (standard-output) "call with ~A ~A~%" 'C0 'C1)
                 (f (create (class C1)) (create (class C))))
(f (create (class c)) (create (class c)))
