
(DEFGENERIC F
    (X Y Z)
  (:METHOD-COMBINATION NIL)) (DEFCLASS C0 () ())(DEFCLASS C1 () ())(DEFCLASS C
                                                                             (C0
                                                                              C1)
                                                                             ())
(DEFMETHOD F ((X C) (Y C0) (Z C1))
  (FORMAT (STANDARD-OUTPUT) "call by ~A ~A ~A~%" 'C 'C0 'C1)
  (IF (NEXT-METHOD-P)
      (CALL-NEXT-METHOD))) 
(F (CREATE (CLASS C)) (CREATE (CLASS C)) (CREATE (CLASS C))) 