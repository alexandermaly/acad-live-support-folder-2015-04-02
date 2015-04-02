(defun c:mirmultx ()
  (princ "\nSelect objects: ")
  (setq ssel (ssget))
  ;(setq scf (getreal "\nEnter Scale Factor: "))
  (setq len  (sslength ssel)
 count 0
  )
  (while (< count len)
	(setq name (ssname ssel count)
   ent  (entget name)
   blk  (cdr (assoc 0 ent))
	)
	(if (or
   (cond
	 (= blk "CIRCLE")
	 (= blk "MTEXT")
   )
 )
	  (scale)
	  (progn
 (setq count (+ count 1))
	  )
	) ;if
	(setq count (+ count 1))
	(princ)
  ) ;while
  (princ)
) ;defun
(princ "enter MIS for Multiple Object Mirror")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun scale ()
  (setq ipt (cdr (assoc 10 ent)))
  (princ ipt)(princ "\n")
  (setq x1 (car ipt))
  (princ "point 1 \n")
  (setq x2 (+ (car ipt) 100.00))
  (princ "point 2 \n")
  (setq y1 (cadr ipt))
  (setq p1 (strcat (rtos x1) ", " (rtos y1)))
  (princ p1)(princ "\n")
  (setq p2 (strcat (rtos x2) ", " (rtos y1)))
  (princ p2)(princ "\n")
  (command "mirror" name "" p1 p2 "Y")
)