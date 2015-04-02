(defun c:mos ()
  (princ "\nSelect circles to Scale: ")
  (setq ssel (ssget))
  (setq scf (getreal "\nEnter Scale Factor: "))
  (setq len   (sslength ssel)
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
(princ "enter MOS for Multiple Object Scale")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun scale ()
  (setq ipt (cdr (assoc 10 ent)))
  (command "scale" name "" ipt scf)
)