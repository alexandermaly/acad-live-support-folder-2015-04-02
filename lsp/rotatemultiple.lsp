; Rotate Multiple
;* Rotates many entities around their respective basepoints
;* allows selection by AUTOCAD selection sets or SSX.
;* Written by David Husch, January 1991

(defun c:rotmult ()
  (prompt "Select Entities to Rotate, <ENTER> for SSX.")
  (setq ss (ssget))
  (if (not ss) (setq ss (ssx)))
  (setq num (sslength ss))
  (setq x 0)
  (if ss 
  	(if (setq ang (getreal "Enter Rotation Angle: "))
	  	(repeat num
		  	(setq ename (ssname ss x))
		    (setq elist (entget ename))
			(setq pnt (cdr(assoc 10 elist)))
			(command "Rotate" ename "" pnt ang)
 		    (setq x (1+ x))
	    	)
	  	)
    )
  )
