;Written By Michael Puckett.
(defun c:tableAssemble (s / d r)
(while (setq d (tblnext s (null d)))
(setq r (cons (cdr (assoc 2 d)) r))
)
)
(defun c:runTableAssemble()
	(c:tableAssemble "layer")
)