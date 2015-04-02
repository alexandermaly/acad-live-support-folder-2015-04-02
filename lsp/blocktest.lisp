; (setq a (tblobjname "BLOCK" "MYBLOCK"))
; (while a
;   (if (setq a (entnext a))
;     (progn
;       (princ "\n Entity = ")
;       (princ (cdr (assoc 0 (entget a))))
;     ) 
;   )
; )
; ;;;
; ;;; returns something like this
; ;;; Entity = CIRCLE
; ;;; Entity = TEXT
; ;;; Entity = ATTDEF
; ;;; Entity = ATTDEF
; ;;;

(setq en (entsel)) ;Get a block's ename through entsel
(setq ed (entget (car en))) ; Get DXF data of en
(if (= (cdr (assoc 0 ed)) "INSERT") ;Check if it's an inserted block reference
  (progn
    (setq blkname (cdr (assoc 2 ed))) ;Get the block's name from the Insert's DXF data
    (setq bn (tblobjname "BLOCK" blkname)) ;Get the ename of the Block Definition
	(while a
	  (if (setq a (entnext a))
	    (progn
	      (princ "\n Entity = ")
	      (princ (cdr (assoc 0 (entget a))))
	    ) 
	  )
	)
  (prompt "\nThis is not a block reference ... nothing to do.")
)