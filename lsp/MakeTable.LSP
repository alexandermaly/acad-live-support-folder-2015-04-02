;;; CADALYST 03/08  www.cadalyst.com/code 
;;; Tip ALSPSOL0308:  MakeTable.LSP	Convert Old Table Data to New Table Format 	
;;; (c) 2008 Tony Hotchkiss and Cadalyst
;;;
;;; Purpose:  	A program to convert lines and text  ;;;
;;; into a table object. The orignal text may be single line ;;;
;;; or Mtext.  Lines may be polylines and may overlap        ;;;
;;; Program by Tony Hotchkiss November 2007                  ;;;

(defun err (s)
  (if (= s "Function cancelled")
    (princ (strcat "\MAKETABLE cancelled - "
		   "thank you for using MAKETABLE"
	   ) ;_ end of strcat
    ) ;_ end of princ
    (progn (princ "\MAKETABLE Error: ")
	   (princ s)
	   (terpri)
    ) ;_ end of progn
  ) ;_ end of if
  (resetting)
  (princ)
) ;_ end of err
(defun setv (systvar newval / x)
  (setq x (read (strcat systvar "1")))
  (set x (getvar systvar))
  (setvar systvar newval)
) ;_ end of setv
(defun setting ()
  (setq oerr *error*)
  (setq *error* err)
  (setv "BLIPMODE" 0)
  (setv "CMDECHO" 0)
) ;_ end of setting
(defun rsetv (systvar)
  (setq x (read (strcat systvar "1")))
  (setvar systvar (eval x))
) ;_ end of rsetv
(defun resetting ()
  (rsetv "BLIPMODE")
  (rsetv "CMDECHO")
  (setq *error* oerr)
) ;_ end of resetting

(defun GetOldTable ()
  ;(vl-load-com)
  (setq	*thisdrawing* (vla-get-activedocument
			(vlax-get-acad-object)
		      ) ;_ end of vla-get-activedocument
	*modelspace*  (vla-get-ModelSpace *thisdrawing*)
	*utility*     (vla-get-Utility *thisdrawing*)
  ) ;_ end of setq
  (setq	ssets	  (vla-get-selectionsets *thisdrawing*)
	ssetcount (vla-get-count ssets)
  ) ;_ end of setq
  (if (> ssetcount 0)
    (repeat ssetcount
      (vla-delete (vla-item ssets 0))
    ) ;_ end of repeat
  ) ;_ end of if
  (setq	p1 (vla-GetPoint
	     *utility*
	     nil
	     (vlax-make-variant
	       "\nSelect old table with a window: Upper left point: "
	     ) ;_ end of vlax-make-variant
	   ) ;_ end of vla-getpoint
  ) ;_ end of setq
  (setq	p2 (vla-GetCorner
	     *utility*
	     p1
	     (vlax-make-variant "\nLower right point: ")
	   ) ;_ end of vla-getcorner
  ) ;_ end of setq
  (setq ssobjprelim (vla-add ssets "selection1"))
  (vla-select ssobjprelim acSelectionSetCrossing p1 p2)
  (setq	count  (vla-get-count ssobjprelim)
	i      (- 1)
	lwplst nil
  ) ;_ end of setq
  (repeat count
    (setq obj (vla-item ssobjprelim (setq i (1+ i))))
    (if	(= (vla-get-objectname obj) "AcDbPolyline")
      (progn
	(setq exlwp (vla-explode obj))
	(vla-delete obj)
      ) ;_ end of progn
    ) ;_ end of if
  ) ;_ end of repeat
  (vla-regen *thisdrawing* acAllViewports)
  (setq ssobj (vla-add ssets "selection2"))
  (vla-select ssobj acSelectionSetCrossing p1 p2)
  (MakeTable ssobj)
) ;_ end of GetOldTable

(defun MakeTable (ss1)
  (setq	count	  (vla-get-count ss1)
	lineslist (get-lines ss1 count)
	txtlist  (get-text ss1 count)
	textlist (car txtlist)
	mtextlist (cadr txtlist)
	rows-cols (get-rowscols lineslist)
	horlines  (car rows-cols)
	rowlines  (sort-lines horlines "X")
	vertlines (cadr rows-cols)
	collines  (sort-lines vertlines "Y")
	tlist nil
	mtlist nil
  ) ;_ end of setq
  (setq	rowlines2 (do-dupes rowlines 1)
	collines2 (do-dupes collines 2)
  ) ;_ end of setq
  (setq	rowlines3 (vl-sort
		   rowlines2
		   (function (lambda (e1 e2)
			       (> (cadar e1) (cadar e2)) ; descending in Y
			     ) ;_ end of lambda
		   ) ;_ end of function
		 ) ;_ end of vl-sort
  ) ;_ end of setq
  (setq	collines3 (vl-sort
		   collines2
		   (function (lambda (e1 e2)
			       (< (caar e1) (caar e2)) ; ascending in X
			     ) ;_ end of lambda
		   ) ;_ end of function
		 ) ;_ end of vl-sort
  ) ;_ end of setq
  (if textlist
    (setq tlist (get-txtmtxt textlist))
    (setq tlist (get-txtmtxt mtextlist))
  ) ;_ end of if
  (setq	rows	  (- (length rowlines3) 1)
	cols	  (- (length collines3) 1)
	RowHeight (distance (car (nth 1 rowlines3)) (car (nth 2 rowlines3)))
	ColWidth  (distance (car (nth 0 collines3)) (car (nth 1 collines3)))
  ) ;_ end of setq
  (setq	tableobj (vla-AddTable
		   *modelspace*
		   (vlax-3D-point (caar collines3))
		   rows
		   cols
		   RowHeight
		   ColWidth
		 ) ;_ end of vla-AddTable
  ) ;_ end of setq
  (vla-put-RegenerateTableSuppressed tableobj :vlax-True)
  (vla-UnmergeCells tableobj 0 0 0 (- cols 1))
  (setq lyr (vla-get-Layer  (nth 0 horlines)))
  (vla-put-layer tableobj lyr)
  (populate-table tableobj tlist rowlines3 collines3)
  (if (> cols 6)
    (progn
      (vla-MergeCells tableobj 0 0 0 5)
      (vla-MergeCells tableobj 0 0 6 7)
    ) ;_ end of progn
    (vla-MergeCells tableobj 0 0 0 (- cols 1))
  ) ;_ end of if
  (vla-put-RegenerateTableSuppressed tableobj :vlax-False)
  (vla-erase ss1)
) ;_ end of MakeTable

(defun get-lines (ss num)
  (setq	i (- 1)
	linelst	nil
  ) ;_ end of setq
  (repeat num
    (setq obj (vla-item ss (setq i (1+ i))))
    (if	(= (vla-get-objectname obj) "AcDbLine")
      (setq linelst (append linelst (list obj)))
    ) ;_ end of if
  ) ;_ end of repeat
  linelst
) ;_ end of get-lines

(defun get-text (ss num)
  (setq	i (- 1)
	textlst	nil
	mtextlst nil
  ) ;_ end of setq
  (repeat num
    (setq obj (vla-item ss (setq i (1+ i))))
    (if	(= (vla-get-objectname obj) "AcDbText")
      (setq textlst (append textlst (list obj)))
    ) ;_ end of if
    (if	(= (vla-get-objectname obj) "AcDbMText")
      (setq mtextlst (append mtextlst (list obj)))
    ) ;_ end of if
  ) ;_ end of repeat
  (list textlst mtextlst)
) ;_ end of get-lines

(defun get-rowscols (llist)
  (setq	j (- 1)
	vertlist nil
	horlist	nil
	junklist nil
	junklist2 nil
	fuzz 0.1
  ) ;_ end of setq
  (repeat (length llist)
    (setq obj (nth (setq j (1+ j)) llist))
    (setq xstart (vlax-safearray-get-element
		   (vlax-variant-value (vla-get-StartPoint obj))
		   0
		 ) ;_ end of vlax-safearray-get-element
	  xend	 (vlax-safearray-get-element
		   (vlax-variant-value (vla-get-EndPoint obj))
		   0
		 ) ;_ end of vlax-safearray-get-element
	  ystart	 (vlax-safearray-get-element
		   (vlax-variant-value (vla-get-StartPoint obj))
		   1
		 ) ;_ end of vlax-safearray-get-element
	  yend	 (vlax-safearray-get-element
		   (vlax-variant-value (vla-get-EndPoint obj))
		   1
		 ) ;_ end of vlax-safearray-get-element
    ) ;_ end of setq
    (cond ((and (equal xstart xend fuzz) (equal ystart yend fuzz))
	   (setq junklist (append junklist (list obj)))
	  )
	  ((and (equal xstart xend fuzz) (not (equal ystart yend fuzz)))
	   (setq vertlist (append vertlist (list obj)))
	  )
	  ((and (not (equal xstart xend fuzz)) (equal ystart yend fuzz))
	   (setq horlist (append horlist (list obj)))
	  )
	  (t (setq junklist2 (append junklist2 (list obj))))
    ) ;_ end of cond
  ) ;_ end of repeat
  (list horlist vertlist junklist)
) ;_ end of get-rowscols

(defun get-txtmtxt (tlist)
  (setq	j (- 1)
	textlist nil
  ) ;_ end of setq
  (repeat (length tlist)
    (setq obj (nth (setq j (1+ j)) tlist))
    (setq inspt	    (vlax-variant-value (vla-get-InsertionPoint obj))
	  txtstr    (vla-get-TextString obj)
	  stylename (vla-get-StyleName obj)
	  txtht	    (vla-get-Height obj)
    ) ;_ end of setq
    (setq textlist
	   (append textlist (list txtstr inspt stylename txtht))
    ) ;_ end of setq
  ) ;_ end of repeat
  (list textlist)
) ;_ end of get-txtmtxt

(defun do-dupes	(rclst rc)
  (setq	item (car rclst)
	i    (- 1)
	len1 nil
	len2 nil
  ) ;_ end of setq
  (while item
    (if	(and len2 (> len1 len2))
      (setq i (- i (- len1 len2)))
    ) ;_ end of if
    (if (< i 0) (setq i (- 1)))
    (setq len1 (length rclst))
    (setq item (nth (setq i (1+ i)) rclst))
    (if	item
      (progn
	(setq rclst (myvl-remove item rclst))
	(setq rclst (append rclst (list item)))
	(setq len2 (length rclst))
      ) ;_ end of progn
    ) ;_ end of if
  ) ;_ end of while
  (setq newlist rclst)
) ;_ end of do-dupes

(defun myvl-remove (item llst)
  (setq j (- 1))
  (repeat (length llst)
    (setq item2 (nth (setq j (1+ j)) llst))
    (if	(equal item item2 fuzz)
      (setq llst (vl-remove item2 llst))
    ) ;_ end of if
  ) ;_ end of repeat
  llst
) ;_ end of myvl-remove

(defun sort-lines (llst *axis*)
  (setq	endpts nil
	plist nil
	j (- 1)
  ) ;_ end of setq
  (repeat (length llst)
    (setq obj (nth (setq j (1+ j)) llst))
    (setq xstart (vlax-safearray-get-element
		   (vlax-variant-value (vla-get-StartPoint obj))
		   0
		 ) ;_ end of vlax-safearray-get-element
	  xend	 (vlax-safearray-get-element
		   (vlax-variant-value (vla-get-EndPoint obj))
		   0
		 ) ;_ end of vlax-safearray-get-element
	  ystart (vlax-safearray-get-element
		   (vlax-variant-value (vla-get-StartPoint obj))
		   1
		 ) ;_ end of vlax-safearray-get-element
	  yend	 (vlax-safearray-get-element
		   (vlax-variant-value (vla-get-EndPoint obj))
		   1
		 ) ;_ end of vlax-safearray-get-element
    ) ;_ end of setq
    (if	(equal xstart xend fuzz)
      (progn
	(if (< ystart yend)
	  (setq	temp ystart
		ystart yend
		yend temp
	  ) ;_ end of setq
	) ;_ end of if
      ) ;_ end of progn
      (progn
	(if (< xstart xend)
	  (setq	temp xstart
		xstart xend
		xend temp
	  ) ;_ end of setq
	) ;_ end of if
      ) ;_ end of progn
    ) ;_ end of if
    (setq p10 (list xstart ystart))
    (setq p11 (list xend yend))
    (setq endpts (list p10 p11))
    (setq plist (append plist (list endpts)))
  ) ;_ end of repeat
  (if (= *axis* "X")
    (progn
      (setq orderlist (vl-sort
			plist
			(function (lambda (e1 e2)
				    (< (cadar e1) (cadar e2))
				  ) ;_ end of lambda
			) ;_ end of function
		      ) ;_ end of vl-sort
      ) ;_ end of setq
    ) ;_ end of progn
    (progn
      (setq orderlist
	     (vl-sort plist
		      (function	(lambda	(e1 e2)
				  (< (caar e1) (caar e2))
				) ;_ end of lambda
		      ) ;_ end of function
	     ) ;_ end of vl-sort
      ) ;_ end of setq
    ) ;_ end of progn
  ) ;_ end of if
) ;_ end of sort-lines

(defun populate-table (tobj tlist rowlines collines)
  (setq	i    (- 1)
	data (car tlist)
  ) ;_ end of setq
  (repeat (/ (length data) 4)
    (setq txtstr    (car data)
	  inspt	    (vlax-safearray->list (cadr data))
	  textstyle (caddr data)
	  txtht	    (cadddr data)
	  rowcol    (get-rowcol inspt rowlines collines)
	  row	    (car rowcol)
	  col	    (cadr rowcol)
	  rowheight (caddr rowcol)
	  colwidth  (cadddr rowcol)
    ) ;_ end of setq
    (if	(= txtstr "DESCRIPTION")
      (setq colleft col)
    ) ;_ end of if
    (if	(= row 0)
      (progn
	(if (< col 5)
	  (progn
	    (vla-SetText tobj 0 0 txtstr)
	    (vla-SetCellTextStyle tobj 0 0 textstyle)
	    (vla-SetCellTextHeight tobj 0 0 txtht)
	  ) ;_ end of progn
	  (progn
	    (if	(> (length collines) 7)
	      (progn
		(vla-SetText tobj 0 6 "CALCULATED CABLE DISTANCE")
		(vla-SetCellTextStyle tobj 0 6 textstyle)
		(vla-SetCellTextHeight tobj 0 6 txtht)
	      ) ;_ end of progn
	    ) ;_ end of if
	  ) ;_ end of progn
	) ;_ end of if
      ) ;_ end of progn
      (progn
	(vla-SetText tobj row col txtstr)
      ) ;_ end of progn
    ) ;_ end of if
    (vla-SetCellTextHeight tobj row col txtht)
    (vla-SetColumnWidth tobj col colwidth)
    (vla-SetRowHeight tobj row rowheight)
    (vla-SetCellTextStyle tobj row col textstyle)
    (repeat 4
      (setq data (cdr data))
    ) ;_ end of repeat
  ) ;_ end of repeat
  (setq j 1)
  (repeat (- (length rowlines) 3)
      (vla-SetCellAlignment tobj (setq j (1+ j)) colleft acMiddleLeft)
  ) ;_ end of repeat
) ;_ end of populate-table

(defun get-rowcol (ipt rowlines collines)
  (setq	j (- 1)
	k (- 1)
	rowlin1	nil
	rowlin2	nil
	row nil
  ) ;_ end of setq
  (repeat (length rowlines)
    (setq rowlin1 (nth (setq j (1+ j)) rowlines)
	  rowlin2 (nth (1+ j) rowlines)
    ) ;_ end of setq
    (if	(and (> (cadar rowlin1) (cadr ipt))
	     (< (cadar rowlin2) (cadr ipt))
	) ;_ end of and
      (progn
	(setq row j)
	(setq rht (- (cadar rowlin1) (cadar rowlin2)))
      ) ;_ end of progn
    ) ;_ end of if
  ) ;_ end of repeat
  (repeat (length collines)
    (setq collin1 (nth (setq k (1+ k)) collines)
	  collin2 (nth (1+ k) collines)
    ) ;_ end of setq
    (if	(and (< (caar collin1) (car ipt))
	     (> (caar collin2) (car ipt))
	) ;_ end of and
      (progn
	(setq col k)
	(setq cwid (- (caar collin2) (caar collin1)))
      ) ;_ end of progn
    ) ;_ end of if
  ) ;_ end of repeat
  (list row col rht cwid)
) ;_ end of get-rowcol

(defun c:mtb ()
  (setting)
  (GetOldTable)
  (resetting)
  (princ)
) ;_ end of c:tse
(prompt "\nMAKETABLE by Tony Hotchkiss, enter MTB to start: ")
(princ)
