;*
;* AlignAttr.lsp
;*
;* Written by: Steve Kemp
;*
;* This routine will allow the user to align attributes on either Vertically or Horizontally on either the same block or different block
;* in the active drawing.
;*
;* THIS AUTOLISP ROUTINE IS PROVIDED 'AS IS' AND WITH ALL FAULTS.
;* THE OPERATION OF THIS ROUTINE IS NOT GUARANTEED IN ANY WAY TO BE ERROR FREE.
;*
;* To run this routine, type in AATTR at the command prompt.  You will be asked 'Select Attribute to align with'  this will be the 'master'
;* attribute that all other attributes will align with.  You will also be able to switch between Vertical (default) or Horizontal alignment
;* at this time.
;*
;* After selecting the 'master' attribute, you will be prompted 'Select Attributes to align'.  This will allow you to select multiple attribute
;* to align with the 'master' attributes insertion point.
;*
;*
;* NOTE: This routine does not work with 'ALIGN' or 'FIT' justified attributes and the routine will exit if one of these types is selected.
;*
(setvar 'cmdecho 0)
(defun c:AAttr (/ TextJust ParentAttr  ChildAttrList TempAttr TempData TempInsPt TempAdPt NewInsPt NewAdPt BlkEntName AttrOrientation AttrData AttrInsPt AttrAdPt AttrHeight GuideLineLen GuideLineStartPt GuideLineEndPt x )
   (command ".undo" "BE")
   (setq
      CurErr *error*
      *error* AAttrErr  ;* Redefine the error function
      ParentAttr  "Vertical"
      tempAttr T
   )
   (while (or (= ParentAttr "Vertical") (= ParentAttr "Horizontal"))
      (initget "Horizontal Vertical")
      (if (= ParentAttr "Vertical")
         (setq
            ParentAttr (nentsel "\nSelect Attribute to align with (Horizontal/<Vertical>): ") ;* Select Master Attribute
            AttrOrientation "Vertical"
         )
         ;else
         (setq
            ParentAttr (nentsel "\nSelect Attribute to align with (Vertical/<Horizontal>): ") ;* Select Master Attribute
            AttrOrientation "Horizontal"
         )
      )
   )
   (if (/= ParentAttr nil)
      (progn
         (setq
            AttrData (entget (car ParentAttr))
         )
         (if (= (cdr (assoc 0 AttrData)) "ATTRIB")  ;* Make sure entity selected was an attribute
            (progn
               ;*
               ;* If this is Align or Fit justified text, exit since these are non-supported justifications
               ;*
               (if (or (= (GetTextJust AttrData) "Align") (= (GetTextJust AttrData) "Fit"))
                  (progn
                     (alert "Unsupported attribute justification.\nAlign and Fit justifications not supported")
                     ;*
                     ;* Redefine the error function back to original
                     ;*
                     (setq
                        *error* CurErr
                        CurErr nil
                     )
                     (command ".undo" "E")
                     (redraw)
                     (exit)
                  )
               )
               ;*
               ;* Draw highlight box around selected attribute
               ;*
               (DrawBox AttrData)
               (setq
                  AttrInsPt (cdr (assoc 11 AttrData)) ;* This is the user selected insertion point, if middle justified, then this is the middle of the text.  If left justified, then this is all zeros
                  AttrAdPt (cdr (assoc 10 AttrData)) ;* This is the Acad adjusted point (attr starting point, alway the lower left of text even on justifications like Middle).
                  AttrHeight (cdr (assoc 40 AttrData))
                  GuideLineLen (* 25 AttrHeight)
               )
               ;*
               ;* If this is lower left justtified text, then use the AttrAdPt for both AttrInsPt and AttrAdPt because in Left justified text, the
               ;* AttrInsPt value will be all zeros
               ;*
               (if (= (GetTextJust AttrData) "Left")
                  (setq
                     AttrInsPt AttrAdPt
                  )
               )
               ;*
               ;* Draw guide line
               ;*
               (if (= AttrOrientation "Vertical")
                  (progn
                     (setq
                        GuideLineStartPt (list (car AttrInsPt) (+ (cadr AttrInsPt) GuideLineLen))
                        GuideLineEndPt (list (car AttrInsPt) (- (cadr AttrInsPt) GuideLineLen))
                     )
                     (grdraw GuideLineStartPt GuideLineEndPt -1 1)
                  )
                  ;else Horizontal
                  (progn
                     (setq
                        GuideLineStartPt (list (- (car AttrInsPt) GuideLineLen) (cadr AttrInsPt))
                        GuideLineEndPt (list (+ (car AttrInsPt) GuideLineLen) (cadr AttrInsPt))
                     )
                     (grdraw GuideLineStartPt GuideLineEndPt -1 1)
                  )
               )
               ;*
               ;* Select all attributes to align with the parent selected above and add the attribute entname and the block
               ;* entname to a list in format ( (AttrEntName BlkEntName) (AttrEntName BlkEntName) (AttrEntName BlkEntName) )
		(setq ChildAttrList (catchwindow))
;;;               (while (/= TempAttr nil)
;;;                  (setq
;;;                     TempAttr (nentsel "\nSelect Attributes to align: ")
;;;                  )
;;;                  (if (/= TempAttr nil)
;;;                     (progn
;;;                        (setq
;;;                           TempData (entget (car TempAttr))
;;;                        )
;;;                        (if (= (cdr (assoc 0 TempData)) "ATTRIB")  ;* Make sure entity selected was an attribute
;;;                           ;*
;;;                           ;* If this is Align or Fit justified text, skip this attribute since these are non-supported justifications
;;;                           ;*
;;;                           (if (or (= (GetTextJust TempData) "Align") (= (GetTextJust TempData) "Fit"))
;;;                              (alert "Unsupported attribute justification.\nAlign and Fit justifications not supported")
;;;                              ;else
;;;                              (progn
;;;                                 (setq
;;;                                    BlkEntName (ssname (ssget (cadr TempAttr)) 0)
;;;                                    ChildAttrList (cons (list (car TempAttr) BlkEntName) ChildAttrList)
;;;                                 )
;;;                                 ;*
;;;                                 ;* Draw highlight box around selected attribute
;;;                                 ;*
;;;                                 (DrawBox TempData)
;;;                              )
;;;                           )
;;;                        )
;;;                     )
;;;                  )
;;;               )
               ;*
               ;* Step through all the attributes to align with the parent and align them
               ;*

               (foreach x ChildAttrList
                  (setq
                     TempData (entget (car x))
                     TempInsPt (assoc 11 TempData)
                     TempAdPt (assoc 10 TempData)
                     TextJust (GetTextJust TempData)
                  )
                  ;*
                  ;* If Vertical allignment, then make the X value of TempInsPt and TempAdPt match the X value of AttrInsPt and AttrAdPt
                  ;*
                  (if (= AttrOrientation "Vertical")
                     ;*
                     ;* This might seem weird, but if the attribute to align is Left justified, I need to use the AttrInsPt (user selected point)
                     ;* in the calculation of the NewAdPt since the AttrInsPt was the actual user selected point and that is the one I want
                     ;* to align with.  Also I only need to set the NewAdPt since the NewInsPt (dxf 11) is not used when left justified.
                     ;*
                     (if (= TextJust "Left")
                        (setq
                           NewAdPt (cons (car AttrInsPt) (cddr TempAdPt))
                           NewAdPt (cons 10 NewAdPt)
                        )
                        ;else
                        (setq
                           NewInsPt (cons (car AttrInsPt) (cddr TempInsPt))
                           NewInsPt (cons 11 NewInsPt)
                           NewAdPt (cons (car AttrAdPt) (cddr TempAdPt))
                           NewAdPt (cons 10 NewAdPt)
                        )
                     )
                     ;else Horizontal
                     ;*
                     ;* This might seem weird, but if the attribute to align is Left justified, I need to use the AttrInsPt (user selected point)
                     ;* in the calculation of the NewAdPt since the AttrInsPt was the actual user selected point and that is the one I want
                     ;* to align with.  Also I only need to set the NewAdPt since the NewInsPt (dxf 11) is not used when left justified.
                     ;*
                     (if (= TextJust "Left")
                        (setq
                           NewAdPt (cons (cadr TempAdPt) (cdr AttrInsPt))
                           NewAdPt (cons 10 NewAdPt)
                        )
                        ;else
                        (setq
                           NewInsPt (cons (cadr TempInsPt) (cdr AttrInsPt))
                           NewInsPt (cons 11 NewInsPt)
                           NewAdPt (cons (cadr TempAdPt) (cdr AttrAdPt))
                           NewAdPt (cons 10 NewAdPt)
                        )
                     )
                  )
                  ;*
                  ;* Update the alignment of the attribute.  If the attribute is lower left justified, then only update the Acad adjusted point (dxf 10)
                  ;* if any other justification update both the attr insertion point and the Acad adjusted point
                  ;*
                  (if (= TextJust "Left")
                     (setq
                        TempData (subst NewAdPt TempAdPt TempData)
                     )
                     ;else
                     (setq
                        TempData (subst NewInsPt TempInsPt TempData)
                        TempData (subst NewAdPt TempAdPt TempData)
                     )
                  )
                  (entmod TempData)
                  (entupd (cadr x))
               )
            )
         )
      )
   )
   ;*
   ;* Redefine the error function back to original
   ;*
   (setq
      *error* CurErr
      CurErr nil
   )
   (command ".undo" "E")
   (redraw)
   (princ)
)
;*
;* Function to draw highlight box around the selected attribute
;*
(defun DrawBox ( Ent / )
   ;*
   ;* Set the USC to the selected entity so the box gets drawn corectly
   ;*
   (command ".ucs" "Entity" (cdr (assoc -1 Ent)))
   (setq
      GblUcsChg T
      TextBoxPts (textbox Ent)
      LowerLeft (car TextBoxPts)
      UpperRight (cadr TextBoxPts)
      UpperLeft (list (car LowerLeft) (cadr UpperRight) 0.0)
      LowerRight (list (car UpperRight) (cadr LowerLeft) 0.0)
   )
   (grvecs (list -1 UpperLeft UpperRight -1 UpperRight LowerRight -1 LowerRight LowerLeft -1 LowerLeft UpperLeft))
   ;*
   ;* Set the USC back to its previous setting
   ;*
   (command ".ucs" "P")
   (setq
      GblUcsChg nil
   )
)
;*
;* Function to return justification of atribute of it is lower Left, Align of Fit.  These justifications are special cases.
;*
(defun GetTextJust ( EntData / Ret )
   (cond
         ((and (= (cdr (assoc 72 EntData)) 0) (= (cdr (assoc 74 EntData)) 0))
            (setq Ret "Left")
         )
         ((and (= (cdr (assoc 72 EntData)) 3) (= (cdr (assoc 74 EntData)) 0))
            (setq Ret "Align")
         )
         ((and (= (cdr (assoc 72 EntData)) 5) (= (cdr (assoc 74 EntData)) 0))
            (setq Ret "Fit")
         )
         (setq Ret nil)
   )
   (setq Ret Ret)
)
;*
;* My Error handler
;*
(defun AAttrErr (msg)
   (redraw)
   ;*
   ;* If an error happened in while the UCS was changed to draw the box around the attribute, set the UCS back to previou setting
   ;*
   (if GblUcsChg
      (progn
         (command ".ucs" "P")
         (setq
            GblUcsChg nil
         )
      )
   )
   ;*
   ;* Redefine the error function back to original
   ;*
   (setq
      *error* CurErr
      CurErr nil
   )
   (command ".undo" "E")
   (princ "\n")
   (princ msg)
)

(defun catchwindow ( / Inside-p _HiLow of blks atlist )(vl-load-com)
;;;	 pbe Oct 17 2012	;;;  
(defun Inside-p (a b c)
;;; http://mathforum.org/library/drmath/view/54386.html ;;;
;;; (.5)(x1*y2 - y1*x2 -x0*y2 + y0*x2 + x0*y1 - y0*x1)	;;;
	 (* (- (+ (+ (- (- (* (car b) (cadr c)) (* (cadr b) (car c)))
		     (* (car a) (cadr c))
		  )  (* (cadr a) (car c))
	       ) (* (car a) (cadr b))
	    ) (* (cadr a) (car b))
	 ) 0.5)
  )
(defun _HiLow  (lev lev2 lst)
      (list (apply lev (mapcar 'car lst))
            (apply lev2 (mapcar 'cadr lst))
            ))     
(setq atlist nil)  
  (while (progn (princ "\nSelect Objects: ")
    (if (and (setq fc (getpoint))
		 (setq oc (getcorner  fc " Specify opposite corner:"))
		 (setq blks (ssget "C" fc oc '((0 . "INSERT")(66 . 1)))))
      	(progn
	  (setq pts (list (_hilow 'min 'min (setq lst (list fc oc)))
		      (_hilow 'max 'min lst)(_hilow 'max 'max lst)
		      (_hilow 'min 'max lst)))
	  	(repeat (sslength blks)
		  	 (setq at_ (entnext (setq prnt (ssname blks 0))))
			     (while (not (eq (setq etyp (cdr (assoc 0 (setq e (entget at_))))) "SEQEND"))
			        (if (eq etyp "ATTRIB") (progn
				 (setq pt (cdr (assoc 10 e)))	
				 (if (not (vl-some '(lambda (k) (minusp k))
					   (mapcar '(lambda (j)
						      (Inside-p (car j) (cadr j) pt))
						   (mapcar '(lambda (q w)
							      (list q w) ) pts
							   (append (cdr pts) (list (car pts)))
							   ))))
					  (setq atlist (cons (list at_ prnt) atlist)
						box (DrawBox  (entget at_)) ))
				 	)
				  ) 
			       (setq at_ (entnext at_))
			       )
		  (ssdel  prnt blks)
		  ))))
    		)
		atlist
  	)

(princ "\nTo run the command, type in: AATTR")