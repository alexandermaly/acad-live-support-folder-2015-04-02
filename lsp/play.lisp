(defun C:play()                                 ;define a program name
(if (setq eset(ssget))                         ;use the ssget function to select entities
    (progn                                      ;use progn since there will be more than 1 statement
      (setq cntr 0)                             ;set the cntr to the first item in the set
      (while (< cntr (sslength eset))              ;while cntr is less than the length of the set
        (setq en (ssname eset cntr))                    ;get the entity name of the item indexed with cntr
        (princ " ")(princ en)
        (setq enlist (entget en))                       ;get the dxf group codes of the enitity
        (princ " ")(princ enlist)
        (setq layName (cdr(assoc -1 enlist)))      ;get the layer name
        (princ "\n ")                            ;print "\n " will cause a new line to be printed
        (princ layName)                            ;print the layer name to the command line
        (setq cntr(+ cntr 1))                      ;increment the counter
      )                                            ;close the while statement
    )                                              ;close the progn on the if statement
    (princ "\n Error - No entities selected.") ;print a message on the else statement
  )                                             ;close the if statement
(princ "done")
)                                               ;close the program

(defun C:layer_codes_test()
setq layer_codes (entget (tblobjname "layer" "your_layer_here"))
pst_ename (cdr (assoc 390 layer_codes))
pst_codes (entget (cdr (assoc 330 (entget pst_ename))))
pst_check (cdr (assoc 3 pst_codes))
)

(defun c:get_pstyle ( / pst_ename pst_codes pst_check cutoff pstyle)
(setq pst_ename (cdr (assoc 390 (entget (car (entsel)))))
pst_codes (entget (cdr (assoc 330 (entget pst_ename))))
pst_check (cons 350 pst_ename)
cutoff (- (length pst_codes) (length (member pst_check pst_codes)))
pstyle (nth (1- cutoff) pst_codes)
)
(cdr pstyle)
)

(defun c:pstyle_layout_test()
(SETQ DICTLAY (DICTSEARCH(NAMEDOBJDICT) "ACAD_LAYOUT")) ;This finds the layout dictionary;
(SETQ CNT 1)
(FOREACH LAYOUT DICTLAY
(IF (= (CAR LAYOUT) 3)
(PROGN(IF (= (CDR LAYOUT) "Model")
(SETQ CNTFOUND (- CNT 1)))(SETQ CNT (+ CNT 1))))); This is a Foreach that filters through all the Layout names in the dictionary and when it finds one named "Model" (Which you could change to fit your needs) it sets the CNTFOUND variable.;
(FOREACH LAYOUT DICTLAY(IF (= (CAR LAYOUT) 350)(SETQ VLIST (CONS (LIST (CDR LAYOUT)) VLIST))))
;Here is another ForEach that filters through and creates a list of all the Layout entity names.;
(SETQ VLIST (REVERSE VLIST));Reverses the VLIST entity name list;
(SETQ LAY1 (ENTGET (CAR (NTH CNTFOUND VLIST))));Gets the correct layout entity name;
(SETQ STYLE (CDR (ASSOC '7 LAY1)));Sets the STYLE variable to the plotstyle filename of the selected layout. But why would you choose to do all that when once you have load the visual lisp functions you could enter a line of code like this.;
(SETQ STYLE (VLA-GET-STYLESHEET (VLA-GET-ACTIVELAYOUT (VLA-GET-ACTIVEDOCUMENT (VLAX-GET-ACAD-OBJECT)))));
)