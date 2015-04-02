(defun C:pLay()                                 ;define a program name
  (if (setq eset(ssget)                         ;use the ssget function to select entities
    (progn                                      ;use progn since there will be more than 1 statement
      (setq cntr 0)                             ;set the cntr to the first item in the set
      (while (< cntr (sslength eset))              ;while cntr is less than the length of the set
        (setq en(ssname eset cntr))                    ;get the entity name of the item indexed with cntr
        (setq enlist(entget en))                       ;get the dxf group codes of the enitity
        (setq layName(cdr(assoc 8 enlist)))      ;get the layer name
                   (princ "\n ")                            ;print "\n " will cause a new line to be printed
        (princ layName)                            ;print the layer name to the command line
        (setq cntr(+ cntr 1))                      ;increment the counter
      )                                            ;close the while statement
    )                                              ;close the progn on the if statement
    (princ "\n Error - No entities selected.") ;print a message on the else statement
  )                                             ;close the if statement
)                                               ;close the program
)