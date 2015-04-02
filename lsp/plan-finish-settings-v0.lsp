;demo plans settings oneoff
(defun c:plan-finish-settings-v0 ()
	(command "_layer" "freeze" "*EQPM*" "")
	(command "_layer" "freeze" "*FURN*" "")
	(command "_layer" "freeze" "*Entourage*" "")
	(command "_layer" "freeze" "*PLAN-LANDSCAPE|A-FLOR-PATT*" "")

	(princ "finish")
);end of defun