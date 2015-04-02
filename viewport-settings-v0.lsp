(defun c:viewport-settings-v0 ()
	(command "_layer" "thaw" "*_Viewport_Crop*" "")
	(command "_layer" "on" "*_Viewport_Crop*" "")
)