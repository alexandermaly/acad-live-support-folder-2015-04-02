;doesnt work
(setq p1 "/Volumes/Architecture/13600 BENTLY RESIDENCE/DRAWINGS/CURRENT/2D AUTOCAD/13600 PLOT SET/13600_A1-10_PLAN-FLR-1-dwg/13600_A1-10_PLAN-FLR-1.dwg")
(setq p2 "/Volumes/Architecture/13600 BENTLY RESIDENCE/DRAWINGS/CURRENT/2D AUTOCAD/13600 PLOT SET/13600_A1-10_PLAN-FLR-1-dwg/template 13600-A0-00-XXX-24X36 copy.dwg")
;(setvar "cmdecho" 0)
(command "._open" "/Volumes/Architecture/13600 BENTLY RESIDENCE/DRAWINGS/CURRENT/2D AUTOCAD/13600 PLOT SET/13600_A1-10_PLAN-FLR-1-dwg/13600_A1-10_PLAN-FLR-1.dwg")
;(setvar "cmdecho" 1)
(c:zs)
(c:saveclose)
(princ "source test model")
(command "._open" p2)
;(c:zd)
(c:saveclose)
(princ "paste test model")
(command "._open" p1)
(c:rs)
(princ "source test paper")
;(c:saveclose)
(command "._open" p2)
;(c:rd)
(princ "paste test paper")
(c:nonplot_settings)
(c:nonplotOff)
(c:bylall)
(c:stb_settings)
;(c:saveclose)