;;
;; ANIMAITON CONTROL MANAGER
;;
.module Animation_control_manager_S

.include "../assets/assets.h.s"

;; Star animation
_star_animation::
	.dw _sp_star_0 			; Init animation (white star)		
	.dw _sp_star_1
	.dw _sp_star_2
	.dw _sp_star_3
	.dw _sp_star_4
	.dw 0x0000
	.dw #_star_animation