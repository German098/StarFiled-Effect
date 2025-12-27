.module Sys_physics_S

.include "../man/man_entity.h.s"
.include "../man/man_components.h.s"

;;
;; PUBLIC FUNCTIONS
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Update physics frame.
;; INPUTS: IX (entity)
;; OUTPUTS: -
;; CHANGED: AF, HL, BC, IX, ?
;; WARNING: -
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
sysphysics_update::
	ld a, #manentity_cmp_alive_mask
	ld de, #sysphysics_move
	call mancomponents_get_array_ptr
	jp manentity_forall_ptr

	;ret

;;
;; PRIVATE FUNCTIONS
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Add Vx to current X position of IX entity
;; INPUTS: IX (entity)
;; OUTPUTS: -
;; CHANGED: AF
;; WARNING: -
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
sysphysics_move:
	ld a, manentity_vx(ix)
	add manentity_x(ix)
	ld manentity_x(ix), a 

	ret