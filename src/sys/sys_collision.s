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
syscollision_update::
	ld a, #manentity_cmp_alive_mask
	ld de, #syscollision_check_left_boundary
	call mancomponents_get_array_ptr
	jp manentity_forall_ptr

	;ret

;;
;; PRIVATE FUNCTIONS
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; If IX entity is out of the left boundary: set for erase from second buffer.
;; INPUTS: IX (entity)
;; OUTPUTS: -
;; CHANGED: AF, HL, BC, IX, ?
;; WARNING: -
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
syscollision_check_left_boundary::
	ld a, manentity_x(ix) 								;; / Check sign of vx
	bit 7, a 											;; \ 
	jp m, syscollision_check_left_boundary_is_out

	ret

	syscollision_check_left_boundary_is_out:
	 jp manentity_set_for_erase 

	;ret