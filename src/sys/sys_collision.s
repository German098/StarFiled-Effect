.module Sys_physics_S

.include "../man/man_entity.h.s"

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
	ld hl, #syscollision_check_left_boundary
	jp manentity_forall_matching

	;ret

;;
;; PRIVATE FUNCTIONS
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; If IX entity is out of the left boundary: set for destruction.
;; INPUTS: IX (entity)
;; OUTPUTS: -
;; CHANGED: AF, HL, BC, IX, ?
;; WARNING: -
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
syscollision_check_left_boundary::
	ld a, manentity_x(ix)
	bit 7, a
	jp m, syscollision_check_left_boundary_is_out

	ret

	syscollision_check_left_boundary_is_out:
	 jp manentity_set_for_destruction 

	;ret