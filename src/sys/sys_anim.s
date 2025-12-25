;;
;; ANIM SYSTEM
;;
.module Anim_system_S

.include "../man/man_animcontrol.h.s"
.include "../man/man_entity.h.s"

;;;;;;;;;;;;;;;;;;;;;
;;
;; Change current sprite with the following one of position (depends on vx value [-1, -3]) in _star_animation, update prev sprite with current one and 
;; current ptr animation.
;; INPUTS: IX (anim ptr entity)
;; OUTPUTS: HL (updated sprite ptr of anim)
;; CHANGED: AF, HL, B
;; WARNING:
;;	- Component vx must be greater than 0.
;;
;;;;;;;;;;;;;;;;;;;;;
animsys_change_sprite:
	;; Update prev sprite ptr with current one (to "erase" sprite in render system)
	ld a, manentity_lsprite_ptr(ix)
	ld manentity_lprevsprite_ptr(ix), a
	ld a, manentity_hsprite_ptr(ix)
	ld manentity_hprevsprite_ptr(ix), a

	;; Change current sprite
	ld h, manentity_hanim_ptr(ix)			;; / HL = current animation ptr
	ld l, manentity_lanim_ptr(ix) 			;; \
	xor a
	ld b, manentity_vx(ix) 					;; B = vx (num of jumps of 2 bytes to reach next anim ptr)
	sub b
	ld b, a

	animsys_change_sprite_update:
	 ld a, #2
	 add l 									;; / 
	 ld l, a 								;; | Move ptr to animation to next animation (HL += 2). If HL != 0x0000: H will be update, else: move 
	 ld a, h 								;; | ptr to first step of _star_animation
	 adc #0 								;; |
	 ld h, a 								;; \

	 ld a, (hl)
	 inc hl
	 or (hl)
	jr z, animsys_change_sprite_reset_animation
	 dec hl
	animsys_change_sprite_update_dec_counter_B:
	djnz animsys_change_sprite_update

	 ;; Update sprite with new animation and anim ptr
	 ld manentity_lanim_ptr(ix), l
	 ld manentity_hanim_ptr(ix), h
	 ld a, (hl)
	 ld manentity_lsprite_ptr(ix), a
	 inc hl
	 ld a, (hl)
	 ld manentity_hsprite_ptr(ix), a

	ret

	animsys_change_sprite_reset_animation:
	 ;; Before inc hl, HL = high bit of 0x0000
	 inc hl 								;; /
	 ld a, (hl) 							;; | HL = *HL (HL points to first step of _star_animation)
	 inc hl 								;; |
	 ld h, (hl) 							;; |
	 ld l, a 								;; \

	jp animsys_change_sprite_update_dec_counter_B

;;;;;;;;;;;;;;;;;;;;;
;;
;; Iterate component anim array and call animsys_change_sprite function for each ptr entity
;; ENTRADAS: -
;; SALIDAS: -
;; ALTERADOS: AF, HL, IX, BC, D, ?
;;
;;;;;;;;;;;;;;;;;;;;;
animsys_update::
	ld a, #manentity_cmp_star_mask
	ld hl, #animsys_change_sprite
	jp manentity_forall_matching