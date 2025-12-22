.module Sys_render_S

.include "cpctelera.h.s"
.include "cpctelera_functions.h.s"
.include "../man/man_entity.h.s"
.include "../assets/assets.h.s"

;;
;; PUBLIC FUNCTIONS
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Init render system
;; INPUTS: -
;; OUTPUTS: -
;; CHANGED: -
;; WARNING: -
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
sysrender_init::
	ld c, #0							;; Video Mode = 0 (16 colors in palette)
	call cpct_setVideoMode_asm
	cpctm_setBorder_asm HW_BLACK		;; Border color
	
	ld hl, #_palette					;; Our color palette
	ld de, #16							;; Palette number of colors
	call cpct_setPalette_asm

	ret

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Update render frame.
;; INPUTS: -
;; OUTPUTS: -
;; CHANGED: AF, HL, BC, IX, ?
;; WARNING: -
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
sysrender_update::
	ld a, #manentity_cmp_render_mask
	ld hl, #rendersys_draw_entity
	jp manentity_forall_matching

	;ret

;;
;; PRIVATE FUNCTIONS
;;

;;;;;;;;;;;;;;;;;;;;;
;;
;; Draw/erase IX entity.
;; INPUTS: IX (entity to draw/erase)
;; OUTPUTS: -
;; CHANGED: H, BC, DE, AF
;; WARNING: -
;;
;;;;;;;;;;;;;;;;;;;;;
rendersys_draw_entity:
	xor a
	cp manentity_last_videol_ptr(ix)
	jr nz, rendersys_draw_entity_continue
	cp manentity_last_videoh_ptr(ix)
	jr z, rendersys_draw_entity_get_screen_ptr

	rendersys_draw_entity_continue:
	 ;; Erase

	 ;; Only a byte
	 ;ld h, manentity_last_videoh_ptr(ix)
	 ;ld l, manentity_last_videol_ptr(ix)
	 ;ld a, (hl)
	 ;xor manentity_color(ix)
	 ;ld (hl), a

	 ;; A sprite
	 ld h, manentity_hprevsprite_ptr(ix)
	 ld l, manentity_lprevsprite_ptr(ix)
	 ld d, manentity_last_videoh_ptr(ix)
	 ld e, manentity_last_videol_ptr(ix)
	 ld b, manentity_w(ix)
	 ld c, manentity_h(ix)
	call rendersys_draw_XOR_entity

	 ld a, #manentity_cmp_alive_mask
	 and manentity_cmps(ix)
	jr nz, rendersys_draw_entity_calculate_ptr
	jp manentity_set_to_no_renderizable

	rendersys_draw_entity_calculate_ptr:
	 ld h, manentity_last_videoh_ptr(ix) 
	 ld l, manentity_last_videol_ptr(ix) 
	 ld a, manentity_vx(ix)

	 ;; Substract current vx to current star position (to draw it in new position of screen)
	 ;; WARNING: uncomment it
	 ;add l
	 ;ld l, a
	 ;ld a, h
	 ;ccf
	 ;sbc #0
	 ;ld h, a

	jp rendersys_draw_entity_draw_new_position

	rendersys_draw_entity_get_screen_ptr:
	 ld de, #0xc000								;; Start of the video memory where calculations are made 
	 ld c, manentity_x(ix)						;; C = X
	 ld b, manentity_y(ix)						;; B = Y  
	call cpct_getScreenPtr_asm					;; HL = screen pointer where draw IX entity

	rendersys_draw_entity_draw_new_position:
	 ld manentity_last_videoh_ptr(ix), h
	 ld manentity_last_videol_ptr(ix), l

	 ;; Draw
	 ;; Only a byte
	 ;ld a, (hl)
	 ;xor manentity_color(ix)
	 ;ld (hl), a

	 ;; A sprite
	 ld d, h
	 ld e, l
	 ld h, manentity_hsprite_ptr(ix)
	 ld l, manentity_lsprite_ptr(ix)
	 ld b, manentity_w(ix)
	 ld c, manentity_h(ix)
	jp rendersys_draw_XOR_entity 

	;ret

;;;;;;;;;;;;;;;;;;;;;
;;
;; Draw or erase entity with XOR method
;; INPUTS: DE (destination addres video memory to draw), HL (sprite content address memory), B (entity width), C (entity height)
;; OUTPUTS: -
;; CHANGED: HL, BC, DE, AF
;; WARNING: -
;;
;;;;;;;;;;;;;;;;;;;;;
rendersys_draw_XOR_entity:
	push ix 									;; [2 | 5] Save IX register
	ld__ixl_b									;; [2 | 2] IXL = B (width)

	rendersys_XOR_entity_main_loop:
	 push de									;; [1 | 4] Save DE (address memory to draw)

	rendersys_XOR_entity_row_loop:
	 ld a, (de) 								;; [1 | 2] If is "erase mode", content of DE address memory = 0x00, else, the previous color of entity
	 xor (hl)									;; [1 | 2] A ^ Sprite byte color, if A == back color: result = sprite byte color, else, A == sprite color, so res = back color
	 inc hl										;; [1 | 2] Next sprite byte color
	 ld (de), a 								;; [1 | 2] Update byte color of DE addres memory
	 inc de 									;; [1 | 2] Next byte of video memory address to draw

 	djnz rendersys_XOR_entity_row_loop			;; [3/4 | 2] If B != 0, blending operation on the next byte 
 	 pop de										;; [1 | 3] DE = destination address video memory to draw
 	 dec c 										;; [1 | 1] 
 	jr z, rendersys_draw_XOR_entity_end			;; [2 | 3/2]

 	 ld__b_ixl									;; [2 | 2] B = entity width
 	 ld a, #0x08								;; [2 | 2] We add 0x0800 to destination pointer to get next byte line of current character
 	 add d 										;; [1 | 1] A = A + D (high byte of destination addres video memory to draw). Low byte is not necessary

 	 ld d, a 									;; [1 | 1]
 	 and #0x38 									;; [2 | 2] / If any bit from 13, 12, 11 (weights) is not 0: we are still inside video memory boundaries, so proceed 
 	jr nz, rendersys_XOR_entity_main_loop		;; [2 | 3/2] \ with next bytes line

 	 ;; Every 8 lines (1 character), we cross the 16K video memory boundaries, 8 * 2048k (0x800), and have to reposition destination pointer. 
 	 ;; That means our nextline is 16K-0x50 bytes back (to c0000) which is the same as advancing 48K(0xc0000)+0x50 = 0xC050 bytes, as memory is 64K 
   	 ;; and our 16bit pointers cycle over it.
 	 ld a, #0x50 								;; [2 | 2] / Add low byte to E
 	 add e										;; [1 | 1] \
 	 ld e, a 									;; [1 | 1]
 	 ld a, #0xc0								;; [2 | 2] / Add high byte to D and the carry flag
 	 adc d										;; [1 | 1] \
 	 ld d, a 									;; [1 | 1]
 	jp rendersys_XOR_entity_main_loop			;; [3 | 3]

 	rendersys_draw_XOR_entity_end:
 	 pop ix 									;; [2 | 5] IX = current entity

	ret											;; [1 | 3]

