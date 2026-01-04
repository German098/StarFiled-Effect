.module Sys_render_S

.include "cpctelera.h.s"
.include "cpctelera_functions.h.s"
.include "../man/man_entity.h.s"
.include "../man/man_components.h.s"
.include "../assets/assets.h.s"

;;
;; PRIVATE VARIABLES
;;
sysrender_front_buffer:
	.db 0xc0
sysrender_back_buffer:
	.db 0x80

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

	call sysrender_clear_back_buffer 	;; Set bytes of back buffer to 0x00

	ret

;;;;;;;;;;;;;;;;;;;;;
;;
;; Switch back and front buffers (sysrender_front_buffer and sysrender_back_buffer variables) and value of register R12 from CRTC.
;; This register is the responsible for holding the 6 MSb (most significant bits) of the place where Video Memory starts (its page).
;; INPUTS: -
;; OUTPUTS: -
;; CHANGED: AF, BC, HL
;; WARNING: -
;;
;;;;;;;;;;;;;;;;;;;;;
sysrender_switch_buffers::
	ld a, (sysrender_back_buffer)
	ld b, a
	ld a, (sysrender_front_buffer)
	ld (sysrender_back_buffer), a
	ld a, b
	ld (sysrender_front_buffer), a

	srl b
	srl b
	ld l, b 								;; L = new starting page for video memory (6 significatns bits of byte)
	call cpct_setVideoMemoryPage_asm

	ld a, #manentity_cmp_render_mask
	ld de, #manentity_swap_back_front_ptrs
	call mancomponents_get_array_ptr
	jp manentity_forall_ptr

	;ret

;;
;; PRIVATE FUNCTIONS
;;

;;;;;;;;;;;;;;;;;;;;;
;;
;; Set all bytes of current back buffer = 0x00
;; INPUTS: -
;; OUTPUTS: -
;; CHANGED: HL, DE, BC
;; WARNING: -
;;
;;;;;;;;;;;;;;;;;;;;;
sysrender_clear_back_buffer:
	;; cpct_memset_f64
	ld a, (sysrender_back_buffer)
	ld h, a
	ld l, #0
	ld d, h
	ld e, #1
	ld (hl), #0 
	ld bc, #0x4000 - 1

	ldir

	ret

;;;;;;;;;;;;;;;;;;;;;
;;
;; Draw/erase IX entity.
;; INPUTS: IX (entity to draw/erase)
;; OUTPUTS: -
;; CHANGED: H, BC, DE, AF
;; WARNING: -
;;
;;;;;;;;;;;;;;;;;;;;;
sysrender_draw_entity:
	ld a, #manentity_cmp_erase_render_mask
	and manentity_cmps(ix)
	jr z, sysrender_draw_entity_init

    ;; Erase sprite of back buffer
	ld d, manentity_last_bvideoh_ptr(ix)
	ld e, manentity_last_bvideol_ptr(ix)
	ld h, manentity_hprevspriteb_ptr(ix)
	ld l, manentity_lprevspriteb_ptr(ix)
	ld b, manentity_w(ix)
	ld c, manentity_h(ix)
	call sysrender_draw_XOR_entity

	jp manentity_set_for_destruction

	sysrender_draw_entity_init: 
	 xor a
	 cp manentity_last_bvideol_ptr(ix)
	jr nz, sysrender_draw_entity_continue
	 cp manentity_last_bvideoh_ptr(ix)
	jr z, sysrender_draw_entity_get_screen_ptr

	sysrender_draw_entity_continue:
	 ;; Erase a sprite of front buffer
	 ld d, manentity_last_fvideoh_ptr(ix)
	 ld e, manentity_last_fvideol_ptr(ix)
	 ld a, d
	 or e
	jr z, sysrender_draw_entity_check_entity_cmps 
	 ld h, manentity_hprevspriteb_ptr(ix)
	 ld l, manentity_lprevspriteb_ptr(ix)
	 ld b, manentity_w(ix)
	 ld c, manentity_h(ix)
	call sysrender_draw_XOR_entity

	sysrender_draw_entity_check_entity_cmps:
	 ld a, #manentity_cmp_alive_mask
	 and manentity_cmps(ix)
	jr nz, sysrender_draw_entity_calculate_ptr

	jp manentity_set_to_no_renderizable

	sysrender_draw_entity_calculate_ptr:
	 ;; Current value of manentity_last_video_ptr = ptr video memory in front buffer, so calculate proper position in back buffer
	 ld a, (sysrender_front_buffer)
	 ld b, a
	 ld a, (sysrender_back_buffer)
	 cp b
	 ld a, manentity_last_bvideoh_ptr(ix)
	 ld manentity_last_fvideoh_ptr(ix), a				;; Update high byte of font video ptr value (to erase in next iteration of render loop)
	jr nc, sysrender_draw_entity_add_buffer_ptr

	 sub #0x40
	jr sysrender_draw_entity_update_last_video_ptr

	sysrender_draw_entity_add_buffer_ptr:
	 add #0x40

	sysrender_draw_entity_update_last_video_ptr:
	 ld h, a
	 ;ld h, manentity_last_bvideoh_ptr(ix)
	 ld l, manentity_last_bvideol_ptr(ix)
	 ld manentity_last_fvideol_ptr(ix), l				;; Update low byte of font video ptr value (to erase in next iteration of render loop)
	 ld a, manentity_vx(ix)

	 ;; Substract current vx to current star position (to draw it in new position of screen)
	 add l
	 ld l, a
	 ld a, h
	 ccf
	 sbc #0
	 ld h, a

	jp sysrender_draw_entity_draw_new_position

	sysrender_draw_entity_get_screen_ptr:
	 ld a, (sysrender_back_buffer)				;; / 
	 ld d, a 									;; | Start of the video memory where calculations are made 
	 ld e, #0									;; \
	 ld c, manentity_x(ix)						;; C = X
	 ld b, manentity_y(ix)						;; B = Y  
	call cpct_getScreenPtr_asm					;; HL = screen pointer where draw IX entity

	sysrender_draw_entity_draw_new_position:
	 ld manentity_last_bvideoh_ptr(ix), h
	 ld manentity_last_bvideol_ptr(ix), l

	 ;; Draw a sprite
	 ld d, h
	 ld e, l
	 ld h, manentity_hsprite_ptr(ix)
	 ld l, manentity_lsprite_ptr(ix)
	 ld b, manentity_w(ix)
	 ld c, manentity_h(ix)
	jp sysrender_draw_XOR_entity 

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
sysrender_draw_XOR_entity:
	push ix 									;; [2 | 5] Save IX register
	ld__ixl_b									;; [2 | 2] IXL = B (width)

	sysrender_XOR_entity_main_loop:
	 push de									;; [1 | 4] Save DE (address memory to draw)

	sysrender_XOR_entity_row_loop:
	 ld a, (de) 								;; [1 | 2] If is "erase mode", content of DE address memory = 0x00, else, the previous color of entity
	 xor (hl)									;; [1 | 2] A ^ Sprite byte color, if A == back color: result = sprite byte color, else, A == sprite color, so res = back color
	 inc hl										;; [1 | 2] Next sprite byte color
	 ld (de), a 								;; [1 | 2] Update byte color of DE addres memory
	 inc de 									;; [1 | 2] Next byte of video memory address to draw

 	djnz sysrender_XOR_entity_row_loop			;; [3/4 | 2] If B != 0, blending operation on the next byte 
 	 pop de										;; [1 | 3] DE = destination address video memory to draw
 	 dec c 										;; [1 | 1] 
 	jr z, sysrender_draw_XOR_entity_end			;; [2 | 3/2]

 	 ld__b_ixl									;; [2 | 2] B = entity width
 	 ld a, #0x08								;; [2 | 2] We add 0x0800 to destination pointer to get next byte line of current character
 	 add d 										;; [1 | 1] A = A + D (high byte of destination addres video memory to draw). Low byte is not necessary

 	 ld d, a 									;; [1 | 1]
 	 and #0x38 									;; [2 | 2] / If any bit from 13, 12, 11 (weights) is not 0: we are still inside video memory boundaries, so proceed 
 	jr nz, sysrender_XOR_entity_main_loop		;; [2 | 3/2] \ with next bytes line

 	 ;; Every 8 lines (1 character), we cross the 16K video memory boundaries, 8 * 2048k (0x800), and have to reposition destination pointer. 
 	 ;; That means our nextline is 16K-0x50 bytes back (to c0000) which is the same as advancing 48K(0xc0000)+0x50 = 0xC050 bytes, as memory is 64K 
   	 ;; and our 16bit pointers cycle over it.
 	 ld a, #0x50 								;; [2 | 2] / Add low byte to E
 	 add e										;; [1 | 1] \
 	 ld e, a 									;; [1 | 1]
 	 ld a, #0xc0								;; [2 | 2] / Add high byte to D and the carry flag
 	 adc d										;; [1 | 1] \
 	 ld d, a 									;; [1 | 1]
 	jp sysrender_XOR_entity_main_loop			;; [3 | 3]

 	sysrender_draw_XOR_entity_end:
 	 pop ix 									;; [2 | 5] IX = current entity

	ret											;; [1 | 3]


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
	ld de, #sysrender_draw_entity
	call mancomponents_get_array_ptr
	jp manentity_forall_ptr

	;ret