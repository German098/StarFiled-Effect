;;
;; COMPONENTS MANAGER
;;
.module Components_manager_S

.include "cpctelera.h.s"
.include "../man/man_entity.h.s"
.include "../man/man_components.h.s"

;; Array of components
mancomponents_arrays_size = (manentity_num_entities + 1) * 2
mancomponents_array:
	;; Array component position
	.ds mancomponents_arrays_size		;; / Array = [ptr to next ptr entity to insert, 0x????, ..., 0x????, 0x0000]
	.dw 0x0000					;; \
mancomponents_array_end:
;; Ptr to current array of _components_array to operate with
mancomponent_array_ptr:
	.dw 0x0000

;;
;; PUBLIC FUNCTIONS
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Init components manager (set value of _component_array_ptr = ptr to first array of _components_array) 
;; INPUTS: A (component array)
;; OUTPUTS: HL (ptr to component array of _components_array)
;; CHANGED: HL, DE, AF
;; WARNING: 
;;		- 0 >= A <= e_component_num - 1
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
mancomponents_init::
	ld a, #e_mancomponents_position
	call mancomponents_set_component_array

	;; Init ptr to next ptr entity of Ath array of components
	ld d, h
	ld e, l
	inc de
	inc de
	ld (hl), e
	inc hl
	ld (hl), d

	ret

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Get mancomponent_array_ptr
;; INPUTS: -
;; OUTPUTS: HL (ptr to array of mancomponents_array)
;; CHANGED: HL
;; WARNING: -
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
mancomponents_get_array_ptr::
	ld hl, (mancomponent_array_ptr)

	ret

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Get next ptr in _components_array and respected ptr of manentity_array depends on HL (otherwise return 0x0000).
;; INPUTS: DE (current ptr of _components_array), A (component entity mask)
;; OUTPUTS: Zero flag (1: ret invalid ptrs: 0x0000, 0: valid next ptr of _components_array), IY (ptr of manentity_array)
;; CHANGED: AF, DE, BC, IY
;; WARNING:
;;		- HL should be a valid ptr of _components_array.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
mancomponents_get_next_ptr::
	ld c, a 

	inc de 												;; /
	inc de 												;; |
	ld a, (de) 												;; | 
	ld b, a 												;; | DE = next ptr of _components_array + 1
	ld__iyl_a 												;; | IY = ptr of next entity respect (DE + 2)
	inc de 												;; |
	ld a, (de) 												;; |
	ld__iyh_a 												;; |
	or b 													;; \
	ret z  												;; If DE == 0x0000: end of _components_array, so ret, else: check cmps 

	ld a, c
	and manentity_cmps(iy) 
	sub c
	jr nz, mancomponents_get_next_ptr 								;; If cmps of IY != A: check next ptr, else: ret 

	inc a 												;; Force to reset Zero flag

	ret

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Get prev ptr in _components_array and respected ptr of manentity_array depends on HL (otherwise return 0x0000).
;; INPUTS: DE (current ptr of _components_array), A (component entity mask)
;; OUTPUTS: Zero flag (1: ret invalid ptrs: 0x0000, 0: valid prev ptr of _components_array), IY (respected ptr of manentity_array)
;; CHANGED: AF, DE, BC, IY
;; WARNING:
;;		- HL should be a valid ptr of _components_array.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
mancomponents_get_prev_ptr::
	ld c, a 

	call mancomponents_get_array_ptr

	dec de 									;; /
	ld a, (de) 									;; |
	ld__iyh_a 									;; |
	dec de 									;; | DE = prev ptr of _components_array
	ld a, (de) 									;; | IY = ptr of prev entity respect (DE - 2)
	ld__iyl_a 									;; |
	cp (hl) 									;; |
	jr nz, mancomponents_get_prev_ptr_continue 			;; |
	inc hl 									;; |
 	ld__a_iyh 									;; |
 	cp (hl) 									;; \
	ret z 									;; If DE == next ptr to be inserted: ret, else: check cmps 

	mancomponents_get_prev_ptr_continue:
	 ld a, c
	 and manentity_cmps(iy) 
	 sub c
	jr nz, mancomponents_get_prev_ptr 								;; If cmps of IY != A: check next ptr, else: ret 

	inc a 												;; Force to reset Zero flag

	ret

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Set number array of components to insert or delete a ptr.
;; INPUTS: A (Ath array of _components_array)
;; OUTPUTS: -
;; CHANGED: HL, DE, AF
;; WARNING: 
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
mancomponents_set_component_array::
	call mancomponents_get_component_array
	ld (mancomponent_array_ptr), hl 					;; Set current array of components

	ret

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Insert new ptr to A array of _components_array
;; INPUTS: DE (ptr to entity of manentity_array)
;; OUTPUTS: BC (inserted ptr of mancomponent_array_ptr)
;; CHANGED: HL, DE, BC, A
;; WARNING: 
;;		- There must be free space in the Ath array.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
mancomponents_insert::
	ld hl, (mancomponent_array_ptr)			;; Ptr to array of components_array (next entity ptr)

	;; Update next entity ptr
	ld c, (hl)
	inc hl
	ld b, (hl)
	dec hl
	inc (hl)
	inc (hl)

	ld a, e
	ld (bc), a 						;; / 
	inc bc 						;; | Insert new ptr in current array of components_array
	ld a, d 						;; |
	ld (bc), a 						;; \

	dec bc 						;; BC = inserted ptr of mancomponent_array_ptr 
	ld h, b  
	ld l, c  

	ret

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Sort ptr entity of mancomponent_array_ptr depends on insertion mode (A).
;; INPUTS: IX (ptr entity of manentity_array), HL (ptr to sort in mancomponent_array_ptr)
;; OUTPUTS: DE (ptr sorted in mancomponent_array_ptr)
;; CHANGED: AF, HL, DE, BC, IY
;; WARNING: 
;;		- Size of mancomponent_array must be >= 1.
;;		- HL muest be a valid pointer of mancomponent_array_ptr.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
mancomponents_sort::
	ld d, h
	ld e, l
	ld bc, #0x0000

	inc hl
	mancomponents_sort_check_right_side:
	 inc hl
	 ld a, (hl)
	 ld__iyl_a
	 inc hl
	 or (hl)
	jr z, mancomponents_sort_left_side
	 ld a, (hl)
	 ld__iyh_a

	 ld a, manentity_y(iy)
	 sub manentity_y(ix)
	jp m, mancomponents_sort_update_counter

	mancomponents_sort_left_side:
	 ld a, b
	 or c
	jr nz, mancomponents_sort_transfer_bytes

	set 7, b
	;; Check left side
	mancomponents_sort_check_left_side:
	call mancomponents_get_array_ptr
	 dec de

	 ld a, (de)
	 ld__iyh_a
	 dec de
	 ld a, (de)
	 ld__iyl_a
	 cp (hl)
	jr nz, mancomponents_sort_left_side_continue
	 inc hl
	 ld__a_iyh
	 cp (hl)
	jr z, mancomponents_sort_transfer_bytes 

	mancomponents_sort_left_side_continue:
	 ld a, manentity_y(iy)
	 sub manentity_y(ix)
	jp m, mancomponents_sort_transfer_bytes 

	mancomponents_sort_update_counter:
	 inc bc
	 inc bc

	 ld a, b
	 and #0b10000000
	jr nz, mancomponents_sort_check_left_side 
	jr mancomponents_sort_check_right_side

	mancomponents_sort_transfer_bytes:
	 ld a, b
	 and #0b10000000
	jr z, mancomponents_sort_transfer_bytes_right
	 inc de 
	 inc de
	 res 7, b
	 ld a, b
	 or c
	ret z

	 ld h, d
	 ld l, e
	 add hl, bc
	 push de
	 ld d, h
	 ld e, l
	 inc de
	 dec hl
	 
	 lddr
	 pop de

	 ld__a_ixl
	 ld (de), a
	 inc de
	 ld__a_ixh
	 ld (de), a
	 dec de

	ret

	mancomponents_sort_transfer_bytes_right:
	 ld h, d
	 ld l, e
	 inc hl
	 inc hl

	 ldir

	 ld__a_ixl
	 ld (de), a
	 inc de
	 ld__a_ixh
	 ld (de), a
	 dec de

	ret


;mancomponents_sort::
;	ld a, (hl)							;; /
;	ld__ixl_a 							;; | IX = ptr entity of manentity_array
;	inc hl 							;; |
;	ld a, (hl) 							;; |
;	ld__ixh_a 							;; \
;	
;	ld hl, (mancomponent_array_ptr)
;	inc hl
;	inc hl
;	ld bc, #0x0000
;
;	mancomponents_sort_loop:
;	 ld a, (hl)
;	 cp__ixl
;	 ld__iyl_a
;	jr nz, mancomponents_sort_continue
;	 inc hl
;	 ld a, (hl)
;	 cp__ixh
;	jr z, mancomponents_sort_transfer_bytes
;	jr mancomponents_sort_continue_2
;
;	mancomponents_sort_continue:
;	 inc hl
;	 ld a, (hl)
;	mancomponents_sort_continue_2:
;	 ld__iyh_a
;
;	 ld a, manentity_y(iy)
;	 sub manentity_y(ix)
;	jp m, mancomponents_sort_next_ptr
;
;	 inc hl
;	jr mancomponents_sort_update_counter
;
;	mancomponents_sort_transfer_bytes:
;	 ld a, b
;	 or c
;	ret z 
;
;	 ld d, h
;	 ld e, l
;	 dec de
;	 dec de
;	 ex de, hl
;
;	 lddr
;
;	 inc hl
;	 ld__a_ixl
;	 ld (hl), a
;	 inc hl
;	 ld__a_ixh
;	 ld (hl), a
;
;	ret
;
;	mancomponents_sort_next_ptr:
;	 inc hl
;
;	 ld a, b
;	 or c
;	jr z, mancomponents_sort_loop 
;
;	mancomponents_sort_update_counter:
;	 inc bc
;	 inc bc
;
;	jp mancomponents_sort_loop
	
	;ret

;;
;; PRIVATE FUNCTIONS
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Get the Ath array of _components_array.
;; INPUTS: A (component array)
;; OUTPUTS: HL (ptr to component array of _components_array)
;; CHANGED: HL, DE, AF
;; WARNING: 
;;		- 0 >= A <= e_component_num - 1
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
mancomponents_get_component_array:
	ld hl, #mancomponents_array					;; Ptr to first array of _components_array
	ld de, #mancomponents_arrays_size 				;; Size of each array of _components_array

	aientityman_get_component_array_loop:
	 or a
	ret z

	 dec a
	 add hl, de
	jp aientityman_get_component_array_loop

	;ret