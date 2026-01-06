.module Man_entity_S

.include "cpctelera.h.s"
.include "../man/man_entity.h.s"
.include "../man/man_components.h.s"
.include "../man/man_animcontrol.h.s"
.include "../assets/assets.h.s"
.include "cpctelera_functions.h.s"

;; Default entity

;xxxx0xxxxxxx1
;
;v mov 	
;1 12 	1 12 
;2 6	2 0
;3 4

manentity_default:
	.db #manentity_cmp_star_mask							;; cmps
	.db #manentity_distance_X, 0 							;; [x, y]
	.db #manentity_star_width, #manentity_star_height 		;; [w, h] (bytes)
	.db 0 													;; vx
	.dw 0x0000 												;; last front video ptr
	.dw 0x0000 												;; last back video ptr
	.dw _star_animation										;; anim ptr
	.dw _sp_star_0											;; star current sprite
	.db #manentity_fps_anim									;; frames counter for animations

;; Entities array
manentity_array::
	.ds manentity_num_bytes
;; Ptr next entity to insert in manentity_array
manentity_array_next:
	.dw #manentity_array

;; Array distance to travel respect to vx values abs([-1, -3])
manentity_array_vx_bytes:
	.db manentity_distance_X / manentity_cmp_vx_0
	.db manentity_distance_X / manentity_cmp_vx_1
	.db manentity_distance_X / manentity_cmp_vx_2

;;
;; PUBLIC FUNCTIONS
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Init entity manager
;; INPUTS: -
;; OUTPUTS: -
;; CHANGED: -
;; WARNING: -
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
manentity_init::
	;; Not neccesary!!!
	ld hl, #manentity_array
	ld (hl), #0
	ld d, h
	ld e, l
	inc de
	ld bc, #manentity_num_bytes - 1

	ldir

	ret

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Create new entity in manentity_array with Y and Vx pseudo-random values, [1, 199] and [-1, -3] respectively.
;; INPUTS: -
;; OUTPUTS: IX (ptr to created entity)
;; CHANGED: HL, DE, BC, AF, IX, IY
;; WARNING:
;;	- There must be free space in the array to insert a new entity.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
manentity_create::
	call manentity_new
	ld__ix_de 											;; IX = DE

	;; Insert ptr entity (DE) in _component_array_ptr
	call mancomponents_insert

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Replace selected entity in manentity_array (DE) with new entity.
;; INPUTS: DE (ptr to array entity to be replaced), BC (current ptr of mancomponent_array_ptr to refresh)
;; OUTPUTS: IX (ptr to created entity)
;; CHANGED: HL, DE, BC, AF, IX, IY
;; WARNING:
;;	- DE ptr to entity must exists in the array.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 4391 0f 43a3 4d 4349 5d 436d 74 437f 8e 435b 9f
manentity_refresh:
	ld (_manentity_refresh_ptr_to_sort), bc

	ld hl, #manentity_default
	ld bc, #manentity_size

	;; HL = ptr to  default entity, IX = DE = ptr to next free space in the array of entities, BC = number of bytes to transfer from HL to DE
	ldir

	;; Increment current num of entities in the array
	;ld a, (manentity_array_counter)
	;inc a
	;ld (manentity_array_counter), a

	;; Improve this part of the code!!!
	;; Fill entity with pseudo-random data
	manentity_create_pseudo_random_value:
	call cpct_getRandom_xsp40_u8_asm
	;; A = L = pseudo-radom byte > 0
	;; Get Y value in range (Y => 0000 0001, 1100 0111)
	 and #0b10000000
	;; A & 1000 0000 = x000 0000: if A == 0: A = [1, 127] so save value, else: check 6th bit of A
 	jr nz, manentity_create_test_6_bit_num_random 
	 ld manentity_y(ix), l
	;; Get Vx value
	jp manentity_create_random_vx 			
	
	manentity_create_test_6_bit_num_random:
 	 bit 6, l
 	 ;; If 6th bit of L == 0: L = 10xx xxxx = [128, 191] so save value, else: check 5, 4 and 3 bits of A
 	jr nz, manentity_create_345_bits_num_random
	 ld manentity_y(ix), l

	jp manentity_create_random_vx

	;; 199, c7 => 1100 0111
	;; 198, c6 => 1100 0110
	;; 197, c5 => 1100 0101
	;; 196, c4 => 1100 0100
	;; 195, c3 => 1100 0011
	;; 194, c2 => 1100 0010
	;; 193, c1 => 1100 0001
	manentity_create_345_bits_num_random:
	 ld a, l
	 and #0b00111000
	 ;; If (A 3th | A 4th | A 5th) == 0: A = [192, 199] so save value, else: set 7th to 0 (A < 128)
	jr nz, manentity_create_downgrade_random_value
	jp manentity_create_check_Y_random_value

	manentity_create_downgrade_random_value:
	 res 7, l
	jr manentity_create_set_Y_random_value
	 
	manentity_create_check_Y_random_value:
	 ld a, #manentity_max_Y_pos
	 sub l
	jr c, manentity_create_set_max_Y_value
	jr manentity_create_set_Y_random_value

	manentity_create_set_max_Y_value:
	 ld l, #manentity_max_Y_pos

	manentity_create_set_Y_random_value:
	 ld manentity_y(ix), l

	manentity_create_set_random_vx:
	 ;; Get Vx value in range (Vx => 0000 0001, 0000 0011)
	 ld a, l
	 cp #0
	 and #manentity_cmp_vx_2
	jr z, manentity_create_random_vx
	 neg
	 ;; Vx = [-1, -3] (WARNING: set to 0 for testing)
	 ld manentity_vx(ix), a

	;; Sort new star ptr in mancomponent_array_ptr
	 _manentity_refresh_ptr_to_sort=.+1
	 ld hl, #0x0000
	call mancomponents_sort

	 ld h, d												;; / HL = DE
	 ld l, e 												;; \
	 ld a, #manentity_cmp_star_mask
	call mancomponents_get_next_ptr
	jr z, manentity_create_check_prev_ptr
	
	call manentity_can_overlap_IX_IY
	jr c, manentity_create_check_prev_ptr

	 ;; No carry: generate new pseudo random value
	 ld (_manentity_refresh_ptr_to_sort), hl 				;; HL = ptr in mancomponent_array_ptr to sort (with other Y and vx values)
	jr manentity_create_pseudo_random_value  

	manentity_create_check_prev_ptr:
	 ld d, h
	 ld e, l
	 ld a, #manentity_cmp_star_mask
	 push hl
	call mancomponents_get_prev_ptr
	 pop hl
	jr nz, manentity_create_check_prev_ptr_overlap
	 ld hl, (_manentity_refresh_ptr_to_sort)
	 ld (_manentity_change_ptr), hl

	ret 

	manentity_create_check_prev_ptr_overlap:
	call manentity_can_overlap_IX_IY
	jr c, manentity_create_check_HL_ptr

	 ;; No carry: generate new pseudo random value
	 ld (_manentity_refresh_ptr_to_sort), hl 				;; HL = ptr in mancomponent_array_ptr to sort (with other Y and vx values)
	jr manentity_create_pseudo_random_value  

	manentity_create_check_HL_ptr:
	 ld de, (_manentity_refresh_ptr_to_sort)
	 ld a, d
	 cp h
	jr c, manentity_create_update_HL_ptr
	ret nz
	 ld a, e
	 cp l
	jr c, manentity_create_update_HL_ptr
	ret

	manentity_create_update_HL_ptr:
	 ld (_manentity_change_ptr), de

	ret

	manentity_create_random_vx:
	call cpct_getRandom_xsp40_u8_asm
	jp manentity_create_set_random_vx

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Create A entities in manentity_array.
;; INPUTS: A (number of entities to create)
;; OUTPUTS: -
;; CHANGED: AF, HL, DE, BC, IX, IY(L)
;; WARNING:
;;	- A >= 0
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
manentity_add_A_entities::
	dec a
	ret m

	push af 							;; Save num entities to delete
	call manentity_create
	pop af

	jr manentity_add_A_entities

	;ret

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Swap back and front prevsprite_ptr.
;; INPUTS: IX (entity ptr)
;; OUTPUTS: -
;; CHANGED: A, B
;; WARNING: -
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;manentity_swap_back_front_ptrs::
;	ld a, manentity_lprevspritef_ptr(ix)
;	ld b, manentity_lprevspriteb_ptr(ix)
;	ld manentity_lprevspritef_ptr(ix), b
;	ld manentity_lprevspriteb_ptr(ix), a
;
;	ld a, manentity_hprevspritef_ptr(ix)
;	ld b, manentity_hprevspriteb_ptr(ix)
;	ld manentity_hprevspritef_ptr(ix), b
;	ld manentity_hprevspriteb_ptr(ix), a
;
;	ret

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Set entity for destruction
;; INPUTS: IX (entity to destroy)
;; OUTPUTS: -
;; CHANGED: AF
;; WARNING: -
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
manentity_set_for_destruction::
	ld a, #manentity_cmp_dead_mask
	ld manentity_cmps(ix), a 									;; A = dead entity

	ret

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Set entity to erase star in next frame (from double buffer)
;; INPUTS: IX (entity)
;; OUTPUTS: -
;; CHANGED: AF
;; WARNING: -
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
manentity_set_to_no_renderizable::
	ld a, #manentity_cmp_star_last_frame_mask 					;; A = not alive star and only render to erase from double buffer
	ld manentity_cmps(ix), a

	ret

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Set entity for erase (of the second buffer)
;; INPUTS: IX (entity to destroy)
;; OUTPUTS: -
;; CHANGED: AF
;; WARNING: -
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
manentity_set_for_erase::
	ld a, #manentity_cmp_alive_mask
	xor manentity_cmps(ix) 
	ld manentity_cmps(ix), a 									;; A = dead entity but renderizable yet

	ret

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Loop over entity array, if e_cmps(IX) == A: call HL function, else: next entity.
;; INPUTS: HL (function to call), A (component entity mask)
;; OUTPUTS: -
;; CHANGED: IX, AF, BC, D, ?
;; WARNINGS: 
;;	- There must be at least one entity to destroy in the array.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;manentity_forall_matching_next_entity_incr:
;	.dw #manentity_size
;manentity_forall_matching::
;	ld ix, #manentity_array
;	ld (_manentity_cmp_mask), a
;
;	;ld a, (manentity_array_counter)
;	;cp #0
;	;ret z
;
;	ld a, #manentity_num_entities
;	ld (_manentity_counter), a
;	manentity_forall_matching_loop:
;	 _manentity_cmp_mask=.+1
;	 ld d, #0x00
;	 ld a, d
;	 and manentity_cmps(ix)
;	 cp d
;	jr nz, manentity_forall_matching_next_entity
;
;	 push ix
;	 push hl
;	 ld (_manentity_function_to_call), hl
;	 _manentity_function_to_call =.+1
;	call #0x0000
;	 pop hl
;	 pop ix
;
;	manentity_forall_matching_next_entity:
;	 _manentity_counter=.+1
;	 ld a, #0x00
;	 dec a
;	 cp #0
;	ret z 
;	 ld (_manentity_counter), a
;
;	 ld bc, (manentity_forall_matching_next_entity_incr)
;	 add ix, bc
;	 ld a, #manentity_size
;	 ld (manentity_forall_matching_next_entity_incr), a
;	jr manentity_forall_matching_loop
;
;	ret

;;;;;;;;;;;;;;;;;;;;;
;;
;; Iterate array of ptrs and call function DE for each one
;; ENTRADAS: HL (array of _components_array), DE (ptr call function), A (component entity mask)
;; SALIDAS: -
;; ALTERADOS: HL, AF, D, ?
;;
;;;;;;;;;;;;;;;;;;;;;
manentity_forall_ptr::
	ld (_manentity_function_to_call), de
	ld (_manentity_cmp_mask), a

	xor a
	ld (_manentity_break_forall), a 		;; If value of pos _manentity_break_forall != 0: force to break for loop, else: continue
	ld (_manentity_change_ptr), a
	ld (_manentity_change_ptr + 1), a

	inc hl 									;; / HL = first ptr of _components_array
	inc hl 									;; \
	entityman_forall_ptr_loop:
	 ld a, (hl)
	 ld__ixl_a
	 inc hl
	 or (hl) 								;; / If &HL == 0x0000: end of array of components, so ret, else: call function
	ret z 									;; \ 
	 
	 ld a, (hl)
	 inc hl
	 ld__ixh_a	 							;; IX = entity ptr in manentity_array

	 _manentity_cmp_mask=.+1
	 ld d, #0x00
	 ld a, d
	 and manentity_cmps(ix)
	 cp d
	jr nz, entityman_forall_ptr_loop

	 push hl
	 _manentity_function_to_call = .+1
	call _manentity_function_to_call
	 pop hl

	 ;; Check break
	 _manentity_break_forall = .+1
	 ld a, #0x00
	 or #0
	ret nz 

	 ;; Change HL value (next ptr in the ptr array)
	 _manentity_change_ptr = .+1
	 ld de, #0x0000
	 ld a, d
	 or e
	jr z, entityman_forall_ptr_loop
	 ex de, hl 
	 xor a
	 ld (_manentity_change_ptr), a
	 ld (_manentity_change_ptr + 1), a

	jp entityman_forall_ptr_loop

	;ret

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Update entities of array per frame (delete manentity_max_entities_to_destroy entities).
;; INPUTS: -
;; OUTPUTS: -
;; CHANGED: IX, AF, BC, ?
;; WARNINGS: -
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
manentity_update::
	call manentity_set_destroy_func_values 			;; Set value of num entities to delete in manentity_destroy function
	ld a, #manentity_cmp_dead_mask
	ld de, #manentity_destroy
	call mancomponents_get_array_ptr
	jp manentity_forall_ptr

	;ret

;;
;; PRIVATE FUNCTIONS
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Define space in manentity_array for a new entity
;; INPUTS: -
;; OUTPUTS: DE (ptr to new entity generated)
;; CHANGED: HL, DE, BC
;; WARNINGS: 
;;	- There must be free space in the array to insert a new entity.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
manentity_new:
	ld hl, (manentity_array_next)
	ld d, h
	ld e, l
	ld bc, #manentity_size
	add hl, bc
	ld (manentity_array_next), hl

	ret

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Set initial values to manentity_destroy function.
;; INPUTS: -
;; OUTPUTS: -
;; CHANGED: A
;; WARNINGS: -
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
manentity_set_destroy_func_values:
	ld a, #manentity_max_entities_to_destroy
	ld (_manentity_num_ents_to_destroy), a

	ret

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Destroy entity of manentity_array. Copy data of last entity inserted in ptr of the entity to delete (only if it is not the last one) and decrement 
;; manentity_array_next in manentity_size bytes.
;; INPUTS: IX (ptr entity to destroy in the array), HL (array of _components_array)
;; OUTPUTS: DE/IX (ptr entity to destroy in manentity_array), HL/BC (current ptr of mancomponent_array_ptr deleted/refreshed)
;; CHANGED: HL, DE, BC, AF
;; WARNINGS: 
;;	- There must be at least one entity to destroy in the array.
;;	- Ptr of entity to destroy must be a valid one.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
manentity_destroy:
	_manentity_num_ents_to_destroy =.+1
	ld a, #0x00
	cp #0
	jr nz, manentity_destroy_continue

	;; Number of entities to delete reached, so, set _manentity_counter of manentity_forall_matching to 1, therefor, in the current iteration of it, 
	;; the loop will end.
	inc a
	ld (_manentity_break_forall), a

	ret

	manentity_destroy_continue:
	 ;; Substract manentity_size from the value of manentity_array_next ptr 
	 ;ld a, (manentity_array_next) 				 
	 ;sub #manentity_size 						
	 ;ld (manentity_array_next), a 				
	 ;ld l, a
	 ;ld a, (manentity_array_next + 1)
	 ;sbc #0
	 ;ld h, a
	 ;ld (manentity_array_next + 1), a
	 ;
	 ;ld__de_ix
	 ;ld bc, #manentity_size
	 ;
	 ;ldir

	 ;; Set manentity_forall_matching_next_entity_incr value to 0
	 ;xor a
	 ;ld (manentity_forall_matching_next_entity_incr), a
 		
 	 ;; Decrement number of max entities to delete in current frame
	 ld a, (_manentity_num_ents_to_destroy)
	 dec a
	 ld (_manentity_num_ents_to_destroy), a

	 ;; To make ldir in manentity_refresh function, DE = ptr entity to destroy in manentity_array
	 ld__de_ix

	 ;; HL = HL (next ptr of mancomponent_array_ptr) - 2 (increments of manentity_forall_ptr) = current ptr of mancomponent_array_ptr
	 dec hl
	 dec hl
	 ld b, h
	 ld c, l

	jp manentity_refresh 

	;ret

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Check whether IX and IY can overlap at any point of screen travel or not.
;; INPUTS: IX (ptr entity 1), IY (ptr entity 2)
;; OUTPUTS: Carry Flag (0: can overlap in some point, 1: can not overlap each other)
;; CHANGED: AF, DE
;; WARNINGS: 
;;			- IX and IY must be valid ptrs. 
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; -- a 07 
;;        
;; -- b
;;        -- c 19 IX
;;
;; 		  -- d	
;;
;; IX 42d1 67 fd 
;; IY 4317 19 fd
;; 42c5 07 42d3 18 42e1 2d 42fd 60 430b 74 42ef 42
;; 
manentity_can_overlap_IX_IY:
	;; Check colision between IX and IY (IX as reference)
	ld a, manentity_y(iy)
	sub manentity_y(ix)
	jr nc, manentity_can_overlap_IX_IY_down 

	;; Check collision on IX up side
	ld a, manentity_y(iy)
	add manentity_h(iy)
	dec a
	sub manentity_y(ix)   
	ret c ;; No collision
	jp manentity_can_overlap_IX_IY_continue

	manentity_can_overlap_IX_IY_down:
	 ;; Check collision on IX down side
	 ld a, manentity_y(ix) 								
	 add manentity_h(ix) 								
	 dec a 
	 sub manentity_y(iy) 
	ret c	;; No collision											

	manentity_can_overlap_IX_IY_continue:
	;; Check for possible collision on the X axis after collision in Y (only possible when entity is created first time, IX, and collision with old entity, IY)
	ld a, manentity_x(iy) 								;; /
	add manentity_w(iy) 								;; | If IY right point > IX left point : overlap in X axis, so ret,  
	dec a 												;; | else: check vx of both entities
	sub manentity_x(ix) 								;; |
	ret nc	;; Collision 								;; \

	;; Y pos of IY is in range, so, check if IX is reacheble with IX (vx, position) and vx of IY (manentity_array_vx_bytes)
	ld a, manentity_vx(iy) 								;; / If IX vx - IY vx <= 0: IX can't reach IY, else: check manentity_array_vx_bytes
	sub manentity_vx(ix) 								;; \
	ret c 	;; No collision
	scf 	;; Set flag carray to 1
	ret z 	;; No collision (Carray flag == 1)

	ld de, #manentity_array_vx_bytes
	ld a, manentity_vx(ix)  							;; /
	neg 												;; | A = abs(vx) - 1 = index to get value of manentity_array_vx_bytes
	dec a 												;; \

	add e 												;; /
	ld e, a 											;; | DE = ptr to min X position that IY must be at this moment so that IX can not reach IY with vx.
	ld a, d 											;; |
	adc #0 												;; |
	ld d, a 											;; \
	ld a, (de) 											;; /
	ld d, a 											;; | If IY X position < *HL: can not overlap, else: will overlap at some point
	ld a, manentity_x(iy) 								;; | 
	sub d 												;; \ 
	;ret c 	;; No collision

	ret