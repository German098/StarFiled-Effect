;;
;; COMPONENTS MANAGER
;;
.module Components_manager_S

.include "../man/man_entity.h.s"
.include "../man/man_components.h.s"

;; Array of components
_components_arrays_size = (manentity_num_entities + 1) * 2
_components_array:
	;; Array component position
	.ds _components_arrays_size		;; / Array = [ptr to next ptr entity to insert, 0x????, ..., 0x????, 0x0000]
	.dw 0x0000					;; \
_components_array_end:
;; Ptr to current array of _components_array to operate with
_component_array_ptr::
	.dw 0x0000

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
	ld a, #e_component_position
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
;; Get the Ath array of _components_array.
;; INPUTS: A (component array)
;; OUTPUTS: HL (ptr to component array of _components_array)
;; CHANGED: HL, DE, AF
;; WARNING: 
;;		- 0 >= A <= e_component_num - 1
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
mancomponents_get_component_array:
	ld hl, #_components_array					;; Ptr to first array of _components_array
	ld de, #_components_arrays_size 				;; Size of each array of _components_array

	aientityman_get_component_array_loop:
	 or a
	ret z

	 dec a
	 add hl, de
	jp aientityman_get_component_array_loop

	;ret

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
	ld (_component_array_ptr), hl 					;; Set current array of components

	ret

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Insert new ptr to A array of _components_array
;; INPUTS: DE (ptr to entity of manentity_array)
;; OUTPUTS: -
;; CHANGED: HL, DE, BC, A
;; WARNING: 
;;		- There must be free space in the Ath array.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
mancomponents_insert::
	ld hl, (_component_array_ptr)			;; Ptr to array of components_array (next entity ptr)

	;; Update next entity ptr
	ld c, (hl)
	inc hl
	ld b, (hl)
	dec hl
	inc (hl)
	inc (hl)

	ld a, e
	ld (bc), a 						;; / 
	inc bc 						;; | Insert new ptr in current array of components_array (HL)
	ld a, d 						;; |
	ld (bc), a 						;; \

	ret