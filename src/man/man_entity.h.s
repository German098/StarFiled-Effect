.module Man_entity_H

;;
;; CONSTANTS
;;
manentity_num_entities 		    = 6
manentity_num_bytes 		    = manentity_num_entities * manentity_size
manentity_max_entities_to_destroy = 4

;; Bit for individual component entity testing
manentity_alive_bit   = 7		;; Alive/Dead
manentity_render_bit  = 6		;; Render/Not render
manentity_destroy_bit = 0		;; Destroy/Not destroy
 
;; Components of entity 
manentity_cmp_alive_mask  = (1 << manentity_alive_bit)
manentity_cmp_render_mask = (1 << manentity_render_bit)
manentity_destroy_mask	  = (1 << manentity_destroy_bit)
manentity_default_mask	  = 0xFF

;; Composite masks
manentity_cmp_star_mask    = (manentity_cmp_alive_mask | manentity_cmp_render_mask)

;; Constants to acces data of entity
manentity_cmps = 0
manentity_x = 1
manentity_y = 2
manentity_w = 3
manentity_h = 4
manentity_vx = 5
manentity_last_videol_ptr = 6
manentity_last_videoh_ptr = 7
manentity_lanim_ptr = 8
manentity_hanim_ptr = 9
manentity_lprevsprite_ptr = 10
manentity_hprevsprite_ptr = 11
manentity_lsprite_ptr = 12
manentity_hsprite_ptr = 13
manentity_size = manentity_hsprite_ptr + 1 

;;
;; GLOBAL TAGS
;;
;.globl manentity_forall_matching_next_entity_incr

;;
;; FUNCTION DECLARATIONS
;;
.globl manentity_init
.globl manentity_create
.globl manentity_update
.globl manentity_add_A_entities
.globl manentity_set_for_destruction
.globl manentity_set_to_no_renderizable
.globl manentity_forall_ptr
;.globl manentity_forall_matching