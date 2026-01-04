.module Man_entity_H

;;
;; CONSTANTS
;;
manentity_num_entities 		    = 1
manentity_num_bytes 		    = manentity_num_entities * manentity_size
manentity_max_entities_to_destroy = 4

;; Star width in bytes
manentity_star_width  = 2
manentity_star_height = 6

;; Frame counter for changing animations
manentity_fps_anim    = 5 

;; X distance to travel in bytes
manentity_distance_X  = 78
manentity_distance_Y  = 200
manentity_max_Y_pos   = manentity_distance_Y - manentity_star_height

;; Absolute values of component vx
manentity_cmp_vx_0    = 1
manentity_cmp_vx_1    = 2
manentity_cmp_vx_2    = 3

;; Bit for individual component entity testing
manentity_alive_bit   	   = 7		;; Alive/Dead
manentity_render_bit  	   = 6		;; Render/Not render
manentity_render_erase_bit = 5 		;; Only to erase in double buffer (after that, star will be destroied)
manentity_destroy_bit 	   = 0		;; Destroy/Not destroy
 
;; Components of entity 
manentity_cmp_alive_mask   	  = (1 << manentity_alive_bit)
manentity_cmp_render_mask 	  = (1 << manentity_render_bit)
manentity_cmp_erase_render_mask = (1 << manentity_render_erase_bit)
manentity_destroy_mask	  	  = (1 << manentity_destroy_bit)
manentity_default_mask	  	  = 0x00

;; Composite masks
manentity_cmp_star_mask    		  = (manentity_cmp_alive_mask | manentity_cmp_render_mask)
manentity_cmp_star_last_frame_mask    = (manentity_cmp_render_mask | manentity_cmp_erase_render_mask)
manentity_cmp_dead_mask    		  = (manentity_destroy_mask)

;; Constants to acces data of entity
manentity_cmps = 0
manentity_x = 1
manentity_y = 2
manentity_w = 3
manentity_h = 4
manentity_vx = 5
manentity_last_fvideol_ptr = 6
manentity_last_fvideoh_ptr = 7
manentity_last_bvideol_ptr = 8
manentity_last_bvideoh_ptr = 9
manentity_lanim_ptr = 10
manentity_hanim_ptr = 11
manentity_lprevspritef_ptr = 12
manentity_hprevspritef_ptr = 13
manentity_lprevspriteb_ptr = 14
manentity_hprevspriteb_ptr = 15
manentity_lsprite_ptr = 16
manentity_hsprite_ptr = 17
manentity_fps_anim_ptr = 18
manentity_size = manentity_fps_anim_ptr + 1 

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
.globl manentity_set_for_erase
.globl manentity_forall_ptr
.globl manentity_swap_back_front_ptrs