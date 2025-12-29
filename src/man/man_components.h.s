;;
;; COMPONENTS MANAGER INCLUDE
;;
.module Components_manager_H

;;
;; FUNCTION DECLARATIONS
;;

.globl mancomponents_init
.globl mancomponents_set_component_array
.globl mancomponents_get_array_ptr
.globl mancomponents_get_next_ptr
.globl mancomponents_get_prev_ptr
.globl mancomponents_insert
.globl mancomponents_sort

;;
;; ENUMS
;;

;; Enum component entity types
e_mancomponents_position = 0
;; Number of components (to size of _components_array)
e_mancomponents_num = e_mancomponents_position + 1
