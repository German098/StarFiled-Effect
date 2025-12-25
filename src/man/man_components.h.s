;;
;; COMPONENTS MANAGER INCLUDE
;;
.module Components_manager_H

;;
;; GLOBAL TAGS
;;
.globl _component_array_ptr

;;
;; FUNCTION DECLARATIONS
;;

.globl mancomponents_init
.globl mancomponents_set_component_array
.globl mancomponents_insert

;; Enum component entity types
e_component_position = 0
;; Number of components (to size of _components_array)
e_component_num = e_component_position + 1