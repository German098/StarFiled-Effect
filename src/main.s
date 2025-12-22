;;-----------------------------LICENSE NOTICE------------------------------------
;;  This file is part of CPCtelera: An Amstrad CPC Game Engine 
;;  Copyright (C) 2018 ronaldo / Fremos / Cheesetea / ByteRealms (@FranGallegoBR)
;;
;;  This program is free software: you can redistribute it and/or modify
;;  it under the terms of the GNU Lesser General Public License as published by
;;  the Free Software Foundation, either version 3 of the License, or
;;  (at your option) any later version.
;;
;;  This program is distributed in the hope that it will be useful,
;;  but WITHOUT ANY WARRANTY; without even the implied warranty of
;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;  GNU Lesser General Public License for more details.
;;
;;  You should have received a copy of the GNU Lesser General Public License
;;  along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;-------------------------------------------------------------------------------

;; Include all CPCtelera constant definitions, macros and variables
.include "cpctelera.h.s"
.include "man/man_entity.h.s"
.include "sys/sys_render.h.s"
.include "sys/sys_physics.h.s"
.include "sys/sys_collision.h.s"
.include "sys/sys_anim.h.s"
.include "cpctelera_functions.h.s"

;;
;; Start of _DATA area 
;;  SDCC requires at least _DATA and _CODE areas to be declared, but you may use
;;  any one of them for any purpose. Usually, compiler puts _DATA area contents
;;  right after _CODE area contents.
;;
.area _DATA
.area _CODE

cpct_waitNoVSYNC::
   ld b, #0xf5
   cpct_waitNoVSYNC_loop:
    in a, (c)
    rra
   jr c, cpct_waitNoVSYNC_loop

   ret

_main::
   ;; Desactivamos el firmware para que no se ejecute su código en cada interrupción de la CPU (ninguna llamada que hagamos a paritr de aquí a funciones 
   ;; del firmware funcionará)
   call cpct_disableFirmware_asm

   call manentity_init
   call sysrender_init

   ld a, #manentity_num_entities
   call manentity_add_A_entities

   ;; Render loop
loop:
   cpctm_setBorder_asm HW_GREEN
   ;call syscollision_update

   cpctm_setBorder_asm HW_YELLOW
   call sysrender_update

   cpctm_setBorder_asm HW_PINK
   call animsys_update
   ;cpctm_setBorder_asm HW_ORANGE
   ;call sysphysics_update
   ;cpctm_setBorder_asm HW_BLUE
   ;call manentity_update
   cpctm_setBorder_asm HW_WHITE

   ld d, #50
   waitVSYNC_loop:
   call cpct_waitVSYNC_asm
   call cpct_waitNoVSYNC

    dec d
   jr nz, waitVSYNC_loop

   jr    loop