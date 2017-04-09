;+
;:ROUTINE NAME:
;SPECCAL_REINIT_RANGES_update
;
; :PURPOSE:
;   Called to update the spectral ranges if the "RW" variable is modified. Keeps the selected wavelenght unchanged while
;   expanding the windows
; :INPUTS:
;
; :OUTPUTS:
;
; :PARAMS:
;    info
;
; :RETURNS:
;
; :REQUIRES:
;
; :NOTES:
;
; :HISTORY:
;
;: Created:	16-mar-2010
;
; :AUTHOR: Lorenzo Busetto; Michele Meroni -
; Environmental Dynamics Remote Sensing Laboratory
; University of Milano-Bicocca
; Milan - Italy (IT)
; 
; ------------------------------------------------------------------------------
; Copyright Lorenzo Busetto and Michele Meroni, 2010
; 
;   This file is part of SpecCal.
;
;    SpecCal is distributed as free software. Redistribution and use in source and binary forms for non-commercial purposes,
;    with or without modification, are permitted by the authors subject to the following restrictions:
;       -   The origin of the software must not be misrepresented: you must not claim you wrote the original software. 
;       -   If you use the software (or part of it) in a product or application, an acknowledgment in the product documentation 
;       -   would be appreciated by the authors.
;       -   Altered source versions should be plainly marked as such, and must not be misrepresented as being the original software.
;
;   SpecCal is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the 
;   implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE
;------------------------------------------------------------------------------
;-
;-
FUNCTION SPECCAL_REINIT_RANGES_UPDATE, info


    info_initranges = SPECCAL_INITRANGES_UPDATE(info)
    info.RANGES = info_initranges.RANGES
    info.RANGES_BROAD1 = info_initranges.RANGES_BROAD1
    ;    info.ranges_broad2 = info_initranges.ranges_Broad2
    info.RANGES_USER = info_initranges.RANGES_USER
    
    slidinfo = info.RANGES[*,0,0]
    WIDGET_CONTROL, info.LOW1_WID, SET_SLIDER_MIN=slidinfo[0]
    WIDGET_CONTROL, info.LOW1_WID, SET_SLIDER_MAX=slidinfo[2]
    WIDGET_CONTROL,info.LOW1_WID, set_value = slidinfo[1]
    
    
    slidinfo = info.RANGES[*,1,0]
    WIDGET_CONTROL, info.UP1_WID, SET_SLIDER_MIN=slidinfo[0]
    WIDGET_CONTROL, info.UP1_WID, SET_SLIDER_MAX=slidinfo[2]
    WIDGET_CONTROL,info.UP1_WID, set_value = slidinfo[1]
    
    ; r2
    
    slidinfo = info.RANGES[*,0,1]
    WIDGET_CONTROL, info.LOW2_WID, SET_SLIDER_MIN=slidinfo[0]
    WIDGET_CONTROL, info.LOW2_WID,SET_SLIDER_MAX=slidinfo[2]
    WIDGET_CONTROL,info.LOW2_WID, set_value = slidinfo[1]
    
    
    slidinfo = info.RANGES[*,1,1]
    WIDGET_CONTROL, info.UP2_WID, SET_SLIDER_MIN=slidinfo[0]
    WIDGET_CONTROL, info.UP2_WID, SET_SLIDER_MAX=slidinfo[2]
    WIDGET_CONTROL,info.UP2_WID, set_value =  slidinfo[1]
    ; r3
    
    slidinfo = info.RANGES[*,0,2]
    WIDGET_CONTROL, info.LOW3_WID, SET_SLIDER_MIN=slidinfo[0]
    WIDGET_CONTROL, info.LOW3_WID, SET_SLIDER_MAX=slidinfo[2]
    WIDGET_CONTROL,info.LOW3_WID, set_value = slidinfo[1]
    
    slidinfo = info.RANGES[*,1,2]
    WIDGET_CONTROL, info.UP3_WID, SET_SLIDER_MIN=slidinfo[0]
    WIDGET_CONTROL, info.UP3_WID, SET_SLIDER_MAX=slidinfo[2]
    WIDGET_CONTROL,info.UP3_WID, set_value = slidinfo[1]
    
    ; r4
    
    slidinfo = info.RANGES[*,0,3]
    WIDGET_CONTROL, info.LOW4_WID,  SET_SLIDER_MIN=slidinfo[0]
    WIDGET_CONTROL, info.LOW4_WID,SET_SLIDER_MAX=slidinfo[2]
    WIDGET_CONTROL,info.LOW4_WID, set_value = slidinfo[1]
    
    slidinfo = info.RANGES[*,1,3]
    WIDGET_CONTROL, info.UP4_WID, SET_SLIDER_MIN=slidinfo[0]
    WIDGET_CONTROL, info.UP4_WID, SET_SLIDER_MAX=slidinfo[2]
    WIDGET_CONTROL,info.UP4_WID, set_value = slidinfo[1]
    
    ; r5
    
    slidinfo = info.RANGES[*,0,4]
    WIDGET_CONTROL, info.LOW5_WID,  SET_SLIDER_MIN=slidinfo[0]
    WIDGET_CONTROL, info.LOW5_WID, SET_SLIDER_MAX=slidinfo[2]
    WIDGET_CONTROL,info.LOW5_WID, set_value =slidinfo[1]
    
    slidinfo = info.RANGES[*,1,4]
    WIDGET_CONTROL, info.UP5_WID, SET_SLIDER_MIN=slidinfo[0]
    WIDGET_CONTROL, info.UP5_WID, SET_SLIDER_MAX=slidinfo[2]
    WIDGET_CONTROL,info.UP5_WID, set_value = slidinfo[1]
    
    ; r6
    
    slidinfo = info.RANGES[*,0,5]
    WIDGET_CONTROL, info.LOW6_WID, SET_SLIDER_MIN=slidinfo[0]
    WIDGET_CONTROL, info.LOW6_WID, SET_SLIDER_MAX=slidinfo[2]
    WIDGET_CONTROL,info.LOW6_WID, set_value =slidinfo[1]
    
    slidinfo = info.RANGES[*,1,5]
    WIDGET_CONTROL, info.UP6_WID, SET_SLIDER_MIN=slidinfo[0]
    WIDGET_CONTROL, info.UP6_WID, SET_SLIDER_MAX=slidinfo[2]
    WIDGET_CONTROL,info.UP6_WID, set_value = slidinfo[1]
    
    ; r7
    
    slidinfo = info.RANGES[*,0,6]
    WIDGET_CONTROL, info.LOW7_WID,  SET_SLIDER_MIN=slidinfo[0]
    WIDGET_CONTROL, info.LOW7_WID, SET_SLIDER_MAX=slidinfo[2]
    WIDGET_CONTROL,info.LOW7_WID, set_value =slidinfo[1]
    
    slidinfo = info.RANGES[*,1,6]
    WIDGET_CONTROL, info.UP7_WID, SET_SLIDER_MIN=slidinfo[0]
    WIDGET_CONTROL, info.UP7_WID, SET_SLIDER_MAX=slidinfo[2]
    WIDGET_CONTROL,info.UP7_WID, set_value = slidinfo[1]
    
    ; r8
    
    slidinfo = info.RANGES[*,0,7]
    WIDGET_CONTROL, info.LOW8_WID,  SET_SLIDER_MIN=slidinfo[0]
    WIDGET_CONTROL, info.LOW8_WID, SET_SLIDER_MAX=slidinfo[2]
    WIDGET_CONTROL,info.LOW8_WID, set_value = slidinfo[1]
    
    slidinfo = info.RANGES[*,1,7]
    WIDGET_CONTROL, info.UP8_WID, SET_SLIDER_MIN=slidinfo[0]
    WIDGET_CONTROL, info.UP8_WID, SET_SLIDER_MAX=slidinfo[2]
    WIDGET_CONTROL,info.UP8_WID, set_value = slidinfo[1]
    
    ; r9
    
    slidinfo = info.RANGES[*,0,8]
    WIDGET_CONTROL, info.LOW9_WID,  SET_SLIDER_MIN=slidinfo[0]
    WIDGET_CONTROL, info.LOW9_WID, SET_SLIDER_MAX=slidinfo[2]
    WIDGET_CONTROL,info.LOW9_WID, set_value = slidinfo[1]
    
    slidinfo = info.RANGES[*,1,8]
    WIDGET_CONTROL, info.UP9_WID, SET_SLIDER_MIN=slidinfo[0]
    WIDGET_CONTROL, info.UP9_WID, SET_SLIDER_MAX=slidinfo[2]
    WIDGET_CONTROL,info.UP9_WID, set_value =  slidinfo[1]
    
    ; r10
    
    slidinfo = info.RANGES[*,0,9]
    WIDGET_CONTROL, info.LOW10_WID,  SET_SLIDER_MIN=slidinfo[0]
    WIDGET_CONTROL, info.LOW10_WID, SET_SLIDER_MAX=slidinfo[2]
    WIDGET_CONTROL,info.LOW10_WID, set_value = slidinfo[1]
    
    slidinfo = info.RANGES[*,1,9]
    WIDGET_CONTROL, info.UP10_WID, SET_SLIDER_MIN=slidinfo[0]
    WIDGET_CONTROL, info.UP10_WID, SET_SLIDER_MAX=slidinfo[2]
    WIDGET_CONTROL,info.UP10_WID, set_value = slidinfo[1]
    
    ; r11
    
    slidinfo = info.RANGES[*,0,10]
    WIDGET_CONTROL, info.LOW11_WID, SET_SLIDER_MIN=slidinfo[0]
    WIDGET_CONTROL, info.LOW11_WID, SET_SLIDER_MAX=slidinfo[2]
    WIDGET_CONTROL,info.LOW11_WID, set_value = slidinfo[1]
    
    
    slidinfo = info.RANGES[*,1,10]
    WIDGET_CONTROL, info.UP11_WID, SET_SLIDER_MIN=slidinfo[0]
    WIDGET_CONTROL, info.UP11_WID, SET_SLIDER_MAX=slidinfo[2]
    WIDGET_CONTROL,info.UP11_WID, set_value = slidinfo[1]
    
    ; r12
    
    slidinfo = info.RANGES[*,0,11]
    WIDGET_CONTROL, info.LOW12_WID, SET_SLIDER_MIN=slidinfo[0]
    WIDGET_CONTROL, info.LOW12_WID, SET_SLIDER_MAX=slidinfo[2]
    WIDGET_CONTROL,info.LOW12_WID, set_value = slidinfo[1]
    
    
    slidinfo = info.RANGES[*,1,11]
    WIDGET_CONTROL, info.UP12_WID, SET_SLIDER_MIN=slidinfo[0]
    WIDGET_CONTROL, info.UP12_WID, SET_SLIDER_MAX=slidinfo[2]
    WIDGET_CONTROL,info.UP12_WID, set_value = slidinfo[1]
    
    ; r13
    
    slidinfo = info.RANGES[*,0,12]
    WIDGET_CONTROL, info.LOW13_WID, SET_SLIDER_MIN=slidinfo[0]
    WIDGET_CONTROL, info.LOW13_WID, SET_SLIDER_MAX=slidinfo[2]
    WIDGET_CONTROL,info.LOW13_WID, set_value = slidinfo[1]
    
    
    slidinfo = info.RANGES[*,1,12]
    WIDGET_CONTROL, info.UP13_WID, SET_SLIDER_MIN=slidinfo[0]
    WIDGET_CONTROL, info.UP13_WID, SET_SLIDER_MAX=slidinfo[2]
    WIDGET_CONTROL,info.UP13_WID, set_value = slidinfo[1]
    
    ; rBroad1
    
    slidinfo = info.RANGES_BROAD1[*,0]
    WIDGET_CONTROL, info.LOWBROAD1_WID, SET_SLIDER_MIN=slidinfo[0]
    WIDGET_CONTROL, info.LOWBROAD1_WID, SET_SLIDER_MAX=slidinfo[2]
    WIDGET_CONTROL, info.LOWBROAD1_WID, set_value = slidinfo[1]
    
    
    slidinfo = info.RANGES_BROAD1[*,1]
    WIDGET_CONTROL, info.UPBROAD1_WID, SET_SLIDER_MIN=slidinfo[0]
    WIDGET_CONTROL, info.UPBROAD1_WID, SET_SLIDER_MAX=slidinfo[2]
    WIDGET_CONTROL, info.UPBROAD1_WID, set_value = slidinfo[1]
    
    ; rUser
    slidinfo[1] = STRING(info.RANGES_USER[0], format ='(f7.2)')  & WIDGET_CONTROL,info.LOWUSER_WID, set_value = slidinfo[1]
    slidinfo[1] =  STRING(info.RANGES_USER[1], format ='(f7.2)')& WIDGET_CONTROL,info.UPUSER_WID, set_value = slidinfo[1]
    
    RETURN, info
END