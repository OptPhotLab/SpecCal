;+
;:ROUTINE NAME:
;SPECCAL_INITRANGES_UPDATE
;
; :PURPOSE:
;   Updates the ranges as a function of the selected "RW" variable
;
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
FUNCTION SPECCAL_INITRANGES_UPDATE,info

    tmp = SPECCAL_INITRANGES(info)
    old_ranges = tmp.RANGES
    ; Get actual selected ranges values
    
    ; Get Ranges
    ; r1
    WIDGET_CONTROL,info.LOW1_WID, get_value = tmp   &   info.RANGES[1,0,0] = tmp
    WIDGET_CONTROL,info.UP1_WID, get_value = tmp   &   info.RANGES[1,1,0] = tmp
    ; r2
    WIDGET_CONTROL,info.LOW2_WID, get_value = tmp   &   info.RANGES[1,0,1] = tmp
    WIDGET_CONTROL,info.UP2_WID, get_value = tmp   &   info.RANGES[1,1,1] = tmp
    ; r3
    WIDGET_CONTROL,info.LOW3_WID, get_value = tmp   &   info.RANGES[1,0,2] = tmp
    WIDGET_CONTROL,info.UP3_WID, get_value = tmp   &   info.RANGES[1,1,2] = tmp
    ; r4
    WIDGET_CONTROL,info.LOW4_WID, get_value = tmp   &   info.RANGES[1,0,3] = tmp
    WIDGET_CONTROL,info.UP4_WID, get_value = tmp   &   info.RANGES[1,1,3] = tmp
    ; r5
    WIDGET_CONTROL,info.LOW5_WID, get_value = tmp   &   info.RANGES[1,0,4] = tmp
    WIDGET_CONTROL,info.UP5_WID, get_value = tmp   &   info.RANGES[1,1,4] = tmp
    ; r6
    WIDGET_CONTROL,info.LOW6_WID, get_value = tmp   &   info.RANGES[1,0,5] = tmp
    WIDGET_CONTROL,info.UP6_WID, get_value = tmp   &   info.RANGES[1,1,5] = tmp
    ; r7
    WIDGET_CONTROL,info.LOW7_WID, get_value = tmp   &   info.RANGES[1,0,6] = tmp
    WIDGET_CONTROL,info.UP7_WID, get_value = tmp   &   info.RANGES[1,1,6] = tmp
    ; r8
    WIDGET_CONTROL,info.LOW8_WID, get_value = tmp   &   info.RANGES[1,0,7] = tmp
    WIDGET_CONTROL,info.UP8_WID, get_value = tmp   &   info.RANGES[1,1,7] = tmp
    ; r9
    WIDGET_CONTROL,info.LOW9_WID, get_value = tmp   &   info.RANGES[1,0,8] = tmp
    WIDGET_CONTROL,info.UP9_WID, get_value = tmp   &   info.RANGES[1,1,8] = tmp
    ; r10
    WIDGET_CONTROL,info.LOW10_WID, get_value = tmp   &   info.RANGES[1,0,9] = tmp
    WIDGET_CONTROL,info.UP10_WID, get_value = tmp   &   info.RANGES[1,1,9] = tmp
    ; r11
    WIDGET_CONTROL,info.LOW11_WID, get_value = tmp   &   info.RANGES[1,0,10] = tmp
    WIDGET_CONTROL,info.UP11_WID, get_value = tmp   &   info.RANGES[1,1,10] = tmp
    ; r12
    WIDGET_CONTROL,info.LOW12_WID, get_value = tmp   &   info.RANGES[1,0,11] = tmp
    WIDGET_CONTROL,info.UP12_WID, get_value = tmp   &   info.RANGES[1,1,11] = tmp
    ; r13
    WIDGET_CONTROL,info.LOW13_WID, get_value = tmp   &   info.RANGES[1,0,12] = tmp
    WIDGET_CONTROL,info.UP13_WID, get_value = tmp   &   info.RANGES[1,1,12] = tmp
    ; rBroad1
    WIDGET_CONTROL,info.LOWBROAD1_WID, get_value = tmp   &   info.RANGES_BROAD1[1,0] = tmp
    WIDGET_CONTROL,info.UPBROAD1_WID, get_value = tmp   &   info.RANGES_BROAD1[1,1] = tmp
    
    ; rUser
    WIDGET_CONTROL,info.LOWUSER_WID, get_value = tmp   &   info.RANGES_USER[0] = tmp
    WIDGET_CONTROL,info.UPUSER_WID, get_value = tmp   &   info.RANGES_USER[1] = tmp
    
    
    ; Get actual ranges values
    n_ranges = N_ELEMENTS(info.RANGES[0,0,*])
    ;ranges = info.RANGES
    ranges = old_ranges
    ranges_Broad1 = info.RANGES_BROAD1
    ranges_User = info.RANGES_USER
    cen_ranges = DBLARR(2,n_ranges)
    right_ranges = DBLARR(2,n_ranges)
    left_ranges = DBLARR(2,n_ranges)
    
    FOR r = 0, n_ranges-1 DO BEGIN
    
        cen_ranges [0,r] = ranges [1,0,r]
        cen_ranges [1,r] = ranges [1,1,r]
        right_ranges [0,r] = ranges [2,0,r]
        right_ranges [1,r] = ranges [2,1,r]
        left_ranges [0,r] = ranges [0,0,r]
        left_ranges [1,r] = ranges [0,1,r]
        
    ENDFOR
    
    cen_ranges_Broad1 =[ranges_broad1 [1,0], ranges_broad1 [1,1]]    ;Broad1
    cen_ranges_User =[0.0D, 0.0D]    ;User
    
    ; Compute new ranges and slider min and max as a function of spectral sampling and filter widt
    
    FOR r = 0, n_ranges-1 DO BEGIN
    
        ; Lower
        ranges [1,0,r] = cen_ranges [0,r]                ; Value
        tmp = old_ranges [1,0,r]
        ranges [0,0,r] = ((tmp-info.INSS*(info.FLTW))>info.INMIN > info.MOD4MIN) ; Min
        ranges [2,0,r] = right_ranges [0,r]  ; Max
        
        ; Upper
        ranges [1,1,r] = cen_ranges [1,r]                 ; Value
        tmp = old_ranges [1,1,r]
        ranges [0,1,r] = left_ranges [1,r]   ; Min
        ranges [2,1,r] = ((tmp+info.INSS*(info.FLTW))<info.INMAX < info.MOD4MAX)  ;Max
        
    ENDFOR
    
    ;Lower Br1
    ranges_broad1 [1,0] = cen_ranges_Broad1 [0]  ; Value = maximum between minimums of modtran and infile
    ranges_broad1 [0,0] = cen_ranges_Broad1 [0]  ; MIn =  maximum between minimums of modtran and infile
    ranges_broad1 [2,0] = (cen_ranges_Broad1 [0]  + (cen_ranges_Broad1 [1] - cen_ranges_Broad1 [0])/2 )  ; min+range/2
    
    ; Upper Br1
    ranges_broad1 [1,1] = cen_ranges_Broad1 [1]  ; Value = minimum between modtran and infile maximums
    ranges_broad1 [0,1] = 1+(cen_ranges_Broad1 [0]  + (cen_ranges_Broad1 [1] - cen_ranges_Broad1 [0])/2 )  ;MIN: 1+ min+range/2
    ranges_broad1 [2,1] = cen_ranges_Broad1 [1] ; MAX  = minimum between modtran and infile maximums
    
    ranges_user = [0.,0.]
    
    info_initranges = {ranges:ranges, ranges_Broad1:ranges_Broad1,  ranges_User:ranges_User}
    
    RETURN, info_initranges
END
