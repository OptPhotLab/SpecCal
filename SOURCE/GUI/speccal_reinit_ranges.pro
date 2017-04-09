;+
;:ROUTINE NAME:
;SPECCAL_REINIT_RANGES
;
; :PURPOSE:
;   Reinitialize the ranges if INPUT or MODTRAN file is changed, or if one of the "Reset" buttons is pressed
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
FUNCTION SPECCAL_REINIT_RANGES, info

    ; If input file is present update in_file characteristics

    IF info.IN_FILE NE '' THEN BEGIN
    
        WIDGET_CONTROL, info.IN_WID, set_value=info.IN_FILE_LAB  ; Modify Label on the Input File Widget
        WIDGET_CONTROL, info.OUT_WID, set_value=info.OUT_FILE_LAB  ; Modify Label on the Input File Widge
        info_in =  SPECCAL_RDCSV(info.IN_FILE)
        info.INMIN = MIN(info_in.WL)
        info.INMAX = MAX(info_in.WL)
        info.INFWHM = info_in.FWHM
        info.NOMFWHM = info_in.FWHM
        info.INSS = (info.INMAX-info.INMIN)/N_ELEMENTS(info_in.WL)
        ;        WIDGET_CONTROL, info.NOMFWHM_WID, SET_VALUE=info.NOMFWHM
        WIDGET_CONTROL, info.INMIN_WID, SET_VALUE=STRING(info.INMIN,format = '(f7.2)')  ; Modify MODTRAN min characteristics label
        WIDGET_CONTROL, info.INMAX_WID, SET_VALUE=STRING(info.INMAX,format = '(f7.2)')   ; Modify MODTRAN max characteristics label
        WIDGET_CONTROL, info.INSS_WID, SET_VALUE=STRING(info.INSS,format = '(f7.2)')   ; Modify MODTRAN SS characteristics label
        WIDGET_CONTROL, info.INFWHM_WID, SET_VALUE=STRING(info.INFWHM,format = '(f8.2)')   ; Modify MODTRAN FWHM characteristics label
        
    ENDIF
    
    ; If MODTRAN  file is present update mod_file characteristics
    
    IF info.MOD_FILE NE '' THEN BEGIN
        WIDGET_CONTROL, info.MOD_WID, set_value=info.MOD_FILE_LAB  ; Modify Label on the Output File Widget
        
        ; Get Characteristics of MODTRAN File
        info_csv = SPECCAL_RDCSV(info.MOD_FILE)
        info.MOD4MIN=MIN(info_csv.WL)
        info.MOD4MAX=MAX(info_csv.WL)
        info.MOD4SS = info_csv.WL[1]-info_csv.WL[0]
        info.MOD4FWHM = info_csv.FWHM
        ; Update characteristics widgets
        WIDGET_CONTROL, info.MOD4MIN_WID, SET_VALUE=STRING(info.MOD4MIN,format = '(f7.2)')  ; Modify MODTRAN min characteristics label
        WIDGET_CONTROL, info.MOD4MAX_WID, SET_VALUE=STRING(info.MOD4MAX,format = '(f7.2)')   ; Modify MODTRAN max characteristics label
        WIDGET_CONTROL, info.MOD4SS_WID, SET_VALUE=STRING(info.MOD4SS,format = '(f7.2)')   ; Modify MODTRAN SS characteristics label
        WIDGET_CONTROL, info.MOD4FWHM_WID, SET_VALUE=STRING(info.MOD4FWHM,format = '(f7.2)')   ; Modify MODTRAN FWHM characteristics label
        
        
    ENDIF
    
    ; If both files are present, update the spectral ranges
    IF info.IN_FILE NE '' AND info.MOD_FILE NE '' THEN BEGIN
    
        info_initranges = SPECCAL_INITRANGES(info)
        info.RANGES = info_initranges.RANGES
        info.RANGES_BROAD1 = info_initranges.RANGES_BROAD1
        ;        info.RANGES_BROAD2 = info_initranges.RANGES_BROAD2
        info.RANGES_USER = info_initranges.RANGES_USER
        
        slidinfo = info.RANGES[*,0,0]
        WIDGET_CONTROL, info.LOW1_WID, SET_SLIDER_MIN=slidinfo[0]
        WIDGET_CONTROL, info.LOW1_WID, SET_SLIDER_MAX=slidinfo[2]
        IF slidinfo[1] GE slidinfo[0] AND slidinfo[1] LE slidinfo[2] THEN WIDGET_CONTROL,info.LOW1_WID, set_value = slidinfo[1] $
        ELSE WIDGET_CONTROL,info.LOW1_WID, set_value = slidinfo[0]
        
        slidinfo = info.RANGES[*,1,0]
        WIDGET_CONTROL, info.UP1_WID, SET_SLIDER_MIN=slidinfo[0]
        WIDGET_CONTROL, info.UP1_WID, SET_SLIDER_MAX=slidinfo[2]
        IF slidinfo[1] GE slidinfo[0] AND slidinfo[1] LE slidinfo[2] THEN WIDGET_CONTROL,info.UP1_WID, set_value = slidinfo[1] $
        ELSE WIDGET_CONTROL,info.UP1_WID, set_value = slidinfo[0]
        
        ; r2
        
        slidinfo = info.RANGES[*,0,1]
        WIDGET_CONTROL, info.LOW2_WID, SET_SLIDER_MIN=slidinfo[0]
        WIDGET_CONTROL, info.LOW2_WID,SET_SLIDER_MAX=slidinfo[2]
        IF slidinfo[1] GE slidinfo[0] AND slidinfo[1] LE slidinfo[2] THEN WIDGET_CONTROL,info.LOW2_WID, set_value = slidinfo[1] $
        ELSE WIDGET_CONTROL,info.LOW2_WID, set_value = slidinfo[0]
        
        
        slidinfo = info.RANGES[*,1,1]
        WIDGET_CONTROL, info.UP2_WID, SET_SLIDER_MIN=slidinfo[0]
        WIDGET_CONTROL, info.UP2_WID, SET_SLIDER_MAX=slidinfo[2]
        IF slidinfo[1] GE slidinfo[0] AND slidinfo[1] LE slidinfo[2] THEN WIDGET_CONTROL,info.UP2_WID, set_value = slidinfo[1] $
        ELSE WIDGET_CONTROL,info.UP2_WID, set_value = slidinfo[0]
        ; r3
        
        slidinfo = info.RANGES[*,0,2]
        WIDGET_CONTROL, info.LOW3_WID, SET_SLIDER_MIN=slidinfo[0]
        WIDGET_CONTROL, info.LOW3_WID, SET_SLIDER_MAX=slidinfo[2]
        IF slidinfo[1] GE slidinfo[0] AND slidinfo[1] LE slidinfo[2] THEN WIDGET_CONTROL,info.LOW3_WID, set_value = slidinfo[1] $
        ELSE WIDGET_CONTROL,info.LOW3_WID, set_value = slidinfo[0]
        
        slidinfo = info.RANGES[*,1,2]
        WIDGET_CONTROL, info.UP3_WID, SET_SLIDER_MIN=slidinfo[0]
        WIDGET_CONTROL, info.UP3_WID, SET_SLIDER_MAX=slidinfo[2]
        IF slidinfo[1] GE slidinfo[0] AND slidinfo[1] LE slidinfo[2] THEN WIDGET_CONTROL,info.UP3_WID, set_value = slidinfo[1] $
        ELSE WIDGET_CONTROL,info.UP3_WID, set_value = slidinfo[0]
        
        ; r4
        
        slidinfo = info.RANGES[*,0,3]
        WIDGET_CONTROL, info.LOW4_WID,  SET_SLIDER_MIN=slidinfo[0]
        WIDGET_CONTROL, info.LOW4_WID,SET_SLIDER_MAX=slidinfo[2]
        IF slidinfo[1] GE slidinfo[0] AND slidinfo[1] LE slidinfo[2] THEN WIDGET_CONTROL,info.LOW4_WID, set_value = slidinfo[1] $
        ELSE WIDGET_CONTROL,info.LOW4_WID, set_value = slidinfo[0]
        
        slidinfo = info.RANGES[*,1,3]
        WIDGET_CONTROL, info.UP4_WID, SET_SLIDER_MIN=slidinfo[0]
        WIDGET_CONTROL, info.UP4_WID, SET_SLIDER_MAX=slidinfo[2]
        IF slidinfo[1] GE slidinfo[0] AND slidinfo[1] LE slidinfo[2] THEN WIDGET_CONTROL,info.UP4_WID, set_value = slidinfo[1] $
        ELSE WIDGET_CONTROL,info.UP4_WID, set_value = slidinfo[0]
        
        ; r5
        
        slidinfo = info.RANGES[*,0,4]
        WIDGET_CONTROL, info.LOW5_WID,  SET_SLIDER_MIN=slidinfo[0]
        WIDGET_CONTROL, info.LOW5_WID, SET_SLIDER_MAX=slidinfo[2]
        IF slidinfo[1] GE slidinfo[0] AND slidinfo[1] LE slidinfo[2] THEN WIDGET_CONTROL,info.LOW5_WID, set_value = slidinfo[1] $
        ELSE WIDGET_CONTROL,info.LOW5_WID, set_value = slidinfo[0]
        
        slidinfo = info.RANGES[*,1,4]
        WIDGET_CONTROL, info.UP5_WID, SET_SLIDER_MIN=slidinfo[0]
        WIDGET_CONTROL, info.UP5_WID, SET_SLIDER_MAX=slidinfo[2]
        IF slidinfo[1] GE slidinfo[0] AND slidinfo[1] LE slidinfo[2] THEN WIDGET_CONTROL,info.UP5_WID, set_value = slidinfo[1] $
        ELSE WIDGET_CONTROL,info.UP5_WID, set_value = slidinfo[0]
        
        ; r6
        
        slidinfo = info.RANGES[*,0,5]
        WIDGET_CONTROL, info.LOW6_WID, SET_SLIDER_MIN=slidinfo[0]
        WIDGET_CONTROL, info.LOW6_WID, SET_SLIDER_MAX=slidinfo[2]
        IF slidinfo[1] GE slidinfo[0] AND slidinfo[1] LE slidinfo[2] THEN WIDGET_CONTROL,info.LOW6_WID, set_value = slidinfo[1] $
        ELSE WIDGET_CONTROL,info.LOW6_WID, set_value = slidinfo[0]
        
        slidinfo = info.RANGES[*,1,5]
        WIDGET_CONTROL, info.UP6_WID, SET_SLIDER_MIN=slidinfo[0]
        WIDGET_CONTROL, info.UP6_WID, SET_SLIDER_MAX=slidinfo[2]
        IF slidinfo[1] GE slidinfo[0] AND slidinfo[1] LE slidinfo[2] THEN WIDGET_CONTROL,info.UP6_WID, set_value = slidinfo[1] $
        ELSE WIDGET_CONTROL,info.UP6_WID, set_value = slidinfo[0]
        
        ; r7
        
        slidinfo = info.RANGES[*,0,6]
        WIDGET_CONTROL, info.LOW7_WID,  SET_SLIDER_MIN=slidinfo[0]
        WIDGET_CONTROL, info.LOW7_WID, SET_SLIDER_MAX=slidinfo[2]
        IF slidinfo[1] GE slidinfo[0] AND slidinfo[1] LE slidinfo[2] THEN WIDGET_CONTROL,info.LOW7_WID, set_value = slidinfo[1] $
        ELSE WIDGET_CONTROL,info.LOW7_WID, set_value = slidinfo[0]
        
        slidinfo = info.RANGES[*,1,6]
        WIDGET_CONTROL, info.UP7_WID, SET_SLIDER_MIN=slidinfo[0]
        WIDGET_CONTROL, info.UP7_WID, SET_SLIDER_MAX=slidinfo[2]
        IF slidinfo[1] GE slidinfo[0] AND slidinfo[1] LE slidinfo[2] THEN WIDGET_CONTROL,info.UP7_WID, set_value = slidinfo[1] $
        ELSE WIDGET_CONTROL,info.UP7_WID, set_value = slidinfo[0]
        
        ; r8
        
        slidinfo = info.RANGES[*,0,7]
        WIDGET_CONTROL, info.LOW8_WID,  SET_SLIDER_MIN=slidinfo[0]
        WIDGET_CONTROL, info.LOW8_WID, SET_SLIDER_MAX=slidinfo[2]
        IF slidinfo[1] GE slidinfo[0] AND slidinfo[1] LE slidinfo[2] THEN WIDGET_CONTROL,info.LOW8_WID, set_value = slidinfo[1] $
        ELSE WIDGET_CONTROL,info.LOW8_WID, set_value = slidinfo[0]
        
        slidinfo = info.RANGES[*,1,7]
        WIDGET_CONTROL, info.UP8_WID, SET_SLIDER_MIN=slidinfo[0]
        WIDGET_CONTROL, info.UP8_WID, SET_SLIDER_MAX=slidinfo[2]
        IF slidinfo[1] GE slidinfo[0] AND slidinfo[1] LE slidinfo[2] THEN WIDGET_CONTROL,info.UP8_WID, set_value = slidinfo[1] $
        ELSE WIDGET_CONTROL,info.UP8_WID, set_value = slidinfo[0]
        
        ; r9
        
        slidinfo = info.RANGES[*,0,8]
        WIDGET_CONTROL, info.LOW9_WID,  SET_SLIDER_MIN=slidinfo[0]
        WIDGET_CONTROL, info.LOW9_WID, SET_SLIDER_MAX=slidinfo[2]
        IF slidinfo[1] GE slidinfo[0] AND slidinfo[1] LE slidinfo[2] THEN WIDGET_CONTROL,info.LOW9_WID, set_value = slidinfo[1] $
        ELSE WIDGET_CONTROL,info.LOW9_WID, set_value = slidinfo[0]
        
        slidinfo = info.RANGES[*,1,8]
        WIDGET_CONTROL, info.UP9_WID, SET_SLIDER_MIN=slidinfo[0]
        WIDGET_CONTROL, info.UP9_WID, SET_SLIDER_MAX=slidinfo[2]
        IF slidinfo[1] GE slidinfo[0] AND slidinfo[1] LE slidinfo[2] THEN WIDGET_CONTROL,info.UP9_WID, set_value = slidinfo[1] $
        ELSE WIDGET_CONTROL,info.UP9_WID, set_value = slidinfo[0]
        
        ; r10
        
        slidinfo = info.RANGES[*,0,9]
        WIDGET_CONTROL, info.LOW10_WID,  SET_SLIDER_MIN=slidinfo[0]
        WIDGET_CONTROL, info.LOW10_WID, SET_SLIDER_MAX=slidinfo[2]
        IF slidinfo[1] GE slidinfo[0] AND slidinfo[1] LE slidinfo[2] THEN WIDGET_CONTROL,info.LOW10_WID, set_value = slidinfo[1] $
        ELSE WIDGET_CONTROL,info.LOW10_WID, set_value = slidinfo[0]
        
        slidinfo = info.RANGES[*,1,9]
        WIDGET_CONTROL, info.UP10_WID, SET_SLIDER_MIN=slidinfo[0]
        WIDGET_CONTROL, info.UP10_WID, SET_SLIDER_MAX=slidinfo[2]
        IF slidinfo[1] GE slidinfo[0] AND slidinfo[1] LE slidinfo[2] THEN WIDGET_CONTROL,info.UP10_WID, set_value = slidinfo[1] $
        ELSE WIDGET_CONTROL,info.UP10_WID, set_value = slidinfo[0]
        
        ; r11
        
        slidinfo = info.RANGES[*,0,10]
        WIDGET_CONTROL, info.LOW11_WID, SET_SLIDER_MIN=slidinfo[0]
        WIDGET_CONTROL, info.LOW11_WID, SET_SLIDER_MAX=slidinfo[2]
        IF slidinfo[1] GE slidinfo[0] AND slidinfo[1] LE slidinfo[2] THEN WIDGET_CONTROL,info.LOW11_WID, set_value = slidinfo[1] $
        ELSE WIDGET_CONTROL,info.LOW11_WID, set_value = slidinfo[0]
        
        
        slidinfo = info.RANGES[*,1,10]
        WIDGET_CONTROL, info.UP11_WID, SET_SLIDER_MIN=slidinfo[0]
        WIDGET_CONTROL, info.UP11_WID, SET_SLIDER_MAX=slidinfo[2]
        IF slidinfo[1] GE slidinfo[0] AND slidinfo[1] LE slidinfo[2] THEN WIDGET_CONTROL,info.UP11_WID, set_value = slidinfo[1] $
        ELSE WIDGET_CONTROL,info.UP11_WID, set_value = slidinfo[0]
        
        ; r12
        
        slidinfo = info.RANGES[*,0,11]
        WIDGET_CONTROL, info.LOW12_WID, SET_SLIDER_MIN=slidinfo[0]
        WIDGET_CONTROL, info.LOW12_WID, SET_SLIDER_MAX=slidinfo[2]
        IF slidinfo[1] GE slidinfo[0] AND slidinfo[1] LE slidinfo[2] THEN WIDGET_CONTROL,info.LOW12_WID, set_value = slidinfo[1] $
        ELSE WIDGET_CONTROL,info.LOW12_WID, set_value = slidinfo[0]
        
        
        slidinfo = info.RANGES[*,1,11]
        WIDGET_CONTROL, info.UP12_WID, SET_SLIDER_MIN=slidinfo[0]
        WIDGET_CONTROL, info.UP12_WID, SET_SLIDER_MAX=slidinfo[2]
        IF slidinfo[1] GE slidinfo[0] AND slidinfo[1] LE slidinfo[2] THEN WIDGET_CONTROL,info.UP12_WID, set_value = slidinfo[1] $
        ELSE WIDGET_CONTROL,info.UP12_WID, set_value = slidinfo[0]
        
        ; r13
        
        slidinfo = info.RANGES[*,0,12]
        WIDGET_CONTROL, info.LOW13_WID, SET_SLIDER_MIN=slidinfo[0]
        WIDGET_CONTROL, info.LOW13_WID, SET_SLIDER_MAX=slidinfo[2]
        IF slidinfo[1] GE slidinfo[0] AND slidinfo[1] LE slidinfo[2] THEN WIDGET_CONTROL,info.LOW13_WID, set_value = slidinfo[1] $
        ELSE WIDGET_CONTROL,info.LOW13_WID, set_value = slidinfo[0]
        
        
        slidinfo = info.RANGES[*,1,12]
        WIDGET_CONTROL, info.UP13_WID, SET_SLIDER_MIN=slidinfo[0]
        WIDGET_CONTROL, info.UP13_WID, SET_SLIDER_MAX=slidinfo[2]
        IF slidinfo[1] GE slidinfo[0] AND slidinfo[1] LE slidinfo[2] THEN WIDGET_CONTROL,info.UP13_WID, set_value = slidinfo[1] $
        ELSE WIDGET_CONTROL,info.UP13_WID, set_value = slidinfo[0]
        
        ; rBroad1
        
        slidinfo = info.RANGES_BROAD1[*,0]
        WIDGET_CONTROL, info.LOWBROAD1_WID, SET_SLIDER_MIN=slidinfo[0]
        WIDGET_CONTROL, info.LOWBROAD1_WID, SET_SLIDER_MAX=slidinfo[2]
        IF slidinfo[1] GE slidinfo[0] AND slidinfo[1] LE slidinfo[2] THEN WIDGET_CONTROL,info.LOWBROAD1_WID, set_value = slidinfo[1] $
        ELSE WIDGET_CONTROL,info.LOWBROAD1_WID, set_value = slidinfo[0]
        
        
        slidinfo = info.RANGES_BROAD1[*,1]
        WIDGET_CONTROL, info.UPBROAD1_WID, SET_SLIDER_MIN=slidinfo[0]
        WIDGET_CONTROL, info.UPBROAD1_WID, SET_SLIDER_MAX=slidinfo[2]
        WIDGET_CONTROL, info.UPBROAD1_WID, set_value = slidinfo[1]
        IF slidinfo[1] GE slidinfo[0] AND slidinfo[1] LE slidinfo[2] THEN WIDGET_CONTROL,info.UPBROAD1_WID, set_value = slidinfo[1] $
        ELSE WIDGET_CONTROL,info.UPBROAD1_WID, set_value = slidinfo[0]
        
        ; rBroad2
        
        ;        slidinfo = info.RANGES_BROAD2[*,0]
        ;        WIDGET_CONTROL, info.LOWBROAD2_WID, SET_SLIDER_MIN=slidinfo[0]
        ;        WIDGET_CONTROL, info.LOWBROAD2_WID, SET_SLIDER_MAX=slidinfo[2]
        ;        WIDGET_CONTROL, info.LOWBROAD2_WID, set_value = slidinfo[1]
        ;         IF slidinfo[1] GE slidinfo[0] AND slidinfo[1] LE slidinfo[2] THEN WIDGET_CONTROL,info.LOWBROAD2_WID, set_value = slidinfo[1] $
        ;        ELSE WIDGET_CONTROL,info.LOWBROAD2_WID, set_value = slidinfo[0]
        ;
        ;        slidinfo = info.RANGES_BROAD2[*,1]
        ;        WIDGET_CONTROL, info.UPBROAD2_WID, SET_SLIDER_MIN=slidinfo[0]
        ;        WIDGET_CONTROL, info.UPBROAD2_WID, SET_SLIDER_MAX=slidinfo[2]
        ;        WIDGET_CONTROL, info.UPBROAD2_WID, set_value = slidinfo[1]
        ;         IF slidinfo[1] GE slidinfo[0] AND slidinfo[1] LE slidinfo[2] THEN WIDGET_CONTROL,info.UPBROAD2_WID, set_value = slidinfo[1] $
        ;        ELSE WIDGET_CONTROL,info.UPBROAD2_WID, set_value = slidinfo[0]
        
        ; rUser
        tmp = STRING(info.RANGES_USER[0], format ='(f7.2)')
        slidinfo[1] = tmp
        WIDGET_CONTROL,info.LOWUSER_WID, set_value = tmp
        tmp = STRING(info.RANGES_USER[1], format ='(f7.2)')
        slidinfo[1] = tmp
        WIDGET_CONTROL,info.UPUSER_WID, set_value =  tmp
        ;  info.ranges = ranges
        info_sens = SPECCAL_SETSENS(info)
        info = info_sens
        
    ENDIF
    
    RETURN, info
END