;+
;:ROUTINE NAME:
;SpecCal_SETSENS
;
; :PURPOSE:
;   Check the wavelenght ranges of input and modtran files to find out if a considered range can be processed. If not, set the sensitivity of
;   the corresponding widget to 0 and set the range to not selected
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
FUNCTION SPECCAL_SETSENS,info

    lowerWl= info.INMIN
    upperWl=info.INMAX
    fwhm = info.NOMFWHM
    ss = info.INSS
    ranges = info.RANGES
    n_ranges = N_ELEMENTS(ranges[0,0,*])
    out = {selsens:INTARR(n_ranges), fwhm: fwhm, ss:ss, lowerWl:lowerWl, upperWl:upperWl}
    selsens = INTARR(n_ranges)
    
    ;Set sensitivity to 1 only if
    ;  1)the minimum WL of the input file and of the modtran file is lower than the maximum value computed for the slider
    ;  2)the maximum WL of the input file and of the modtran file is higher than the minimum value computed for the slider
    FOR r_ind = 0L, N_ELEMENTS(ranges[0,0,*])-1 DO BEGIN
        IF (lowerWl LE (ranges [2,0,r_ind])) $
            AND (upperWl GE (ranges [0,1,r_ind])) $
            AND (info.MOD4MIN LE (ranges [2,0,r_ind]))$
            AND (info.MOD4MAX GE (ranges [0,1,r_ind])) $
            AND  (ranges [2,0,r_ind]) NE  (ranges [0,0,r_ind]) $
            AND  (ranges [2,1,r_ind]) NE  (ranges [0,1,r_ind]) $
            THEN selsens [r_ind] = 1 ELSE selsens [r_ind] = 0
    ENDFOR
    
    ; If input file doesn't "intersect" modtran file then remove sensitivity to Broad intervals
    IF lowerwl GT info.MOD4MAX OR $
        upperwl LT info.MOD4MIN THEN BEGIN
        selsens_Broad1 = 0
;        selsens_Broad2 = 0
        selsens_user = 0
    ENDIF ELSE BEGIN
        selsens_Broad1 = 1
;        selsens_Broad2 = 1
        selsens_user = 1
    ENDELSE
    
    ; Set the sensitivities
    WIDGET_CONTROL, info.R1_WID, sensitive = selsens [0]
    WIDGET_CONTROL, info.R2_WID, sensitive = selsens [1]
    WIDGET_CONTROL, info.R3_WID, sensitive = selsens [2]
    WIDGET_CONTROL, info.R4_WID, sensitive = selsens [3]
    WIDGET_CONTROL, info.R5_WID, sensitive = selsens [4]
    WIDGET_CONTROL, info.R6_WID, sensitive = selsens [5]
    WIDGET_CONTROL, info.R7_WID, sensitive = selsens [6]
    WIDGET_CONTROL, info.R8_WID, sensitive = selsens [7]
    WIDGET_CONTROL, info.R9_WID, sensitive = selsens [8]
    WIDGET_CONTROL, info.R10_WID, sensitive = selsens [9]
    WIDGET_CONTROL, info.R11_WID, sensitive = selsens [10]
    WIDGET_CONTROL, info.R12_WID, sensitive = selsens [11]
    WIDGET_CONTROL, info.R13_WID, sensitive = selsens [12]
    WIDGET_CONTROL, info.RBROAD1_WID, sensitive = selsens_Broad1
;    WIDGET_CONTROL, info.RBROAD2_WID, sensitive = selsens_Broad2
    WIDGET_CONTROL, info.RUSER_WID, sensitive = selsens_user
    
    ; Set the "selected" info to 0 for ranges that cannot be processed
    WIDGET_CONTROL, info.SEL1_WID, set_value = selsens [0]
    WIDGET_CONTROL, info.SEL2_WID, set_value = selsens [1]
    WIDGET_CONTROL, info.SEL3_WID, set_value = selsens [2]
    WIDGET_CONTROL, info.SEL4_WID, set_value = selsens [3]
    WIDGET_CONTROL, info.SEL5_WID, set_value = selsens [4]
    WIDGET_CONTROL, info.SEL6_WID, set_value = selsens [5]
    WIDGET_CONTROL, info.SEL7_WID, set_value = selsens [6]
    WIDGET_CONTROL, info.SEL8_WID, set_value = selsens [7]
    WIDGET_CONTROL, info.SEL9_WID, set_value = selsens [8]
    WIDGET_CONTROL, info.SEL10_WID, set_value = selsens [9]
    WIDGET_CONTROL, info.SEL11_WID, set_value = selsens [10]
    WIDGET_CONTROL, info.SEL12_WID, set_value = selsens [11]
    WIDGET_CONTROL, info.SEL13_WID, set_value = selsens [12]
    WIDGET_CONTROL, info.SELBROAD1_WID, set_value = selsens_Broad1
;    WIDGET_CONTROL, info.SELBROAD2_WID, set_value = selsens_Broad2
    WIDGET_CONTROL, info.SELUSER_WID, sensitive = selsens_user
    ; Set the sensitivities
    WIDGET_CONTROL,info.R1_WID_RG,sensitive = selsens [0]
    WIDGET_CONTROL,info.R2_WID_RG,sensitive = selsens [1]
    WIDGET_CONTROL,info.R3_WID_RG,sensitive = selsens [2]
    WIDGET_CONTROL,info.R4_WID_RG,sensitive = selsens [3]
    WIDGET_CONTROL,info.R5_WID_RG,sensitive = selsens [4]
    WIDGET_CONTROL,info.R6_WID_RG,sensitive = selsens [5]
    WIDGET_CONTROL,info.R7_WID_RG,sensitive = selsens [6]
    WIDGET_CONTROL,info.R8_WID_RG,sensitive = selsens [7]
    WIDGET_CONTROL,info.R9_WID_RG,sensitive = selsens [8]
    WIDGET_CONTROL,info.R10_WID_RG,sensitive = selsens [9]
    WIDGET_CONTROL,info.R11_WID_RG,sensitive = selsens [10]
    WIDGET_CONTROL,info.R12_WID_RG,sensitive = selsens [11]
    WIDGET_CONTROL,info.R13_WID_RG,sensitive = selsens [12]
    WIDGET_CONTROL,info.RBROAD1_WID_RG,sensitive = selsens_Broad1
;    WIDGET_CONTROL,info.RBROAD2_WID_RG,sensitive = selsens_Broad2
    WIDGET_CONTROL, info.RUSER_WID_RG, sensitive = info.RANGESEL_USER
    
    info.RANGESEL = selsens
    info.RANGESEL_BROAD1 = selsens_Broad1
;    info.RANGESEL_BROAD2 = selsens_Broad2
    info.SENSEL = selsens
    info.SENSEL_BROAD1 = selsens_Broad1
;    info.SENSEL_BROAD2 = selsens_Broad2
    info.SENSEL_USER = selsens_user
    RETURN,info
END
