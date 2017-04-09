;+
;:ROUTINE NAME:
;SpecCal_INITRANGES
;
; :PURPOSE:
;   Initialize the ranges of the different spectral windows as a function of Spectral Sampling and Scale
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
FUNCTION SPECCAL_INITRANGES,info

    ; Defining the central wavelenghts of each range
    n_ranges = N_ELEMENTS(info.RANGES[0,0,*])
    ranges = info.RANGES*0
    ranges_Broad1 = info.RANGES_BROAD1*0
    ranges_User = info.RANGES_USER*0
    
    ; Set  ranges at default
    cen_ranges = DBLARR(2,n_ranges)
    
    cen_ranges[*,0]=[382.0D, 397.0D]
    cen_ranges[*,1]=[410.0D, 411.0D]
    cen_ranges[*,2]=[430.0D, 439.0D]
    cen_ranges[*,3]=[486.0D, 496.0D]
    cen_ranges[*,4]=[516.0D, 519.0D]
    cen_ranges[*,5]=[589.0D, 590.0D]
    cen_ranges[*,6]=[656.0D, 657.0D]
    cen_ranges[*,7]=[686.0D, 689.0D]
    cen_ranges[*,8]=[716.0D, 735.0D]
    cen_ranges[*,9]=[759.0D, 771.0D]
    cen_ranges[*,10]=[809.0D, 841.0D]
    cen_ranges[*,11]=[849.0D, 867.0D]
    cen_ranges[*,12]=[925.0D, 980.0D]
    cen_ranges_Broad1=[info.MOD4MIN > info.INMIN, info.MOD4MAX < info.INMAX]    ;Broad1
    
    cen_ranges_User =[0.0D, 0.0D]    ;User
    
    ; Compute new ranges and slider min and max as a function of spectral sampling and filter widt
    
    FOR r = 0, n_ranges-1 DO BEGIN
    
        ; Lower
        ranges [1,0,r] = cen_ranges [0,r] - info.INSS*(info.FLTW)                ; Value
        tmp = ranges [1,0,r]
        
        ranges [0,0,r] = ((tmp-info.INSS*(info.FLTW))>info.INMIN > info.MOD4MIN)< info.MOD4MAX  ; Min
        ranges [2,0,r] = ((tmp+info.INSS*(info.FLTW))<info.INMAX < info.MOD4MAX)> info.MOD4MIN  ; Max
        
        ; Upper
        ranges [1,1,r] = cen_ranges [1,r] + info.INSS*(info.FLTW)                ; Value
        tmp = ranges [1,1,r]
        ranges [0,1,r] = ((tmp-info.INSS*(info.FLTW))>info.INMIN > info.MOD4MIN)< info.MOD4MAX  ; Min
        ranges [2,1,r] = ((tmp+info.INSS*(info.FLTW))<info.INMAX < info.MOD4MAX)> info.MOD4MIN  ;Max
        
        
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
    
    info_initranges = {ranges:ranges, ranges_Broad1:ranges_Broad1, ranges_User:ranges_User}
    
    
    RETURN, info_initranges
END
