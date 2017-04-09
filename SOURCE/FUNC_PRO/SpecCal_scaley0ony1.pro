;+
; :Description:
;   Rescales the input array Y0 on the Min-Max of the Input Array Y1
;   If the MEDIANX Keyword is set , the rescaling is performed on a "Median smoothed" version of Y1 (width = 3)
;
; :Params:
;    Y0: Input array to be rescaled
;    Y1: Reference Input Array
;
; :Keywords:
;    MEDIANX: If set, a median smoothing is applied on Y1 before computing the rescaling
;
; :OUTPUTS:
;   Y0on1: 
;
; :REQUIRES:
;
; :NOTES:
;
; :AUTHOR: Michele Meroni - 
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
; :HISTORY:
;
; Created: 29-apr-2009
;-

FUNCTION SPECCAL_SCALEY0ONY1, Y0, Y1, MEDIANX = med

    IF  KEYWORD_SET(med) THEN Y0m=MEDIAN(Y0, 3, /DOUBLE) ELSE Y0m=Y0
    Y0on1=(Y0-MIN(Y0m))/(MAX(Y0m)-MIN(Y0m))* (MAX(Y1)-MIN(Y1))+ MIN(Y1)
    RETURN, Y0on1
    
END