;+
;:ROUTINE NAME:
;SpecCal_FIND_IND_OF_NEAREST
;
; :PURPOSE:
;   Find the index of the element within an array which is closer to a selected value
;
; :Params:
;    array: Input array
;    value: Value to be "located" within the array
; 																																						;
; :RETURNS:
;   sub: Index of the alement of Array which is closest to Value
; 
; :REQUIRES:
;
; :NOTES:
;
; :HISTORY:
;
;: Created:	16-feb-2010
;
; :AUTHOR: Michele Meroni, Lorenzo Busetto.
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

FUNCTION SPECCAL_FIND_IND_OF_NEAREST, array, value

    delta=MIN((array-value), sub, /ABSOLUTE, /NAN)
    IF N_ELEMENTS(sub) GT 2 THEN sub=-1
    IF N_ELEMENTS(sub) EQ 2 THEN sub=sub[0]
    RETURN, sub
    
END