;+
;:ROUTINE NAME:
;SPECCAL_FILTER
;
; :PURPOSE:
;   Apply a smoothing filter of selected type and width on the input array
;
;:PARAMS:
;    array: Input Array
;    filterType: 0 = Mean filter; 1 = Median Filter; 2: Savitzky and Golay
;    filterWing: Half Width of the smoothing kernel
;
; :RETURNS:
;   result: smoothed array
;
; :REQUIRES:
;
; :NOTES:
;
; :HISTORY:
;
;: Created:	16-feb-2010
;
; :AUTHOR:
;
; :AUTHOR: Michele Meroni -
; Environmental Dynamics Remote Sensing Laboratory
; University of Milano-Bicocca
; Milan - Italy (IT)
; 
; ------------------------------------------------------------------------------
; Copyright Lorenzo Busetto and Michele Meroni, 2010
; License: GPL-3
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

FUNCTION SPECCAL_FILTER, array, filterType, filterWing

    CASE filterType OF
        0:BEGIN	;Mean
        Result= SMOOTH(array, filterWing*2+1, /NAN, /EDGE_TRUNCATE)
    END
    1:BEGIN ;Median
    Result= MEDIAN(array, filterWing*2+1, /DOUBLE)
    END
        2:BEGIN	;SavGol
        Result=CONVOL(array, SAVGOL(filterWing, filterWing, 0, 2), /NAN, /EDGE_TRUNCATE)
        END
    ENDCASE

RETURN, result

END
