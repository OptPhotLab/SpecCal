;+
;:ROUTINE NAME:
;SPECCAL_TRANSFORM_WL_FROM_VAC_TO_AIR
;
; :PURPOSE:
;
;   Function used to correct the wavelenghts of MODTRAN irradiance from vacuum to air, according to the formula of Edlén 1966
; :INPUTS:
;
; :OUTPUTS:
; 
;   la: Corrected wavelenghts for the MODTRAN irradiance file
;    
;INPUT PARAMS:
;    
;    lv: Wavelenghts of the different channels of the MODTRAN input irradiance file
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
; Michele Meroni
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

FUNCTION SPECCAL_TRANSFORM_WL_FROM_VAC_TO_AIR, lv
    ;	;compute coefficients to trasform l_vac to l_air according to the formula of Edlén 1966
    ;	l_air=dindgen(750)+350.0d
    ;	l_vac=l_air*((1.0000834213+2.40603/100.0d/(double(130)-1000000.0d/l_air^2))+1.5997/10000.0d/(38.9-1000000.0d/l_air^2))
    ;	v2a_factor=l_air/l_vac
    ;	plot, l_vac, v2a_factor, psym=3, yrange=[0.9997,0.99975]
    ;	c=poly_fit(l_vac, v2a_factor, 5, /DOUBLE, YFIT=yfit)
    ;	print, c
    ;	;oplot, l_vac, yfit, color=30000
    ;	oplot, l_vac, c[0]+c[1]*l_vac+c[2]*l_vac^2+c[3]*l_vac^3+$
    ;								c[4]*l_vac^4+c[5]*l_vac^5, color=3000
    ;6 coefficients for the 5th degree poly for v2a determined as above
    c=[ 0.99962627, 5.4209431e-007, -1.2545200e-009, $
        1.4952310e-012, -8.9989458e-016, 2.1672297e-019]
    la=	lv*(c[0]+c[1]*lv+c[2]*lv^2+c[3]*lv^3+$
        c[4]*lv^4+c[5]*lv^5)
    RETURN, la
END