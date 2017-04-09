;+
;:ROUTINE NAME:
;SPECCAL_EXPLORESHIFT
;
; :PURPOSE:
;   The routine searches for a "first guess" of the spectral shift for the spectroradiometer analyzed, at a selected spectral window
;
; :PARAMS:
;    yo: Observed Values
;    xm: wl of original modtran simulation
;    ym_NFH: Modeled modtran data resampled to Nominal FWHM  of observations 
;    range0: starting wavelenght of the range
;    X_Sample: Spectral Sampling Interval of observations
;    MethodType: 0 = Ratio; 1 = Correlation
;    filtWing: Set to 5
;    shl: Spectral Shift Limit. Minimum searched between + and - shl
;    step: Step to be used to explore the Spectral Shift spcae
; 																																						;
; :RETURNS:
;
; :REQUIRES:
; SPECCAL_LINEAR_RESAMPLE
;
; SPECCAL_SCALEY0ONY1
;
; SPECCAL_FILTER
;
; :NOTES:
;     the shift is explored with a step equal to 0.25*X_Sample
;     if step results in less then 100 function evaluations, then step=interval/100
;
; :HISTORY:
;
;: Created:	16-feb-2010
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
FUNCTION SPECCAL_EXPLORESHIFT,yo, xm, ym_NFH, range0, X_Sample, MethodType, filtWing, shl, step, ERRMSG=errmsg
    ;the shift will be explored with a step equal step (e.g. step SSO/2)
    ;if step results in less then 100 function evaluations, then step=interval/100
    errmsg = ''
    wrng=0
    interval=2.0D*shl										;interval to be explored
    nFunctEval=FLOOR(interval/step)     ;admitted shift interval / step
    IF (nFunctEval LT 100) THEN BEGIN
        wrng=1
        errmsg= 'Explore Shift Function:The number of function evaluations was '+ STRTRIM(nFunctEval,2) + $
            ' and was set to 100.'
        nFunctEval=100
        step = interval/DOUBLE(nFunctEval)
    ENDIF
    shiftCostArray = DBLARR(nFunctEval, 2) ; [i,0] is the shift value, [i,1] is the cost function for the i shift
    FOR j=0,nFunctEval-1 DO BEGIN
        shiftCostArray[j,0] = -DOUBLE(shl)+step*DOUBLE(j)
        xms = xm + shiftCostArray[j,0]
        SPECCAL_LINEAR_RESAMPLE, xms, ym_NFH, xms_RSS, ym_RSS_NFH,X_Sample, range0, N_ELEMENTS(yo), N_ELEMENTS(xms)
        IF (MethodType EQ 1) THEN BEGIN
            ym2o_RSS_NFH=SPECCAL_SCALEY0ONY1(ym_RSS_NFH,yo);/MEDIANX)
            shiftCostArray[j,1]= 1-CORRELATE(yo, ym2o_RSS_NFH, /DOUBLE)
        ENDIF ELSE BEGIN
            ;scale obs on mod
            ym2o_RSS_NFH=SPECCAL_SCALEY0ONY1(ym_RSS_NFH,yo);/MEDIANX)
            R=yo/ym2o_RSS_NFH
            Rsmooth=SPECCAL_FILTER(R, MethodType, filtWing)
            shiftCostArray[j,1]=TOTAL((((R-Rsmooth)/Rsmooth)^2),/DOUBLE)
        ENDELSE
    ENDFOR
    minCost=MIN(shiftCostArray[*,1], subMinCost)
    RETURN, shiftCostArray
END