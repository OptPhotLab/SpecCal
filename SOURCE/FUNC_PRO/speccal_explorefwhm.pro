;+
;:ROUTINE NAME:
;SpecCal_exploreFwhm
;
; :PURPOSE:
; The routine searches for a "first guess" of the real FWHM for the spectroradiometer analyzed, at a selected spectral window
;:Params:
;    yo: Observed Values
;    xm: wl of original modtran simulation
;    ym: Irradiances of original modtran simulation
;    range0: Starting WL of the range
;    X_Sample: Spectral Sampling Interval of observations
;    MethodType:   0 = Ratio; 1 = Correlation
;    filtWing: Set to 5
;    fwl: User-selected limit for FWHM exploration (interval = NFH/fwl --> NFH*fwl)
;    FM: FWHM of MODTRAN simulation
;    NFH: Nominal FWHM of observations
; 																																						;
; :RETURNS:
;
; :REQUIRES:
; SpecCal_STANDARDIZE_CONST_FWHM
; 
; SpecCal_LINEAR_RESAMPLE
; 
; SpecCal_SCALEY0ONY1
;
; SpecCal_filter
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
;
;-
FUNCTION SPECCAL_EXPLOREFWHM, yo, xm, ym, range0,X_Sample, MethodType, filtWing, fwl, FM, NFH, ERRMSG=errmsg
    errmsg = ''
    wrng=0
    xmt=xm  & ymt=ym
    sdBest=FM/(2.0d*SQRT(2.0D*ALOG(2)))
    minFhwm=NFH/DOUBLE(fwl)
    IF (minFhwm LE FM) THEN BEGIN
        minFhwm=FM+FM/100.0D
        wrng=1
        errmsg= 'Explore FWHM Function: Min fwhm lower than MODTRAN4 one. New min set to '+ STRTRIM(minFhwm,2)
    ENDIF
    step=NFH/50.0d
    nFunctEval=FLOOR((NFH*DOUBLE(fwl)-minFhwm)/step)
    fwhmCostArray=DBLARR(nFunctEval, 2) ; [*,0] is fwhm, [*,1] is cost
    
    FOR j=0,nFunctEval-1 DO BEGIN
        fwhmCostArray[j,0]=minFhwm+step*DOUBLE(j)
        sdWorst=fwhmCostArray[j,0]/(2.0d*SQRT(2.0D*ALOG(2)))
        
        SPECCAL_STANDARDIZE_CONST_FWHM, xmt, ymt, sdBest, sdWorst, ym_RFH  ;degrade MODTRAN sim to p[1] SD
        SPECCAL_LINEAR_RESAMPLE, xmt, ym_RFH, xm_RSS_RFH, ym_RSS_RFH,X_Sample, range0, N_ELEMENTS(yo), N_ELEMENTS(xm)  ;resample modelled data (degraded, yModNominalFwhmObs) to observed ss=SSO
        
        ; Compute the cost function between original and resampled and rescaled MODTRAN irradiance
        IF (MethodType EQ 1) THEN BEGIN
            ym2o_RSS_RFH=SPECCAL_SCALEY0ONY1(ym_RSS_RFH,yo)
            fwhmCostArray[j,1]= 1-CORRELATE(yo, ym2o_RSS_RFH, /DOUBLE)
        ENDIF ELSE BEGIN
            ym2o_RSS_RFH=SPECCAL_SCALEY0ONY1(ym_RSS_RFH,yo);, /MEDIANX)
            R=yo/ym2o_RSS_RFH
            Rsmooth=SPECCAL_FILTER(R, MethodType, filtWing)
            fwhmCostArray[j,1]=TOTAL((((R-Rsmooth)/Rsmooth)^2),/DOUBLE)
        ENDELSE
        
        
    ENDFOR
    minCost=MIN(fwhmCostArray[*,1], subMinCost)             ; Find the FWHM which minimizes the cost function
    
    RETURN, fwhmCostArray
END