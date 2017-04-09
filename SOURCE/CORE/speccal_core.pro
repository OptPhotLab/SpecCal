FUNCTION SPECCAL_TMIN_RATIO, p, xo=xo, yo=yo, xm=xm, ym=ym, FM=FM, X_Sample=X_Sample, ll=ll, MethodType=MethodType, filtWing=filtWing

    ; minimization function for the Cratio method

    ;P=[spectral shift, fwhm]

    ;Shift wl of MODTRAN4 data
    xms=xm+p[0]
    ;Degrade MODTRAN FWHM (FM) to that of p[1]
    sdBest=FM/(2.0d*SQRT(2.0D*ALOG(2)))
    sdWorst=p[1]/(2.0d*SQRT(2.0D*ALOG(2)))
    SPECCAL_STANDARDIZE_CONST_FWHM, xm, ym, sdBest, sdWorst, ym_RFH 	;degrade MODTRAN sim to p[1] SD
    SPECCAL_LINEAR_RESAMPLE, xms,	ym_RFH, xm_RSS_RFH, ym_RSS_RFH, X_Sample, ll, N_ELEMENTS(xo), N_ELEMENTS(xm)			;resample modelled data (degraded, ym_RFO) to observed ss=SSO
    
    ;scale Y of modelled data on observed Y, so that the ratio sould be about 1
    ;yo2m=SPECCAL_SCALEY0ONY1(yo, ym_SSO_RFO, /MEDIANX)
    ym2o_RSS_RFH=SPECCAL_SCALEY0ONY1(ym_RSS_RFH, yo);, /MEDIANX)
    ;compute the ratio
    R=yo/ym2o_RSS_RFH
    ;smooth it
    Rsmooth=SPECCAL_FILTER(R, MethodType, filtWing)
    ;MEAN is used instead of summeation to make the magnitude of cost indipendent of the numebr of elements of R
    ;In this way, the effect of epsrel is similar at all the ranges
    cost=MEAN(((R-Rsmooth)/Rsmooth)^2, /DOUBLE)
    ;    print,cost
    RETURN, cost
END

FUNCTION SPECCAL_TMIN_CORRELATION, p, xo=xo, yo=yo, xm=xm, ym=ym, FM=FM, X_Sample=X_Sample, ll=ll, MethodType=MethodType, filtWing=filtWing

    ; minimization function for the correlation method

    ;P=[spectral shift, fwhm]
    ;Shift wl of MODTRAN4 data
    xms=xm+p[0]
    ;Degrade MODTRAN FWHM (FM) to that of p[1]
    sdBest=FM/(2.0d*SQRT(2.0D*ALOG(2)))
    sdWorst=p[1]/(2.0d*SQRT(2.0D*ALOG(2)))
    SPECCAL_STANDARDIZE_CONST_FWHM, xm, ym, sdBest, sdWorst, ym_RFH	;degrade MODTRAN sim to p[1] SD
    SPECCAL_LINEAR_RESAMPLE, xms,	ym_RFH, xm_RSS_RFH, ym_RSS_RFH, X_Sample, ll, N_ELEMENTS(xo), N_ELEMENTS(xm)			;resample modelled data (degraded, ym_RFO) to observed ss=SSO
    ym2o_RSS_RFH=SPECCAL_SCALEY0ONY1(ym_RSS_RFH, yo)
    cost=1-CORRELATE(yo, ym2o_RSS_RFH, /DOUBLE)
    ;print, p, cost
    RETURN, cost
END

;+
;:ROUTINE NAME:
;SPECCAL_CORE
;
; :PURPOSE:
;       Compute the spectral shift and fwhm of the analyzed irradiance file, at the selected spectral window
;
; :PARAMS:
;xObs:                                wl of observed spectrum
;yObs:                              observed spectrum (can be radiance, irradiance or counts)
;nominalFwhmObs:            nominal FWHM of observed spectrum
;tarRef:                            reflectance of observed target
;xMod:                              wl of modelled spectrum
;yMod:                              modelled spectrum (irradiance)
;fwhmMod:                           FWHM of modelled data (known and exact)
;range:                             user selected spectral interval (subset of xObs) to be used the computations
;MethodType:                    Processing method to be used: 0= Ratio; 1 = Correlation
;shl:                                   spectral SHift limit is maximum shift to be searched (-shl, +shl)
;fwl:                                   Full Width limit factor, real FWHM is searched in the range (nominalFwhmObs/fwl, nominalFwhmObs*fwl)
;do_plot:                          if = 1 plot graphs on screen
;doSaveImage:                if = 1 save graphs to tiff
;save_csv:                       if = 1 save graph data on csv files
;path_out:                      specify the path for saving the results
; warnings:                     variable were possible warnings are saved
;meastype:                      0 = refl. radiance; 1 = incident irradiance
;
;:RETURNS:
;Five Element vector, containing:
;
;P: Results of optimizaion.Two elements array. [Spectral Shift, FWHM].
;
;cost: Cost function for optimized parameters
;
;status: Status of the optimization (See TNMIN documentation)
;
;nobs: Number of observations within the spectral window analyzed
;
;filtWing: Half width of the smoothing window
;
; :NOTES:
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
;-

FUNCTION SPECCAL_CORE, xObs, yObs, nominalFwhmObs, tarRef, xMod, yMod, fwhmMod, $
        range, MethodType, shl, fwl, doplot, doSaveImage, save_csv, path_out, WARNINGS=warnings, meastype
        
    ;	VAR														Content					                                     Sampling Interval                Spectral	Shift				Fwhm
    ;-----------------------------  --------        ----------------                                               ------------ ----------                  -------- -----------              -----------
    ; (xo, yo) 											      observed data		                                xo[1]-xo[0] (X_sample)			 NSS (= 0)                       NFH
    ; (xm, ym)                                          original modeled data                                     xm[1]-xm[0]                          NSS (= 0)                       FM
    ; (xm, ym_NFH)                   modeled data with modified FWHM                           xm[1]-xm[0]                          NSS (= 0)                      NFH
    ; (xm, ym_RFH)                   modeled data with modified FWHM                           xm[1]-xm[0]                          NSS (= 0)                      RFH
    ; (xm_NSS_NFH, ym_NSS_NFH)          modelled data                                               X_Sample                           NSS (= 0)                      NFH
    ; (xm_RSS_RFH, ym_RSS_RFH)          modelled data                                                X_Sample                            RFO Real (=optimized Fwhm of Observation)
        
    errmsg = ''
    ; Save original fwl and shl in "_orig" variables
    fwl_orig = fwl
    shl_orig = shl
    
    NFH = nominalFwhmObs
    FM = fwhmMod
    xo=xObs	& yo=yObs	& xm=xMod	& ym=yMod
    
    warnings=STRARR(10)
    
    ;1 check that x of obs data is regularly spaced (i.e. that spectral sampling is constant)
    mc=MACHAR()
    IF (ABS(MEAN(xo[1:*]-xo[0:N_ELEMENTS(xo)-2]-(xo[1]-xo[0]), /DOUBLE)) GT 2*mc.EPS) THEN BEGIN
        ; the xarray is not equally spaced and it is resampled to the smallest ss found
        x_tmp= xo
        y_tmp= yo
        SPECCAL_LINEAR_RESAMPLE, x_tmp,	y_tmp, xo, yo, 0, xo[0], N_ELEMENTS(xo), N_ELEMENTS(xo)
        ;        wrng=1						;a warning has to be issued
        warnings[MIN(WHERE(warnings EQ ''))]='SpecCalCore Function:  Spectral Sampling  of Measured File not constant. Observations were linearly resampled with a SSI of ' + STRTRIM(xo[1]-xo[0],2) + ' nm.'
    END
    X_Sample=xo[1]-xo[0]																				; x Spectral sampling
    
    ;2 extract the spectral range defined by user (note that id the wavlength calibration is completely wrong, the range extracted here
    ;	 will be not coincident with the modelled one
    
    ; This messages are obsolete ! The check for concordance between MODTRAN and spectral ranges is now made in spaccalGUI
    ; 
;    IF (range[0] LT MIN(xm)) THEN BEGIN range[0]=MIN(xm) & warnings[MIN(WHERE(warnings EQ ''))]='SpecCalCore Function: Range Min smaller than min wl of mod data, Range min set to ' + STRTRIM(range[0],2) + '.' & ENDIF
;    IF (range[1] GT MAX(xm)) THEN BEGIN range[1]=MAX(xm) & warnings[MIN(WHERE(warnings EQ ''))]= 'SpecCalCore Function: Range Max larger than max wl of mod data, Range max set to '  + STRTRIM(range[1],2) + '.' & ENDIF
;    IF (range[0] LT MIN(xo)) THEN BEGIN range[0]=MIN(xo) &  warnings[MIN(WHERE(warnings EQ ''))]= 'SpecCalCore Function: Range Min smaller than min wl of obs data, Range min set to ' + STRTRIM(range[0],2) + '.' & ENDIF
;    IF (range[1] GT MAX(xo)) THEN BEGIN range[1]=MAX(xo) & warnings[MIN(WHERE(warnings EQ ''))]= 'SpecCalCore Function: Range Max larger than max wl of obs data, Range max set to '  + STRTRIM(range[1],2) + '.'& ENDIF
;    IF range[1] LE range[0] THEN BEGIN
;        PRINT, 'Error: Range cannot be applied to obs'
;        P = [-999,-999]
;        info_out = {P : P}
;        RETURN, P
;    ENDIF

    indXMin=SPECCAL_FIND_IND_OF_NEAREST(xo, range[0])
    indXMax=SPECCAL_FIND_IND_OF_NEAREST(xo, range[1])
    xo=xo[indXMin:indXMax] & yo=yo[indXMin:indXMax]		; observed data to be checked, x and y
    range = [MIN(xo), MAX(xo)]  ; Added to make the range consistent !!!!!
    
    nobs=N_ELEMENTS(xo)
    filtWing=5
    
    ; If meastype is radiance then multiply by PI and divide Y obs by target reflectance to get incident irradiance
    IF meastype EQ 0 THEN BEGIN
        rho=SPLINE(tarref[0,*], tarref[1,*], xo, /double)	;resample target reflectance (1nm SS) to x of obs with spline
        y_tmp= yo
        yo=!dpi*yo/DOUBLE(rho)
    ENDIF
    
    ;3 Check nobs e swg input parameters, treat it differently if method is correlation
    IF (nobs LT 10) THEN BEGIN  warnings[MIN(WHERE(warnings EQ ''))]= 'SpecCalCore Function: The specified range results in ' + STRTRIM(nobs,2) + ' observations.' & ENDIF
    IF (MethodType EQ 0) THEN BEGIN
        IF (filtWing EQ 0) THEN filtWing=5
        IF filtWing GT (nobs/5.0) THEN BEGIN
            filtWing=FLOOR(nobs/5.0)
            IF (filtWing LT 2.0) THEN filtWing = 2		;filter of 1 do not smooth anything
            warnings[MIN(WHERE(warnings EQ ''))]= 'SpecCal Core Function: Filter HalfWidth was GT nobs/5. It was set to nobs/5 = ' + STRTRIM(filtWing,2)+ '.'
        ENDIF
    ENDIF
    
    ;4 degrade modelled simulation to NFO
    sdBest=FM/(2.0d*SQRT(2.0D*ALOG(2))) & 	sdWorst=NFH/(2.0d*SQRT(2.0D*ALOG(2)))
    SPECCAL_STANDARDIZE_CONST_FWHM, xm, ym, sdBest, sdWorst, ym_NFH	;degrade MODTRAN sim to p[1] SD
    
    ;5 resample modelled simulation (degraded to NFO) to SSO: xm_SSO_NFO, ym_SSO_NFO
    SPECCAL_LINEAR_RESAMPLE, xm,	ym_NFH, xm_NSS_NFH, ym_NSS_NFH,X_Sample, xo[0], N_ELEMENTS(yo), N_ELEMENTS(xm)
    
    ;6 scale ym on ym_NSS_NFH using a min-max scaling
    ym2o_NSS_NFH=SPECCAL_SCALEY0ONY1(ym_NSS_NFH,yo)
    
    ;7 Store raw ratio
    Rraw=yo/ym2o_NSS_NFH
    
    ; Initialize Warnings (Residual from auto-expanding version. Kept for next versions)
    
    found_warns = where (warnings NE '', count_warns)
    IF count_warns NE 0 THEN tmp_warns = warnings[found_warns]
    retry:
    ; Reset Warnings on retry
    IF count_warns NE 0 THEN BEGIN
        warnings[*] = ''
        warnings[found_warns] = tmp_warns
    ENDIF
    
    ;    Find first guess for shift e realFwhmObs by performing two separate searches
    ;    8 explore the shift assuming nominal fwhm is correct and find a best first guess
    step=X_Sample/4.0D				;step for exploring the admitted shift interval (interval=2.0D*shl) , if step results in less then 100 function evaluations then  step=interval/100
    shiftCostArray=SPECCAL_EXPLORESHIFT(yo, xm, ym_NFH, range[0], X_Sample, MethodType, filtWing, shl, step, ERRMSG=errmsg)
    IF errmsg NE '' THEN warnings[MIN(WHERE(warnings EQ ''))]=errmsg
    minCost=MIN(shiftCostArray[*,1], subMinCost)
    fgSpecShift=shiftCostArray[subMinCost,0]
    
    ;9 explore the fwhm assuming that the shift found above is correct
    fwhmCostArray=SPECCAL_EXPLOREFWHM(yo, xm+fgSpecShift, ym, range[0],X_Sample, MethodType, filtWing, fwl, FM, NFH, ERRMSG=errmsg)
    IF errmsg NE '' THEN warnings[MIN(WHERE(warnings EQ ''))]=errmsg
    minCost=MIN(fwhmCostArray[*,1], subMinCost)
    fgFwhm=fwhmCostArray[subMinCost,0]
    
    ;10 Optimization of Spectral Shift and FWHM that has to be applied to MODTRAN4 data to match observation.
    first_guess=[fgSpecShift, fgFwhm]	;[spectral shift, fwhm]
    nparms=N_ELEMENTS(first_guess)
    parinfo = REPLICATE({limited:[0,0], limits:[0.d,0.d], fixed:0}, nparms)
    parinfo[0].limited = 1		;Spctral shift
    parinfo[0].limits  = [-DOUBLE(shl), DOUBLE(shl)]
    parinfo[1].limited = 1		;Fwhm
    parinfo[1].limits  = [NFH/DOUBLE(fwl), NFH*DOUBLE(fwl)]
    
    IF (parinfo[1].limits[0] LE FM) THEN parinfo[1].limits[0] = FM+FM/100.0D
    ll=range[0]
    ;TMIN, unweighted minimization
    IF (MethodType EQ 1) THEN funct_name='SpecCal_TMIN_Correlation' ELSE $
        funct_name='SpecCal_TMIN_ratio'
    fcnargs = {xo:xo, yo:yo, xm:xm, ym:ym, FM:FM, X_Sample:X_Sample, ll:ll, MethodType:MethodType, filtWing:filtWing}
    mch=MACHAR(/double)
    P = TNMIN(funct_name, first_guess, FUNCTARGS=fcnargs, $
        PARINFO=parinfo, BESTMIN=rss, ERRMSG=error, STATUS=status, AUTODERIVATIVE=1, epsrel = mch.EPS*1.0e+6,/quiet)
        
    ; Reassign STATUS vlaues and issue warnings
    IF STATUS EQ 5 THEN BEGIN
        warnings[MIN(WHERE(warnings NE ''))] = 'SpecCalCore Function: Maximum Iterations reached. Convergence not reached.'
        STATUS = -1
        p = [-999,-999]
    ENDIF
    IF STATUS EQ -18 THEN BEGIN
        warnings[MIN(WHERE(warnings NE ''))] = 'SpecCalCore Function: An Error occured within the optimization algorithm (TNMIN) Convergence not reached.'
        STATUS = -2
        p = [-999,-999]
    ENDIF
    
    IF (P[0] EQ parinfo[0].limits[0]) OR (P[0] EQ parinfo[0].limits[1]) THEN status = -3
    IF (P[1] EQ parinfo[1].limits[0]) OR (P[1] EQ parinfo[1].limits[1]) THEN status = -4
    IF STATUS EQ -3 THEN BEGIN
        warnings[MIN(WHERE(warnings EQ ''))] = 'TNMIN Function: Minimum error SS found at one of the edges of the amditted shift interval. Convergence not reached'
        p = [-999,-999]
    ENDIF
    
    IF STATUS EQ -4 THEN BEGIN
        warnings[MIN(WHERE(warnings EQ ''))] = 'TNMIN Function:  Minimum error FWHM found at one of the edges of the amditted shift interval. Convergence not reached'
        p = [-999,-999]
    ENDIF
    
    ; If STATUS = 1 then prepare the data for the plots and plot data
    
    IF MIN(p) NE -999 THEN BEGIN
        sdBest=FM/(2.0d*SQRT(2.0D*ALOG(2)))
        sdWorst=P[1]/(2.0d*SQRT(2.0D*ALOG(2)))
        SPECCAL_STANDARDIZE_CONST_FWHM, xm, ym, sdBest, sdWorst, ym_RFH	;degrade MODTRAN sim to p[1] SD
        xms=xm+P[0]
        SPECCAL_LINEAR_RESAMPLE, xms,	ym_RFH, xm_RSS_RFH, ym_RSS_RFH, X_Sample, xo[0], N_ELEMENTS(xo), N_ELEMENTS(xm)    ;shift MODTRAN sim to p[0] SS
        ;scale modtran @retrieved parameters on obs
        ym2o_RSS_RFH=SPECCAL_SCALEY0ONY1(ym_RSS_RFH,yo)
        ;yo2m=SPECCAL_SCALEY0ONY1(yo, ym_SSO_RFO, /MEDIANX)
        R=yo/ym2o_RSS_RFH
        IF (MethodType EQ 0) THEN BEGIN
            Rsmooth=SPECCAL_FILTER(R, MethodType, filtWing)
            cost=TOTAL((((R-Rsmooth)/Rsmooth)^2),/DOUBLE)
        ENDIF ELSE BEGIN
            cost=CORRELATE(yo,ym2o_RSS_RFH, /DOUBLE)
        ENDELSE
        
        ; Start plotting outputs in accordance to plotting options
        
        SPECCAL_PLOT,range, $
            xo,xm_RSS_RFH, yo, ym2o_RSS_RFH,ym2o_NSS_NFH,xm_NSS_NFH,$
            NFH, P, $
            doPLOT, dosaveimage, save_csv, MethodType, $
            R,Rraw, $
            path_out
            
    ENDIF ELSE BEGIN
        cost = -999
        nobs = nobs
        filtwing = filtwing
    ENDELSE
    info_out = {P : P, cost:cost, status:status, nobs:nobs, filtwing:filtwing}
    ;retrieve "original" fwl and shl
    
    fwl = fwl_orig
    shl = shl_orig
    
    RETURN, info_out
END