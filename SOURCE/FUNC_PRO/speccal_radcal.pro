;+
;:ROUTINE NAME:
;SPECCAL_RADCAL
;
; :PURPOSE:
;   Perform a "Basic" radiometric calibration of the file if the input file Units Measure is "Counts"
;   MODTRAN File is resampled on observation FWHM and spectral sampling
;   OBS and Resampled MODTRAN are processed with a lowess smoothing with a window equal to 1/10 of the total
;   number of observations, then the gain between observed counts and modtran irradiance is computed and applied to observations.
;
; :INPUTS:
;
; :OUTPUTS:
;
; :PARAMS:
;    XOBS: wl of observations --> On exit it may be different from the original
;    YOBS: Observed Counts --> On exit, it is transformed to observed IRRADIANCE
;    NOMINALFWHMOBS: Nominal FWHM of observations
;    TARREF: Target Reflectances
;    XMOD: Wl of modtran
;    YMOD: Irradiance of MODTRAN simulation
;    FWHMMOD: FWHM of MODTRAN simulation
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
PRO SPECCAL_RADCAL, XOBS, YOBS, NOMINALFWHMOBS,  TARREF, XMOD,YMOD, $
        FWHMMOD, meastype
        
    NFO = nominalFwhmObs
    FM = fwhmMod
    mc=MACHAR()
    IF (ABS(MEAN(xobs[1:*]-xobs[0:N_ELEMENTS(xobs)-2]-(xobs[1]-xobs[0]), /DOUBLE)) GT 2*mc.EPS) THEN BEGIN
        ; the xarray is not equally spaced and it is resampled to the smallest ss found
        x_tmp= xobs
        y_tmp= yobs
        SPECCAL_LINEAR_RESAMPLE, x_tmp, y_tmp, xobs, yobs, 0, xobs[0], N_ELEMENTS(xobs), N_ELEMENTS(xobs)
    ENDIF
    SSO=xobs[1]-xobs[0]
    range = [MIN(xobs),MAX(xobs)]
    IF (range[0] LT MIN(xmod)) THEN BEGIN range[0]=MIN(xmod) & ENDIF
    IF (range[1] GT MAX(xmod)) THEN BEGIN range[1]=MAX(xmod)& ENDIF
    IF (range[0] LT MIN(xobs)) THEN BEGIN range[0]=MIN(xobs) & ENDIF
    IF (range[1] GT MAX(xobs)) THEN BEGIN range[1]=MAX(xobs) & ENDIF
    
    indXMin=SPECCAL_FIND_IND_OF_NEAREST(xobs, range[0])
    indXMax=SPECCAL_FIND_IND_OF_NEAREST(xobs, range[1])
    xobs=xobs[indXMin:indXMax] & yobs=yobs[indXMin:indXMax]     ; observed data to be checked, x and y
    nobs=N_ELEMENTS(xobs)
    filtWing=5
    
    ; If meastype is radiance then  multiply by PI and divide Y obs by target reflectance to get incident irradiance
    IF meastype EQ 0 THEN BEGIN
        rho=SPLINE(tarref[0,*], tarref[1,*], xobs, /double)   ;resample target reflectance (1nm SS) to x of obs with spline
        y_tmp= yobs
        yobs=!dpi*yobs/DOUBLE(rho)
    ENDIF
    ;4 degrade modelled simulation to NFO
    sdBest=FM/(2.0d*SQRT(2.0D*ALOG(2))) &   sdWorst=NFO/(2.0d*SQRT(2.0D*ALOG(2)))
    SPECCAL_STANDARDIZE_CONST_FWHM, xmod, ymod, sdBest, sdWorst, ym_NFO ;degrade MODTRAN sim to p[1] SD
    ;5 resample modelled simulation (degraded to NFO) to SSO: xm_SSO_NFO, ym_SSO_NFO
    SPECCAL_LINEAR_RESAMPLE, xmod,  ym_NFO, xm_SSO_NFO, ym_SSO_NFO, SSO, xobs[0], N_ELEMENTS(yobs), N_ELEMENTS(xmod)
    ; Apply a Lowess filter on both observed and modeled data
    filt_mod= LOWESS(xm_sso_nfo, ym_sso_nfo, N_ELEMENTS(xm_sso_nfo)/10,2)
    filt_obs= LOWESS(xobs, yobs, N_ELEMENTS(xm_sso_nfo)/10,2)
    ratio = filt_mod/filt_obs
    cal_obs  = yobs*ratio
    yobs = cal_obs
    
END