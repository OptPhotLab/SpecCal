;+
;:ROUTINE NAME:
;SpecCal_LINEAR_RESAMPLE
;
; :PURPOSE:
;  given an irregular gridded wl_in (wavelenght) and corresponding y_in (whatever)
;     this procedure resample both vectors to regular grid and return then in wl_out and y_out.
;
; :Params:
;    wl_in: input wavelengths
;    y_in:  input Y (can be whatever, counts, L , E)
;    wl_out: resampled output wl
;    y_out: resampled output Y
;    step: spectral sampling
;              if step is 0 the new sampling step 
;             is the minimum sampling step found in the wl vector
;     
;             If step, first_wl, bands are given, the procedure forces
;             the sampling to these parameters.
;    first_wl:first wavelength of the output array
;    bands: number of channels in the output
;    valid_pixel_num: number of channels in the input that has to be used 
; 																																						;
; :RETURNS:
;
; :REQUIRES:
;
; :NOTES:
;  The procedure handles -999 an NaN properly.
;   WARNING: returned wl and rho vector may have diffrent size from input vectors. No data are returned as -999
;
; :HISTORY:
;
;; Oct 7, 2009, Mic aggiunge il trattamento dei NaN
;
; :AUTHOR:
;
; :AUTHOR: Michele Meroni
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

PRO SPECCAL_LINEAR_RESAMPLE,  wl_in, y_in, $                                ;input vectrors
        wl_out, y_out, $                              ;output vectors
        step, first_wl, bands, valid_pixel_num, $        ;paramters
        NAN=NAN
        
    tmp_wl_in=wl_in
    tmp_y_in=y_in
    
    ;set NaN after valid_pixel_num
    IF valid_pixel_num LT N_ELEMENTS(y_in) THEN y_in[valid_pixel_num-1:*]=!VALUES.F_NAN
    ;handle no data: set possible -999 (no data) to NaN
    finiteInd=WHERE(FINITE(y_in) EQ 1, countFinite)
    
    noDataInd=WHERE(y_in[finiteInd] EQ -999, countNoData)
    IF countNoData NE 0 THEN BEGIN
        wl_in[finiteInd[noDataInd]]=!VALUES.F_NAN
        y_in[finiteInd[noDataInd]]=!VALUES.F_NAN
    ENDIF
    
    ;step set to 0 ->exctract parameters from dat
    IF step EQ 0 THEN BEGIN
        ;find minimum sampling interval to which resample
        delta_wl=wl_in[1:*]-wl_in[0:N_ELEMENTS(wl_in)-2]
        min_delta=MIN(delta_wl, /absolute, /NaN)
        ;find min and max wl_in, and new number of bands
        max_wl = max (wl_in, /NaN)  & min_wl =MIN(wl_in, /NaN)
        n_bands= ((max_wl-min_wl)/min_delta)+1
    ENDIF ELSE BEGIN
        min_delta=step
        min_wl=first_wl
        n_bands=bands
    ENDELSE
    
    wl_out=INDGEN(n_bands, /double)
    wl_out=wl_out*min_delta+min_wl
    
    chk=CHECK_MATH(/NOCLEAR)						;avoid print floating error coming from interpol
    ;- Check keywords
    IF KEYWORD_SET(NAN)  THEN BEGIN
        y_out = INTERPOL(y_in, wl_in, wl_out)				;interpol data with NaN values that are ignored
        NanInd=WHERE(FINITE(y_in) EQ 0, countNan)
        IF (countNan NE 0) THEN BEGIN
            y_in9=y_in & y_in9[NanInd]=-999
            y_out9 = INTERPOL(y_in9, wl_in, wl_out)		;interpol data with -9999 instead of NaN
            y_out_NanInd=WHERE((y_out-y_out9) NE 0, countNanIn_y_out) ;where the results are different, a Nan was encounterd in the computation, set to Nan)
            IF (countNanIn_y_out NE 0) THEN y_out[y_out_NanInd]=!VALUES.F_NAN
        ENDIF
    ENDIF ELSE BEGIN
        y_out = INTERPOL(y_in, wl_in, wl_out)
    ENDELSE
    
    IF chk EQ 0 THEN chk = CHECK_MATH()	;avoid print floating error coming from interpol
    wl_in=tmp_wl_in
    y_in=tmp_y_in
END