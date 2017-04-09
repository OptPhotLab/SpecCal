;+
; standardize_const_fwhm
; Apr 16, 2009
;
; :Description:
;     This procedure degrades the spectrum of the spectrometer with better resolution to the
;     resolution of the spec with worse resolution given the standard deviation of the two
;     INPUT DATA MUST BE ALREADY RESAMPLED to a regular grid
;     fwhm is given in term of SD, fwhm=2*sqrt(2*ln(2))*SD
;     SD is given as a linear function of wl sd=a+b*wl
;     
;     given a true spectral signal u[], the spectrum dected, convolved by a gaussian filter, is y[]
;     y[]=h*u[]
;     where h[]=exp(-bx^2) is the kernel of the convolution
;     being x=wl-wl0, distance from the convolution center (TO BE SET), and b=1/2SD^2
;     the degraded resolution is thus:
;     y'[]=h3*y[], see manual for details
;
; :Params:
;       wl:                                         input wl (X)
;       best_res_signal_in:           input signal (Y)
;       best_sd_in:                         original SD resolution, must be better than output SD 
;       worst_sd_in:                       output resolution   
;       degraded_signal_out:       Y with degraded resolution
;		 NOTE that sd=fwhm/(2.0d*SQRT(2.0D*ALOG(2)))
;:NOTES:
;
;  INPUT DATA MUST BE ALREADY RESAMPLED to a regular grid !!!!!!
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

PRO SPECCAL_STANDARDIZE_CONST_FWHM, $
        wl, best_res_signal_in, best_sd_in, worst_sd_out, $   ;INPUT parameters
        degraded_signal_out                                   ;OUTPUT parameters
        
    ;convolution parameters
    n=N_ELEMENTS(wl)                ;number of elements in the array
    sint=DOUBLE(wl[1])-DOUBLE(wl[0]);sampling interval
    xWingNm=worst_sd_out*5.0D               ;half width of the kernel in nm=worst_sd*5 (twenty times SD)
    xWing=FLOOR(xWingNm/DOUBLE(sint))       ;half width of the kernel in number of vector elements
    ;form kernel array
    x=DINDGEN(xWing*2+1)
    x=(x-xWing)*DOUBLE(sint)
    
    ;convolve the array
    degraded_signal_out=DBLARR(n)
    ;best resolved parameters
    b1=1.0D/(2.0D*DOUBLE(best_sd_in)^2)
    ;worst resolved parameters
    b2=1.0D/(2.0D*DOUBLE(worst_sd_out)^2)
    ;a=SQRT(b1/(b1-b2))* SQRT(b1/!DPI) & c=-b1*b2  & d=b1-b2
    a=SQRT(b1/(b1-b2))* SQRT(b2/!DPI) & c=-b1*b2  & d=b1-b2
    h3=a*EXP((c*x^2)/d) & rh3=h3/TOTAL(h3,/double)
    degraded_signal_out=CONVOL(best_res_signal_in, rh3, /EDGE_TRUNCATE)
END