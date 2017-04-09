;+
;:ROUTINE NAME:
;SPECCAL_REPORT_STRUCTURE_DEFINE
;
; :PURPOSE:
; 
; Initialize the info structure that will contain the simulation parameters and the output results of SpecCal
;
; :INPUTS:
;
; :OUTPUTS:
; 
; record: Structure to be filled with simulation input parameters and output results, successively used for writing the outputs
;              in the CSV output file
;
; :REQUIRES:
;
; :NOTES:
;
; :HISTORY:
;
;: Created:	16-feb-2010
;
; :AUTHOR: Michele Meroni; Lorenzo Busetto.
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
FUNCTION SPECCAL_REPORT_STRUCTURE_DEFINE
    record={$
        range_wl0       	:{hdr:'Range_wl_min', 									units:'nm',			value:''}, $
        range_wl1			:{hdr:'Range_wl_max', 				     				units:'nm',			value:''}, $
        range_center		:{hdr:'Range_center', 				    			units:'nm',			value:''}, $
        spec_shift			:{hdr:'SS_cal (WL_true = Wl_obs+SS_cal)',   units:'nm',			value:0.0D}, $
        spec_fwhm		:{hdr:'FWHM_cal',                                            units:'nm',			value:0.0D}, $
        cost						:{hdr:'Cost Function (@SS_cal and FWHM_cal)', 				    units:'-',	      		value:0.0D}, $
        status					:{hdr:'Minimization Status',				 					units:'',				value:0.0}, $
        method_type     :{hdr:'Method',                                         units:'',               value:''}, $
        meastype           :{hdr:'Measurement Type',                                         units:'',               value:''}, $
        umeas                 :{hdr:'Measurement Units',                                         units:'',               value:''}, $
        nominal_fwhm :{hdr:'Nominal FWHM',                           units:'nm',         value:0.0D}, $
        shift_limit          :{hdr:'Spectral Shift Limit (+/- limit)',     units:'nm',         value:0.0}, $
        fwhm_limit        :{hdr:'Fwhm Limit (*/ limit)',       units:'-',              value:0.0}, $
        nobs                    :{hdr:'N° of channels',                              units:'',               value:0}, $
        RW                       :{hdr:'Range Widening',                                              units:'',               value:0}, $
        smoothFltHW    :{hdr:'Smoothing filter HW',                    units:'',               value:''}, $
        in_file                 :{hdr:'Measured file',                                           units:'',               value:''}, $
        mod_file				:{hdr:'MODTRAN file',						 					units:'',				value:''}, $
        tarref_file			:{hdr:'Target reflectance file',					units:'',				value:''}, $
        warnings			:{hdr:'Messages',						 				units:'',				value: STRARR(10)} $
        }
    RETURN, record
END