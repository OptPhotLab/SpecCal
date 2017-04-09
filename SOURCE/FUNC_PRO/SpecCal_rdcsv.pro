;+
; :Description:
;    Reads an input CSV file and returns info about wavelenghts, irradiance and FWHM.
;
; :Params:
;    file: Input CSV file to be read by the program. The file must be a semicolumn delimited csv . 
;           The first line is an Header. Second line reports the wavelenght and irradiance values for the first channel, and the nominal FWHM of the 
;           spectroradiometer. Following lines contain wavelenghts and Irradiance or Counts values for the othe channels.
;       e.g.:       
;       1st line:            Wl;   U_Meas;   fwhm
;       2nd line:           717;   0.297;   0.13
;       3rd line:            718;   0.256; 
;       .........                   ..........  .......
;      nth line:           717.02; 0.296; 
;      
;      If Unit_Measure is equal to "Counts" then a "Basic" radiometric calibration is performed BEFORE running the spectral calibration
;
; :OUTPUTS:
;  info_csv:  structure containing fields:
;    -  wl: wavelenghts
;    -  vals: Irradiance values
;    -  fwhm: Nominal FWHM
;    - U_Meas: Measurement unit for vals. If equal to "Counts" then  a "Basic" radiometric calibration is performed BEFORE running the spectral calibration
;    
; :REQUIRES:
;
; :NOTES:
;
; :AUTHOR: Michele Meroni; Lorenzo Busetto - 
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
;
; :HISTORY:
;
; Created: 28-apr-2009
;-

FUNCTION SPECCAL_RDCSV,file
    str=''
    nlines=FILE_LINES(file)                                                                                    ; Get n° of lines of the input file
    wl=DBLARR(nlines-1) & vals=DBLARR(nlines-1)
    OPENR, lun, file, /get_lun                                                                             ; Open the file
    READF, lun, str                                                                                                  ; Skip the Header
    ; STart cycle on lines to get the data
    FOR i=0L, nlines[0]-2 DO BEGIN
        READF, lun, str                                                                                         ; Read data line
        dataInTheRow=DOUBLE(STRSPLIT(str, ';', count=n_row, /EXTRACT))
        IF (i EQ 0) THEN fwhm=dataInTheRow[2]                                               ; Read the FWHM
        wl[i]=dataInTheRow[0]                                                                            ; Read WL
        vals[i]=dataInTheRow[1]                                                                         ; Read Irradiance
        
    ENDFOR
    FREE_LUN, lun
    info_csv = {wl:wl, vals:vals, fwhm:fwhm}        ; Put the data in an output structure
    RETURN, info_csv                                                                                  ; Pass back the values
END