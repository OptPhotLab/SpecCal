;+
;:ROUTINE NAME:
;SpecCal_Read_Tar_Ref
;
; :PURPOSE:
;   Read the target reflectance CSV file
;:PARAMS:
;    in_file: Input target reflectance file
; 	 tar_ref: array containing target reflectance data (used to check dimensions)
;
; :RETURNS:
;   upon return, the rar_ref variable contains the data regarding spectral reflectance of the target, as read form 
;   the selected file
;
; :REQUIRES:
;
; :NOTES:
;   Target reflectance file must be a CSV file (';' delimited) with 701 rows(Reflectance from 350 to 1050, with Sampling of 1 nm) and this structure:
;       row1:            wl  ;   Refl
;       row2:           350  ; 0.0001
;       row2:           351  ;  0.0002
;                ....        ....        .....
;       row701:      1050 ;     0.0002
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
FUNCTION SPECCAL_READ_TAR_REF, in_file, tarref
    COMPILE_OPT IDL2,STRICTARRSUBS
    
    ON_IOERROR, jump_err
    str = ''
    nlines=FILE_LINES(in_file)                                                 ; Get n° of lines of the input file
    data =DBLARR(2,nlines-1)
    dims = SIZE(data)
    dims_tar = size (tarref)
    IF dims [1] NE dims_tar[1] OR dims[2] NE dims_tar [2] THEN BEGIN
        mess = DIALOG_MESSAGE(' Dimensions of User Defined Reflectance File are wrong, Please Select another file', TITLE='Warning')
        
        jump_err:
        RETURN, 'Err'
    ENDIF
    OPENR, lun, in_file, /get_lun                                 ; Open the file
    READF, lun, str                                                      ; Read the header
    FOR i=0L, nlines[0]-2 DO BEGIN
        READF, lun, str                                                  ; Read data line
        dataInTheRow=DOUBLE(STRSPLIT(str, ';', count=n_row, /EXTRACT))
        data[0, i]= dataInTheRow[0]                                    ; Read WL
        data[1,i] = dataInTheRow[1]                                 ; Read Irradiance
    ENDFOR
    FREE_LUN, lun
    RETURN, data
    
END


