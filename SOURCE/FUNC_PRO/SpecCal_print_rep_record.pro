;+
;:ROUTINE NAME:
;SPECCAL_PRINT_REP_RECORD
;
; :PURPOSE:
;
; :INPUTS:
;
; :OUTPUTS:
;
; :PARAMS:
;    action: STRING
;           It can be "header" or "record". If "header" is passed the routine opens the input file and writes the headers.
;           Nothing is done if the user chosses to "append" on a old output.
;           If "record" is passed, the routine writes the output for the spectral window considered
;    record: info structure containing outputs for the spectral window analyzed
;    file: Name of the output file
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
; Michele Meroni, Lorenzo Busetto
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

FUNCTION SPECCAL_PRINT_REP_RECORD, action, record, file, dlmtr=dlmtr
    IF KEYWORD_SET(dlmtr) EQ 0 THEN dlmtr=';'
    
    CASE action OF
        'header': BEGIN                                                                ; Open the file and (if not appending) write the headers
            res0 = FILE_SEARCH(file, count=num_file)                ; Search if the file is already existing
            
            IF num_file NE 0 THEN BEGIN
                res = DIALOG_CHOICE( ['Overwrite', 'Append', 'Back To Main'],MESSAGE_TEXT= 'Output File already exists -  Select an Option ' , TITLE = 'Warning')
                IF res EQ 'Overwrite' THEN BEGIN                         ; If user selects to overwrite, write the headers
                    OPENW, lun, file, /get_lun, width=2000, error= error
                    IF error EQ -248 THEN BEGIN                            ; Check if output is already open
                        mess = DIALOG_MESSAGE('An Error Occured while Opening the Output File !',TITLE='Warning')
                        RETURN, 'Err'
                    ENDIF
                    tmp=''
                    FOR i=0, N_TAGS(record)-1 DO tmp=tmp+STRTRIM(record.(i).hdr,2) + dlmtr
                    PRINTF, lun, tmp
                    tmp=''
                    FOR i=0, N_TAGS(record)-1 DO tmp=tmp+STRTRIM(record.(i).units,2) + dlmtr
                    PRINTF, lun, tmp
                    FREE_LUN,lun
                ENDIF
                
                IF res EQ 'Back To Main' THEN BEGIN                     ; If user selects to exit, go back to the GUI
                
                    RETURN, 'Err'
                    
                ENDIF
                
                IF res EQ 'Append' THEN BEGIN                     ; If user selects to append, just check if the file is already open
                
                    OPENW, lun, file, /get_lun, width=2000, error= error,/append
                    IF error EQ -248 THEN BEGIN                            ; Check if output is already open
                        mess = DIALOG_MESSAGE('An Error Occured while Opening the Output File !',TITLE='Warning')
                        RETURN, 'Err'
                    ENDIF
                    FREE_LUN, lun
                    
                ENDIF
            ENDIF ELSE BEGIN                                                          ; If file does not exist, write the headers
                OPENW, lun, file, /get_lun, width=2000, error= error
                
                IF error EQ -248 THEN BEGIN
                    mess = DIALOG_MESSAGE('An Error Occured while Opening the Output File !',TITLE='Warning')
                    RETURN, 'Err'
                ENDIF
                tmp=''
                FOR i=0, N_TAGS(record)-1 DO tmp=tmp+STRTRIM(record.(i).hdr,2) + dlmtr
                PRINTF, lun, tmp
                tmp=''
                FOR i=0, N_TAGS(record)-1 DO tmp=tmp+STRTRIM(record.(i).units,2) + dlmtr
                PRINTF, lun, tmp
                FREE_LUN,lun
            ENDELSE
            
        END
        
        ; If 'record' is passed, then write the "record" structure on the output file
        
        'record': BEGIN                                            ; Write the outputs for the spectral window analyzed
            OPENW, lun, file, /append, /get_lun, width=2000
            tmp=''
            FOR i=0, N_TAGS(record)-2 DO BEGIN
                IF STRTRIM(record.(i).value,2) EQ '-999.00000' THEN  filler = 1 ELSE filler = 0             ; Replace -999 values with ----
                IF filler EQ 0 THEN tmp=tmp+STRTRIM(record.(i).value,2) + dlmtr ELSE  tmp=tmp+' --- ' + dlmtr
            ENDFOR
            ; Write Messages
            FOR i=0, N_ELEMENTS(record.(N_TAGS(record)-1).value)-1 DO BEGIN
            
                IF (record.(N_TAGS(record)-1).value[i] NE '') THEN BEGIN
                    tmp=tmp+record.(N_TAGS(record)-1).value[i] + dlmtr
                ENDIF
            ENDFOR
            PRINTF, lun, tmp
            FREE_LUN,lun
        END
    ENDCASE
    RETURN,'Ok'
END