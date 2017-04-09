
;+
; :DESCRIPTION:
;    Main Program for the SpecCal application.
;    The routine calls the main GUI of the program and calls the
;    processing routines on the basis of User's selections.
;
; :OUTPUTS:
;
; :REQUIRES:
;
; :NOTES:
;
; :AUTHOR: Lorenzo Busetto; Michele Meroni - 
; Environmental Dynamics Remote Sensing Laboratory
; University of Milano-Bicocca
; Milan - Italy (IT)
; 
; 
; :HISTORY:
;
; Created: 27-apr-2009
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
PRO SPECCAL_12

    COMPILE_OPT IDL2
    
    ; Define Code Version
    version = '1.2'
    dlmtr=';'

    
    !error_state.CODE = 0      ; Initialize error code at 0
    
    CATCH, theError                ; Initialize Error catching procedure
    IF theError NE 0 THEN BEGIN
        PRINT, !error_state.MSG
        CATCH, /cancel
        ok = ERROR_MESSAGE(/Traceback)
        RETURN
    ENDIF
    
    ;load report structure
    record=SPECCAL_REPORT_STRUCTURE_DEFINE()
    
    ; Identify the Installation Folder  and determine the name of the "PREVIOUS " Directory
    
    inst_dir = PROGRAMROOTDIR()
   
    ;PRINT, inst_dir
    
    ; Call the MAIN GUI Management Function
    ; The function returns an INFO Structure (info) containing the selected processing parameters, and a full_info structure that is saved in previous
    ; file if all completed well. FULL_INFO contains all data necessary to initialize the GUI at the next run
start:
    
    full_info = ''
    info = SPECCAL_GUI(inst_dir, version, full_info)
    
    ; Check if Quit or Close Window has been selected
    IF info.QUIT EQ 1 THEN BEGIN
        ;    quit_mess = DIALOG_MESSAGE('Processing Aborted !',/INFORMATION)
        RETURN
    ENDIF
    
    ; Retrieve data from MODTRAN and INPUT files
    record.mod_file.VALUE	= info.MOD_FILE
    info_csv = SPECCAL_RDCSV(info.MOD_FILE)
    xMod = info_csv.WL  &  yMod=info_csv.VALS  &  fwhmMod = info_csv.FWHM
    
    ; Correct MODTRAN wavelenghts from vacuum to air
    xMod= SPECCAL_TRANSFORM_WL_FROM_VAC_TO_AIR(xMod)
    
    info_csv = SPECCAL_RDCSV(info.IN_FILE)
    record.in_file.VALUE   = info.IN_FILE
    record.nominal_fwhm.VALUE	= info.NOMFWHM
    xObs = info_csv.WL  &  yObs=info_csv.VALS  &  nominalFwhmObs = info.NOMFWHM
    IF (nominalFwhmObs LE fwhmMod) THEN BEGIN
        err = DIALOG_MESSAGE('The FWHM of observation is lower than the FWHM of Simulation, the program cannot run.',/ERROR)
        GOTO, jump_start
    ENDIF
    
    ; Open output file and write headers
    out_rd = SPECCAL_PRINT_REP_RECORD('header', record, info.OUT_FILE, dlmtr=dlmtr)
    IF out_rd EQ 'Err' THEN GOTO, jump_start
    
    ; Perform a "Basic" radiometric calibration of observed data if the input is in COUNTS
    
    IF info.umeas EQ 1  THEN SPECCAL_RADCAL, XOBS, YOBS, NOMINALFWHMOBS,  info.TARREF, XMOD,YMOD, $
        FWHMMOD, info.meastype
        
    ; Start processing of selected ranges
        
    sel_ranges = WHERE(info.RANGESEL EQ 1, count_ranges)  ; Identify Selected Ranges
    
    ; Write first lines of log file
    
    log_file = inst_dir+'LOG_FILES\SpecCal_log_'+FILE_BASENAME(info.OUT_FILE)+'.log'
    OPENW, lun_log, log_file, width = 5000, /GET_LUN
    PRINTF, lun_log, '---------------------------'
    PRINTF, lun_log, 'SpecCal Processing'
    PRINTF, lun_log, '---------------------------'
    PRINTF, lun_log, ''
    PRINTF, lun_log,'Input File: '+ info.IN_FILE
    PRINTF, lun_log,'MODTRAN Simulation File: '+info.MOD_FILE
    PRINTF, lun_log,'Output File: '+info.OUT_FILE
    PRINTF, lun_log, '---------------------------'
    PRINTF, lun_log, ''
    
    ; print Simulation Specifications
    
    ;Write first lines of command line output
    PRINT,  '---------------------------'
    PRINT,  'SpecCal Processing'
    PRINT,  '---------------------------'
    PRINT,  ''
    PRINT,'Input File: '+ info.IN_FILE
    PRINT, 'MODTRAN Simulation File: '+info.MOD_FILE
    PRINT, 'Output File: '+info.OUT_FILE
    PRINT,  '---------------------------'
    PRINT, ''
    
    ; Initialize the Progress Bar
    progressBar = OBJ_NEW("PROGRESSBAR", ysize = 40, TITLE='SpecCal Processing')
    progressBar -> Start
    progressBar -> UPDATE, 0, Text='Processing Range: '+STRTRIM(1,2)+'  of  '+STRTRIM(count_ranges,2)
    
    ; Start Cycling on the selected ranges
    
    FOR i=0, count_ranges-1 DO BEGIN
    
        IF progressBar -> CheckCancel() THEN BEGIN                                      ; Check for Cancel on ProgressBar
            ok = DIALOG_MESSAGE('The user cancelled operation.')
            progressBar -> DESTROY
            GOTO, jump_start
        ENDIF
        
        ; Update Progress Bar
        progressBar -> UPDATE, FLOAT(i)/FLOAT(count_ranges)*100, Text='Processing Range: '+STRTRIM(i+1,2)+'  of  '+STRTRIM(count_ranges,2)
        
        ; Write LOG messages
        path_out=FILE_DIRNAME(info.OUT_FILE,/MARK_DIRECTORY)+FILE_BASENAME(info.OUT_FILE, '.csv')
        PRINT, 'Processing Range    ', STRTRIM(i+1,2), '    of: ', STRTRIM(count_ranges,2),'    '   ,'Spectral Interval: ', (info.RANGES[*,sel_ranges[i]])
        PRINTF, lun_log, 'Processing Range  ', STRTRIM(i+1,2), '    of: ', STRTRIM(count_ranges,2),'    '   ,'Spectral Interval: ', (info.RANGES[*,sel_ranges[i]])
        
        ; Launch the processing
        info_out=SPECCAL_CORE(XOBS, YOBS, NOMINALFWHMOBS,  info.TARREF, XMOD,YMOD, $
            FWHMMOD, REFORM(info.RANGES[*,sel_ranges[i]]), $
            info.METHTYPE,  info.SSL, info.FWL, info.DOPLOT, info.DOSAVEIMAGE, info.SAVE_CSV, path_out, WARNINGS=WARNINGS, info.meastype)
            
        ; Fill the "record" structure which contains outputs
            
        record.range_wl0.VALUE=STRTRIM(STRING(info.RANGES[0,sel_ranges[i]], format='(i4)'),2)
        record.range_wl1.VALUE=STRTRIM(STRING(info.RANGES[1,sel_ranges[i]], format='(i4)'),2)
        record.range_center.VALUE=STRTRIM((info.RANGES[1,sel_ranges[i]]+info.RANGES[0,sel_ranges[i]])/2.0D,2)
        IF info_out.P[0] NE -999 THEN BEGIN
            record.spec_shift.VALUE=-info_out.P[0]		;invert the signo of the shift
            record.spec_fwhm.VALUE=info_out.P[1]
            record.cost.VALUE=info_out.COST
            record.status.VALUE=info_out.STATUS
        ENDIF ELSE BEGIN
            record.spec_shift.VALUE=-999		;invert the signo of the shift
            record.spec_fwhm.VALUE=-999
            record.cost.VALUE=-999
            record.status.VALUE=-999
            record.status.VALUE=info_out.STATUS
        ENDELSE
        
        if info.umeas EQ 0 then record.umeas.VALUE = 'Physical Units' ELSE record.umeas.VALUE = 'Counts'
        if info.meastype EQ 0 then record.meastype.VALUE = 'Reflected Radiance' ELSE record.meastype.VALUE = 'Solar Irradiance'
        if info.TARREF_FNAME EQ  INST_DIR+'TARGET_REFLECTANCES\Full_reflectance.csv' then record.tarref_file.VALUE='None' $
                ELSE record.tarref_file.VALUE=info.TARREF_FNAME
        record.shift_limit.VALUE=info.SSL
        record.fwhm_limit.VALUE=info.FWL
        if info.methtype eq 0 then  record.method_type.VALUE= 'Ratio' else record.method_type.VALUE= 'Correlation' 
        record.nobs.VALUE=info_out.NOBS
        if info.methtype EQ 0 then record.smoothFltHW.VALUE=string(info_out.FILTWING) else record.smoothFltHW.VALUE=' --- ' 
        record.RW.VALUE=info.FLTW
        record.warnings.VALUE=warnings
        
        ; Print outputs on "report" CSV file
        out_rd = SPECCAL_PRINT_REP_RECORD('record', record, info.OUT_FILE, dlmtr=dlmt)
        
    ;End of cycle on selected ranges
    ENDFOR
    message = dialog_message('Processing Completed Succesfully !', /Information)
    ;  ; Destroy the "ProgressBar" widget
    
    progressBar -> DESTROY
    
    ; If all completed well, save the info in the previous file
    info = full_info
    SAVE, info, FILENAME = inst_dir+'\PREVIOUS\'+'previous_SpecCal.sav'
    jump_start:                 ; IF errors encountered, go back to the MAIN GUI
    GOTO, start
    
    FREE_LUN, lun_log       ; Close and free lun of log file
    
; End processing
    
END


