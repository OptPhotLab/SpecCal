;+
; :AUTHOR: Lorenzo Busetto
;-
;- ------------------------------------------------------------------------------------------------ -
; - Event handling procedure for SpecCal_GUI                             -
; - ------------------------------------------------------------------------------------------------ -

PRO SPECCAL_GUI_EVENT,event

    COMPILE_OPT IDL2
    CATCH, theError
    IF theError NE 0 THEN BEGIN
        PRINT, !error_state.MSG
        CATCH, /cancel
        ok = ERROR_MESSAGE(/Traceback)
        WIDGET_CONTROL, wwidget, /destroy
        RETURN
    ENDIF
    
    
    wWidget =  Event.TOP                                                                                ; Get the TLB "Address"
    WIDGET_CONTROL, wwidget,  get_uvalue  = info_ptr,/NO_COPY            ; Retrieve the pointer to the info structure frmo UVALUE of TLB
    info=*info_ptr                                                                                            ; Retrieve INFO Structure from the pointer
    selection = WIDGET_INFO(event.ID, /UNAME)                                       ; Get the UNAME of the widget that generated the event
    
    
    ; Check For Resize Event
    IF (event.HANDLER EQ event.ID) AND (event.HANDLER EQ info.MAIN_WID ) THEN BEGIN
    
        geometry = WIDGET_INFO(event.TOP, /geometry)
        
        IF event.Y LT info.YSIZE  $
            THEN BEGIN
            WIDGET_CONTROL, event.TOP, ysize = event.Y
            IF event.X LE info.XSIZE $+2*geometry.XPAD+geometry.SPACE+15 $
                THEN WIDGET_CONTROL, event.TOP, xsize = event.X $;+2*geometry.XPAD+geometry.SPACE+15 $
            ELSE WIDGET_CONTROL, event.TOP, xsize = info.XSIZE ;+2*geometry.XPAD+geometry.SPACE+15
            
        ENDIF ELSE BEGIN
            WIDGET_CONTROL, event.TOP, ysize = info.YSIZE
            IF event.X LE info.XSIZE $;+2*geometry.XPAD+geometry.SPACE $
                THEN WIDGET_CONTROL, event.TOP, xsize = event.X $;+2*geometry.XPAD+geometry.SPACE $
            ELSE WIDGET_CONTROL, event.TOP, xsize = info.XSIZE; +2*geometry.xpad+geometry.space
        ENDELSE
        
    ENDIF
    
    ;- On the basis of the widget that generated the event, do the necessary operations
    
    CASE selection OF
    
        'in_file_browse':  BEGIN
        
            IF XREGISTERED('in_file_browse') EQ  0 THEN BEGIN
            
                ; Select Input and set the File names on the widgets
                in_dir = info.IN_DIR     ; Retrieve Directory of current file name
                IN_FILE =  DIALOG_PICKFILE(/READ,dialog_parent=event.ID,title ='Select Input File',$  ; Realize Dialog Window
                    path=in_dir, filter = '*.csv',/must_exist )
                    
                IF IN_FILE NE'' THEN BEGIN
                    info.IN_FILE = in_file
                    info.IN_DIR = FILE_DIRNAME(in_file, /MARK_DIRECTORY)
                    path_seps = STRSPLIT(info.IN_FILE,PATH_SEP())
                    in_basename = FILE_BASENAME(info.IN_FILE)
                    ipos =  STRPOS(in_basename,'.')
                    IF (ipos NE -1) THEN in_basename = STRMID(in_basename,0,ipos)
                    info.OUT_FILE = info.INST_DIR+'OUTPUTS\'+in_basename+'_report.csv'
                    IF STRLEN (info.IN_FILE) LE 50 THEN BEGIN
                        info.IN_FILE_LAB = info.IN_FILE
                        info.OUT_FILE_LAB = info.OUT_FILE
                    ENDIF ELSE BEGIN
                        info.IN_FILE_LAB = PATH_SEP()+'..'+ PATH_SEP()+ STRMID(info.IN_FILE,path_seps[N_ELEMENTS(path_seps)-3])
                        info.OUT_FILE_LAB= PATH_SEP()+'..'+ PATH_SEP()+ STRMID(info.OUT_FILE,path_seps[N_ELEMENTS(path_seps)-3])
                    ENDELSE
                    
                    ; Get and Set File characteristics in accordance to new input file
                    tmp_info = SPECCAL_INIT_INFO(info.INST_DIR)
                    tmp = tmp_info.SSL  & WIDGET_CONTROL, info.SSL_WID, SET_VALUE=TMP
                    tmp = tmp_info.FWL  & WIDGET_CONTROL, info.FWL_WID, SET_VALUE=TMP
                    tmp = tmp_info.FLTW & WIDGET_CONTROL, info.FLTW_WID, SET_VALUE=TMP
                    info.SSL = tmp_info.SSL & info.FWL = tmp_info.FWL &  info.FLTW = tmp_info.FLTW
                    info = SPECCAL_REINIT_RANGES(info)
                    
                    ; Set sensitivity of default button to 1 if at least one range can be selected
                    
                    WIDGET_CONTROL, info.DEFBUT_WID, sensitive = MAX(info.SENSEL)
                    
                ENDIF
                
            ENDIF
            
            *info_ptr=info
            WIDGET_CONTROL,wwidget, set_uvalue = info_ptr
        END
        
        ; Select Input and set the File names on the widgets
        'out_file_browse':  BEGIN
        
            IF XREGISTERED('out_file_browse') EQ  0 THEN BEGIN
            
                out_dir = info.OUT_DIR     ; Retrieve Directory of current file name
                OUT_FILE =  DIALOG_PICKFILE(/WRITE,dialog_parent=event.ID,title ='Select Output File',$  ; Realize Dialog Window
                    path=out_dir, filter = '*.csv' )
                    
                IF OUT_FILE NE '' THEN BEGIN
                    ; Check for extension, add if needed
                    tmp_name = FILE_BASENAME(OUT_FILE)
                    tmp_dir = FILE_DIRNAME(OUT_FILE,/MARK_DIRECTORY)
                    split = STRSPLIT(tmp_name, '.csv', /REGEX,/extract,/PRESERVE_NULL)
                    IF STRLEN(split[0]) NE STRLEN(tmp_name)-4 THEN tmp_name = split+'.csv'
                    out_file = tmp_dir+tmp_name
                    
                    info.OUT_FILE = out_file
                    info.OUT_DIR = FILE_DIRNAME(out_file, /MARK_DIRECTORY)
                    path_seps = STRSPLIT(info.OUT_FILE,PATH_SEP())
                    IF STRLEN (info.OUT_FILE) LE 50 THEN info.OUT_FILE_LAB = info.OUT_FILE  $
                    ELSE info.OUT_FILE_LAB = PATH_SEP()+'..'+ PATH_SEP()+ STRMID(info.OUT_FILE,path_seps[N_ELEMENTS(path_seps)-3])
                    WIDGET_CONTROL, info.OUT_WID, set_value=info.OUT_FILE_LAB  ; Modify Label on the Output File Widget
                ENDIF
                
            ENDIF
            *info_ptr=info
            WIDGET_CONTROL,wwidget, set_uvalue = info_ptr
        END
        
        ; Select MODTRAN File and set  File names on the widgets
        'mod_file_browse':  BEGIN
        
            IF XREGISTERED('mod_file_browse') EQ  0 THEN BEGIN
            
                mod_dir = info.MOD_DIR     ; Retrieve Directory of current file name
                MOD_FILE =  DIALOG_PICKFILE(/WRITE,dialog_parent=event.ID,title ='Select MODTRAN File', path=mod_dir, filter = '*.csv',/MUST_EXIST )  ; Realize Dialog Window
                
                
                IF MOD_FILE NE '' THEN BEGIN
                    info.MOD_FILE = mod_file
                    info.MOD_DIR = FILE_DIRNAME(mod_file)
                    path_seps = STRSPLIT(info.MOD_FILE,PATH_SEP())
                    IF STRLEN (info.MOD_FILE) LE 50 THEN info.MOD_FILE_LAB = info.MOD_FILE  $
                    ELSE info.MOD_FILE_LAB = PATH_SEP()+'..'+ PATH_SEP()+ STRMID(info.MOD_FILE,path_seps[N_ELEMENTS(path_seps)-3])
                    tmp_info = SPECCAL_INIT_INFO(info.INST_DIR)
                    tmp = tmp_info.SSL  & WIDGET_CONTROL, info.SSL_WID, SET_VALUE=TMP
                    tmp = tmp_info.FWL  & WIDGET_CONTROL, info.FWL_WID, SET_VALUE=TMP
                    tmp = tmp_info.FLTW & WIDGET_CONTROL, info.FLTW_WID, SET_VALUE=TMP
                    info.SSL = tmp_info.SSL & info.FWL = tmp_info.FWL &  info.FLTW = tmp_info.FLTW
                    info = SPECCAL_REINIT_RANGES(info)
                    WIDGET_CONTROL, info.DEFBUT_WID, sensitive = MAX(info.SENSEL)
                    
                    *info_ptr=info
                    WIDGET_CONTROL,wwidget, set_uvalue = info_ptr
                ENDIF
                
            ENDIF
        END
        ; Reset Parameters to Defaults
        
        'def_par_but':  BEGIN
            tmp_info = SPECCAL_INIT_INFO(info.INST_DIR)
            tmp = tmp_info.SSL	&	WIDGET_CONTROL, info.SSL_WID, SET_VALUE=TMP
            tmp = tmp_info.FWL	&	WIDGET_CONTROL, info.FWL_WID, SET_VALUE=TMP
            tmp = tmp_info.FLTW	&	WIDGET_CONTROL, info.FLTW_WID, SET_VALUE=TMP
            info.SSL = tmp_info.SSL	&	info.FWL = tmp_info.FWL	&  info.FLTW = tmp_info.FLTW
            info = SPECCAL_REINIT_RANGES(info)
            *info_ptr=info
            WIDGET_CONTROL,wwidget, set_uvalue = info_ptr
        END
        
        'sgol_wid': BEGIN
        
            WIDGET_CONTROL,info.FLTW_WID, get_value = tmp   &   info.FLTW = tmp
            info = SPECCAL_REINIT_RANGES_UPDATE(info)
            *info_ptr=info
            WIDGET_CONTROL,wwidget, set_uvalue = info_ptr
            
            
        END
        
        ; Reset Ranges to Defaults
        
        'def_range_but':  BEGIN
        
            info = SPECCAL_REINIT_RANGES(info)
            *info_ptr=info
            WIDGET_CONTROL,wwidget, set_uvalue = info_ptr
        END
        
        'meastype_wid':BEGIN
        
        ; Retrieve Selected Value
        WIDGET_CONTROL, info.MEASTYPE_WID,GET_VALUE=tmp
        info.MEASTYPE_IND = WIDGET_INFO(info.MEASTYPE_WID,/DROPLIST_SELECT)
        IF info.MEASTYPE_IND EQ 1 THEN BEGIN
            WIDGET_CONTROL, info.TARREF_WID,SET_DROPLIST_SELECT = 0
            WIDGET_CONTROL, info.TARREF_WID,SENSITIVE= 0
            info.TARREF_IND = 0
        ENDIF ELSE   WIDGET_CONTROL, info.TARREF_WID,SENSITIVE=  1
        *info_ptr=info
        WIDGET_CONTROL,wwidget, set_uvalue = info_ptr
        
    ENDCASE
    
    'umeas_wid':BEGIN
    
    ; Retrieve Selected Value
    WIDGET_CONTROL, info.UMEAS_WID,GET_VALUE=tmp
    info.UMEAS_IND = WIDGET_INFO(info.UMEAS_WID,/DROPLIST_SELECT)
    *info_ptr=info
    WIDGET_CONTROL,wwidget, set_uvalue = info_ptr
    
ENDCASE

'tarref_wid': BEGIN
    ; Retrieve Target Reflectance from the selected file
    WIDGET_CONTROL, info.TARREF_WID,GET_VALUE=tmp
    old_ind = info.TARREF_IND
    info.TARREF_IND = WIDGET_INFO(info.TARREF_WID,/DROPLIST_SELECT)
    sel_file = tmp[info.TARREF_IND]
    IF sel_file EQ 'Change File' THEN BEGIN                                                                                                     ; If the user wants to select a different file
        tar_dir = FILE_DIRNAME(info.TARREF_FNAME,/MARK_DIRECTORY)
        new_fNAME = DIALOG_PICKFILE(TITLE='Select Target Reflectance File', path = tar_dir)
        IF new_fNAME NE '' THEN BEGIN                                                                                                              ; If a file was selected, update widget and list
            info.TARREF_FNAME = new_fname
            WIDGET_CONTROL, info.TARREF_WID,GET_VALUE  =old_list
            new_list = [old_list[0:(N_ELEMENTS(old_list)-2)],FILE_BASENAME(info.TARREF_FNAME)]
            info.REFNAMES = new_list
            WIDGET_CONTROL, info.TARREF_WID,SET_VALUE  = new_list
            WIDGET_CONTROL, info.TARREF_WID,SET_DROPLIST_SELECT = N_ELEMENTS(new_list)-1
            info.TARREF_IND = N_ELEMENTS(new_list)-1
        ENDIF ELSE BEGIN                                                                                                                                         ; If a file was not selected, keep old selection
            IF info.TARREF_FNAME EQ info.INST_DIR+'TARGET_REFLECTANCES\Standard_White_Reference.csv' THEN $
                info.TARREF_IND = 0 ELSE info.TARREF_IND =N_ELEMENTS(info.REFNAMES)-1
            WIDGET_CONTROL, info.TARREF_WID,SET_DROPLIST_SELECT = info.TARREF_IND
        ENDELSE
        
    ENDIF ELSE BEGIN
        IF info.TARREF_IND NE N_ELEMENTS(info.REFNAMES)-1 THEN BEGIN
            WIDGET_CONTROL, info.TARREF_WID,GET_VALUE  =old_list
            new_list = [old_list[0:(N_ELEMENTS(old_list)-2)],'']
            info.REFNAMES = new_list
            WIDGET_CONTROL, info.TARREF_WID,SET_VALUE  = new_list
            WIDGET_CONTROL, info.TARREF_WID,SET_DROPLIST_SELECT = N_ELEMENTS(new_list)-1
            IF info.TARREF_IND EQ 1 THEN info.TARREF_FNAME = info.INST_DIR+'TARGET_REFLECTANCES\Standard_White_Reference.csv'
            IF info.TARREF_IND EQ 0 THEN info.TARREF_FNAME = info.INST_DIR+'TARGET_REFLECTANCES\Full_reflectance.csv'
            WIDGET_CONTROL, info.TARREF_WID,SET_DROPLIST_SELECT = info.TARREF_IND
        ENDIF ELSE BEGIN
            WIDGET_CONTROL, info.TARREF_WID,GET_VALUE  =old_list
            IF old_list [info.TARREF_IND] EQ '' THEN BEGIN
                info.TARREF_IND = old_ind
                WIDGET_CONTROL, info.TARREF_WID,SET_DROPLIST_SELECT = info.TARREF_IND
            ENDIF
        ENDELSE
    ENDELSE
    *info_ptr=info
    WIDGET_CONTROL,wwidget, set_uvalue = info_ptr
    PRINT, info.TARREF_FNAME
END

; Sensitize\desensitize selected ranges

'sel1_wid':  BEGIN

    info.RANGESEL[0] = event.VALUE
    WIDGET_CONTROL,info.R1_WID_RG,sensitive = event.VALUE
    *info_ptr=info
    WIDGET_CONTROL,wwidget, set_uvalue = info_ptr

END

'sel2_wid':  BEGIN

    info.RANGESEL[1] = event.VALUE
    WIDGET_CONTROL,info.R2_WID_RG,sensitive = event.VALUE
    *info_ptr=info
    WIDGET_CONTROL,wwidget, set_uvalue = info_ptr
END

'sel3_wid':  BEGIN

    info.RANGESEL[2] = event.VALUE
    WIDGET_CONTROL,info.R3_WID_RG,sensitive = event.VALUE
    *info_ptr=info
    WIDGET_CONTROL,wwidget, set_uvalue = info_ptr
END

'sel4_wid':  BEGIN

    info.RANGESEL[3] = event.VALUE
    WIDGET_CONTROL,info.R4_WID_RG,sensitive = event.VALUE
    *info_ptr=info
    WIDGET_CONTROL,wwidget, set_uvalue = info_ptr
END

'sel5_wid':  BEGIN

    info.RANGESEL[4] = event.VALUE
    WIDGET_CONTROL,info.R5_WID_RG,sensitive = event.VALUE
    *info_ptr=info
    WIDGET_CONTROL,wwidget, set_uvalue = info_ptr
END

'sel6_wid':  BEGIN

    info.RANGESEL[5] = event.VALUE
    WIDGET_CONTROL,info.R6_WID_RG,sensitive = event.VALUE
    *info_ptr=info
    WIDGET_CONTROL,wwidget, set_uvalue = info_ptr
END

'sel7_wid':  BEGIN

    info.RANGESEL[6] = event.VALUE
    WIDGET_CONTROL,info.R7_WID_RG,sensitive = event.VALUE
    *info_ptr=info
    WIDGET_CONTROL,wwidget, set_uvalue = info_ptr
END

'sel8_wid':  BEGIN

    info.RANGESEL[7] = event.VALUE
    WIDGET_CONTROL,info.R8_WID_RG,sensitive = event.VALUE
    *info_ptr=info
END

'sel9_wid':  BEGIN

    info.RANGESEL[8] = event.VALUE
    WIDGET_CONTROL,info.R9_WID_RG,sensitive = event.VALUE
    *info_ptr=info
    WIDGET_CONTROL,wwidget, set_uvalue = info_ptr
END

'sel10_wid':  BEGIN

    info.RANGESEL[9] = event.VALUE
    WIDGET_CONTROL,info.R10_WID_RG,sensitive = event.VALUE
    *info_ptr=info
    WIDGET_CONTROL,wwidget, set_uvalue = info_ptr
END

'sel11_wid':  BEGIN

    info.RANGESEL[10] = event.VALUE
    WIDGET_CONTROL,info.R11_WID_RG,sensitive = event.VALUE
    *info_ptr=info
    WIDGET_CONTROL,wwidget, set_uvalue = info_ptr
END

'sel12_wid':  BEGIN

    info.RANGESEL[11] = event.VALUE
    WIDGET_CONTROL,info.R12_WID_RG,sensitive = event.VALUE
    *info_ptr=info
    WIDGET_CONTROL,wwidget, set_uvalue = info_ptr
END

'sel13_wid':  BEGIN

    info.RANGESEL[12] = event.VALUE
    WIDGET_CONTROL,info.R13_WID_RG,sensitive = event.VALUE
    *info_ptr=info
    WIDGET_CONTROL,wwidget, set_uvalue = info_ptr
END


'selBroad1_wid':  BEGIN

    info.RANGESEL_BROAD1 = event.VALUE
    WIDGET_CONTROL,info.RBROAD1_WID_RG,sensitive = event.VALUE
    *info_ptr=info
    WIDGET_CONTROL,wwidget, set_uvalue = info_ptr
;      print, info.rangesel_broad1
END

'selUser_wid':  BEGIN

    info.RANGESEL_USER = event.VALUE
    WIDGET_CONTROL,info.RUSER_WID_RG,sensitive = event.VALUE
    *info_ptr=info
    WIDGET_CONTROL,wwidget, set_uvalue = info_ptr
END

;- If "Help" then open Help File

'Help': BEGIN

    ONLINE_HELP, BOOK=info.HELP_FILE, /FULL_PATH
    *info_ptr=info
    WIDGET_CONTROL,wwidget, set_uvalue = info_ptr
    
    
END

;- If "About" then open About File

'About': BEGIN

    ONLINE_HELP, BOOK=info.ABOUT_FILE, /FULL_PATH
    *info_ptr=info
    WIDGET_CONTROL,wwidget, set_uvalue = info_ptr
    
    
END

;- If "Start" then make some checks,  get all info from the widgets and start processing

'Start': BEGIN
    info.QUIT = 0
    SPECCAL_GUI_GETDATA,info,info_ptr
    IF info.IN_FILE EQ '' THEN BEGIN
        file_mess = DIALOG_MESSAGE('Please Select an Input Measured Irradiance File', /INFORMATION)
        *info_PTR=info
        WIDGET_CONTROL,wwidget, set_uvalue = info_ptr
        RETURN
    ENDIF
    
    IF info.OUT_FILE EQ '' THEN BEGIN
        file_mess = DIALOG_MESSAGE('Please Select an Output Report File', /INFORMATION)
        *info_PTR=info
        WIDGET_CONTROL,wwidget, set_uvalue = info_ptr
        RETURN
    ENDIF ELSE BEGIN
        res0 = FILE_SEARCH(info.OUT_FILE , count=num_file)                ; Search if the file is already existing
        IF num_file NE 0 THEN BEGIN
            res = DIALOG_MESSAGE( ['Output File already exists. Outputs will be overwritten or appended','Do you want to Continue ? '], $
                /QUESTION, TITLE='Warning')
            IF res EQ 'No' THEN BEGIN
                *info_PTR=info
                WIDGET_CONTROL,wwidget, set_uvalue = info_ptr
                RETURN
            ENDIF
        ENDIF
    ENDELSE
    
    IF info.MOD_FILE EQ '' THEN BEGIN
        file_mess = DIALOG_MESSAGE('Please Select an Input MODTRAN Irradiance File', /INFORMATION)
        *info_PTR=info
        WIDGET_CONTROL,wwidget, set_uvalue = info_ptr
        RETURN
    ENDIF
    
    IF MAX(info.RANGESEL) EQ 0 AND info.RANGESEL_BROAD1 EQ 0 AND info.RANGESEL_USER EQ 0 THEN BEGIN
        file_mess = DIALOG_MESSAGE('Please Select at least one processing range', /INFORMATION)
        *info_PTR=info
        WIDGET_CONTROL,wwidget, set_uvalue = info_ptr
        RETURN
    ENDIF
    
    WIDGET_CONTROL,wwidget, set_uvalue = info_ptr
    WIDGET_CONTROL, wwidget, /destroy
    RETURN
    
END

;- If "Quit" then destroy the GUI and exit

'Quit': BEGIN
    info.QUIT = 1
    *info_PTR=info
    WIDGET_CONTROL,wwidget, set_uvalue = info_ptr
    WIDGET_CONTROL, wwidget, /destroy
    RETURN
END

ELSE : WIDGET_CONTROL,wwidget, set_uvalue = info_ptr
ENDCASE

*info_ptr=info
WIDGET_CONTROL,wwidget, set_uvalue = info_ptr

END

;+
;:ROUTINE NAME:
;SPECCAL_GUI
;
; :PURPOSE:
; MAIN Selection GUI for the SpecCal application
;
; :INPUTS:
;
; :OUTPUTS:
;       Structure containing the fields:
;
;       in_file: Name of Input File
;
;       ssl: Nominal SSI of Input File
;
;        fwl:info.FWL,
;
;        METHTYPE: Selected MEthod - 0 = Smooth Ratio; 1 = Correlation
;
;        fltW: Range Widening parameter - indicates how much far from the absorption line the user can select the WL for each range
;
;        ranges: Minimum and Maximum WL selected by the user at each spectral window
;
;        tarref_fname: Name of the target reflectance file
;
;        nomfwhm:Nominal FWHM of Input File
;
;        rangesel: [0,1] Array indicating wether the specified range was selected for processing. 1 = Process Range
;        ;
;        tarref: Reflectance of the target. Array with 2 columns, 701 elements (From 350 to 1050 - SSI = 1 nm)
;
;        out_file: Name of the output file
;
;        mod_file: name of the MODTRAN simulation file
;
;        doplot: IF = 1 plot to screen  the outputs. Default = 1
;
;        doSaveImage: IF = 1 then save the plots to file. Default: 1
;
;        dosave_csv: IF = 1 Save the output csv files
;
;        quit: IF = 0, indicates to quit the program
;
; :PARAMS:
;    inst_dir: Installation Directory of SpecCal
;    version: Version Number
;
; :RETURNS:
;
; :REQUIRES:
;
; :NOTES:
;
; :HISTORY:
;
;: Created:	19-feb-2010
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
;
;-

FUNCTION SPECCAL_GUI,inst_dir,version,info
    COMPILE_OPT IDL2
    
    
    ; Build or retrieve the parameters for building the GUI
    jump:
    
    prev_dir = inst_dir+'PREVIOUS'+PATH_SEP()
    
    ; Check for the existence of the Previous Directory. If not present, create it
    
    prevdir_control = FILE_SEARCH(prev_dir, /MARK_DIRECTORY,/TEST_DIRECTORY)
    IF prevdir_control EQ '' THEN FILE_MKDIR,prev_dir
    
    ; Check for the existence of the Previous_SpecCal.sav file in the previous directory.
    ; If the file is present, restore the last preferences, otherwise set the INFO structure to
    ; dummy values
    
    check_prev = FILE_SEARCH(prev_dir+'previous_SpecCal.sav', count=count)
    IF check_prev NE '' THEN BEGIN
    
        ;    init_info = SPECCAL_INIT_INFO(inst_dir)
        RESTORE,prev_dir+ 'previous_SpecCal.sav'    ; Restore Previous File
        info.QUIT = 1
    ; Define the limit WL for the different ranges
        
    ENDIF ELSE BEGIN
    
    
        ; If not retrieving from previous files, set the ranges at default values and build a "dummy" info
        ; structure
    
        info = SPECCAL_INIT_INFO(inst_dir)
        
    ENDELSE
    
    ; Initialize Useful Variables
    
    stdfont18 = 'Arial*22*Bold'
    stdfont14 = 'Arial*14*Bold'
    filesfont = 'Arial*14*Bold'
    under12 = 'Arial*12*Underline'
    head_font = 'Arial*14*Bold*UNDERLINE'
    methnames = ['Ratio','Correlation']
    Labels = ['Fe,Ca+ (S)', 'Hd (S)', 'Fe,Ca,Hg,Fe (S)','Hb,Fe (S)','Mg,Fe (S)','Na (S)','Ha (S)', $
        'O2 (T)','Water Vapor (T)','O2 (T)','O2, Water Vapor (T)','CaII  (S)','Water Vapor (T)', $
        'Broad', 'User Defined']
        
    ; Start Building the GUI
        
    ; Define TLB
    info.MAIN_WID = WIDGET_BASE(title = 'SpecCal v.'+version, /base_align_center, $
        xoffset = 200, yoffset = 200, COLUMN = 1, /TLB_SIZE_EVENTS,Y_SCROLL_SIZE =760, TLB_FRAME_ATTR=8)
        
    fake_wid = WIDGET_LABEL(info.MAIN_WID,value = '', uname = 'fake_label',font=stdfont18, ysize = 4)
    
    ; Define Input and Output Files Widgets
    
    ; Input Files
    files_lab = WIDGET_LABEL(info.MAIN_WID,value = 'Files Selection', uname = 'files_label',font=stdfont18)
    files_wid= WIDGET_BASE(info.MAIN_WID,/COLUMN, FRAME =1)
    input_wid = WIDGET_BASE(files_wid, /ROW,/ALIGN_LEFT)
    meas_lab = WIDGET_LABEL(input_wid, value = 'Measured File:', uname = 'fake_lab',/ALIGN_LEFT, font = filesfont, scr_xsize = 108)
    info.IN_WID = CW_FIELD(input_wid, value = info.IN_FILE_LAB, uname = 'in_wid',title = '',XSIZE = 58, font = filesfont,/NOEDIT)
    in_file_browse = WIDGET_BUTTON(input_wid, value = 'Change', uname = 'in_file_browse', xsize = 54)
    
    ; Input Characteristics Labels
    char_labels =WIDGET_BASE(files_wid, /ROW,/ALIGN_LEFT, SPACE=25)
    fake_lab = WIDGET_LABEL(char_labels, value = '', uname = 'fake_lab',/ALIGN_LEFT, scr_xsize = 86)
    char_lab1= WIDGET_LABEL(char_labels, value = 'Measurement Type', uname = 'char_lab1', font = head_font,/ALIGN_LEFT, SCR_XSIZE=120)
    char_lab2= WIDGET_LABEL(char_labels, value = 'Target Reflectance', uname = 'char_lab2', font = head_font,/ALIGN_LEFT, SCR_XSIZE=120)
    char_lab3= WIDGET_LABEL(char_labels, value = 'Measurement Units', uname = 'char_lab3', font = head_font,/ALIGN_LEFT, SCR_XSIZE=120)
    
    input_char_wid = WIDGET_BASE(files_wid, /ROW,/ALIGN_LEFT, SPACE=25)
    fake_lab = WIDGET_LABEL(input_char_wid, value = '', uname = 'fake_lab', scr_xsize = 86,/ALIGN_LEFT)
    info.MEASTYPE_WID = WIDGET_DROPLIST(input_char_wid, uname = 'meastype_wid', value = info.MEASTYPENAMES, uvalue = info.MEASTYPENAMES[info.MEASTYPE_IND], SCR_XSIZE=120,/ALIGN_LEFT)
    info.TARREF_WID = WIDGET_DROPLIST(input_char_wid, uname = 'tarref_wid', value = info.REFNAMES, uvalue = info.REFNAMES[info.TARREF_IND], SCR_XSIZE=120,/ALIGN_LEFT)
    info.UMEAS_WID = WIDGET_DROPLIST(input_char_wid, uname = 'umeas_wid', value =info.UMEAS_NAMES, uvalue = info.UMEAS_NAMES[info.UMEAS_IND], SCR_XSIZE=120,/ALIGN_LEFT)
    
    ; MODTRAN File
    modtran_wid = WIDGET_BASE(files_wid,/ROW)
    mod_lab = WIDGET_LABEL(modtran_wid, value = 'MODTRAN File:', uname = 'fake_lab', font = filesfont, scr_xsize = 108,/ALIGN_LEFT)
    info.MOD_WID = CW_FIELD(modtran_wid, value = info.MOD_FILE_LAB, uname = 'mod_wid', title = '',XSIZE = 58, /NOEDIT, font = filesfont)
    mod_file_browse = WIDGET_BUTTON(modtran_wid, value = 'Change', uname = 'mod_file_browse', xsize = 54)
    
    ; Output File
    output_wid = WIDGET_BASE(files_wid,/ROW)
    out_lab = WIDGET_LABEL(output_wid, value = 'Output Report File:', uname = 'fake_lab',/ALIGN_LEFT, font = filesfont, scr_xsize = 108)
    info.OUT_WID = CW_FIELD(output_wid, value = info.OUT_FILE_LAB, uname = 'out_wid',title = '',XSIZE = 58,/NOEDIT, font = filesfont)
    out_file_browse = WIDGET_BUTTON(output_wid, value = 'Change', uname = 'out_file_browse', xsize = 54)
    
    ; Define Labels of files characteristics
    char_frame = WIDGET_BASE(files_wid, /column, /frame )
    files_label = WIDGET_LABEL(char_frame, value = 'Spectral Characteristics of Input Files', uname = 'mod4char_lab', xsize = 400, ysize = 0.8,font=stdfont14, /ALIGN_CENTER)
    files_int_wid = WIDGET_BASE(char_frame,/row, space = 15 )
    fake_lab_wid =  WIDGET_LABEL(files_int_wid, value = '', uname = 'fake_lab', scr_xsize = 90, ysize = 0.8,font = stdfont14)
    lab_MIN_WID =  WIDGET_LABEL(files_int_wid, value = 'Minimum WL (nm)', uname = 'min_lab', scr_xsize = 90, ysize = 0.8,font = under12,/ALIGN_CENTER)
    lab_MAX_WID =  WIDGET_LABEL(files_int_wid, value = 'Maximum WL (nm)', uname = 'max_lab',  scr_xsize = 90, ysize = 0.8,font = under12,/ALIGN_CENTER)
    lab_SS_WID =  WIDGET_LABEL(files_int_wid, value = 'SSI (nm)', uname = 'ss_lab',  scr_xsize = 90, ysize = 0.8,font = under12,/ALIGN_CENTER)
    lab_FWHM_WID =  WIDGET_LABEL(files_int_wid, value = 'FWHM (nm)', uname = 'fwhm_lab', scr_xsize = 90, ysize = 0.8,font = under12,/ALIGN_CENTER)
    
    IF check_prev EQ '' THEN BEGIN
        in_file_char_wid = WIDGET_BASE(char_frame, /row, space = 15 )
        IN_TIT_wid =  WIDGET_LABEL(in_file_char_wid, value = 'Measured File:', uname = 'fake_lab', ysize = 0.8,font = stdfont14,/ALIGN_LEFT, scr_xsize = 90)
        info.INMIN_WID =  WIDGET_LABEL(in_file_char_wid, value = '*****', uname = 'inmin_wid', scr_xsize = 90, ysize = 0.8,/ALIGN_CENTER)
        info.INMAX_WID =  WIDGET_LABEL(in_file_char_wid, value = '*****', uname = 'inmax_wid',  scr_xsize = 90, ysize = 0.8,/ALIGN_CENTER)
        info.INSS_WID =  WIDGET_LABEL(in_file_char_wid, value = '*****', uname = 'inss_wid',  scr_xsize = 90, ysize = 0.8,/ALIGN_CENTER)
        info.INFWHM_WID =  WIDGET_LABEL(in_file_char_wid, value = '*****', uname = 'infwhm_wid', scr_xsize = 90, ysize = 0.8,/ALIGN_CENTER)
        mod_file_char_wid = WIDGET_BASE(char_frame, /row )
        
        mod_file_char_wid = WIDGET_BASE(char_frame, /row, space = 15 )
        IN_MOD_wid =  WIDGET_LABEL(mod_file_char_wid, value = 'MODTRAN File:', uname = 'fake_lab',  ysize = 0.8,font = stdfont14,/ALIGN_LEFT, scr_xsize = 90)
        info.MOD4MIN_WID =  WIDGET_LABEL(mod_file_char_wid, value = '*****', uname = 'mod4min_wid', scr_xsize = 90, ysize = 0.8,/ALIGN_CENTER)
        info.MOD4MAX_WID =  WIDGET_LABEL(mod_file_char_wid, value = '*****', uname = 'mod4max_wid',  scr_xsize = 90, ysize = 0.8,/ALIGN_CENTER)
        info.MOD4SS_WID =  WIDGET_LABEL(mod_file_char_wid, value = '*****', uname = 'mod4ss_wid',  scr_xsize = 90, ysize = 0.8,/ALIGN_CENTER)
        info.MOD4FWHM_WID =  WIDGET_LABEL(mod_file_char_wid, value = '*****', uname = 'mod4fwhm_wid', scr_xsize = 90, ysize = 0.8,/ALIGN_CENTER)
    ENDIF ELSE BEGIN
    
        in_file_char_wid = WIDGET_BASE(char_frame, /row, space = 15 )
        IN_TIT_wid =  WIDGET_LABEL(in_file_char_wid, value = 'Measured File:', uname = 'fake_lab', ysize = 0.8,font = stdfont14,/ALIGN_LEFT, scr_xsize = 90)
        info.INMIN_WID =  WIDGET_LABEL(in_file_char_wid, value = STRING(info.INMIN,format = '(f7.2)'), uname = 'inmin_wid', scr_xsize = 90, ysize = 0.8,/ALIGN_CENTER)
        info.INMAX_WID =  WIDGET_LABEL(in_file_char_wid, value = STRING(info.INMAX,format = '(f7.2)'), uname = 'in4max_wid',  scr_xsize = 90, ysize = 0.8,/ALIGN_CENTER)
        info.INSS_WID =  WIDGET_LABEL(in_file_char_wid, value = STRING(info.INSS,format = '(f7.2)'), uname = 'inss_wid',  scr_xsize = 90, ysize = 0.8,/ALIGN_CENTER)
        info.INFWHM_WID =  WIDGET_LABEL(in_file_char_wid, value = STRING(info.INFWHM,format = '(f8.2)'), uname = 'infwhm_wid', scr_xsize = 90, ysize = 0.8,/ALIGN_CENTER)
        
        
        mod_file_char_wid = WIDGET_BASE(char_frame, /row, space = 15 )
        IN_MOD_wid =  WIDGET_LABEL(mod_file_char_wid, value = 'MODTRAN File:', uname = 'fake_lab',  ysize = 0.8,font = stdfont14,/ALIGN_LEFT, scr_xsize = 90)
        info.MOD4MIN_WID =  WIDGET_LABEL(mod_file_char_wid, value = STRING(info.MOD4MIN,format = '(f7.2)'), uname = 'mod4min_wid', scr_xsize = 90, ysize = 0.8,/ALIGN_CENTER)
        info.MOD4MAX_WID =  WIDGET_LABEL(mod_file_char_wid, value = STRING(info.MOD4MAX,format = '(f7.2)'), uname = 'mod4max_wid',  scr_xsize = 90, ysize = 0.8,/ALIGN_CENTER)
        info.MOD4SS_WID =  WIDGET_LABEL(mod_file_char_wid, value = STRING(info.MOD4SS,format = '(f7.2)'), uname = 'mod4ss_wid',  scr_xsize = 90, ysize = 0.8,/ALIGN_CENTER)
        info.MOD4FWHM_WID =  WIDGET_LABEL(mod_file_char_wid, value = STRING(info.MOD4FWHM,format = '(f8.3)'), uname = 'mod4fwhm_wid', scr_xsize = 90, ysize = 0.8,/ALIGN_CENTER)
        
    ENDELSE
    
    fake_wid = WIDGET_LABEL(info.MAIN_WID,value = '', uname = 'fake_label',font=stdfont18, ysize = 4)
    
    par_lab = WIDGET_LABEL(info.MAIN_WID,value = 'Computation Parameters', uname = 'par_label', font=stdfont18,ysize =24)
    
    ; Define Processing Parameters Widgets
    
    par_wid_head =  WIDGET_BASE(info.MAIN_WID,/COLUMN, FRAME= 1,/BASE_ALIGN_CENTER, SCR_XSIZE=541)
    head_wid = WIDGET_BASE(par_wid_head, /ALIGN_LEFT)
    head_lab1= WIDGET_LABEL(head_wid, value = 'Method', uname = 'Spectral WL', font = head_font, xoffset =1, scr_xsize = 120,/ALIGN_CENTER)
    head_lab2= WIDGET_LABEL(head_wid, value = 'Spectral Shift Limit (nm)', uname = 'Spectral WL', font = head_font, SCR_XSIZE=132,xoffset = 134)
    head_lab3= WIDGET_LABEL(head_wid, value = 'FWHM Limit (Multiplier)', uname = 'FWHM WL', font = head_font, SCR_XSIZE=128,xoffset = 275)
    head_lab4= WIDGET_LABEL(head_wid, value = 'Range Widening (N)', uname = 'Scale', font = head_font, SCR_XSIZE=105,xoffset = 420)
    
    par_wid =  WIDGET_BASE(par_wid_head, /ALIGN_LEFT)
    info.METHTYPE_WID =  WIDGET_DROPLIST(par_wid, uname = 'METHTYPE_WID', value = methnames,yoffset = 11, xoffset =1, scr_xsize = 120)
    info.SSL_WID = WIDGET_SLIDER(par_wid, uname = 'ssl_wid', value = info.SSL, MINIMUM= 1,MAXIMUM=10, xoffset = 156, scr_xsize = 90)
    info.FWL_WID = WIDGET_SLIDER(par_wid, uname = 'fwl_wid', value = info.FWL, MINIMUM= 2,MAXIMUM=10, xoffset = 293, scr_xsize = 90)
    info.FLTW_WID = WIDGET_SLIDER(par_wid, uname = 'sgol_wid', value = info.FLTW, MINIMUM= 1,MAXIMUM=100, xoffset = 430, scr_xsize = 90) 
    
    ; Default Button
    DEF1BUT_WID = WIDGET_BASE(par_wid_head, ysize = 30, /ALIGN_CENTER)
    DEF1_WID = WIDGET_BUTTON(DEF1BUT_WID, uname = 'def_par_but', value = 'Default', font = stdfont10, ysize =28, /ALIGN_CENTER, xsize = 54, xoffset = 474);, xoffset = 476, xsize = 54)
    
    fake_wid = WIDGET_LABEL(info.MAIN_WID,value = '', uname = 'fake_label',font=stdfont18, ysize = 6)
    
    ; Define Label Widgets
    
    Lab_wid = WIDGET_LABEL(info.MAIN_WID,value = 'Range Selection', uname = 'range_label',font=stdfont18, ysize = 24)
    range_wid = WIDGET_BASE(info.MAIN_WID, /column,/frame, SCR_XSIZE=541)
    head_wid = WIDGET_BASE(range_wid, /ALIGN_LEFT)
    
    head_lab1= WIDGET_LABEL(head_wid, value = ' Window', uname = 'range_name', font = head_font, ysize = 14, SCR_XSIZE=120,/ALIGN_CENTER)
    head_lab2= WIDGET_LABEL(head_wid, value = 'Lower Wl (nm)', uname = 'Lower WL', font = head_font, ysize = 14, SCR_XSIZE=128,xoffset = 160)
    head_lab3= WIDGET_LABEL(head_wid, value = 'Upper Wl (nm)', uname = 'Upper WL', font = head_font, ysize = 14, SCR_XSIZE=128,xoffset = 300)
    head_lab4= WIDGET_LABEL(head_wid, value = 'Selection', uname = 'Selection', font = head_font, ysize = 14, SCR_XSIZE=105,xoffset = 448)
    
    ; Define Range Selection Widgets
    
    info.RANGE_WID_SCR =  WIDGET_BASE(range_wid, /column,Y_SCROLL_SIZE=250)
    ; Range 1 Ca+
    info.R1_WID = WIDGET_BASE(info.RANGE_WID_SCR,sensitive = info.SENSEL[0],ysize = 40)
    info.R1_WID_RG = WIDGET_BASE(info.R1_WID, sensitive = info.RANGESEL[0],/ALIGN_LEFT)
    name1_wid = WIDGET_LABEL(info.R1_WID_RG, value = Labels[0], uname = 'range1_name', font = stdfont14, ysize = 14,scr_xsize = 115, /ALIGN_CENTER, yoffset = 15)
    slidinfo = info.RANGES [*,0,0]
    IF FIX(slidinfo[0]) EQ FIX(slidinfo[2]) THEN slidinfo [0] =(slidinfo[2])-1
    info.LOW1_WID =WIDGET_SLIDER(info.R1_WID_RG, uname = 'low1_wid', value = slidinfo[1], MINIMUM= slidinfo [0], MAXIMUM = slidinfo[2],scr_xsize = 90, scroll = 1, xoffset = 153)
    slidinfo = info.RANGES [*,1,0]
    IF FIX(slidinfo[0]) EQ FIX(slidinfo[2]) THEN slidinfo [2] =(slidinfo[0])+1
    info.UP1_WID = WIDGET_SLIDER(info.R1_WID_RG, uname = 'up1_wid', value = slidinfo[1], MINIMUM= slidinfo [0], MAXIMUM = slidinfo[2],scr_xsize = 90,scroll = 1, xoffset = 290)
    info.SEL1_WID = CW_BGROUP(info.R1_WID,['No','Yes'], uvalue = [1,0],uname = 'sel1_wid',  SET_VALUE= info.RANGESEL[0], /EXCLUSIVE,/NO_RELEASE, column = 2, XSIZE=85, xoffset = 424,ysize = 25, yoffset = 5)
    
    ; Range 2Fe,Ca,Fe
    info.R2_WID = WIDGET_BASE(info.RANGE_WID_SCR, sensitive = info.SENSEL[1],ysize = 40)
    info.R2_WID_RG = WIDGET_BASE(info.R2_WID, sensitive = info.RANGESEL[1],/ALIGN_LEFT)
    name2_wid = WIDGET_LABEL(info.R2_WID_RG, value = Labels[1], uname = 'range2_name', font = stdfont14, ysize = 14,scr_xsize = 115,/ALIGN_center, yoffset = 15)
    slidinfo = info.RANGES [*,0,1]
    IF FIX(slidinfo[0]) EQ FIX(slidinfo[2]) THEN slidinfo [0] =(slidinfo[2])-1
    info.LOW2_WID = WIDGET_SLIDER(info.R2_WID_RG, uname = 'low2_wid', value =slidinfo[1], MINIMUM= slidinfo [0], MAXIMUM = slidinfo[2],scr_xsize = 90, scroll = 1, xoffset = 153)
    slidinfo = info.RANGES [*,1,1]
    IF FIX(slidinfo[0]) EQ FIX(slidinfo[2]) THEN slidinfo [2] =(slidinfo[0])+1
    info.UP2_WID = WIDGET_SLIDER(info.R2_WID_RG, uname = 'up2_wid', value = slidinfo[1], MINIMUM= slidinfo [0], MAXIMUM = slidinfo[2],scr_xsize = 90,scroll = 1, xoffset = 290)
    info.SEL2_WID = CW_BGROUP(info.R2_WID,['No','Yes'], uvalue = [1,0],uname = 'sel2_wid',  SET_VALUE= info.RANGESEL[1], /EXCLUSIVE,/NO_RELEASE, column = 2, XSIZE=85, xoffset = 424,ysize = 25, yoffset = 5)
    
    ; Range 3Hb,Fe
    info.R3_WID = WIDGET_BASE(info.RANGE_WID_SCR, sensitive = info.SENSEL[2],ysize = 40)
    info.R3_WID_RG = WIDGET_BASE(info.R3_WID, sensitive = info.RANGESEL[2],/ALIGN_LEFT)
    name3_wid = WIDGET_LABEL(info.R3_WID_RG, value = Labels[2], uname = 'range3_name', font = stdfont14, ysize = 14,scr_xsize = 115,/ALIGN_center, yoffset = 15)
    slidinfo = info.RANGES [*,0,2]
    IF FIX(slidinfo[0]) EQ FIX(slidinfo[2]) THEN slidinfo [0] =(slidinfo[2])-1
    info.LOW3_WID = WIDGET_SLIDER(info.R3_WID_RG, uname = 'low3_wid', value = slidinfo[1], MINIMUM= slidinfo [0], MAXIMUM = slidinfo[2],scr_xsize = 90,scroll = 1, xoffset = 153)
    slidinfo = info.RANGES [*,1,2]
    IF FIX(slidinfo[0]) EQ FIX(slidinfo[2]) THEN slidinfo [2] =(slidinfo[0])+1
    info.UP3_WID = WIDGET_SLIDER(info.R3_WID_RG, uname = 'up3_wid', value = slidinfo[1], MINIMUM= slidinfo [0], MAXIMUM = slidinfo[2],scr_xsize = 90,scroll = 1, xoffset = 290)
    info.SEL3_WID = CW_BGROUP(info.R3_WID,['No','Yes'], uvalue = [1,0],uname = 'sel3_wid',  SET_VALUE= info.RANGESEL[2], /EXCLUSIVE,/NO_RELEASE, column = 2, XSIZE=85, xoffset = 424,ysize = 25, yoffset = 5)
    
    ; Range 4_Mg,Fe
    info.R4_WID = WIDGET_BASE(info.RANGE_WID_SCR,sensitive = info.SENSEL[3],ysize = 40)
    info.R4_WID_RG = WIDGET_BASE(info.R4_WID,sensitive = info.RANGESEL[3],/ALIGN_LEFT)
    name4_wid = WIDGET_LABEL(info.R4_WID_RG, value = Labels[3], uname = 'range4_name', font = stdfont14, ysize = 14,scr_xsize = 115,/ALIGN_center, yoffset = 15)
    slidinfo = info.RANGES [*,0,3]
    IF FIX(slidinfo[0]) EQ FIX(slidinfo[2]) THEN slidinfo [0] =(slidinfo[2])-1
    info.LOW4_WID = WIDGET_SLIDER(info.R4_WID_RG, uname = 'low4_wid', value = slidinfo[1],MINIMUM= slidinfo [0], MAXIMUM = slidinfo[2],scr_xsize = 90,scroll = 1, xoffset = 153)
    slidinfo = info.RANGES [*,1,3]
    IF FIX(slidinfo[0]) EQ FIX(slidinfo[2]) THEN slidinfo [2] =(slidinfo[0])+1
    info.UP4_WID = WIDGET_SLIDER(info.R4_WID_RG, uname = 'up4_wid', value = slidinfo[1], MINIMUM= slidinfo [0], MAXIMUM = slidinfo[2],scr_xsize = 90,scroll = 1, xoffset = 290)
    info.SEL4_WID = CW_BGROUP(info.R4_WID,['No','Yes'], uvalue = [1,0],uname = 'sel4_wid',  SET_VALUE= info.RANGESEL[3], /EXCLUSIVE,/NO_RELEASE, column = 2, XSIZE=85, xoffset = 424,ysize = 25, yoffset = 5)
    
    ; Range 5_Na
    info.R5_WID = WIDGET_BASE(info.RANGE_WID_SCR, sensitive = info.SENSEL[4],ysize = 40)
    info.R5_WID_RG = WIDGET_BASE(info.R5_WID,sensitive = info.RANGESEL[4],/ALIGN_LEFT)
    name5_wid = WIDGET_LABEL(info.R5_WID_RG, value = Labels[4], uname = 'range5_name', font = stdfont14, ysize = 14,scr_xsize = 115,/ALIGN_center, yoffset = 15)
    slidinfo = info.RANGES [*,0,4]
    IF FIX(slidinfo[0]) EQ FIX(slidinfo[2]) THEN slidinfo [0] =(slidinfo[2])-1
    info.LOW5_WID = WIDGET_SLIDER(info.R5_WID_RG, uname = 'low5_wid', value = slidinfo[1], MINIMUM= slidinfo [0], MAXIMUM = slidinfo[2],scr_xsize = 90,scroll = 1, xoffset = 153)
    slidinfo = info.RANGES [*,1,4]
    IF FIX(slidinfo[0]) EQ FIX(slidinfo[2]) THEN slidinfo [2] =(slidinfo[0])+1
    info.UP5_WID = WIDGET_SLIDER(info.R5_WID_RG, uname = 'up5_wid', value = slidinfo[1], MINIMUM= slidinfo [0], MAXIMUM = slidinfo[2],scr_xsize = 90,scroll = 1, xoffset = 290)
    info.SEL5_WID = CW_BGROUP(info.R5_WID,['No','Yes'], uvalue = [1,0],uname = 'sel5_wid',  SET_VALUE= info.RANGESEL[4], /EXCLUSIVE,/NO_RELEASE, column = 2, XSIZE=85, xoffset = 424,ysize = 25, yoffset = 5)
    
    ; Range 6_Ha
    info.R6_WID = WIDGET_BASE(info.RANGE_WID_SCR, sensitive = info.SENSEL[5],ysize = 40)
    info.R6_WID_RG = WIDGET_BASE(info.R6_WID, sensitive = info.RANGESEL[5],/ALIGN_LEFT)
    name6_wid = WIDGET_LABEL(info.R6_WID_RG, value = Labels[5], uname = 'range6_name', font = stdfont14, ysize = 14,scr_xsize = 115,/ALIGN_center, yoffset = 15)
    slidinfo = info.RANGES [*,0,5]
    IF FIX(slidinfo[0]) EQ FIX(slidinfo[2]) THEN slidinfo [0] =(slidinfo[2])-1
    info.LOW6_WID = WIDGET_SLIDER(info.R6_WID_RG, uname = 'low6_wid', value = slidinfo[1],MINIMUM= slidinfo [0], MAXIMUM = slidinfo[2],scr_xsize = 90,scroll = 1, xoffset = 153)
    slidinfo = info.RANGES [*,1,5]
    IF FIX(slidinfo[0]) EQ FIX(slidinfo[2]) THEN slidinfo [2] =(slidinfo[0])+1
    info.UP6_WID = WIDGET_SLIDER(info.R6_WID_RG, uname = 'up6_wid', value = slidinfo[1],MINIMUM= slidinfo [0], MAXIMUM = slidinfo[2],scr_xsize = 90,scroll = 1, xoffset = 290)
    info.SEL6_WID = CW_BGROUP(info.R6_WID,['No','Yes'], uvalue = [1,0],uname = 'sel6_wid',  SET_VALUE= info.RANGESEL[5], /EXCLUSIVE,/NO_RELEASE, column = 2, XSIZE=85, xoffset = 424,ysize = 25, yoffset = 5)
    
    ; Range 7O2
    info.R7_WID = WIDGET_BASE(info.RANGE_WID_SCR, sensitive = info.SENSEL[6],ysize = 40)
    info.R7_WID_RG = WIDGET_BASE(info.R7_WID,sensitive = info.RANGESEL[6],/ALIGN_LEFT)
    name7_wid = WIDGET_LABEL(info.R7_WID_RG, value = Labels[6], uname = 'range7_name', font = stdfont14, ysize = 14,scr_xsize = 115,/ALIGN_center, yoffset = 15)
    slidinfo = info.RANGES [*,0,6]
    IF FIX(slidinfo[0]) EQ FIX(slidinfo[2]) THEN slidinfo [0] =(slidinfo[2])-1
    info.LOW7_WID = WIDGET_SLIDER(info.R7_WID_RG, uname = 'low7_wid', value = slidinfo[1], MINIMUM= slidinfo [0], MAXIMUM = slidinfo[2],scr_xsize = 90,scroll = 1, xoffset = 153)
    slidinfo = info.RANGES [*,1,6]
    IF FIX(slidinfo[0]) EQ FIX(slidinfo[2]) THEN slidinfo [2] =(slidinfo[0])+1
    info.UP7_WID = WIDGET_SLIDER(info.R7_WID_RG, uname = 'up7_wid', value = slidinfo[1], MINIMUM= slidinfo [0], MAXIMUM = slidinfo[2],scr_xsize = 90,scroll = 1, xoffset = 290)
    info.SEL7_WID = CW_BGROUP(info.R7_WID,['No','Yes'], uvalue = [1,0],uname = 'sel7_wid',  SET_VALUE= info.RANGESEL[6], /EXCLUSIVE,/NO_RELEASE, column = 2, XSIZE=85, xoffset = 424,ysize = 25, yoffset = 5)
    
    ; Range 8_Water Vapor
    info.R8_WID = WIDGET_BASE(info.RANGE_WID_SCR,sensitive = info.SENSEL[7],ysize = 40)
    info.R8_WID_RG = WIDGET_BASE(info.R8_WID,sensitive = info.RANGESEL[7],/ALIGN_LEFT)
    name8_wid = WIDGET_LABEL(info.R8_WID_RG, value = labels[7], uname = 'range8_name', font = stdfont14, ysize = 14,scr_xsize = 115,/ALIGN_center, yoffset = 15)
    slidinfo = info.RANGES [*,0,7]
    IF FIX(slidinfo[0]) EQ FIX(slidinfo[2]) THEN slidinfo [0] =(slidinfo[2])-1
    info.LOW8_WID = WIDGET_SLIDER(info.R8_WID_RG, uname = 'low8_wid', value = slidinfo[1],MINIMUM= slidinfo [0], MAXIMUM = slidinfo[2],scr_xsize = 90,scroll = 1, xoffset = 153)
    slidinfo = info.RANGES [*,1,7]
    IF FIX(slidinfo[0]) EQ FIX(slidinfo[2]) THEN slidinfo [2] =(slidinfo[0])+1
    info.UP8_WID = WIDGET_SLIDER(info.R8_WID_RG, uname = 'up8_wid', value = slidinfo[1], MINIMUM= slidinfo [0], MAXIMUM = slidinfo[2],scr_xsize = 90,scroll = 1, xoffset = 290)
    info.SEL8_WID = CW_BGROUP(info.R8_WID,['No','Yes'], uvalue = [1,0],uname = 'sel8_wid',  SET_VALUE= info.RANGESEL[7], /EXCLUSIVE,/NO_RELEASE, column = 2, XSIZE=85, xoffset = 424,ysize = 25, yoffset = 5)
    
    ; Range 9_( O2)
    info.R9_WID = WIDGET_BASE(info.RANGE_WID_SCR, sensitive = info.SENSEL[8],ysize = 40)
    info.R9_WID_RG = WIDGET_BASE(info.R9_WID,sensitive = info.RANGESEL[8],/ALIGN_LEFT)
    name9_wid = WIDGET_LABEL(info.R9_WID_RG, value = Labels[8], uname = 'range9_name', font = stdfont14, ysize = 14,scr_xsize = 115,/ALIGN_center, yoffset = 15)
    slidinfo = info.RANGES [*,0,8]
    IF FIX(slidinfo[0]) EQ FIX(slidinfo[2]) THEN slidinfo [0] =(slidinfo[2])-1
    info.LOW9_WID = WIDGET_SLIDER(info.R9_WID_RG, uname = 'low9_wid', value = slidinfo[1], MINIMUM= slidinfo [0], MAXIMUM = slidinfo[2],scr_xsize = 90,scroll = 1, xoffset = 153)
    slidinfo = info.RANGES [*,1,8]
    IF FIX(slidinfo[0]) EQ FIX(slidinfo[2]) THEN slidinfo [2] =(slidinfo[0])+1
    info.UP9_WID = WIDGET_SLIDER(info.R9_WID_RG, uname = 'up9_wid', value = slidinfo[1], MINIMUM= slidinfo [0], MAXIMUM = slidinfo[2],scr_xsize = 90,scroll = 1, xoffset = 290)
    info.SEL9_WID = CW_BGROUP(info.R9_WID,['No','Yes'], uvalue = [1,0],uname = 'sel9_wid',  SET_VALUE= info.RANGESEL[8], /EXCLUSIVE,/NO_RELEASE, column = 2, XSIZE=85, xoffset = 424,ysize = 25, yoffset = 5)
    
    ; Range 10_(O2, Water Vapor)
    info.R10_WID = WIDGET_BASE(info.RANGE_WID_SCR, sensitive = info.SENSEL[9],ysize = 40)
    info.R10_WID_RG = WIDGET_BASE(info.R10_WID,sensitive = info.RANGESEL[9],/ALIGN_LEFT)
    name10_wid = WIDGET_LABEL(info.R10_WID_RG, value = Labels[9], uname = 'range10_name', font = stdfont14, ysize = 14,scr_xsize = 115,/ALIGN_center, yoffset = 15)
    slidinfo = info.RANGES [*,0,9]
    IF FIX(slidinfo[0]) EQ FIX(slidinfo[2]) THEN slidinfo [0] =(slidinfo[2])-1
    info.LOW10_WID = WIDGET_SLIDER(info.R10_WID_RG, uname = 'low10_wid', value =slidinfo[1],MINIMUM= slidinfo [0], MAXIMUM = slidinfo[2],scr_xsize = 90,scroll = 1, xoffset = 153)
    slidinfo = info.RANGES [*,1,9]
    IF FIX(slidinfo[0]) EQ FIX(slidinfo[2]) THEN slidinfo [2] =(slidinfo[0])+1
    info.UP10_WID = WIDGET_SLIDER(info.R10_WID_RG, uname = 'up10_wid', value = slidinfo[1], MINIMUM= slidinfo [0], MAXIMUM = slidinfo[2],scr_xsize = 90,scroll = 1, xoffset = 290)
    info.SEL10_WID = CW_BGROUP(info.R10_WID,['No','Yes'], uvalue = [1,0],uname = 'sel10_wid',  SET_VALUE= info.RANGESEL[9], /EXCLUSIVE,/NO_RELEASE, column = 2, XSIZE=85, xoffset = 424,ysize = 25, yoffset = 5)
    
    ; Range 11 (CaII)
    info.R11_WID = WIDGET_BASE(info.RANGE_WID_SCR, sensitive = info.SENSEL[10],ysize = 40)
    info.R11_WID_RG = WIDGET_BASE(info.R11_WID,sensitive = info.RANGESEL[10],/ALIGN_LEFT)
    name11_wid = WIDGET_LABEL(info.R11_WID_RG, value = labels[10], uname = 'range11_name', font = stdfont14, ysize = 14,scr_xsize = 115,/ALIGN_center, yoffset = 15)
    slidinfo = info.RANGES [*,0,10]
    IF FIX(slidinfo[0]) EQ FIX(slidinfo[2]) THEN slidinfo [0] =(slidinfo[2])-1
    info.LOW11_WID = WIDGET_SLIDER(info.R11_WID_RG, uname = 'low11_wid', value = slidinfo[1],MINIMUM= slidinfo [0], MAXIMUM = slidinfo[2],scr_xsize = 90,scroll = 1, xoffset = 153)
    slidinfo = info.RANGES [*,1,10]
    IF FIX(slidinfo[0]) EQ FIX(slidinfo[2]) THEN slidinfo [2] =(slidinfo[0])+1
    info.UP11_WID = WIDGET_SLIDER(info.R11_WID_RG, uname = 'up11_wid', value = slidinfo[1], MINIMUM= slidinfo [0], MAXIMUM = slidinfo[2],scr_xsize = 90,scroll = 1, xoffset = 290)
    info.SEL11_WID = CW_BGROUP(info.R11_WID,['No','Yes'], uvalue = [1,0],uname = 'sel11_wid',  SET_VALUE= info.RANGESEL[10], /EXCLUSIVE,/NO_RELEASE, column = 2, XSIZE=85, xoffset = 424,ysize = 25, yoffset = 5)
    
    ; Range 12 (Water)
    info.R12_WID = WIDGET_BASE(info.RANGE_WID_SCR, sensitive = info.SENSEL[11],ysize = 40)
    info.R12_WID_RG = WIDGET_BASE(info.R12_WID,sensitive = info.RANGESEL[11],/ALIGN_LEFT)
    name12_wid = WIDGET_LABEL(info.R12_WID_RG, value = labels[11], uname = 'range12_name', font = stdfont14, ysize = 14,scr_xsize = 115,/ALIGN_center, yoffset = 15)
    slidinfo = info.RANGES [*,0,11]
    IF FIX(slidinfo[0]) EQ FIX(slidinfo[2]) THEN slidinfo [0] =(slidinfo[2])-1
    info.LOW12_WID = WIDGET_SLIDER(info.R12_WID_RG, uname = 'low12_wid', value =slidinfo[1], MINIMUM= slidinfo [0], MAXIMUM = slidinfo[2],scr_xsize = 90,scroll = 1, xoffset = 153)
    slidinfo = info.RANGES [*,1,11]
    IF FIX(slidinfo[0]) EQ FIX(slidinfo[2]) THEN slidinfo [2] =(slidinfo[0])+1
    info.UP12_WID = WIDGET_SLIDER(info.R12_WID_RG, uname = 'up12_wid', value = slidinfo[1], MINIMUM= slidinfo [0], MAXIMUM = slidinfo[2],scr_xsize = 90,scroll = 1, xoffset = 290)
    info.SEL12_WID = CW_BGROUP(info.R12_WID,['No','Yes'], uvalue = [1,0],uname = 'sel12_wid',  SET_VALUE= info.RANGESEL[11], /EXCLUSIVE,/NO_RELEASE, column = 2, XSIZE=85, xoffset = 424,ysize = 25, yoffset = 5)
    
    ; Range 13 (T-Water)
    info.R13_WID = WIDGET_BASE(info.RANGE_WID_SCR, sensitive = info.SENSEL[12],ysize = 40)
    info.R13_WID_RG = WIDGET_BASE(info.R13_WID,sensitive = info.RANGESEL[12],/ALIGN_LEFT)
    name13_wid = WIDGET_LABEL(info.R13_WID_RG, value = labels[12], uname = 'range13_name', font = stdfont14, ysize = 14,scr_xsize = 115,/ALIGN_center, yoffset = 15)
    slidinfo = info.RANGES [*,0,12]
    IF FIX(slidinfo[0]) EQ FIX(slidinfo[2]) THEN slidinfo [0] =(slidinfo[2])-1
    info.LOW13_WID = WIDGET_SLIDER(info.R13_WID_RG, uname = 'low13_wid', value = slidinfo[1], MINIMUM= slidinfo [0], MAXIMUM = slidinfo[2],scr_xsize = 90,scroll = 1, xoffset = 153)
    slidinfo = info.RANGES [*,1,12]
    IF FIX(slidinfo[0]) EQ FIX(slidinfo[2]) THEN slidinfo [2] =(slidinfo[0])+1
    info.UP13_WID = WIDGET_SLIDER(info.R13_WID_RG, uname = 'up13_wid', value = slidinfo[1], MINIMUM= slidinfo [0], MAXIMUM = slidinfo[2],scr_xsize = 90,scroll = 1, xoffset = 290)
    info.SEL13_WID = CW_BGROUP(info.R13_WID,['No','Yes'], uvalue = [1,0],uname = 'sel13_wid',  SET_VALUE= info.RANGESEL[12], /EXCLUSIVE,/NO_RELEASE, column = 2, XSIZE=85, xoffset = 424,ysize = 25, yoffset = 5)
    
    
    ; Range Broad1
    info.RBROAD1_WID = WIDGET_BASE(info.RANGE_WID_SCR, sensitive = info.SENSEL_BROAD1,ysize = 40)
    info.RBROAD1_WID_RG = WIDGET_BASE(info.RBROAD1_WID,sensitive = info.RANGESEL_BROAD1,/ALIGN_LEFT)
    nameBroad1_wid = WIDGET_LABEL(info.RBROAD1_WID_RG, value = Labels[13], uname = 'rangeBroad1_name', font = stdfont14, ysize = 14,scr_xsize = 115,/ALIGN_center, yoffset = 15)
    slidinfo = info.RANGES_BROAD1 [*,0]
    IF FIX(slidinfo[0]) EQ FIX(slidinfo[2]) THEN slidinfo [0] =(slidinfo[2])-1
    info.LOWBROAD1_WID = WIDGET_SLIDER(info.RBROAD1_WID_RG, uname = 'lowBroad1_wid', value = slidinfo[1],MINIMUM= slidinfo [0], MAXIMUM = slidinfo[2],scr_xsize = 90,scroll = 1, xoffset = 153)
    slidinfo = info.RANGES_BROAD1 [*,1]
    IF FIX(slidinfo[0]) EQ FIX(slidinfo[2]) THEN slidinfo [2] =(slidinfo[0])+1
    
    info.UPBROAD1_WID = WIDGET_SLIDER(info.RBROAD1_WID_RG, uname = 'upBroad1_wid', value = slidinfo[1], MINIMUM= slidinfo [0], MAXIMUM = slidinfo[2],scr_xsize = 90,scroll = 1, xoffset = 290)
    info.SELBROAD1_WID = CW_BGROUP(info.RBROAD1_WID,['No','Yes'], uvalue = [1,0],uname = 'selBroad1_wid',  SET_VALUE= info.RANGESEL_BROAD1, /EXCLUSIVE,/NO_RELEASE, column = 2, XSIZE=85, xoffset = 424,ysize = 25, yoffset = 5)
    
    ;    ; Range Broad2
    ;    info.RBROAD2_WID = WIDGET_BASE(info.RANGE_WID_SCR, /row, /align_center,sensitive = info.SENSEL_BROAD2,ysize = 40)
    ;    info.RBROAD2_WID_RG = WIDGET_BASE(info.RBROAD2_WID, column = 5, /align_center,sensitive = info.RANGESEL_BROAD2)
    ;    nameBroad2_wid = WIDGET_LABEL(info.RBROAD2_WID_RG, value = Labels[14], uname = 'rangeBroad2_name', font = stdfont14, ysize = 14,scr_xsize = 115,/ALIGN_center)
    ;    slidinfo = info.RANGES_BROAD2 [*,0]
    ;    IF FIX(slidinfo[0]) EQ FIX(slidinfo[2]) THEN slidinfo [0] =(slidinfo[2])-1
    ;    info.LOWBROAD2_WID = WIDGET_SLIDER(info.RBROAD2_WID_RG, uname = 'lowBroad2_wid', value =slidinfo[1], MINIMUM= slidinfo [0], MAXIMUM = slidinfo[2],scr_xsize = 90,scroll = 1, xoffset = 290)
    ;    fake_Broad2 = WIDGET_LABEL(info.RBROAD2_WID_RG,value = ' ', uname = 'fake_label',font=stdfont14,scr_xsize = 6)
    ;    slidinfo = info.RANGES_BROAD1 [*,1]
    ;    IF FIX(slidinfo[0]) EQ FIX(slidinfo[2]) THEN slidinfo [2] =(slidinfo[0])+1
    ;    info.UPBROAD2_WID = WIDGET_SLIDER(info.RBROAD2_WID_RG, uname = 'upBroad2_wid', value =slidinfo[1], MINIMUM= slidinfo [0], MAXIMUM = slidinfo[2],scr_xsize = 90,scroll = 1, xoffset = 290)
    ;    info.SELBROAD2_WID = CW_BGROUP(info.RBROAD2_WID,['No','Yes'], uvalue = [1,0],uname = 'selBroad2_wid',  SET_VALUE= info.RANGESEL_BROAD2, /EXCLUSIVE,/NO_RELEASE, column = 2, XSIZE=85, xoffset = 424,ysize = 25)
    
    ; Range User Defined
    info.RUSER_WID = WIDGET_BASE(info.RANGE_WID_SCR, sensitive =  info.SENSEL_USER,ysize = 40)
    info.RUSER_WID_RG = WIDGET_BASE(info.RUSER_WID,  /ALIGN_LEFT,sensitive = info.RANGESEL_USER)
    nameUser_wid = WIDGET_LABEL(info.RUSER_WID_RG, value = labels[14], uname = 'rangeUser_name', font = stdfont14, ysize = 14,xsize = 115,/ALIGN_center, yoffset = 12)
    info.LOWUSER_WID =CW_FIELD(info.RUSER_WID_RG, value = STRING(info.RANGES_USER[0],format = '(f7.2)'), uname = 'lowUserwid', Title = '                                                 ', xsize =13, ysize = 0.8, tEXT_FRAME=0)
    info.UPUSER_WID=CW_FIELD(info.RUSER_WID_RG, value = STRING(info.RANGES_USER[1],format = '(f7.2)'), uname = 'upUser_wid', Title = '                                                                                               ', xsize = 13, ysize = 0.8, tEXT_FRAME=0)
    info.SELUSER_WID = CW_BGROUP(info.RUSER_WID,['No','Yes'],uvalue = [1,0],uname = 'selUser_wid',  SET_VALUE= info.RANGESEL_USER, /EXCLUSIVE,/NO_RELEASE, column = 2, XSIZE=85, xoffset = 424,ysize = 25, yoffset = 2)
    
    ;  WIDGET_SLIDER(info.R14_WID_RG, uname = 'low14_wid', value = info.RANGES[0,13], MINIMUM= init_ranges[0,13]-10,MAXIMUM=init_ranges[0,13]+10,scr_xsize = 105,/ALIGN_center)
    ;  fake_wid14 = WIDGET_LABEL(info.R14_WID_RG,value = ' ', uname = 'fake_label',font=stdfont14,scr_xsize = 6)
    ;  info.UP14_WID = WIDGET_SLIDER(info.R14_WID_RG, uname = 'up14_wid', value = info.RANGES[1,13], MINIMUM = init_ranges[1,13]-10,MAXIMUM=init_ranges[1,13]+10,scr_xsize = 105,/ALIGN_center)
    ;  info.SEL14_WID = CW_BGROUP(info.R14_WID,['No','Yes'], uvalue = [1,0],uname = 'sel14_wid',  SET_VALUE= info.RANGESEL[13],ysize = 30, /EXCLUSIVE, column = 2,xsize = 105,xpad = 15)
    
    ; Default Button
    info.DEFBUT_WID = WIDGET_BASE(range_wid,sensitive = MAX(info.SENSEL[*]), ysize = 30, /ALIGN_CENTER)
    DEF_WID = WIDGET_BUTTON(info.DEFBUT_WID, uname = 'def_range_but', value ='Default', font = stdfont10, ysize =28, /ALIGN_CENTER, xsize = 54, xoffset = 474);, xoffset = 476, xsize = 54)
    
    fake_wid = WIDGET_LABEL(info.MAIN_WID,value = '', uname = 'fake_label',font=stdfont18, ysize = 6)
    ;
    Lab_wid_plt = WIDGET_LABEL(info.MAIN_WID,value = 'Plotting Options', uname = 'plt_label',/DYNAMIC_RESIZE,font=stdfont18)
    out_wid = WIDGET_BASE(info.MAIN_WID,column=2,/BASE_ALIGN_CENTER,scr_xsize = 550)
    ; Define PLOT widgets
    
    
    plt_wid = WIDGET_BASE(out_wid,/FRAME,scr_xsize = 543)
    plt_base = WIDGET_BASE(plt_wid, ysize = 30, /BASE_ALIGN_CENTER )
    
    ;    info.SAVE_BASE = WIDGET_BASE(plt_base,/ALIGN_CENTER, sensitive = info.PLOT )
    info.PLOT_WID = CW_BGROUP(plt_base,'Plot to Screen', uvalue = [1,0],uname = 'plot_wid',  SET_VALUE= info.DOPLOT,ysize = 30, /NONEXCLUSIVE, xpad = 0, font = 'Arial*16*Bold', xoffset = 40)
    info.SAVE_WID = CW_BGROUP(plt_base,'Save Plots as TIFF', uvalue = [1,0],uname = 'save_wid',  SET_VALUE= info.DOSAVEIMAGE,ysize = 30, /NONEXCLUSIVE, xpad = 0, font = 'Arial*16*Bold', xoffset = 200)
    info.SAVE_WID2 = CW_BGROUP(plt_base,'Save Data as CSV', uvalue = [1,0],uname = 'save_wid2',  SET_VALUE= info.SAVE_CSV,ysize = 30, /NONEXCLUSIVE, xpad = 0, font = 'Arial*16*Bold', xoffset =370)
    ;    plt_base_2 = WIDGET_BASE(plt_wid, /ROW,/ALIGN_CENTER )
    ;    info.PLT_OPT_BASE = WIDGET_BASE(plt_base_2, sensitive = info.PLOT )
    ;    info.PLOT_OPT_WID = CW_BGROUP(info.PLT_OPT_BASE,['Irradiance','r/ratios'], uvalue = info.PLOT_OPT,uname = 'plot_opt_wid',  SET_VALUE= info.PLOT_OPT,column = 3,ysize = 30, $
    ;        /NONEXCLUSIVE, xsize =250, xpad = 5, space = 2)
    
    ; Define SAVE widgets
    ;    save_wid = WIDGET_BASE(out_wid,/column,/BASE_ALIGN_CENTER, /FRAME, ysize = 100, xsize = 245)
    ;    Lab_wid_save = WIDGET_LABEL(save_wid,value = 'Save Options', uname = 'save_label',font=stdfont18)
    ;    save_base = WIDGET_BASE(save_wid, /ROW,/ALIGN_CENTER )
    ;    info.SAVE_CSV_WID = CW_BGROUP(save_base,'Save Outputs to CSV File', uvalue = [1,0],uname = 'save_csv_wid',  SET_VALUE= info.SAVE_CSV,ysize = 30, /NONEXCLUSIVE, xpad = 0, font = 'Arial*Italic*16*Bold')
    ;    ;CW_BGROUP(save_wid,'Save Outputs to CSV', uvalue = [1,0],uname = 'save_wid',  SET_VALUE= info.save_csv,ysize = 30, /NONEXCLUSIVE,xpad = 0, font = 'Arial*Italic*16*Bold')
    ; Define Bottom Buttons Widgets (Start, Quit and Help)
    
    button_wid = WIDGET_BASE(info.MAIN_WID,scr_xsize = 543, /Align_Center )
    About_wid = WIDGET_TEXT(button_wid, value = 'About', uname = 'About',scr_xsize = 54,font='Arial*16*Bold*Underline*Italic',xoffset = 0, FRAME= 0, /ALL_EVENTS )
    Start_wid = WIDGET_BUTTON(button_wid, value = 'Start', uname = 'Start',scr_xsize = 54,ysize = 28,font='Arial*16*Bold',xoffset = 380)
    Quit_wid = WIDGET_BUTTON(button_wid, value = 'Quit', uname = 'Quit',scr_xsize = 54,ysize = 28,font='Arial*16*Bold',xoffset = 435)
    Help_wid = WIDGET_BUTTON(button_wid, value = 'Help', uname = 'Help',scr_xsize = 54,ysize = 28,font='Arial*16*Bold',xoffset = 490)
    
    WIDGET_CONTROL, info.TARREF_WID, set_droplist_select = info.TARREF_IND
    WIDGET_CONTROL, info.MEASTYPE_WID, set_droplist_select = info.MEASTYPE_IND
    WIDGET_CONTROL, info.UMEAS_WID, set_droplist_select = info.UMEAS_IND
    WIDGET_CONTROL, info.METHTYPE_WID, set_droplist_select = info.METHTYPE
    
    IF info.MEASTYPE_IND EQ 1 THEN  WIDGET_CONTROL, info.TARREF_WID, SENSITIVE=0
    
    ; Get GUI Geometry
    geometry = WIDGET_INFO(info.MAIN_WID, /geometry)
    info.XSIZE = geometry.XSIZE  & info.YSIZE = geometry.YSIZE
    ; Get Screen Size
    screen = GET_SCREEN_SIZE(resolution = scr_res)
    IF screen [0] GT geometry.XSIZE THEN WIDGET_CONTROL, info.MAIN_WID, xsize = geometry.XSIZE $;+2*geometry.XPAD+geometry.SPACE $
    ELSE WIDGET_CONTROL, info.MAIN_WID, xsize = screen[0] - 100
    IF screen [1] GT geometry.SCR_YSIZE THEN WIDGET_CONTROL, info.MAIN_WID, ysize = geometry.YSIZE $
    ELSE WIDGET_CONTROL, info.MAIN_WID, ysize = screen[1] - 100
    
    ;  IF CHECK_PREV ne '' THEN BEGIN
    ;    INFO_UPDATE = Speccal_Reinit_Ranges(info)
    
    ;- Realize the widgets
    
    WIDGET_CONTROL, info.MAIN_WID, /REALIZE
    
    ;- Store temporary variables in a pointer to be passed to the event handling
    
    info_ptr = PTR_NEW(info)
    *info_ptr = info
    WIDGET_CONTROL, info.MAIN_WID, set_uvalue=info_ptr
    XMANAGER, 'SpecCal_GUI', info.MAIN_WID
    
    ; Retrieve modified info structure from the pointers
    info = *info_ptr
    
    ; Check for problems in user selection or "Quit" selection
    
    IF info.QUIT EQ 1 THEN BEGIN
        ;file_mess = DIALOG_MESSAGE('Processing Aborted', /INFORMATION)
        RETURN, info
    ENDIF
    
    
    
    ; All is OK ! Save the previous file name and pass the useful infos back to main
    ;SAVE, info, FILENAME = prev_dir+ 'previous_SpecCal.sav'
    rangesel = [info.RANGESEL, info.RANGESEL_BROAD1, info.RANGESEL_USER]
    ranges = DBLARR(2, N_ELEMENTS(info.RANGES[0,0,*]))
    FOR r = 0, N_ELEMENTS(info.RANGES[0,0,*])-1 DO BEGIN
        ranges [0,r] = info.RANGES[1,0,r]
        ranges [1,r] = info.RANGES[1,1,r]
    ENDFOR
    ranges_broad1 = [info.RANGES_BROAD1[1,0],info.RANGES_BROAD1[1,1]]
    
    ;    ranges_broad2 = [info.RANGES_BROAD2[1,0],info.RANGES_BROAD2[1,1]]
    
    
    ranges_user = info.RANGES_USER
    
    ranges = [[ranges],[ranges_Broad1],[ranges_user]]
    
    
    result = {in_file:info.IN_FILE, ssl:info.SSL, fwl:info.FWL, METHTYPE: info.METHTYPE ,fltW:info.FLTW, ranges:RANGES, $
        tarref_fname:info.TARREF_FNAME, nomfwhm:info.INFWHM, $
        umeas:INFO.UMEAS_IND,  meastype:info.MEASTYPE_IND,$
        rangesel:RANGESEL, tarref: info.TARREF, $
        out_file:info.OUT_FILE, mod_file:info.MOD_FILE, $
        doplot:info.DOPLOT, doSaveImage:info.DOSAVEIMAGE, save_csv: info.SAVE_CSV, $
        quit:info.QUIT}
        
    ;        print, info.TARREF_FNAME
        
    RETURN, result
END

