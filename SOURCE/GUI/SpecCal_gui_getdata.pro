
; :DESCRIPTION:
;   Retrieve the data from the SpecCalGUI and update the info structure.
;
; :PARAMS:
;    info
;    info_ptr
;
; :OUTPUTS:
;
; :REQUIRES:
;
; :NOTES:
;
; :AUTHOR: Lorenzo Busetto
;
; :HISTORY:
;
; Created: 1-ott-2009
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
PRO SPECCAL_GUI_GETDATA,info,info_ptr

    ; Get Processing Parameters
    WIDGET_CONTROL,info.SSL_WID, get_value = tmp   &   info.SSL = tmp
    WIDGET_CONTROL,info.FWL_WID, get_value = tmp   &   info.FWL = tmp
    WIDGET_CONTROL,info.FLTW_WID, get_value = tmp   &   info.FLTW = tmp
;    WIDGET_CONTROL, info.NOMFWHM_WID, GET_VALUE=tmp & info.NOMFWHM = FLOAT(tmp)
    info.METHTYPE = WIDGET_INFO(info.METHTYPE_WID,/DROPLIST_SELECT)
    info.MEASTYPE_IND = WIDGET_INFO(info.MEASTYPE_WID,/DROPLIST_SELECT)
    info.UMEAS_IND = WIDGET_INFO(info.UMEAS_WID,/DROPLIST_SELECT)
     
    ; Read Target Reflectance File
    data = SPECCAL_READ_TAR_REF(info.TARREF_FNAME, info.TARREF)
    tmp_size = size (data)
    
    IF  tmp_size[1] EQ 7 THEN BEGIN
        mess = DIALOG_MESSAGE('A problem was encountered while reading the Target Reflectance File. Returning to MAIN', TITLE='Warning')
        info.TARREF_FNAME =  info.INST_DIR+'SPECCAL_FILES\SpecCal_White Reference.txt'
        info.TARREF_IND = 0
        *info_ptr = info
        RETURN
    ENDIF ELSE info.TARREF = data
    
    ; Get Ranges Values
    ; r1
    WIDGET_CONTROL,info.LOW1_WID, get_value = tmp   &   info.RANGES[1,0,0] = tmp
    WIDGET_CONTROL,info.UP1_WID, get_value = tmp   &   info.RANGES[1,1,0] = tmp
    ; r2
    WIDGET_CONTROL,info.LOW2_WID, get_value = tmp   &   info.RANGES[1,0,1] = tmp
    WIDGET_CONTROL,info.UP2_WID, get_value = tmp   &   info.RANGES[1,1,1] = tmp
    ; r3
    WIDGET_CONTROL,info.LOW3_WID, get_value = tmp   &   info.RANGES[1,0,2] = tmp
    WIDGET_CONTROL,info.UP3_WID, get_value = tmp   &   info.RANGES[1,1,2] = tmp
    ; r4
    WIDGET_CONTROL,info.LOW4_WID, get_value = tmp   &   info.RANGES[1,0,3] = tmp
    WIDGET_CONTROL,info.UP4_WID, get_value = tmp   &   info.RANGES[1,1,3] = tmp
    ; r5
    WIDGET_CONTROL,info.LOW5_WID, get_value = tmp   &   info.RANGES[1,0,4] = tmp
    WIDGET_CONTROL,info.UP5_WID, get_value = tmp   &   info.RANGES[1,1,4] = tmp
    ; r6
    WIDGET_CONTROL,info.LOW6_WID, get_value = tmp   &   info.RANGES[1,0,5] = tmp
    WIDGET_CONTROL,info.UP6_WID, get_value = tmp   &   info.RANGES[1,1,5] = tmp
    ; r7
    WIDGET_CONTROL,info.LOW7_WID, get_value = tmp   &   info.RANGES[1,0,6] = tmp
    WIDGET_CONTROL,info.UP7_WID, get_value = tmp   &   info.RANGES[1,1,6] = tmp
    ; r8
    WIDGET_CONTROL,info.LOW8_WID, get_value = tmp   &   info.RANGES[1,0,7] = tmp
    WIDGET_CONTROL,info.UP8_WID, get_value = tmp   &   info.RANGES[1,1,7] = tmp
    ; r9
    WIDGET_CONTROL,info.LOW9_WID, get_value = tmp   &   info.RANGES[1,0,8] = tmp
    WIDGET_CONTROL,info.UP9_WID, get_value = tmp   &   info.RANGES[1,1,8] = tmp
    ; r10
    WIDGET_CONTROL,info.LOW10_WID, get_value = tmp   &   info.RANGES[1,0,9] = tmp
    WIDGET_CONTROL,info.UP10_WID, get_value = tmp   &   info.RANGES[1,1,9] = tmp
    ; r11
    WIDGET_CONTROL,info.LOW11_WID, get_value = tmp   &   info.RANGES[1,0,10] = tmp
    WIDGET_CONTROL,info.UP11_WID, get_value = tmp   &   info.RANGES[1,1,10] = tmp
    ; r12
    WIDGET_CONTROL,info.LOW12_WID, get_value = tmp   &   info.RANGES[1,0,11] = tmp
    WIDGET_CONTROL,info.UP12_WID, get_value = tmp   &   info.RANGES[1,1,11] = tmp
    ; r13
    WIDGET_CONTROL,info.LOW13_WID, get_value = tmp   &   info.RANGES[1,0,12] = tmp
    WIDGET_CONTROL,info.UP13_WID, get_value = tmp   &   info.RANGES[1,1,12] = tmp
    ; rBroad1
    WIDGET_CONTROL,info.LOWBROAD1_WID, get_value = tmp   &   info.RANGES_BROAD1[1,0] = tmp
    WIDGET_CONTROL,info.UPBROAD1_WID, get_value = tmp   &   info.RANGES_BROAD1[1,1] = tmp
    
;    ; rBroad2
;    WIDGET_CONTROL,info.LOWBROAD2_WID, get_value = tmp   &   info.RANGES_BROAD2[1,0] = tmp
;    WIDGET_CONTROL,info.UPBROAD2_WID, get_value = tmp   &   info.RANGES_BROAD2[1,1] = tmp
    
    ; rUser
    WIDGET_CONTROL,info.LOWUSER_WID, get_value = tmp   &   info.RANGES_USER[0] = tmp
    WIDGET_CONTROL,info.UPUSER_WID, get_value = tmp   &   info.RANGES_USER[1] = tmp
        
    ; Get Selected Ranges
    
    WIDGET_CONTROL,info.SEL1_WID, get_value = tmp   &   info.RANGESEL[0] = tmp
    WIDGET_CONTROL,info.SEL2_WID, get_value = tmp   &   info.RANGESEL[1] = tmp
    WIDGET_CONTROL,info.SEL3_WID, get_value = tmp   &   info.RANGESEL[2] = tmp
    WIDGET_CONTROL,info.SEL4_WID, get_value = tmp   &   info.RANGESEL[3] = tmp
    WIDGET_CONTROL,info.SEL5_WID, get_value = tmp   &   info.RANGESEL[4] = tmp
    WIDGET_CONTROL,info.SEL6_WID, get_value = tmp   &   info.RANGESEL[5] = tmp
    WIDGET_CONTROL,info.SEL7_WID, get_value = tmp   &   info.RANGESEL[6] = tmp
    WIDGET_CONTROL,info.SEL8_WID, get_value = tmp   &   info.RANGESEL[7] = tmp
    WIDGET_CONTROL,info.SEL9_WID, get_value = tmp   &   info.RANGESEL[8] = tmp
    WIDGET_CONTROL,info.SEL10_WID, get_value = tmp   &   info.RANGESEL[9] = tmp
    WIDGET_CONTROL,info.SEL11_WID, get_value = tmp   &   info.RANGESEL[10] = tmp
    WIDGET_CONTROL,info.SEL12_WID, get_value = tmp   &   info.RANGESEL[11] = tmp
    WIDGET_CONTROL,info.SEL13_WID, get_value = tmp   &   info.RANGESEL[12] = tmp
    WIDGET_CONTROL,info.SELBROAD1_WID, get_value = tmp   &   info.RANGESEL_BROAD1 = tmp
;    WIDGET_CONTROL,info.SELBROAD2_WID, get_value = tmp   &   info.RANGESEL_BROAD2 = tmp
    WIDGET_CONTROL,info.SELUSER_WID, get_value = tmp   &   info.RANGESEL_USER = tmp
    
    ; Get Plot Parameter
    WIDGET_CONTROL,info.PLOT_WID, get_value = tmp   &   info.DOPLOT = tmp
    WIDGET_CONTROL,info.SAVE_WID, get_value = tmp   &   info.DOSAVEIMAGE = tmp
    WIDGET_CONTROL,info.SAVE_WID2, get_value = tmp   &   info.SAVE_CSV = tmp
    
    ;  WIDGET_CONTROL,info.PLOT_OPT_WID, get_value = tmp   &   info.PLOT_OPT = tmp
    ;  WIDGET_CONTROL,info.SAVE_CSV_WID, get_value = tmp   &   info.SAVE_CSV = tmp
    
    *info_ptr = info

END
