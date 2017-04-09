;+
;:ROUTINE NAME:
;SPECCAL_PLOT
;
; :PURPOSE:
;   This routine is used for plotting and svaing results of the SpecCal application
; :INPUTS:
;
; :OUTPUTS:
;
; :PARAMS:
;    range: min and max WL of the spectral range analyzed
;    xo: wl of observations
;    xm_SSO_RFO: WL of MODTRAN data resampled to retrieved parameters
;    yo: observed data
;    ym2o_SSO_RFO:MODTRAN data resampled to retrieved parameters and scaled on observed data
;    ym2o_SSO_NFO:MODTRAN data resampled to nominal parameters and scaled on observed data
;    xm_SSO_NFO: WL of MODTRAN data resampled to nominal parameters
;    NFO: Nominal FWHM
;    P: Results of minimization
;    doPLOT: 1 --> plot to screen
;    dosaveimage: 1 --> save to tiff 
;    save_csv: 1 --> save to CSV
;    MethodType: --> 0 = ratio: 1 correlation
;    R: rho ratio @retrieved pars
;    Rraw: rho ratio @ nominal pars
;    out_file: name of output file
;
; :RETURNS:
;
; :REQUIRES:
;
; :NOTES:
;
; :HISTORY:
;
;: Created:	12-apr-2010
;
; :AUTHOR: Lorenzo Busetto, Michele Meroni
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
PRO SPECCAL_PLOT,range, $
        xo,xm_SSO_RFO, yo, ym2o_SSO_RFO,ym2o_SSO_NFO,xm_SSO_NFO,$
        NFO, P, $
        doPLOT, dosaveimage, save_csv, MethodType, $
        R,Rraw, $
        out_file
        
    ; Set multiple graphs
        
    !P.MULTI = [0, 2, 1]
    
    ; Define Colors
    
    
    cblack = FSC_COLOR("Black", !D.Table_Size-2)
    cwhite = FSC_COLOR("White", !D.Table_Size-3)
    cred = FSC_COLOR("Red", !D.Table_Size-4)
    cgreen = FSC_COLOR("Green", !D.Table_Size-5)
    cblu = FSC_COLOR("Blue", !D.Table_Size-6)
    cpurp = FSC_COLOR("Purple", !D.Table_Size-7)
   
    windowTitle='SpecCal v. 1.2 - Results at Spectral Range '+STRTRIM(STRING(range[0], format='(i4)'),2)+'-'+STRTRIM(STRING(range[1], format='(i4)'),2)
    
    ; Plot to Screen
    
    ;    IF doplot EQ 1 THEN BEGIN
    
    WINDOW, /FREE, xsize=1050, ysize=470, title=windowTitle
    
    ; Plot Irradiances Graph
    
    ; Make Preliminary Computaions for irradiances graph
    
    xrange=FIX([MIN(xo)-(MAX(xo)-MIN(xo))/20.0, MAX(xo)+(MAX(xo)-MIN(xo))/20.0])  ; Compute wl_range
    maxy=MAX([yo,ym2o_SSO_RFO,ym2o_SSO_NFO])
    miny=MIN([yo,ym2o_SSO_RFO,ym2o_SSO_NFO])
    bord=(maxy-miny)/8.0
    yrange=[miny-bord, maxy+bord]
    
    ;observed irradiance , modelled irradiance (scaled on obs) (nominal fwhm, shift = 0), modelled final
            
    PLOT, xo, yo, xtitle='Wavelength (nm)', ytitle='E ', YRANGE=yrange, YSTYLE=1,$
        XRANGE=xrange, XSTYLE=1,  /NORMAL, position = [0.104762, 0.106383,0.428571,0.744681], $
        COLOR=cblack, BACKGROUND= cwhite, THICK=2 , font = -1, charsize = 1.2, xticks = 4, yticks = 4, charthick = 1, xthick = 1, ythick = 1
    OPLOT, xm_SSO_RFO, ym2o_SSO_RFO, color= cred, THICK=2
    OPLOT, xm_SSO_NFO, ym2o_SSO_NFO, color= cblu,linestyle =2, THICK=2
    ret_fwhm  = STRING(p[1],FORMAT='(f6.3)')
    IF p[0] GE 0 THEN ret_ss = STRING(p[0],FORMAT='(f6.3)') ELSE ret_ss = STRING(p[0],FORMAT='(f6.3)')
    nom_fwhm = STRING(NFO,FORMAT='(f6.3)')
    
    LEGEND,['Emeas', $
        'Emod_sc - Retrieved values (SS = '+ret_ss+' nm, FWHM = '+ret_fwhm +' nm)', $
        'Emod_sc - Nominal values (SS = 0 nm, FWHM = '+nom_fwhm+' nm)'], $
        thick = 2, linestyle = [0,0,2],  colors = [cblack, cred, cblu], position = [0.0142857,0.882979], /normal,$
        textcolors = cblack, spacing = 1.5, pspacing = 1.2, charthick = 1
        
    IF (MethodType EQ 0) THEN BEGIN
    
        maxy=MAX([R,Rraw])
        miny=MIN([R,Rraw])
        bord=(maxy-miny)/8.0
        yrange=[miny-bord, maxy+bord]
        ;rho = GREEK('rho')
        rho = '!4' + String("161B) + '!X'
        
        PLOT, xo, R, BACKGROUND= cwhite, COLOR= cblack, YSTYLE=1, xstyle = 1,$            ;yrange=[0,2]
            lineSTYLE=0, XTITLE='Wavelength (nm)', YTITLE=rho, $
            POSITION=[0.57142857, 0.1063829, 0.89523809, 0.7446808], /NORMAL, font = -1, charsize = 1.2, yrange = yrange, xrange = xrange, xticks = 4, yticks = 4, xthick = 1, ythick = 1, charthick = 1
        OPLOT, xo, R, color = cred, linestyle = 0, thick = 2
        OPLOT, xo, Rraw, color = cblu, linestyle = 2, thick = 2
        
        
        LEGEND,[rho + ' - Retrieved values (SS = '+ret_ss+' nm, FWHM = '+ret_fwhm +' nm)', $
            rho + ' - Nominal values (SS = 0 nm, FWHM = '+nom_fwhm+' nm)'], $
            thick = 2, LINESTYLE=  [0,2],  colors = [cred, cblu], position = [0.5142857,0.8829787], /NORMAL,$
            textcolors = cblack, spacing = 1.5, pspacing = 1.2, charthick = 1
        XYOUTS, 0.261904, 0.9361702, windowTitle, color = cblack, charsize = 1.5, charthick = 1.5, /NORMAL
        snapshot = TVRD(True=1)
        stop
        ;       Set_Plot, thisDevice
        IF doPLOT EQ 0 THEN WDELETE
        IF dosaveimage EQ 1 THEN WRITE_TIFF, out_file+'_'+STRTRIM(STRING(range[0], format='(i4)'),2)+'-'+STRTRIM(STRING(range[1], format='(i4)'),2)+'.tiff',snapshot, orientation = 0, xresol = 150, yresol = 150
     
        IF save_csv EQ 1 THEN BEGIN
            headers = ['wl (nm)',$
                'Emeas ', $
                'Emod_sc - Retrieved values (SS = '+ret_ss+' nm, FWHM = '+ret_fwhm +' nm)', $
                'Emod_sc - Nominal values (SS = 0 nm, FWHM = '+nom_fwhm+' nm)', $
                'rho - Retrieved values (SS = '+ret_ss+' nm, FWHM = '+ret_fwhm +' nm)',$
                'rho - Nominal values (SS = 0 nm, FWHM = '+nom_fwhm+' nm)' $
                ]
            out_csv = out_file+'_'+STRTRIM(STRING(range[0], format='(i4)'),2)+'-'+STRTRIM(STRING(range[1], format='(i4)'),2)+'_plt.csv'
            OPENW, tmp_lun, out_csv, ERROR = err, /GET_LUN     ; Check if file is already open in other apps
            IF err EQ -248 THEN BEGIN
                mess = DIALOG_MESSAGE('An Error Occured while Opening the PLOT Output File !',TITLE='Warning')
            ENDIF ELSE BEGIN
                WRITE_CSV_DATA,   TRANSPOSE([[xo], [yo], [ym2o_SSO_NFO], [ym2o_SSO_RFO], [R],[Rraw]]),  DELIMITER=';', $
                FILENAME=out_csv, COLUMNHEADERS= headers
                FREE_LUN, tmp_lun
            ENDELSE
            
        ENDIF
    ENDIF
    
    
    
    ; Plot R_smooth or COrrelation Data, according to method type
    
    IF (MethodType EQ 1) THEN BEGIN                                      ;correlation
    
        cost=1-CORRELATE(yo, ym2o_SSO_RFO, /DOUBLE)
        c12=REGRESS(yo, ym2o_SSO_NFO, yfit=yregfit2, correlation=rcoeff2, const=c02)            ; Regression @ nominal
        c1=REGRESS(yo, ym2o_SSO_RFO, yfit=yregfit, correlation=rcoeff, const=c0)                    ; Regression @ retrieved
        PLOT, yo, ym2o_SSO_RFO, BACKGROUND= cwhite, COLOR= cblack, YSTYLE=1, $            ;yrange=[0,2]
            XSTYLE=1, XTITLE='Emeas', YTITLE='Emod_sc ', $
            POSITION=[0.57142857, 0.1063829, 0.89523809, 0.7446808], /NORMAL, PSYM=6 , font = -1, charsize = 1.2, charthick = 1, yrange = yrange, xrange = yrange, xticks = 4, yticks = 4, xthick = 1 , ythick = 1
            
        OPLOT, yo, yregfit2, color= cblu, thick = 2, LINESTYLE= 2
        OPLOT,yo, yregfit, COLOR= cred, thick = 2
        OPLOT, yo, ym2o_SSO_NFO, color= cblu, PSYM=5
        X = [-1, 0, 1, 0, -1]
        Y = [0, 1, 0, -1, 0]
        USERSYM, X, Y  , /fill
        OPLOT, yo, ym2o_SSO_RFO, PSYM=8, color= cwhite, thick = 1
        OPLOT, yo, ym2o_SSO_RFO, PSYM=6, color= cred, thick = 1
        LEGEND,['Emeas vs Emod_sc - Retrieved values (SS = '+ret_ss+' nm, FWHM = '+ret_fwhm +' nm)', $
            'Emeas vs Emod_sc - Nominal values (SS = 0 nm, FWHM = '+nom_fwhm+' nm)'], $
            thick = 1, psym =  [6,5],  colors = [cred, cblu], position = [0.4857142,0.8829787], /NORMAL,$
            textcolors = cblack, spacing = 1.5, pspacing = 1.2, charthick = 1
        XYOUTS, 0.261904, 0.9361702, windowTitle, color = cblack, charsize = 1.5, charthick = 1.5, /NORMAL
        snapshot = TVRD(True=1)
stop
        IF doPLOT EQ 0 THEN WDELETE
        IF dosaveimage EQ 1 THEN WRITE_TIFF, out_file+'_'+STRTRIM(STRING(range[0], format='(i4)'),2)+'-'+STRTRIM(STRING(range[1], format='(i4)'),2)+'.tiff',snapshot, orientation = 0, xresol = 150, yresol = 150
        
        IF save_csv EQ 1 THEN BEGIN
            headers = ['wl (nm)',$
                'Emeas ', $
                'Emod_sc - Retrieved values (SS = '+ret_ss+' nm, FWHM = '+ret_fwhm +' nm)', $
                'Emod_sc - Nominal values (SS = 0 nm, FWHM = '+nom_fwhm+' nm)' $
                ]
                
            out_csv = out_file+'_'+STRTRIM(STRING(range[0], format='(i4)'),2)+'-'+STRTRIM(STRING(range[1], format='(i4)'),2)+'_plt.csv'
            OPENW, tmp_lun, out_csv, ERROR = err, /GET_LUN       ; Check if file is already open in other apps
            IF err EQ -248 THEN BEGIN
                mess = DIALOG_MESSAGE('An Error Occured while Opening the PLOT Output File !',TITLE='Warning')
            ENDIF ELSE BEGIN
                WRITE_CSV_DATA,   TRANSPOSE([[xo], [yo], [ym2o_SSO_NFO], [ym2o_SSO_RFO]]),  DELIMITER=';', $
                FILENAME=out_csv, COLUMNHEADERS= headers
                FREE_LUN, tmp_lun
            ENDELSE
        ENDIF
    ENDIF
    
    !P.MULTI = 0
    
END
    