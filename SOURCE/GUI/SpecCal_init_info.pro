;+
; :DESCRIPTION:
;    Initialize SpeCalGUI parameters if no PREVIOUS file is found
;
; :PARAMS:
;    inst_dir: SpecCal installation directory
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
FUNCTION SPECCAL_INIT_INFO, inst_dir
    info_tmp = {mod4max:1000L}
    
    help_file = inst_dir+'Help'+PATH_SEP()+'SpecCal v1.2 - User''s Guide.pdf'
    about_file = inst_dir+'Help'+PATH_SEP()+'About SpecCal v1.2.pdf'
    init_ranges = DBLARR(3,2,13)+1
    init_ranges[0,*,*] = -1.
    init_ranges[1,*,*] = 0.
    init_ranges[2,*,*] = 1.
    init_ranges_Broad1 =[[-1.0D, 0.d,1.0D],[-1.0D, 0.d,1.0D]]    ;Broad1
    init_ranges_User =[0.0D, 0.0D]    ;User
    
    
    info = {$
        in_file:'', in_dir:inst_dir+'EXAMPLE_INPUT_DATA\', in_file_lab:'', in_wid:0L, out_file:'', out_dir:inst_dir+'EXAMPLE_OUTPUT_DATA\',out_wid:0L, out_file_lab:'', $
        help_file:help_file, about_file:about_file, $
        mod_file:'', mod_dir:inst_dir+'MODTRAN_FILES\', mod_file_lab:'',mod_wid:0L, U_Meas:'L',$
        ssl:1., ssl_wid:0L, fwl:2, fwl_wid:0l, fltW:10, fltW_WID:0L, $
        nomfwhm: 1.0, nomfwhm_wid: 0L, $
        refnames :['None','Standard White Ref.','Change File',''],$
        tarref:FLTARR(2,701), tarref_fname:inst_dir+'TARGET_REFLECTANCES\Standard_White_Reference.csv', tarref_wid:0L, tarref_ind:1, $
        methtype:0, methtype_wid: 0L, $
        meastype:0, meastype_wid:0L, meastype_ind:0,meastypenames:['Reflected Radiance','Solar Irradiance'], umeas_ind:0L, $
        umeas:0, umeas_wid:0L, umeas_names:['Physical Units', 'Counts'], $
        ranges: init_ranges, $
        ranges_Broad1:init_ranges_Broad1, $
        ranges_User:init_ranges_User, $
        rangesel:[1,1,1,1,1,1,1,1,1,1,1,1,1], sensel:[0,0,0,0,0,0,0,0,0,0,0,0,0], $
        rangesel_Broad1: 1,  rangesel_user:0 ,$
        sensel_Broad1: 0, sensel_user:0 ,$
        InMin:-99999., InMax:99999., $
        Infwhm:-99999., InSS:-99999., $
        mod4Min:-99999., mod4Max:-99999., $
        mod4fwhm:-99999., mod4ss:-99999., $
        doplot:1L, dosaveimage:1L,    save_csv:1, $
        main_wid:0L, range_wid_scr:0L,  $
        mod4min_wid:0L,  mod4max_wid:0l, $
        mod4ss_wid:0L,  mod4fwhm_wid:0L, $
        INmin_wid:0L,  INmax_wid:0l, $
        INss_wid:0L,  INfwhm_wid:0L, $
        r1_wid:0L, r2_wid:0L, r3_wid:0L, r4_wid:0L, r5_wid:0L, r6_wid:0L, r7_wid:0L,$
        r8_wid:0L, r9_wid:0L, r10_wid:0L,r11_wid:0L, r12_wid:0L, r13_wid:0L, $
        rBroad1_wid:0l, rUser_wid:0L, $
        r1_wid_rg:0L, r2_wid_rg:0L, r3_wid_rg:0L, r4_wid_rg:0L, r5_wid_rg:0L, $
        r6_wid_rg:0L, r7_wid_rg:0L, r8_wid_rg:0L, r9_wid_rg:0L, r10_wid_rg:0L, $
        r11_wid_rg:0L, r12_wid_rg:0L, r13_wid_rg:0L, $
        rBroad1_wid_rg:0l, rUser_wid_rg:0l,$
        low1_wid: 0l, up1_wid:0L, sel1_wid:0L, $
        low2_wid: 0l, up2_wid:0L, sel2_wid:0L,$
        low3_wid: 0l, up3_wid:0L, sel3_wid:0L,$
        low4_wid: 0l, up4_wid:0L, sel4_wid:0L,$
        low5_wid: 0l, up5_wid:0L, sel5_wid:0L,$
        low6_wid: 0l, up6_wid:0L, sel6_wid:0L,$
        low7_wid: 0l, up7_wid:0L, sel7_wid:0L,$
        low8_wid: 0l, up8_wid:0L, sel8_wid:0L,$
        low9_wid: 0l, up9_wid:0L, sel9_wid:0L,$
        low10_wid: 0l, up10_wid:0L, sel10_wid:0L,$
        low11_wid: 0l, up11_wid:0L, sel11_wid:0L,$
        low12_wid: 0l, up12_wid:0L, sel12_wid:0L,$
        low13_wid: 0l, up13_wid:0L, sel13_wid:0L,$
        lowBroad1_wid: 0l, upBroad1_wid:0L, selBroad1_wid:0L,$
        lowUser_wid: 0l, upUser_wid:0L, selUser_wid:0L,$
        defbut_wid:0L, $
        inst_dir:inst_dir, $
        plot_wid:0l, save_wid:0L, save_wid2:0L, $
        Quit:0L, $
        xsize : 0L, ysize : 0L $
        }
        
    RETURN, info
    
END