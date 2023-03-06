*&---------------------------------------------------------------------*
*& Include          ZRMM4430F01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form VARIANT_F4
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> GS_VARIANT
*&      <-- P_VAR
*&---------------------------------------------------------------------*
FORM variant_f4 USING is_variant TYPE any
                 CHANGING  cv_variant_screen TYPE any.

  DATA: ls_variant TYPE disvariant,
        lv_exit(1) TYPE c.

  CALL FUNCTION 'LVC_VARIANT_F4'
    EXPORTING
      is_variant    = is_variant
      i_save        = 'A'
    IMPORTING
      e_exit        = lv_exit
      es_variant    = ls_variant
    EXCEPTIONS
      not_found     = 1
      program_error = 2
      OTHERS        = 3.
  IF sy-subrc <> 0.
    MESSAGE i031(zpca01).  "'No found layout available for F4.'
  ENDIF.

  IF lv_exit IS INITIAL.
    cv_variant_screen = ls_variant-variant.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form INITIALIZATION
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM initialization .

*------------------------------
* Set Variant
*------------------------------
  gs_variant-report   = sy-cprog.
  gs_variant-username = sy-uname.

*  SELECT SINGLE company, company_name
*      INTO ( @p_bukrs, @gv_butxt )
*    FROM zsvmm_user_info
*  WHERE user_id = @sy-uname.

  GV_LANGU = SY-LANGU.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CHECK_AUTHORITY
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM check_authority .

*  call function 'ZFMM_AUTH_CHECK'
*    EXPORTING
*      iv_user = sy-uname.
**     IV_OBJECT                   = 'ZMM_COMMON'
**     IV_BUKRS                    =
**     IV_EKORG                    =
**     IV_LGORT                    =
**     IV_WERKS                    =
**     IV_ZEXSPA                   =
**     IV_ZPODEP                   =
**     IV_ZPRDEP                   =
** TABLES
**     IT_BUKRS                    =
**     IT_EKORG                    =
**     IT_LGORT                    =
**     IT_WERKS                    =
**     IT_ZEXSPA                   =
**     IT_ZPODEP                   =
**     IT_ZPRDEP                   =
** EXCEPTIONS
**     NO_ID_DATA_FOUND            = 1
**     AUTHORIZATION_FAILURE       = 2
**     NO_INPUT_AUTH_VALUE         = 3
**     NO_DATA_FOUND               = 4
**     MANDATORYFIELDISMISS        = 5
**     OTHERS  = 6
*  IF sy-subrc <> 0.
*    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 DISPLAY LIKE 'E'.
*    LEAVE LIST-PROCESSING.
*  ENDIF.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM get_data .

*--------------------------------
* Data 추출
*--------------------------------
  CLEAR : gt_data, gt_disp.

  PERFORM get_data_all.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form get_data_all
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM get_data_all .

*-
  DATA: ls_disp      TYPE ts_disp.
*-

  SELECT FROM zsvbmmmatdocb
    FIELDS budat, charg, cpudt, cputm, mjahr, mblnr, zeile, bukrs, butxt, matnr, maktx, bwart, btext, mvttype, sobkz, werks, werksname,
           lgort, lgortname, umwrk, umwrkname, umlgo, umlgoname, bwtar, menge, erfme, erfmg, meins, lifnr, lifnrname, kunnr, kunnrname,
           hsdat, vfdat, lichn, zmaker, waterratio, lbbsa_sid, kzbew, bstaus_sg, bsttyp_sg, berid, waers, dmbtr,
           ebeln, ebelp, aufnr, aufps, rsnum, rspos, vbeln_im, vbelp_im, shkzg, elikz, kokrs, kzvbr, prctr, aufpl, aplzl, sakto, tcode2, usnam

  WHERE bukrs = @p_bukrs
    AND matnr IN @s_matnr
    AND charg IN @s_charg
    AND hsdat IN @s_hsdat
    AND vfdat IN @s_vfdat
    AND lichn IN @s_lichn
    AND lifnr IN @s_lifnr
    AND kunnr IN @s_kunnr
    AND zmaker IN @s_maker
    AND charg NE ''
  INTO CORRESPONDING FIELDS OF TABLE @gt_data.


  IF gt_data[] IS NOT INITIAL.
    LOOP AT gt_data INTO DATA(ls_data).

      MOVE-CORRESPONDING ls_data TO ls_disp.
      APPEND ls_disp TO gt_disp. CLEAR ls_disp.

    ENDLOOP.


    IF p_chk1 = 'X'.
      SORT gt_disp BY matnr ASCENDING charg ASCENDING cpudt DESCENDING cputm DESCENDING.
      DELETE ADJACENT DUPLICATES FROM gt_disp COMPARING matnr charg.
    ENDIF.

    SORT gt_disp BY   matnr ASCENDING charg ASCENDING cpudt DESCENDING cputm DESCENDING aufnr DESCENDING aufps DESCENDING ebeln DESCENDING ebelp DESCENDING.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form evt_grid_data_changed
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> IRF_DATA_CHANGED
*&      --> IV_ONF4
*&      --> IV_ONF4_BEFORE
*&      --> IV_ONF4_AFTER
*&      --> IV_UCOMM
*&---------------------------------------------------------------------*
FORM EVT_GRID_DATA_CHANGED USING IRF_DATA_CHANGED
                                  TYPE REF TO CL_ALV_CHANGED_DATA_PROTOCOL
                                  IV_ONF4
                                  IV_ONF4_BEFORE
                                  IV_ONF4_AFTER
                                  IV_UCOMM TYPE  SY-UCOMM.


  LOOP AT IRF_DATA_CHANGED->MT_GOOD_CELLS INTO DATA(LS_LVC_MODI).

    READ TABLE GT_DISP ASSIGNING FIELD-SYMBOL(<LS_DISP>) INDEX LS_LVC_MODI-ROW_ID.

*-----------------------------------------------------------------
* 컬럼별 세팅 (Check_changed_컬럼명 으로 구분하여 네이밍룰 생성)
*-----------------------------------------------------------------
    CASE LS_LVC_MODI-FIELDNAME.
      WHEN 'BUKRS'.
*        PERFORM check_changed_bukrs  USING   irf_data_changed
*                                             ls_lvc_modi
*                                     CHANGING <ls_disp>.

      WHEN OTHERS.
        PERFORM CHECK_CHANGED_OTHERS USING    IRF_DATA_CHANGED
                                              LS_LVC_MODI
                                     CHANGING <LS_DISP>.
    ENDCASE.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form TOP_DOWN_PRO
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LT_SUB_PRO
*&      --> LT_SUB
*&---------------------------------------------------------------------*
FORM top_down_pro TABLES it_sub_pro TYPE tt_item
                          it_sub     TYPE tt_item.

  DATA :  ls_disp_item TYPE ts_disp.
  DATA : lt_sub_pro_101 TYPE TABLE OF ts_disp,    "생산입고 집계용
         lt_sub_pro_102 TYPE TABLE OF ts_disp,    "생산입고취소 집계용
         lt_sub_pro_261 TYPE TABLE OF ts_disp,    "생산출고집계용
         lt_sub_pro_262 TYPE TABLE OF ts_disp,    "생산출고취소집계용
         lt_sub_pro_etc TYPE TABLE OF ts_disp.    "기타집계용

*  IF IT_SUB_PRO[] IS NOT INITIAL.
*
*    LOOP AT IT_SUB_PRO ASSIGNING FIELD-SYMBOL(<LS_SUB_PROT>) .
*      IF <LS_SUB_PROT>-BWART     =  GC_101.         APPEND <LS_SUB_PROT> TO LT_SUB_PRO_101.
*      ELSEIF <LS_SUB_PROT>-BWART =  GC_102.         APPEND <LS_SUB_PROT> TO LT_SUB_PRO_102.
*      ELSEIF <LS_SUB_PROT>-BWART =  GC_261.         APPEND <LS_SUB_PROT> TO LT_SUB_PRO_261.
*      ELSEIF <LS_SUB_PROT>-BWART =  GC_262.         APPEND <LS_SUB_PROT> TO LT_SUB_PRO_262.
*      ELSE.
*        APPEND <LS_SUB_PROT> TO LT_SUB_PRO_ETC.
*      ENDIF.
*    ENDLOOP.

** 261,262
*    IF LT_SUB_PRO_101[] IS NOT INITIAL.
*
*      DATA(LT_PRO_101) = LT_SUB_PRO_101[].
*      IF LT_PRO_101[] IS NOT INITIAL.
*
*        SORT LT_PRO_101 BY AUFNR MATNR CPUDT.         DELETE ADJACENT DUPLICATES FROM LT_PRO_101 COMPARING AUFNR MATNR CPUDT.
*
*        SELECT FROM ZSVBMMMATDOCB
*        FIELDS BUDAT, CHARG, CPUDT, CPUTM, MJAHR, MBLNR, ZEILE, BUKRS, BUTXT, MATNR, MAKTX, BWART, BTEXT, MVTTYPE, SOBKZ, WERKS, WERKSNAME,
*               LGORT, LGORTNAME, UMWRK, UMWRKNAME, UMLGO, UMLGONAME, BWTAR, MENGE, ERFME, ERFMG, MEINS, LIFNR, LIFNRNAME, KUNNR, KUNNRNAME,
*               HSDAT, VFDAT, LICHN, ZMAKER, WATERRATIO, LBBSA_SID, KZBEW, BSTAUS_SG, BSTTYP_SG, BERID, WAERS, DMBTR,
*               EBELN, EBELP, AUFNR, AUFPS, RSNUM, RSPOS, VBELN_IM, VBELP_IM, SHKZG, ELIKZ, KOKRS, KZVBR, PRCTR, AUFPL, APLZL, SAKTO, TCODE2, USNAM
*        FOR ALL ENTRIES IN @LT_PRO_101
*          WHERE AUFNR = @LT_PRO_101-AUFNR
*          AND MATNR NE @LT_PRO_101-MATNR
*          AND  CPUDT < @LT_PRO_101-CPUDT
*          AND BWART IN ( @GC_261 , @GC_262 )
*          APPENDING CORRESPONDING FIELDS OF TABLE @IT_SUB.
*
*        FREE LT_PRO_101.
*
*        LT_PRO_101[] = LT_SUB_PRO_101[].
*        SORT LT_PRO_101 BY AUFNR MATNR CPUDT CPUTM.         DELETE ADJACENT DUPLICATES FROM LT_PRO_101 COMPARING AUFNR MATNR CPUDT CPUTM.
*
*        SELECT FROM ZSVBMMMATDOCB
*        FIELDS BUDAT, CHARG, CPUDT, CPUTM, MJAHR, MBLNR, ZEILE, BUKRS, BUTXT, MATNR, MAKTX, BWART, BTEXT, MVTTYPE, SOBKZ, WERKS, WERKSNAME,
*               LGORT, LGORTNAME, UMWRK, UMWRKNAME, UMLGO, UMLGONAME, BWTAR, MENGE, ERFME, ERFMG, MEINS, LIFNR, LIFNRNAME, KUNNR, KUNNRNAME,
*               HSDAT, VFDAT, LICHN, ZMAKER, WATERRATIO, LBBSA_SID, KZBEW, BSTAUS_SG, BSTTYP_SG, BERID, WAERS, DMBTR,
*               EBELN, EBELP, AUFNR, AUFPS, RSNUM, RSPOS, VBELN_IM, VBELP_IM, SHKZG, ELIKZ, KOKRS, KZVBR, PRCTR, AUFPL, APLZL, SAKTO, TCODE2, USNAM
*        FOR ALL ENTRIES IN @LT_PRO_101
*          WHERE AUFNR = @LT_PRO_101-AUFNR
*          AND MATNR NE @LT_PRO_101-MATNR
*          AND  CPUDT = @LT_PRO_101-CPUDT
*          AND  CPUTM <=  @LT_PRO_101-CPUTM
*          AND BWART IN ( @GC_261 , @GC_262 )
*          APPENDING CORRESPONDING FIELDS OF TABLE @IT_SUB.
*
*        FREE LT_PRO_101.
*      ENDIF.
*    ENDIF.
*
*** 101
*    IF LT_SUB_PRO_102[] IS NOT INITIAL.
*
*      DATA(LT_PRO_102) = LT_SUB_PRO_102[].
*
*      IF LT_PRO_102[] IS NOT INITIAL.
*        SORT LT_PRO_102 BY AUFNR MATNR CHARG CPUDT.         DELETE ADJACENT DUPLICATES FROM LT_PRO_102 COMPARING AUFNR MATNR CHARG CPUDT.
*
*        SELECT FROM ZSVBMMMATDOCB
*        FIELDS BUDAT, CHARG, CPUDT, CPUTM, MJAHR, MBLNR, ZEILE, BUKRS, BUTXT, MATNR, MAKTX, BWART, BTEXT, MVTTYPE, SOBKZ, WERKS, WERKSNAME,
*               LGORT, LGORTNAME, UMWRK, UMWRKNAME, UMLGO, UMLGONAME, BWTAR, MENGE, ERFME, ERFMG, MEINS, LIFNR, LIFNRNAME, KUNNR, KUNNRNAME,
*               HSDAT, VFDAT, LICHN, ZMAKER, WATERRATIO, LBBSA_SID, KZBEW, BSTAUS_SG, BSTTYP_SG, BERID, WAERS, DMBTR,
*               EBELN, EBELP, AUFNR, AUFPS, RSNUM, RSPOS, VBELN_IM, VBELP_IM, SHKZG, ELIKZ, KOKRS, KZVBR, PRCTR, AUFPL, APLZL, SAKTO, TCODE2, USNAM
*        FOR ALL ENTRIES IN @LT_PRO_102
*          WHERE AUFNR = @LT_PRO_102-AUFNR
*          AND MATNR = @LT_PRO_102-MATNR
*          AND CHARG = @LT_PRO_102-CHARG
*          AND CPUDT < @LT_PRO_102-CPUDT
*          AND BWART = @GC_101
*          APPENDING CORRESPONDING FIELDS OF TABLE @IT_SUB.
*
*        FREE LT_PRO_102.
*
*        LT_PRO_102[] = LT_SUB_PRO_102[].
*        SORT LT_PRO_102 BY AUFNR MATNR CHARG CPUDT CPUTM.         DELETE ADJACENT DUPLICATES FROM LT_PRO_102 COMPARING AUFNR MATNR CHARG CPUDT CPUTM.
*
*        SELECT FROM ZSVBMMMATDOCB
*        FIELDS BUDAT, CHARG, CPUDT, CPUTM, MJAHR, MBLNR, ZEILE, BUKRS, BUTXT, MATNR, MAKTX, BWART, BTEXT, MVTTYPE, SOBKZ, WERKS, WERKSNAME,
*               LGORT, LGORTNAME, UMWRK, UMWRKNAME, UMLGO, UMLGONAME, BWTAR, MENGE, ERFME, ERFMG, MEINS, LIFNR, LIFNRNAME, KUNNR, KUNNRNAME,
*               HSDAT, VFDAT, LICHN, ZMAKER, WATERRATIO, LBBSA_SID, KZBEW, BSTAUS_SG, BSTTYP_SG, BERID, WAERS, DMBTR,
*               EBELN, EBELP, AUFNR, AUFPS, RSNUM, RSPOS, VBELN_IM, VBELP_IM, SHKZG, ELIKZ, KOKRS, KZVBR, PRCTR, AUFPL, APLZL, SAKTO, TCODE2, USNAM
*        FOR ALL ENTRIES IN @LT_PRO_102
*          WHERE AUFNR = @LT_PRO_102-AUFNR
*          AND MATNR = @LT_PRO_102-MATNR
*          AND CHARG = @LT_PRO_102-CHARG
*          AND  CPUDT = @LT_PRO_102-CPUDT
*          AND  CPUTM <=  @LT_PRO_102-CPUTM
*          AND BWART = @GC_101
*          APPENDING CORRESPONDING FIELDS OF TABLE @IT_SUB.
*
*        FREE LT_PRO_102.
*      ENDIF.
*    ENDIF.
*
***261
*    IF LT_SUB_PRO_262[] IS NOT INITIAL.
*
*      DATA(LT_PRO_262) = LT_SUB_PRO_262[].
*
*      IF LT_PRO_262[] IS NOT INITIAL.
*
*        SORT LT_PRO_262 BY AUFNR MATNR CHARG CPUDT.         DELETE ADJACENT DUPLICATES FROM LT_PRO_262 COMPARING AUFNR MATNR CHARG CPUDT.
*
*        SELECT FROM ZSVBMMMATDOCB
*        FIELDS BUDAT, CHARG, CPUDT, CPUTM, MJAHR, MBLNR, ZEILE, BUKRS, BUTXT, MATNR, MAKTX, BWART, BTEXT, MVTTYPE, SOBKZ, WERKS, WERKSNAME,
*               LGORT, LGORTNAME, UMWRK, UMWRKNAME, UMLGO, UMLGONAME, BWTAR, MENGE, ERFME, ERFMG, MEINS, LIFNR, LIFNRNAME, KUNNR, KUNNRNAME,
*               HSDAT, VFDAT, LICHN, ZMAKER, WATERRATIO, LBBSA_SID, KZBEW, BSTAUS_SG, BSTTYP_SG, BERID, WAERS, DMBTR,
*               EBELN, EBELP, AUFNR, AUFPS, RSNUM, RSPOS, VBELN_IM, VBELP_IM, SHKZG, ELIKZ, KOKRS, KZVBR, PRCTR, AUFPL, APLZL, SAKTO, TCODE2, USNAM
*        FOR ALL ENTRIES IN @LT_PRO_262
*          WHERE AUFNR = @LT_PRO_262-AUFNR
*          AND MATNR = @LT_PRO_262-MATNR
*          AND CHARG = @LT_PRO_262-CHARG
*          AND  CPUDT < @LT_PRO_262-CPUDT
*          AND BWART = @GC_261
*          APPENDING CORRESPONDING FIELDS OF TABLE @IT_SUB.
*
*        FREE LT_PRO_262.
*
*        LT_PRO_262[] = LT_SUB_PRO_262[].
*        SORT LT_PRO_262 BY AUFNR MATNR CHARG CPUDT CPUTM.         DELETE ADJACENT DUPLICATES FROM LT_PRO_262 COMPARING AUFNR MATNR CHARG CPUDT CPUTM.
*
*        SELECT FROM ZSVBMMMATDOCB
*        FIELDS BUDAT, CHARG, CPUDT, CPUTM, MJAHR, MBLNR, ZEILE, BUKRS, BUTXT, MATNR, MAKTX, BWART, BTEXT, MVTTYPE, SOBKZ, WERKS, WERKSNAME,
*               LGORT, LGORTNAME, UMWRK, UMWRKNAME, UMLGO, UMLGONAME, BWTAR, MENGE, ERFME, ERFMG, MEINS, LIFNR, LIFNRNAME, KUNNR, KUNNRNAME,
*               HSDAT, VFDAT, LICHN, ZMAKER, WATERRATIO, LBBSA_SID, KZBEW, BSTAUS_SG, BSTTYP_SG, BERID, WAERS, DMBTR,
*               EBELN, EBELP, AUFNR, AUFPS, RSNUM, RSPOS, VBELN_IM, VBELP_IM, SHKZG, ELIKZ, KOKRS, KZVBR, PRCTR, AUFPL, APLZL, SAKTO, TCODE2, USNAM
*        FOR ALL ENTRIES IN @LT_PRO_262
*          WHERE AUFNR = @LT_PRO_262-AUFNR
*          AND MATNR = @LT_PRO_262-MATNR
*          AND CHARG = @LT_PRO_262-CHARG
*          AND CPUDT = @LT_PRO_262-CPUDT
*          AND CPUTM <=  @LT_PRO_262-CPUTM
*          AND BWART = @GC_261
*          APPENDING CORRESPONDING FIELDS OF TABLE @IT_SUB.
*
*        FREE LT_PRO_262.
*      ENDIF.
*    ENDIF.
*
***261 이하
*    IF LT_SUB_PRO_261[] IS NOT INITIAL.
*
*      DATA(LT_PRO_261) = LT_SUB_PRO_261[].
*
*      IF LT_PRO_261[] IS NOT INITIAL.
*
*        SORT LT_PRO_261 BY MATNR CHARG CPUDT.         DELETE ADJACENT DUPLICATES FROM LT_PRO_261 COMPARING MATNR CHARG CPUDT.
*
*        SELECT FROM ZSVBMMMATDOCB
*        FIELDS BUDAT, CHARG, CPUDT, CPUTM, MJAHR, MBLNR, ZEILE, BUKRS, BUTXT, MATNR, MAKTX, BWART, BTEXT, MVTTYPE, SOBKZ, WERKS, WERKSNAME,
*               LGORT, LGORTNAME, UMWRK, UMWRKNAME, UMLGO, UMLGONAME, BWTAR, MENGE, ERFME, ERFMG, MEINS, LIFNR, LIFNRNAME, KUNNR, KUNNRNAME,
*               HSDAT, VFDAT, LICHN, ZMAKER, WATERRATIO, LBBSA_SID, KZBEW, BSTAUS_SG, BSTTYP_SG, BERID, WAERS, DMBTR,
*               EBELN, EBELP, AUFNR, AUFPS, RSNUM, RSPOS, VBELN_IM, VBELP_IM, SHKZG, ELIKZ, KOKRS, KZVBR, PRCTR, AUFPL, APLZL, SAKTO, TCODE2, USNAM
*        FOR ALL ENTRIES IN @LT_PRO_261
*          WHERE AUFNR = ''
*          AND MATNR = @LT_PRO_261-MATNR
*          AND CHARG = @LT_PRO_261-CHARG
*          AND CPUDT < @LT_PRO_261-CPUDT
*          APPENDING CORRESPONDING FIELDS OF TABLE @IT_SUB.
*
*        FREE LT_PRO_261.
*
*        LT_PRO_261[] = LT_SUB_PRO_261[].
*        SORT LT_PRO_261 BY MATNR CHARG CPUDT CPUTM.
*        DELETE ADJACENT DUPLICATES FROM LT_PRO_261 COMPARING MATNR CHARG CPUDT CPUTM.
*
*        SELECT FROM ZSVBMMMATDOCB
*        FIELDS BUDAT, CHARG, CPUDT, CPUTM, MJAHR, MBLNR, ZEILE, BUKRS, BUTXT, MATNR, MAKTX, BWART, BTEXT, MVTTYPE, SOBKZ, WERKS, WERKSNAME,
*               LGORT, LGORTNAME, UMWRK, UMWRKNAME, UMLGO, UMLGONAME, BWTAR, MENGE, ERFME, ERFMG, MEINS, LIFNR, LIFNRNAME, KUNNR, KUNNRNAME,
*               HSDAT, VFDAT, LICHN, ZMAKER, WATERRATIO, LBBSA_SID, KZBEW, BSTAUS_SG, BSTTYP_SG, BERID, WAERS, DMBTR,
*               EBELN, EBELP, AUFNR, AUFPS, RSNUM, RSPOS, VBELN_IM, VBELP_IM, SHKZG, ELIKZ, KOKRS, KZVBR, PRCTR, AUFPL, APLZL, SAKTO, TCODE2, USNAM
*        FOR ALL ENTRIES IN @LT_PRO_261
*          WHERE AUFNR = ''
*          AND MATNR = @LT_PRO_261-MATNR
*          AND CHARG = @LT_PRO_261-CHARG
*          AND CPUDT = @LT_PRO_261-CPUDT
*          AND CPUTM <= @LT_PRO_261-CPUTM
*          APPENDING CORRESPONDING FIELDS OF TABLE @IT_SUB.
*
*        FREE LT_PRO_261.
*      ENDIF.
*    ENDIF.
*
***기타
*    IF LT_SUB_PRO_ETC[] IS NOT INITIAL.
*
*      DATA(LT_PRO_ETC) = LT_SUB_PRO_ETC[].
*
*      IF LT_PRO_ETC[] IS NOT INITIAL.
*
*        SORT LT_PRO_ETC BY AUFNR MATNR CHARG CPUDT.         DELETE ADJACENT DUPLICATES FROM LT_PRO_ETC COMPARING AUFNR MATNR CHARG CPUDT.
*
*        SELECT FROM ZSVBMMMATDOCB
*        FIELDS BUDAT, CHARG, CPUDT, CPUTM, MJAHR, MBLNR, ZEILE, BUKRS, BUTXT, MATNR, MAKTX, BWART, BTEXT, MVTTYPE, SOBKZ, WERKS, WERKSNAME,
*               LGORT, LGORTNAME, UMWRK, UMWRKNAME, UMLGO, UMLGONAME, BWTAR, MENGE, ERFME, ERFMG, MEINS, LIFNR, LIFNRNAME, KUNNR, KUNNRNAME,
*               HSDAT, VFDAT, LICHN, ZMAKER, WATERRATIO, LBBSA_SID, KZBEW, BSTAUS_SG, BSTTYP_SG, BERID, WAERS, DMBTR,
*               EBELN, EBELP, AUFNR, AUFPS, RSNUM, RSPOS, VBELN_IM, VBELP_IM, SHKZG, ELIKZ, KOKRS, KZVBR, PRCTR, AUFPL, APLZL, SAKTO, TCODE2, USNAM
*        FOR ALL ENTRIES IN @LT_PRO_ETC
*          WHERE AUFNR = @LT_PRO_ETC-AUFNR
*          AND MATNR = @LT_PRO_ETC-MATNR
*          AND CHARG = @LT_PRO_ETC-CHARG
*          AND  CPUDT < @LT_PRO_ETC-CPUDT
*          APPENDING CORRESPONDING FIELDS OF TABLE @IT_SUB.
*
*        FREE LT_PRO_ETC.
*
*        LT_PRO_ETC[] = LT_SUB_PRO_ETC[].
*        SORT LT_PRO_ETC BY AUFNR MATNR CHARG CPUDT CPUTM.         DELETE ADJACENT DUPLICATES FROM LT_PRO_ETC COMPARING AUFNR MATNR CHARG CPUDT CPUTM.
*
*        SELECT FROM ZSVBMMMATDOCB
*        FIELDS BUDAT, CHARG, CPUDT, CPUTM, MJAHR, MBLNR, ZEILE, BUKRS, BUTXT, MATNR, MAKTX, BWART, BTEXT, MVTTYPE, SOBKZ, WERKS, WERKSNAME,
*               LGORT, LGORTNAME, UMWRK, UMWRKNAME, UMLGO, UMLGONAME, BWTAR, MENGE, ERFME, ERFMG, MEINS, LIFNR, LIFNRNAME, KUNNR, KUNNRNAME,
*               HSDAT, VFDAT, LICHN, ZMAKER, WATERRATIO, LBBSA_SID, KZBEW, BSTAUS_SG, BSTTYP_SG, BERID, WAERS, DMBTR,
*               EBELN, EBELP, AUFNR, AUFPS, RSNUM, RSPOS, VBELN_IM, VBELP_IM, SHKZG, ELIKZ, KOKRS, KZVBR, PRCTR, AUFPL, APLZL, SAKTO, TCODE2, USNAM
*        FOR ALL ENTRIES IN @LT_PRO_ETC
*          WHERE AUFNR = @LT_PRO_ETC-AUFNR
*          AND MATNR = @LT_PRO_ETC-MATNR
*          AND CHARG = @LT_PRO_ETC-CHARG
*          AND CPUDT = @LT_PRO_ETC-CPUDT
*          AND CPUTM <=  @LT_PRO_ETC-CPUTM
*          APPENDING CORRESPONDING FIELDS OF TABLE @IT_SUB.
*
*        FREE LT_PRO_ETC.
*      ENDIF.
*    ENDIF.

  IF it_sub_pro[] IS NOT INITIAL.

    LOOP AT it_sub_pro ASSIGNING FIELD-SYMBOL(<ls_sub_prot>) .
      IF <ls_sub_prot>-bwart =  gc_261.
        APPEND <ls_sub_prot> TO lt_sub_pro_261.
*      ELSEIF <LS_SUB_PROT>-BWART =  GC_262.         APPEND <LS_SUB_PROT> TO LT_SUB_PRO_262.
      ELSE.
        APPEND <ls_sub_prot> TO lt_sub_pro_etc.
      ENDIF.
    ENDLOOP.

** 261,262
*    IF LT_SUB_PRO_101[] IS NOT INITIAL.
*
*      DATA(LT_PRO_101) = LT_SUB_PRO_101[].
*      IF LT_PRO_101[] IS NOT INITIAL.
*
*        SORT LT_PRO_101 BY AUFNR MATNR CPUDT.         DELETE ADJACENT DUPLICATES FROM LT_PRO_101 COMPARING AUFNR MATNR CPUDT.
*
*        SELECT FROM ZSVBMMMATDOCB
*        FIELDS BUDAT, CHARG, CPUDT, CPUTM, MJAHR, MBLNR, ZEILE, BUKRS, BUTXT, MATNR, MAKTX, BWART, BTEXT, MVTTYPE, SOBKZ, WERKS, WERKSNAME,
*               LGORT, LGORTNAME, UMWRK, UMWRKNAME, UMLGO, UMLGONAME, BWTAR, MENGE, ERFME, ERFMG, MEINS, LIFNR, LIFNRNAME, KUNNR, KUNNRNAME,
*               HSDAT, VFDAT, LICHN, ZMAKER, WATERRATIO, LBBSA_SID, KZBEW, BSTAUS_SG, BSTTYP_SG, BERID, WAERS, DMBTR,
*               EBELN, EBELP, AUFNR, AUFPS, RSNUM, RSPOS, VBELN_IM, VBELP_IM, SHKZG, ELIKZ, KOKRS, KZVBR, PRCTR, AUFPL, APLZL, SAKTO, TCODE2, USNAM
*        FOR ALL ENTRIES IN @LT_PRO_101
*          WHERE AUFNR = @LT_PRO_101-AUFNR
*           AND  CPUDT < @LT_PRO_101-CPUDT
*
*          APPENDING CORRESPONDING FIELDS OF TABLE @IT_SUB.
*
*        FREE LT_PRO_101.
*
*        LT_PRO_101[] = LT_SUB_PRO_101[].
*        SORT LT_PRO_101 BY AUFNR MATNR CPUDT CPUTM.         DELETE ADJACENT DUPLICATES FROM LT_PRO_101 COMPARING AUFNR MATNR CPUDT CPUTM.
*
*        SELECT FROM ZSVBMMMATDOCB
*        FIELDS BUDAT, CHARG, CPUDT, CPUTM, MJAHR, MBLNR, ZEILE, BUKRS, BUTXT, MATNR, MAKTX, BWART, BTEXT, MVTTYPE, SOBKZ, WERKS, WERKSNAME,
*               LGORT, LGORTNAME, UMWRK, UMWRKNAME, UMLGO, UMLGONAME, BWTAR, MENGE, ERFME, ERFMG, MEINS, LIFNR, LIFNRNAME, KUNNR, KUNNRNAME,
*               HSDAT, VFDAT, LICHN, ZMAKER, WATERRATIO, LBBSA_SID, KZBEW, BSTAUS_SG, BSTTYP_SG, BERID, WAERS, DMBTR,
*               EBELN, EBELP, AUFNR, AUFPS, RSNUM, RSPOS, VBELN_IM, VBELP_IM, SHKZG, ELIKZ, KOKRS, KZVBR, PRCTR, AUFPL, APLZL, SAKTO, TCODE2, USNAM
*        FOR ALL ENTRIES IN @LT_PRO_101
*          WHERE AUFNR = @LT_PRO_101-AUFNR
*          AND  CPUDT = @LT_PRO_101-CPUDT
*          AND  CPUTM <=  @LT_PRO_101-CPUTM
*
*          APPENDING CORRESPONDING FIELDS OF TABLE @IT_SUB.
*
*        FREE LT_PRO_101.
*      ENDIF.
*    ENDIF.

*261 이하
    IF lt_sub_pro_261[] IS NOT INITIAL.

      DATA(lt_pro_261) = lt_sub_pro_261[].

      IF lt_pro_261[] IS NOT INITIAL.

        SORT lt_pro_261 BY matnr charg cpudt.         DELETE ADJACENT DUPLICATES FROM lt_pro_261 COMPARING matnr charg cpudt.

        SELECT FROM zsvbmmmatdocb
        FIELDS budat, charg, cpudt, cputm, mjahr, mblnr, zeile, bukrs, butxt, matnr, maktx, bwart, btext, mvttype, sobkz, werks, werksname,
               lgort, lgortname, umwrk, umwrkname, umlgo, umlgoname, bwtar, menge, erfme, erfmg, meins, lifnr, lifnrname, kunnr, kunnrname,
               hsdat, vfdat, lichn, zmaker, waterratio, lbbsa_sid, kzbew, bstaus_sg, bsttyp_sg, berid, waers, dmbtr,
               ebeln, ebelp, aufnr, aufps, rsnum, rspos, vbeln_im, vbelp_im, shkzg, elikz, kokrs, kzvbr, prctr, aufpl, aplzl, sakto, tcode2, usnam
        FOR ALL ENTRIES IN @lt_pro_261
          WHERE aufnr = ''
          AND matnr = @lt_pro_261-matnr
          AND charg = @lt_pro_261-charg
          AND cpudt < @lt_pro_261-cpudt
          APPENDING CORRESPONDING FIELDS OF TABLE @it_sub.

        FREE lt_pro_261.

        lt_pro_261[] = lt_sub_pro_261[].
        SORT lt_pro_261 BY matnr charg cpudt cputm.
        DELETE ADJACENT DUPLICATES FROM lt_pro_261 COMPARING matnr charg cpudt cputm.

        SELECT FROM zsvbmmmatdocb
        FIELDS budat, charg, cpudt, cputm, mjahr, mblnr, zeile, bukrs, butxt, matnr, maktx, bwart, btext, mvttype, sobkz, werks, werksname,
               lgort, lgortname, umwrk, umwrkname, umlgo, umlgoname, bwtar, menge, erfme, erfmg, meins, lifnr, lifnrname, kunnr, kunnrname,
               hsdat, vfdat, lichn, zmaker, waterratio, lbbsa_sid, kzbew, bstaus_sg, bsttyp_sg, berid, waers, dmbtr,
               ebeln, ebelp, aufnr, aufps, rsnum, rspos, vbeln_im, vbelp_im, shkzg, elikz, kokrs, kzvbr, prctr, aufpl, aplzl, sakto, tcode2, usnam
        FOR ALL ENTRIES IN @lt_pro_261
          WHERE aufnr = ''
          AND matnr = @lt_pro_261-matnr
          AND charg = @lt_pro_261-charg
          AND cpudt = @lt_pro_261-cpudt
          AND cputm <= @lt_pro_261-cputm
          APPENDING CORRESPONDING FIELDS OF TABLE @it_sub.

        FREE lt_pro_261.

        lt_pro_261[] = lt_sub_pro_261[].
        SORT lt_pro_261 BY aufnr cpudt.
        DELETE ADJACENT DUPLICATES FROM lt_pro_261 COMPARING aufnr cpudt.

        SELECT FROM zsvbmmmatdocb
        FIELDS budat, charg, cpudt, cputm, mjahr, mblnr, zeile, bukrs, butxt, matnr, maktx, bwart, btext, mvttype, sobkz, werks, werksname,
               lgort, lgortname, umwrk, umwrkname, umlgo, umlgoname, bwtar, menge, erfme, erfmg, meins, lifnr, lifnrname, kunnr, kunnrname,
               hsdat, vfdat, lichn, zmaker, waterratio, lbbsa_sid, kzbew, bstaus_sg, bsttyp_sg, berid, waers, dmbtr,
               ebeln, ebelp, aufnr, aufps, rsnum, rspos, vbeln_im, vbelp_im, shkzg, elikz, kokrs, kzvbr, prctr, aufpl, aplzl, sakto, tcode2, usnam
        FOR ALL ENTRIES IN @lt_pro_261
          WHERE aufnr = @lt_pro_261-aufnr
          AND cpudt < @lt_pro_261-cpudt
          AND bwart IN ( @gc_101 , @gc_102 )
          APPENDING CORRESPONDING FIELDS OF TABLE @it_sub.

        FREE lt_pro_261.

        lt_pro_261[] = lt_sub_pro_261[].
        SORT lt_pro_261 BY aufnr cpudt cputm.
        DELETE ADJACENT DUPLICATES FROM lt_pro_261 COMPARING aufnr cpudt cputm.

        SELECT FROM zsvbmmmatdocb
        FIELDS budat, charg, cpudt, cputm, mjahr, mblnr, zeile, bukrs, butxt, matnr, maktx, bwart, btext, mvttype, sobkz, werks, werksname,
               lgort, lgortname, umwrk, umwrkname, umlgo, umlgoname, bwtar, menge, erfme, erfmg, meins, lifnr, lifnrname, kunnr, kunnrname,
               hsdat, vfdat, lichn, zmaker, waterratio, lbbsa_sid, kzbew, bstaus_sg, bsttyp_sg, berid, waers, dmbtr,
               ebeln, ebelp, aufnr, aufps, rsnum, rspos, vbeln_im, vbelp_im, shkzg, elikz, kokrs, kzvbr, prctr, aufpl, aplzl, sakto, tcode2, usnam
        FOR ALL ENTRIES IN @lt_pro_261
          WHERE aufnr = @lt_pro_261-aufnr
          AND cpudt = @lt_pro_261-cpudt
          AND cputm <= @lt_pro_261-cputm
          AND bwart IN ( @gc_101 , @gc_102 )
          APPENDING CORRESPONDING FIELDS OF TABLE @it_sub.

        FREE lt_pro_261.
      ENDIF.
    ENDIF.

**기타
    IF lt_sub_pro_etc[] IS NOT INITIAL.

      DATA(lt_pro_etc) = lt_sub_pro_etc[].

      IF lt_pro_etc[] IS NOT INITIAL.

        SORT lt_pro_etc BY aufnr cpudt.         DELETE ADJACENT DUPLICATES FROM lt_pro_etc COMPARING aufnr cpudt.

        SELECT FROM zsvbmmmatdocb
        FIELDS budat, charg, cpudt, cputm, mjahr, mblnr, zeile, bukrs, butxt, matnr, maktx, bwart, btext, mvttype, sobkz, werks, werksname,
               lgort, lgortname, umwrk, umwrkname, umlgo, umlgoname, bwtar, menge, erfme, erfmg, meins, lifnr, lifnrname, kunnr, kunnrname,
               hsdat, vfdat, lichn, zmaker, waterratio, lbbsa_sid, kzbew, bstaus_sg, bsttyp_sg, berid, waers, dmbtr,
               ebeln, ebelp, aufnr, aufps, rsnum, rspos, vbeln_im, vbelp_im, shkzg, elikz, kokrs, kzvbr, prctr, aufpl, aplzl, sakto, tcode2, usnam
        FOR ALL ENTRIES IN @lt_pro_etc
          WHERE aufnr = @lt_pro_etc-aufnr
*          AND MATNR = @LT_PRO_ETC-MATNR
*          AND CHARG = @LT_PRO_ETC-CHARG
          AND  cpudt < @lt_pro_etc-cpudt
          APPENDING CORRESPONDING FIELDS OF TABLE @it_sub.

        FREE lt_pro_etc.

        lt_pro_etc[] = lt_sub_pro_etc[].
        SORT lt_pro_etc BY aufnr cpudt cputm.         DELETE ADJACENT DUPLICATES FROM lt_pro_etc COMPARING aufnr cpudt cputm.

        SELECT FROM zsvbmmmatdocb
        FIELDS budat, charg, cpudt, cputm, mjahr, mblnr, zeile, bukrs, butxt, matnr, maktx, bwart, btext, mvttype, sobkz, werks, werksname,
               lgort, lgortname, umwrk, umwrkname, umlgo, umlgoname, bwtar, menge, erfme, erfmg, meins, lifnr, lifnrname, kunnr, kunnrname,
               hsdat, vfdat, lichn, zmaker, waterratio, lbbsa_sid, kzbew, bstaus_sg, bsttyp_sg, berid, waers, dmbtr,
               ebeln, ebelp, aufnr, aufps, rsnum, rspos, vbeln_im, vbelp_im, shkzg, elikz, kokrs, kzvbr, prctr, aufpl, aplzl, sakto, tcode2, usnam
        FOR ALL ENTRIES IN @lt_pro_etc
          WHERE aufnr = @lt_pro_etc-aufnr
*          AND MATNR = @LT_PRO_ETC-MATNR
*          AND CHARG = @LT_PRO_ETC-CHARG
          AND cpudt = @lt_pro_etc-cpudt
          AND cputm <=  @lt_pro_etc-cputm
          APPENDING CORRESPONDING FIELDS OF TABLE @it_sub.

        FREE lt_pro_etc.
      ENDIF.
    ENDIF.


    IF it_sub[] IS NOT INITIAL.

      LOOP AT it_sub INTO DATA(ls_sub).

        CLEAR : ls_disp_item.

        READ TABLE gt_disp_item TRANSPORTING NO FIELDS WITH KEY mjahr = ls_sub-mjahr
                                                                mblnr = ls_sub-mblnr
                                                                zeile = ls_sub-zeile.
        IF sy-subrc = 0.
          DELETE TABLE it_sub FROM ls_sub.
        ELSE.
          MOVE-CORRESPONDING ls_sub TO ls_disp_item.
          APPEND ls_disp_item TO gt_disp_item. CLEAR ls_disp_item.
        ENDIF.

      ENDLOOP.
    ENDIF.
  ENDIF.
  _g_init lt_sub_pro_101.   _g_init lt_sub_pro_102.   _g_init lt_sub_pro_261.   _g_init lt_sub_pro_262.   _g_init lt_sub_pro_etc.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form TOP_DOWN_PUR
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LT_SUB_PUR
*&      --> LT_SUB
*&---------------------------------------------------------------------*
FORM top_down_pur TABLES it_sub_pur TYPE tt_item
                          it_sub     TYPE tt_item.


  DATA : ls_disp_item TYPE ts_disp.
  DATA : lt_sub_pur_101 TYPE TABLE OF ts_disp,
         lt_sub_pur_102 TYPE TABLE OF ts_disp,
         lt_sub_pur_161 TYPE TABLE OF ts_disp,
         lt_sub_pur_162 TYPE TABLE OF ts_disp,
         lt_sub_pur_543 TYPE TABLE OF ts_disp,
         lt_sub_pur_etc TYPE TABLE OF ts_disp.    "기타집계용

  IF it_sub_pur[] IS NOT INITIAL.

    SORT it_sub_pur BY matnr mjahr mblnr zeile ebeln ebelp budat.
    DELETE ADJACENT DUPLICATES FROM it_sub_pur COMPARING matnr mjahr mblnr zeile ebeln ebelp budat..

    LOOP AT it_sub_pur ASSIGNING FIELD-SYMBOL(<ls_sub_purt>) .
      IF <ls_sub_purt>-bwart =  gc_101.
        APPEND <ls_sub_purt> TO lt_sub_pur_101.
      ELSEIF <ls_sub_purt>-bwart =  gc_102.
        APPEND <ls_sub_purt> TO lt_sub_pur_102.
      ELSEIF  <ls_sub_purt>-bwart =  gc_161.
        APPEND <ls_sub_purt> TO lt_sub_pur_161.
      ELSEIF  <ls_sub_purt>-bwart =  gc_162.
        APPEND <ls_sub_purt> TO lt_sub_pur_162.
      ELSEIF  <ls_sub_purt>-bwart =  gc_543 OR <ls_sub_purt>-bwart =  gc_544.
        APPEND <ls_sub_purt> TO lt_sub_pur_543.
      ELSE.
        APPEND <ls_sub_purt> TO lt_sub_pur_etc.
      ENDIF.
    ENDLOOP.


    IF lt_sub_pur_162[] IS NOT INITIAL.

      DATA(lt_pur_162) = lt_sub_pur_162[].
      IF lt_pur_162[] IS NOT INITIAL.

        SORT lt_pur_162 BY ebeln ebelp cpudt.
        DELETE ADJACENT DUPLICATES FROM lt_pur_162 COMPARING ebeln ebelp cpudt.

        SELECT FROM zsvbmmmatdocb
           FIELDS budat, charg, cpudt, cputm, mjahr, mblnr, zeile, bukrs, butxt, matnr, maktx, bwart, btext, mvttype, sobkz, werks, werksname,
                  lgort, lgortname, umwrk, umwrkname, umlgo, umlgoname, bwtar, menge, erfme, erfmg, meins, lifnr, lifnrname, kunnr, kunnrname,
                  hsdat, vfdat, lichn, zmaker, waterratio, lbbsa_sid, kzbew, bstaus_sg, bsttyp_sg, berid, waers, dmbtr,
                  ebeln, ebelp, aufnr, aufps, rsnum, rspos, vbeln_im, vbelp_im, shkzg, elikz, kokrs, kzvbr, prctr, aufpl, aplzl, sakto, tcode2, usnam
         FOR ALL ENTRIES IN @lt_pur_162
           WHERE ebeln = @lt_pur_162-ebeln
             AND ebelp = @lt_pur_162-ebelp
             AND cpudt < @lt_pur_162-cpudt
             AND bwart = @gc_161
           APPENDING CORRESPONDING FIELDS OF TABLE @it_sub.

        FREE lt_pur_162.

        lt_pur_162[] = lt_sub_pur_162[].
        SORT lt_pur_162 BY ebeln ebelp cpudt cputm.
        DELETE ADJACENT DUPLICATES FROM lt_pur_162 COMPARING ebeln ebelp cpudt cputm.

        SELECT FROM zsvbmmmatdocb
          FIELDS budat, charg, cpudt, cputm, mjahr, mblnr, zeile, bukrs, butxt, matnr, maktx, bwart, btext, mvttype, sobkz, werks, werksname,
                 lgort, lgortname, umwrk, umwrkname, umlgo, umlgoname, bwtar, menge, erfme, erfmg, meins, lifnr, lifnrname, kunnr, kunnrname,
                 hsdat, vfdat, lichn, zmaker, waterratio, lbbsa_sid, kzbew, bstaus_sg, bsttyp_sg, berid, waers, dmbtr,
                 ebeln, ebelp, aufnr, aufps, rsnum, rspos, vbeln_im, vbelp_im, shkzg, elikz, kokrs, kzvbr, prctr, aufpl, aplzl, sakto, tcode2, usnam
        FOR ALL ENTRIES IN @lt_pur_162
          WHERE ebeln = @lt_pur_162-ebeln
            AND ebelp = @lt_pur_162-ebelp
            AND cpudt = @lt_pur_162-cpudt
            AND cputm <= @lt_pur_162-cputm
            AND bwart =  @gc_161
          APPENDING CORRESPONDING FIELDS OF TABLE @it_sub.
        FREE lt_pur_162.
      ENDIF.
    ENDIF.

    IF lt_sub_pur_161[] IS NOT INITIAL.

      DATA(lt_pur_161) = lt_sub_pur_161[].
      IF lt_pur_161[] IS NOT INITIAL.

        SORT lt_pur_161 BY matnr charg cpudt.
        DELETE ADJACENT DUPLICATES FROM lt_pur_161 COMPARING matnr charg cpudt.

        SELECT FROM zsvbmmmatdocb
           FIELDS budat, charg, cpudt, cputm, mjahr, mblnr, zeile, bukrs, butxt, matnr, maktx, bwart, btext, mvttype, sobkz, werks, werksname,
                  lgort, lgortname, umwrk, umwrkname, umlgo, umlgoname, bwtar, menge, erfme, erfmg, meins, lifnr, lifnrname, kunnr, kunnrname,
                  hsdat, vfdat, lichn, zmaker, waterratio, lbbsa_sid, kzbew, bstaus_sg, bsttyp_sg, berid, waers, dmbtr,
                  ebeln, ebelp, aufnr, aufps, rsnum, rspos, vbeln_im, vbelp_im, shkzg, elikz, kokrs, kzvbr, prctr, aufpl, aplzl, sakto, tcode2, usnam
         FOR ALL ENTRIES IN @lt_pur_161
           WHERE matnr = @lt_pur_161-matnr
             AND charg = @lt_pur_161-charg
             AND cpudt < @lt_pur_161-cpudt
             AND bwart IN ( @gc_101 , @gc_102 )
           APPENDING CORRESPONDING FIELDS OF TABLE @it_sub.

        FREE lt_pur_161.

        lt_pur_161[] = lt_sub_pur_161[].
        SORT lt_pur_161 BY matnr charg cpudt cputm.
        DELETE ADJACENT DUPLICATES FROM lt_pur_161 COMPARING matnr charg cpudt cputm.

        SELECT FROM zsvbmmmatdocb
          FIELDS budat, charg, cpudt, cputm, mjahr, mblnr, zeile, bukrs, butxt, matnr, maktx, bwart, btext, mvttype, sobkz, werks, werksname,
                 lgort, lgortname, umwrk, umwrkname, umlgo, umlgoname, bwtar, menge, erfme, erfmg, meins, lifnr, lifnrname, kunnr, kunnrname,
                 hsdat, vfdat, lichn, zmaker, waterratio, lbbsa_sid, kzbew, bstaus_sg, bsttyp_sg, berid, waers, dmbtr,
                 ebeln, ebelp, aufnr, aufps, rsnum, rspos, vbeln_im, vbelp_im, shkzg, elikz, kokrs, kzvbr, prctr, aufpl, aplzl, sakto, tcode2, usnam
        FOR ALL ENTRIES IN @lt_pur_161
          WHERE matnr = @lt_pur_161-matnr
            AND charg = @lt_pur_161-charg
            AND cpudt = @lt_pur_161-cpudt
            AND cputm <= @lt_pur_161-cputm
            AND bwart IN ( @gc_101 , @gc_102 )
          APPENDING CORRESPONDING FIELDS OF TABLE @it_sub.

        FREE lt_pur_161.
      ENDIF.
    ENDIF.

    IF lt_sub_pur_102[] IS NOT INITIAL.

      DATA(lt_pur_102) = lt_sub_pur_102[].
      IF lt_pur_102[] IS NOT INITIAL.

        SORT lt_pur_102 BY ebeln ebelp cpudt.
        DELETE ADJACENT DUPLICATES FROM lt_pur_102 COMPARING ebeln ebelp cpudt.

        SELECT FROM zsvbmmmatdocb
           FIELDS budat, charg, cpudt, cputm, mjahr, mblnr, zeile, bukrs, butxt, matnr, maktx, bwart, btext, mvttype, sobkz, werks, werksname,
                  lgort, lgortname, umwrk, umwrkname, umlgo, umlgoname, bwtar, menge, erfme, erfmg, meins, lifnr, lifnrname, kunnr, kunnrname,
                  hsdat, vfdat, lichn, zmaker, waterratio, lbbsa_sid, kzbew, bstaus_sg, bsttyp_sg, berid, waers, dmbtr,
                  ebeln, ebelp, aufnr, aufps, rsnum, rspos, vbeln_im, vbelp_im, shkzg, elikz, kokrs, kzvbr, prctr, aufpl, aplzl, sakto, tcode2, usnam
         FOR ALL ENTRIES IN @lt_pur_102
           WHERE ebeln = @lt_pur_102-ebeln
             AND ebelp = @lt_pur_102-ebelp
             AND cpudt < @lt_pur_102-cpudt
             AND bwart = @gc_101
           APPENDING CORRESPONDING FIELDS OF TABLE @it_sub.

        FREE lt_pur_102.

        lt_pur_102[] = lt_sub_pur_102[].
        SORT lt_pur_102 BY ebeln ebelp cpudt cputm.
        DELETE ADJACENT DUPLICATES FROM lt_pur_102 COMPARING ebeln ebelp cpudt cputm.

        SELECT FROM zsvbmmmatdocb
          FIELDS budat, charg, cpudt, cputm, mjahr, mblnr, zeile, bukrs, butxt, matnr, maktx, bwart, btext, mvttype, sobkz, werks, werksname,
                 lgort, lgortname, umwrk, umwrkname, umlgo, umlgoname, bwtar, menge, erfme, erfmg, meins, lifnr, lifnrname, kunnr, kunnrname,
                 hsdat, vfdat, lichn, zmaker, waterratio, lbbsa_sid, kzbew, bstaus_sg, bsttyp_sg, berid, waers, dmbtr,
                 ebeln, ebelp, aufnr, aufps, rsnum, rspos, vbeln_im, vbelp_im, shkzg, elikz, kokrs, kzvbr, prctr, aufpl, aplzl, sakto, tcode2, usnam
        FOR ALL ENTRIES IN @lt_pur_102
          WHERE ebeln = @lt_pur_102-ebeln
            AND ebelp = @lt_pur_102-ebelp
            AND cpudt = @lt_pur_102-cpudt
            AND cputm <= @lt_pur_102-cputm
            AND bwart = @gc_101
          APPENDING CORRESPONDING FIELDS OF TABLE @it_sub.
        FREE lt_pur_102.
      ENDIF.
    ENDIF.

    IF lt_sub_pur_101[] IS NOT INITIAL.


      PERFORM top_down_pur_101 TABLES lt_sub_pur_101 it_sub.

*      DATA(LT_PUR_101) = LT_SUB_PUR_101[].
*      IF LT_PUR_101[] IS NOT INITIAL.
*
*        SORT LT_PUR_101 BY EBELN EBELP CPUDT.
*        DELETE ADJACENT DUPLICATES FROM LT_PUR_101 COMPARING EBELN EBELP CPUDT.
*
*        SELECT FROM ZSVBMMMATDOCB
*           FIELDS BUDAT, CHARG, CPUDT, CPUTM, MJAHR, MBLNR, ZEILE, BUKRS, BUTXT, MATNR, MAKTX, BWART, BTEXT, MVTTYPE, SOBKZ, WERKS, WERKSNAME,
*                  LGORT, LGORTNAME, UMWRK, UMWRKNAME, UMLGO, UMLGONAME, BWTAR, MENGE, ERFME, ERFMG, MEINS, LIFNR, LIFNRNAME, KUNNR, KUNNRNAME,
*                  HSDAT, VFDAT, LICHN, ZMAKER, WATERRATIO, LBBSA_SID, KZBEW, BSTAUS_SG, BSTTYP_SG, BERID, WAERS, DMBTR,
*                  EBELN, EBELP, AUFNR, AUFPS, RSNUM, RSPOS, VBELN_IM, VBELP_IM, SHKZG, ELIKZ, KOKRS, KZVBR, PRCTR, AUFPL, APLZL, SAKTO, TCODE2, USNAM
*         FOR ALL ENTRIES IN @LT_PUR_101
*           WHERE EBELN = @LT_PUR_101-EBELN
*             AND EBELP = @LT_PUR_101-EBELP
*             AND CPUDT < @LT_PUR_101-CPUDT
*             AND BWART IN (  @GC_102 ,  @GC_543, @GC_544 )
*           APPENDING CORRESPONDING FIELDS OF TABLE @IT_SUB.
*
*        FREE LT_PUR_101.
*
*        LT_PUR_101[] = LT_SUB_PUR_101[].
*        SORT LT_PUR_101 BY EBELN EBELP CPUDT CPUTM.
*        DELETE ADJACENT DUPLICATES FROM LT_PUR_101 COMPARING EBELN EBELP CPUDT CPUTM.
*
*        SELECT FROM ZSVBMMMATDOCB
*          FIELDS BUDAT, CHARG, CPUDT, CPUTM, MJAHR, MBLNR, ZEILE, BUKRS, BUTXT, MATNR, MAKTX, BWART, BTEXT, MVTTYPE, SOBKZ, WERKS, WERKSNAME,
*                 LGORT, LGORTNAME, UMWRK, UMWRKNAME, UMLGO, UMLGONAME, BWTAR, MENGE, ERFME, ERFMG, MEINS, LIFNR, LIFNRNAME, KUNNR, KUNNRNAME,
*                 HSDAT, VFDAT, LICHN, ZMAKER, WATERRATIO, LBBSA_SID, KZBEW, BSTAUS_SG, BSTTYP_SG, BERID, WAERS, DMBTR,
*                 EBELN, EBELP, AUFNR, AUFPS, RSNUM, RSPOS, VBELN_IM, VBELP_IM, SHKZG, ELIKZ, KOKRS, KZVBR, PRCTR, AUFPL, APLZL, SAKTO, TCODE2, USNAM
*        FOR ALL ENTRIES IN @LT_PUR_101
*          WHERE EBELN = @LT_PUR_101-EBELN
*            AND EBELP = @LT_PUR_101-EBELP
*            AND CPUDT = @LT_PUR_101-CPUDT
*            AND CPUTM <= @LT_PUR_101-CPUTM
*            AND BWART IN (  @GC_102 ,  @GC_543, @GC_544 )
*          APPENDING CORRESPONDING FIELDS OF TABLE @IT_SUB.
*        FREE LT_PUR_101.
*      ENDIF.
    ENDIF.


    IF lt_sub_pur_543[] IS NOT INITIAL.

      PERFORM top_down_pur_543 TABLES lt_sub_pur_543 it_sub.

*      DATA(LT_PUR_543) = LT_SUB_PUR_543[].
*      IF LT_PUR_543[] IS NOT INITIAL.
*
*        SORT LT_PUR_543 BY EBELN EBELP MATNR CHARG CPUDT.
*        DELETE ADJACENT DUPLICATES FROM LT_PUR_543 COMPARING EBELN EBELP MATNR CHARG CPUDT.
*
*        SELECT FROM ZSVBMMMATDOCB
*          FIELDS BUDAT, CHARG, CPUDT, CPUTM, MJAHR, MBLNR, ZEILE, BUKRS, BUTXT, MATNR, MAKTX, BWART, BTEXT, MVTTYPE, SOBKZ, WERKS, WERKSNAME,
*                 LGORT, LGORTNAME, UMWRK, UMWRKNAME, UMLGO, UMLGONAME, BWTAR, MENGE, ERFME, ERFMG, MEINS, LIFNR, LIFNRNAME, KUNNR, KUNNRNAME,
*                 HSDAT, VFDAT, LICHN, ZMAKER, WATERRATIO, LBBSA_SID, KZBEW, BSTAUS_SG, BSTTYP_SG, BERID, WAERS, DMBTR,
*                 EBELN, EBELP, AUFNR, AUFPS, RSNUM, RSPOS, VBELN_IM, VBELP_IM, SHKZG, ELIKZ, KOKRS, KZVBR, PRCTR, AUFPL, APLZL, SAKTO, TCODE2, USNAM
*        FOR ALL ENTRIES IN @LT_PUR_543
*          WHERE EBELN = @LT_PUR_543-EBELN
*            AND EBELP = @LT_PUR_543-EBELP
*            AND MATNR = @LT_PUR_543-MATNR
*            AND CHARG = @LT_PUR_543-CHARG
*            AND CPUDT < @LT_PUR_543-CPUDT
*            AND BWART IN ( @GC_541 ,  @GC_542 )
*          APPENDING CORRESPONDING FIELDS OF TABLE @IT_SUB.
*
*        FREE LT_PUR_543.
*
*        LT_PUR_543[] = LT_SUB_PUR_543[].
*        SORT LT_PUR_543 BY EBELN EBELP MATNR CHARG CPUDT CPUTM.
*        DELETE ADJACENT DUPLICATES FROM LT_PUR_543 COMPARING EBELN EBELP MATNR CHARG CPUDT CPUTM.
*
*        SELECT FROM ZSVBMMMATDOCB
*          FIELDS BUDAT, CHARG, CPUDT, CPUTM, MJAHR, MBLNR, ZEILE, BUKRS, BUTXT, MATNR, MAKTX, BWART, BTEXT, MVTTYPE, SOBKZ, WERKS, WERKSNAME,
*                 LGORT, LGORTNAME, UMWRK, UMWRKNAME, UMLGO, UMLGONAME, BWTAR, MENGE, ERFME, ERFMG, MEINS, LIFNR, LIFNRNAME, KUNNR, KUNNRNAME,
*                 HSDAT, VFDAT, LICHN, ZMAKER, WATERRATIO, LBBSA_SID, KZBEW, BSTAUS_SG, BSTTYP_SG, BERID, WAERS, DMBTR,
*                 EBELN, EBELP, AUFNR, AUFPS, RSNUM, RSPOS, VBELN_IM, VBELP_IM, SHKZG, ELIKZ, KOKRS, KZVBR, PRCTR, AUFPL, APLZL, SAKTO, TCODE2, USNAM
*        FOR ALL ENTRIES IN @LT_PUR_543
*          WHERE EBELN = @LT_PUR_543-EBELN
*            AND EBELP = @LT_PUR_543-EBELP
*            AND MATNR = @LT_PUR_543-MATNR
*            AND CHARG = @LT_PUR_543-CHARG
*            AND CPUDT = @LT_PUR_543-CPUDT
*            AND CPUTM <= @LT_PUR_543-CPUTM
*            AND BWART IN ( @GC_541 ,  @GC_542 )
*          APPENDING CORRESPONDING FIELDS OF TABLE @IT_SUB.
*        FREE LT_PUR_543.
*      ENDIF.
    ENDIF.

    IF lt_sub_pur_etc[] IS NOT INITIAL.

      PERFORM top_down_pur_etc TABLES lt_sub_pur_etc it_sub.
*
*      DATA(LT_PUR_ETC) = LT_SUB_PUR_ETC[].
*      IF LT_PUR_ETC[] IS NOT INITIAL.
*
*        SORT LT_PUR_ETC BY MATNR CHARG CPUDT.
*        DELETE ADJACENT DUPLICATES FROM LT_PUR_ETC COMPARING MATNR CHARG CPUDT.
*
*        SELECT FROM ZSVBMMMATDOCB
*          FIELDS BUDAT, CHARG, CPUDT, CPUTM, MJAHR, MBLNR, ZEILE, BUKRS, BUTXT, MATNR, MAKTX, BWART, BTEXT, MVTTYPE, SOBKZ, WERKS, WERKSNAME,
*                 LGORT, LGORTNAME, UMWRK, UMWRKNAME, UMLGO, UMLGONAME, BWTAR, MENGE, ERFME, ERFMG, MEINS, LIFNR, LIFNRNAME, KUNNR, KUNNRNAME,
*                 HSDAT, VFDAT, LICHN, ZMAKER, WATERRATIO, LBBSA_SID, KZBEW, BSTAUS_SG, BSTTYP_SG, BERID, WAERS, DMBTR,
*                 EBELN, EBELP, AUFNR, AUFPS, RSNUM, RSPOS, VBELN_IM, VBELP_IM, SHKZG, ELIKZ, KOKRS, KZVBR, PRCTR, AUFPL, APLZL, SAKTO, TCODE2, USNAM
*        FOR ALL ENTRIES IN @LT_PUR_ETC
*          WHERE  MATNR = @LT_PUR_ETC-MATNR
*            AND CHARG = @LT_PUR_ETC-CHARG
*            AND CPUDT < @LT_PUR_ETC-CPUDT
*            AND BWART NOT IN ( @GC_541 ,  @GC_542,  @GC_543,  @GC_544 )
*          APPENDING CORRESPONDING FIELDS OF TABLE @IT_SUB.
*
*        FREE LT_PUR_ETC.
*
*        LT_PUR_ETC[] = LT_SUB_PUR_ETC[].
*        SORT LT_PUR_ETC BY MATNR CHARG CPUDT CPUTM.
*        DELETE ADJACENT DUPLICATES FROM LT_PUR_ETC COMPARING MATNR CHARG CPUDT CPUTM.
*
*        SELECT FROM ZSVBMMMATDOCB
*          FIELDS BUDAT, CHARG, CPUDT, CPUTM, MJAHR, MBLNR, ZEILE, BUKRS, BUTXT, MATNR, MAKTX, BWART, BTEXT, MVTTYPE, SOBKZ, WERKS, WERKSNAME,
*                 LGORT, LGORTNAME, UMWRK, UMWRKNAME, UMLGO, UMLGONAME, BWTAR, MENGE, ERFME, ERFMG, MEINS, LIFNR, LIFNRNAME, KUNNR, KUNNRNAME,
*                 HSDAT, VFDAT, LICHN, ZMAKER, WATERRATIO, LBBSA_SID, KZBEW, BSTAUS_SG, BSTTYP_SG, BERID, WAERS, DMBTR,
*                 EBELN, EBELP, AUFNR, AUFPS, RSNUM, RSPOS, VBELN_IM, VBELP_IM, SHKZG, ELIKZ, KOKRS, KZVBR, PRCTR, AUFPL, APLZL, SAKTO, TCODE2, USNAM
*        FOR ALL ENTRIES IN @LT_PUR_ETC
*          WHERE MATNR = @LT_PUR_ETC-MATNR
*            AND CHARG = @LT_PUR_ETC-CHARG
*            AND CPUDT = @LT_PUR_ETC-CPUDT
*            AND CPUTM <= @LT_PUR_ETC-CPUTM
*            AND BWART NOT IN ( @GC_541 ,  @GC_542,  @GC_543,  @GC_544 )
*          APPENDING CORRESPONDING FIELDS OF TABLE @IT_SUB.
*        FREE LT_PUR_ETC.
*      ENDIF.
    ENDIF.


*    SELECT FROM ZSVBMMMATDOCB
*      FIELDS BUDAT, CHARG, CPUDT, CPUTM, MJAHR, MBLNR, ZEILE, BUKRS, BUTXT, MATNR, MAKTX, BWART, BTEXT, MVTTYPE, SOBKZ, WERKS, WERKSNAME,
*             LGORT, LGORTNAME, UMWRK, UMWRKNAME, UMLGO, UMLGONAME, BWTAR, MENGE, ERFME, ERFMG, MEINS, LIFNR, LIFNRNAME, KUNNR, KUNNRNAME,
*             HSDAT, VFDAT, LICHN, ZMAKER, WATERRATIO, LBBSA_SID, KZBEW, BSTAUS_SG, BSTTYP_SG, BERID, WAERS, DMBTR,
*             EBELN, EBELP, AUFNR, AUFPS, RSNUM, RSPOS, VBELN_IM, VBELP_IM, SHKZG, ELIKZ, KOKRS, KZVBR, PRCTR, AUFPL, APLZL, SAKTO, TCODE2, USNAM
*    FOR ALL ENTRIES IN @IT_SUB_PUR
*      WHERE EBELN = @IT_SUB_PUR-EBELN
*        AND EBELP = @IT_SUB_PUR-EBELP
*        AND CPUDT < @IT_SUB_PUR-CPUDT
*      APPENDING CORRESPONDING FIELDS OF TABLE @IT_SUB.
*
*    SELECT FROM ZSVBMMMATDOCB
*      FIELDS BUDAT, CHARG, CPUDT, CPUTM, MJAHR, MBLNR, ZEILE, BUKRS, BUTXT, MATNR, MAKTX, BWART, BTEXT, MVTTYPE, SOBKZ, WERKS, WERKSNAME,
*             LGORT, LGORTNAME, UMWRK, UMWRKNAME, UMLGO, UMLGONAME, BWTAR, MENGE, ERFME, ERFMG, MEINS, LIFNR, LIFNRNAME, KUNNR, KUNNRNAME,
*             HSDAT, VFDAT, LICHN, ZMAKER, WATERRATIO, LBBSA_SID, KZBEW, BSTAUS_SG, BSTTYP_SG, BERID, WAERS, DMBTR,
*             EBELN, EBELP, AUFNR, AUFPS, RSNUM, RSPOS, VBELN_IM, VBELP_IM, SHKZG, ELIKZ, KOKRS, KZVBR, PRCTR, AUFPL, APLZL, SAKTO, TCODE2, USNAM
*    FOR ALL ENTRIES IN @IT_SUB_PUR
*      WHERE EBELN = @IT_SUB_PUR-EBELN
*        AND EBELP = @IT_SUB_PUR-EBELP
*        AND CPUDT = @IT_SUB_PUR-CPUDT
*        AND CPUTM <= @IT_SUB_PUR-CPUTM
*      APPENDING CORRESPONDING FIELDS OF TABLE @IT_SUB.
*
*    SELECT FROM ZSVBMMMATDOCB
*      FIELDS BUDAT, CHARG, CPUDT, CPUTM, MJAHR, MBLNR, ZEILE, BUKRS, BUTXT, MATNR, MAKTX, BWART, BTEXT, MVTTYPE, SOBKZ, WERKS, WERKSNAME,
*             LGORT, LGORTNAME, UMWRK, UMWRKNAME, UMLGO, UMLGONAME, BWTAR, MENGE, ERFME, ERFMG, MEINS, LIFNR, LIFNRNAME, KUNNR, KUNNRNAME,
*             HSDAT, VFDAT, LICHN, ZMAKER, WATERRATIO, LBBSA_SID, KZBEW, BSTAUS_SG, BSTTYP_SG, BERID, WAERS, DMBTR,
*             EBELN, EBELP, AUFNR, AUFPS, RSNUM, RSPOS, VBELN_IM, VBELP_IM, SHKZG, ELIKZ, KOKRS, KZVBR, PRCTR, AUFPL, APLZL, SAKTO, TCODE2, USNAM
*
*    FOR ALL ENTRIES IN @IT_SUB_PUR
*      WHERE EBELN = ''
*        AND MATNR = @IT_SUB_PUR-MATNR
*        AND CHARG = @IT_SUB_PUR-CHARG
*        AND CPUDT < @IT_SUB_PUR-CPUDT
*      APPENDING CORRESPONDING FIELDS OF TABLE @IT_SUB.
*
*    SELECT FROM ZSVBMMMATDOCB
*      FIELDS BUDAT, CHARG, CPUDT, CPUTM, MJAHR, MBLNR, ZEILE, BUKRS, BUTXT, MATNR, MAKTX, BWART, BTEXT, MVTTYPE, SOBKZ, WERKS, WERKSNAME,
*             LGORT, LGORTNAME, UMWRK, UMWRKNAME, UMLGO, UMLGONAME, BWTAR, MENGE, ERFME, ERFMG, MEINS, LIFNR, LIFNRNAME, KUNNR, KUNNRNAME,
*             HSDAT, VFDAT, LICHN, ZMAKER, WATERRATIO, LBBSA_SID, KZBEW, BSTAUS_SG, BSTTYP_SG, BERID, WAERS, DMBTR,
*             EBELN, EBELP, AUFNR, AUFPS, RSNUM, RSPOS, VBELN_IM, VBELP_IM, SHKZG, ELIKZ, KOKRS, KZVBR, PRCTR, AUFPL, APLZL, SAKTO, TCODE2, USNAM
*
*    FOR ALL ENTRIES IN @IT_SUB_PUR
*      WHERE EBELN = ''
*        AND MATNR = @IT_SUB_PUR-MATNR
*        AND CHARG = @IT_SUB_PUR-CHARG
*        AND CPUDT = @IT_SUB_PUR-CPUDT
*        AND CPUTM <= @IT_SUB_PUR-CPUTM
*      APPENDING CORRESPONDING FIELDS OF TABLE @IT_SUB.


    IF it_sub[] IS NOT INITIAL.

      LOOP AT it_sub INTO DATA(ls_sub).

        CLEAR : ls_disp_item.

        READ TABLE gt_disp_item TRANSPORTING NO FIELDS WITH KEY mjahr = ls_sub-mjahr
                                                                mblnr = ls_sub-mblnr
                                                                zeile = ls_sub-zeile.

        IF sy-subrc = 0.
          DELETE TABLE it_sub FROM ls_sub.
        ELSE.
          MOVE-CORRESPONDING ls_sub TO ls_disp_item.
          APPEND ls_disp_item TO gt_disp_item. CLEAR ls_disp_item.
        ENDIF.

      ENDLOOP.
    ENDIF.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form TOP_DOWN_STO
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LT_SUB_STO
*&      --> LT_SUB
*&---------------------------------------------------------------------*
FORM top_down_sto TABLES it_sub_sto TYPE tt_item
                          it_sub TYPE tt_item.


  DATA : ls_disp_item TYPE ts_disp.

  IF it_sub_sto[] IS NOT INITIAL.
    SORT it_sub_sto BY matnr mjahr mblnr zeile charg budat.
    DELETE ADJACENT DUPLICATES FROM it_sub_sto COMPARING matnr mjahr mblnr zeile charg budat.

    DATA(lt_sub_sto) = it_sub_sto[].
    IF lt_sub_sto[] IS NOT INITIAL.

      SORT lt_sub_sto BY matnr charg cpudt.
      DELETE ADJACENT DUPLICATES FROM lt_sub_sto COMPARING matnr charg cpudt.

      SELECT FROM zsvbmmmatdocb
        FIELDS budat, charg, cpudt, cputm, mjahr, mblnr, zeile, bukrs, butxt, matnr, maktx, bwart, btext, mvttype, sobkz, werks, werksname,
               lgort, lgortname, umwrk, umwrkname, umlgo, umlgoname, bwtar, menge, erfme, erfmg, meins, lifnr, lifnrname, kunnr, kunnrname,
               hsdat, vfdat, lichn, zmaker, waterratio, lbbsa_sid, kzbew, bstaus_sg, bsttyp_sg, berid, waers, dmbtr,
               ebeln, ebelp, aufnr, aufps, rsnum, rspos, vbeln_im, vbelp_im, shkzg, elikz, kokrs, kzvbr, prctr, aufpl, aplzl, sakto, tcode2, usnam

      FOR ALL ENTRIES IN @lt_sub_sto
        WHERE matnr = @lt_sub_sto-matnr
          AND charg = @lt_sub_sto-charg
          AND cpudt < @lt_sub_sto-cpudt
        APPENDING CORRESPONDING FIELDS OF TABLE @it_sub.

      FREE lt_sub_sto.

      lt_sub_sto[] = it_sub_sto[].
      SORT lt_sub_sto BY matnr charg cpudt cputm.
      DELETE ADJACENT DUPLICATES FROM lt_sub_sto COMPARING matnr charg cpudt cputm.

      SELECT FROM zsvbmmmatdocb
        FIELDS budat, charg, cpudt, cputm, mjahr, mblnr, zeile, bukrs, butxt, matnr, maktx, bwart, btext, mvttype, sobkz, werks, werksname,
               lgort, lgortname, umwrk, umwrkname, umlgo, umlgoname, bwtar, menge, erfme, erfmg, meins, lifnr, lifnrname, kunnr, kunnrname,
               hsdat, vfdat, lichn, zmaker, waterratio, lbbsa_sid, kzbew, bstaus_sg, bsttyp_sg, berid, waers, dmbtr,
               ebeln, ebelp, aufnr, aufps, rsnum, rspos, vbeln_im, vbelp_im, shkzg, elikz, kokrs, kzvbr, prctr, aufpl, aplzl, sakto, tcode2, usnam

      FOR ALL ENTRIES IN @lt_sub_sto
        WHERE matnr = @lt_sub_sto-matnr
          AND charg = @lt_sub_sto-charg
          AND cpudt = @lt_sub_sto-cpudt
          AND cputm <= @lt_sub_sto-cputm
        APPENDING CORRESPONDING FIELDS OF TABLE @it_sub.

      FREE lt_sub_sto.
    ENDIF.

    IF it_sub[] IS NOT INITIAL.

      LOOP AT it_sub INTO DATA(ls_sub).

        CLEAR : ls_disp_item.

        READ TABLE gt_disp_item TRANSPORTING NO FIELDS WITH KEY mjahr = ls_sub-mjahr
                                                              mblnr = ls_sub-mblnr
                                                              zeile = ls_sub-zeile.

        IF sy-subrc = 0.
          DELETE TABLE it_sub FROM ls_sub.
        ELSE.
          MOVE-CORRESPONDING ls_sub TO ls_disp_item.
          APPEND ls_disp_item TO gt_disp_item. CLEAR ls_disp_item.
        ENDIF.

      ENDLOOP.

    ENDIF.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form ALL_PRO
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LT_SUB_PRO
*&      --> LT_SUB
*&---------------------------------------------------------------------*
FORM all_pro TABLES it_sub_pro TYPE tt_item
                          it_sub     TYPE tt_item.


  DATA : ls_disp_item TYPE ts_disp.
  IF it_sub_pro[] IS NOT INITIAL.
    SORT it_sub_pro BY aufnr aufps.
    DELETE ADJACENT DUPLICATES FROM it_sub_pro COMPARING aufnr aufps.

    SELECT FROM zsvbmmmatdocb
    FIELDS budat, charg, cpudt, cputm, mjahr, mblnr, zeile, bukrs, butxt, matnr, maktx, bwart, btext, mvttype, sobkz, werks, werksname,
           lgort, lgortname, umwrk, umwrkname, umlgo, umlgoname, bwtar, menge, erfme, erfmg, meins, lifnr, lifnrname, kunnr, kunnrname,
           hsdat, vfdat, lichn, zmaker, waterratio, lbbsa_sid, kzbew, bstaus_sg, bsttyp_sg, berid, waers, dmbtr,
           ebeln, ebelp, aufnr, aufps, rsnum, rspos, vbeln_im, vbelp_im, shkzg, elikz, kokrs, kzvbr, prctr, aufpl, aplzl, sakto, tcode2, usnam

    FOR ALL ENTRIES IN @it_sub_pro
      WHERE aufnr = @it_sub_pro-aufnr
        AND aufps = @it_sub_pro-aufps
      INTO CORRESPONDING FIELDS OF TABLE @it_sub.
  ENDIF.

  IF it_sub[] IS NOT INITIAL.

    LOOP AT it_sub INTO DATA(ls_sub).

      CLEAR : ls_disp_item.


      READ TABLE gt_disp_item TRANSPORTING NO FIELDS WITH KEY budat = ls_sub-budat
                                                              charg = ls_sub-charg
                                                              cpudt = ls_sub-cpudt
                                                              cputm = ls_sub-cputm
                                                              mvttype = ls_sub-mvttype.

      IF sy-subrc = 0.
        DELETE TABLE it_sub FROM ls_sub.
      ELSE.
        MOVE-CORRESPONDING ls_sub TO ls_disp_item.
        APPEND ls_disp_item TO gt_disp_item. CLEAR ls_disp_item.
      ENDIF.

    ENDLOOP.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form ALL_PUR
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LT_SUB_PUR
*&      --> LT_SUB
*&---------------------------------------------------------------------*
FORM all_pur TABLES it_sub_pur TYPE tt_item
                          it_sub     TYPE tt_item.


  DATA : ls_disp_item TYPE ts_disp.

  IF it_sub_pur[] IS NOT INITIAL.

    SORT it_sub_pur BY ebeln ebelp.
    DELETE ADJACENT DUPLICATES FROM it_sub_pur COMPARING ebeln ebelp.

    SELECT FROM zsvbmmmatdocb
      FIELDS budat, charg, cpudt, cputm, mjahr, mblnr, zeile, bukrs, butxt, matnr, maktx, bwart, btext, mvttype, sobkz, werks, werksname,
             lgort, lgortname, umwrk, umwrkname, umlgo, umlgoname, bwtar, menge, erfme, erfmg, meins, lifnr, lifnrname, kunnr, kunnrname,
             hsdat, vfdat, lichn, zmaker, waterratio, lbbsa_sid, kzbew, bstaus_sg, bsttyp_sg, berid, waers, dmbtr,
             ebeln, ebelp, aufnr, aufps, rsnum, rspos, vbeln_im, vbelp_im, shkzg, elikz, kokrs, kzvbr, prctr, aufpl, aplzl, sakto, tcode2, usnam

    FOR ALL ENTRIES IN @it_sub_pur
      WHERE ebeln = @it_sub_pur-ebeln
        AND ebelp = @it_sub_pur-ebelp
      INTO CORRESPONDING FIELDS OF TABLE @it_sub.

    IF it_sub[] IS NOT INITIAL.

      LOOP AT it_sub INTO DATA(ls_sub).

        CLEAR : ls_disp_item.
        READ TABLE gt_disp_item TRANSPORTING NO FIELDS WITH KEY budat = ls_sub-budat
                                                                charg = ls_sub-charg
                                                                cpudt = ls_sub-cpudt
                                                                cputm = ls_sub-cputm
                                                                mvttype = ls_sub-mvttype.

        IF sy-subrc = 0.
          DELETE TABLE it_sub FROM ls_sub.
        ELSE.
          MOVE-CORRESPONDING ls_sub TO ls_disp_item.
          APPEND ls_disp_item TO gt_disp_item. CLEAR ls_disp_item.
        ENDIF.
      ENDLOOP.

    ENDIF.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form ALL_STO
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LT_SUB_STO
*&      --> LT_SUB
*&---------------------------------------------------------------------*
FORM all_sto TABLES it_sub_sto TYPE tt_item
                          it_sub TYPE tt_item.


  DATA : ls_disp_item TYPE ts_disp.

  IF it_sub_sto[] IS NOT INITIAL.
    SORT it_sub_sto BY matnr charg.
    DELETE ADJACENT DUPLICATES FROM it_sub_sto COMPARING matnr charg.

    SELECT FROM zsvbmmmatdocb
      FIELDS budat, charg, cpudt, cputm, mjahr, mblnr, zeile, bukrs, butxt, matnr, maktx, bwart, btext, mvttype, sobkz, werks, werksname,
             lgort, lgortname, umwrk, umwrkname, umlgo, umlgoname, bwtar, menge, erfme, erfmg, meins, lifnr, lifnrname, kunnr, kunnrname,
             hsdat, vfdat, lichn, zmaker, waterratio, lbbsa_sid, kzbew, bstaus_sg, bsttyp_sg, berid, waers, dmbtr,
             ebeln, ebelp, aufnr, aufps, rsnum, rspos, vbeln_im, vbelp_im, shkzg, elikz, kokrs, kzvbr, prctr, aufpl, aplzl, sakto, tcode2, usnam

    FOR ALL ENTRIES IN @it_sub_sto
      WHERE matnr = @it_sub_sto-matnr
        AND charg = @it_sub_sto-charg
        AND aufnr = ''
        AND ebeln = ''
      INTO CORRESPONDING FIELDS OF TABLE @it_sub.

    IF it_sub[] IS NOT INITIAL.

      LOOP AT it_sub INTO DATA(ls_sub).

        CLEAR : ls_disp_item.
        READ TABLE gt_disp_item TRANSPORTING NO FIELDS WITH KEY budat = ls_sub-budat
                                                                charg = ls_sub-charg
                                                                cpudt = ls_sub-cpudt
                                                                cputm = ls_sub-cputm
                                                                mvttype = ls_sub-mvttype.

        IF sy-subrc = 0.
          DELETE TABLE it_sub FROM ls_sub.
        ELSE.
          MOVE-CORRESPONDING ls_sub TO ls_disp_item.
          APPEND ls_disp_item TO gt_disp_item. CLEAR ls_disp_item.
        ENDIF.
      ENDLOOP.

    ENDIF.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form BOTTOM_UP_PRO
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LT_SUB_PRO
*&      --> LT_SUB
*&---------------------------------------------------------------------*
FORM bottom_up_pro TABLES it_sub_pro TYPE tt_item
                          it_sub     TYPE tt_item.


  DATA : ls_disp_item TYPE ts_disp.
  DATA : lt_sub_pro_101 TYPE TABLE OF ts_disp,    "생산입고 집계용
*         LT_SUB_PRO_102 TYPE TABLE OF TS_DISP,    "생산입고취소 집계용
         lt_sub_pro_261 TYPE TABLE OF ts_disp,    "생산출고집계용
*         LT_SUB_PRO_262 TYPE TABLE OF TS_DISP,    "생산출고취소집계용
         lt_sub_pro_etc TYPE TABLE OF ts_disp.    "기타집계용

  IF it_sub_pro[] IS NOT INITIAL.

    SORT it_sub_pro BY aufnr aufps budat.
    DELETE ADJACENT DUPLICATES FROM it_sub_pro COMPARING aufnr aufps budat.


    LOOP AT it_sub_pro ASSIGNING FIELD-SYMBOL(<ls_sub_prot>) .
      IF <ls_sub_prot>-bwart =  gc_101 OR <ls_sub_prot>-bwart =  gc_102.
        APPEND <ls_sub_prot> TO lt_sub_pro_101.
      ELSEIF <ls_sub_prot>-bwart =  gc_261 OR <ls_sub_prot>-bwart =  gc_262.
        APPEND <ls_sub_prot> TO lt_sub_pro_261.
      ELSE.
        APPEND <ls_sub_prot> TO lt_sub_pro_etc.
      ENDIF.
    ENDLOOP.

    IF lt_sub_pro_101[] IS NOT INITIAL.

      DATA(lt_pro_101) = lt_sub_pro_101[].
      IF lt_pro_101[] IS NOT INITIAL.

        SORT lt_pro_101 BY aufnr matnr cpudt.
        DELETE ADJACENT DUPLICATES FROM lt_pro_101 COMPARING aufnr matnr cpudt.

        SELECT FROM zsvbmmmatdocb
         FIELDS budat, charg, cpudt, cputm, mjahr, mblnr, zeile, bukrs, butxt, matnr, maktx, bwart, btext, mvttype, sobkz, werks, werksname,
                lgort, lgortname, umwrk, umwrkname, umlgo, umlgoname, bwtar, menge, erfme, erfmg, meins, lifnr, lifnrname, kunnr, kunnrname,
                hsdat, vfdat, lichn, zmaker, waterratio, lbbsa_sid, kzbew, bstaus_sg, bsttyp_sg, berid, waers, dmbtr,
                ebeln, ebelp, aufnr, aufps, rsnum, rspos, vbeln_im, vbelp_im, shkzg, elikz, kokrs, kzvbr, prctr, aufpl, aplzl, sakto, tcode2, usnam
         FOR ALL ENTRIES IN @lt_pro_101
           WHERE aufnr = @lt_pro_101-aufnr
              AND matnr =  @lt_pro_101-matnr
              AND charg =  @lt_pro_101-charg
              AND cpudt GT @lt_pro_101-cpudt
           APPENDING CORRESPONDING FIELDS OF TABLE @it_sub.

        FREE lt_pro_101.

        lt_pro_101[] = lt_sub_pro_101[].
        SORT lt_pro_101 BY aufnr matnr cpudt cputm.
        DELETE ADJACENT DUPLICATES FROM lt_pro_101 COMPARING aufnr matnr cpudt cputm.

        SELECT FROM zsvbmmmatdocb
         FIELDS budat, charg, cpudt, cputm, mjahr, mblnr, zeile, bukrs, butxt, matnr, maktx, bwart, btext, mvttype, sobkz, werks, werksname,
                lgort, lgortname, umwrk, umwrkname, umlgo, umlgoname, bwtar, menge, erfme, erfmg, meins, lifnr, lifnrname, kunnr, kunnrname,
                hsdat, vfdat, lichn, zmaker, waterratio, lbbsa_sid, kzbew, bstaus_sg, bsttyp_sg, berid, waers, dmbtr,
                ebeln, ebelp, aufnr, aufps, rsnum, rspos, vbeln_im, vbelp_im, shkzg, elikz, kokrs, kzvbr, prctr, aufpl, aplzl, sakto, tcode2, usnam
         FOR ALL ENTRIES IN @lt_pro_101
           WHERE aufnr = @lt_pro_101-aufnr
              AND matnr =  @lt_pro_101-matnr
              AND charg =  @lt_pro_101-charg
              AND cpudt EQ @lt_pro_101-cpudt
              AND cputm GE @lt_pro_101-cputm
           APPENDING CORRESPONDING FIELDS OF TABLE @it_sub.
        FREE lt_pro_101.
      ENDIF.
    ENDIF.


    IF lt_sub_pro_261[] IS NOT INITIAL.

      DATA(lt_pro_261) = lt_sub_pro_261[].

      IF lt_pro_261[] IS NOT INITIAL.

        SORT lt_pro_261 BY aufnr matnr cpudt.
        DELETE ADJACENT DUPLICATES FROM lt_pro_261 COMPARING aufnr matnr cpudt.

        SELECT FROM zsvbmmmatdocb
         FIELDS budat, charg, cpudt, cputm, mjahr, mblnr, zeile, bukrs, butxt, matnr, maktx, bwart, btext, mvttype, sobkz, werks, werksname,
                lgort, lgortname, umwrk, umwrkname, umlgo, umlgoname, bwtar, menge, erfme, erfmg, meins, lifnr, lifnrname, kunnr, kunnrname,
                hsdat, vfdat, lichn, zmaker, waterratio, lbbsa_sid, kzbew, bstaus_sg, bsttyp_sg, berid, waers, dmbtr,
                ebeln, ebelp, aufnr, aufps, rsnum, rspos, vbeln_im, vbelp_im, shkzg, elikz, kokrs, kzvbr, prctr, aufpl, aplzl, sakto, tcode2, usnam
         FOR ALL ENTRIES IN @lt_pro_261
           WHERE aufnr = @lt_pro_261-aufnr
              AND matnr NE  @lt_pro_261-matnr
              AND cpudt GT @lt_pro_261-cpudt
              AND bwart IN ( @gc_101 , @gc_102 )
           APPENDING CORRESPONDING FIELDS OF TABLE @it_sub.

        FREE lt_pro_261.

        lt_pro_261[] = lt_sub_pro_261[].
        SORT lt_pro_261 BY aufnr matnr cpudt cputm.
        DELETE ADJACENT DUPLICATES FROM lt_pro_261 COMPARING aufnr matnr cpudt cputm.

        SELECT FROM zsvbmmmatdocb
         FIELDS budat, charg, cpudt, cputm, mjahr, mblnr, zeile, bukrs, butxt, matnr, maktx, bwart, btext, mvttype, sobkz, werks, werksname,
                lgort, lgortname, umwrk, umwrkname, umlgo, umlgoname, bwtar, menge, erfme, erfmg, meins, lifnr, lifnrname, kunnr, kunnrname,
                hsdat, vfdat, lichn, zmaker, waterratio, lbbsa_sid, kzbew, bstaus_sg, bsttyp_sg, berid, waers, dmbtr,
                ebeln, ebelp, aufnr, aufps, rsnum, rspos, vbeln_im, vbelp_im, shkzg, elikz, kokrs, kzvbr, prctr, aufpl, aplzl, sakto, tcode2, usnam
         FOR ALL ENTRIES IN @lt_pro_261
           WHERE aufnr = @lt_pro_261-aufnr
              AND matnr NE  @lt_pro_261-matnr
              AND cpudt EQ @lt_pro_261-cpudt
              AND cputm GE @lt_pro_261-cputm
              AND bwart IN ( @gc_101 , @gc_102 )
           APPENDING CORRESPONDING FIELDS OF TABLE @it_sub.
        FREE lt_pro_261.
      ENDIF.
    ENDIF.

    IF lt_sub_pro_etc[] IS NOT INITIAL.

      DATA(lt_pro_etc) = lt_sub_pro_etc[].

      IF lt_pro_etc[] IS NOT INITIAL.

        SORT lt_pro_etc BY matnr charg cpudt.
        DELETE ADJACENT DUPLICATES FROM lt_pro_etc COMPARING matnr charg cpudt.

        SELECT FROM zsvbmmmatdocb
         FIELDS budat, charg, cpudt, cputm, mjahr, mblnr, zeile, bukrs, butxt, matnr, maktx, bwart, btext, mvttype, sobkz, werks, werksname,
                lgort, lgortname, umwrk, umwrkname, umlgo, umlgoname, bwtar, menge, erfme, erfmg, meins, lifnr, lifnrname, kunnr, kunnrname,
                hsdat, vfdat, lichn, zmaker, waterratio, lbbsa_sid, kzbew, bstaus_sg, bsttyp_sg, berid, waers, dmbtr,
                ebeln, ebelp, aufnr, aufps, rsnum, rspos, vbeln_im, vbelp_im, shkzg, elikz, kokrs, kzvbr, prctr, aufpl, aplzl, sakto, tcode2, usnam
         FOR ALL ENTRIES IN @lt_pro_etc
           WHERE aufnr = ''
              AND matnr =  @lt_pro_etc-matnr
              AND charg =  @lt_pro_etc-charg
              AND cpudt GT @lt_pro_etc-cpudt
           APPENDING CORRESPONDING FIELDS OF TABLE @it_sub.

        FREE lt_pro_etc.

        lt_pro_etc[] = lt_sub_pro_etc[].
        SORT lt_pro_etc BY matnr charg cpudt cputm.
        DELETE ADJACENT DUPLICATES FROM lt_pro_etc COMPARING matnr charg cpudt cputm.

        SELECT FROM zsvbmmmatdocb
         FIELDS budat, charg, cpudt, cputm, mjahr, mblnr, zeile, bukrs, butxt, matnr, maktx, bwart, btext, mvttype, sobkz, werks, werksname,
                lgort, lgortname, umwrk, umwrkname, umlgo, umlgoname, bwtar, menge, erfme, erfmg, meins, lifnr, lifnrname, kunnr, kunnrname,
                hsdat, vfdat, lichn, zmaker, waterratio, lbbsa_sid, kzbew, bstaus_sg, bsttyp_sg, berid, waers, dmbtr,
                ebeln, ebelp, aufnr, aufps, rsnum, rspos, vbeln_im, vbelp_im, shkzg, elikz, kokrs, kzvbr, prctr, aufpl, aplzl, sakto, tcode2, usnam
         FOR ALL ENTRIES IN @lt_pro_etc
           WHERE aufnr = ''
              AND matnr =  @lt_pro_etc-matnr
              AND charg =  @lt_pro_etc-charg
              AND cpudt EQ @lt_pro_etc-cpudt
              AND cputm GE @lt_pro_etc-cputm
           APPENDING CORRESPONDING FIELDS OF TABLE @it_sub.
        FREE lt_pro_etc.
      ENDIF.
    ENDIF.

*
*
*    SELECT FROM ZSVBMMMATDOCB
*    FIELDS BUDAT, CHARG, CPUDT, CPUTM, MJAHR, MBLNR, ZEILE, BUKRS, BUTXT, MATNR, MAKTX, BWART, BTEXT, MVTTYPE, SOBKZ, WERKS, WERKSNAME,
*           LGORT, LGORTNAME, UMWRK, UMWRKNAME, UMLGO, UMLGONAME, BWTAR, MENGE, ERFME, ERFMG, MEINS, LIFNR, LIFNRNAME, KUNNR, KUNNRNAME,
*           HSDAT, VFDAT, LICHN, ZMAKER, WATERRATIO, LBBSA_SID, KZBEW, BSTAUS_SG, BSTTYP_SG, BERID, WAERS, DMBTR,
*           EBELN, EBELP, AUFNR, AUFPS, RSNUM, RSPOS, VBELN_IM, VBELP_IM, SHKZG, ELIKZ, KOKRS, KZVBR, PRCTR, AUFPL, APLZL, SAKTO, TCODE2, USNAM
*
*    FOR ALL ENTRIES IN @IT_SUB_PRO
*      WHERE AUFNR = @IT_SUB_PRO-AUFNR
**        AND AUFPS = @IT_SUB_PRO-AUFPS
*        AND CPUDT GT @IT_SUB_PRO-CPUDT
*      APPENDING CORRESPONDING FIELDS OF TABLE @IT_SUB.
*
*    SELECT FROM ZSVBMMMATDOCB
*    FIELDS BUDAT, CHARG, CPUDT, CPUTM, MJAHR, MBLNR, ZEILE, BUKRS, BUTXT, MATNR, MAKTX, BWART, BTEXT, MVTTYPE, SOBKZ, WERKS, WERKSNAME,
*           LGORT, LGORTNAME, UMWRK, UMWRKNAME, UMLGO, UMLGONAME, BWTAR, MENGE, ERFME, ERFMG, MEINS, LIFNR, LIFNRNAME, KUNNR, KUNNRNAME,
*           HSDAT, VFDAT, LICHN, ZMAKER, WATERRATIO, LBBSA_SID, KZBEW, BSTAUS_SG, BSTTYP_SG, BERID, WAERS, DMBTR,
*           EBELN, EBELP, AUFNR, AUFPS, RSNUM, RSPOS, VBELN_IM, VBELP_IM, SHKZG, ELIKZ, KOKRS, KZVBR, PRCTR, AUFPL, APLZL, SAKTO, TCODE2, USNAM
*
*    FOR ALL ENTRIES IN @IT_SUB_PRO
*      WHERE AUFNR = @IT_SUB_PRO-AUFNR
**        AND AUFPS = @IT_SUB_PRO-AUFPS
*        AND CPUDT EQ @IT_SUB_PRO-CPUDT
*        AND CPUTM GE @IT_SUB_PRO-CPUTM
*      APPENDING CORRESPONDING FIELDS OF TABLE @IT_SUB.

    IF it_sub[] IS NOT INITIAL.

      LOOP AT it_sub INTO DATA(ls_sub).

        CLEAR : ls_disp_item.

        READ TABLE gt_disp_item TRANSPORTING NO FIELDS WITH KEY  mjahr = ls_sub-mjahr
                                                              mblnr = ls_sub-mblnr
                                                              zeile = ls_sub-zeile.

        IF sy-subrc = 0.
          DELETE TABLE it_sub FROM ls_sub.
        ELSE.
          MOVE-CORRESPONDING ls_sub TO ls_disp_item.
          APPEND ls_disp_item TO gt_disp_item. CLEAR ls_disp_item.
        ENDIF.

      ENDLOOP.

    ENDIF.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form BOTTOM_UP_PUR
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LT_SUB_PUR
*&      --> LT_SUB
*&---------------------------------------------------------------------*
FORM bottom_up_pur TABLES it_sub_pur TYPE tt_item
                          it_sub     TYPE tt_item.



  DATA : ls_disp_item TYPE ts_disp.
  DATA : lt_sub_pur_101 TYPE TABLE OF ts_disp,
         lt_sub_pur_543 TYPE TABLE OF ts_disp,
         lt_sub_pur_etc TYPE TABLE OF ts_disp.    "기타집계용


  IF it_sub_pur[] IS NOT INITIAL.
    SORT it_sub_pur BY ebeln ebelp budat.
    DELETE ADJACENT DUPLICATES FROM it_sub_pur COMPARING ebeln ebelp budat.

    LOOP AT it_sub_pur ASSIGNING FIELD-SYMBOL(<ls_sub_purt>) .
      IF <ls_sub_purt>-bwart =  gc_101 OR <ls_sub_purt>-bwart = gc_102 OR <ls_sub_purt>-bwart = gc_161 OR <ls_sub_purt>-bwart = gc_162.
        APPEND <ls_sub_purt> TO lt_sub_pur_101.
      ELSEIF <ls_sub_purt>-bwart =  gc_543 OR <ls_sub_purt>-bwart =  gc_544.
        APPEND <ls_sub_purt> TO lt_sub_pur_543..
      ELSE.
        APPEND <ls_sub_purt> TO lt_sub_pur_etc.
      ENDIF.
    ENDLOOP.

    IF lt_sub_pur_101[] IS NOT INITIAL.


      DATA(lt_pur_101) = lt_sub_pur_101[].
      IF lt_pur_101[] IS NOT INITIAL.

        SORT lt_pur_101 BY matnr charg ebeln ebelp cpudt.
        DELETE ADJACENT DUPLICATES FROM lt_pur_101 COMPARING matnr charg ebeln ebelp cpudt.

        SELECT FROM zsvbmmmatdocb
          FIELDS budat, charg, cpudt, cputm, mjahr, mblnr, zeile, bukrs, butxt, matnr, maktx, bwart, btext, mvttype, sobkz, werks, werksname,
                 lgort, lgortname, umwrk, umwrkname, umlgo, umlgoname, bwtar, menge, erfme, erfmg, meins, lifnr, lifnrname, kunnr, kunnrname,
                 hsdat, vfdat, lichn, zmaker, waterratio, lbbsa_sid, kzbew, bstaus_sg, bsttyp_sg, berid, waers, dmbtr,
                 ebeln, ebelp, aufnr, aufps, rsnum, rspos, vbeln_im, vbelp_im, shkzg, elikz, kokrs, kzvbr, prctr, aufpl, aplzl, sakto, tcode2, usnam

        FOR ALL ENTRIES IN @lt_pur_101
          WHERE matnr = @lt_pur_101-matnr
            AND charg = @lt_pur_101-charg
            AND ebeln NE @lt_pur_101-ebeln
            AND ebelp NE @lt_pur_101-ebelp
            AND cpudt GT @lt_pur_101-cpudt
          APPENDING CORRESPONDING FIELDS OF TABLE @it_sub.

        FREE lt_pur_101.

        lt_pur_101[] = lt_sub_pur_101[].
        SORT lt_pur_101 BY matnr charg ebeln ebelp cpudt cputm.
        DELETE ADJACENT DUPLICATES FROM lt_pur_101 COMPARING matnr charg ebeln ebelp cpudt cputm.

        SELECT FROM zsvbmmmatdocb
          FIELDS budat, charg, cpudt, cputm, mjahr, mblnr, zeile, bukrs, butxt, matnr, maktx, bwart, btext, mvttype, sobkz, werks, werksname,
                 lgort, lgortname, umwrk, umwrkname, umlgo, umlgoname, bwtar, menge, erfme, erfmg, meins, lifnr, lifnrname, kunnr, kunnrname,
                 hsdat, vfdat, lichn, zmaker, waterratio, lbbsa_sid, kzbew, bstaus_sg, bsttyp_sg, berid, waers, dmbtr,
                 ebeln, ebelp, aufnr, aufps, rsnum, rspos, vbeln_im, vbelp_im, shkzg, elikz, kokrs, kzvbr, prctr, aufpl, aplzl, sakto, tcode2, usnam

        FOR ALL ENTRIES IN @lt_pur_101
          WHERE matnr = @lt_pur_101-matnr
            AND charg = @lt_pur_101-charg
            AND ebeln NE @lt_pur_101-ebeln
            AND ebelp NE @lt_pur_101-ebelp
            AND cpudt EQ @lt_pur_101-cpudt
            AND cputm GE @lt_pur_101-cputm
          APPENDING CORRESPONDING FIELDS OF TABLE @it_sub.
        FREE lt_pur_101.
      ENDIF.
    ENDIF.

    IF lt_sub_pur_543[] IS NOT INITIAL.

      DATA(lt_pur_543) = lt_sub_pur_543[].
      IF lt_pur_543[] IS NOT INITIAL.

        SORT lt_pur_543 BY ebeln ebelp cpudt.
        DELETE ADJACENT DUPLICATES FROM lt_pur_543 COMPARING ebeln ebelp cpudt.

        SELECT FROM zsvbmmmatdocb
          FIELDS budat, charg, cpudt, cputm, mjahr, mblnr, zeile, bukrs, butxt, matnr, maktx, bwart, btext, mvttype, sobkz, werks, werksname,
                 lgort, lgortname, umwrk, umwrkname, umlgo, umlgoname, bwtar, menge, erfme, erfmg, meins, lifnr, lifnrname, kunnr, kunnrname,
                 hsdat, vfdat, lichn, zmaker, waterratio, lbbsa_sid, kzbew, bstaus_sg, bsttyp_sg, berid, waers, dmbtr,
                 ebeln, ebelp, aufnr, aufps, rsnum, rspos, vbeln_im, vbelp_im, shkzg, elikz, kokrs, kzvbr, prctr, aufpl, aplzl, sakto, tcode2, usnam
        FOR ALL ENTRIES IN @lt_pur_543
          WHERE ebeln = @lt_pur_543-ebeln
            AND ebelp = @lt_pur_543-ebelp
            AND cpudt GT @lt_pur_543-cpudt
          APPENDING CORRESPONDING FIELDS OF TABLE @it_sub.

        FREE lt_pur_543.

        lt_pur_543[] = lt_sub_pur_543[].
        SORT lt_pur_543 BY ebeln ebelp cpudt cputm.
        DELETE ADJACENT DUPLICATES FROM lt_pur_543 COMPARING ebeln ebelp cpudt cputm.

        SELECT FROM zsvbmmmatdocb
          FIELDS budat, charg, cpudt, cputm, mjahr, mblnr, zeile, bukrs, butxt, matnr, maktx, bwart, btext, mvttype, sobkz, werks, werksname,
                 lgort, lgortname, umwrk, umwrkname, umlgo, umlgoname, bwtar, menge, erfme, erfmg, meins, lifnr, lifnrname, kunnr, kunnrname,
                 hsdat, vfdat, lichn, zmaker, waterratio, lbbsa_sid, kzbew, bstaus_sg, bsttyp_sg, berid, waers, dmbtr,
                 ebeln, ebelp, aufnr, aufps, rsnum, rspos, vbeln_im, vbelp_im, shkzg, elikz, kokrs, kzvbr, prctr, aufpl, aplzl, sakto, tcode2, usnam
        FOR ALL ENTRIES IN @lt_pur_543
          WHERE ebeln = @lt_pur_543-ebeln
            AND ebelp = @lt_pur_543-ebelp
            AND cpudt EQ @lt_pur_543-cpudt
            AND cputm GE @lt_pur_543-cputm
          APPENDING CORRESPONDING FIELDS OF TABLE @it_sub.
        FREE lt_pur_543.

      ENDIF.
    ENDIF.


    IF lt_sub_pur_etc[] IS NOT INITIAL.

      DATA(lt_pur_etc) = lt_sub_pur_etc[].
      IF lt_pur_etc[] IS NOT INITIAL.

        SORT lt_pur_etc BY matnr charg cpudt.
        DELETE ADJACENT DUPLICATES FROM lt_pur_etc COMPARING matnr charg cpudt.

        SELECT FROM zsvbmmmatdocb
          FIELDS budat, charg, cpudt, cputm, mjahr, mblnr, zeile, bukrs, butxt, matnr, maktx, bwart, btext, mvttype, sobkz, werks, werksname,
                 lgort, lgortname, umwrk, umwrkname, umlgo, umlgoname, bwtar, menge, erfme, erfmg, meins, lifnr, lifnrname, kunnr, kunnrname,
                 hsdat, vfdat, lichn, zmaker, waterratio, lbbsa_sid, kzbew, bstaus_sg, bsttyp_sg, berid, waers, dmbtr,
                 ebeln, ebelp, aufnr, aufps, rsnum, rspos, vbeln_im, vbelp_im, shkzg, elikz, kokrs, kzvbr, prctr, aufpl, aplzl, sakto, tcode2, usnam

        FOR ALL ENTRIES IN @lt_pur_etc
          WHERE matnr = @lt_pur_etc-matnr
            AND charg = @lt_pur_etc-charg
            AND cpudt GT @lt_pur_etc-cpudt
          APPENDING CORRESPONDING FIELDS OF TABLE @it_sub.

        FREE lt_pur_etc.

        lt_pur_etc[] = lt_sub_pur_etc[].
        SORT lt_pur_etc BY matnr charg cpudt cputm.
        DELETE ADJACENT DUPLICATES FROM lt_pur_etc COMPARING matnr charg cpudt cputm.

        SELECT FROM zsvbmmmatdocb
          FIELDS budat, charg, cpudt, cputm, mjahr, mblnr, zeile, bukrs, butxt, matnr, maktx, bwart, btext, mvttype, sobkz, werks, werksname,
                 lgort, lgortname, umwrk, umwrkname, umlgo, umlgoname, bwtar, menge, erfme, erfmg, meins, lifnr, lifnrname, kunnr, kunnrname,
                 hsdat, vfdat, lichn, zmaker, waterratio, lbbsa_sid, kzbew, bstaus_sg, bsttyp_sg, berid, waers, dmbtr,
                 ebeln, ebelp, aufnr, aufps, rsnum, rspos, vbeln_im, vbelp_im, shkzg, elikz, kokrs, kzvbr, prctr, aufpl, aplzl, sakto, tcode2, usnam

        FOR ALL ENTRIES IN @lt_pur_etc
          WHERE matnr = @lt_pur_etc-matnr
            AND charg = @lt_pur_etc-charg
            AND cpudt EQ @lt_pur_etc-cpudt
            AND cputm GE @lt_pur_etc-cputm
          APPENDING CORRESPONDING FIELDS OF TABLE @it_sub.
        FREE lt_pur_etc.
      ENDIF.
    ENDIF.

*    SELECT FROM ZSVBMMMATDOCB
*      FIELDS BUDAT, CHARG, CPUDT, CPUTM, MJAHR, MBLNR, ZEILE, BUKRS, BUTXT, MATNR, MAKTX, BWART, BTEXT, MVTTYPE, SOBKZ, WERKS, WERKSNAME,
*             LGORT, LGORTNAME, UMWRK, UMWRKNAME, UMLGO, UMLGONAME, BWTAR, MENGE, ERFME, ERFMG, MEINS, LIFNR, LIFNRNAME, KUNNR, KUNNRNAME,
*             HSDAT, VFDAT, LICHN, ZMAKER, WATERRATIO, LBBSA_SID, KZBEW, BSTAUS_SG, BSTTYP_SG, BERID, WAERS, DMBTR,
*             EBELN, EBELP, AUFNR, AUFPS, RSNUM, RSPOS, VBELN_IM, VBELP_IM, SHKZG, ELIKZ, KOKRS, KZVBR, PRCTR, AUFPL, APLZL, SAKTO, TCODE2, USNAM
*
*    FOR ALL ENTRIES IN @IT_SUB_PUR
*      WHERE EBELN = @IT_SUB_PUR-EBELN
*        AND EBELP = @IT_SUB_PUR-EBELP
*        AND CPUDT GT @IT_SUB_PUR-CPUDT
*      APPENDING CORRESPONDING FIELDS OF TABLE @IT_SUB.
*
*    SELECT FROM ZSVBMMMATDOCB
*      FIELDS BUDAT, CHARG, CPUDT, CPUTM, MJAHR, MBLNR, ZEILE, BUKRS, BUTXT, MATNR, MAKTX, BWART, BTEXT, MVTTYPE, SOBKZ, WERKS, WERKSNAME,
*             LGORT, LGORTNAME, UMWRK, UMWRKNAME, UMLGO, UMLGONAME, BWTAR, MENGE, ERFME, ERFMG, MEINS, LIFNR, LIFNRNAME, KUNNR, KUNNRNAME,
*             HSDAT, VFDAT, LICHN, ZMAKER, WATERRATIO, LBBSA_SID, KZBEW, BSTAUS_SG, BSTTYP_SG, BERID, WAERS, DMBTR,
*             EBELN, EBELP, AUFNR, AUFPS, RSNUM, RSPOS, VBELN_IM, VBELP_IM, SHKZG, ELIKZ, KOKRS, KZVBR, PRCTR, AUFPL, APLZL, SAKTO, TCODE2, USNAM
*
*    FOR ALL ENTRIES IN @IT_SUB_PUR
*      WHERE EBELN = @IT_SUB_PUR-EBELN
*        AND EBELP = @IT_SUB_PUR-EBELP
*        AND CPUDT EQ @IT_SUB_PUR-CPUDT
*        AND CPUTM GE @IT_SUB_PUR-CPUTM
*      APPENDING CORRESPONDING FIELDS OF TABLE @IT_SUB.

    IF it_sub[] IS NOT INITIAL.

      LOOP AT it_sub INTO DATA(ls_sub).

        CLEAR : ls_disp_item.

        READ TABLE gt_disp_item TRANSPORTING NO FIELDS WITH KEY mjahr = ls_sub-mjahr
                                                              mblnr = ls_sub-mblnr
                                                              zeile = ls_sub-zeile.

        IF sy-subrc = 0.
          DELETE TABLE it_sub FROM ls_sub.
        ELSE.
          MOVE-CORRESPONDING ls_sub TO ls_disp_item.
          APPEND ls_disp_item TO gt_disp_item. CLEAR ls_disp_item.
        ENDIF.

      ENDLOOP.

    ENDIF.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form BOTTOM_UP_STO
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LT_SUB_STO
*&      --> LT_SUB
*&---------------------------------------------------------------------*
FORM bottom_up_sto TABLES it_sub_sto TYPE tt_item
                          it_sub TYPE tt_item.


  DATA : ls_disp_item TYPE ts_disp.

  IF it_sub_sto[] IS NOT INITIAL.

    SORT it_sub_sto BY matnr charg budat.
    DELETE ADJACENT DUPLICATES FROM it_sub_sto COMPARING matnr charg budat.

    DATA(lt_sub_sto) = it_sub_sto[].
    IF lt_sub_sto[] IS NOT INITIAL.

      SORT lt_sub_sto BY matnr charg cpudt.
      DELETE ADJACENT DUPLICATES FROM lt_sub_sto COMPARING matnr charg cpudt.

      SELECT FROM zsvbmmmatdocb
        FIELDS budat, charg, cpudt, cputm, mjahr, mblnr, zeile, bukrs, butxt, matnr, maktx, bwart, btext, mvttype, sobkz, werks, werksname,
               lgort, lgortname, umwrk, umwrkname, umlgo, umlgoname, bwtar, menge, erfme, erfmg, meins, lifnr, lifnrname, kunnr, kunnrname,
               hsdat, vfdat, lichn, zmaker, waterratio, lbbsa_sid, kzbew, bstaus_sg, bsttyp_sg, berid, waers, dmbtr,
               ebeln, ebelp, aufnr, aufps, rsnum, rspos, vbeln_im, vbelp_im, shkzg, elikz, kokrs, kzvbr, prctr, aufpl, aplzl, sakto, tcode2, usnam

      FOR ALL ENTRIES IN @lt_sub_sto
        WHERE matnr = @lt_sub_sto-matnr
          AND charg = @lt_sub_sto-charg
          AND cpudt GT @lt_sub_sto-cpudt
        APPENDING CORRESPONDING FIELDS OF TABLE @it_sub.

      FREE lt_sub_sto.

      lt_sub_sto[] = it_sub_sto[].
      SORT lt_sub_sto BY matnr charg cpudt cputm.
      DELETE ADJACENT DUPLICATES FROM lt_sub_sto COMPARING matnr charg cpudt cputm.

      SELECT FROM zsvbmmmatdocb
        FIELDS budat, charg, cpudt, cputm, mjahr, mblnr, zeile, bukrs, butxt, matnr, maktx, bwart, btext, mvttype, sobkz, werks, werksname,
               lgort, lgortname, umwrk, umwrkname, umlgo, umlgoname, bwtar, menge, erfme, erfmg, meins, lifnr, lifnrname, kunnr, kunnrname,
               hsdat, vfdat, lichn, zmaker, waterratio, lbbsa_sid, kzbew, bstaus_sg, bsttyp_sg, berid, waers, dmbtr,
               ebeln, ebelp, aufnr, aufps, rsnum, rspos, vbeln_im, vbelp_im, shkzg, elikz, kokrs, kzvbr, prctr, aufpl, aplzl, sakto, tcode2, usnam

      FOR ALL ENTRIES IN @lt_sub_sto
        WHERE matnr = @lt_sub_sto-matnr
          AND charg = @lt_sub_sto-charg
          AND cpudt EQ @lt_sub_sto-cpudt
          AND cputm GE @lt_sub_sto-cputm
        APPENDING CORRESPONDING FIELDS OF TABLE @it_sub.
      FREE lt_sub_sto.

    ENDIF.

    IF it_sub[] IS NOT INITIAL.

      LOOP AT it_sub INTO DATA(ls_sub).

        CLEAR : ls_disp_item.

        READ TABLE gt_disp_item TRANSPORTING NO FIELDS WITH KEY mjahr = ls_sub-mjahr
                                                              mblnr = ls_sub-mblnr
                                                              zeile = ls_sub-zeile..

        IF sy-subrc = 0.
          DELETE TABLE it_sub FROM ls_sub.
        ELSE.
          MOVE-CORRESPONDING ls_sub TO ls_disp_item.
          APPEND ls_disp_item TO gt_disp_item. CLEAR ls_disp_item.
        ENDIF.

      ENDLOOP.

    ENDIF.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form top_down_pur_101
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LT_SUB_PUR_101
*&      --> IT_SUB
*&---------------------------------------------------------------------*
FORM top_down_pur_101 TABLES it_sub_pur_101 LIKE gt_disp
                                it_sub     TYPE tt_item.

  DATA(lt_pur_101) = it_sub_pur_101[].
  IF lt_pur_101[] IS NOT INITIAL.

    SORT lt_pur_101 BY ebeln ebelp cpudt.
    DELETE ADJACENT DUPLICATES FROM lt_pur_101 COMPARING ebeln ebelp cpudt.

    SELECT FROM zsvbmmmatdocb
       FIELDS budat, charg, cpudt, cputm, mjahr, mblnr, zeile, bukrs, butxt, matnr, maktx, bwart, btext, mvttype, sobkz, werks, werksname,
              lgort, lgortname, umwrk, umwrkname, umlgo, umlgoname, bwtar, menge, erfme, erfmg, meins, lifnr, lifnrname, kunnr, kunnrname,
              hsdat, vfdat, lichn, zmaker, waterratio, lbbsa_sid, kzbew, bstaus_sg, bsttyp_sg, berid, waers, dmbtr,
              ebeln, ebelp, aufnr, aufps, rsnum, rspos, vbeln_im, vbelp_im, shkzg, elikz, kokrs, kzvbr, prctr, aufpl, aplzl, sakto, tcode2, usnam
     FOR ALL ENTRIES IN @lt_pur_101
       WHERE ebeln = @lt_pur_101-ebeln
         AND ebelp = @lt_pur_101-ebelp
         AND cpudt < @lt_pur_101-cpudt
         AND bwart IN (  @gc_102 ,  @gc_543, @gc_544 )
       APPENDING CORRESPONDING FIELDS OF TABLE @it_sub.

    FREE lt_pur_101.

    lt_pur_101[] = it_sub_pur_101[].
    SORT lt_pur_101 BY ebeln ebelp cpudt cputm.
    DELETE ADJACENT DUPLICATES FROM lt_pur_101 COMPARING ebeln ebelp cpudt cputm.

    SELECT FROM zsvbmmmatdocb
      FIELDS budat, charg, cpudt, cputm, mjahr, mblnr, zeile, bukrs, butxt, matnr, maktx, bwart, btext, mvttype, sobkz, werks, werksname,
             lgort, lgortname, umwrk, umwrkname, umlgo, umlgoname, bwtar, menge, erfme, erfmg, meins, lifnr, lifnrname, kunnr, kunnrname,
             hsdat, vfdat, lichn, zmaker, waterratio, lbbsa_sid, kzbew, bstaus_sg, bsttyp_sg, berid, waers, dmbtr,
             ebeln, ebelp, aufnr, aufps, rsnum, rspos, vbeln_im, vbelp_im, shkzg, elikz, kokrs, kzvbr, prctr, aufpl, aplzl, sakto, tcode2, usnam
    FOR ALL ENTRIES IN @lt_pur_101
      WHERE ebeln = @lt_pur_101-ebeln
        AND ebelp = @lt_pur_101-ebelp
        AND cpudt = @lt_pur_101-cpudt
        AND cputm <= @lt_pur_101-cputm
        AND bwart IN (  @gc_102 ,  @gc_543, @gc_544 )
      APPENDING CORRESPONDING FIELDS OF TABLE @it_sub.
    FREE lt_pur_101.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form top_down_pur_543
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LT_SUB_PUR_543
*&      --> IT_SUB
*&---------------------------------------------------------------------*
FORM top_down_pur_543 TABLES it_sub_pur_543 LIKE gt_disp
                                it_sub     TYPE tt_item.

  DATA(lt_pur_543) = it_sub_pur_543[].
  IF lt_pur_543[] IS NOT INITIAL.

    SORT lt_pur_543 BY ebeln ebelp matnr charg cpudt.
    DELETE ADJACENT DUPLICATES FROM lt_pur_543 COMPARING ebeln ebelp matnr charg cpudt.

    SELECT FROM zsvbmmmatdocb
      FIELDS budat, charg, cpudt, cputm, mjahr, mblnr, zeile, bukrs, butxt, matnr, maktx, bwart, btext, mvttype, sobkz, werks, werksname,
             lgort, lgortname, umwrk, umwrkname, umlgo, umlgoname, bwtar, menge, erfme, erfmg, meins, lifnr, lifnrname, kunnr, kunnrname,
             hsdat, vfdat, lichn, zmaker, waterratio, lbbsa_sid, kzbew, bstaus_sg, bsttyp_sg, berid, waers, dmbtr,
             ebeln, ebelp, aufnr, aufps, rsnum, rspos, vbeln_im, vbelp_im, shkzg, elikz, kokrs, kzvbr, prctr, aufpl, aplzl, sakto, tcode2, usnam
    FOR ALL ENTRIES IN @lt_pur_543
      WHERE ebeln = @lt_pur_543-ebeln
        AND ebelp = @lt_pur_543-ebelp
        AND matnr = @lt_pur_543-matnr
        AND charg = @lt_pur_543-charg
        AND cpudt < @lt_pur_543-cpudt
        AND bwart IN ( @gc_541 ,  @gc_542 )
      APPENDING CORRESPONDING FIELDS OF TABLE @it_sub.

    FREE lt_pur_543.

    lt_pur_543[] = it_sub_pur_543[].
    SORT lt_pur_543 BY ebeln ebelp matnr charg cpudt cputm.
    DELETE ADJACENT DUPLICATES FROM lt_pur_543 COMPARING ebeln ebelp matnr charg cpudt cputm.

    SELECT FROM zsvbmmmatdocb
      FIELDS budat, charg, cpudt, cputm, mjahr, mblnr, zeile, bukrs, butxt, matnr, maktx, bwart, btext, mvttype, sobkz, werks, werksname,
             lgort, lgortname, umwrk, umwrkname, umlgo, umlgoname, bwtar, menge, erfme, erfmg, meins, lifnr, lifnrname, kunnr, kunnrname,
             hsdat, vfdat, lichn, zmaker, waterratio, lbbsa_sid, kzbew, bstaus_sg, bsttyp_sg, berid, waers, dmbtr,
             ebeln, ebelp, aufnr, aufps, rsnum, rspos, vbeln_im, vbelp_im, shkzg, elikz, kokrs, kzvbr, prctr, aufpl, aplzl, sakto, tcode2, usnam
    FOR ALL ENTRIES IN @lt_pur_543
      WHERE ebeln = @lt_pur_543-ebeln
        AND ebelp = @lt_pur_543-ebelp
        AND matnr = @lt_pur_543-matnr
        AND charg = @lt_pur_543-charg
        AND cpudt = @lt_pur_543-cpudt
        AND cputm <= @lt_pur_543-cputm
        AND bwart IN ( @gc_541 ,  @gc_542 )
      APPENDING CORRESPONDING FIELDS OF TABLE @it_sub.
    FREE lt_pur_543.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form top_down_pur_etc
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LT_SUB_PUR_ETC
*&      --> IT_SUB
*&---------------------------------------------------------------------*
FORM top_down_pur_etc TABLES it_sub_pur_etc LIKE gt_disp
                                    it_sub     TYPE tt_item.

  DATA(lt_pur_etc) = it_sub_pur_etc[].
  IF lt_pur_etc[] IS NOT INITIAL.

    SORT lt_pur_etc BY matnr charg cpudt.
    DELETE ADJACENT DUPLICATES FROM lt_pur_etc COMPARING matnr charg cpudt.

    SELECT FROM zsvbmmmatdocb
      FIELDS budat, charg, cpudt, cputm, mjahr, mblnr, zeile, bukrs, butxt, matnr, maktx, bwart, btext, mvttype, sobkz, werks, werksname,
             lgort, lgortname, umwrk, umwrkname, umlgo, umlgoname, bwtar, menge, erfme, erfmg, meins, lifnr, lifnrname, kunnr, kunnrname,
             hsdat, vfdat, lichn, zmaker, waterratio, lbbsa_sid, kzbew, bstaus_sg, bsttyp_sg, berid, waers, dmbtr,
             ebeln, ebelp, aufnr, aufps, rsnum, rspos, vbeln_im, vbelp_im, shkzg, elikz, kokrs, kzvbr, prctr, aufpl, aplzl, sakto, tcode2, usnam
    FOR ALL ENTRIES IN @lt_pur_etc
      WHERE  matnr = @lt_pur_etc-matnr
        AND charg = @lt_pur_etc-charg
        AND cpudt < @lt_pur_etc-cpudt
        AND bwart NOT IN ( @gc_541 ,  @gc_542,  @gc_543,  @gc_544 )
      APPENDING CORRESPONDING FIELDS OF TABLE @it_sub.

    FREE lt_pur_etc.

    lt_pur_etc[] = it_sub_pur_etc[].
    SORT lt_pur_etc BY matnr charg cpudt cputm.
    DELETE ADJACENT DUPLICATES FROM lt_pur_etc COMPARING matnr charg cpudt cputm.

    SELECT FROM zsvbmmmatdocb
      FIELDS budat, charg, cpudt, cputm, mjahr, mblnr, zeile, bukrs, butxt, matnr, maktx, bwart, btext, mvttype, sobkz, werks, werksname,
             lgort, lgortname, umwrk, umwrkname, umlgo, umlgoname, bwtar, menge, erfme, erfmg, meins, lifnr, lifnrname, kunnr, kunnrname,
             hsdat, vfdat, lichn, zmaker, waterratio, lbbsa_sid, kzbew, bstaus_sg, bsttyp_sg, berid, waers, dmbtr,
             ebeln, ebelp, aufnr, aufps, rsnum, rspos, vbeln_im, vbelp_im, shkzg, elikz, kokrs, kzvbr, prctr, aufpl, aplzl, sakto, tcode2, usnam
    FOR ALL ENTRIES IN @lt_pur_etc
      WHERE matnr = @lt_pur_etc-matnr
        AND charg = @lt_pur_etc-charg
        AND cpudt = @lt_pur_etc-cpudt
        AND cputm <= @lt_pur_etc-cputm
        AND bwart NOT IN ( @gc_541 ,  @gc_542,  @gc_543,  @gc_544 )
      APPENDING CORRESPONDING FIELDS OF TABLE @it_sub.
    FREE lt_pur_etc.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CALL_MIGO_DIALOG
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LS_DISP_MBLNR
*&      --> LS_DISP_MJAHR
*&---------------------------------------------------------------------*
FORM CALL_MIGO_DIALOG USING IV_MBLNR
                                IV_MJAHR.

  CONSTANTS: LC_A04   TYPE GOACTION VALUE 'A04',
             LC_R02   TYPE REFDOC   VALUE 'R02',
             LC_OK_GO TYPE OKCODE   VALUE 'OK_GO'.

  CHECK IV_MBLNR IS NOT INITIAL AND IV_MJAHR IS NOT INITIAL.

* 'MIGO_DIALOG’ 펑션 호출
  CALL FUNCTION 'MIGO_DIALOG'
    EXPORTING
      I_ACTION            = LC_A04
      I_REFDOC            = LC_R02
      I_NOTREE            = 'X'
      I_SKIP_FIRST_SCREEN = 'X'
      I_DEADEND           = 'X'
      I_OKCODE            = LC_OK_GO
      I_NEW_ROLLAREA      = 'X'
      I_MBLNR             = IV_MBLNR   "자재전표번호
      I_MJAHR             = IV_MJAHR   "전표년도
    EXCEPTIONS
      ILLEGAL_COMBINATION = 1
      OTHERS              = 2.

ENDFORM.
