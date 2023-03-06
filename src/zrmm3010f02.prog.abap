*&---------------------------------------------------------------------*
*& Include          ZRMM3010F02
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form POPUP_COND_HISTORY
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM popup_cond_history .

  DATA: LT_MAT TYPE TABLE OF TY_COND_HIST.

  DATA: LV_SDATE TYPE SY-DATUM.


  CLEAR: GT_ROWS, GT_COND_HIST.

  CALL METHOD GRF_GRID->GET_SELECTED_ROWS
    IMPORTING
      ET_INDEX_ROWS = GT_ROWS.
  DELETE GT_ROWS WHERE ROWTYPE IS NOT INITIAL.

  IF GT_ROWS IS INITIAL.
    MESSAGE I006 DISPLAY LIKE 'E'.  "선택된 데이타가 없습니다.
    EXIT.
  ENDIF.

*-----------------------------
* 조회 기간 설정
*-----------------------------
* 회사코드별 구매단가 이력조회 기간

  ZCL_MM_COMMON=>COMMON_CONFIG(
     EXPORTING IS_COMMON = VALUE #( M = 'C1' D = 'PMAST' S = 'PM002' )
                                    IT_WHERE = VALUE #( ( FIELD = 1 VALUE = P_BUKRS ) )
     IMPORTING ET_OUTTAB = DATA(LT_CONFIG) ).

  READ TABLE LT_CONFIG INTO DATA(LS_CONFIG) INDEX 1.

  GV_YEAR = LS_CONFIG-FIELD2.

  CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
    EXPORTING
      DATE      = SY-DATLO
      SIGNUM    = '-'
      DAYS      = 0
      MONTHS    = 0
      YEARS     = GV_YEAR
    IMPORTING
      CALC_DATE = LV_SDATE.

*-----------------------------
* 검색대상 자재 추출
*-----------------------------
  LOOP AT GT_ROWS INTO DATA(LS_ROWS).

    READ TABLE GT_DISP INTO DATA(LS_DISP) INDEX LS_ROWS-INDEX.

    IF SY-SUBRC EQ 0 AND LS_DISP-MATNR IS NOT INITIAL.
      LT_MAT = VALUE #( BASE LT_MAT ( MATNR = LS_DISP-MATNR ) ).
    ENDIF.

  ENDLOOP.

  SORT LT_MAT BY MATNR.
  DELETE ADJACENT DUPLICATES FROM LT_MAT COMPARING MATNR.

*-----------------------------
* 단가 이력 검색
*-----------------------------
  IF LT_MAT IS NOT INITIAL.

    SELECT INFNR,
           LIFNR,
           NAME1,
           MATNR,
           MATERIALDESCRIPTION AS MAKTX,
           PURCHASINGORGANIZATION AS EKORG,
           PURCHASINGINFORECORDCATEGORY AS ESOKZ,
           PLANT AS WERKS,
           CONDITIONRATEVALUE AS NETPR,
           CONDITIONQUANTITY AS PEINH,
           CONDITIONRATEVALUEUNIT AS WAERS,
           CONDITIONVALIDITYSTARTDATE AS DATAB,
           CONDITIONVALIDITYENDDATE AS DATBI
      FROM ZSVBMMINFOPRICE
       FOR ALL ENTRIES IN @LT_MAT
     WHERE MATNR = @LT_MAT-MATNR
       AND PURCHASINGORGANIZATION = @GV_EKORG
       AND CONDITIONVALIDITYSTARTDATE BETWEEN @LV_SDATE AND @SY-DATLO
       AND CONDITIONTYPE = @GC_COND_TYPE
       AND CONFIRM_PRICE = @GC_CONFIRM
      INTO CORRESPONDING FIELDS OF TABLE @GT_COND_HIST.

    SORT GT_COND_HIST BY MATNR DATAB DESCENDING.

    FREE LT_MAT.

  ENDIF.

  IF GT_COND_HIST IS INITIAL.
    MESSAGE S033 WITH TEXT-M01 DISPLAY LIKE 'E'.  "단가 이력이 존재하지 않습니다.
    EXIT.
  ENDIF.

  LOOP AT GT_COND_HIST ASSIGNING FIELD-SYMBOL(<LS_COND_HIST>).

    "구매정보레코드 범주
    PERFORM DOMAIN_VALUE_GET USING    'ESOKZ'
                                      <LS_COND_HIST>-ESOKZ
                             CHANGING <LS_COND_HIST>-ESOKZ_TEXT.

  ENDLOOP.

  CALL SCREEN '0200' STARTING AT 01 01.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form DOMAIN_VALUE_GET
*&---------------------------------------------------------------------*
FORM DOMAIN_VALUE_GET USING IV_DOMNAME
                               IV_DOMVALUE
                      CHANGING CV_DDTEXT.

  DATA: LV_DOMNAME  TYPE DD07V-DOMNAME,
        LV_DOMVALUE TYPE DD07V-DOMVALUE_L,
        LV_DDTEXT   TYPE DD07V-DDTEXT.

  CLEAR CV_DDTEXT.

  LV_DOMNAME  = IV_DOMNAME.
  LV_DOMVALUE = IV_DOMVALUE.

  CALL FUNCTION 'DOMAIN_VALUE_GET'
    EXPORTING
      I_DOMNAME  = LV_DOMNAME
      I_DOMVALUE = LV_DOMVALUE
    IMPORTING
      E_DDTEXT   = LV_DDTEXT
    EXCEPTIONS
      NOT_EXIST  = 1
      OTHERS     = 2.

  IF SY-SUBRC = 0.
    CV_DDTEXT = LV_DDTEXT.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form EVT_GRID_TOOLBAR
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- CT_ADD_TOOLBAR
*&---------------------------------------------------------------------*
FORM EVT_GRID_TOOLBAR CHANGING CT_TOOLBAR TYPE TTB_BUTTON.

  DEFINE _L_ADD_TOOLBAR.
    ls_add_toolbar-function    = &1.
    ls_add_toolbar-icon        = &2.
    ls_add_toolbar-quickinfo   = &3.
    ls_add_toolbar-butn_type   = &4.
    ls_add_toolbar-disabled    = &5.
    ls_add_toolbar-text        = &6.

    APPEND ls_add_toolbar TO ct_toolbar.
  END-OF-DEFINITION.

*----------------------------
*-- 추가 User Toolbar
*----------------------------
  DATA:LS_ADD_TOOLBAR TYPE STB_BUTTON.

  _L_ADD_TOOLBAR: 'BTN_HIST'  ICON_HISTORY  TEXT-U01  ''  ''  TEXT-U01.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form EVT_GRID_DOUBLE_CLICK
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> E_ROW_INDEX
*&      --> E_COLUMN
*&---------------------------------------------------------------------*
FORM EVT_GRID_DOUBLE_CLICK USING IV_ROW
                                 IV_COLUMN.

  READ TABLE GT_DISP INTO DATA(LS_DISP) INDEX IV_ROW.

  CASE IV_COLUMN.
    WHEN 'STATU'.
*-----------------------------
* Display Error Msg Table
*-----------------------------
      GRF_GRID->SHOW_MSGTB( IT_MSGTB = LS_DISP-MSGTB ).

    WHEN 'BANFN'.
      CHECK LS_DISP-BANFN IS NOT INITIAL.

      SET PARAMETER ID 'BAN' FIELD LS_DISP-BANFN.
      CALL TRANSACTION 'ME53N' AND SKIP FIRST SCREEN.

    WHEN 'EBELN'.
      CHECK LS_DISP-EBELN IS NOT INITIAL.

      CASE LS_DISP-BSTYP_PO.
        WHEN 'F'. "구매오더
          SET PARAMETER ID 'BES' FIELD LS_DISP-EBELN.
          CALL TRANSACTION 'ME23N' AND SKIP FIRST SCREEN.

        WHEN 'K'. "계약
          SET PARAMETER ID 'CTR' FIELD LS_DISP-EBELN.
          CALL TRANSACTION 'ME33K' AND SKIP FIRST SCREEN.

        WHEN OTHERS.
      ENDCASE.

    WHEN 'SALESORDER'.
      CHECK LS_DISP-SALESORDER IS NOT INITIAL.

      SET PARAMETER ID 'AUN' FIELD LS_DISP-SALESORDER.
      CALL TRANSACTION 'VA03' AND SKIP FIRST SCREEN.

    WHEN 'MATNR'.
      CHECK LS_DISP-MATNR IS NOT INITIAL.

      SET PARAMETER ID 'MAT' FIELD LS_DISP-MATNR.
      SET PARAMETER ID 'WRK' FIELD LS_DISP-WERKS.
      SET PARAMETER ID 'MXX' FIELD 'E'.               "Purchasing View (Basic View: "K" without WERKS)
      CALL TRANSACTION 'MM03' AND SKIP FIRST SCREEN.

    WHEN OTHERS.
      RETURN.

  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form EVT_HIST_DOUBLE_CLICK
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> E_ROW_INDEX
*&      --> E_COLUMN
*&---------------------------------------------------------------------*
FORM EVT_HIST_DOUBLE_CLICK USING IV_ROW
                                 IV_COLUMN.

  READ TABLE GT_COND_HIST INTO DATA(LS_COND_HIST) INDEX IV_ROW.

  CASE IV_COLUMN.
    WHEN 'INFNR'.
      CHECK LS_COND_HIST-INFNR IS NOT INITIAL.

      SET PARAMETER ID 'LIF' FIELD LS_COND_HIST-LIFNR.
      SET PARAMETER ID 'MAT' FIELD LS_COND_HIST-MATNR.
      SET PARAMETER ID 'INF' FIELD LS_COND_HIST-INFNR.
      SET PARAMETER ID 'EKO' FIELD LS_COND_HIST-EKORG.
      SET PARAMETER ID 'WRK' FIELD LS_COND_HIST-WERKS.
      SET PARAMETER ID 'ESO' FIELD LS_COND_HIST-ESOKZ.
      CALL TRANSACTION 'ME13' AND SKIP FIRST SCREEN.

    WHEN OTHERS.
      RETURN.

  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form EVT_BOM_DOUBLE_CLICK
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> E_ROW_INDEX
*&      --> E_COLUMN
*&---------------------------------------------------------------------*
FORM EVT_BOM_DOUBLE_CLICK USING IV_ROW
                                IV_COLUMN.

  READ TABLE GT_TOLL_MANUF INTO DATA(LS_TOLL_MANUF) INDEX IV_ROW.

  CASE IV_COLUMN.
    WHEN 'MATNR'.
      CHECK LS_TOLL_MANUF-MATNR IS NOT INITIAL.

      SET PARAMETER ID 'MAT' FIELD LS_TOLL_MANUF-MATNR.
      SET PARAMETER ID 'WRK' FIELD LS_TOLL_MANUF-WERKS.
      SET PARAMETER ID 'MXX' FIELD 'E'.               "Purchasing View (Basic View: "K" without WERKS)
      CALL TRANSACTION 'MM03' AND SKIP FIRST SCREEN.

    WHEN OTHERS.
      RETURN.

  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form EVT_GRID_BUTTON_CLICK
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> ES_COL_ID
*&      --> ES_ROW_NO
*&---------------------------------------------------------------------*
FORM EVT_GRID_BUTTON_CLICK USING IS_COL_ID TYPE LVC_S_COL
                                 IS_ROW_NO TYPE LVC_S_ROID.

*  DATA: LT_APVKEY  TYPE TABLE OF ZSCN00219,
  DATA: LV_RESULT  TYPE BAPI_MTYPE,
        LV_MESSAGE TYPE BAPI_MSG.

  READ TABLE GT_DISP INTO DATA(LS_DISP) INDEX IS_ROW_NO-ROW_ID.

  CHECK SY-SUBRC EQ 0.

  CASE IS_COL_ID-FIELDNAME.
*    WHEN 'APV_STAT'.
*      READ TABLE GT_APV WITH KEY WFOBJECT = LS_DISP-BANFN
*                                 BINARY SEARCH
*                                 TRANSPORTING NO FIELDS.
*
*      IF SY-SUBRC = 0.
*        LOOP AT GT_APV INTO DATA(LS_APV) FROM SY-TABIX.
*          IF LS_APV-WFOBJECT NE LS_DISP-BANFN.
*            EXIT.
*          ENDIF.
*
*          LT_APVKEY = VALUE #( BASE LT_APVKEY ( APVIFKEY = LS_APV-APVIFKEY ) ).
*        ENDLOOP.
*
*        call function 'ZFCN_APV_STATUS_MONI'
*          EXPORTING
*            IV_BUKRS   = LS_DISP-BUKRS
*          IMPORTING
*            EV_RESULT  = LV_RESULT
*            EV_MESSAGE = LV_MESSAGE
*          TABLES
*            IT_APVKEY  = LT_APVKEY.
*
*        IF LV_RESULT EQ 'E'.
*          MESSAGE I000 WITH LV_MESSAGE DISPLAY LIKE 'E'.
*        ENDIF.
*      ENDIF.

    WHEN 'TOLL_MANUF'.
      PERFORM POPUP_TOLL_MANUF USING LS_DISP.

    WHEN 'AFILE'.
      DATA(LS_SIBFLPORB) = VALUE SIBFLPORB( ).

      LS_SIBFLPORB = VALUE #( INSTID = LS_DISP-BANFN
                              TYPEID = GC_GOS_TYPEID
                              CATID  = GC_GOS_CATID ).

      CALL FUNCTION 'GOS_ATTACHMENT_LIST_POPUP'
        EXPORTING
          IS_OBJECT = LS_SIBFLPORB
          IP_MODE   = 'D'.         "Create/Display/Edit

    WHEN OTHERS.
  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form POPUP_TOLL_MANUF
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LS_DISP
*&---------------------------------------------------------------------*
FORM POPUP_TOLL_MANUF USING IS_DISP TYPE TY_DISP.

  CLEAR: GT_TOLL_MANUF.

*-----------------------------
* 임가공 BOM 조회
*-----------------------------
  SELECT A~MATERIAL                 AS MATNR,
         T~MAKTX,
         A~REQUIREDQUANTITY         AS BDMNG,
         A~ENTRYUNIT                AS ERFME,
         A~PLANT                    AS WERKS,
         A~STORAGELOCATION          AS LGORT,
         A~REQUIREMENTDATE          AS BDTER,
         A~BILLOFMATERIALITEMNUMBER AS POSNR,
         A~BATCH                    AS CHARG,
         A~COMPONENTSCRAPINPERCENT  AS AUSCH,
         A~PURCHASEREQUISITION      AS BANFN,
         A~PURCHASEREQUISITIONITEM  AS BNFPO
    FROM IPPSUBCONTRCOMP AS A INNER JOIN MAKT AS T
                                      ON T~SPRAS = @SY-LANGU
                                     AND A~MATERIAL = T~MATNR
   WHERE A~PURCHASEREQUISITION     = @IS_DISP-BANFN
     AND A~PURCHASEREQUISITIONITEM = @IS_DISP-BNFPO
    INTO CORRESPONDING FIELDS OF TABLE @GT_TOLL_MANUF.

  SORT GT_TOLL_MANUF BY POSNR.

  IF GT_TOLL_MANUF IS INITIAL.
    MESSAGE S005 WITH 'BOM'(m02) DISPLAY LIKE 'E'.  "단가 이력이 존재하지 않습니다.
    EXIT.
  ENDIF.

  CALL SCREEN '0300' STARTING AT 01 01.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form ALV_GRID_FCAT_MODIFY
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- CT_FCAT
*&---------------------------------------------------------------------*
FORM ALV_GRID_FCAT_MODIFY CHANGING CT_FCAT TYPE LVC_T_FCAT.

  DEFINE _L_SET_FCAT.
    <ls_fcat>-col_pos    = &1.
    <ls_fcat>-key        = &2.
    <ls_fcat>-edit       = &3.
    <ls_fcat>-coltext    = &4.
    <ls_fcat>-just       = &5.
    <ls_fcat>-f4availabl = &6.
    <ls_fcat>-cfieldname = &7.
    <ls_fcat>-qfieldname = &8.
    <ls_fcat>-parameter0 = &9.  "PO관련 필드 'P' (개요 선택시 숨김, PR 기준 중복라인 Data 숨김) / 'S' (개요 선택시에만  출력)
*    <ls_fcat>-OUTPUTLEN  = &10.
    <ls_fcat>-col_opt    = 'X'.
  END-OF-DEFINITION.

  SORT CT_FCAT BY FIELDNAME.

*U3> T1 전용 필드 활성화 - START
  IF GV_ORG1 = 'T2'.
    DATA(LV_T2_TECH) = 'X'.
  ELSE.
    CLEAR LV_T2_TECH.
  ENDIF.
*U3> T1 전용 필드 활성화 - END

*----------------------------------------
* Set Modify Field..
*----------------------------------------
  LOOP AT CT_FCAT ASSIGNING FIELD-SYMBOL(<LS_FCAT>).

    CASE <LS_FCAT>-FIELDNAME.
      WHEN 'APV_STAT'.  "결재진행
        _L_SET_FCAT: 01  'X'  ''  TEXT-C71  'C'  ''  ''  ''  ''.
      WHEN 'STATUS'.  "발주상태
        _L_SET_FCAT: 02  'X'  ''  TEXT-C01  'C'  ''  ''  ''  ''.
*U3> 외부적인 표시는 언어별로/내부적으로는 한글로 - START
*      WHEN 'WF_STAUS'.  "PR결재상태
      WHEN 'WF_STAUS_TEXT'.  "PR결재상태
*U3> 외부적인 표시는 언어별로/내부적으로는 한글로 - END
        _L_SET_FCAT: 03  'X'  ''  TEXT-C02  ''  ''  ''  ''  ''.
      WHEN 'BSART_PR'.  "PR유형
        _L_SET_FCAT: 04  'X'  ''  TEXT-C03  ''  ''  ''  ''  ''.
      WHEN 'BANFN'.  "구매요청
        _L_SET_FCAT: 05  'X'  ''  TEXT-C04  ''  ''  ''  ''  ''.
      WHEN 'BNFPO'.  "품목
        _L_SET_FCAT: 06  'X'  ''  TEXT-C05  ''  ''  ''  ''  ''.
      WHEN 'MATNR'.  "자재
        _L_SET_FCAT: 07  'X'  ''  TEXT-C06  ''  ''  ''  ''  ''.
      WHEN 'TXZ01'.  "내역
        _L_SET_FCAT: 08  'X'  ''  TEXT-C07  ''  ''  ''  ''  ''.
      WHEN 'MENGE_PR'.  "요청수량
        _L_SET_FCAT: 09  ''  ''  TEXT-C08  ''  ''  ''  'MEINS_PR'  ''.
      WHEN 'MEINS_PR'.  "단위
        _L_SET_FCAT: 10  ''  ''  TEXT-C09  ''  ''  ''  ''  ''.
      WHEN 'TOLL_MANUF'.  "임가공
        _L_SET_FCAT: 11  ''  ''  TEXT-C10  ''  ''  ''  ''  ''.
      WHEN 'BSMNG'.  "총PO수량
        _L_SET_FCAT: 12  ''  ''  TEXT-C11  ''  ''  ''  'MEINS_PR'  ''.
      WHEN 'GR_QTY'.  "총입고
        _L_SET_FCAT: 13  ''  ''  TEXT-C12  ''  ''  ''  'MEINS_PR'  ''.
      WHEN 'VALID_COND'.  "유효단가 유무
        _L_SET_FCAT: 14  ''  ''  TEXT-C13  'C'  ''  ''  ''  ''.
        <LS_FCAT>-CHECKBOX = 'X'.
      WHEN 'WERKS'.  "플랜트
        _L_SET_FCAT: 15  ''  ''  TEXT-C14  ''  ''  ''  ''  ''.

      WHEN 'WF_STATUS'.  "PO결재상태
        _L_SET_FCAT: 20  ''  ''  TEXT-C15  ''  ''  ''  ''  'P'.
      WHEN 'BSART_PO'.  "PO유형
        _L_SET_FCAT: 21  ''  ''  TEXT-C16  ''  ''  ''  ''  'P'.
      WHEN 'EBELN'.  "구매오더
        _L_SET_FCAT: 22  ''  ''  TEXT-C17  ''  ''  ''  ''  'P'.
      WHEN 'EBELP'.  "품목
        _L_SET_FCAT: 23  ''  ''  TEXT-C05  ''  ''  ''  ''  'P'.
      WHEN 'MENGE_PO'.  "PO수량
        _L_SET_FCAT: 24  ''  ''  TEXT-C18  ''  ''  ''  'MEINS_PO'  'P'.
      WHEN 'MEINS_PO'.  "PO단위
        _L_SET_FCAT: 25  ''  ''  TEXT-C19  ''  ''  ''  ''  'P'.
      WHEN 'NETPR'.  "구매단가
        _L_SET_FCAT: 26  ''  ''  TEXT-C20  ''  ''  'WAERS'  ''  'P'.
      WHEN 'WAERS'.  "통화
        _L_SET_FCAT: 27  ''  ''  TEXT-C21  ''  ''  ''  ''  'P'.
      WHEN 'SUM_GRQTY'.  "PO별 입고
        _L_SET_FCAT: 28  ''  ''  TEXT-C22  ''  ''  ''  'MEINS_PO'  'P'.

      WHEN 'BADAT'.  "요청일
        _L_SET_FCAT: 40  ''  ''  TEXT-C23  ''  ''  ''  ''  ''.
      WHEN 'ZPEQ_DEPARTMENT'.  "요청부서
        _L_SET_FCAT: 41  ''  ''  TEXT-C24  ''  ''  ''  ''  ''.
      WHEN 'DEPART_NAME_PR'.  "요청부서명
        _L_SET_FCAT: 42  ''  ''  TEXT-C25  ''  ''  ''  ''  ''.
      WHEN 'ZREQUESTER'.  "요청자
        _L_SET_FCAT: 43  ''  ''  TEXT-C26  ''  ''  ''  ''  ''.
      WHEN 'EMPLOY_NAME_PR'.  "요청자명
        _L_SET_FCAT: 44  ''  ''  TEXT-C27  ''  ''  ''  ''  ''.
      WHEN 'ZPRTITLE'.  "요청명
        _L_SET_FCAT: 45  ''  ''  TEXT-C28  ''  ''  ''  ''  ''.
      WHEN 'BATXT'.  "문서유형내역
        _L_SET_FCAT: 46  ''  ''  TEXT-C29  ''  ''  ''  ''  ''.
      WHEN 'PEINH'.  "가격단위
        _L_SET_FCAT: 46  ''  ''  TEXT-C64  ''  ''  ''  ''  ''.
      WHEN 'PREIS'.  "추정단가
        _L_SET_FCAT: 47  ''  ''  TEXT-C30  ''  ''  'PUR_CURR'  ''  ''.
      WHEN 'BAPRE'.  "추정금액
        _L_SET_FCAT: 48  ''  ''  TEXT-C31  ''  ''  'PUR_CURR'  ''  ''.
      WHEN 'PUR_CURR'.  "통화
        _L_SET_FCAT: 49  ''  ''  TEXT-C21  ''  ''  ''  ''  ''.
      WHEN 'LFDAT'.  "납품일
        _L_SET_FCAT: 50  ''  ''  TEXT-C32  ''  ''  ''  ''  ''.
      WHEN 'PLANTNAME'.  "플랜트명
        _L_SET_FCAT: 51  ''  ''  TEXT-C33  ''  ''  ''  ''  ''.
      WHEN 'LGORT'.  "저장위치
        _L_SET_FCAT: 52  ''  ''  TEXT-C34  ''  ''  ''  ''  ''.
      WHEN 'LGOBE'.  "저장위치명
        _L_SET_FCAT: 53  ''  ''  TEXT-C35  ''  ''  ''  ''  ''.
      WHEN 'LIFNR'.  "희망공급업체
        _L_SET_FCAT: 54  ''  ''  TEXT-C36  ''  ''  ''  ''  'S'.
      WHEN 'LIFNR_TEXT'.  "업체명
        _L_SET_FCAT: 55  ''  ''  TEXT-C37  ''  ''  ''  ''  'S'.
      WHEN 'FLIEF'.  "고정공급업체
        _L_SET_FCAT: 56  ''  ''  TEXT-C38  ''  ''  ''  ''  'S'.
      WHEN 'FLIEF_TEXT'.  "업체명
        _L_SET_FCAT: 57  ''  ''  TEXT-C37  ''  ''  ''  ''  'S'.
      WHEN 'EKGRP'.  "구매그룹
        _L_SET_FCAT: 58  ''  ''  TEXT-C39  ''  ''  ''  ''  ''.
      WHEN 'EKNAM'.  "구매그룹명
        _L_SET_FCAT: 59  ''  ''  TEXT-C40  ''  ''  ''  ''  ''.
      WHEN 'ZORDER_PERSON'.  "계약담당자
        _L_SET_FCAT: 60  ''  ''  TEXT-C41  ''  ''  ''  ''  ''.
      WHEN 'EMPLOY_NAME_PO'.  "계약담당자명
        _L_SET_FCAT: 61  ''  ''  TEXT-C42  ''  ''  ''  ''  ''.
      WHEN 'ZORDER_DEPARTMENT'.  "계약부서
        _L_SET_FCAT: 62  ''  ''  TEXT-C43  ''  ''  ''  ''  ''.
      WHEN 'DEPART_NAME_PO'.  "계약부서명
        _L_SET_FCAT: 63  ''  ''  TEXT-C44  ''  ''  ''  ''  ''.
      WHEN 'MATKL'.  "자재그룹
        _L_SET_FCAT: 64  ''  ''  TEXT-C45  ''  ''  ''  ''  ''.
      WHEN 'MATKL_NAME'.  "자재그룹명
        _L_SET_FCAT: 65  ''  ''  TEXT-C46  ''  ''  ''  ''  ''.
      WHEN 'ZNOPRICE'.  "미단가계약
        _L_SET_FCAT: 66  ''  ''  TEXT-C47  ''  ''  ''  ''  ''.
      WHEN 'ZURGENT'.  "긴급여부
        _L_SET_FCAT: 67  ''  ''  TEXT-C48  'C'  ''  ''  ''  ''.
      WHEN 'ZPRE_INPUT'.  "선투입여부
        _L_SET_FCAT: 68  ''  ''  TEXT-C49  'C'  ''  ''  ''  ''.

      WHEN 'SALESORDER'.  "판매오더
        _L_SET_FCAT: 71  ''  ''  TEXT-C50  ''  ''  ''  ''  ''.
      WHEN 'SALESORDERITEM'.  "항목
        _L_SET_FCAT: 72  ''  ''  TEXT-C51  ''  ''  ''  ''  ''.
      WHEN 'GLACCOUNT'.  "계정
        _L_SET_FCAT: 73  ''  ''  TEXT-C52  ''  ''  ''  ''  ''.
      WHEN 'COSTCENTER'.  "코스트센터
        _L_SET_FCAT: 74  ''  ''  TEXT-C53  ''  ''  ''  ''  ''.
      WHEN 'FIXEDASSET'.  "고정자산
        _L_SET_FCAT: 75  ''  ''  TEXT-C54  ''  ''  ''  ''  ''.
      WHEN 'ORDERID'.  "ORDER
        _L_SET_FCAT: 76  ''  ''  TEXT-C55  ''  ''  ''  ''  ''.
      WHEN 'WBSELEMENT'.  "WBS
        _L_SET_FCAT: 77  ''  ''  TEXT-C56  ''  ''  ''  ''  ''.
      WHEN 'ZRECEIPT'.  "SRM/타시스템 전송
        _L_SET_FCAT: 81  ''  ''  TEXT-C57  'C'  ''  ''  ''  ''.
        <LS_FCAT>-TECH = LV_T2_TECH. "U3> T1 전용
      WHEN 'AFILE'.  "첨부문서
        _L_SET_FCAT: 82  ''  ''  TEXT-C58  ''  ''  ''  ''  ''.
      WHEN 'ZMRO_CATALOG'.  "MRO Catalog
        _L_SET_FCAT: 83  ''  ''  TEXT-C75  ''  ''  ''  ''  ''.
        <LS_FCAT>-TECH = LV_T2_TECH. "U3> T1 전용
      WHEN 'ZMRO_CATALOG_DE'.  "MRO Catalog내역
        _L_SET_FCAT: 84  ''  ''  TEXT-C76  ''  ''  ''  ''  ''.
        <LS_FCAT>-TECH = LV_T2_TECH. "U3> T1 전용
      WHEN 'ZMROP'.  "MRO단가
        _L_SET_FCAT: 85  ''  ''  TEXT-C77  ''  ''  'MRO_WAERS'  ''  ''. "U1 수정 22.08.18
        <LS_FCAT>-TECH = LV_T2_TECH. "U3> T1 전용
      WHEN 'ICON'.    "MRO단가(2개)
        _L_SET_FCAT: 86   ''  '' TEXT-C72  'C' ''  ''  ''  ''.
        <LS_FCAT>-TECH = LV_T2_TECH. "U3> T1 전용

      WHEN OTHERS.
        <LS_FCAT>-TECH = 'X'.
    ENDCASE.

* PO 관련 필드는 개요 선택 시 숨김
    CASE <LS_FCAT>-PARAMETER0.
      WHEN 'P' OR 'H'.
        IF P_RA = 'X'.
          <LS_FCAT>-TECH = 'X'.
        ELSE.
          <LS_FCAT>-EMPHASIZE = GC_EMPHSZ_C300.
        ENDIF.

      WHEN 'S'.
        IF P_RB = 'X'.
          <LS_FCAT>-TECH = 'X'.
        ENDIF.
    ENDCASE.

    <LS_FCAT>-SCRTEXT_S = <LS_FCAT>-COLTEXT.
    <LS_FCAT>-SCRTEXT_M = <LS_FCAT>-COLTEXT.
    <LS_FCAT>-SCRTEXT_L = <LS_FCAT>-COLTEXT.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form ALV_BOM_FCAT_MODIFY
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- CT_FCAT
*&---------------------------------------------------------------------*
FORM ALV_BOM_FCAT_MODIFY CHANGING CT_FCAT TYPE LVC_T_FCAT.

  DEFINE _L_SET_FCAT.
    <ls_fcat>-col_pos    = &1.
    <ls_fcat>-key        = &2.
    <ls_fcat>-edit       = &3.
    <ls_fcat>-coltext    = &4.
    <ls_fcat>-just       = &5.
    <ls_fcat>-f4availabl = &6.
    <ls_fcat>-cfieldname = &7.
    <ls_fcat>-qfieldname = &8.
*    <LS_FCAT>-OUTPUTLEN  = &9.
    <ls_fcat>-col_opt    = 'X'.
  END-OF-DEFINITION.

  SORT CT_FCAT BY FIELDNAME.

*----------------------------------------
* Set Modify Field..
*----------------------------------------
  LOOP AT CT_FCAT ASSIGNING FIELD-SYMBOL(<LS_FCAT>).

    CASE <LS_FCAT>-FIELDNAME.
      WHEN 'MATNR'. "자재
        _L_SET_FCAT: 01  'X'  ''  TEXT-C06  ''  ''  ''  ''.
      WHEN 'MAKTX'. "자재내역
        _L_SET_FCAT: 02  ''  ''  TEXT-C07  ''  ''  ''  ''.
      WHEN 'BDMNG'. "소요량
        _L_SET_FCAT: 03  ''  ''  TEXT-C67  'R'  ''  ''  'ERFME'.
      WHEN 'ERFME'. "단위
        _L_SET_FCAT: 04  ''  ''  TEXT-C09  ''  ''  ''  ''.
      WHEN 'WERKS'. "플랜트
        _L_SET_FCAT: 05  ''  ''  TEXT-C14  ''  ''  ''  ''.
      WHEN 'LGORT'. "저장위치
        _L_SET_FCAT: 06  ''  ''  TEXT-C34  ''  ''  ''  ''.
      WHEN 'BDTER'. "소요일
        _L_SET_FCAT: 07  ''  ''  TEXT-C68  ''  ''  ''  ''.
      WHEN 'POSNR'. "항목
        _L_SET_FCAT: 08  ''  ''  TEXT-C51  ''  ''  ''  ''.
      WHEN 'CHARG'. "배치
        _L_SET_FCAT: 09  ''  ''  TEXT-C69  ''  ''  ''  ''.
      WHEN 'AUSCH'. "구성품 스크랩
        _L_SET_FCAT: 10  ''  ''  TEXT-C70  ''  ''  ''  ''.

      WHEN OTHERS.
        <LS_FCAT>-TECH = 'X'.
    ENDCASE.

    <LS_FCAT>-SCRTEXT_S = <LS_FCAT>-COLTEXT.
    <LS_FCAT>-SCRTEXT_M = <LS_FCAT>-COLTEXT.
    <LS_FCAT>-SCRTEXT_L = <LS_FCAT>-COLTEXT.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form ALV_HIST_FCAT_MODIFY
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- CT_FCAT
*&---------------------------------------------------------------------*
FORM ALV_HIST_FCAT_MODIFY CHANGING CT_FCAT TYPE LVC_T_FCAT.

  CONSTANTS: LC_REF_TABLE TYPE TABNAME VALUE 'ZSVBMMPRICE'.

  DEFINE _L_SET_FCAT.
    <ls_fcat>-col_pos    = &1.
    <ls_fcat>-key        = &2.
    <ls_fcat>-no_out     = &3.
    <ls_fcat>-coltext    = &4.
    <ls_fcat>-just       = &5.
    <ls_fcat>-f4availabl = &6.
    <ls_fcat>-cfieldname = &7.
    <ls_fcat>-qfieldname = &8.
*    <LS_FCAT>-OUTPUTLEN  = &9.
    <ls_fcat>-col_opt    = 'X'.
    <ls_fcat>-ref_table  = lc_ref_table.
  END-OF-DEFINITION.

  SORT CT_FCAT BY FIELDNAME.

*----------------------------------------
* Set Modify Field..
*----------------------------------------
  LOOP AT CT_FCAT ASSIGNING FIELD-SYMBOL(<LS_FCAT>).

    CASE <LS_FCAT>-FIELDNAME.
      WHEN 'INFNR'. "구매정보레코드
        _L_SET_FCAT: 01  'X'  ''  TEXT-C59  ''  ''  ''  ''.
      WHEN 'ESOKZ'. "범주
        _L_SET_FCAT: 02  ''  'X'  TEXT-C60  'C'  ''  ''  ''.
      WHEN 'ESOKZ_TEXT'. "범주
        _L_SET_FCAT: 02  ''  ''  TEXT-C60  ''  ''  ''  ''.
      WHEN 'LIFNR'. "공급업체
        _L_SET_FCAT: 03  ''  ''  TEXT-C61  ''  ''  ''  ''.
      WHEN 'NAME1'. "업체명
        _L_SET_FCAT: 04  ''  ''  TEXT-C37  ''  ''  ''  ''.
      WHEN 'MATNR'. "자재
        _L_SET_FCAT: 05  ''  ''  TEXT-C06  ''  ''  ''  ''.
      WHEN 'MAKTX'. "자재내역
        _L_SET_FCAT: 06  ''  ''  TEXT-C07  ''  ''  ''  ''.
      WHEN 'EKORG'. "구매조직
        _L_SET_FCAT: 07  ''  ''  TEXT-C62  ''  ''  ''  ''.
      WHEN 'WERKS'. "플랜트
        _L_SET_FCAT: 08  ''  ''  TEXT-C14  ''  ''  ''  ''.
      WHEN 'NETPR'. "단가
        _L_SET_FCAT: 09  ''  ''  TEXT-C63  ''  ''  'WAERS'  ''.
      WHEN 'PEINH'. "가격단위
        _L_SET_FCAT: 10  ''  ''  TEXT-C64  ''  ''  ''  ''.
      WHEN 'WAERS'. "통화
        _L_SET_FCAT: 11  ''  ''  TEXT-C21  ''  ''  ''  ''.
      WHEN 'DATAB'. "유효 시작일
        _L_SET_FCAT: 12  ''  ''  TEXT-C65  ''  ''  ''  ''.
      WHEN 'DATBI'. "유효 종료일
        _L_SET_FCAT: 13  ''  ''  TEXT-C66  ''  ''  ''  ''.

      WHEN OTHERS.
        <LS_FCAT>-TECH = 'X'.
    ENDCASE.

    <LS_FCAT>-SCRTEXT_S = <LS_FCAT>-COLTEXT.
    <LS_FCAT>-SCRTEXT_M = <LS_FCAT>-COLTEXT.
    <LS_FCAT>-SCRTEXT_L = <LS_FCAT>-COLTEXT.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form ALV_GRID_SET_LINE_STYLE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> IT_FCAT
*&---------------------------------------------------------------------*
FORM ALV_GRID_SET_LINE_STYLE USING IT_FCAT.

  DATA : LT_LVC_STYL TYPE LVC_T_STYL,
         LV_INDEX    TYPE I.

  LOOP AT GT_DISP INTO DATA(LS_DISP).
    LV_INDEX = SY-TABIX.

    CLEAR LT_LVC_STYL[].

*---------------------------
* Set Field Style..
*---------------------------
    PERFORM SET_FIELD_CELLTAB USING    IT_FCAT
                              CHANGING LT_LVC_STYL LS_DISP.

*-- Insert Style Talble
    CLEAR LS_DISP-CELLS.
    INSERT LINES OF LT_LVC_STYL INTO TABLE LS_DISP-CELLS.

*-- Modify Line..
    MODIFY GT_DISP FROM LS_DISP INDEX LV_INDEX.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_FIELD_CELLTAB
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> IT_FCAT
*&      <-- LT_LVC_STYL
*&      <-- LS_DISP
*&---------------------------------------------------------------------*
FORM SET_FIELD_CELLTAB USING IT_FCAT TYPE LVC_T_FCAT
                       CHANGING CT_STYL TYPE LVC_T_STYL
                                CS_DISP TYPE TY_DISP.

  DATA : LS_LVC_STYL TYPE LVC_S_STYL.

  CONSTANTS: LC_DISABLE TYPE RAW4 VALUE '00060000'.

  LOOP AT IT_FCAT INTO DATA(LS_FIELDCAT).
    CLEAR LS_LVC_STYL.

    LS_LVC_STYL-FIELDNAME = LS_FIELDCAT-FIELDNAME.

* PUSH BUTTON STYLE 구성
    CASE LS_LVC_STYL-FIELDNAME.
*      WHEN 'APV_STAT'.
*        IF CS_DISP-APV_STAT IS NOT INITIAL.
*          LS_LVC_STYL-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_BUTTON.
*        ENDIF.

      WHEN 'TOLL_MANUF'.
        IF CS_DISP-TOLL_MANUF IS NOT INITIAL.
          LS_LVC_STYL-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_BUTTON.
        ENDIF.

      WHEN 'AFILE'.
        IF CS_DISP-AFILE IS NOT INITIAL.
          LS_LVC_STYL-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_BUTTON.
        ENDIF.

      WHEN OTHERS.
    ENDCASE.

* PR 기준 중복라인 Data 숨김
    IF LS_FIELDCAT-PARAMETER0 <> 'P'.
      IF CS_DISP-ZDUPL = 'X'.
        LS_LVC_STYL-STYLE = LC_DISABLE.
      ENDIF.
    ENDIF.

    INSERT LS_LVC_STYL INTO TABLE CT_STYL.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_GRID
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM SET_GRID.

  IF GRF_DOCKING_CON IS INITIAL.

* Creating Docking container instance
    PERFORM CREATE_CONTAINER.
*--------------------------------
* Create Alv Grid
*--------------------------------
    PERFORM CREATE_ALV_GRID.

*--------------------------------
* Dislay Grid..
*--------------------------------
    GRF_GRID->SET_GRID( CHANGING  CT_DATA = GT_DISP ).

  ELSE.
    GRF_GRID->REFRESH_GRID_DISPLAY( ).
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CREATE_CONTAINER
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM CREATE_CONTAINER.

*----------------------------------------------------
* Create Docking Container..
*----------------------------------------------------
  CREATE OBJECT GRF_DOCKING_CON
    EXPORTING
      REPID     = SY-REPID    "프로그램명 id
      DYNNR     = SY-DYNNR    "화면번호(Screen)
      SIDE      = GRF_DOCKING_CON->DOCK_AT_TOP
      EXTENSION = 10000.

*----------------------------------------------------
* Split Container (1 Row:header 2 Row: ALV Grid)
*----------------------------------------------------
  DATA(LRF_SPLITTER) = NEW CL_GUI_SPLITTER_CONTAINER( PARENT  = GRF_DOCKING_CON
                                                      NO_AUTODEF_PROGID_DYNNR = 'X'
                                                      ROWS    = 2
                                                      COLUMNS = 1 ).
  LRF_SPLITTER->SET_ROW_MODE( MODE = CL_GUI_SPLITTER_CONTAINER=>TYPE_MOVABLE ).
  LRF_SPLITTER->SET_ROW_HEIGHT( ID = 1 HEIGHT = 100 ).
  LRF_SPLITTER->SET_BORDER( BORDER = SPACE ).

*--------------------------------
* Set Header Container
*--------------------------------
  DATA(LRF_CONT) = LRF_SPLITTER->GET_CONTAINER( ROW = 1 COLUMN = 1 ).

  DATA(LRF_SPLITTER_HTML) = NEW CL_GUI_SPLITTER_CONTAINER( PARENT  = LRF_CONT
                                                           NO_AUTODEF_PROGID_DYNNR = 'X'
                                                           ROWS    = 1
                                                           COLUMNS = 1 ).
  GRF_HEAD = LRF_SPLITTER_HTML->GET_CONTAINER( ROW = 1 COLUMN = 1 ).

*--------------------------------
* Set Body Container
*--------------------------------
  GRF_BODY = LRF_SPLITTER->GET_CONTAINER( ROW = 2 COLUMN = 1 ).

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CREATE_ALV_GRID
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM CREATE_ALV_GRID.

  DATA: LS_TOOLBTN TYPE ZSCN00004,
*        LT_DFTVL   TYPE ZCL_CN_ALV_GRID=>TT_FIELD, "Add Row 시 자동으로 입력될 필드 관리
        LT_HEADER  TYPE ZCL_CN_ALV_GRID=>TT_HEADER.

*--------------------------------------------------
* User Toolbar Button (Default Setting).
*--------------------------------------------------
  LS_TOOLBTN-BTN_REC    = 'X'.       "Recovery Row
  LS_TOOLBTN-BTN_EXLD   = 'X'.       "Excel Download
  LS_TOOLBTN-BTN_EXLU   = 'X'.       "Excel Upload
  LS_TOOLBTN-MLTI_LINES = GV_MROW.   "Multi Row

*--------------------------------------------------
* Set Header Information
*--------------------------------------------------
  PERFORM SET_HEADER_INFO CHANGING LT_HEADER.


  CREATE OBJECT GRF_GRID
    EXPORTING
      IV_NAME    = 'ALV_GRID'   "다수의 그리드일 경우 식별하기 위함..
      IRF_PARENT = GRF_BODY
*     IV_VARIANT = P_VAR
*     IT_DFTVL   = LT_DFTVL
      IS_TOOLBTN = LS_TOOLBTN
      IRF_HEAD   = GRF_HEAD
*     IV_SCR_WR  = '20:10:70'
      IV_CELLC   = ''           "공통 스트럭쳐 CELLC필드 사용하지 않을 경우
      IT_HEADER  = LT_HEADER.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_HEADER_INFO
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- LT_HEADER
*&---------------------------------------------------------------------*
FORM SET_HEADER_INFO CHANGING CT_HEADER TYPE GRF_GRID->TT_HEADER.

  DATA: LS_HEADER TYPE GRF_GRID->TS_HEADER.

  DEFINE _L_SET_HEADER.
    CLEAR ls_header.
    ls_header-key   = &1.
    ls_header-info  = &2.
    ls_header-text  = &3.
    APPEND ls_header TO ct_header.
  END-OF-DEFINITION.

*---------------------------------------
* Header Text 지정
*---------------------------------------
*-프로그램명 헤더에서 제외

  "회사코드 TEXT
  SELECT SINGLE BUTXT
    FROM T001
   WHERE BUKRS = @P_BUKRS
    INTO @DATA(LV_BUTXT).

*-----------------------------------
* Header Column 지정
*-----------------------------------.
  _L_SET_HEADER: TEXT-F01   P_BUKRS   LV_BUTXT.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_GRID_HIST
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM SET_GRID_HIST.

  IF GRF_CONT_HIST IS NOT INITIAL.
    CALL METHOD GRF_CONT_HIST->FREE.
    CLEAR: GRF_CONT_HIST, GRF_GRID_HIST.
  ENDIF.

* Creating Custom container instance
*----------------------------------------------------
* Create Custom Container..
*----------------------------------------------------
  CREATE OBJECT GRF_CONT_HIST
    EXPORTING
      CONTAINER_NAME = 'CONT_HIST'
    EXCEPTIONS
      OTHERS         = 1.

*--------------------------------
* Create Alv Grid
*--------------------------------
  CREATE OBJECT GRF_GRID_HIST
    EXPORTING
      IV_NAME    = 'ALV_HIST'   "다수의 그리드일 경우 식별하기 위함..
      IRF_PARENT = GRF_CONT_HIST.

*--------------------------------
* Dislay Grid..
*--------------------------------
  GRF_GRID_HIST->SET_GRID( CHANGING  CT_DATA = GT_COND_HIST ).

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_GRID_BOM
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM SET_GRID_BOM.

  IF GRF_CONT_BOM IS NOT INITIAL.
    CALL METHOD GRF_CONT_BOM->FREE.
    CLEAR: GRF_CONT_BOM, GRF_GRID_BOM.
  ENDIF.

* Creating Custom container instance
*----------------------------------------------------
* Create Custom Container..
*----------------------------------------------------
  CREATE OBJECT GRF_CONT_BOM
    EXPORTING
      CONTAINER_NAME = 'CONT_BOM'
    EXCEPTIONS
      OTHERS         = 1.

*--------------------------------
* Create Alv Grid
*--------------------------------
  CREATE OBJECT GRF_GRID_BOM
    EXPORTING
      IV_NAME    = 'ALV_BOM'   "다수의 그리드일 경우 식별하기 위함..
      IRF_PARENT = GRF_CONT_BOM.

*--------------------------------
* Dislay Grid..
*--------------------------------
  GRF_GRID_BOM->SET_GRID( CHANGING  CT_DATA = GT_TOLL_MANUF ).

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CHECK_EXIT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM CHECK_EXIT.

  CASE GV_OK_CODE.
    WHEN 'EXIT'.
      LEAVE PROGRAM.
    WHEN OTHERS.
      LEAVE TO SCREEN 0.
  ENDCASE.

ENDFORM.
