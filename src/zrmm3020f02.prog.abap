*&---------------------------------------------------------------------*
*& Include          ZRMM3020F02
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form EVT_GRID_DOUBLE_CLICK
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> E_ROW_INDEX
*&      --> E_COLUMN
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form EVT_GRID_DOUBLE_CLICK
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

    WHEN 'EBELN'.
      CHECK LS_DISP-EBELN IS NOT INITIAL.

      CASE LS_DISP-BSTYP.
        WHEN 'F'. "구매오더
          SET PARAMETER ID 'BES' FIELD LS_DISP-EBELN.
          CALL TRANSACTION 'ME23N' AND SKIP FIRST SCREEN.

        WHEN 'L'. "일정라인
          SET PARAMETER ID 'SAG' FIELD LS_DISP-EBELN.
          CALL TRANSACTION 'ME33L' AND SKIP FIRST SCREEN.

        WHEN OTHERS.
      ENDCASE.

    WHEN 'KONNR'.
      CHECK LS_DISP-KONNR IS NOT INITIAL.

      SET PARAMETER ID 'CTR' FIELD LS_DISP-KONNR.
      CALL TRANSACTION 'ME33K' AND SKIP FIRST SCREEN.

    WHEN 'LIFNR'.
      CHECK LS_DISP-LIFNR IS NOT INITIAL.

      PERFORM CALL_TRANSACTION_BP USING LS_DISP-LIFNR.

    WHEN 'MATNR'.
      CHECK LS_DISP-MATNR IS NOT INITIAL.

      SET PARAMETER ID 'MAT' FIELD LS_DISP-MATNR.
      SET PARAMETER ID 'WRK' FIELD LS_DISP-WERKS.
      SET PARAMETER ID 'MXX' FIELD 'E'.               "Purchasing View (Basic View: "K" without WERKS)
      CALL TRANSACTION 'MM03' AND SKIP FIRST SCREEN.

    WHEN 'MENGE_GR_SUM' OR 'MENGE_IV_SUM' OR 'WRBTR_GR_SUM' OR 'WRBTR_IV_SUM'.  "입고수량, 송장수량, 입고금액, 송장금액
      CHECK LS_DISP-EBELN IS NOT INITIAL.

      "구매오더 이력 팝업
      CALL FUNCTION 'MM_HISTORY_POPUP_SHOW'
        EXPORTING
          PI_EBELN = LS_DISP-EBELN
          PI_EBELP = LS_DISP-EBELP
        EXCEPTIONS
*         error_message = 1                                     "949288
          OTHERS   = 2.

      IF SY-SUBRC <> 0.
        MESSAGE ID SY-MSGID TYPE 'S' NUMBER SY-MSGNO
                            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      ENDIF.

    WHEN OTHERS.
      RETURN.

  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CALL_TRANSACTION_BP
*&---------------------------------------------------------------------*
FORM CALL_TRANSACTION_BP USING IV_LIFNR.

  DATA: LV_REQUEST TYPE REF TO CL_BUPA_NAVIGATION_REQUEST,
        LV_OPTIONS TYPE REF TO CL_BUPA_DIALOG_JOEL_OPTIONS,
        LV_BP_GUID TYPE BU_PARTNER_GUID,
        LS_BP_ROLE TYPE BUS_ROLES.

  CONSTANTS: LC_FOLE TYPE BUS_ROLES-ROLE VALUE 'FLVN01'.  "공급업체

  LS_BP_ROLE-ROLE = LC_FOLE.

* Get BP number linked to the selected vendor
  DATA(LO_BP_VENDOR) = CVI_KA_BP_VENDOR=>GET_INSTANCE( ).
  LV_BP_GUID = LO_BP_VENDOR->GET_ASSIGNED_BP_FOR_VENDOR( IV_LIFNR ).

* Create a request/option.
  CREATE OBJECT LV_REQUEST.
  CREATE OBJECT LV_OPTIONS.

* Fill the request fields.
  CALL METHOD LV_REQUEST->SET_MAINTENANCE_ID( LV_REQUEST->GC_MAINTENANCE_ID_PARTNER ).
  CALL METHOD LV_REQUEST->SET_PARTNER_GUID( LV_BP_GUID ).
  CALL METHOD LV_REQUEST->SET_BUPA_PARTNER_ROLE( LS_BP_ROLE ).
  CALL METHOD LV_REQUEST->SET_BUPA_ACTIVITY( LV_REQUEST->GC_ACTIVITY_DISPLAY ).

* Fill option - Navigation disable..
  CALL METHOD LV_OPTIONS->SET_NAVIGATION_DISABLED( 'X' ).
  CALL METHOD LV_OPTIONS->SET_ACTIVITY_SWITCHING_OFF( 'X' ).
  CALL METHOD LV_OPTIONS->SET_NAVIGATE_ON_FIRST_TAB( 'X' ).

* Start the maintenance.
  CALL METHOD CL_BUPA_DIALOG_JOEL=>START_WITH_NAVIGATION
    EXPORTING
      IV_REQUEST = LV_REQUEST
      IV_OPTIONS = LV_OPTIONS
    EXCEPTIONS
      OTHERS     = 1.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form EVT_GRID_BUTTON_CLICK
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> ES_COL_ID
*&      --> ES_ROW_NO
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form EVT_GRID_BUTTON_CLICK
*&---------------------------------------------------------------------*
FORM EVT_GRID_BUTTON_CLICK USING IS_COL_ID TYPE LVC_S_COL
                                 IS_ROW_NO TYPE LVC_S_ROID.

*  DATA: LT_APVKEY  TYPE TABLE OF ZSCN00219.
  DATA: LV_RESULT  TYPE BAPI_MTYPE,
        LV_MESSAGE TYPE BAPI_MSG.

  READ TABLE GT_DISP INTO DATA(LS_DISP) INDEX IS_ROW_NO-ROW_ID.

  CHECK SY-SUBRC EQ 0.

  CASE IS_COL_ID-FIELDNAME.
*    WHEN 'APV_STAT'.
*      READ TABLE GT_APV WITH KEY WFOBJECT = LS_DISP-EBELN
*                                 BINARY SEARCH
*                                 TRANSPORTING NO FIELDS.
*
*      IF SY-SUBRC = 0.
*        LOOP AT GT_APV INTO DATA(LS_APV) FROM SY-TABIX.
*          IF LS_APV-WFOBJECT NE LS_DISP-EBELN.
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

    WHEN 'AFILE'.
      DATA(LS_SIBFLPORB) = VALUE SIBFLPORB( ).

      CASE LS_DISP-BSTYP.
        WHEN 'F'. "구매오더
          LS_SIBFLPORB = VALUE #( INSTID = LS_DISP-EBELN
                                  TYPEID = GC_GOS_TYPEID_PO
                                  CATID  = GC_GOS_CATID ).
        WHEN 'K'. "계약
          LS_SIBFLPORB = VALUE #( INSTID = LS_DISP-EBELN
                                  TYPEID = GC_GOS_TYPEID_CO
                                  CATID  = GC_GOS_CATID ).
        WHEN OTHERS.
      ENDCASE.

      CALL FUNCTION 'GOS_ATTACHMENT_LIST_POPUP'
        EXPORTING
          IS_OBJECT = LS_SIBFLPORB
          IP_MODE   = 'D'.         "Create/Display/Edit

    WHEN OTHERS.
  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form ALV_GRID_FCAT_MODIFY
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- CT_FCAT
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form ALV_GRID_FCAT_MODIFY
*&---------------------------------------------------------------------*
FORM ALV_GRID_FCAT_MODIFY CHANGING CT_FCAT TYPE LVC_T_FCAT.

  SORT CT_FCAT BY FIELDNAME.

*----------------------------------------
* Set Modify Field..
*----------------------------------------
  IF GV_CRITERIA = GC_CRITERIA_ALL.
    PERFORM ALV_GRID_FCAT_ALL CHANGING CT_FCAT.
  ELSE.
    PERFORM ALV_GRID_FCAT_SUM CHANGING CT_FCAT.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form ALV_GRID_FCAT_ALL
*&---------------------------------------------------------------------*
FORM ALV_GRID_FCAT_ALL CHANGING CT_FCAT TYPE LVC_T_FCAT.

  DEFINE _L_SET_FCAT_01.
    <LS_FCAT>-COL_POS    = &1.
    <LS_FCAT>-KEY        = &2.
    <LS_FCAT>-FIX_COLUMN = &3.
    <LS_FCAT>-NO_OUT     = &4.
    <LS_FCAT>-COLTEXT    = &5.
    <LS_FCAT>-JUST       = &6.
    <LS_FCAT>-F4AVAILABL = &7.
    <LS_FCAT>-CFIELDNAME = &8.
    <LS_FCAT>-QFIELDNAME = &9.
*    <LS_FCAT>-OUTPUTLEN  = &9.
    <LS_FCAT>-COL_OPT    = 'X'.
  END-OF-DEFINITION.

  DEFINE _L_SET_FCAT_02.
    <LS_FCAT>-DO_SUM     = &1.
    <LS_FCAT>-NO_ZERO    = &2.
    <LS_FCAT>-EMPHASIZE  = &3.
    <LS_FCAT>-CURRENCY   = &4.
  END-OF-DEFINITION.

  LOOP AT CT_FCAT ASSIGNING FIELD-SYMBOL(<LS_FCAT>).

    CASE <LS_FCAT>-FIELDNAME.
      WHEN 'APV_STAT'.  "결재진행
        _L_SET_FCAT_01: 01  'X'  'X'  ''  TEXT-C01  'C'  ''  ''  ''.
      WHEN 'STATUS'.  "발주상태
        _L_SET_FCAT_01: 02  ''  'X'  ''  TEXT-C02  'C'  ''  ''  ''.
      WHEN 'BEDAT'.  "발주일
        _L_SET_FCAT_01: 03  ''  'X'  ''  TEXT-C03  ''  ''  ''  ''.
      WHEN 'BSTYP'.  "구매문서범주
        _L_SET_FCAT_01: 04  ''  'X'  'X'  TEXT-C04  ''  ''  ''  ''.
      WHEN 'BSART'.  "구매문서유형
        _L_SET_FCAT_01: 05  ''  'X'  'X'  TEXT-C05  ''  ''  ''  ''.
      WHEN 'BATXT'.  "구매문서유형
        _L_SET_FCAT_01: 06  ''  'X'  ''  TEXT-C05  ''  ''  ''  ''.
      WHEN 'EBELN'.  "구매오더번호
        _L_SET_FCAT_01: 07  'X'  'X'  ''  TEXT-C06  ''  ''  ''  ''.
      WHEN 'EBELP'.  "품목
        _L_SET_FCAT_01: 08  'X'  'X'  ''  TEXT-C07  ''  ''  ''  ''.
      WHEN 'ZEMANAGE2'.  "관리번호
        _L_SET_FCAT_01: 09  ''  ''  ''  TEXT-C08  ''  ''  ''  ''.
      WHEN 'LIFNR'.  "공급업체
        _L_SET_FCAT_01: 10  ''  ''  ''  TEXT-C09  ''  ''  ''  ''.
      WHEN 'LIFNR_TEXT'.  "업체명
        _L_SET_FCAT_01: 11  ''  ''  ''  TEXT-C10  ''  ''  ''  ''.
      WHEN 'KALSK'.  "스키마그룹
        _L_SET_FCAT_01: 12  ''  ''  'X'  TEXT-C11  ''  ''  ''  ''.
      WHEN 'KALSB'.  "내외자구분
        _L_SET_FCAT_01: 13  ''  ''  ''  TEXT-C12  ''  ''  ''  ''.
      WHEN 'MATKL'.  "자재그룹
        _L_SET_FCAT_01: 14  ''  ''  ''  TEXT-C56  ''  ''  ''  ''.
      WHEN 'WGBEZ'.  "자재그룹명
        _L_SET_FCAT_01: 15  ''  ''  ''  TEXT-C57  ''  ''  ''  ''.
      WHEN 'MATNR'.  "자재
        _L_SET_FCAT_01: 16  ''  ''  ''  TEXT-C13  ''  ''  ''  ''.
      WHEN 'TXZ01'.  "자재내역
        _L_SET_FCAT_01: 17  ''  ''  ''  TEXT-C14  ''  ''  ''  ''.
      WHEN 'BWTAR'.  "평가유형
        _L_SET_FCAT_01: 18  ''  ''  ''  TEXT-C15  ''  ''  ''  ''.
      WHEN 'BKLAS'.  "평가클래스
        _L_SET_FCAT_01: 19  ''  ''  ''  TEXT-C16  ''  ''  ''  ''.
      WHEN 'WERKS'.  "플랜트
        _L_SET_FCAT_01: 20  ''  ''  ''  TEXT-C60  ''  ''  ''  ''.
      WHEN 'BUDAT'.  "전기일
        _L_SET_FCAT_01: 21  ''  ''  ''  TEXT-C17  ''  ''  ''  ''.
      WHEN 'ELIKZ_ICON'.  "납품완료
        _L_SET_FCAT_01: 22  ''  ''  ''  TEXT-C18  'C'  ''  ''  ''.
      WHEN 'MENGE'.  "발주수량
        _L_SET_FCAT_01: 23  ''  ''  ''  TEXT-C19  ''  ''  ''  'MEINS'.
        _L_SET_FCAT_02: 'X'  'X'  GC_EMPHSZ_C300  ''.
      WHEN 'MENGE_GR_SUM'.  "입고수량합
        _L_SET_FCAT_01: 24  ''  ''  ''  TEXT-C20  ''  ''  ''  'MEINS'.
        _L_SET_FCAT_02: 'X'  'X'  GC_EMPHSZ_C300  ''.
      WHEN 'MENGE_IV_SUM'.  "송장수량합
        _L_SET_FCAT_01: 25  ''  ''  ''  TEXT-C21  ''  ''  ''  'MEINS'.
        _L_SET_FCAT_02: 'X'  'X'  GC_EMPHSZ_C300  ''.
      WHEN 'MENGE_OP_GR_SUM'.  "미입고수량합
        _L_SET_FCAT_01: 26  ''  ''  ''  TEXT-C22  ''  ''  ''  'MEINS'.
        _L_SET_FCAT_02: 'X'  'X'  GC_EMPHSZ_C300  ''.
      WHEN 'MENGE_OP_SUM'.  "미착수량합
        _L_SET_FCAT_01: 27  ''  ''  ''  TEXT-C23  ''  ''  ''  'MEINS'.
        _L_SET_FCAT_02: 'X'  'X'  GC_EMPHSZ_C300  ''.
      WHEN 'MEINS'.  "단위
        _L_SET_FCAT_01: 28  ''  ''  ''  TEXT-C24  ''  ''  ''  ''.
        _L_SET_FCAT_02: ''  ''  GC_EMPHSZ_C300  ''.

      WHEN 'NETPR'.  "단가
        _L_SET_FCAT_01: 31  ''  ''  ''  TEXT-C25  ''  ''  'WAERS'  ''.
      WHEN 'NETWR'.  "발주금액
        _L_SET_FCAT_01: 32  ''  ''  ''  TEXT-C26  ''  ''  'WAERS'  ''.
        _L_SET_FCAT_02: 'X'  'X'  GC_EMPHSZ_C700  ''.
      WHEN 'WRBTR_GR_SUM'.  "입고금액합
        _L_SET_FCAT_01: 33  ''  ''  ''  TEXT-C27  ''  ''  'WAERS_EKBE'  ''.
        _L_SET_FCAT_02: 'X'  'X'  GC_EMPHSZ_C700  ''.
      WHEN 'WRBTR_IV_SUM'.  "송장금액합
        _L_SET_FCAT_01: 34  ''  ''  ''  TEXT-C28  ''  ''  'WAERS_EKBE'  ''.
        _L_SET_FCAT_02: 'X'  'X'  GC_EMPHSZ_C700  ''.
      WHEN 'WRBTR_OP_SUM'.  "미착금액합
        _L_SET_FCAT_01: 35  ''  ''  ''  TEXT-C29  ''  ''  'WAERS_EKBE'  ''.
        _L_SET_FCAT_02: 'X'  'X'  GC_EMPHSZ_C700  ''.
      WHEN 'WAERS'.  "통화
        _L_SET_FCAT_01: 36  ''  ''  ''  TEXT-C30  ''  ''  ''  ''.
        _L_SET_FCAT_02: ''  ''  GC_EMPHSZ_C700  ''.
      WHEN 'WAERS_EKBE'.  "통화
        _L_SET_FCAT_01: 37  ''  ''  'X'  TEXT-C30  ''  ''  ''  ''.
        _L_SET_FCAT_02: ''  ''  GC_EMPHSZ_C700  ''.
      WHEN 'DMBTR_GR_SUM'.  "입고금액합(KRW)
        _L_SET_FCAT_01: 38  ''  ''  ''  TEXT-C31  ''  ''  ''  ''.
*U2(T2)> Local Currency 로 변경 - Start
*        _L_SET_FCAT_02: 'X'  'X'  GC_EMPHSZ_C500  GC_CUKY_KRW.
        _L_SET_FCAT_02: 'X'  'X'  GC_EMPHSZ_C500  GV_LOCAL_WAERS.
*U2(T2)> Local Currency 로 변경 - End
      WHEN 'DMBTR_IV_SUM'.  "송장금액합(KRW)
        _L_SET_FCAT_01: 39  ''  ''  ''  TEXT-C32  ''  ''  ''  ''.
*U2(T2)> Local Currency 로 변경 - Start
*        _L_SET_FCAT_02: 'X'  'X'  GC_EMPHSZ_C500  GC_CUKY_KRW.
        _L_SET_FCAT_02: 'X'  'X'  GC_EMPHSZ_C500  GV_LOCAL_WAERS.
*U2(T2)> Local Currency 로 변경 - End
      WHEN 'DMBTR_OP_SUM'.  "미착금액합(KRW)
        _L_SET_FCAT_01: 40  ''  ''  ''  TEXT-C33  ''  ''  ''  ''.
*U2(T2)> Local Currency 로 변경 - Start
*        _L_SET_FCAT_02: 'X'  'X'  GC_EMPHSZ_C500  GC_CUKY_KRW.
        _L_SET_FCAT_02: 'X'  'X'  GC_EMPHSZ_C500  GV_LOCAL_WAERS.
*U2(T2)> Local Currency 로 변경 - End
      WHEN 'DMBTR_GRIR_SUM'.  "GR/IR반제차이(KRW)
        _L_SET_FCAT_01: 41   ''  ''  ''  TEXT-C34  ''  ''  ''  ''.
*U2(T2)> Local Currency 로 변경 - Start
*        _L_SET_FCAT_02: 'X'  'X'  GC_EMPHSZ_C500  GC_CUKY_KRW.
        _L_SET_FCAT_02: 'X'  'X'  GC_EMPHSZ_C500  GV_LOCAL_WAERS.
*U2(T2)> Local Currency 로 변경 - End
      WHEN 'ACTEX_SUM'.  "실비금액
        _L_SET_FCAT_01: 42   ''  ''  ''  TEXT-C58  ''  ''  ''  ''.
*U2(T2)> Local Currency 로 변경 - Start
*        _L_SET_FCAT_02: 'X'  'X'  GC_EMPHSZ_C700  GC_CUKY_KRW.
        _L_SET_FCAT_02: 'X'  'X'  GC_EMPHSZ_C700  GV_LOCAL_WAERS.
*U2(T2)> Local Currency 로 변경 - End
      WHEN 'ZTERM'.  "지급조건
        _L_SET_FCAT_01: 51  ''  ''  'X'  TEXT-C35  ''  ''  ''  ''.
      WHEN 'VTEXT'.  "지급조건
        _L_SET_FCAT_01: 52  ''  ''  ''  TEXT-C35  ''  ''  ''  ''.
      WHEN 'INFNR'.  "정보레코드
        _L_SET_FCAT_01: 53  ''  ''  ''  TEXT-C36  ''  ''  ''  ''.
      WHEN 'POPOLICY'.  "Contract Plan
        _L_SET_FCAT_01: 56  ''  ''  ''  TEXT-C63  ''  ''  ''  ''.
      WHEN 'ZORDER_PERSON'.  "발주담당자
        _L_SET_FCAT_01: 57  ''  ''  ''  TEXT-C37  ''  ''  ''  ''.
      WHEN 'ZORDER_PERSON_NAME'.  "발주담당자
        _L_SET_FCAT_01: 58  ''  ''  ''  TEXT-C37  ''  ''  ''  ''.
      WHEN 'ZORDER_DEPARTMENT'.  "발주부서
        _L_SET_FCAT_01: 59  ''  ''  ''  TEXT-C38  ''  ''  ''  ''.
      WHEN 'ZORDER_DEPARTMENT_NAME'.  "발주부서
        _L_SET_FCAT_01: 60  ''  ''  ''  TEXT-C38  ''  ''  ''  ''.
      WHEN 'KTPNR'.  "계약품목
        _L_SET_FCAT_01: 62  ''  ''  ''  TEXT-C07  ''  ''  ''  ''.

      WHEN OTHERS.
        "BY CODE INSPECTION
        PERFORM ALV_GRID_FCAT_ALL_OTHERS CHANGING <LS_FCAT>.
    ENDCASE.

    <LS_FCAT>-SCRTEXT_S = <LS_FCAT>-COLTEXT.
    <LS_FCAT>-SCRTEXT_M = <LS_FCAT>-COLTEXT.
    <LS_FCAT>-SCRTEXT_L = <LS_FCAT>-COLTEXT.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form ALV_GRID_FCAT_SUM
*&---------------------------------------------------------------------*
FORM ALV_GRID_FCAT_SUM CHANGING CT_FCAT TYPE LVC_T_FCAT.

  DEFINE _L_SET_FCAT.
    <LS_FCAT>-COL_POS    = &1.
    <LS_FCAT>-KEY        = &2.
    <LS_FCAT>-NO_OUT     = &3.
    <LS_FCAT>-COLTEXT    = &4.
    <LS_FCAT>-JUST       = &5.
    <LS_FCAT>-F4AVAILABL = &6.
    <LS_FCAT>-CFIELDNAME = &7.
    <LS_FCAT>-QFIELDNAME = &8.
*    <LS_FCAT>-OUTPUTLEN  = &9.
    <LS_FCAT>-COL_OPT    = 'X'.
  END-OF-DEFINITION.

  LOOP AT CT_FCAT ASSIGNING FIELD-SYMBOL(<LS_FCAT>).

    CASE <LS_FCAT>-FIELDNAME.
      WHEN 'STATUS'.  "발주상태
        _L_SET_FCAT: 01  ''  'X'  TEXT-C02  'C'  ''  ''  ''.

      WHEN 'LIFNR'.  "공급업체
        _L_SET_FCAT: 02  'X'  ''  TEXT-C09  ''  ''  ''  ''.
        <LS_FCAT>-TECH = GV_TECH+0(1).
      WHEN 'LIFNR_TEXT'.  "업체명
        _L_SET_FCAT: 03  'X'  ''  TEXT-C10  ''  ''  ''  ''.
        <LS_FCAT>-TECH = GV_TECH+0(1).
      WHEN 'MATKL'.  "자재그룹
        _L_SET_FCAT: 04  'X'  ''  TEXT-C56  ''  ''  ''  ''.
        <LS_FCAT>-TECH = GV_TECH+4(1).
      WHEN 'WGBEZ'.  "자재그룹명
        _L_SET_FCAT: 05  'X'  ''  TEXT-C57  ''  ''  ''  ''.
        <LS_FCAT>-TECH = GV_TECH+4(1).
      WHEN 'MATNR'.  "자재
        _L_SET_FCAT: 06  'X'  ''  TEXT-C13  ''  ''  ''  ''.
        <LS_FCAT>-TECH = GV_TECH+1(1).
      WHEN 'TXZ01'.  "자재내역
        _L_SET_FCAT: 07  'X'  ''  TEXT-C14  ''  ''  ''  ''.
        <LS_FCAT>-TECH = GV_TECH+1(1).
      WHEN 'ZORDER_PERSON'.  "발주담당자
        _L_SET_FCAT: 08  'X'  ''  TEXT-C37  ''  ''  ''  ''.
        <LS_FCAT>-TECH = GV_TECH+2(1).
      WHEN 'ZORDER_PERSON_NAME'.  "발주담당자
        _L_SET_FCAT: 09  'X'  ''  TEXT-C37  ''  ''  ''  ''.
        <LS_FCAT>-TECH = GV_TECH+2(1).
      WHEN 'ZORDER_DEPARTMENT'.  "발주부서
        _L_SET_FCAT: 10  'X'  ''  TEXT-C38  ''  ''  ''  ''.
        <LS_FCAT>-TECH = GV_TECH+3(1).
      WHEN 'ZORDER_DEPARTMENT_NAME'.  "발주부서
        _L_SET_FCAT: 11  'X'  ''  TEXT-C38  ''  ''  ''  ''.
        <LS_FCAT>-TECH = GV_TECH+3(1).

      WHEN 'KALSK'.  "스키마그룹
        _L_SET_FCAT: 11  ''  'X'  TEXT-C11  ''  ''  ''  ''.
      WHEN 'KALSB'.  "내외자구분
        _L_SET_FCAT: 12  ''  ''  TEXT-C12  ''  ''  ''  ''.

      WHEN 'MENGE'.  "발주수량
        _L_SET_FCAT: 21  ''  ''  TEXT-C19  ''  ''  ''  'MEINS'.
        <LS_FCAT>-DO_SUM    = 'X'.
        <LS_FCAT>-NO_ZERO   = 'X'.
        <LS_FCAT>-EMPHASIZE = GC_EMPHSZ_C300.
      WHEN 'MENGE_GR_SUM'.  "입고수량합
        _L_SET_FCAT: 22  ''  ''  TEXT-C20  ''  ''  ''  'MEINS'.
        <LS_FCAT>-DO_SUM    = 'X'.
        <LS_FCAT>-NO_ZERO   = 'X'.
        <LS_FCAT>-EMPHASIZE = GC_EMPHSZ_C300.
      WHEN 'MENGE_IV_SUM'.  "송장수량합
        _L_SET_FCAT: 23  ''  ''  TEXT-C21  ''  ''  ''  'MEINS'.
        <LS_FCAT>-DO_SUM    = 'X'.
        <LS_FCAT>-NO_ZERO   = 'X'.
        <LS_FCAT>-EMPHASIZE = GC_EMPHSZ_C300.
      WHEN 'MENGE_OP_GR_SUM'.  "미입고수량합
        _L_SET_FCAT: 24  ''  ''  TEXT-C22  ''  ''  ''  'MEINS'.
        <LS_FCAT>-DO_SUM    = 'X'.
        <LS_FCAT>-NO_ZERO   = 'X'.
        <LS_FCAT>-EMPHASIZE = GC_EMPHSZ_C300.
      WHEN 'MENGE_OP_SUM'.  "미착수량합
        _L_SET_FCAT: 25  ''  ''  TEXT-C23  ''  ''  ''  'MEINS'.
        <LS_FCAT>-DO_SUM    = 'X'.
        <LS_FCAT>-NO_ZERO   = 'X'.
        <LS_FCAT>-EMPHASIZE = GC_EMPHSZ_C300.
      WHEN 'MEINS'.  "단위
        _L_SET_FCAT: 26  ''  ''  TEXT-C24  ''  ''  ''  ''.
        <LS_FCAT>-EMPHASIZE = GC_EMPHSZ_C300.

      WHEN 'NETWR'.  "발주금액
        _L_SET_FCAT: 31  ''  ''  TEXT-C26  ''  ''  'WAERS'  ''.
        <LS_FCAT>-EMPHASIZE = GC_EMPHSZ_C700.
      WHEN 'WRBTR_GR_SUM'.  "입고금액합
        _L_SET_FCAT: 32  ''  ''  TEXT-C27  ''  ''  'WAERS'  ''.
        <LS_FCAT>-DO_SUM    = 'X'.
        <LS_FCAT>-NO_ZERO   = 'X'.
        <LS_FCAT>-EMPHASIZE = GC_EMPHSZ_C700.
      WHEN 'WRBTR_IV_SUM'.  "송장금액합
        _L_SET_FCAT: 33  ''  ''  TEXT-C28  ''  ''  'WAERS'  ''.
        <LS_FCAT>-DO_SUM    = 'X'.
        <LS_FCAT>-NO_ZERO   = 'X'.
        <LS_FCAT>-EMPHASIZE = GC_EMPHSZ_C700.
      WHEN 'WRBTR_OP_SUM'.  "미착금액합
        _L_SET_FCAT: 34  ''  ''  TEXT-C29  ''  ''  'WAERS'  ''.
        <LS_FCAT>-DO_SUM    = 'X'.
        <LS_FCAT>-NO_ZERO   = 'X'.
        <LS_FCAT>-EMPHASIZE = GC_EMPHSZ_C700.
      WHEN 'WAERS'.  "통화
        _L_SET_FCAT: 35  ''  ''  TEXT-C30  ''  ''  ''  ''.
        <LS_FCAT>-EMPHASIZE = GC_EMPHSZ_C700.
*      WHEN 'WAERS_EKBE'.  "통화
*        _L_SET_FCAT: 36  ''  'X'  TEXT-C30  ''  ''  ''  ''.
*        <LS_FCAT>-EMPHASIZE = GC_EMPHSZ_C700.

      WHEN 'DMBTR_GR_SUM'.  "입고금액합(KRW)
        _L_SET_FCAT: 41  ''  ''  TEXT-C31  ''  ''  ''  ''.
        <LS_FCAT>-DO_SUM    = 'X'.
        <LS_FCAT>-NO_ZERO   = 'X'.
*U2(T2)> Local Currency 로 변경 - Start
*        <LS_FCAT>-CURRENCY  = GC_CUKY_KRW.
        <LS_FCAT>-CURRENCY  = GV_LOCAL_WAERS.
*U2(T2)> Local Currency 로 변경 - End
        <LS_FCAT>-EMPHASIZE = GC_EMPHSZ_C500.
      WHEN 'DMBTR_IV_SUM'.  "송장금액합(KRW)
        _L_SET_FCAT: 42  ''  ''  TEXT-C32  ''  ''  ''  ''.
        <LS_FCAT>-DO_SUM    = 'X'.
        <LS_FCAT>-NO_ZERO   = 'X'.
*U2(T2)> Local Currency 로 변경 - Start
*        <LS_FCAT>-CURRENCY  = GC_CUKY_KRW.
        <LS_FCAT>-CURRENCY  = GV_LOCAL_WAERS.
*U2(T2)> Local Currency 로 변경 - End
        <LS_FCAT>-EMPHASIZE = GC_EMPHSZ_C500.
      WHEN 'DMBTR_OP_SUM'.  "미착금액합(KRW)
        _L_SET_FCAT: 43  ''  ''  TEXT-C33  ''  ''  ''  ''.
        <LS_FCAT>-DO_SUM    = 'X'.
        <LS_FCAT>-NO_ZERO   = 'X'.
*U2(T2)> Local Currency 로 변경 - Start
*        <LS_FCAT>-CURRENCY  = GC_CUKY_KRW.
        <LS_FCAT>-CURRENCY  = GV_LOCAL_WAERS.
*U2(T2)> Local Currency 로 변경 - End
        <LS_FCAT>-EMPHASIZE = GC_EMPHSZ_C500.
      WHEN 'DMBTR_GRIR_SUM'.  "GR/IR반제차이(KRW)
        _L_SET_FCAT: 44   ''  ''  TEXT-C34  ''  ''  ''  ''.
        <LS_FCAT>-DO_SUM    = 'X'.
        <LS_FCAT>-NO_ZERO   = 'X'.
*U2(T2)> Local Currency 로 변경 - Start
*        <LS_FCAT>-CURRENCY  = GC_CUKY_KRW.
        <LS_FCAT>-CURRENCY  = GV_LOCAL_WAERS.
*U2(T2)> Local Currency 로 변경 - End
        <LS_FCAT>-EMPHASIZE = GC_EMPHSZ_C500.

      WHEN OTHERS.
        <LS_FCAT>-TECH = 'X'.
    ENDCASE.

    <LS_FCAT>-SCRTEXT_S = <LS_FCAT>-COLTEXT.
    <LS_FCAT>-SCRTEXT_M = <LS_FCAT>-COLTEXT.
    <LS_FCAT>-SCRTEXT_L = <LS_FCAT>-COLTEXT.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form ALV_BKLAS_FCAT_MODIFY
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- CT_FCAT
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form ALV_BKLAS_FCAT_MODIFY
*&---------------------------------------------------------------------*
FORM ALV_BKLAS_FCAT_MODIFY CHANGING CT_FCAT TYPE LVC_T_FCAT.

  DEFINE _L_SET_FCAT.
    <LS_FCAT>-COL_POS    = &1.
    <LS_FCAT>-KEY        = &2.
    <LS_FCAT>-EDIT       = &3.
    <LS_FCAT>-COLTEXT    = &4.
    <LS_FCAT>-JUST       = &5.
    <LS_FCAT>-F4AVAILABL = &6.
    <LS_FCAT>-CFIELDNAME = &7.
    <LS_FCAT>-QFIELDNAME = &8.
*    <LS_FCAT>-OUTPUTLEN  = &9.
    <LS_FCAT>-COL_OPT    = 'X'.
  END-OF-DEFINITION.

  SORT CT_FCAT BY FIELDNAME.

*----------------------------------------
* Set Modify Field..
*----------------------------------------
  LOOP AT CT_FCAT ASSIGNING FIELD-SYMBOL(<LS_FCAT>).

    CASE <LS_FCAT>-FIELDNAME.
      WHEN 'BKLAS'. "평가클래스
        _L_SET_FCAT: 01  ''  ''  TEXT-C16  ''  ''  ''  ''.
      WHEN 'BKBEZ'. "평가클래스 내역
        _L_SET_FCAT: 02  ''  ''  TEXT-C54  ''  ''  ''  ''.
      WHEN 'KONTS'. "G/L계정
        _L_SET_FCAT: 03  ''  ''  TEXT-C41  ''  ''  ''  ''.
      WHEN 'TXT20'. "G/L계정 내역
        _L_SET_FCAT: 04  ''  ''  TEXT-C55  ''  ''  ''  ''.
      WHEN 'DMBTR_OP_SUM'. "미착금액합
        _L_SET_FCAT: 05  ''  ''  TEXT-C29  ''  ''  'WAERS'  ''.
        <LS_FCAT>-DO_SUM    = 'X'.
        <LS_FCAT>-EMPHASIZE = GC_EMPHSZ_C500.
      WHEN 'WAERS'. "통화
        _L_SET_FCAT: 05  ''  ''  TEXT-C30  ''  ''  ''  ''.

      WHEN OTHERS.
        <LS_FCAT>-TECH = 'X'.
    ENDCASE.

    <LS_FCAT>-SCRTEXT_S = <LS_FCAT>-COLTEXT.
    <LS_FCAT>-SCRTEXT_M = <LS_FCAT>-COLTEXT.
    <LS_FCAT>-SCRTEXT_L = <LS_FCAT>-COLTEXT.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form ALV_GRID_FCAT_ALL_OTHERS
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- <LS_FCAT>
*&---------------------------------------------------------------------*
FORM ALV_GRID_FCAT_ALL_OTHERS CHANGING CS_FCAT TYPE LVC_S_FCAT.

  DEFINE _L_SET_FCAT_01.
    CS_FCAT-COL_POS    = &1.
    CS_FCAT-KEY        = &2.
    CS_FCAT-FIX_COLUMN = &3.
    CS_FCAT-NO_OUT     = &4.
    CS_FCAT-COLTEXT    = &5.
    CS_FCAT-JUST       = &6.
    CS_FCAT-F4AVAILABL = &7.
    CS_FCAT-CFIELDNAME = &8.
    CS_FCAT-QFIELDNAME = &9.
*    CS_FCAT-OUTPUTLEN  = &9.
    CS_FCAT-COL_OPT    = 'X'.
  END-OF-DEFINITION.

  DEFINE _L_SET_FCAT_02.
    CS_FCAT-DO_SUM     = &1.
    CS_FCAT-NO_ZERO    = &2.
    CS_FCAT-EMPHASIZE  = &3.
    CS_FCAT-CURRENCY   = &4.
  END-OF-DEFINITION.


  CASE CS_FCAT-FIELDNAME.
    WHEN 'KNTTP'.  "계정지정범주
      _L_SET_FCAT_01: 63  ''  ''  'X'  TEXT-C40  ''  ''  ''  ''.
    WHEN 'KNTTX'.  "계정지정범주
      _L_SET_FCAT_01: 64  ''  ''  ''  TEXT-C40  ''  ''  ''  ''.
    WHEN 'SAKTO'.  "G/L계정
      _L_SET_FCAT_01: 65  ''  ''  'X'  TEXT-C41  ''  ''  ''  ''.
    WHEN 'SAKTO_TEXT'.  "G/L계정
      _L_SET_FCAT_01: 66  ''  ''  ''  TEXT-C41  ''  ''  ''  ''.
    WHEN 'PRCTR'.  "손익센터
      _L_SET_FCAT_01: 67  ''  ''  'X'  TEXT-C42  ''  ''  ''  ''.
    WHEN 'PRCTR_TEXT'.  "손익센터
      _L_SET_FCAT_01: 68  ''  ''  ''  TEXT-C42  ''  ''  ''  ''.
    WHEN 'KOSTL'.  "코스트센터
      _L_SET_FCAT_01: 69  ''  ''  'X'  TEXT-C43  ''  ''  ''  ''.
    WHEN 'KOSTL_TEXT'.  "코스트센터
      _L_SET_FCAT_01: 70  ''  ''  ''  TEXT-C43  ''  ''  ''  ''.
    WHEN 'FISTL'.  "자금관리센터
      _L_SET_FCAT_01: 71  ''  ''  ''  TEXT-C44  ''  ''  ''  ''.
*      WHEN 'FISTL_TEXT'.  " 자금관리센터
*        _L_SET_FCAT_01: 70  ''  ''  ''  TEXT-C44  ''  ''  ''  ''.
    WHEN 'FIPOS'.  "약정항목
      _L_SET_FCAT_01: 72  ''  ''  ''  TEXT-C59  ''  ''  ''  ''.

    WHEN 'VBELN'.  "판매오더번호
      _L_SET_FCAT_01: 73  ''  ''  ''  TEXT-C45  ''  ''  ''  ''.
    WHEN 'VBELP'.  "품목
      _L_SET_FCAT_01: 74  ''  ''  ''  TEXT-C07  ''  ''  ''  ''.

    WHEN 'ANLN1'.  "자산
      _L_SET_FCAT_01: 81  ''  ''  'X'  TEXT-C46  ''  ''  ''  ''.
    WHEN 'ANLN1_TEXT'.  "자산
      _L_SET_FCAT_01: 82  ''  ''  ''  TEXT-C46  ''  ''  ''  ''.
    WHEN 'PSPNR'.  "WBS 요소
      _L_SET_FCAT_01: 83  ''  ''  'X'  TEXT-C47  ''  ''  ''  ''.
    WHEN 'PSPNR_TEXT'.  "WBS 요소
      _L_SET_FCAT_01: 84  ''  ''  ''  TEXT-C47  ''  ''  ''  ''.
    WHEN 'AUFNR'.  "Internal Order
      _L_SET_FCAT_01: 85  ''  ''  'X'  TEXT-C48  ''  ''  ''  ''.
    WHEN 'AUFNR_TEXT'.  "Internal Order
      _L_SET_FCAT_01: 86  ''  ''  ''  TEXT-C48  ''  ''  ''  ''.

    WHEN 'RETPO'.  "반품품목
      _L_SET_FCAT_01: 91  ''  ''  ''  TEXT-C49  'C'  ''  ''  ''.
      CS_FCAT-CHECKBOX = 'X'.
    WHEN 'REPOS'.  "무상품목
      _L_SET_FCAT_01: 92  ''  ''  ''  TEXT-C50  'C'  ''  ''  ''.
      CS_FCAT-CHECKBOX = 'X'.
    WHEN 'LOEKZ'.  "삭제지시자
      _L_SET_FCAT_01: 93  ''  ''  ''  TEXT-C64  'C'  ''  ''  ''.
      CS_FCAT-CHECKBOX = 'X'.
    WHEN 'EKORG'.  "구매조직
      _L_SET_FCAT_01: 94  ''  ''  'X'  TEXT-C52  ''  ''  ''  ''.
    WHEN 'EKGRP'.  "구매그룹
      _L_SET_FCAT_01: 95  ''  ''  'X'  TEXT-C51  ''  ''  ''  ''.

    WHEN 'AFILE'.  "첨부파일
      _L_SET_FCAT_01: 96  ''  ''  ''  TEXT-C53  'C'  ''  ''  ''.

    WHEN OTHERS.
      CS_FCAT-TECH = 'X'.
  ENDCASE.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form ALV_GRID_SET_LINE_STYLE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> IT_FCAT
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form ALV_GRID_SET_LINE_STYLE
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
FORM SET_FIELD_CELLTAB USING IT_FCAT TYPE LVC_T_FCAT
                       CHANGING CT_STYL TYPE LVC_T_STYL
                                CS_DISP TYPE TY_DISP.

  DATA : LS_LVC_STYL TYPE LVC_S_STYL.

  LOOP AT IT_FCAT INTO DATA(LS_FIELDCAT).
    CLEAR LS_LVC_STYL.

    LS_LVC_STYL-FIELDNAME = LS_FIELDCAT-FIELDNAME.

* PUSH BUTTON STYLE 구성
    CASE LS_LVC_STYL-FIELDNAME.
      WHEN 'APV_STAT'.
        IF CS_DISP-APV_STAT IS NOT INITIAL.
          LS_LVC_STYL-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_BUTTON.
        ENDIF.

      WHEN 'AFILE'.
        IF CS_DISP-AFILE IS NOT INITIAL.
          LS_LVC_STYL-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_BUTTON.
        ENDIF.

      WHEN OTHERS.
    ENDCASE.

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
*&---------------------------------------------------------------------*
*& Form SET_GRID
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
*    GRF_GRID->REFRESH_GRID_DISPLAY( ).

    PERFORM REFRESH_GRID_DISPLAY.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CREATE_CONTAINER
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
*& Form REFRESH_GRID_DISPLAY
*&---------------------------------------------------------------------*
FORM REFRESH_GRID_DISPLAY.

  DATA LS_STABLE TYPE LVC_S_STBL.

*  LS_STABLE = VALUE #( ROW = 'X' COL = 'X' ).

  CALL METHOD GRF_GRID->REFRESH_TABLE_DISPLAY
    EXPORTING
      IS_STABLE = LS_STABLE.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_GRID_BKLAS
*&---------------------------------------------------------------------*
FORM SET_GRID_BKLAS.

  IF GRF_CONT_BKLAS IS NOT INITIAL.
    CALL METHOD GRF_CONT_BKLAS->FREE.
    CLEAR: GRF_CONT_BKLAS, GRF_GRID_BKLAS.
  ENDIF.

* Creating Custom container instance
*----------------------------------------------------
* Create Custom Container..
*----------------------------------------------------
  CREATE OBJECT GRF_CONT_BKLAS
    EXPORTING
      CONTAINER_NAME = 'CONT_BKLAS'
    EXCEPTIONS
      OTHERS         = 1.

*--------------------------------
* Create Alv Grid
*--------------------------------
  CREATE OBJECT GRF_GRID_BKLAS
    EXPORTING
      IV_NAME    = 'ALV_BKLAS'   "다수의 그리드일 경우 식별하기 위함..
      IRF_PARENT = GRF_CONT_BKLAS.

*--------------------------------
* Dislay Grid..
*--------------------------------
  GRF_GRID_BKLAS->SET_GRID( CHANGING CT_DATA = GT_BKLAS_SUM ).

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_HEADER_INFO
*&---------------------------------------------------------------------*
FORM SET_HEADER_INFO CHANGING CT_HEADER TYPE GRF_GRID->TT_HEADER.

  DATA: LS_HEADER TYPE GRF_GRID->TS_HEADER.

  DEFINE _L_SET_HEADER.
    CLEAR LS_HEADER.
    LS_HEADER-KEY   = &1.
    LS_HEADER-INFO  = &2.
    LS_HEADER-TEXT  = &3.
    APPEND LS_HEADER TO CT_HEADER.
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
*-----------------------------------
  _L_SET_HEADER: TEXT-F00   P_BUKRS   LV_BUTXT.

  IF P_RA = 'X'.
    _L_SET_HEADER: TEXT-F01  ''  ''.
  ENDIF.

  IF P_RB = 'X'.
    _L_SET_HEADER: TEXT-F02  ''  ''.
  ENDIF.

*-----------------------------------
* Header 주석
*-----------------------------------

*_g_set_value:'20:10:70'.  "Default 비율 (비율 변경시 사용)

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CHECK_EXIT
*&---------------------------------------------------------------------*
FORM CHECK_EXIT.

  CASE GV_OK_CODE.
    WHEN 'EXIT'.
      LEAVE PROGRAM.
    WHEN OTHERS.
      LEAVE TO SCREEN 0.
  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SORT_BY_CRITERIA
*&---------------------------------------------------------------------*
FORM SORT_BY_CRITERIA USING IV_OK_CODE.

  DATA: LT_SORT TYPE LVC_T_SORT,
        LS_SORT TYPE LVC_S_SORT.

  CONSTANTS: LC_TECH_X1 LIKE GV_TECH VALUE ' XXXX',   "업체별
             LC_TECH_X2 LIKE GV_TECH VALUE 'X XX ',   "자재별
             LC_TECH_X3 LIKE GV_TECH VALUE 'XX XX',   "발주담당자별
             LC_TECH_X4 LIKE GV_TECH VALUE 'XXX X',   "발주부서별
             LC_TECH_X5 LIKE GV_TECH VALUE 'XXXX '.   "자재그룹별

  DEFINE _L_SET_SORT.
    ls_sort-spos      = &1.
    ls_sort-fieldname = &2.
    ls_sort-up        = 'X'.
    APPEND ls_sort TO lt_sort.
  END-OF-DEFINITION.

  CLEAR: GT_DISP, GT_FCAT, GV_TECH.

*--------------------------------
* Summary 기준 별 Sorting, Data 및 Field Catalog 조정
*--------------------------------
  CASE IV_OK_CODE.
    WHEN 'ALL'. "전체
      GV_CRITERIA = GC_CRITERIA_ALL.
      GT_DISP = GT_ALL.

    WHEN 'LIFNR'.  "업체별
      GV_CRITERIA = GC_CRITERIA_LIFNR.
      MOVE-CORRESPONDING GT_LIFNR_SUM TO GT_DISP.

      _L_SET_SORT: 1  'LIFNR',
                   2  'KALSK'.

      GV_TECH = LC_TECH_X1.

    WHEN 'MATNR'.  "자재별
      GV_CRITERIA = GC_CRITERIA_MATNR.
      MOVE-CORRESPONDING GT_MATNR_SUM TO GT_DISP.

      _L_SET_SORT: 1  'MATKL',
                   1  'MATNR',
                   2  'KALSK'.

      GV_TECH = LC_TECH_X2.

    WHEN 'ORDER'.  "발주담당자별
      GV_CRITERIA = GC_CRITERIA_ORDER.
      MOVE-CORRESPONDING GT_ORDER_SUM TO GT_DISP.

      _L_SET_SORT: 1  'ZORDER_PERSON',
                   2  'KALSK'.

      GV_TECH = LC_TECH_X3.

    WHEN 'DEPTO'.  "발주부서별
      GV_CRITERIA = GC_CRITERIA_DEPTO.
      MOVE-CORRESPONDING GT_DEPTO_SUM TO GT_DISP.

      _L_SET_SORT: 1  'ZORDER_DEPARTMENT',
                   2  'KALSK'.

      GV_TECH = LC_TECH_X4.

    WHEN 'MATKL'.  "자재그룹별
      GV_CRITERIA = GC_CRITERIA_MATKL.
      MOVE-CORRESPONDING GT_MATKL_SUM TO GT_DISP.

      _L_SET_SORT: 1  'MATKL',
                   2  'KALSK'.

      GV_TECH = LC_TECH_X5.

    WHEN OTHERS.
      EXIT.
  ENDCASE.

* Sorting
  CALL METHOD GRF_GRID->SET_SORT_CRITERIA
    EXPORTING
      IT_SORT                   = LT_SORT
    EXCEPTIONS
      NO_FIELDCATALOG_AVAILABLE = 1
      OTHERS                    = 2.

* Field Catalog 조정
  GT_FCAT = GRF_GRID->GET_FCAT( IT_TABLE = GT_DISP ).

  PERFORM ALV_GRID_FCAT_MODIFY CHANGING GT_FCAT.

  CALL METHOD GRF_GRID->SET_FRONTEND_FIELDCATALOG
    EXPORTING
      IT_FIELDCATALOG = GT_FCAT.

* Style
  PERFORM ALV_GRID_SET_LINE_STYLE USING GT_FCAT.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form POPUP_BKLAS_SUM
*&---------------------------------------------------------------------*
FORM POPUP_BKLAS_SUM.

  SORT GT_BKLAS_SUM BY BKLAS.

  DELETE GT_BKLAS_SUM WHERE BKLAS IS INITIAL.

*-----------------------------
* 평가클래스의 계정 및 TEXT
*-----------------------------
  IF GT_BKLAS_SUM IS NOT INITIAL.

* 평가클래스 내역
    SELECT A~BKLAS,
           B~BKBEZ
      INTO TABLE @DATA(LT_T025)
      FROM T025 AS A INNER JOIN T025T AS B
                             ON B~SPRAS = @SY-LANGU
                            AND B~BKLAS = A~BKLAS
       FOR ALL ENTRIES IN @GT_BKLAS_SUM
     WHERE A~BKLAS = @GT_BKLAS_SUM-BKLAS.

    SORT LT_T025 BY BKLAS.

* G/L계정
    SELECT A~BKLAS,
           A~KONTS,
           B~TXT20
      INTO TABLE @DATA(LT_T030)
      FROM T030 AS A INNER JOIN SKAT AS B
                             ON B~SPRAS = @SY-LANGU
                            AND B~KTOPL = @GC_1000
                            AND B~SAKNR = A~KONTS
                     INNER JOIN T001K AS C
                             ON C~BUKRS = @P_BUKRS
                            AND C~BWMOD = A~BWMOD
       FOR ALL ENTRIES IN @GT_BKLAS_SUM
     WHERE A~KTOPL = @GC_1000
       AND A~KTOSL = 'WRX'
       AND A~BKLAS = @GT_BKLAS_SUM-BKLAS.

    SORT LT_T030 BY BKLAS KONTS.

  ENDIF.


  LOOP AT GT_BKLAS_SUM ASSIGNING FIELD-SYMBOL(<LS_BKLAS_SUM>).

    "평가클래스 내역
    READ TABLE LT_T025 WITH KEY BKLAS = <LS_BKLAS_SUM>-BKLAS
                                BINARY SEARCH
                                INTO DATA(LS_T025).
    IF SY-SUBRC = 0.
      <LS_BKLAS_SUM>-BKBEZ = LS_T025-BKBEZ.
    ENDIF.

    "G/L계정
    READ TABLE LT_T030 WITH KEY BKLAS = <LS_BKLAS_SUM>-BKLAS
                                BINARY SEARCH
                                INTO DATA(LS_T030).
    IF SY-SUBRC = 0.
      <LS_BKLAS_SUM>-KONTS = LS_T030-KONTS.
      <LS_BKLAS_SUM>-TXT20 = LS_T030-TXT20.
    ENDIF.

  ENDLOOP.

  CALL SCREEN '0200' STARTING AT 05 05.

ENDFORM.
