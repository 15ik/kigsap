*&---------------------------------------------------------------------*
*& Include          ZRMM3030F02
*&---------------------------------------------------------------------*
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

    WHEN 'MBLNR_GR'.
      CHECK LS_DISP-MBLNR_GR IS NOT INITIAL.

      CALL FUNCTION 'MIGO_DIALOG'
        EXPORTING
          I_MBLNR             = LS_DISP-MBLNR_GR
          I_MJAHR             = LS_DISP-MJAHR_GR
        EXCEPTIONS
          ILLEGAL_COMBINATION = 1
          OTHERS              = 2.

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

    WHEN 'VBELN_IM'.
      CHECK LS_DISP-VBELN_IM IS NOT INITIAL.

      SET PARAMETER ID 'VLM' FIELD LS_DISP-VBELN_IM.
      CALL TRANSACTION 'VL32N' AND SKIP FIRST SCREEN.

    WHEN 'LIFNR'.
      CHECK LS_DISP-LIFNR IS NOT INITIAL.

      PERFORM CALL_TRANSACTION_BP USING LS_DISP-LIFNR.

    WHEN 'MATNR'.
      CHECK LS_DISP-MATNR IS NOT INITIAL.

      SET PARAMETER ID 'MAT' FIELD LS_DISP-MATNR.
      SET PARAMETER ID 'WRK' FIELD LS_DISP-WERKS.
      SET PARAMETER ID 'MXX' FIELD 'E'.               "Purchasing View (Basic View: "K" without WERKS)
      CALL TRANSACTION 'MM03' AND SKIP FIRST SCREEN.

    WHEN 'MENGE_GR' OR 'WRBTR_GR' OR 'MENGE_RE' OR 'WRBTR_RE' OR
         'MENGE_IV' OR 'WRBTR_IV' OR 'MENGE_DIF' OR 'WRBTR_DIF'.

      CHECK LS_DISP-MBLNR_GR IS NOT INITIAL.

      PERFORM POPUP_GR_DETAIL USING LS_DISP. "입고 상세 현황 Popup

    WHEN OTHERS.
      RETURN.

  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form EVT_DETL_DOUBLE_CLICK
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> E_ROW_INDEX
*&      --> E_COLUMN
*&---------------------------------------------------------------------*
FORM EVT_DETL_DOUBLE_CLICK USING IV_ROW
                                 IV_COLUMN.

  READ TABLE GT_DETL INTO DATA(LS_DETL) INDEX IV_ROW.

  CASE IV_COLUMN.
    WHEN 'MBLNR_GR'.
      CHECK LS_DETL-MBLNR_GR IS NOT INITIAL.

      CALL FUNCTION 'MIGO_DIALOG'
        EXPORTING
          I_MBLNR             = LS_DETL-MBLNR_GR
          I_MJAHR             = LS_DETL-MJAHR_GR
        EXCEPTIONS
          ILLEGAL_COMBINATION = 1
          OTHERS              = 2.

    WHEN 'MBLNR_RE'.
      CHECK LS_DETL-MBLNR_RE IS NOT INITIAL.

      CALL FUNCTION 'MIGO_DIALOG'
        EXPORTING
          I_MBLNR             = LS_DETL-MBLNR_RE
          I_MJAHR             = LS_DETL-MJAHR_RE
        EXCEPTIONS
          ILLEGAL_COMBINATION = 1
          OTHERS              = 2.

    WHEN 'BELNR'.
      CHECK LS_DETL-BELNR IS NOT INITIAL.

      SET PARAMETER ID 'RBN' FIELD LS_DETL-BELNR.
      SET PARAMETER ID 'GJR' FIELD LS_DETL-GJAHR.

      CALL TRANSACTION 'MIR4' AND SKIP FIRST SCREEN.

    WHEN 'EBELN'.
      CHECK LS_DETL-EBELN IS NOT INITIAL.

      CASE LS_DETL-BSTYP.
        WHEN 'F'. "구매오더
          SET PARAMETER ID 'BES' FIELD LS_DETL-EBELN.
          CALL TRANSACTION 'ME23N' AND SKIP FIRST SCREEN.

        WHEN 'L'. "일정라인
          SET PARAMETER ID 'SAG' FIELD LS_DETL-EBELN.
          CALL TRANSACTION 'ME33L' AND SKIP FIRST SCREEN.

        WHEN OTHERS.
      ENDCASE.

    WHEN 'VBELN_IM'.
      CHECK LS_DETL-VBELN_IM IS NOT INITIAL.

      SET PARAMETER ID 'VLM' FIELD LS_DETL-VBELN_IM.
      CALL TRANSACTION 'VL32N' AND SKIP FIRST SCREEN.

    WHEN 'MENGE_GR' OR 'WRBTR_GR' OR 'MENGE_RE' OR 'WRBTR_RE' OR
         'MENGE_IV' OR 'WRBTR_IV'.

      CHECK LS_DETL-EBELN IS NOT INITIAL.

      "구매오더 이력 팝업
      CALL FUNCTION 'MM_HISTORY_POPUP_SHOW'
        EXPORTING
          PI_EBELN = LS_DETL-EBELN
          PI_EBELP = LS_DETL-EBELP
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
*& text
*&---------------------------------------------------------------------*
*&      --> LS_DISP_LIFNR
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
*& Form ALV_GRID_FCAT_MODIFY
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- CT_FCAT
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
*& text
*&---------------------------------------------------------------------*
*&      <-- CT_FCAT
*&---------------------------------------------------------------------*
FORM ALV_GRID_FCAT_ALL CHANGING CT_FCAT TYPE LVC_T_FCAT.

  DEFINE _L_SET_FCAT.
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

  LOOP AT CT_FCAT ASSIGNING FIELD-SYMBOL(<LS_FCAT>).

    CASE <LS_FCAT>-FIELDNAME.
      WHEN'BUDAT_GR'. "입고일(전기일)
        _L_SET_FCAT: 01  'X'  ''  ''  TEXT-C01  ''  ''  ''  ''.
      WHEN'LIFNR'. "공급업체
        _L_SET_FCAT: 02  'X'  ''  ''  TEXT-C02  ''  ''  ''  ''.
      WHEN'LIFNR_TEXT'. "업체명
        _L_SET_FCAT: 03  'X'  ''  ''  TEXT-C03  ''  ''  ''  ''.
      WHEN'MBLNR_GR'. "입고문서
        _L_SET_FCAT: 11  'X'  ''  ''  TEXT-C04  ''  ''  ''  ''.
      WHEN'MJAHR_GR'. "연도
        _L_SET_FCAT: 12  'X'  ''  ''  TEXT-C05  ''  ''  ''  ''.
      WHEN'ZEILE_GR'. "항목
        _L_SET_FCAT: 13  'X'  ''  ''  TEXT-C06  ''  ''  ''  ''.
      WHEN'BKTXT'. "헤더 텍스트
        _L_SET_FCAT: 15  'X'  ''  ''  TEXT-C46  ''  ''  ''  ''.
      WHEN'MATNR'. "자재
        _L_SET_FCAT: 16  'X'  ''  ''  TEXT-C07  ''  ''  ''  ''.
      WHEN'TXZ01'. "자재내역
        _L_SET_FCAT: 17  ''  ''  ''  TEXT-C08  ''  ''  ''  ''.
        <LS_FCAT>-EMPHASIZE = GC_EMPHSZ_C110.
      WHEN'CHARG'. "배치
        _L_SET_FCAT: 18  ''  ''  ''  TEXT-C09  ''  ''  ''  ''.
        <LS_FCAT>-EMPHASIZE = GC_EMPHSZ_C110.
      WHEN'BWTAR'. "평가유형
        _L_SET_FCAT: 19  ''  ''  ''  TEXT-C10  ''  ''  ''  ''.
        <LS_FCAT>-EMPHASIZE = GC_EMPHSZ_C110.

      WHEN'MENGE_GR'. "입고수량
        _L_SET_FCAT: 21  ''  ''  ''  TEXT-C11  ''  ''  ''  'MEINS_GR'.
        <LS_FCAT>-NO_SIGN   = ''.
        <LS_FCAT>-DO_SUM    = 'X'.
        <LS_FCAT>-EMPHASIZE = GC_EMPHSZ_C110.
      WHEN'MEINS_GR'. "단위
        _L_SET_FCAT: 22  ''  ''  ''  TEXT-C12  ''  ''  ''  ''.
        <LS_FCAT>-EMPHASIZE = GC_EMPHSZ_C110.
      WHEN'WRBTR_GR'. "입고금액
        _L_SET_FCAT: 23  ''  ''  ''  TEXT-C13  ''  ''  'WAERS_GR'  ''.
        <LS_FCAT>-DO_SUM    = 'X'.
        <LS_FCAT>-EMPHASIZE = GC_EMPHSZ_C110.
      WHEN'WAERS_GR'. "통화
        _L_SET_FCAT: 24  ''  ''  ''  TEXT-C14  ''  ''  ''  ''.
        <LS_FCAT>-EMPHASIZE = GC_EMPHSZ_C110.

      WHEN'MENGE_RE'. "∑반품수량
        _L_SET_FCAT: 31  ''  ''  ''  TEXT-C15  ''  ''  ''  'MEINS_GR'.
        <LS_FCAT>-NO_SIGN   = ''.
        <LS_FCAT>-DO_SUM    = 'X'.
        <LS_FCAT>-EMPHASIZE = GC_EMPHSZ_C500.
        <LS_FCAT>-NO_ZERO   = 'X'.
      WHEN'WRBTR_RE'. "∑반품금액
        _L_SET_FCAT: 32  ''  ''  ''  TEXT-C16  ''  ''  'WAERS_GR'  ''.
        <LS_FCAT>-DO_SUM    = 'X'.
        <LS_FCAT>-EMPHASIZE = GC_EMPHSZ_C500.
        <LS_FCAT>-NO_ZERO = 'X'.

      WHEN'MENGE_IV'. "∑송장수량
        _L_SET_FCAT: 41  ''  ''  ''  TEXT-C17  ''  ''  ''  'MEINS_GR'.
        <LS_FCAT>-NO_SIGN   = ''.
        <LS_FCAT>-DO_SUM    = 'X'.
        <LS_FCAT>-EMPHASIZE = GC_EMPHSZ_C300.
        <LS_FCAT>-NO_ZERO   = 'X'.
      WHEN'WRBTR_IV'. "∑송장금액
        _L_SET_FCAT: 42  ''  ''  ''  TEXT-C18  ''  ''  'WAERS_IV'  ''.
        <LS_FCAT>-DO_SUM    = 'X'.
        <LS_FCAT>-EMPHASIZE = GC_EMPHSZ_C300.
        <LS_FCAT>-NO_ZERO   = 'X'.
      WHEN'WAERS_IV'. "통화
        _L_SET_FCAT: 43  ''  ''  ''  TEXT-C14  ''  ''  ''  ''.
        <LS_FCAT>-EMPHASIZE = GC_EMPHSZ_C300.
      WHEN'NETPR_IV'. "∑송장단가
        _L_SET_FCAT: 44  ''  ''  ''  TEXT-C19  ''  ''  'WAERS_IV'  ''.
        <LS_FCAT>-EMPHASIZE = GC_EMPHSZ_C300.
        <LS_FCAT>-NO_ZERO   = 'X'.

      WHEN'STATUS'. "상태
        _L_SET_FCAT: 51  ''  ''  ''  TEXT-C20  ''  ''  ''  ''.
      WHEN'MENGE_DIF'. "송장예정수량
        _L_SET_FCAT: 52  ''  ''  ''  TEXT-C21  ''  ''  ''  'MEINS_GR'.
        <LS_FCAT>-NO_SIGN   = ''.
        <LS_FCAT>-DO_SUM    = 'X'.
        <LS_FCAT>-EMPHASIZE = GC_EMPHSZ_C700.
        <LS_FCAT>-NO_ZERO   = 'X'.
      WHEN'WRBTR_DIF'. "송장예정금액
        _L_SET_FCAT: 53  ''  ''  ''  TEXT-C22  ''  ''  'WAERS_GR'  ''.
        <LS_FCAT>-DO_SUM    = 'X'.
        <LS_FCAT>-EMPHASIZE = GC_EMPHSZ_C700.
        <LS_FCAT>-NO_ZERO   = 'X'.

      WHEN'EBELN'. "구매문서
        _L_SET_FCAT: 61  ''  ''  ''  TEXT-C23  ''  ''  ''  ''.
      WHEN'EBELP'. "품목
        _L_SET_FCAT: 62  ''  ''  ''  TEXT-C24  ''  ''  ''  ''.
      WHEN'EINDT'. "납품일
        _L_SET_FCAT: 62  ''  ''  ''  TEXT-C45  ''  ''  ''  ''.
      WHEN 'DEPT_QM'. "검수부서
        _L_SET_FCAT: 63  ''  ''  ''  TEXT-C43  ''  ''  ''  ''.
      WHEN 'DEPT_QM_NAME'.  "검수부서명
        _L_SET_FCAT: 64  ''  ''  ''  TEXT-C44  ''  ''  ''  ''.
      WHEN'PSTYP'. "품목범주
        _L_SET_FCAT: 65  ''  ''  'X'  TEXT-C25  ''  ''  ''  ''.
      WHEN'PSTYP_IND'. "임가공
        _L_SET_FCAT: 65  ''  ''  ''  TEXT-C26  ''  ''  ''  ''.
        <LS_FCAT>-CHECKBOX = 'X'.
      WHEN'VBELN_IM'. "납품문서
        _L_SET_FCAT: 66  ''  ''  ''  TEXT-C27  ''  ''  ''  ''.
      WHEN'VBELP_IM'. "품목
        _L_SET_FCAT: 67  ''  ''  ''  TEXT-C24  ''  ''  ''  ''.
      WHEN'LFIMG'. "납품수량
        _L_SET_FCAT: 68  ''  ''  ''  TEXT-C28  ''  ''  ''  'MEINS_GR'.
        <LS_FCAT>-DO_SUM  = 'X'.
        <LS_FCAT>-NO_ZERO = 'X'.
      WHEN'NETPR_PO'. "구매단가
        _L_SET_FCAT: 69  ''  ''  ''  TEXT-C29  ''  ''  'WAERS_PO'  ''.
      WHEN'WAERS_PO'. "통화
        _L_SET_FCAT: 70  ''  ''  ''  TEXT-C14  ''  ''  ''  ''.
      WHEN'PEINH'. "가격단위
        _L_SET_FCAT: 71  ''  ''  ''  TEXT-C30  ''  ''  ''  ''.
      WHEN'BPRME'. "오더가격단위
        _L_SET_FCAT: 72  ''  ''  ''  TEXT-C31  ''  ''  ''  ''.
      WHEN'WERKS'. "플랜트
        _L_SET_FCAT: 73  ''  ''  ''  TEXT-C32  ''  ''  ''  ''.
      WHEN'WERKS_TEXT'. "플랜트명
        _L_SET_FCAT: 74  ''  ''  ''  TEXT-C33  ''  ''  ''  ''.

      WHEN OTHERS.
        <LS_FCAT>-TECH = 'X'.
    ENDCASE.

    <LS_FCAT>-SCRTEXT_S = <LS_FCAT>-COLTEXT.
    <LS_FCAT>-SCRTEXT_M = <LS_FCAT>-COLTEXT.
    <LS_FCAT>-SCRTEXT_L = <LS_FCAT>-COLTEXT.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form ALV_GRID_FCAT_SUM
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- CT_FCAT
*&---------------------------------------------------------------------*
FORM ALV_GRID_FCAT_SUM CHANGING CT_FCAT TYPE LVC_T_FCAT.

  DEFINE _L_SET_FCAT.
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

  LOOP AT CT_FCAT ASSIGNING FIELD-SYMBOL(<LS_FCAT>).

    CASE <LS_FCAT>-FIELDNAME.
      WHEN'LIFNR'. "공급업체
        _L_SET_FCAT: 01  'X'  ''  ''  TEXT-C02  ''  ''  ''  ''.
        <LS_FCAT>-TECH = GV_TECH+0(1).
      WHEN'LIFNR_TEXT'. "업체명
        _L_SET_FCAT: 02  'X'  ''  ''  TEXT-C03  ''  ''  ''  ''.
        <LS_FCAT>-TECH = GV_TECH+0(1).
      WHEN'MATNR'. "자재
        _L_SET_FCAT: 03  'X'  ''  ''  TEXT-C07  ''  ''  ''  ''.
        <LS_FCAT>-TECH = GV_TECH+1(1).
      WHEN'TXZ01'. "자재내역
        _L_SET_FCAT: 04  'X'  ''  ''  TEXT-C08  ''  ''  ''  ''.
        <LS_FCAT>-TECH = GV_TECH+1(1).
      WHEN'DEPT_QM'. "검수부서
        _L_SET_FCAT: 05  'X'  ''  ''  TEXT-C43  ''  ''  ''  ''.
        <LS_FCAT>-TECH = GV_TECH+2(1).
      WHEN'DEPT_QM_NAME'. "검수부서명
        _L_SET_FCAT: 06  'X'  ''  ''  TEXT-C44  ''  ''  ''  ''.
        <LS_FCAT>-TECH = GV_TECH+2(1).
      WHEN'MENGE_GR'. "입고수량
        _L_SET_FCAT: 11  ''  ''  ''  TEXT-C11  ''  ''  ''  'MEINS_GR'.
        <LS_FCAT>-NO_SIGN = ''.
        <LS_FCAT>-DO_SUM  = 'X'.
        IF GV_TECH+2(1) = 'X'.  "자재 기준일 경우만 수량 보이지만, 검수부서 기준은 예외로 보임
          <LS_FCAT>-TECH = GV_TECH+1(1).
        ENDIF.
      WHEN'MEINS_GR'. "단위
        _L_SET_FCAT: 12  ''  ''  ''  TEXT-C12  ''  ''  ''  ''.
        IF GV_TECH+2(1) = 'X'.
          <LS_FCAT>-TECH = GV_TECH+1(1).
        ENDIF.
      WHEN'WRBTR_GR'. "입고금액
        _L_SET_FCAT: 13  ''  ''  ''  TEXT-C13  ''  ''  'WAERS_GR'  ''.
        <LS_FCAT>-DO_SUM = 'X'.
      WHEN'WAERS_GR'. "통화
        _L_SET_FCAT: 14  ''  ''  ''  TEXT-C14  ''  ''  ''  ''.
      WHEN'MENGE_RE'. "∑반품수량
        _L_SET_FCAT: 21  ''  ''  ''  TEXT-C15  ''  ''  ''  'MEINS_GR'.
        <LS_FCAT>-NO_SIGN   = ''.
        <LS_FCAT>-DO_SUM    = 'X'.
        <LS_FCAT>-EMPHASIZE = GC_EMPHSZ_C500.
        <LS_FCAT>-NO_ZERO   = 'X'.
      WHEN'WRBTR_RE'. "∑반품금액
        _L_SET_FCAT: 22  ''  ''  ''  TEXT-C16  ''  ''  'WAERS_GR'  ''.
        <LS_FCAT>-DO_SUM    = 'X'.
        <LS_FCAT>-EMPHASIZE = GC_EMPHSZ_C500.
        <LS_FCAT>-NO_ZERO   = 'X'.
      WHEN'MENGE_IV'. "∑송장수량
        _L_SET_FCAT: 31  ''  ''  ''  TEXT-C17  ''  ''  ''  'MEINS_GR'.
        <LS_FCAT>-NO_SIGN   = ''.
        <LS_FCAT>-DO_SUM    = 'X'.
        <LS_FCAT>-EMPHASIZE = GC_EMPHSZ_C300.
        <LS_FCAT>-NO_ZERO   = 'X'.
      WHEN'WRBTR_IV'. "∑송장금액
        _L_SET_FCAT: 32  ''  ''  ''  TEXT-C18  ''  ''  'WAERS_IV'  ''.
        <LS_FCAT>-DO_SUM    = 'X'.
        <LS_FCAT>-EMPHASIZE = GC_EMPHSZ_C300.
        <LS_FCAT>-NO_ZERO   = 'X'.
      WHEN'WAERS_IV'. "통화
        _L_SET_FCAT: 33  ''  ''  ''  TEXT-C14  ''  ''  ''  ''.
        <LS_FCAT>-EMPHASIZE = GC_EMPHSZ_C300.
      WHEN'NETPR_IV'. "∑송장단가
        _L_SET_FCAT: 34  ''  ''  ''  TEXT-C19  ''  ''  'WAERS_IV'  ''.
        <LS_FCAT>-EMPHASIZE = GC_EMPHSZ_C300.
        <LS_FCAT>-NO_ZERO = 'X'.
      WHEN'MENGE_DIF'. "송장예정수량
        _L_SET_FCAT: 41  ''  ''  ''  TEXT-C21  ''  ''  ''  'MEINS_GR'.
        <LS_FCAT>-NO_SIGN   = ''.
        <LS_FCAT>-DO_SUM    = 'X'.
        <LS_FCAT>-EMPHASIZE = GC_EMPHSZ_C700.
        <LS_FCAT>-NO_ZERO   = 'X'.
      WHEN'WRBTR_DIF'. "송장예정금액
        _L_SET_FCAT: 42  ''  ''  ''  TEXT-C22  ''  ''  'WAERS_GR'  ''.
        <LS_FCAT>-DO_SUM    = 'X'.
        <LS_FCAT>-EMPHASIZE = GC_EMPHSZ_C700.
        <LS_FCAT>-NO_ZERO   = 'X'.

      WHEN OTHERS.
        <LS_FCAT>-TECH = 'X'.
    ENDCASE.

    <LS_FCAT>-SCRTEXT_S = <LS_FCAT>-COLTEXT.
    <LS_FCAT>-SCRTEXT_M = <LS_FCAT>-COLTEXT.
    <LS_FCAT>-SCRTEXT_L = <LS_FCAT>-COLTEXT.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form ALV_DETL_FCAT_MODIFY
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- CT_FCAT
*&---------------------------------------------------------------------*
FORM ALV_DETL_FCAT_MODIFY CHANGING CT_FCAT TYPE LVC_T_FCAT.

  DEFINE _L_SET_FCAT.
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

  LOOP AT CT_FCAT ASSIGNING FIELD-SYMBOL(<LS_FCAT>).

    CASE <LS_FCAT>-FIELDNAME.
      WHEN'BUDAT_GR'. "입고일(전기일)
        _L_SET_FCAT: 01  'X'  ''  ''  TEXT-C01  ''  ''  ''  ''.
      WHEN'LIFNR'. "공급업체
        _L_SET_FCAT: 02  'X'  ''  ''  TEXT-C02  ''  ''  ''  ''.
      WHEN'LIFNR_TEXT'. "업체명
        _L_SET_FCAT: 03  'X'  ''  ''  TEXT-C03  ''  ''  ''  ''.
      WHEN'MBLNR_GR'. "입고문서
        _L_SET_FCAT: 11  'X'  ''  ''  TEXT-C04  ''  ''  ''  ''.
      WHEN'MJAHR_GR'. "연도
        _L_SET_FCAT: 12  'X'  ''  ''  TEXT-C05  ''  ''  ''  ''.
      WHEN'ZEILE_GR'. "항목
        _L_SET_FCAT: 13  'X'  ''  ''  TEXT-C06  ''  ''  ''  ''.
      WHEN'MATNR'. "자재
        _L_SET_FCAT: 14  'X'  ''  ''  TEXT-C07  ''  ''  ''  ''.
      WHEN'TXZ01'. "자재내역
        _L_SET_FCAT: 15  ''  ''  ''  TEXT-C08  ''  ''  ''  ''.
        <LS_FCAT>-EMPHASIZE = GC_EMPHSZ_C110.
      WHEN'CHARG'. "배치
        _L_SET_FCAT: 16  ''  ''  ''  TEXT-C09  ''  ''  ''  ''.
        <LS_FCAT>-EMPHASIZE = GC_EMPHSZ_C110.
      WHEN'BWTAR'. "평가유형
        _L_SET_FCAT: 17  ''  ''  ''  TEXT-C10  ''  ''  ''  ''.
        <LS_FCAT>-EMPHASIZE = GC_EMPHSZ_C110.

      WHEN'MENGE_GR'. "입고수량
        _L_SET_FCAT: 21  ''  ''  ''  TEXT-C11  ''  ''  ''  'MEINS_GR'.
        <LS_FCAT>-NO_SIGN   = ''.
        <LS_FCAT>-DO_SUM    = 'X'.
        <LS_FCAT>-EMPHASIZE = GC_EMPHSZ_C110.
      WHEN'MEINS_GR'. "단위
        _L_SET_FCAT: 22  ''  ''  ''  TEXT-C12  ''  ''  ''  ''.
        <LS_FCAT>-EMPHASIZE = GC_EMPHSZ_C110.
      WHEN'WRBTR_GR'. "입고금액
        _L_SET_FCAT: 23  ''  ''  ''  TEXT-C13  ''  ''  'WAERS_GR'  ''.
        <LS_FCAT>-DO_SUM    = 'X'.
        <LS_FCAT>-EMPHASIZE = GC_EMPHSZ_C110.
      WHEN'WAERS_GR'. "통화
        _L_SET_FCAT: 24  ''  ''  ''  TEXT-C14  ''  ''  ''  ''.
        <LS_FCAT>-EMPHASIZE = GC_EMPHSZ_C110.

      WHEN'MBLNR_RE'. "반품문서
        _L_SET_FCAT: 31  ''  ''  ''  TEXT-C34  ''  ''  ''  ''.
        <LS_FCAT>-EMPHASIZE = GC_EMPHSZ_C500.
      WHEN'MJAHR_RE'. "연도
        _L_SET_FCAT: 32  ''  ''  ''  TEXT-C05  ''  ''  ''  ''.
        <LS_FCAT>-EMPHASIZE = GC_EMPHSZ_C500.
      WHEN'ZEILE_RE'. "항목
        _L_SET_FCAT: 33  ''  ''  ''  TEXT-C06  ''  ''  ''  ''.
        <LS_FCAT>-EMPHASIZE = GC_EMPHSZ_C500.
      WHEN'MENGE_RE'. "반품수량
        _L_SET_FCAT: 34  ''  ''  ''  TEXT-C35  ''  ''  ''  'MEINS_GR'.
        <LS_FCAT>-NO_SIGN   = ''.
        <LS_FCAT>-DO_SUM    = 'X'.
        <LS_FCAT>-EMPHASIZE = GC_EMPHSZ_C500.
        <LS_FCAT>-NO_ZERO   = 'X'.
      WHEN'WRBTR_RE'. "반품금액
        _L_SET_FCAT: 35  ''  ''  ''  TEXT-C36  ''  ''  'WAERS_GR'  ''.
        <LS_FCAT>-DO_SUM    = 'X'.
        <LS_FCAT>-EMPHASIZE = GC_EMPHSZ_C500.
        <LS_FCAT>-NO_ZERO = 'X'.

      WHEN'BELNR'. "송장문서
        _L_SET_FCAT: 41  ''  ''  ''  TEXT-C37  ''  ''  ''  ''.
        <LS_FCAT>-EMPHASIZE = GC_EMPHSZ_C300.
      WHEN'GJAHR'. "연도
        _L_SET_FCAT: 42  ''  ''  ''  TEXT-C05  ''  ''  ''  ''.
        <LS_FCAT>-EMPHASIZE = GC_EMPHSZ_C300.
      WHEN'BUZEI'. "항목
        _L_SET_FCAT: 43  ''  ''  ''  TEXT-C06  ''  ''  ''  ''.
        <LS_FCAT>-EMPHASIZE = GC_EMPHSZ_C300.
      WHEN'BUDAT_IV'. "전기일
        _L_SET_FCAT: 44  ''  ''  ''  TEXT-C38  ''  ''  ''  ''.
        <LS_FCAT>-EMPHASIZE = GC_EMPHSZ_C300.
      WHEN'BLDAT_IV'. "증빙일
        _L_SET_FCAT: 45  ''  ''  ''  TEXT-C39  ''  ''  ''  ''.
        <LS_FCAT>-EMPHASIZE = GC_EMPHSZ_C300.
      WHEN'VGABENM'. "유형
        _L_SET_FCAT: 46  ''  ''  ''  TEXT-C40  ''  ''  ''  ''.
        <LS_FCAT>-EMPHASIZE = GC_EMPHSZ_C300.
      WHEN'MENGE_IV'. "송장수량
        _L_SET_FCAT: 47  ''  ''  ''  TEXT-C41  ''  ''  ''  'MEINS_GR'.
        <LS_FCAT>-NO_SIGN   = ''.
        <LS_FCAT>-DO_SUM    = 'X'.
        <LS_FCAT>-EMPHASIZE = GC_EMPHSZ_C300.
        <LS_FCAT>-NO_ZERO   = 'X'.
      WHEN'WRBTR_IV'. "송장금액
        _L_SET_FCAT: 48  ''  ''  ''  TEXT-C42  ''  ''  'WAERS_IV'  ''.
        <LS_FCAT>-DO_SUM    = 'X'.
        <LS_FCAT>-EMPHASIZE = GC_EMPHSZ_C300.
        <LS_FCAT>-NO_ZERO   = 'X'.
      WHEN'WAERS_IV'. "통화
        _L_SET_FCAT: 49  ''  ''  ''  TEXT-C14  ''  ''  ''  ''.
        <LS_FCAT>-EMPHASIZE = GC_EMPHSZ_C300.

      WHEN'EBELN'. "구매문서
        _L_SET_FCAT: 61  ''  ''  ''  TEXT-C23  ''  ''  ''  ''.
      WHEN'EBELP'. "품목
        _L_SET_FCAT: 62  ''  ''  ''  TEXT-C24  ''  ''  ''  ''.
      WHEN'VBELN_IM'. "납품문서
        _L_SET_FCAT: 63  ''  ''  ''  TEXT-C27  ''  ''  ''  ''.
      WHEN'VBELP_IM'. "품목
        _L_SET_FCAT: 64  ''  ''  ''  TEXT-C24  ''  ''  ''  ''.
      WHEN'LFIMG'. "납품수량
        _L_SET_FCAT: 65  ''  ''  ''  TEXT-C28  ''  ''  'MEINS_GR'  ''.
        <LS_FCAT>-NO_SIGN = ''.
        <LS_FCAT>-DO_SUM  = 'X'.
        <LS_FCAT>-NO_ZERO = 'X'.
      WHEN'NETPR_PO'. "구매단가
        _L_SET_FCAT: 66  ''  ''  ''  TEXT-C29  ''  ''  'WAERS_PO'  ''.
      WHEN'WAERS_PO'. "통화
        _L_SET_FCAT: 67  ''  ''  ''  TEXT-C14  ''  ''  ''  ''.
      WHEN'PEINH'. "가격단위
        _L_SET_FCAT: 68  ''  ''  ''  TEXT-C30  ''  ''  ''  ''.
      WHEN'BPRME'. "오더가격단위
        _L_SET_FCAT: 69  ''  ''  ''  TEXT-C31  ''  ''  ''  ''.

      WHEN OTHERS.
        <LS_FCAT>-TECH = 'X'.
    ENDCASE.

    <LS_FCAT>-SCRTEXT_S = <LS_FCAT>-COLTEXT.
    <LS_FCAT>-SCRTEXT_M = <LS_FCAT>-COLTEXT.
    <LS_FCAT>-SCRTEXT_L = <LS_FCAT>-COLTEXT.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form ALV_GRID_SET_SORT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- CT_SORT
*&---------------------------------------------------------------------*
FORM ALV_GRID_SET_SORT CHANGING CT_SORT TYPE LVC_T_SORT.

  DATA: LS_SORT TYPE LVC_S_SORT.

  DEFINE _L_APPEND_SORT.

    LS_SORT-SPOS      = &1.
    LS_SORT-FIELDNAME = &2.
    LS_SORT-UP        = &3.
    LS_SORT-DOWN      = &4.
    LS_SORT-GROUP     = &5.
    LS_SORT-SUBTOT    = &6.
    LS_SORT-COMP      = &7.
    LS_SORT-LEVEL     = &8.

    APPEND LS_SORT TO CT_SORT.

  END-OF-DEFINITION.

  _L_APPEND_SORT: 1  'BUDAT_GR'  'X'  ''  ''  ''   ''  ''.

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
      IV_CELLS   = ''           "공통 스트럭쳐 CELLS필드 사용하지 않을 경우
      IT_HEADER  = LT_HEADER.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_GRID_DETL
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form SET_GRID_DETL
*&---------------------------------------------------------------------*
FORM SET_GRID_DETL.

  IF GRF_CONT_DETL IS NOT INITIAL.
    CALL METHOD GRF_CONT_DETL->FREE.
    CLEAR: GRF_CONT_DETL, GRF_GRID_DETL.
  ENDIF.

* Creating Custom container instance
*----------------------------------------------------
* Create Custom Container..
*----------------------------------------------------
  CREATE OBJECT GRF_CONT_DETL
    EXPORTING
      CONTAINER_NAME = 'CONT_DETL'
    EXCEPTIONS
      OTHERS         = 1.

*--------------------------------
* Create Alv Grid
*--------------------------------
  CREATE OBJECT GRF_GRID_DETL
    EXPORTING
      IV_NAME    = 'ALV_DETL'   "다수의 그리드일 경우 식별하기 위함..
      IRF_PARENT = GRF_CONT_DETL.

*--------------------------------
* Dislay Grid..
*--------------------------------
  GRF_GRID_DETL->SET_GRID( CHANGING  CT_DATA = GT_DETL ).

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SORT_BY_CRITERIA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> GV_OK_CODE
*&---------------------------------------------------------------------*
FORM SORT_BY_CRITERIA USING IV_OK_CODE.

  DATA: LT_SORT TYPE LVC_T_SORT,
        LS_SORT TYPE LVC_S_SORT.

  "업체/자재/검수부서
  CONSTANTS: LC_TECH_X1(3) TYPE C VALUE ' XX',
             LC_TECH_X2(3) TYPE C VALUE 'X X',
             LC_TECH_X3(3) TYPE C VALUE '  X',
             LC_TECH_X4(3) TYPE C VALUE 'XX '.

  DEFINE _L_SET_SORT.
    LS_SORT-SPOS      = &1.
    LS_SORT-FIELDNAME = &2.
    LS_SORT-SUBTOT    = &3.
*    LS_SORT-EXPA      = &4.
    LS_SORT-UP        = 'X'.
    APPEND LS_SORT TO LT_SORT.
  END-OF-DEFINITION.

  CLEAR: GT_DISP, GT_FCAT, GV_TECH.

*--------------------------------
* Summary 기준 별 Sorting, Data 및 Field Catalog 조정
*--------------------------------
  CASE IV_OK_CODE.
    WHEN 'ALL'. "전체
      GV_CRITERIA = GC_CRITERIA_ALL.
      GT_DISP = GT_ALL.

      _L_SET_SORT: 1  'BUDAT_GR'  ''.

    WHEN 'LIFNR'.  "업체별
      GV_CRITERIA = GC_CRITERIA_LIFNR.
      MOVE-CORRESPONDING GT_LIFNR_SUM TO GT_DISP.

      _L_SET_SORT: 1  'LIFNR'  'X',
                   2  'LIFNR_TEXT'  ''.

      GV_TECH = LC_TECH_X1.

    WHEN 'MATNR'.  "자재별
      GV_CRITERIA = GC_CRITERIA_MATNR.
      MOVE-CORRESPONDING GT_MATNR_SUM TO GT_DISP.

      _L_SET_SORT: 1  'MATNR'  '',
                   2  'TXZ01'  ''.

      GV_TECH = LC_TECH_X2.

    WHEN 'LIMAT'.  "업체/자재별
      GV_CRITERIA = GC_CRITERIA_LIMAT.
      MOVE-CORRESPONDING GT_LIMAT_SUM TO GT_DISP.

      _L_SET_SORT: 1  'LIFNR'  '',
                   2  'LIFNR_TEXT'  '',
                   3  'MATNR'  '',
                   4  'TXZ01'  ''.

      GV_TECH = LC_TECH_X3.

    WHEN 'DEPT_QM'.  "검수부서별
      GV_CRITERIA = GC_CRITERIA_DEPT_QM.
      MOVE-CORRESPONDING GT_DEPT_QM_SUM TO GT_DISP.

      _L_SET_SORT: 1  'DEPT_QM'  '',
                   2  'DEPT_QM_NAME'  ''.

      GV_TECH = LC_TECH_X4.

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
*&---------------------------------------------------------------------*
*& Form REFRESH_GRID_DISPLAY
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM REFRESH_GRID_DISPLAY.

  DATA LS_STABLE TYPE LVC_S_STBL.

*  LS_STABLE = VALUE #( ROW = 'X' COL = 'X' ).

  CALL METHOD GRF_GRID->REFRESH_TABLE_DISPLAY
    EXPORTING
      IS_STABLE = LS_STABLE.

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
    CLEAR LS_HEADER.
    LS_HEADER-KEY   = &1.
    LS_HEADER-INFO  = &2.
    LS_HEADER-TEXT  = &3.
    APPEND LS_HEADER TO CT_HEADER.
  END-OF-DEFINITION.

*U2> 번역을 위한 TEXT SYMBOL 변환
*  CONSTANTS : LC_BUKRS(20) TYPE C VALUE '회사코드'.

  DATA: LV_SELE_TEXT(30) TYPE C,
        LV_BILL_TEXT(30) TYPE C.

*---------------------------------------
* Header Text 지정
*---------------------------------------
*-프로그램명 헤더에서 제외

  "회사코드 TEXT
  SELECT SINGLE BUTXT
    FROM T001
   WHERE BUKRS = @P_BUKRS
    INTO @DATA(LV_BUTXT).

  "선택조건 TEXT
  CASE 'X'.
    WHEN P_RS1.
      LV_SELE_TEXT = TEXT-F02.
    WHEN P_RS2.
      LV_SELE_TEXT = TEXT-F03.
    WHEN P_RS3.
      LV_SELE_TEXT = TEXT-F04.
    WHEN P_RS4.
      LV_SELE_TEXT = TEXT-F05.
  ENDCASE.

  "세금계산서 작성방법 TEXT
  CASE 'X'.
    WHEN P_RB1.
      LV_BILL_TEXT = TEXT-F06.
    WHEN P_RB2.
      LV_BILL_TEXT = TEXT-F07.
  ENDCASE.


*-----------------------------------
* Header Column 지정
*-----------------------------------

*U2> 번역을 위한 TEXT SYMBOL 변환 - START
*  _L_SET_HEADER: LC_BUKRS   P_BUKRS   LV_BUTXT,
  _L_SET_HEADER: TEXT-F01   P_BUKRS   LV_BUTXT,
*U2> 번역을 위한 TEXT SYMBOL 변환 - END
                 TEXT-B02   ''        LV_SELE_TEXT,
                 TEXT-B03   ''        LV_BILL_TEXT.

*-----------------------------------
* Header 주석
*-----------------------------------

*_g_set_value:'20:10:70'.  "Default 비율 (비율 변경시 사용)

ENDFORM.
