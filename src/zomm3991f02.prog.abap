*&---------------------------------------------------------------------*
*& Include          ZOMM3991F02
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
    GRF_GRID->REFRESH_GRID_DISPLAY( ).
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

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CREATE_ALV_GRID
*&---------------------------------------------------------------------*
FORM CREATE_ALV_GRID.

  DATA: LS_TOOLBTN TYPE ZSCN00004.

*--------------------------------------------------
* User Toolbar Button (Default Setting).
*--------------------------------------------------
  LS_TOOLBTN-BTN_EXLD   = 'X'.       "Excel Download

  CREATE OBJECT GRF_GRID
    EXPORTING
      IV_NAME    = 'ALV_GRID'   "다수의 그리드일 경우 식별하기 위함..
      IRF_PARENT = GRF_DOCKING_CON
      IS_TOOLBTN = LS_TOOLBTN
      IV_CELLC   = ''           "공통 스트럭쳐 CELLC필드 사용하지 않을 경우
      IV_CELLS   = ''.          "공통 스트럭쳐 CELLS필드 사용하지 않을 경우

ENDFORM.
*&---------------------------------------------------------------------*
*& Form ALV_GRID_FCAT_MODIFY
*&---------------------------------------------------------------------*
FORM ALV_GRID_FCAT_MODIFY CHANGING CT_FCAT TYPE LVC_T_FCAT.

  IF P_DOPO = 'X'.
    PERFORM ALV_GRID_FCAT_MODIFY_DO CHANGING CT_FCAT.
  ELSE.
    PERFORM ALV_GRID_FCAT_MODIFY_IM CHANGING CT_FCAT.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form ALV_GRID_FCAT_MODIFY_DO
*&---------------------------------------------------------------------*
FORM ALV_GRID_FCAT_MODIFY_DO CHANGING CT_FCAT TYPE LVC_T_FCAT.

  DEFINE _L_SET_FCAT.
    <LS_FCAT>-COL_POS    = &1.
    <LS_FCAT>-KEY        = &2.
    <LS_FCAT>-PARAMETER0 = &3.
    <LS_FCAT>-COLTEXT    = &4.
    <LS_FCAT>-JUST       = &5.
    <LS_FCAT>-F4AVAILABL = &6.
    <LS_FCAT>-CFIELDNAME = &7.
    <LS_FCAT>-QFIELDNAME = &8.
    <LS_FCAT>-OUTPUTLEN  = &9.
*    <LS_FCAT>-COL_OPT    = 'X'.
  END-OF-DEFINITION.

  SORT CT_FCAT BY FIELDNAME.

*----------------------------------------
* Set Modify Field..
*----------------------------------------
  LOOP AT CT_FCAT ASSIGNING FIELD-SYMBOL(<LS_FCAT>).

    CASE <LS_FCAT>-FIELDNAME.
      WHEN 'STATU'.  "처리결과
        _L_SET_FCAT: 01  'X'  ''  TEXT-C01  'C'  ''  ''  ''  '06'.
      WHEN 'CNTR_NO'.  "AS-IS 계약번호
        _L_SET_FCAT: 12  'X'  ''  TEXT-C02  ''  ''  ''  ''  '12'.
      WHEN 'CNTR_REV'.  "AS-IS 차수
        _L_SET_FCAT: 13  'X'  ''  TEXT-C03  ''  ''  ''  ''  '04'.
      WHEN 'CNTR_ITEM'.  "AS-IS 품목
        _L_SET_FCAT: 14  'X'  ''  TEXT-C04  ''  ''  ''  ''  '05'.

      WHEN 'BSART'.  "문서유형
        _L_SET_FCAT: 21  ''  ''  TEXT-C05  ''  ''  ''  ''  '06'.
      WHEN 'EKGRP'.  "구매그룹
        _L_SET_FCAT: 22  ''  ''  TEXT-C08  ''  ''  ''  ''  '06'.
      WHEN 'TITLE'.  "발주명
        _L_SET_FCAT: 23  ''  ''  TEXT-C09  ''  ''  ''  ''  '15'.
      WHEN 'ORDER'.  "발주담당자
        _L_SET_FCAT: 24  ''  ''  TEXT-C10  ''  ''  ''  ''  '12'.
      WHEN 'EXPER'.  "지출발의자
        _L_SET_FCAT: 25  ''  ''  TEXT-C06  ''  ''  ''  ''  '12'.
      WHEN 'QMPER'.  "검수담당자
        _L_SET_FCAT: 26  ''  ''  TEXT-C07  ''  ''  ''  ''  '12'.
      WHEN 'ABSGR'.  "세금계산서발행방식
        _L_SET_FCAT: 27  ''  ''  TEXT-C13  ''  ''  ''  ''  '06'.
      WHEN 'MTYPE'.  "임가공발주
        _L_SET_FCAT: 28  ''  ''  TEXT-C14  ''  ''  ''  ''  '06'.

      WHEN 'LIFNR'.  "공급업체
        _L_SET_FCAT: 31  ''  ''  TEXT-C11  ''  ''  ''  ''  '10'.
      WHEN 'VERKF'.  "담당자
        _L_SET_FCAT: 32  ''  ''  TEXT-C12  ''  ''  ''  ''  '10'.
      WHEN 'BEDAT'.  "계약체결일
        _L_SET_FCAT: 33  ''  ''  TEXT-C15  ''  ''  ''  ''  '10'.
      WHEN 'ZTERM'.  "지급조건
        _L_SET_FCAT: 34  ''  ''  TEXT-C16  ''  ''  ''  ''  '06'.
      WHEN 'INCO1'.  "인도조건
        _L_SET_FCAT: 35  ''  ''  TEXT-C17  ''  ''  ''  ''  '06'.
      WHEN 'WAERS'.  "통화
        _L_SET_FCAT: 36  ''  ''  TEXT-C18  ''  ''  ''  ''  '05'.
      WHEN 'DPPCT'.  "선급비율
        _L_SET_FCAT: 37  ''  ''  TEXT-C50  ''  ''  ''  ''  '06'.
      WHEN 'ZREAL_COST'.  "실비정산여부
        _L_SET_FCAT: 38  ''  ''  TEXT-C19  ''  ''  ''  ''  '06'.

      WHEN 'MATNR'.  "자재
        _L_SET_FCAT: 50  ''  ''  TEXT-C21  ''  ''  ''  ''  '15'.
      WHEN 'MAKTX'.  "자재내역
        _L_SET_FCAT: 51  ''  ''  TEXT-C53  ''  ''  ''  ''  '20'.
      WHEN 'BWTAR'.  "평가유형
        _L_SET_FCAT: 52  ''  ''  TEXT-C22  ''  ''  ''  ''  '06'.
      WHEN 'MEINS'.  "단위
        _L_SET_FCAT: 53  ''  ''  TEXT-C23  ''  ''  ''  ''  '05'.
      WHEN 'MENGE'.  "수량
        _L_SET_FCAT: 54  ''  ''  TEXT-C24  ''  ''  ''  'MEINS'  '12'.
      WHEN 'NETPR'.  "단가
        _L_SET_FCAT: 55  ''  ''  TEXT-C25  ''  ''  'WAERS'  ''  '12'.
      WHEN 'PEINH'.  "가격단위
        _L_SET_FCAT: 56  ''  ''  TEXT-C26  ''  ''  ''  ''  '06'.
      WHEN 'MWSKZ'.  "세금코드
        _L_SET_FCAT: 57  ''  ''  TEXT-C20  ''  ''  ''  ''  '06'.
      WHEN 'MIUWR'.  "품목가감액
        _L_SET_FCAT: 58  ''  ''  TEXT-C31  ''  ''  'WAERS'  ''  '06'.
      WHEN 'ZPRICE_CHANGE'.  "조정사유코드
        _L_SET_FCAT: 59  ''  ''  TEXT-C32  ''  ''  ''  ''  '06'.
      WHEN 'ZPRICE_REASON'.  "조정사유상세
        _L_SET_FCAT: 60  ''  ''  TEXT-C33  ''  ''  ''  ''  '06'.

      WHEN 'WERKS'.  "플랜트
        _L_SET_FCAT: 61  ''  ''  TEXT-C27  ''  ''  ''  ''  '06'.
      WHEN 'LGORT'.  "창고
        _L_SET_FCAT: 62  ''  ''  TEXT-C34  ''  ''  ''  ''  '06'.
      WHEN 'EINDT'.  "납품일
        _L_SET_FCAT: 63  ''  ''  TEXT-C35  ''  ''  ''  ''  '10'.
      WHEN 'UMWRK'.  "입고플랜트
        _L_SET_FCAT: 64  ''  ''  TEXT-C36  ''  ''  ''  ''  '06'.
      WHEN 'SC_VENDER'.  "SC업체
        _L_SET_FCAT: 65  ''  ''  TEXT-C37  ''  ''  ''  ''  '10'.


      WHEN 'SAKTO'.  "G/L계정
        _L_SET_FCAT: 71  ''  ''  TEXT-C28  ''  ''  ''  ''  '10'.
      WHEN 'KOSTL'.  "코스트센터
        _L_SET_FCAT: 72  ''  ''  TEXT-C29  ''  ''  ''  ''  '10'.
      WHEN 'WBSNO'.  "WBS
        _L_SET_FCAT: 73  ''  ''  TEXT-C30  ''  ''  ''  ''  '10'.
      WHEN 'VBELN'.  "영업오더
        _L_SET_FCAT: 74  ''  ''  TEXT-C38  ''  ''  ''  ''  '10'.
      WHEN 'VBELP'.  "영업품목
        _L_SET_FCAT: 75  ''  ''  TEXT-C39  ''  ''  ''  ''  '10'.
      WHEN 'IDNLF'.  "공급업체 자재번호
        _L_SET_FCAT: 76  ''  ''  TEXT-C48  ''  ''  ''  ''  '15'.
      WHEN 'CHARG'.  "배치
        _L_SET_FCAT: 77  ''  ''  TEXT-C49  ''  ''  ''  ''  '10'.

      WHEN 'EBELN'. "발주번호
        _L_SET_FCAT: 91  ''  ''  TEXT-C51  ''  ''  ''  ''  '10'.
      WHEN 'EBELP'. "발주품목
        _L_SET_FCAT: 92  ''  ''  TEXT-C52  ''  ''  ''  ''  '06'.
      WHEN 'MESSAGE'. "메시지
        _L_SET_FCAT: 99  ''  ''  TEXT-C40  ''  ''  ''  ''  '50'.

      WHEN OTHERS.
        <LS_FCAT>-TECH = 'X'.
    ENDCASE.

    <LS_FCAT>-SCRTEXT_S = <LS_FCAT>-COLTEXT.
    <LS_FCAT>-SCRTEXT_M = <LS_FCAT>-COLTEXT.
    <LS_FCAT>-SCRTEXT_L = <LS_FCAT>-COLTEXT.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form ALV_GRID_FCAT_MODIFY_IM
*&---------------------------------------------------------------------*
FORM ALV_GRID_FCAT_MODIFY_IM CHANGING CT_FCAT TYPE LVC_T_FCAT.

  DEFINE _L_SET_FCAT.
    <LS_FCAT>-COL_POS    = &1.
    <LS_FCAT>-KEY        = &2.
    <LS_FCAT>-PARAMETER0 = &3.
    <LS_FCAT>-COLTEXT    = &4.
    <LS_FCAT>-JUST       = &5.
    <LS_FCAT>-F4AVAILABL = &6.
    <LS_FCAT>-CFIELDNAME = &7.
    <LS_FCAT>-QFIELDNAME = &8.
    <LS_FCAT>-OUTPUTLEN  = &9.
*    <LS_FCAT>-COL_OPT    = 'X'.
  END-OF-DEFINITION.

  SORT CT_FCAT BY FIELDNAME.

*----------------------------------------
* Set Modify Field..
*----------------------------------------
  LOOP AT CT_FCAT ASSIGNING FIELD-SYMBOL(<LS_FCAT>).

    CASE <LS_FCAT>-FIELDNAME.
      WHEN 'STATU'.  "처리결과
        _L_SET_FCAT: 01  'X'  ''  TEXT-C01  'C'  ''  ''  ''  '06'.
      WHEN 'CNTR_NO'.  "AS-IS 계약번호
        _L_SET_FCAT: 11  'X'  ''  TEXT-C02  ''  ''  ''  ''  '12'.
      WHEN 'CNTR_REV'.  "AS-IS 차수
        _L_SET_FCAT: 12  'X'  ''  TEXT-C03  ''  ''  ''  ''  '04'.
      WHEN 'CNTR_ITEM'.  "AS-IS 품목
        _L_SET_FCAT: 13  'X'  ''  TEXT-C04  ''  ''  ''  ''  '05'.

      WHEN 'BSART'.  "문서유형
        _L_SET_FCAT: 21  ''  ''  TEXT-C05  ''  ''  ''  ''  '06'.
      WHEN 'EKGRP'.  "구매그룹
        _L_SET_FCAT: 22  ''  ''  TEXT-C08  ''  ''  ''  ''  '06'.
      WHEN 'TITLE'.  "발주명
        _L_SET_FCAT: 23  ''  ''  TEXT-C09  ''  ''  ''  ''  '30'.
      WHEN 'ORDER'.  "발주담당자
        _L_SET_FCAT: 24  ''  ''  TEXT-C10  ''  ''  ''  ''  '12'.
      WHEN 'EXPER'.  "지출발의자
        _L_SET_FCAT: 25  ''  ''  TEXT-C06  ''  ''  ''  ''  '12'.

      WHEN 'LIFNR'.  "공급업체
        _L_SET_FCAT: 31  ''  ''  TEXT-C11  ''  ''  ''  ''  '10'.
      WHEN 'VERKF'.  "담당자
        _L_SET_FCAT: 32  ''  ''  TEXT-C12  ''  ''  ''  ''  '10'.
      WHEN 'BEDAT'.  "계약체결일
        _L_SET_FCAT: 33  ''  ''  TEXT-C15  ''  ''  ''  ''  '10'.
      WHEN 'ZTERM'.  "지급조건
        _L_SET_FCAT: 34  ''  ''  TEXT-C16  ''  ''  ''  ''  '06'.
      WHEN 'INCO1'.  "인도조건
        _L_SET_FCAT: 35  ''  ''  TEXT-C17  ''  ''  ''  ''  '06'.
      WHEN 'WAERS'.  "통화
        _L_SET_FCAT: 36  ''  ''  TEXT-C18  ''  ''  ''  ''  '05'.
      WHEN 'DPPCT'.  "선급비율
        _L_SET_FCAT: 37  ''  ''  TEXT-C50  ''  ''  ''  ''  '06'.

      WHEN 'ZEINSPECT'.  "수입검사여부
        _L_SET_FCAT: 41  ''  ''  TEXT-C41  ''  ''  ''  ''  '06'.
      WHEN 'ZEMANAGE2'.  "관리번호
        _L_SET_FCAT: 42  ''  ''  TEXT-C42  ''  ''  ''  ''  '10'.
      WHEN 'HERKL'.  "원산지
        _L_SET_FCAT: 43  ''  ''  TEXT-C43  ''  ''  ''  ''  '05'.
      WHEN 'ZESHIPTYPE'.  "선적구분
        _L_SET_FCAT: 44  ''  ''  TEXT-C44  ''  ''  ''  ''  '05'.
      WHEN 'ZEDEDLINE'.  "선적기한
        _L_SET_FCAT: 45  ''  ''  TEXT-C45  ''  ''  ''  ''  '10'.
      WHEN 'INCO2_L'.  "선적항
        _L_SET_FCAT: 46  ''  ''  TEXT-C46  ''  ''  ''  ''  '06'.
      WHEN 'INCO3_L'.  "도착항
        _L_SET_FCAT: 47  ''  ''  TEXT-C47  ''  ''  ''  ''  '06'.

      WHEN 'MATNR'.  "자재
        _L_SET_FCAT: 50  ''  ''  TEXT-C21  ''  ''  ''  ''  '15'.
      WHEN 'MAKTX'.  "자재내역
        _L_SET_FCAT: 51  ''  ''  TEXT-C53  ''  ''  ''  ''  '20'.
      WHEN 'BWTAR'.  "평가유형
        _L_SET_FCAT: 52  ''  'C'  TEXT-C22  ''  ''  ''  ''  '06'.
      WHEN 'MEINS'.  "단위
        _L_SET_FCAT: 53  ''  ''  TEXT-C23  ''  ''  ''  ''  '05'.
      WHEN 'MENGE'.  "수량
        _L_SET_FCAT: 54  ''  ''  TEXT-C24  ''  ''  ''  'MEINS'  '12'.
      WHEN 'NETPR'.  "단가
        _L_SET_FCAT: 55  ''  'C'  TEXT-C25  ''  ''  'WAERS'  ''  '12'.
      WHEN 'PEINH'.  "가격단위
        _L_SET_FCAT: 56  ''  ''  TEXT-C26  ''  ''  ''  ''  '06'.
      WHEN 'MWSKZ'.  "세금코드
        _L_SET_FCAT: 57  ''  ''  TEXT-C20  ''  ''  ''  ''  '06'.
      WHEN 'MIUWR'.  "품목가감액
        _L_SET_FCAT: 58  ''  ''  TEXT-C31  ''  ''  'WAERS'  ''  '06'.
      WHEN 'ZPRICE_CHANGE'.  "조정사유코드
        _L_SET_FCAT: 59  ''  ''  TEXT-C32  ''  ''  ''  ''  '06'.
      WHEN 'ZPRICE_REASON'.  "조정사유상세
        _L_SET_FCAT: 60  ''  ''  TEXT-C33  ''  ''  ''  ''  '06'.

      WHEN 'WERKS'.  "플랜트
        _L_SET_FCAT: 61  ''  ''  TEXT-C27  ''  ''  ''  ''  '06'.
      WHEN 'LGORT'.  "창고
        _L_SET_FCAT: 62  ''  ''  TEXT-C34  ''  ''  ''  ''  '06'.
      WHEN 'EINDT'.  "납품일
        _L_SET_FCAT: 63  ''  ''  TEXT-C35  ''  ''  ''  ''  '10'.
      WHEN 'UMWRK'.  "입고플랜트
        _L_SET_FCAT: 64  ''  ''  TEXT-C36  ''  ''  ''  ''  '06'.
      WHEN 'UMWRK'.  "SC업체
        _L_SET_FCAT: 65  ''  ''  TEXT-C37  ''  ''  ''  ''  '10'.


      WHEN 'SAKTO'.  "G/L계정
        _L_SET_FCAT: 71  ''  'C'  TEXT-C28  ''  ''  ''  ''  '10'.
      WHEN 'KOSTL'.  "코스트센터
        _L_SET_FCAT: 72  ''  'C'  TEXT-C29  ''  ''  ''  ''  '10'.
      WHEN 'WBSNO'.  "WBS
        _L_SET_FCAT: 73  ''  'C'  TEXT-C30  ''  ''  ''  ''  '10'.
      WHEN 'VBELN'.  "영업오더
        _L_SET_FCAT: 74  ''  'C'  TEXT-C38  ''  ''  ''  ''  '10'.
      WHEN 'VBELP'.  "영업품목
        _L_SET_FCAT: 75  ''  'C'  TEXT-C39  ''  ''  ''  ''  '10'.

      WHEN 'EBELN'. "발주번호
        _L_SET_FCAT: 91  ''  ''  TEXT-C51  ''  ''  ''  ''  '10'.
      WHEN 'EBELP'. "발주품목
        _L_SET_FCAT: 92  ''  ''  TEXT-C52  ''  ''  ''  ''  '06'.
      WHEN 'MESSAGE'. "메시지
        _L_SET_FCAT: 99  ''  ''  TEXT-C40  ''  ''  ''  ''  '50'.

      WHEN OTHERS.
        <LS_FCAT>-TECH = 'X'.
    ENDCASE.

    <LS_FCAT>-SCRTEXT_S = <LS_FCAT>-COLTEXT.
    <LS_FCAT>-SCRTEXT_M = <LS_FCAT>-COLTEXT.
    <LS_FCAT>-SCRTEXT_L = <LS_FCAT>-COLTEXT.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form EVT_GRID_TOOLBAR
*&---------------------------------------------------------------------*
FORM EVT_GRID_TOOLBAR CHANGING CT_TOOLBAR TYPE TTB_BUTTON.

  DEFINE _L_ADD_TOOLBAR.
    LS_ADD_TOOLBAR-FUNCTION    = &1.
    LS_ADD_TOOLBAR-ICON        = &2.
    LS_ADD_TOOLBAR-QUICKINFO   = &3.
    LS_ADD_TOOLBAR-BUTN_TYPE   = &4.
    LS_ADD_TOOLBAR-DISABLED    = &5.
    LS_ADD_TOOLBAR-TEXT        = &6.

    APPEND LS_ADD_TOOLBAR TO CT_TOOLBAR.
  END-OF-DEFINITION.

*----------------------------
*-- 추가 User Toolbar
*----------------------------
  DATA:LS_ADD_TOOLBAR TYPE STB_BUTTON.

  _L_ADD_TOOLBAR: 'BTN_CONVERS'   ICON_TRANSLATION  TEXT-U05 '' '' TEXT-U05,
                  'BTN_TEST_SAVE'     ICON_CHECK        TEXT-U06 '' '' TEXT-U06,
                  'BTN_ERR_DOWN'  ICON_EXPORT       TEXT-U07 '' '' TEXT-U07.

ENDFORM.
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
      SET PARAMETER ID 'BES' FIELD LS_DISP-EBELN.
      CALL TRANSACTION 'ME23N'.

    WHEN OTHERS.
      RETURN.

  ENDCASE.

ENDFORM.
