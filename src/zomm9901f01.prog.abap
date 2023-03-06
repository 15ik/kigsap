*&---------------------------------------------------------------------*
*& Include          ZOMM9901F01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form FILE_OPEN
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- P_FILE
*&---------------------------------------------------------------------*
FORM FILE_OPEN CHANGING CV_FILE.

  DATA: LT_FILETABLE TYPE FILETABLE,
        LS_FILETABLE TYPE FILE_TABLE.

  DATA LV_RC TYPE I.

  CALL METHOD CL_GUI_FRONTEND_SERVICES=>FILE_OPEN_DIALOG
    EXPORTING
      FILE_FILTER             = CL_GUI_FRONTEND_SERVICES=>FILETYPE_EXCEL
    CHANGING
      FILE_TABLE              = LT_FILETABLE[]
      RC                      = LV_RC
    EXCEPTIONS
      FILE_OPEN_DIALOG_FAILED = 1
      CNTL_ERROR              = 2
      ERROR_NO_GUI            = 3
      NOT_SUPPORTED_BY_GUI    = 4
      OTHERS                  = 5.

  READ TABLE LT_FILETABLE INTO LS_FILETABLE INDEX 1.

  CV_FILE = LS_FILETABLE-FILENAME.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_LIST_BOX
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM SET_LIST_BOX.

  CONSTANTS: LC_BUKRS(7) VALUE 'P_BUKRS'.

  DATA: LV_NAME TYPE VRM_ID,
        LT_LIST TYPE VRM_VALUES.     "Key, Text

* 회사코드
  LV_NAME = LC_BUKRS.

  ZCL_MM_COMMON=>COMMON_CONFIG(
     EXPORTING IS_COMMON = VALUE #( M = 'A1' D = 'A1000' S = 'AA100' )
                                    IT_WHERE = VALUE #( ( FIELD = 1 VALUE = 'BUKRS' ) )
     IMPORTING ET_OUTTAB = DATA(LT_CONFIG) ).

  LT_LIST = CORRESPONDING #( LT_CONFIG MAPPING KEY = FIELD2  TEXT = FIELD3  ).

  _G_SET_VALUES: LV_NAME LT_LIST.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CREATE_OBJ
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM CREATE_OBJ.

  CREATE OBJECT GRF_GRID.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CHECK_AUTHORITY
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM CHECK_AUTHORITY.

** 권한체크 룰
*  call function 'ZFMM_AUTH_CHECK'
*    EXPORTING
**     IV_USER  = SY-UNAME.
**     IV_OBJECT                   = 'ZMM_COMMON'
*      IV_BUKRS = P_BUKRS.
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
*     OTHERS  = 6
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4 DISPLAY LIKE 'E'.
    LEAVE LIST-PROCESSING.
  ENDIF.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form UPLOAD_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM UPLOAD_DATA.

  DATA: LV_FILE TYPE STRING,
        LS_DISP TYPE TY_DISP.

  DATA: LS_COLL TYPE TY_COLL.

  CLEAR: GT_EXCEL, GT_DISP, GT_COLL.

  IF P_FILE IS INITIAL.
    MESSAGE S017 WITH TEXT-F02 DISPLAY LIKE 'E'.  "업로드 파일 을(를) 입력하세요.
    LEAVE LIST-PROCESSING.
  ENDIF.

  LV_FILE = P_FILE.

  GRF_GRID->BTN_EXCL_UPLOAD( EXPORTING IV_FILENAME = CONV #( LV_FILE )
                                       IV_BEG_ROW  = 4
                                       IV_BEG_COL  = 2
                             CHANGING  CT_DATA     = GT_EXCEL ).

  CHECK GT_EXCEL IS NOT INITIAL.

  LOOP AT GT_EXCEL INTO DATA(LS_EXCEL).

    MOVE-CORRESPONDING LS_EXCEL TO LS_DISP.

    LS_DISP-ROW_NO = SY-TABIX.

    LS_DISP-STATUS = ICON_YELLOW_LIGHT.
    CLEAR LS_DISP-MESSAGE.

    "자재코드 Conversion
    CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
      EXPORTING
        INPUT  = LS_DISP-MATNR
      IMPORTING
        OUTPUT = LS_DISP-MATNR.

    "업체코드 Conversion
    LS_DISP-LIFNR = |{ LS_DISP-LIFNR ALPHA = IN }|.

    "품목범주
    IF LS_DISP-PSTYP = '3' OR LS_DISP-PSTYP = 'L'.
      LS_DISP-PSTYP = 'L'.
    ELSE.
      CLEAR LS_DISP-PSTYP.
    ENDIF.

    "통화
    IF LS_DISP-WAERS IS INITIAL.
      LS_DISP-WAERS = 'KRW'.
    ENDIF.

    "추정가격 Conversion
    PERFORM CURRENCY_CONV_TO_INTERNAL USING    LS_DISP-WAERS
                                      CHANGING LS_DISP-PREIS.

    "대문자로 변환
    TRANSLATE: LS_DISP-MATNR TO UPPER CASE,
               LS_DISP-WAERS TO UPPER CASE.

    "소문자로 변환
*    TRANSLATE: LS_DISP-REQER TO LOWER CASE,
*               LS_DISP-ORDER TO LOWER CASE.

    "계정 정보 Conversion
    PERFORM CONVERSION_EXIT_ALPHA_INPUT CHANGING: LS_DISP-SAKTO,
                                                  LS_DISP-WBSNO,
                                                  LS_DISP-KOSTL.

    APPEND LS_DISP TO GT_DISP.

* AS-IS 계약요청, 차수, 품목별 Collect
    CLEAR LS_COLL.
    MOVE-CORRESPONDING LS_DISP TO LS_COLL.
    LS_COLL-COUNT = 1.
    COLLECT LS_COLL INTO GT_COLL.

    CLEAR LS_DISP.

  ENDLOOP.

  SORT GT_COLL BY CNTR_NO CNTR_ITEM.

  DESCRIBE TABLE GT_DISP LINES DATA(LV_TCNT).
  MESSAGE S011(ZMM01) WITH LV_TCNT.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CURRENCY_CONV_TO_INTERNAL
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LS_DISP_WAERS
*&      <-- LS_DISP_PREIS
*&---------------------------------------------------------------------*
FORM CURRENCY_CONV_TO_INTERNAL USING IV_WAERS
                               CHANGING CV_PRICE.

  DATA: LV_NETPR TYPE BAPICURR-BAPICURR.

  LV_NETPR = CV_PRICE.

  CALL FUNCTION 'BAPI_CURRENCY_CONV_TO_INTERNAL'
    EXPORTING
      CURRENCY             = IV_WAERS
      AMOUNT_EXTERNAL      = LV_NETPR
      MAX_NUMBER_OF_DIGITS = 15
    IMPORTING
      AMOUNT_INTERNAL      = CV_PRICE.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CONVERSION_EXIT_ALPHA_INPUT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- LS_DISP_SAKTO
*&---------------------------------------------------------------------*
FORM CONVERSION_EXIT_ALPHA_INPUT CHANGING CV_VALUE.

  CHECK CV_VALUE IS NOT INITIAL.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      INPUT  = CV_VALUE
    IMPORTING
      OUTPUT = CV_VALUE.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form PROCESSING_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM PROCESSING_DATA.

*-----------------------------
* 검증 Data 추출
*-----------------------------
  PERFORM GET_CHECK_DATA.

*-----------------------------
* Data 검증
*-----------------------------
  PERFORM CHECK_DATA.

* AS-IS 계약요청 기준 전체 오류 처리
  DATA(LT_DISP) = GT_DISP.
  SORT LT_DISP BY CNTR_NO.
  DELETE LT_DISP WHERE STATUS <> ICON_RED_LIGHT.

  LOOP AT GT_DISP ASSIGNING FIELD-SYMBOL(<LS_DISP>).

    IF <LS_DISP>-BANFN IS INITIAL AND <LS_DISP>-STATUS <> ICON_RED_LIGHT.

      READ TABLE LT_DISP WITH KEY CNTR_NO = <LS_DISP>-CNTR_NO
                                BINARY SEARCH
                                TRANSPORTING NO FIELDS.
      IF SY-SUBRC = 0.
        <LS_DISP>-STATUS  = ICON_RED_LIGHT.
        <LS_DISP>-MESSAGE = TEXT-M19. "같은 AS-IS 계약요청 항목에 오류가 있습니다.
      ENDIF.

    ENDIF.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_CHECK_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM GET_CHECK_DATA.

  CHECK GT_DISP IS NOT INITIAL.

*  CLEAR: GT_USER_REQ, GT_USER_ORD,
  CLEAR: GT_T001W, GT_T001L, GT_ORG,
         GT_MBEW, GT_MARA, GT_MAKT.

* User Info. 요청자
  DATA(LT_FAE) = GT_DISP.
*  SORT LT_FAE BY REQER.
*  DELETE ADJACENT DUPLICATES FROM LT_FAE COMPARING REQER.
*
*  IF LT_FAE IS NOT INITIAL.
*    SELECT USER_ID,
*           EMPLOY_NO,
*           DEPARTMENT,
*           COMPANY
*      INTO CORRESPONDING FIELDS OF TABLE @GT_USER_REQ
*      FROM ZSVMM_USER_INFO
*       FOR ALL ENTRIES IN @LT_FAE
*     WHERE COMPANY   = @P_BUKRS
*       AND EMPLOY_NO = @LT_FAE-REQER.
*
*    SORT GT_USER_REQ BY EMPLOY_NO.
*  ENDIF.
  FREE LT_FAE.

* User Info. 계약담당자
*  LT_FAE = GT_DISP.
*  SORT LT_FAE BY ORDER.
*  DELETE ADJACENT DUPLICATES FROM LT_FAE COMPARING ORDER.
*
*  IF LT_FAE IS NOT INITIAL.
*    SELECT USER_ID,
*           EMPLOY_NO,
*           DEPARTMENT,
*           COMPANY
*      INTO CORRESPONDING FIELDS OF TABLE @GT_USER_ORD
*      FROM ZSVMM_USER_INFO
*       FOR ALL ENTRIES IN @LT_FAE
*     WHERE COMPANY   = @P_BUKRS
*       AND EMPLOY_NO = @LT_FAE-ORDER.
*
*    SORT GT_USER_ORD BY EMPLOY_NO.
*  ENDIF.
*  FREE LT_FAE.

* 플랜트 체크
  LT_FAE = GT_DISP.
  SORT LT_FAE BY WERKS.
  DELETE ADJACENT DUPLICATES FROM LT_FAE COMPARING WERKS.

  IF LT_FAE IS NOT INITIAL.
    SELECT WERKS
      INTO CORRESPONDING FIELDS OF TABLE @GT_T001W
      FROM T001W
       FOR ALL ENTRIES IN @LT_FAE
     WHERE WERKS = @LT_FAE-WERKS.

    SORT GT_T001W BY WERKS.
  ENDIF.
  FREE LT_FAE.

* 저장위치 체크
  LT_FAE = GT_DISP.
  SORT LT_FAE BY WERKS LGORT.
  DELETE ADJACENT DUPLICATES FROM LT_FAE COMPARING WERKS LGORT.

  IF LT_FAE IS NOT INITIAL.
    SELECT WERKS,
           LGORT
      INTO CORRESPONDING FIELDS OF TABLE @GT_T001L
      FROM T001L
       FOR ALL ENTRIES IN @LT_FAE
     WHERE WERKS = @LT_FAE-WERKS
       AND LGORT = @LT_FAE-LGORT.

    SORT GT_T001L BY WERKS LGORT.
  ENDIF.
  FREE LT_FAE.

* 구매그룹 체크
  SELECT EKGRP,
         EKORG,
         BUKRS
    INTO CORRESPONDING FIELDS OF TABLE @GT_ORG
    FROM ZSVCMM_ORG
   WHERE BUKRS = @P_BUKRS.

  SORT GT_ORG BY EKGRP EKORG.

* 자재 체크
  LT_FAE = GT_DISP.
  SORT LT_FAE BY MATNR.
  DELETE ADJACENT DUPLICATES FROM LT_FAE COMPARING MATNR.

  IF LT_FAE IS NOT INITIAL.
    SELECT MATNR,
           MEINS
      FROM MARA
       FOR ALL ENTRIES IN @LT_FAE
     WHERE MATNR = @LT_FAE-MATNR
       AND LVORM IS INITIAL
      INTO CORRESPONDING FIELDS OF TABLE @GT_MARA.

    SORT GT_MARA BY MATNR.

    "자재내역
    SELECT MATNR,
           MAKTX
      INTO CORRESPONDING FIELDS OF TABLE @GT_MAKT
      FROM MAKT
       FOR ALL ENTRIES IN @LT_FAE
     WHERE MATNR = @LT_FAE-MATNR
       AND SPRAS = @SY-LANGU.

    SORT GT_MAKT BY MATNR.
  ENDIF.
  FREE LT_FAE.

* 평가유형 체크
  LT_FAE = GT_DISP.
  SORT LT_FAE BY MATNR WERKS.
  DELETE ADJACENT DUPLICATES FROM LT_FAE COMPARING MATNR WERKS.

  IF LT_FAE IS NOT INITIAL.
    SELECT MATNR,
           BWKEY,
           BWTAR
      FROM MBEW
       FOR ALL ENTRIES IN @LT_FAE
     WHERE MATNR = @LT_FAE-MATNR
       AND BWKEY = @LT_FAE-WERKS
       AND BWTAR IS NOT INITIAL
       AND LVORM IS INITIAL
      INTO CORRESPONDING FIELDS OF TABLE @GT_MBEW.

    SORT GT_MBEW BY MATNR BWKEY BWTAR.
  ENDIF.
  FREE LT_FAE.

* G/L계정 체크
  LT_FAE = GT_DISP.
  SORT LT_FAE BY SAKTO.
  DELETE ADJACENT DUPLICATES FROM LT_FAE COMPARING SAKTO.

  CONSTANTS: LC_KTOPL_1000 TYPE SKA1-KTOPL VALUE '1000'.

  IF LT_FAE IS NOT INITIAL.
    SELECT SAKNR
      FROM SKA1
       FOR ALL ENTRIES IN @LT_FAE
     WHERE KTOPL = @LC_KTOPL_1000
       AND SAKNR = @LT_FAE-SAKTO
      INTO CORRESPONDING FIELDS OF TABLE @GT_SKA1.

    SORT GT_SKA1 BY SAKNR.
  ENDIF.
  FREE LT_FAE.

* WBS 체크
  LT_FAE = GT_DISP.
  SORT LT_FAE BY WBSNO.
  DELETE ADJACENT DUPLICATES FROM LT_FAE COMPARING WBSNO.

  IF LT_FAE IS NOT INITIAL.
    SELECT PSPNR
      FROM PRPS
       FOR ALL ENTRIES IN @LT_FAE
     WHERE PSPNR = @LT_FAE-WBSNO
      INTO CORRESPONDING FIELDS OF TABLE @GT_PRPS.

    SORT GT_PRPS BY PSPNR.
  ENDIF.
  FREE LT_FAE.

* 코스트센터 체크
  LT_FAE = GT_DISP.
  SORT LT_FAE BY KOSTL.
  DELETE ADJACENT DUPLICATES FROM LT_FAE COMPARING KOSTL.

  IF LT_FAE IS NOT INITIAL.
    SELECT KOSTL
      FROM CSKS
       FOR ALL ENTRIES IN @LT_FAE
     WHERE KOKRS  = @P_BUKRS
       AND KOSTL  = @LT_FAE-KOSTL
*U1> 유효일자 조건에 User-Id 참조 TimeZone 적용 - START
*       AND DATBI >= @SY-DATUM
       AND DATBI >= @SY-DATLO
*U1> 유효일자 조건에 User-Id 참조 TimeZone 적용 - END
      INTO CORRESPONDING FIELDS OF TABLE @GT_CSKS.

    SORT GT_CSKS BY KOSTL.
  ENDIF.
  FREE LT_FAE.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CHECK_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM CHECK_DATA.

  LOOP AT GT_DISP ASSIGNING FIELD-SYMBOL(<LS_DISP>).

    PERFORM CHECK_DATA_01 CHANGING <LS_DISP>.

    PERFORM CHECK_DATA_02 CHANGING <LS_DISP>.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CHECK_DATA_01
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- <LS_DISP>
*&---------------------------------------------------------------------*
FORM CHECK_DATA_01 CHANGING CS_DISP TYPE TY_DISP.

  DEFINE _L_SET_MESSAGE.

    CS_DISP-STATUS  = ICON_RED_LIGHT.
    CS_DISP-MESSAGE = &1.

  END-OF-DEFINITION.

* 계약번호, 품목 기준으로 중복라인 체크
  READ TABLE GT_COLL WITH KEY CNTR_NO   = CS_DISP-CNTR_NO
                              CNTR_ITEM = CS_DISP-CNTR_ITEM
                              BINARY SEARCH
                              INTO DATA(LS_COLL).
  IF LS_COLL-COUNT > 1.
    _L_SET_MESSAGE TEXT-M15.  "AS-IS 계약요청, 품목 기준으로 중복라인이 있습니다.
    RETURN.
  ENDIF.

* 구매요청유형 체크
  IF CS_DISP-BSART = GC_BSART_C10 OR
     CS_DISP-BSART = GC_BSART_M10 OR
     CS_DISP-BSART = GC_BSART_M20.

    CASE CS_DISP-BSART.
      WHEN GC_BSART_C10 OR GC_BSART_M10.  "계정지정 관련 사항을 점검하세요.

        IF CS_DISP-SAKTO IS INITIAL.
          _L_SET_MESSAGE TEXT-M03.  "G/L 계정은 필수입니다.
          RETURN.
        ENDIF.

        IF CS_DISP-KOSTL IS INITIAL AND CS_DISP-WBSNO IS INITIAL.
          _L_SET_MESSAGE TEXT-M04.  "코스트센터와 WBS중 하나는 필수입니다.
          RETURN.
        ENDIF.

        IF CS_DISP-KOSTL IS NOT INITIAL AND CS_DISP-WBSNO IS NOT INITIAL.
          _L_SET_MESSAGE TEXT-M05.  "코스트센터와 WBS중 하나만 입력하세요.
          RETURN.
        ENDIF.

        "계정지정유형
        IF CS_DISP-KOSTL IS NOT INITIAL.
          CS_DISP-KNTTP = 'K'.
        ENDIF.

        IF CS_DISP-WBSNO IS NOT INITIAL.
          CS_DISP-KNTTP = 'P'.
        ENDIF.

      WHEN OTHERS.
        IF CS_DISP-SAKTO IS NOT INITIAL OR
           CS_DISP-KOSTL IS NOT INITIAL OR
           CS_DISP-WBSNO IS NOT INITIAL.
          _L_SET_MESSAGE TEXT-M06.  "계정지정 정보를 입력 할 수 없는 구매요청유형입니다.
          RETURN.
        ENDIF.
    ENDCASE.

  ELSE.
    _L_SET_MESSAGE TEXT-M02.  "구매요청유형을 점검하세요.(C10, M10, M20만 가능)
    RETURN.
  ENDIF.

* 요청자 체크
*  READ TABLE GT_USER_REQ WITH KEY EMPLOY_NO = CS_DISP-REQER
*                                  BINARY SEARCH
*                                  INTO DATA(LS_USER_REQ).
*  IF SY-SUBRC = 0.
*    CS_DISP-DEPTQ = LS_USER_REQ-DEPARTMENT.
*  ELSE.
*    _L_SET_MESSAGE TEXT-M07.  "요청자를 확인하세요.
*    RETURN.
*  ENDIF.

* 계약담당자 체크
*  READ TABLE GT_USER_ORD WITH KEY EMPLOY_NO = CS_DISP-ORDER
*                                  BINARY SEARCH
*                                  INTO DATA(LS_USER_ORD).
*  IF SY-SUBRC = 0.
*    CS_DISP-DEPTO = LS_USER_ORD-DEPARTMENT.
*  ELSE.
*    _L_SET_MESSAGE TEXT-M08.  "계약담당자를 확인하세요.
*    RETURN.
*  ENDIF.

* 품목범주 체크
  IF CS_DISP-PSTYP IS NOT INITIAL AND CS_DISP-PSTYP <> 'L'.
    _L_SET_MESSAGE TEXT-M09.  "품목범주를 점검하세요.
    RETURN.
  ENDIF.

* 플랜트 체크
  READ TABLE GT_T001W WITH KEY WERKS = CS_DISP-WERKS
                               BINARY SEARCH
                               TRANSPORTING NO FIELDS.
  IF SY-SUBRC <> 0.
    _L_SET_MESSAGE TEXT-M10.  "플랜트를 점검하세요.
    RETURN.
  ENDIF.

* 저장위치 체크
  IF CS_DISP-LGORT IS NOT INITIAL.
    READ TABLE GT_T001L WITH KEY WERKS = CS_DISP-WERKS
                                 LGORT = CS_DISP-LGORT
                                 BINARY SEARCH
                                 TRANSPORTING NO FIELDS.
    IF SY-SUBRC <> 0.
      _L_SET_MESSAGE TEXT-M11.  "저장위치를 점검하세요.
      RETURN.
    ENDIF.
  ENDIF.

* 구매그룹 체크
  READ TABLE GT_ORG WITH KEY EKGRP = CS_DISP-EKGRP
                             BINARY SEARCH
                             TRANSPORTING NO FIELDS.
  IF SY-SUBRC <> 0.
    _L_SET_MESSAGE TEXT-M12.  "구매그룹을 점검하세요.
    RETURN.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CHECK_DATA_02
*&---------------------------------------------------------------------*
FORM CHECK_DATA_02 CHANGING CS_DISP TYPE TY_DISP.

  DEFINE _L_SET_MESSAGE.

    CS_DISP-STATUS  = ICON_RED_LIGHT.
    CS_DISP-MESSAGE = &1.

  END-OF-DEFINITION.

  CHECK CS_DISP-STATUS <> ICON_RED_LIGHT.

* 자재 체크
  READ TABLE GT_MARA WITH KEY MATNR = CS_DISP-MATNR
                              BINARY SEARCH
                              INTO DATA(LS_MARA).
  IF SY-SUBRC = 0.
    CS_DISP-MEINS = LS_MARA-MEINS.
  ELSE.
    _L_SET_MESSAGE TEXT-M13.  "자재코드를 점검하세요.
    RETURN.
  ENDIF.

* 자재내역
  IF CS_DISP-TXZ01 IS INITIAL.
    READ TABLE GT_MAKT WITH KEY MATNR = CS_DISP-MATNR
                                BINARY SEARCH
                                INTO DATA(LS_MAKT).
    IF SY-SUBRC = 0.
      CS_DISP-TXZ01 = LS_MAKT-MAKTX.
    ELSE.
      _L_SET_MESSAGE TEXT-M13.  "자재코드를 점검하세요.
      RETURN.
    ENDIF.
  ENDIF.

* 평가유형 체크
  IF CS_DISP-BWTAR IS INITIAL.
    READ TABLE GT_MBEW WITH KEY MATNR = CS_DISP-MATNR
                                BWKEY = CS_DISP-WERKS
                                BINARY SEARCH
                                TRANSPORTING NO FIELDS.
    IF SY-SUBRC = 0.
      _L_SET_MESSAGE TEXT-M14.  "평가유형을 확인하세요.
      RETURN.
    ENDIF.

  ELSE.
    READ TABLE GT_MBEW WITH KEY MATNR = CS_DISP-MATNR
                                BWKEY = CS_DISP-WERKS
                                BWTAR = CS_DISP-BWTAR
                                BINARY SEARCH
                                TRANSPORTING NO FIELDS.
    IF SY-SUBRC <> 0.
      _L_SET_MESSAGE TEXT-M14.  "평가유형을 확인하세요.
      RETURN.
    ENDIF.
  ENDIF.

* G/L계정 체크
  IF CS_DISP-SAKTO IS NOT INITIAL.
    READ TABLE GT_SKA1 WITH KEY SAKNR = CS_DISP-SAKTO
                                BINARY SEARCH
                                TRANSPORTING NO FIELDS.
    IF SY-SUBRC <> 0.
      _L_SET_MESSAGE TEXT-M16.  "계정지정 관련 사항을 점검하세요.(G/L계정)
      RETURN.
    ENDIF.
  ENDIF.

* WBS 체크
  IF CS_DISP-WBSNO IS NOT INITIAL.
    READ TABLE GT_PRPS WITH KEY PSPNR = CS_DISP-WBSNO
                                BINARY SEARCH
                                TRANSPORTING NO FIELDS.
    IF SY-SUBRC <> 0.
      _L_SET_MESSAGE TEXT-M17.  "계정지정 관련 사항을 점검하세요.(WBS)
      RETURN.
    ENDIF.
  ENDIF.

* 코스트센터 체크
  IF CS_DISP-KOSTL IS NOT INITIAL.
    READ TABLE GT_CSKS WITH KEY KOSTL = CS_DISP-KOSTL
                                BINARY SEARCH
                                TRANSPORTING NO FIELDS.
    IF SY-SUBRC <> 0.
      _L_SET_MESSAGE TEXT-M18.  "계정지정 관련 사항을 점검하세요.(코스트센터)
      RETURN.
    ENDIF.
  ENDIF.

* 필수 입력 체크
  IF CS_DISP-CNTR_NO   IS INITIAL OR
     CS_DISP-CNTR_ITEM IS INITIAL OR
     CS_DISP-BSART     IS INITIAL OR
*     CS_DISP-REQER     IS INITIAL OR
     CS_DISP-MATNR     IS INITIAL OR
     CS_DISP-MENGE     IS INITIAL OR
     CS_DISP-LFDAT     IS INITIAL OR
     CS_DISP-WERKS     IS INITIAL OR
*     CS_DISP-ORDER     IS INITIAL OR
     CS_DISP-EKGRP     IS INITIAL.
    _L_SET_MESSAGE TEXT-M01.  "필수 입력값이 누락되었습니다.
    RETURN.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CREATE_PR
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM CREATE_PR.

  DATA: LS_HEADER   TYPE ZSMM_PRHEADER,
        LS_HEADERX  TYPE ZSMM_PRHEADERX,
        LV_BANFN    TYPE EBAN-BANFN,
        LT_ITEM     TYPE TABLE OF ZSMM_PRITEM,
        LT_ITEMX    TYPE TABLE OF ZSMM_PRITEMX,
        LT_ACCOUNT  TYPE TABLE OF ZSMM_PRACCOUNT,
        LT_ACCOUNTX TYPE TABLE OF ZSMM_PRACCOUNTX,
        LT_RETURN   TYPE TABLE OF BAPIRET2.

  DATA: LV_BNFPO TYPE EBAN-BNFPO.

  DATA(LT_COLL) = GT_COLL.

*-----------------------------
* Validation Check
*-----------------------------
* Confirm
  CHECK GRF_GRID->POP_TO_MSG( IV_TYPE  = 'A'
                              IV_TITLE = ZCL_CN_ALV_GRID=>AC_MSG_TITLE    "저장확인
                              IV_TEXT1 = CONV #( TEXT-M21 ) "구매요청을 생성하시겠습니까?
                              IV_TEXT2 = SPACE ) EQ ABAP_TRUE. "YES

*-----------------------------
* AS-IS 계약번호별 PR 생성
*-----------------------------
  SORT: LT_COLL BY CNTR_NO,
        GT_DISP BY CNTR_NO CNTR_ITEM.

  DELETE ADJACENT DUPLICATES FROM LT_COLL COMPARING CNTR_NO.

  LOOP AT LT_COLL INTO DATA(LS_COLL).
    CLEAR: LS_HEADER, LS_HEADERX, LV_BANFN, LV_BNFPO,
           LT_ITEM, LT_ITEMX, LT_ACCOUNT, LT_ACCOUNTX, LT_RETURN.


    READ TABLE GT_DISP WITH KEY CNTR_NO = LS_COLL-CNTR_NO
                                BINARY SEARCH
                                TRANSPORTING NO FIELDS.

    IF SY-SUBRC = 0.
      LOOP AT GT_DISP FROM SY-TABIX INTO DATA(LS_DISP).

        IF LS_DISP-STATUS   = ICON_RED_LIGHT OR
           LS_DISP-CNTR_NO <> LS_COLL-CNTR_NO.
          EXIT.
        ENDIF.

* Header Data
        IF LS_HEADER IS INITIAL.
          PERFORM MAKE_HEADER_DATA USING    LS_DISP
                                   CHANGING LS_HEADER LS_HEADERX.
        ENDIF.

* Item Data
        LV_BNFPO = LV_BNFPO + 10.
        LS_DISP-BNFPO = LV_BNFPO.

        PERFORM MAKE_ITEM_DATA TABLES LT_ITEM LT_ITEMX
                               USING  LS_DISP.

* Modify BNFPO
        MODIFY GT_DISP FROM LS_DISP TRANSPORTING BNFPO.

* Account Assignment
        PERFORM MAKE_ACCOUNT_DATA TABLES LT_ACCOUNT LT_ACCOUNTX
                                  USING  LS_DISP.

      ENDLOOP.
    ENDIF.

*-----------------------------
* PR 생성
*-----------------------------
    IF LS_HEADER IS INITIAL.
      CONTINUE.
    ENDIF.

    call function 'ZFIMM_PR_CREATE'
      EXPORTING
        IS_PRHEADER   = LS_HEADER
        IS_PRHEADERX  = LS_HEADERX
      IMPORTING
        EV_BANFN      = LV_BANFN
      TABLES
        IT_PRITEM     = LT_ITEM
        IT_PRITEMX    = LT_ITEMX
        IT_PRACCOUNT  = LT_ACCOUNT
        IT_PRACCOUNTX = LT_ACCOUNTX
        ET_RETURN     = LT_RETURN.

*-----------------------------
* Return
*-----------------------------
    DELETE LT_RETURN WHERE TYPE = 'E'
                       AND ID   = 'BAPI' AND NUMBER = '001'.  "오브젝트유형 PurchaseRequisition 인스턴스를 생성할 수 없습니다.

* Modify Return Message or BANFN
    READ TABLE LT_RETURN WITH KEY TYPE = 'E' INTO DATA(LS_RETURN).

    IF SY-SUBRC = 0.
      LS_DISP-STATUS  = ICON_RED_LIGHT.
      LS_DISP-MESSAGE = LS_RETURN-MESSAGE.
      CLEAR: LS_DISP-BANFN, LS_DISP-BNFPO.

      MODIFY GT_DISP FROM LS_DISP TRANSPORTING STATUS BANFN BNFPO MESSAGE
       WHERE CNTR_NO  = LS_COLL-CNTR_NO.

    ELSE.
      LS_DISP-STATUS = ICON_GREEN_LIGHT.
      LS_DISP-BANFN  = LV_BANFN.
      CLEAR: LS_DISP-MESSAGE.

      MODIFY GT_DISP FROM LS_DISP TRANSPORTING STATUS BANFN MESSAGE
       WHERE CNTR_NO  = LS_COLL-CNTR_NO.

    ENDIF.

  ENDLOOP.

*-----------------------------
* Refresh
*-----------------------------
  SORT GT_DISP BY ROW_NO.

  GRF_GRID->REFRESH_GRID_DISPLAY( ).

ENDFORM.
*&---------------------------------------------------------------------*
*& Form MAKE_HEADER_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LS_DISP
*&      <-- LS_HEADER
*&      <-- LS_HEADERX
*&---------------------------------------------------------------------*
FORM MAKE_HEADER_DATA USING IS_DISP TYPE TY_DISP
                      CHANGING CS_HEADER  TYPE ZSMM_PRHEADER
                               CS_HEADERX TYPE ZSMM_PRHEADERX.

  DATA : LV_OBJNAME TYPE DDOBJNAME VALUE 'ZSMM_PRHEADER',
         LT_DFIES   TYPE TABLE OF DFIES.

  DATA: LV_TITLE TYPE ZTMM30010-ZPRTITLE.

* HEADER
  CONCATENATE IS_DISP-CNTR_NO IS_DISP-TITLE INTO LV_TITLE SEPARATED BY SPACE.

  CS_HEADER = VALUE #( BSART           = IS_DISP-BSART
                       ZPRTITLE        = LV_TITLE
*                       ZREQUESTER      = IS_DISP-REQER
*                       ZPEQ_DEPARTMENT = IS_DISP-DEPTQ
                        ).

* HEADERX
  CALL FUNCTION 'DDIF_FIELDINFO_GET'
    EXPORTING
      TABNAME   = LV_OBJNAME
    TABLES
      DFIES_TAB = LT_DFIES.

  LOOP AT LT_DFIES INTO DATA(LS_DFIES).

    DATA(LV_FIELD) = 'CS_HEADER-' && LS_DFIES-FIELDNAME.
    ASSIGN (LV_FIELD) TO FIELD-SYMBOL(<LRF_FIELD>).

    IF <LRF_FIELD> IS NOT INITIAL.
      DATA(LV_FIELDX) =  'CS_HEADERX-' && LS_DFIES-FIELDNAME.
      ASSIGN (LV_FIELDX) TO FIELD-SYMBOL(<LRF_FIELDX>).

      <LRF_FIELDX> = ABAP_TRUE.
    ENDIF.

  ENDLOOP.

  CLEAR: LV_FIELD, LV_FIELDX.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form MAKE_ITEM_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LT_ITEM
*&      --> LT_ITEMX
*&      --> LS_DISP
*&---------------------------------------------------------------------*
FORM MAKE_ITEM_DATA TABLES CT_ITEM STRUCTURE ZSMM_PRITEM
                           CT_ITEMX STRUCTURE ZSMM_PRITEMX
                    USING  IS_DISP  TYPE TY_DISP.

  DATA: LV_PREIS TYPE BAPICURR-BAPICURR.

  DATA: LS_ITEM  TYPE ZSMM_PRITEM,
        LS_ITEMX TYPE ZSMM_PRITEMX.

  DATA: LV_OBJNAME TYPE DDOBJNAME VALUE 'ZSMM_PRITEM',
        LT_DFIES   TYPE TABLE OF DFIES.

  IF IS_DISP-PREIS IS NOT INITIAL.
    CALL FUNCTION 'BAPI_CURRENCY_CONV_TO_EXTERNAL'
      EXPORTING
        CURRENCY        = IS_DISP-WAERS
        AMOUNT_INTERNAL = IS_DISP-PREIS
      IMPORTING
        AMOUNT_EXTERNAL = LV_PREIS.
  ENDIF.

* ITEM
  LS_ITEM = VALUE #( BNFPO             = IS_DISP-BNFPO
                     EKGRP             = IS_DISP-EKGRP
                     TXZ01             = IS_DISP-TXZ01
                     MATNR             = IS_DISP-MATNR
                     WERKS             = IS_DISP-WERKS
                     LGORT             = IS_DISP-LGORT
                     BAMNG             = IS_DISP-MENGE
                     BAMEI             = IS_DISP-MEINS
                     EINDT             = IS_DISP-LFDAT
                     PREIS             = LV_PREIS
                     EPEIN             = 1
                     PSTYP             = IS_DISP-PSTYP
                     KNTTP             = IS_DISP-KNTTP
                     WLIEF             = IS_DISP-LIFNR
*                     FLIEF             = IS_DISP-LIFNR
                     EKORG             = P_EKORG
                     BWTAR             = IS_DISP-BWTAR
                     WAERS             = IS_DISP-WAERS
*                     ZORDER_PERSON     = IS_DISP-ORDER
*                     ZORDER_DEPARTMENT = IS_DISP-DEPTO
                     ).

* ITEMX
  CALL FUNCTION 'DDIF_FIELDINFO_GET'
    EXPORTING
      TABNAME   = LV_OBJNAME
    TABLES
      DFIES_TAB = LT_DFIES.

  LS_ITEMX-BNFPO = LS_ITEM-BNFPO.

  LOOP AT LT_DFIES INTO DATA(LS_DFIES).

    CHECK SY-TABIX > 1.   "품목(BNFPO)은 제외

    DATA(LV_FIELD) = 'LS_ITEM-' && LS_DFIES-FIELDNAME.
    ASSIGN (LV_FIELD) TO FIELD-SYMBOL(<LRF_FIELD>).

    IF <LRF_FIELD> IS NOT INITIAL.
      DATA(LV_FIELDX) =  'LS_ITEMX-' && LS_DFIES-FIELDNAME.
      ASSIGN (LV_FIELDX) TO FIELD-SYMBOL(<LRF_FIELDX>).

      <LRF_FIELDX> = ABAP_TRUE.
    ENDIF.

  ENDLOOP.

  APPEND: LS_ITEM  TO CT_ITEM,
          LS_ITEMX TO CT_ITEMX.

  CLEAR: LV_FIELD, LV_FIELDX.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form MAKE_ACCOUNT_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LT_ACCOUNT
*&      --> LT_ACCOUNTX
*&      --> LS_DISP
*&---------------------------------------------------------------------*
FORM MAKE_ACCOUNT_DATA TABLES CT_ACCOUNT STRUCTURE ZSMM_PRACCOUNT
                              CT_ACCOUNTX STRUCTURE ZSMM_PRACCOUNTX
                       USING  IS_DISP     TYPE TY_DISP.

  CONSTANTS: LC_INT_DATE(8) VALUE '00000000'.

  DATA: LS_ACCOUNT  TYPE ZSMM_PRACCOUNT,
        LS_ACCOUNTX TYPE ZSMM_PRACCOUNTX.

  DATA : LV_OBJNAME TYPE DDOBJNAME VALUE 'ZSMM_PRACCOUNT',
         LT_DFIES   TYPE TABLE OF DFIES.

* ACCOUNT
  LS_ACCOUNT = VALUE #( BNFPO = IS_DISP-BNFPO
                        ZEBKN = '01'
                        MENGE = IS_DISP-MENGE
                        SAKNR = IS_DISP-SAKTO
                        KOSTL = IS_DISP-KOSTL ).

  IF IS_DISP-WBSNO = LC_INT_DATE OR IS_DISP-WBSNO IS INITIAL.
    LS_ACCOUNT-PS_POSID = SPACE.
  ELSE.
    CALL FUNCTION 'CONVERSION_EXIT_ABPSP_OUTPUT'
      EXPORTING
        INPUT  = IS_DISP-WBSNO
      IMPORTING
        OUTPUT = LS_ACCOUNT-PS_POSID.
  ENDIF.

* ACCOUNTX
  CALL FUNCTION 'DDIF_FIELDINFO_GET'
    EXPORTING
      TABNAME   = LV_OBJNAME
    TABLES
      DFIES_TAB = LT_DFIES.

  LS_ACCOUNTX-BNFPO = LS_ACCOUNT-BNFPO.
  LS_ACCOUNTX-ZEBKN = LS_ACCOUNT-ZEBKN.

  LOOP AT LT_DFIES INTO DATA(LS_DFIES).

    CHECK SY-TABIX > 2.   "품목(BNFPO)은 제외

    DATA(LV_FIELD) = 'LS_ACCOUNT-' && LS_DFIES-FIELDNAME.
    ASSIGN (LV_FIELD) TO FIELD-SYMBOL(<LRF_FIELD>).

    IF <LRF_FIELD> IS NOT INITIAL.
      DATA(LV_FIELDX) =  'LS_ACCOUNTX-' && LS_DFIES-FIELDNAME.
      ASSIGN (LV_FIELDX) TO FIELD-SYMBOL(<LRF_FIELDX>).

      <LRF_FIELDX> = ABAP_TRUE.
    ENDIF.

  ENDLOOP.

  APPEND: LS_ACCOUNT  TO CT_ACCOUNT,
          LS_ACCOUNTX TO CT_ACCOUNTX.

  CLEAR: LV_FIELD, LV_FIELDX.

ENDFORM.
