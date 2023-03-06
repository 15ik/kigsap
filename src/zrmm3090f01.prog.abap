*&---------------------------------------------------------------------*
*& Include          ZRMM3090F01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form INITIALIZATION
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form initialization
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM INITIALIZATION .

*------------------------------
* Set Variant
*------------------------------
  GS_VARIANT-REPORT   = SY-CPROG.
  GS_VARIANT-USERNAME = SY-UNAME.

  ZCL_MM_COMMON=>COMMON_CONFIG(
     EXPORTING  IS_COMMON =  VALUE #( M = 'A1' D = 'A1010' S = 'AB100' )
                                     IT_WHERE = VALUE #(
                                                         ( FIELD = 1 VALUE = SY-REPID )
                                                         ( FIELD = 2 VALUE = 'EX01' )
                                                         ( FIELD = 3 VALUE = SY-UNAME ) )
     IMPORTING ET_OUTTAB = DATA(LT_CONFIG) ).

  READ TABLE LT_CONFIG INTO DATA(LS_CONFIG) INDEX 1.

  IF SY-SUBRC EQ 0.
    GV_EXC_USER = 'X'.
  ENDIF.


  IF GV_EXC_USER IS INITIAL.
*> 회사 / 접수자 / 발주부서 기본값 설정.
*    SELECT SINGLE EMPLOY_NO, EMPLOY_NAME, DEPARTMENT, DEPART_NAME, COMPANY, COMPANY_NAME
*      FROM ZSVMM_USER_INFO
*     WHERE USER_ID = @SY-UNAME
*      INTO @DATA(LS_USER_INIT).
*    IF SY-SUBRC EQ 0.
*      P_BUKRS = LS_USER_INIT-COMPANY.
*    ENDIF.
  ENDIF.

*> 납품일 기본값 설정. (2개월)
  DATA: LV_START_DATE TYPE SY-DATLO.

  "전월 계산.
  CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
    EXPORTING
      DATE      = SY-DATLO
      SIGNUM    = '-'
      DAYS      = 0
      MONTHS    = 2
      YEARS     = 0
    IMPORTING
      CALC_DATE = LV_START_DATE.

  APPEND VALUE #( SIGN = 'I' OPTION = 'BT' LOW = LV_START_DATE HIGH = SY-DATLO )
      TO S_EINDT.

  P_KALSK = GC_KALSK_DO.  "국내 기본값 설정.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form get_data
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM GET_DATA .

  DATA: LR_KALSK TYPE RANGE OF LFM1-KALSK.

* 내외자구분 검색조건 설정
  IF P_KALSK IS INITIAL.
    CLEAR LR_KALSK.
  ELSE.
    LR_KALSK = VALUE #( ( SIGN = 'I' OPTION = 'EQ' LOW = P_KALSK ) ).
  ENDIF.

  SELECT
        LIFNR,     "공급업체
        NAME1,     "업체명
        EBELN,     "구매오더
        EBELP,     "품목
        MATNR,     "자재
        TXZ01,     "내역
        BWTAR,     "평가유형
        MEINS,     "단위
        MENGE,     "PO수량
        ETENR,     "납품일정
        EINDT,     "납품일
        EKET_QTY,     "납품예정수량
        ID_QTY,     "납품서수량
        GR_QTY,     "입고수량
        REMAIN_ID,     "납품미입고
        REMAIN_GR,     "미입고잔량
        WERKS,     "플랜트
        LGORT,     "창고
        BSART,     "구매문서유형
        BATXT,     "문서유형 내역
        RETPO,     "반품품목
        BSTAE,     "확인 관리 키
        ZORDER_PERSON,     "계약담당자
*        USER_NM,     "계약담당자명
         ZORDER_DEPARTMENT,     "계약부서
*        ORGN_NM,     "계약부서명
        ZEXPEN_PERSON,     "지출발의담당자
*        USER_NM,     "자출발의담당자명
        ZEXPEN_DEPARTMENT,     "지출발의부서
*        ORGN_NM     "지출발의부서명
        AEDAT_PO    "(+)U2
    FROM ZSVCMM_EKET1
   WHERE BUKRS = @P_BUKRS
     AND EINDT IN @S_EINDT
     AND LIFNR IN @S_LIFNR
     AND BSART IN @S_BSART
     AND EKGRP IN @S_EKGRP
     AND WERKS IN @S_WERKS
     AND EBELN IN @S_EBELN
     AND ZORDER_PERSON IN @S_ORPSN
     AND ZORDER_DEPARTMENT IN @S_ORDPT
     AND ZEXPEN_PERSON IN @S_EXPSN
     AND ZEXPEN_DEPARTMENT IN @S_EXDPT
     AND AEDAT IN @S_AEDAT
     AND MATNR IN @S_MATNR
     AND MATKL IN @S_MATKL
     AND PSTYP IN @S_PSTYP
     AND KNTTP IN @S_KNTTP
     AND BSTAE IN @S_BSTAE
     AND KALSK IN @LR_KALSK
    INTO CORRESPONDING FIELDS OF TABLE @GT_DISP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form PROCESSING_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form processing_data
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM PROCESSING_DATA .

*  DATA: BEGIN OF LS_TMP_USER,
*          EMP_NO TYPE ZTCN00002-EMP_NO,
*        END OF LS_TMP_USER,
*        LT_TMP_USER LIKE TABLE OF LS_TMP_USER.
*
*  DATA: BEGIN OF LS_TMP_DEPT,
*          ORGN_CD TYPE ZTCN00001-ORGN_CD,
*        END OF LS_TMP_DEPT,
*        LT_TMP_DEPT LIKE TABLE OF LS_TMP_DEPT.
*
*  LOOP AT GT_DISP INTO DATA(LS_DISP).
*    IF NOT LS_DISP-ZORDER_PERSON IS INITIAL.
*      LS_TMP_USER-EMP_NO = LS_DISP-ZORDER_PERSON.
*      COLLECT LS_TMP_USER INTO LT_TMP_USER.
*    ENDIF.
*
*    IF NOT LS_DISP-ZORDER_DEPARTMENT IS INITIAL.
*      LS_TMP_DEPT-ORGN_CD = LS_DISP-ZORDER_DEPARTMENT.
*      COLLECT LS_TMP_DEPT INTO LT_TMP_DEPT.
*    ENDIF.
*
*    IF NOT LS_DISP-ZEXPEN_PERSON IS INITIAL.
*      LS_TMP_USER-EMP_NO = LS_DISP-ZEXPEN_PERSON.
*      COLLECT LS_TMP_USER INTO LT_TMP_USER.
*    ENDIF.
*
*    IF NOT LS_DISP-ZEXPEN_DEPARTMENT IS INITIAL.
*      LS_TMP_DEPT-ORGN_CD = LS_DISP-ZEXPEN_DEPARTMENT.
*      COLLECT LS_TMP_DEPT INTO LT_TMP_DEPT.
*    ENDIF.
*  ENDLOOP.

**> GET USER 정보
*  SORT LT_TMP_USER BY EMP_NO.
*  DELETE ADJACENT DUPLICATES FROM LT_TMP_USER COMPARING EMP_NO.
*  IF NOT LT_TMP_USER[] IS INITIAL.
*    SELECT EMP_NO, USER_NM
*      FROM ZTCN00002
*       FOR ALL ENTRIES IN @LT_TMP_USER
*     WHERE EMP_NO = @LT_TMP_USER-EMP_NO
*       AND BUKRS = @P_BUKRS
*      INTO TABLE @DATA(LT_ZTCN00002).
*    FREE LT_TMP_USER.
*    SORT LT_ZTCN00002 BY EMP_NO.
*  ENDIF.

**> GET 부서 정보
*  SORT LT_TMP_DEPT BY ORGN_CD.
*  DELETE ADJACENT DUPLICATES FROM LT_TMP_DEPT COMPARING ORGN_CD.
*  IF NOT LT_TMP_DEPT[] IS INITIAL.
*    SELECT ORGN_CD, ORGN_NM
*      FROM ZTCN00001
*       FOR ALL ENTRIES IN @LT_TMP_DEPT
*     WHERE ORGN_CD = @LT_TMP_DEPT-ORGN_CD
*       AND BUKRS = @P_BUKRS
*      INTO TABLE @DATA(LT_ZTCN00001).
*    FREE LT_TMP_DEPT.
*    SORT LT_ZTCN00001 BY ORGN_CD.
*  ENDIF.

*> 납품일정라인 표시
  CLEAR GT_ALL_0200.
  DATA(LT_TMP) = GT_DISP[].
  SORT LT_TMP BY EBELN EBELP.
  DELETE ADJACENT DUPLICATES FROM LT_TMP COMPARING EBELN EBELP.
  IF NOT LT_TMP[] IS INITIAL.
    SELECT EBELN, EBELP, VBELN, VBELP, EINDT, MENGE, ETENS
      FROM EKES
       FOR ALL ENTRIES IN @LT_TMP
     WHERE EBELN = @LT_TMP-EBELN
       AND EBELP = @LT_TMP-EBELP
       AND LOEKZ = @SPACE
      INTO CORRESPONDING FIELDS OF TABLE @GT_ALL_0200.
    FREE LT_TMP.
    SORT GT_ALL_0200 BY EBELN EBELP EINDT.
  ENDIF.

*> 납품완료 표시.
  LT_TMP = GT_DISP[].
  SORT LT_TMP BY EBELN EBELP.
  DELETE ADJACENT DUPLICATES FROM LT_TMP COMPARING EBELN EBELP.
  IF NOT LT_TMP[] IS INITIAL.
    SELECT EBELN, EBELP, ELIKZ
      FROM EKPO
       FOR ALL ENTRIES IN @LT_TMP
     WHERE EBELN = @LT_TMP-EBELN
       AND EBELP = @LT_TMP-EBELP
       AND LOEKZ = @SPACE
      INTO TABLE @DATA(LT_EKPO).
    FREE LT_TMP.
    SORT LT_EKPO BY EBELN EBELP.
  ENDIF.

  LOOP AT GT_DISP ASSIGNING FIELD-SYMBOL(<LS_DISP>).

    READ TABLE LT_EKPO INTO DATA(LS_EKPO)
                       WITH KEY EBELN = <LS_DISP>-EBELN
                                EBELP = <LS_DISP>-EBELP
                       BINARY SEARCH.
    IF SY-SUBRC EQ 0.
      "납품완료 여부
      <LS_DISP>-ELIKZ = LS_EKPO-ELIKZ.
      "납품완료 시 미입고 잔량 0 표시
      IF NOT <LS_DISP>-ELIKZ IS INITIAL.
        CLEAR <LS_DISP>-REMAIN_GR.
      ENDIF.
    ENDIF.

    IF P_RP2 = 'X' AND <LS_DISP>-REMAIN_GR = 0.
      DELETE GT_DISP.
      CONTINUE.
    ENDIF.

*    "발주담당자 명
*    READ TABLE LT_ZTCN00002 INTO DATA(LS_ZTCN00002)
*                            WITH KEY EMP_NO = <LS_DISP>-ZORDER_PERSON
*                            BINARY SEARCH.
*    IF SY-SUBRC EQ 0.
*      <LS_DISP>-ZORDER_PERSON_NM = LS_ZTCN00002-USER_NM.
*    ENDIF.
*
*    "발주부서
*    READ TABLE LT_ZTCN00001 INTO DATA(LS_ZTCN00001)
*                            WITH KEY ORGN_CD = <LS_DISP>-ZORDER_DEPARTMENT
*                            BINARY SEARCH.
*    IF SY-SUBRC EQ 0.
*      <LS_DISP>-ZORDER_DEPARTMENT_NM = LS_ZTCN00001-ORGN_NM.
*    ENDIF.
*
*    "지출발의자 명
*    READ TABLE LT_ZTCN00002 INTO LS_ZTCN00002
*                            WITH KEY EMP_NO = <LS_DISP>-ZEXPEN_PERSON
*                            BINARY SEARCH.
*    IF SY-SUBRC EQ 0.
*      <LS_DISP>-ZEXPEN_PERSON_NM = LS_ZTCN00002-USER_NM.
*    ENDIF.
*
*    "지출발의 부서
*    READ TABLE LT_ZTCN00001 INTO LS_ZTCN00001
*                            WITH KEY ORGN_CD = <LS_DISP>-ZEXPEN_DEPARTMENT
*                            BINARY SEARCH.
*    IF SY-SUBRC EQ 0.
*      <LS_DISP>-ZEXPEN_DEPARTMENT_NM = LS_ZTCN00001-ORGN_NM.
*    ENDIF.

    "대표 일정 라인 표시 (1차 날짜 , 2차 다른건있으면 *** 표시)
    READ TABLE GT_ALL_0200 INTO DATA(LS_ALL_0200)
                           WITH KEY EBELN = <LS_DISP>-EBELN
                                    EBELP = <LS_DISP>-EBELP
                                    EINDT = <LS_DISP>-EINDT
                           BINARY SEARCH.
    IF SY-SUBRC EQ 0.
      <LS_DISP>-VBELN = LS_ALL_0200-VBELN.
    ELSE.
      READ TABLE GT_ALL_0200 INTO LS_ALL_0200
                             WITH KEY EBELN = <LS_DISP>-EBELN
                                      EBELP = <LS_DISP>-EBELP
                             BINARY SEARCH.
      IF SY-SUBRC EQ 0.
        <LS_DISP>-VBELN = GC_VEBLN_MULTI.
      ENDIF.
    ENDIF.


  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_F4_BSART
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> P_
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form SET_F4_BSART
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM SET_F4_BSART USING IV_SCR_NAME.

  DATA: LT_RETURN    TYPE TABLE OF DDSHRETVAL,
        LC_TITLE(15) TYPE C.


  CONSTANTS: LC_RETFIELD  TYPE FIELDNAME VALUE 'BSART'.

  LC_TITLE = TEXT-D04.


  FIELD-SYMBOLS: <LV_SCR_VALUE> TYPE ANY.
  ASSIGN (IV_SCR_NAME) TO <LV_SCR_VALUE>.

* Get Data
  SELECT BSTYP,
         BSART,
         BATXT
    FROM T161T
   WHERE SPRAS = @SY-LANGU
     AND BSTYP IN ( 'F', 'L' )
    INTO TABLE @DATA(LT_T161T).

  SORT LT_T161T BY BSTYP BSART.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      WINDOW_TITLE      = LC_TITLE
      RETFIELD          = LC_RETFIELD        "더블클릭하면 가져올 값
      DYNPPROG          = SY-CPROG
      DYNPNR            = SY-DYNNR
      DYNPROFIELD       = IV_SCR_NAME        "Retern Field가 실제로 복사될 화면 필드
      VALUE_ORG         = 'S'
    TABLES
      VALUE_TAB         = LT_T161T
      RETURN_TAB        = LT_RETURN
    EXCEPTIONS
      FIELD_NOT_FOUND   = 1
      NO_HELP_FOR_FIELD = 2
      INCONSISTENT_HELP = 3
      NO_VALUES_FOUND   = 4
      OTHERS            = 5.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_F4_SEL_SCR_EKGRP
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> P_
*&      --> SPACE
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form set_f4_sel_scr_EKGRP
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM SET_F4_SEL_SCR_EKGRP USING IV_SCR_NAME
                                  IV_SCR_DESC.


  DATA: LT_RETURN    TYPE TABLE OF DDSHRETVAL,
        LC_TITLE(15) TYPE C.


  CONSTANTS: LC_RETFIELD  TYPE FIELDNAME VALUE 'EKGRP'.

  LC_TITLE = TEXT-D03.


  PERFORM GET_DYNP_VALUE USING 'P_BUKRS' CHANGING P_BUKRS.

  IF P_BUKRS IS INITIAL.
    MESSAGE S017 WITH TEXT-F01 DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  SELECT DISTINCT
         EKGRP, EKNAM, BUKRS
    FROM ZSVCMM_ORG
   WHERE BUKRS EQ @P_BUKRS
    INTO TABLE @DATA(LT_ZSVCMM_ORG).

  SORT LT_ZSVCMM_ORG BY EKGRP.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      WINDOW_TITLE      = LC_TITLE
      RETFIELD          = LC_RETFIELD        "더블클릭하면 가져올 값
      DYNPPROG          = SY-CPROG
      DYNPNR            = SY-DYNNR
      DYNPROFIELD       = IV_SCR_NAME        "retfield 가 실제로 복사될 화면 필드
      VALUE_ORG         = 'S'
    TABLES
      VALUE_TAB         = LT_ZSVCMM_ORG
      RETURN_TAB        = LT_RETURN
    EXCEPTIONS
      FIELD_NOT_FOUND   = 1
      NO_HELP_FOR_FIELD = 2
      INCONSISTENT_HELP = 3
      NO_VALUES_FOUND   = 4
      OTHERS            = 5.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form get_dynp_value
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM GET_DYNP_VALUE USING IV_SCR_NAME
                     CHANGING EV_VALUE.

  DATA: LT_DYNPFIELDS TYPE TABLE OF DYNPREAD,
        LS_DYNPFIELDS TYPE DYNPREAD.

  LS_DYNPFIELDS-FIELDNAME = IV_SCR_NAME.
  APPEND LS_DYNPFIELDS TO LT_DYNPFIELDS.

  CALL FUNCTION 'DYNP_VALUES_READ'
    EXPORTING
      DYNAME               = SY-CPROG
      DYNUMB               = SY-DYNNR
    TABLES
      DYNPFIELDS           = LT_DYNPFIELDS
    EXCEPTIONS
      INVALID_ABAPWORKAREA = 1
      INVALID_DYNPROFIELD  = 2
      INVALID_DYNPRONAME   = 3
      INVALID_DYNPRONUMMER = 4
      INVALID_REQUEST      = 5
      NO_FIELDDESCRIPTION  = 6
      INVALID_PARAMETER    = 7
      UNDEFIND_ERROR       = 8
      DOUBLE_CONVERSION    = 9
      STEPL_NOT_FOUND      = 10
      OTHERS               = 11.

  IF SY-SUBRC = 0.
    SORT LT_DYNPFIELDS BY FIELDNAME.
    READ TABLE LT_DYNPFIELDS INTO LS_DYNPFIELDS
                             WITH KEY FIELDNAME = IV_SCR_NAME
                             BINARY SEARCH.
    IF SY-SUBRC = 0.
      EV_VALUE = LS_DYNPFIELDS-FIELDVALUE.
    ENDIF.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form VARIANT_F4
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> GS_VARIANT
*&      <-- P_VAR
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form variant_f4
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> GS_VARIANT
*&      <-- P_VAR
*&---------------------------------------------------------------------*
FORM VARIANT_F4 USING IS_VARIANT TYPE ANY
                 CHANGING  CV_VARIANT_SCREEN TYPE ANY.

  DATA: LS_VARIANT TYPE DISVARIANT,
        LV_EXIT(1) TYPE C.

  CALL FUNCTION 'LVC_VARIANT_F4'
    EXPORTING
      IS_VARIANT    = IS_VARIANT
      I_SAVE        = 'A'
    IMPORTING
      E_EXIT        = LV_EXIT
      ES_VARIANT    = LS_VARIANT
    EXCEPTIONS
      NOT_FOUND     = 1
      PROGRAM_ERROR = 2
      OTHERS        = 3.
  IF SY-SUBRC <> 0.
    MESSAGE I031(ZPCA01).  "'No found layout available for F4.'
  ENDIF.

  IF LV_EXIT IS INITIAL.
    CV_VARIANT_SCREEN = LS_VARIANT-VARIANT.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_F4_LISTBOX
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM SET_F4_LISTBOX .

*>회사코드 List box
  PERFORM SET_F4_BUKRS_LISTBOX.

*> 내/외자구분 List box
  PERFORM SET_F4_KALSK_LISTBOX.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_SEL_SCR_OUTPUT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM SET_SEL_SCR_OUTPUT .
  CONSTANTS : lc_2101(4) VALUE '2101'.
  LOOP AT SCREEN.
    IF SCREEN-GROUP1 EQ 'MST'.
      IF GV_EXC_USER IS INITIAL.
        SCREEN-INPUT = 0.
      ELSE.
        SCREEN-INPUT = 1.
      ENDIF.
    ENDIF.
    MODIFY SCREEN.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form set_f4_bukrs_listbox
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM SET_F4_BUKRS_LISTBOX.

  DATA: LV_NAME TYPE  VRM_ID,
        LT_LIST TYPE  VRM_VALUES.

  CONSTANTS: LC_SCR_NAME TYPE SCREEN-NAME VALUE 'P_BUKRS'.

  LV_NAME =  LC_SCR_NAME.

  ZCL_MM_COMMON=>COMMON_CONFIG(
     EXPORTING  IS_COMMON =  VALUE #( M = 'A1' D = 'A1000' S = 'AA100' )
                                     IT_WHERE = VALUE #(
                                                         ( FIELD = 1 VALUE = 'BUKRS' ) )
     IMPORTING ET_OUTTAB = DATA(LT_CONFIG) ).

  SELECT A~FIELD2 AS KEY, A~FIELD3 AS TEXT
    FROM @LT_CONFIG AS A
   WHERE A~FIELD2 NE @SPACE
    INTO CORRESPONDING FIELDS OF TABLE @LT_LIST.

  SORT  LT_LIST  BY  KEY.

  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      ID     = LV_NAME
      VALUES = LT_LIST.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_F4_KALSK_LISTBOX
*&---------------------------------------------------------------------*
FORM SET_F4_KALSK_LISTBOX.

  DATA: LT_LIST TYPE VRM_VALUES,     "Key, Text
        LS_LIST TYPE VRM_VALUE.

  CONSTANTS: LC_NAME TYPE VRM_ID VALUE 'P_KALSK'.

  DEFINE _L_APPEND_VALUES.

    CLEAR: LS_LIST.
    LS_LIST-KEY  = &1.
    LS_LIST-TEXT = &2.
    APPEND LS_LIST TO LT_LIST.

  END-OF-DEFINITION.


  SELECT KALSK,
         KALSB
    INTO TABLE @DATA(LT_HELP)
    FROM TMKKT
   WHERE SPRAS = @SY-LANGU.

  LOOP AT LT_HELP INTO DATA(LS_HELP).

    IF LS_HELP-KALSK IS INITIAL.  "전체는 안보이게
      CONTINUE.
    ENDIF.

    _L_APPEND_VALUES: LS_HELP-KALSK  LS_HELP-KALSB.

  ENDLOOP.

  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      ID     = LC_NAME
      VALUES = LT_LIST.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form MOVE_TO_MM03
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LS_DISP_MATNR
*&      --> LS_DISP_WERKS
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form MOVE_TO_MM03
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM MOVE_TO_MM03 USING IV_MATNR
                            IV_WERKS.

  CHECK NOT IV_MATNR IS INITIAL.

  CALL FUNCTION 'MMPUR_MATERIAL_DISPLAY'
    EXPORTING
      IM_MATNR      = IV_MATNR
      IM_WERKS      = IV_WERKS
*     IM_LGORT      = IV_LGORT
    EXCEPTIONS                                          "930588
      ERROR_MESSAGE = 1
      OTHERS        = 2.
  IF SY-SUBRC NE 0.
    MESSAGE ID SY-MSGID TYPE 'S' NUMBER SY-MSGNO DISPLAY LIKE 'E'
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

ENDFORM.
