*&---------------------------------------------------------------------*
*& Include          ZRMM4420F01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form BUKRS_CHECK
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> P_BUKRS
*&---------------------------------------------------------------------*
FORM BUKRS_CHECK USING IV_BUKRS.

  IF IV_BUKRS IS NOT INITIAL.
    SELECT SINGLE BUTXT FROM T001 INTO @GV_BUTXT
    WHERE BUKRS = @IV_BUKRS.
    IF SY-SUBRC NE 0.
      MESSAGE E005 WITH TEXT-F02 DISPLAY LIKE 'E'.
    ENDIF.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form WERKS_CHECK
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> S_WERKS_LOW
*&---------------------------------------------------------------------*
FORM WERKS_CHECK USING IV_WERKS.

  IF P_BUKRS IS INITIAL AND IV_WERKS IS NOT INITIAL.
    CLEAR : S_WERKS.
    MESSAGE S017 WITH TEXT-F02.
    LEAVE SCREEN.
  ENDIF.

  IF IV_WERKS IS NOT INITIAL.
    SELECT SINGLE B~NAME1
       FROM T001K AS A INNER JOIN T001W AS B
                                       ON A~BWKEY = B~BWKEY
       INTO @GV_NAME1
    WHERE A~BUKRS = @P_BUKRS
         AND B~WERKS = @IV_WERKS.
    IF SY-SUBRC NE 0.
      MESSAGE E005 WITH TEXT-F03 DISPLAY LIKE 'E'.
    ENDIF.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form LGORT_CHECK
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> S_LGORT_LOW
*&---------------------------------------------------------------------*
FORM LGORT_CHECK USING IV_LGORT.

  IF S_WERKS IS INITIAL AND IV_LGORT IS NOT INITIAL.
    MESSAGE S017 WITH TEXT-F03.
    LEAVE SCREEN.
  ENDIF.

*  IF IV_LGORT IS NOT INITIAL.
*
*    SELECT SINGLE A~LGORT, A~LGOBE, B~ZWMS, B~Z3PLT
*       INTO @DATA(LS_T001L)
*      FROM T001L AS A INNER JOIN ZTTM01010 AS B
*                                     ON A~WERKS = B~WERKS
*                                    AND A~LGORT = B~LGORT
*     WHERE A~WERKS IN @S_WERKS
*         AND A~LGORT = @IV_LGORT.
*    IF SY-SUBRC NE 0.
*      MESSAGE E005 WITH TEXT-F04 DISPLAY LIKE 'E'.
*    ENDIF.
*
*  ENDIF.

*  GV_LGOBE = LS_T001L-LGOBE.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form VARIANT_F4_1000
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> P_
*&      --> P_
*&      --> P_
*&---------------------------------------------------------------------*
FORM VARIANT_F4_1000 USING IV_FNAME IV_TEXT IV_OPTION.

  TYPES: BEGIN OF LTS_VALUE_WERKS,
           KYFLD TYPE T001W-WERKS,
           DESCR TYPE T001W-NAME1,
         END OF LTS_VALUE_WERKS.

  TYPES: BEGIN OF LTS_VALUE_LGORT,
           KYFLD TYPE T001L-LGORT,
           DESCR TYPE T001L-LGOBE,
         END OF LTS_VALUE_LGORT.

  TYPES: BEGIN OF LTS_VALUE_DISPO,
           KYFLD1 TYPE T024D-WERKS,
           KYFLD  TYPE T024D-DISPO,
           DESCR  TYPE T024D-DSNAM,
         END OF LTS_VALUE_DISPO.

  DATA: LT_FIELD    TYPE APB_LPD_T_DFIES, "TABLE OF DFIES,
        LT_RETURN   TYPE TABLE OF DDSHRETVAL,
        LT_DYUPDATE TYPE TABLE OF  DYNPREAD.

  DATA: LV_REFLD     TYPE DFIES-FIELDNAME,
        LV_DYFLD     TYPE HELP_INFO-DYNPROFLD,
        LV_DESC      TYPE HELP_INFO-DYNPROFLD,
        LV_TITLE(50).

  DATA: LRF_DATA TYPE REF TO DATA.

  FIELD-SYMBOLS: <LT_DATA> TYPE STANDARD TABLE.

  CONSTANTS: LC_KYFLD   TYPE FIELDNAME VALUE 'KYFLD',
             LC_KYFLD1  TYPE FIELDNAME VALUE 'KYFLD1',
             LC_DESCR   TYPE FIELDNAME VALUE 'DESCR',
             LC_1000(4) VALUE '1000'.

  DEFINE _L_SET_VAL.

    LT_FIELD = VALUE #( BASE LT_FIELD
               ( FIELDNAME = &1 INTTYPE = &2
               LENG = &3 INTLEN = &3 OUTPUTLEN = &3
                 REPTEXT = &4 OFFSET = &5 ) ).

  END-OF-DEFINITION.


  IF P_BUKRS IS INITIAL.
    PERFORM SCREEN_VALUE_FIND USING GC_BUKRS
                              CHANGING P_BUKRS.
  ENDIF.

  IF S_WERKS[] IS INITIAL.

    DATA : LV_FNAME TYPE FIELDNAME.

    LV_FNAME = GC_WERKS.

    DATA(LT_PARAMS) = ZCL_MM_COMMON=>GET_DYNP_PARAM(
                          IV_FNAME = LV_FNAME
                          IV_DYNNR = GC_1000  ).
    DATA(LS_PARAMS) = LT_PARAMS[ SELNAME = GC_WERKS ].

    IF LS_PARAMS-LOW IS NOT INITIAL.
      S_WERKS[] = VALUE #(
                          ( SIGN = 'I'  OPTION = 'EQ'
                            LOW = LS_PARAMS-LOW HIGH = LS_PARAMS-HIGH ) ).
    ENDIF.

  ENDIF.


  LV_DYFLD = IV_FNAME.
  LV_DESC = IV_TEXT.


  CASE IV_FNAME.
    WHEN 'WERKS'.

      CREATE DATA LRF_DATA TYPE TABLE OF LTS_VALUE_WERKS.
      ASSIGN LRF_DATA->* TO <LT_DATA>.

      _L_SET_VAL : 'KYFLD' 'C' '8'  TEXT-C07     '0',
                   'DESCR' 'C' '30' TEXT-C08     '8'.

    WHEN 'LGORT'.

      CREATE DATA LRF_DATA TYPE TABLE OF LTS_VALUE_LGORT.
      ASSIGN LRF_DATA->* TO <LT_DATA>.

      _L_SET_VAL : 'KYFLD' 'C' '8'  TEXT-C13     '0',
                   'DESCR' 'C' '30' TEXT-C14     '8'.

    WHEN 'DISPO'.

      CREATE DATA LRF_DATA TYPE TABLE OF LTS_VALUE_DISPO.
      ASSIGN LRF_DATA->* TO <LT_DATA>.

      _L_SET_VAL : 'KYFLD1' 'C' '8'  TEXT-C07    '0',
                   'KYFLD'  'C' '6'  TEXT-C11    '8',
                   'DESCR'  'C' '30' TEXT-C12    '14'.
  ENDCASE.


*-- 네이밍룰 EVT_M_NAME으로 구별가능한 것으로
  PERFORM EVT_GRID_F4_1000 USING  IV_FNAME
                                    CHANGING <LT_DATA>
                                             LV_TITLE.

  DATA(LT_MAP) = VALUE ICL_DSELC_T(
              ( FLDNAME = LC_KYFLD DYFLDNAME = LV_DYFLD )
              ( FLDNAME = LC_DESCR DYFLDNAME = LV_DESC ) ).

  "-Call F4 Search Help
  CHECK LV_TITLE IS NOT INITIAL.
  LV_REFLD = IV_FNAME.
  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      RETFIELD        = LV_REFLD
      DYNPPROG        = SY-CPROG
      DYNPNR          = SY-DYNNR
      DYNPROFIELD     = LV_DYFLD
      WINDOW_TITLE    = LV_TITLE
      VALUE_ORG       = 'S'
    TABLES
      VALUE_TAB       = <LT_DATA>
      FIELD_TAB       = LT_FIELD
      DYNPFLD_MAPPING = LT_MAP
      RETURN_TAB      = LT_RETURN
    EXCEPTIONS
      PARAMETER_ERROR = 1
      NO_VALUES_FOUND = 2
      OTHERS          = 3.

  CHECK LT_RETURN IS NOT INITIAL.
  SORT LT_RETURN BY FIELDNAME.
  READ TABLE LT_RETURN INTO DATA(LS_RETURN)
          WITH KEY FIELDNAME = LC_KYFLD BINARY SEARCH.

  IF LV_DESC IS NOT INITIAL.
    READ TABLE LT_RETURN INTO DATA(LS_RETURN_T)
            WITH KEY FIELDNAME = LC_DESCR BINARY SEARCH.
  ENDIF.

  CASE IV_FNAME.
    WHEN 'WERKS'.

      IF IV_OPTION = 'LOW'.
        S_WERKS-LOW = LS_RETURN-FIELDVAL.
        GV_NAME1 = LS_RETURN_T-FIELDVAL.
      ELSE.
        S_WERKS-HIGH = LS_RETURN-FIELDVAL.
        GV_NAME1 = LS_RETURN_T-FIELDVAL.
      ENDIF.

    WHEN 'LGORT'.

      IF IV_OPTION = 'LOW'.
        S_LGORT-LOW = LS_RETURN-FIELDVAL.
        GV_LGOBE = LS_RETURN_T-FIELDVAL.
      ELSE.
        S_LGORT-HIGH = LS_RETURN-FIELDVAL.
        GV_LGOBE = LS_RETURN_T-FIELDVAL.
      ENDIF.

    WHEN 'DISPO'.

      IF IV_OPTION = 'LOW'.
        S_DISPO-LOW = LS_RETURN-FIELDVAL.
        GV_DISPO = LS_RETURN_T-FIELDVAL.
      ELSE.
        S_DISPO-HIGH = LS_RETURN-FIELDVAL.
        GV_DISPO = LS_RETURN_T-FIELDVAL.
      ENDIF.

  ENDCASE.

  CHECK LS_RETURN-FIELDVAL IS NOT INITIAL AND
            LV_DESC IS NOT INITIAL.

  CLEAR LT_DYUPDATE[].
  LT_DYUPDATE = VALUE #(
             ( FIELDNAME  = IV_TEXT FIELDVALUE = LS_RETURN_T-FIELDVAL ) ).

  "PBO가 없는 화면필드내용 변경
  CALL FUNCTION 'DYNP_VALUES_UPDATE'
    EXPORTING
      DYNAME               = SY-REPID
      DYNUMB               = SY-DYNNR
    TABLES
      DYNPFIELDS           = LT_DYUPDATE[]
    EXCEPTIONS
      INVALID_ABAPWORKAREA = 1
      INVALID_DYNPROFIELD  = 2
      INVALID_DYNPRONAME   = 3
      INVALID_DYNPRONUMMER = 4
      INVALID_REQUEST      = 5
      NO_FIELDDESCRIPTION  = 6
      UNDEFIND_ERROR       = 7
      OTHERS               = 8.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SCREEN_VALUE_FIND
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> GC_BUKRS
*&      <-- P_BUKRS
*&---------------------------------------------------------------------*
FORM SCREEN_VALUE_FIND USING IV_PARA
                        CHANGING CV_RETURN.

  DATA : LV_FNAME TYPE FIELDNAME.

  LV_FNAME = IV_PARA.

  DATA(LT_PARAMS) = ZCL_MM_COMMON=>GET_DYNP_PARAM(
                        IV_FNAME = LV_FNAME
                        IV_DYNNR = GC_1000  ).
  DATA(LS_PARAMS) = LT_PARAMS[ SELNAME = IV_PARA ].
  CV_RETURN = LS_PARAMS-LOW.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form EVT_GRID_F4_1000
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> IV_FNAME
*&      <-- <LT_DATA>
*&      <-- LV_TITLE
*&---------------------------------------------------------------------*
FORM EVT_GRID_F4_1000 USING IV_FIELDNAME
                  CHANGING CT_F4_LIST  TYPE TABLE
                           CV_TITLE.

  CASE IV_FIELDNAME.
    WHEN 'WERKS'.

      IF P_BUKRS IS INITIAL.
        MESSAGE S017 WITH TEXT-F02.
        EXIT.
      ENDIF.

      SELECT B~WERKS AS KYFLD , B~NAME1 AS DESCR
        INTO CORRESPONDING FIELDS OF TABLE @CT_F4_LIST
        FROM T001K AS A INNER JOIN T001W AS B
                                       ON A~BWKEY = B~BWKEY
       WHERE A~BUKRS = @P_BUKRS
       ORDER BY B~WERKS.
      IF SY-SUBRC NE 0.
        MESSAGE S005 WITH TEXT-F03 DISPLAY LIKE 'E'.
        EXIT.
      ENDIF.
      _G_SET_VALUE : CV_TITLE          TEXT-C07.

    WHEN 'LGORT'.
      IF S_WERKS[] IS INITIAL.
        MESSAGE S017 WITH TEXT-F03.
        EXIT.
      ENDIF.

      SELECT LGORT AS KYFLD , LGOBE AS DESCR
        INTO CORRESPONDING FIELDS OF TABLE @CT_F4_LIST
        FROM T001L
       WHERE WERKS IN @S_WERKS
      ORDER BY LGORT.
      IF SY-SUBRC NE 0.
        MESSAGE S005 WITH TEXT-F04 DISPLAY LIKE 'E'.
        EXIT.
      ENDIF.
      _G_SET_VALUE : CV_TITLE          TEXT-C13.

    WHEN 'DISPO'.
      IF S_WERKS[] IS NOT INITIAL.

        SELECT WERKS AS KYFLD1, DISPO AS KYFLD , DSNAM AS DESCR
          INTO CORRESPONDING FIELDS OF TABLE @CT_F4_LIST
          FROM T024D
         WHERE WERKS IN @S_WERKS.

        IF SY-SUBRC NE 0.
          MESSAGE S005 WITH TEXT-F05 DISPLAY LIKE 'E'.
          EXIT.
        ENDIF.

*        DELETE ADJACENT DUPLICATES FROM CT_F4_LIST COMPARING ALL FIELDS.
        _G_SET_VALUE : CV_TITLE          TEXT-C11.

      ELSE.
        SELECT WERKS AS KYFLD1, DISPO AS KYFLD , DSNAM AS DESCR
        INTO CORRESPONDING FIELDS OF TABLE @CT_F4_LIST
        FROM T024D
        ORDER BY DISPO.

        IF SY-SUBRC NE 0.
          MESSAGE S005 WITH TEXT-F05 DISPLAY LIKE 'E'.
          EXIT.
        ENDIF.

        DELETE ADJACENT DUPLICATES FROM CT_F4_LIST COMPARING ALL FIELDS.
        _G_SET_VALUE : CV_TITLE          TEXT-C11.
      ENDIF.
  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form VARIANT_F4
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
*& Form SELECTION_SCREEN_OUTPUT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM SELECTION_SCREEN_OUTPUT .

  LOOP AT SCREEN.

    CASE SCREEN-NAME.
      WHEN 'P_BUKRS'.
        SCREEN-INPUT = 1.
    ENDCASE.

    CASE 'X'.
      WHEN P_RD2B.  "저장위치 재고
        IF SCREEN-GROUP1 EQ 'SC1'.
          SCREEN-INPUT = 1.
          SCREEN-INVISIBLE = 0.
        ENDIF.
        IF SCREEN-GROUP1 EQ 'SC2'.
          SCREEN-INPUT = 0.
          SCREEN-INVISIBLE = 1.
        ENDIF.

      WHEN P_RD2C.
        IF SCREEN-GROUP1 EQ 'SC1'.
          SCREEN-INPUT = 0.
          SCREEN-INVISIBLE = 1.
        ENDIF.
        IF SCREEN-GROUP1 EQ 'SC2'.
          SCREEN-INPUT = 1.
          SCREEN-INVISIBLE = 0.
        ENDIF.

    ENDCASE.

    MODIFY SCREEN.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form INITIALIZATION
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM INITIALIZATION .

  "기간 DEFAULT SETTING: SY-DATLO -1 MONTH ~ SY-DATLO

  DATA : LV_FROM_DATE TYPE SY-DATLO.                     "U5 - TimeZone 적용

  CLEAR : LV_FROM_DATE.

  CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
    EXPORTING
      DATE      = SY-DATLO
      DAYS      = '00'
      MONTHS    = '01'
      SIGNUM    = '-'
      YEARS     = '00'
    IMPORTING
      CALC_DATE = LV_FROM_DATE.

  S_BUDAT[] = VALUE #( ( SIGN = 'I' OPTION = 'BT' LOW = LV_FROM_DATE HIGH = SY-DATLO ) ).

*  SELECT SINGLE COMPANY, COMPANY_NAME
*      INTO ( @P_BUKRS, @GV_BUTXT )
*    FROM ZSVMM_USER_INFO
*  WHERE USER_ID = @SY-UNAME.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CHECK_AUTHORITY
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM CHECK_AUTHORITY .

*  call function 'ZFMM_AUTH_CHECK'
*    EXPORTING
*      IV_USER = SY-UNAME.
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
*  IF SY-SUBRC <> 0.
*    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4 DISPLAY LIKE 'E'.
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
FORM GET_DATA .

*--------------------------------
* 검색조건 설정
*--------------------------------

  DATA : LV_DAYS TYPE VTBBEWE-ATAGE.

  CLEAR : GV_PERIODTYPE, LV_DAYS.
  _G_INIT : GR_TYPECODE.

  IF S_BUDAT-HIGH IS INITIAL.
    S_BUDAT-HIGH = S_BUDAT-LOW.
  ENDIF.

  CASE 'X'. "기간구분
    WHEN P_RD1A.  "Daily
      GV_PERIODTYPE = 'D'.
    WHEN P_RD1B. "Monthly
      GV_PERIODTYPE = 'M'.
    WHEN P_RD1C.  "Quarter
      GV_PERIODTYPE = 'Q'.
    WHEN P_RD1D.  "Yearly
      GV_PERIODTYPE = 'Y'.
  ENDCASE.

  CALL FUNCTION 'FIMA_DAYS_AND_MONTHS_AND_YEARS'
    EXPORTING
      I_DATE_FROM = S_BUDAT-LOW
      I_DATE_TO   = S_BUDAT-HIGH
    IMPORTING
      E_DAYS      = LV_DAYS.

  CASE 'X'. "재고유형코드
    WHEN P_RD3A. "전체
      GR_TYPECODE = VALUE #( ( SIGN = 'I' OPTION = 'BT' LOW = '01' HIGH = '03' ) ).
    WHEN P_RD3B. "가용 재고
      GR_TYPECODE = VALUE #( ( SIGN = 'I' OPTION = 'EQ' LOW = '01' ) ).
    WHEN P_RD3C.  "품질 검사 중 재고
      GR_TYPECODE = VALUE #( ( SIGN = 'I' OPTION = 'EQ' LOW = '02' ) ).
    WHEN P_RD3D.  "보류 재고
      GR_TYPECODE = VALUE #( ( SIGN = 'I' OPTION = 'EQ' LOW = '03' ) ).
  ENDCASE.


  IF GV_PERIODTYPE = 'D' AND LV_DAYS > 365 .
    GV_ERROR = 'X'.
    EXIT.
  ENDIF.

*--------------------------------
* Data 추출
*--------------------------------
  CLEAR : GT_DATA, GT_DISP.

  PERFORM GET_DATA_ALL.

*- Setting Display date
  PERFORM SET_DAY.

*- SET BASE_DATA
  PERFORM SET_BASE_DATA.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_DATA_ALL
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM GET_DATA_ALL .

  PERFORM GET_DATA_ALL_EXTRA.

  IF GT_DATA[] IS NOT INITIAL.
    IF P_CHK1 = ''.
      SORT GT_DATA BY MATNR WERKS LGORT LIFNR TYPECODE ZDATE.
    ELSE.
      SORT GT_DATA BY MATNR WERKS LGORT LIFNR TYPECODE BATCH ZDATE.
    ENDIF.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_DAY
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM SET_DAY .

*-
  _G_INIT: GT_DAY.

  DATA(LT_DATA) = GT_DATA[].

  MOVE-CORRESPONDING LT_DATA TO GT_DAY.

  SORT GT_DAY BY ZDATE ASCENDING.
  DELETE ADJACENT DUPLICATES FROM GT_DAY COMPARING ZDATE.


  DATA: LV_CDAY  TYPE NUMC3.

  LOOP AT GT_DAY ASSIGNING FIELD-SYMBOL(<LS_DAY>).

*-
    ADD 1 TO LV_CDAY.
    <LS_DAY>-DAY  = |{ TEXT-O01 }| && LV_CDAY.
*-
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_BASE_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM SET_BASE_DATA .

  _G_INIT: GT_BASE, GT_BASE_A.

  IF P_CHK1 = 'X'.
    GT_BASE[] = CORRESPONDING #( GT_DATA DISCARDING DUPLICATES
                                     MAPPING MATNR = MATNR
                                             WERKS = WERKS
                                             LGORT = LGORT
                                             LIFNR = LIFNR
                                             TYPECODE = TYPECODE
                                             BATCH = BATCH ).

  ELSE.

    GT_BASE_A[] = CORRESPONDING #( GT_DATA DISCARDING DUPLICATES
                                     MAPPING MATNR = MATNR
                                             WERKS = WERKS
                                             LGORT = LGORT
                                             LIFNR = LIFNR
                                             TYPECODE = TYPECODE ).

  ENDIF.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_DATA_ALL_EXTRA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM GET_DATA_ALL_EXTRA .


  DATA : ls_data TYPE ty_data.

  IF P_CHK1 = ''.

    CASE 'X'.
      WHEN P_RD2A.  "Total 재고
        SELECT FROM ZSVPMMSTOCKTIMES( P_STARTDATE = @S_BUDAT-LOW, P_ENDDATE = @S_BUDAT-HIGH, P_PERIODTYPE = @GV_PERIODTYPE )
          FIELDS
            BUKRS, COMPANYCODENAME, MATNR, MATERIALNAME, WERKS, PLANTNAME, LGORT, STORAGELOCATIONNAME, LIFNR, NAME1,
             INVENTORYSPECIALSTOCKTYPE, PERIODTYPE, TYPENAME, TYPECODE, MEINS, ZDATE, STOCKQTY, EKGRP, EKNAM, DISPO, DSNAM, MATKL,
             batch
        WHERE BUKRS = @P_BUKRS
          AND WERKS IN @S_WERKS
         INTO TABLE @DATA(lt_tmp).

        LOOP AT lt_tmp INTO DATA(ls_tmp).
          CHECK ls_tmp-LGORT IN S_LGORT        AND
                ls_tmp-LIFNR IN S_LIFNR        AND
                ls_tmp-MATNR IN S_MATNR        AND
                ls_tmp-BATCH IN S_CHARG        AND
                ls_tmp-TYPECODE IN GR_TYPECODE AND
                ls_tmp-EKGRP IN S_EKGRP        AND
                ls_tmp-DISPO IN S_DISPO        AND
                ls_tmp-MATKL IN S_MATKL        .

            ls_data = CORRESPONDING #( ls_tmp ).
            CLEAR ls_data-batch.
            APPEND ls_data TO gt_data. CLEAR ls_data.
        ENDLOOP.

      WHEN P_RD2B.  "저장위치 재고

        SELECT FROM ZSVPMMSTOCKTIMES( P_STARTDATE = @S_BUDAT-LOW, P_ENDDATE = @S_BUDAT-HIGH, P_PERIODTYPE = @GV_PERIODTYPE )
          FIELDS
            BUKRS, COMPANYCODENAME, MATNR, MATERIALNAME, WERKS, PLANTNAME, LGORT, STORAGELOCATIONNAME, LIFNR, NAME1,
            INVENTORYSPECIALSTOCKTYPE, PERIODTYPE, TYPENAME, TYPECODE, MEINS, ZDATE, STOCKQTY, EKGRP, EKNAM, DISPO, DSNAM, MATKL,
            batch
        WHERE BUKRS = @P_BUKRS
          AND WERKS IN @S_WERKS
         INTO TABLE @lt_tmp.

        LOOP AT lt_tmp INTO ls_tmp.
          CHECK
                ls_tmp-LGORT IN S_LGORT        AND
                ls_tmp-MATNR IN S_MATNR        AND
                ls_tmp-BATCH IN S_CHARG        AND
                ls_tmp-INVENTORYSPECIALSTOCKTYPE = '' AND
                ls_tmp-TYPECODE IN GR_TYPECODE AND
                ls_tmp-EKGRP IN S_EKGRP        AND
                ls_tmp-DISPO IN S_DISPO        AND
                ls_tmp-MATKL IN S_MATKL.

            ls_data = CORRESPONDING #( ls_tmp ).
            CLEAR ls_data-batch.
            APPEND ls_data TO gt_data. CLEAR ls_data.
        ENDLOOP.


      WHEN P_RD2C.  "업체 SC 재고

        SELECT FROM ZSVPMMSTOCKTIMES( P_STARTDATE = @S_BUDAT-LOW, P_ENDDATE = @S_BUDAT-HIGH, P_PERIODTYPE = @GV_PERIODTYPE )
          FIELDS
            BUKRS, COMPANYCODENAME, MATNR, MATERIALNAME, WERKS, PLANTNAME, LGORT, STORAGELOCATIONNAME, LIFNR, NAME1,
             INVENTORYSPECIALSTOCKTYPE, PERIODTYPE, TYPENAME, TYPECODE, MEINS, ZDATE, STOCKQTY, EKGRP, EKNAM, DISPO, DSNAM, MATKL,
             batch
        WHERE BUKRS = @P_BUKRS
          AND WERKS IN @S_WERKS
         INTO TABLE @lt_tmp.

        LOOP AT lt_tmp INTO ls_tmp.
          CHECK ls_tmp-LIFNR IN S_LIFNR        AND
                ls_tmp-MATNR IN S_MATNR        AND
                ls_tmp-BATCH IN S_CHARG        AND
                ls_tmp-INVENTORYSPECIALSTOCKTYPE = 'O' AND
                ls_tmp-TYPECODE IN GR_TYPECODE AND
                ls_tmp-EKGRP IN S_EKGRP        AND
                ls_tmp-DISPO IN S_DISPO        AND
                ls_tmp-MATKL IN S_MATKL        .

            ls_data = CORRESPONDING #( ls_tmp ).
            CLEAR ls_data-batch.
            APPEND ls_data TO gt_data. CLEAR ls_data.
        ENDLOOP.


      WHEN P_RD2D.  "업체 위탁 재고

        SELECT FROM ZSVPMMSTOCKTIMES( P_STARTDATE = @S_BUDAT-LOW, P_ENDDATE = @S_BUDAT-HIGH, P_PERIODTYPE = @GV_PERIODTYPE )
        FIELDS
          BUKRS, COMPANYCODENAME, MATNR, MATERIALNAME, WERKS, PLANTNAME, LGORT, STORAGELOCATIONNAME, LIFNR, NAME1,
           INVENTORYSPECIALSTOCKTYPE, PERIODTYPE, TYPENAME, TYPECODE, MEINS, ZDATE, STOCKQTY, EKGRP, EKNAM, DISPO, DSNAM, MATKL,
           batch    "(+) KTGL-34176
      WHERE BUKRS = @P_BUKRS
        AND WERKS IN @S_WERKS
        INTO TABLE @lt_tmp.


        LOOP AT lt_tmp INTO ls_tmp.
          CHECK ls_tmp-LGORT IN S_LGORT        AND
                ls_tmp-LIFNR IN S_LIFNR        AND
                ls_tmp-MATNR IN S_MATNR        AND
                ls_tmp-BATCH IN S_CHARG        AND
                ls_tmp-INVENTORYSPECIALSTOCKTYPE = 'K' AND
                ls_tmp-TYPECODE IN GR_TYPECODE AND
                ls_tmp-EKGRP IN S_EKGRP        AND
                ls_tmp-DISPO IN S_DISPO        AND
                ls_tmp-MATKL IN S_MATKL        .

            ls_data = CORRESPONDING #( ls_tmp ).
            CLEAR ls_data-batch.
            APPEND ls_data TO gt_data. CLEAR ls_data.
        ENDLOOP.


      WHEN P_RD2E.  "고객 판매 재고

        SELECT FROM ZSVPMMSTOCKTIMES( P_STARTDATE = @S_BUDAT-LOW, P_ENDDATE = @S_BUDAT-HIGH, P_PERIODTYPE = @GV_PERIODTYPE )
        FIELDS
          BUKRS, COMPANYCODENAME, MATNR, MATERIALNAME, WERKS, PLANTNAME, LGORT, STORAGELOCATIONNAME, LIFNR, NAME1,
           INVENTORYSPECIALSTOCKTYPE, PERIODTYPE, TYPENAME, TYPECODE, MEINS, ZDATE, STOCKQTY, EKGRP, EKNAM, DISPO, DSNAM, MATKL,
           batch    "(+) KTGL-34176
      WHERE BUKRS = @P_BUKRS
        AND WERKS IN @S_WERKS
          INTO TABLE @lt_tmp.

        LOOP AT lt_tmp INTO ls_tmp.
          CHECK ls_tmp-LGORT IN S_LGORT        AND
                ls_tmp-LIFNR IN S_LIFNR        AND
                ls_tmp-MATNR IN S_MATNR        AND
                ls_tmp-BATCH IN S_CHARG        AND
                ls_tmp-INVENTORYSPECIALSTOCKTYPE = 'E' AND
                ls_tmp-TYPECODE IN GR_TYPECODE AND
                ls_tmp-EKGRP IN S_EKGRP        AND
                ls_tmp-DISPO IN S_DISPO        AND
                ls_tmp-MATKL IN S_MATKL        .

            ls_data = CORRESPONDING #( ls_tmp ).
            CLEAR ls_data-batch.
            APPEND ls_data TO gt_data. CLEAR ls_data.
        ENDLOOP.


    ENDCASE.

  ELSE.

    CASE 'X'.
      WHEN P_RD2A.  "Total 재고

        SELECT FROM ZSVPMMSTOCKTIMES( P_STARTDATE = @S_BUDAT-LOW, P_ENDDATE = @S_BUDAT-HIGH, P_PERIODTYPE = @GV_PERIODTYPE )
          FIELDS
            BUKRS, COMPANYCODENAME, MATNR, MATERIALNAME, WERKS, PLANTNAME, LGORT, STORAGELOCATIONNAME, LIFNR, NAME1,
            BATCH, INVENTORYSPECIALSTOCKTYPE, PERIODTYPE, TYPENAME, TYPECODE, MEINS, ZDATE, STOCKQTY, LICHN, HSDAT, VFDAT, EKGRP, EKNAM, DISPO, DSNAM, MATKL
        WHERE BUKRS = @P_BUKRS
          AND WERKS IN @S_WERKS
           INTO TABLE @DATA(lt_tmp1).

        LOOP AT lt_tmp1 INTO DATA(ls_tmp1).
          CHECK ls_tmp1-LGORT IN S_LGORT        AND
                ls_tmp1-LIFNR IN S_LIFNR        AND
                ls_tmp1-MATNR IN S_MATNR        AND
                ls_tmp1-BATCH IN S_CHARG        AND
                ls_tmp1-TYPECODE IN GR_TYPECODE AND
                ls_tmp1-EKGRP IN S_EKGRP        AND
                ls_tmp1-DISPO IN S_DISPO        AND
                ls_tmp1-MATKL IN S_MATKL        .

            ls_data = CORRESPONDING #( ls_tmp1 ).
            APPEND ls_data TO gt_data. CLEAR ls_data.
        ENDLOOP.


      WHEN P_RD2B.  "저장위치 재고

        SELECT FROM ZSVPMMSTOCKTIMES( P_STARTDATE = @S_BUDAT-LOW, P_ENDDATE = @S_BUDAT-HIGH, P_PERIODTYPE = @GV_PERIODTYPE )
          FIELDS
            BUKRS, COMPANYCODENAME, MATNR, MATERIALNAME, WERKS, PLANTNAME, LGORT, STORAGELOCATIONNAME, LIFNR, NAME1,
            BATCH, INVENTORYSPECIALSTOCKTYPE, PERIODTYPE, TYPENAME, TYPECODE, MEINS, ZDATE, STOCKQTY, LICHN, HSDAT, VFDAT, EKGRP, EKNAM, DISPO, DSNAM, MATKL
        WHERE BUKRS = @P_BUKRS
          AND WERKS IN @S_WERKS
         INTO TABLE @lt_tmp1.


        LOOP AT lt_tmp1 INTO ls_tmp1.
          CHECK ls_tmp1-LGORT IN S_LGORT        AND
                ls_tmp1-MATNR IN S_MATNR        AND
                ls_tmp1-BATCH IN S_CHARG        AND
                ls_tmp1-INVENTORYSPECIALSTOCKTYPE = '' AND
                ls_tmp1-TYPECODE IN GR_TYPECODE AND
                ls_tmp1-EKGRP IN S_EKGRP        AND
                ls_tmp1-DISPO IN S_DISPO        AND
                ls_tmp1-MATKL IN S_MATKL        .

            ls_data = CORRESPONDING #( ls_tmp1 ).
            APPEND ls_data TO gt_data. CLEAR ls_data.
        ENDLOOP.


      WHEN P_RD2C.  "업체 SC 재고

        SELECT FROM ZSVPMMSTOCKTIMES( P_STARTDATE = @S_BUDAT-LOW, P_ENDDATE = @S_BUDAT-HIGH, P_PERIODTYPE = @GV_PERIODTYPE )
          FIELDS
            BUKRS, COMPANYCODENAME, MATNR, MATERIALNAME, WERKS, PLANTNAME, LGORT, STORAGELOCATIONNAME, LIFNR, NAME1,
            BATCH, INVENTORYSPECIALSTOCKTYPE, PERIODTYPE, TYPENAME, TYPECODE, MEINS, ZDATE, STOCKQTY, LICHN, HSDAT, VFDAT, EKGRP, EKNAM, DISPO, DSNAM, MATKL
        WHERE BUKRS = @P_BUKRS
          AND WERKS IN @S_WERKS
         INTO TABLE @lt_tmp1.

        LOOP AT lt_tmp1 INTO ls_tmp1.
          CHECK ls_tmp1-LIFNR IN S_LIFNR        AND
                ls_tmp1-MATNR IN S_MATNR        AND
                ls_tmp1-BATCH IN S_CHARG        AND
                ls_tmp1-INVENTORYSPECIALSTOCKTYPE = 'O' AND
                ls_tmp1-TYPECODE IN GR_TYPECODE AND
                ls_tmp1-EKGRP IN S_EKGRP        AND
                ls_tmp1-DISPO IN S_DISPO        AND
                ls_tmp1-MATKL IN S_MATKL        .

            ls_data = CORRESPONDING #( ls_tmp1 ).
            APPEND ls_data TO gt_data. CLEAR ls_data.
        ENDLOOP.


      WHEN P_RD2D.  "업체 위탁 재고

        SELECT FROM ZSVPMMSTOCKTIMES( P_STARTDATE = @S_BUDAT-LOW, P_ENDDATE = @S_BUDAT-HIGH, P_PERIODTYPE = @GV_PERIODTYPE )
        FIELDS
          BUKRS, COMPANYCODENAME, MATNR, MATERIALNAME, WERKS, PLANTNAME, LGORT, STORAGELOCATIONNAME, LIFNR, NAME1,
          BATCH, INVENTORYSPECIALSTOCKTYPE, PERIODTYPE, TYPENAME, TYPECODE, MEINS, ZDATE, STOCKQTY, LICHN, HSDAT, VFDAT, EKGRP, EKNAM, DISPO, DSNAM, MATKL
      WHERE BUKRS = @P_BUKRS
        AND WERKS IN @S_WERKS
         INTO TABLE @lt_tmp1.

        LOOP AT lt_tmp1 INTO ls_tmp1.
          CHECK ls_tmp1-LGORT IN S_LGORT        AND
                ls_tmp1-LIFNR IN S_LIFNR        AND
                ls_tmp1-MATNR IN S_MATNR        AND
                ls_tmp1-BATCH IN S_CHARG        AND
                ls_tmp1-INVENTORYSPECIALSTOCKTYPE = 'K' AND
                ls_tmp1-TYPECODE IN GR_TYPECODE AND
                ls_tmp1-EKGRP IN S_EKGRP        AND
                ls_tmp1-DISPO IN S_DISPO        AND
                ls_tmp1-MATKL IN S_MATKL        .

            ls_data = CORRESPONDING #( ls_tmp1 ).
            APPEND ls_data TO gt_data. CLEAR ls_data.
        ENDLOOP.

      WHEN P_RD2E.  "고객 판매 재고
        SELECT FROM ZSVPMMSTOCKTIMES( P_STARTDATE = @S_BUDAT-LOW, P_ENDDATE = @S_BUDAT-HIGH, P_PERIODTYPE = @GV_PERIODTYPE )
        FIELDS
          BUKRS, COMPANYCODENAME, MATNR, MATERIALNAME, WERKS, PLANTNAME, LGORT, STORAGELOCATIONNAME, LIFNR, NAME1,
          BATCH, INVENTORYSPECIALSTOCKTYPE, PERIODTYPE, TYPENAME, TYPECODE, MEINS, ZDATE, STOCKQTY, LICHN, HSDAT, VFDAT, EKGRP, EKNAM, DISPO, DSNAM, MATKL
      WHERE BUKRS = @P_BUKRS
        AND WERKS IN @S_WERKS
        INTO TABLE @lt_tmp1.

      LOOP AT lt_tmp1 INTO ls_tmp1.
        CHECK ls_tmp1-LGORT IN S_LGORT        AND
              ls_tmp1-LIFNR IN S_LIFNR        AND
              ls_tmp1-MATNR IN S_MATNR        AND
              ls_tmp1-BATCH IN S_CHARG        AND
              ls_tmp1-INVENTORYSPECIALSTOCKTYPE = 'E' AND
              ls_tmp1-TYPECODE IN GR_TYPECODE AND
              ls_tmp1-EKGRP IN S_EKGRP        AND
              ls_tmp1-DISPO IN S_DISPO        AND
              ls_tmp1-MATKL IN S_MATKL        .
          ls_data = CORRESPONDING #( ls_tmp1 ).
          APPEND ls_data TO gt_data. CLEAR ls_data.
      ENDLOOP.

    ENDCASE.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form PROCESSING_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM PROCESSING_DATA .

  _G_INIT: GT_DISP.

  DATA : LS_DISP   TYPE TS_DISP.
  DATA : LV_TABIX  TYPE SY-TABIX.
  DATA : LV_LANGU  TYPE SY-LANGU.


  SELECT FROM T023T
    FIELDS MATKL, WGBEZ, WGBEZ60
   WHERE SPRAS = @SY-LANGU                                "U4 - '3'에서 SY-LANGU 로 변경.
    INTO TABLE @DATA(LT_T023T).

  SORT LT_T023T BY MATKL.

  CLEAR : LV_TABIX.

  IF P_CHK1 = ''.
    LOOP AT GT_BASE_A INTO DATA(LS_BASE_A).

      CLEAR : LS_DISP.

      READ TABLE GT_DATA INTO DATA(LS_DATA_A) WITH KEY MATNR = LS_BASE_A-MATNR
                                                       WERKS = LS_BASE_A-WERKS
                                                       LGORT = LS_BASE_A-LGORT
                                                       LIFNR = LS_BASE_A-LIFNR
                                                       TYPECODE = LS_BASE_A-TYPECODE BINARY SEARCH.
      IF SY-SUBRC = 0.

        LV_TABIX = SY-TABIX.

        MOVE-CORRESPONDING LS_DATA_A TO LS_DISP.

        READ TABLE LT_T023T INTO DATA(LS_T023T) WITH KEY MATKL = LS_DISP-MATKL.
        IF SY-SUBRC = 0.
          LS_DISP-WGBEZ = LS_T023T-WGBEZ.
        ENDIF.

        PERFORM SET_DISPLAY_DATA_A  USING LS_DATA_A
                                          LV_TABIX
                                    CHANGING LS_DISP.

        APPEND LS_DISP TO GT_DISP.
        CLEAR : LS_DATA_A, LS_DISP.

      ENDIF.

    ENDLOOP.

  ELSE.

    LOOP AT GT_BASE INTO DATA(LS_BASE).

      CLEAR : LS_DISP.

      READ TABLE GT_DATA INTO DATA(LS_DATA) WITH KEY MATNR = LS_BASE-MATNR
                                                     WERKS = LS_BASE-WERKS
                                                     LGORT = LS_BASE-LGORT
                                                     LIFNR = LS_BASE-LIFNR
                                                     TYPECODE = LS_BASE-TYPECODE
                                                     BATCH = LS_BASE-BATCH BINARY SEARCH.
      IF SY-SUBRC = 0.

        LV_TABIX = SY-TABIX.

        MOVE-CORRESPONDING LS_DATA TO LS_DISP.

        READ TABLE LT_T023T INTO DATA(LS_T023T_A) WITH KEY MATKL = LS_DISP-MATKL.
        IF SY-SUBRC = 0.
          LS_DISP-WGBEZ = LS_T023T_A-WGBEZ.
        ENDIF.

        PERFORM SET_DISPLAY_DATA USING LS_DATA
                                       LV_TABIX
                                 CHANGING LS_DISP.

        APPEND LS_DISP TO GT_DISP.

        CLEAR : LS_DATA, LS_DISP.

      ENDIF.

    ENDLOOP.

  ENDIF.


  LV_LANGU = SY-LANGU.

  IF LV_LANGU NE '3'.

    LOOP AT GT_DISP INTO LS_DISP.
      CASE LS_DISP-TYPENAME.
        WHEN GC_ASTOCK.
           LS_DISP-TYPENAME = TEXT-T10.
        WHEN GC_QSTOCK.
           LS_DISP-TYPENAME = TEXT-T11.
         WHEN GC_BSTOCK.
           LS_DISP-TYPENAME = TEXT-T12..
      ENDCASE.

      MODIFY GT_DISP FROM LS_DISP TRANSPORTING TYPENAME.

    ENDLOOP.
  ENDIF.


  SORT GT_DISP BY MATNR WERKS LGORT LIFNR TYPECODE ZDATE.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_DISPLAY_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LS_DATA
*&      --> LV_TABIX
*&      <-- LS_DISP
*&---------------------------------------------------------------------*
FORM SET_DISPLAY_DATA USING IS_DATA TYPE TY_DATA
                                IV_TABIX TYPE SY-TABIX
                       CHANGING CS_DISP TYPE TS_DISP.

  FIELD-SYMBOLS: <LV_FIELD> TYPE CHAR6,
                 <LV_VALUE> TYPE ZSMM_DATE_VALUE-DAT001.

  LOOP AT GT_DATA INTO DATA(LS_DATA) FROM IV_TABIX.
    IF LS_DATA-MATNR <> CS_DISP-MATNR OR
      LS_DATA-WERKS <> CS_DISP-WERKS OR
      LS_DATA-LGORT <> CS_DISP-LGORT OR
      LS_DATA-LIFNR <> CS_DISP-LIFNR OR
      LS_DATA-TYPECODE <> CS_DISP-TYPECODE OR
      LS_DATA-BATCH <> CS_DISP-BATCH.
      EXIT.
    ENDIF.


    READ TABLE GT_DAY INTO DATA(LS_DAY) WITH KEY ZDATE = LS_DATA-ZDATE  BINARY SEARCH.
    IF SY-SUBRC = 0.
      ASSIGN LS_DAY-DAY TO <LV_FIELD>.
      IF <LV_FIELD> IS ASSIGNED.
        ASSIGN COMPONENT <LV_FIELD> OF STRUCTURE CS_DISP TO <LV_VALUE>.
        <LV_VALUE> = <LV_VALUE> + LS_DATA-STOCKQTY.
      ENDIF.
      UNASSIGN : <LV_FIELD>, <LV_VALUE>.
    ENDIF.

  ENDLOOP.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_DISPLAY_DATA_A
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LS_DATA_A
*&      --> LV_TABIX
*&      <-- LS_DISP
*&---------------------------------------------------------------------*
FORM SET_DISPLAY_DATA_A USING IS_DATA TYPE TY_DATA
                                  IV_TABIX TYPE SY-TABIX
                         CHANGING CS_DISP TYPE TS_DISP.

  FIELD-SYMBOLS: <LV_FIELD> TYPE CHAR6,
                 <LV_VALUE> TYPE ZSMM_DATE_VALUE-DAT001.


  LOOP AT GT_DATA INTO DATA(LS_DATA) FROM IV_TABIX.

    IF LS_DATA-MATNR <> CS_DISP-MATNR OR
       LS_DATA-WERKS <> CS_DISP-WERKS OR
       LS_DATA-LGORT <> CS_DISP-LGORT OR
       LS_DATA-LIFNR <> CS_DISP-LIFNR OR
       LS_DATA-TYPECODE <> CS_DISP-TYPECODE.
      EXIT.
    ENDIF.

    READ TABLE GT_DAY INTO DATA(LS_DAY) WITH KEY ZDATE = LS_DATA-ZDATE  BINARY SEARCH.
    IF SY-SUBRC = 0.
      ASSIGN LS_DAY-DAY TO <LV_FIELD>.
      IF <LV_FIELD> IS ASSIGNED.
        ASSIGN COMPONENT <LV_FIELD> OF STRUCTURE CS_DISP TO <LV_VALUE>.
        <LV_VALUE> = <LV_VALUE> + LS_DATA-STOCKQTY.
        UNASSIGN : <LV_VALUE>.   "(+)U7
      ENDIF.
      UNASSIGN : <LV_FIELD> .  ", "(-)U7 <LV_VALUE>.
    ENDIF.

  ENDLOOP.

ENDFORM.
