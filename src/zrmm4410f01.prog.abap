*&---------------------------------------------------------------------*
*& Include          ZRMM4410F01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form INITIALIZATION
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM INITIALIZATION .

  "기간 DEFAULT SETTING: 당월 1일 ~ 당일
  DATA: LV_START_DATE TYPE SY-DATLO.

  LV_START_DATE = SY-DATLO.
  LV_START_DATE+6(2) = '01'.

  S_DATE[] = VALUE #( ( SIGN = 'I' OPTION = 'BT' LOW = LV_START_DATE HIGH = SY-DATLO ) ).

*  SELECT SINGLE COMPANY, COMPANY_NAME
*      INTO ( @P_BUKRS, @GV_BUTXT )
*    FROM ZSVMM_USER_INFO
*  WHERE USER_ID = @SY-UNAME.

ENDFORM.
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
*& Form werks_check
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
*& Form lgort_check
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

  IF IV_LGORT IS NOT INITIAL.

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

  ENDIF.

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

      _L_SET_VAL : 'KYFLD' 'C' '8'  TEXT-F03         '0',
                   'DESCR' 'C' '30' TEXT-C01      '8'.

    WHEN 'LGORT'.

      CREATE DATA LRF_DATA TYPE TABLE OF LTS_VALUE_LGORT.
      ASSIGN LRF_DATA->* TO <LT_DATA>.

      _L_SET_VAL : 'KYFLD' 'C' '8'  TEXT-F04          '0',
                   'DESCR' 'C' '30' TEXT-C02     '8'.

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
*& Form evt_grid_f4_1000
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
      _G_SET_VALUE : CV_TITLE          TEXT-F03.

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
      _G_SET_VALUE : CV_TITLE          TEXT-F04.

  ENDCASE.
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

    MODIFY SCREEN.
  ENDLOOP.

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
*& Form CHECK_INPUT_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM CHECK_INPUT_DATA .

  IF S_DATE-LOW IS INITIAL OR S_DATE-HIGH IS INITIAL.
    MESSAGE S000 DISPLAY LIKE 'E' WITH TEXT-M02.  "기간은 필수 입력입니다.
    LEAVE LIST-PROCESSING.
  ENDIF.

  DATA: LV_TO_DATE   TYPE SY-DATLO.

  CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
    EXPORTING
      DATE      = S_DATE-LOW
      DAYS      = '00'
      MONTHS    = '03'
      SIGNUM    = '+'
      YEARS     = '00'
    IMPORTING
      CALC_DATE = LV_TO_DATE.

  LV_TO_DATE = LV_TO_DATE - 1.

  IF S_DATE-HIGH > LV_TO_DATE.
    MESSAGE S000 DISPLAY LIKE 'E' WITH TEXT-M04.  "수불부 조회시 기간을 3개월 이내로 입력 바랍니다.
    LEAVE LIST-PROCESSING.
  ENDIF.

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

  DATA: LS_DISP  TYPE TS_DISP.

  _G_INIT : GT_DATA, GT_DISP.

  SELECT FROM ZCCMMESTOCKREP( P_STARTDATE = @S_DATE-LOW, P_ENDDATE = @S_DATE-HIGH,
                              P_BUKRS = @P_BUKRS ) AS A
    FIELDS
    A~BUKRS, A~WERKS, A~NAME1, A~LGORT, A~LGOBE, A~LIFNR, A~NAME2, A~MATNR, A~MAKTX, A~MATKL, A~CHARG, A~BWTAR, A~INITSTOCK, A~INITSTOCK_VAL, A~INITQISTOCK, A~INITQISTOCK_VAL,
    A~INITBLSTOCK, A~INITBLSTOCK_VAL, A~TRSTOCKSLQTY, A~TRSTOCKSLVALUE, A~TRSTOCKPLQTY, A~TRSTOCKPLVALUE, A~SITQTY, A~SITVALUE,
    A~GR_PUR_QTY_UN, A~GR_PUR_QTY_QI, A~GR_PUR_QTY_BL,
    A~GR_PRD_QTY_UN, A~GR_PRD_QTY_QI, A~GR_PRD_QTY_BL,
    A~GR_DSC_QTY_UN, A~GR_DSC_QTY_QI, A~GR_DSC_QTY_BL,
    A~GR_STO_QTY_UN, A~GR_STO_QTY_QI, A~GR_STO_QTY_BL, A~GR_STO_QTY_ST,
    A~GR_SL_QTY_UN, A~GR_SL_QTY_QI, A~GR_SL_QTY_BL,
    A~GR_ETC_QTY_UN, A~GR_ETC_QTY_QI, A~GR_ETC_QTY_BL,
    A~GR_TR_QTY_UN, A~GR_TR_QTY_QI, A~GR_TR_QTY_BL,
    A~GR_SC_QTY_UN, A~GR_SC_QTY_QI, A~GR_SC_QTY_BL,
    A~GR_INV_QTY_UN, A~GR_INV_QTY_QI, A~GR_INV_QTY_BL,
    A~GI_RETS_QTY_UN, A~GI_RETS_QTY_QI, A~GI_RETS_QTY_BL,
    A~SIT_QTY_UN, A~SIT_QTY_QI, A~SIT_QTY_BL,
    A~GI_PRD_QTY_UN, A~GI_PRD_QTY_QI, A~GI_PRD_QTY_BL,
    A~GI_SO_QTY_UN, A~GI_SO_QTY_QI, A~GI_SO_QTY_BL,
    A~GI_STO_QTY_UN, A~GI_STO_QTY_QI, A~GI_STO_QTY_BL, A~GI_STO_QTY_ST,
    A~GI_SL_QTY_UN, A~GI_SL_QTY_QI, A~GI_SL_QTY_BL,
    A~GI_ETC_QTY_UN, A~GI_ETC_QTY_QI, A~GI_ETC_QTY_BL,
    A~GI_TR_QTY_UN, A~GI_TR_QTY_QI, A~GI_TR_QTY_BL,
    A~GI_SC_QTY_UN, A~GI_SC_QTY_QI, A~GI_SC_QTY_BL,
    A~GI_RETM_QTY_UN, A~GI_RETM_QTY_QI, A~GI_RETM_QTY_BL,
    A~GI_INV_QTY_UN, A~GI_INV_QTY_QI, A~GI_INV_QTY_BL, A~MEINS,
    A~GR_PUR_VAL_UN, A~GR_PUR_VAL_QI, A~GR_PUR_VAL_BL,
    A~GR_PRD_VAL_UN, A~GR_PRD_VAL_QI, A~GR_PRD_VAL_BL,
    A~GR_DSC_VAL_UN, A~GR_DSC_VAL_QI, A~GR_DSC_VAL_BL,
    A~GR_STO_VAL_UN, A~GR_STO_VAL_QI, A~GR_STO_VAL_BL, A~GR_STO_VAL_ST,
    A~GR_SL_VAL_UN, A~GR_SL_VAL_QI, A~GR_SL_VAL_BL,
    A~GR_ETC_VAL_UN, A~GR_ETC_VAL_QI, A~GR_ETC_VAL_BL,
    A~GR_TR_VAL_UN, A~GR_TR_VAL_QI, A~GR_TR_VAL_BL,
    A~GR_SC_VAL_UN, A~GR_SC_VAL_QI, A~GR_SC_VAL_BL,
    A~GR_INV_VAL_UN, A~GR_INV_VAL_QI, A~GR_INV_VAL_BL,
    A~GI_RETS_VAL_UN, A~GI_RETS_VAL_QI, A~GI_RETS_VAL_BL,
    A~SIT_VAL_UN, A~SIT_VAL_QI, A~SIT_VAL_BL,
    A~GI_PRD_VAL_UN, A~GI_PRD_VAL_QI, A~GI_PRD_VAL_BL,
    A~GI_SO_VAL_UN, A~GI_SO_VAL_QI, A~GI_SO_VAL_BL,
    A~GI_STO_VAL_UN, A~GI_STO_VAL_QI, A~GI_STO_VAL_BL, A~GI_STO_VAL_ST,
    A~GI_SL_VAL_UN, A~GI_SL_VAL_QI, A~GI_SL_VAL_BL,
    A~GI_ETC_VAL_UN, A~GI_ETC_VAL_QI, A~GI_ETC_VAL_BL,
    A~GI_TR_VAL_UN, A~GI_TR_VAL_QI, A~GI_TR_VAL_BL,
    A~GI_SC_VAL_UN, A~GI_SC_VAL_QI, A~GI_SC_VAL_BL,
    A~GI_RETM_VAL_UN, A~GI_RETM_VAL_QI, A~GI_RETM_VAL_BL,
    A~GI_INV_VAL_UN, A~GI_INV_VAL_QI, A~GI_INV_VAL_BL,
    A~WAERS, A~LASTQTY_SIT,
    A~LASTQTY_UN, A~LASTQTY_QI, A~LASTQTY_BL,
    A~LASTQTY_GRUN, A~LASTQTY_GRQI, A~LASTQTY_GRBL,
    A~LASTQTY_GIUN, A~LASTQTY_GIQI, A~LASTQTY_GIBL
    WHERE A~WERKS IN @S_WERKS
      AND A~LGORT IN @S_LGORT
      AND A~LIFNR IN @S_LIFNR
      AND A~MATNR IN @S_MATNR
      AND A~CHARG IN @S_CHARG
      AND MATKL IN @S_MATKL

    APPENDING CORRESPONDING FIELDS OF TABLE @GT_DATA.

*-자재그룹 내역 추가

  DATA(LT_DATA) = GT_DATA[].
  SORT LT_DATA BY MATKL.
  DELETE ADJACENT DUPLICATES FROM LT_DATA COMPARING MATKL.

  IF LT_DATA[] IS NOT INITIAL.

    SELECT FROM T023T
      FIELDS MATKL, WGBEZ, WGBEZ60
      FOR ALL ENTRIES IN @LT_DATA
      WHERE MATKL = @LT_DATA-MATKL
      AND SPRAS = @SY-LANGU
      INTO TABLE @DATA(LT_T023T).

  ENDIF.

  LOOP AT GT_DATA INTO DATA(LS_DATA).

    MOVE-CORRESPONDING LS_DATA TO LS_DISP.

    READ TABLE LT_T023T INTO DATA(LS_T023T) WITH KEY MATKL = LS_DISP-MATKL.
    IF SY-SUBRC = 0.
      LS_DISP-WGBEZ = LS_T023T-WGBEZ.
    ENDIF.

    APPEND LS_DISP TO GT_DISP. CLEAR LS_DISP.

  ENDLOOP.

  SORT GT_DISP BY BUKRS WERKS LGORT.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form CHECK_EXIT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM CHECK_EXIT .
*  DATA:LV_ANS.

*--------------------------------
* 화면 OFF전 변경 데이타 확인
*--------------------------------
*  PERFORM CHECK_CHANGED_DATA USING 'E' CHANGING LV_ANS.

*  CHECK LV_ANS IS INITIAL.

  CASE GV_OK_CODE.
    WHEN 'EXIT'.
      LEAVE PROGRAM.
    WHEN OTHERS.
      LEAVE TO SCREEN 0.
  ENDCASE.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form REFRESH_DATA_110
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM REFRESH_DATA_110 .

  PERFORM GET_DATA.

  GRF_GRID->REFRESH_GRID_DISPLAY( ).
ENDFORM.
