*&---------------------------------------------------------------------*
*& Include          ZRMM2020F01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form INITIALIZATION
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM INITIALIZATION.

*- 조회구간
  _G_INIT: GT_COMM_CFG_A.
  ZCL_MM_COMMON=>COMMON_CONFIG(  EXPORTING IS_COMMON =  VALUE #( M = 'B1' D = 'B0003' S = '00002' )
                                 IMPORTING ET_OUTTAB = DATA(LT_CONFIG) ).
  GT_COMM_CFG_A[] = VALUE #( BASE GT_COMM_CFG_A FOR LS_VALUE IN LT_CONFIG
                            WHERE ( FIELD1 = SY-CPROG )
                                  ( CORRESPONDING #( LS_VALUE ) ) ).

  SORT GT_COMM_CFG_A BY FIELD5.

*-
  LOOP AT SCREEN.
    CASE SCREEN-NAME.
      WHEN 'S_WERKS-LOW' OR 'S_WERKS-HIGH' OR 'P_PLSCN'.
        SCREEN-REQUIRED = 2.
    ENDCASE.
    MODIFY SCREEN.
  ENDLOOP.

*-
  DATA LV_FUNCTXT TYPE SMP_DYNTXT.
  LV_FUNCTXT-ICON_ID   = ICON_CALCULATION. "Tool Bar에 쓸 icon종류
  LV_FUNCTXT-QUICKINFO = TEXT-M01.      "풍선도움말 text
  LV_FUNCTXT-ICON_TEXT = TEXT-M01.      "icon의 text
  SSCRFIELDS-FUNCTXT_01 = LV_FUNCTXT.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form create_obj
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM CREATE_OBJ .

  CHECK 1 <> 1.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form check_authority
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM CHECK_AUTHORITY.

  DATA: LV_WERKS TYPE WERKS_D.

  CHECK S_WERKS[] IS NOT INITIAL.
*-
  SELECT FROM T001W FIELDS WERKS WHERE WERKS IN @S_WERKS INTO TABLE @DATA(LT_WERKS).

  LOOP AT LT_WERKS INTO LV_WERKS.
*--
    AUTHORITY-CHECK OBJECT 'M_MTDI_ORG' FOR USER SY-UNAME
              ID 'MDAKT' FIELD 'A'
              ID 'WERKS' FIELD LV_WERKS.

    IF SY-SUBRC <> 0.
      "플랜트 &1 에 대한 실행 권한이 없습니다.
      MESSAGE S125(ZMM01) WITH LV_WERKS DISPLAY LIKE 'E'. LEAVE LIST-PROCESSING.
    ENDIF.

  ENDLOOP.

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

  DATA: LV_PRG.
  CLEAR LV_PRG.

  _G_INIT: GT_MAIN, GT_DISP, GT_BERID.

*-
  PERFORM CHECK_IMPUT_DATA CHANGING LV_PRG.

*- Common Code
  PERFORM GET_COMMON_CONFIG CHANGING LV_PRG.

*-
  CHECK LV_PRG IS NOT INITIAL.

*- Variable Where Cause
  PERFORM SET_VAR_WHERE.

*- GET MAIN DATA
  PERFORM GET_MAIN_DATA.

*- SET BASE_DATA
  PERFORM SET_BASE_DATA.

*- GET MRP DATA
  PERFORM GET_MRP_DATA.

*-- GET 외주업체재고
  PERFORM GET_STOCK_REALTIME_DATA.  "U2

*- Setting Display date
  PERFORM SET_DAY.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CHECK_IMPUT_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- LV_PRG
*&---------------------------------------------------------------------*
FORM CHECK_IMPUT_DATA CHANGING EV_PRG.

  EV_PRG = 'X'.

*- 필수값 체크
  IF S_WERKS[] IS INITIAL.
    MESSAGE S000(ZMM01) WITH TEXT-M08 DISPLAY LIKE 'E'. CLEAR EV_PRG.
    LEAVE LIST-PROCESSING.
  ENDIF.
  IF ( P_FORMAT = 'D' AND S_DATE[]  IS INITIAL ) OR
     ( P_FORMAT = 'W' AND S_SWEEK[] IS INITIAL ) OR
     ( P_FORMAT = 'M' AND S_SPMON[] IS INITIAL ).
    MESSAGE S000(ZMM01) WITH TEXT-M09 DISPLAY LIKE 'E'. CLEAR EV_PRG.
    LEAVE LIST-PROCESSING.
  ENDIF.

*- MRP 영역
  IF S_BERID[] IS NOT INITIAL.
    DATA(LT_BERID) = S_BERID[].
    SORT LT_BERID BY LOW. DELETE ADJACENT DUPLICATES FROM LT_BERID COMPARING LOW.
    IF LT_BERID[] IS NOT INITIAL.
      SELECT FROM MDLV FIELDS BERID FOR ALL ENTRIES IN @LT_BERID
             WHERE BERID = @LT_BERID-LOW AND BERTY = @GC_BERTY_02 INTO TABLE @DATA(LT_MDLV).
    ENDIF.
    FREE LT_BERID.

    DESCRIBE TABLE S_BERID LINES DATA(LV_BCNT).
    DESCRIBE TABLE LT_MDLV LINES DATA(LV_ACNT).

    IF LV_BCNT NE LV_ACNT.
      "MRP 영역은 창고 레벨만 가능합니다. 입력값을 점검하세요!
      MESSAGE S000(ZMM01) WITH TEXT-M04 DISPLAY LIKE 'E'. CLEAR EV_PRG.
      LEAVE LIST-PROCESSING.
    ENDIF.
  ENDIF.

*-
  DATA: LV_CFDATE TYPE SY-DATLO,
        LV_DATE   TYPE SY-DATLO.

  CASE P_FORMAT.
    WHEN 'D'.
      LV_DATE  = S_SPMON-LOW.
      PERFORM F_RP_CALC_DATE_IN_INTERVAL:  USING LV_DATE '00' '6' '00' '+'
                                        CHANGING LV_CFDATE.
      IF S_SPMON-HIGH > LV_CFDATE.
        "조회구간을 최장 6개월을 초과할 수 없습니다.
        MESSAGE S000(ZMM01) WITH TEXT-M12 DISPLAY LIKE 'E'. CLEAR EV_PRG.
        LEAVE LIST-PROCESSING.
      ENDIF.
    WHEN 'W'.
      DATA: LV_FIRST_DAY TYPE DAT01,
            LV_LAST_DAY  TYPE DAT01,
            LV_CWEEK     LIKE SCAL-WEEK.

      PERFORM F_WEEK_GET_FIRST_DAY USING S_SWEEK-LOW  CHANGING LV_FIRST_DAY.
      PERFORM F_WEEK_GET_FIRST_DAY USING S_SWEEK-HIGH CHANGING LV_LAST_DAY.

      IF LV_FIRST_DAY IS INITIAL OR LV_LAST_DAY IS INITIAL.
        "존재하지 않는 주차입니다.
        MESSAGE S000(ZMM01) WITH TEXT-M13 DISPLAY LIKE 'E'. CLEAR EV_PRG.
        LEAVE LIST-PROCESSING.
      ENDIF.

      LV_DATE  = LV_FIRST_DAY.
      PERFORM F_RP_CALC_DATE_IN_INTERVAL:  USING LV_DATE '00' '6' '00' '+'
                                        CHANGING LV_CFDATE.

      CALL FUNCTION 'DATE_GET_WEEK'
        EXPORTING
          DATE = LV_CFDATE
        IMPORTING
          WEEK = LV_CWEEK.

      IF S_SWEEK-HIGH > LV_CWEEK.
        "조회구간을 최장 6개월을 초과할 수 없습니다.
        MESSAGE S000(ZMM01) WITH TEXT-M12 DISPLAY LIKE 'E'. CLEAR EV_PRG.
        LEAVE LIST-PROCESSING.
      ENDIF.

    WHEN 'M'.
      LV_DATE  = S_SPMON-LOW && '01'.
      PERFORM F_RP_CALC_DATE_IN_INTERVAL:  USING LV_DATE '00' '6' '00' '+'
                                        CHANGING LV_CFDATE.
      IF S_SPMON-HIGH > LV_CFDATE(06).
        "조회구간을 최장 6개월을 초과할 수 없습니다.
        MESSAGE S000(ZMM01) WITH TEXT-M12 DISPLAY LIKE 'E'. CLEAR EV_PRG.
        LEAVE LIST-PROCESSING.
      ENDIF.
  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_COMMON_CONFIG
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- LV_PRG
*&---------------------------------------------------------------------*
FORM GET_COMMON_CONFIG CHANGING EV_PRG.

  EV_PRG = 'X'.

  _G_INIT : GT_COMM_CFG_C, GT_COMM_CFG_D, GT_SEL_LIST, GT_STB_MATNR, GT_COMM_CFG_B312.

*-
  ZCL_MM_COMMON=>COMMON_CONFIG(  EXPORTING IS_COMMON =  VALUE #( M = 'B1' D = 'B0001' S = 'MRP04' )
                                 IMPORTING ET_OUTTAB = DATA(LT_CONFIG) ).
  GT_COMM_CFG_C[] = CORRESPONDING #( LT_CONFIG[] ).

  IF S_PROD[] IS NOT INITIAL.
*- BOM 전개방식
    ZCL_MM_COMMON=>COMMON_CONFIG(  EXPORTING IS_COMMON =  VALUE #( M = 'B1' D = 'B0003' S = '00013' )
                                   IT_WHERE = VALUE #( ( FIELD = 1 VALUE = SY-CPROG ) )
                                   IMPORTING ET_OUTTAB = LT_CONFIG ).
    GT_COMM_CFG_D[] = VALUE #( BASE GT_COMM_CFG_D FOR LS_VALUE IN LT_CONFIG
                                    ( CORRESPONDING #( LS_VALUE ) ) ).

*- "생산계획점검여부
    ZCL_MM_COMMON=>COMMON_CONFIG(  EXPORTING IS_COMMON =  VALUE #( M = 'B1' D = 'B0003' S = '00012' )
                                   IT_WHERE = VALUE #( ( FIELD = 1 VALUE = SY-CPROG ) )
                                   IMPORTING ET_OUTTAB = LT_CONFIG ).
    GT_COMM_CFG_B312[] = CORRESPONDING #( LT_CONFIG[] ).

*-
    IF GT_COMM_CFG_D[] IS INITIAL.
      "Config : [ B0003 / 00013 ]  제품 BOM 전개 레벨 정의가 없습니다.
      MESSAGE S000(ZMM01) WITH TEXT-M10 DISPLAY LIKE 'E'. CLEAR EV_PRG.
      LEAVE LIST-PROCESSING.
    ENDIF.
  ENDIF.

*-
  IF GT_COMM_CFG_C[] IS INITIAL.
    "Config : [ B0001 / MRP05 ]  LTP List 구성 조건필드가 없습니다.
    MESSAGE S000(ZMM01) WITH TEXT-M05 DISPLAY LIKE 'E'. CLEAR EV_PRG.
    LEAVE LIST-PROCESSING.
  ENDIF.

*- "MRP Data select List Field
  PERFORM SET_SEL_FIELD_LTP_TABLE CHANGING EV_PRG.

*-- 제품코드 입력시 데이타 여부
  IF S_PROD[] IS NOT INITIAL.
    PERFORM GET_BOM_IDNRK_DATA.
    IF GT_STB[] IS INITIAL.
      "입력된 제품의 하위자재가 존재하지 않습니다.
      MESSAGE S000(ZMM01) WITH TEXT-M11 DISPLAY LIKE 'E'. CLEAR EV_PRG.
      LEAVE LIST-PROCESSING.
    ENDIF.
  ENDIF.
*--
  IF GT_STB[] IS NOT INITIAL.
    GT_STB_MATNR[] = VALUE #( BASE GT_STB_MATNR[] FOR LS_STB IN GT_STB
                            ( MATNR = LS_STB-MATNR WERKS = LS_STB-WERKS ) ).
  ENDIF.

  SORT GT_STB_MATNR BY MATNR WERKS. DELETE ADJACENT DUPLICATES FROM GT_STB_MATNR COMPARING MATNR WERKS.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_WEEK_GET_FIRST_DAY
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> S_SWEEK_LOW
*&      <-- LV_FIRST_DAY
*&---------------------------------------------------------------------*
FORM F_WEEK_GET_FIRST_DAY USING IV_WEEK
               CHANGING EV_DAY_FROM.

  CLEAR : EV_DAY_FROM.

  CALL FUNCTION 'WEEK_GET_FIRST_DAY'
    EXPORTING
      WEEK         = IV_WEEK
    IMPORTING
      DATE         = EV_DAY_FROM
    EXCEPTIONS
      WEEK_INVALID = 1.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_RP_CALC_DATE_IN_INTERVAL
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LV_DATE
*&      --> P_
*&      --> P_
*&      --> P_
*&      --> P_
*&      <-- LV_CFDATE
*&---------------------------------------------------------------------*
FORM F_RP_CALC_DATE_IN_INTERVAL USING IV_DATE IV_DAY IV_MONTH IV_YEAR IV_SIGN
                             CHANGING EV_DATE.

  CLEAR EV_DATE.

*-
  CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
    EXPORTING
      DATE      = IV_DATE
      DAYS      = IV_DAY
      MONTHS    = IV_MONTH
      SIGNUM    = IV_SIGN
      YEARS     = IV_YEAR
    IMPORTING
      CALC_DATE = EV_DATE.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_VAR_WHERE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM SET_VAR_WHERE .

  _G_INIT: GT_BERID_R.

  IF S_BERID[] IS INITIAL.
    GT_BERID_R[] = VALUE #( BASE GT_BERID_R[] FOR LS_WERKS IN S_WERKS
                                   ( SIGN = 'I' OPTION = 'EQ' LOW = LS_WERKS-LOW ) ).
  ELSE.
    GT_BERID_R[] = S_BERID[].
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_MAIN_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM GET_MAIN_DATA.

  IF S_PROD[] IS INITIAL.

    SELECT FROM PPH_DBVM AS A INNER JOIN MARA AS B
                                  ON A~MATNR = B~MATNR
                          INNER JOIN V_MARC_MD AS C
                                  ON C~MATNR = A~MATNR AND
                                     C~WERKS = A~WERKS
                          INNER JOIN T001W     AS D
                                  ON D~WERKS = C~WERKS
                          INNER JOIN MAKT      AS E
                                  ON E~MATNR = B~MATNR AND
                                     E~SPRAS = @SY-LANGU
                          LEFT OUTER JOIN  T141T AS F
                                       ON F~MMSTA = C~MMSTA
                                      AND F~SPRAS = @SY-LANGU


           FIELDS
            B~MATNR, B~MEINS,
            C~WERKS, D~NAME1, E~MAKTX,
            A~BERID, C~MMSTA, F~MTSTB

           WHERE "A~MATNR IN @GT_MATNR
                 A~MATNR IN @S_MATNR
             AND A~WERKS IN @S_WERKS
             AND A~BERID IN @GT_BERID_R
             AND A~PLSCN = @GC_000
**           AND A~NODISP = ''
             AND B~MATKL IN @S_MATKL
             AND B~LVORM EQ @SPACE
             AND C~DISPO IN @S_DISPO
             AND C~EKGRP IN @S_EKGRP
             AND C~MMSTA IN @S_MMSTA
             AND C~LVORM EQ @SPACE
    INTO CORRESPONDING FIELDS OF TABLE @GT_MAIN.

  ELSE.
    SORT GT_STB_MATNR BY MATNR WERKS.
    DELETE ADJACENT DUPLICATES FROM GT_STB_MATNR COMPARING MATNR WERKS.
    IF GT_STB_MATNR[] IS NOT INITIAL.
      SELECT FROM PPH_DBVM AS A INNER JOIN MARA AS B
                                    ON A~MATNR = B~MATNR
                            INNER JOIN V_MARC_MD AS C
                                    ON C~MATNR = A~MATNR AND
                                       C~WERKS = A~WERKS
                            INNER JOIN T001W     AS D
                                    ON D~WERKS = C~WERKS
                            INNER JOIN MAKT      AS E
                                    ON E~MATNR = B~MATNR AND
                                       E~SPRAS = @SY-LANGU
                            LEFT OUTER JOIN  T141T AS F
                                         ON F~MMSTA = C~MMSTA
                                        AND F~SPRAS = @SY-LANGU


             FIELDS
              B~MATNR, B~MEINS,
              C~WERKS, D~NAME1, E~MAKTX,
              A~BERID, C~MMSTA, F~MTSTB

             FOR ALL ENTRIES IN @GT_STB_MATNR
             WHERE A~MATNR EQ @GT_STB_MATNR-MATNR
               AND A~WERKS EQ @GT_STB_MATNR-WERKS
               AND A~MATNR IN @S_MATNR
               AND A~WERKS IN @S_WERKS
               AND A~BERID IN @GT_BERID_R
               AND A~PLSCN = @GC_000
**           AND A~NODISP = ''
               AND B~MATKL IN @S_MATKL
               AND B~LVORM EQ @SPACE
               AND C~DISPO IN @S_DISPO
               AND C~EKGRP IN @S_EKGRP
               AND C~MMSTA IN @S_MMSTA
               AND C~LVORM EQ @SPACE
      INTO CORRESPONDING FIELDS OF TABLE @GT_MAIN.

    ENDIF.
  ENDIF.

*-
  DATA(LT_MAIN) = GT_MAIN[].
  SORT LT_MAIN BY BERID. DELETE ADJACENT DUPLICATES FROM LT_MAIN COMPARING BERID.
  IF LT_MAIN[] IS NOT INITIAL.
    SELECT FROM MDLV
           FIELDS
           BERID, BERTX
           FOR ALL ENTRIES IN @LT_MAIN
           WHERE BERID = @LT_MAIN-BERID
    INTO CORRESPONDING FIELDS OF TABLE @GT_BERID.
  ENDIF.
  FREE LT_MAIN.

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

  _G_INIT: GT_BASE_DAT.

  GT_BASE_DAT[] = CORRESPONDING #( GT_MAIN DISCARDING DUPLICATES
                                   MAPPING WERKS = WERKS MATNR = MATNR ).

ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_MRP_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM GET_MRP_DATA.

**  A   기초재고 ( 가용재고 포함여부 )
**  B   소요량
**  C   확정입고예정1
**  D   확정입고예정2
**  E   예상기말재고

  _G_INIT: GT_LTP_ORG, GT_LTP_DAT.

*- "최신데이타
  TRY.
      IF GT_BASE_DAT[] IS NOT INITIAL.
        SELECT (GT_SEL_LIST)
          INTO CORRESPONDING FIELDS OF TABLE GT_LTP_ORG
          FROM ZTMM20010 FOR ALL ENTRIES IN GT_BASE_DAT
          WHERE MATNR = GT_BASE_DAT-MATNR AND WERKS = GT_BASE_DAT-WERKS
                                          AND BERID IN GT_BERID_R.
      ENDIF.
*--
    CATCH CX_SY_DYNAMIC_OSQL_ERROR.
  ENDTRY.

*-
  DATA: LV_CLAUSE_1 TYPE STRING,
        LV_CLAUSE_2 TYPE STRING,
        LV_CLAUSE_3 TYPE STRING,
        LV_CLAUSE_4 TYPE STRING.

  SORT GT_COMM_CFG_C BY FIELD1.
  PERFORM : GET_MAKE_CLAUSE USING 'A' CHANGING LV_CLAUSE_1, "가용재고
            GET_MAKE_CLAUSE USING 'B' CHANGING LV_CLAUSE_2. "소요량

  IF P_CHK2 = 'X'. "기 발주 잔량
    PERFORM : GET_MAKE_CLAUSE USING 'C' CHANGING LV_CLAUSE_3. "확정입고예정1
  ENDIF.
  IF P_CHK3 = 'X'. "확정 구매요청
    PERFORM : GET_MAKE_CLAUSE USING 'D' CHANGING LV_CLAUSE_4. "확정입고예정2
  ENDIF.

*-
  PERFORM GET_OPT_LTP_DATA USING : 'A' LV_CLAUSE_1,
                                   'B' LV_CLAUSE_2,
                                   'C' LV_CLAUSE_3,
                                   'C' LV_CLAUSE_4.

*-
  SORT GT_LTP_DAT BY OPT MATNR WERKS.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_MAKE_CLAUSE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> P_
*&      <-- LV_CLAUSE_1
*&---------------------------------------------------------------------*
FORM GET_MAKE_CLAUSE USING IV_FIELD1
                      CHANGING EV_CLAUSE.

  DATA: LV_CLAUSE    TYPE STRING,
        LV_VAL_CNT   TYPE SY-TABIX,
        LV_CNT       TYPE SY-TABIX,
        LV_OP_AND(5) VALUE ' AND ',
        LV_OP_OR(4)  VALUE ' OR ',
        LV_OP_EQ(2)  VALUE 'EQ'.

*-
  DEFINE      _L_CLAUSE.

    IF &1 IS NOT INITIAL.
      IF     LV_VAL_CNT = 1.
        LV_CLAUSE = |{ LV_CLAUSE }| && |{ &1 }|.
      ELSE.
        LV_CLAUSE = |{ LV_CLAUSE }| && |{ LV_OP_OR } { LS_CFG-FIELD2 } { LV_OP_EQ } | && |{ &1 }|.
      ENDIF.
      ADD 1 TO LV_VAL_CNT.
    ENDIF.
  END-OF-DEFINITION.

*-
  CLEAR LV_CNT.
  LOOP AT GT_COMM_CFG_C INTO DATA(LS_CFG) WHERE FIELD1 = IV_FIELD1.
    ADD 1 TO LV_CNT.
    IF LV_CNT = 1.
      CLEAR : LV_CLAUSE, LV_VAL_CNT. ADD 1 TO LV_VAL_CNT.
    ENDIF.

    IF LS_CFG-FIELD5 IS INITIAL. "몇건인지..
      IF LV_CNT = 1.
        LV_CLAUSE   = |{ LS_CFG-FIELD2 } { LS_CFG-FIELD3 } | && |{ LS_CFG-FIELD4 }|.
      ELSE.
        LV_CLAUSE   = |{ LV_CLAUSE }| && |{ LV_OP_AND } { LS_CFG-FIELD2 } { LS_CFG-FIELD3 } | && |{ LS_CFG-FIELD4 }|.
      ENDIF.
    ELSE.
      IF LV_CNT = 1.
        LV_CLAUSE   = |{ LS_CFG-FIELD2 } { LV_OP_EQ } |.
      ELSE.
        LV_CLAUSE   = |{ LV_CLAUSE }| &&  |{ LV_OP_AND } { LS_CFG-FIELD2 } { LV_OP_EQ } |.
      ENDIF.

      _L_CLAUSE: LS_CFG-FIELD4, LS_CFG-FIELD5,  LS_CFG-FIELD6, LS_CFG-FIELD7, LS_CFG-FIELD8,
                 LS_CFG-FIELD9, LS_CFG-FIELD10.
    ENDIF.

  ENDLOOP.

*-
  EV_CLAUSE = LV_CLAUSE.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_OPT_LTP_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> P_
*&      --> LV_CLAUSE_1
*&---------------------------------------------------------------------*
FORM GET_OPT_LTP_DATA USING IV_OPT IV_CLAUSE.

  DATA: LT_LTP_DAT TYPE STANDARD TABLE OF TY_LTP_DAT,
        LS_LTP_DAT LIKE LINE OF LT_LTP_DAT.

  _G_INIT: LT_LTP_DAT. CLEAR: LS_LTP_DAT.

  CHECK IV_CLAUSE IS NOT INITIAL.

*-
  LOOP AT GT_LTP_ORG INTO DATA(LS_LTP_ORG) WHERE (IV_CLAUSE).
    MOVE-CORRESPONDING LS_LTP_ORG TO LS_LTP_DAT.
    LS_LTP_DAT-OPT = IV_OPT.
    APPEND LS_LTP_DAT TO LT_LTP_DAT.
  ENDLOOP.

  CHECK LT_LTP_DAT[] IS NOT INITIAL.
*-
  APPEND LINES OF LT_LTP_DAT TO GT_LTP_DAT[].

ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_STOCK_REALTIME_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM GET_STOCK_REALTIME_DATA .

  _G_INIT: GT_SUB_STK.

*-
  IF GT_BASE_DAT[] IS NOT INITIAL.
    SELECT FROM @GT_BASE_DAT AS A INNER JOIN ZCCMM_STOCK_REALTIME AS B
                                          ON B~MATERIAL = A~MATNR
                                         AND B~PLANT    = A~WERKS
                                         AND B~BP       NE @SPACE
           FIELDS
           MATERIAL, PLANT,
           ( SUM( AVAILABLESTOCK ) + SUM( QISTOCK ) ) AS STOCK
           GROUP BY MATERIAL, PLANT
     INTO CORRESPONDING FIELDS OF TABLE @GT_SUB_STK.
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
FORM SET_DAY.

  DATA: LS_DAY       TYPE TY_DAY,
        LV_SDATE     TYPE DATUM,
        LV_CDATE     TYPE DATUM,
        LV_CNT       TYPE SY-TABIX,
        LV_DAY       TYPE NUMC2,
        LV_CMONTH    TYPE NUMC2,
        LV_CDAY      TYPE NUMC3,
        LV_TIME      TYPE MARA-MHDHB,
        LV_DATE_TO   TYPE DATUM,
        LV_DAYS      LIKE VTBBEWE-ATAGE,
        LV_WEEK      LIKE VTBBEWE-ATAGE,
        LV_CWEEK     LIKE SCAL-WEEK,
        LV_MONTH     LIKE VTBBEWE-ATAGE,
        LV_WEEK_FROM TYPE DATUM,
        LV_WEEK_TO   TYPE DATUM.

*-
  _G_INIT: GT_DAY.
  CLEAR: GV_FIRST_DAY, GV_LAST_DAY, GV_DAY_DO, LV_MONTH, LV_DAYS, LV_WEEK,
         LV_CWEEK, LV_WEEK_FROM, LV_WEEK_TO, LV_TIME.

*-
  CASE P_FORMAT.
    WHEN 'D'.
      GV_FIRST_DAY      = S_DATE-LOW.
      LV_DATE_TO        = S_DATE-HIGH.
    WHEN 'W'.
      PERFORM F_WEEK_GET_FIRST_DAY USING S_SWEEK-LOW  CHANGING GV_FIRST_DAY.
      PERFORM F_WEEK_GET_FIRST_DAY USING S_SWEEK-HIGH CHANGING GV_LAST_DAY.
    WHEN 'M'.
      GV_FIRST_DAY      = S_SPMON-LOW  && SY-DATLO+06(02).
      LV_DATE_TO        = S_SPMON-HIGH && SY-DATLO+06(02).
  ENDCASE.

*-
  CASE P_FORMAT.
    WHEN 'D' OR 'M'.
      CALL FUNCTION 'FIMA_DAYS_AND_MONTHS_AND_YEARS'
        EXPORTING
          I_DATE_FROM = GV_FIRST_DAY
          I_DATE_TO   = LV_DATE_TO
        IMPORTING
          E_DAYS      = LV_DAYS
          E_MONTHS    = LV_MONTH.
    WHEN 'W'.

  ENDCASE.

*-
  CASE P_FORMAT.
    WHEN 'D'.
      GV_DAY_DO = LV_DAYS + 1.
    WHEN 'M'.
      GV_DAY_DO = LV_MONTH + 1.
  ENDCASE.

*-
  IF P_FORMAT = 'D' OR P_FORMAT = 'M'.
    CLEAR: LV_CNT, LV_DAY, LV_CMONTH, LV_TIME.

    DO GV_DAY_DO TIMES.

      ADD 1 TO LV_CNT.

*-
      IF     LV_CNT = 1.
        LV_CDATE  = GV_FIRST_DAY.
      ELSE.
*-
        CASE P_FORMAT.
          WHEN 'D'.
            ADD 1 TO LV_TIME.
            PERFORM F_ADD_TIME_TO_DATE USING GV_FIRST_DAY LV_TIME '' CHANGING LV_CDATE.
          WHEN 'W'.
          WHEN 'M'.
            ADD 1 TO LV_CMONTH.
            PERFORM F_RP_CALC_DATE_IN_INTERVAL USING GV_FIRST_DAY LV_DAY LV_CMONTH 0 '+' CHANGING LV_CDATE.
        ENDCASE.


      ENDIF.

      ADD 1 TO LV_CDAY.

      LS_DAY-SEQ  = LV_CNT.
      LS_DAY-DATE = LV_CDATE.
      LS_DAY-DAY  = |{ TEXT-O01 }| && LV_CDAY.

      PERFORM CHANGE_HEADER_DATE USING P_FORMAT LV_CDATE CHANGING LS_DAY-DAY_TXT.

      APPEND LS_DAY TO GT_DAY.
    ENDDO.

*--
  ELSEIF P_FORMAT = 'W'.
    CLEAR: LV_CNT, LV_CWEEK, LV_CDAY, LV_CDATE.
*---
    LV_SDATE = GV_FIRST_DAY.
    DO.
      ADD 1 TO LV_CNT.

      IF     LV_CNT = 1.
        LV_CWEEK      = S_SWEEK-LOW.
      ELSE.

        IF LV_CDATE IS NOT INITIAL.
          LV_SDATE = LV_CDATE.
        ENDIF.
        PERFORM F_RP_CALC_DATE_IN_INTERVAL USING LV_SDATE 7 0 0 '+' CHANGING LV_CDATE.
        CALL FUNCTION 'DATE_GET_WEEK'
          EXPORTING
            DATE = LV_CDATE
          IMPORTING
            WEEK = LV_CWEEK.
      ENDIF.

      PERFORM F_WEEK_GET_FIRST_DAY USING LV_CWEEK  CHANGING LV_WEEK_FROM.

*---
      IF LV_WEEK_FROM IS NOT INITIAL.
        ADD 1 TO LV_CDAY.

        LS_DAY-SEQ  = LV_CNT.
        LS_DAY-DATE = LV_WEEK_FROM.
        LS_DAY-DAY  = |{ TEXT-O01 }| && LV_CDAY.

        PERFORM CHANGE_HEADER_DATE USING P_FORMAT LV_CWEEK CHANGING LS_DAY-DAY_TXT.

*--- 주차
        LS_DAY-WEEK = LV_CWEEK.
*---
        APPEND LS_DAY TO GT_DAY.
      ENDIF.
*---
      IF LV_WEEK_FROM = GV_LAST_DAY. EXIT. ENDIF.
    ENDDO.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_ADD_TIME_TO_DATE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> GV_FIRST_DAY
*&      --> LV_TIME
*&      --> P_
*&      <-- LV_CDATE
*&---------------------------------------------------------------------*
FORM F_ADD_TIME_TO_DATE USING IV_DATE
                                  IV_DAYS
                                  IV_FORMAT
                         CHANGING EV_CDATE TYPE DATUM.

  DATA: LV_TIME  TYPE MARA-MHDHB,
        LV_IPRKZ TYPE MARA-IPRKZ.

  LV_TIME   = IV_DAYS.
  LV_IPRKZ  = IV_FORMAT.

  CLEAR EV_CDATE.

  CALL FUNCTION 'ADD_TIME_TO_DATE'
    EXPORTING
      I_IDATE = IV_DATE
      I_TIME  = LV_TIME
      I_IPRKZ = LV_IPRKZ
    IMPORTING
      O_IDATE = EV_CDATE.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CHANGE_HEADER_DATE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> P_FORMAT
*&      --> LV_CDATE
*&      <-- LS_DAY_DAY_TXT
*&---------------------------------------------------------------------*
FORM CHANGE_HEADER_DATE USING IV_FORMAT IV_DATE CHANGING EV_CDATE.

  CLEAR EV_CDATE.

*-
  CHECK IV_DATE IS NOT INITIAL.

*-
  DATA: LV_NAME TYPE KURZT.
  IF IV_FORMAT EQ 'D'.
    PERFORM F_RH_GET_DATE_DAYNAME  USING SY-LANGU
                                         IV_DATE
                                CHANGING LV_NAME.
  ENDIF.

*-
  CASE IV_FORMAT.
    WHEN 'D'.
      EV_CDATE = |{ IV_DATE+04(02) }| && |.| && |{ IV_DATE+06(02) }| && |(| && LV_NAME(01) && |)|.
    WHEN 'W'.
      DATA: LV_CHAR(10).
      WRITE IV_DATE TO LV_CHAR. CONDENSE LV_CHAR.
      EV_CDATE = |W | && |{ LV_CHAR+04(02) }| && |/| && |{ LV_CHAR+(04) }|.
    WHEN 'M'.
      EV_CDATE = |M | && |{ IV_DATE+04(02) }| && |/| && |{ IV_DATE+(04) }|.
  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_RH_GET_DATE_DAYNAME
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> SY_LANGU
*&      --> IV_DATE
*&      <-- LV_NAME
*&---------------------------------------------------------------------*
FORM F_RH_GET_DATE_DAYNAME USING IV_LANGU
                                     IV_DATUM
                            CHANGING EV_NAME.

  CLEAR EV_NAME.

  DATA: LV_WRK_DAY     LIKE SCAL-INDICATOR.

  CALL FUNCTION 'DATE_COMPUTE_DAY'
    EXPORTING
      DATE = IV_DATUM
    IMPORTING
      DAY  = LV_WRK_DAY.

  SELECT SINGLE KURZT
    FROM T246
    INTO @DATA(LV_KURZT)
   WHERE SPRSL = @IV_LANGU
     AND WOTNR = @LV_WRK_DAY.

  IF SY-SUBRC = 0.
    EV_NAME = LV_KURZT.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_BOM_IDNRK_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM GET_BOM_IDNRK_DATA .

  DATA: LV_WERKS TYPE WERKS_D.

*-
  _G_INIT: GT_STB.

*- BOM 정전개
  SORT GT_COMM_CFG_D BY FIELD2.
  LOOP AT S_WERKS INTO DATA(LS_WERKS).

    LV_WERKS = LS_WERKS-LOW.
    READ TABLE GT_COMM_CFG_D INTO DATA(LS_CONFIG) WITH KEY FIELD2 = LV_WERKS.
    CHECK SY-SUBRC = 0.

    IF LS_CONFIG-FIELD4 = 'Y'. "Multi Level
      PERFORM GET_INPUTED_BOM_MULTI USING LV_WERKS LS_CONFIG-FIELD3.

    ELSE.
      PERFORM GET_INPUTED_BOM USING LV_WERKS LS_CONFIG-FIELD3.
    ENDIF.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_INPUTED_BOM_MULTI
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LV_WERKS
*&      --> LS_CONFIG_FIELD3
*&---------------------------------------------------------------------*
FORM GET_INPUTED_BOM_MULTI USING IV_WERKS TYPE WERKS_D
                                  IV_STPST.

  DATA: LV_WERKS TYPE WERKS_D,
        LV_STPST TYPE HISTU.

*-
  LV_WERKS = IV_WERKS.
  LV_STPST = IV_STPST.

*-

  DATA: LT_MATNR      TYPE STANDARD TABLE OF ZSMM00001 WITH HEADER LINE,
        LT_CMATNR     TYPE STANDARD TABLE OF ZSMM00001, " WITH HEADER LINE,
        LT_MATNR_MKAL TYPE STANDARD TABLE OF ZSMM00001,
        LT_CMATNR_VER TYPE STANDARD TABLE OF ZSMM00001 WITH HEADER LINE,
        LT_STB        TYPE STANDARD TABLE OF ZSMM_STPOX WITH HEADER LINE,
        LT_RETURN     TYPE STANDARD TABLE OF BAPIRET2 WITH HEADER LINE.

  _G_INIT: LT_MATNR, LT_CMATNR, LT_CMATNR_VER, LT_STB, LT_MATNR_MKAL.

  READ TABLE GT_COMM_CFG_B312 INTO DATA(LS_CFG) INDEX 1.

*-
  SELECT FROM V_MARC_MD AS A INNER JOIN MARA AS B
                                     ON B~MATNR = A~MATNR
                                    AND B~LVORM = @SPACE
          FIELDS A~MATNR, A~WERKS, 1 AS EMENG
          WHERE A~MATNR IN @S_PROD
            AND A~WERKS EQ @LV_WERKS
            AND A~LVORM = @SPACE
  INTO CORRESPONDING FIELDS OF TABLE @LT_CMATNR.

*-
  SORT LT_CMATNR BY MATNR WERKS.
  DELETE ADJACENT DUPLICATES FROM LT_CMATNR COMPARING MATNR WERKS.
  IF LT_CMATNR[] IS NOT INITIAL.
    SELECT FROM @LT_CMATNR AS A INNER JOIN MKAL AS B
                                   ON B~MATNR = A~MATNR
                                  AND B~WERKS = A~WERKS
                                  AND B~MKSP EQ @SPACE "잠금처리 제외
               FIELDS
               B~MATNR, B~WERKS, B~VERID, 1 AS EMENG
               WHERE ( B~ADATU <= @SY-DATLO AND B~BDATU >= @SY-DATLO )
       INTO CORRESPONDING FIELDS OF TABLE @LT_MATNR_MKAL.
  ENDIF.
  FREE LT_CMATNR.

  IF LS_CFG-FIELD4 = 'Y'.
    SORT LT_MATNR_MKAL BY MATNR WERKS VERID.
    DELETE ADJACENT DUPLICATES FROM LT_MATNR_MKAL COMPARING MATNR WERKS VERID.
    IF LT_MATNR_MKAL[] IS NOT INITIAL.
      SELECT FROM @LT_MATNR_MKAL AS A INNER JOIN PLAF AS B
                                             ON B~MATNR = A~MATNR
                                            AND B~PLWRK = A~WERKS
                                            AND B~VERID = A~VERID
                                            AND B~PLSCN = 0
                  FIELDS
                  DISTINCT B~MATNR, B~PLWRK AS WERKS, B~VERID, 1 AS EMENG
                  ORDER BY B~MATNR, B~PLWRK, B~VERID
      INTO CORRESPONDING FIELDS OF TABLE @LT_CMATNR_VER.
    ENDIF.
    FREE LT_MATNR_MKAL.

  ELSE.
    LT_CMATNR_VER[] = LT_MATNR_MKAL[].
  ENDIF.
  SORT LT_CMATNR_VER BY MATNR WERKS VERID.
  DELETE ADJACENT DUPLICATES FROM LT_CMATNR_VER COMPARING MATNR WERKS VERID.

*-
  call function 'ZFMM_BOM_EXPL_MAT'
    EXPORTING
      IV_DATUV  = SY-DATLO
      IV_MEHRS  = 'X'
      IV_STPST  = LV_STPST
    IMPORTING
      EV_RETURN = LT_RETURN
    TABLES
      IT_MATNR  = LT_CMATNR_VER
      IT_STB    = LT_STB.

*-
  IF LT_STB[] IS NOT INITIAL.
    GT_STB[] = VALUE #( BASE GT_STB[] FOR LS_VALUE IN LT_STB
                          ( MATNR = LS_VALUE-IDNRK WERKS = LV_WERKS ) ).
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_INPUTED_BOM
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LV_WERKS
*&      --> LS_CONFIG_FIELD3
*&---------------------------------------------------------------------*
FORM GET_INPUTED_BOM USING IV_WERKS TYPE WERKS_D
                            IV_BLOOP.

  DATA: LV_WERKS TYPE WERKS_D.

*-
  LV_WERKS = IV_WERKS.

*-
  DATA: LT_MATNR      TYPE STANDARD TABLE OF ZSMM00001 WITH HEADER LINE,
        LT_CMATNR     TYPE STANDARD TABLE OF ZSMM00001 WITH HEADER LINE,
        LT_MATNR_MKAL TYPE STANDARD TABLE OF ZSMM00001,
        LT_STB        TYPE STANDARD TABLE OF ZSMM_STPOX WITH HEADER LINE,
        LT_RETURN     TYPE STANDARD TABLE OF BAPIRET2 WITH HEADER LINE.

  DATA: LV_BOM_LOOP TYPE I.

  _G_INIT: LT_MATNR, LT_CMATNR, LT_STB, LT_MATNR_MKAL.

*-
  LV_BOM_LOOP = IV_BLOOP.
  IF LV_BOM_LOOP IS INITIAL. LV_BOM_LOOP = 1. ENDIF.

  READ TABLE GT_COMM_CFG_B312 INTO DATA(LS_CFG) INDEX 1.

*-
  SELECT FROM V_MARC_MD AS A INNER JOIN MARA AS B
                                     ON B~MATNR = A~MATNR
                                    AND B~LVORM = @SPACE
          FIELDS A~MATNR, A~WERKS, 1 AS EMENG
          WHERE A~MATNR IN @S_PROD
            AND A~WERKS EQ @LV_WERKS
            AND A~LVORM = @SPACE
  INTO TABLE @DATA(LT_MARC).

  SORT LT_MARC BY MATNR WERKS.
  DELETE ADJACENT DUPLICATES FROM LT_MARC COMPARING MATNR WERKS.
  IF LT_MARC[] IS NOT INITIAL.
    SELECT FROM @LT_MARC AS A INNER JOIN MKAL AS B
                                      ON B~MATNR = A~MATNR
                                     AND B~WERKS = A~WERKS
                                     AND B~VERID NE @SPACE
                                     AND B~MKSP  EQ @SPACE
           FIELDS
           B~MATNR, B~WERKS, B~VERID, B~STLAL, 1 AS EMENG
           WHERE B~WERKS  = @LV_WERKS
             AND B~BDATU >= @SY-DATLO
             AND B~ADATU <= @SY-DATLO
    INTO CORRESPONDING FIELDS OF TABLE @LT_MATNR_MKAL.
  ENDIF.
  FREE LT_MARC.

  IF LS_CFG-FIELD4 = 'Y'.
    SORT LT_MATNR_MKAL BY MATNR WERKS VERID.
    DELETE ADJACENT DUPLICATES FROM LT_MATNR_MKAL COMPARING MATNR WERKS VERID.
    IF LT_MATNR_MKAL[] IS NOT INITIAL.
      SELECT FROM @LT_MATNR_MKAL AS A INNER JOIN PLAF AS B
                                             ON B~MATNR = A~MATNR
                                            AND B~PLWRK = A~WERKS
                                            AND B~VERID = A~VERID
                                            AND B~PLSCN = 0
                  FIELDS
                  DISTINCT B~MATNR, B~PLWRK AS WERKS, B~VERID, 1 AS EMENG
                  ORDER BY B~MATNR, B~PLWRK, B~VERID
      INTO CORRESPONDING FIELDS OF TABLE @LT_CMATNR.
    ENDIF.
    FREE LT_MATNR_MKAL.

  ELSE.
    LT_CMATNR[] = LT_MATNR_MKAL[].
  ENDIF.
  SORT LT_CMATNR BY MATNR WERKS VERID.
  DELETE ADJACENT DUPLICATES FROM LT_CMATNR COMPARING MATNR WERKS VERID.

*-
  DATA: LV_STPST TYPE STPOX-STUFE VALUE '04'.
  call function 'ZFMM_BOM_EXPL_MAT'
    EXPORTING
      IV_DATUV  = SY-DATLO
      IV_STPST  = LV_STPST
    IMPORTING
      EV_RETURN = LT_RETURN
    TABLES
      IT_MATNR  = LT_CMATNR
      IT_STB    = LT_STB.

*-
  SORT LT_STB BY IDNRK WERKS.
  DELETE ADJACENT DUPLICATES FROM LT_STB COMPARING IDNRK WERKS.
  IF LT_STB[] IS NOT INITIAL.
    SELECT FROM V_MARC_MD
           FIELDS DISTINCT MATNR, WERKS
           FOR ALL ENTRIES IN @LT_STB
           WHERE MATNR = @LT_STB-IDNRK
             AND WERKS = @LT_STB-WERKS
             AND LVORM = @SPACE
    INTO TABLE @DATA(LT_MAT).
  ENDIF.

*-
  IF LT_MAT[] IS NOT INITIAL.
    GT_STB[] = VALUE #( BASE GT_STB[] FOR LS_VALUE IN LT_MAT
                          ( MATNR = LS_VALUE-MATNR WERKS = LS_VALUE-WERKS ) ).
  ENDIF.

*******************************************************************************************
  DO LV_BOM_LOOP TIMES.
    PERFORM BOM_LEVEL_LOOP TABLES LT_STB.

    IF LT_STB[] IS INITIAL. EXIT. ENDIF.
  ENDDO.
*******************************************************************************************
  FREE LT_STB.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form BOM_LEVEL_LOOP
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LT_STB
*&---------------------------------------------------------------------*
FORM BOM_LEVEL_LOOP TABLES CT_STB STRUCTURE ZSMM_STPOX.

*-
  DATA: LT_CMATNR     TYPE STANDARD TABLE OF ZSMM00001 WITH HEADER LINE,
        LT_MATNR_MKAL TYPE STANDARD TABLE OF ZSMM00001,
        LT_STB        TYPE STANDARD TABLE OF ZSMM_STPOX WITH HEADER LINE,
        LT_MAT        TYPE STANDARD TABLE OF ZSMM_STPOX,
        LT_RETURN     TYPE STANDARD TABLE OF BAPIRET2 WITH HEADER LINE.

  _G_INIT: LT_CMATNR, LT_STB, LT_MATNR_MKAL.

  READ TABLE GT_COMM_CFG_B312 INTO DATA(LS_CFG) INDEX 1.

*-
  LT_MAT[] = CT_STB[].

  SORT LT_MAT BY IDNRK WERKS.
  DELETE ADJACENT DUPLICATES FROM LT_MAT COMPARING IDNRK WERKS.
  IF LT_MAT[] IS NOT INITIAL.
    SELECT FROM @LT_MAT AS A INNER JOIN MKAL AS B
                                      ON B~MATNR = A~IDNRK
                                     AND B~WERKS = A~WERKS
                                     AND B~VERID NE @SPACE
                                     AND B~MKSP  EQ @SPACE
           FIELDS
           B~MATNR, B~WERKS, B~VERID, B~STLAL, 1 AS EMENG
           WHERE B~BDATU >= @SY-DATLO
             AND B~ADATU <= @SY-DATLO
    INTO CORRESPONDING FIELDS OF TABLE @LT_MATNR_MKAL.
  ENDIF.
  FREE LT_MAT.

  IF LS_CFG-FIELD4 = 'Y'.
    SORT LT_MATNR_MKAL BY MATNR WERKS VERID.
    DELETE ADJACENT DUPLICATES FROM LT_MATNR_MKAL COMPARING MATNR WERKS VERID.
    IF LT_MATNR_MKAL[] IS NOT INITIAL.
      SELECT FROM @LT_MATNR_MKAL AS A INNER JOIN PLAF AS B
                                             ON B~MATNR = A~MATNR
                                            AND B~PLWRK = A~WERKS
                                            AND B~VERID = A~VERID
                                            AND B~PLSCN = 0
                  FIELDS
                  DISTINCT B~MATNR, B~PLWRK AS WERKS, B~VERID, 1 AS EMENG
                  ORDER BY B~MATNR, B~PLWRK, B~VERID
      INTO CORRESPONDING FIELDS OF TABLE @LT_CMATNR.
    ENDIF.
    FREE LT_MATNR_MKAL.

  ELSE.
    LT_CMATNR[] = LT_MATNR_MKAL[].
  ENDIF.
  SORT LT_CMATNR BY MATNR WERKS VERID.
  DELETE ADJACENT DUPLICATES FROM LT_CMATNR COMPARING MATNR WERKS VERID.

*-
  DATA: LV_STPST TYPE STPOX-STUFE VALUE '04'.
  call function 'ZFMM_BOM_EXPL_MAT'
    EXPORTING
      IV_DATUV  = SY-DATLO
      IV_STPST  = LV_STPST
    IMPORTING
      EV_RETURN = LT_RETURN
    TABLES
      IT_MATNR  = LT_CMATNR
      IT_STB    = LT_STB.

  CT_STB[] = LT_STB[].

*-
  CHECK LT_STB[] IS NOT INITIAL.

  SORT LT_STB BY IDNRK WERKS.
  DELETE ADJACENT DUPLICATES FROM LT_STB COMPARING IDNRK WERKS.
  IF LT_STB[] IS NOT INITIAL.
    SELECT FROM V_MARC_MD
           FIELDS DISTINCT MATNR, WERKS
           FOR ALL ENTRIES IN @LT_STB
           WHERE MATNR = @LT_STB-IDNRK
             AND WERKS = @LT_STB-WERKS
             AND LVORM = @SPACE
    INTO TABLE @DATA(LT_MATNR).
  ENDIF.
  FREE LT_STB.

*-
  IF LT_MATNR[] IS NOT INITIAL.
    GT_STB[] = VALUE #( BASE GT_STB[] FOR LS_VALUE IN LT_MATNR
                          ( MATNR = LS_VALUE-MATNR WERKS = LS_VALUE-WERKS ) ).
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_SEL_FIELD_LTP_TABLE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- EV_PRG
*&---------------------------------------------------------------------*
FORM SET_SEL_FIELD_LTP_TABLE CHANGING EV_PRG.

*-
  TRY.
      SELECT FIELDNAME, POSITION, KEYFLAG FROM DD03L WHERE TABNAME = @TEXT-O02 AND FIELDNAME NE @TEXT-O03
                                 ORDER BY POSITION INTO TABLE @DATA(LT_ORG_LTP_T).

      LOOP AT LT_ORG_LTP_T INTO DATA(LS_ORG_LTP_T) WHERE KEYFLAG = 'X'.
        APPEND LS_ORG_LTP_T-FIELDNAME TO: GT_SEL_LIST.
      ENDLOOP.

      SORT LT_ORG_LTP_T BY FIELDNAME.
      DATA(LT_COMM_CFG_C) = GT_COMM_CFG_C[].
      SORT LT_COMM_CFG_C BY FIELD2. DELETE ADJACENT DUPLICATES FROM LT_COMM_CFG_C COMPARING FIELD2.
      LOOP AT LT_COMM_CFG_C INTO DATA(LS_CFG).
        CHECK LS_CFG-FIELD2 IS NOT INITIAL.
        READ TABLE LT_ORG_LTP_T WITH KEY FIELDNAME = LS_CFG-FIELD2 BINARY SEARCH TRANSPORTING NO FIELDS.
        IF SY-SUBRC = 0.
          APPEND LS_CFG-FIELD2 TO: GT_SEL_LIST.
        ELSE.
          "Config : [ MRP05 ]  LTP List 구성 조건필드(Field2)가 실제 DB 에 존재하지 않습니다.
          MESSAGE S003(ZMM01) WITH TEXT-M06 LS_CFG-FIELD2 TEXT-M07 DISPLAY LIKE 'E'. CLEAR EV_PRG.
          LEAVE LIST-PROCESSING.
        ENDIF.
      ENDLOOP.

*--
    CATCH CX_SY_DYNAMIC_OSQL_ERROR.
  ENDTRY.

*-
  APPEND : 'DAT00'(F01) TO: GT_SEL_LIST,
           'MNG01'(F02) TO: GT_SEL_LIST.

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

**  A   기초재고 ( 가용재고 포함여부 )
**  B   소요량
**  C   확정입고예정
**  E   예상기말재고

*-
  DATA: LV_MEINS TYPE MEINS,
        LV_DEL   TYPE CHAR1.

*-
  _G_INIT: GT_DISP, GT_DATE.

*-
  SORT : GT_MAIN    BY WERKS MATNR,
         GT_LTP_DAT BY OPT MATNR WERKS DAT00,
         GT_SUB_STK BY MATERIAL PLANT,  "U2
         GT_DAY     BY DATE,
         GT_WEEK    BY WEEK,
         GT_BERID   BY BERID.

  GT_WEEK[] = GT_DAY[].

*-
  LOOP AT GT_MAIN INTO DATA(LS_MAIN).
    CLEAR: GV_BASE_STK, GV_SUB_STK, LV_DEL, GV_DAY1_DAY, GV_DAY2_DAY.
    _G_INIT: GT_DATE.

*-- 최초 가용재고 ( GV_BASE_STK ), 외주업체 재고 ( GV_SUB_STK )
    PERFORM GET_BASE_STOCK USING 'A' LS_MAIN.

*-- 소요량
    PERFORM APPEND_OPT_DATA USING 'B' TEXT-T09 LS_MAIN.

*--확정입고예정
    PERFORM APPEND_OPT_DATA USING 'C' TEXT-T10 LS_MAIN.

***----------------------------------------------------------------***
*** 기초재고 -> 기말재고 -> 예상기말재고
***----------------------------------------------------------------***
    PERFORM CACUL_STOCK USING 'A' TEXT-T08 "기초재고
                              'E' TEXT-T11 "예상기말재고
                              LS_MAIN.
***----------------------------------------------------------------***
*-  추가선택조건 Filtering
    PERFORM FILTERING_DISP_DATA CHANGING LV_DEL.

*--
    IF LV_DEL IS INITIAL.
      PERFORM APPEND_ALL_DATA USING LS_MAIN LV_MEINS.
    ENDIF.

  ENDLOOP.

*- 공급가능일수 감안
  DELETE GT_DISP WHERE NOT ( DAY1 IN S_DAY1  AND
                             DAY2 IN S_DAY2 ).
*-
  SORT GT_DISP BY WERKS MATNR OPT.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_BASE_STOCK
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> P_
*&      --> LS_MAIN
*&---------------------------------------------------------------------*
FORM GET_BASE_STOCK USING IV_OPT IS_MAIN TYPE TY_DATA.

*-
  CLEAR : GV_BASE_STK, GV_SUB_STK.

*- 현가용재고
  READ TABLE GT_LTP_DAT INTO DATA(LS_LTP_DAT) WITH KEY OPT = IV_OPT
                                                     MATNR = IS_MAIN-MATNR
                                                     WERKS = IS_MAIN-WERKS BINARY SEARCH.
  IF SY-SUBRC = 0.
    DATA(LV_TABIX) = SY-TABIX.
    LOOP AT GT_LTP_DAT INTO LS_LTP_DAT FROM LV_TABIX.
      IF LS_LTP_DAT-OPT   <> IV_OPT OR
         LS_LTP_DAT-MATNR <> IS_MAIN-MATNR OR
         LS_LTP_DAT-WERKS <> IS_MAIN-WERKS.
        EXIT.

      ELSE.
        ADD LS_LTP_DAT-MNG01 TO GV_BASE_STK.
      ENDIF.
    ENDLOOP.
  ENDIF.

* 외주업체재고 포함
  READ TABLE GT_SUB_STK INTO DATA(LS_SUB_STK) WITH TABLE KEY IDX_KEY  "U2
                                              COMPONENTS MATERIAL = IS_MAIN-MATNR
                                                         PLANT    = IS_MAIN-WERKS.
  IF SY-SUBRC = 0.
    GV_SUB_STK = LS_SUB_STK-STOCK.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form APPEND_OPT_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> P_
*&      --> TEXT_T09
*&      --> LS_MAIN
*&---------------------------------------------------------------------*
FORM APPEND_OPT_DATA USING IV_OPT IV_TEXT
                              IS_MAIN TYPE TY_DATA.

**  A   기초재고 ( 가용재고 포함여부 )
**  B   소요량
**  C   확정입고예정1
**  D   확정입고예정2
**  E   예상기말재고

  DATA: LS_DISP     TYPE TS_DISP,
        LS_DATE     TYPE TY_DATE,
        LV_DAT001   TYPE CHAR6 VALUE 'DAT001',
        LV_B_SUM    TYPE ZTMM2001H-MNG01,
        LV_BASE_STK TYPE ZTMM2001H-MNG01.

*-
  CLEAR : LS_DISP, LS_DATE, LV_B_SUM.

*-
  LS_DATE-OPT     = IV_OPT.
  LS_DATE-OPT_TXT = IV_TEXT.

*-
  FIELD-SYMBOLS: <LV_FIELD> TYPE CHAR6,
                 <LV_VALUE> TYPE ZSMM_DATE_VALUE-DAT001.

  TRY.
*-
      READ TABLE GT_LTP_DAT INTO DATA(LS_LTP_DAT) WITH KEY OPT = IV_OPT
                                                         MATNR = IS_MAIN-MATNR
                                                         WERKS = IS_MAIN-WERKS BINARY SEARCH.
      IF SY-SUBRC = 0.
        DATA(LV_TABIX) = SY-TABIX.
        LOOP AT GT_LTP_DAT INTO LS_LTP_DAT FROM LV_TABIX.
          IF LS_LTP_DAT-OPT   <> IV_OPT OR
             LS_LTP_DAT-MATNR <> IS_MAIN-MATNR OR
             LS_LTP_DAT-WERKS <> IS_MAIN-WERKS.
            EXIT.

          ELSE.

            CASE P_FORMAT.
              WHEN 'D'.
                READ TABLE GT_DAY INTO DATA(LS_DAY) WITH KEY DATE = LS_LTP_DAT-DAT00 BINARY SEARCH.
                IF SY-SUBRC = 0.
                  ASSIGN LS_DAY-DAY TO <LV_FIELD>.
                ELSE.
                  IF GV_FIRST_DAY > LS_LTP_DAT-DAT00. "초기일자에 SUM
                    ASSIGN LV_DAT001 TO <LV_FIELD>.
                  ENDIF.
                ENDIF.
              WHEN 'W'.
                IF GV_FIRST_DAY <= LS_LTP_DAT-DAT00.
                  DATA: LV_WEEK TYPE SCAL-WEEK.
                  CALL FUNCTION 'DATE_GET_WEEK'
                    EXPORTING
                      DATE = LS_LTP_DAT-DAT00
                    IMPORTING
                      WEEK = LV_WEEK.
                  READ TABLE GT_WEEK INTO LS_DAY WITH KEY WEEK = LV_WEEK BINARY SEARCH.
                  IF SY-SUBRC = 0.
                    ASSIGN LS_DAY-DAY TO <LV_FIELD>.
                  ELSE.
                    IF GV_FIRST_DAY > LS_LTP_DAT-DAT00. "초기일자에 SUM
                      ASSIGN LV_DAT001 TO <LV_FIELD>.
                    ENDIF.
                  ENDIF.
                ELSE.
                  ASSIGN LV_DAT001 TO <LV_FIELD>.
                ENDIF.
              WHEN 'M'.
                READ TABLE GT_DAY INTO LS_DAY WITH KEY DATE(06) = LS_LTP_DAT-DAT00(06) BINARY SEARCH.
                IF SY-SUBRC = 0.
                  ASSIGN LS_DAY-DAY TO <LV_FIELD>.
                ELSE.
                  IF GV_FIRST_DAY(06) > LS_LTP_DAT-DAT00(06). "초기일자에 SUM
                    ASSIGN LV_DAT001 TO <LV_FIELD>.
                  ENDIF.
                ENDIF.
            ENDCASE.
*--
            IF <LV_FIELD> IS ASSIGNED.
              ASSIGN COMPONENT <LV_FIELD> OF STRUCTURE LS_DATE TO <LV_VALUE>.
              IF <LV_VALUE> IS ASSIGNED.
                <LV_VALUE> = <LV_VALUE> + LS_LTP_DAT-MNG01.
                IF IV_OPT = 'B'.
                  ADD LS_LTP_DAT-MNG01 TO LV_B_SUM.

                  "공급가능 일수 계산
                  LV_BASE_STK = LV_BASE_STK - LS_LTP_DAT-MNG01.
                  IF LV_BASE_STK < 0.
                    GV_DAY1_DAY = LS_LTP_DAT-DAT00.
                  ENDIF.

                ENDIF.
                UNASSIGN: <LV_VALUE>.
              ENDIF.
              UNASSIGN : <LV_FIELD>.
            ENDIF.
          ENDIF.
        ENDLOOP.
      ENDIF.

*-
      IF IV_OPT = 'B'.
        LS_DATE-MGSUM = LV_B_SUM.
      ENDIF.
      APPEND LS_DATE TO GT_DATE. CLEAR : LS_DATE.

*--
    CATCH CX_SY_ARITHMETIC_ERROR INTO DATA(LR_ORF_1).
  ENDTRY.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form CACUL_STOCK
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> P_
*&      --> TEXT_T08
*&      --> P_
*&      --> TEXT_T11
*&      --> LS_MAIN
*&---------------------------------------------------------------------*
FORM CACUL_STOCK USING IV_OPT_A IV_OPT_A_TEXT
                         IV_OPT_B IV_OPT_B_TEXT
                         IS_MAIN TYPE TY_DATA.

  FIELD-SYMBOLS: <LV_FIELD>   TYPE CHAR6,
                 <LV_VALUE_A> TYPE ZSMM_DATE_VALUE-DAT001,
                 <LV_VALUE_B> TYPE ZSMM_DATE_VALUE-DAT001,
                 <LV_VALUE_C> TYPE ZSMM_DATE_VALUE-DAT001.

  DATA: LS_DATE_A    TYPE TY_DATE, "기초재고
        LS_DATE_B    TYPE TY_DATE, "예상기말재고
        LV_BASE_STK  TYPE ZTMM20010-MNG01,
        LV_CACUL_STK TYPE ZTMM20010-MNG01,
        LV_LAST_STK  TYPE ZTMM20010-MNG01,
        LV_DAY1_QTY  TYPE ZTMM20010-MNG01,
        LV_DAY2_QTY  TYPE ZTMM20010-MNG01,
        LV_REQ_QTY   TYPE ZTMM20010-MNG01,
        LV_DATE      TYPE SY-DATUM,
        LV_DAY1_CHK, LV_DAY2_CHK.

*-
  CLEAR: LS_DATE_A, LS_DATE_B, LV_DAY1_CHK, LV_DAY2_CHK, LV_LAST_STK,
         GV_DAY1_DAYS, GV_DAY2_DAYS.

  TRY.

      LOOP AT GT_DAY INTO DATA(LS_DAY) FROM 1.

        CLEAR: LV_BASE_STK, LV_CACUL_STK, LV_DAY1_QTY, LV_DAY2_QTY, LV_REQ_QTY.

        ASSIGN LS_DAY-DAY TO <LV_FIELD>.
        CHECK <LV_FIELD> IS ASSIGNED.

*-- 기초재고
        LS_DATE_A-OPT     = IV_OPT_A.
        LS_DATE_A-OPT_TXT = IV_OPT_A_TEXT.

        IF SY-TABIX = 1.
**          IF  P_CHK1 = 'X' OR P_CHK5 = 'X'. "현재고 포함 체크('X'), 업체재고 포함 체크('X')
**            LS_DATE_A-DAT001   = LV_BASE_STK = ( GV_BASE_STK + GV_SUB_STK ). "당일기초재고   "U2
**          ENDIF.
          IF  P_CHK1 = 'X'. "현재고 포함 체크('X')
            LS_DATE_A-DAT001   = LS_DATE_A-DAT001 + GV_BASE_STK. "당일기초재고   "U2
          ENDIF.
          IF  P_CHK5 = 'X'. "업체재고 포함 체크('X')
            LS_DATE_A-DAT001   = LS_DATE_A-DAT001 + GV_SUB_STK. "당일기초재고   "U2
          ENDIF.
          LV_BASE_STK = LS_DATE_A-DAT001.
        ELSE.
          ASSIGN COMPONENT <LV_FIELD> OF STRUCTURE LS_DATE_A TO <LV_VALUE_A>.
          IF <LV_VALUE_A> IS ASSIGNED.
            <LV_VALUE_A> = LV_BASE_STK = LV_LAST_STK. "전일예상기말재고
          ENDIF.

        ENDIF.


*-- 예상기말재고 ( 기초재고 - 소요량 + 확정입고예정1 + 확정입고예정2 )
        LS_DATE_B-OPT     = IV_OPT_B.
        LS_DATE_B-OPT_TXT = IV_OPT_B_TEXT.

        CLEAR LV_CACUL_STK.
        LV_CACUL_STK = LV_DAY1_QTY = LV_DAY2_QTY = LV_BASE_STK.

        IF <LV_FIELD> IS ASSIGNED.
          "소요량
          READ TABLE GT_DATE INTO DATA(LS_ODATE) WITH TABLE KEY IDX_KEY
                             COMPONENTS OPT = 'B'.
          IF SY-SUBRC = 0.
            ASSIGN COMPONENT <LV_FIELD> OF STRUCTURE LS_ODATE TO <LV_VALUE_B>.
            IF <LV_VALUE_B> IS ASSIGNED.
              LV_CACUL_STK = LV_CACUL_STK - <LV_VALUE_B>.
              LV_REQ_QTY = <LV_VALUE_B>.
              UNASSIGN : <LV_VALUE_B>.
            ENDIF.
          ENDIF.
          "확정입고예정1
          READ TABLE GT_DATE INTO LS_ODATE WITH TABLE KEY IDX_KEY
                             COMPONENTS OPT = 'C'.
          IF SY-SUBRC = 0.
            ASSIGN COMPONENT <LV_FIELD> OF STRUCTURE LS_ODATE TO <LV_VALUE_B>.
            IF <LV_VALUE_B> IS ASSIGNED.
              LV_CACUL_STK = LV_CACUL_STK + <LV_VALUE_B>.
              ADD <LV_VALUE_B> TO LV_DAY2_QTY.
              UNASSIGN : <LV_VALUE_B>.
            ENDIF.
          ENDIF.
          "확정입고예정2
          READ TABLE GT_DATE INTO LS_ODATE WITH TABLE KEY IDX_KEY
                             COMPONENTS OPT = 'D'.
          IF SY-SUBRC = 0.
            ASSIGN COMPONENT <LV_FIELD> OF STRUCTURE LS_ODATE TO <LV_VALUE_B>.
            IF <LV_VALUE_B> IS ASSIGNED.
              LV_CACUL_STK = LV_CACUL_STK + <LV_VALUE_B>.
              ADD <LV_VALUE_B> TO LV_DAY2_QTY.
              UNASSIGN : <LV_VALUE_B>.
            ENDIF.
          ENDIF.
        ENDIF.

        ASSIGN COMPONENT <LV_FIELD> OF STRUCTURE LS_DATE_B TO <LV_VALUE_B>.
        IF <LV_VALUE_B> IS ASSIGNED.
          <LV_VALUE_B> = LV_LAST_STK = LV_CACUL_STK.  "예상기말재고(초기값)
          UNASSIGN : <LV_VALUE_B>.
        ENDIF.

*-- 공급가능일수
        IF P_FORMAT = 'D'.
          IF ( LV_DAY1_QTY - LV_REQ_QTY ) < 0 AND GV_FIRST_DAY <= LS_DAY-DATE AND LV_DAY1_CHK IS INITIAL.
            IF SY-DATLO = LS_DAY-DATE.
              LS_DATE_A-DAY1 = 0.
            ELSE.
              LV_DATE = SY-DATLO.
              PERFORM F_DAYS_AND_MONTHS_AND_YEARS  USING LV_DATE LS_DAY-DATE CHANGING GV_DAY1_DAYS.
            ENDIF.
            LV_DAY1_CHK ='X'.
          ENDIF.
          IF ( LV_DAY2_QTY - LV_REQ_QTY ) < 0 AND GV_FIRST_DAY <= LS_DAY-DATE AND LV_DAY2_CHK IS INITIAL.
            IF SY-DATLO = LS_DAY-DATE.
              LS_DATE_A-DAY1 = 0.
            ELSE.
              LV_DATE = SY-DATLO.
              PERFORM F_DAYS_AND_MONTHS_AND_YEARS  USING LV_DATE LS_DAY-DATE CHANGING GV_DAY2_DAYS.
            ENDIF.
            LV_DAY2_CHK ='X'.
          ENDIF.
        ENDIF.

        UNASSIGN <LV_FIELD>.
      ENDLOOP.

*- 음수발생 재고가 없는경우 '999' 로 세팅
      PERFORM SET_MINUS_STOCK USING LV_DAY1_CHK LV_DAY2_CHK.
*--
      APPEND : LS_DATE_A TO GT_DATE, LS_DATE_B TO GT_DATE.

*--
    CATCH CX_SY_ARITHMETIC_ERROR INTO DATA(LR_ORF_1).
  ENDTRY.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_DAYS_AND_MONTHS_AND_YEARS
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LV_DATE
*&      --> LS_DAY_DATE
*&      <-- GV_DAY1_DAYS
*&---------------------------------------------------------------------*
FORM F_DAYS_AND_MONTHS_AND_YEARS USING IV_DATE_F
                                           IV_DATE_T
                                  CHANGING EV_DAY.
  DATA: LV_DAYS TYPE VTBBEWE-ATAGE.

  CLEAR LV_DAYS.

  CALL FUNCTION 'FIMA_DAYS_AND_MONTHS_AND_YEARS'
    EXPORTING
      I_DATE_FROM = IV_DATE_F
      I_DATE_TO   = IV_DATE_T
    IMPORTING
      E_DAYS      = LV_DAYS.

  EV_DAY = LV_DAYS.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_MINUS_STOCK
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LV_DAY1_CHK
*&      --> LV_DAY2_CHK
*&---------------------------------------------------------------------*
FORM SET_MINUS_STOCK USING IV_DAY1_CHK
                           IV_DAY2_CHK.

*-
  IF IV_DAY1_CHK IS INITIAL AND GV_DAY1_DAYS = 0. GV_DAY1_DAYS = 999. ENDIF.
  IF IV_DAY2_CHK IS INITIAL AND GV_DAY2_DAYS = 0. GV_DAY2_DAYS = 999. ENDIF.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form FILTERING_DISP_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- LV_DEL
*&---------------------------------------------------------------------*
FORM FILTERING_DISP_DATA CHANGING EV_DEL.

  IF P_CHK4 = 'X'.
    "소요량 있는 자재만 조회
    READ TABLE GT_DATE INTO DATA(LS_DATE) WITH TABLE KEY IDX_KEY COMPONENTS OPT = 'B'.
    IF SY-SUBRC = 0.
      IF LS_DATE-MGSUM <= 0.
        EV_DEL = 'X'.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form APPEND_ALL_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LS_MAIN
*&      --> LV_MEINS
*&---------------------------------------------------------------------*
FORM APPEND_ALL_DATA USING IS_MAIN TYPE TY_DATA
                           IV_MEINS.

  DATA: LS_DISP     TYPE TS_DISP.

*-
  LOOP AT GT_DATE INTO DATA(LS_DATE) FROM 1.
    CLEAR LS_DISP.

    MOVE-CORRESPONDING : IS_MAIN TO LS_DISP.
    MOVE-CORRESPONDING : LS_DATE TO LS_DISP.

    IF LS_DATE-OPT = 'A'.
      LS_DISP-STOCK = GV_BASE_STK. "가용재고
      LS_DISP-SUB_STOCK = GV_SUB_STK. "업체재고재고  "U2

      LS_DISP-DAY1 = GV_DAY1_DAYS.
      LS_DISP-DAY2 = GV_DAY2_DAYS.

      READ TABLE GT_BERID INTO DATA(LS_BERID) WITH KEY BERID = LS_DISP-BERID BINARY SEARCH.
      IF SY-SUBRC = 0.
        LS_DISP-BERTX = LS_BERID-BERTX.
      ENDIF.
    ENDIF.

    APPEND LS_DISP TO GT_DISP.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F4_DATE_FORMAT_YYYYMMDD
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> S_SWEEK_LOW
*&---------------------------------------------------------------------*
FORM F4_DATE_FORMAT_YYYYMMDD USING IV_DATE.

  CALL FUNCTION 'F4_DATE'
    IMPORTING
      SELECT_WEEK = IV_DATE.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form RUN_MRP
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM RUN_MRP.

*-
  DATA: LT_SELTAB TYPE TABLE OF RSPARAMS.

*-
  _G_INIT: GT_COMM_CFG_B.

*- MRP 실행 Config
  ZCL_MM_COMMON=>COMMON_CONFIG(  EXPORTING IS_COMMON =  VALUE #( M = 'B1' D = 'B0002' S = '00001' )
                                 IT_WHERE = VALUE #( ( FIELD = 4 VALUE = SY-CPROG ) ( FIELD = 5 VALUE = 'OUT' ) )
                                 IMPORTING ET_OUTTAB = DATA(LT_CONFIG) ).
  GT_COMM_CFG_B[] = CORRESPONDING #( LT_CONFIG[] ).

  IF GT_COMM_CFG_B[] IS INITIAL.
    "Config : [ B0002 / 00001 ]  MRP 집계 프로그램 설정이 없습니다.
    MESSAGE S000(ZMM01) WITH TEXT-M02 DISPLAY LIKE 'E'.
    LEAVE LIST-PROCESSING.
  ENDIF.

*-
  DATA: LV_ANSWER TYPE CHAR1.

  PERFORM POPUP_CONFIRM  USING TEXT-T07
                          TEXT-M03
                          LV_ANSWER.

  CHECK LV_ANSWER = 1.

*-
  READ TABLE GT_COMM_CFG_B INTO DATA(LS_CFG_B) INDEX 1.

  CLEAR LT_SELTAB[].

*- 플랜트
  IF S_WERKS[] IS NOT INITIAL.
    LT_SELTAB[] = VALUE #( BASE LT_SELTAB FOR LS_VALUE IN S_WERKS
                           ( SELNAME = 'SO_WERKS-LOW' SIGN    = 'I' OPTION  = 'EQ'
                             LOW     = S_WERKS-LOW ) ).
    LT_SELTAB[] = VALUE #( BASE LT_SELTAB FOR LS_VALUE IN S_WERKS
                           ( SELNAME = 'SO_BERID-LOW' SIGN    = 'I' OPTION  = 'EQ'
                             LOW     = S_WERKS-LOW ) ).
  ENDIF.

  LT_SELTAB[] = VALUE #( BASE LT_SELTAB
                         ( SELNAME = 'PV_EXMOD' LOW     = LS_CFG_B-FIELD2 )
                         ( SELNAME = 'PV_SVGRP' LOW     = LS_CFG_B-FIELD3 ) ).

*-

  "RMDMRPEXTRACT02
  SUBMIT RMDMRPEXTRACT01 WITH SELECTION-TABLE LT_SELTAB AND RETURN.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form POPUP_CONFIRM
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> TEXT_T18
*&      --> TEXT_M12
*&      --> LV_ANSWER
*&---------------------------------------------------------------------*
FORM POPUP_CONFIRM USING IV_TITLE TYPE ITEX132
                            IV_QUEST  TYPE ITEX132
                            IV_ANSWER TYPE CHAR1.

  CLEAR IV_ANSWER.
  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
      TITLEBAR              = IV_TITLE
      TEXT_QUESTION         = IV_QUEST
      TEXT_BUTTON_1         = 'Yes'
      TEXT_BUTTON_2         = 'No'
      DEFAULT_BUTTON        = '1'
      DISPLAY_CANCEL_BUTTON = ''
    IMPORTING
      ANSWER                = IV_ANSWER
    EXCEPTIONS
      TEXT_NOT_FOUND        = 1
      OTHERS                = 2.
  IF SY-SUBRC NE 0.
    IV_ANSWER = 2.
  ENDIF.

ENDFORM. " POPUP_CONFIRM
*&---------------------------------------------------------------------*
*& Form GET_LISTBOX_FORMAT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM GET_LISTBOX_FORMAT .

  DATA: LT_LIST TYPE VRM_VALUES.

  _G_INIT: LT_LIST.
  LT_LIST[] = VALUE #( BASE LT_LIST FOR LS_VALUE IN GT_COMM_CFG_A
                          ( KEY =  LS_VALUE-FIELD3  TEXT = LS_VALUE-FIELD6 ) ).

  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      ID     = CONV VRM_ID( 'P_FORMAT' )
      VALUES = LT_LIST.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SELECTION_SCREEN_OUTPUT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM SELECTION_SCREEN_OUTPUT.

  LOOP AT SCREEN.
    CASE P_FORMAT.
      WHEN 'D'.
        IF ( SCREEN-NAME = 'S_SPMON-LOW' OR SCREEN-NAME = 'S_SPMON-HIGH' ) OR
           ( SCREEN-NAME = 'S_SWEEK-LOW' OR SCREEN-NAME = 'S_SWEEK-HIGH' ).
          SCREEN-ACTIVE = 0.
        ELSEIF SCREEN-NAME = 'S_DATE-LOW' OR SCREEN-NAME = 'S_DATE-HIGH'.
          SCREEN-ACTIVE = 1.
        ENDIF.
      WHEN 'W'.
        IF ( SCREEN-NAME = 'S_DATE-LOW' OR SCREEN-NAME = 'S_DATE-HIGH' ) OR
           ( SCREEN-NAME = 'S_SPMON-LOW' OR SCREEN-NAME = 'S_SPMON-HIGH' ).
          SCREEN-ACTIVE = 0.
        ELSEIF SCREEN-NAME = 'S_SWEEK-LOW' OR SCREEN-NAME = 'S_SWEEK-HIGH'.
          SCREEN-ACTIVE = 1.
        ENDIF.
        IF SCREEN-GROUP1 = 'DAY'.
          SCREEN-INPUT = 0.
        ENDIF.

      WHEN 'M'.
        IF ( SCREEN-NAME = 'S_DATE-LOW' OR SCREEN-NAME = 'S_DATE-HIGH' ) OR
           ( SCREEN-NAME = 'S_SWEEK-LOW' OR SCREEN-NAME = 'S_SWEEK-HIGH' ).
          SCREEN-ACTIVE = 0.
        ELSEIF SCREEN-NAME = 'S_SPMON-LOW' OR SCREEN-NAME = 'S_SPMON-HIGH'.
          SCREEN-ACTIVE = 1.
        ENDIF.
        IF SCREEN-GROUP1 = 'DAY'.
          SCREEN-INPUT = 0.
        ENDIF.
      WHEN OTHERS.
        IF ( SCREEN-NAME = 'S_DATE-LOW' OR SCREEN-NAME = 'S_DATE-HIGH' ) OR
           ( SCREEN-NAME = 'S_SWEEK-LOW' OR SCREEN-NAME = 'S_SWEEK-HIGH' ) OR
           ( SCREEN-NAME = 'S_SPMON-LOW' OR SCREEN-NAME = 'S_SPMON-HIGH' ).
          SCREEN-ACTIVE = 0.
        ENDIF.
    ENDCASE.

    CASE SCREEN-NAME.
      WHEN 'S_WERKS-LOW' OR 'S_WERKS-HIGH' OR 'P_PLSCN'.
        SCREEN-REQUIRED = 2.
    ENDCASE.

    MODIFY SCREEN.
  ENDLOOP.

*-
  SORT GT_COMM_CFG_A BY FIELD3.
  CASE P_FORMAT.
    WHEN 'D'.
      IF S_DATE[] IS INITIAL.
        READ TABLE GT_COMM_CFG_A INTO DATA(LS_CFG) WITH KEY FIELD3 = 'D' BINARY SEARCH.
        IF SY-SUBRC = 0.
          PERFORM SET_INTERVAL USING 'D' LS_CFG.
        ENDIF.
      ENDIF.
    WHEN 'W'.
      IF S_SWEEK[] IS INITIAL.
        READ TABLE GT_COMM_CFG_A INTO LS_CFG WITH KEY FIELD3 = 'W' BINARY SEARCH.
        IF SY-SUBRC = 0.
          PERFORM SET_INTERVAL USING 'W' LS_CFG.
        ENDIF.
      ENDIF.
      _G_INIT: S_DAY1, S_DAY2.
    WHEN 'M'.
      IF S_SPMON[] IS INITIAL.
        READ TABLE GT_COMM_CFG_A INTO LS_CFG WITH KEY FIELD3 = 'M' BINARY SEARCH.
        IF SY-SUBRC = 0.
          PERFORM SET_INTERVAL USING 'M' LS_CFG.
        ENDIF.
      ENDIF.
      _G_INIT: S_DAY1, S_DAY2.
  ENDCASE.

  SORT GT_COMM_CFG_A BY FIELD5.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_INTERVAL
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM SET_INTERVAL USING IV_ENTLU IS_CFG LIKE ZTMM00002.

  DATA: LV_CFDATE TYPE SY-DATLO,
        LV_CTDATE TYPE SY-DATLO,
        LV_FRCNT  TYPE DLYMO,
        LV_TOCNT  TYPE DLYMO,
        LV_DATE   TYPE SY-DATLO,
        LV_FWEEK  TYPE SCAL-WEEK,
        LV_TWEEK  TYPE SCAL-WEEK,
        LV_TIME   TYPE MARA-MHDHB.


  _G_INIT: S_SPMON, S_DATE, S_SWEEK.

  CASE IV_ENTLU.
    WHEN 'D'.
      LV_DATE  = SY-DATLO.

      LV_TIME = IS_CFG-FIELD4.
      PERFORM F_ADD_TIME_TO_DATE USING LV_DATE LV_TIME '' CHANGING LV_CFDATE.

      LV_TIME = IS_CFG-FIELD2.
      LV_TIME = LV_TIME - 1.
      PERFORM F_ADD_TIME_TO_DATE USING LV_DATE LV_TIME '' CHANGING LV_CTDATE.

      _G_APPEND_VALUE : S_DATE 'BT' 'I' LV_CFDATE LV_CTDATE.
    WHEN 'W'.
      DATA: LV_WEEK  LIKE  SCAL-WEEK,
            LV_IDATE LIKE  SY-DATUM,
            LV_IPRKZ LIKE MARA-IPRKZ.

      CLEAR LV_WEEK.
      CALL FUNCTION 'DATE_GET_WEEK'
        EXPORTING
          DATE = SY-DATLO
        IMPORTING
          WEEK = LV_WEEK.

      LV_FWEEK = LV_WEEK.

      LV_TIME  = IS_CFG-FIELD2.
      LV_IPRKZ = '1'.

      CLEAR LV_IDATE.
      CALL FUNCTION 'ADD_TIME_TO_DATE'
        EXPORTING
          I_IDATE               = SY-DATLO
          I_TIME                = LV_TIME
          I_IPRKZ               = LV_IPRKZ
        IMPORTING
          O_IDATE               = LV_IDATE
        EXCEPTIONS
          INVALID_PERIOD        = 1
          INVALID_ROUND_UP_RULE = 2
          INTERNAL_ERROR        = 3
          OTHERS                = 4.

      CLEAR LV_WEEK.
      CALL FUNCTION 'DATE_GET_WEEK'
        EXPORTING
          DATE = LV_IDATE
        IMPORTING
          WEEK = LV_WEEK.

      LV_TWEEK = LV_WEEK.

      _G_APPEND_VALUE : S_SWEEK 'BT' 'I' LV_FWEEK LV_TWEEK.
    WHEN 'M'.
      LV_DATE  = SY-DATLO.
      LV_FRCNT = IS_CFG-FIELD4.
      LV_TOCNT = IS_CFG-FIELD2.
      LV_TOCNT = LV_TOCNT - 1.

      PERFORM F_RP_CALC_DATE_IN_INTERVAL:  USING LV_DATE '00' LV_FRCNT '00' '+'
                                        CHANGING LV_CFDATE,
                                           USING LV_DATE '00' LV_TOCNT '00' '+'
                                       CHANGING LV_CTDATE.

      _G_APPEND_VALUE : S_SPMON 'BT' 'I' LV_CFDATE(06) LV_CTDATE(06).
  ENDCASE.

ENDFORM.
