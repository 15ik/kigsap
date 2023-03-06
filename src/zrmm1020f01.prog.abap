*&---------------------------------------------------------------------*
*& Include          ZRMM1020F01
*&---------------------------------------------------------------------*
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
  GS_VARIANT-REPORT   = SY-CPROG.
  GS_VARIANT-USERNAME = SY-UNAME.

*-
  CLEAR: S_CHKDT.


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

  DATA: LV_WERKS TYPE WERKS_D.

  CHECK S_WERKS[] IS NOT INITIAL.
*-
  SELECT FROM T001W FIELDS WERKS WHERE WERKS IN @S_WERKS INTO TABLE @DATA(LT_WERKS).

  LOOP AT LT_WERKS INTO LV_WERKS.
*--
    AUTHORITY-CHECK OBJECT 'M_MATE_WRK' FOR USER SY-UNAME
              ID 'ACTVT' FIELD '03'
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
  CLEAR LV_PRG. "U2

*-
  _G_INIT: GT_MAIN, GT_DISP.

*- Main
  PERFORM GET_MAIN_DATA.

*- BOM, 생산버젼
  PERFORM GET_BOM_VER_INIT.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_MAIN_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM GET_MAIN_DATA .

*-
  SELECT FROM V_MARC_MD AS A INNER JOIN MARA   AS B
                                     ON B~MATNR = A~MATNR
                                    AND A~SOBSL = @GC_SOBSL_30
                             INNER JOIN MAKT   AS C
                                     ON C~MATNR = B~MATNR
                                    AND C~SPRAS = @SY-LANGU
                        LEFT OUTER JOIN T023T  AS D
                                     ON D~MATKL = B~MATKL
                                    AND D~SPRAS = @SY-LANGU
                        LEFT OUTER JOIN T024   AS E
                                     ON E~EKGRP = A~EKGRP
                        LEFT OUTER JOIN  T141T AS F
                                     ON F~MMSTA = A~MMSTA
                                    AND F~SPRAS = @SY-LANGU
                        LEFT OUTER JOIN T024D  AS G
                                     ON G~WERKS = A~WERKS
                                    AND G~DISPO = A~DISPO

         FIELDS
         B~MATKL, B~MATNR,
         D~WGBEZ, C~MAKTX, A~MMSTA, F~MTSTB, B~LAEDA, A~WERKS, A~DISPO, G~DSNAM,
         A~EKGRP, E~EKNAM, A~BESKZ, A~SOBSL

         WHERE A~MATNR IN @S_MATNR
           AND A~WERKS IN @S_WERKS
           AND A~DISPO IN @S_DISPO
           AND A~EKGRP IN @S_EKGRP
           AND A~MMSTA IN @S_MMSTA
           AND A~LVORM NE 'X'
           AND B~MATKL IN @S_MATKL
           AND B~LAEDA IN @S_LAEDA
           AND B~LVORM NE 'X'
  INTO CORRESPONDING FIELDS OF TABLE @GT_MAIN.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_BOM_VER_INIT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM GET_BOM_VER_INIT.

*- BOM / 생산버젼
  _G_INIT: GT_BOM_VER_KEY.
  DATA(LT_MAIN) = GT_MAIN[].
  GT_BOM_VER_KEY[] = CORRESPONDING #( LT_MAIN DISCARDING DUPLICATES
                                         MAPPING MATNR = MATNR
                                                 WERKS = WERKS ).
  PERFORM GET_BOM_VER.

*- Info
  _G_INIT: GT_INFO_KEY.
  LT_MAIN[] = GT_MAIN[].
  GT_INFO_KEY[] = CORRESPONDING #( LT_MAIN DISCARDING DUPLICATES
                                      MAPPING MATNR = MATNR
                                              WERKS = WERKS ).
  PERFORM GET_INFO.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_BOM_VER
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM GET_BOM_VER.

  _G_INIT: GT_OTHERS-BOM, GT_OTHERS-VER.

  IF GT_BOM_VER_KEY[] IS NOT INITIAL.
    SELECT FROM MAST AS A INNER JOIN STKO AS B
                             ON A~STLNR = B~STLNR
                            AND A~STLAL = B~STLAL
           FIELDS
           A~MATNR, A~WERKS, A~STLNR, B~DATUV

           FOR ALL ENTRIES IN @GT_BOM_VER_KEY
           WHERE A~MATNR = @GT_BOM_VER_KEY-MATNR
             AND A~WERKS = @GT_BOM_VER_KEY-WERKS
             AND B~DATUV <= @S_CHKDT-LOW
             AND A~STLAN = 1  "생산용
             AND B~STLTY = 'M'
             AND B~LKENZ EQ @SPACE  "삭제 지시자
             AND B~LOEKZ EQ @SPACE  "BOM 삭제표시
   INTO CORRESPONDING FIELDS OF TABLE @GT_OTHERS-BOM.

*--
    SELECT FROM MKAL
           FIELDS
           MATNR, WERKS, VERID, TEXT1
           FOR ALL ENTRIES IN @GT_BOM_VER_KEY
           WHERE MATNR = @GT_BOM_VER_KEY-MATNR
             AND WERKS = @GT_BOM_VER_KEY-WERKS
             AND ( ADATU <= @S_CHKDT-LOW AND BDATU >= @S_CHKDT-LOW )
             AND (    VERID LIKE @TEXT-O01   "'S%'
                   OR VERID LIKE @TEXT-O02 ) "'A%'
             AND MKSP EQ @SPACE "잠금처리 제외
   INTO CORRESPONDING FIELDS OF TABLE @GT_OTHERS-VER.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_INFO
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM GET_INFO.

  DATA: LT_INFO TYPE TABLE OF TY_INFO.

*-
  _G_INIT: GT_OTHERS-INFO.

*-
  IF GT_INFO_KEY[] IS NOT INITIAL.
    SELECT FROM EINA AS A INNER JOIN EINE AS B
                             ON A~INFNR = B~INFNR
                          LEFT OUTER JOIN ZSVBMMINFOPRICE AS C
                             ON A~INFNR = C~INFNR

           FIELDS
           A~MATNR, A~INFNR, C~LIFNR, C~NAME1,
           C~PURCHASINGORGANIZATION       AS EKORG,
           C~PURCHASINGINFORECORDCATEGORY AS ESOKZ,
           C~MATERIALDESCRIPTION          AS MATKX,
           C~PLANT                        AS WERKS,
           C~VERID

           FOR ALL ENTRIES IN @GT_INFO_KEY
           WHERE A~MATNR = @GT_INFO_KEY-MATNR
             AND A~LOEKZ = @SPACE
             AND B~LOEKZ = @SPACE
             AND C~PURCHASINGINFORECORDCATEGORY = '3'  "외주임가공
             AND C~PURCHASINGORGANIZATION IN ( SELECT EKORG FROM T024W WHERE WERKS = @GT_INFO_KEY-WERKS )
             AND C~VALIDPRICE IN @GR_VALIDPRICE " U2 'Y'
*             AND C~CONDITIONVALIDITYENDDATE >= @SY-DATLO "현재일자 기준 유효한 단가조회
             AND C~EINA_LOEKZ = @SPACE
             AND C~EINE_LOEKZ = @SPACE
    INTO CORRESPONDING FIELDS OF TABLE @GT_OTHERS-INFO.

  ENDIF.

*-
  TYPES: BEGIN OF LTY_EINE,
           INFNR TYPE EINA-INFNR,
           WERKS TYPE ZSVBMMINFOPRICE-PLANT,
           VERID TYPE ZSVBMMINFOPRICE-VERID,
         END OF LTY_EINE.
  DATA: LT_EINE TYPE STANDARD TABLE OF LTY_EINE.

  LT_EINE[] = CORRESPONDING #( GT_OTHERS-INFO DISCARDING DUPLICATES
                                         MAPPING INFNR = INFNR
                                                 WERKS = WERKS
                                                 VERID = VERID ).
  SORT LT_EINE BY INFNR.
  DELETE ADJACENT DUPLICATES FROM LT_EINE COMPARING INFNR.

  IF LT_EINE[] IS NOT INITIAL.
    SELECT FROM EINA AS A INNER JOIN EINE AS B
                             ON A~INFNR = B~INFNR
           FIELDS
           A~INFNR, A~MATNR, B~WERKS, B~VERID
           FOR ALL ENTRIES IN @LT_EINE
           WHERE A~INFNR = @LT_EINE-INFNR
             AND B~ESOKZ = '3'
             AND B~WERKS IN @S_WERKS
    INTO TABLE @DATA(LT_INFNR).
  ENDIF.
  FREE LT_EINE.

  DELETE LT_INFNR WHERE WERKS IS INITIAL AND VERID IS INITIAL.
  SORT LT_INFNR BY MATNR INFNR.

*-
  TYPES: BEGIN OF LTY_INFO,
           MATNR TYPE MARD-MATNR, "자재
           INFNR TYPE EINA-INFNR,
           LIFNR TYPE ZSVBMMINFOPRICE-LIFNR,
           NAME1 TYPE ZSVBMMINFOPRICE-NAME1,
           EKORG TYPE ZSVBMMINFOPRICE-PURCHASINGORGANIZATION,
           ESOKZ TYPE ZSVBMMINFOPRICE-PURCHASINGINFORECORDCATEGORY,
           MATKX TYPE ZSVBMMINFOPRICE-MATERIALDESCRIPTION,
           WERKS TYPE ZSVBMMINFOPRICE-PLANT,
           VERID TYPE ZSVBMMINFOPRICE-VERID,
           APLYN TYPE CHAR1,
         END OF LTY_INFO.
  DATA: LT_INFO_B TYPE STANDARD TABLE OF LTY_INFO,
        LT_INFO_C TYPE STANDARD TABLE OF LTY_INFO.

  DATA: LV_INFNR_EXIST TYPE C.
  _G_INIT: LT_INFO_B, LT_INFO_C. CLEAR LV_INFNR_EXIST.
  LT_INFO_B[] = VALUE #( BASE LT_INFO_B[] FOR LS_VALUE IN GT_OTHERS-INFO
                        ( CORRESPONDING LTY_INFO( LS_VALUE ) ) ).

  LOOP AT LT_INFO_B INTO DATA(LS_INFO) WHERE WERKS IS INITIAL AND VERID IS INITIAL.
**    DATA(LV_TABIX) = SY-TABIX.

**    READ TABLE LT_INFNR INTO DATA(LS_INFNR) WITH KEY MATNR = LS_INFO-MATNR
**                                                     INFNR = LS_INFO-INFNR BINARY SEARCH.
**    IF SY-SUBRC = 0.
**      LS_INFO-WERKS = LS_INFNR-WERKS.
**      LS_INFO-VERID = LS_INFNR-VERID.
**      MODIFY LT_INFO_B FROM LS_INFO INDEX LV_TABIX.
**    ENDIF.
    CLEAR LV_INFNR_EXIST.
    LOOP AT LT_INFNR INTO DATA(LS_INFNR) WHERE MATNR = LS_INFO-MATNR
                                           AND INFNR = LS_INFO-INFNR.
      LS_INFO-WERKS = LS_INFNR-WERKS.
      LS_INFO-VERID = LS_INFNR-VERID.
      APPEND LS_INFO TO LT_INFO_C. LV_INFNR_EXIST = 'X'.
    ENDLOOP.

    IF LV_INFNR_EXIST IS INITIAL.
      APPEND LS_INFO TO LT_INFO_C.
    ENDIF.

  ENDLOOP.

*  SORT LT_INFO_B BY MATNR INFNR WERKS VERID. DELETE ADJACENT DUPLICATES FROM LT_INFO_B COMPARING MATNR INFNR WERKS VERID.
  SORT LT_INFO_C BY MATNR INFNR WERKS VERID. DELETE ADJACENT DUPLICATES FROM LT_INFO_C COMPARING MATNR INFNR WERKS VERID.

  CLEAR: GT_OTHERS-INFO[].
  GT_OTHERS-INFO[] = LT_INFO_C[]. "LT_INFO_B[].

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

  DATA: LS_DISP       TYPE TS_DISP,
        LT_INFO       TYPE TABLE OF TY_INFO,
        LT_INFO_S     TYPE TABLE OF TY_INFO,
        LT_INFO_APLYN TYPE TABLE OF TY_INFO,
        LT_VER        TYPE TABLE OF TY_VER.

*-
  LOOP AT GT_MAIN INTO DATA(LS_MAIN).

*-
    MOVE-CORRESPONDING LS_MAIN TO LS_DISP.
    "BOM
    LS_DISP-BOM_ID = SWITCH #( VALUE #( GT_OTHERS-BOM[ MATNR = LS_MAIN-MATNR
                                                WERKS = LS_MAIN-WERKS ]-STLNR OPTIONAL )
                                              WHEN SPACE THEN ICON_INCOMPLETE
                                              ELSE            ICON_LED_GREEN ).
    "생산버젼
    _G_INIT: LT_VER.
    LT_VER[] = VALUE #( BASE LT_VER FOR LS_VER IN GT_OTHERS-VER
                            WHERE ( MATNR = LS_MAIN-MATNR AND
                                    WERKS = LS_MAIN-WERKS )
                                  ( LS_VER ) ).
    DATA(LV_VER_COUNT) = LINES( LT_VER ).
    IF LV_VER_COUNT > 1.
      LS_DISP-VERID = '*'.
    ELSE.
      LS_DISP-VERID  = VALUE #( GT_OTHERS-VER[ MATNR = LS_MAIN-MATNR
                                               WERKS = LS_MAIN-WERKS ]-VERID OPTIONAL ).
    ENDIF.

    LS_DISP-VER_ID = SWITCH #( VALUE #( GT_OTHERS-VER[ MATNR = LS_MAIN-MATNR
                                             WERKS = LS_MAIN-WERKS ]-VERID OPTIONAL )
                                             WHEN SPACE THEN ICON_INCOMPLETE
                                               ELSE          ICON_LED_GREEN ).
    "INFO
    LS_DISP-INFO_ID = SWITCH #( VALUE #( GT_OTHERS-INFO[ MATNR = LS_MAIN-MATNR ]-INFNR OPTIONAL )
                                               WHEN SPACE THEN ICON_INCOMPLETE
                                               ELSE            ICON_LED_GREEN ).


*--
    _G_INIT: LT_INFO, LT_INFO_S.
    LT_INFO[] = VALUE #( BASE LT_INFO FOR LS_WA IN GT_OTHERS-INFO
                            WHERE ( MATNR = LS_MAIN-MATNR )
                                  ( LS_WA ) ).

**    DATA(LV_COUNT) = LINES( LT_INFO ).
***************************************************************************************
    LOOP AT LT_INFO ASSIGNING FIELD-SYMBOL(<LS_INFO>).
      DATA(LV_MAIN_TABIX) = SY-TABIX.
      _G_INIT: LT_INFO_S.
      LT_INFO_S[] = VALUE #( BASE LT_INFO_S FOR LS_WA IN GT_OTHERS-INFO
                             WHERE ( INFNR = <LS_INFO>-INFNR )
                                   ( LS_WA ) ).
      DATA(LV_INF_COUNT) = LINES( LT_INFO_S ).
      IF LV_INF_COUNT > 1.
        IF <LS_INFO>-WERKS NE LS_MAIN-WERKS.
          DELETE LT_INFO INDEX LV_MAIN_TABIX.
        ENDIF.
      ELSE.
        IF <LS_INFO>-WERKS NE LS_MAIN-WERKS.
          CLEAR <LS_INFO>-VERID.
        ENDIF.
      ENDIF.

    ENDLOOP.
    DATA(LV_COUNT) = LINES( LT_INFO ).
***************************************************************************************
    IF LV_COUNT > 1.
      LS_DISP-INFNR = LS_DISP-LIFNR = LS_DISP-NAME1 = LS_DISP-EKORG = LS_DISP-ESOKZ = '*'.

    ELSE.
      READ TABLE GT_OTHERS-INFO INTO DATA(LS_INFO) WITH KEY MATNR = LS_MAIN-MATNR BINARY SEARCH.
      IF SY-SUBRC = 0.
        LS_DISP-INFNR = LS_INFO-INFNR.
        LS_DISP-LIFNR = LS_INFO-LIFNR.
        LS_DISP-NAME1 = LS_INFO-NAME1.
        LS_DISP-EKORG = LS_INFO-EKORG.
        LS_DISP-ESOKZ = LS_INFO-ESOKZ.
      ENDIF.
    ENDIF.

*- 적용여부
    IF LS_DISP-VERID = '*'.
      _G_INIT: LT_INFO_APLYN.
      LT_INFO_APLYN[] = VALUE #( BASE LT_INFO_APLYN FOR LS_WA IN LT_INFO "U1 GT_OTHERS-INFO
                                WHERE ( MATNR = LS_MAIN-MATNR AND
**                                        WERKS = LS_MAIN-WERKS AND
                                        VERID = SPACE )
                                      ( LS_WA ) ).

      DATA(LV_COUNT_APLYN) = LINES( LT_INFO_APLYN ).
      IF LV_COUNT_APLYN = 0.
        LS_DISP-APLYN = 'Y'.
      ELSE.
*        IF LV_COUNT = LV_COUNT_APLYN.
*          LS_DISP-APLYN = 'Y'.
*        ELSE.
        LS_DISP-APLYN = 'N'.
*        ENDIF.
      ENDIF.

    ELSE.
      _G_INIT: LT_INFO_APLYN.
      LT_INFO_APLYN[] = VALUE #( BASE LT_INFO_APLYN FOR LS_WA IN GT_OTHERS-INFO
                                WHERE ( MATNR = LS_MAIN-MATNR AND
                                        WERKS = LS_MAIN-WERKS AND
                                        VERID = LS_DISP-VERID )
                                      ( LS_WA ) ).

      LV_COUNT_APLYN = LINES( LT_INFO_APLYN ).

      IF LV_COUNT = 0.
        LS_DISP-APLYN = 'N'.
      ELSE.
        IF LV_COUNT = LV_COUNT_APLYN.
          LS_DISP-APLYN = 'Y'.
        ELSE.
          LS_DISP-APLYN = 'N'.
        ENDIF.
      ENDIF.
    ENDIF.

*--
    "구매정보레코드에 생산버전 미 적용 자재만 조회
    IF P_CHK1 IS NOT INITIAL.
      IF NOT ( LS_DISP-VERID IS NOT INITIAL AND LS_DISP-APLYN = 'N' ).
        CONTINUE.
      ENDIF.
    ENDIF.

*--
    APPEND LS_DISP TO GT_DISP.

*--
    CLEAR: LS_MAIN, LS_DISP, LV_COUNT, LV_COUNT_APLYN.
  ENDLOOP.

*-
  SORT GT_DISP BY MATKL MATNR WERKS.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form BJOB_VER_INIT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM BJOB_VER_INIT .

  DATA : LT_SELIDX TYPE LVC_T_ROW,
         LS_SELIDX TYPE LVC_S_ROW.
  DATA: LV_RESULT_TXT TYPE NATXT.

  IF SY-BATCH IS INITIAL.
    DATA: LV_ANSWER.
    MESSAGE S012(ZMM01) INTO DATA(LV_TEXT1). "진행하시겠습니까?

    PERFORM CHECK_POPUP_TO_CONFIRM USING TEXT-T02 LV_TEXT1 "백그라운드 JOB
                                CHANGING LV_ANSWER.

    CHECK LV_ANSWER = 1.
  ENDIF.

*-
  _G_INIT: LT_SELIDX.

*-
  LOOP AT GT_DISP INTO DATA(LS_DISP).
    IF ( LS_DISP-VERID IS NOT INITIAL AND LS_DISP-INFNR IS NOT INITIAL AND LS_DISP-APLYN = 'N' )
       AND NOT ( LS_DISP-VERID = '*' OR LS_DISP-INFNR = '*' ).  "생산버젼이나 정보레코드 멀티인거 제외(수작업필요)

      LS_SELIDX-INDEX = SY-TABIX.
      APPEND LS_SELIDX TO LT_SELIDX.
    ENDIF.
  ENDLOOP.

*-
  IF LT_SELIDX[] IS INITIAL.
    MESSAGE S021(ZMM01) INTO LV_RESULT_TXT.

  ELSE.
*--
    PERFORM PROC_VER_INIT TABLES LT_SELIDX USING 'X'.

*--
    IF GV_ECNT IS INITIAL.
      MESSAGE I038(ZMM01) WITH GV_TCNT TEXT-T04 INTO LV_RESULT_TXT. "전체 &1 건이 &2 되었습니다.
    ELSE.
      MESSAGE I037(ZMM01) WITH GV_ECNT INTO LV_RESULT_TXT.
    ENDIF.

  ENDIF.

*-
  WRITE : /03 TEXT-C34, LV_RESULT_TXT.  "U2
  CHECK LT_SELIDX[] IS NOT INITIAL.
  ULINE AT /0(80).

  LOOP AT LT_SELIDX INTO DATA(LV_SELIDX).
    READ TABLE GT_DISP INTO LS_DISP INDEX LV_SELIDX-INDEX.
    IF SY-SUBRC = 0.
      IF LS_DISP-MESSAGE IS NOT INITIAL.
        DATA(LV_ERR_TXT) = LS_DISP-MESSAGE.
      ENDIF.

      WRITE: /03 LS_DISP-MATNR, LS_DISP-WERKS, LS_DISP-VERID, LV_ERR_TXT.
      CLEAR LV_ERR_TXT.
    ENDIF.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CHECK_POPUP_TO_CONFIRM
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> TEXT_T02
*&      --> LV_TEXT1
*&      <-- LV_ANSWER
*&---------------------------------------------------------------------*
FORM CHECK_POPUP_TO_CONFIRM USING IV_TITLE IV_TEXT
                          CHANGING EV_ANSWER.


* CALL CONFIRM STEP FUNCTION.
  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
      TITLEBAR              = IV_TITLE
      TEXT_QUESTION         = IV_TEXT
      DISPLAY_CANCEL_BUTTON = ' '
      TEXT_BUTTON_1         = 'Yes'
      ICON_BUTTON_1         = 'ICON_OKAY'
      TEXT_BUTTON_2         = 'No'
      ICON_BUTTON_2         = 'ICON_CANCEL'
    IMPORTING
      ANSWER                = EV_ANSWER
    EXCEPTIONS
      TEXT_NOT_FOUND        = 1.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form PROC_VER_INIT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LT_SELIDX
*&      --> P_
*&---------------------------------------------------------------------*
FORM PROC_VER_INIT TABLES IT_SELIDX TYPE LVC_T_ROW
                    USING IV_BJOB.

  DATA: LT_INFO     TYPE TABLE OF TY_INFO,
        LS_INFO     LIKE LINE OF LT_INFO,
        LT_INFO_S   TYPE TABLE OF TY_INFO,
        LS_INFO_KEY LIKE LINE OF GT_INFO_KEY,
        LT_INFO_KEY TYPE TABLE OF TY_INFO_KEY.

*-

  CLEAR: GV_TCNT, GV_SCNT, GV_ECNT. _G_INIT: GT_INFO_KEY, LT_INFO_KEY.

  LOOP AT IT_SELIDX INTO DATA(LV_SELIDX).
    READ TABLE GT_DISP ASSIGNING FIELD-SYMBOL(<LS_DISP>) INDEX LV_SELIDX-INDEX.
    IF SY-SUBRC = 0.

      ADD 1 TO GV_TCNT.

      CLEAR: <LS_DISP>-MSG_MSGTB[].

************************************************************************************************
      IF <LS_DISP>-VERID = '*' OR <LS_DISP>-INFNR = '*'.

        DATA: LS_DET  TYPE TS_DET.

        _G_INIT : GT_DET, LT_INFO, GT_VERID. CLEAR GV_MULTI_VER.

        LT_INFO[] = VALUE #( BASE LT_INFO FOR LS_WA IN GT_OTHERS-INFO
                                WHERE ( MATNR = <LS_DISP>-MATNR )
                                      ( LS_WA ) ).

***************************************************************************************
        LOOP AT LT_INFO ASSIGNING FIELD-SYMBOL(<LS_INFO>).
          DATA(LV_MAIN_TABIX) = SY-TABIX.
          _G_INIT: LT_INFO_S.
          LT_INFO_S[] = VALUE #( BASE LT_INFO_S FOR LS_WA IN GT_OTHERS-INFO
                                 WHERE ( INFNR = <LS_INFO>-INFNR )
                                       ( LS_WA ) ).
          DATA(LV_INF_COUNT) = LINES( LT_INFO_S ).
          IF LV_INF_COUNT > 1.
            IF <LS_INFO>-WERKS NE <LS_DISP>-WERKS.
              DELETE LT_INFO INDEX LV_MAIN_TABIX.
            ENDIF.
          ELSE.
            IF <LS_INFO>-WERKS NE <LS_DISP>-WERKS.
              CLEAR <LS_INFO>-VERID.
            ENDIF.
          ENDIF.

        ENDLOOP.
***************************************************************************************

        LOOP AT LT_INFO INTO LS_INFO.
          MOVE-CORRESPONDING LS_INFO TO LS_DET.
          IF LS_DET-WERKS IS INITIAL.
            LS_DET-WERKS = <LS_DISP>-WERKS.
          ENDIF.

          IF <LS_DISP>-VERID = '*'.
            IF LS_DET-VERID IS INITIAL.
              LS_DET-APLYN = 'N'.
            ELSE.
              LS_DET-APLYN = 'Y'.
            ENDIF.
          ELSE.
            IF ( LS_DET-VERID <> <LS_DISP>-VERID ) OR
               ( LS_DET-WERKS <> <LS_DISP>-WERKS ).  "U1 ADD
              LS_DET-APLYN = 'N'.
              CLEAR LS_DET-VERID.  "U1 ADD
            ELSE.
              LS_DET-APLYN = 'Y'.
            ENDIF.
          ENDIF.

          LS_DET-SWERKS = <LS_DISP>-WERKS. "U1 ADD
          APPEND LS_DET TO GT_DET.
        ENDLOOP.

        IF GT_DET[] IS NOT INITIAL.
          IF <LS_DISP>-VERID = '*' OR <LS_DISP>-INFNR = '*'.
            GV_MULTI_VER = 'X'.
            GT_VERID[] = VALUE #( BASE GT_VERID FOR LS_VER IN GT_OTHERS-VER
                           WHERE ( MATNR = <LS_DISP>-MATNR AND
                                   WERKS = <LS_DISP>-WERKS )
                                 ( LS_VER ) ).
          ENDIF.

          CALL SCREEN '0200' STARTING AT 10 10 ENDING AT 120 20.
        ENDIF.
************************************************************************************************
      ELSE.
        IF <LS_DISP>-INFNR = '*'.
          _G_INIT: LT_INFO.
          LT_INFO[] = VALUE #( BASE LT_INFO FOR LS_WA IN GT_OTHERS-INFO
                                  WHERE ( MATNR = <LS_DISP>-MATNR AND
                                          ( WERKS NE <LS_DISP>-WERKS OR VERID NE <LS_DISP>-VERID ) )
                                        ( LS_WA ) ).

          READ TABLE LT_INFO INTO LS_INFO INDEX 1.
          IF SY-SUBRC = 0.
            DATA(LV_TABIX) = SY-TABIX.
            LOOP AT LT_INFO INTO LS_INFO FROM LV_TABIX.
              IF LS_INFO-MATNR NE <LS_DISP>-MATNR.
                EXIT.
              ELSE.
                PERFORM FM_ZFMM_INFORECORD_MAINTAIN USING LS_INFO IV_BJOB CHANGING <LS_DISP>.
              ENDIF.
            ENDLOOP.
          ENDIF.

        ELSE.
          MOVE-CORRESPONDING <LS_DISP> TO LS_INFO.
          PERFORM FM_ZFMM_INFORECORD_MAINTAIN USING LS_INFO IV_BJOB CHANGING <LS_DISP>.
        ENDIF.

*--
        LS_INFO_KEY-MATNR = <LS_DISP>-MATNR.
        LS_INFO_KEY-WERKS = <LS_DISP>-WERKS.
        APPEND LS_INFO_KEY TO LT_INFO_KEY.

      ENDIF.
    ENDIF.

  ENDLOOP.

*-
  GT_INFO_KEY[] = CORRESPONDING #( LT_INFO_KEY DISCARDING DUPLICATES
                                      MAPPING MATNR = MATNR
                                              WERKS = WERKS ).

ENDFORM.
*&---------------------------------------------------------------------*
*& Form FM_ZFMM_INFORECORD_MAINTAIN
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LS_INFO
*&      --> IV_BJOB
*&      <-- <LS_DISP>
*&---------------------------------------------------------------------*
FORM FM_ZFMM_INFORECORD_MAINTAIN USING IS_INFO TYPE TY_INFO
                                       IV_BJOB
                              CHANGING CS_DISP TYPE TS_DISP.

  DATA:
    LT_INFOR_EINA   LIKE TABLE OF ZSMM_INFOR_MAINT_EINA,
    LT_INFOR_EINAX  LIKE TABLE OF ZSMM_INFOR_MAINT_EINAX,
    LT_INFOR_EINE   LIKE TABLE OF ZSMM_INFOR_MAINT_EINE,
    LT_INFOR_EINEX  LIKE TABLE OF ZSMM_INFOR_MAINT_EINEX,
    LT_INFOR_RETURN TYPE TABLE OF FS4MIG_S_BAPIRET2,
    LT_RETURN       TYPE TABLE OF FS4MIG_S_BAPIRET2
                    WITH NON-UNIQUE SORTED KEY IDX_KEY COMPONENTS TYPE.

*-
  _G_INIT: LT_INFOR_EINA, LT_INFOR_EINAX, LT_INFOR_EINE, LT_INFOR_EINEX, LT_RETURN, LT_INFOR_RETURN.

*-
  LT_INFOR_EINA  = VALUE #( ( INFNR = IS_INFO-INFNR
                              MATNR = IS_INFO-MATNR
                              ELIFN = IS_INFO-LIFNR ) ).

  LT_INFOR_EINAX = VALUE #( ( INFNR = IS_INFO-INFNR
                              MATNR = 'X'
                              ELIFN = 'X' ) ).

  LT_INFOR_EINE   = VALUE #( ( INFNR = IS_INFO-INFNR
                               EKORG = IS_INFO-EKORG
                               ESOKZ = IS_INFO-ESOKZ
                               WERKS = CS_DISP-WERKS
                               BPUMZ = 1
                               BPUMN = 1
                               VERID = CS_DISP-VERID ) ).

  LT_INFOR_EINEX  = VALUE #( ( INFNR = IS_INFO-INFNR
                               EKORG = 'X'
                               ESOKZ = 'X'
                               WERKS = 'X'
                               BPUMZ = 'X'
                               BPUMN = 'X'
                               VERID = 'X' ) ).

*-
  call function 'ZFMM_INFORECORD_MAINTAIN'
    TABLES
      IT_EINA   = LT_INFOR_EINA
      IT_EINAX  = LT_INFOR_EINAX
      IT_EINE   = LT_INFOR_EINE
      IT_EINEX  = LT_INFOR_EINEX
      ET_RETURN = LT_INFOR_RETURN.

  LT_RETURN[] = LT_INFOR_RETURN[].

  READ TABLE LT_RETURN WITH TABLE KEY IDX_KEY COMPONENTS TYPE = 'E' TRANSPORTING NO FIELDS.
  IF SY-SUBRC = 0.
    LOOP AT LT_RETURN INTO DATA(LS_RETURN) FROM 1.
      CHECK LS_RETURN-TYPE = 'E'.
      CLEAR: GS_MSGTB.
      GS_MSGTB = VALUE #( FIELDNAME = CS_DISP-MATNR    MSGTY = 'E'  ARBGB = LS_RETURN-ID   TXTNR = LS_RETURN-NUMBER
                          MSGV1     = LS_RETURN-MESSAGE_V1   MSGV2 = LS_RETURN-MESSAGE_V2   MSGV3 = LS_RETURN-MESSAGE_V3  ).
      APPEND GS_MSGTB TO CS_DISP-MSG_MSGTB.

      IF CS_DISP-MESSAGE IS INITIAL.
        CS_DISP-MESSAGE = LS_RETURN-MESSAGE.
      ENDIF.

    ENDLOOP.
    DATA(LV_ERR_CNT) = LINES( CS_DISP-MSG_MSGTB[] ).
    IF LV_ERR_CNT > 1.
      CS_DISP-MSG = ICON_LIST.
    ELSE.
      CS_DISP-MSG = CS_DISP-MESSAGE.
    ENDIF.
    ADD 1 TO GV_ECNT.

  ELSE.
    CS_DISP-APLYN = 'Y'.
    ADD 1 TO GV_SCNT.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CHECK_EXIT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form CHECK_EXIT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM CHECK_EXIT .

  DATA:LV_ANS.

*--------------------------------
* 화면 OFF전 변경 데이타 확인
*--------------------------------
  PERFORM CHECK_CHANGED_DATA USING 'E' CHANGING LV_ANS.

  CHECK LV_ANS IS INITIAL.

  CASE GV_OK_CODE.
    WHEN 'EXIT'.
      LEAVE PROGRAM.
    WHEN OTHERS.
      LEAVE TO SCREEN 0.
  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CHECK_CHANGED_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> P_
*&      <-- LV_ANS
*&---------------------------------------------------------------------*
FORM CHECK_CHANGED_DATA USING IV_TYPE
                         CHANGING CV_ANS.

*  CHECK GRF_GRID->GET_STATUS( ) EQ ABAP_TRUE.
  READ TABLE GT_DISP WITH TABLE KEY IDX_STATU COMPONENTS STATU = ICON_CHANGE TRANSPORTING NO FIELDS.
  CHECK SY-SUBRC = 0.
  CASE IV_TYPE.
    WHEN 'E'.
      DATA(LV_MSG) = ZCL_CN_ALV_GRID=>AC_MSG_EXIT2.
    WHEN 'C'.
      LV_MSG = ZCL_CN_ALV_GRID=>AC_MSG_CONT.  "계속 하시겠습니까?
  ENDCASE.

  CV_ANS = 'X'.

  CHECK GRF_GRID->POP_TO_MSG( IV_TYPE  = 'A'
                              IV_TITLE = ZCL_CN_ALV_GRID=>AC_MSG_TITLE2   "'Exit Confirm'
                              IV_TEXT1 = ZCL_CN_ALV_GRID=>AC_MSG_EXIT     "'변경된 데이타가 있습니다'
                              IV_TEXT2 = LV_MSG )                         "'화면에서 Off 하시겠습니까?' )
                              EQ ABAP_TRUE. " YES

  CV_ANS = ''.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_CHANGE_MODE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- GV_MODE
*&---------------------------------------------------------------------*
FORM SET_CHANGE_MODE CHANGING CV_MODE.

  DATA:LV_ANS.

  IF CV_MODE = 'X'.  "Edit 모드일경우 ..
*--------------------------------
* 모드 변경전 변경 데이타 확인
*--------------------------------
    PERFORM CHECK_CHANGED_DATA USING 'C' CHANGING LV_ANS.

    CHECK LV_ANS IS INITIAL.
  ENDIF.

*-------------------------
*-- 최신 Data
*-------------------------
  PERFORM REFRESH_DATA.

*-----------------------------
*-- Mode 변경 (Display<->Edit)
*-----------------------------
  CALL METHOD GRF_GRID->SET_CHANGE_MODE( CHANGING CV_MODE = CV_MODE ).

ENDFORM.
*&---------------------------------------------------------------------*
*& Form BTN_VER_INIT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM BTN_VER_INIT .

  DATA: LV_TITLE    TYPE STRING,
        LS_INFO_KEY LIKE LINE OF GT_INFO_KEY,
        LT_INFO_KEY TYPE TABLE OF TY_INFO_KEY.

  DATA:
    LT_INFOR_EINA   LIKE TABLE OF ZSMM_INFOR_MAINT_EINA,
    LT_INFOR_EINAX  LIKE TABLE OF ZSMM_INFOR_MAINT_EINAX,
    LT_INFOR_EINE   LIKE TABLE OF ZSMM_INFOR_MAINT_EINE,
    LT_INFOR_EINEX  LIKE TABLE OF ZSMM_INFOR_MAINT_EINEX,
    LT_INFOR_RETURN TYPE TABLE OF FS4MIG_S_BAPIRET2,
    LT_RETURN       TYPE TABLE OF FS4MIG_S_BAPIRET2
                    WITH NON-UNIQUE SORTED KEY IDX_KEY COMPONENTS TYPE.

  DATA : LT_SELIDX  TYPE LVC_T_ROW.

*-
  CALL METHOD GRF_GRID_0200->GET_SELECTED_ROWS
    IMPORTING
      ET_INDEX_ROWS = LT_SELIDX.

  DELETE LT_SELIDX WHERE ROWTYPE IS NOT INITIAL.
  IF LT_SELIDX[] IS INITIAL.
    MESSAGE I006(ZMM01). EXIT.
  ENDIF.

*-
**  LOOP AT LT_SELIDX INTO DATA(LV_SELIDX).
**    READ TABLE GT_DET ASSIGNING FIELD-SYMBOL(<LS_DET>) INDEX LV_SELIDX-INDEX.
**    IF SY-SUBRC = 0.
**      IF <LS_DET>-VERID = SPACE.
****      READ TABLE GT_DET WITH TABLE KEY IDX_VERID COMPONENTS VERID = SPACE TRANSPORTING NO FIELDS.
****      IF SY-SUBRC = 0.
**        "생산 버전이 선택되어 있지 않는 라인이 포함되어 있습니다.
**        MESSAGE I000(ZMM01) WITH TEXT-M04. EXIT.
**      ENDIF.
**    ENDIF.
**  ENDLOOP.
**  UNASSIGN <LS_DET>.

*-
  LV_TITLE = TEXT-T04. "생산버젼 적용
  MESSAGE S012(ZMM01) INTO DATA(LV_TEXT1). "진행하시겠습니까?
  CHECK GRF_GRID->POP_TO_MSG( IV_TYPE  = 'A'
                              IV_TITLE = LV_TITLE
                              IV_TEXT1 = LV_TEXT1
                              IV_TEXT2 = '' )  EQ ABAP_TRUE. " YES

*-
  _G_INIT: GT_INFO_KEY, GV_MSGTB, LT_INFO_KEY.

*-
  LOOP AT LT_SELIDX INTO DATA(LV_SELIDX).
    READ TABLE GT_DET ASSIGNING FIELD-SYMBOL(<LS_DET>) INDEX LV_SELIDX-INDEX.
    IF SY-SUBRC = 0.
*      LOOP AT GT_DET ASSIGNING FIELD-SYMBOL(<LS_DET>).
**        CHECK <LS_DET>-APLYN = 'N'.

*****************************************************************************************************
*--
      DATA: LT_BAPIEINA LIKE STANDARD TABLE OF BAPIEINA,
            LT_BAPIEINE LIKE STANDARD TABLE OF BAPIEINE WITH NON-UNIQUE SORTED KEY IDX_KEY COMPONENTS PLANT.

      _G_INIT: LT_BAPIEINA, LT_BAPIEINE.

      CALL FUNCTION 'BAPI_INFORECORD_GETLIST'
        EXPORTING
          PLANT               = <LS_DET>-SWERKS
          MATERIAL            = <LS_DET>-MATNR(18)
          PURCHASINGINFOREC   = <LS_DET>-INFNR
          PURCH_ORG           = <LS_DET>-EKORG
          INFO_TYPE           = <LS_DET>-ESOKZ
        TABLES
          INFORECORD_GENERAL  = LT_BAPIEINA
          INFORECORD_PURCHORG = LT_BAPIEINE.


      READ TABLE LT_BAPIEINE INTO DATA(LS_BAPIEINE) WITH TABLE KEY IDX_KEY COMPONENTS PLANT = <LS_DET>-SWERKS.
      IF SY-SUBRC = 0.
        DATA: LT_MSG TYPE STANDARD TABLE OF BDCMSGCOLL WITH NON-UNIQUE SORTED KEY IDX_KEY COMPONENTS MSGTYP.
        PERFORM CHG_ME12_BDC_INIT TABLES LT_MSG CHANGING <LS_DET>.

        READ TABLE LT_MSG INTO DATA(LS_MSG) WITH TABLE KEY IDX_KEY COMPONENTS MSGTYP = 'E'.
        IF SY-SUBRC EQ 0.
          DATA: LV_MSGV1 TYPE SYMSGV.
          CALL FUNCTION 'MESSAGE_TEXT_BUILD'
            EXPORTING
              MSGID               = LS_MSG-MSGID
              MSGNR               = '002'
              MSGV1               = LS_MSG-MSGV1
              MSGV2               = LS_MSG-MSGV2
              MSGV3               = LS_MSG-MSGV3
              MSGV4               = LS_MSG-MSGV4
            IMPORTING
              MESSAGE_TEXT_OUTPUT = LV_MSGV1.

          GS_MSGTB = VALUE #( FIELDNAME = <LS_DET>-VERID   MSGTY = 'E'  ARBGB = TEXT-O03   TXTNR = '002'
                              MSGV1     = LV_MSGV1 MSGV2 = LS_MSG-MSGV2 MSGV3 = LS_MSG-MSGV3 ).
          APPEND GS_MSGTB TO GV_MSGTB.
          CONTINUE.

        ELSE.
          IF <LS_DET>-VERID IS INITIAL.
            <LS_DET>-APLYN = 'N'.
          ELSE.
            <LS_DET>-APLYN = 'Y'.
          ENDIF.

        ENDIF.

*****************************************************************************************************
      ELSE.
*--
        _G_INIT: LT_INFOR_EINA, LT_INFOR_EINAX, LT_INFOR_EINE, LT_INFOR_EINEX, LT_RETURN, LT_INFOR_RETURN.

*--
        LT_INFOR_EINA  = VALUE #( ( INFNR = <LS_DET>-INFNR
                                    MATNR = <LS_DET>-MATNR
                                    ELIFN = <LS_DET>-LIFNR ) ).

        LT_INFOR_EINAX = VALUE #( ( INFNR = <LS_DET>-INFNR
                                    MATNR = 'X'
                                    ELIFN = 'X' ) ).

        LT_INFOR_EINE   = VALUE #( ( INFNR = <LS_DET>-INFNR
                                     EKORG = <LS_DET>-EKORG
                                     ESOKZ = <LS_DET>-ESOKZ
                                     WERKS = <LS_DET>-SWERKS "U1 최초선택된 플랜트 적용
                                     BPUMZ = 1
                                     BPUMN = 1
                                     VERID = <LS_DET>-VERID ) ).

        LT_INFOR_EINEX  = VALUE #( ( INFNR = <LS_DET>-INFNR
                                     EKORG = 'X'
                                     ESOKZ = 'X'
                                     WERKS = 'X'
                                     BPUMZ = 'X'
                                     BPUMN = 'X'
                                     VERID = 'X' ) ).

*--
        call function 'ZFMM_INFORECORD_MAINTAIN'
          TABLES
            IT_EINA   = LT_INFOR_EINA
            IT_EINAX  = LT_INFOR_EINAX
            IT_EINE   = LT_INFOR_EINE
            IT_EINEX  = LT_INFOR_EINEX
            ET_RETURN = LT_INFOR_RETURN.

        LT_RETURN[] = LT_INFOR_RETURN[].

*--

        READ TABLE LT_RETURN WITH TABLE KEY IDX_KEY COMPONENTS TYPE = 'E' TRANSPORTING NO FIELDS.
        IF SY-SUBRC = 0.
          LOOP AT LT_RETURN INTO DATA(LS_RETURN) FROM 1.
            CHECK LS_RETURN-TYPE = 'E'.
            CLEAR: GS_MSGTB.
            GS_MSGTB = VALUE #( FIELDNAME = <LS_DET>-VERID   MSGTY = 'E'  ARBGB = TEXT-O03   TXTNR = '002'
                                MSGV1     = LS_RETURN-MESSAGE_V1 MSGV2 = LS_RETURN-MESSAGE_V2 MSGV3 = LS_RETURN-MESSAGE_V3 ).
            APPEND GS_MSGTB TO GV_MSGTB.
            CONTINUE.

          ENDLOOP.

          CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
        ELSE.
          IF <LS_DET>-VERID IS INITIAL.
            <LS_DET>-APLYN = 'N'.
          ELSE.
            <LS_DET>-APLYN = 'Y'.
          ENDIF.
**        READ TABLE GT_DISP ASSIGNING FIELD-SYMBOL(<LS_DISP>) WITH TABLE KEY IDX_KEY
**                                                             COMPONENTS MATNR = <LS_DET>-MATNR
**                                                                        WERKS = <LS_DET>-WERKS.
**        IF <LS_DISP> IS ASSIGNED.
**          <LS_DISP>-APLYN = 'Y'.
**        ENDIF.
          CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
            EXPORTING
              WAIT = 'X'.
        ENDIF.

      ENDIF.


*--
      LS_INFO_KEY-MATNR = <LS_DET>-MATNR.
      LS_INFO_KEY-WERKS = <LS_DET>-SWERKS.

*      ENDLOOP.
    ENDIF.

  ENDLOOP.

*-
  READ TABLE GT_DISP ASSIGNING FIELD-SYMBOL(<LS_DISP>) WITH TABLE KEY IDX_KEY
                                                       COMPONENTS MATNR = <LS_DET>-MATNR
                                                                  WERKS = <LS_DET>-SWERKS.
  IF <LS_DISP> IS ASSIGNED.
    READ TABLE GT_DET WITH TABLE KEY IDX_VERID COMPONENTS VERID = SPACE TRANSPORTING NO FIELDS.
    IF SY-SUBRC = 0.
      <LS_DISP>-APLYN = 'N'.
    ELSE.
      <LS_DISP>-APLYN = 'Y'.
    ENDIF.
  ENDIF.

*--
  APPEND LS_INFO_KEY TO LT_INFO_KEY.
*-
  GT_INFO_KEY[] = CORRESPONDING #( LT_INFO_KEY DISCARDING DUPLICATES
                                      MAPPING MATNR = MATNR
                                              WERKS = WERKS ).

*-
  WAIT UP TO 1 SECONDS.

*-
  PERFORM RESULT_SCREEN_100.

*-
  GRF_GRID_0200->REFRESH_GRID_DISPLAY( ).

*-
  GRF_GRID->REFRESH_GRID_DISPLAY( ).

*-
  IF GV_MSGTB IS NOT INITIAL.
    GRF_GRID_0200->SHOW_MSGTB( IT_MSGTB = GV_MSGTB ). EXIT.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CHG_ME12_BDC_INIT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LT_MSG
*&      <-- <LS_DET>
*&---------------------------------------------------------------------*
FORM CHG_ME12_BDC_INIT TABLES CT_MSG STRUCTURE BDCMSGCOLL
                     CHANGING CS_DET LIKE LINE OF GT_DET.


  DATA : LT_BDC TYPE STANDARD TABLE OF BDCDATA,
         LS_BDC LIKE LINE OF LT_BDC,
         LT_MSG TYPE STANDARD TABLE OF BDCMSGCOLL.

  _G_INIT: LT_BDC, LT_MSG, CT_MSG.
* First Screen - Enter Material,Vendor,Purch Org,Plant & Inf Rec Cat.
  CLEAR LS_BDC.
  WRITE : 'SAPMM06I' TO LS_BDC-PROGRAM,
          '0100'     TO LS_BDC-DYNPRO,
          'X'        TO LS_BDC-DYNBEGIN.
  APPEND LS_BDC TO LT_BDC.

  CLEAR LS_BDC.
  WRITE : 'EINA-LIFNR' TO LS_BDC-FNAM.
  LS_BDC-FVAL      = CS_DET-LIFNR.
  APPEND LS_BDC TO LT_BDC.

  CLEAR LS_BDC.
  WRITE : 'EINA-MATNR' TO LS_BDC-FNAM.
  LS_BDC-FVAL      = CS_DET-MATNR.
  APPEND LS_BDC TO LT_BDC.

  CLEAR LS_BDC.
  WRITE : 'EINE-EKORG' TO LS_BDC-FNAM.
  LS_BDC-FVAL      = CS_DET-EKORG.
  APPEND LS_BDC TO LT_BDC.

  CLEAR LS_BDC.
  WRITE : 'EINE-WERKS' TO LS_BDC-FNAM.
  LS_BDC-FVAL      = CS_DET-SWERKS.
  APPEND LS_BDC TO LT_BDC.

  CLEAR LS_BDC.
  WRITE : 'EINA-INFNR' TO LS_BDC-FNAM.
  LS_BDC-FVAL      = CS_DET-INFNR.
  APPEND LS_BDC TO LT_BDC.

  CLEAR LS_BDC.
  WRITE : 'RM06I-LOHNB' TO LS_BDC-FNAM.
  LS_BDC-FVAL = 'X'.
  APPEND LS_BDC TO LT_BDC.

  CLEAR LS_BDC.
  WRITE : 'BDC_OKCODE' TO LS_BDC-FNAM,
          '/00'        TO LS_BDC-FVAL.
  APPEND LS_BDC TO LT_BDC.

* Second Screen - Go To Purch Org Data 1.
  CLEAR LS_BDC.
  WRITE : 'SAPMM06I' TO LS_BDC-PROGRAM,
          '0101'     TO LS_BDC-DYNPRO,
          'X'        TO LS_BDC-DYNBEGIN.
  APPEND LS_BDC TO LT_BDC.

  CLEAR LS_BDC.
  WRITE : 'BDC_OKCODE' TO LS_BDC-FNAM,
          '=EINE'      TO LS_BDC-FVAL.
  APPEND LS_BDC TO LT_BDC.

*
  CLEAR LS_BDC.
  WRITE : 'SAPMM06I' TO LS_BDC-PROGRAM,
          '0102'     TO LS_BDC-DYNPRO,
          'X'        TO LS_BDC-DYNBEGIN.
  APPEND LS_BDC TO LT_BDC.

  CLEAR LS_BDC.
  WRITE : 'EINE-VERID' TO LS_BDC-FNAM.
  LS_BDC-FVAL      = CS_DET-VERID.
  APPEND LS_BDC TO LT_BDC.

  CLEAR LS_BDC.
  WRITE : 'BDC_OKCODE' TO LS_BDC-FNAM,
          '=BU'        TO LS_BDC-FVAL.
  APPEND LS_BDC TO LT_BDC.

* Call ME12 for updation work.
  CALL TRANSACTION 'ME12' USING LT_BDC
                          MODE  'N'
                          MESSAGES INTO LT_MSG.

  CT_MSG[] = LT_MSG[].

ENDFORM.
*&---------------------------------------------------------------------*
*& Form REFRESH_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM REFRESH_DATA .

  PERFORM GET_DATA.

  PERFORM PROCESSING_DATA.

ENDFORM.
