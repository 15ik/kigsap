*&---------------------------------------------------------------------*
*& Include          ZRMM1020F02
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form EVT_GRID_0200_DATA_CHANGED
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> IRF_DATA_CHANGED
*&      --> IV_ONF4
*&      --> IV_ONF4_BEFORE
*&      --> IV_ONF4_AFTER
*&      --> IV_UCOMM
*&---------------------------------------------------------------------*
FORM EVT_GRID_0200_DATA_CHANGED USING IRF_DATA_CHANGED
                                  TYPE REF TO CL_ALV_CHANGED_DATA_PROTOCOL
                                  IV_ONF4
                                  IV_ONF4_BEFORE
                                  IV_ONF4_AFTER
                                  IV_UCOMM TYPE  SY-UCOMM.

  DATA: LS_ALV_EMSG TYPE TY_ALV_EMSG.

  LOOP AT IRF_DATA_CHANGED->MT_GOOD_CELLS INTO DATA(LS_LVC_MODI).

    READ TABLE GT_DET INTO DATA(LS_DET) INDEX LS_LVC_MODI-ROW_ID.
  ENDLOOP.
*-----------------------------------------------------------------
* 컬럼별 세팅 (Check_changed_컬럼명 으로 구분하여 네이밍룰 생성)
*-----------------------------------------------------------------
  CASE LS_LVC_MODI-FIELDNAME.
    WHEN 'VERID'.
      PERFORM CHECK_CHANGED_VERID  USING IRF_DATA_CHANGED
                                         LS_LVC_MODI
                                 CHANGING LS_DET LS_ALV_EMSG.

    WHEN OTHERS.
      PERFORM CHECK_CHANGED_OTHERS USING    IRF_DATA_CHANGED
                                            LS_LVC_MODI
                                   CHANGING LS_DET.
  ENDCASE.

*-
  IF LS_ALV_EMSG IS NOT INITIAL.
    PERFORM ADD_ALV_ERROR_MSG USING IRF_DATA_CHANGED LS_ALV_EMSG.
  ENDIF.

*  CALL METHOD cl_gui_cfw=>set_new_ok_code( 'OK' ).

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CHECK_CHANGED_VERID
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> IRF_DATA_CHANGED
*&      --> LS_LVC_MODI
*&      <-- LS_DET
*&      <-- LS_ALV_EMSG
*&---------------------------------------------------------------------*
FORM CHECK_CHANGED_VERID USING IRF_DATA_CHANGED TYPE REF TO CL_ALV_CHANGED_DATA_PROTOCOL
                                 IS_CELL TYPE LVC_S_MODI
                        CHANGING CS_DET TYPE TS_DET
                                 ES_ALV_EMSG TYPE TY_ALV_EMSG.

*-
  DATA: LV_VERID TYPE VERID.


  CLEAR : ES_ALV_EMSG, LV_VERID.

  IF IS_CELL-VALUE IS NOT INITIAL.
    LV_VERID = IS_CELL-VALUE. CONDENSE LV_VERID.

    READ TABLE GT_VERID WITH TABLE KEY IDX_KEY COMPONENTS VERID = LV_VERID TRANSPORTING NO FIELDS.

    IF SY-SUBRC <> 0.
      ES_ALV_EMSG-ROW_ID    = IS_CELL-ROW_ID.
      ES_ALV_EMSG-FIELDNAME = IS_CELL-FIELDNAME.
      "생산버젼를 확인하세요. 해당 자재의 생산버젼이 아닙니다.
      MESSAGE S000(ZMM01) WITH TEXT-M03 INTO ES_ALV_EMSG-MSGV1. EXIT.
    ENDIF.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CHECK_CHANGED_OTHERS
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> IRF_DATA_CHANGED
*&      --> LS_LVC_MODI
*&      <-- LS_DET
*&---------------------------------------------------------------------*
FORM CHECK_CHANGED_OTHERS USING IRF_DATA_CHANGED TYPE REF TO CL_ALV_CHANGED_DATA_PROTOCOL
                                   IS_CELL TYPE LVC_S_MODI
                           CHANGING CS_DET TYPE ANY.


*-------------------------------
* Delete Message
*-------------------------
  _G_SET_MSGTB:'D' CS_DET IS_CELL-FIELDNAME 'ZCN00' '000' '' '' '' .  "Msg 삭제

ENDFORM.
*&---------------------------------------------------------------------*
*& Form ADD_ALV_ERROR_MSG
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> IRF_DATA_CHANGED
*&      --> LS_ALV_EMSG
*&---------------------------------------------------------------------*
FORM ADD_ALV_ERROR_MSG USING IR_DATA_CHANGED TYPE REF TO CL_ALV_CHANGED_DATA_PROTOCOL
                             IS_ALV_EMSG TYPE TY_ALV_EMSG.

  CALL METHOD IR_DATA_CHANGED->ADD_PROTOCOL_ENTRY
    EXPORTING
      I_MSGID     = '0K'
      I_MSGNO     = '000'
      I_MSGTY     = 'E'
      I_MSGV1     = IS_ALV_EMSG-MSGV1
      I_MSGV2     = IS_ALV_EMSG-MSGV2
      I_MSGV3     = IS_ALV_EMSG-MSGV3
      I_MSGV4     = IS_ALV_EMSG-MSGV4
      I_FIELDNAME = IS_ALV_EMSG-FIELDNAME
      I_ROW_ID    = IS_ALV_EMSG-ROW_ID.

  CALL METHOD IR_DATA_CHANGED->DISPLAY_PROTOCOL.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form BTN_0100_VER_INIT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM BTN_0100_VER_INIT .

  DATA : LT_SELIDX    TYPE LVC_T_ROW,
         LV_TITLE     TYPE STRING,
         LV_PROC      TYPE CHAR1 VALUE 'X',
         LV_MULTI_CHK.

*-
  CALL METHOD GRF_GRID->GET_SELECTED_ROWS
    IMPORTING
      ET_INDEX_ROWS = LT_SELIDX.

  DELETE LT_SELIDX WHERE ROWTYPE IS NOT INITIAL.
  IF LT_SELIDX[] IS INITIAL.
    CLEAR LV_PROC. MESSAGE I006(ZMM01). EXIT.
  ENDIF.

*-
  PERFORM CHK_0100_VER_INIT TABLES LT_SELIDX CHANGING LV_PROC LV_MULTI_CHK.

*-
  CHECK LV_PROC IS NOT INITIAL.

*-
  DESCRIBE TABLE LT_SELIDX LINES DATA(LV_TSELLINES).

*-
  IF LV_MULTI_CHK IS INITIAL.
    LV_TITLE = TEXT-T04. "생산버젼 적용
    MESSAGE S008(ZMM01) WITH LV_TSELLINES INTO DATA(LV_TEXT1). "& 건을 진행하시겠습니까?
    CHECK GRF_GRID->POP_TO_MSG( IV_TYPE  = 'A'
                                IV_TITLE = LV_TITLE
                                IV_TEXT1 = LV_TEXT1
                                IV_TEXT2 = '' )  EQ ABAP_TRUE. " YES
  ENDIF.

*-
  PERFORM PROC_VER_INIT TABLES LT_SELIDX USING ''.

*-
  WAIT UP TO 1 SECONDS.

  CHECK LV_MULTI_CHK IS INITIAL.

*-
  PERFORM RESULT_SCREEN_100.

*-
  IF GV_ECNT IS INITIAL.
    MESSAGE I038(ZMM01) WITH GV_TCNT TEXT-T04. "전체 &1 건이 &2 되었습니다.
  ELSE.
    MESSAGE I037(ZMM01) WITH GV_ECNT.
  ENDIF.

*-
  GRF_GRID->REFRESH_GRID_DISPLAY( ).

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CHK_0100_VER_INIT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LT_SELIDX
*&      <-- LV_PROC
*&      <-- LV_MULTI_CHK
*&---------------------------------------------------------------------*
FORM CHK_0100_VER_INIT TABLES IT_SELIDX TYPE LVC_T_ROW
                     CHANGING EV_PROC EV_MULTI_CHK.

  CLEAR EV_MULTI_CHK.

*-
  CLEAR EV_MULTI_CHK.
  LOOP AT IT_SELIDX INTO DATA(LV_SELIDX).
    READ TABLE GT_DISP ASSIGNING FIELD-SYMBOL(<LS_DISP>) INDEX LV_SELIDX-INDEX.
    IF SY-SUBRC = 0.

*--
      IF <LS_DISP>-VERID IS INITIAL.
        "생산 버전이 존재하지 않는 라인이 포함되어 있습니다.
        MESSAGE I000(ZMM01) WITH TEXT-M01. CLEAR EV_PROC. EXIT.
      ENDIF.

*--
      IF <LS_DISP>-VERID NE '*' AND <LS_DISP>-INFNR NE '*'.
        IF <LS_DISP>-APLYN = 'Y'.
          "이미 적용되어 있는 라인이 포함되어 있습니다.
          MESSAGE I000(ZMM01) WITH TEXT-M02. CLEAR EV_PROC. EXIT.
        ENDIF.
      ENDIF.

*--
      IF <LS_DISP>-VERID = '*' OR <LS_DISP>-INFNR = '*'.
        EV_MULTI_CHK = 'X'.
      ENDIF.

    ENDIF.
  ENDLOOP.

*-
  DESCRIBE TABLE IT_SELIDX LINES DATA(LV_TSELLINES).
  IF LV_TSELLINES > 1.
    IF EV_MULTI_CHK = 'X'.
      "생산버젼(구매정보레코드)이 * 인 라인은 단일 처리만 가능합니다.
      MESSAGE I000(ZMM01) WITH TEXT-M05. CLEAR EV_PROC. EXIT.
    ENDIF.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form RESULT_SCREEN_100
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM RESULT_SCREEN_100.

  CHECK GT_INFO_KEY[] IS NOT INITIAL.

*-
  DATA: LT_REFRESH_INFO TYPE STANDARD TABLE OF TY_INFO.

  _G_INIT: LT_REFRESH_INFO.

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
             AND C~VALIDPRICE = 'Y'
*             AND C~CONDITIONVALIDITYENDDATE >= @SY-DATLO "현재일자 기준 유효한 단가조회
             AND C~EINA_LOEKZ = @SPACE
             AND C~EINE_LOEKZ = @SPACE
    INTO CORRESPONDING FIELDS OF TABLE @LT_REFRESH_INFO.

  ENDIF.


*-
  TYPES: BEGIN OF LTY_EINE,
           INFNR TYPE EINA-INFNR,
           WERKS TYPE ZSVBMMINFOPRICE-PLANT,
           VERID TYPE ZSVBMMINFOPRICE-VERID,
         END OF LTY_EINE.
  DATA: LT_EINE TYPE STANDARD TABLE OF LTY_EINE.

  LT_EINE[] = CORRESPONDING #( LT_REFRESH_INFO DISCARDING DUPLICATES
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
    INTO TABLE @DATA(LT_INFNR).
  ENDIF.
  FREE LT_EINE.

  DELETE LT_INFNR WHERE WERKS IS INITIAL AND VERID IS INITIAL.
  SORT LT_INFNR BY MATNR INFNR.

*-
  LOOP AT LT_REFRESH_INFO INTO DATA(LS_REFRESH_INFO).
    DELETE GT_OTHERS-INFO WHERE MATNR = LS_REFRESH_INFO-MATNR.
  ENDLOOP.

  GT_OTHERS-INFO[] = VALUE #( BASE GT_OTHERS-INFO FOR LS_WA IN LT_REFRESH_INFO
                              ( LS_WA ) ).

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
**
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

**  SORT LT_INFO_B BY MATNR INFNR WERKS VERID. DELETE ADJACENT DUPLICATES FROM LT_INFO_B COMPARING MATNR INFNR WERKS VERID.
  SORT LT_INFO_C BY MATNR INFNR WERKS VERID. DELETE ADJACENT DUPLICATES FROM LT_INFO_C COMPARING MATNR INFNR WERKS VERID.

  CLEAR: GT_OTHERS-INFO[].
  GT_OTHERS-INFO[] = LT_INFO_C[]. "LT_INFO_B[].

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

    APPEND ls_add_toolbar TO Ct_toolbar.
  END-OF-DEFINITION.

*----------------------------
*-- 추가 User Toolbar
*----------------------------
**  IF GRF_GRID->GET_EDIT_MODE( ) = ABAP_FALSE.
**    RETURN.
**  ENDIF.

  DATA:LS_ADD_TOOLBAR TYPE STB_BUTTON.
  _L_ADD_TOOLBAR : 'BTN_0100_VER'  ICON_SUPPLY_AREA TEXT-T04 '' '' TEXT-T04.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form EVT_GRID_F4
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> IV_FIELDNAME
*&      --> IS_ROW_NO
*&      <-- <LT_F4_VERID_LIST>
*&      <-- CV_TITLE
*&      <-- EV_MONTH_DISPLAY
*&---------------------------------------------------------------------*
FORM EVT_GRID_F4 USING IV_FIELDNAME
                           IS_ROW_NO TYPE LVC_S_ROID
                  CHANGING CT_F4_LIST  TYPE TABLE
                           CV_TITLE CV_MONTH_DISPLAY .

  CASE IV_FIELDNAME.
    WHEN 'VERID'.
      _G_SET_VALUE : CV_TITLE          TEXT-C35.  "U2

      PERFORM F4_VERID_GRID USING    IV_FIELDNAME
                            CHANGING CT_F4_LIST.
    WHEN OTHERS.
  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F4_VERID_GRID
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> IV_FIELDNAME
*&      <-- CT_F4_LIST
*&---------------------------------------------------------------------*
FORM F4_VERID_GRID USING IV_FIELDNAME TYPE LVC_FNAME
                    CHANGING CT_F4_LIST      TYPE TABLE.

  TYPES:BEGIN OF LTY_F4,
          MATNR TYPE MARD-MATNR, "자재
          WERKS TYPE MARC-WERKS, "플랜트
          VERID TYPE MKAL-VERID, "생산버젼
          TEXT1 TYPE MKAL-TEXT1,
        END OF LTY_F4.

  DATA:LT_F4_LIST TYPE TABLE OF LTY_F4.

*-------------------------------------------------------------
* Set F4 List
* Select 에서 데이타를 가져올 경우는 Type 선언 불필요)
*-------------------------------------------------------------

  LT_F4_LIST[] = GT_VERID[].

  MOVE-CORRESPONDING LT_F4_LIST TO CT_F4_LIST.

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

  CASE IV_COLUMN.
    WHEN 'STATU'.
*-----------------------------
* Display Error Msg Table
*-----------------------------
      READ TABLE GT_DISP INTO DATA(LS_DISP) INDEX IV_ROW.
      IF LS_DISP-MSGTB[] IS NOT INITIAL.
        GRF_GRID->SHOW_MSGTB( IT_MSGTB = LS_DISP-MSGTB ).
      ENDIF.

    WHEN 'MSG'.
      READ TABLE GT_DISP INTO LS_DISP INDEX IV_ROW.
      IF LS_DISP-MSG = ICON_LIST.
        GRF_GRID->SHOW_MSGTB( IT_MSGTB = LS_DISP-MSG_MSGTB ).
      ENDIF.

  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form EVT_GRID_HOTSPOT_CLICK
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> ES_ROW_NO_ROW_ID
*&      --> E_COLUMN_ID
*&---------------------------------------------------------------------*
FORM EVT_GRID_HOTSPOT_CLICK USING IV_ROW_ID TYPE INT4
                                       IV_COLUMN.

  READ TABLE GT_DISP INTO DATA(LS_DISP) INDEX IV_ROW_ID.

  CASE IV_COLUMN.

    WHEN 'INFNR' OR 'LIFNR' OR 'NAME1'.
      CHECK LS_DISP-INFNR IS NOT INITIAL.

      IF LS_DISP-INFNR = '*'.

        DATA: LS_DET    TYPE TS_DET,
              LT_INFO   TYPE TABLE OF TY_INFO,
              LS_INFO   LIKE LINE OF LT_INFO,
              LT_INFO_S TYPE TABLE OF TY_INFO.

        _G_INIT : GT_DET, LT_INFO, GT_VERID. CLEAR GV_MULTI_VER.

        LT_INFO[] = VALUE #( BASE LT_INFO FOR LS_WA IN GT_OTHERS-INFO
                                WHERE ( MATNR = LS_DISP-MATNR )
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
            IF <LS_INFO>-WERKS NE LS_DISP-WERKS.
              DELETE LT_INFO INDEX LV_MAIN_TABIX.
            ENDIF.
          ELSE.
            IF <LS_INFO>-WERKS NE LS_DISP-WERKS.
               CLEAR <LS_INFO>-VERID.
            ENDIF.
          ENDIF.

        ENDLOOP.
***************************************************************************************

        LOOP AT LT_INFO INTO LS_INFO.
          MOVE-CORRESPONDING LS_INFO TO LS_DET.
          IF LS_DET-WERKS IS INITIAL.
            LS_DET-WERKS = LS_DISP-WERKS.
          ENDIF.

          IF LS_DISP-VERID = '*'.
            IF LS_DET-VERID IS INITIAL.
              LS_DET-APLYN = 'N'.
            ELSE.
              LS_DET-APLYN = 'Y'.
            ENDIF.
          ELSE.
            IF ( LS_DET-VERID <> LS_DISP-VERID ) OR
               ( LS_DET-WERKS <> LS_DISP-WERKS ).  "U1 ADD
              LS_DET-APLYN = 'N'.
              CLEAR LS_DET-VERID.  "U1 ADD
            ELSE.
              LS_DET-APLYN = 'Y'.
            ENDIF.
          ENDIF.

          LS_DET-SWERKS = LS_DISP-WERKS. "U1 ADD
          APPEND LS_DET TO GT_DET.
        ENDLOOP.

        IF GT_DET[] IS NOT INITIAL.
          IF LS_DISP-VERID = '*'.
**            GV_MULTI_VER = 'X'.
            GT_VERID[] = VALUE #( BASE GT_VERID FOR LS_VER IN GT_OTHERS-VER
                           WHERE ( MATNR = LS_DISP-MATNR AND
                                   WERKS = LS_DISP-WERKS )
                                 ( LS_VER ) ).

          ENDIF.

          CALL SCREEN '0200' STARTING AT 10 10 ENDING AT 120 20.
        ENDIF.

      ELSE.

        CALL FUNCTION 'MMPUR_INFO_RECORD_DISPLAY'
          EXPORTING
            IM_INFNR = LS_DISP-INFNR
            IM_EKORG = LS_DISP-EKORG
            IM_WERKS = LS_DISP-WERKS
            IM_PSTYP = '3'
            IM_MATNR = LS_DISP-MATNR
            IM_LIFNR = LS_DISP-LIFNR.

      ENDIF.

  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form EVT_GRID_0200_HOTSPOT_CLICK
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> ES_ROW_NO_ROW_ID
*&      --> E_COLUMN_ID
*&---------------------------------------------------------------------*
FORM EVT_GRID_0200_HOTSPOT_CLICK USING IV_ROW_ID TYPE INT4
                                       IV_COLUMN.

  READ TABLE GT_DET INTO DATA(LS_DET) INDEX IV_ROW_ID.

  CASE IV_COLUMN.

    WHEN 'INFNR'.
      CHECK LS_DET-INFNR IS NOT INITIAL.

      CALL FUNCTION 'MMPUR_INFO_RECORD_DISPLAY'
        EXPORTING
          IM_INFNR = LS_DET-INFNR
          IM_EKORG = LS_DET-EKORG
          IM_WERKS = LS_DET-SWERKS "U1 LS_DET-WERKS
          IM_PSTYP = '3'
          IM_MATNR = LS_DET-MATNR
          IM_LIFNR = LS_DET-LIFNR.

  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form ALV_GRID_FCAT_MODIFY
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- CT_FCAT
*&---------------------------------------------------------------------*
FORM ALV_GRID_FCAT_MODIFY CHANGING CT_FCAT TYPE LVC_T_FCAT.

  DATA: LV_CNT TYPE SY-TABIX.

  DEFINE _L_SET_FCAT.
    <ls_fcat>-col_pos    = &1.
    <ls_fcat>-key        = &2.
    <ls_fcat>-edit       = &3.
    <ls_fcat>-coltext    = &4.
    <ls_fcat>-just       = &5.
    <ls_fcat>-f4availabl = &6.
    <ls_fcat>-cfieldname = &7.
    <ls_fcat>-qfieldname = &8.
    <ls_fcat>-no_out     = &9.
  END-OF-DEFINITION.

  SORT CT_FCAT BY FIELDNAME.

*----------------------------------------
* Set Modify Field..
*----------------------------------------
  CLEAR LV_CNT.
  LOOP AT CT_FCAT ASSIGNING FIELD-SYMBOL(<LS_FCAT>).

    CASE <LS_FCAT>-FIELDNAME.
      WHEN 'MATKL'.
        _L_SET_FCAT: 1   'X' ''  TEXT-C01   'C'  '' '' '' ''.
        <LS_FCAT>-OUTPUTLEN = 10.
        <LS_FCAT>-FIX_COLUMN = 'X'.
      WHEN 'WGBEZ'.
        _L_SET_FCAT: 2   '' ''  TEXT-C02     ''  '' '' '' ''.
        <LS_FCAT>-OUTPUTLEN = 20.
        <LS_FCAT>-FIX_COLUMN = 'X'.
      WHEN 'MATNR'.
        _L_SET_FCAT: 3   'X' ''  TEXT-C03          ''  'X' '' '' ''.
        <LS_FCAT>-OUTPUTLEN = 12.
        <LS_FCAT>-FIX_COLUMN = 'X'.
      WHEN 'MAKTX'.
        _L_SET_FCAT: 4   '' ''  TEXT-C04       ''  ''  '' '' ''.
        <LS_FCAT>-OUTPUTLEN = 35.
        <LS_FCAT>-FIX_COLUMN = 'X'.
      WHEN 'MMSTA'.
        _L_SET_FCAT: 5   '' ''  TEXT-C05       'C'  ''  '' '' ''.
        <LS_FCAT>-OUTPUTLEN = 6.
      WHEN 'MTSTB'.
        _L_SET_FCAT: 6   '' ''  TEXT-C06         ''  '' '' '' ''.
        <LS_FCAT>-OUTPUTLEN = 8.
      WHEN 'LAEDA'.
        _L_SET_FCAT: 7   '' ''  TEXT-C07         'C'  '' '' '' ''.
        <LS_FCAT>-OUTPUTLEN = 10.
        <LS_FCAT>-NO_OUT = 'X'.
      WHEN 'WERKS'.
        _L_SET_FCAT: 8   '' ''  TEXT-C08         'C'  '' '' '' ''.
        <LS_FCAT>-OUTPUTLEN = 6.
        <LS_FCAT>-FIX_COLUMN = 'X'.
      WHEN 'DISPO'.
        _L_SET_FCAT: 9   '' ''  TEXT-C09         'C'  '' '' '' ''.
        <LS_FCAT>-OUTPUTLEN = 4.
        <LS_FCAT>-NO_OUT = 'X'.
      WHEN 'DSNAM'.
        _L_SET_FCAT: 10   '' ''  TEXT-C10         ''  '' '' '' ''.
        <LS_FCAT>-OUTPUTLEN = 12.
        <LS_FCAT>-FIX_COLUMN = 'X'.
      WHEN 'EKGRP'.
        _L_SET_FCAT: 11   '' ''  TEXT-C11         'C'  '' '' '' ''.
        <LS_FCAT>-OUTPUTLEN = 4.
        <LS_FCAT>-NO_OUT = 'X'.
      WHEN 'EKNAM'.
        _L_SET_FCAT: 12   '' ''  TEXT-C12         ''  '' '' '' ''.
        <LS_FCAT>-OUTPUTLEN = 14.
        <LS_FCAT>-FIX_COLUMN = 'X'.
      WHEN 'BESKZ'.
        _L_SET_FCAT: 13   '' ''  TEXT-C13         'C'  '' '' '' ''.
        <LS_FCAT>-OUTPUTLEN = 6.
      WHEN 'SOBSL'.
        _L_SET_FCAT: 14   '' ''  TEXT-C14         'C'  '' '' '' ''.
        <LS_FCAT>-OUTPUTLEN = 8.
      WHEN 'BOM_ID'.
        _L_SET_FCAT: 15   '' ''  TEXT-C15         'C'  '' '' '' ''.
        <LS_FCAT>-OUTPUTLEN = 4.
      WHEN 'VER_ID'.
        _L_SET_FCAT: 16   '' ''  TEXT-C16         'C'  '' '' '' ''.
        <LS_FCAT>-OUTPUTLEN = 4.
      WHEN 'INFO_ID'.
        _L_SET_FCAT: 17   '' ''  TEXT-C17         'C'  '' '' '' ''.
        <LS_FCAT>-OUTPUTLEN = 4.
      WHEN 'VERID'.
        _L_SET_FCAT: 18   '' ''  TEXT-C18         'C'  '' '' '' ''.
        <LS_FCAT>-OUTPUTLEN = 6.
      WHEN 'APLYN'.
        _L_SET_FCAT: 19   '' ''  TEXT-C19         'C'  '' '' '' ''.
        <LS_FCAT>-OUTPUTLEN = 6.
        <LS_FCAT>-SCRTEXT_L  = <LS_FCAT>-SCRTEXT_M = <LS_FCAT>-SCRTEXT_S = <LS_FCAT>-COLTEXT.
      WHEN 'INFNR'.
        _L_SET_FCAT: 20   '' ''  TEXT-C20         'C'  '' '' '' ''.
        <LS_FCAT>-OUTPUTLEN = 12.
        <LS_FCAT>-HOTSPOT = 'X'.
      WHEN 'LIFNR'.
        _L_SET_FCAT: 21   '' ''  TEXT-C21         ''  '' '' '' ''.
        <LS_FCAT>-OUTPUTLEN = 10.
        <LS_FCAT>-HOTSPOT = 'X'.
      WHEN 'NAME1'.
        _L_SET_FCAT: 22   '' ''  TEXT-C22         ''  '' '' '' ''.
        <LS_FCAT>-OUTPUTLEN = 18.
        <LS_FCAT>-HOTSPOT = 'X'.
      WHEN 'MSG'.
        _L_SET_FCAT: 23   '' ''  TEXT-C23         'C'  '' '' '' ''.
        <LS_FCAT>-OUTPUTLEN = 4.

      WHEN 'CDLST' OR 'DCFLG' OR 'IEDIT' OR 'ZDELE'.
        <LS_FCAT>-SCRTEXT_L  = <LS_FCAT>-SCRTEXT_M = <LS_FCAT>-SCRTEXT_S = 'T'.
        <LS_FCAT>-NO_OUT = 'X'.

      WHEN OTHERS.
        <LS_FCAT>-NO_OUT = 'X'.
    ENDCASE.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form ALV_GRID_0200_FCAT_MODIFY
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- CT_FCAT
*&---------------------------------------------------------------------*
FORM ALV_GRID_0200_FCAT_MODIFY CHANGING CT_FCAT TYPE LVC_T_FCAT.

  DEFINE _L_SET_FCAT.
    <ls_fcat>-col_pos    = &1.
    <ls_fcat>-key        = &2.
    <ls_fcat>-edit       = &3.
    <ls_fcat>-coltext    = &4.
    <ls_fcat>-just       = &5.
    <ls_fcat>-f4availabl = &6.
    <ls_fcat>-cfieldname = &7.
    <ls_fcat>-qfieldname = &8.
    <ls_fcat>-no_out     = &9.
  END-OF-DEFINITION.

  SORT CT_FCAT BY FIELDNAME.

*----------------------------------------
* Set Modify Field..
*----------------------------------------
  LOOP AT CT_FCAT ASSIGNING FIELD-SYMBOL(<LS_FCAT>).

    CASE <LS_FCAT>-FIELDNAME.
      WHEN 'EKORG'.
        _L_SET_FCAT: 1   ''  ''  TEXT-C25        'C' ''  '' '' ''.
        <LS_FCAT>-OUTPUTLEN = 08.
      WHEN 'INFNR'.
        _L_SET_FCAT: 2   ''  ''  TEXT-C26   '' ''  '' '' ''.
        <LS_FCAT>-OUTPUTLEN = 14.
        <LS_FCAT>-HOTSPOT = 'X'.
      WHEN 'LIFNR'.
        _L_SET_FCAT: 3   ''  ''  TEXT-C27        '' ''  '' '' ''.
        <LS_FCAT>-OUTPUTLEN = 10.
      WHEN 'NAME1'.
        _L_SET_FCAT: 4   ''  ''  TEXT-C28        '' ''  '' '' ''.
        <LS_FCAT>-OUTPUTLEN = 20.
      WHEN 'MATNR'.
        _L_SET_FCAT: 5   ''  ''  TEXT-C29        '' ''  '' '' ''.
        <LS_FCAT>-OUTPUTLEN = 12.
      WHEN 'MAKTX'.
        _L_SET_FCAT: 6   ''  ''  TEXT-C30        '' ''  '' '' ''.
        <LS_FCAT>-OUTPUTLEN = 18.
      WHEN 'SWERKS'.
        _L_SET_FCAT: 7   '' ''  TEXT-C31         'C'  '' '' '' ''.
        <LS_FCAT>-OUTPUTLEN = 6.
      WHEN 'VERID'.
        _L_SET_FCAT: 8   ''  'X'  TEXT-C32        'C' 'X'  '' '' ''.
        <LS_FCAT>-OUTPUTLEN = 12.
      WHEN 'APLYN'.
        _L_SET_FCAT: 9   ''  ''  TEXT-C33        'C' ''  '' '' ''.
        <LS_FCAT>-OUTPUTLEN = 6.
        <LS_FCAT>-SCRTEXT_L  = <LS_FCAT>-SCRTEXT_M = <LS_FCAT>-SCRTEXT_S = <LS_FCAT>-COLTEXT.
      WHEN 'CDLST' OR 'DCFLG' OR 'ZDELE'.
        <LS_FCAT>-SCRTEXT_L  = <LS_FCAT>-SCRTEXT_M = <LS_FCAT>-SCRTEXT_S = 'T'.
        <LS_FCAT>-NO_OUT = 'X'.
      WHEN OTHERS.
        <LS_FCAT>-NO_OUT = 'X'.
    ENDCASE.
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
         LT_LVC_SCOL TYPE LVC_T_SCOL,
         LV_INDEX    TYPE I.

  LOOP AT GT_DISP INTO DATA(LS_DISP).
    LV_INDEX = SY-TABIX.

    CLEAR: LT_LVC_STYL[], LT_LVC_SCOL[].
*---------------------------
* Set Field Style..
*---------------------------
    PERFORM SET_FIELD_CELLTAB USING    IT_FCAT
                              CHANGING LT_LVC_STYL LS_DISP.

*-- Insert Style Talble
    CLEAR LS_DISP-CELLS.
    INSERT LINES OF LT_LVC_STYL INTO TABLE LS_DISP-CELLS.

*---------------------------
* Set Field Color..
*---------------------------
    PERFORM SET_FIELD_CELLTAB_COLOR USING   IT_FCAT
                                 CHANGING LT_LVC_SCOL LS_DISP.

    CLEAR LS_DISP-CELLC.
    INSERT LINES OF LT_LVC_SCOL INTO TABLE LS_DISP-CELLC.

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
                                CS_DISP TYPE TS_DISP.

  DATA : LS_LVC_STYL TYPE LVC_S_STYL.

  LOOP AT IT_FCAT INTO DATA(LS_FIELDCAT).
    CHECK LS_FIELDCAT-EDIT = 'X'.

    LS_LVC_STYL-FIELDNAME = LS_FIELDCAT-FIELDNAME.

    CASE LS_FIELDCAT-KEY.
      WHEN 'X'.
        IF CS_DISP-DCFLG EQ 'C'. "'N'. "신규필드인 경우
          LS_LVC_STYL-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_ENABLED.
        ELSE.
          LS_LVC_STYL-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_DISABLED.
        ENDIF.
      WHEN OTHERS.
        LS_LVC_STYL-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_DISABLED.
    ENDCASE.

    INSERT LS_LVC_STYL INTO TABLE CT_STYL.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_FIELD_CELLTAB_COLOR
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> IT_FCAT
*&      <-- LT_LVC_SCOL
*&      <-- LS_DISP
*&---------------------------------------------------------------------*
FORM SET_FIELD_CELLTAB_COLOR USING IT_FCAT TYPE LVC_T_FCAT
                            CHANGING CT_SCOL TYPE LVC_T_SCOL
                                     CS_DISP TYPE TS_DISP.

  DATA : LS_LVC_SCOL TYPE LVC_S_SCOL.

  LOOP AT IT_FCAT INTO DATA(LS_FIELDCAT).

    CLEAR LS_LVC_SCOL.

    LS_LVC_SCOL-FNAME = LS_FIELDCAT-FIELDNAME.

    CASE LS_LVC_SCOL-FNAME.
      WHEN 'STATU' OR 'MATKL' OR 'WGBEZ' OR 'MATNR' OR 'MAKTX' OR 'WERKS' OR 'DSNAM' OR 'EKNAM'
        OR 'MMSTA' OR 'MTSTB'.
        LS_LVC_SCOL-COLOR-COL = 5.
        LS_LVC_SCOL-COLOR-INT = 0.
        LS_LVC_SCOL-COLOR-INV = 0.
      WHEN 'BESKZ' OR 'SOBSL' OR 'BOM_ID' OR 'VER_ID' OR 'INFO_ID' OR 'VERID' OR 'APLYN'.
        LS_LVC_SCOL-COLOR-COL = 3.
        LS_LVC_SCOL-COLOR-INT = 0.
        LS_LVC_SCOL-COLOR-INV = 0.
      WHEN 'INFNR' OR 'LIFNR' OR 'NAME1'.
        LS_LVC_SCOL-COLOR-COL = 7.
        LS_LVC_SCOL-COLOR-INT = 0.
        LS_LVC_SCOL-COLOR-INV = 0.
      WHEN 'MSG'.
        LS_LVC_SCOL-COLOR-COL = 3.
        LS_LVC_SCOL-COLOR-INT = 1.
        LS_LVC_SCOL-COLOR-INV = 1.
      WHEN OTHERS.
        LS_LVC_SCOL-COLOR-COL = 8.
        LS_LVC_SCOL-COLOR-INT = 1.
        LS_LVC_SCOL-COLOR-INV = 1.
    ENDCASE.

    INSERT LS_LVC_SCOL INTO TABLE CT_SCOL.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form ALV_GRID_0200_SET_LINE_STYLE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> IT_FCAT
*&---------------------------------------------------------------------*
FORM ALV_GRID_0200_SET_LINE_STYLE USING IT_FCAT.

  DATA : LT_LVC_STYL TYPE LVC_T_STYL,
         LT_LVC_SCOL TYPE LVC_T_SCOL,
         LV_INDEX    TYPE I.

  LOOP AT GT_DET INTO DATA(LS_DET).
    LV_INDEX = SY-TABIX.

    CLEAR: LT_LVC_STYL[], LT_LVC_SCOL[].
*---------------------------
* Set Field Style..
*---------------------------
    PERFORM SET_FIELD_CELLTAB_0200 USING    IT_FCAT
                              CHANGING LT_LVC_STYL LS_DET.

*-- Insert Style Talble
    CLEAR LS_DET-CELLS.
    INSERT LINES OF LT_LVC_STYL INTO TABLE LS_DET-CELLS.

*---------------------------
* Set Field Color..
*---------------------------
**    PERFORM SET_FIELD_CELLTAB_COLOR USING   IT_FCAT
**                                 CHANGING LT_LVC_SCOL LS_DISP_0200.
**    PERFORM SET_FIELD_CELLTAB_COLOR_0200 USING   IT_FCAT
**                                 CHANGING LT_LVC_SCOL LS_DISP_0200.
**
**    CLEAR LS_DISP_0200-CELLC.
**    INSERT LINES OF LT_LVC_SCOL INTO TABLE LS_DISP_0200-CELLC.

*-- Modify Line..
    MODIFY GT_DET FROM LS_DET INDEX LV_INDEX.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_FIELD_CELLTAB_0200
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> IT_FCAT
*&      <-- LT_LVC_STYL
*&      <-- LS_DISP_0200
*&---------------------------------------------------------------------*
FORM SET_FIELD_CELLTAB_0200 USING IT_FCAT TYPE LVC_T_FCAT
                       CHANGING CT_STYL TYPE LVC_T_STYL
                                CS_DISP TYPE TS_DET.

  DATA : LS_LVC_STYL TYPE LVC_S_STYL.

  LOOP AT IT_FCAT INTO DATA(LS_FIELDCAT).
    CHECK LS_FIELDCAT-EDIT = 'X'.

    LS_LVC_STYL-FIELDNAME = LS_FIELDCAT-FIELDNAME.

    CASE LS_FIELDCAT-KEY.
      WHEN 'X'.
        IF CS_DISP-DCFLG EQ 'C'. "'N'. "신규필드인 경우
          LS_LVC_STYL-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_ENABLED.
        ELSE.
          LS_LVC_STYL-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_DISABLED.
        ENDIF.
      WHEN OTHERS.
        IF GV_MULTI_VER IS INITIAL.
          LS_LVC_STYL-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_DISABLED.
        ELSE.
          LS_LVC_STYL-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_ENABLED.
        ENDIF.
    ENDCASE.

    INSERT LS_LVC_STYL INTO TABLE CT_STYL.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form ALV_SET_F4_0200
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- CT_FIELD
*&---------------------------------------------------------------------*
FORM ALV_SET_F4_0200 CHANGING CT_FIELD TYPE ZCL_CN_ALV_GRID=>TT_FIELD.

*----------------------------
* Set F4 Field..
*----------------------------
  CT_FIELD = VALUE #( ( FIELDNAME = 'VERID' ) ).

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_GRID
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM SET_GRID .

  IF GRF_DOCKING_CON IS INITIAL.

* Creating Docing container instance
    PERFORM CREATE_CONTAINER.
*--------------------------------
* Create Alv Grid
*--------------------------------
    PERFORM CREATE_ALV_GRID.

*--------------------------------
* Dislay Grid..
*--------------------------------
    GRF_GRID->SET_GRID( "EXPORTING IV_VARI = P_VAR
*                      it_fcat = lt_fcat   "Dynamic Alv일경우 사용
                        CHANGING  CT_DATA = GT_DISP ).

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
FORM CREATE_CONTAINER .

*----------------------------------------------------
* Create Docking Container..
*----------------------------------------------------
  CREATE OBJECT GRF_DOCKING_CON
    EXPORTING
      REPID     = SY-REPID " 프로그램명 id
      DYNNR     = SY-DYNNR " 화면번호 (Screen) "
      SIDE      = GRF_DOCKING_CON->DOCK_AT_TOP "
      EXTENSION = 10000.


*----------------------------------------------------
* Split Container (1 Row:header 2 Row: ALV Grid)
*----------------------------------------------------
  DATA(LRF_SPLITTER) = NEW CL_GUI_SPLITTER_CONTAINER( PARENT  = GRF_DOCKING_CON
                                                      NO_AUTODEF_PROGID_DYNNR = 'X'
                                                      ROWS    = 2
                                                      COLUMNS = 1 ).
  LRF_SPLITTER->SET_ROW_MODE( MODE = CL_GUI_SPLITTER_CONTAINER=>TYPE_MOVABLE ).
  LRF_SPLITTER->SET_ROW_HEIGHT( ID = 1 HEIGHT = 110 ).
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
FORM CREATE_ALV_GRID .
  DEFINE _L_ADD_FIELD.

    lt_dftvl = VALUE #( BASE lt_dftvl ( fieldname = &1 value = &2 ) ).

  END-OF-DEFINITION.

  DATA:LS_TOOLBTN TYPE ZSCN00004,
       LT_DFTVL   TYPE ZCL_CN_ALV_GRID=>TT_FIELD,
       LT_HEADER  TYPE ZCL_CN_ALV_GRID=>TT_HEADER.

*--------------------------------------------------
* User Toolbar Button (Default Setting).
*--------------------------------------------------
*  LS_TOOLBTN-BTN_ADD    = 'X'.       "Add Row
*  LS_TOOLBTN-BTN_MADD   = 'X'.       "Multi Add Row
*  LS_TOOLBTN-MLTI_LINES = GV_MROW.   "Multi Row
*  LS_TOOLBTN-BTN_DEL    = 'X'.       "Delete Row
*  LS_TOOLBTN-BTN_REC    = 'X'.       "Recovery Row
  LS_TOOLBTN-BTN_EXLD   = 'X'.       "Excel Download
*  LS_TOOLBTN-BTN_EXLU   = 'X'.       "Excel Upload
*  LS_TOOLBTN-BTN_INFO   = 'X'.       "Information
*-- History Table..
*  LS_TOOLBTN-BTN_HIST   = 'X'.       "History Button
*  _G_SET_VALUE:LS_TOOLBTN-HIST_TABNM 'ZTMM20030'.  " 그리드별 마스터 Table Name..
*--------------------------------------------------
* Add Row시 Default로 세팅되어지는 필드
*--------------------------------------------------
**  _L_ADD_FIELD: 'S_CHKDT' S_CHKDT-LOW.


*--------------------------------------------------
* Set Header Information
*--------------------------------------------------
  PERFORM SET_HEADER_INFO CHANGING LT_HEADER.

*--------------------------------------------------
* Set Infomationv 버튼
*--------------------------------------------------
*  LT_INFO = VALUE #( ( FIELDNAME = 'STLAL' ) ( FIELDNAME = 'INFNR' ) ( FIELDNAME = 'MSG' ) ).

  CREATE OBJECT GRF_GRID
    EXPORTING
      IV_NAME    = 'ALV_GRID'   "다수의 그리드일 경우 식별하기 위함.
      IRF_PARENT = GRF_BODY
*     IV_VARIANT = P_VAR
      IT_DFTVL   = LT_DFTVL
      IS_TOOLBTN = LS_TOOLBTN
*     IT_INFO    = LT_INFO
      IRF_HEAD   = GRF_HEAD
      IV_SCR_WR  = '15:25:60'
      IT_HEADER  = LT_HEADER.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_GRID_0200
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM SET_GRID_0200 .

  IF GRF_CUSTOM_CONTAINER_0200 IS INITIAL.

    CREATE OBJECT GRF_CUSTOM_CONTAINER_0200
      EXPORTING
        CONTAINER_NAME = GV_CONTAINER_0200.

*-
*--------------------------------
* Create Alv Grid
*--------------------------------
    PERFORM CREATE_ALV_GRID_0200.

*--------------------------------
* Dislay Grid..
*--------------------------------
    GRF_GRID_0200->SET_GRID( EXPORTING IV_VARI = 'VERI_0200' CHANGING  CT_DATA = GT_DET ).

    GV_MODE = 'X'.
    CALL METHOD GRF_GRID_0200->SET_CHANGE_MODE( CHANGING CV_MODE = GV_MODE ).

  ELSE.
    GRF_GRID_0200->REFRESH_GRID_DISPLAY( ).
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CREATE_ALV_GRID_0200
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM CREATE_ALV_GRID_0200 .

  DEFINE _L_ADD_FIELD.

    lt_dftvl = VALUE #( BASE lt_dftvl ( fieldname = &1 value = &2 ) ).

  END-OF-DEFINITION.

  DATA:LS_TOOLBTN TYPE ZSCN00004,
       LT_DFTVL   TYPE ZCL_CN_ALV_GRID=>TT_FIELD.
*       LT_HEADER  TYPE ZCL_CN_ALV_GRID=>TT_HEADER.

*--------------------------------------------------
* User Toolbar Button (Default Setting).
*--------------------------------------------------
*  LS_TOOLBTN-BTN_ADD    = 'X'.       "Add Row
*  LS_TOOLBTN-BTN_MADD   = 'X'.       "Multi Add Row
*  LS_TOOLBTN-MLTI_LINES = GV_MROW.   "Multi Row
*  LS_TOOLBTN-BTN_DEL    = 'X'.       "Delete Row
*  LS_TOOLBTN-BTN_REC    = 'X'.       "Recovery Row
*  LS_TOOLBTN-BTN_EXLD   = 'X'.       "Excel Download
*  LS_TOOLBTN-BTN_EXLU   = 'X'.       "Excel Upload
*-- History Table..
*  LS_TOOLBTN-BTN_HIST   = 'X'.       "History Button
*  _G_SET_VALUE:LS_TOOLBTN-HIST_TABNM 'ZTMM10010'.  " 그리드별 마스터 Table Name..
*--------------------------------------------------
* Add Row시 Default로 세팅되어지는 필드
*--------------------------------------------------
**  SELECT SINGLE BUTXT,WAERS
**    INTO @DATA(LS_DATA)
**    FROM T001
**   WHERE BUKRS = @P_BUKRS.
**
**  _L_ADD_FIELD: 'KOKRS' P_KOKRS,
**                'BUKRS' P_BUKRS,
**                'BUTXT' LS_DATA-BUTXT,
**                'WAERS' LS_DATA-WAERS,
**                'VERSN' P_VERSN.

  CREATE OBJECT GRF_GRID_0200
    EXPORTING
      IV_NAME    = 'ALV_GRID_0200'   "다수의 그리드일 경우 식별하기 위함..
      IRF_PARENT = GRF_CUSTOM_CONTAINER_0200
      IT_DFTVL   = LT_DFTVL
      IS_TOOLBTN = LS_TOOLBTN
      IRF_HEAD   = GRF_HEAD.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_HEADER_INFO
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- LT_HEADER
*&---------------------------------------------------------------------*
FORM SET_HEADER_INFO CHANGING CT_HEADER TYPE GRF_GRID->TT_HEADER.

**  DATA : LV_TEXT(255) TYPE C.

  DATA : LS_HEADER TYPE GRF_GRID->TS_HEADER.

  DEFINE _L_SET_HEADER.
    IF &2 IS NOT INITIAL.
    CLEAR ls_header.
    ls_header-key  = &1.
    ls_header-info  = &2.
    ls_header-text  = &3.
    APPEND ls_header TO ct_header.
    ENDIF.
  END-OF-DEFINITION.

**  CONSTANTS : LC_TEXT1(20) TYPE C VALUE '점검 기준 일자'.

  DATA: LV_TEXT1(20) TYPE C.
  LV_TEXT1 = TEXT-C24.  "U2
*---------------------------------------
* Header Text 지정
*---------------------------------------
*-프로그램명 헤더에서 제외
  DATA(LV_TEXT1_TXT) = S_CHKDT-LOW(04) && '.' && S_CHKDT-LOW+4(02) && '.' && S_CHKDT-LOW+6(02).

***-----------------------------------
*** Header Column 지정
***-----------------------------------
  _L_SET_HEADER : LV_TEXT1  LV_TEXT1_TXT ''.

*_g_set_value:'20:10:70'.  "Default 비율 (비율 변경시 사용)

ENDFORM.
