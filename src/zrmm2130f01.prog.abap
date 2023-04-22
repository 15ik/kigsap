*&---------------------------------------------------------------------*
*& Include          ZRMM2130F01
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
*------------------------------
* Set Variant
*------------------------------
*-
  IF GR_PWERKS[] IS INITIAL.
    CLEAR: GR_PWERKS.

    _G_APPEND_VALUE : GR_PWERKS 'EQ' 'I' TEXT-O01 '', "1001
                      GR_PWERKS 'EQ' 'I' TEXT-O02 '', "1002
                      GR_PWERKS 'EQ' 'I' TEXT-O03 '', "1003
                      GR_PWERKS 'EQ' 'I' TEXT-O04 '', "1004
                      GR_PWERKS 'EQ' 'I' TEXT-O05 ''. "1005
  ENDIF.


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

  "TBD 임시
  CHECK 1 <> 1.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM GET_DATA.

  DATA: LV_PRG.
  CLEAR LV_PRG.

  _G_INIT: GT_MAIN, GT_DISP, GT_DISP_BK.

*- Check Input Data
  PERFORM CHK_INPUTED_DATA CHANGING LV_PRG.

*- Common Code
  PERFORM GET_COMMON_CONFIG CHANGING LV_PRG.

*-
  CHECK LV_PRG IS NOT INITIAL.

*- Get Material
  PERFORM GET_MATERIAL.

*- SET BASE_DATA
  PERFORM SET_BASE_DATA.

*- GET MRP DATA
  PERFORM GET_MAP_DATA.

*- 자재의 공급 업체 정보
  PERFORM GET_LIFNR_DATA.

*- Setting Display date
  PERFORM SET_DAY.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CHK_INPUTED_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- LV_PRG
*&---------------------------------------------------------------------*
FORM CHK_INPUTED_DATA CHANGING EV_PRG.

  EV_PRG = 'X'.

*-
  IF S_WERKS[] IS NOT INITIAL.

*    SELECT WERKS
*      FROM T001W
*     WHERE WERKS IN @S_WERKS
*    INTO TABLE @DATA(LT_T001W).
*
*    DELETE LT_T001W WHERE WERKS IN GR_PWERKS.
*
*    DESCRIBE TABLE LT_T001W LINES DATA(LV_T001W).
*
*    IF LV_T001W IS NOT INITIAL.
*      "플랜트는 제조플랜트만 조회 가능합니다.
*      MESSAGE S000(ZMM01) WITH TEXT-M01 DISPLAY LIKE 'E'. CLEAR EV_PRG.
*      LEAVE LIST-PROCESSING.
*    ENDIF.

  ENDIF.

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

*-
  _G_INIT : GT_COMM_CFG_A, GT_COMM_CFG_B, GT_COMM_CFG_C, GT_COMM_CFG_D, GT_COMM_CFG_B312,
            GT_SEL_LIST, GT_STB, GT_STB_MATNR.

*-생산처 관리
  ZCL_MM_COMMON=>COMMON_CONFIG(  EXPORTING IS_COMMON =  VALUE #( M = 'A1' D = 'A1000' S = 'PRT01' )
                                 IMPORTING ET_OUTTAB = DATA(LT_CONFIG) ).
  GT_COMM_CFG_A[] = VALUE #( BASE GT_COMM_CFG_A FOR LS_VALUE IN LT_CONFIG
                                  ( CORRESPONDING #( LS_VALUE ) ) ).

*- 데이터발췌기준
  ZCL_MM_COMMON=>COMMON_CONFIG(  EXPORTING IS_COMMON =  VALUE #( M = 'B1' D = 'B0001' S = 'MRP01' )
                                 IMPORTING ET_OUTTAB = LT_CONFIG ).
  GT_COMM_CFG_B[] = CORRESPONDING #( LT_CONFIG[] ).

*- "계획구간설정
  ZCL_MM_COMMON=>COMMON_CONFIG(  EXPORTING IS_COMMON =  VALUE #( M = 'B1' D = 'B0003' S = '00002' )
                                 IMPORTING ET_OUTTAB = LT_CONFIG ).
  GT_COMM_CFG_C[] = VALUE #( BASE GT_COMM_CFG_C FOR LS_VALUE IN LT_CONFIG
                            WHERE ( FIELD1 = SY-CPROG )
                                  ( CORRESPONDING #( LS_VALUE ) ) ).

*- "확정구간설정
  ZCL_MM_COMMON=>COMMON_CONFIG(  EXPORTING IS_COMMON =  VALUE #( M = 'B1' D = 'B0001' S = 'MRP02' )
                                 IMPORTING ET_OUTTAB = LT_CONFIG ).
  GT_COMM_CFG_D[] = CORRESPONDING #( LT_CONFIG[] ).

*- "생산계획점검여부
  ZCL_MM_COMMON=>COMMON_CONFIG(  EXPORTING IS_COMMON =  VALUE #( M = 'B1' D = 'B0003' S = '00012' )
                                 IT_WHERE = VALUE #( ( FIELD = 1 VALUE = SY-CPROG ) )
                                 IMPORTING ET_OUTTAB = LT_CONFIG ).
  GT_COMM_CFG_B312[] = CORRESPONDING #( LT_CONFIG[] ).

*-
*  IF GT_COMM_CFG_A[] IS INITIAL.
*    "Config : [ A1000 / PRT01 ]  생산처 관리가 없습니다.
*    MESSAGE S000(ZMM01) WITH TEXT-M05 DISPLAY LIKE 'E'. CLEAR EV_PRG.
*    LEAVE LIST-PROCESSING.
*  ENDIF.
*
**-
*  IF GT_COMM_CFG_B[] IS INITIAL.
*    "Config : [ MRP01 ]  예상재고 List 구성 조건필드가 없습니다.
*    MESSAGE S000(ZMM01) WITH TEXT-M02 DISPLAY LIKE 'E'. CLEAR EV_PRG.
*    LEAVE LIST-PROCESSING.
*  ENDIF.
*
**- "MRP Data select List Field
*  PERFORM SET_SEL_FIELD_MRP_TABLE CHANGING EV_PRG.
*
**-
*  IF GT_COMM_CFG_C[] IS INITIAL.
*    "Config : [ 0002 ]  계획구간설정 이 없습니다.
*    MESSAGE S000(ZMM01) WITH TEXT-M03 DISPLAY LIKE 'E'. CLEAR EV_PRG.
*    LEAVE LIST-PROCESSING.
*  ENDIF.
*
**-
*  IF GT_COMM_CFG_D[] IS INITIAL.
*    "Config : [ MRP02 ]  확정구간설정 이 없습니다.
*    MESSAGE S000(ZMM01) WITH TEXT-M04 DISPLAY LIKE 'E'. CLEAR EV_PRG.
*    LEAVE LIST-PROCESSING.
*  ENDIF.

*-- 제품코드 입력시 데이타 여부
  IF S_PROD[] IS NOT INITIAL.
    PERFORM GET_INPUTED_BOM.
    IF GT_STB[] IS INITIAL.
      "입력된 제품의 하위자재가 존재하지 않습니다.
      MESSAGE S000(ZMM01) WITH TEXT-M11 DISPLAY LIKE 'E'. CLEAR EV_PRG.
      LEAVE LIST-PROCESSING.
    ENDIF.
  ENDIF.
*--
  IF GT_STB[] IS NOT INITIAL.
    GT_STB_MATNR[] = VALUE #( BASE GT_STB_MATNR[] FOR LS_DATA IN GT_STB
                            ( MATNR = LS_DATA-MATNR WERKS = LS_DATA-WERKS ) ).
  ENDIF.

  SORT GT_STB_MATNR BY MATNR WERKS. DELETE ADJACENT DUPLICATES FROM GT_STB_MATNR COMPARING MATNR WERKS.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_SET_SEL_FIELD_MRP_TABLE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM SET_SEL_FIELD_MRP_TABLE CHANGING EV_PRG.

*-
  TRY.
      SELECT FIELDNAME, POSITION, KEYFLAG FROM DD03L WHERE TABNAME = @TEXT-O08 AND FIELDNAME NE @TEXT-O09
                                 ORDER BY POSITION INTO TABLE @DATA(LT_ORG_MRP_T).

      LOOP AT LT_ORG_MRP_T INTO DATA(LS_ORG_MRP_T) WHERE KEYFLAG = 'X'.
        APPEND LS_ORG_MRP_T-FIELDNAME TO: GT_SEL_LIST.
      ENDLOOP.

      SORT LT_ORG_MRP_T BY FIELDNAME.
      DATA(LT_COMM_CFG_B) = GT_COMM_CFG_B[].
      SORT LT_COMM_CFG_B BY FIELD2. DELETE ADJACENT DUPLICATES FROM LT_COMM_CFG_B COMPARING FIELD2.
      LOOP AT LT_COMM_CFG_B INTO DATA(LS_CFG).
        CHECK LS_CFG-FIELD2 IS NOT INITIAL.
        READ TABLE LT_ORG_MRP_T WITH KEY FIELDNAME = LS_CFG-FIELD2 BINARY SEARCH TRANSPORTING NO FIELDS.
        IF SY-SUBRC = 0.
          APPEND LS_CFG-FIELD2 TO: GT_SEL_LIST.
        ELSE.
          "Config : [ MRP01 ]  자율납품 List 구성 조건필드(Field2)가 실제 DB 에 존재하지 않습니다.
          MESSAGE S003(ZMM01) WITH TEXT-M01 LS_CFG-FIELD2 TEXT-M02 DISPLAY LIKE 'E'. CLEAR EV_PRG.
          LEAVE LIST-PROCESSING.
        ENDIF.
      ENDLOOP.

*--
    CATCH CX_SY_DYNAMIC_OSQL_ERROR.
  ENDTRY.

*-
  APPEND : 'DAT01'(F01) TO: GT_SEL_LIST,
           'MNG01'(F02) TO: GT_SEL_LIST,
           'DELET'(F03) TO: GT_SEL_LIST,
           'ETMEN'(F04) TO: GT_SEL_LIST,
           'VRFKZ'(F05) TO: GT_SEL_LIST.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_MATERIAL
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM GET_MATERIAL.

*-
  IF S_PROD[] IS INITIAL.
    IF S_BERID[] IS INITIAL.
      SELECT FROM MARA AS A INNER JOIN V_MARC_MD AS B
                                    ON B~MATNR = A~MATNR
                            INNER JOIN MBEW      AS C
                                    ON C~MATNR = B~MATNR
                                   AND C~BWKEY = B~WERKS
                                   AND C~BKLAS IN ( @TEXT-O06, @TEXT-O07 ) "3010, 3011
                            INNER JOIN T001W     AS D
                                    ON D~WERKS = B~WERKS
                            INNER JOIN T023T     AS E
                                    ON E~MATKL = A~MATKL
                                   AND E~SPRAS = @SY-LANGU
                            INNER JOIN MAKT      AS F
                                    ON F~MATNR = A~MATNR
                                   AND F~SPRAS = @SY-LANGU
             FIELDS
              B~MATNR, B~WERKS, A~MATKL, A~MEINS,
              D~NAME1, E~WGBEZ, E~WGBEZ60, F~MAKTX, B~WERKS AS BERID

             WHERE "A~MATNR IN @GT_MATNR
                   A~MATNR IN @S_MATNR
               AND A~MATKL IN @S_MATKL
               AND A~LVORM EQ @SPACE
               AND B~WERKS IN @S_WERKS
               AND B~DISPO IN @S_DISPO
               AND B~EKGRP IN @S_EKGRP
               AND B~LVORM EQ @SPACE
       INTO CORRESPONDING FIELDS OF TABLE @GT_MAIN.
    ELSE.
      SELECT FROM MARA AS A INNER JOIN V_MARC_MD AS B
                                    ON B~MATNR = A~MATNR
                            INNER JOIN MBEW      AS C
                                    ON C~MATNR = B~MATNR
                                   AND C~BWKEY = B~WERKS
                                   AND C~BKLAS IN ( @TEXT-O06, @TEXT-O07 ) "3010, 3011
                            INNER JOIN T001W     AS D
                                    ON D~WERKS = B~WERKS
                            INNER JOIN T023T     AS E
                                    ON E~MATKL = A~MATKL
                                   AND E~SPRAS = @SY-LANGU
                            INNER JOIN MAKT      AS F
                                    ON F~MATNR = A~MATNR
                                   AND F~SPRAS = @SY-LANGU
                            INNER JOIN PPH_DBVM  AS G
                                    ON G~MATNR = B~MATNR AND
                                       G~WERKS = B~WERKS
             FIELDS
              B~MATNR, B~WERKS, A~MATKL, A~MEINS,
              D~NAME1, E~WGBEZ, E~WGBEZ60, F~MAKTX, G~BERID
             WHERE "A~MATNR IN @GT_MATNR
                   A~MATNR IN @S_MATNR
               AND A~MATKL IN @S_MATKL
               AND A~LVORM EQ @SPACE
               AND B~WERKS IN @S_WERKS
               AND B~DISPO IN @S_DISPO
               AND B~EKGRP IN @S_EKGRP
               AND B~LVORM EQ @SPACE
               AND G~BERID IN @S_BERID
       INTO CORRESPONDING FIELDS OF TABLE @GT_MAIN.

      SORT GT_MAIN BY WERKS MATNR MATKL BERID.
      DELETE ADJACENT DUPLICATES FROM GT_MAIN COMPARING WERKS MATNR MATKL BERID.
    ENDIF.

  ELSE.
    IF S_BERID[] IS INITIAL.
      SORT GT_STB_MATNR BY MATNR WERKS.
      DELETE ADJACENT DUPLICATES FROM GT_STB_MATNR COMPARING MATNR WERKS.
      IF GT_STB_MATNR[] IS NOT INITIAL.
        SELECT FROM MARA AS A INNER JOIN V_MARC_MD AS B
                                      ON B~MATNR = A~MATNR
                              INNER JOIN MBEW      AS C
                                      ON C~MATNR = B~MATNR
                                     AND C~BWKEY = B~WERKS
                                     AND C~BKLAS IN ( @TEXT-O06, @TEXT-O07 ) "3010, 3011
                              INNER JOIN T001W     AS D
                                      ON D~WERKS = B~WERKS
                              INNER JOIN T023T     AS E
                                      ON E~MATKL = A~MATKL
                                     AND E~SPRAS = @SY-LANGU
                              INNER JOIN MAKT      AS F
                                      ON F~MATNR = A~MATNR
                                     AND F~SPRAS = @SY-LANGU
               FIELDS
                B~MATNR, B~WERKS, A~MATKL, A~MEINS,
                D~NAME1, E~WGBEZ, E~WGBEZ60, F~MAKTX, B~WERKS AS BERID

              FOR ALL ENTRIES IN @GT_STB_MATNR
               WHERE B~MATNR EQ @GT_STB_MATNR-MATNR
                 AND B~WERKS EQ @GT_STB_MATNR-WERKS
                 AND A~MATNR IN @S_MATNR
                 AND A~MATKL IN @S_MATKL
                 AND A~LVORM EQ @SPACE
                 AND B~WERKS IN @S_WERKS
                 AND B~DISPO IN @S_DISPO
                 AND B~EKGRP IN @S_EKGRP
                 AND B~LVORM EQ @SPACE
         INTO CORRESPONDING FIELDS OF TABLE @GT_MAIN.
      ENDIF.
    ELSE.
      SORT GT_STB_MATNR BY MATNR WERKS.
      DELETE ADJACENT DUPLICATES FROM GT_STB_MATNR COMPARING MATNR WERKS.
      IF GT_STB_MATNR[] IS NOT INITIAL.
        SELECT FROM MARA AS A INNER JOIN V_MARC_MD AS B
                                      ON B~MATNR = A~MATNR
                              INNER JOIN MBEW      AS C
                                      ON C~MATNR = B~MATNR
                                     AND C~BWKEY = B~WERKS
                                     AND C~BKLAS IN ( @TEXT-O06, @TEXT-O07 ) "3010, 3011
                              INNER JOIN T001W     AS D
                                      ON D~WERKS = B~WERKS
                              INNER JOIN T023T     AS E
                                      ON E~MATKL = A~MATKL
                                     AND E~SPRAS = @SY-LANGU
                              INNER JOIN MAKT      AS F
                                      ON F~MATNR = A~MATNR
                                     AND F~SPRAS = @SY-LANGU
                              INNER JOIN PPH_DBVM  AS G
                                    ON G~MATNR = B~MATNR AND
                                       G~WERKS = B~WERKS

               FIELDS
                B~MATNR, B~WERKS, A~MATKL, A~MEINS,
                D~NAME1, E~WGBEZ, E~WGBEZ60, F~MAKTX, G~BERID

              FOR ALL ENTRIES IN @GT_STB_MATNR
               WHERE B~MATNR EQ @GT_STB_MATNR-MATNR
                 AND B~WERKS EQ @GT_STB_MATNR-WERKS
                 AND A~MATNR IN @S_MATNR
                 AND A~MATKL IN @S_MATKL
                 AND A~LVORM EQ @SPACE
                 AND B~WERKS IN @S_WERKS
                 AND B~DISPO IN @S_DISPO
                 AND B~EKGRP IN @S_EKGRP
                 AND B~LVORM EQ @SPACE
                 AND G~BERID IN @S_BERID
         INTO CORRESPONDING FIELDS OF TABLE @GT_MAIN.

        SORT GT_MAIN BY WERKS MATNR MATKL BERID.
        DELETE ADJACENT DUPLICATES FROM GT_MAIN COMPARING WERKS MATNR MATKL BERID.
      ENDIF.
    ENDIF.
  ENDIF.

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

  _G_INIT: GT_BASE_DAT, GT_BASE_BERID_DAT.

*-
  GT_BASE_DAT[] = CORRESPONDING #( GT_MAIN DISCARDING DUPLICATES
                                               MAPPING WERKS = WERKS MATNR = MATNR ).
*-
  GT_BASE_BERID_DAT[] = CORRESPONDING #( GT_MAIN DISCARDING DUPLICATES
                                                 MAPPING WERKS = WERKS MATNR = MATNR BERID = BERID ).

ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_MAP_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM GET_MAP_DATA.

**  A   기초재고 ( 가용재고 )
**  B   소요량
**  C   입고예정
**  D   관리전환
**  E   예정청구수량
**  F   확정청구수량
**  G   기말재고
**  H   수동발주잔량
**  I   예상기말재고

  _G_INIT: GT_MRP_ORG, GT_MRP_DAT.

*- "최신데이타
  TRY.
**      IF GT_BASE_DAT[] IS NOT INITIAL.
**        SELECT (GT_SEL_LIST)
**          INTO CORRESPONDING FIELDS OF TABLE GT_MRP_ORG
**          FROM ZTMM20010 FOR ALL ENTRIES IN GT_BASE_DAT
**          WHERE MATNR = GT_BASE_DAT-MATNR AND WERKS = GT_BASE_DAT-WERKS.
**      ENDIF.
      IF GT_BASE_BERID_DAT[] IS NOT INITIAL.
        SELECT (GT_SEL_LIST)
          INTO CORRESPONDING FIELDS OF TABLE GT_MRP_ORG
          FROM ZTMM20010 FOR ALL ENTRIES IN GT_BASE_BERID_DAT
          WHERE MATNR = GT_BASE_BERID_DAT-MATNR
            AND WERKS = GT_BASE_BERID_DAT-WERKS
            AND BERID = GT_BASE_BERID_DAT-BERID.
      ENDIF.
*--
    CATCH CX_SY_DYNAMIC_OSQL_ERROR.
  ENDTRY.

*-
  DATA: LV_CLAUSE_1 TYPE STRING,
        LV_CLAUSE_2 TYPE STRING,
        LV_CLAUSE_3 TYPE STRING,
        LV_CLAUSE_4 TYPE STRING,
        LV_CLAUSE_5 TYPE STRING,
        LV_CLAUSE_6 TYPE STRING,
        LV_CLAUSE_7 TYPE STRING.

  SORT GT_COMM_CFG_B BY FIELD1.
  PERFORM : GET_MAKE_CLAUSE USING 'A' CHANGING LV_CLAUSE_1, "가용재고
            GET_MAKE_CLAUSE USING 'B' CHANGING LV_CLAUSE_2, "소요량
            GET_MAKE_CLAUSE USING 'C' CHANGING LV_CLAUSE_3, "입고예졍
            GET_MAKE_CLAUSE USING 'D' CHANGING LV_CLAUSE_4, "관리전환
            GET_MAKE_CLAUSE USING 'E' CHANGING LV_CLAUSE_5, "예정청구수량
            GET_MAKE_CLAUSE USING 'F' CHANGING LV_CLAUSE_6, "확정청구수량
            GET_MAKE_CLAUSE USING 'H' CHANGING LV_CLAUSE_7. "수동발주잔량

*-
  PERFORM GET_OPT_MRP_DATA USING : 'A' LV_CLAUSE_1,
                                   'B' LV_CLAUSE_2,
                                   'C' LV_CLAUSE_3,
                                   'D' LV_CLAUSE_4,
                                   'E' LV_CLAUSE_5,
                                   'F' LV_CLAUSE_6,
                                   'H' LV_CLAUSE_7.

*-
  SORT GT_MRP_DAT BY OPT MATNR WERKS.

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
  SORT GT_COMM_CFG_B BY FIELD1 FIELD3.
  CLEAR LV_CNT.
  LOOP AT GT_COMM_CFG_B INTO DATA(LS_CFG) WHERE FIELD1 = IV_FIELD1.
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
        LV_CLAUSE   = | { LS_CFG-FIELD2 } { LV_OP_EQ } |.
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
*& Form GET_OPT_MRP_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> P_
*&      --> LV_CLAUSE_1
*&---------------------------------------------------------------------*
FORM GET_OPT_MRP_DATA USING IV_OPT IV_CLAUSE.

  DATA: LT_MRP_DAT TYPE STANDARD TABLE OF TY_MRP_DAT,
        LS_MRP_DAT LIKE LINE OF LT_MRP_DAT.

  _G_INIT: LT_MRP_DAT. CLEAR: LS_MRP_DAT.

*-
  LOOP AT GT_MRP_ORG INTO DATA(LS_MRP_ORG) WHERE (IV_CLAUSE).

    MOVE-CORRESPONDING LS_MRP_ORG TO LS_MRP_DAT.
    LS_MRP_DAT-OPT = IV_OPT.
    APPEND LS_MRP_DAT TO LT_MRP_DAT.
  ENDLOOP.

  CHECK LT_MRP_DAT[] IS NOT INITIAL.
*-
  APPEND LINES OF LT_MRP_DAT TO GT_MRP_DAT[].


ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_LIFNR_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM GET_LIFNR_DATA .

  DATA: LS_PLANWH LIKE LINE OF GT_PLANWH.

  _G_INIT: GT_EORD, GT_EKPO, GT_PRD, GT_PLANWH, GT_INFOP.

******************************************************
*- 구매정보레코드 테이블(ZSVBMMINFOPRICE)에서 아래 조건에 해당하는 유효한 공급업체 조회
******************************************************
  DATA(LT_BASE_DAT) = GT_MAIN[].
  SORT LT_BASE_DAT BY MATNR. DELETE ADJACENT DUPLICATES FROM LT_BASE_DAT COMPARING MATNR.

*-
  IF LT_BASE_DAT[] IS NOT INITIAL.
    SELECT FROM ZSVBMMINFOPRICE
           FIELDS MATNR, LIFNR, NAME1
           FOR ALL ENTRIES IN @LT_BASE_DAT WHERE MATNR = @LT_BASE_DAT-MATNR
                                             AND PURCHASINGORGANIZATION = @TEXT-O11  "1000
                                             AND PLANT EQ @SPACE
                                             AND VALIDPRICE = 'Y'
                                             AND EINA_LOEKZ = @SPACE
                                             AND EINA_LOEKZ = @SPACE
    INTO CORRESPONDING FIELDS OF TABLE @GT_INFOP.
  ENDIF.
  FREE LT_BASE_DAT.

  SORT GT_INFOP BY MATNR LIFNR. DELETE ADJACENT DUPLICATES FROM GT_INFOP COMPARING MATNR LIFNR.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_INPUTED_BOM
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM GET_INPUTED_BOM.

  CHECK S_PROD[] IS NOT INITIAL.

  DATA: LS_MATNR      TYPE ZSMM00001,
        LT_MATNR      TYPE STANDARD TABLE OF ZSMM00001 WITH HEADER LINE,
        LT_MATNR_MKAL TYPE STANDARD TABLE OF ZSMM00001,
        LT_MATNR_VER  TYPE STANDARD TABLE OF ZSMM00001 WITH HEADER LINE,
        LT_STB        TYPE STANDARD TABLE OF ZSMM_STPOX WITH HEADER LINE,
        LT_RETURN     TYPE STANDARD TABLE OF BAPIRET2 WITH HEADER LINE.

  _G_INIT: LT_MATNR, LT_MATNR_MKAL, LT_MATNR_VER, LT_STB.

*-
  READ TABLE GT_COMM_CFG_B312 INTO DATA(LS_CFG) INDEX 1.

*-
  LOOP AT S_WERKS INTO DATA(LS_WERKS).

    READ TABLE S_PROD INTO DATA(LS_PROD) INDEX 1.
    IF SY-SUBRC = 0.
      DATA(LV_TABIX) = SY-TABIX.
      LOOP AT S_PROD INTO LS_PROD FROM LV_TABIX.
        IF 1 <> 1.
          EXIT.
        ELSE.
          LS_MATNR-MATNR = LS_PROD-LOW.
          LS_MATNR-WERKS = LS_WERKS-LOW.
          APPEND LS_MATNR TO LT_MATNR.
        ENDIF.

      ENDLOOP.
    ENDIF.
  ENDLOOP.

  SORT LT_MATNR BY MATNR WERKS.
  DELETE ADJACENT DUPLICATES FROM LT_MATNR COMPARING MATNR WERKS.
  IF LT_MATNR[] IS NOT INITIAL.
    SELECT FROM MKAL
               FIELDS
               MATNR, WERKS, VERID
               FOR ALL ENTRIES IN @LT_MATNR
               WHERE MATNR = @LT_MATNR-MATNR
                 AND WERKS = @LT_MATNR-WERKS
                 AND ( ADATU <= @SY-DATLO AND BDATU >= @SY-DATLO )
                 AND MKSP EQ @SPACE "잠금처리 제외
       INTO CORRESPONDING FIELDS OF TABLE @LT_MATNR_MKAL.

  ENDIF.
  FREE LT_MATNR.

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
      INTO CORRESPONDING FIELDS OF TABLE @LT_MATNR_VER.

    ENDIF.
    FREE LT_MATNR_MKAL.

  ELSE.
    LT_MATNR_VER[] = LT_MATNR_MKAL[].
  ENDIF.
  SORT LT_MATNR_VER BY MATNR WERKS VERID.
  DELETE ADJACENT DUPLICATES FROM LT_MATNR_VER COMPARING MATNR WERKS VERID.
*-
  call function 'ZFMM_BOM_EXPL_MAT'
    EXPORTING
      IV_DATUV  = SY-DATLO
      IV_MEHRS  = 'X'
*     IV_ZFDLV  = 'X'      "2022.01.04  ZRNN2130의 경우 자율납룸 자재 뿐 아니라 모두 조회되어야 함으로 주석 처리 함.
      IV_MDMPS  = 'X'
    IMPORTING
      EV_RETURN = LT_RETURN
    TABLES
      IT_MATNR  = LT_MATNR_VER
      IT_STB    = LT_STB.

*-
  IF LT_STB[] IS NOT INITIAL.
    GT_STB[] = VALUE #( BASE GT_STB[] FOR LS_VALUE IN LT_STB
                          ( MATNR = CONV MATNR( LS_VALUE-IDNRK ) WERKS = LS_VALUE-WERKS ) ).
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

  DATA: LS_DAY      TYPE TY_DAY,
        LV_DATE     TYPE DATUM,
        LV_CDATE    TYPE DATUM,
        LV_CNT      TYPE SY-TABIX,
        LV_DAY      TYPE NUMC2,
        LV_CDAY     TYPE NUMC3,
        LV_HDAY     TYPE CHAR1,
        LS_HOLIDAY  LIKE LINE OF GT_HOLIDAY,
        LS_CONF_DAY LIKE LINE OF GT_CONF_DAY,
        LV_CONF_CNT TYPE SY-TABIX,
        LV_CONF_DD  TYPE NUMC2,
        LV_TIME     TYPE MARA-MHDHB.

*-
  _G_INIT: GT_DAY, GT_HOLIDAY, GT_CONF_DAY.
  CLEAR: GV_FIRST_DAY, GV_DAY_DO, LV_CONF_CNT, LV_CONF_DD, GV_CONFIRM_DAY, GV_CONFIRM_DD.

*-
  READ TABLE GT_COMM_CFG_C INTO DATA(LS_COMM_CFG_C) INDEX 1.

*-
  LV_DAY    = LS_COMM_CFG_C-FIELD4.
  LV_DATE = SY-DATLO.

  IF LV_DAY >= 0.
    PERFORM F_RP_CALC_DATE_IN_INTERVAL USING LV_DATE LV_DAY 0 0 '+' CHANGING GV_FIRST_DAY.
  ELSE.
    PERFORM F_RP_CALC_DATE_IN_INTERVAL USING LV_DATE LV_DAY 0 0 '-' CHANGING GV_FIRST_DAY.
  ENDIF.

  GV_DAY_DO = LS_COMM_CFG_C-FIELD2.

  CLEAR: LV_CNT, LV_DAY, LV_TIME.

  DO GV_DAY_DO TIMES.

    ADD 1 TO LV_CNT.

*-
    IF     LV_CNT = 1.
      LV_CDATE  = GV_FIRST_DAY.
    ELSE.
      ADD 1 TO LV_TIME.
      PERFORM F_ADD_TIME_TO_DATE USING GV_FIRST_DAY LV_TIME '' CHANGING LV_CDATE.

    ENDIF.

    ADD 1 TO LV_CDAY.

    LS_DAY-SEQ  = LV_CNT.
    LS_DAY-DATE = LV_CDATE.
    LS_DAY-DAY  = |{ TEXT-O10 }| && LV_CDAY.

    PERFORM CHANGE_HEADER_DATE USING LV_CDATE CHANGING LS_DAY-DAY_TXT.

    APPEND LS_DAY TO GT_DAY.
  ENDDO.

*-
  DATA(LT_WERKS) = GT_MAIN[].
  SORT LT_WERKS BY WERKS. DELETE ADJACENT DUPLICATES FROM LT_WERKS COMPARING WERKS.

  IF LT_WERKS[] IS NOT INITIAL.
    SELECT FROM T001W FIELDS WERKS, FABKL FOR ALL ENTRIES IN @LT_WERKS WHERE WERKS = @LT_WERKS-WERKS INTO TABLE @DATA(LT_FABKL).
  ENDIF.
  FREE LT_WERKS.

*-
  READ TABLE GT_COMM_CFG_D INTO DATA(LS_COMM_CFG_D) WITH TABLE KEY IDX_FIELD1 COMPONENTS FIELD1 = 'E'.
  LV_CONF_DD = GV_CONFIRM_DD = LS_COMM_CFG_D-FIELD4.

*-
  CLEAR LV_CONF_CNT.

  SORT GT_DAY BY SEQ.

  LOOP AT LT_FABKL INTO DATA(LS_FABKL).
    READ TABLE GT_DAY INDEX 1 TRANSPORTING NO FIELDS.
    IF SY-SUBRC = 0.
      DATA(LV_TABIX) = SY-TABIX.
      LOOP AT GT_DAY INTO LS_DAY FROM LV_TABIX.
        IF 1 <> 1.
          EXIT.
        ELSE.

          PERFORM GET_PLANT_HOLIDAY USING LS_FABKL-WERKS LS_FABKL-FABKL LS_DAY-DATE CHANGING LV_HDAY.
          IF LV_HDAY IS NOT INITIAL.
            LS_HOLIDAY-WERKS = LS_FABKL-WERKS.
            LS_HOLIDAY-DAY   = LS_DAY-DAY.
            APPEND LS_HOLIDAY TO GT_HOLIDAY.
          ELSE.
            ADD 1 TO LV_CONF_CNT.
            IF LV_CONF_CNT = LV_CONF_DD.
              LS_CONF_DAY-WERKS = LS_FABKL-WERKS.
              LS_CONF_DAY-DATE  = LS_DAY-DATE.
              LS_CONF_DAY-DAY   = LS_DAY-DAY.
              APPEND LS_CONF_DAY TO GT_CONF_DAY.
            ENDIF.
          ENDIF.

        ENDIF.

      ENDLOOP.

      CLEAR : LV_CONF_CNT.
    ENDIF.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CHANGE_HEADER_DATE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LV_CDATE
*&      <-- LS_DAY_DAY_TXT
*&---------------------------------------------------------------------*
FORM CHANGE_HEADER_DATE USING IV_DATE CHANGING EV_CDATE.

  CLEAR EV_CDATE.

*-
  CHECK IV_DATE IS NOT INITIAL.

*-
  DATA: LV_NAME TYPE KURZT.
  PERFORM F_RH_GET_DATE_DAYNAME  USING SY-LANGU
                                       IV_DATE
                              CHANGING LV_NAME.

*-

* read user profile from table USR01
  SELECT SINGLE DATFM FROM USR01 INTO @DATA(LV_DATFM) WHERE BNAME = @SY-UNAME.
  IF SY-SUBRC = 0.
    CASE LV_DATFM.
      WHEN '1'.
        EV_CDATE = |{ IV_DATE+06(02) }| && |.| && |{ IV_DATE+04(02) }| && |(| && LV_NAME(01) && |)|.
      WHEN '2'.
        EV_CDATE = |{ IV_DATE+04(02) }| && |/| && |{ IV_DATE+06(02) }| && |(| && LV_NAME(01) && |)|.
      WHEN '3'.
        EV_CDATE = |{ IV_DATE+04(02) }| && |-| && |{ IV_DATE+06(02) }| && |(| && LV_NAME(01) && |)|.
      WHEN '4'.
        EV_CDATE = |{ IV_DATE+04(02) }| && |.| && |{ IV_DATE+06(02) }| && |(| && LV_NAME(01) && |)|.
      WHEN '5' OR 'A' OR 'B' OR 'C'.
        EV_CDATE = |{ IV_DATE+04(02) }| && |/| && |{ IV_DATE+06(02) }| && |(| && LV_NAME(01) && |)|.
      WHEN '6'.
        EV_CDATE = |{ IV_DATE+04(02) }| && |-| && |{ IV_DATE+06(02) }| && |(| && LV_NAME(01) && |)|.
      WHEN OTHERS.
        RAISE ERROR_IN_READING_USR01.
    ENDCASE.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_RH_GET_DATE_DAYNAME
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> SY_LANGU
*&      --> GT_POP_DFAELL
*&      <-- GT_POP_XWOTAG2
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
*& Form F_RP_CALC_DATE_IN_INTERVAL
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LV_DATE
*&      --> LV_DAY
*&      --> P_0
*&      --> P_0
*&      --> P_
*&      <-- GV_FIRST_DAY
*&---------------------------------------------------------------------*
FORM F_RP_CALC_DATE_IN_INTERVAL USING IV_DATE TYPE P0001-BEGDA
                                      IV_DAY    TYPE T5A4A-DLYDY
                                      IV_MONTH  TYPE T5A4A-DLYMO
                                      IV_YEAR   TYPE T5A4A-DLYYR
                                      IV_SIGN   TYPE T5A4A-SPLIT
                             CHANGING EV_DATE   TYPE P0001-BEGDA.

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
*& Form GET_PLANT_HOLIDAY
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LS_FABKL_WERKS
*&      --> LS_FABKL_FABKL
*&      --> LS_DAY_DATE
*&      <-- LV_HDAY
*&---------------------------------------------------------------------*
FORM GET_PLANT_HOLIDAY USING IV_WERKS IV_FABKL
                                 IV_DATE
                        CHANGING EV_HDAY.

  DATA: LV_CDATE                TYPE SCAL-DATE,
        LV_WORKINGDAY_INDICATOR TYPE SCAL-INDICATOR.


  CLEAR EV_HDAY.

*-
  CHECK IV_FABKL IS NOT INITIAL AND IV_DATE IS NOT INITIAL.

  CLEAR : LV_CDATE, LV_WORKINGDAY_INDICATOR.
  CALL FUNCTION 'DATE_CONVERT_TO_FACTORYDATE'
    EXPORTING
      CORRECT_OPTION               = '+'
      DATE                         = IV_DATE
      FACTORY_CALENDAR_ID          = IV_FABKL
    IMPORTING
      DATE                         = LV_CDATE
      WORKINGDAY_INDICATOR         = LV_WORKINGDAY_INDICATOR
    EXCEPTIONS
      CALENDAR_BUFFER_NOT_LOADABLE = 1
      CORRECT_OPTION_INVALID       = 2
      DATE_AFTER_RANGE             = 3
      DATE_BEFORE_RANGE            = 4
      DATE_INVALID                 = 5
      FACTORY_CALENDAR_NOT_FOUND   = 6
      OTHERS                       = 7.

  IF LV_WORKINGDAY_INDICATOR IS NOT INITIAL.
    EV_HDAY = LV_WORKINGDAY_INDICATOR.
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

**  A 기초재고 ( 가용재고 )
**  B 소요량
**  C 입고예정
**  D 관리전환
**  D1 기말재고
**  E 예정청구수량
**  F 확정청구수량
**  H 수동발주잔량
**  I 예상기말재고

*-
  DATA: LV_MEINS TYPE MEINS,
        LV_DEL   TYPE CHAR1.

*-
  _G_INIT: GT_DISP, GT_DATE.

*-
  SORT : GT_MAIN    BY WERKS MATNR,
         GT_MRP_DAT BY OPT MATNR WERKS,
         GT_DAY     BY DATE.

  LOOP AT GT_MAIN INTO DATA(LS_MAIN).
    CLEAR: LV_MEINS, GV_BASE_STK, LV_DEL. _G_INIT: GT_DATE.

    LV_MEINS      = LS_MAIN-MEINS.

*-- 최초 가용재고 ( GV_BASE_STK )
    PERFORM GET_BASE_STOCK USING 'A' LS_MAIN.

*-- 소요량
    PERFORM APPEND_OPT_DATA USING 'B' TEXT-T14 LS_MAIN.

*--입고 예정
    PERFORM APPEND_OPT_DATA USING 'C' TEXT-T15 LS_MAIN.

*-- 관리 전환
    PERFORM APPEND_OPT_DATA USING 'D' TEXT-T16 LS_MAIN.

*-- 예정청구수량
    PERFORM APPEND_OPT_DATA USING 'E' TEXT-T17 LS_MAIN.

*-- 확정청구수량
    PERFORM APPEND_OPT_DATA USING 'F' TEXT-T18 LS_MAIN.

*-- 수동발주잔량
    PERFORM APPEND_OPT_DATA USING 'H' TEXT-T19 LS_MAIN.

***----------------------------------------------------------------***
*** 기초재고 -> 기말재고 -> 예상기말재고
***----------------------------------------------------------------***
    PERFORM CACUL_STOCK USING 'A' TEXT-T11 "기초재고
                              'D1' TEXT-T12 "기말재고
                              'I' TEXT-T13 "예상기말재고
                              LS_MAIN.
***----------------------------------------------------------------***

*-  추가선택조건 Filtering
    PERFORM FILTERING_DISP_DATA  USING LS_MAIN-WERKS CHANGING LV_DEL.
    PERFORM FILTERING_DISP_DATA2 USING LS_MAIN-WERKS CHANGING LV_DEL.

*--
    IF LV_DEL IS INITIAL.
      PERFORM APPEND_ALL_DATA USING LS_MAIN LV_MEINS.
    ENDIF.

  ENDLOOP.

*-
  IF P_RD1C = 'X'.
    SORT GT_DISP BY KEY.
  ELSE.
    SORT GT_DISP BY WERKS MATKL MATNR LIFNR OPT.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_BASE_STOCK
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> P_
*&      --> LS_MAIN
*&---------------------------------------------------------------------*
FORM GET_BASE_STOCK USING IV_OPT IS_MAIN TYPE TY_MAIN.

*-
  CLEAR GV_BASE_STK.

*-
  READ TABLE GT_MRP_DAT INTO DATA(LS_MRP_DAT) WITH KEY OPT = IV_OPT
                                                     MATNR = IS_MAIN-MATNR
                                                     WERKS = IS_MAIN-WERKS BINARY SEARCH.
  IF SY-SUBRC = 0.
    DATA(LV_TABIX) = SY-TABIX.
    LOOP AT GT_MRP_DAT INTO LS_MRP_DAT FROM LV_TABIX.
      IF LS_MRP_DAT-OPT   <> IV_OPT OR
         LS_MRP_DAT-MATNR <> IS_MAIN-MATNR OR
         LS_MRP_DAT-WERKS <> IS_MAIN-WERKS.
        EXIT.

      ELSE.
        ADD LS_MRP_DAT-MNG01 TO GV_BASE_STK.
      ENDIF.
    ENDLOOP.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form APPEND_OPT_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> P_
*&      --> TEXT_T14
*&      --> LS_MAIN
*&---------------------------------------------------------------------*
FORM APPEND_OPT_DATA USING IV_OPT IV_TEXT
                              IS_MAIN TYPE TY_MAIN.

**  A 기초재고 ( 가용재고 )
**  B 소요량
**  C 입고예정
**  D 관리전환
**  E 예정청구수량
**  F 확정청구수량
**  G 기말재고
**  H 수동발주잔량
**  I 예상기말재고

  DATA: LS_DISP     TYPE TS_DISP,
        LS_DATE     TYPE TY_DATE,
        LV_CONF_DAY TYPE DATUM,
        LV_DAT001   TYPE CHAR6 VALUE 'DAT001'.

  DATA: LV_QTY_EXIST TYPE CHAR1.

*-
  CLEAR : LS_DISP, LS_DATE, LV_QTY_EXIST.

*-
  LS_DATE-OPT     = IV_OPT.
  LS_DATE-OPT_TXT = IV_TEXT.

*-
  FIELD-SYMBOLS: <LV_FIELD> TYPE CHAR6,
                 <LV_VALUE> TYPE ZSMM_DATE_VALUE-DAT001.

*-
  IF IV_OPT = 'E' OR IV_OPT = 'F'.
    READ TABLE GT_CONF_DAY INTO DATA(LS_CONF_DAY) WITH TABLE KEY IDX_KEY
                                                  COMPONENTS WERKS = IS_MAIN-WERKS.
    IF SY-SUBRC = 0.
      LV_CONF_DAY = LS_CONF_DAY-DATE.
    ENDIF.
  ENDIF.

*-
  TRY.
      READ TABLE GT_MRP_DAT INTO DATA(LS_MRP_DAT) WITH KEY OPT = IV_OPT
                                                         MATNR = IS_MAIN-MATNR
                                                         WERKS = IS_MAIN-WERKS BINARY SEARCH.
      IF SY-SUBRC = 0.
        DATA(LV_TABIX) = SY-TABIX.
        LOOP AT GT_MRP_DAT INTO LS_MRP_DAT FROM LV_TABIX.
          IF LS_MRP_DAT-OPT   <> IV_OPT OR
             LS_MRP_DAT-MATNR <> IS_MAIN-MATNR OR
             LS_MRP_DAT-WERKS <> IS_MAIN-WERKS.
            EXIT.

          ELSE.

            IF IV_OPT = 'E'. "예정확정수량
              IF LV_CONF_DAY <= LS_MRP_DAT-DAT01.
                READ TABLE GT_DAY INTO DATA(LS_DAY) WITH KEY DATE = LS_MRP_DAT-DAT01 BINARY SEARCH.
                IF SY-SUBRC = 0.
                  ASSIGN LS_DAY-DAY TO <LV_FIELD>.
                ENDIF.
              ENDIF.
            ELSEIF IV_OPT = 'F'. "확정수량
              IF LV_CONF_DAY > LS_MRP_DAT-DAT01.
                READ TABLE GT_DAY INTO LS_DAY WITH KEY DATE = LS_MRP_DAT-DAT01 BINARY SEARCH.
                IF SY-SUBRC = 0.
                  ASSIGN LS_DAY-DAY TO <LV_FIELD>.
                ELSE. "초기일자에 SUM
***---------------------------------------------------------------------------------------***
                  IF GV_FIRST_DAY > LS_MRP_DAT-DAT01.
                    ASSIGN LV_DAT001 TO <LV_FIELD>.
                  ENDIF.
***---------------------------------------------------------------------------------------***
                ENDIF.
              ENDIF.
            ELSE.
              READ TABLE GT_DAY INTO LS_DAY WITH KEY DATE = LS_MRP_DAT-DAT01 BINARY SEARCH.
              IF SY-SUBRC = 0.
                ASSIGN LS_DAY-DAY TO <LV_FIELD>.

              ELSE.
***---------------------------------------------------------------------------------------***
                IF GV_FIRST_DAY > LS_MRP_DAT-DAT01.
                  ASSIGN LV_DAT001 TO <LV_FIELD>.
                ENDIF.
***---------------------------------------------------------------------------------------***
              ENDIF.

            ENDIF.

            IF <LV_FIELD> IS ASSIGNED.
              ASSIGN COMPONENT <LV_FIELD> OF STRUCTURE LS_DATE TO <LV_VALUE>.
              <LV_VALUE> = <LV_VALUE> + LS_MRP_DAT-MNG01.
              LS_DATE-MGSUM = LS_DATE-MGSUM + LS_MRP_DAT-MNG01. "SUM
              IF LS_MRP_DAT-MNG01 NE 0 AND LV_QTY_EXIST IS INITIAL. LV_QTY_EXIST = 'X'. ENDIF. "U4
            ENDIF.
            UNASSIGN : <LV_FIELD>, <LV_VALUE>.
          ENDIF.
        ENDLOOP.
      ENDIF.

*--
    CATCH CX_SY_ARITHMETIC_ERROR INTO DATA(LR_ORF_1).
  ENDTRY.

*-
  LS_DATE-QTY_EXIST = LV_QTY_EXIST. "U4
  APPEND LS_DATE TO GT_DATE. CLEAR : LS_DATE.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form CACUL_STOCK
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> P_
*&      --> TEXT_T11
*&      --> P_
*&      --> TEXT_T12
*&      --> P_
*&      --> TEXT_T13
*&      --> LS_MAIN
*&---------------------------------------------------------------------*
FORM CACUL_STOCK USING IV_OPT_A IV_OPT_A_TEXT
                         IV_OPT_B IV_OPT_B_TEXT
                         IV_OPT_C IV_OPT_C_TEXT
                         IS_MAIN TYPE TY_MAIN.

  FIELD-SYMBOLS: <LV_FIELD>   TYPE CHAR6,
                 <LV_VALUE_A> TYPE ZSMM_DATE_VALUE-DAT001,
                 <LV_VALUE_B> TYPE ZSMM_DATE_VALUE-DAT001,
                 <LV_VALUE_C> TYPE ZSMM_DATE_VALUE-DAT001.

  DATA: LS_DATE_A    TYPE TY_DATE, "기초재고
        LS_DATE_B    TYPE TY_DATE, "기말재고
        LS_DATE_C    TYPE TY_DATE, "예상기말재고
        LV_BASE_STK  TYPE ZTMM2001H-MNG01,
        LV_CACUL_STK TYPE ZTMM2001H-MNG01,
        LV_LAST_STK  TYPE ZTMM2001H-MNG01,
        LV_FBASE_STK TYPE ZTMM2001H-MNG01, "전일기말재고
        LV_FLAST_STK TYPE ZTMM2001H-MNG01, "전일예상기말재고
        LV_B_SUM     TYPE FGD_QNT,
        LV_C_SUM     TYPE FGD_QNT.

*-
  CLEAR: LS_DATE_A, LS_DATE_B, LS_DATE_C, LV_BASE_STK, LV_CACUL_STK, LV_LAST_STK, LV_FBASE_STK, LV_FLAST_STK,
         LV_B_SUM, LV_C_SUM.

  TRY.

      LOOP AT GT_DAY INTO DATA(LS_DAY) FROM 1.

        DATA(LV_TABIX) = SY-TABIX.

        ASSIGN LS_DAY-DAY TO <LV_FIELD>.
        CHECK <LV_FIELD> IS ASSIGNED.

*-- 기초재고
        LS_DATE_A-OPT     = IV_OPT_A.
        LS_DATE_A-OPT_TXT = IV_OPT_A_TEXT.

        IF LV_TABIX = 1.
          LS_DATE_A-DAT001   = LV_BASE_STK = GV_BASE_STK. "당일기초재고
          IF GV_BASE_STK NE 0. LS_DATE_A-QTY_EXIST = 'X'. ENDIF. "U4
        ELSE.
          ASSIGN COMPONENT <LV_FIELD> OF STRUCTURE LS_DATE_A TO <LV_VALUE_A>.
          IF <LV_VALUE_A> IS ASSIGNED.
            <LV_VALUE_A> = LV_BASE_STK = LV_FBASE_STK. "전일기말재고
          ENDIF.

        ENDIF.

*-- 기말재고 ( 기초재고 - 소요량 + 입고예정 + 관리전환 )
        LS_DATE_B-OPT     = IV_OPT_B.
        LS_DATE_B-OPT_TXT = IV_OPT_B_TEXT.

        CLEAR LV_CACUL_STK.
        LV_CACUL_STK = LV_BASE_STK.

        IF <LV_FIELD> IS ASSIGNED.
          "소요량
          PERFORM SET_RECACUL_STK USING GT_DATE 'B'<LV_FIELD> CHANGING LV_CACUL_STK.

          "입고예정
          PERFORM SET_RECACUL_STK USING GT_DATE 'C'<LV_FIELD> CHANGING LV_CACUL_STK.

          "관리전환
          PERFORM SET_RECACUL_STK USING GT_DATE 'D'<LV_FIELD> CHANGING LV_CACUL_STK.

        ENDIF.

        ASSIGN COMPONENT <LV_FIELD> OF STRUCTURE LS_DATE_B TO <LV_VALUE_B>.
        IF <LV_VALUE_B> IS ASSIGNED.
          <LV_VALUE_B> = LV_LAST_STK = LV_FBASE_STK = LV_CACUL_STK.  "기말재고 = 예상기말재고(초기값)
          UNASSIGN : <LV_VALUE_B>.
        ENDIF.

        ADD LV_CACUL_STK TO LV_B_SUM.

*-- 예상기말재고 ( (기말재고 - 기초재고) + 예정청구수량 + 확정청구수량 + 수동발주잔량 )
        LS_DATE_C-OPT     = IV_OPT_C.
        LS_DATE_C-OPT_TXT = IV_OPT_C_TEXT.

        CLEAR LV_CACUL_STK.
        IF <LV_FIELD> IS ASSIGNED.
          "수동발주잔량
          READ TABLE GT_DATE INTO DATA(LS_ODATE) WITH TABLE KEY IDX_KEY
                             COMPONENTS OPT = 'H'.
          IF SY-SUBRC = 0.
            ASSIGN COMPONENT <LV_FIELD> OF STRUCTURE LS_ODATE TO <LV_VALUE_C>.
            IF <LV_VALUE_C> IS ASSIGNED.
              LV_LAST_STK = LV_LAST_STK + <LV_VALUE_C>.
              UNASSIGN : <LV_VALUE_C>.
            ENDIF.
          ENDIF.
          "예정청구수량
          READ TABLE GT_DATE INTO LS_ODATE WITH TABLE KEY IDX_KEY
                             COMPONENTS OPT = 'E'.
          IF SY-SUBRC = 0.
            ASSIGN COMPONENT <LV_FIELD> OF STRUCTURE LS_ODATE TO <LV_VALUE_B>.
            IF <LV_VALUE_B> IS ASSIGNED.
              LV_LAST_STK = LV_LAST_STK + <LV_VALUE_B>.
              UNASSIGN : <LV_VALUE_B>.
            ENDIF.
          ENDIF.
          "확정청구수량
          READ TABLE GT_DATE INTO LS_ODATE WITH TABLE KEY IDX_KEY
                             COMPONENTS OPT = 'F'.
          IF SY-SUBRC = 0.
            ASSIGN COMPONENT <LV_FIELD> OF STRUCTURE LS_ODATE TO <LV_VALUE_B>.
            IF <LV_VALUE_B> IS ASSIGNED.
              LV_LAST_STK = LV_LAST_STK + <LV_VALUE_B>.
              UNASSIGN : <LV_VALUE_B>.
            ENDIF.
          ENDIF.
        ENDIF.

        ASSIGN COMPONENT <LV_FIELD> OF STRUCTURE LS_DATE_C TO <LV_VALUE_C>.
        IF <LV_VALUE_C> IS ASSIGNED.
          IF LV_TABIX = 1.
            <LV_VALUE_C> = LV_FLAST_STK = LV_LAST_STK.
          ELSE.
            <LV_VALUE_C> = LV_LAST_STK + LV_FLAST_STK - LV_BASE_STK.  "예상기말재고( 기초재고제외 )
            LV_FLAST_STK = <LV_VALUE_C>.
          ENDIF.
          UNASSIGN : <LV_VALUE_C>.
        ENDIF.

        ADD LV_LAST_STK TO LV_C_SUM.

        UNASSIGN <LV_FIELD>.
      ENDLOOP.

*--
    CATCH CX_SY_ARITHMETIC_ERROR INTO DATA(LR_ORF_1).
  ENDTRY.

*--
  LS_DATE_B-MGSUM = LV_B_SUM. LS_DATE_C-MGSUM = LV_C_SUM.
  APPEND : LS_DATE_A TO GT_DATE, LS_DATE_B TO GT_DATE, LS_DATE_C TO GT_DATE.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_RECACUL_STK
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> GT_DATE
*&      --> P_
*&      --> <LV_FIELD>
*&      <-- LV_CACUL_STK
*&---------------------------------------------------------------------*
FORM SET_RECACUL_STK USING IT_TABLE TYPE ANY TABLE
                          IV_OPT
                          IV_FIELD TYPE ANY
                 CHANGING EV_STK.

  FIELD-SYMBOLS: <LV_VALUE> TYPE ZSMM_DATE_VALUE-DAT001.
  DATA: LT_TABLE   TYPE TABLE OF TY_DATE
                   WITH NON-UNIQUE SORTED KEY IDX_KEY COMPONENTS OPT.

  LT_TABLE[] = IT_TABLE[].

*-
  READ TABLE LT_TABLE INTO DATA(LS_TABLE) WITH TABLE KEY IDX_KEY
                                         COMPONENTS OPT = IV_OPT.

  IF SY-SUBRC = 0.
    ASSIGN COMPONENT IV_FIELD OF STRUCTURE LS_TABLE TO <LV_VALUE>.
    IF <LV_VALUE> IS ASSIGNED.
      IF IV_OPT EQ 'B'.
        EV_STK = EV_STK - <LV_VALUE>.
      ELSE.
        EV_STK = EV_STK + <LV_VALUE>.
      ENDIF.
      UNASSIGN <LV_VALUE>.
    ENDIF.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form FILTERING_DISP_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LS_MAIN_WERKS
*&      <-- LV_DEL
*&---------------------------------------------------------------------*
FORM FILTERING_DISP_DATA USING IV_WERKS CHANGING EV_DEL.

  DATA: LV_CNT   TYPE NUMC3,
        LV_FIELD TYPE CHAR6.
  FIELD-SYMBOLS: <LV_VALUE> TYPE ZTMM20010-MNG01.

  READ TABLE GT_CONF_DAY INTO DATA(LS_CONF_DAY) WITH TABLE KEY IDX_KEY
                                                COMPONENTS WERKS = IV_WERKS.
  IF SY-SUBRC = 0.
    DATA(LV_CONF_DAY) = LS_CONF_DAY-DATE.
  ENDIF.

*-
  "기말재고 - 만 조회
  IF P_CHK1 = 'X'.
    READ TABLE GT_DATE INTO DATA(LS_DATE) WITH TABLE KEY IDX_KEY COMPONENTS OPT = 'D1'.
    IF SY-SUBRC = 0.
      CLEAR : LV_CNT.
      EV_DEL = 'X'.
      DO 20 TIMES.

        ADD 1 TO LV_CNT.

        CONCATENATE 'DAT' LV_CNT INTO LV_FIELD.

        READ TABLE GT_DAY INTO DATA(LS_DAY) WITH TABLE KEY IDX_DAY COMPONENTS DAY = LV_FIELD.
        IF LS_DAY-DATE >= LV_CONF_DAY.
          EXIT.
        ENDIF.

        READ TABLE GT_HOLIDAY WITH TABLE KEY IDX_KEY COMPONENTS WERKS = IV_WERKS
                                                                DAY   = LV_FIELD
                                                                TRANSPORTING NO FIELDS.
        IF SY-SUBRC NE 0.
          ASSIGN COMPONENT LV_FIELD OF STRUCTURE LS_DATE TO <LV_VALUE>.
          IF <LV_VALUE> IS ASSIGNED.
            IF <LV_VALUE> < 0.
              CLEAR EV_DEL. EXIT.
            ENDIF.
          ENDIF.
          UNASSIGN <LV_VALUE>.

        ENDIF.

      ENDDO.

    ENDIF.
  ENDIF.

  "예상기말재고 - 만 조회
  IF P_CHK2 = 'X'.
    READ TABLE GT_DATE INTO LS_DATE WITH TABLE KEY IDX_KEY COMPONENTS OPT = 'I'.
    IF SY-SUBRC = 0.
      CLEAR : LV_CNT.
      EV_DEL = 'X'.
      DO 20 TIMES.
        ADD 1 TO LV_CNT.

        CONCATENATE 'DAT' LV_CNT INTO LV_FIELD.

        READ TABLE GT_DAY INTO LS_DAY WITH TABLE KEY IDX_DAY COMPONENTS DAY = LV_FIELD.
        IF LS_DAY-DATE >= LV_CONF_DAY.
          EXIT.
        ENDIF.

        READ TABLE GT_HOLIDAY WITH TABLE KEY IDX_KEY COMPONENTS WERKS = IV_WERKS
                                                                DAY   = LV_FIELD
                                                                TRANSPORTING NO FIELDS.
        IF SY-SUBRC NE 0.
          ASSIGN COMPONENT LV_FIELD OF STRUCTURE LS_DATE TO <LV_VALUE>.
          IF <LV_VALUE> IS ASSIGNED.
            IF <LV_VALUE> < 0.
              CLEAR EV_DEL. EXIT.
            ENDIF.
          ENDIF.
          UNASSIGN <LV_VALUE>.
        ENDIF.

      ENDDO.
    ENDIF.
  ENDIF.

  "소요량 있는 자재만 조회
  IF P_CHK3 = 'X'.
    READ TABLE GT_DATE INTO LS_DATE WITH TABLE KEY IDX_KEY COMPONENTS OPT = 'B'.
    IF SY-SUBRC = 0.
      IF LS_DATE-MGSUM <= 0.
        EV_DEL = 'X'.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form FILTERING_DISP_DATA2
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LS_MAIN_WERKS
*&      <-- LV_DEL
*&---------------------------------------------------------------------*
FORM FILTERING_DISP_DATA2 USING IV_WERKS CHANGING EV_DEL.

*-
  "소요량없으나, 나머지 항목이 존재하는 자재만 조회
  IF P_CHK4 = 'X'. "U4
    READ TABLE GT_DATE INTO DATA(LS_DATE) WITH TABLE KEY IDX_KEY COMPONENTS OPT = 'B'.
    IF SY-SUBRC = 0.
      IF LS_DATE-MGSUM <= 0.
        READ TABLE GT_DATE INTO LS_DATE WITH TABLE KEY IDX_KEY_EXIST COMPONENTS QTY_EXIST = 'X'.
        IF SY-SUBRC NE 0.
          EV_DEL = 'X'.
        ENDIF.
      ELSE.
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
FORM APPEND_ALL_DATA USING IS_MAIN TYPE TY_MAIN
                           IV_MEINS.

  DATA: LS_DISP TYPE TS_DISP,
        LV_OPT  TYPE CHAR10.

*-
  CASE 'X'.
    WHEN P_RD1A. LV_OPT = 'D1'. "기말재고
    WHEN P_RD1B. LV_OPT = 'I'. "예상기말재고
  ENDCASE.

*-
  LOOP AT GT_DATE INTO DATA(LS_DATE) FROM 1.

    IF P_RD1C IS INITIAL.
      CHECK LS_DATE-OPT = LV_OPT.
    ENDIF.

    CONCATENATE IS_MAIN-MATNR IS_MAIN-WERKS IS_MAIN-MATKL LS_DATE-OPT INTO DATA(LV_KEY) RESPECTING BLANKS.

    CLEAR LS_DISP.

    MOVE-CORRESPONDING : IS_MAIN TO LS_DISP.

    MOVE-CORRESPONDING : LS_DATE TO LS_DISP.
    LS_DISP-KEY = LV_KEY.

    LS_DISP-MEINS = IV_MEINS.

    IF ( P_RD1C = 'X' AND LS_DATE-OPT = 'A' ) OR ( P_RD1A = 'X' OR P_RD1B = 'X' ).

*-- 공급업체,명

      READ TABLE GT_INFOP INTO DATA(LS_INFOP) WITH TABLE KEY IDX_KEY COMPONENTS MATNR = LS_DISP-MATNR.
      IF SY-SUBRC = 0.
        LS_DISP-LIFNR      = LS_INFOP-LIFNR.
        LS_DISP-LFA1_NAME1 = LS_INFOP-NAME1.
      ENDIF.

*-- 기본텍스트
      PERFORM MATERIAL_TEXT USING LS_DISP-MATNR CHANGING LS_DISP-MAT_TEXT.
    ENDIF.
*--
    APPEND LS_DISP TO GT_DISP.

  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form MATERIAL_TEXT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LS_DISP_MATNR
*&      <-- LS_DISP_MAT_TEXT
*&---------------------------------------------------------------------*
FORM MATERIAL_TEXT USING IV_MATNR TYPE MATNR_D
                    CHANGING CV_MAT_TEXT TYPE TDLINE.

  DATA : LV_ID     LIKE THEAD-TDID,
         LV_OBJECT LIKE THEAD-TDOBJECT,
         LV_NAME   LIKE THEAD-TDNAME.

  DATA : LT_XLINES LIKE STANDARD TABLE OF TLINE.

  _G_INIT : LT_XLINES.

  LV_NAME = IV_MATNR.
  WRITE : 'GRUN'      TO LV_ID,
          'MATERIAL'  TO LV_OBJECT.

  CALL FUNCTION 'READ_TEXT'
    EXPORTING
      CLIENT          = SY-MANDT
      ID              = LV_ID
      LANGUAGE        = '3'
      NAME            = LV_NAME
      OBJECT          = LV_OBJECT
    TABLES
      LINES           = LT_XLINES
    EXCEPTIONS
      ID              = 1
      LANGUAGE        = 2
      NAME            = 3
      NOT_FOUND       = 4
      OBJECT          = 5
      REFERENCE_CHECK = 6.

  IF SY-SUBRC EQ 0.
    LOOP AT LT_XLINES INTO DATA(LS_XLINES).

      CONCATENATE CV_MAT_TEXT LS_XLINES-TDLINE INTO CV_MAT_TEXT.

      CLEAR: LS_XLINES.
    ENDLOOP.
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
FORM SELECTION_SCREEN_OUTPUT.

*-
  LOOP AT SCREEN.
    CHECK SCREEN-GROUP1 = 'KEY'.
    SCREEN-INTENSIFIED = '1'.
    MODIFY SCREEN.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_BDC_USING_TAB
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LT_BDC
*&---------------------------------------------------------------------*
FORM SET_BDC_USING_TAB TABLES PT_BDC STRUCTURE BDCDATA.

  DATA : LT_BDC TYPE STANDARD TABLE OF BDCDATA,
         LS_BDC LIKE LINE OF LT_BDC.

  _G_INIT: PT_BDC, LT_BDC.

*-
  CLEAR LS_BDC.
  WRITE : 'ZRMM2160' TO LS_BDC-PROGRAM,
          '1000'     TO LS_BDC-DYNPRO,
          'X'        TO LS_BDC-DYNBEGIN.
  APPEND LS_BDC TO LT_BDC.

  READ TABLE GT_BDC_WRK INTO DATA(LS_BDC_WRK) INDEX 1.

  CLEAR LS_BDC.
  WRITE : 'S_WERKS-LOW' TO LS_BDC-FNAM.
  LS_BDC-FVAL      = LS_BDC_WRK-WERKS.
  APPEND LS_BDC TO LT_BDC.

  CLEAR LS_BDC.
  WRITE : 'BDC_OKCODE' TO LS_BDC-FNAM,
          '/00'        TO LS_BDC-FVAL.
  APPEND LS_BDC TO LT_BDC.

  LOOP AT GT_BDC_WRK INTO LS_BDC_WRK FROM 2.

    CLEAR LS_BDC.
    WRITE : 'BDC_OKCODE' TO LS_BDC-FNAM,
            '=%011'        TO LS_BDC-FVAL.
    APPEND LS_BDC TO LT_BDC.

*--
    CLEAR LS_BDC.
    WRITE : 'SAPLALDB' TO LS_BDC-PROGRAM,
            '3000'     TO LS_BDC-DYNPRO,
            'X'        TO LS_BDC-DYNBEGIN.
    APPEND LS_BDC TO LT_BDC.

    WRITE : 'BDC_OKCODE' TO LS_BDC-FNAM,
            '=LINS'      TO LS_BDC-FVAL.
    APPEND LS_BDC TO LT_BDC.

    CLEAR LS_BDC.
    WRITE : 'SAPLALDB' TO LS_BDC-PROGRAM,
            '3000'     TO LS_BDC-DYNPRO,
            'X'        TO LS_BDC-DYNBEGIN.
    APPEND LS_BDC TO LT_BDC.

    CLEAR LS_BDC.
    WRITE : 'RSCSEL_255-SLOW_I(01)' TO LS_BDC-FNAM.
    LS_BDC-FVAL = LS_BDC_WRK-WERKS.
    APPEND LS_BDC TO LT_BDC.

    WRITE : 'BDC_OKCODE' TO LS_BDC-FNAM,
            '=ACPT'      TO LS_BDC-FVAL.
    APPEND LS_BDC TO LT_BDC.

    CLEAR LS_BDC.
    WRITE : 'ZRMM2160' TO LS_BDC-PROGRAM,
            '1000'     TO LS_BDC-DYNPRO,
            'X'        TO LS_BDC-DYNBEGIN.
    APPEND LS_BDC TO LT_BDC.

  ENDLOOP.

*-
LOOP AT GT_BDC_MAT INTO DATA(LS_BDC_MAT).

    CLEAR LS_BDC.
    WRITE : 'BDC_OKCODE' TO LS_BDC-FNAM,
            '=%038'        TO LS_BDC-FVAL.
    APPEND LS_BDC TO LT_BDC.

*--
    CLEAR LS_BDC.
    WRITE : 'SAPLALDB' TO LS_BDC-PROGRAM,
            '3000'     TO LS_BDC-DYNPRO,
            'X'        TO LS_BDC-DYNBEGIN.
    APPEND LS_BDC TO LT_BDC.

    WRITE : 'BDC_OKCODE' TO LS_BDC-FNAM,
            '=LINS'      TO LS_BDC-FVAL.
    APPEND LS_BDC TO LT_BDC.

    CLEAR LS_BDC.
    WRITE : 'SAPLALDB' TO LS_BDC-PROGRAM,
            '3000'     TO LS_BDC-DYNPRO,
            'X'        TO LS_BDC-DYNBEGIN.
    APPEND LS_BDC TO LT_BDC.

    CLEAR LS_BDC.
    WRITE : 'RSCSEL_255-SLOW_I(01)' TO LS_BDC-FNAM.
    LS_BDC-FVAL = LS_BDC_MAT-MATNR.
    APPEND LS_BDC TO LT_BDC.

    WRITE : 'BDC_OKCODE' TO LS_BDC-FNAM,
            '=ACPT'      TO LS_BDC-FVAL.
    APPEND LS_BDC TO LT_BDC.

    CLEAR LS_BDC.
    WRITE : 'ZRMM2160' TO LS_BDC-PROGRAM,
            '1000'     TO LS_BDC-DYNPRO,
            'X'        TO LS_BDC-DYNBEGIN.
    APPEND LS_BDC TO LT_BDC.

  ENDLOOP.

*-
  CLEAR LS_BDC.
  WRITE : 'ZRMM2160' TO LS_BDC-PROGRAM,
          '1000'     TO LS_BDC-DYNPRO,
          'X'        TO LS_BDC-DYNBEGIN.
  APPEND LS_BDC TO LT_BDC.

  CLEAR LS_BDC.
  WRITE : 'P_RD1B' TO LS_BDC-FNAM.
  LS_BDC-FVAL      = 'X'.
  APPEND LS_BDC TO LT_BDC.

  WRITE : 'BDC_OKCODE' TO LS_BDC-FNAM,
          '=ONLI'      TO LS_BDC-FVAL.
  APPEND LS_BDC TO LT_BDC.

*-
  PT_BDC[] = LT_BDC[].

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CALL_POPUP_BOM
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> IV_ROW
*&---------------------------------------------------------------------*
FORM CALL_POPUP_BOM USING IV_ROW.

  DATA: LT_MATCAT TYPE TABLE OF ZSMAT.

*-
  _G_INIT: LT_MATCAT, GT_BOM. CLEAR GV_MATNR_0200.

*-
  READ TABLE GT_DISP INTO DATA(LS_DISP) INDEX IV_ROW.
  CHECK SY-SUBRC  = 0.
*-
  SELECT FROM ZTMM20010 AS A INNER JOIN MAKT AS B ON A~BAUGR = B~MATNR AND B~SPRAS = @SY-LANGU
         FIELDS DISTINCT A~BAUGR AS MATNR, B~MAKTX
         WHERE A~MATNR = @LS_DISP-MATNR AND A~WERKS = @LS_DISP-WERKS
           AND A~DAT00 >= @SY-DATLO AND ( A~BAUGR IS NOT NULL AND A~BAUGR NE @LS_DISP-MATNR )
  INTO CORRESPONDING FIELDS OF TABLE @LT_MATCAT.

  GT_BOM[] = VALUE #( BASE GT_BOM[] FOR LS_VALUE IN LT_MATCAT
                      ( MATNR = LS_VALUE-MATNR
                        MAKTX = LS_VALUE-MAKTX ) ).

*-
  IF GT_BOM[] IS NOT INITIAL.
    GV_MATNR_0200 = |{ LS_DISP-MATNR ALPHA = OUT WIDTH = 18 }| && '/' && LS_DISP-MAKTX.
    CALL SCREEN '0200' STARTING AT 2 2
                       ENDING   AT 65 20.
  ELSE.
    MESSAGE S005(ZMM01).
  ENDIF.

ENDFORM.
