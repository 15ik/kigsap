*&---------------------------------------------------------------------*
*& Include          ZRMM3030F01
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

* 예외처리 유저 점검
  ZCL_MM_COMMON=>COMMON_CONFIG(
     EXPORTING IS_COMMON = VALUE #( M = 'A1' D = 'A1010' S = 'AB100' )
                                    IT_WHERE = VALUE #( ( FIELD = 1 VALUE = SY-REPID )
                                                        ( FIELD = 2 VALUE = 'EX01' )
                                                        ( FIELD = 3 VALUE = SY-UNAME ) )
     IMPORTING ET_OUTTAB = DATA(LT_CONFIG) ).

  READ TABLE LT_CONFIG INTO DATA(LS_CONFIG) INDEX 1.

  IF SY-SUBRC EQ 0.
    GV_EXC_USER = 'X'.
  ENDIF.

* 사용자 기본값
*  IF GV_EXC_USER IS INITIAL.
  PERFORM SET_INIT_VALUES.
*  ENDIF.

* 입고일(전기일) 기본값 (1개월 전)
  DATA: LV_SDATE TYPE SY-DATUM.

  CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
    EXPORTING
      DATE      = SY-DATLO
      SIGNUM    = '-'
      DAYS      = 0
      MONTHS    = 1
      YEARS     = 0
    IMPORTING
      CALC_DATE = LV_SDATE.

  S_BUDAT[] = VALUE #( ( SIGN = 'I' OPTION = 'BT' LOW = LV_SDATE HIGH = SY-DATUM ) ).

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_INIT_VALUES
*&---------------------------------------------------------------------*
FORM SET_INIT_VALUES.

* 회사코드 기본값
*  SELECT SINGLE COMPANY, COMPANY_NAME
*    FROM ZSVMM_USER_INFO
*   WHERE USER_ID EQ @SY-UNAME
*    INTO @DATA(LS_USER_INFO).
*
*  IF SY-SUBRC EQ 0.
*    P_BUKRS = LS_USER_INFO-COMPANY.
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
FORM GET_DATA.

  CONSTANTS: LC_BSART_PKC1 TYPE EKKO-BSART VALUE 'PKC1',
             LC_BSART_PSIC TYPE EKKO-BSART VALUE 'PSIC'.

*--------------------------------
* 검색조건 설정
*--------------------------------
* 세금계산서 발행방식
  IF P_RS3 = 'X'.   "자율납품&위탁정산은 무조건 역발행이기 때문에 조건 필요 없음
    CLEAR GR_ABSGR.
  ELSE.
    CASE 'X'.
      WHEN P_RB1. "정발행
        GR_ABSGR = VALUE #( ( SIGN = 'I' OPTION = 'EQ' LOW = GC_ABSGR_01 )
                            ( SIGN = 'I' OPTION = 'EQ' LOW = SPACE ) ).
      WHEN P_RB2. "역발행
        GR_ABSGR = VALUE #( ( SIGN = 'I' OPTION = 'EQ' LOW = GC_ABSGR_02 ) ).

      WHEN OTHERS.
        CLEAR GR_ABSGR.
    ENDCASE.
  ENDIF.

* 선택조건에 따른 송장처리 문서유형
  CASE 'X'.
    WHEN P_RS1. "물품일반
      GR_BSTYP = VALUE #( ( SIGN = 'I' OPTION = 'EQ' LOW = 'F' ) ).
      GR_BSART = VALUE #( ( SIGN = 'E' OPTION = 'EQ' LOW = LC_BSART_PKC1 )
                          ( SIGN = 'E' OPTION = 'EQ' LOW = LC_BSART_PSIC ) ).
      GR_DPPCT = VALUE #( ( SIGN = 'I' OPTION = 'EQ' LOW = 0 ) ).

    WHEN P_RS2. "물품선급
      GR_BSTYP = VALUE #( ( SIGN = 'I' OPTION = 'EQ' LOW = 'F' ) ).
      GR_BSART = VALUE #( ( SIGN = 'E' OPTION = 'EQ' LOW = LC_BSART_PKC1 )
                          ( SIGN = 'E' OPTION = 'EQ' LOW = LC_BSART_PSIC ) ).
      GR_DPPCT = VALUE #( ( SIGN = 'I' OPTION = 'NE' LOW = 0 ) ).

    WHEN P_RS3. "자율납품&위탁정산
      GR_BSTYP = VALUE #( ( SIGN = 'I' OPTION = 'EQ' LOW = 'L' ) ).

    WHEN P_RS4. "공사용역
      GR_BSTYP = VALUE #( ( SIGN = 'I' OPTION = 'EQ' LOW = 'F' ) ).
      GR_BSART = VALUE #( ( SIGN = 'I' OPTION = 'EQ' LOW = LC_BSART_PKC1 )
                          ( SIGN = 'I' OPTION = 'EQ' LOW = LC_BSART_PSIC ) ).
    WHEN OTHERS.
      CLEAR: GR_BSTYP, GR_BSART, GR_DPPCT.
  ENDCASE.

  CLEAR GV_LOCAL_LAND1.
  SELECT SINGLE LAND1
    FROM T001
   WHERE BUKRS = @P_BUKRS
    INTO @GV_LOCAL_LAND1.
  IF GV_LOCAL_LAND1 IS INITIAL.
    GV_LOCAL_LAND1 = 'KR'.
  ENDIF.

*--------------------------------
* Data 추출
*--------------------------------
* 입고 문서 추출
  PERFORM GET_MATERIAL_DOCUMENT.

* 입고/입고 취소 분리
  PERFORM DIVIDE_MATERIAL_DOCUMENT.

* 구매오더 이력 추출(EKBE)
  PERFORM GET_PO_HISTORY_DATA.

* PO, 납품 정보
  PERFORM GET_ORDER_INFO.

* 입고/입고 취소에 PO, 납품 정보 업데이트
  PERFORM MODIFY_MATERIAL_DOCUMENT.

* 송장 문서 추출
  PERFORM GET_INVOICE_DOCUMENT.

* 입고/입고 취소, 송장문서 Sequence 처리
  PERFORM PROCESSING_DOCUMENT_SEQUENCE.

* 결과 Table
  PERFORM MAKE_RESULT_DATA.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_MATERIAL_DOCUMENT
*&---------------------------------------------------------------------*
FORM GET_MATERIAL_DOCUMENT.

  CONSTANTS: LC_BSART_PSIC TYPE EKKO-BSART VALUE 'PSIC'.

  CLEAR: GT_DATA.

  SELECT K~BUDAT, K~MBLNR, K~MJAHR, S~ZEILE, S~MATNR, S~CHARG, S~BWTAR,
         S~MENGE, S~MEINS, S~ERFMG, S~ERFME, S~BPMNG, S~BPRME,
         S~EBELN, S~EBELP, S~VBELN_IM, S~VBELP_IM, K~BKTXT,
         S~LFBNR, S~LFBJA, S~LFPOS, S~LIFNR,
         S~BWART, S~SHKZG, S~KZBEW, K~VGART, S~KZZUG, S~SOBKZ,
         E~BSART, S~WERKS, S~TCODE2_MKPF, W~NAME1 AS WERKSNM
    INTO CORRESPONDING FIELDS OF TABLE @GT_DATA
    FROM MKPF AS K INNER JOIN MSEG AS S
                           ON K~MBLNR = S~MBLNR
                          AND K~MJAHR = S~MJAHR
                   INNER JOIN EKKO AS E
                           ON S~EBELN = E~EBELN
                   INNER JOIN EKPO AS P
                           ON S~EBELN = P~EBELN
                          AND S~EBELP = P~EBELP
                    LEFT JOIN T001W AS W
                           ON S~WERKS = W~WERKS
                    LEFT JOIN MLAN AS J
                           ON J~MATNR = P~MATNR
                          AND J~ALAND = @GV_LOCAL_LAND1
   WHERE E~BUKRS  = @P_BUKRS
     AND E~LIFNR IN @S_LIFNR
     AND E~EKGRP IN @S_EKGRP
     AND E~BSART IN @S_BSART
     AND E~BSART IN @GR_BSART
     AND E~BSTYP IN @GR_BSTYP
     AND E~DPPCT IN @GR_DPPCT                               "->선금 비율
     AND E~ZORDER_PERSON     IN @S_ORDER
     AND E~ZORDER_DEPARTMENT IN @S_DEPTO
     AND E~ABSGR IN @GR_ABSGR                               "->세금계산서 발행방식
     AND ( ( E~BSART <> @LC_BSART_PSIC AND P~WEBRE = 'X' )  "->지시자: 입고 기준 송장 검증
          OR E~BSART = @LC_BSART_PSIC )
*     AND P~WEBRE = 'X'
     AND P~MATKL IN @S_MATKL
     AND K~BUDAT IN @S_BUDAT
     AND K~VGART IN ( 'WE', 'WQ' )                          "->구매 오더에 대한 입고, 용도 결정에 대한 자재 이동
     AND S~EBELN IN @S_EBELN
     AND S~MATNR IN @S_MATNR
     AND S~WERKS IN @S_WERKS
     AND S~BWART IN ( '101', '102', '122', '161', '162', 'ZZY', 'ZZZ'  )
     AND S~KZBEW = 'B'                                      "->구매오더에 대한 자재이동
     AND S~KZZUG = ''
     AND J~TAXIM IN @S_TAXIM.

  "102, 122로 참조없이 생성한 문서 삭제
  LOOP AT GT_DATA INTO DATA(LS_DATA).
    IF LS_DATA-BWART = '102' OR LS_DATA-BWART = '122' OR LS_DATA-BWART = 'ZZZ'.
      IF LS_DATA-LFBNR IS INITIAL.
        DELETE GT_DATA INDEX SY-TABIX.
        CONTINUE.
      ENDIF.
    ENDIF.
  ENDLOOP.

  CHECK GT_DATA IS NOT INITIAL.

  DATA(LT_DATA) = GT_DATA.    "-->FAE 사용
  SORT LT_DATA BY MBLNR MJAHR ZEILE.
  DELETE ADJACENT DUPLICATES FROM LT_DATA COMPARING MBLNR MJAHR ZEILE.

  "취소 전표 삭제
  IF LT_DATA IS NOT INITIAL.
    CLEAR: GT_SMBLN.

    SELECT SMBLN, SJAHR, SMBLP
      INTO CORRESPONDING FIELDS OF TABLE @GT_SMBLN
      FROM MSEG
       FOR ALL ENTRIES IN @LT_DATA
     WHERE SMBLN = @LT_DATA-MBLNR
       AND SJAHR = @LT_DATA-MJAHR
       AND SMBLP = @LT_DATA-ZEILE.

    SORT GT_SMBLN BY SMBLN SJAHR SMBLP.
  ENDIF.

  IF GT_SMBLN IS NOT INITIAL.

    CLEAR LS_DATA.
    LOOP AT GT_DATA INTO LS_DATA.
      GV_TABIX = SY-TABIX.

      READ TABLE GT_SMBLN WITH KEY SMBLN = LS_DATA-MBLNR
                                   SJAHR = LS_DATA-MJAHR
                                   SMBLP = LS_DATA-ZEILE
                                   BINARY SEARCH
                                   TRANSPORTING NO FIELDS.
      IF SY-SUBRC = 0.
        DELETE GT_DATA INDEX GV_TABIX.
        CONTINUE.
      ENDIF.
    ENDLOOP.

  ENDIF.

  "입고 취소/취소 전표인 경우 최초 입고(101/B)전표로 대체
  IF LT_DATA IS NOT INITIAL.
    CLEAR: GT_MBLNR.

    SELECT MBLNR, MJAHR, ZEILE
      FROM MSEG
       FOR ALL ENTRIES IN @LT_DATA
     WHERE MBLNR = @LT_DATA-MBLNR
       AND MJAHR = @LT_DATA-MJAHR
       AND ZEILE = @LT_DATA-ZEILE
       AND SMBLN IS NOT INITIAL
      INTO CORRESPONDING FIELDS OF TABLE @GT_MBLNR.

    SORT GT_MBLNR BY MBLNR MJAHR ZEILE.
  ENDIF.

  CLEAR LS_DATA.
  LOOP AT GT_DATA INTO LS_DATA.

    IF ( LS_DATA-BWART = '101' OR LS_DATA-BWART = '161' OR LS_DATA-BWART = 'ZZY' ) AND
       LS_DATA-LFBNR IS NOT INITIAL AND LS_DATA-MBLNR <> LS_DATA-LFBNR.

      READ TABLE GT_MBLNR WITH KEY MBLNR = LS_DATA-MBLNR
                                   MJAHR = LS_DATA-MJAHR
                                   ZEILE = LS_DATA-ZEILE
                                   BINARY SEARCH
                                   TRANSPORTING NO FIELDS.

      IF SY-SUBRC = 0.
        LS_DATA-MBLNR =  LS_DATA-LFBNR.
        LS_DATA-MJAHR =  LS_DATA-LFBJA.
        LS_DATA-ZEILE =  LS_DATA-LFPOS.

        MODIFY GT_DATA FROM LS_DATA TRANSPORTING MBLNR ZEILE MJAHR.
      ENDIF.

    ENDIF.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form DIVIDE_MATERIAL_DOCUMENT
*&---------------------------------------------------------------------*
FORM DIVIDE_MATERIAL_DOCUMENT.

  DATA: LS_101   LIKE LINE OF GT_101,
        LS_101_B LIKE LINE OF GT_101_B,
        LS_102   LIKE LINE OF GT_102,
        LS_102_B LIKE LINE OF GT_102_B.

  CLEAR: GT_101, GT_101_B, GT_102, GT_102_B.

  GT_101 = GT_DATA.
  DELETE GT_101 WHERE BWART = '102' OR BWART = '122' OR BWART = '162' OR BWART = 'ZZZ'.
  SORT GT_101 BY MBLNR MJAHR ZEILE.

  LOOP AT GT_DATA INTO DATA(LS_DATA).

    IF LS_DATA-BWART = '102' OR LS_DATA-BWART = '122' OR LS_DATA-BWART = '162' OR LS_DATA-BWART = 'ZZZ'.

      READ TABLE GT_101 WITH KEY MBLNR = LS_DATA-LFBNR
                                 MJAHR = LS_DATA-LFBJA
                                 ZEILE = LS_DATA-LFPOS
                                 BINARY SEARCH
                                 TRANSPORTING NO FIELDS.
      IF SY-SUBRC <> 0.
        CLEAR LS_102_B.
        MOVE-CORRESPONDING LS_DATA TO LS_102_B.
        APPEND LS_102_B TO GT_102_B.
      ENDIF.

      CLEAR: LS_102.
      MOVE-CORRESPONDING LS_DATA TO LS_102.
      APPEND LS_102 TO GT_102.

    ENDIF.

  ENDLOOP.

  DATA(LT_102_B) = GT_102_B.    "-->FAE 사용
  SORT LT_102_B BY LFBNR LFBJA LFPOS.
  DELETE ADJACENT DUPLICATES FROM LT_102_B COMPARING LFBNR LFBJA LFPOS.

  IF LT_102_B IS NOT INITIAL.

    SELECT K~BUDAT, K~MBLNR, K~MJAHR, S~ZEILE, S~MATNR, S~CHARG, S~BWTAR,
           S~MENGE, S~MEINS, S~ERFMG, S~ERFME, S~BPMNG, S~BPRME,
           S~EBELN, S~EBELP, S~VBELN_IM, S~VBELP_IM, K~BKTXT,
           S~LFBNR, S~LFBJA, S~LFPOS, S~LIFNR, S~INSMK,
           S~BWART, S~SHKZG, S~KZBEW, K~VGART, S~KZZUG, S~SOBKZ,
           E~BSART, S~WERKS, S~TCODE2_MKPF, W~NAME1 AS WERKSNM
      INTO CORRESPONDING FIELDS OF TABLE @GT_101_B
      FROM MKPF AS K INNER JOIN MSEG AS S
                             ON K~MBLNR = S~MBLNR
                            AND K~MJAHR = S~MJAHR
                     INNER JOIN EKKO AS E
                             ON S~EBELN = E~EBELN
                      LEFT JOIN T001W AS W
                             ON S~WERKS = W~WERKS
       FOR ALL ENTRIES IN @LT_102_B
     WHERE S~MBLNR = @LT_102_B-LFBNR
       AND S~MJAHR = @LT_102_B-LFBJA
       AND S~ZEILE = @LT_102_B-LFPOS.

  ENDIF.

  DATA(LT_101_B) = GT_101_B.    "-->FAE 사용
  SORT LT_101_B BY MBLNR MJAHR ZEILE.
  DELETE ADJACENT DUPLICATES FROM LT_101_B COMPARING MBLNR MJAHR ZEILE.

  IF LT_101_B IS NOT INITIAL.
    CLEAR: GT_SMBLN.

    SELECT SMBLN, SJAHR, SMBLP
      INTO CORRESPONDING FIELDS OF TABLE @GT_SMBLN
      FROM MSEG
       FOR ALL ENTRIES IN @LT_101_B
     WHERE SMBLN = @LT_101_B-MBLNR
       AND SJAHR = @LT_101_B-MJAHR
       AND SMBLP = @LT_101_B-ZEILE.

    SORT GT_SMBLN BY SMBLN SJAHR SMBLP.

    IF GT_SMBLN IS NOT INITIAL.

      CLEAR LS_101_B.
      LOOP AT GT_101_B INTO LS_101_B.
        GV_TABIX = SY-TABIX.

        READ TABLE GT_SMBLN WITH KEY SMBLN = LS_101_B-MBLNR
                                     SJAHR = LS_101_B-MJAHR
                                     SMBLP = LS_101_B-ZEILE
                                     BINARY SEARCH
                                     TRANSPORTING NO FIELDS.
        IF SY-SUBRC  = 0.
          DELETE GT_101_B INDEX GV_TABIX.
          CONTINUE.
        ENDIF.
      ENDLOOP.

    ENDIF.
  ENDIF.

  CLEAR LS_101_B.
  LOOP AT GT_101_B INTO LS_101_B.
    CLEAR LS_101.
    MOVE-CORRESPONDING LS_101_B TO LS_101.
    APPEND LS_101 TO GT_101.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_PO_HISTORY_DATA
*&---------------------------------------------------------------------*
FORM GET_PO_HISTORY_DATA.

  CLEAR: GT_EKBE_101, GT_EKBE_102.

  IF GT_101 IS NOT INITIAL.
    SELECT EBELN, EBELP, GJAHR, BELNR, BUZEI, WRBTR, WAERS, MENGE
      INTO CORRESPONDING FIELDS OF TABLE @GT_EKBE_101
      FROM EKBE
       FOR ALL ENTRIES IN @GT_101
     WHERE EBELN = @GT_101-EBELN
       AND EBELP = @GT_101-EBELP
       AND VGABE = '1'            "-->입고
       AND GJAHR = @GT_101-MJAHR
       AND BELNR = @GT_101-MBLNR
       AND BUZEI = @GT_101-ZEILE.

    SORT GT_EKBE_101 BY EBELN EBELP GJAHR BELNR BUZEI.
  ENDIF.

  IF GT_102 IS NOT INITIAL.
    SELECT EBELN, EBELP, GJAHR, BELNR, BUZEI, WRBTR, WAERS, MENGE
      INTO CORRESPONDING FIELDS OF TABLE @GT_EKBE_102
      FROM EKBE
       FOR ALL ENTRIES IN @GT_102
     WHERE EBELN = @GT_102-EBELN
       AND EBELP = @GT_102-EBELP
       AND VGABE = '1'            "-->입고
       AND GJAHR = @GT_102-MJAHR
       AND BELNR = @GT_102-MBLNR
       AND BUZEI = @GT_102-ZEILE.

    SORT GT_EKBE_102 BY EBELN EBELP GJAHR BELNR BUZEI.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_ORDER_INFO
*&---------------------------------------------------------------------*
FORM GET_ORDER_INFO.

  DATA: LS_PO    LIKE LINE OF GT_PO,
        LS_DO    LIKE LINE OF GT_DO,
        LS_LIFNR LIKE LINE OF GT_LIFNR.

  CLEAR: GT_PO, GT_DO, GT_LIFNR, GT_EKET.

  LOOP AT GT_101 INTO DATA(LS_101).
    CLEAR LS_PO.
    LS_PO-EBELN = LS_101-EBELN.
    LS_PO-EBELP = LS_101-EBELP.
    COLLECT LS_PO INTO GT_PO.

    CLEAR LS_DO.
    LS_DO-VBELN_IM = LS_101-VBELN_IM.
    LS_DO-VBELP_IM = LS_101-VBELP_IM.
    COLLECT LS_DO INTO GT_DO.

    CLEAR LS_LIFNR.
    LS_LIFNR-LIFNR = LS_101-LIFNR.
    COLLECT LS_LIFNR INTO GT_LIFNR.
  ENDLOOP.

* PO 정보
  DATA(LT_PO) = GT_PO.    "-->FAE 사용
  SORT LT_PO BY EBELN EBELP.
  DELETE ADJACENT DUPLICATES FROM LT_PO COMPARING EBELN EBELP.

  IF LT_PO IS NOT INITIAL.
    CLEAR: GT_EKPO.

    SELECT P~EBELN, P~EBELP, P~TXZ01, P~BSTYP, P~MEINS, P~BPRME, P~NETPR,
           K~LIFNR, K~EKORG, P~INFNR, P~PSTYP, P~WERKS, P~PEINH,
           K~WAERS AS PO_WAERS, W~NAME1 AS WERKSNM
      INTO CORRESPONDING FIELDS OF TABLE @GT_EKPO
      FROM EKPO AS P INNER JOIN EKKO AS K
                             ON P~EBELN = K~EBELN
                      LEFT JOIN T001W AS W
                             ON P~WERKS = W~WERKS
       FOR ALL ENTRIES IN @LT_PO
     WHERE P~EBELN = @LT_PO-EBELN
       AND P~EBELP = @LT_PO-EBELP.

    SORT GT_EKPO BY EBELN EBELP.

    SELECT EBELN, EBELP, ETENR, EINDT
      FROM EKET
       FOR ALL ENTRIES IN @LT_PO
     WHERE EBELN = @LT_PO-EBELN
       AND EBELP = @LT_PO-EBELP
      INTO CORRESPONDING FIELDS OF TABLE @GT_EKET.
    SORT GT_EKET BY EBELN EBELP EINDT DESCENDING.
  ENDIF.

* 납품 정보
  DATA(LT_DO) = GT_DO.    "-->FAE 사용
  SORT LT_DO BY VBELN_IM VBELP_IM.
  DELETE ADJACENT DUPLICATES FROM LT_DO COMPARING VBELN_IM VBELP_IM.

  IF LT_DO IS NOT  INITIAL.
    CLEAR GT_LIPS.

    SELECT VBELN AS VBELN_IM, POSNR AS VBELP_IM, LFIMG
      INTO CORRESPONDING FIELDS OF TABLE @GT_LIPS
      FROM LIPS
       FOR ALL ENTRIES IN @LT_DO
     WHERE VBELN = @LT_DO-VBELN_IM
       AND POSNR = @LT_DO-VBELP_IM.

    SORT GT_LIPS BY VBELN_IM VBELP_IM.
  ENDIF.

* 업체명
  DATA(LT_LIFNR) = GT_LIFNR.    "-->FAE 사용
  SORT LT_LIFNR BY LIFNR.
  DELETE ADJACENT DUPLICATES FROM LT_LIFNR COMPARING LIFNR.

  IF LT_LIFNR IS NOT INITIAL.
    CLEAR: GT_LIFNR_NM.

    SELECT LIFNR, NAME1
      INTO CORRESPONDING FIELDS OF TABLE @GT_LIFNR_NM
      FROM LFA1
       FOR ALL ENTRIES IN @LT_LIFNR
     WHERE LIFNR = @LT_LIFNR-LIFNR.

    SORT GT_LIFNR_NM BY LIFNR.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form MODIFY_MATERIAL_DOCUMENT
*&---------------------------------------------------------------------*
FORM MODIFY_MATERIAL_DOCUMENT.

  LOOP AT GT_102 INTO DATA(LS_102).

    LS_102-MENGE = LS_102-BPMNG.
    LS_102-MEINS = LS_102-BPRME.
    MODIFY GT_102 FROM LS_102 TRANSPORTING MENGE MEINS.

* 입고이력 정보
    READ TABLE GT_EKBE_102 WITH KEY EBELN = LS_102-EBELN
                                    EBELP = LS_102-EBELP
                                    GJAHR = LS_102-MJAHR
                                    BELNR = LS_102-MBLNR
                                    BUZEI = LS_102-ZEILE
                                    BINARY SEARCH
                                    INTO DATA(LS_EKBE_102).
    IF SY-SUBRC = 0.
      LS_102-WRBTR = LS_EKBE_102-WRBTR. "->EKBE 기준으로 수정
      LS_102-WAERS = LS_EKBE_102-WAERS.
      LS_102-MENGE = LS_EKBE_102-MENGE. "->EKBE 기준으로 수정
      MODIFY GT_102 FROM LS_102 TRANSPORTING WRBTR WAERS MENGE.
    ENDIF.

* PO 정보
    READ TABLE GT_EKPO WITH KEY EBELN = LS_102-EBELN
                                EBELP = LS_102-EBELP
                                BINARY SEARCH
                                INTO DATA(LS_EKPO).
    IF SY-SUBRC = 0.
      LS_102-TXZ01 = LS_EKPO-TXZ01.
      LS_102-BSTYP = LS_EKPO-BSTYP.

      LS_102-MEINS_PO = LS_EKPO-MEINS.
      LS_102-BPRME_PO = LS_EKPO-BPRME.
      LS_102-NETPR    = LS_EKPO-NETPR.
      LS_102-INFNR    = LS_EKPO-INFNR.
      LS_102-EKORG    = LS_EKPO-EKORG.
      LS_102-PSTYP    = LS_EKPO-PSTYP.
      LS_102-WERKS    = LS_EKPO-WERKS.
      LS_102-WERKSNM  = LS_EKPO-WERKSNM.
      LS_102-PEINH    = LS_EKPO-PEINH.
      LS_102-PO_WAERS = LS_EKPO-PO_WAERS.

      MODIFY GT_102 FROM LS_102 TRANSPORTING TXZ01 BSTYP MEINS_PO BPRME_PO NETPR
                                             INFNR EKORG PSTYP WERKS PEINH PO_WAERS WERKSNM.
    ENDIF.
  ENDLOOP.


  LOOP AT GT_101 INTO DATA(LS_101).
    LS_101-MENGE = LS_101-BPMNG.
    LS_101-MEINS = LS_101-BPRME.
    MODIFY GT_101 FROM LS_101 TRANSPORTING MENGE MEINS.

* 공급업체명
    READ TABLE GT_LIFNR_NM WITH KEY LIFNR = LS_101-LIFNR
                                    BINARY SEARCH
                                    INTO DATA(LS_LIFNR_NM).
    IF SY-SUBRC = 0.
      LS_101-LIFNRNM = LS_LIFNR_NM-NAME1.
      MODIFY GT_101 FROM LS_101 TRANSPORTING LIFNRNM.
    ENDIF.

* 입고이력 정보
    READ TABLE GT_EKBE_101 WITH KEY EBELN = LS_101-EBELN
                                    EBELP = LS_101-EBELP
                                    GJAHR = LS_101-MJAHR
                                    BELNR = LS_101-MBLNR
                                    BUZEI = LS_101-ZEILE
                                    BINARY SEARCH
                                    INTO DATA(LS_EKBE_101).
    IF SY-SUBRC  = 0.
      LS_101-WRBTR = LS_EKBE_101-WRBTR.  "->EKBE기준으로 수정
      LS_101-WAERS = LS_EKBE_101-WAERS.
      LS_101-MENGE = LS_EKBE_101-MENGE.  "->EKBE기준으로 수정
      MODIFY GT_101 FROM LS_101 TRANSPORTING WRBTR WAERS MENGE.
    ENDIF.

* PO 정보
    CLEAR LS_EKPO.
    READ TABLE GT_EKPO WITH KEY EBELN = LS_101-EBELN
                                EBELP = LS_101-EBELP
                                BINARY SEARCH
                                INTO LS_EKPO.
    IF SY-SUBRC = 0.
      LS_101-TXZ01 = LS_EKPO-TXZ01.
      LS_101-BSTYP = LS_EKPO-BSTYP.

      LS_101-MEINS_PO = LS_EKPO-MEINS.
      LS_101-BPRME_PO = LS_EKPO-BPRME.
      LS_101-NETPR    = LS_EKPO-NETPR.
      LS_101-INFNR    = LS_EKPO-INFNR.
      LS_101-EKORG    = LS_EKPO-EKORG.
      LS_101-PSTYP    = LS_EKPO-PSTYP.
      LS_101-WERKS    = LS_EKPO-WERKS.
      LS_101-WERKSNM  = LS_EKPO-WERKSNM.
      LS_101-PEINH    = LS_EKPO-PEINH.
      LS_101-PO_WAERS = LS_EKPO-PO_WAERS.

      MODIFY GT_101 FROM LS_101 TRANSPORTING TXZ01 BSTYP MEINS_PO BPRME_PO NETPR
                                             INFNR EKORG PSTYP WERKS PEINH PO_WAERS WERKSNM.
    ENDIF.

* 납품 정보
    READ TABLE GT_LIPS WITH KEY VBELN_IM = LS_101-VBELN_IM
                                VBELP_IM = LS_101-VBELP_IM
                                BINARY SEARCH
                                INTO DATA(LS_LIPS).
    IF SY-SUBRC = 0.
      LS_101-LFIMG = LS_LIPS-LFIMG.
      MODIFY GT_101 FROM LS_101 TRANSPORTING LFIMG.
    ENDIF.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_INVOICE_DOCUMENT
*&---------------------------------------------------------------------*
FORM GET_INVOICE_DOCUMENT.

  CLEAR: GT_IV1.

  DATA(LT_PO) = GT_PO.    "-->FAE 사용
  SORT LT_PO BY EBELN EBELP.
  DELETE ADJACENT DUPLICATES FROM LT_PO COMPARING EBELN EBELP.

  IF LT_PO IS NOT INITIAL.

    SELECT RSEG~BELNR, RSEG~GJAHR, RSEG~BUZEI,
           RBKP~BUDAT, RBKP~BLDAT, RBKP~VGART, RBKP~LIFNR,
           RBKP~STBLG, RBKP~STJAH,
           RSEG~EBELN, RSEG~EBELP, RSEG~SHKZG,
           RSEG~WRBTR, RBKP~WAERS, RSEG~MENGE,
           RSEG~MEINS, RSEG~XBLNR, RSEG~PSTYP,
           RSEG~LFBNR, RSEG~LFGJA, RSEG~LFPOS, EKKO~EKORG, EKKO~BSART,
           EKPO~INFNR, EKKO~EKORG, EKPO~PSTYP, EKPO~WERKS, EKPO~MATNR
       FROM RBKP INNER JOIN RSEG
                         ON RBKP~BELNR = RSEG~BELNR
                        AND RBKP~GJAHR = RSEG~GJAHR
                 INNER JOIN EKPO
                         ON RSEG~EBELN = EKPO~EBELN
                        AND RSEG~EBELP = EKPO~EBELP
                 INNER JOIN EKKO
                         ON EKPO~EBELN = EKKO~EBELN
        FOR ALL ENTRIES IN @LT_PO
      WHERE EKPO~EBELN = @LT_PO-EBELN
        AND EKPO~EBELP = @LT_PO-EBELP
        AND RBKP~STBLG IS INITIAL
      INTO CORRESPONDING FIELDS OF TABLE @GT_IV1.

  ENDIF.

  SORT GT_101 BY MBLNR MJAHR ZEILE.

  LOOP AT GT_IV1 INTO DATA(LS_IV1).
    GV_TABIX = SY-TABIX.

    IF LS_IV1-PSTYP <> '9'.
      READ TABLE GT_101 WITH KEY MBLNR = LS_IV1-LFBNR
                                 MJAHR = LS_IV1-LFGJA
                                 ZEILE = LS_IV1-LFPOS
                                 BINARY SEARCH
                                 TRANSPORTING NO FIELDS.
      IF SY-SUBRC <> 0.
        DELETE GT_IV1 INDEX GV_TABIX.
        CONTINUE.
      ENDIF.
    ENDIF.

    LS_IV1-BUZEI2 = LS_IV1-BUZEI.
    MODIFY GT_IV1 FROM LS_IV1 TRANSPORTING BUZEI2.
  ENDLOOP.

* 송장 이력 정보
  IF GT_IV1 IS NOT INITIAL.
    CLEAR: GT_EBKE_IV.

    SELECT EBELN, EBELP, ZEKKN, VGABE, GJAHR, BELNR, BUZEI, MENGE, WRBTR
      INTO CORRESPONDING FIELDS OF TABLE @GT_EBKE_IV
      FROM EKBE
       FOR ALL ENTRIES IN @GT_IV1
     WHERE EBELN = @GT_IV1-EBELN
       AND EBELP = @GT_IV1-EBELP
       AND GJAHR = @GT_IV1-GJAHR
       AND BELNR = @GT_IV1-BELNR
       AND BUZEI = @GT_IV1-BUZEI2
       AND VGABE IN ( '2', '3' ).   "->송장 수령, 차후 차변/대변

    SORT GT_EBKE_IV BY EBELN EBELP GJAHR BELNR BUZEI.

    CLEAR LS_IV1.
    LOOP AT GT_IV1 INTO LS_IV1.
      READ TABLE GT_EBKE_IV WITH KEY EBELN = LS_IV1-EBELN
                                     EBELP = LS_IV1-EBELP
                                     GJAHR = LS_IV1-GJAHR
                                     BELNR = LS_IV1-BELNR
                                     BUZEI = LS_IV1-BUZEI2
                                     BINARY SEARCH
                                     INTO DATA(LS_EKBE_IV).
      IF SY-SUBRC = 0.
        LS_IV1-VGABE = LS_EKBE_IV-VGABE.

        IF LS_IV1-VGABE = '2'.
          LS_IV1-MENGE = LS_EKBE_IV-MENGE.   "->EKBE 기준으로 수정
          LS_IV1-WRBTR = LS_EKBE_IV-WRBTR.   "->EKBE 기준으로 수정
          LS_IV1-VGABENM = TEXT-M04.  "송장 수령

        ELSEIF LS_IV1-VGABE = '3'.
          LS_IV1-WRBTR = LS_EKBE_IV-WRBTR.   "->EKBE 기준으로 수정
          LS_IV1-MENGE = 0.                  "차후차변이면 수량은 0
          LS_IV1-VGABENM = TEXT-M05.  "차후 차변/대변
        ENDIF.

        MODIFY GT_IV1 FROM LS_IV1 TRANSPORTING VGABE VGABENM MENGE WRBTR.
      ENDIF.

    ENDLOOP.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form PROCESSING_DOCUMENT_SEQUENCE
*&---------------------------------------------------------------------*
FORM PROCESSING_DOCUMENT_SEQUENCE.

  DATA: LV_SEQ TYPE CATS_TABIX.

  SORT GT_101 BY MBLNR MJAHR ZEILE BUDAT.
  CLEAR: LV_SEQ.
  LOOP AT GT_101 INTO DATA(LS_101).
    LV_SEQ = LV_SEQ + 1.
    LS_101-SEQ = LV_SEQ.
    MODIFY GT_101 FROM LS_101 TRANSPORTING SEQ.
    AT END OF ZEILE.
      CLEAR: LV_SEQ.
    ENDAT.
  ENDLOOP.

  SORT GT_102 BY LFBNR LFBJA LFPOS BUDAT.
  CLEAR: LV_SEQ.
  LOOP AT GT_102 INTO DATA(LS_102).
    LV_SEQ = LV_SEQ + 1.
    LS_102-SEQ = LV_SEQ.
    MODIFY GT_102 FROM LS_102 TRANSPORTING SEQ.
    AT END OF LFPOS.
      CLEAR: LV_SEQ.
    ENDAT.
  ENDLOOP.

  SORT GT_IV1 BY LFBNR LFGJA LFPOS BUDAT.
  CLEAR: LV_SEQ.
  LOOP AT GT_IV1 INTO DATA(LS_IV1).
    LV_SEQ = LV_SEQ + 1.
    LS_IV1-SEQ = LV_SEQ.
    MODIFY GT_IV1 FROM LS_IV1 TRANSPORTING SEQ.
    AT END OF LFPOS.
      CLEAR: LV_SEQ.
    ENDAT.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form MAKE_RESULT_DATA
*&---------------------------------------------------------------------*
FORM MAKE_RESULT_DATA.

  DATA: LS_DDATA LIKE LINE OF GT_DDATA.

  CLEAR: GT_HDATA, GT_DDATA.

  LOOP AT GT_101 INTO DATA(LS_101).
    CLEAR LS_DDATA.
    MOVE-CORRESPONDING LS_101 TO LS_DDATA.

    "161 반품PO
    IF LS_101-SHKZG = 'H'.
      LS_DDATA-MENGE = LS_101-MENGE * -1.
      LS_DDATA-WRBTR = LS_101-WRBTR * -1.
    ENDIF.

    LS_DDATA-MBLNR1 = LS_DDATA-MBLNR.
    LS_DDATA-MJAHR1 = LS_DDATA-MJAHR.
    LS_DDATA-ZEILE1 = LS_DDATA-ZEILE.
    LS_DDATA-BUDAT1 = LS_DDATA-BUDAT.
    LS_DDATA-SHKZG1 = LS_101-SHKZG.
    LS_DDATA-BPRME  = LS_101-BPRME_PO.
    APPEND LS_DDATA TO GT_DDATA.
  ENDLOOP.

  GT_HDATA[] = GT_DDATA[].
  SORT GT_DDATA BY MBLNR MJAHR ZEILE SEQ.

* 수량/금액 SUM
  PERFORM SUM_QTY_AMT.


  CLEAR GT_MAIN.
  SORT GT_102 BY LFBNR LFBJA LFPOS SEQ.

  LOOP AT GT_102 INTO DATA(LS_102).
    CLEAR LS_DDATA.
    READ TABLE GT_DDATA WITH KEY MBLNR = LS_102-LFBNR
                                 MJAHR = LS_102-LFBJA
                                 ZEILE = LS_102-LFPOS
                                 SEQ   = LS_102-SEQ
                                 BINARY SEARCH
                                 INTO LS_DDATA.
    IF SY-SUBRC = 0.
      LS_DDATA-MBLNR2 = LS_102-MBLNR.
      LS_DDATA-MJAHR2 = LS_102-MJAHR.
      LS_DDATA-ZEILE2 = LS_102-ZEILE.
      LS_DDATA-BUDAT2 = LS_102-BUDAT.
      LS_DDATA-BKTXT = LS_102-BKTXT.
      LS_DDATA-MENGE2 = LS_102-MENGE.
      LS_DDATA-WRBTR2 = LS_102-WRBTR.
      LS_DDATA-SHKZG2 = LS_102-SHKZG.
      MODIFY GT_DDATA FROM LS_DDATA
                      TRANSPORTING MBLNR2 MJAHR2 ZEILE2 BUDAT2 MENGE2 WRBTR2 SHKZG2
                             WHERE MBLNR = LS_102-LFBNR
                               AND MJAHR = LS_102-LFBJA
                               AND ZEILE = LS_102-LFPOS
                               AND SEQ   = LS_102-SEQ.
    ELSE.
      CLEAR LS_DDATA.
      READ TABLE GT_DDATA WITH KEY MBLNR = LS_102-LFBNR
                                   MJAHR = LS_102-LFBJA
                                   ZEILE = LS_102-LFPOS
                                   BINARY SEARCH
                                   INTO LS_DDATA.
      IF SY-SUBRC = 0.
        CLEAR GS_MAIN.
        GS_MAIN-MBLNR        = LS_DDATA-MBLNR.
        GS_MAIN-MJAHR        = LS_DDATA-MJAHR.
        GS_MAIN-ZEILE        = LS_DDATA-ZEILE.
        GS_MAIN-BUDAT        = LS_DDATA-BUDAT.
        GS_MAIN-BSTYP        = LS_DDATA-BSTYP.
        GS_MAIN-SEQ          = LS_102-SEQ.
        GS_MAIN-MBLNR1       = LS_DDATA-MBLNR.
        GS_MAIN-MJAHR1       = LS_DDATA-MJAHR.
        GS_MAIN-ZEILE1       = LS_DDATA-ZEILE.
        GS_MAIN-BKTXT       = LS_DDATA-BKTXT.
        GS_MAIN-BUDAT1       = LS_DDATA-BUDAT.
        GS_MAIN-SHKZG1       = LS_DDATA-SHKZG1.
        GS_MAIN-MATNR        = LS_DDATA-MATNR.
        GS_MAIN-TXZ01        = LS_DDATA-TXZ01.
        GS_MAIN-MENGE        = LS_DDATA-MENGE.
        GS_MAIN-MEINS        = LS_DDATA-MEINS.
        GS_MAIN-WRBTR        = LS_DDATA-WRBTR.
        GS_MAIN-WAERS        = LS_DDATA-WAERS.
        GS_MAIN-EBELN        = LS_DDATA-EBELN.
        GS_MAIN-EBELP        = LS_DDATA-EBELP.
        GS_MAIN-VBELN_IM     = LS_DDATA-VBELN_IM.
        GS_MAIN-VBELP_IM     = LS_DDATA-VBELP_IM.
        GS_MAIN-LFIMG        = LS_DDATA-LFIMG.
        GS_MAIN-LIFNR        = LS_DDATA-LIFNR.
        GS_MAIN-LIFNRNM      = LS_DDATA-LIFNRNM.

        GS_MAIN-MBLNR2       = LS_102-MBLNR.
        GS_MAIN-MJAHR2       = LS_102-MJAHR.
        GS_MAIN-ZEILE2       = LS_102-ZEILE.
        GS_MAIN-BUDAT2       = LS_102-BUDAT.
        GS_MAIN-MENGE2       = LS_102-MENGE.
        GS_MAIN-WRBTR2       = LS_102-WRBTR.
        GS_MAIN-SHKZG2       = LS_102-SHKZG.
        APPEND GS_MAIN TO GT_MAIN.
      ENDIF.
    ENDIF.
  ENDLOOP.

  IF GT_MAIN IS NOT INITIAL.
    CLEAR GS_MAIN.

    LOOP AT GT_MAIN INTO GS_MAIN.
      APPEND GS_MAIN TO GT_DDATA.
    ENDLOOP.
  ENDIF.

  SORT GT_DDATA BY MBLNR MJAHR ZEILE SEQ.

* 송장 정보
  CLEAR GT_MAIN.

  LOOP AT GT_IV1 INTO DATA(LS_IV1).
    CLEAR LS_DDATA.
    READ TABLE GT_DDATA WITH KEY MBLNR = LS_IV1-LFBNR
                                 MJAHR = LS_IV1-LFGJA
                                 ZEILE = LS_IV1-LFPOS
                                 SEQ   = LS_IV1-SEQ
                                 BINARY SEARCH
                                 INTO LS_DDATA.
    IF SY-SUBRC = 0.
      LS_DDATA-BELNR   = LS_IV1-BELNR.
      LS_DDATA-GJAHR   = LS_IV1-GJAHR.
      LS_DDATA-BUZEI   = LS_IV1-BUZEI.
      LS_DDATA-BUDAT3  = LS_IV1-BUDAT.
      LS_DDATA-BLDAT3  = LS_IV1-BLDAT.
      LS_DDATA-WRBTR3  = LS_IV1-WRBTR.
      LS_DDATA-WAERS3  = LS_IV1-WAERS.
      LS_DDATA-MENGE3  = LS_IV1-MENGE.
      LS_DDATA-SHKZG3  = LS_IV1-SHKZG.
      LS_DDATA-VGABE   = LS_IV1-VGABE.
      LS_DDATA-VGABENM = LS_IV1-VGABENM.
      MODIFY GT_DDATA FROM LS_DDATA
                      TRANSPORTING BELNR GJAHR BUZEI BUDAT3 BLDAT3 WRBTR3 WAERS3
                                   MENGE3 SHKZG3 VGABE VGABENM
                             WHERE MBLNR = LS_IV1-LFBNR
                               AND MJAHR = LS_IV1-LFGJA
                               AND ZEILE = LS_IV1-LFPOS
                               AND SEQ   = LS_IV1-SEQ.
    ELSE.
      CLEAR LS_DDATA.
      READ TABLE GT_DDATA WITH KEY MBLNR = LS_IV1-LFBNR
                                   MJAHR = LS_IV1-LFGJA
                                   ZEILE = LS_IV1-LFPOS
                                   BINARY SEARCH
                                   INTO LS_DDATA.
      IF SY-SUBRC = 0.
        CLEAR GS_MAIN.
        GS_MAIN-MBLNR        = LS_DDATA-MBLNR.
        GS_MAIN-MJAHR        = LS_DDATA-MJAHR.
        GS_MAIN-ZEILE        = LS_DDATA-ZEILE.
        GS_MAIN-BUDAT        = LS_DDATA-BUDAT.
        GS_MAIN-BSTYP        = LS_DDATA-BSTYP.
        GS_MAIN-SEQ          = LS_IV1-SEQ.
        GS_MAIN-MBLNR1       = LS_DDATA-MBLNR.
        GS_MAIN-MJAHR1       = LS_DDATA-MJAHR.
        GS_MAIN-ZEILE1       = LS_DDATA-ZEILE.
        GS_MAIN-BUDAT1       = LS_DDATA-BUDAT.
        GS_MAIN-SHKZG1       = LS_DDATA-SHKZG1.
        GS_MAIN-MATNR        = LS_DDATA-MATNR.
        GS_MAIN-TXZ01        = LS_DDATA-TXZ01.
        GS_MAIN-MENGE        = LS_DDATA-MENGE.
        GS_MAIN-MEINS        = LS_DDATA-MEINS.
        GS_MAIN-WRBTR        = LS_DDATA-WRBTR.
        GS_MAIN-WAERS        = LS_DDATA-WAERS.
        GS_MAIN-EBELN        = LS_DDATA-EBELN.
        GS_MAIN-EBELP        = LS_DDATA-EBELP.
        GS_MAIN-VBELN_IM     = LS_DDATA-VBELN_IM.
        GS_MAIN-VBELP_IM     = LS_DDATA-VBELP_IM.
        GS_MAIN-LFIMG        = LS_DDATA-LFIMG.
        GS_MAIN-LIFNR        = LS_DDATA-LIFNR.
        GS_MAIN-LIFNRNM      = LS_DDATA-LIFNRNM.

        GS_MAIN-BELNR        = LS_IV1-BELNR.
        GS_MAIN-GJAHR        = LS_IV1-GJAHR.
        GS_MAIN-BUZEI        = LS_IV1-BUZEI.
        GS_MAIN-BUDAT3       = LS_IV1-BUDAT.
        GS_MAIN-BLDAT3       = LS_IV1-BLDAT.
        GS_MAIN-WRBTR3       = LS_IV1-WRBTR.
        IF GS_MAIN-WRBTR3 IS INITIAL. "송장 통화 없으면 입고 통화로 대체
          GS_MAIN-WRBTR3 = LS_DDATA-WAERS.
        ENDIF.
        GS_MAIN-WAERS3       = LS_IV1-WAERS.
        GS_MAIN-MENGE3       = LS_IV1-MENGE.
        GS_MAIN-SHKZG3       = LS_IV1-SHKZG.
        GS_MAIN-VGABE        = LS_IV1-VGABE.
        GS_MAIN-VGABENM      = LS_IV1-VGABENM.
        APPEND GS_MAIN TO GT_MAIN.
      ENDIF.
    ENDIF.
  ENDLOOP.

  IF GT_MAIN IS NOT INITIAL.
    CLEAR GS_MAIN.

    LOOP AT GT_MAIN INTO GS_MAIN.
      APPEND GS_MAIN TO GT_DDATA.
    ENDLOOP.
  ENDIF.

  PERFORM CALCULATE_DATA.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CALCULATE_DATA
*&---------------------------------------------------------------------*
FORM CALCULATE_DATA.

  DATA: LS_HDATA LIKE LINE OF GT_HDATA.

  LOOP AT GT_HDATA INTO LS_HDATA.

* 반품은 - 표기 하기로 했으므로 계산 시 더해줌 (원로직은 입고 - 반품 - 송장)
    LS_HDATA-MENGE_DIFF = LS_HDATA-MENGE +  LS_HDATA-MENGE2 - LS_HDATA-MENGE3.
    LS_HDATA-WRBTR_DIFF = LS_HDATA-WRBTR +  LS_HDATA-WRBTR2 - LS_HDATA-WRBTR3.


* 차후차변등 고려해서 송장 예정수량이 0이면 송장 예정금액도 0.
    IF LS_HDATA-MENGE_DIFF = 0.
      LS_HDATA-WRBTR_DIFF = 0.
    ENDIF.

    MODIFY GT_HDATA FROM LS_HDATA TRANSPORTING MENGE_DIFF WRBTR_DIFF.

* 송장단가 (NETPR3) 계산 ( 송장금액 WRBTR3 / 송장수량 MENGE3 )
    IF LS_HDATA-WRBTR3 IS NOT INITIAL AND LS_HDATA-MENGE3 IS NOT INITIAL.
      LS_HDATA-NETPR3 = LS_HDATA-WRBTR3 / LS_HDATA-MENGE3.

      MODIFY GT_HDATA FROM LS_HDATA TRANSPORTING NETPR3.
    ENDIF.

*  송장 통화 없으면 입고 통화로 대체
    IF LS_HDATA-WAERS3 IS INITIAL.
      LS_HDATA-WAERS3 = LS_HDATA-WAERS.

      MODIFY GT_HDATA FROM LS_HDATA TRANSPORTING WAERS3.
    ENDIF.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SUM_QTY_AMT
*&---------------------------------------------------------------------*
FORM SUM_QTY_AMT.

  DATA: LS_102_SUM LIKE LINE OF GT_102_SUM,
        LS_IV1_SUM LIKE LINE OF GT_IV1_SUM.

  CLEAR: GT_102_SUM.

  LOOP AT GT_102 INTO DATA(LS_102).
    IF LS_102-SHKZG = 'H'.
      LS_102-MENGE = LS_102-MENGE * -1.
      LS_102-WRBTR = LS_102-WRBTR * -1.
    ENDIF.
    MODIFY GT_102 FROM LS_102 TRANSPORTING MENGE WRBTR.

    CLEAR LS_102_SUM.
    MOVE-CORRESPONDING LS_102 TO LS_102_SUM.
    COLLECT LS_102_SUM INTO GT_102_SUM.
  ENDLOOP.

  SORT GT_102_SUM BY LFBNR LFBJA LFPOS.


  CLEAR: GT_IV1_SUM.

  LOOP AT GT_IV1 INTO DATA(LS_IV1).
    IF LS_IV1-SHKZG = 'H'.
      LS_IV1-MENGE = LS_IV1-MENGE  * -1.
      LS_IV1-WRBTR = LS_IV1-WRBTR  * -1.
      MODIFY GT_IV1 FROM LS_IV1 TRANSPORTING MENGE WRBTR.
    ENDIF.

    CLEAR LS_IV1_SUM.
    MOVE-CORRESPONDING LS_IV1 TO LS_IV1_SUM.
    COLLECT LS_IV1_SUM INTO GT_IV1_SUM.
  ENDLOOP.

  SORT GT_IV1_SUM BY LFBNR LFGJA LFPOS.

  GT_IV2_SUM = GT_IV1_SUM.
  SORT GT_IV2_SUM BY LFBNR LFGJA LFPOS WAERS.

  LOOP AT GT_HDATA INTO DATA(LS_HDATA).
    CLEAR LS_102_SUM.
    READ TABLE GT_102_SUM WITH KEY LFBNR = LS_HDATA-MBLNR
                                   LFBJA = LS_HDATA-MJAHR
                                   LFPOS = LS_HDATA-ZEILE
                                   BINARY SEARCH
                                   INTO LS_102_SUM.
    IF SY-SUBRC = 0.
      LS_HDATA-MENGE2 = LS_102_SUM-MENGE.
      LS_HDATA-WRBTR2 = LS_102_SUM-WRBTR.
      MODIFY GT_HDATA FROM LS_HDATA TRANSPORTING MENGE2 WRBTR2.
    ENDIF.

    CLEAR LS_IV1_SUM.
    READ TABLE GT_IV2_SUM WITH KEY LFBNR = LS_HDATA-MBLNR
                                   LFGJA = LS_HDATA-MJAHR
                                   LFPOS = LS_HDATA-ZEILE
                                   WAERS = LS_HDATA-WAERS   "입고 통화 베이스로 우선 검색
                                   BINARY SEARCH
                                   INTO LS_IV1_SUM.

    IF SY-SUBRC = 0 AND LS_IV1_SUM-MENGE IS NOT INITIAL.
      LS_HDATA-MENGE3 = LS_IV1_SUM-MENGE.
      LS_HDATA-WRBTR3 = LS_IV1_SUM-WRBTR.
      LS_HDATA-WAERS3 = LS_IV1_SUM-WAERS.
      MODIFY GT_HDATA FROM LS_HDATA TRANSPORTING MENGE3 WRBTR3 WAERS3.

    ELSE.
      CLEAR LS_IV1_SUM.
      READ TABLE GT_IV1_SUM WITH KEY LFBNR = LS_HDATA-MBLNR
                                     LFGJA = LS_HDATA-MJAHR
                                     LFPOS = LS_HDATA-ZEILE
                                     BINARY SEARCH
                                     INTO LS_IV1_SUM.
      IF SY-SUBRC = 0.
        LS_HDATA-MENGE3 = LS_IV1_SUM-MENGE.
        LS_HDATA-WRBTR3 = LS_IV1_SUM-WRBTR.
        LS_HDATA-WAERS3 = LS_IV1_SUM-WAERS.
        MODIFY GT_HDATA FROM LS_HDATA TRANSPORTING MENGE3 WRBTR3 WAERS3.
      ENDIF.
    ENDIF.
  ENDLOOP.

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

  DATA: LS_DISP TYPE TY_DISP.

  DATA: LS_LIFNR_SUM   TYPE TY_LIFNR_SUM,
        LS_MATNR_SUM   TYPE TY_MATNR_SUM,
        LS_LIMAT_SUM   TYPE TY_LIMAT_SUM,
        LS_DEPT_QM_SUM TYPE TY_DEPT_QM_SUM.

  CHECK GT_HDATA IS NOT INITIAL.

  CLEAR: GT_DISP, GT_ALL, GT_MATNR_SUM, GT_LIMAT_SUM.

  LOOP AT GT_HDATA INTO DATA(LS_HDATA).
    CLEAR LS_DISP.

    LS_DISP-BUDAT_GR   = LS_HDATA-BUDAT.
    LS_DISP-LIFNR      = LS_HDATA-LIFNR.
    LS_DISP-LIFNR_TEXT = LS_HDATA-LIFNRNM.
    LS_DISP-MBLNR_GR   = LS_HDATA-MBLNR.
    LS_DISP-MJAHR_GR   = LS_HDATA-MJAHR.
    LS_DISP-ZEILE_GR   = LS_HDATA-ZEILE.
    LS_DISP-BKTXT   = LS_HDATA-BKTXT.
    LS_DISP-MATNR      = LS_HDATA-MATNR.
    LS_DISP-TXZ01      = LS_HDATA-TXZ01.
    LS_DISP-CHARG      = LS_HDATA-CHARG.
    LS_DISP-BWTAR      = LS_HDATA-BWTAR.
    LS_DISP-MENGE_GR   = LS_HDATA-MENGE.
    LS_DISP-MEINS_GR   = LS_HDATA-MEINS.
    LS_DISP-WRBTR_GR   = LS_HDATA-WRBTR.
    LS_DISP-WAERS_GR   = LS_HDATA-WAERS.
    LS_DISP-MENGE_RE   = LS_HDATA-MENGE2.
    LS_DISP-WRBTR_RE   = LS_HDATA-WRBTR2.
    LS_DISP-MENGE_IV   = LS_HDATA-MENGE3.
    LS_DISP-WRBTR_IV   = LS_HDATA-WRBTR3.
    LS_DISP-WAERS_IV   = LS_HDATA-WAERS3.
    LS_DISP-NETPR_IV   = LS_HDATA-NETPR3.
    LS_DISP-MENGE_DIF  = LS_HDATA-MENGE_DIFF.
    LS_DISP-WRBTR_DIF  = LS_HDATA-WRBTR_DIFF.
    LS_DISP-EBELN      = LS_HDATA-EBELN.
    LS_DISP-EBELP      = LS_HDATA-EBELP.
    LS_DISP-BSTYP      = LS_HDATA-BSTYP.
    LS_DISP-PSTYP      = LS_HDATA-PSTYP.
    LS_DISP-VBELN_IM   = LS_HDATA-VBELN_IM.
    LS_DISP-VBELP_IM   = LS_HDATA-VBELP_IM.
    LS_DISP-LFIMG      = LS_HDATA-LFIMG.
    LS_DISP-NETPR_PO   = LS_HDATA-NETPR.
    LS_DISP-WAERS_PO   = LS_HDATA-PO_WAERS.
    LS_DISP-PEINH      = LS_HDATA-PEINH.
    LS_DISP-BPRME      = LS_HDATA-BPRME.
    LS_DISP-WERKS      = LS_HDATA-WERKS.
    LS_DISP-WERKS_TEXT = LS_HDATA-WERKSNM.

    APPEND LS_DISP TO GT_DISP.
  ENDLOOP.


  CLEAR LS_DISP.
  LOOP AT GT_DISP INTO LS_DISP.

* 상태 ICON (반품은 - 로 표기 하므로 계산시 - 대신 + 로 처리)
    IF LS_DISP-WRBTR_IV = LS_DISP-WRBTR_GR + LS_DISP-WRBTR_RE AND
       LS_DISP-MENGE_IV = LS_DISP-MENGE_GR + LS_DISP-MENGE_RE.
      LS_DISP-STATUS = ICON_LED_GREEN.
    ENDIF.

    IF LS_DISP-WRBTR_IV <> LS_DISP-WRBTR_GR + LS_DISP-WRBTR_RE.
      LS_DISP-STATUS = ICON_LED_YELLOW.
    ENDIF.

    IF LS_DISP-MENGE_IV <> LS_DISP-MENGE_GR + LS_DISP-MENGE_RE.
      LS_DISP-STATUS = ICON_LED_RED.
    ENDIF.

    IF LS_DISP-MENGE_DIF = 0.
      LS_DISP-STATUS = ICON_LED_GREEN.
    ENDIF.

* 임가공
    IF LS_DISP-PSTYP = '3'.
      LS_DISP-PSTYP_IND = 'X'.
    ENDIF.

    "납품일
    IF LS_DISP-BSTYP = 'F'.
      READ TABLE GT_EKET INTO DATA(LS_EKET)
                         WITH KEY EBELN = LS_DISP-EBELN
                                  EBELP = LS_DISP-EBELP
                                  BINARY SEARCH.
      IF SY-SUBRC EQ 0.
        LS_DISP-EINDT = LS_EKET-EINDT.
      ENDIF.
    ENDIF.

    MODIFY GT_DISP FROM LS_DISP.

*--------------------------------
* 합계 기준 별 Collect
*--------------------------------
    CLEAR: LS_LIFNR_SUM, LS_MATNR_SUM, LS_LIMAT_SUM, LS_DEPT_QM_SUM.

    MOVE-CORRESPONDING LS_DISP TO: LS_LIFNR_SUM, LS_MATNR_SUM, LS_LIMAT_SUM, LS_DEPT_QM_SUM.

    COLLECT: LS_LIFNR_SUM   INTO GT_LIFNR_SUM,
             LS_MATNR_SUM   INTO GT_MATNR_SUM,
             LS_LIMAT_SUM   INTO GT_LIMAT_SUM,
             LS_DEPT_QM_SUM INTO GT_DEPT_QM_SUM.

  ENDLOOP.

* 송장단가 재계산
  LOOP AT GT_LIFNR_SUM INTO LS_LIFNR_SUM.
    IF LS_LIFNR_SUM-WRBTR_IV IS NOT INITIAL AND
       LS_LIFNR_SUM-MENGE_IV IS NOT INITIAL.
      LS_LIFNR_SUM-NETPR_IV = LS_LIFNR_SUM-WRBTR_IV / LS_LIFNR_SUM-MENGE_IV.
    ENDIF.

    MODIFY GT_LIFNR_SUM FROM LS_LIFNR_SUM.
  ENDLOOP.

  SORT GT_LIMAT_SUM BY LIFNR LIFNR_TEXT MEINS_GR WAERS_GR WAERS_IV.

  LOOP AT GT_MATNR_SUM INTO LS_MATNR_SUM.
    IF LS_MATNR_SUM-WRBTR_IV IS NOT INITIAL AND
       LS_MATNR_SUM-MENGE_IV IS NOT INITIAL.
      LS_MATNR_SUM-NETPR_IV = LS_MATNR_SUM-WRBTR_IV / LS_MATNR_SUM-MENGE_IV.
    ENDIF.

    MODIFY GT_MATNR_SUM FROM LS_MATNR_SUM.
  ENDLOOP.

  SORT GT_MATNR_SUM BY MATNR TXZ01 MEINS_GR WAERS_GR WAERS_IV.

  LOOP AT GT_LIMAT_SUM INTO LS_LIMAT_SUM.
    IF LS_LIMAT_SUM-WRBTR_IV IS NOT INITIAL AND
       LS_LIMAT_SUM-MENGE_IV IS NOT INITIAL.
      LS_LIMAT_SUM-NETPR_IV = LS_LIMAT_SUM-WRBTR_IV / LS_LIMAT_SUM-MENGE_IV.
    ENDIF.

    MODIFY GT_LIMAT_SUM FROM LS_LIMAT_SUM.
  ENDLOOP.

  SORT GT_LIMAT_SUM BY LIFNR LIFNR_TEXT MATNR TXZ01 MEINS_GR WAERS_GR WAERS_IV.

  LOOP AT GT_DEPT_QM_SUM INTO LS_DEPT_QM_SUM.
    IF LS_DEPT_QM_SUM-WRBTR_IV IS NOT INITIAL AND
       LS_DEPT_QM_SUM-MENGE_IV IS NOT INITIAL.
      LS_DEPT_QM_SUM-NETPR_IV = LS_DEPT_QM_SUM-WRBTR_IV / LS_DEPT_QM_SUM-MENGE_IV.
    ENDIF.

    MODIFY GT_DEPT_QM_SUM FROM LS_DEPT_QM_SUM.
  ENDLOOP.

  SORT GT_DEPT_QM_SUM BY MEINS_GR WAERS_GR WAERS_IV.

  GV_CRITERIA = GC_CRITERIA_ALL.
  GT_ALL = GT_DISP.

  DESCRIBE TABLE GT_DISP LINES DATA(LV_TCNT).
  MESSAGE S011(ZMM01) WITH LV_TCNT.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_F4_WERKS
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> P_
*&---------------------------------------------------------------------*
FORM SET_F4_WERKS USING IV_SCR_NAME.

  DATA: LT_RETURN     TYPE TABLE OF DDSHRETVAL,
        LT_DYNPFIELDS TYPE TABLE OF DYNPREAD.

*U2> 번역을 위한 TEXT SYMBOL 변환.
  CONSTANTS: "LC_TITLE(15) TYPE C VALUE '플랜트',
             LC_RETFIELD  TYPE FIELDNAME VALUE 'WERKS'.

  FIELD-SYMBOLS: <LV_SCR_VALUE> TYPE ANY.
  ASSIGN (IV_SCR_NAME) TO <LV_SCR_VALUE>.


* 회사코드 Check
  PERFORM DYNP_VALUES_READ USING 'P_BUKRS' CHANGING P_BUKRS.

  IF P_BUKRS IS INITIAL.
    MESSAGE S017 WITH TEXT-F01 DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

* Get Data
  SELECT A~WERKS,
         A~NAME1
    FROM T001W AS A INNER JOIN T001K AS B
                            ON A~WERKS = B~BWKEY
   WHERE B~BUKRS EQ @P_BUKRS
    INTO TABLE @DATA(LT_T001W).

  SORT LT_T001W BY WERKS.

* Search Help
  PERFORM F4IF_INT_TABLE_VALUE_REQUEST TABLES LT_T001W
                                              LT_RETURN
*U2> 번역을 위한 TEXT SYMBOL 변환 - START
*                                       USING  LC_TITLE
                                       USING  TEXT-C32  "플랜트
*U2> 번역을 위한 TEXT SYMBOL 변환 - END
                                              LC_RETFIELD
                                              IV_SCR_NAME.

* Return
  READ TABLE LT_RETURN INTO DATA(LS_RETURN) INDEX 1.

  IF SY-SUBRC = 0.
    READ TABLE LT_T001W WITH KEY WERKS = LS_RETURN-FIELDVAL
                        BINARY SEARCH
                        INTO DATA(LS_T001W).

    <LV_SCR_VALUE> = LS_T001W-WERKS.

    LT_DYNPFIELDS = VALUE #( ( FIELDNAME = IV_SCR_NAME FIELDVALUE = LS_T001W-WERKS ) ).

    PERFORM DYNP_VALUES_UPDATE TABLES LT_DYNPFIELDS.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form DYNP_VALUES_READ
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> P_
*&      <-- P_BUKRS
*&---------------------------------------------------------------------*
FORM DYNP_VALUES_READ USING IV_SCR_NAME
                      CHANGING EV_SCR_VALUE.

  DATA: LT_DYNPFIELDS TYPE TABLE OF DYNPREAD,
        LS_DYNPFIELDS TYPE DYNPREAD.

  LS_DYNPFIELDS = VALUE #( FIELDNAME  = IV_SCR_NAME ).
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

    READ TABLE LT_DYNPFIELDS WITH KEY FIELDNAME = IV_SCR_NAME
                             BINARY SEARCH
                             INTO LS_DYNPFIELDS.
    IF SY-SUBRC = 0.
      EV_SCR_VALUE = LS_DYNPFIELDS-FIELDVALUE.
    ENDIF.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F4IF_INT_TABLE_VALUE_REQUEST
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LT_T001W
*&      --> LT_RETURN
*&      --> TEXT_C32
*&      --> LC_RETFIELD
*&      --> IV_SCR_NAME
*&---------------------------------------------------------------------*
FORM F4IF_INT_TABLE_VALUE_REQUEST TABLES IT_HELP_TAB
                                         CT_RETURN
                                  USING  IV_TITLE
                                         IV_RETFIELD
                                         IV_SCR_NAME.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      WINDOW_TITLE      = IV_TITLE
      RETFIELD          = IV_RETFIELD        "더블클릭하면 가져올 값
      DYNPPROG          = SY-CPROG
      DYNPNR            = SY-DYNNR
      DYNPROFIELD       = IV_SCR_NAME        "Retern Field가 실제로 복사될 화면 필드
      VALUE_ORG         = 'S'
    TABLES
      VALUE_TAB         = IT_HELP_TAB
      RETURN_TAB        = CT_RETURN
    EXCEPTIONS
      FIELD_NOT_FOUND   = 1
      NO_HELP_FOR_FIELD = 2
      INCONSISTENT_HELP = 3
      NO_VALUES_FOUND   = 4
      OTHERS            = 5.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form DYNP_VALUES_UPDATE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LT_DYNPFIELDS
*&---------------------------------------------------------------------*
FORM DYNP_VALUES_UPDATE TABLES IT_DYNPFIELDS STRUCTURE DYNPREAD.

  CALL FUNCTION 'DYNP_VALUES_UPDATE'
    EXPORTING
      DYNAME               = SY-CPROG
      DYNUMB               = SY-DYNNR
    TABLES
      DYNPFIELDS           = IT_DYNPFIELDS
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
*& Form SET_F4_BSART
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> P_
*&---------------------------------------------------------------------*
FORM SET_F4_BSART USING IV_SCR_NAME.

  DATA: LT_RETURN     TYPE TABLE OF DDSHRETVAL,
        LT_DYNPFIELDS TYPE TABLE OF DYNPREAD.

*U2> 번역을 위한 TEXT SYMBOL 변환
  CONSTANTS: "LC_TITLE(15) TYPE C VALUE '구매 문서 유형',
             LC_RETFIELD  TYPE FIELDNAME VALUE 'BSART'.

  FIELD-SYMBOLS: <LV_SCR_VALUE> TYPE ANY.
  ASSIGN (IV_SCR_NAME) TO <LV_SCR_VALUE>.

* Get Data
  SELECT BSTYP,
         BSART,
         BATXT
    FROM T161T
   WHERE SPRAS  = @SY-LANGU
     AND BSTYP IN ( 'F', 'L' )        "계약/PO 문서유형
    INTO TABLE @DATA(LT_T161T).

  SORT LT_T161T BY BSTYP BSART.

* Search Help
  PERFORM F4IF_INT_TABLE_VALUE_REQUEST TABLES LT_T161T
                                              LT_RETURN
*U2> 번역을 위한 TEXT SYMBOL 변환 - START
*                                       USING  LC_TITLE
                                       USING  TEXT-C48  "구매 문서 유형
*U2> 번역을 위한 TEXT SYMBOL 변환 - END
                                              LC_RETFIELD
                                              IV_SCR_NAME.

* Return
  READ TABLE LT_RETURN INTO DATA(LS_RETURN) INDEX 1.

  IF SY-SUBRC = 0.
    SORT LT_T161T BY BSART.

    READ TABLE LT_T161T WITH KEY BSART = LS_RETURN-FIELDVAL
                        BINARY SEARCH
                        INTO DATA(LS_T161T).

    <LV_SCR_VALUE> = LS_T161T-BSART.

    LT_DYNPFIELDS = VALUE #( ( FIELDNAME = IV_SCR_NAME FIELDVALUE = LS_T161T-BSART ) ).

    PERFORM DYNP_VALUES_UPDATE TABLES LT_DYNPFIELDS.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_F4_EKGRP
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> P_
*&---------------------------------------------------------------------*
FORM SET_F4_EKGRP USING IV_SCR_NAME.

  DATA: LT_RETURN     TYPE TABLE OF DDSHRETVAL,
        LT_DYNPFIELDS TYPE TABLE OF DYNPREAD.

*U2> 번역을 위한 TEXT SYMBOL 변환
  CONSTANTS: "LC_TITLE(15) TYPE C VALUE '구매그룹 정보',
             LC_RETFIELD  TYPE FIELDNAME VALUE 'EKGRP'.

  FIELD-SYMBOLS: <LV_SCR_VALUE> TYPE ANY.
  ASSIGN (IV_SCR_NAME) TO <LV_SCR_VALUE>.

* 회사코드 체크
  PERFORM DYNP_VALUES_READ USING 'P_BUKRS' CHANGING P_BUKRS.

  IF P_BUKRS IS INITIAL.
    MESSAGE S017 WITH TEXT-F01 DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

* Get Data
  SELECT DISTINCT EKGRP, EKNAM
    FROM ZSVCMM_ORG
   WHERE BUKRS = @P_BUKRS
    INTO TABLE @DATA(LT_ORG_INFO).

  SORT LT_ORG_INFO BY EKGRP.

* Search Help
  PERFORM F4IF_INT_TABLE_VALUE_REQUEST TABLES LT_ORG_INFO
                                              LT_RETURN
*U2> 번역을 위한 TEXT SYMBOL 변환 - START
*                                       USING  LC_TITLE
                                       USING  TEXT-C49  "구매그룹 정보
*U2> 번역을 위한 TEXT SYMBOL 변환 - END
                                              LC_RETFIELD
                                              IV_SCR_NAME.

* Return
  READ TABLE LT_RETURN INTO DATA(LS_RETURN) INDEX 1.

  IF SY-SUBRC = 0.
    READ TABLE LT_ORG_INFO WITH KEY EKGRP = LS_RETURN-FIELDVAL
                           BINARY SEARCH
                           INTO DATA(LS_ORG_INFO).

    <LV_SCR_VALUE> = LS_ORG_INFO-EKGRP.

    LT_DYNPFIELDS = VALUE #( ( FIELDNAME = IV_SCR_NAME FIELDVALUE = LS_ORG_INFO-EKGRP ) ).

    PERFORM DYNP_VALUES_UPDATE TABLES LT_DYNPFIELDS.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_F4_TAXIM
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> P_
*&---------------------------------------------------------------------*
FORM SET_F4_TAXIM USING IV_SCR_NAME..


  DATA: LT_RETURN TYPE TABLE OF DDSHRETVAL.

  CONSTANTS: LC_RETFIELD  TYPE FIELDNAME VALUE 'TAXIM'.

  DATA: LR_LAND1 TYPE RANGE OF T001-LAND1.

* 회사코드 Check
  PERFORM DYNP_VALUES_READ USING 'P_BUKRS' CHANGING P_BUKRS.

  SELECT SINGLE LAND1
    FROM T001
   WHERE BUKRS = @P_BUKRS
    INTO @DATA(LV_LAND1).
  IF SY-SUBRC EQ 0.
    APPEND VALUE #( SIGN = 'I' OPTION = 'EQ' LOW = LV_LAND1 ) TO LR_LAND1.
  ELSE.
    CLEAR LR_LAND1.
  ENDIF.

  SELECT DISTINCT ( TAXIM ),
         TAXIB
    FROM TMKM1T
   WHERE SPRAS EQ @SY-LANGU
     AND LAND1 IN @LR_LAND1
   ORDER BY TAXIM
    INTO TABLE @DATA(LT_TMKM1T).

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      WINDOW_TITLE      = TEXT-C47            "세금지시자
      RETFIELD          = LC_RETFIELD        "더블클릭하면 가져올 값
      DYNPPROG          = SY-CPROG
      DYNPNR            = SY-DYNNR
      DYNPROFIELD       = IV_SCR_NAME        "retfield 가 실제로 복사될 화면 필드
      VALUE_ORG         = 'S'
    TABLES
      VALUE_TAB         = LT_TMKM1T
      RETURN_TAB        = LT_RETURN
    EXCEPTIONS
      FIELD_NOT_FOUND   = 1
      NO_HELP_FOR_FIELD = 2
      INCONSISTENT_HELP = 3
      NO_VALUES_FOUND   = 4
      OTHERS            = 5.

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
*& Form SET_SEL_SCREEN
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM SET_SEL_SCREEN.

  CHECK SY-DYNNR = GC_DYNNR_1000.

* 물품선급,공사용역: 정발행 Only, 자율납품&위탁정산: 역발행 Only
  CASE 'X'.
    WHEN P_RS1. "물품일반
    WHEN P_RS2 OR P_RS4. "물품선급, 공사용역
      P_RB1 = 'X'.
      CLEAR P_RB2.
    WHEN P_RS3. "자율납품&위탁정산
      CLEAR P_RB1.
      P_RB2 = 'X'.
    WHEN OTHERS.
  ENDCASE.

  LOOP AT SCREEN.

    CASE SCREEN-GROUP1.
      WHEN 'EXC'.

        IF GV_EXC_USER IS INITIAL.
          SCREEN-INPUT = 0.
        ELSE.
          SCREEN-INPUT = 1.
        ENDIF.

      WHEN 'MRB'.
        IF P_RS2 EQ 'X' OR P_RS4 EQ 'X' OR P_RS3 EQ 'X'.
          SCREEN-INPUT = 0.
        ELSE.
          SCREEN-INPUT = 1.
        ENDIF.

      WHEN 'RS3'.
        IF P_BUKRS <> GC_BUKRS_1101.
          SCREEN-INPUT = 0.
        ENDIF.

      WHEN OTHERS.
    ENDCASE.

    MODIFY SCREEN.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form POPUP_GR_DETAIL
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LS_DISP
*&---------------------------------------------------------------------*
FORM POPUP_GR_DETAIL USING IS_DISP TYPE TY_DISP.

  DATA: LS_DETL TYPE TY_DETL.

  CHECK GT_DDATA IS NOT INITIAL.

  CLEAR GT_DETL.

*-----------------------------
* 입고 문서별 상세 조회
*-----------------------------
  SORT GT_DDATA BY MBLNR MJAHR ZEILE.

  READ TABLE GT_DDATA WITH KEY MBLNR = IS_DISP-MBLNR_GR
                               MJAHR = IS_DISP-MJAHR_GR
                               ZEILE = IS_DISP-ZEILE_GR
                               BINARY SEARCH
                               TRANSPORTING NO FIELDS.

  IF SY-SUBRC = 0.
    LOOP AT GT_DDATA INTO DATA(LS_DDATA) FROM SY-TABIX.

      IF LS_DDATA-MBLNR <> IS_DISP-MBLNR_GR OR
         LS_DDATA-MJAHR <> IS_DISP-MJAHR_GR OR
         LS_DDATA-ZEILE <> IS_DISP-ZEILE_GR.
        EXIT.
      ENDIF.

      CLEAR LS_DETL.

      IF GT_DETL IS INITIAL.
        LS_DETL-BUDAT_GR   = LS_DDATA-BUDAT.
        LS_DETL-LIFNR      = LS_DDATA-LIFNR.
        LS_DETL-LIFNR_TEXT = LS_DDATA-LIFNRNM.
        LS_DETL-MBLNR_GR   = LS_DDATA-MBLNR.
        LS_DETL-MJAHR_GR   = LS_DDATA-MJAHR.
        LS_DETL-ZEILE_GR   = LS_DDATA-ZEILE.
        LS_DETL-MATNR      = LS_DDATA-MATNR.
        LS_DETL-TXZ01      = LS_DDATA-TXZ01.
        LS_DETL-CHARG      = LS_DDATA-CHARG.
        LS_DETL-BWTAR      = LS_DDATA-BWTAR.
        LS_DETL-MENGE_GR   = LS_DDATA-MENGE.
        LS_DETL-MEINS_GR   = LS_DDATA-MEINS.
        LS_DETL-WRBTR_GR   = LS_DDATA-WRBTR.
        LS_DETL-WAERS_GR   = LS_DDATA-WAERS.
      ENDIF.

      LS_DETL-MBLNR_RE   = LS_DDATA-MBLNR2.
      LS_DETL-MJAHR_RE   = LS_DDATA-MJAHR2.
      LS_DETL-ZEILE_RE   = LS_DDATA-ZEILE2.
      LS_DETL-MENGE_RE   = LS_DDATA-MENGE2.
      LS_DETL-WRBTR_RE   = LS_DDATA-WRBTR2.

      LS_DETL-BELNR      = LS_DDATA-BELNR.
      LS_DETL-GJAHR      = LS_DDATA-GJAHR.
      LS_DETL-BUZEI      = LS_DDATA-BUZEI.
      LS_DETL-BUDAT_IV   = LS_DDATA-BUDAT3.
      LS_DETL-BLDAT_IV   = LS_DDATA-BLDAT3.
      LS_DETL-VGABENM    = LS_DDATA-VGABENM.
      LS_DETL-MENGE_IV   = LS_DDATA-MENGE3.
      LS_DETL-WRBTR_IV   = LS_DDATA-WRBTR3.
      LS_DETL-WAERS_IV   = LS_DDATA-WAERS3.

      LS_DETL-EBELN      = LS_DDATA-EBELN.
      LS_DETL-EBELP      = LS_DDATA-EBELP.
      LS_DETL-BSTYP      = LS_DDATA-BSTYP.
      LS_DETL-VBELN_IM   = LS_DDATA-VBELN_IM.
      LS_DETL-VBELP_IM   = LS_DDATA-VBELP_IM.
      LS_DETL-LFIMG      = LS_DDATA-LFIMG.
      LS_DETL-NETPR_PO   = LS_DDATA-NETPR.
      LS_DETL-WAERS_PO   = LS_DDATA-PO_WAERS.
      LS_DETL-PEINH      = LS_DDATA-PEINH.
      LS_DETL-BPRME      = LS_DDATA-BPRME.

      APPEND LS_DETL TO GT_DETL.
    ENDLOOP.

    CALL SCREEN '0200' STARTING AT 05 05.

  ELSE.
    MESSAGE S033 WITH TEXT-M01 DISPLAY LIKE 'E'.  "입고 상세내역이 존재하지 않습니다.
  ENDIF.

ENDFORM.
