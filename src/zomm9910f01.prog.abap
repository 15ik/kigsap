*&---------------------------------------------------------------------*
*& Include          ZOMM9910F01
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

  DATA : LV_DATE TYPE SY-DATLO.
  CONCATENATE SY-DATLO(6) '01' INTO LV_DATE.
*-

  " Excel 양식 Download
  DATA LV_FUNCTXT TYPE SMP_DYNTXT.
  LV_FUNCTXT-ICON_ID   = ICON_XLS. "Tool Bar에 쓸 icon종류
  LV_FUNCTXT-QUICKINFO = TEXT-M01.      "풍선도움말 text
  LV_FUNCTXT-ICON_TEXT = TEXT-M01.      "icon의 text
  SSCRFIELDS-FUNCTXT_01 = LV_FUNCTXT.

*  "전기일 : 전월 마지막 날 Default
*  CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
*    EXPORTING
*      DATE      = LV_DATE
*      DAYS      = '01'
*      MONTHS    = '00'
*      SIGNUM    = '-'
*      YEARS     = '00'
*    IMPORTING
*      CALC_DATE = LV_DATE.

*  P_BUDAT = LV_DATE.
*  P_BUDAT = sy-datlo.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form CREATE_OBJ
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM CREATE_OBJ .

  CREATE OBJECT GRF_GRID.

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

  IF P_BUKRS IS INITIAL.
    MESSAGE S000 WITH TEXT-M03 DISPLAY LIKE 'E'.  "회사코드는 필수 입력입니다.
    LEAVE LIST-PROCESSING.
  ENDIF.

  IF P_RD2A = 'X'.  " 작업구분 - 생성

    IF P_FILE IS INITIAL.
      MESSAGE S000 WITH TEXT-M02 DISPLAY LIKE 'E'.  "업로드 파일 을(를) 입력하세요.
      LEAVE LIST-PROCESSING.
    ENDIF.

    "Currency
    CLEAR GV_WAERS.
    SELECT SINGLE WAERS INTO @GV_WAERS
      FROM T001
     WHERE BUKRS EQ @P_BUKRS.

    CASE 'X'.
      WHEN P_RD1A.  " 배치 Migration
        PERFORM GET_UPLOAD_DATA_A.
      WHEN P_RD1B.  " 창고 Migration
        PERFORM GET_UPLOAD_DATA_B.
      WHEN P_RD1C.  " 사급 Migration
        PERFORM GET_UPLOAD_DATA_C.
      WHEN P_RD1D.  " 위탁 Migration
        PERFORM GET_UPLOAD_DATA_D.
      WHEN P_RD1E.  " 고객 Migration
        PERFORM GET_UPLOAD_DATA_E.
    ENDCASE.

    PERFORM PROCESSING_DATA.

  ELSEIF P_RD2B = 'X'.  " 작업구분 - 결과조회
    CLEAR : P_RD3D. "사용하지 않는 필드값 제외

    CASE 'X'.
      WHEN P_RD1A.  " 배치 Migration
        PERFORM GET_DATA_BATCH.
      WHEN P_RD1B.  " 창고 Migration
        PERFORM GET_DATA_B.
      WHEN P_RD1C.  " 사급 Migration
        PERFORM GET_DATA_C.
      WHEN P_RD1D.  " 위탁 Migration
        PERFORM GET_DATA_D.
      WHEN P_RD1E.  " 고객 Migration
        PERFORM GET_DATA_E.
    ENDCASE.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_UPLOAD_DATA_A
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM GET_UPLOAD_DATA_A .

  DATA: LV_FILE  TYPE STRING,
        LS_BATCH TYPE TS_BATCH.

  _G_INIT GT_UPD_A.


  LV_FILE = P_FILE.
  GRF_GRID->BTN_EXCL_UPLOAD( EXPORTING IV_FILENAME = CONV #( LV_FILE )
                                       IV_BEG_ROW  = 3
                                       IV_BEG_COL  = 1
                             CHANGING  CT_DATA     = GT_UPD_A ).


  CHECK GT_UPD_A IS NOT INITIAL.

*-
  LOOP AT GT_UPD_A INTO DATA(LS_UPD_A).

    MOVE-CORRESPONDING LS_UPD_A TO LS_BATCH.

    LS_BATCH-BUKRS = P_BUKRS.
    LS_BATCH-ERNAM = SY-UNAME.

    CLEAR LS_BATCH-ZMESSAGE.

    "자재코드 Conversion
    CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
      EXPORTING
        INPUT  = LS_BATCH-MATNR
      IMPORTING
        OUTPUT = LS_BATCH-MATNR.

    "업체코드 Conversion
    LS_BATCH-LIFNR = |{ LS_BATCH-LIFNR ALPHA = IN }|.

    "대문자로 변환
    TRANSLATE: LS_BATCH-MATNR         TO UPPER CASE.

    APPEND LS_BATCH TO GT_BATCH.     CLEAR LS_BATCH.
  ENDLOOP.

*-
ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_UPLOAD_DATA_B
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM GET_UPLOAD_DATA_B .

  DATA: LV_FILE TYPE STRING,
        LS_DISP TYPE TS_DISP.
*-
  _G_INIT GT_UPD_B.
*-

  IF P_WERKS IS INITIAL.
    MESSAGE S000 WITH TEXT-M04 DISPLAY LIKE 'E'.  "플랜트는 필수 입력입니다.
    LEAVE LIST-PROCESSING.
  ENDIF.

  IF P_BUDAT IS INITIAL.
    MESSAGE S000 WITH TEXT-M10 DISPLAY LIKE 'E'.  "전기일은 필수 입력입니다.
    LEAVE LIST-PROCESSING.
  ENDIF.

*  IF P_LGORT IS INITIAL.
*    MESSAGE S000 WITH TEXT-M05 DISPLAY LIKE 'E'.  "저장위치는 필수 입력입니다.
*    LEAVE LIST-PROCESSING.
*  ENDIF.

  LV_FILE = P_FILE.
  GRF_GRID->BTN_EXCL_UPLOAD( EXPORTING IV_FILENAME = CONV #( LV_FILE )
                                       IV_BEG_ROW  = 3
                                       IV_BEG_COL  = 1
                             CHANGING  CT_DATA     = GT_UPD_B ).


  CHECK GT_UPD_B IS NOT INITIAL.


  LOOP AT GT_UPD_B INTO DATA(LS_UPD_B).

    IF LS_UPD_B-WERKS NE P_WERKS.
      MESSAGE S000 WITH TEXT-M11 DISPLAY LIKE 'E'.  "Migration 대상 플랜트와 다른 정보가 있습니다. 확인하세요.
      LEAVE LIST-PROCESSING.
    ELSE.
      IF LS_UPD_B-INSMK IS INITIAL OR LS_UPD_B-INSMK = 'X' OR LS_UPD_B-INSMK = 'S'.

        MOVE-CORRESPONDING LS_UPD_B TO LS_DISP.

        LS_DISP-BUDAT = P_BUDAT.

        "단위 CONVERSION
        CALL FUNCTION 'CONVERSION_EXIT_CUNIT_INPUT'
          EXPORTING
            INPUT    = LS_DISP-ZMEINS
            LANGUAGE = SY-LANGU
          IMPORTING
            OUTPUT   = LS_DISP-ZMEINS.
*       EXCEPTIONS
*         UNIT_NOT_FOUND       = 1
*   OTHERS               = 2
        .

        "자재코드 Conversion
        CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
          EXPORTING
            INPUT  = LS_DISP-MATNR
          IMPORTING
            OUTPUT = LS_DISP-MATNR.

        "대문자로 변환
        TRANSLATE: LS_DISP-MATNR         TO UPPER CASE.

        APPEND LS_DISP TO GT_DISP.     CLEAR LS_DISP.

      ELSE.
        MESSAGE S000 WITH TEXT-M57 DISPLAY LIKE 'E'.  "Migration 대상 재고상태가 유효하지 않은 정보가 있습니다. 확인하세요.
        LEAVE LIST-PROCESSING.
      ENDIF.
    ENDIF.
  ENDLOOP.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_UPLOAD_DATA_C
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM GET_UPLOAD_DATA_C .


  DATA: LV_FILE TYPE STRING,
        LS_DISP TYPE TS_DISP.
*-
  _G_INIT GT_UPD_C.
*-

  IF P_WERKS IS INITIAL.
    MESSAGE S000 WITH TEXT-M04 DISPLAY LIKE 'E'.  "플랜트는 필수 입력입니다.
    LEAVE LIST-PROCESSING.
  ENDIF.

  IF P_BUDAT IS INITIAL.
    MESSAGE S000 WITH TEXT-M10 DISPLAY LIKE 'E'.  "전기일은 필수 입력입니다.
    LEAVE LIST-PROCESSING.
  ENDIF.

*  IF P_LIFNR IS INITIAL.
*    MESSAGE S000 WITH TEXT-M08 DISPLAY LIKE 'E'.  "공급업체는 필수 입력입니다.
*    LEAVE LIST-PROCESSING.
*  ELSE.
*
*    SELECT FROM LFM1
*      FIELDS LIFNR
*      WHERE LIFNR = @P_LIFNR
*      INTO TABLE @DATA(LT_LFM1).
*
*    IF LT_LFM1[] IS INITIAL.
*      MESSAGE S000 WITH TEXT-M15 DISPLAY LIKE 'E'.  "BP에 관리되지 않는 공급업체입니다.
*      LEAVE LIST-PROCESSING.
*    ENDIF.
*  ENDIF.
*
*  IF P_BUDAT IS INITIAL.
*    MESSAGE S000 WITH TEXT-M10 DISPLAY LIKE 'E'.  "저장위치는 필수 입력입니다.
*    LEAVE LIST-PROCESSING.
*  ENDIF.


*-

  LV_FILE = P_FILE.
  GRF_GRID->BTN_EXCL_UPLOAD( EXPORTING IV_FILENAME = CONV #( LV_FILE )
                                       IV_BEG_ROW  = 3
                                       IV_BEG_COL  = 1
                             CHANGING  CT_DATA     = GT_UPD_C ).


  CHECK GT_UPD_C IS NOT INITIAL.


  LOOP AT GT_UPD_C INTO DATA(LS_UPD_C).

    IF LS_UPD_C-WERKS NE P_WERKS.
      MESSAGE S000 WITH TEXT-M11 DISPLAY LIKE 'E'.  "Migration 대상 플랜트와 다른 정보가 있습니다. 확인하세요.
      LEAVE LIST-PROCESSING.

    ELSE.
      IF LS_UPD_C-INSMK IS INITIAL OR LS_UPD_C-INSMK = 'X' OR LS_UPD_C-INSMK = 'S'.
        MOVE-CORRESPONDING LS_UPD_C TO LS_DISP.

        LS_DISP-BUDAT = P_BUDAT.

        CALL FUNCTION 'CONVERSION_EXIT_CUNIT_INPUT'
          EXPORTING
            INPUT    = LS_DISP-ZMEINS
            LANGUAGE = SY-LANGU
          IMPORTING
            OUTPUT   = LS_DISP-ZMEINS.

        "자재코드 Conversion
        CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
          EXPORTING
            INPUT  = LS_DISP-MATNR
          IMPORTING
            OUTPUT = LS_DISP-MATNR.

        "업체코드 Conversion
        LS_DISP-LIFNR = |{ LS_DISP-LIFNR ALPHA = IN }|.

        "대문자로 변환
        TRANSLATE: LS_DISP-MATNR         TO UPPER CASE.

        APPEND LS_DISP TO GT_DISP.     CLEAR LS_DISP.
      ELSE.
        MESSAGE S000 WITH TEXT-M57 DISPLAY LIKE 'E'.  "Migration 대상 재고상태가 유효하지 않은 정보가 있습니다. 확인하세요.
        LEAVE LIST-PROCESSING.
      ENDIF.
    ENDIF.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_UPLOAD_DATA_D
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM GET_UPLOAD_DATA_D .

  DATA: LV_FILE TYPE STRING,
        LS_DISP TYPE TS_DISP.
*-
  _G_INIT GT_UPD_D.
*-
  IF P_WERKS IS INITIAL.
    MESSAGE S000 WITH TEXT-M04 DISPLAY LIKE 'E'.  "플랜트는 필수 입력입니다.
    LEAVE LIST-PROCESSING.
  ENDIF.

*  IF P_LGORT IS INITIAL.
*    MESSAGE S000 WITH TEXT-M05 DISPLAY LIKE 'E'.  "저장위치는 필수 입력입니다.
*    LEAVE LIST-PROCESSING.
*  ENDIF.
*
*  IF P_LIFNR IS INITIAL.
*    MESSAGE S000 WITH TEXT-M08 DISPLAY LIKE 'E'.  "공급업체는 필수 입력입니다.
*    LEAVE LIST-PROCESSING.
*  ELSE.
*
*    SELECT FROM LFM1
*      FIELDS LIFNR
*     WHERE LIFNR = @P_LIFNR
*     INTO TABLE @DATA(LT_LFM1).
*
*    IF LT_LFM1[] IS INITIAL.
*      MESSAGE S000 WITH TEXT-M15 DISPLAY LIKE 'E'.  "BP에 관리되지 않는 공급업체입니다.
*      LEAVE LIST-PROCESSING.
*    ENDIF.
*  ENDIF.

  IF P_BUDAT IS INITIAL.
    MESSAGE S000 WITH TEXT-M10 DISPLAY LIKE 'E'.  "전기일은 필수 입력입니다.
    LEAVE LIST-PROCESSING.
  ENDIF.

  LV_FILE = P_FILE.
  GRF_GRID->BTN_EXCL_UPLOAD( EXPORTING IV_FILENAME = CONV #( LV_FILE )
                                       IV_BEG_ROW  = 3
                                       IV_BEG_COL  = 1
                             CHANGING  CT_DATA     = GT_UPD_D ).

  CHECK GT_UPD_D IS NOT INITIAL.


  LOOP AT GT_UPD_D INTO DATA(LS_UPD_D).

    IF LS_UPD_D-WERKS NE P_WERKS.
      MESSAGE S000 WITH TEXT-M11 DISPLAY LIKE 'E'.  "Migration 대상 플랜트와 다른 정보가 있습니다. 확인하세요.
      LEAVE LIST-PROCESSING.

    ELSE.

      IF LS_UPD_D-INSMK IS INITIAL OR LS_UPD_D-INSMK = 'X' OR LS_UPD_D-INSMK = 'S'.

        MOVE-CORRESPONDING LS_UPD_D TO LS_DISP.

        LS_DISP-BUDAT = P_BUDAT.

        CALL FUNCTION 'CONVERSION_EXIT_CUNIT_INPUT'
          EXPORTING
            INPUT    = LS_DISP-ZMEINS
            LANGUAGE = SY-LANGU
          IMPORTING
            OUTPUT   = LS_DISP-ZMEINS.

        "자재코드 Conversion
        CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
          EXPORTING
            INPUT  = LS_DISP-MATNR
          IMPORTING
            OUTPUT = LS_DISP-MATNR.

        "업체코드 Conversion
        LS_DISP-LIFNR = |{ LS_DISP-LIFNR ALPHA = IN }|.

        "대문자로 변환
        TRANSLATE: LS_DISP-MATNR         TO UPPER CASE.

        "평가 금액 체크
        IF LS_DISP-EXBWR IS NOT INITIAL.
          LS_DISP-STATUS   = ICON_LED_RED.
          LS_DISP-ZMESSAGE = TEXT-M56.     "Fill UP 오류 : 위탁재고는 자사 재고가 아닙니다. 평가금액은 없습니다.
        ENDIF.

        APPEND LS_DISP TO GT_DISP.     CLEAR LS_DISP.

      ELSE.
        MESSAGE S000 WITH TEXT-M57 DISPLAY LIKE 'E'.  "Migration 대상 재고상태가 유효하지 않은 정보가 있습니다. 확인하세요.
        LEAVE LIST-PROCESSING.
      ENDIF.

    ENDIF.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_UPLOAD_DATA_E
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM GET_UPLOAD_DATA_E .

  DATA: LV_FILE TYPE STRING,
        LS_DISP TYPE TS_DISP.
*-
  _G_INIT GT_UPD_E.
*-

  IF P_WERKS IS INITIAL.
    MESSAGE S000 WITH TEXT-M04 DISPLAY LIKE 'E'.  "플랜트는 필수 입력입니다.
    LEAVE LIST-PROCESSING.
  ENDIF.

*  IF P_LGORT IS INITIAL.
*    MESSAGE S000 WITH TEXT-M05 DISPLAY LIKE 'E'.  "저장위치는 필수 입력입니다.
*    LEAVE LIST-PROCESSING.
*  ENDIF.
*
*  IF P_KUNNR IS INITIAL.
*    MESSAGE S000 WITH TEXT-M09 DISPLAY LIKE 'E'.  "고객은 필수 입력입니다.
*    LEAVE LIST-PROCESSING.
*  ELSE.
*
*    SELECT FROM KNA1
*      FIELDS KUNNR
*      WHERE KUNNR = @P_KUNNR
*      INTO TABLE @DATA(LT_KNA1).
*
*    IF LT_KNA1[] IS INITIAL.
*      MESSAGE S000 WITH TEXT-M16 DISPLAY LIKE 'E'.  "BP에 관리되지 않는 고객입니다.
*      LEAVE LIST-PROCESSING.
*    ENDIF.
*  ENDIF.

  IF P_BUDAT IS INITIAL.
    MESSAGE S000 WITH TEXT-M10 DISPLAY LIKE 'E'.  "전기일은 필수 입력입니다.
    LEAVE LIST-PROCESSING.
  ENDIF.


  LV_FILE = P_FILE.
  GRF_GRID->BTN_EXCL_UPLOAD( EXPORTING IV_FILENAME = CONV #( LV_FILE )
                                       IV_BEG_ROW  = 3
                                       IV_BEG_COL  = 1
                             CHANGING  CT_DATA     = GT_UPD_E ).

  CHECK GT_UPD_E IS NOT INITIAL.


  LOOP AT GT_UPD_E INTO DATA(LS_UPD_E).

    IF LS_UPD_E-WERKS NE P_WERKS.
      MESSAGE S000 WITH TEXT-M11 DISPLAY LIKE 'E'.  "Migration 대상 플랜트와 다른 정보가 있습니다. 확인하세요.
      LEAVE LIST-PROCESSING.

    ELSE.

      IF LS_UPD_E-INSMK IS INITIAL OR LS_UPD_E-INSMK = 'X' OR LS_UPD_E-INSMK = 'S'.

        MOVE-CORRESPONDING LS_UPD_E TO LS_DISP.

        LS_DISP-BUDAT = P_BUDAT.

        CALL FUNCTION 'CONVERSION_EXIT_CUNIT_INPUT'
          EXPORTING
            INPUT    = LS_DISP-ZMEINS
            LANGUAGE = SY-LANGU
          IMPORTING
            OUTPUT   = LS_DISP-ZMEINS.

        "자재코드 Conversion
        CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
          EXPORTING
            INPUT  = LS_DISP-MATNR
          IMPORTING
            OUTPUT = LS_DISP-MATNR.

        "업체코드 Conversion
        LS_DISP-KUNNR = |{ LS_DISP-KUNNR  ALPHA = IN }|.

        "대문자로 변환
        TRANSLATE: LS_DISP-MATNR         TO UPPER CASE.

        APPEND LS_DISP TO GT_DISP.     CLEAR LS_DISP.
      ELSE.
        MESSAGE S000 WITH TEXT-M57 DISPLAY LIKE 'E'.  "Migration 대상 재고상태가 유효하지 않은 정보가 있습니다. 확인하세요.
        LEAVE LIST-PROCESSING.
      ENDIF.
    ENDIF.
  ENDLOOP.

  "판매 오더 CHECK
  DATA(LT_DISP) = GT_DISP[].
  LT_DISP[] = GT_DISP[].
  SORT LT_DISP BY VBELN POSNR.
  DELETE ADJACENT DUPLICATES FROM LT_DISP COMPARING VBELN POSNR.

  IF LT_DISP[] IS NOT INITIAL.
    SELECT FROM VBAP
      FIELDS VBELN, POSNR
      FOR ALL ENTRIES IN @LT_DISP
      WHERE VBELN = @LT_DISP-VBELN
        AND POSNR = @LT_DISP-POSNR
      INTO TABLE @DATA(LT_VBAP).

    FREE LT_DISP.
  ENDIF.

  LOOP AT GT_DISP ASSIGNING FIELD-SYMBOL(<LS_DISP>).
    READ TABLE LT_VBAP INTO DATA(LS_VBAP) WITH KEY VBELN = <LS_DISP>-VBELN
                                                     POSNR = <LS_DISP>-POSNR.
    IF SY-SUBRC NE 0.
      <LS_DISP>-STATUS = ICON_LED_RED.
      <LS_DISP>-ZMESSAGE = TEXT-M41.        "Fill UP 오류 : 판매 오더 또는 판매 오더 항목이 유효하지 않습니다.
    ENDIF.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& FORM PROCESSING_DATA_BATCH
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM PROCESSING_DATA_BATCH .

  DATA : LV_ZSEQ TYPE ZTMM99110-ZSEQ.

*-
  SET UPDATE TASK LOCAL.

  DATA(LT_BATCH) = GT_BATCH[].
  SORT LT_BATCH BY MATNR.
  DELETE ADJACENT DUPLICATES FROM LT_BATCH COMPARING MATNR.

  IF LT_BATCH[] IS NOT INITIAL.
*- 데이터 사전 집계

    " 자재코드 체크용
    SELECT FROM MARA AS A INNER JOIN MAKT AS B ON A~MATNR = B~MATNR
      FIELDS A~MATNR, A~XCHPF, B~MAKTX
      FOR ALL ENTRIES IN @LT_BATCH
      WHERE A~MATNR = @LT_BATCH-MATNR
        AND B~SPRAS = @SY-LANGU
      INTO TABLE @DATA(LT_MARA).


    "배치클래스 체크용
    SELECT FROM MARA AS A INNER JOIN INOB AS B ON A~MATNR = B~OBJEK
                          INNER JOIN KSSK AS C ON B~CUOBJ = C~OBJEK
                          INNER JOIN KLAH AS D ON C~CLINT = D~CLINT AND C~KLART = D~KLART
      FIELDS A~MATNR, D~CLASS
      FOR ALL ENTRIES IN @LT_BATCH
      WHERE A~MATNR = @LT_BATCH-MATNR
      INTO TABLE @DATA(LT_CLASS).


    "외부 채번 체크용
    SELECT FROM MARC
      FIELDS MATNR, BESKZ, SOBSL
      FOR ALL ENTRIES IN @LT_BATCH
      WHERE MATNR = @LT_BATCH-MATNR
        AND WERKS = @GC_3001
      INTO TABLE @DATA(LT_MARC).

    FREE LT_BATCH.

    LT_BATCH[] = GT_BATCH[].
    SORT LT_BATCH BY LIFNR.
    DELETE ADJACENT DUPLICATES FROM LT_BATCH COMPARING LIFNR.

    "공급업체 체크용
    SELECT FROM LFA1
      FIELDS LIFNR
      FOR ALL ENTRIES IN @LT_BATCH
      WHERE LIFNR = @LT_BATCH-LIFNR
      INTO TABLE @DATA(LT_LFA1).

    FREE LT_BATCH.

  ENDIF.

  SELECT SINGLE FROM ZTMM99110
    FIELDS MAX( ZSEQ )
    WHERE BUKRS = @P_BUKRS
      AND ERNAM = @SY-UNAME
    INTO ( @LV_ZSEQ ).

  IF LV_ZSEQ IS INITIAL.
    LV_ZSEQ = GC_00000000.
  ENDIF.

  ZCL_MM_COMMON=>COMMON_CONFIG(
     EXPORTING IS_COMMON = VALUE #( M = 'D1' D = 'D1100' S = 'D1103' )
     IMPORTING ET_OUTTAB = DATA(LT_COMMON) ).

  LT_BATCH[] = GT_BATCH[].
  SORT LT_BATCH BY MATNR LIFNR HSDAT LICHA VFDAT.
  DELETE ADJACENT DUPLICATES FROM LT_BATCH COMPARING MATNR LIFNR HSDAT LICHA VFDAT.

  IF LT_BATCH[] IS NOT INITIAL.
    "동일 배치 조회(구매배치)
    SELECT FROM ZSVCMM_BATFIND
      FIELDS MATNR, BATCH, LIFNR, LWEDT, HSDAT, LICHN, VFDAT, ZMAKER
      FOR ALL ENTRIES IN @LT_BATCH
      WHERE MATNR      = @LT_BATCH-MATNR
        AND LIFNR      = @LT_BATCH-LIFNR
        AND HSDAT      = @LT_BATCH-HSDAT
        AND LICHN      = @LT_BATCH-LICHA
        AND VFDAT      = @LT_BATCH-VFDAT
      INTO TABLE @DATA(LT_CHARG).

    FREE LT_BATCH.

    LT_BATCH[] = GT_BATCH[].
    SORT LT_BATCH BY MATNR EX_CHARG.
    DELETE ADJACENT DUPLICATES FROM LT_BATCH COMPARING MATNR EX_CHARG.

    SELECT FROM MCH1
      FIELDS MATNR, CHARG
      FOR ALL ENTRIES IN @LT_BATCH
      WHERE MATNR = @LT_BATCH-MATNR
        AND CHARG = @LT_BATCH-EX_CHARG
      INTO TABLE @DATA(LT_MCH1).

    FREE LT_BATCH.

  ENDIF.


  SORT : LT_MARA BY MATNR,
         LT_CLASS BY MATNR,
         LT_MARC BY MATNR,
         LT_LFA1 BY LIFNR,
         LT_CHARG BY MATNR.

*-
  LOOP AT GT_BATCH ASSIGNING FIELD-SYMBOL(<LS_BATCH>).

    CLEAR : <LS_BATCH>-STATUS, <LS_BATCH>-ZMESSAGE.

*-
    CLEAR GS_MSGTB.

    "순번(ZSEQ) = LV_ZSEQ + 1.
    LV_ZSEQ = LV_ZSEQ + 1.
    <LS_BATCH>-ZSEQ = LV_ZSEQ.

    "자재코드 CHECK
    READ TABLE LT_MARA INTO DATA(LS_MARA) WITH KEY MATNR = <LS_BATCH>-MATNR.
    IF SY-SUBRC = 0.
      <LS_BATCH>-MAKTX = LS_MARA-MAKTX.
      <LS_BATCH>-XCHPF = LS_MARA-XCHPF.
      IF <LS_BATCH>-XCHPF IS INITIAL.
        <LS_BATCH>-STATUS = ICON_LED_RED.
        <LS_BATCH>-ZMESSAGE = TEXT-M20.     "배치관리 오류 : 배치 관리 대상이 아닙니다.
        CONTINUE.
      ENDIF.
    ELSE.
      <LS_BATCH>-STATUS = ICON_LED_RED.
      <LS_BATCH>-ZMESSAGE = TEXT-M21.       "자재코드 오류 : 자재코드가 없습니다.
      CONTINUE.
    ENDIF.

    "BATCH CLASS CHECK
    READ TABLE LT_CLASS INTO DATA(LS_CLASS) WITH KEY MATNR = <LS_BATCH>-MATNR.
    IF SY-SUBRC NE 0.
      <LS_BATCH>-STATUS = ICON_LED_RED.
      <LS_BATCH>-ZMESSAGE = TEXT-M22.       "배치관리 오류 : 배치 클래스가 지정되지 않았습니다.
      CONTINUE.
    ELSE.
      <LS_BATCH>-CLASS = LS_CLASS-CLASS.
      READ TABLE LT_COMMON INTO DATA(LS_COMMON) WITH KEY FIELD1 = <LS_BATCH>-CLASS.
      IF SY-SUBRC = 0.
        IF <LS_BATCH>-CLASS = GC_ZPP_KGC1  AND <LS_BATCH>-EX_CHARG IS NOT INITIAL.
          <LS_BATCH>-ZCHECK = ''.
        ELSE.
          <LS_BATCH>-ZCHECK = 'X'. "구매 관리

          IF P_BUKRS NE GC_3101. " KGC는 동일 속성은 같은 배치, YJP는 각각의 배치
*              IF  <LS_BATCH>-CLASS  = GC_ZPP_KGC1  AND STRLEN( <LS_BATCH>-EX_CHARG ) <> 10 AND <LS_BATCH>-EX_CHARG IS NOT INITIAL.
*
*              ELSE.
            READ TABLE LT_CHARG INTO DATA(LS_CHARG) WITH KEY MATNR      = <LS_BATCH>-MATNR
                                                             LIFNR      = <LS_BATCH>-LIFNR
                                                             HSDAT      = <LS_BATCH>-HSDAT
                                                             VFDAT      = <LS_BATCH>-VFDAT
                                                             LICHN      = <LS_BATCH>-LICHA
                                                             ZMAKER     = <LS_BATCH>-ZMM_MAKER.
            IF SY-SUBRC = 0.
              <LS_BATCH>-CHARG = LS_CHARG-BATCH.
              <LS_BATCH>-STATUS = ICON_LED_YELLOW.
              <LS_BATCH>-ZMESSAGE = TEXT-M23.    "중복 배치 : 동일 속성 배치가 이미 존재합니다.
              CONTINUE.
            ELSE.
              <LS_BATCH>-CHARG = ''.
            ENDIF.

*              ENDIF.

          ENDIF.
        ENDIF.
      ELSE.
        <LS_BATCH>-ZCHECK = ''.
      ENDIF.
    ENDIF.

    "필수 배치클래스 속성 정보 누락 체크
    PERFORM CHECK_CLASS_BATCHDATA USING <LS_BATCH>.
    IF <LS_BATCH>-STATUS = ICON_LED_RED.
      CONTINUE.
    ENDIF.

    "입력 속성 MAKER CHECK
    IF <LS_BATCH>-CLASS = GC_KGCBATCH2 OR <LS_BATCH>-CLASS = GC_KGCBATCH3 OR <LS_BATCH>-CLASS = GC_KGCBATCH4 OR <LS_BATCH>-CLASS = GC_ZPP_KGC1
       OR <LS_BATCH>-CLASS = GC_ZPP_YJP OR <LS_BATCH>-CLASS = GC_YJPBATCH2.
      IF <LS_BATCH>-ZMM_MAKER IS NOT INITIAL.
        <LS_BATCH>-STATUS = ICON_LED_RED.
        <LS_BATCH>-ZMESSAGE = TEXT-M47.         "Fill UP 오류 : 제조처는 배치 관리 속성이 아닙니다.
        CONTINUE.
      ENDIF.
    ENDIF.

    IF  <LS_BATCH>-EX_CHARG IS NOT INITIAL.
      READ TABLE LT_MCH1 INTO DATA(LS_MCH1) WITH KEY CHARG = <LS_BATCH>-EX_CHARG MATNR = <LS_BATCH>-MATNR.
      IF SY-SUBRC = 0.
        <LS_BATCH>-CHARG = LS_MCH1-CHARG.
        <LS_BATCH>-STATUS = ICON_LED_YELLOW.
        <LS_BATCH>-ZMESSAGE = TEXT-M36.    "중복 배치 : 동일 배치가 이미 존재합니다.
        CONTINUE.
      ENDIF.
    ENDIF.

    "YJP 외부 배치번호 Check
    IF <LS_BATCH>-ZCHECK = 'X' AND P_BUKRS = GC_3101.
      READ TABLE LT_MARC INTO DATA(LS_MARC) WITH KEY MATNR = <LS_BATCH>-MATNR.
      IF SY-SUBRC = 0 AND ( LS_MARC-BESKZ = 'X' OR LS_MARC-BESKZ = 'F' ) AND LS_MARC-SOBSL = '30' AND <LS_BATCH>-EX_CHARG IS INITIAL.
        <LS_BATCH>-STATUS = ICON_LED_RED.
        <LS_BATCH>-ZMESSAGE = TEXT-M24.         "채번 오류 : 외부 채번 대상은 외부 배치 번호가 필요합니다.
*          CONTINUE.

        READ TABLE LT_LFA1 INTO DATA(LS_LFA1) WITH KEY LIFNR = <LS_BATCH>-LIFNR.
        IF SY-SUBRC NE 0.
          <LS_BATCH>-STATUS = ICON_LED_RED.
          <LS_BATCH>-ZMESSAGE = TEXT-M24 && '/' && TEXT-M25.    "속성 오류 : 공급업체가 존재하지 않습니다.
          CONTINUE.
        ENDIF.

      ENDIF.
    ENDIF.

  ENDLOOP.

  "KGC 중복 데이터 CHECK
  "동일 LotNo 발생으로 인해 중복 체크 로직 제외_KGC이준열(담당자) 요청_220214
*  PERFORM DUP_CHECK_2101.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_DATA_BATCH
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM GET_DATA_BATCH .

  _G_INIT : GT_BATCH.

  DATA : LT_ZTMM99110 TYPE TABLE OF ZTMM99110,
         LS_BATCH     TYPE TS_BATCH.

  CASE 'X'.
    WHEN P_RD3A.    "전체

      SELECT FROM ZTMM99110
        FIELDS BUKRS, ERNAM, ZSEQ, MATNR, XCHPF, CLASS, ZCHECK, LIFNR,
               HSDAT, VFDAT, LICHA, ZMM_MAKER, ZCOMP_RATIO, ZCOMP_MOIST, ZCOMP_NET, ZCOMP_DRY,
               ZSTATUS, ZMESSAGE, EX_CHARG, CHARG, ERDAT, ERZET

        WHERE BUKRS = @P_BUKRS
          AND MATNR IN @S_MATNR
          AND CHARG IN @S_CHARG
          AND ERDAT IN @S_ERDAT
          AND ERNAM IN @S_ERNAM

        APPENDING CORRESPONDING FIELDS OF TABLE @LT_ZTMM99110.

    WHEN P_RD3B.    "성공

      SELECT FROM ZTMM99110
        FIELDS BUKRS, ERNAM, ZSEQ, MATNR, XCHPF, CLASS, ZCHECK, LIFNR,
               HSDAT, VFDAT, LICHA, ZMM_MAKER, ZCOMP_RATIO, ZCOMP_MOIST, ZCOMP_NET, ZCOMP_DRY,
               ZSTATUS, ZMESSAGE, EX_CHARG, CHARG, ERDAT, ERZET

        WHERE BUKRS = @P_BUKRS
          AND MATNR IN @S_MATNR
          AND CHARG IN @S_CHARG
          AND ERDAT IN @S_ERDAT
          AND ERNAM IN @S_ERNAM
          AND ZSTATUS = 'S'

        APPENDING CORRESPONDING FIELDS OF TABLE @LT_ZTMM99110.

    WHEN P_RD3C.    "실패

      SELECT FROM ZTMM99110
        FIELDS BUKRS, ERNAM, ZSEQ, MATNR, XCHPF, CLASS, ZCHECK, LIFNR,
               HSDAT, VFDAT, LICHA, ZMM_MAKER, ZCOMP_RATIO, ZCOMP_MOIST, ZCOMP_NET, ZCOMP_DRY,
               ZSTATUS, ZMESSAGE, EX_CHARG, CHARG, ERDAT, ERZET

        WHERE BUKRS = @P_BUKRS
          AND MATNR IN @S_MATNR
          AND CHARG IN @S_CHARG
          AND ERDAT IN @S_ERDAT
          AND ERNAM IN @S_ERNAM
          AND ZSTATUS NE 'S'

        APPENDING CORRESPONDING FIELDS OF TABLE @LT_ZTMM99110.

    WHEN OTHERS.

  ENDCASE.

  IF LT_ZTMM99110[] IS NOT INITIAL.

    DATA(LT_SUB) = LT_ZTMM99110[].
    SORT LT_SUB BY MATNR.
    DELETE ADJACENT DUPLICATES FROM LT_SUB COMPARING MATNR.

    IF LT_SUB[] IS NOT INITIAL.

      SELECT FROM MARA AS A INNER JOIN MAKT AS B ON A~MATNR = B~MATNR
        FIELDS A~MATNR, A~XCHPF, B~MAKTX
        FOR ALL ENTRIES IN @LT_SUB
        WHERE A~MATNR = @LT_SUB-MATNR
          AND B~SPRAS = @SY-LANGU
        INTO TABLE @DATA(LT_MARA).

    ENDIF.

    LOOP AT LT_ZTMM99110 INTO DATA(LS_ZTMM99110).
      MOVE-CORRESPONDING LS_ZTMM99110 TO LS_BATCH.

      IF LS_ZTMM99110-ZSTATUS = 'S'.
        LS_BATCH-STATUS = ICON_LED_GREEN.
      ELSE.
        LS_BATCH-STATUS = ICON_LED_RED.
      ENDIF.

      READ TABLE LT_MARA INTO DATA(LS_MARA) WITH KEY MATNR = LS_BATCH-MATNR.
      IF SY-SUBRC = 0.
        LS_BATCH-MAKTX = LS_MARA-MAKTX.
      ENDIF.

      APPEND LS_BATCH TO GT_BATCH. CLEAR LS_BATCH.

    ENDLOOP.

  ENDIF.

  IF GT_BATCH[] IS NOT INITIAL.
    SORT GT_BATCH BY ZSEQ.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_DATA_B
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM GET_DATA_B .


  _G_INIT : GT_DISP.

  DATA : LT_ZTMM99120 TYPE TABLE OF ZTMM99120,
         LS_DISP      TYPE TS_DISP.


  CASE 'X'.
    WHEN P_RD3A.    "전체

      SELECT FROM ZTMM99120
        FIELDS BUKRS, WERKS, LGORT, ERNAM, ZSEQ, MATNR, BESKZ, SOBSL, MMSTA, BKLAS, XCHPF, CHARG, BWTTY,
               BWTAR, VPRSV, BWART, ERFMG, MEINS, ZMEINS, INSMK, EXBWR, WAERS, MJAHR, MBLNR, ZEILE, BUDAT,
               MJAHR_R, MBLNR_R, ZEILE_R, ZSTATUS, ZMESSAGE, ERDAT, ERZET, AEDAT, AEZET, AENAM

        WHERE BUKRS = @P_BUKRS
          AND MATNR IN @S_MATNR
          AND WERKS IN @S_WERKS
          AND LGORT IN @S_LGORT
          AND ERDAT IN @S_ERDAT
          AND ERNAM IN @S_ERNAM

        APPENDING CORRESPONDING FIELDS OF TABLE @LT_ZTMM99120.

    WHEN P_RD3B.    "성공

      SELECT FROM ZTMM99120
        FIELDS BUKRS, WERKS, LGORT, ERNAM, ZSEQ, MATNR, BESKZ, SOBSL, MMSTA, BKLAS, XCHPF, CHARG, BWTTY,
               BWTAR, VPRSV, BWART, ERFMG, MEINS, ZMEINS, INSMK, EXBWR, WAERS, MJAHR, MBLNR, ZEILE, BUDAT,
               MJAHR_R, MBLNR_R, ZEILE_R, ZSTATUS, ZMESSAGE, ERDAT, ERZET, AEDAT, AEZET, AENAM

        WHERE BUKRS = @P_BUKRS
          AND MATNR IN @S_MATNR
          AND WERKS IN @S_WERKS
          AND LGORT IN @S_LGORT
          AND ERDAT IN @S_ERDAT
          AND ERNAM IN @S_ERNAM
          AND ZSTATUS = 'S'

        APPENDING CORRESPONDING FIELDS OF TABLE @LT_ZTMM99120.

    WHEN P_RD3C.    "실패

      SELECT FROM ZTMM99120
        FIELDS BUKRS, WERKS, LGORT, ERNAM, ZSEQ, MATNR, BESKZ, SOBSL, MMSTA, BKLAS, XCHPF, CHARG, BWTTY,
               BWTAR, VPRSV, BWART, ERFMG, MEINS, ZMEINS, INSMK, EXBWR, WAERS, MJAHR, MBLNR, ZEILE, BUDAT,
               MJAHR_R, MBLNR_R, ZEILE_R, ZSTATUS, ZMESSAGE, ERDAT, ERZET, AEDAT, AEZET, AENAM

        WHERE BUKRS = @P_BUKRS
          AND MATNR IN @S_MATNR
          AND WERKS IN @S_WERKS
          AND LGORT IN @S_LGORT
          AND ERDAT IN @S_ERDAT
          AND ERNAM IN @S_ERNAM
          AND ZSTATUS NE 'S'

        APPENDING CORRESPONDING FIELDS OF TABLE @LT_ZTMM99120.

  ENDCASE.

  IF LT_ZTMM99120[] IS NOT INITIAL.

    DATA(LT_SUB) = LT_ZTMM99120[].
    SORT LT_SUB BY MATNR WERKS LGORT.
    DELETE ADJACENT DUPLICATES FROM LT_SUB COMPARING MATNR WERKS LGORT.

    IF LT_SUB[] IS NOT INITIAL.

      SELECT FROM MARA AS A INNER JOIN MAKT AS B ON A~MATNR = B~MATNR
                       INNER JOIN MARC AS C ON A~MATNR = C~MATNR
                       INNER JOIN MBEW AS D ON C~MATNR = D~MATNR AND C~WERKS = D~BWKEY
                       INNER JOIN T001W AS E ON D~BWKEY = E~WERKS
                       INNER JOIN T001L AS F ON E~WERKS = F~WERKS
        FIELDS A~MATNR, B~MAKTX, E~NAME1, F~LGORT, F~LGOBE, C~WERKS, C~XCHPF, C~BESKZ, C~SOBSL,
               C~MMSTA, D~BKLAS, D~BWTAR, C~BWTTY, D~VPRSV, A~MEINS
        FOR ALL ENTRIES IN @LT_SUB
        WHERE A~MATNR = @LT_SUB-MATNR
          AND C~WERKS = @LT_SUB-WERKS
          AND F~LGORT = @LT_SUB-LGORT
          AND B~SPRAS = @SY-LANGU
        INTO TABLE @DATA(LT_MARA).

    ENDIF.

    LOOP AT LT_ZTMM99120 INTO DATA(LS_ZTMM99120).

      MOVE-CORRESPONDING LS_ZTMM99120 TO LS_DISP.

      IF LS_ZTMM99120-ZSTATUS = 'S'.
        LS_DISP-STATUS = ICON_LED_GREEN.
      ELSE.
        LS_DISP-STATUS = ICON_LED_RED.
      ENDIF.

      READ TABLE LT_MARA INTO DATA(LS_MARA) WITH KEY MATNR = LS_DISP-MATNR WERKS = LS_DISP-WERKS LGORT  = LS_DISP-LGORT.
      IF SY-SUBRC = 0.
        LS_DISP-MAKTX = LS_MARA-MAKTX.
        LS_DISP-NAME1_W = LS_MARA-NAME1.
        LS_DISP-LGOBE = LS_MARA-LGOBE.
      ENDIF.

      APPEND LS_DISP TO GT_DISP. CLEAR LS_DISP.

    ENDLOOP.

  ENDIF.

  IF GT_DISP[] IS NOT INITIAL.
    SORT GT_DISP BY ZSEQ.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_DATA_C
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM GET_DATA_C .


  _G_INIT : GT_DISP.

  DATA : LT_ZTMM99130 TYPE TABLE OF ZTMM99130,
         LS_DISP      TYPE TS_DISP.


  CASE 'X'.
    WHEN P_RD3A.    "전체

      SELECT FROM ZTMM99130
        FIELDS BUKRS, WERKS, LIFNR, ZSEQ, MATNR, BESKZ, SOBSL, MMSTA, BKLAS, XCHPF, CHARG, BWTTY, BWTAR, VPRSV, BWART,
               ERFMG, MEINS, ZMEINS, INSMK, EXBWR, WAERS, MJAHR, MBLNR, ZEILE, BUDAT, MJAHR_R, MBLNR_R, ZEILE_R, ZSTATUS,
               ZMESSAGE, ERDAT, ERZET, ERNAM, AEDAT, AEZET, AENAM, ZDELE

        WHERE BUKRS = @P_BUKRS
          AND MATNR IN @S_MATNR
          AND WERKS IN @S_WERKS
          AND LIFNR IN @S_LIFNR
          AND ERDAT IN @S_ERDAT
          AND ERNAM IN @S_ERNAM

        APPENDING CORRESPONDING FIELDS OF TABLE @LT_ZTMM99130.

    WHEN P_RD3B.    "성공

      SELECT FROM ZTMM99130
        FIELDS BUKRS, WERKS, LIFNR, ZSEQ, MATNR, BESKZ, SOBSL, MMSTA, BKLAS, XCHPF, CHARG, BWTTY, BWTAR, VPRSV, BWART,
               ERFMG, MEINS, ZMEINS, INSMK, EXBWR, WAERS, MJAHR, MBLNR, ZEILE, BUDAT, MJAHR_R, MBLNR_R, ZEILE_R, ZSTATUS,
               ZMESSAGE, ERDAT, ERZET, ERNAM, AEDAT, AEZET, AENAM, ZDELE

        WHERE BUKRS = @P_BUKRS
          AND MATNR IN @S_MATNR
          AND WERKS IN @S_WERKS
          AND LIFNR IN @S_LIFNR
          AND ERDAT IN @S_ERDAT
          AND ERNAM IN @S_ERNAM
          AND ZSTATUS = 'S'

        APPENDING CORRESPONDING FIELDS OF TABLE @LT_ZTMM99130.

    WHEN P_RD3C.    "실패

      SELECT FROM ZTMM99130
        FIELDS BUKRS, WERKS, LIFNR, ZSEQ, MATNR, BESKZ, SOBSL, MMSTA, BKLAS, XCHPF, CHARG, BWTTY, BWTAR, VPRSV, BWART,
               ERFMG, MEINS, ZMEINS, INSMK, EXBWR, WAERS, MJAHR, MBLNR, ZEILE, BUDAT, MJAHR_R, MBLNR_R, ZEILE_R, ZSTATUS,
               ZMESSAGE, ERDAT, ERZET, ERNAM, AEDAT, AEZET, AENAM, ZDELE

        WHERE BUKRS = @P_BUKRS
          AND MATNR IN @S_MATNR
          AND WERKS IN @S_WERKS
          AND LIFNR IN @S_LIFNR
          AND ERDAT IN @S_ERDAT
          AND ERNAM IN @S_ERNAM
          AND ZSTATUS NE 'S'

        APPENDING CORRESPONDING FIELDS OF TABLE @LT_ZTMM99130.

  ENDCASE.

  IF LT_ZTMM99130[] IS NOT INITIAL.

    DATA(LT_SUB) = LT_ZTMM99130[].
    SORT LT_SUB BY MATNR WERKS.
    DELETE ADJACENT DUPLICATES FROM LT_SUB COMPARING MATNR WERKS.

    IF LT_SUB[] IS NOT INITIAL.

      SELECT FROM MARA AS A INNER JOIN MAKT AS B ON A~MATNR = B~MATNR
                       INNER JOIN MARC AS C ON A~MATNR = C~MATNR
                       INNER JOIN MBEW AS D ON C~MATNR = D~MATNR AND C~WERKS = D~BWKEY
                       INNER JOIN T001W AS E ON D~BWKEY = E~WERKS
        FIELDS A~MATNR, B~MAKTX, E~NAME1, C~WERKS, C~XCHPF, C~BESKZ, C~SOBSL,
               C~MMSTA, D~BKLAS, D~BWTAR, C~BWTTY, D~VPRSV, A~MEINS
        FOR ALL ENTRIES IN @LT_SUB
        WHERE A~MATNR = @LT_SUB-MATNR
          AND C~WERKS = @LT_SUB-WERKS
          AND B~SPRAS = @SY-LANGU
        INTO TABLE @DATA(LT_MARA).

      FREE LT_SUB.

      LT_SUB[] = LT_ZTMM99130[].
      SORT LT_SUB BY LIFNR.
      DELETE ADJACENT DUPLICATES FROM LT_SUB COMPARING LIFNR.

      SELECT FROM LFA1
        FIELDS LIFNR, NAME1
        FOR ALL ENTRIES IN @LT_SUB
        WHERE LIFNR = @LT_SUB-LIFNR
        INTO TABLE @DATA(LT_BP).

      FREE LT_SUB.

    ENDIF.

    LOOP AT LT_ZTMM99130 INTO DATA(LS_ZTMM99130).
      MOVE-CORRESPONDING LS_ZTMM99130 TO LS_DISP.

      IF LS_ZTMM99130-ZSTATUS = 'S'.
        LS_DISP-STATUS = ICON_LED_GREEN.
      ELSE.
        LS_DISP-STATUS = ICON_LED_RED.
      ENDIF.

      READ TABLE LT_MARA INTO DATA(LS_MARA) WITH KEY MATNR = LS_DISP-MATNR WERKS = LS_DISP-WERKS.
      IF SY-SUBRC = 0.
        LS_DISP-MAKTX = LS_MARA-MAKTX.
        LS_DISP-NAME1_W = LS_MARA-NAME1.
      ENDIF.

      READ TABLE LT_BP INTO DATA(LS_BP) WITH KEY LIFNR = LS_DISP-LIFNR.
      IF SY-SUBRC = 0.
        LS_DISP-NAME1_L = LS_BP-NAME1.
      ENDIF.

      APPEND LS_DISP TO GT_DISP. CLEAR LS_DISP.

    ENDLOOP.

  ENDIF.

  IF GT_DISP[] IS NOT INITIAL.
    SORT GT_DISP BY ZSEQ.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_DATA_D
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM GET_DATA_D .


  _G_INIT : GT_DISP.

  DATA : LT_ZTMM99140 TYPE TABLE OF ZTMM99140,
         LS_DISP      TYPE TS_DISP.


  CASE 'X'.
    WHEN P_RD3A.    "전체

      SELECT FROM ZTMM99140
        FIELDS BUKRS, WERKS, LGORT, LIFNR, ZSEQ, MATNR, BESKZ, SOBSL, MMSTA, BKLAS, XCHPF, CHARG, BWTTY, BWTAR, VPRSV, BWART, ERFMG,
               MEINS, ZMEINS, INSMK, EXBWR, WAERS, MJAHR, MBLNR, ZEILE, BUDAT, MJAHR_R, MBLNR_R, ZEILE_R, ZSTATUS, ZMESSAGE, ERDAT,
               ERZET, ERNAM, AEDAT, AEZET, AENAM, ZDELE
        WHERE BUKRS = @P_BUKRS
          AND MATNR IN @S_MATNR
          AND WERKS IN @S_WERKS
          AND LIFNR IN @S_LIFNR
          AND LGORT IN @S_LGORT
          AND ERDAT IN @S_ERDAT
          AND ERNAM IN @S_ERNAM

        APPENDING CORRESPONDING FIELDS OF TABLE @LT_ZTMM99140.

    WHEN P_RD3B.    "성공

      SELECT FROM ZTMM99140
        FIELDS BUKRS, WERKS, LGORT, LIFNR, ZSEQ, MATNR, BESKZ, SOBSL, MMSTA, BKLAS, XCHPF, CHARG, BWTTY, BWTAR, VPRSV, BWART, ERFMG,
               MEINS, ZMEINS, INSMK, EXBWR, WAERS, MJAHR, MBLNR, ZEILE, BUDAT, MJAHR_R, MBLNR_R, ZEILE_R, ZSTATUS, ZMESSAGE, ERDAT,
               ERZET, ERNAM, AEDAT, AEZET, AENAM, ZDELE
        WHERE BUKRS = @P_BUKRS
          AND MATNR IN @S_MATNR
          AND WERKS IN @S_WERKS
          AND LGORT IN @S_LGORT
          AND LIFNR IN @S_LIFNR
          AND ERDAT IN @S_ERDAT
          AND ERNAM IN @S_ERNAM
          AND ZSTATUS = 'S'

        APPENDING CORRESPONDING FIELDS OF TABLE @LT_ZTMM99140.

    WHEN P_RD3C.    "실패

      SELECT FROM ZTMM99140
        FIELDS BUKRS, WERKS, LGORT, LIFNR, ZSEQ, MATNR, BESKZ, SOBSL, MMSTA, BKLAS, XCHPF, CHARG, BWTTY, BWTAR, VPRSV, BWART, ERFMG,
               MEINS, ZMEINS, INSMK, EXBWR, WAERS, MJAHR, MBLNR, ZEILE, BUDAT, MJAHR_R, MBLNR_R, ZEILE_R, ZSTATUS, ZMESSAGE, ERDAT,
               ERZET, ERNAM, AEDAT, AEZET, AENAM, ZDELE
        WHERE BUKRS = @P_BUKRS
          AND MATNR IN @S_MATNR
          AND WERKS IN @S_WERKS
          AND LIFNR IN @S_LIFNR
          AND LGORT IN @S_LGORT
          AND ERDAT IN @S_ERDAT
          AND ERNAM IN @S_ERNAM
          AND ZSTATUS NE 'S'

        APPENDING CORRESPONDING FIELDS OF TABLE @LT_ZTMM99140.

  ENDCASE.


  IF LT_ZTMM99140[] IS NOT INITIAL.

    DATA(LT_SUB) = LT_ZTMM99140[].
    SORT LT_SUB BY MATNR WERKS LGORT.
    DELETE ADJACENT DUPLICATES FROM LT_SUB COMPARING MATNR WERKS LGORT.

    IF LT_SUB[] IS NOT INITIAL.

      SELECT FROM MARA AS A INNER JOIN MAKT AS B ON A~MATNR = B~MATNR
                       INNER JOIN MARC AS C ON A~MATNR = C~MATNR
                       INNER JOIN MBEW AS D ON C~MATNR = D~MATNR AND C~WERKS = D~BWKEY
                       INNER JOIN T001W AS E ON D~BWKEY = E~WERKS
                       INNER JOIN T001L AS F ON E~WERKS = F~WERKS
        FIELDS A~MATNR, B~MAKTX, E~NAME1, F~LGORT, F~LGOBE, C~WERKS, C~XCHPF, C~BESKZ, C~SOBSL,
               C~MMSTA, D~BKLAS, D~BWTAR, C~BWTTY, D~VPRSV, A~MEINS
        FOR ALL ENTRIES IN @LT_SUB
        WHERE A~MATNR = @LT_SUB-MATNR
          AND C~WERKS = @LT_SUB-WERKS
          AND F~LGORT = @LT_SUB-LGORT
          AND B~SPRAS = @SY-LANGU
        INTO TABLE @DATA(LT_MARA).

      FREE LT_SUB.

      LT_SUB[] = LT_ZTMM99140[].
      SORT LT_SUB BY LIFNR.
      DELETE ADJACENT DUPLICATES FROM LT_SUB COMPARING LIFNR.

      SELECT FROM LFA1
        FIELDS LIFNR, NAME1
        FOR ALL ENTRIES IN @LT_SUB
        WHERE LIFNR = @LT_SUB-LIFNR
        INTO TABLE @DATA(LT_BP).

      FREE LT_SUB.

    ENDIF.

    LOOP AT LT_ZTMM99140 INTO DATA(LS_ZTMM99140).
      MOVE-CORRESPONDING LS_ZTMM99140 TO LS_DISP.

      IF LS_ZTMM99140-ZSTATUS = 'S'.
        LS_DISP-STATUS = ICON_LED_GREEN.
      ELSE.
        LS_DISP-STATUS = ICON_LED_RED.
      ENDIF.

      READ TABLE LT_MARA INTO DATA(LS_MARA) WITH KEY MATNR = LS_DISP-MATNR WERKS = LS_DISP-WERKS LGORT  = LS_DISP-LGORT.
      IF SY-SUBRC = 0.
        LS_DISP-MAKTX = LS_MARA-MAKTX.
        LS_DISP-NAME1_W = LS_MARA-NAME1.
        LS_DISP-LGOBE = LS_MARA-LGOBE.
      ENDIF.

      READ TABLE LT_BP INTO DATA(LS_BP) WITH KEY LIFNR = LS_DISP-LIFNR.
      IF SY-SUBRC = 0.
        LS_DISP-NAME1_L = LS_BP-NAME1.
      ENDIF.

      APPEND LS_DISP TO GT_DISP. CLEAR LS_DISP.

    ENDLOOP.

  ENDIF.

  IF GT_DISP[] IS NOT INITIAL.
    SORT GT_DISP BY ZSEQ.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_DATA_E
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM GET_DATA_E .

  _G_INIT : GT_DISP.

  DATA : LT_ZTMM99150 TYPE TABLE OF ZTMM99150,
         LS_DISP      TYPE TS_DISP.


  CASE 'X'.
    WHEN P_RD3A.    "전체

      SELECT FROM ZTMM99150
        FIELDS BUKRS, WERKS, LGORT, KUNNR, ZSEQ, MATNR, BESKZ, SOBSL, MMSTA, BKLAS, XCHPF, CHARG, BWTTY, BWTAR,
               VPRSV, BWART, ERFMG, MEINS, ZMEINS, INSMK, EXBWR, WAERS, VBELN, POSNR, MJAHR, MBLNR, ZEILE, BUDAT,
               MJAHR_R, MBLNR_R, ZEILE_R, ZSTATUS, ZMESSAGE, ERDAT, ERZET, ERNAM, AEDAT, AEZET, AENAM, ZDELE
        WHERE BUKRS = @P_BUKRS
          AND MATNR IN @S_MATNR
          AND WERKS IN @S_WERKS
          AND LGORT IN @S_LGORT
          AND KUNNR IN @S_KUNNR
          AND ERDAT IN @S_ERDAT
          AND ERNAM IN @S_ERNAM

        APPENDING CORRESPONDING FIELDS OF TABLE @LT_ZTMM99150.

    WHEN P_RD3B.    "성공

      SELECT FROM ZTMM99150
        FIELDS BUKRS, WERKS, LGORT, KUNNR, ZSEQ, MATNR, BESKZ, SOBSL, MMSTA, BKLAS, XCHPF, CHARG, BWTTY, BWTAR,
               VPRSV, BWART, ERFMG, MEINS, ZMEINS, INSMK, EXBWR, WAERS, VBELN, POSNR, MJAHR, MBLNR, ZEILE, BUDAT,
               MJAHR_R, MBLNR_R, ZEILE_R, ZSTATUS, ZMESSAGE, ERDAT, ERZET, ERNAM, AEDAT, AEZET, AENAM, ZDELE
        WHERE BUKRS = @P_BUKRS
          AND MATNR IN @S_MATNR
          AND WERKS IN @S_WERKS
          AND LGORT IN @S_LGORT
          AND KUNNR IN @S_KUNNR
          AND ERDAT IN @S_ERDAT
          AND ERNAM IN @S_ERNAM
          AND ZSTATUS = 'X'

        APPENDING CORRESPONDING FIELDS OF TABLE @LT_ZTMM99150.

    WHEN P_RD3C.    "실패

      SELECT FROM ZTMM99150
        FIELDS BUKRS, WERKS, LGORT, KUNNR, ZSEQ, MATNR, BESKZ, SOBSL, MMSTA, BKLAS, XCHPF, CHARG, BWTTY, BWTAR,
               VPRSV, BWART, ERFMG, MEINS, ZMEINS, INSMK, EXBWR, WAERS, VBELN, POSNR, MJAHR, MBLNR, ZEILE, BUDAT,
               MJAHR_R, MBLNR_R, ZEILE_R, ZSTATUS, ZMESSAGE, ERDAT, ERZET, ERNAM, AEDAT, AEZET, AENAM, ZDELE
        WHERE BUKRS = @P_BUKRS
          AND MATNR IN @S_MATNR
          AND WERKS IN @S_WERKS
          AND LGORT IN @S_LGORT
          AND KUNNR IN @S_KUNNR
          AND ERDAT IN @S_ERDAT
          AND ERNAM IN @S_ERNAM
          AND ZSTATUS NE 'X'

        APPENDING CORRESPONDING FIELDS OF TABLE @LT_ZTMM99150.

  ENDCASE.


  IF LT_ZTMM99150[] IS NOT INITIAL.

    DATA(LT_SUB) = LT_ZTMM99150[].
    SORT LT_SUB BY MATNR WERKS LGORT.
    DELETE ADJACENT DUPLICATES FROM LT_SUB COMPARING MATNR WERKS LGORT.

    IF LT_SUB[] IS NOT INITIAL.

      SELECT FROM MARA AS A INNER JOIN MAKT AS B ON A~MATNR = B~MATNR
                       INNER JOIN MARC AS C ON A~MATNR = C~MATNR
                       INNER JOIN MBEW AS D ON C~MATNR = D~MATNR AND C~WERKS = D~BWKEY
                       INNER JOIN T001W AS E ON D~BWKEY = E~WERKS
                       INNER JOIN T001L AS F ON E~WERKS = F~WERKS
        FIELDS A~MATNR, B~MAKTX, E~NAME1, F~LGORT, F~LGOBE, C~WERKS, C~XCHPF, C~BESKZ, C~SOBSL,
               C~MMSTA, D~BKLAS, D~BWTAR, C~BWTTY, D~VPRSV, A~MEINS
        FOR ALL ENTRIES IN @LT_SUB
        WHERE A~MATNR = @LT_SUB-MATNR
          AND C~WERKS = @LT_SUB-WERKS
          AND F~LGORT = @LT_SUB-LGORT
          AND B~SPRAS = @SY-LANGU
        INTO TABLE @DATA(LT_MARA).

    ENDIF.

    LOOP AT LT_ZTMM99150 INTO DATA(LS_ZTMM99150).
      MOVE-CORRESPONDING LS_ZTMM99150 TO LS_DISP.

      IF LS_ZTMM99150-ZSTATUS = 'S'.
        LS_DISP-STATUS = ICON_LED_GREEN.
      ELSE.
        LS_DISP-STATUS = ICON_LED_RED.
      ENDIF.

      READ TABLE LT_MARA INTO DATA(LS_MARA) WITH KEY MATNR = LS_DISP-MATNR WERKS = LS_DISP-WERKS LGORT  = LS_DISP-LGORT.
      IF SY-SUBRC = 0.
        LS_DISP-MAKTX = LS_MARA-MAKTX.
        LS_DISP-NAME1_W = LS_MARA-NAME1.
        LS_DISP-LGOBE = LS_MARA-LGOBE.
      ENDIF.

      APPEND LS_DISP TO GT_DISP. CLEAR LS_DISP.

    ENDLOOP.

  ENDIF.

  IF GT_DISP[] IS NOT INITIAL.
    SORT GT_DISP BY ZSEQ.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form CHECK_CLASS_BATCHDATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> <LS_BATCH>
*&---------------------------------------------------------------------*
FORM CHECK_CLASS_BATCHDATA USING IS_DATA TYPE TS_BATCH.

  DATA : LV_TYPE(4) TYPE C.

  "필수 입력 CHECK
  CASE IS_DATA-CLASS.

    WHEN GC_ZPP_KGC1.
      IF IS_DATA-EX_CHARG IS INITIAL.
        IF IS_DATA-LIFNR IS  INITIAL OR IS_DATA-HSDAT IS  INITIAL OR IS_DATA-VFDAT IS  INITIAL OR IS_DATA-LICHA IS  INITIAL.
          IS_DATA-STATUS = ICON_LED_RED.
          IS_DATA-ZMESSAGE = TEXT-M49.         "Fill UP 오류 : 공급업체,제조일,사용기한,제조LOT은 필수 입력입니다.
        ENDIF.
      ELSE.

        CALL FUNCTION 'NUMERIC_CHECK'
          EXPORTING
            STRING_IN = IS_DATA-EX_CHARG
          IMPORTING
            HTYPE     = LV_TYPE.

        IF STRLEN( IS_DATA-EX_CHARG ) = 10 AND LV_TYPE = GC_NUMC.
          IF IS_DATA-LIFNR IS  INITIAL OR IS_DATA-HSDAT IS  INITIAL OR IS_DATA-VFDAT IS  INITIAL OR IS_DATA-LICHA IS  INITIAL.
            IS_DATA-STATUS = ICON_LED_RED.
            IS_DATA-ZMESSAGE = TEXT-M49.         "Fill UP 오류 : 공급업체,제조일,사용기한,제조LOT은 필수 입력입니다.
          ENDIF.
        ELSE.
*          IF is_data-hsdat IS  INITIAL OR is_data-vfdat IS  INITIAL OR is_data-licha IS  INITIAL.
          IF IS_DATA-HSDAT IS  INITIAL OR IS_DATA-LICHA IS  INITIAL.
            IS_DATA-STATUS = ICON_LED_RED.
            IS_DATA-ZMESSAGE = TEXT-M59.         "Fill UP 오류 : 제조일,사용기한,제조LOT은 필수 입력입니다.
          ENDIF.
        ENDIF.
      ENDIF.

      "입력 속성 역가,함량,수탁 LOT CHECK
      IF IS_DATA-ZCOMP_RATIO IS NOT INITIAL OR IS_DATA-ZCOMP_NET IS NOT INITIAL OR IS_DATA-ZCOMP_DRY IS NOT INITIAL OR IS_DATA-ZLOTNO2 IS NOT INITIAL.
        IS_DATA-STATUS = ICON_LED_RED.
        IS_DATA-ZMESSAGE = TEXT-M48.         "Fill UP 오류 : 역가,함량,건조중량,수탁LOTNO는 배치 관리 속성이 아닙니다.
      ENDIF.

    WHEN GC_KGCBATCH2.

      IF IS_DATA-LIFNR IS  INITIAL OR IS_DATA-HSDAT IS  INITIAL OR IS_DATA-VFDAT IS  INITIAL.
        IS_DATA-STATUS = ICON_LED_RED.
        IS_DATA-ZMESSAGE = TEXT-M51.         "Fill UP 오류 : 공급업체,제조일,사용기한은 필수 입력입니다.
      ENDIF.

      "입력 속성 제조 LOT CHECK
      IF IS_DATA-LICHA IS NOT INITIAL.
        IS_DATA-STATUS = ICON_LED_RED.
        IS_DATA-ZMESSAGE = TEXT-M45.         "Fill UP 오류 : 제조LOT은 배치 관리 속성이 아닙니다.
      ENDIF.

    WHEN GC_KGCBATCH3.
      IF IS_DATA-HSDAT IS  INITIAL OR IS_DATA-VFDAT IS  INITIAL.
        IS_DATA-STATUS = ICON_LED_RED.
        IS_DATA-ZMESSAGE = TEXT-M52.         "Fill UP 오류 : 제조일,사용기한은 필수 입력입니다.
      ENDIF.

      "입력 속성 제조 LOT CHECK
      IF IS_DATA-LICHA IS NOT INITIAL.
        IS_DATA-STATUS = ICON_LED_RED.
        IS_DATA-ZMESSAGE = TEXT-M45.         "Fill UP 오류 : 제조LOT은 배치 관리 속성이 아닙니다.
      ENDIF.

    WHEN GC_KGCBATCH4.
      IF IS_DATA-LIFNR IS  INITIAL OR IS_DATA-HSDAT IS  INITIAL OR IS_DATA-VFDAT IS  INITIAL OR IS_DATA-LICHA IS  INITIAL.
        IS_DATA-STATUS = ICON_LED_RED.
        IS_DATA-ZMESSAGE = TEXT-M49.         "Fill UP 오류 : 공급업체,제조일,사용기한,제조LOT은 필수 입력입니다.
      ENDIF.

    WHEN GC_YJPBATCH1.
      IF IS_DATA-LIFNR IS  INITIAL OR IS_DATA-HSDAT IS  INITIAL OR IS_DATA-VFDAT IS  INITIAL OR IS_DATA-LICHA IS  INITIAL OR IS_DATA-ZMM_MAKER IS  INITIAL.
        IS_DATA-STATUS = ICON_LED_RED.
        IS_DATA-ZMESSAGE = TEXT-M53.         "Fill UP 오류 : 공급업체,제조일,사용기한,제조LOT,제조처는 필수 입력입니다.
      ENDIF.

    WHEN GC_YJPBATCH2.
      IF IS_DATA-LIFNR IS  INITIAL OR IS_DATA-HSDAT IS  INITIAL OR IS_DATA-VFDAT IS  INITIAL OR IS_DATA-LICHA IS INITIAL.
        IS_DATA-STATUS = ICON_LED_RED.
        IS_DATA-ZMESSAGE = TEXT-M49.         "Fill UP 오류 : 공급업체,제조일,사용기한,제조LOT은 필수 입력입니다.
      ENDIF.

    WHEN GC_YJPBATCH3 OR GC_YJPBATCH3.
      IF IS_DATA-LIFNR IS  INITIAL OR  IS_DATA-VFDAT IS  INITIAL OR IS_DATA-LICHA IS INITIAL OR IS_DATA-ZMM_MAKER  IS  INITIAL.
        IS_DATA-STATUS = ICON_LED_RED.
        IS_DATA-ZMESSAGE = TEXT-M54.         "Fill UP 오류 : 공급업체,사용기한,제조LOT,제조처는 필수 입력입니다.
      ENDIF.

  ENDCASE.

  "입력 속성 수탁 LOT CHECK
  IF IS_DATA-CLASS <> GC_ZPP_YJP AND IS_DATA-ZLOTNO2 IS NOT INITIAL..
    IS_DATA-STATUS = ICON_LED_RED.
    IS_DATA-ZMESSAGE = TEXT-M55.         "Fill UP 오류 : 제조LOT은 배치 관리 속성이 아닙니다.
  ENDIF.

  "입력 속성 역가,수분,함량,수탁 LOT CHECK
  IF IS_DATA-CLASS = GC_KGCBATCH2 OR IS_DATA-CLASS = GC_KGCBATCH3 OR IS_DATA-CLASS = GC_KGCBATCH4.
    IF IS_DATA-ZCOMP_RATIO IS NOT INITIAL OR IS_DATA-ZCOMP_MOIST IS NOT INITIAL OR IS_DATA-ZCOMP_NET IS NOT INITIAL OR
       IS_DATA-ZCOMP_DRY IS NOT INITIAL OR IS_DATA-ZLOTNO2 IS NOT INITIAL.
      IS_DATA-STATUS = ICON_LED_RED.
      IS_DATA-ZMESSAGE = TEXT-M46.         "Fill UP 오류 : 역가,수분비율,함량,건조중량,수탁LOTNO는 배치 관리 속성이 아닙니다.
    ENDIF.
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

  CASE 'X'.
    WHEN P_RD1A.  " 배치 Migration
      IF GT_BATCH[] IS NOT INITIAL.
        PERFORM PROCESSING_DATA_BATCH.
      ELSE.
        MESSAGE S000 WITH TEXT-M17 DISPLAY LIKE 'E'.  "업로드 파일을 열고 있거나, 입력 형식이 올바른지 확인하세요.
        LEAVE LIST-PROCESSING.
      ENDIF.

    WHEN OTHERS.  " 재고 Migration
      IF GT_DISP[] IS NOT INITIAL.
        READ TABLE GT_DISP TRANSPORTING NO FIELDS WITH KEY STATUS = ICON_LED_RED.
        IF SY-SUBRC NE 0.
          PERFORM PROCESSING_DATA_STOCK.
        ELSE.
          EXIT.
        ENDIF.
      ELSE.
        MESSAGE S000 WITH TEXT-M17 DISPLAY LIKE 'E'.  "업로드 파일을 열고 있거나, 입력 형식이 올바른지 확인하세요.
        LEAVE LIST-PROCESSING.
      ENDIF.

  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form PROCESSING_DATA_STOCK
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM PROCESSING_DATA_STOCK .

  DATA : LT_MARA TYPE TABLE OF TS_MARA.

  DATA : LV_ZSEQ TYPE ZTMM99120-ZSEQ,   LV_SEQ  TYPE ZTMM99120-ZSEQ.

  DATA(LT_DISP) = GT_DISP[].
  SORT LT_DISP BY MATNR WERKS.
  DELETE ADJACENT DUPLICATES FROM LT_DISP COMPARING MATNR WERKS.

  IF LT_DISP[] IS NOT INITIAL.
    _G_INIT : LT_MARA.

*- 데이터 사전 집계 (1)
    IF P_RD1C NE 'X'.
      IF P_RD3D NE 'X'.
        SELECT FROM MARA AS A INNER JOIN MAKT AS B ON A~MATNR = B~MATNR
                              INNER JOIN MARC AS C ON A~MATNR = C~MATNR
                              INNER JOIN MBEW AS D ON C~MATNR = D~MATNR AND C~WERKS = D~BWKEY
                              INNER JOIN T001W AS E ON D~BWKEY = E~WERKS
*                            INNER JOIN T001L AS F ON E~WERKS = F~WERKS
          FIELDS A~MATNR, A~MEINS, B~MAKTX, C~WERKS, C~XCHPF, C~BESKZ, C~SOBSL, C~MMSTA, C~BWTTY,
                 D~BKLAS, D~BWTAR,  D~VPRSV, E~NAME1
*        , F~LGORT, F~LGOBE
          FOR ALL ENTRIES IN @LT_DISP
          WHERE A~MATNR = @LT_DISP-MATNR
            AND C~WERKS = @LT_DISP-WERKS
*          AND F~LGORT = @LT_DISP-LGORT
            AND B~SPRAS = @SY-LANGU
          INTO CORRESPONDING FIELDS OF TABLE @LT_MARA.
      ELSE.
        SELECT FROM MARA AS A INNER JOIN MAKT AS B ON A~MATNR = B~MATNR
                              INNER JOIN MARC AS C ON A~MATNR = C~MATNR
                              INNER JOIN T001W AS E ON C~WERKS = E~WERKS
*                            INNER JOIN T001L AS F ON E~WERKS = F~WERKS
          FIELDS A~MATNR, A~MEINS, B~MAKTX, C~WERKS, C~XCHPF, C~BESKZ, C~SOBSL, C~MMSTA, C~BWTTY,
                  E~NAME1
*        , F~LGORT, F~LGOBE
          FOR ALL ENTRIES IN @LT_DISP
          WHERE A~MATNR = @LT_DISP-MATNR
            AND C~WERKS = @LT_DISP-WERKS
*          AND F~LGORT = @LT_DISP-LGORT
            AND B~SPRAS = @SY-LANGU
          INTO CORRESPONDING FIELDS OF TABLE @LT_MARA.
      ENDIF.


    ELSE. " P_RD1C = 사급 - 저장위치X
      FREE LT_DISP.
      LT_DISP[] = GT_DISP[].
      SORT LT_DISP BY MATNR WERKS.
      DELETE ADJACENT DUPLICATES FROM LT_DISP COMPARING MATNR WERKS.

      IF P_RD3D NE 'X'.
        SELECT FROM MARA AS A INNER JOIN MAKT AS B ON A~MATNR = B~MATNR
                              INNER JOIN MARC AS C ON A~MATNR = C~MATNR
                              INNER JOIN MBEW AS D ON C~MATNR = D~MATNR AND C~WERKS = D~BWKEY
                              INNER JOIN T001W AS E ON D~BWKEY = E~WERKS
          FIELDS A~MATNR, A~MEINS, B~MAKTX, C~WERKS, C~XCHPF, C~BESKZ, C~SOBSL, C~MMSTA, C~BWTTY,
                 D~BKLAS, D~BWTAR,  D~VPRSV, E~NAME1
          FOR ALL ENTRIES IN @LT_DISP
          WHERE A~MATNR = @LT_DISP-MATNR
            AND C~WERKS = @LT_DISP-WERKS
            AND B~SPRAS = @SY-LANGU
          INTO CORRESPONDING FIELDS OF TABLE @LT_MARA.

      ELSE.
        SELECT FROM MARA AS A INNER JOIN MAKT AS B ON A~MATNR = B~MATNR
                              INNER JOIN MARC AS C ON A~MATNR = C~MATNR
                              INNER JOIN T001W AS E ON C~WERKS = E~WERKS
          FIELDS A~MATNR, A~MEINS, B~MAKTX, C~WERKS, C~XCHPF, C~BESKZ, C~SOBSL, C~MMSTA, C~BWTTY,
                  E~NAME1
          FOR ALL ENTRIES IN @LT_DISP
          WHERE A~MATNR = @LT_DISP-MATNR
            AND C~WERKS = @LT_DISP-WERKS
            AND B~SPRAS = @SY-LANGU
          INTO CORRESPONDING FIELDS OF TABLE @LT_MARA.

      ENDIF.
    ENDIF.

*- ZSEQ 채번
    PERFORM SELECT_LAST_SEQ CHANGING LV_ZSEQ.

*- 공급업체 체크 : 사급/위탁
    FREE LT_DISP.
    LT_DISP[] = GT_DISP[].
    SORT LT_DISP BY LIFNR.
    DELETE ADJACENT DUPLICATES FROM LT_DISP COMPARING LIFNR.

    SELECT FROM LFA1      FIELDS LIFNR, NAME1
      FOR ALL ENTRIES IN @LT_DISP
      WHERE LIFNR = @LT_DISP-LIFNR
      INTO TABLE @DATA(LT_LFA1).

*- 배치 체크
    FREE LT_DISP.
    LT_DISP[] = GT_DISP[].
    SORT LT_DISP BY MATNR CHARG.
    DELETE ADJACENT DUPLICATES FROM LT_DISP COMPARING MATNR CHARG.

    SELECT FROM MCH1      FIELDS MATNR, CHARG
      FOR ALL ENTRIES IN @LT_DISP
      WHERE MATNR = @LT_DISP-MATNR
        AND CHARG = @LT_DISP-CHARG
    INTO TABLE @DATA(LT_MCH1).


    FREE LT_DISP.
    LT_DISP[] = GT_DISP[].
    SORT LT_DISP BY MATNR WERKS CHARG.
    DELETE ADJACENT DUPLICATES FROM LT_DISP COMPARING MATNR WERKS CHARG.

    SELECT FROM MCHA      FIELDS MATNR, WERKS, CHARG, BWTAR
      FOR ALL ENTRIES IN @LT_DISP
      WHERE MATNR = @LT_DISP-MATNR
        AND WERKS = @LT_DISP-WERKS
        AND CHARG = @LT_DISP-CHARG
      INTO TABLE @DATA(LT_MCHA).

    FREE LT_DISP.

  ENDIF.

  LV_SEQ = GC_00000000.

  "Data
  LOOP AT GT_DISP ASSIGNING FIELD-SYMBOL(<LS_DISP>).

    CLEAR : <LS_DISP>-STATUS, <LS_DISP>-ZMESSAGE.

    "순번(ZSEQ) = LV_ZSEQ + 1.
    _G_ADD_1 LV_ZSEQ.
    <LS_DISP>-ZSEQ = LV_ZSEQ.

    _G_ADD_1 LV_SEQ.
    <LS_DISP>-INDEX_C = LV_SEQ.

    <LS_DISP>-ERNAM = SY-UNAME.
    <LS_DISP>-BUKRS = P_BUKRS.


    READ TABLE LT_MARA INTO DATA(LS_MARA) WITH KEY MATNR = <LS_DISP>-MATNR
                                                   WERKS = <LS_DISP>-WERKS.

    IF SY-SUBRC = 0.
      <LS_DISP>-MAKTX = LS_MARA-MAKTX.
      <LS_DISP>-NAME1_W = LS_MARA-NAME1.
      <LS_DISP>-BESKZ = LS_MARA-BESKZ.
      <LS_DISP>-SOBSL = LS_MARA-SOBSL.
      <LS_DISP>-MMSTA = LS_MARA-MMSTA.
      <LS_DISP>-BKLAS = LS_MARA-BKLAS.
      <LS_DISP>-XCHPF = LS_MARA-XCHPF.
      <LS_DISP>-BWTTY = LS_MARA-BWTTY.
      <LS_DISP>-MEINS = LS_MARA-MEINS.
      <LS_DISP>-LGOBE = LS_MARA-LGOBE.
    ELSE.
      <LS_DISP>-STATUS = ICON_LED_RED.
      <LS_DISP>-ZMESSAGE = TEXT-M27.     "자재코드 오류 : 재고 이관 대상 마스터가 없습니다
      CONTINUE.
    ENDIF.

    READ TABLE LT_MARA INTO DATA(LS_MARA_V) WITH KEY MATNR = <LS_DISP>-MATNR
                                                     WERKS = <LS_DISP>-WERKS
                                                     BWTAR = <LS_DISP>-BWTAR.
    IF SY-SUBRC = 0.  "가격 지시자
      <LS_DISP>-VPRSV = LS_MARA_V-VPRSV.
    ENDIF.

    IF <LS_DISP>-LIFNR IS NOT INITIAL.
      READ TABLE LT_LFA1 INTO DATA(LS_LFA1) WITH KEY LIFNR = <LS_DISP>-LIFNR.

      IF SY-SUBRC = 0.
        <LS_DISP>-NAME1_L = LS_LFA1-NAME1.
      ELSE.
        <LS_DISP>-STATUS = ICON_LED_RED.
        <LS_DISP>-ZMESSAGE = TEXT-M15.   "BP에 관리되지 않는 공급업체입니다.
      ENDIF.
    ENDIF.

    IF <LS_DISP>-MMSTA EQ '20' OR <LS_DISP>-MMSTA EQ '30'.
    ELSE.
      <LS_DISP>-STATUS = ICON_LED_RED.
      <LS_DISP>-ZMESSAGE = TEXT-M28.     "자재상태 오류 : 자재 상태가 가용하지 않습니다
      CONTINUE.
    ENDIF.

    IF <LS_DISP>-XCHPF = 'X'.
      IF <LS_DISP>-CHARG IS INITIAL.
        <LS_DISP>-STATUS = ICON_LED_RED.
        <LS_DISP>-ZMESSAGE = TEXT-M29.     "Fill UP 오류 : 배치번호가 누락되었습니다
        CONTINUE.
      ELSE.
        READ TABLE LT_MCH1 INTO DATA(LS_MCH1) WITH KEY MATNR = <LS_DISP>-MATNR
                                                       CHARG = <LS_DISP>-CHARG.
        IF SY-SUBRC NE 0.
          <LS_DISP>-STATUS = ICON_LED_RED.
          <LS_DISP>-ZMESSAGE = TEXT-M30.   "Fill UP 오류 : 입력된 배치는 존재하지 않습니다
          CONTINUE.
        ENDIF.
      ENDIF.
    ENDIF.

    IF <LS_DISP>-BWTTY = 'Y' AND <LS_DISP>-BWTAR IS INITIAL.
      <LS_DISP>-STATUS = ICON_LED_RED.
      <LS_DISP>-ZMESSAGE = TEXT-M31.      "Fill UP 오류 : 평가유형이 누락되었습니다
      CONTINUE.
    ELSEIF  <LS_DISP>-BWTTY = 'Y' AND <LS_DISP>-BWTAR IS NOT INITIAL.

      READ TABLE LT_MARA INTO DATA(LS_MARA_3) WITH KEY MATNR = <LS_DISP>-MATNR
                                                       WERKS = <LS_DISP>-WERKS
                                                       BWTAR = <LS_DISP>-BWTAR.

      IF SY-SUBRC NE 0.
        <LS_DISP>-STATUS = ICON_LED_RED.
        <LS_DISP>-ZMESSAGE = TEXT-M32.     "Fill UP 오류 : 입력된 평가유형은 존재하지 않습니다
        CONTINUE.
      ENDIF.

      IF <LS_DISP>-CHARG IS NOT INITIAL.
        READ TABLE LT_MCHA INTO DATA(LS_MCHA) WITH KEY MATNR = <LS_DISP>-MATNR
                                                       WERKS = <LS_DISP>-WERKS
                                                       CHARG = <LS_DISP>-CHARG.
        IF SY-SUBRC = 0.
          IF LS_MCHA-BWTAR NE <LS_DISP>-BWTAR.
            <LS_DISP>-STATUS = ICON_LED_RED.
            <LS_DISP>-ZMESSAGE = TEXT-M38.     "Fill UP 오류 : 입력된 평가유형은 배치 평가유형과 다릅니다.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.

    "금액,수량 관련 Validation Check.
    PERFORM CHECK_VALIDATION CHANGING <LS_DISP>.

  ENDLOOP.


  PERFORM KNA1_CHECK.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CHECK_VALIDATION
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- <LS_DISP>
*&---------------------------------------------------------------------*
FORM CHECK_VALIDATION CHANGING CS_DATA TYPE TS_DISP.


  IF CS_DATA-VPRSV = 'V' AND CS_DATA-EXBWR = 0.

*    CS_DATA-STATUS = ICON_LED_RED.
*    CS_DATA-ZMESSAGE = TEXT-M33.       "Fill UP 오류 : 평가금액이 누락되었습니다
  ELSEIF CS_DATA-VPRSV = 'V' AND CS_DATA-EXBWR IS NOT INITIAL.
    CS_DATA-WAERS = GV_WAERS.  "20221018(+)
  ELSEIF CS_DATA-VPRSV = 'S' AND CS_DATA-EXBWR IS NOT INITIAL.
    CS_DATA-STATUS = ICON_LED_RED.
    CS_DATA-ZMESSAGE = TEXT-M39.       "Fill UP 오류 : 표준 가격을 사용하는 자재는 평가금액입력 제외입니다.
  ENDIF.

  IF CS_DATA-ERFMG <= 0.
    CS_DATA-STATUS = ICON_LED_RED.
    CS_DATA-ZMESSAGE = TEXT-M40.       "Fill UP 오류 : 수량이 입력되지 않았습니다.
  ENDIF.

  IF CS_DATA-MEINS NE CS_DATA-ZMEINS.
    CS_DATA-STATUS = ICON_LED_RED.
    CS_DATA-ZMESSAGE = TEXT-M34.        "Fill UP 오류 : 입력단위와 재고 관리 단위가 다릅니다
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form KNA1_CHECK
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM KNA1_CHECK .

  IF P_RD1E = 'X'.   "고객 Migration
    DATA(LT_DISP) = GT_DISP[].
    IF LT_DISP[] IS NOT INITIAL.
      SORT LT_DISP BY KUNNR.
      DELETE ADJACENT DUPLICATES FROM LT_DISP COMPARING KUNNR.
      SELECT FROM KNA1      FIELDS KUNNR
        FOR ALL ENTRIES IN @LT_DISP
        WHERE KUNNR = @LT_DISP-KUNNR
        INTO TABLE @DATA(LT_KNA1).
      FREE LT_DISP.

      LOOP AT GT_DISP ASSIGNING FIELD-SYMBOL(<LS_DISP>).

        READ TABLE LT_KNA1 TRANSPORTING NO FIELDS WITH KEY KUNNR = <LS_DISP>-KUNNR.
        IF SY-SUBRC NE 0.
          <LS_DISP>-STATUS = ICON_LED_RED.
          <LS_DISP>-ZMESSAGE = TEXT-M16.  "BP에 관리되지 않는 고객입니다.
        ENDIF.

      ENDLOOP.
    ENDIF.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SELECT_LAST_SEQ
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- LV_ZSEQ
*&---------------------------------------------------------------------*
FORM SELECT_LAST_SEQ CHANGING CV_ZSEQ.

  CASE 'X'.
    WHEN P_RD1B.  "창고

      SELECT SINGLE FROM ZTMM99120
        FIELDS MAX( ZSEQ )
        WHERE BUKRS = @P_BUKRS
          AND ERNAM = @SY-UNAME
          AND WERKS = @P_WERKS
*          AND LGORT = @P_LGORT
        INTO (@CV_ZSEQ).

      IF CV_ZSEQ IS INITIAL.
        CV_ZSEQ = GC_00000000.
      ENDIF.

    WHEN P_RD1C.  "사급

      SELECT SINGLE FROM ZTMM99130
       FIELDS MAX( ZSEQ )
        WHERE BUKRS = @P_BUKRS
          AND ERNAM = @SY-UNAME
          AND WERKS = @P_WERKS
*          AND LIFNR = @P_LIFNR
        INTO (@CV_ZSEQ).

      IF CV_ZSEQ IS INITIAL.
        CV_ZSEQ = GC_00000000.
      ENDIF.

    WHEN P_RD1D.  "위탁

      SELECT SINGLE FROM ZTMM99140
        FIELDS MAX( ZSEQ )
        WHERE BUKRS = @P_BUKRS
        AND ERNAM = @SY-UNAME
        AND WERKS = @P_WERKS
*        AND LGORT = @P_LGORT
        INTO (@CV_ZSEQ).

      IF CV_ZSEQ IS INITIAL.
        CV_ZSEQ = GC_00000000.
      ENDIF.

    WHEN P_RD1E.  "고객
      SELECT SINGLE FROM ZTMM99150
        FIELDS MAX( ZSEQ )
        WHERE BUKRS = @P_BUKRS
          AND ERNAM = @SY-UNAME
          AND WERKS = @P_WERKS
*          AND LGORT = @P_LGORT
*          AND KUNNR = @P_KUNNR
        INTO (@CV_ZSEQ).

      IF CV_ZSEQ IS INITIAL.
        CV_ZSEQ = GC_00000000.
      ENDIF.

  ENDCASE.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form DOWNLOAD_EXCEL_SMW0
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM DOWNLOAD_EXCEL_SMW0 .
*------------------------------
* SMW0 관련
*------------------------------
  DATA : LV_FILENAME  TYPE STRING,
         LV_OBJID(20).

  CONSTANTS: LC_ZOMM9910_01(11)    VALUE 'ZOMM9910_01',
             LC_ZOMM9910_02(11)    VALUE 'ZOMM9910_02',
             LC_ZOMM9910_03(11)    VALUE 'ZOMM9910_03',
             LC_ZOMM9910_04(11)    VALUE 'ZOMM9910_04',
             LC_ZOMM9910_05(11)    VALUE 'ZOMM9910_05',
             LC_ZOMM9910_EN_01(14) VALUE 'ZOMM9910_EN_01',
             LC_ZOMM9910_EN_02(14) VALUE 'ZOMM9910_EN_02',
             LC_ZOMM9910_EN_03(14) VALUE 'ZOMM9910_EN_03',
             LC_ZOMM9910_EN_04(14) VALUE 'ZOMM9910_EN_04',
             LC_ZOMM9910_EN_05(14) VALUE 'ZOMM9910_EN_05'.

  CASE 'X'.
    WHEN P_RD1A.
      IF SY-LANGU = '3'.
        LV_OBJID = LC_ZOMM9910_01.
      ELSE.
        LV_OBJID = LC_ZOMM9910_EN_01.
      ENDIF.

      CONCATENATE TEXT-T01 '_' SY-DATLO  '_' SY-UNAME INTO LV_FILENAME.
      ZCL_CN_ABAP_UTIL=>EXCL_SMW0_DOWNLOAD( EXPORTING IV_OBJID = LV_OBJID
                                                      IV_FILENAME = LV_FILENAME ).  "Iv Filename 생략시 현재 프로그램 id
    WHEN P_RD1B.
      IF SY-LANGU = '3'.
        LV_OBJID = LC_ZOMM9910_02.
      ELSE.
        LV_OBJID = LC_ZOMM9910_EN_02.
      ENDIF.

      CONCATENATE TEXT-T02 '_' SY-DATLO  '_' SY-UNAME INTO LV_FILENAME.
      ZCL_CN_ABAP_UTIL=>EXCL_SMW0_DOWNLOAD( EXPORTING IV_OBJID = LV_OBJID
                                                      IV_FILENAME = LV_FILENAME  ).  "Iv Filename 생략시 현재 프로그램 id
    WHEN P_RD1C.
      IF SY-LANGU = '3'.
        LV_OBJID = LC_ZOMM9910_03.
      ELSE.
        LV_OBJID = LC_ZOMM9910_EN_03.
      ENDIF.

      CONCATENATE TEXT-T03 '_' SY-DATLO  '_' SY-UNAME INTO LV_FILENAME.
      ZCL_CN_ABAP_UTIL=>EXCL_SMW0_DOWNLOAD( EXPORTING IV_OBJID = LV_OBJID
                                                      IV_FILENAME = LV_FILENAME ).  "Iv Filename 생략시 현재 프로그램 id
    WHEN P_RD1D.
      IF SY-LANGU = '3'.
        LV_OBJID = LC_ZOMM9910_04.
      ELSE.
        LV_OBJID = LC_ZOMM9910_EN_04.
      ENDIF.

      CONCATENATE TEXT-T04 '_' SY-DATLO  '_' SY-UNAME INTO LV_FILENAME.
      ZCL_CN_ABAP_UTIL=>EXCL_SMW0_DOWNLOAD( EXPORTING IV_OBJID = LV_OBJID
                                                      IV_FILENAME = LV_FILENAME  ).  "Iv Filename 생략시 현재 프로그램 id
    WHEN P_RD1E.
      IF SY-LANGU = '3'.
        LV_OBJID = LC_ZOMM9910_05.
      ELSE.
        LV_OBJID = LC_ZOMM9910_EN_05.
      ENDIF.

      CONCATENATE TEXT-T05 '_' SY-DATLO  '_' SY-UNAME INTO LV_FILENAME.
      ZCL_CN_ABAP_UTIL=>EXCL_SMW0_DOWNLOAD( EXPORTING IV_OBJID = LV_OBJID
                                                      IV_FILENAME = LV_FILENAME  ).  "Iv Filename 생략시 현재 프로그램 id
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

    CASE 'X'.
      WHEN P_RD2A.

        " 조회구분 HIDE
        CASE SCREEN-GROUP3.
          WHEN 'BLK'.
            IF SCREEN-NAME = '%BB05045_BLOCK_1000'.     SCREEN-INPUT = 0.            SCREEN-INVISIBLE = 1.             ENDIF.
        ENDCASE.

        CASE SCREEN-GROUP1.
          WHEN 'RD3'.             SCREEN-INPUT = 0.            SCREEN-INVISIBLE = 1.
          WHEN 'SMA'.             SCREEN-INPUT = 0.            SCREEN-INVISIBLE = 1.
          WHEN 'SCH'.             SCREEN-INPUT = 0.            SCREEN-INVISIBLE = 1.
          WHEN 'SWE'.             SCREEN-INPUT = 0.            SCREEN-INVISIBLE = 1.
          WHEN 'SLG'.             SCREEN-INPUT = 0.            SCREEN-INVISIBLE = 1.
          WHEN 'SLI'.             SCREEN-INPUT = 0.            SCREEN-INVISIBLE = 1.
          WHEN 'SKU'.             SCREEN-INPUT = 0.            SCREEN-INVISIBLE = 1.
          WHEN 'SER'.             SCREEN-INPUT = 0.            SCREEN-INVISIBLE = 1.
          WHEN 'SER'.             SCREEN-INPUT = 0.            SCREEN-INVISIBLE = 1.
          WHEN 'SER'.             SCREEN-INPUT = 0.            SCREEN-INVISIBLE = 1.
        ENDCASE.

        CASE 'X'.
          WHEN P_RD1A.  "배치 Migration
            CASE SCREEN-GROUP1.
              WHEN 'PWE'.            SCREEN-INPUT = 0.             SCREEN-INVISIBLE = 1.
              WHEN 'PLG'.            SCREEN-INPUT = 0.             SCREEN-INVISIBLE = 1.
              WHEN 'PLI'.            SCREEN-INPUT = 0.             SCREEN-INVISIBLE = 1.
              WHEN 'PBU'.            SCREEN-INPUT = 0.             SCREEN-INVISIBLE = 1.
              WHEN 'PKU'.            SCREEN-INPUT = 0.             SCREEN-INVISIBLE = 1.
            ENDCASE.
*          WHEN P_RD1B.  "창고 재고 Migration
*            CASE SCREEN-GROUP1.
*              WHEN 'PLI'.            SCREEN-INPUT = 0.            SCREEN-INVISIBLE = 1.
*              WHEN 'PKU'.            SCREEN-INPUT = 0.            SCREEN-INVISIBLE = 1.
*              WHEN 'PLG'.            SCREEN-INPUT = 0.            SCREEN-INVISIBLE = 1.
*            ENDCASE.
*          WHEN P_RD1C.  "사급 재고 Migration
*            CASE SCREEN-GROUP1.
*              WHEN 'PLG'.            SCREEN-INPUT = 0.            SCREEN-INVISIBLE = 1.
*              WHEN 'PKU'.            SCREEN-INPUT = 0.            SCREEN-INVISIBLE = 1.
*            ENDCASE.
*          WHEN P_RD1D.  "위탁 재고 Migration
*            CASE SCREEN-GROUP1.
*              WHEN 'PKU'.            SCREEN-INPUT = 0.            SCREEN-INVISIBLE = 1.
*            ENDCASE.
*          WHEN P_RD1E.  "고객 재고 Migration
*            CASE SCREEN-GROUP1.
*              WHEN 'PLI'.            SCREEN-INPUT = 0.            SCREEN-INVISIBLE = 1.
*            ENDCASE.
        ENDCASE.


      WHEN P_RD2B.

        CASE SCREEN-GROUP1.
          WHEN 'PWE'.             SCREEN-INPUT = 0.            SCREEN-INVISIBLE = 1.
          WHEN 'PLG'.             SCREEN-INPUT = 0.            SCREEN-INVISIBLE = 1.
          WHEN 'PLI'.             SCREEN-INPUT = 0.            SCREEN-INVISIBLE = 1.
          WHEN 'PKU'.             SCREEN-INPUT = 0.            SCREEN-INVISIBLE = 1.
          WHEN 'PBU'.             SCREEN-INPUT = 0.            SCREEN-INVISIBLE = 1.
          WHEN 'PFI'.             SCREEN-INPUT = 0.            SCREEN-INVISIBLE = 1.
          WHEN 'RD4'.            SCREEN-INPUT = 0.            SCREEN-INVISIBLE = 1. "수량관리자재 upload
        ENDCASE.

        CASE 'X'.
          WHEN P_RD1A.  "배치 Migration
            CASE SCREEN-GROUP1.
              WHEN 'SWE'.            SCREEN-INPUT = 0.             SCREEN-INVISIBLE = 1.
              WHEN 'SLG'.            SCREEN-INPUT = 0.             SCREEN-INVISIBLE = 1.
              WHEN 'SLI'.            SCREEN-INPUT = 0.             SCREEN-INVISIBLE = 1.
              WHEN 'SKU'.            SCREEN-INPUT = 0.             SCREEN-INVISIBLE = 1.
            ENDCASE.
          WHEN P_RD1B.  "창고 재고 Migration
            CASE SCREEN-GROUP1.
              WHEN 'SLI'.            SCREEN-INPUT = 0.            SCREEN-INVISIBLE = 1.
              WHEN 'SKU'.            SCREEN-INPUT = 0.            SCREEN-INVISIBLE = 1.
              WHEN 'SCH'.            SCREEN-INPUT = 0.            SCREEN-INVISIBLE = 1.
            ENDCASE.
          WHEN P_RD1C.  "사급 재고 Migration
            CASE SCREEN-GROUP1.
              WHEN 'SLG'.            SCREEN-INPUT = 0.            SCREEN-INVISIBLE = 1.
              WHEN 'SKU'.            SCREEN-INPUT = 0.            SCREEN-INVISIBLE = 1.
              WHEN 'SCH'.            SCREEN-INPUT = 0.            SCREEN-INVISIBLE = 1.
            ENDCASE.
          WHEN P_RD1D.  "위탁 재고 Migration
            CASE SCREEN-GROUP1.
              WHEN 'SKU'.            SCREEN-INPUT = 0.            SCREEN-INVISIBLE = 1.
              WHEN 'SCH'.            SCREEN-INPUT = 0.            SCREEN-INVISIBLE = 1.
            ENDCASE.
          WHEN P_RD1E.  "고객 재고 Migration
            CASE SCREEN-GROUP1.
              WHEN 'SLI'.            SCREEN-INPUT = 0.            SCREEN-INVISIBLE = 1.
              WHEN 'SCH'.            SCREEN-INPUT = 0.            SCREEN-INVISIBLE = 1.
            ENDCASE.
        ENDCASE.


    ENDCASE.

    MODIFY SCREEN.

  ENDLOOP.


ENDFORM.
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
