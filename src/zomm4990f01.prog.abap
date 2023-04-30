*&---------------------------------------------------------------------*
*& Include          ZOMM4990F01
*&---------------------------------------------------------------------*
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
FORM FILE_OPEN  CHANGING CV_FILE.

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

  CONSTANTS: LC_ZOMM4990_01(11)    VALUE 'ZOMM4990_01',
             LC_ZOMM4990_02(11)    VALUE 'ZOMM4990_02',
             LC_ZOMM4990_03(11)    VALUE 'ZOMM4990_03',
             LC_ZOMM4990_04(11)    VALUE 'ZOMM4990_04',
             LC_ZOMM4990_05(11)    VALUE 'ZOMM4990_05',
             LC_ZOMM4990_EN_01(14) VALUE 'ZOMM4990_EN_01',
             LC_ZOMM4990_EN_02(14) VALUE 'ZOMM4990_EN_02',
             LC_ZOMM4990_EN_03(14) VALUE 'ZOMM4990_EN_03',
             LC_ZOMM4990_EN_04(14) VALUE 'ZOMM4990_EN_04',
             LC_ZOMM4990_EN_05(14) VALUE 'ZOMM4990_EN_05'.

* U3 접속언어로 엑셀 포맷 분기
  CASE 'X'.
    WHEN P_RD1A.
      IF SY-LANGU = '3'.
        LV_OBJID = LC_ZOMM4990_01.
      ELSE.
        LV_OBJID = LC_ZOMM4990_EN_01.
      ENDIF.

      CONCATENATE TEXT-T01 '_' SY-DATLO  '_' SY-UNAME INTO LV_FILENAME.
      ZCL_CN_ABAP_UTIL=>EXCL_SMW0_DOWNLOAD( EXPORTING IV_OBJID = LV_OBJID
                                                      IV_FILENAME = LV_FILENAME ).  "Iv Filename 생략시 현재 프로그램 id
    WHEN P_RD1B.
      IF SY-LANGU = '3'.
        LV_OBJID = LC_ZOMM4990_02.
      ELSE.
        LV_OBJID = LC_ZOMM4990_EN_02.
      ENDIF.

      CONCATENATE TEXT-T02 '_' SY-DATLO  '_' SY-UNAME INTO LV_FILENAME.
      ZCL_CN_ABAP_UTIL=>EXCL_SMW0_DOWNLOAD( EXPORTING IV_OBJID = LV_OBJID
                                                      IV_FILENAME = LV_FILENAME  ).  "Iv Filename 생략시 현재 프로그램 id
    WHEN P_RD1C.
      IF SY-LANGU = '3'.
        LV_OBJID = LC_ZOMM4990_03.
      ELSE.
        LV_OBJID = LC_ZOMM4990_EN_03.
      ENDIF.

      CONCATENATE TEXT-T03 '_' SY-DATLO  '_' SY-UNAME INTO LV_FILENAME.
      ZCL_CN_ABAP_UTIL=>EXCL_SMW0_DOWNLOAD( EXPORTING IV_OBJID = LV_OBJID
                                                      IV_FILENAME = LV_FILENAME ).  "Iv Filename 생략시 현재 프로그램 id
    WHEN P_RD1D.
      IF SY-LANGU = '3'.
        LV_OBJID = LC_ZOMM4990_04.
      ELSE.
        LV_OBJID = LC_ZOMM4990_EN_04.
      ENDIF.

      CONCATENATE TEXT-T04 '_' SY-DATLO  '_' SY-UNAME INTO LV_FILENAME.
      ZCL_CN_ABAP_UTIL=>EXCL_SMW0_DOWNLOAD( EXPORTING IV_OBJID = LV_OBJID
                                                      IV_FILENAME = LV_FILENAME  ).  "Iv Filename 생략시 현재 프로그램 id
    WHEN P_RD1E.
      IF SY-LANGU = '3'.
        LV_OBJID = LC_ZOMM4990_05.
      ELSE.
        LV_OBJID = LC_ZOMM4990_EN_05.
      ENDIF.

      CONCATENATE TEXT-T05 '_' SY-DATLO  '_' SY-UNAME INTO LV_FILENAME.
      ZCL_CN_ABAP_UTIL=>EXCL_SMW0_DOWNLOAD( EXPORTING IV_OBJID = LV_OBJID
                                                      IV_FILENAME = LV_FILENAME  ).  "Iv Filename 생략시 현재 프로그램 id
  ENDCASE.

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
*& Form CHECK_AUTHORITY
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM CHECK_AUTHORITY .

  CALL FUNCTION 'ZFMM_AUTH_CHECK'
    EXPORTING
      IV_USER = SY-UNAME.
*     IV_OBJECT                   = 'ZMM_COMMON'
*     IV_BUKRS                    =
*     IV_EKORG                    =
*     IV_LGORT                    =
*     IV_WERKS                    =
*     IV_ZEXSPA                   =
*     IV_ZPODEP                   =
*     IV_ZPRDEP                   =
* TABLES
*     IT_BUKRS                    =
*     IT_EKORG                    =
*     IT_LGORT                    =
*     IT_WERKS                    =
*     IT_ZEXSPA                   =
*     IT_ZPODEP                   =
*     IT_ZPRDEP                   =
* EXCEPTIONS
*     NO_ID_DATA_FOUND            = 1
*     AUTHORIZATION_FAILURE       = 2
*     NO_INPUT_AUTH_VALUE         = 3
*     NO_DATA_FOUND               = 4
*     MANDATORYFIELDISMISS        = 5
*     OTHERS  = 6
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4 DISPLAY LIKE 'E'.
    LEAVE LIST-PROCESSING.
  ENDIF.


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
*  PERFORM CHECK_CHANGED_DATA USING 'E' CHANGING LV_ANS.

  IF LV_ANS IS INITIAL.

    CASE GV_OK_CODE.
      WHEN 'EXIT'.
        LEAVE PROGRAM.
      WHEN OTHERS.
        LEAVE TO SCREEN 0.
    ENDCASE.

  ELSE.
    GV_MODE = 'X'.
    CALL METHOD GRF_GRID->SET_CHANGE_MODE( CHANGING CV_MODE = GV_MODE ).
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
*& Form PROCESSING_DATA_BATCH
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM PROCESSING_DATA_BATCH .

  DATA : LV_ZSEQ TYPE ZTMM40910-ZSEQ.

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

  SELECT SINGLE FROM ZTMM40910
    FIELDS MAX( ZSEQ )
    WHERE BUKRS = @P_BUKRS
      AND ERNAM = @SY-UNAME
    INTO ( @LV_ZSEQ ).

  IF LV_ZSEQ IS INITIAL.
    LV_ZSEQ = GC_00000000.
  ENDIF.

* U1 START : 공통 코드 조회. T1, T2 코드마스터 분기
  ZCL_MMG_COMMON=>COMMON_G_CONFIG_OLDNEW(
     EXPORTING IV_BUKRS  = P_BUKRS
               IS_COMMON = VALUE #( M = 'D1' D = 'D1100' S = 'D1103' )
     IMPORTING ET_OUTTAB = DATA(LT_COMMON) ).
* U1 END

* 20221024 START : 당도(SUGARRATIO) 속성 추가
  LT_BATCH[] = GT_BATCH[].
  SORT LT_BATCH BY MATNR LIFNR HSDAT LICHA VFDAT SUGARRATIO.
  DELETE ADJACENT DUPLICATES FROM LT_BATCH COMPARING MATNR LIFNR HSDAT LICHA VFDAT SUGARRATIO.

  IF LT_BATCH[] IS NOT INITIAL.
    "동일 배치 조회(구매배치)
    SELECT FROM ZSVCMMG_BATFIND "ZSVCMM_BATFIND
      FIELDS MATNR, BATCH, LIFNR, LWEDT, HSDAT, LICHN, VFDAT, ZMAKER, SUGARRATIO
      FOR ALL ENTRIES IN @LT_BATCH
      WHERE MATNR      = @LT_BATCH-MATNR
        AND LIFNR      = @LT_BATCH-LIFNR
        AND HSDAT      = @LT_BATCH-HSDAT
        AND LICHN      = @LT_BATCH-LICHA
        AND VFDAT      = @LT_BATCH-VFDAT
        AND SUGARRATIO = @LT_BATCH-SUGARRATIO
      INTO TABLE @DATA(LT_CHARG).

    FREE LT_BATCH.
* 20221024 END

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
                                                             ZMAKER     = <LS_BATCH>-ZMM_MAKER
                                                             SUGARRATIO = <LS_BATCH>-SUGARRATIO. "20221024(+)
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
*& Form CHECK_DATA_LIFNR
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> <LS_DISP>
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
*& Form PROCESSING_DATA_STOCK
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM PROCESSING_DATA_STOCK .

  DATA : LT_MARA TYPE TABLE OF TS_MARA.

  DATA : LV_ZSEQ TYPE ZTMM40920-ZSEQ,   LV_SEQ  TYPE ZTMM40920-ZSEQ.

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
*& Form BTN_ON_MIGRATION_INIT_STOCK
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM BTN_ON_MIGRATION_INIT_STOCK .

  DATA: LV_TOTAL TYPE I, LV_SUCCE TYPE I, LV_ERROR TYPE I.

  READ TABLE GT_DISP INTO DATA(LS_DISP_CHK) WITH KEY STATUS = ICON_LED_RED.
  IF LS_DISP_CHK IS NOT INITIAL.
    MESSAGE S000 WITH TEXT-M07 DISPLAY LIKE 'E'. EXIT.
  ENDIF.

  CHECK LS_DISP_CHK IS INITIAL.


  READ TABLE GT_DISP INTO DATA(LS_DISP_CHK2) WITH KEY STATUS = ICON_LED_GREEN ZSTATUS = 'S'.
  IF LS_DISP_CHK2 IS NOT INITIAL.
    MESSAGE S000 WITH TEXT-M58 DISPLAY LIKE 'E'. EXIT.
  ENDIF.

  CHECK LS_DISP_CHK2 IS INITIAL.



  DESCRIBE TABLE GT_DISP LINES DATA(LV_TSELLINES).

  CLEAR : LV_TOTAL, LV_SUCCE, LV_ERROR.

  LV_TOTAL = LV_TSELLINES.

  CHECK GRF_GRID->POP_TO_MSG( IV_TYPE  = 'A'
                              IV_TITLE = CONV STRING( TEXT-U01 )      "Migration
                              IV_TEXT1 = CONV #( TEXT-M60 && LV_TOTAL && TEXT-M61 )
                              IV_TEXT2 = CONV #( '' ) )
                              EQ ABAP_TRUE. " YES

  DATA : LS_GOODSMVT_HEADER  LIKE BAPI2017_GM_HEAD_01,
         LS_GOODSMVT_CODE    LIKE BAPI2017_GM_CODE,
         LV_MATERIALDOCUMENT TYPE BAPI2017_GM_HEAD_RET-MAT_DOC,
         LV_MATDOCUMENTYEAR  TYPE BAPI2017_GM_HEAD_RET-DOC_YEAR,
         LS_GOODSMVT_ITEM    TYPE BAPI2017_GM_ITEM_CREATE,
         LT_GOODSMVT_ITEM    TYPE TABLE OF BAPI2017_GM_ITEM_CREATE,
         LT_RETURN           TYPE TABLE OF BAPIRET2,
         LV_ZEILE            TYPE ZTMM40920-ZEILE,
         LS_ZTMM40920        TYPE ZTMM40920,
         LT_ZTMM40920        TYPE TABLE OF ZTMM40920,
         LS_ZTMM40930        TYPE ZTMM40930,
         LT_ZTMM40930        TYPE TABLE OF ZTMM40930,
         LS_ZTMM40940        TYPE ZTMM40940,
         LT_ZTMM40940        TYPE TABLE OF ZTMM40940,
         LS_ZTMM40950        TYPE ZTMM40950,
         LT_ZTMM40950        TYPE TABLE OF ZTMM40950.

  _G_INIT : LT_ZTMM40920, LT_ZTMM40930, LT_ZTMM40940, LT_ZTMM40950.

  LOOP AT GT_DISP INTO DATA(LS_DISP).

    CLEAR : LS_GOODSMVT_HEADER, LS_GOODSMVT_CODE, LS_GOODSMVT_ITEM,
            LV_MATERIALDOCUMENT, LV_MATDOCUMENTYEAR.

    _G_INIT : LT_GOODSMVT_ITEM, LT_RETURN.

*- 공통
    LS_GOODSMVT_HEADER-PSTNG_DATE = P_BUDAT.
    LS_GOODSMVT_HEADER-DOC_DATE   = P_BUDAT.
    LS_GOODSMVT_HEADER-PR_UNAME   = SY-UNAME.
    LS_GOODSMVT_HEADER-HEADER_TXT = TEXT-T13.   "기초재고 Migration

    LS_GOODSMVT_CODE-GM_CODE    = '05'.

    LS_GOODSMVT_ITEM-MATERIAL_LONG    = LS_DISP-MATNR.
    LS_GOODSMVT_ITEM-PLANT            = LS_DISP-WERKS.
    LS_GOODSMVT_ITEM-BATCH            = LS_DISP-CHARG.
    LS_GOODSMVT_ITEM-MOVE_TYPE        = '561'.
    LS_GOODSMVT_ITEM-STCK_TYPE        = LS_DISP-INSMK.
    LS_GOODSMVT_ITEM-VAL_TYPE         = LS_DISP-BWTAR.
    LS_GOODSMVT_ITEM-ENTRY_QNT        = LS_DISP-ERFMG.
    LS_GOODSMVT_ITEM-ENTRY_UOM        = LS_DISP-ZMEINS.

    IF LS_DISP-VPRSV = 'V'.
      LS_GOODSMVT_ITEM-AMOUNT_LC        = LS_DISP-EXBWR.
    ENDIF.

*-
    IF P_RD1C NE 'X'.
      LS_GOODSMVT_ITEM-STGE_LOC         = LS_DISP-LGORT.
    ENDIF.

    CASE 'X'.
      WHEN P_RD1C.  "사급
        LS_GOODSMVT_ITEM-SPEC_STOCK    = 'O'.
        LS_GOODSMVT_ITEM-VENDOR        = LS_DISP-LIFNR.
      WHEN P_RD1D.  "위탁
        LS_GOODSMVT_ITEM-SPEC_STOCK    = 'K'.
        LS_GOODSMVT_ITEM-VENDOR        = LS_DISP-LIFNR.
      WHEN P_RD1E.  "고객
        LS_GOODSMVT_ITEM-SPEC_STOCK    = 'E'.
        LS_GOODSMVT_ITEM-CUSTOMER      = LS_DISP-KUNNR.
        LS_GOODSMVT_ITEM-VAL_SALES_ORD     = LS_DISP-VBELN.
        LS_GOODSMVT_ITEM-VAL_S_ORD_ITEM    = LS_DISP-POSNR.
    ENDCASE.

    APPEND LS_GOODSMVT_ITEM TO LT_GOODSMVT_ITEM. CLEAR : LS_GOODSMVT_ITEM.

    IF LT_GOODSMVT_ITEM[] IS NOT INITIAL.

      CALL FUNCTION 'BAPI_GOODSMVT_CREATE'
        EXPORTING
          GOODSMVT_HEADER  = LS_GOODSMVT_HEADER
          GOODSMVT_CODE    = LS_GOODSMVT_CODE
        IMPORTING
          MATERIALDOCUMENT = LV_MATERIALDOCUMENT
          MATDOCUMENTYEAR  = LV_MATDOCUMENTYEAR
        TABLES
          GOODSMVT_ITEM    = LT_GOODSMVT_ITEM
          RETURN           = LT_RETURN.

      CLEAR : LV_ZEILE, LS_ZTMM40920, LS_ZTMM40930, LS_ZTMM40940, LS_ZTMM40950.
      LV_ZEILE = GC_0001.

      IF LV_MATERIALDOCUMENT IS NOT INITIAL AND LV_MATDOCUMENTYEAR IS NOT INITIAL.
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            WAIT = 'X'.

*        PERFORM UPDATE_SUCCESS_RESULT USING LV_MATDOCUMENTYEAR
*                                            LV_MATERIALDOCUMENT.

        LS_DISP-STATUS = ICON_LED_GREEN.
        LS_DISP-ZMESSAGE = TEXT-M62 && LV_MATERIALDOCUMENT && TEXT-M63.

        CASE 'X'.
          WHEN P_RD1B.  "창고재고 Migration

            MOVE-CORRESPONDING LS_DISP TO LS_ZTMM40920.
            LS_ZTMM40920-ERNAM = SY-UNAME.
            LS_ZTMM40920-MJAHR = LV_MATDOCUMENTYEAR.
            LS_ZTMM40920-MBLNR = LV_MATERIALDOCUMENT.
            LS_ZTMM40920-ZEILE = LV_ZEILE.
            LS_ZTMM40920-ZSTATUS = 'S'.
            LS_ZTMM40920-ERDAT = SY-DATUM.  LS_ZTMM40920-ERZET = SY-UZEIT. LS_ZTMM40920-ERNAM = SY-UNAME.
            LS_ZTMM40920-AEDAT = SY-DATUM.  LS_ZTMM40920-AEZET = SY-UZEIT. LS_ZTMM40920-AENAM = SY-UNAME.

            APPEND LS_ZTMM40920 TO LT_ZTMM40920. CLEAR LS_ZTMM40920.

          WHEN P_RD1C.  "사급재고 Migration

            MOVE-CORRESPONDING LS_DISP TO LS_ZTMM40930.
            LS_ZTMM40930-ERNAM = SY-UNAME.
            LS_ZTMM40930-MJAHR = LV_MATDOCUMENTYEAR.
            LS_ZTMM40930-MBLNR = LV_MATERIALDOCUMENT.
            LS_ZTMM40930-ZEILE = LV_ZEILE.
            LS_ZTMM40930-ZSTATUS = 'S'.
            LS_ZTMM40930-ERDAT = SY-DATUM.  LS_ZTMM40930-ERZET = SY-UZEIT. LS_ZTMM40930-ERNAM = SY-UNAME.
            LS_ZTMM40930-AEDAT = SY-DATUM.  LS_ZTMM40930-AEZET = SY-UZEIT. LS_ZTMM40930-AENAM = SY-UNAME.

            APPEND LS_ZTMM40930 TO LT_ZTMM40930. CLEAR LS_ZTMM40930.

          WHEN P_RD1D.  "위탁재고 Migration

            MOVE-CORRESPONDING LS_DISP TO LS_ZTMM40940.
            LS_ZTMM40940-ERNAM = SY-UNAME.
            LS_ZTMM40940-MJAHR = LV_MATDOCUMENTYEAR.
            LS_ZTMM40940-MBLNR = LV_MATERIALDOCUMENT.
            LS_ZTMM40940-ZEILE = LV_ZEILE.
            LS_ZTMM40940-ZSTATUS = 'S'.
            LS_ZTMM40940-ERDAT = SY-DATUM.  LS_ZTMM40940-ERZET = SY-UZEIT. LS_ZTMM40940-ERNAM = SY-UNAME.
            LS_ZTMM40940-AEDAT = SY-DATUM.  LS_ZTMM40940-AEZET = SY-UZEIT. LS_ZTMM40940-AENAM = SY-UNAME.

            APPEND LS_ZTMM40940 TO LT_ZTMM40940. CLEAR LS_ZTMM40940.

          WHEN P_RD1E.  "고객재고 Migration

            MOVE-CORRESPONDING LS_DISP TO LS_ZTMM40950.
            LS_ZTMM40950-ERNAM = SY-UNAME.
            LS_ZTMM40950-MJAHR = LV_MATDOCUMENTYEAR.
            LS_ZTMM40950-MBLNR = LV_MATERIALDOCUMENT.
            LS_ZTMM40950-ZEILE = LV_ZEILE.
            LS_ZTMM40950-ZSTATUS = 'S'.
            LS_ZTMM40950-ERDAT = SY-DATUM.  LS_ZTMM40950-ERZET = SY-UZEIT. LS_ZTMM40950-ERNAM = SY-UNAME.
            LS_ZTMM40950-AEDAT = SY-DATUM.  LS_ZTMM40950-AEZET = SY-UZEIT. LS_ZTMM40950-AENAM = SY-UNAME.

            APPEND LS_ZTMM40950 TO LT_ZTMM40950. CLEAR LS_ZTMM40950.
        ENDCASE.

      ELSE.

        CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
        READ TABLE LT_RETURN INTO DATA(LS_RETURN2) INDEX 1.

*        PERFORM UPDATE_FAIL_RESULT USING LS_RETURN2-MESSAGE.

        LS_DISP-STATUS = ICON_LED_RED.
        LS_DISP-ZMESSAGE = TEXT-M64 && LS_RETURN2-MESSAGE.

        CASE 'X'.
          WHEN P_RD1B.
            MOVE-CORRESPONDING LS_DISP TO LS_ZTMM40920.
            LS_ZTMM40920-ERNAM = SY-UNAME.
            LS_ZTMM40920-ZSTATUS = 'E'.
            LS_ZTMM40920-ERDAT = SY-DATUM.  LS_ZTMM40920-ERZET = SY-UZEIT. LS_ZTMM40920-ERNAM = SY-UNAME.
            LS_ZTMM40920-AEDAT = SY-DATUM.  LS_ZTMM40920-AEZET = SY-UZEIT. LS_ZTMM40920-AENAM = SY-UNAME.

            APPEND LS_ZTMM40920 TO LT_ZTMM40920. CLEAR LS_ZTMM40920.

          WHEN P_RD1C.
            MOVE-CORRESPONDING LS_DISP TO LS_ZTMM40930.
            LS_ZTMM40930-ERNAM = SY-UNAME.
            LS_ZTMM40930-ZSTATUS = 'E'.
            LS_ZTMM40930-ERDAT = SY-DATUM.  LS_ZTMM40930-ERZET = SY-UZEIT. LS_ZTMM40930-ERNAM = SY-UNAME.
            LS_ZTMM40930-AEDAT = SY-DATUM.  LS_ZTMM40930-AEZET = SY-UZEIT. LS_ZTMM40930-AENAM = SY-UNAME.

            APPEND LS_ZTMM40930 TO LT_ZTMM40930. CLEAR LS_ZTMM40930.

          WHEN P_RD1D.
            MOVE-CORRESPONDING LS_DISP TO LS_ZTMM40940.
            LS_ZTMM40940-ERNAM = SY-UNAME.
            LS_ZTMM40940-ZSTATUS = 'E'.
            LS_ZTMM40940-ERDAT = SY-DATUM.  LS_ZTMM40940-ERZET = SY-UZEIT. LS_ZTMM40940-ERNAM = SY-UNAME.
            LS_ZTMM40940-AEDAT = SY-DATUM.  LS_ZTMM40940-AEZET = SY-UZEIT. LS_ZTMM40940-AENAM = SY-UNAME.

            APPEND LS_ZTMM40940 TO LT_ZTMM40940. CLEAR LS_ZTMM40940.

          WHEN P_RD1E.
            MOVE-CORRESPONDING LS_DISP TO LS_ZTMM40950.
            LS_ZTMM40950-ERNAM = SY-UNAME.
            LS_ZTMM40950-ZSTATUS = 'E'.
            LS_ZTMM40950-ERDAT = SY-DATUM.  LS_ZTMM40950-ERZET = SY-UZEIT. LS_ZTMM40950-ERNAM = SY-UNAME.
            LS_ZTMM40950-AEDAT = SY-DATUM.  LS_ZTMM40950-AEZET = SY-UZEIT. LS_ZTMM40950-AENAM = SY-UNAME.

            APPEND LS_ZTMM40950 TO LT_ZTMM40950. CLEAR LS_ZTMM40950.

        ENDCASE.
      ENDIF.
    ENDIF.

  ENDLOOP.

  CASE 'X'.
    WHEN P_RD1B.
      MODIFY ZTMM40920 FROM TABLE LT_ZTMM40920.
    WHEN P_RD1C.
      MODIFY ZTMM40930 FROM TABLE LT_ZTMM40930.
    WHEN P_RD1D.
      MODIFY ZTMM40940 FROM TABLE LT_ZTMM40940.
    WHEN P_RD1E.
      MODIFY ZTMM40950 FROM TABLE LT_ZTMM40950.
  ENDCASE.

  "Refresh
  CASE 'X'.
    WHEN P_RD1B.  " 창고 Migration
      PERFORM REFRESH_DATA_B.
    WHEN P_RD1C.  " 사급 Migration
      PERFORM REFRESH_DATA_C.
    WHEN P_RD1D.  " 위탁 Migration
      PERFORM REFRESH_DATA_D.
    WHEN P_RD1E.  " 고객 Migration
      PERFORM REFRESH_DATA_E.
  ENDCASE.

  GRF_GRID->REFRESH_GRID_DISPLAY( ).

ENDFORM.
*&---------------------------------------------------------------------*
*& Form BTN_ON_MIGRATION_INIT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM BTN_ON_MIGRATION_INIT .

  DATA: LV_TOTAL TYPE I, LV_SUCCE TYPE I, LV_ERROR TYPE I.
*-
  READ TABLE GT_BATCH INTO DATA(LS_BATCH_CHK) WITH KEY STATUS = ICON_LED_RED.
  IF LS_BATCH_CHK IS NOT INITIAL.
    MESSAGE S000 WITH TEXT-M07 DISPLAY LIKE 'E'. EXIT.
  ENDIF.

  READ TABLE GT_BATCH INTO DATA(LS_BATCH_CHK2) WITH KEY STATUS = ICON_LED_YELLOW.
  IF LS_BATCH_CHK2 IS NOT INITIAL.
    MESSAGE S000 WITH TEXT-M07 DISPLAY LIKE 'E'. EXIT.
  ENDIF.

  CHECK LS_BATCH_CHK IS INITIAL OR LS_BATCH_CHK2 IS INITIAL.
*-

  DESCRIBE TABLE GT_BATCH LINES DATA(LV_TSELLINES).

  CLEAR : LV_TOTAL, LV_SUCCE, LV_ERROR.

  LV_TOTAL = LV_TSELLINES.

  CHECK GRF_GRID->POP_TO_MSG( IV_TYPE  = 'A'
                              IV_TITLE = CONV STRING( TEXT-U01 )      "Migration
                              IV_TEXT1 = CONV #( TEXT-M60 && LV_TOTAL && TEXT-M61 )
                              IV_TEXT2 = CONV #( '' ) )
                              EQ ABAP_TRUE. " YES

  DATA : LV_CHARG               TYPE ZSVBMMBATLAST-CHARG,
         LV_NEW_CHARG           TYPE ZSVBMMBATLAST-CHARG,
         LV_TYPE                LIKE DD01V-DATATYPE,
         LS_ZTMM40910           TYPE ZTMM40910,
         LT_ZTMM40910           TYPE TABLE OF ZTMM40910,
         LS_BATCHATTRIBUTES     LIKE BAPIBATCHATT,
         LS_BATCHATTRIBUTESX    LIKE BAPIBATCHATTX,
         LS_BATCHCONTROLFIELDS  LIKE BAPIBATCHCTRL,
         LT_RETURN              TYPE TABLE OF BAPIRET2,
         LS_CLASSALLOCATIONS    LIKE BAPI3060_ALLOCATION,
         LT_CLASSALLOCATIONS    TYPE TABLE OF  BAPI3060_ALLOCATION,
         LT_CLASSVALUATIONSCHAR TYPE TABLE OF  BAPI3060_VALUATION_CHAR.


  IF GT_BATCH[] IS NOT INITIAL.

    _G_INIT : LT_ZTMM40910.

    LOOP AT GT_BATCH ASSIGNING FIELD-SYMBOL(<LS_BATCH>).

      CLEAR : LV_NEW_CHARG, LV_CHARG, LV_TYPE, LS_BATCHATTRIBUTES, LS_BATCHATTRIBUTESX, LS_BATCHCONTROLFIELDS, LS_CLASSALLOCATIONS, LS_ZTMM40910.
      _G_INIT : LT_RETURN, LT_CLASSALLOCATIONS, LT_CLASSVALUATIONSCHAR.

      IF <LS_BATCH>-ZCHECK = 'X'. "구매 관리
        IF <LS_BATCH>-EX_CHARG IS NOT INITIAL.
          LV_NEW_CHARG = <LS_BATCH>-EX_CHARG.
        ELSE.
          "자재별 마지막 배치번호 갖고 오기
          SELECT SINGLE CHARG
            INTO @LV_CHARG
            FROM ZSVBMMBATLAST
            WHERE MATNR = @<LS_BATCH>-MATNR.

          IF LV_CHARG IS NOT INITIAL.
            CALL FUNCTION 'NUMERIC_CHECK'
              EXPORTING
                STRING_IN = LV_CHARG
              IMPORTING
                HTYPE     = LV_TYPE.
            IF LV_TYPE+0(1) NE 'N'.
              <LS_BATCH>-STATUS = ICON_LED_RED.
              <LS_BATCH>-ZMESSAGE = TEXT-M35.      "외부 채번된 배치는 다음 배치 번호를 자동 지정 할 수 없습니다
              EXIT.
            ENDIF.

            LV_NEW_CHARG = LV_CHARG + 1.
          ELSE.
            LV_NEW_CHARG = 0000000001.
          ENDIF.

          DATA(LV_NEW) = | { LV_NEW_CHARG ALPHA = IN WIDTH = 10 } | .
          CONDENSE LV_NEW.
          LV_NEW_CHARG = LV_NEW.
        ENDIF.


        LS_BATCHATTRIBUTES-EXPIRYDATE = <LS_BATCH>-VFDAT.
        LS_BATCHATTRIBUTESX-EXPIRYDATE = 'X'.
        LS_BATCHATTRIBUTES-VENDOR_NO  = <LS_BATCH>-LIFNR.
        LS_BATCHATTRIBUTESX-VENDOR_NO  = 'X'.

        IF <LS_BATCH>-CLASS = GC_ZPP_YJP OR <LS_BATCH>-CLASS = GC_YJPBATCH1 OR <LS_BATCH>-CLASS = GC_YJPBATCH2 OR <LS_BATCH>-CLASS = GC_YJPBATCH3 OR <LS_BATCH>-CLASS = GC_YJPBATCH4.
          LS_BATCHATTRIBUTES-VENDRBATCH = <LS_BATCH>-LICHA.
          LS_BATCHATTRIBUTESX-VENDRBATCH = 'X'.
        ENDIF.

        LS_BATCHATTRIBUTES-PROD_DATE  = <LS_BATCH>-HSDAT.
        LS_BATCHATTRIBUTESX-PROD_DATE  = 'X'.

        LS_BATCHCONTROLFIELDS-BATCHLEVEL = '1'.
        LS_BATCHCONTROLFIELDS-CLASS_NUM  = <LS_BATCH>-CLASS.
        LS_BATCHCONTROLFIELDS-DOCLASSIFY = 'X'.

        LS_CLASSALLOCATIONS-CLASS_TYPE  = '023'.

        PERFORM NUMERIC_CHECK USING <LS_BATCH> LV_NEW_CHARG
                              CHANGING LS_CLASSALLOCATIONS-OBJECTKEY.

        LS_CLASSALLOCATIONS-OBJECTTABLE = GC_MCH1.
        LS_CLASSALLOCATIONS-STATUS      = '1'.
        LS_CLASSALLOCATIONS-CLASSNUM    = <LS_BATCH>-CLASS.
        LS_CLASSALLOCATIONS-CLASSTYPE   = '023'.

        APPEND LS_CLASSALLOCATIONS TO LT_CLASSALLOCATIONS.


        PERFORM APPEND_BATCH_ATTRIBUTE TABLES LT_CLASSVALUATIONSCHAR
                                       USING <LS_BATCH> LV_NEW_CHARG.

        CALL FUNCTION 'BAPI_BATCH_SAVE_REPLICA'
          EXPORTING
            BATCH               = LV_NEW_CHARG
            BATCHATTRIBUTES     = LS_BATCHATTRIBUTES
            BATCHATTRIBUTESX    = LS_BATCHATTRIBUTESX
            BATCHCONTROLFIELDS  = LS_BATCHCONTROLFIELDS
            MATERIAL_LONG       = <LS_BATCH>-MATNR
          TABLES
            RETURN              = LT_RETURN
            CLASSALLOCATIONS    = LT_CLASSALLOCATIONS
            CLASSVALUATIONSCHAR = LT_CLASSVALUATIONSCHAR.

        WAIT UP TO '0.1' SECONDS.

        READ TABLE LT_RETURN INTO DATA(LS_RETURN) WITH KEY TYPE = 'E'.
        IF SY-SUBRC = 0.
          CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
          <LS_BATCH>-STATUS = ICON_LED_RED.
          <LS_BATCH>-ZMESSAGE = LS_RETURN-MESSAGE.
          _G_ADD_1 LV_ERROR.
        ELSE.
          CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
            EXPORTING
              WAIT = 'X'.
          <LS_BATCH>-STATUS = ICON_LED_GREEN.
          _G_ADD_1 LV_SUCCE.

        ENDIF.

      ELSE. "구매 관리 X

        LV_NEW_CHARG = <LS_BATCH>-EX_CHARG.

        LS_BATCHATTRIBUTES-EXPIRYDATE = <LS_BATCH>-VFDAT.
        LS_BATCHATTRIBUTESX-EXPIRYDATE = 'X'.
        LS_BATCHATTRIBUTES-VENDOR_NO  = <LS_BATCH>-LIFNR.
        LS_BATCHATTRIBUTESX-VENDOR_NO  = 'X'.

        LS_BATCHATTRIBUTES-VENDRBATCH = <LS_BATCH>-LICHA.
        LS_BATCHATTRIBUTESX-VENDRBATCH = 'X'.

        LS_BATCHATTRIBUTES-PROD_DATE  = <LS_BATCH>-HSDAT.
        LS_BATCHATTRIBUTESX-PROD_DATE  = 'X'.


        LS_BATCHCONTROLFIELDS-BATCHLEVEL = '1'.
        LS_BATCHCONTROLFIELDS-CLASS_NUM  = <LS_BATCH>-CLASS.
        LS_BATCHCONTROLFIELDS-DOCLASSIFY = 'X'.

        LS_CLASSALLOCATIONS-CLASS_TYPE  = '023'.

        PERFORM NUMERIC_CHECK USING <LS_BATCH> LV_NEW_CHARG
                              CHANGING LS_CLASSALLOCATIONS-OBJECTKEY.

        LS_CLASSALLOCATIONS-OBJECTTABLE = GC_MCH1.
        LS_CLASSALLOCATIONS-STATUS      = '1'.
        LS_CLASSALLOCATIONS-CLASSNUM    = <LS_BATCH>-CLASS.
        LS_CLASSALLOCATIONS-CLASSTYPE   = '023'.

        APPEND LS_CLASSALLOCATIONS TO LT_CLASSALLOCATIONS. CLEAR LS_CLASSALLOCATIONS.

        PERFORM APPEND_BATCH_ATTRIBUTE TABLES LT_CLASSVALUATIONSCHAR
                                       USING <LS_BATCH> LV_NEW_CHARG.

        CALL FUNCTION 'BAPI_BATCH_SAVE_REPLICA'
          EXPORTING
            BATCH               = LV_NEW_CHARG
            BATCHATTRIBUTES     = LS_BATCHATTRIBUTES
            BATCHATTRIBUTESX    = LS_BATCHATTRIBUTESX
            BATCHCONTROLFIELDS  = LS_BATCHCONTROLFIELDS
            MATERIAL_LONG       = <LS_BATCH>-MATNR
          TABLES
            RETURN              = LT_RETURN
            CLASSALLOCATIONS    = LT_CLASSALLOCATIONS
            CLASSVALUATIONSCHAR = LT_CLASSVALUATIONSCHAR.

        WAIT UP TO '0.1' SECONDS.

        READ TABLE LT_RETURN INTO DATA(LS_RETURN2) WITH KEY TYPE = 'E'.
        IF SY-SUBRC = 0.
          CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
          <LS_BATCH>-STATUS = ICON_LED_RED.
          <LS_BATCH>-ZMESSAGE = LS_RETURN2-MESSAGE.
          _G_ADD_1 LV_ERROR.
        ELSE.
          CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
            EXPORTING
              WAIT = 'X'.
          <LS_BATCH>-STATUS = ICON_LED_GREEN.
          _G_ADD_1 LV_SUCCE.
        ENDIF.

      ENDIF.

      MOVE-CORRESPONDING <LS_BATCH> TO LS_ZTMM40910.
      LS_ZTMM40910-CHARG = LV_NEW_CHARG.
      LS_ZTMM40910-BUKRS = P_BUKRS.
      LS_ZTMM40910-ERNAM = SY-UNAME.
      IF <LS_BATCH>-STATUS = ICON_LED_GREEN.
        LS_ZTMM40910-ZSTATUS = 'S'.
      ELSE.
        LS_ZTMM40910-ZSTATUS = 'E'.
      ENDIF.
      LS_ZTMM40910-ZMESSAGE = <LS_BATCH>-ZMESSAGE.
      LS_ZTMM40910-ERDAT = SY-DATUM.
      LS_ZTMM40910-ERZET = SY-UZEIT.
      LS_ZTMM40910-ERNAM = SY-UNAME.

      APPEND LS_ZTMM40910 TO LT_ZTMM40910. CLEAR : LS_ZTMM40910.

*      _G_WAIT_1_SECOND.

    ENDLOOP.

    MODIFY ZTMM40910 FROM TABLE LT_ZTMM40910.
  ENDIF.

*-Refresh Data
  DATA : LS_BATCH TYPE TS_BATCH.

  _G_INIT : GT_BATCH.

  IF LT_ZTMM40910[] IS NOT INITIAL.

    DATA(LT_SUB) = LT_ZTMM40910[].
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

    LOOP AT LT_ZTMM40910 INTO DATA(LS_ZTMM40910_SUB).
      MOVE-CORRESPONDING LS_ZTMM40910_SUB TO LS_BATCH.

      IF LS_ZTMM40910_SUB-ZSTATUS = 'S'.
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
*-

  MESSAGE S014(ZMM01) WITH LV_TOTAL LV_SUCCE LV_ERROR.
  GRF_GRID->REFRESH_GRID_DISPLAY( ).

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

  DATA : LT_ZTMM40910 TYPE TABLE OF ZTMM40910,
         LS_BATCH     TYPE TS_BATCH.

  CASE 'X'.
    WHEN P_RD3A.    "전체

      SELECT FROM ZTMM40910
        FIELDS BUKRS, ERNAM, ZSEQ, MATNR, XCHPF, CLASS, ZCHECK, LIFNR,
               HSDAT, VFDAT, LICHA, ZMM_MAKER, ZCOMP_RATIO, ZCOMP_MOIST, ZCOMP_NET, ZCOMP_DRY,
               ZSTATUS, ZMESSAGE, EX_CHARG, CHARG, ERDAT, ERZET

        WHERE BUKRS = @P_BUKRS
          AND MATNR IN @S_MATNR
          AND CHARG IN @S_CHARG
          AND ERDAT IN @S_ERDAT
          AND ERNAM IN @S_ERNAM

        APPENDING CORRESPONDING FIELDS OF TABLE @LT_ZTMM40910.

    WHEN P_RD3B.    "성공

      SELECT FROM ZTMM40910
        FIELDS BUKRS, ERNAM, ZSEQ, MATNR, XCHPF, CLASS, ZCHECK, LIFNR,
               HSDAT, VFDAT, LICHA, ZMM_MAKER, ZCOMP_RATIO, ZCOMP_MOIST, ZCOMP_NET, ZCOMP_DRY,
               ZSTATUS, ZMESSAGE, EX_CHARG, CHARG, ERDAT, ERZET

        WHERE BUKRS = @P_BUKRS
          AND MATNR IN @S_MATNR
          AND CHARG IN @S_CHARG
          AND ERDAT IN @S_ERDAT
          AND ERNAM IN @S_ERNAM
          AND ZSTATUS = 'S'

        APPENDING CORRESPONDING FIELDS OF TABLE @LT_ZTMM40910.

    WHEN P_RD3C.    "실패

      SELECT FROM ZTMM40910
        FIELDS BUKRS, ERNAM, ZSEQ, MATNR, XCHPF, CLASS, ZCHECK, LIFNR,
               HSDAT, VFDAT, LICHA, ZMM_MAKER, ZCOMP_RATIO, ZCOMP_MOIST, ZCOMP_NET, ZCOMP_DRY,
               ZSTATUS, ZMESSAGE, EX_CHARG, CHARG, ERDAT, ERZET

        WHERE BUKRS = @P_BUKRS
          AND MATNR IN @S_MATNR
          AND CHARG IN @S_CHARG
          AND ERDAT IN @S_ERDAT
          AND ERNAM IN @S_ERNAM
          AND ZSTATUS NE 'S'

        APPENDING CORRESPONDING FIELDS OF TABLE @LT_ZTMM40910.

    WHEN OTHERS.

  ENDCASE.

  IF LT_ZTMM40910[] IS NOT INITIAL.

    DATA(LT_SUB) = LT_ZTMM40910[].
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

    LOOP AT LT_ZTMM40910 INTO DATA(LS_ZTMM40910).
      MOVE-CORRESPONDING LS_ZTMM40910 TO LS_BATCH.

      IF LS_ZTMM40910-ZSTATUS = 'S'.
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

  DATA : LT_ZTMM40920 TYPE TABLE OF ZTMM40920,
         LS_DISP      TYPE TS_DISP.


  CASE 'X'.
    WHEN P_RD3A.    "전체

      SELECT FROM ZTMM40920
        FIELDS BUKRS, WERKS, LGORT, ERNAM, ZSEQ, MATNR, BESKZ, SOBSL, MMSTA, BKLAS, XCHPF, CHARG, BWTTY,
               BWTAR, VPRSV, BWART, ERFMG, MEINS, ZMEINS, INSMK, EXBWR, WAERS, MJAHR, MBLNR, ZEILE, BUDAT,
               MJAHR_R, MBLNR_R, ZEILE_R, ZSTATUS, ZMESSAGE, ERDAT, ERZET, AEDAT, AEZET, AENAM

        WHERE BUKRS = @P_BUKRS
          AND MATNR IN @S_MATNR
          AND WERKS IN @S_WERKS
          AND LGORT IN @S_LGORT
          AND ERDAT IN @S_ERDAT
          AND ERNAM IN @S_ERNAM

        APPENDING CORRESPONDING FIELDS OF TABLE @LT_ZTMM40920.

    WHEN P_RD3B.    "성공

      SELECT FROM ZTMM40920
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

        APPENDING CORRESPONDING FIELDS OF TABLE @LT_ZTMM40920.

    WHEN P_RD3C.    "실패

      SELECT FROM ZTMM40920
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

        APPENDING CORRESPONDING FIELDS OF TABLE @LT_ZTMM40920.

  ENDCASE.

  IF LT_ZTMM40920[] IS NOT INITIAL.

    DATA(LT_SUB) = LT_ZTMM40920[].
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

    LOOP AT LT_ZTMM40920 INTO DATA(LS_ZTMM40920).

      MOVE-CORRESPONDING LS_ZTMM40920 TO LS_DISP.

      IF LS_ZTMM40920-ZSTATUS = 'S'.
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
*& Form BTN_ON_CANCEL_INIT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
*FORM BTN_ON_CANCEL_INIT .
*
*  DATA : LT_DISP TYPE TABLE OF TS_DISP.
*
**-
*  CALL METHOD GRF_GRID->GET_SELECTED_ROWS
*    IMPORTING
*      ET_INDEX_ROWS = DATA(LT_SELIDX).
*
*  DESCRIBE TABLE LT_SELIDX LINES DATA(LV_TSELLINES).
*
*  IF LV_TSELLINES IS INITIAL.
*    MESSAGE S000 WITH '취소 대상을 선택하세요.'(M14) DISPLAY LIKE 'E'. EXIT.
*  ENDIF.
*
**-
*
*  _G_INIT : LT_DISP.
*
*  LOOP AT LT_SELIDX INTO DATA(LS_SELIDX).
*    READ TABLE GT_DISP INTO DATA(LS_CHECK) INDEX LS_SELIDX-INDEX.
*    IF LS_CHECK-STATUS = ICON_LED_GREEN AND LS_CHECK-MBLNR_R IS INITIAL.
*      APPEND LS_CHECK TO LT_DISP. CLEAR LS_CHECK.
*    ENDIF.
*  ENDLOOP.
*
*  CHECK GRF_GRID->POP_TO_MSG( IV_TYPE  = 'A'
*                              IV_TITLE = CONV STRING( TEXT-U01 )      "Migration 취소
*                              IV_TEXT1 = CONV #( '선택된 정보를 취소합니다. 취소하시겠습니까?')
*                              IV_TEXT2 = CONV #( '단, 취소자재문서를 포함한 라인의 경우 자동 해제됩니다.' ) )
*                              EQ ABAP_TRUE. " YES
*
*  DATA : LS_GOODSMVT_HEADER  LIKE BAPI2017_GM_HEAD_01,
*         LS_GOODSMVT_CODE    LIKE BAPI2017_GM_CODE,
*         LV_MATERIALDOCUMENT TYPE BAPI2017_GM_HEAD_RET-MAT_DOC,
*         LV_MATDOCUMENTYEAR  TYPE BAPI2017_GM_HEAD_RET-DOC_YEAR,
*         LS_GOODSMVT_ITEM    TYPE BAPI2017_GM_ITEM_CREATE,
*         LT_GOODSMVT_ITEM    TYPE TABLE OF BAPI2017_GM_ITEM_CREATE,
*         LT_RETURN           TYPE TABLE OF BAPIRET2.
*
*  DATA: LV_ZEILE     TYPE ZTMM40920-ZEILE,
*        LS_ZTMM40920 TYPE ZTMM40920,
*        LT_ZTMM40920 TYPE TABLE OF ZTMM40920,
*        LS_ZTMM40930 TYPE ZTMM40930,
*        LT_ZTMM40930 TYPE TABLE OF ZTMM40930,
*        LS_ZTMM40940 TYPE ZTMM40940,
*        LT_ZTMM40940 TYPE TABLE OF ZTMM40940,
*        LS_ZTMM40950 TYPE ZTMM40950,
*        LT_ZTMM40950 TYPE TABLE OF ZTMM40950.
*
*  IF LT_DISP[] IS NOT INITIAL.
*    _G_INIT : LT_GOODSMVT_ITEM, LT_RETURN.
*
*    LOOP AT LT_DISP INTO DATA(LS_DISP).
*
*      CLEAR : LS_GOODSMVT_HEADER, LS_GOODSMVT_CODE, LS_GOODSMVT_ITEM,
*              LV_MATERIALDOCUMENT, LV_MATDOCUMENTYEAR.
*
**- 공통
*      LS_GOODSMVT_HEADER-PSTNG_DATE = P_BUDAT.
*      LS_GOODSMVT_HEADER-DOC_DATE   = P_BUDAT.
*      LS_GOODSMVT_HEADER-PR_UNAME   = SY-UNAME.
*      LS_GOODSMVT_HEADER-HEADER_TXT = TEXT-T13.     "기초재고 Migration
*      LS_GOODSMVT_CODE-GM_CODE    = '05'.
*      LS_GOODSMVT_ITEM-MATERIAL_LONG    = LS_DISP-MATNR.
*      LS_GOODSMVT_ITEM-PLANT            = LS_DISP-WERKS.
*      LS_GOODSMVT_ITEM-BATCH            = LS_DISP-CHARG.
*      LS_GOODSMVT_ITEM-MOVE_TYPE        = '562'.
*      LS_GOODSMVT_ITEM-STCK_TYPE        = LS_DISP-INSMK.
*      LS_GOODSMVT_ITEM-VAL_TYPE         = LS_DISP-BWTAR.
*      LS_GOODSMVT_ITEM-ENTRY_QNT        = LS_DISP-ERFMG.
*      LS_GOODSMVT_ITEM-ENTRY_UOM        = LS_DISP-ZMEINS.
*
*      IF LS_DISP-VPRSV = 'V'.
*        LS_GOODSMVT_ITEM-AMOUNT_LC        = LS_DISP-EXBWR.
*      ENDIF.
**-
*      CASE 'X'.
*        WHEN P_RD1B.  "창고
*          LS_GOODSMVT_ITEM-STGE_LOC         = LS_DISP-LGORT.
*        WHEN P_RD1C.  "사급
*          LS_GOODSMVT_ITEM-SPEC_STOCK    = 'O'.
*          LS_GOODSMVT_ITEM-VENDOR        = LS_DISP-LIFNR.
*        WHEN P_RD1D.  "위탁
*          LS_GOODSMVT_ITEM-SPEC_STOCK    = 'K'.
*          LS_GOODSMVT_ITEM-VENDOR        = LS_DISP-LIFNR.
*          LS_GOODSMVT_ITEM-STGE_LOC         = LS_DISP-LGORT.
*        WHEN P_RD1E.  "고객
*          LS_GOODSMVT_ITEM-SPEC_STOCK    = 'E'.
*          LS_GOODSMVT_ITEM-CUSTOMER      = LS_DISP-KUNNR.
*          LS_GOODSMVT_ITEM-SALES_ORD     = LS_DISP-VBELN.
*          LS_GOODSMVT_ITEM-S_ORD_ITEM    = LS_DISP-POSNR.
*          LS_GOODSMVT_ITEM-STGE_LOC         = LS_DISP-LGORT.
*      ENDCASE.
*
*      APPEND LS_GOODSMVT_ITEM TO LT_GOODSMVT_ITEM. CLEAR : LS_GOODSMVT_ITEM.
*
*    ENDLOOP.
*
*
*    IF LT_GOODSMVT_ITEM[] IS NOT INITIAL.
*
*      CALL FUNCTION 'BAPI_GOODSMVT_CREATE'
*        EXPORTING
*          GOODSMVT_HEADER  = LS_GOODSMVT_HEADER
*          GOODSMVT_CODE    = LS_GOODSMVT_CODE
*        IMPORTING
*          MATERIALDOCUMENT = LV_MATERIALDOCUMENT
*          MATDOCUMENTYEAR  = LV_MATDOCUMENTYEAR
*        TABLES
*          GOODSMVT_ITEM    = LT_GOODSMVT_ITEM
*          RETURN           = LT_RETURN.
*    ENDIF.
*
*    IF LV_MATERIALDOCUMENT IS NOT INITIAL AND LV_MATDOCUMENTYEAR IS NOT INITIAL.
*      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
*        EXPORTING
*          WAIT = 'X'.
*
*      CLEAR : LV_ZEILE, LS_ZTMM40920, LS_ZTMM40930, LS_ZTMM40940, LS_ZTMM40950.
*      _G_INIT : LT_ZTMM40920, LT_ZTMM40930, LT_ZTMM40940, LT_ZTMM40950.
*
*      LV_ZEILE = GC_0001.
*
*      LOOP AT LT_DISP INTO DATA(LS_DISP_S).
*
*        LS_DISP_S-STATUS = ICON_LED_GREEN.
*        LS_DISP_S-ZMESSAGE = '자재문서' && LV_MATERIALDOCUMENT && '로 마이그레이션 취소 완료.'.
*
*        CASE 'X'.
*          WHEN P_RD1B.  "창고 Migration
*            MOVE-CORRESPONDING LS_DISP_S TO LS_ZTMM40920.
*            LS_ZTMM40920-BUKRS = P_BUKRS.
*            LS_ZTMM40920-MJAHR_R = LV_MATDOCUMENTYEAR.
*            LS_ZTMM40920-MBLNR_R = LV_MATERIALDOCUMENT.
*            LS_ZTMM40920-ZEILE_R = LV_ZEILE.
*            LS_ZTMM40920-BUDAT = P_BUDAT.
*            LS_ZTMM40920-ZSTATUS = 'S'.
*            LS_ZTMM40920-AEDAT = SY-DATUM.
*            LS_ZTMM40920-AEZET = SY-UZEIT.
*            LS_ZTMM40920-AENAM = SY-UNAME.
*
*            APPEND LS_ZTMM40920 TO LT_ZTMM40920. CLEAR LS_ZTMM40920.
*
*          WHEN P_RD1C.  "사급 Migration
*            MOVE-CORRESPONDING LS_DISP_S TO LS_ZTMM40930.
*            LS_ZTMM40930-BUKRS = P_BUKRS.
*            LS_ZTMM40930-MJAHR_R = LV_MATDOCUMENTYEAR.
*            LS_ZTMM40930-MBLNR_R = LV_MATERIALDOCUMENT.
*            LS_ZTMM40930-ZEILE_R = LV_ZEILE.
*            LS_ZTMM40930-BUDAT = P_BUDAT.
*            LS_ZTMM40930-ZSTATUS = 'S'.
*            LS_ZTMM40930-AEDAT = SY-DATUM.
*            LS_ZTMM40930-AEZET = SY-UZEIT.
*            LS_ZTMM40930-AENAM = SY-UNAME.
*
*            APPEND LS_ZTMM40930 TO LT_ZTMM40930. CLEAR LS_ZTMM40930.
*
*          WHEN P_RD1D.  "위탁 Migration
*            MOVE-CORRESPONDING LS_DISP_S TO LS_ZTMM40940.
*            LS_ZTMM40940-BUKRS = P_BUKRS.
*            LS_ZTMM40940-MJAHR_R = LV_MATDOCUMENTYEAR.
*            LS_ZTMM40940-MBLNR_R = LV_MATERIALDOCUMENT.
*            LS_ZTMM40940-ZEILE_R = LV_ZEILE.
*            LS_ZTMM40940-BUDAT = P_BUDAT.
*            LS_ZTMM40940-ZSTATUS = 'S'.
*            LS_ZTMM40940-AEDAT = SY-DATUM.
*            LS_ZTMM40940-AEZET = SY-UZEIT.
*            LS_ZTMM40940-AENAM = SY-UNAME.
*
*            APPEND LS_ZTMM40940 TO LT_ZTMM40940. CLEAR LS_ZTMM40940.
*
*          WHEN P_RD1E.  "고객 Migration
*            MOVE-CORRESPONDING LS_DISP_S TO LS_ZTMM40950.
*            LS_ZTMM40950-BUKRS = P_BUKRS.
*            LS_ZTMM40950-MJAHR_R = LV_MATDOCUMENTYEAR.
*            LS_ZTMM40950-MBLNR_R = LV_MATERIALDOCUMENT.
*            LS_ZTMM40950-ZEILE_R = LV_ZEILE.
*            LS_ZTMM40950-BUDAT = P_BUDAT.
*            LS_ZTMM40950-ZSTATUS = 'S'.
*            LS_ZTMM40950-AEDAT = SY-DATUM.
*            LS_ZTMM40950-AEZET = SY-UZEIT.
*            LS_ZTMM40950-AENAM = SY-UNAME.
*
*            APPEND LS_ZTMM40950 TO LT_ZTMM40950. CLEAR LS_ZTMM40950.
*
*            LV_ZEILE = LV_ZEILE + 1.
*        ENDCASE.
*      ENDLOOP.
*    ELSE.
*
*      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
*      READ TABLE LT_RETURN INTO DATA(LS_RETURN) INDEX 1.
*
*      LOOP AT LT_DISP INTO DATA(LS_DISP_E).
*        LS_DISP_E-STATUS = ICON_LED_RED.
*        LS_DISP_E-ZMESSAGE = 'MIGRATION 취소 실패.' && LS_RETURN-MESSAGE.
*
*        CASE 'X'.
*          WHEN P_RD1B.  "창고 Migration
*            MOVE-CORRESPONDING LS_DISP_E TO LS_ZTMM40920.
*            LS_ZTMM40920-BUKRS = P_BUKRS.
*            LS_ZTMM40920-ZSTATUS = 'E'.
*            LS_ZTMM40920-AEDAT = SY-DATUM.
*            LS_ZTMM40920-AEZET = SY-UZEIT.
*            LS_ZTMM40920-AENAM = SY-UNAME.
*
*            APPEND LS_ZTMM40920 TO LT_ZTMM40920. CLEAR LS_ZTMM40920.
*
*          WHEN P_RD1C.  "사급 Migration
*            MOVE-CORRESPONDING LS_DISP_E TO LS_ZTMM40930.
*            LS_ZTMM40930-BUKRS = P_BUKRS.
*            LS_ZTMM40930-ZSTATUS = 'E'.
*            LS_ZTMM40930-AEDAT = SY-DATUM.
*            LS_ZTMM40930-AEZET = SY-UZEIT.
*            LS_ZTMM40930-AENAM = SY-UNAME.
*
*            APPEND LS_ZTMM40930 TO LT_ZTMM40930. CLEAR LS_ZTMM40930.
*
*          WHEN P_RD1D.  "위탁 Migration
*            MOVE-CORRESPONDING LS_DISP_E TO LS_ZTMM40940.
*            LS_ZTMM40940-BUKRS = P_BUKRS.
*            LS_ZTMM40940-ZSTATUS = 'E'.
*            LS_ZTMM40940-AEDAT = SY-DATUM.
*            LS_ZTMM40940-AEZET = SY-UZEIT.
*            LS_ZTMM40940-AENAM = SY-UNAME.
*
*            APPEND LS_ZTMM40940 TO LT_ZTMM40940. CLEAR LS_ZTMM40940.
*
*          WHEN P_RD1E.  "고객 Migration
*            MOVE-CORRESPONDING LS_DISP_E TO LS_ZTMM40950.
*            LS_ZTMM40950-BUKRS = P_BUKRS.
*            LS_ZTMM40950-ZSTATUS = 'E'.
*            LS_ZTMM40950-AEDAT = SY-DATUM.
*            LS_ZTMM40950-AEZET = SY-UZEIT.
*            LS_ZTMM40950-AENAM = SY-UNAME.
*
*            APPEND LS_ZTMM40950 TO LT_ZTMM40950. CLEAR LS_ZTMM40950.
*
*        ENDCASE.
*      ENDLOOP.
*    ENDIF.
*
*    CASE 'X'.
*      WHEN P_RD1B. " 창고 Migration
*        MODIFY ZTMM40920 FROM TABLE LT_ZTMM40920.
*        PERFORM REFRESH_DATA_B.
*
*      WHEN P_RD1C. " 사급 Migration
*        MODIFY ZTMM40930 FROM TABLE LT_ZTMM40930.
*        PERFORM REFRESH_DATA_C.
*
*      WHEN P_RD1D. " 위탁 Migration
*        MODIFY ZTMM40940 FROM TABLE LT_ZTMM40940.
*        PERFORM REFRESH_DATA_D.
*
*      WHEN P_RD1E. " 고객 Migration
*        MODIFY ZTMM40950 FROM TABLE LT_ZTMM40950.
*        PERFORM REFRESH_DATA_E.
*
*    ENDCASE.
*
*  ENDIF.
*
*  GRF_GRID->REFRESH_GRID_DISPLAY( ).
*ENDFORM.
*&---------------------------------------------------------------------*
*& Form UPDATE_DATA_RESULT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
*FORM UPDATE_SUCCESS_RESULT USING IV_MATDOCUMENTYEAR
*                                 IV_MATERIALDOCUMENT.
*
*  DATA: LV_ZEILE     TYPE ZTMM40920-ZEILE,
*        LS_ZTMM40920 TYPE ZTMM40920,
*        LT_ZTMM40920 TYPE TABLE OF ZTMM40920,
*        LS_ZTMM40930 TYPE ZTMM40930,
*        LT_ZTMM40930 TYPE TABLE OF ZTMM40930,
*        LS_ZTMM40940 TYPE ZTMM40940,
*        LT_ZTMM40940 TYPE TABLE OF ZTMM40940,
*        LS_ZTMM40950 TYPE ZTMM40950,
*        LT_ZTMM40950 TYPE TABLE OF ZTMM40950.
*
*
*  CLEAR : LV_ZEILE, LS_ZTMM40920, LS_ZTMM40930, LS_ZTMM40940, LS_ZTMM40950.
*  _G_INIT : LT_ZTMM40920, LT_ZTMM40930, LT_ZTMM40940, LT_ZTMM40950.
*
*  LV_ZEILE = GC_0001.
*
*  LOOP AT GT_DISP INTO DATA(LS_DISP_S).
*
*    LS_DISP_S-STATUS = ICON_LED_GREEN.
*    LS_DISP_S-ZMESSAGE = '자재문서' && IV_MATERIALDOCUMENT && '로 마이그레이션 완료.'.
*
*    CASE 'X'.
*      WHEN P_RD1B.  "창고재고 Migration
*
*        MOVE-CORRESPONDING LS_DISP_S TO LS_ZTMM40920.
**        LS_ZTMM40920-BUKRS = P_BUKRS.
**        LS_ZTMM40920-WERKS = P_WERKS.
**        LS_ZTMM40920-LGORT = P_LGORT.
*        LS_ZTMM40920-ERNAM = SY-UNAME.
*        LS_ZTMM40920-MJAHR = IV_MATDOCUMENTYEAR.
*        LS_ZTMM40920-MBLNR = IV_MATERIALDOCUMENT.
*        IF SY-TABIX = 1.
*          LS_ZTMM40920-ZEILE = LV_ZEILE.
*        ELSE.
*          LV_ZEILE = LV_ZEILE + 1.
*          LS_ZTMM40920-ZEILE = LV_ZEILE.
*        ENDIF.
*        LS_ZTMM40920-BUDAT = P_BUDAT.
*        LS_ZTMM40920-ZSTATUS = 'S'.
*        LS_ZTMM40920-ERDAT = SY-DATUM.
*        LS_ZTMM40920-ERZET = SY-UZEIT.
*        LS_ZTMM40920-ERNAM = SY-UNAME.
*        LS_ZTMM40920-AEDAT = SY-DATUM.
*        LS_ZTMM40920-AEZET = SY-UZEIT.
*        LS_ZTMM40920-AENAM = SY-UNAME.
*
*        APPEND LS_ZTMM40920 TO LT_ZTMM40920. CLEAR LS_ZTMM40920.
*
*      WHEN P_RD1C.  "사급재고 Migration
*
*        MOVE-CORRESPONDING LS_DISP_S TO LS_ZTMM40930.
*        LS_ZTMM40930-BUKRS = P_BUKRS.
**        LS_ZTMM40930-WERKS = P_WERKS.
**        LS_ZTMM40930-LIFNR = P_LIFNR.
*        LS_ZTMM40930-ERNAM = SY-UNAME.
*        LS_ZTMM40930-MJAHR = IV_MATDOCUMENTYEAR.
*        LS_ZTMM40930-MBLNR = IV_MATERIALDOCUMENT.
*        IF SY-TABIX = 1.
*          LS_ZTMM40930-ZEILE = LV_ZEILE.
*        ELSE.
*          LV_ZEILE = LV_ZEILE + 1.
*          LS_ZTMM40930-ZEILE = LV_ZEILE.
*        ENDIF.
*        LS_ZTMM40930-BUDAT = P_BUDAT.
*        LS_ZTMM40930-ZSTATUS = 'S'.
*        LS_ZTMM40930-ERDAT = SY-DATUM.
*        LS_ZTMM40930-ERZET = SY-UZEIT.
*        LS_ZTMM40930-ERNAM = SY-UNAME.
*        LS_ZTMM40930-AEDAT = SY-DATUM.
*        LS_ZTMM40930-AEZET = SY-UZEIT.
*        LS_ZTMM40930-AENAM = SY-UNAME.
*
*        APPEND LS_ZTMM40930 TO LT_ZTMM40930. CLEAR LS_ZTMM40930.
*
*      WHEN P_RD1D.  "위탁재고 Migration
*
*        MOVE-CORRESPONDING LS_DISP_S TO LS_ZTMM40940.
*        LS_ZTMM40940-BUKRS = P_BUKRS.
**        LS_ZTMM40940-WERKS = P_WERKS.
**        LS_ZTMM40940-LGORT = P_LGORT.
**        LS_ZTMM40940-LIFNR = P_LIFNR.
*        LS_ZTMM40940-ERNAM = SY-UNAME.
*        LS_ZTMM40940-MJAHR = IV_MATDOCUMENTYEAR.
*        LS_ZTMM40940-MBLNR = IV_MATERIALDOCUMENT.
*        IF SY-TABIX = 1.
*          LS_ZTMM40940-ZEILE = LV_ZEILE.
*        ELSE.
*          LV_ZEILE = LV_ZEILE + 1.
*          LS_ZTMM40940-ZEILE = LV_ZEILE.
*        ENDIF.
*        LS_ZTMM40940-BUDAT = P_BUDAT.
*        LS_ZTMM40940-ZSTATUS = 'S'.
*        LS_ZTMM40940-ERDAT = SY-DATUM.
*        LS_ZTMM40940-ERZET = SY-UZEIT.
*        LS_ZTMM40940-ERNAM = SY-UNAME.
*        LS_ZTMM40940-AEDAT = SY-DATUM.
*        LS_ZTMM40940-AEZET = SY-UZEIT.
*        LS_ZTMM40940-AENAM = SY-UNAME.
*
*        APPEND LS_ZTMM40940 TO LT_ZTMM40940. CLEAR LS_ZTMM40940.
*
*      WHEN P_RD1E.  "고객재고 Migration
*
*        MOVE-CORRESPONDING LS_DISP_S TO LS_ZTMM40950.
*        LS_ZTMM40950-BUKRS = P_BUKRS.
**        LS_ZTMM40950-WERKS = P_WERKS.
**        LS_ZTMM40950-LGORT = P_LGORT.
**        LS_ZTMM40950-KUNNR = P_KUNNR.
*        LS_ZTMM40950-ERNAM = SY-UNAME.
*        LS_ZTMM40950-MJAHR = IV_MATDOCUMENTYEAR.
*        LS_ZTMM40950-MBLNR = IV_MATERIALDOCUMENT.
*        IF SY-TABIX = 1.
*          LS_ZTMM40950-ZEILE = LV_ZEILE.
*        ELSE.
*          LV_ZEILE = LV_ZEILE + 1.
*          LS_ZTMM40950-ZEILE = LV_ZEILE.
*        ENDIF.
*        LS_ZTMM40950-BUDAT = P_BUDAT.
*        LS_ZTMM40950-ZSTATUS = 'S'.
*        LS_ZTMM40950-ERDAT = SY-DATUM.
*        LS_ZTMM40950-ERZET = SY-UZEIT.
*        LS_ZTMM40950-ERNAM = SY-UNAME.
*        LS_ZTMM40950-AEDAT = SY-DATUM.
*        LS_ZTMM40950-AEZET = SY-UZEIT.
*        LS_ZTMM40950-AENAM = SY-UNAME.
*
*        APPEND LS_ZTMM40950 TO LT_ZTMM40950. CLEAR LS_ZTMM40950.
*    ENDCASE.
*  ENDLOOP.
*
*  CASE 'X'.
*    WHEN P_RD1B.
*      MODIFY ZTMM40920 FROM TABLE LT_ZTMM40920.
*    WHEN P_RD1C.
*      MODIFY ZTMM40930 FROM TABLE LT_ZTMM40930.
*    WHEN P_RD1D.
*      MODIFY ZTMM40940 FROM TABLE LT_ZTMM40940.
*    WHEN P_RD1E.
*      MODIFY ZTMM40950 FROM TABLE LT_ZTMM40950.
*  ENDCASE.
*
*ENDFORM.
*&---------------------------------------------------------------------*
*& Form UPDATE_DATA_RESULT_E
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LS_RETURN
*&---------------------------------------------------------------------*
*FORM UPDATE_FAIL_RESULT  USING IS_RETURN-MESSAGE.
*
*  DATA: LS_ZTMM40920 TYPE ZTMM40920,
*        LT_ZTMM40920 TYPE TABLE OF ZTMM40920,
*        LS_ZTMM40930 TYPE ZTMM40930,
*        LT_ZTMM40930 TYPE TABLE OF ZTMM40930,
*        LS_ZTMM40940 TYPE ZTMM40940,
*        LT_ZTMM40940 TYPE TABLE OF ZTMM40940,
*        LS_ZTMM40950 TYPE ZTMM40950,
*        LT_ZTMM40950 TYPE TABLE OF ZTMM40950.
*
*
*  CLEAR : LS_ZTMM40920, LS_ZTMM40930, LS_ZTMM40940, LS_ZTMM40950.
*  _G_INIT : LT_ZTMM40920, LT_ZTMM40930, LT_ZTMM40940, LT_ZTMM40950.
*
*  LOOP AT GT_DISP INTO DATA(LS_DISP_E).
*    LS_DISP_E-STATUS = ICON_LED_RED.
*    LS_DISP_E-ZMESSAGE = 'MIGRATION 실패.' && IS_RETURN-MESSAGE.
*
*    CASE 'X'.
*      WHEN P_RD1B.
*        MOVE-CORRESPONDING LS_DISP_E TO LS_ZTMM40920.
*        LS_ZTMM40920-BUKRS = P_BUKRS.
*        LS_ZTMM40920-WERKS = P_WERKS.
**        LS_ZTMM40920-LGORT = P_LGORT.
*        LS_ZTMM40920-ERNAM = SY-UNAME.
*        LS_ZTMM40920-ZSTATUS = 'E'.
*        LS_ZTMM40920-ERDAT = SY-DATUM.
*        LS_ZTMM40920-ERZET = SY-UZEIT.
*        LS_ZTMM40920-ERNAM = SY-UNAME.
*        LS_ZTMM40920-AEDAT = SY-DATUM.
*        LS_ZTMM40920-AEZET = SY-UZEIT.
*        LS_ZTMM40920-AENAM = SY-UNAME.
*
*        APPEND LS_ZTMM40920 TO LT_ZTMM40920. CLEAR LS_ZTMM40920.
*
*      WHEN P_RD1C.
*        MOVE-CORRESPONDING LS_DISP_E TO LS_ZTMM40930.
*        LS_ZTMM40930-BUKRS = P_BUKRS.
*        LS_ZTMM40930-WERKS = P_WERKS.
**        LS_ZTMM40930-LIFNR = P_LIFNR.
*        LS_ZTMM40930-ERNAM = SY-UNAME.
*        LS_ZTMM40930-ZSTATUS = 'E'.
*        LS_ZTMM40930-ERDAT = SY-DATUM.
*        LS_ZTMM40930-ERZET = SY-UZEIT.
*        LS_ZTMM40930-ERNAM = SY-UNAME.
*        LS_ZTMM40930-AEDAT = SY-DATUM.
*        LS_ZTMM40930-AEZET = SY-UZEIT.
*        LS_ZTMM40930-AENAM = SY-UNAME.
*
*        APPEND LS_ZTMM40930 TO LT_ZTMM40930. CLEAR LS_ZTMM40930.
*
*      WHEN P_RD1D.
*        MOVE-CORRESPONDING LS_DISP_E TO LS_ZTMM40940.
*        LS_ZTMM40940-BUKRS = P_BUKRS.
*        LS_ZTMM40940-WERKS = P_WERKS.
**        LS_ZTMM40940-LGORT = P_LGORT.
**        LS_ZTMM40940-LIFNR = P_LIFNR.
*        LS_ZTMM40940-ERNAM = SY-UNAME.
*        LS_ZTMM40940-ZSTATUS = 'E'.
*        LS_ZTMM40940-ERDAT = SY-DATUM.
*        LS_ZTMM40940-ERZET = SY-UZEIT.
*        LS_ZTMM40940-ERNAM = SY-UNAME.
*        LS_ZTMM40940-AEDAT = SY-DATUM.
*        LS_ZTMM40940-AEZET = SY-UZEIT.
*        LS_ZTMM40940-AENAM = SY-UNAME.
*
*        APPEND LS_ZTMM40940 TO LT_ZTMM40940. CLEAR LS_ZTMM40940.
*
*      WHEN P_RD1E.
*        MOVE-CORRESPONDING LS_DISP_E TO LS_ZTMM40950.
*        LS_ZTMM40950-BUKRS = P_BUKRS.
*        LS_ZTMM40950-WERKS = P_WERKS.
**        LS_ZTMM40950-LGORT = P_LGORT.
**        LS_ZTMM40950-KUNNR = P_KUNNR.
*        LS_ZTMM40950-ERNAM = SY-UNAME.
*        LS_ZTMM40950-ZSTATUS = 'E'.
*        LS_ZTMM40950-ERDAT = SY-DATUM.
*        LS_ZTMM40950-ERZET = SY-UZEIT.
*        LS_ZTMM40950-ERNAM = SY-UNAME.
*        LS_ZTMM40950-AEDAT = SY-DATUM.
*        LS_ZTMM40950-AEZET = SY-UZEIT.
*        LS_ZTMM40950-AENAM = SY-UNAME.
*
*        APPEND LS_ZTMM40950 TO LT_ZTMM40950. CLEAR LS_ZTMM40950.
*
*    ENDCASE.
*  ENDLOOP.
*
*  CASE 'X'.
*    WHEN P_RD1B.
*      MODIFY ZTMM40920 FROM TABLE LT_ZTMM40920.
*    WHEN P_RD1C.
*      MODIFY ZTMM40930 FROM TABLE LT_ZTMM40930.
*    WHEN P_RD1D.
*      MODIFY ZTMM40940 FROM TABLE LT_ZTMM40940.
*    WHEN P_RD1E.
*      MODIFY ZTMM40950 FROM TABLE LT_ZTMM40950.
*  ENDCASE.
*
*ENDFORM.
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

  DATA : LT_ZTMM40930 TYPE TABLE OF ZTMM40930,
         LS_DISP      TYPE TS_DISP.


  CASE 'X'.
    WHEN P_RD3A.    "전체

      SELECT FROM ZTMM40930
        FIELDS BUKRS, WERKS, LIFNR, ZSEQ, MATNR, BESKZ, SOBSL, MMSTA, BKLAS, XCHPF, CHARG, BWTTY, BWTAR, VPRSV, BWART,
               ERFMG, MEINS, ZMEINS, INSMK, EXBWR, WAERS, MJAHR, MBLNR, ZEILE, BUDAT, MJAHR_R, MBLNR_R, ZEILE_R, ZSTATUS,
               ZMESSAGE, ERDAT, ERZET, ERNAM, AEDAT, AEZET, AENAM, ZDELE

        WHERE BUKRS = @P_BUKRS
          AND MATNR IN @S_MATNR
          AND WERKS IN @S_WERKS
          AND LIFNR IN @S_LIFNR
          AND ERDAT IN @S_ERDAT
          AND ERNAM IN @S_ERNAM

        APPENDING CORRESPONDING FIELDS OF TABLE @LT_ZTMM40930.

    WHEN P_RD3B.    "성공

      SELECT FROM ZTMM40930
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

        APPENDING CORRESPONDING FIELDS OF TABLE @LT_ZTMM40930.

    WHEN P_RD3C.    "실패

      SELECT FROM ZTMM40930
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

        APPENDING CORRESPONDING FIELDS OF TABLE @LT_ZTMM40930.

  ENDCASE.

  IF LT_ZTMM40930[] IS NOT INITIAL.

    DATA(LT_SUB) = LT_ZTMM40930[].
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

      LT_SUB[] = LT_ZTMM40930[].
      SORT LT_SUB BY LIFNR.
      DELETE ADJACENT DUPLICATES FROM LT_SUB COMPARING LIFNR.

      SELECT FROM LFA1
        FIELDS LIFNR, NAME1
        FOR ALL ENTRIES IN @LT_SUB
        WHERE LIFNR = @LT_SUB-LIFNR
        INTO TABLE @DATA(LT_BP).

      FREE LT_SUB.

    ENDIF.

    LOOP AT LT_ZTMM40930 INTO DATA(LS_ZTMM40930).
      MOVE-CORRESPONDING LS_ZTMM40930 TO LS_DISP.

      IF LS_ZTMM40930-ZSTATUS = 'S'.
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

  DATA : LT_ZTMM40940 TYPE TABLE OF ZTMM40940,
         LS_DISP      TYPE TS_DISP.


  CASE 'X'.
    WHEN P_RD3A.    "전체

      SELECT FROM ZTMM40940
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

        APPENDING CORRESPONDING FIELDS OF TABLE @LT_ZTMM40940.

    WHEN P_RD3B.    "성공

      SELECT FROM ZTMM40940
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

        APPENDING CORRESPONDING FIELDS OF TABLE @LT_ZTMM40940.

    WHEN P_RD3C.    "실패

      SELECT FROM ZTMM40940
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

        APPENDING CORRESPONDING FIELDS OF TABLE @LT_ZTMM40940.

  ENDCASE.


  IF LT_ZTMM40940[] IS NOT INITIAL.

    DATA(LT_SUB) = LT_ZTMM40940[].
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

      LT_SUB[] = LT_ZTMM40940[].
      SORT LT_SUB BY LIFNR.
      DELETE ADJACENT DUPLICATES FROM LT_SUB COMPARING LIFNR.

      SELECT FROM LFA1
        FIELDS LIFNR, NAME1
        FOR ALL ENTRIES IN @LT_SUB
        WHERE LIFNR = @LT_SUB-LIFNR
        INTO TABLE @DATA(LT_BP).

      FREE LT_SUB.

    ENDIF.

    LOOP AT LT_ZTMM40940 INTO DATA(LS_ZTMM40940).
      MOVE-CORRESPONDING LS_ZTMM40940 TO LS_DISP.

      IF LS_ZTMM40940-ZSTATUS = 'S'.
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

  DATA : LT_ZTMM40950 TYPE TABLE OF ZTMM40950,
         LS_DISP      TYPE TS_DISP.


  CASE 'X'.
    WHEN P_RD3A.    "전체

      SELECT FROM ZTMM40950
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

        APPENDING CORRESPONDING FIELDS OF TABLE @LT_ZTMM40950.

    WHEN P_RD3B.    "성공

      SELECT FROM ZTMM40950
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

        APPENDING CORRESPONDING FIELDS OF TABLE @LT_ZTMM40950.

    WHEN P_RD3C.    "실패

      SELECT FROM ZTMM40950
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

        APPENDING CORRESPONDING FIELDS OF TABLE @LT_ZTMM40950.

  ENDCASE.


  IF LT_ZTMM40950[] IS NOT INITIAL.

    DATA(LT_SUB) = LT_ZTMM40950[].
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

    LOOP AT LT_ZTMM40950 INTO DATA(LS_ZTMM40950).
      MOVE-CORRESPONDING LS_ZTMM40950 TO LS_DISP.

      IF LS_ZTMM40950-ZSTATUS = 'S'.
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
*& Form DUP_CHECK_2101
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
*FORM DUP_CHECK_2101 .
*
*  DATA : LT_BATCH TYPE TABLE OF TS_BATCH.
*
*  IF P_BUKRS = GC_2101.
*    LOOP AT GT_BATCH INTO DATA(LS_BATCH) WHERE ZCHECK = 'X'.
*      APPEND LS_BATCH TO LT_BATCH. CLEAR LS_BATCH.
*    ENDLOOP.
*  ENDIF.
*
*  IF LT_BATCH[] IS NOT INITIAL.
*
*    DATA(LT_BATCH2) = LT_BATCH[].
*    SORT LT_BATCH BY MATNR EX_CHARG LIFNR HSDAT VFDAT LICHA ZMM_MAKER.
*    DELETE ADJACENT DUPLICATES FROM LT_BATCH COMPARING MATNR EX_CHARG LIFNR HSDAT VFDAT LICHA ZMM_MAKER.
*
*
*    IF SY-SUBRC = 0.
*
*      LOOP AT LT_BATCH2 INTO DATA(LS_SUB).
*
*        LOOP AT GT_BATCH ASSIGNING FIELD-SYMBOL(<LS_DUP>) WHERE MATNR = LS_SUB-MATNR
*                                                            AND EX_CHARG = LS_SUB-EX_CHARG
*                                                            AND LIFNR = LS_SUB-LIFNR
*                                                            AND HSDAT = LS_SUB-HSDAT
*                                                            AND VFDAT = LS_SUB-VFDAT
*                                                            AND LICHA = LS_SUB-LICHA
*                                                            AND ZMM_MAKER = LS_SUB-ZMM_MAKER.
**       IF  <LS_DUP>-CLASS  = GC_ZPP_KGC1  AND STRLEN( <LS_DUP>-EX_CHARG ) <> 10 and <LS_DUP>-ex_charg is not INITIAL.
**       else.
*
*          IF <LS_DUP>-DUP_CHECK IS INITIAL.
*            <LS_DUP>-DUP_CHECK = 1.
*          ELSE.
*            <LS_DUP>-DUP_CHECK = <LS_DUP>-DUP_CHECK + 1.
*          ENDIF.
**       endif.
*        ENDLOOP.
*
*      ENDLOOP.
*
*
*      LOOP AT GT_BATCH ASSIGNING FIELD-SYMBOL(<LS_BATCH>) WHERE DUP_CHECK > 1.
*        <LS_BATCH>-STATUS = ICON_LED_RED.
*        <LS_BATCH>-ZMESSAGE = TEXT-M37.       "중복 오류 : 이관 대상에 동일 정보가 존재합니다.(외부배치번호,공급업체,제조일,사용기한,제조LOT 기준)
*      ENDLOOP.
*
*    ENDIF.
*  ENDIF.
*
*ENDFORM.
*&---------------------------------------------------------------------*
*& Form REFRESH_DATA_B
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM REFRESH_DATA_B .


*  _G_INIT : GT_DISP.

  DATA : LT_ZTMM40920 TYPE TABLE OF ZTMM40920,
         LS_DISP      TYPE TS_DISP,
         LT_DISP      TYPE TABLE OF TS_DISP.


  CASE 'X'.
    WHEN P_RD3A.    "전체

      SELECT FROM ZTMM40920
        FIELDS BUKRS, WERKS, LGORT, ERNAM, ZSEQ, MATNR, BESKZ, SOBSL, MMSTA, BKLAS, XCHPF, CHARG, BWTTY,
               BWTAR, VPRSV, BWART, ERFMG, MEINS, ZMEINS, INSMK, EXBWR, WAERS, MJAHR, MBLNR, ZEILE, BUDAT,
               MJAHR_R, MBLNR_R, ZEILE_R, ZSTATUS, ZMESSAGE, ERDAT, ERZET, AEDAT, AEZET, AENAM

        WHERE BUKRS = @P_BUKRS
          AND MATNR IN @S_MATNR
          AND WERKS IN @S_WERKS
*          AND LGORT IN @S_LGORT
          AND ERDAT IN @S_ERDAT
          AND ERNAM IN @S_ERNAM

        APPENDING CORRESPONDING FIELDS OF TABLE @LT_ZTMM40920.

    WHEN P_RD3B.    "성공

      SELECT FROM ZTMM40920
        FIELDS BUKRS, WERKS, LGORT, ERNAM, ZSEQ, MATNR, BESKZ, SOBSL, MMSTA, BKLAS, XCHPF, CHARG, BWTTY,
               BWTAR, VPRSV, BWART, ERFMG, MEINS, ZMEINS, INSMK, EXBWR, WAERS, MJAHR, MBLNR, ZEILE, BUDAT,
               MJAHR_R, MBLNR_R, ZEILE_R, ZSTATUS, ZMESSAGE, ERDAT, ERZET, AEDAT, AEZET, AENAM

        WHERE BUKRS = @P_BUKRS
          AND MATNR IN @S_MATNR
          AND WERKS IN @S_WERKS
*          AND LGORT IN @S_LGORT
          AND ERDAT IN @S_ERDAT
          AND ERNAM IN @S_ERNAM
          AND ZSTATUS = 'S'

        APPENDING CORRESPONDING FIELDS OF TABLE @LT_ZTMM40920.

    WHEN P_RD3C.    "실패

      SELECT FROM ZTMM40920
        FIELDS BUKRS, WERKS, LGORT, ERNAM, ZSEQ, MATNR, BESKZ, SOBSL, MMSTA, BKLAS, XCHPF, CHARG, BWTTY,
               BWTAR, VPRSV, BWART, ERFMG, MEINS, ZMEINS, INSMK, EXBWR, WAERS, MJAHR, MBLNR, ZEILE, BUDAT,
               MJAHR_R, MBLNR_R, ZEILE_R, ZSTATUS, ZMESSAGE, ERDAT, ERZET, AEDAT, AEZET, AENAM

        WHERE BUKRS = @P_BUKRS
          AND MATNR IN @S_MATNR
          AND WERKS IN @S_WERKS
*          AND LGORT IN @S_LGORT
          AND ERDAT IN @S_ERDAT
          AND ERNAM IN @S_ERNAM
          AND ZSTATUS NE 'S'

        APPENDING CORRESPONDING FIELDS OF TABLE @LT_ZTMM40920.

  ENDCASE.

  IF LT_ZTMM40920[] IS NOT INITIAL.

    DATA(LT_SUB) = LT_ZTMM40920[].
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

    LOOP AT GT_DISP ASSIGNING FIELD-SYMBOL(<LS_DISP>).

      READ TABLE LT_ZTMM40920 INTO DATA(LS_ZTMM40920) WITH KEY MATNR = <LS_DISP>-MATNR
                                                               WERKS = <LS_DISP>-WERKS
                                                               LGORT = <LS_DISP>-LGORT
                                                               ZSEQ = <LS_DISP>-ZSEQ.
      IF LS_ZTMM40920 IS NOT INITIAL.
        MOVE-CORRESPONDING LS_ZTMM40920 TO LS_DISP.

        IF LS_ZTMM40920-ZSTATUS = 'S'.
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

        APPEND LS_DISP TO LT_DISP. CLEAR LS_DISP.
      ENDIF.

    ENDLOOP.

    GT_DISP[] = LT_DISP[].

  ENDIF.

  IF GT_DISP[] IS NOT INITIAL.
    SORT GT_DISP BY ZSEQ.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form REFRESH_DATA_C
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM REFRESH_DATA_C .

*  _G_INIT : GT_DISP.

  DATA : LT_ZTMM40930 TYPE TABLE OF ZTMM40930,
         LS_DISP      TYPE TS_DISP,
         LT_DISP      TYPE TABLE OF TS_DISP.


  CASE 'X'.
    WHEN P_RD3A.    "전체

      SELECT FROM ZTMM40930
        FIELDS BUKRS, WERKS, LIFNR, ZSEQ, MATNR, BESKZ, SOBSL, MMSTA, BKLAS, XCHPF, CHARG, BWTTY, BWTAR, VPRSV, BWART,
               ERFMG, MEINS, ZMEINS, INSMK, EXBWR, WAERS, MJAHR, MBLNR, ZEILE, BUDAT, MJAHR_R, MBLNR_R, ZEILE_R, ZSTATUS,
               ZMESSAGE, ERDAT, ERZET, ERNAM, AEDAT, AEZET, AENAM, ZDELE

        WHERE BUKRS = @P_BUKRS
          AND MATNR IN @S_MATNR
          AND WERKS IN @S_WERKS
          AND LIFNR IN @S_LIFNR
          AND ERDAT IN @S_ERDAT
          AND ERNAM IN @S_ERNAM

        APPENDING CORRESPONDING FIELDS OF TABLE @LT_ZTMM40930.

    WHEN P_RD3B.    "성공

      SELECT FROM ZTMM40930
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

        APPENDING CORRESPONDING FIELDS OF TABLE @LT_ZTMM40930.

    WHEN P_RD3C.    "실패

      SELECT FROM ZTMM40930
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

        APPENDING CORRESPONDING FIELDS OF TABLE @LT_ZTMM40930.

  ENDCASE.

  IF LT_ZTMM40930[] IS NOT INITIAL.

    DATA(LT_SUB) = LT_ZTMM40930[].
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

      LT_SUB[] = LT_ZTMM40930[].
      SORT LT_SUB BY LIFNR.
      DELETE ADJACENT DUPLICATES FROM LT_SUB COMPARING LIFNR.

      SELECT FROM LFA1
        FIELDS LIFNR, NAME1
        FOR ALL ENTRIES IN @LT_SUB
        WHERE LIFNR = @LT_SUB-LIFNR
        INTO TABLE @DATA(LT_BP).

      FREE LT_SUB.
    ENDIF.

    LOOP AT GT_DISP ASSIGNING FIELD-SYMBOL(<LS_DISP>).

      READ TABLE LT_ZTMM40930 INTO DATA(LS_ZTMM40930) WITH KEY BUKRS = P_BUKRS
                                                               WERKS = <LS_DISP>-WERKS
                                                               LIFNR = <LS_DISP>-LIFNR
                                                               ZSEQ = <LS_DISP>-ZSEQ.
      IF LS_ZTMM40930 IS NOT INITIAL.

        MOVE-CORRESPONDING LS_ZTMM40930 TO LS_DISP.

        IF LS_ZTMM40930-ZSTATUS = 'S'.
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

        APPEND LS_DISP TO LT_DISP. CLEAR LS_DISP.
      ENDIF.
    ENDLOOP.

    GT_DISP[] = LT_DISP[].

  ENDIF.

  IF GT_DISP[] IS NOT INITIAL.
    SORT GT_DISP BY ZSEQ.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form REFRESH_DATA_D
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM REFRESH_DATA_D .

*  _G_INIT : GT_DISP.

  DATA : LT_ZTMM40940 TYPE TABLE OF ZTMM40940,
         LS_DISP      TYPE TS_DISP,
         LT_DISP      TYPE TABLE OF TS_DISP.


  CASE 'X'.
    WHEN P_RD3A.    "전체

      SELECT FROM ZTMM40940
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

        APPENDING CORRESPONDING FIELDS OF TABLE @LT_ZTMM40940.

    WHEN P_RD3B.    "성공

      SELECT FROM ZTMM40940
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

        APPENDING CORRESPONDING FIELDS OF TABLE @LT_ZTMM40940.

    WHEN P_RD3C.    "실패

      SELECT FROM ZTMM40940
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

        APPENDING CORRESPONDING FIELDS OF TABLE @LT_ZTMM40940.

  ENDCASE.


  IF LT_ZTMM40940[] IS NOT INITIAL.

    DATA(LT_SUB) = LT_ZTMM40940[].
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

      LT_SUB[] = LT_ZTMM40940[].
      SORT LT_SUB BY LIFNR.
      DELETE ADJACENT DUPLICATES FROM LT_SUB COMPARING LIFNR.

      SELECT FROM LFA1
        FIELDS LIFNR, NAME1
        FOR ALL ENTRIES IN @LT_SUB
        WHERE LIFNR = @LT_SUB-LIFNR
        INTO TABLE @DATA(LT_BP).

      FREE LT_SUB.
    ENDIF.

    LOOP AT GT_DISP ASSIGNING FIELD-SYMBOL(<LS_DISP>).

      READ TABLE LT_ZTMM40940 INTO DATA(LS_ZTMM40940) WITH KEY MATNR = <LS_DISP>-MATNR
                                                               WERKS = <LS_DISP>-WERKS
                                                               LGORT = <LS_DISP>-LGORT
                                                               LIFNR = <LS_DISP>-LIFNR
                                                               ZSEQ = <LS_DISP>-ZSEQ.

      IF LS_ZTMM40940 IS NOT INITIAL.
        MOVE-CORRESPONDING LS_ZTMM40940 TO LS_DISP.

        IF LS_ZTMM40940-ZSTATUS = 'S'.
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

        APPEND LS_DISP TO LT_DISP. CLEAR LS_DISP.
      ENDIF.
    ENDLOOP.

    GT_DISP[] = LT_DISP[].

  ENDIF.

  IF GT_DISP[] IS NOT INITIAL.
    SORT GT_DISP BY ZSEQ.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form REFRESH_DATA_E
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM REFRESH_DATA_E .


*  _G_INIT : GT_DISP.

  DATA : LT_ZTMM40950 TYPE TABLE OF ZTMM40950,
         LS_DISP      TYPE TS_DISP,
         LT_DISP      TYPE TABLE OF TS_DISP.


  CASE 'X'.
    WHEN P_RD3A.    "전체

      SELECT FROM ZTMM40950
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

        APPENDING CORRESPONDING FIELDS OF TABLE @LT_ZTMM40950.

    WHEN P_RD3B.    "성공

      SELECT FROM ZTMM40950
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

        APPENDING CORRESPONDING FIELDS OF TABLE @LT_ZTMM40950.

    WHEN P_RD3C.    "실패

      SELECT FROM ZTMM40950
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

        APPENDING CORRESPONDING FIELDS OF TABLE @LT_ZTMM40950.

  ENDCASE.


  IF LT_ZTMM40950[] IS NOT INITIAL.

    DATA(LT_SUB) = LT_ZTMM40950[].
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

    LOOP AT GT_DISP ASSIGNING FIELD-SYMBOL(<LS_DISP>).

      READ TABLE LT_ZTMM40950 INTO DATA(LS_ZTMM40950) WITH KEY MATNR = <LS_DISP>-MATNR
                                                               WERKS = <LS_DISP>-WERKS
                                                               LGORT = <LS_DISP>-LGORT
                                                               KUNNR = <LS_DISP>-KUNNR
                                                               ZSEQ = <LS_DISP>-ZSEQ.
      IF LS_ZTMM40950 IS NOT INITIAL.

        MOVE-CORRESPONDING LS_ZTMM40950 TO LS_DISP.

        IF LS_ZTMM40950-ZSTATUS = 'S'.
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

        APPEND LS_DISP TO LT_DISP. CLEAR LS_DISP.

      ENDIF.

    ENDLOOP.

    GT_DISP[] = LT_DISP[].

  ENDIF.

  IF GT_DISP[] IS NOT INITIAL.
    SORT GT_DISP BY ZSEQ.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form UPDATE_SUCCESS_RESULT_DIV
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LV_MATDOCUMENTYEAR
*&      --> LV_MATERIALDOCUMENT
*&---------------------------------------------------------------------*
*FORM UPDATE_SUCCESS_RESULT_DIV  USING IV_MATDOCUMENTYEAR
*                                 IV_MATERIALDOCUMENT.
*
*  DATA: LV_ZEILE     TYPE ZTMM40920-ZEILE,
*        LS_ZTMM40920 TYPE ZTMM40920,
*        LS_ZTMM40930 TYPE ZTMM40930,
*        LS_ZTMM40940 TYPE ZTMM40940,
*        LS_ZTMM40950 TYPE ZTMM40950.
*
*  CLEAR : LV_ZEILE, LS_ZTMM40920, LS_ZTMM40930, LS_ZTMM40940, LS_ZTMM40950.
*
*  LV_ZEILE = GC_0001.
*
*  LOOP AT GT_DISP_SUB INTO DATA(LS_DISP_S).
*
*    LS_DISP_S-STATUS = ICON_LED_GREEN.
*    LS_DISP_S-ZMESSAGE = '자재문서' && IV_MATERIALDOCUMENT && '로 마이그레이션 완료.'.
*
*    CASE 'X'.
*      WHEN P_RD1B.  "창고재고 Migration
*
*        MOVE-CORRESPONDING LS_DISP_S TO LS_ZTMM40920.
*        LS_ZTMM40920-BUKRS = P_BUKRS.
*        LS_ZTMM40920-WERKS = P_WERKS.
**        LS_ZTMM40920-LGORT = P_LGORT.
*        LS_ZTMM40920-ERNAM = SY-UNAME.
*        LS_ZTMM40920-MJAHR = IV_MATDOCUMENTYEAR.
*        LS_ZTMM40920-MBLNR = IV_MATERIALDOCUMENT.
*        IF SY-TABIX = 1.
*          LS_ZTMM40920-ZEILE = LV_ZEILE.
*        ELSE.
*          LV_ZEILE = LV_ZEILE + 1.
*          LS_ZTMM40920-ZEILE = LV_ZEILE.
*        ENDIF.
*        LS_ZTMM40920-BUDAT = P_BUDAT.
*        LS_ZTMM40920-ZSTATUS = 'S'.
*        LS_ZTMM40920-ERDAT = SY-DATUM.
*        LS_ZTMM40920-ERZET = SY-UZEIT.
*        LS_ZTMM40920-ERNAM = SY-UNAME.
*        LS_ZTMM40920-AEDAT = SY-DATUM.
*        LS_ZTMM40920-AEZET = SY-UZEIT.
*        LS_ZTMM40920-AENAM = SY-UNAME.
*
*        APPEND LS_ZTMM40920 TO GT_ZTMM40920. CLEAR LS_ZTMM40920.
*
*      WHEN P_RD1C.  "사급재고 Migration
*
*        MOVE-CORRESPONDING LS_DISP_S TO LS_ZTMM40930.
*        LS_ZTMM40930-BUKRS = P_BUKRS.
*        LS_ZTMM40930-WERKS = P_WERKS.
**        LS_ZTMM40930-LIFNR = P_LIFNR.
*        LS_ZTMM40930-ERNAM = SY-UNAME.
*        LS_ZTMM40930-MJAHR = IV_MATDOCUMENTYEAR.
*        LS_ZTMM40930-MBLNR = IV_MATERIALDOCUMENT.
*        IF SY-TABIX = 1.
*          LS_ZTMM40930-ZEILE = LV_ZEILE.
*        ELSE.
*          LV_ZEILE = LV_ZEILE + 1.
*          LS_ZTMM40930-ZEILE = LV_ZEILE.
*        ENDIF.
*        LS_ZTMM40930-BUDAT = P_BUDAT.
*        LS_ZTMM40930-ZSTATUS = 'S'.
*        LS_ZTMM40930-ERDAT = SY-DATUM.
*        LS_ZTMM40930-ERZET = SY-UZEIT.
*        LS_ZTMM40930-ERNAM = SY-UNAME.
*        LS_ZTMM40930-AEDAT = SY-DATUM.
*        LS_ZTMM40930-AEZET = SY-UZEIT.
*        LS_ZTMM40930-AENAM = SY-UNAME.
*
*        APPEND LS_ZTMM40930 TO GT_ZTMM40930. CLEAR LS_ZTMM40930.
*
*      WHEN P_RD1D.  "위탁재고 Migration
*
*        MOVE-CORRESPONDING LS_DISP_S TO LS_ZTMM40940.
*        LS_ZTMM40940-BUKRS = P_BUKRS.
*        LS_ZTMM40940-WERKS = P_WERKS.
**        LS_ZTMM40940-LGORT = P_LGORT.
**        LS_ZTMM40940-LIFNR = P_LIFNR.
*        LS_ZTMM40940-ERNAM = SY-UNAME.
*        LS_ZTMM40940-MJAHR = IV_MATDOCUMENTYEAR.
*        LS_ZTMM40940-MBLNR = IV_MATERIALDOCUMENT.
*        IF SY-TABIX = 1.
*          LS_ZTMM40940-ZEILE = LV_ZEILE.
*        ELSE.
*          LV_ZEILE = LV_ZEILE + 1.
*          LS_ZTMM40940-ZEILE = LV_ZEILE.
*        ENDIF.
*        LS_ZTMM40940-BUDAT = P_BUDAT.
*        LS_ZTMM40940-ZSTATUS = 'S'.
*        LS_ZTMM40940-ERDAT = SY-DATUM.
*        LS_ZTMM40940-ERZET = SY-UZEIT.
*        LS_ZTMM40940-ERNAM = SY-UNAME.
*        LS_ZTMM40940-AEDAT = SY-DATUM.
*        LS_ZTMM40940-AEZET = SY-UZEIT.
*        LS_ZTMM40940-AENAM = SY-UNAME.
*
*        APPEND LS_ZTMM40940 TO GT_ZTMM40940. CLEAR LS_ZTMM40940.
*
*      WHEN P_RD1E.  "고객재고 Migration
*
*        MOVE-CORRESPONDING LS_DISP_S TO LS_ZTMM40950.
*        LS_ZTMM40950-BUKRS = P_BUKRS.
*        LS_ZTMM40950-WERKS = P_WERKS.
**        LS_ZTMM40950-LGORT = P_LGORT.
**        LS_ZTMM40950-KUNNR = P_KUNNR.
*        LS_ZTMM40950-ERNAM = SY-UNAME.
*        LS_ZTMM40950-MJAHR = IV_MATDOCUMENTYEAR.
*        LS_ZTMM40950-MBLNR = IV_MATERIALDOCUMENT.
*        IF SY-TABIX = 1.
*          LS_ZTMM40950-ZEILE = LV_ZEILE.
*        ELSE.
*          LV_ZEILE = LV_ZEILE + 1.
*          LS_ZTMM40950-ZEILE = LV_ZEILE.
*        ENDIF.
*        LS_ZTMM40950-BUDAT = P_BUDAT.
*        LS_ZTMM40950-ZSTATUS = 'S'.
*        LS_ZTMM40950-ERDAT = SY-DATUM.
*        LS_ZTMM40950-ERZET = SY-UZEIT.
*        LS_ZTMM40950-ERNAM = SY-UNAME.
*        LS_ZTMM40950-AEDAT = SY-DATUM.
*        LS_ZTMM40950-AEZET = SY-UZEIT.
*        LS_ZTMM40950-AENAM = SY-UNAME.
*
*        APPEND LS_ZTMM40950 TO GT_ZTMM40950. CLEAR LS_ZTMM40950.
*    ENDCASE.
*  ENDLOOP.
*
*ENDFORM.
*&---------------------------------------------------------------------*
*& Form UPDATE_FAIL_RESULT_DIV
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LS_RETURN_MESSAGE
*&---------------------------------------------------------------------*
*FORM UPDATE_FAIL_RESULT_DIV  USING IS_RETURN-MESSAGE.
*
*  DATA: LS_ZTMM40920 TYPE ZTMM40920,
*        LS_ZTMM40930 TYPE ZTMM40930,
*        LS_ZTMM40940 TYPE ZTMM40940,
*        LS_ZTMM40950 TYPE ZTMM40950.
*
*
*  CLEAR : LS_ZTMM40920, LS_ZTMM40930, LS_ZTMM40940, LS_ZTMM40950.
*
*  LOOP AT GT_DISP_SUB INTO DATA(LS_DISP_E).
*    LS_DISP_E-STATUS = ICON_LED_RED.
*    LS_DISP_E-ZMESSAGE = 'MIGRATION 실패.' && IS_RETURN-MESSAGE.
*
*    CASE 'X'.
*      WHEN P_RD1B.
*        MOVE-CORRESPONDING LS_DISP_E TO LS_ZTMM40920.
*        LS_ZTMM40920-BUKRS = P_BUKRS.
*        LS_ZTMM40920-WERKS = P_WERKS.
**        LS_ZTMM40920-LGORT = P_LGORT.
*        LS_ZTMM40920-ERNAM = SY-UNAME.
*        LS_ZTMM40920-ZSTATUS = 'E'.
*        LS_ZTMM40920-ERDAT = SY-DATUM.
*        LS_ZTMM40920-ERZET = SY-UZEIT.
*        LS_ZTMM40920-ERNAM = SY-UNAME.
*        LS_ZTMM40920-AEDAT = SY-DATUM.
*        LS_ZTMM40920-AEZET = SY-UZEIT.
*        LS_ZTMM40920-AENAM = SY-UNAME.
*
*        APPEND LS_ZTMM40920 TO GT_ZTMM40920. CLEAR LS_ZTMM40920.
*
*      WHEN P_RD1C.
*        MOVE-CORRESPONDING LS_DISP_E TO LS_ZTMM40930.
*        LS_ZTMM40930-BUKRS = P_BUKRS.
*        LS_ZTMM40930-WERKS = P_WERKS.
**        LS_ZTMM40930-LIFNR = P_LIFNR.
*        LS_ZTMM40930-ERNAM = SY-UNAME.
*        LS_ZTMM40930-ZSTATUS = 'E'.
*        LS_ZTMM40930-ERDAT = SY-DATUM.
*        LS_ZTMM40930-ERZET = SY-UZEIT.
*        LS_ZTMM40930-ERNAM = SY-UNAME.
*        LS_ZTMM40930-AEDAT = SY-DATUM.
*        LS_ZTMM40930-AEZET = SY-UZEIT.
*        LS_ZTMM40930-AENAM = SY-UNAME.
*
*        APPEND LS_ZTMM40930 TO GT_ZTMM40930. CLEAR LS_ZTMM40930.
*
*      WHEN P_RD1D.
*        MOVE-CORRESPONDING LS_DISP_E TO LS_ZTMM40940.
*        LS_ZTMM40940-BUKRS = P_BUKRS.
*        LS_ZTMM40940-WERKS = P_WERKS.
**        LS_ZTMM40940-LGORT = P_LGORT.
**        LS_ZTMM40940-LIFNR = P_LIFNR.
*        LS_ZTMM40940-ERNAM = SY-UNAME.
*        LS_ZTMM40940-ZSTATUS = 'E'.
*        LS_ZTMM40940-ERDAT = SY-DATUM.
*        LS_ZTMM40940-ERZET = SY-UZEIT.
*        LS_ZTMM40940-ERNAM = SY-UNAME.
*        LS_ZTMM40940-AEDAT = SY-DATUM.
*        LS_ZTMM40940-AEZET = SY-UZEIT.
*        LS_ZTMM40940-AENAM = SY-UNAME.
*
*        APPEND LS_ZTMM40940 TO GT_ZTMM40940. CLEAR LS_ZTMM40940.
*
*      WHEN P_RD1E.
*        MOVE-CORRESPONDING LS_DISP_E TO LS_ZTMM40950.
*        LS_ZTMM40950-BUKRS = P_BUKRS.
*        LS_ZTMM40950-WERKS = P_WERKS.
**        LS_ZTMM40950-LGORT = P_LGORT.
**        LS_ZTMM40950-KUNNR = P_KUNNR.
*        LS_ZTMM40950-ERNAM = SY-UNAME.
*        LS_ZTMM40950-ZSTATUS = 'E'.
*        LS_ZTMM40950-ERDAT = SY-DATUM.
*        LS_ZTMM40950-ERZET = SY-UZEIT.
*        LS_ZTMM40950-ERNAM = SY-UNAME.
*        LS_ZTMM40950-AEDAT = SY-DATUM.
*        LS_ZTMM40950-AEZET = SY-UZEIT.
*        LS_ZTMM40950-AENAM = SY-UNAME.
*
*        APPEND LS_ZTMM40950 TO GT_ZTMM40950. CLEAR LS_ZTMM40950.
*
*    ENDCASE.
*  ENDLOOP.
*
*ENDFORM.
*&---------------------------------------------------------------------*
*& Form BTN_ON_EXCEL_DOWNLOAD_INIT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM BTN_ON_EXCEL_DOWNLOAD_INIT .


  CONSTANTS : LC_OBJID      TYPE WWWDATA-OBJID VALUE 'ZOMM4990_01',
              LC_EXTENTION  TYPE STRING VALUE '.XLSX',
              LC_DEFAULT    TYPE STRING VALUE 'XLSX',
              LC_PATH_EXCEL TYPE STRING VALUE 'Excel (*.XLSX)'.

  DATA: LS_WWWDATA_ITEM LIKE WWWDATATAB,
        LV_FNAME_EXE    TYPE STRING,
        LV_FILENAME     TYPE RLGRAP-FILENAME,
        LV_FNAME        TYPE STRING VALUE 'BATCH Migration Result',
        LV_PATH         TYPE STRING VALUE 'C:\TEMP',
        LV_UP_PATH      TYPE STRING,
        LV_DN_PATH      TYPE STRING.

  DATA: LV_CNT  TYPE I.

  DATA: LV_FULL   TYPE STRING,
        LV_ACTION TYPE I.

  _G_INIT : GT_EXCEL_BAT.

  PERFORM MAKE_EXCEL_DATA.

  SELECT SINGLE RELID,
                OBJID,
                CHECKOUT,
                CHECKNEW,
                CHNAME,
                TDATE,
                TTIME,
                TEXT
    INTO CORRESPONDING FIELDS OF @LS_WWWDATA_ITEM
    FROM WWWDATA
   WHERE OBJID EQ @LC_OBJID.

  IF LS_WWWDATA_ITEM IS INITIAL.
    "SAP 웹저장소(SMW0)에 오브젝트 &이 등록되지 않았습니다.
    MESSAGE S002 WITH TEXT-M42 LC_OBJID TEXT-M43 DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.


*-------------------------------
* File Open Dialog
*-------------------------------
  TRY.
      CALL METHOD CL_GUI_FRONTEND_SERVICES=>GET_UPLOAD_DOWNLOAD_PATH
        CHANGING
          UPLOAD_PATH                 = LV_UP_PATH
          DOWNLOAD_PATH               = LV_DN_PATH
        EXCEPTIONS
          CNTL_ERROR                  = 1
          ERROR_NO_GUI                = 2
          NOT_SUPPORTED_BY_GUI        = 3
          GUI_UPLOAD_DOWNLOAD_PATH    = 4
          UPLOAD_DOWNLOAD_PATH_FAILED = 5
          OTHERS                      = 6.

      CONCATENATE LV_FNAME LC_EXTENTION INTO   LV_FNAME.

      CALL METHOD CL_GUI_FRONTEND_SERVICES=>FILE_SAVE_DIALOG
        EXPORTING
          WINDOW_TITLE              = 'DOWNLOAD'
          DEFAULT_EXTENSION         = LC_DEFAULT  "XLSX
          DEFAULT_FILE_NAME         = LV_FNAME
          INITIAL_DIRECTORY         = LV_DN_PATH
          FILE_FILTER               = CONV #( LC_PATH_EXCEL )
        CHANGING
          FILENAME                  = LV_FNAME_EXE
          PATH                      = LV_PATH
          FULLPATH                  = LV_FULL
          USER_ACTION               = LV_ACTION
        EXCEPTIONS
          CNTL_ERROR                = 1
          ERROR_NO_GUI              = 2
          NOT_SUPPORTED_BY_GUI      = 3
          INVALID_DEFAULT_FILE_NAME = 4
          OTHERS                    = 5.
    CATCH CX_ROOT INTO DATA(LS_ROOT).

  ENDTRY.

  IF LV_ACTION = CL_GUI_FRONTEND_SERVICES=>ACTION_CANCEL.
    "취소 했습니다.
    MESSAGE S000 WITH TEXT-M44.
    EXIT.
  ENDIF.

  LV_FILENAME = LV_FNAME_EXE.

  CALL FUNCTION 'DOWNLOAD_WEB_OBJECT'
    EXPORTING
      KEY         = LS_WWWDATA_ITEM
      DESTINATION = LV_FILENAME.

  LOOP AT GT_EXCEL_BAT INTO GS_EXCEL_BAT.
    CONCATENATE
      GS_EXCEL_BAT-MATNR
      GS_EXCEL_BAT-LIFNR
      GS_EXCEL_BAT-HSDAT
      GS_EXCEL_BAT-VFDAT
      GS_EXCEL_BAT-LICHA
      GS_EXCEL_BAT-ZMM_MAKER
      GS_EXCEL_BAT-ZCOMP_RATIO
      GS_EXCEL_BAT-ZCOMP_MOIST
      GS_EXCEL_BAT-ZCOMP_NET
      GS_EXCEL_BAT-ZCOMP_DRY
      GS_EXCEL_BAT-ZLOTNO2
      GS_EXCEL_BAT-CHARG
      GS_EXCEL_BAT-ZMESSAGE
    INTO GS_DATA SEPARATED BY CL_ABAP_CHAR_UTILITIES=>HORIZONTAL_TAB.
    APPEND GS_DATA TO GT_DATA.
    CLEAR: GS_DATA, GS_EXCEL_BAT.
  ENDLOOP.

  "엑셀파일 실행
  CREATE OBJECT GO_EXCEL 'EXCEL.APPLICATION'.

  SET PROPERTY OF GO_EXCEL 'VISIBLE' = 0.
  CALL METHOD OF GO_EXCEL 'WORKBOOKS' = GO_WORKBOOK.

  CALL METHOD OF GO_WORKBOOK 'OPEN'
    EXPORTING
      #1 = LV_FULL.

* Excel Save안하고 종료
*  SET PROPERTY OF GO_WORKBOOK 'SAVED' = 0. "0저장 1 그냥 종료

  CALL METHOD OF GO_EXCEL 'WORKSHEETS' = GO_WORKSHEET
    EXPORTING #1 = 1.

  CALL METHOD OF GO_WORKSHEET 'ACTIVATE'.
  SET PROPERTY OF GO_WORKSHEET 'NAME' = 'Batch Migration Sheet'.

*  엑셀 라인추가
  IF GT_EXCEL_BAT[] IS NOT INITIAL.
    LV_CNT = LINES( GT_EXCEL_BAT ) - 1.
    PERFORM LINE_INSERT USING 3 LV_CNT.
  ENDIF.

  " 데이터 라인 생성
  DATA LV_RC TYPE I.
  CALL METHOD CL_GUI_FRONTEND_SERVICES=>CLIPBOARD_EXPORT
    IMPORTING
      DATA                 = GT_DATA
    CHANGING
      RC                   = LV_RC
    EXCEPTIONS
      CNTL_ERROR           = 1
      ERROR_NO_GUI         = 2
      NOT_SUPPORTED_BY_GUI = 3
      OTHERS               = 4.

  CALL METHOD OF GO_EXCEL 'CELLS' = GO_CELL1
    EXPORTING
      #1 = 3
      #2 = 1.
  CALL METHOD OF GO_EXCEL 'CELLS' = GO_CELL2
    EXPORTING
      #1 = 3
      #2 = 1.
  CALL METHOD OF GO_EXCEL 'RANGE' = GO_RANGE
    EXPORTING
      #1 = GO_CELL1
      #2 = GO_CELL2.
  CALL METHOD OF GO_RANGE 'SELECT'.
  CALL METHOD OF GO_WORKSHEET 'PASTE'.

  SET PROPERTY OF GO_EXCEL 'VISIBLE' = 1.
*  CALL METHOD OF GO_EXCEL 'SaveAs' EXPORTING #1 = LV_FULL.
  CALL METHOD OF GO_EXCEL 'CLOSE'.
  CALL METHOD OF GO_EXCEL 'QUIT'.

*  LV_FILENAME = LV_FULL.
*  CALL FUNCTION 'WS_FILE_DELETE'
*    EXPORTING
*      FILE = LV_FILENAME.

  FREE OBJECT: GO_CELL1, GO_CELL2, GO_RANGE, GO_WORKSHEET, GO_WORKBOOK, GO_EXCEL.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form MAKE_EXCEL_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM MAKE_EXCEL_DATA .

  LOOP AT GT_BATCH INTO DATA(LS_BATCH).
    GS_EXCEL_BAT-MATNR          =  LS_BATCH-MATNR.
    GS_EXCEL_BAT-LIFNR          =  LS_BATCH-LIFNR.
    GS_EXCEL_BAT-HSDAT          =  LS_BATCH-HSDAT.
    GS_EXCEL_BAT-VFDAT          =  LS_BATCH-VFDAT.
    GS_EXCEL_BAT-LICHA          =  LS_BATCH-LICHA.
    GS_EXCEL_BAT-ZMM_MAKER      =  LS_BATCH-ZMM_MAKER.
    GS_EXCEL_BAT-ZCOMP_RATIO    =  LS_BATCH-ZCOMP_RATIO.
    GS_EXCEL_BAT-ZCOMP_MOIST    =  LS_BATCH-ZCOMP_MOIST.
    GS_EXCEL_BAT-ZCOMP_NET      =  LS_BATCH-ZCOMP_NET.
    GS_EXCEL_BAT-ZLOTNO2        =  LS_BATCH-ZLOTNO2.
    GS_EXCEL_BAT-CHARG          =  LS_BATCH-CHARG.
    GS_EXCEL_BAT-ZMESSAGE       =  LS_BATCH-ZMESSAGE.
    APPEND GS_EXCEL_BAT TO GT_EXCEL_BAT.
    CLEAR GS_EXCEL_BAT.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form LINE_INSERT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> P_5
*&      --> LV_CNT
*&---------------------------------------------------------------------*
FORM LINE_INSERT  USING  IV_ROW
                         IV_CNT.

  DATA: LV_LINE TYPE I.

*==============================
* 다중 LINE 복사
*==============================

  LV_LINE = IV_ROW.

  DO IV_CNT TIMES.

    ADD 1 TO LV_LINE.

*..LINE(ROW) 선택
    CALL METHOD OF GO_EXCEL 'ROWS' = GO_ROW EXPORTING #1 = IV_ROW.
    CALL METHOD OF GO_ROW 'SELECT'.
    CALL METHOD OF GO_EXCEL 'SELECTION' = GO_BUFFER.
    CALL METHOD OF GO_BUFFER 'COPY'.

*..LINE INSERT & PASTE
    CALL METHOD OF GO_EXCEL 'ROWS' = GO_ROW EXPORTING #1 = LV_LINE.
    CALL METHOD OF GO_ROW 'SELECT'.
    CALL METHOD OF GO_EXCEL 'SELECTION' = GO_BUFFER.
    CALL METHOD OF GO_BUFFER 'ENTIREROW' = GO_BUFFER.
    CALL METHOD OF GO_BUFFER 'INSERT' = GO_BUFFER.

  ENDDO.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form BTN_ON_EXCEL_DOWN_STO_INIT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM BTN_ON_EXCEL_DOWN_STO_INIT .

  CONSTANTS : LC_OBJID_B    TYPE WWWDATA-OBJID VALUE 'ZOMM4990_02',
              LC_OBJID_C    TYPE WWWDATA-OBJID VALUE 'ZOMM4990_03',
              LC_OBJID_D    TYPE WWWDATA-OBJID VALUE 'ZOMM4990_04',
              LC_OBJID_E    TYPE WWWDATA-OBJID VALUE 'ZOMM4990_05',
              LC_EXTENTION  TYPE STRING VALUE '.XLSX',
              LC_DEFAULT    TYPE STRING VALUE 'XLSX',
              LC_PATH_EXCEL TYPE STRING VALUE 'Excel (*.XLSX)'.

  DATA: LS_WWWDATA_ITEM LIKE WWWDATATAB,
        LV_FNAME_EXE    TYPE STRING,
        LV_FILENAME     TYPE RLGRAP-FILENAME,
        LV_FNAME        TYPE STRING,
*        LV_FNAME_B      TYPE STRING VALUE '창고 Migration Result',
*        LV_FNAME_C      TYPE STRING VALUE '사급 Migration Result',
*        LV_FNAME_D      TYPE STRING VALUE '위탁 Migration Result',
*        LV_FNAME_E      TYPE STRING VALUE '고객 Migration Result',
        LV_PATH         TYPE STRING VALUE 'C:\TEMP',
        LV_UP_PATH      TYPE STRING,
        LV_DN_PATH      TYPE STRING.

  DATA: LV_CNT  TYPE I.

  DATA: LV_FULL   TYPE STRING,
        LV_ACTION TYPE I.

  _G_INIT : GT_EXCEL_STO.

  PERFORM MAKE_EXCEL_DATA_STO.

  CASE 'X'.
    WHEN P_RD1B.
      SELECT SINGLE RELID,
                    OBJID,
                    CHECKOUT,
                    CHECKNEW,
                    CHNAME,
                    TDATE,
                    TTIME,
                    TEXT
        INTO CORRESPONDING FIELDS OF @LS_WWWDATA_ITEM
        FROM WWWDATA
       WHERE OBJID EQ @LC_OBJID_B.
    WHEN P_RD1C.
      SELECT SINGLE RELID,
                    OBJID,
                    CHECKOUT,
                    CHECKNEW,
                    CHNAME,
                    TDATE,
                    TTIME,
                    TEXT
        INTO CORRESPONDING FIELDS OF @LS_WWWDATA_ITEM
        FROM WWWDATA
       WHERE OBJID EQ @LC_OBJID_C.
    WHEN P_RD1D.
      SELECT SINGLE RELID,
                    OBJID,
                    CHECKOUT,
                    CHECKNEW,
                    CHNAME,
                    TDATE,
                    TTIME,
                    TEXT
        INTO CORRESPONDING FIELDS OF @LS_WWWDATA_ITEM
        FROM WWWDATA
       WHERE OBJID EQ @LC_OBJID_D.
    WHEN P_RD1E.
      SELECT SINGLE RELID,
                    OBJID,
                    CHECKOUT,
                    CHECKNEW,
                    CHNAME,
                    TDATE,
                    TTIME,
                    TEXT
        INTO CORRESPONDING FIELDS OF @LS_WWWDATA_ITEM
        FROM WWWDATA
       WHERE OBJID EQ @LC_OBJID_E.
  ENDCASE.

  IF LS_WWWDATA_ITEM IS INITIAL.
    "SAP 웹저장소(SMW0)에 오브젝트 &이 등록되지 않았습니다.
    CASE 'X'.
      WHEN P_RD1B.
        MESSAGE S002 WITH TEXT-M42 LC_OBJID_B TEXT-M43 DISPLAY LIKE 'E'.
      WHEN P_RD1C.
        MESSAGE S002 WITH TEXT-M42 LC_OBJID_C TEXT-M43 DISPLAY LIKE 'E'.
      WHEN P_RD1D.
        MESSAGE S002 WITH TEXT-M42 LC_OBJID_D TEXT-M43 DISPLAY LIKE 'E'.
      WHEN P_RD1E.
        MESSAGE S002 WITH TEXT-M42 LC_OBJID_E TEXT-M43 DISPLAY LIKE 'E'.
    ENDCASE.
    EXIT.
  ENDIF.


*-------------------------------
* File Open Dialog
*-------------------------------
  TRY.
      CALL METHOD CL_GUI_FRONTEND_SERVICES=>GET_UPLOAD_DOWNLOAD_PATH
        CHANGING
          UPLOAD_PATH                 = LV_UP_PATH
          DOWNLOAD_PATH               = LV_DN_PATH
        EXCEPTIONS
          CNTL_ERROR                  = 1
          ERROR_NO_GUI                = 2
          NOT_SUPPORTED_BY_GUI        = 3
          GUI_UPLOAD_DOWNLOAD_PATH    = 4
          UPLOAD_DOWNLOAD_PATH_FAILED = 5
          OTHERS                      = 6.


      CASE 'X'.
        WHEN P_RD1B.
          CONCATENATE TEXT-M65 LC_EXTENTION INTO   LV_FNAME.
        WHEN P_RD1C.
          CONCATENATE TEXT-M66 LC_EXTENTION INTO   LV_FNAME.
        WHEN P_RD1D.
          CONCATENATE TEXT-M67 LC_EXTENTION INTO   LV_FNAME.
        WHEN P_RD1E.
          CONCATENATE TEXT-M68 LC_EXTENTION INTO   LV_FNAME.
      ENDCASE.

      CALL METHOD CL_GUI_FRONTEND_SERVICES=>FILE_SAVE_DIALOG
        EXPORTING
          WINDOW_TITLE              = 'DOWNLOAD'
          DEFAULT_EXTENSION         = LC_DEFAULT  "XLSX
          DEFAULT_FILE_NAME         = LV_FNAME
          INITIAL_DIRECTORY         = LV_DN_PATH
          FILE_FILTER               = CONV #( LC_PATH_EXCEL )
        CHANGING
          FILENAME                  = LV_FNAME_EXE
          PATH                      = LV_PATH
          FULLPATH                  = LV_FULL
          USER_ACTION               = LV_ACTION
        EXCEPTIONS
          CNTL_ERROR                = 1
          ERROR_NO_GUI              = 2
          NOT_SUPPORTED_BY_GUI      = 3
          INVALID_DEFAULT_FILE_NAME = 4
          OTHERS                    = 5.
    CATCH CX_ROOT INTO DATA(LS_ROOT).

  ENDTRY.

  IF LV_ACTION = CL_GUI_FRONTEND_SERVICES=>ACTION_CANCEL.
    "취소 했습니다.
    MESSAGE S000 WITH TEXT-M44.
    EXIT.
  ENDIF.

  LV_FILENAME = LV_FNAME_EXE.

  CALL FUNCTION 'DOWNLOAD_WEB_OBJECT'
    EXPORTING
      KEY         = LS_WWWDATA_ITEM
      DESTINATION = LV_FILENAME.


  LOOP AT GT_EXCEL_STO INTO GS_EXCEL_STO.
    CASE 'X'.
      WHEN P_RD1B.
        CONCATENATE
          GS_EXCEL_STO-WERKS       GS_EXCEL_STO-LGORT       GS_EXCEL_STO-MATNR       GS_EXCEL_STO-CHARG       GS_EXCEL_STO-BWTAR
          GS_EXCEL_STO-ERFMG       GS_EXCEL_STO-ZMEINS      GS_EXCEL_STO-INSMK       GS_EXCEL_STO-EXBWR       GS_EXCEL_STO-ZMESSAGE
        INTO GS_DATA SEPARATED BY CL_ABAP_CHAR_UTILITIES=>HORIZONTAL_TAB.
      WHEN P_RD1C.
        CONCATENATE
          GS_EXCEL_STO-WERKS       GS_EXCEL_STO-LIFNR       GS_EXCEL_STO-MATNR       GS_EXCEL_STO-CHARG       GS_EXCEL_STO-BWTAR
          GS_EXCEL_STO-ERFMG       GS_EXCEL_STO-ZMEINS      GS_EXCEL_STO-INSMK       GS_EXCEL_STO-EXBWR       GS_EXCEL_STO-ZMESSAGE
        INTO GS_DATA SEPARATED BY CL_ABAP_CHAR_UTILITIES=>HORIZONTAL_TAB.
      WHEN P_RD1D.
        CONCATENATE
          GS_EXCEL_STO-WERKS       GS_EXCEL_STO-LGORT       GS_EXCEL_STO-LIFNR       GS_EXCEL_STO-MATNR       GS_EXCEL_STO-CHARG       GS_EXCEL_STO-BWTAR
          GS_EXCEL_STO-ERFMG       GS_EXCEL_STO-ZMEINS      GS_EXCEL_STO-INSMK       GS_EXCEL_STO-EXBWR       GS_EXCEL_STO-ZMESSAGE
        INTO GS_DATA SEPARATED BY CL_ABAP_CHAR_UTILITIES=>HORIZONTAL_TAB.
      WHEN P_RD1E.
        CONCATENATE
          GS_EXCEL_STO-WERKS       GS_EXCEL_STO-LGORT       GS_EXCEL_STO-KUNNR       GS_EXCEL_STO-MATNR       GS_EXCEL_STO-VBELN       GS_EXCEL_STO-POSNR
          GS_EXCEL_STO-CHARG       GS_EXCEL_STO-BWTAR       GS_EXCEL_STO-ERFMG       GS_EXCEL_STO-ZMEINS      GS_EXCEL_STO-INSMK       GS_EXCEL_STO-EXBWR       GS_EXCEL_STO-ZMESSAGE
        INTO GS_DATA SEPARATED BY CL_ABAP_CHAR_UTILITIES=>HORIZONTAL_TAB.
    ENDCASE.
    APPEND GS_DATA TO GT_DATA.
    CLEAR: GS_DATA, GS_EXCEL_STO.
  ENDLOOP.

  "엑셀파일 실행
  CREATE OBJECT GO_EXCEL 'EXCEL.APPLICATION'.

  SET PROPERTY OF GO_EXCEL 'VISIBLE' = 0.
  CALL METHOD OF GO_EXCEL 'WORKBOOKS' = GO_WORKBOOK.

  CALL METHOD OF GO_WORKBOOK 'OPEN'
    EXPORTING
      #1 = LV_FULL.

* Excel Save안하고 종료
*  SET PROPERTY OF GO_WORKBOOK 'SAVED' = 0. "0저장 1 그냥 종료

  CALL METHOD OF GO_EXCEL 'WORKSHEETS' = GO_WORKSHEET
    EXPORTING #1 = 1.



  CALL METHOD OF GO_WORKSHEET 'ACTIVATE'.
  CASE 'X'.
    WHEN P_RD1B.
      SET PROPERTY OF GO_WORKSHEET 'NAME' = 'Stock Migration Sheet'.
    WHEN P_RD1C.
      SET PROPERTY OF GO_WORKSHEET 'NAME' = TEXT-M69.
    WHEN P_RD1D.
      SET PROPERTY OF GO_WORKSHEET 'NAME' = TEXT-M70.
    WHEN P_RD1E.
      SET PROPERTY OF GO_WORKSHEET 'NAME' = TEXT-M71.
  ENDCASE.

*  엑셀 라인추가
  IF GT_EXCEL_STO[] IS NOT INITIAL.
    LV_CNT = LINES( GT_EXCEL_STO ) - 1.
    PERFORM LINE_INSERT USING 3 LV_CNT.
  ENDIF.

  " 데이터 라인 생성
  DATA LV_RC TYPE I.
  CALL METHOD CL_GUI_FRONTEND_SERVICES=>CLIPBOARD_EXPORT
    IMPORTING
      DATA                 = GT_DATA
    CHANGING
      RC                   = LV_RC
    EXCEPTIONS
      CNTL_ERROR           = 1
      ERROR_NO_GUI         = 2
      NOT_SUPPORTED_BY_GUI = 3
      OTHERS               = 4.

  CALL METHOD OF GO_EXCEL 'CELLS' = GO_CELL1
    EXPORTING
      #1 = 3
      #2 = 1.
  CALL METHOD OF GO_EXCEL 'CELLS' = GO_CELL2
    EXPORTING
      #1 = 3
      #2 = 1.
  CALL METHOD OF GO_EXCEL 'RANGE' = GO_RANGE
    EXPORTING
      #1 = GO_CELL1
      #2 = GO_CELL2.
  CALL METHOD OF GO_RANGE 'SELECT'.
  CALL METHOD OF GO_WORKSHEET 'PASTE'.

  SET PROPERTY OF GO_EXCEL 'VISIBLE' = 1.
*  CALL METHOD OF GO_EXCEL 'SaveAs' EXPORTING #1 = LV_FULL.
  CALL METHOD OF GO_EXCEL 'CLOSE'.
  CALL METHOD OF GO_EXCEL 'QUIT'.

*  LV_FILENAME = LV_FULL.
*  CALL FUNCTION 'WS_FILE_DELETE'
*    EXPORTING
*      FILE = LV_FILENAME.

  FREE OBJECT: GO_CELL1, GO_CELL2, GO_RANGE, GO_WORKSHEET, GO_WORKBOOK, GO_EXCEL.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form MAKE_EXCEL_DATA_STO
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM MAKE_EXCEL_DATA_STO .

  CASE 'X'.
    WHEN P_RD1B.
      LOOP AT GT_DISP INTO DATA(LS_DISP_B).
        GS_EXCEL_STO-WERKS          =  LS_DISP_B-WERKS.
        GS_EXCEL_STO-LGORT          =  LS_DISP_B-LGORT.
        GS_EXCEL_STO-MATNR          =  LS_DISP_B-MATNR.
        GS_EXCEL_STO-CHARG          =  LS_DISP_B-CHARG.
        GS_EXCEL_STO-BWTAR          =  LS_DISP_B-BWTAR.
        GS_EXCEL_STO-ERFMG          =  LS_DISP_B-ERFMG.
        GS_EXCEL_STO-ZMEINS         =  LS_DISP_B-ZMEINS.
        GS_EXCEL_STO-INSMK          =  LS_DISP_B-INSMK.
        GS_EXCEL_STO-EXBWR          =  LS_DISP_B-EXBWR.
        GS_EXCEL_STO-ZMESSAGE       =  LS_DISP_B-ZMESSAGE.
        APPEND GS_EXCEL_STO TO GT_EXCEL_STO.
        CLEAR GS_EXCEL_STO.
      ENDLOOP.
    WHEN P_RD1C.
      LOOP AT GT_DISP INTO DATA(LS_DISP_C).
        GS_EXCEL_STO-WERKS          =  LS_DISP_C-WERKS.
        GS_EXCEL_STO-LIFNR          =  LS_DISP_C-LIFNR.
        GS_EXCEL_STO-MATNR          =  LS_DISP_C-MATNR.
        GS_EXCEL_STO-CHARG          =  LS_DISP_C-CHARG.
        GS_EXCEL_STO-BWTAR          =  LS_DISP_C-BWTAR.
        GS_EXCEL_STO-ERFMG          =  LS_DISP_C-ERFMG.
        GS_EXCEL_STO-ZMEINS         =  LS_DISP_C-ZMEINS.
        GS_EXCEL_STO-INSMK          =  LS_DISP_C-INSMK.
        GS_EXCEL_STO-EXBWR          =  LS_DISP_C-EXBWR.
        GS_EXCEL_STO-ZMESSAGE       =  LS_DISP_C-ZMESSAGE.
        APPEND GS_EXCEL_STO TO GT_EXCEL_STO.
        CLEAR GS_EXCEL_STO.
      ENDLOOP.
    WHEN P_RD1D.
      LOOP AT GT_DISP INTO DATA(LS_DISP_D).
        GS_EXCEL_STO-WERKS          =  LS_DISP_D-WERKS.
        GS_EXCEL_STO-LGORT          =  LS_DISP_D-LGORT.
        GS_EXCEL_STO-LIFNR          =  LS_DISP_D-LIFNR.
        GS_EXCEL_STO-MATNR          =  LS_DISP_D-MATNR.
        GS_EXCEL_STO-CHARG          =  LS_DISP_D-CHARG.
        GS_EXCEL_STO-BWTAR          =  LS_DISP_D-BWTAR.
        GS_EXCEL_STO-ERFMG          =  LS_DISP_D-ERFMG.
        GS_EXCEL_STO-ZMEINS         =  LS_DISP_D-ZMEINS.
        GS_EXCEL_STO-INSMK          =  LS_DISP_D-INSMK.
        GS_EXCEL_STO-EXBWR          =  LS_DISP_D-EXBWR.
        GS_EXCEL_STO-ZMESSAGE       =  LS_DISP_D-ZMESSAGE.
        APPEND GS_EXCEL_STO TO GT_EXCEL_STO.
        CLEAR GS_EXCEL_STO.
      ENDLOOP.
    WHEN P_RD1E.
      LOOP AT GT_DISP INTO DATA(LS_DISP_E).
        GS_EXCEL_STO-WERKS          =  LS_DISP_E-WERKS.
        GS_EXCEL_STO-LGORT          =  LS_DISP_E-LGORT.
        GS_EXCEL_STO-KUNNR          =  LS_DISP_E-KUNNR.
        GS_EXCEL_STO-MATNR          =  LS_DISP_E-MATNR.
        GS_EXCEL_STO-VBELN          =  LS_DISP_E-VBELN.
        GS_EXCEL_STO-POSNR          =  LS_DISP_E-POSNR.
        GS_EXCEL_STO-CHARG          =  LS_DISP_E-CHARG.
        GS_EXCEL_STO-BWTAR          =  LS_DISP_E-BWTAR.
        GS_EXCEL_STO-ERFMG          =  LS_DISP_E-ERFMG.
        GS_EXCEL_STO-ZMEINS         =  LS_DISP_E-ZMEINS.
        GS_EXCEL_STO-INSMK          =  LS_DISP_E-INSMK.
        GS_EXCEL_STO-EXBWR          =  LS_DISP_E-EXBWR.
        GS_EXCEL_STO-ZMESSAGE       =  LS_DISP_E-ZMESSAGE.
        APPEND GS_EXCEL_STO TO GT_EXCEL_STO.
        CLEAR GS_EXCEL_STO.
      ENDLOOP.
  ENDCASE.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form SELECT_LAST_SEQ_B
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- LV_ZSEQ
*&---------------------------------------------------------------------*
FORM SELECT_LAST_SEQ  CHANGING CV_ZSEQ.

  CASE 'X'.
    WHEN P_RD1B.  "창고

      SELECT SINGLE FROM ZTMM40920
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

      SELECT SINGLE FROM ZTMM40930
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

      SELECT SINGLE FROM ZTMM40940
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
      SELECT SINGLE FROM ZTMM40950
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
*& Form CHECK_VALIDATION
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- <LS_DISP>
*&---------------------------------------------------------------------*
FORM CHECK_VALIDATION  CHANGING CS_DATA TYPE TS_DISP.


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
*& Form APPEND_BATCH_ATTRIBUTE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LT_CLASSVALUATIONSCHAR
*&      --> <LS_BATCH>
*&      <-- LS_CLASSVALUATIONSCHAR
*&---------------------------------------------------------------------*
FORM APPEND_BATCH_ATTRIBUTE  TABLES   IT_CLASSVALUATIONSCHAR STRUCTURE BAPI3060_VALUATION_CHAR
                             USING    IS_DATA TYPE TS_BATCH
                                      IV_NEW_CHARG TYPE ZSVBMMBATLAST-CHARG.

  DATA : LS_CLASSVALUATIONSCHAR LIKE BAPI3060_VALUATION_CHAR.

  CONSTANTS LC_ZSUGAR_CONTENT1 TYPE BAPI3060_VALUATION_CHAR-CHARACT VALUE 'ZSUGAR_CONTENT1'.

  IF IS_DATA-LICHA IS NOT INITIAL AND ( IS_DATA-CLASS = GC_ZPP_KGC1 OR IS_DATA-CLASS = GC_KGCBATCH2 OR
     IS_DATA-CLASS = GC_KGCBATCH3 OR  IS_DATA-CLASS = GC_KGCBATCH4 ).

    LS_CLASSVALUATIONSCHAR-CLASS_TYPE  = '023'.
    LS_CLASSVALUATIONSCHAR-OBJECTKEY   = IS_DATA-MATNR && IV_NEW_CHARG.
    LS_CLASSVALUATIONSCHAR-OBJECTTABLE = GC_MARA.
    LS_CLASSVALUATIONSCHAR-CHARACT     = GC_ZFMFRPN.
    LS_CLASSVALUATIONSCHAR-VALUE_CHAR  = IS_DATA-LICHA.

    APPEND LS_CLASSVALUATIONSCHAR TO IT_CLASSVALUATIONSCHAR.
    CLEAR : LS_CLASSVALUATIONSCHAR.

  ENDIF.

  IF IS_DATA-HSDAT IS NOT INITIAL AND  IS_DATA-CLASS = GC_ZPP_KGC1 AND  IS_DATA-ZCHECK = ''.

    LS_CLASSVALUATIONSCHAR-CLASS_TYPE  = '023'.
    LS_CLASSVALUATIONSCHAR-OBJECTKEY   = IS_DATA-MATNR && IV_NEW_CHARG.
    LS_CLASSVALUATIONSCHAR-OBJECTTABLE = GC_MARA.
    LS_CLASSVALUATIONSCHAR-CHARACT     = GC_ZPROD_DATE.
    LS_CLASSVALUATIONSCHAR-VALUE_CHAR  = IS_DATA-HSDAT.

    APPEND LS_CLASSVALUATIONSCHAR TO IT_CLASSVALUATIONSCHAR.
    CLEAR : LS_CLASSVALUATIONSCHAR.

  ENDIF.

  IF IS_DATA-VFDAT IS NOT INITIAL AND ( IS_DATA-CLASS = GC_ZPP_KGC1 OR IS_DATA-CLASS = GC_ZPP_YJP ) AND  IS_DATA-ZCHECK = ''..

    LS_CLASSVALUATIONSCHAR-CLASS_TYPE  = '023'.
    LS_CLASSVALUATIONSCHAR-OBJECTKEY   = IS_DATA-MATNR && IV_NEW_CHARG.
    LS_CLASSVALUATIONSCHAR-OBJECTTABLE = GC_MARA.

    IF  IS_DATA-CLASS =  GC_ZPP_KGC1.
      LS_CLASSVALUATIONSCHAR-CHARACT     = GC_ZEXPIRY_DATE.
    ELSE.
      LS_CLASSVALUATIONSCHAR-CHARACT     = GC_ZEXPIRY_DATE2.
    ENDIF.
    LS_CLASSVALUATIONSCHAR-VALUE_CHAR  = IS_DATA-VFDAT.

    APPEND LS_CLASSVALUATIONSCHAR TO IT_CLASSVALUATIONSCHAR.
    CLEAR : LS_CLASSVALUATIONSCHAR.

  ENDIF.


  IF IS_DATA-ZMM_MAKER IS NOT INITIAL.

    LS_CLASSVALUATIONSCHAR-CLASS_TYPE  = '023'.
    LS_CLASSVALUATIONSCHAR-OBJECTKEY   = IS_DATA-MATNR && IV_NEW_CHARG.
    LS_CLASSVALUATIONSCHAR-OBJECTTABLE = GC_MARA.
    LS_CLASSVALUATIONSCHAR-CHARACT     = GC_ZMM_MAKER.
    LS_CLASSVALUATIONSCHAR-VALUE_CHAR  = IS_DATA-ZMM_MAKER.

    APPEND LS_CLASSVALUATIONSCHAR TO IT_CLASSVALUATIONSCHAR.
    CLEAR : LS_CLASSVALUATIONSCHAR.
  ENDIF.


  IF IS_DATA-ZCOMP_RATIO IS NOT INITIAL.

    LS_CLASSVALUATIONSCHAR-CLASS_TYPE  = '023'.
    LS_CLASSVALUATIONSCHAR-OBJECTKEY   = IS_DATA-MATNR && IV_NEW_CHARG.
    LS_CLASSVALUATIONSCHAR-OBJECTTABLE = GC_MARA.
    LS_CLASSVALUATIONSCHAR-CHARACT     = GC_ZCOMP_RATIO.
    LS_CLASSVALUATIONSCHAR-VALUE_CHAR  = IS_DATA-ZCOMP_RATIO.

    APPEND LS_CLASSVALUATIONSCHAR TO IT_CLASSVALUATIONSCHAR.
    CLEAR : LS_CLASSVALUATIONSCHAR.

  ENDIF.

  IF IS_DATA-ZCOMP_MOIST IS NOT INITIAL.

    LS_CLASSVALUATIONSCHAR-CLASS_TYPE  = '023'.
    LS_CLASSVALUATIONSCHAR-OBJECTKEY   = IS_DATA-MATNR && IV_NEW_CHARG.
    LS_CLASSVALUATIONSCHAR-OBJECTTABLE = GC_MARA.
    IF  IS_DATA-CLASS =  GC_ZPP_KGC1.
      LS_CLASSVALUATIONSCHAR-CHARACT     = GC_ZWATER_CONTENT.
    ELSE.
      LS_CLASSVALUATIONSCHAR-CHARACT     = GC_ZCOMP_MOIST.
    ENDIF.
    LS_CLASSVALUATIONSCHAR-VALUE_CHAR  = IS_DATA-ZCOMP_MOIST.

    APPEND LS_CLASSVALUATIONSCHAR TO IT_CLASSVALUATIONSCHAR.
    CLEAR : LS_CLASSVALUATIONSCHAR.

  ENDIF.

  IF IS_DATA-ZCOMP_NET IS NOT INITIAL.

    LS_CLASSVALUATIONSCHAR-CLASS_TYPE  = '023'.
    LS_CLASSVALUATIONSCHAR-OBJECTKEY   = IS_DATA-MATNR && IV_NEW_CHARG.
    LS_CLASSVALUATIONSCHAR-OBJECTTABLE = GC_MARA.
    LS_CLASSVALUATIONSCHAR-CHARACT     = GC_ZCOMP_NET.
    LS_CLASSVALUATIONSCHAR-VALUE_CHAR  = IS_DATA-ZCOMP_NET.

    APPEND LS_CLASSVALUATIONSCHAR TO IT_CLASSVALUATIONSCHAR.
    CLEAR : LS_CLASSVALUATIONSCHAR.

  ENDIF.

  IF IS_DATA-ZCOMP_DRY IS NOT INITIAL.

    LS_CLASSVALUATIONSCHAR-CLASS_TYPE  = '023'.
    LS_CLASSVALUATIONSCHAR-OBJECTKEY   = IS_DATA-MATNR && IV_NEW_CHARG.
    LS_CLASSVALUATIONSCHAR-OBJECTTABLE = GC_MARA.
    LS_CLASSVALUATIONSCHAR-CHARACT     = GC_ZCOMP_DRY.
    LS_CLASSVALUATIONSCHAR-VALUE_CHAR  = IS_DATA-ZCOMP_DRY.

    APPEND LS_CLASSVALUATIONSCHAR TO IT_CLASSVALUATIONSCHAR.
    CLEAR : LS_CLASSVALUATIONSCHAR.

  ENDIF.

  IF IS_DATA-ZLOTNO2 IS NOT INITIAL AND    IS_DATA-CLASS =  GC_ZPP_YJP.

    LS_CLASSVALUATIONSCHAR-CLASS_TYPE  = '023'.
    LS_CLASSVALUATIONSCHAR-OBJECTKEY   = IS_DATA-MATNR && IV_NEW_CHARG.
    LS_CLASSVALUATIONSCHAR-OBJECTTABLE = GC_MARA.
    LS_CLASSVALUATIONSCHAR-CHARACT     = GC_CUST_LOTNO.
    LS_CLASSVALUATIONSCHAR-VALUE_CHAR  = IS_DATA-ZLOTNO2.

    APPEND LS_CLASSVALUATIONSCHAR TO IT_CLASSVALUATIONSCHAR.
    CLEAR : LS_CLASSVALUATIONSCHAR.

  ENDIF.

* 20221024 START : SUGARRATIO 구성 로직 추가
  IF IS_DATA-SUGARRATIO IS NOT INITIAL AND IS_DATA-CLASS = GC_ZPP_KGC1 AND IS_DATA-ZCHECK = ''.
    LS_CLASSVALUATIONSCHAR-CLASS_TYPE  = '023'.
    LS_CLASSVALUATIONSCHAR-OBJECTKEY   = IS_DATA-MATNR && IV_NEW_CHARG.
    LS_CLASSVALUATIONSCHAR-OBJECTTABLE = GC_MARA.
    LS_CLASSVALUATIONSCHAR-CHARACT     = LC_ZSUGAR_CONTENT1.
    LS_CLASSVALUATIONSCHAR-VALUE_CHAR  = IS_DATA-SUGARRATIO.

    APPEND LS_CLASSVALUATIONSCHAR TO IT_CLASSVALUATIONSCHAR.
    CLEAR : LS_CLASSVALUATIONSCHAR.
  ENDIF.
* 20221024 END

ENDFORM.
*&---------------------------------------------------------------------*
*& Form NUMERIC_CHECK
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> <LS_BATCH>
*&      --> LV_TYPE
*&      --> LV_LEN
*&      --> LV_CHARG_30
*&      <-- LS_CLASSALLOCATIONS
*&---------------------------------------------------------------------*
FORM NUMERIC_CHECK  USING    IS_DATA TYPE TS_BATCH
                             IV_NEW_CHARG TYPE ZSVBMMBATLAST-CHARG
                    CHANGING CS_CLASSALLOCATIONS-OBJECTKEY.

  DATA : LV_CHARG TYPE ZSVBMMBATLAST-CHARG,
         LV_TYPE  LIKE DD01V-DATATYPE.

  DATA : LV_LEN          TYPE I,
         LV_CHARG_30(30) TYPE C.

  CALL FUNCTION 'NUMERIC_CHECK'
    EXPORTING
      STRING_IN = IS_DATA-MATNR
    IMPORTING
      HTYPE     = LV_TYPE.

  IF LV_TYPE NE GC_NUMC.

    LV_LEN = STRLEN( IS_DATA-MATNR ).
    LV_CHARG_30 = IV_NEW_CHARG.

    DATA(LV_COUNT) = 18 - LV_LEN.

    IF LV_COUNT > 1.
      DO LV_COUNT TIMES.
        CONCATENATE ` ` LV_CHARG_30 INTO LV_CHARG_30.
      ENDDO.
    ENDIF.

    CS_CLASSALLOCATIONS-OBJECTKEY   = IS_DATA-MATNR && LV_CHARG_30.
  ELSE.
    CS_CLASSALLOCATIONS-OBJECTKEY   = IS_DATA-MATNR && IV_NEW_CHARG.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form BTN_ON_BAPI_CANCEL_INIT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM BTN_ON_BAPI_CANCEL_INIT .

  DATA : LT_DISP TYPE TABLE OF TS_DISP.
  DATA : LS_GOODSMVT_HEADRET TYPE BAPI2017_GM_HEAD_RET,
         LT_RETURN           TYPE TABLE OF BAPIRET2.

  DATA: LV_ZEILE     TYPE ZTMM40920-ZEILE,
        LS_ZTMM40920 TYPE ZTMM40920,
        LT_ZTMM40920 TYPE TABLE OF ZTMM40920,
        LS_ZTMM40930 TYPE ZTMM40930,
        LT_ZTMM40930 TYPE TABLE OF ZTMM40930,
        LS_ZTMM40940 TYPE ZTMM40940,
        LT_ZTMM40940 TYPE TABLE OF ZTMM40940,
        LS_ZTMM40950 TYPE ZTMM40950,
        LT_ZTMM40950 TYPE TABLE OF ZTMM40950.

*-
  CALL METHOD GRF_GRID->GET_SELECTED_ROWS
    IMPORTING
      ET_INDEX_ROWS = DATA(LT_SELIDX).

  DESCRIBE TABLE LT_SELIDX LINES DATA(LV_TSELLINES).

  IF LV_TSELLINES IS INITIAL.
    MESSAGE S000 WITH TEXT-M14 DISPLAY LIKE 'E'. EXIT.
  ENDIF.

*-

  _G_INIT : LT_DISP.

  LOOP AT LT_SELIDX INTO DATA(LS_SELIDX).
    READ TABLE GT_DISP INTO DATA(LS_CHECK) INDEX LS_SELIDX-INDEX.
    IF LS_CHECK-STATUS = ICON_LED_GREEN AND LS_CHECK-MBLNR_R IS INITIAL.
      APPEND LS_CHECK TO LT_DISP. CLEAR LS_CHECK.
    ENDIF.
  ENDLOOP.

  CHECK GRF_GRID->POP_TO_MSG( IV_TYPE  = 'A'
                              IV_TITLE = CONV STRING( TEXT-U01 )      "Migration 취소
                              IV_TEXT1 = CONV #( TEXT-M72 )
                              IV_TEXT2 = CONV #( TEXT-M73 ) )
                              EQ ABAP_TRUE. " YES

  IF LT_DISP[] IS NOT INITIAL.
    CLEAR : LV_ZEILE, LS_ZTMM40920, LS_ZTMM40930, LS_ZTMM40940, LS_ZTMM40950.
    _G_INIT : LT_ZTMM40920, LT_ZTMM40930, LT_ZTMM40940, LT_ZTMM40950.

    LV_ZEILE = GC_0001.

    LOOP AT LT_DISP INTO DATA(LS_DISP).

      CLEAR : LS_GOODSMVT_HEADRET, LT_RETURN[].

      CALL FUNCTION 'BAPI_GOODSMVT_CANCEL'
        EXPORTING
          MATERIALDOCUMENT    = LS_DISP-MBLNR
          MATDOCUMENTYEAR     = LS_DISP-MJAHR
          GOODSMVT_PSTNG_DATE = LS_DISP-BUDAT
        IMPORTING
          GOODSMVT_HEADRET    = LS_GOODSMVT_HEADRET
        TABLES
          RETURN              = LT_RETURN.


      IF LS_GOODSMVT_HEADRET-MAT_DOC IS NOT INITIAL.
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            WAIT = 'X'.

        LS_DISP-STATUS = ICON_LED_GREEN.
        LS_DISP-ZMESSAGE = TEXT-M62 && LS_GOODSMVT_HEADRET-MAT_DOC && TEXT-M74.

        CASE 'X'.
          WHEN P_RD1B.  "창고 Migration
            MOVE-CORRESPONDING LS_DISP TO LS_ZTMM40920.
            LS_ZTMM40920-BUKRS = P_BUKRS.
            LS_ZTMM40920-MJAHR_R = LS_GOODSMVT_HEADRET-DOC_YEAR.
            LS_ZTMM40920-MBLNR_R = LS_GOODSMVT_HEADRET-MAT_DOC.
            LS_ZTMM40920-ZEILE_R = LV_ZEILE.
            LS_ZTMM40920-ZSTATUS = 'S'.
            LS_ZTMM40920-AEDAT = SY-DATUM.
            LS_ZTMM40920-AEZET = SY-UZEIT.
            LS_ZTMM40920-AENAM = SY-UNAME.

            APPEND LS_ZTMM40920 TO LT_ZTMM40920. CLEAR LS_ZTMM40920.

          WHEN P_RD1C.  "사급 Migration
            MOVE-CORRESPONDING LS_DISP TO LS_ZTMM40930.
            LS_ZTMM40930-BUKRS = P_BUKRS.
            LS_ZTMM40930-MJAHR_R = LS_GOODSMVT_HEADRET-DOC_YEAR.
            LS_ZTMM40930-MBLNR_R = LS_GOODSMVT_HEADRET-MAT_DOC.
            LS_ZTMM40930-ZEILE_R = LV_ZEILE.
            LS_ZTMM40930-ZSTATUS = 'S'.
            LS_ZTMM40930-AEDAT = SY-DATUM.
            LS_ZTMM40930-AEZET = SY-UZEIT.
            LS_ZTMM40930-AENAM = SY-UNAME.

            APPEND LS_ZTMM40930 TO LT_ZTMM40930. CLEAR LS_ZTMM40930.

          WHEN P_RD1D.  "위탁 Migration
            MOVE-CORRESPONDING LS_DISP TO LS_ZTMM40940.
            LS_ZTMM40940-BUKRS = P_BUKRS.
            LS_ZTMM40940-MJAHR_R = LS_GOODSMVT_HEADRET-DOC_YEAR.
            LS_ZTMM40940-MBLNR_R = LS_GOODSMVT_HEADRET-MAT_DOC.
            LS_ZTMM40940-ZEILE_R = LV_ZEILE.
            LS_ZTMM40940-ZSTATUS = 'S'.
            LS_ZTMM40940-AEDAT = SY-DATUM.
            LS_ZTMM40940-AEZET = SY-UZEIT.
            LS_ZTMM40940-AENAM = SY-UNAME.

            APPEND LS_ZTMM40940 TO LT_ZTMM40940. CLEAR LS_ZTMM40940.

          WHEN P_RD1E.  "고객 Migration
            MOVE-CORRESPONDING LS_DISP TO LS_ZTMM40950.
            LS_ZTMM40950-BUKRS = P_BUKRS.
            LS_ZTMM40950-MJAHR_R = LS_GOODSMVT_HEADRET-DOC_YEAR.
            LS_ZTMM40950-MBLNR_R = LS_GOODSMVT_HEADRET-MAT_DOC..
            LS_ZTMM40950-ZEILE_R = LV_ZEILE.
            LS_ZTMM40950-ZSTATUS = 'S'.
            LS_ZTMM40950-AEDAT = SY-DATUM.
            LS_ZTMM40950-AEZET = SY-UZEIT.
            LS_ZTMM40950-AENAM = SY-UNAME.

            APPEND LS_ZTMM40950 TO LT_ZTMM40950. CLEAR LS_ZTMM40950.

*            LV_ZEILE = LV_ZEILE + 1.
        ENDCASE.

      ELSE.

        CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
        READ TABLE LT_RETURN INTO DATA(LS_RETURN) INDEX 1.

        IF LS_RETURN IS NOT INITIAL.

          LS_DISP-STATUS = ICON_LED_RED.
          LS_DISP-ZMESSAGE = TEXT-M75 && LS_RETURN-MESSAGE.

          CASE 'X'.
            WHEN P_RD1B.  "창고 Migration
              MOVE-CORRESPONDING LS_DISP TO LS_ZTMM40920.
              LS_ZTMM40920-BUKRS = P_BUKRS.
              LS_ZTMM40920-ZSTATUS = 'E'.
              LS_ZTMM40920-AEDAT = SY-DATUM.
              LS_ZTMM40920-AEZET = SY-UZEIT.
              LS_ZTMM40920-AENAM = SY-UNAME.

              APPEND LS_ZTMM40920 TO LT_ZTMM40920. CLEAR LS_ZTMM40920.

            WHEN P_RD1C.  "사급 Migration
              MOVE-CORRESPONDING LS_DISP TO LS_ZTMM40930.
              LS_ZTMM40930-BUKRS = P_BUKRS.
              LS_ZTMM40930-ZSTATUS = 'E'.
              LS_ZTMM40930-AEDAT = SY-DATUM.
              LS_ZTMM40930-AEZET = SY-UZEIT.
              LS_ZTMM40930-AENAM = SY-UNAME.

              APPEND LS_ZTMM40930 TO LT_ZTMM40930. CLEAR LS_ZTMM40930.

            WHEN P_RD1D.  "위탁 Migration
              MOVE-CORRESPONDING LS_DISP TO LS_ZTMM40940.
              LS_ZTMM40940-BUKRS = P_BUKRS.
              LS_ZTMM40940-ZSTATUS = 'E'.
              LS_ZTMM40940-AEDAT = SY-DATUM.
              LS_ZTMM40940-AEZET = SY-UZEIT.
              LS_ZTMM40940-AENAM = SY-UNAME.

              APPEND LS_ZTMM40940 TO LT_ZTMM40940. CLEAR LS_ZTMM40940.

            WHEN P_RD1E.  "고객 Migration
              MOVE-CORRESPONDING LS_DISP TO LS_ZTMM40950.
              LS_ZTMM40950-BUKRS = P_BUKRS.
              LS_ZTMM40950-ZSTATUS = 'E'.
              LS_ZTMM40950-AEDAT = SY-DATUM.
              LS_ZTMM40950-AEZET = SY-UZEIT.
              LS_ZTMM40950-AENAM = SY-UNAME.

              APPEND LS_ZTMM40950 TO LT_ZTMM40950. CLEAR LS_ZTMM40950.

          ENDCASE.
        ENDIF.
      ENDIF.
    ENDLOOP.

    CASE 'X'.
      WHEN P_RD1B. " 창고 Migration
        MODIFY ZTMM40920 FROM TABLE LT_ZTMM40920.
        PERFORM REFRESH_DATA_B.

      WHEN P_RD1C. " 사급 Migration
        MODIFY ZTMM40930 FROM TABLE LT_ZTMM40930.
        PERFORM REFRESH_DATA_C.

      WHEN P_RD1D. " 위탁 Migration
        MODIFY ZTMM40940 FROM TABLE LT_ZTMM40940.
        PERFORM REFRESH_DATA_D.

      WHEN P_RD1E. " 고객 Migration
        MODIFY ZTMM40950 FROM TABLE LT_ZTMM40950.
        PERFORM REFRESH_DATA_E.
    ENDCASE.

  ENDIF.


  GRF_GRID->REFRESH_GRID_DISPLAY( ).
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
