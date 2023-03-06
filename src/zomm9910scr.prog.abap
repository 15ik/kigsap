*&---------------------------------------------------------------------*
*& Include          ZOMM9910SCR
*&---------------------------------------------------------------------*
"-----------------------------------------------------------------------
" 기본선택
"-----------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK BLK1 WITH FRAME TITLE TEXT-B01.
  PARAMETERS: P_BUKRS LIKE ZTMM99120-BUKRS,                        "회사코드
              P_WERKS LIKE ZTMM99120-WERKS MODIF ID PWE,           "플랜트
*              P_LGORT LIKE ZTMM99120-LGORT MODIF ID PLG,           "저장위치
*              P_LIFNR LIKE ZTMM99130-LIFNR MODIF ID PLI,           "공급업체
*              P_KUNNR LIKE ZTMM99150-KUNNR MODIF ID PKU,           "고객
              P_BUDAT LIKE ZTMM99150-BUDAT MODIF ID PBU,           "전기일
              P_FILE  TYPE RLGRAP-FILENAME MODIF ID PFI.           "파일경로
  SELECT-OPTIONS: S_MATNR FOR ZTMM99120-MATNR MODIF ID SMA,        "자재코드
                  S_CHARG FOR ZTMM99120-CHARG MODIF ID SCH,        "배치번호
                  S_WERKS FOR ZTMM99120-WERKS MODIF ID SWE,        "플랜트
                  S_LGORT FOR ZTMM99120-LGORT MODIF ID SLG,        "저장위치
                  S_LIFNR FOR ZTMM99130-LIFNR MODIF ID SLI,        "공급업체
                  S_KUNNR FOR ZTMM99150-KUNNR MODIF ID SKU,        "고객
                  S_ERDAT FOR ZTMM99120-ERDAT MODIF ID SER,        "생성일
                  S_ERNAM FOR ZTMM99120-ERNAM MODIF ID SER.        "작업자

SELECTION-SCREEN END OF BLOCK BLK1.
SELECTION-SCREEN FUNCTION KEY 1.
"-----------------------------------------------------------------------
" 작업대상
"-----------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK BLK2 WITH FRAME TITLE TEXT-B02.
  SELECTION-SCREEN BEGIN OF LINE.
    PARAMETERS: P_RD1A RADIOBUTTON GROUP RD1 DEFAULT 'X' USER-COMMAND RA.
    SELECTION-SCREEN COMMENT 4(30) TEXT-T01 FOR FIELD P_RD1A.  "배치 Migration
    SELECTION-SCREEN POSITION 35.

    PARAMETERS: P_RD1B RADIOBUTTON GROUP RD1.
    SELECTION-SCREEN COMMENT 36(24) TEXT-T02 FOR FIELD P_RD1B. "창고 재고 Migration
    SELECTION-SCREEN POSITION 62.

    PARAMETERS: P_RD1C RADIOBUTTON GROUP RD1.
    SELECTION-SCREEN COMMENT 63(25) TEXT-T03 FOR FIELD P_RD1C. "사급 재고 Migration
 SELECTION-SCREEN END OF LINE.

  SELECTION-SCREEN BEGIN OF LINE.
    PARAMETERS: P_RD1D RADIOBUTTON GROUP RD1.
    SELECTION-SCREEN COMMENT 4(30) TEXT-T04 FOR FIELD P_RD1D. "위탁 재고 Migration
    SELECTION-SCREEN POSITION 35.

    PARAMETERS: P_RD1E RADIOBUTTON GROUP RD1.
    SELECTION-SCREEN COMMENT 36(25) TEXT-T05 FOR FIELD P_RD1E. "고객 재고 Migration
  SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK BLK2.

"-----------------------------------------------------------------------
" 작업구분
"-----------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK BLK3 WITH FRAME TITLE TEXT-B03.
  SELECTION-SCREEN BEGIN OF LINE.
    PARAMETERS: P_RD2A RADIOBUTTON GROUP RD2 DEFAULT 'X' USER-COMMAND RA2.
    SELECTION-SCREEN COMMENT 10(6) TEXT-T06 FOR FIELD P_RD2A.  "생성
    SELECTION-SCREEN POSITION 20.
    PARAMETERS: P_RD2B RADIOBUTTON GROUP RD2.
    SELECTION-SCREEN COMMENT 22(16) TEXT-T07 FOR FIELD P_RD2B. "결과 조회
  SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK BLK3.

"-----------------------------------------------------------------------
" 조회구분
"-----------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK BLK4 WITH FRAME TITLE TEXT-B05 .
  SELECTION-SCREEN BEGIN OF LINE.
    PARAMETERS: P_RD3A RADIOBUTTON GROUP RD3 DEFAULT 'X' MODIF ID RD3.
    SELECTION-SCREEN COMMENT 3(7) TEXT-T08 FOR FIELD P_RD3A MODIF ID RD3.  "전체
    SELECTION-SCREEN POSITION 12.
    PARAMETERS: P_RD3B RADIOBUTTON GROUP RD3 MODIF ID RD3.
    SELECTION-SCREEN COMMENT 15(7) TEXT-T09 FOR FIELD P_RD3B MODIF ID RD3. "성공
    SELECTION-SCREEN POSITION 26.
    PARAMETERS: P_RD3C RADIOBUTTON GROUP RD3 MODIF ID RD3.
    SELECTION-SCREEN COMMENT 27(7) TEXT-T10 FOR FIELD P_RD3C MODIF ID RD3. "오류
  SELECTION-SCREEN END OF LINE.

  SELECTION-SCREEN BEGIN OF LINE.
    PARAMETERS: P_RD3D type C AS CHECKBOX MODIF ID RD4.
    SELECTION-SCREEN COMMENT (35) TEXT-T15 FOR FIELD P_RD3D MODIF ID RD4.
  SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN END OF BLOCK BLK4.

"-----------------------------------------------------------------------
" 출력 Layout
"-----------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK BLKV WITH FRAME TITLE TEXT-B04.
  PARAMETERS: P_VAR LIKE DISVARIANT-VARIANT.
SELECTION-SCREEN END OF BLOCK BLKV.

*----------------------------------------------------------------------*
*  AT SELECTION-SCREEN
*----------------------------------------------------------------------*
AT SELECTION-SCREEN.

  IF SY-UCOMM EQ GC_FC01.
    PERFORM DOWNLOAD_EXCEL_SMW0.
  ENDIF.

  IF SY-UCOMM = GC_RA.
    CLEAR : P_FILE.
  ENDIF.

*----------------------------------------------------------------------*
* AT SELECTION-SCREEN OUTPUT
*----------------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT.

  PERFORM SELECTION_SCREEN_OUTPUT.

*---------------------------------------------------------------------*
* AT SELECTION-SCREEN ON VALUE-REQUEST FOR
*---------------------------------------------------------------------*
* 업로드 파일 F4
AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_FILE.
  PERFORM FILE_OPEN CHANGING P_FILE.
