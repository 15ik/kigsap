*&---------------------------------------------------------------------*
*& Include          ZRMM4090SCR
*&---------------------------------------------------------------------*
"-----------------------------------------------------------------------
" 기본 선택
"-----------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK blk1 WITH FRAME TITLE TEXT-b01.
  PARAMETERS:     p_bukrs LIKE t001-bukrs OBLIGATORY.       "회사코드
  SELECT-OPTIONS: s_lifnr FOR lfa1-lifnr,                   "업체코드
                  s_udate FOR cdhdr-udate OBLIGATORY,       "납품일(PO 기준)
                  s_vbeln FOR likp-vbeln MODIF ID sc1,      "납품서번호
                  s_ebeln FOR ekko-ebeln MODIF ID sc2.      "PO 번호
SELECTION-SCREEN END OF BLOCK blk1.

"-----------------------------------------------------------------------
" 작업 구분
"-----------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK blk2 WITH FRAME TITLE TEXT-b02.
  SELECTION-SCREEN BEGIN OF LINE.
    PARAMETERS: p_rd1a RADIOBUTTON GROUP rd1 DEFAULT 'X' USER-COMMAND ra.
    SELECTION-SCREEN COMMENT 4(20) TEXT-t01 FOR FIELD p_rd1a.  "Inbound Delivery
    SELECTION-SCREEN POSITION 24.
    PARAMETERS: p_rd1b RADIOBUTTON GROUP rd1.
    SELECTION-SCREEN COMMENT 25(20) TEXT-t02 FOR FIELD p_rd1b. "Purchase Order
  SELECTION-SCREEN END OF LINE.

  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN POSITION 1.
    PARAMETERS: p_chk1 AS CHECKBOX DEFAULT ''.
    SELECTION-SCREEN COMMENT 3(20) TEXT-t03. "백그라운드 처리
  SELECTION-SCREEN END OF LINE.

  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN POSITION 1.
    PARAMETERS: p_chk2 AS CHECKBOX DEFAULT ''  USER-COMMAND p2.
    SELECTION-SCREEN COMMENT 3(20) TEXT-t04. "전송 완료 건 조회
  SELECTION-SCREEN END OF LINE.

  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN POSITION 1.
    PARAMETERS: p_chk3 AS CHECKBOX DEFAULT ''.
    SELECTION-SCREEN COMMENT 3(20) TEXT-t05. "삭제 PO 재전송
  SELECTION-SCREEN END OF LINE.

*&    U3      T0210054     2022.05.19
  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN POSITION 1.
    PARAMETERS: p_chk4 AS CHECKBOX DEFAULT '' USER-COMMAND p4.
    SELECTION-SCREEN COMMENT 3(20) TEXT-t06. "변경 PO 전송
  SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN END OF BLOCK blk2.

"-----------------------------------------------------------------------
" 출력 Layout
"-----------------------------------------------------------------------
SELECTION-SCREEN BEGIN OF BLOCK blkv WITH FRAME TITLE TEXT-b03.
  PARAMETERS: p_var LIKE disvariant-variant.
SELECTION-SCREEN END OF BLOCK blkv.

AT SELECTION-SCREEN.
  PERFORM set_screen_chk4.

*----------------------------------------------------------------------*
* AT SELECTION-SCREEN OUTPUT
*----------------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT.

  PERFORM selection_screen_output.

*  PERFORM SET_SCREEN_CHK4.
