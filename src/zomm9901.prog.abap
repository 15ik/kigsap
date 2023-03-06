*&---------------------------------------------------------------------*
*& Report ZOMM9901
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Module              : MM
*& Program ID          : ZOMM9901
*& T-CODE              : ZOMM9901
*& Referenced Program  : N/A
*& Created by          : N/A
*& Created On          : N/A
*& Type                : Online
*& Description         : PR 일괄 생성 프로그램
*&---------------------------------------------------------------------*
*& Change History
*&---------------------------------------------------------------------*
*& 변경SEQ    변경자        변경일자             변경내용
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
REPORT ZOMM9901 MESSAGE-ID ZMM01.

include ZOMM9901TOP. "Top Include
include ZOMM9901CLS. "ALV/TREE Class
include ZOMM9901SCR. "Selection Screen
include ZOMM9901O01. "Process Before Output
include ZOMM9901I01. "Process After Input
include ZOMM9901F01. "Business Logic Routine
include ZOMM9901F02. "ALV /Tree Logic Routine

*----------------------------------------------------------------------*
* START-OF-SELECTION                                                   *
*----------------------------------------------------------------------*
START-OF-SELECTION.

  IF SY-BATCH IS NOT INITIAL. EXIT. ENDIF.

  PERFORM CREATE_OBJ.

  PERFORM CHECK_AUTHORITY.  "권한체크

  PERFORM UPLOAD_DATA.

  PERFORM PROCESSING_DATA.

  IF GT_DISP IS INITIAL.
    MESSAGE S005(ZMM01) DISPLAY LIKE 'E'.
  ELSE.
    CALL SCREEN '0100'.
  ENDIF.

*----------------------------------------------------------------------*
* END-OF-SELECTION                                                     *
*----------------------------------------------------------------------*
END-OF-SELECTION.
