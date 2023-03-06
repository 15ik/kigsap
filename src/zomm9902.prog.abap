*&---------------------------------------------------------------------*
*& Report ZOMM9902
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Module              : MM
*& Program ID          : ZOMM9902
*& T-CODE              : ZOMM9902
*& Referenced Program  : N/A
*& Created by          : N/A
*& Created On          : N/A
*& Type                : Online
*& Description         : Open PO 마이그레이션용 프로그램
*&---------------------------------------------------------------------*
*& Change History
*&---------------------------------------------------------------------*
*& 변경SEQ    변경자        변경일자             변경내용
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
REPORT ZOMM9902 MESSAGE-ID ZMM01.

include ZOMM9902TOP. "Top Include
include ZOMM9902CLS. "ALV/TREE Class
include ZOMM9902SCR. "Selection Screen
include ZOMM9902O01. "Process Before Output
include ZOMM9902I01. "Process After Input
include ZOMM9902F01. "Business Logic Routine
include ZOMM9902F02. "ALV /Tree Logic Routine

*----------------------------------------------------------------------*
* Initialization
*----------------------------------------------------------------------*
INITIALIZATION.

  PERFORM INITIALIZATION.

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
