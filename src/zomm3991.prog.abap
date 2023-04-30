*&---------------------------------------------------------------------*
*& Report ZOMM3991
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Module              : MM
*& Program ID          : ZOMM3991
*& T-CODE              : ZOMM3991
*& Referenced Program  : N/A
*& Created by          : T0210053
*& Created On          : 2021.08.20
*& Type                : Online
*& Description         : Open PO 마이그레이션용 프로그램
*&---------------------------------------------------------------------*
*& Change History
*&---------------------------------------------------------------------*
*& 변경SEQ    변경자        변경일자             변경내용
*&---------------------------------------------------------------------*
*&    N       T0210053      2021.08.20          최초생성
*&    U1
*&---------------------------------------------------------------------*
REPORT ZOMM3991 MESSAGE-ID ZMM01.

INCLUDE ZOMM3991TOP.        "Top Include
INCLUDE ZOMM3991CLS.        "ALV/TREE Class
INCLUDE ZOMM3991SCR.        "Selection Screen
INCLUDE ZOMM3991O01.        "Process Before Output
INCLUDE ZOMM3991I01.        "Process After Input
INCLUDE ZOMM3991F01.        "Business Logic Routine
INCLUDE ZOMM3991F02.        "ALV /Tree Logic Routine

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
