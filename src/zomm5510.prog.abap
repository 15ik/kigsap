*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Report ZOMM5110
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Module              : MM
*& Program ID          : ZOMM5510
*& T-CODE              : ZOMM5510
*& Referenced Program  : N/A
*& Created by          : N/A
*& Created On          : N/A
*& Type                : Online
*& Description         : 구매 선급금 반제 처리
*&---------------------------------------------------------------------*
*& Change History
*&---------------------------------------------------------------------*
*& 변경SEQ    변경자        변경일자             변경내용
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
REPORT ZOMM5510 MESSAGE-ID ZMM01.

include ZOMM5510TOP. "Top Include
include ZOMM5510CLS. "ALV/TREE Class
include ZOMM5510SCR. "Selection Screen
include ZOMM5510O01. "Process Before Output
include ZOMM5510I01. "Process After Input
include ZOMM5510F01. "Business Logic Routine
include ZOMM5510F02. "ALV /Tree Logic Routine

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

*  PERFORM CREATE_OBJ.

  PERFORM CHECK_AUTHORITY.  "권한체크

  PERFORM GET_DATA_01.

  PERFORM GET_DATA_02.

  IF GT_DISP_01 IS INITIAL.
    MESSAGE S005(ZMM01) DISPLAY LIKE 'E'.
  ELSE.
    CALL SCREEN '0100'.
  ENDIF.

*----------------------------------------------------------------------*
* END-OF-SELECTION                                                     *
*----------------------------------------------------------------------*
END-OF-SELECTION.
