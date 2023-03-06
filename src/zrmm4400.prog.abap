*&---------------------------------------------------------------------*
*& Report ZRMM4400
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Report ZRMM4400
*&---------------------------------------------------------------------*
*& Module              : MM
*& Program ID          : ZRMM4400
*& T-CODE              : ZRMM4400
*& Referenced Program  : N/A
*& Created by          : T0210051
*& Created On          : 2021.03.17
*& Type                : ONLR
*& Description         : 재고 현황 조회
*&---------------------------------------------------------------------*
*& Change History
*&---------------------------------------------------------------------*
*& SEQ  SR번호      변경자     변경일자     변경내용
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
REPORT ZRMM4400 MESSAGE-ID ZMM01 LINE-SIZE 80.

include ZRMM4400TOP. "Top Include
include ZRMM4400CLS. "ALV/TREE Class
include ZRMM4400SCR. "Selection Screen
include ZRMM4400O01. "Process Before Output
include ZRMM4400I01. "Process After Input
include ZRMM4400F01. "Business Logic Routine
include ZRMM4400F02. "ALV /Tree Logic Routine

*----------------------------------------------------------------------*
* Initialization
*----------------------------------------------------------------------*
INITIALIZATION.
  PERFORM INITIALIZATION.

*----------------------------------------------------------------------*
* START-OF-SELECTION                                                   *
*----------------------------------------------------------------------*
START-OF-SELECTION.

  PERFORM CHECK_AUTHORITY.  "권한체크가 필요한 경우

  PERFORM GET_DATA.

  IF GT_DISP[] IS NOT INITIAL.
    DESCRIBE TABLE GT_DISP LINES GV_TCNT. MESSAGE S011(ZMM01) WITH GV_TCNT. " & 건의 데이타가 조회되었습니다.
    CALL SCREEN '0100'.
  ELSE.
    MESSAGE S000 DISPLAY LIKE 'E' WITH TEXT-M01.  "조회 조건에 맞는 재고가 없습니다.
  ENDIF.


*----------------------------------------------------------------------*
* END-OF-SELECTION                                                     *
*----------------------------------------------------------------------*
END-OF-SELECTION.
