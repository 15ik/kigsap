*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Report ZRMM4430
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Module              : MM
*& Program ID          : ZRMM4430
*& T-CODE              : ZRMM4430
*& Referenced Program  : N/A
*& Created by          : N/A
*& Created On          : N/A
*& Type                : R
*& Description         : 배치 이력 조회
*&---------------------------------------------------------------------*
*& Change History
*&---------------------------------------------------------------------*
*& 변경SEQ    변경자        변경일자             변경내용
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
REPORT ZRMM4430 MESSAGE-ID ZCN00 LINE-SIZE 80.

include ZRMM4430TOP. "Top Include
include ZRMM4430CLS. "ALV/TREE Class
include ZRMM4430SCR. "Selection Screen
include ZRMM4430O01. "Process Before Output
include ZRMM4430I01. "Process After Input
include ZRMM4430F01. "Business Logic Routine
include ZRMM4430F02. "ALV /Tree Logic Routine.

*----------------------------------------------------------------------*
* Initialization
*----------------------------------------------------------------------*
INITIALIZATION.
  PERFORM INITIALIZATION.

*----------------------------------------------------------------------*
* START-OF-SELECTION
*----------------------------------------------------------------------*
START-OF-SELECTION.

  PERFORM CHECK_AUTHORITY.  "권한체크가 필요한 경우

  PERFORM GET_DATA.

  IF GT_DISP[] IS NOT INITIAL.
    DESCRIBE TABLE GT_DISP LINES GV_TCNT. MESSAGE S011(ZMM01) WITH GV_TCNT. " & 건의 데이타가 조회되었습니다.
    CALL SCREEN '0100'.
  ELSE.
    MESSAGE S000 DISPLAY LIKE 'E' WITH TEXT-M01.  "조회 조건과 일치하는 정보가 없습니다.
  ENDIF.


*----------------------------------------------------------------------*
* END-OF-SELECTION
*----------------------------------------------------------------------*
END-OF-SELECTION.
