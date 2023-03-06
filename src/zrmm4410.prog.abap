*&--------------------------------------------------------------------*
*& Report ZRMM4410
*&--------------------------------------------------------------------*
*&
*&--------------------------------------------------------------------*
*&--------------------------------------------------------------------*
*& Module              : MM
*& Program ID          : ZRMM4410
*& T-CODE              : ZRMM4410
*& Referenced Program  : N/A
*& Created by          : N/A
*& Created On          : N/A
*& Type                : Report
*& Description         : 재고 일 수불부(기간)
*&--------------------------------------------------------------------*
*& Change History
*&--------------------------------------------------------------------*
*& 변경SEQ    변경자        변경일자             변경내용
*&--------------------------------------------------------------------*
*&--------------------------------------------------------------------*
REPORT ZRMM4410 MESSAGE-ID ZMM01 LINE-SIZE 80.

include ZRMM4410TOP. "Top Include
include ZRMM4410CLS. "ALV/TREE Class
include ZRMM4410SCR. "Selection Screen
include ZRMM4410O01. "Process Before Output
include ZRMM4410I01. "Process After Input
include ZRMM4410F01. "Business Logic Routine
include ZRMM4410F02. "ALV /Tree Logic Routine

*----------------------------------------------------------------------*
* Initialization
*----------------------------------------------------------------------*
INITIALIZATION.
  PERFORM INITIALIZATION.

*----------------------------------------------------------------------*
* START-OF-SELECTION                                                   *
*----------------------------------------------------------------------*
START-OF-SELECTION.

  PERFORM CHECK_INPUT_DATA. "U4(+)

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
