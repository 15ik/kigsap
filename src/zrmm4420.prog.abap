*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Report ZRMM4420
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Module              : MM
*& Program ID          : ZRMM4420
*& T-CODE              : ZRMM4420
*& Referenced Program  : N/A
*& Created by          : N/A
*& Created On          : N/A
*& Type                : R
*& Description         : 기간별 재고 현황
*&---------------------------------------------------------------------*
*& Change History
*&---------------------------------------------------------------------*
*& SEQ     SRNO      변경자    변경일자      변경내용
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ZRMM4420 MESSAGE-ID ZMM01 LINE-SIZE 80.

include ZRMM4420TOP. "Top Include
include ZRMM4420CLS. "ALV/TREE Class
include ZRMM4420SCR. "Selection Screen
include ZRMM4420O01. "Process Before Output
include ZRMM4420I01. "Process After Input
include ZRMM4420F01. "Business Logic Routine
include ZRMM4420F02. "ALV /Tree Logic Routine

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

  PERFORM PROCESSING_DATA.

  IF GT_DISP[] IS NOT INITIAL.
    DESCRIBE TABLE GT_DISP LINES GV_TCNT. MESSAGE S011(ZMM01) WITH GV_TCNT. " & 건의 데이타가 조회되었습니다.
    CALL SCREEN '0100'.
  ELSE.
    IF GV_ERROR NE 'X'.
      MESSAGE S000 DISPLAY LIKE 'E' WITH TEXT-M01.  "조회 조건에 맞는 재고가 없습니다.
    ELSE.
      MESSAGE S000 DISPLAY LIKE 'E' WITH TEXT-M02.  "Daily의 경우 날짜 간격이 365일(1년)을 초과할 수 없습니다.
    ENDIF.
  ENDIF.

*----------------------------------------------------------------------*
* END-OF-SELECTION                                                     *
*----------------------------------------------------------------------*
END-OF-SELECTION.
