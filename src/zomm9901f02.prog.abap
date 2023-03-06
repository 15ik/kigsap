*&---------------------------------------------------------------------*
*& Include          ZOMM9901F02
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form SET_GRID
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM SET_GRID.

  IF GRF_DOCKING_CON IS INITIAL.

* Creating Docking container instance
    PERFORM CREATE_CONTAINER.
*--------------------------------
* Create Alv Grid
*--------------------------------
    PERFORM CREATE_ALV_GRID.

*--------------------------------
* Dislay Grid..
*--------------------------------
    GRF_GRID->SET_GRID( CHANGING  CT_DATA = GT_DISP ).

  ELSE.
    GRF_GRID->REFRESH_GRID_DISPLAY( ).
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CREATE_CONTAINER
*&---------------------------------------------------------------------*
FORM CREATE_CONTAINER.

*----------------------------------------------------
* Create Docking Container..
*----------------------------------------------------
  CREATE OBJECT GRF_DOCKING_CON
    EXPORTING
      REPID     = SY-REPID    "프로그램명 id
      DYNNR     = SY-DYNNR    "화면번호(Screen)
      SIDE      = GRF_DOCKING_CON->DOCK_AT_TOP
      EXTENSION = 10000.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CREATE_ALV_GRID
*&---------------------------------------------------------------------*
FORM CREATE_ALV_GRID.

  DATA: LS_TOOLBTN TYPE ZSCN00004.

*--------------------------------------------------
* User Toolbar Button (Default Setting).
*--------------------------------------------------
  LS_TOOLBTN-BTN_EXLD   = 'X'.       "Excel Download

  CREATE OBJECT GRF_GRID
    EXPORTING
      IV_NAME    = 'ALV_GRID'   "다수의 그리드일 경우 식별하기 위함..
      IRF_PARENT = GRF_DOCKING_CON
      IS_TOOLBTN = LS_TOOLBTN
      IV_CELLC   = ''           "공통 스트럭쳐 CELLC필드 사용하지 않을 경우
      IV_CELLS   = ''.          "공통 스트럭쳐 CELLS필드 사용하지 않을 경우

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CHECK_EXIT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form CHECK_EXIT
*&---------------------------------------------------------------------*
FORM CHECK_EXIT.

  CHECK GRF_GRID->POP_TO_MSG( IV_TYPE  = 'A'
                              IV_TITLE = ZCL_CN_ALV_GRID=>AC_MSG_TITLE2    "Exit Confirm
                              IV_TEXT1 = CONV #( TEXT-M20 ) "저장되지 않은 데이터는 사라집니다.
                              IV_TEXT2 = ZCL_CN_ALV_GRID=>AC_MSG_EXIT2 )   "화면에서 나가시겠습니까?
                              EQ ABAP_TRUE. "YES

  CASE GV_OK_CODE.
    WHEN 'EXIT'.
      LEAVE PROGRAM.
    WHEN OTHERS.
      LEAVE TO SCREEN 0.
  ENDCASE.

ENDFORM.
