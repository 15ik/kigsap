*&---------------------------------------------------------------------*
*& Include          ZOMM5010O01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      MODULE  STATUS_0001  OUTPUT
*&---------------------------------------------------------------------*
*       TEXT
*----------------------------------------------------------------------*
MODULE STATUS_0001 OUTPUT.

  SET PF-STATUS 'ZOMM5010' EXCLUDING GT_UI_FUNC.
  SET TITLEBAR  'T0001' WITH '대급지급 요청'(T17).

ENDMODULE. " STATUS_0001 OUTPUT
*&---------------------------------------------------------------------*
*&      MODULE  ICON_CREATE  OUTPUT
*&---------------------------------------------------------------------*
*       TEXT
*----------------------------------------------------------------------*
MODULE ICON_CREATE OUTPUT.

  PERFORM ICON_CREATE_TOGGLE.

ENDMODULE. " ICON_CREATE OUTPUT
*&---------------------------------------------------------------------*
*&      MODULE  EVENT_PBO  OUTPUT
*&---------------------------------------------------------------------*
*       TEXT
*----------------------------------------------------------------------*
MODULE EVENT_PBO_0061 OUTPUT.

  PERFORM EVNET_PBO_0061.

  IF NOT GV_LAST_CURSOR_61 IS INITIAL.
    SET CURSOR FIELD GV_LAST_CURSOR_61.
  ENDIF.

ENDMODULE. " EVENT_PBO OUTPUT
*&---------------------------------------------------------------------*
*&      MODULE  CREATE_CONTAINER_HEAD  OUTPUT
*&---------------------------------------------------------------------*
*       TEXT
*----------------------------------------------------------------------*
MODULE CREATE_CONTAINER_HEAD OUTPUT.

  CHECK GV_CUSTOM_CONTAINER_HEAD IS NOT BOUND.

  CREATE OBJECT GV_CUSTOM_CONTAINER_HEAD
    EXPORTING
      CONTAINER_NAME              = GC_CONTAINER_HEAD
    EXCEPTIONS
      CNTL_ERROR                  = 1
      CNTL_SYSTEM_ERROR           = 2
      CREATE_ERROR                = 3
      LIFETIME_ERROR              = 4
      LIFETIME_DYNPRO_DYNPRO_LINK = 5
      OTHERS                      = 6.

  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
               WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

  PERFORM BUILD_ALV_HEAD.


ENDMODULE. " CREATE_CONTAINER_HEAD OUTPUT
*&---------------------------------------------------------------------*
*&      MODULE  SET_NETDT  OUTPUT
*&---------------------------------------------------------------------*
*       TEXT
*----------------------------------------------------------------------*
MODULE SET_NETDT OUTPUT.
  IF GS_SCR100-ZFBDT IS INITIAL.
    PERFORM SET_NETDT.
  ENDIF.
ENDMODULE. " SET_NETDT OUTPUT
*&---------------------------------------------------------------------*
*& Module STATUS_0071 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE STATUS_0071 OUTPUT.
  SET PF-STATUS 'POPUP'.
  SET TITLEBAR 'T0001' WITH '선급금현황'(T91).
ENDMODULE.
*&---------------------------------------------------------------------*
*& Module SET_ALV_SCREEN_0071 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE SET_ALV_SCREEN_0071 OUTPUT.

  PERFORM SET_GRID_0071.

ENDMODULE.
