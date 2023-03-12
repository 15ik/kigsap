class ZCL_CN_ABAP_UTIL definition
  public
  final
  create public .

public section.

  types:
    ty_rsel_info TYPE TABLE OF rsel_info .
  types:
    BEGIN OF ts_form,
        objname TYPE tabname,
        tabkey  TYPE trobj_name,
      END OF ts_form .
  types:
    ty_form TYPE STANDARD TABLE OF ts_form .
  types:
    BEGIN OF ts_variant,
        report  TYPE rsvar-report,
        variant TYPE rsvar-variant,
      END OF ts_variant .
  types:
    ty_variant TYPE TABLE OF ts_variant .

  class-methods CHECK_GUI_INFO
    returning
      value(RV_SAPGUI) type CHAR1 .
protected section.
private section.

  class-data AV_PERCENT type INT4 .
  class-data AC_PATH_EXCEL type CHAR50 value 'Excel(*.XLSX)' ##NO_TEXT.
ENDCLASS.



CLASS ZCL_CN_ABAP_UTIL IMPLEMENTATION.


  METHOD check_gui_info.
*----------------------------------------
* 샙구이 여부
* Web Gui 이 일경우 rv_sapgui = space
*----------------------------------------
    CALL FUNCTION 'GUI_GET_DESKTOP_INFO'
      EXPORTING
        type   = 1
      CHANGING
        return = rv_sapgui.

    CHECK rv_sapgui IS NOT INITIAL.

    rv_sapgui = 'X'.


  ENDMETHOD.
ENDCLASS.
