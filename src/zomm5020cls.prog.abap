*&---------------------------------------------------------------------*
*& Include          ZOMM5020CLS
*&---------------------------------------------------------------------*
*---------------------------------------------------------------------*
*  CLASSES lcl_alv_receiver Definition
*---------------------------------------------------------------------*
CLASS LCL_ALV_RECEIVER DEFINITION.

  PUBLIC SECTION.
    METHODS  HANDLE_TOOLBAR
                  FOR EVENT TOOLBAR OF CL_GUI_ALV_GRID
      IMPORTING E_OBJECT
                  E_INTERACTIVE
                  SENDER.

    METHODS  HANDLE_USER_COMMAND
                  FOR EVENT USER_COMMAND OF CL_GUI_ALV_GRID
      IMPORTING E_UCOMM SENDER.

    METHODS  HANDLE_HOTSPOT_CLICK
                  FOR EVENT HOTSPOT_CLICK OF CL_GUI_ALV_GRID
      IMPORTING E_ROW_ID
                  E_COLUMN_ID
                  SENDER.

    METHODS  HANDLE_DOUBLE_CLICK
                  FOR EVENT DOUBLE_CLICK OF CL_GUI_ALV_GRID
      IMPORTING E_COLUMN
                  ES_ROW_NO
                  SENDER.


    METHODS HANDLE_DATA_CHANGED
                  FOR EVENT DATA_CHANGED OF CL_GUI_ALV_GRID
      IMPORTING ER_DATA_CHANGED
                  SENDER.

    METHODS HANDLE_DATA_CHANGED_FINISHED
                  FOR EVENT DATA_CHANGED_FINISHED OF CL_GUI_ALV_GRID
      IMPORTING E_MODIFIED
                  ET_GOOD_CELLS
                  SENDER.


ENDCLASS. " lcl_alv_receiver
*---------------------------------------------------------------------*
*  CLASSES lcl_alv_receiver Implementation
*---------------------------------------------------------------------*
CLASS LCL_ALV_RECEIVER IMPLEMENTATION.

* ToolBar
  METHOD HANDLE_TOOLBAR.
*    PERFORM MAKE_TOOLBAR USING E_OBJECT E_INTERACTIVE SENDER.
  ENDMETHOD.                    "handle_toolbar

* User command
  METHOD HANDLE_USER_COMMAND.
*    PERFORM USER_COMMAND USING E_UCOMM SENDER.
  ENDMETHOD.                   "handle_user_command


* HOTSPOT
  METHOD HANDLE_HOTSPOT_CLICK.
*    PERFORM HOTSPOT_CLICK USING E_ROW_ID E_COLUMN_ID SENDER.
  ENDMETHOD.                    "handle_hotspot_click


* DOUBLE CLICK
  METHOD HANDLE_DOUBLE_CLICK.
*    PERFORM DOUBLE_CLICK USING E_COLUMN ES_ROW_NO SENDER.
  ENDMETHOD.                  "handle_double_click

* Data change
  METHOD HANDLE_DATA_CHANGED.
*    PERFORM HANDLE_DATA_CHANGED USING ER_DATA_CHANGED SENDER.
  ENDMETHOD.                      " HANDLE_DATA_CHANGED

* changed finished
  METHOD HANDLE_DATA_CHANGED_FINISHED.
*    PERFORM  HANDLE_DATA_CHANGED_FINISHED USING E_MODIFIED
*                                                ET_GOOD_CELLS
*                                                SENDER.
  ENDMETHOD.

ENDCLASS. " lcl_alv_receiver
