*&---------------------------------------------------------------------*
*& Include          ZOMM5010CLS
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
* Define Alv Grid
*----------------------------------------------------------------------*
CLASS LCL_GRID_EVENT_RECEIVER DEFINITION DEFERRED.
 CLASS LCL_GUI_ALV_GRID DEFINITION DEFERRED.

 DATA GV_GRID_EVENT_RECEIVER TYPE REF TO LCL_GRID_EVENT_RECEIVER.

 DATA: GV_CUSTOM_CONTAINER_HEAD TYPE REF TO CL_GUI_CUSTOM_CONTAINER.

 DATA: GV_GRID_HEAD TYPE REF TO CL_GUI_ALV_GRID,
       GV_GRID_LEFT              TYPE REF TO CL_GUI_ALV_GRID.

*----------------------------------------------------------------------*
*  Tree Control
*----------------------------------------------------------------------*
 DATA: GV_DOCKING_CONTAINER TYPE REF TO CL_GUI_DOCKING_CONTAINER.

*----------------------------------------------------------------------*
* Define Event
*----------------------------------------------------------------------*
CLASS LCL_GRID_EVENT_RECEIVER DEFINITION.
   PUBLIC SECTION.
     METHODS HANDLE_DOUBLE_CLICK
             FOR EVENT DOUBLE_CLICK OF CL_GUI_ALV_GRID
             IMPORTING E_ROW
                       E_COLUMN
                       ES_ROW_NO
                       SENDER.

     METHODS HANDLE_TOOLBAR
             FOR EVENT TOOLBAR OF CL_GUI_ALV_GRID
             IMPORTING E_OBJECT
                       E_INTERACTIVE
                       SENDER.

     METHODS HANDLE_USER_COMMAND
             FOR EVENT USER_COMMAND OF CL_GUI_ALV_GRID
             IMPORTING E_UCOMM
                       SENDER.

     METHODS HANDLE_HOTSPOT_CLICK
             FOR EVENT HOTSPOT_CLICK OF CL_GUI_ALV_GRID
             IMPORTING E_ROW_ID
                       E_COLUMN_ID
                       ES_ROW_NO
                       SENDER.

     METHODS HANDLE_DATA_CHANGED
             FOR EVENT DATA_CHANGED OF CL_GUI_ALV_GRID
             IMPORTING ER_DATA_CHANGED
                       SENDER.


     METHODS HANDLE_ON_F4
             FOR EVENT ONF4 OF CL_GUI_ALV_GRID
             IMPORTING  SENDER
                        E_FIELDNAME
                        E_FIELDVALUE
                        ES_ROW_NO
                        ER_EVENT_DATA
                        ET_BAD_CELLS
                        E_DISPLAY.

     METHODS HANDLE_DATA_CHANGED_FINISHED
             FOR EVENT DATA_CHANGED_FINISHED OF CL_GUI_ALV_GRID
             IMPORTING E_MODIFIED
                       ET_GOOD_CELLS
                       SENDER.

 ENDCLASS. "LCL_GRID_EVENT_RECEIVER DEFINITION


*----------------------------------------------------------------------*
*       CLASS LCL_GRID_EVENT_RECEIVER IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS LCL_GRID_EVENT_RECEIVER IMPLEMENTATION.
   METHOD HANDLE_DOUBLE_CLICK.
     PERFORM HANDLE_DOUBLE_CLICK USING E_ROW E_COLUMN ES_ROW_NO SENDER.
   ENDMETHOD.                    "HANDLE_DATA_CHANGED

   METHOD HANDLE_TOOLBAR.
     PERFORM HANDLE_TOOLBAR USING E_OBJECT E_INTERACTIVE SENDER.
   ENDMETHOD.                    "HANDLE_TOOLBAR

   METHOD HANDLE_USER_COMMAND.
     PERFORM HANDLE_USER_COMMAND USING E_UCOMM SENDER.
   ENDMETHOD.                    "HANDLE_USER_COMMAND

   METHOD HANDLE_HOTSPOT_CLICK.
     PERFORM HANDLE_HOTSPOT_CLICK USING E_ROW_ID E_COLUMN_ID ES_ROW_NO SENDER.
   ENDMETHOD.                    "HANDLE_HOTSPOT_CLICK

   METHOD HANDLE_DATA_CHANGED.
     PERFORM HANDLE_DATA_CHANGED USING ER_DATA_CHANGED SENDER.
   ENDMETHOD.                    "HANDLE_DATA_CHANGED

   METHOD HANDLE_ON_F4.
     PERFORM HANDLE_ON_F4 USING SENDER
                                E_FIELDNAME
                                E_FIELDVALUE
                                ES_ROW_NO
                                ER_EVENT_DATA
                                ET_BAD_CELLS
                                E_DISPLAY.
   ENDMETHOD.                    "HANDLE_ON_F4
   METHOD HANDLE_DATA_CHANGED_FINISHED.
     PERFORM  HANDLE_DATA_CHANGED_FINISHED USING E_MODIFIED
                                                 ET_GOOD_CELLS
                                                 SENDER.
   ENDMETHOD.
 ENDCLASS. " LCL_GRID_EVENT_RECEIVER IMPLEMENTATION


*----------------------------------------------------------------------*
*       CLASS LCL_GUI_ALV_GRID DEFINITION
*----------------------------------------------------------------------*
CLASS LCL_GUI_ALV_GRID DEFINITION INHERITING FROM CL_GUI_ALV_GRID.

   PUBLIC SECTION.

     METHODS CONSTRUCTOR
       IMPORTING
         IV_PARENT TYPE REF TO CL_GUI_CUSTOM_CONTAINER.


 ENDCLASS. "LCL_GUI_ALV_GRID DEFINITION


*----------------------------------------------------------------------*
*       CLASS LCL_GUI_ALV_GRID IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS LCL_GUI_ALV_GRID IMPLEMENTATION.

   METHOD CONSTRUCTOR.
     SUPER->CONSTRUCTOR( IV_PARENT ).
     ME->SET_DELAY_CHANGE_SELECTION( 1 ).
   ENDMETHOD.                    "constructor

 ENDCLASS. "LCL_GUI_ALV_GRID IMPLEMENTATION
