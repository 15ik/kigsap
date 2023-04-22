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
  class-methods EXCL_SMW0_DOWNLOAD
    importing
      !IV_OBJID type ANY
      !IV_FILENAME type ANY optional
      !IV_EXT type ANY optional .
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


METHOD EXCL_SMW0_DOWNLOAD.
*&---------------------------------------------------------------------*
*& Module              : CN
*& Program ID          : ZCL_CN_ALV_GRID
*&---------------------------------------------------------------------*
*& Change History
*&---------------------------------------------------------------------*
*& 변경SEQ    변경자        변경일자             변경내용
*&---------------------------------------------------------------------*
*    CONSTANTS :lc_path_excel(50) TYPE c  VALUE 'Excel (*.XLSX)'.
    DATA : LS_WWWDATA_ITEM TYPE WWWDATATAB.

    DATA : LT_MIME TYPE TABLE OF W3MIME .

    DATA : LV_FILENAME     TYPE STRING,
           LV_PATH         TYPE STRING,
           LV_UP_PATH      TYPE STRING,
           LV_DN_PATH      TYPE STRING,
           LV_FNAME        TYPE STRING,
           LV_RETURN       TYPE ABAP_BOOL,
           LV_FULLPATH     TYPE STRING,
           LV_FILESIZE(10),
           LV_SIZE         TYPE I,
           LT_CONTENT_BIN  TYPE BBPT_ATT_CONT,
           LV_RC           TYPE I,
           LV_EXT          TYPE STRING,
           LV_FILE_FILTER  TYPE STRING.

    DATA : LO_EXCEL    TYPE OLE2_OBJECT,
           LO_BOOKS    TYPE OLE2_OBJECT,
           LO_WORKBOOK TYPE OLE2_OBJECT,
           LO_SHEETS   TYPE OLE2_OBJECT,
           LO_SHEET    TYPE OLE2_OBJECT.

    DATA:LV_DEST TYPE LOCALFILE.

    LV_FNAME = COND #( WHEN IV_FILENAME IS NOT INITIAL THEN IV_FILENAME
                       ELSE IV_OBJID ).

    DATA(LV_GUI) = ZCL_CN_ABAP_UTIL=>CHECK_GUI_INFO( ) .

*---------------------------------------------
* Select WWW 오브젝트 저장에 대한 INDX-유형
*---------------------------------------------
    SELECT SINGLE RELID,
                  OBJID,
                  CHECKOUT,
                  CHECKNEW,
                  CHNAME,
                  TDATE,
                  TTIME,
                  TEXT
      FROM  WWWDATA
      INTO CORRESPONDING FIELDS OF @LS_WWWDATA_ITEM
     WHERE OBJID = @IV_OBJID.   "<-- SMW0 Object 명

    IF SY-SUBRC NE 0.
      MESSAGE S011(ZCN00) DISPLAY LIKE 'E' WITH TEXT-E08.  "011 &1 데이타가 없습니다.
      EXIT.
    ENDIF.

*-------------------------------
* Get Import Web Objects
*-------------------------------
    CALL FUNCTION 'WWWDATA_IMPORT'
      EXPORTING
        KEY               = LS_WWWDATA_ITEM
      TABLES
        MIME              = LT_MIME
      EXCEPTIONS
        WRONG_OBJECT_TYPE = 1
        IMPORT_ERROR      = 2
        OTHERS            = 3.
*-------------------------------
* File Read
*-------------------------------
    CALL FUNCTION 'WWWPARAMS_READ'
      EXPORTING
        RELID = LS_WWWDATA_ITEM-RELID
        OBJID = LS_WWWDATA_ITEM-OBJID
        NAME  = 'filesize'
      IMPORTING
        VALUE = LV_FILESIZE.

*-------------------------------
* File Open Dialog
*-------------------------------
    TRY.
        CALL METHOD CL_GUI_FRONTEND_SERVICES=>GET_UPLOAD_DOWNLOAD_PATH
          CHANGING
            UPLOAD_PATH                 = LV_UP_PATH
            DOWNLOAD_PATH               = LV_DN_PATH
          EXCEPTIONS
            CNTL_ERROR                  = 1
            ERROR_NO_GUI                = 2
            NOT_SUPPORTED_BY_GUI        = 3
            GUI_UPLOAD_DOWNLOAD_PATH    = 4
            UPLOAD_DOWNLOAD_PATH_FAILED = 5
            OTHERS                      = 6.

        "-- 확장자 입력값에 따른 처리
        IF IV_EXT IS INITIAL OR IV_EXT EQ 'XLSX'.
          CONCATENATE LV_FNAME '.XLSX' INTO   LV_FNAME.
          LV_EXT = 'XLSX'.
          LV_FILE_FILTER = AC_PATH_EXCEL.
        ELSE.
          CONCATENATE LV_FNAME '.' IV_EXT INTO   LV_FNAME.
          LV_EXT = IV_EXT.
          CLEAR LV_FILE_FILTER.
        ENDIF.


        CALL METHOD CL_GUI_FRONTEND_SERVICES=>FILE_SAVE_DIALOG
          EXPORTING
            WINDOW_TITLE      = 'Download'
            DEFAULT_EXTENSION = LV_EXT "'XLSX'
            DEFAULT_FILE_NAME = LV_FNAME
            INITIAL_DIRECTORY = LV_DN_PATH
            FILE_FILTER       = LV_FILE_FILTER
          CHANGING
            FILENAME          = LV_FILENAME
            PATH              = LV_PATH
            FULLPATH          = LV_FULLPATH.

      CATCH CX_ROOT INTO DATA(LS_ROOT).

    ENDTRY.
    CHECK LV_FILENAME IS NOT INITIAL.

    CASE LV_GUI.
      WHEN 'X'.   "Sapgui


*-------------------------------
* Check File Exists
*-------------------------------
        CALL METHOD CL_GUI_FRONTEND_SERVICES=>FILE_EXIST
          EXPORTING
            FILE                 = LV_FILENAME
          RECEIVING
            RESULT               = LV_RETURN
          EXCEPTIONS
            CNTL_ERROR           = 1
            ERROR_NO_GUI         = 2
            WRONG_PARAMETER      = 3
            NOT_SUPPORTED_BY_GUI = 4
            OTHERS               = 5.

        IF LV_RETURN EQ ABAP_TRUE.
*-------------------------------
* Delete File
*-------------------------------
          CLEAR : LV_RETURN .
          CALL METHOD CL_GUI_FRONTEND_SERVICES=>FILE_DELETE
            EXPORTING
              FILENAME             = LV_FILENAME
            CHANGING
              RC                   = LV_RC
            EXCEPTIONS
              FILE_DELETE_FAILED   = 1
              CNTL_ERROR           = 2
              ERROR_NO_GUI         = 3
              FILE_NOT_FOUND       = 4
              ACCESS_DENIED        = 5
              UNKNOWN_ERROR        = 6
              NOT_SUPPORTED_BY_GUI = 7
              WRONG_PARAMETER      = 8
              OTHERS               = 9.
          IF LV_RC NE 0 .
            MESSAGE S000(ZCN00) WITH TEXT-E09  DISPLAY LIKE 'E'.   "파일이 열려 있습니다.
            EXIT.
          ENDIF.
        ENDIF.
      WHEN OTHERS.
        "Web Gui
*                CONCATENATE 'Z:\' lv_fname '.XLSX' INTO   lv_fname.
    ENDCASE.

*---------------------------------
* Gui DOwnload
*---------------------------------
    LV_SIZE = LV_FILESIZE.
    CALL FUNCTION 'GUI_DOWNLOAD'
      EXPORTING
        FILENAME     = LV_FILENAME
        FILETYPE     = 'BIN'
        BIN_FILESIZE = LV_SIZE
      TABLES
        DATA_TAB     = LT_MIME.

    CHECK LV_GUI = 'X'.
*---------------------------------
* Excel 실행
*---------------------------------
    CALL METHOD CL_GUI_FRONTEND_SERVICES=>EXECUTE
      EXPORTING
        DOCUMENT               = LV_FILENAME
      EXCEPTIONS
        CNTL_ERROR             = 1
        ERROR_NO_GUI           = 2
        BAD_PARAMETER          = 3
        FILE_NOT_FOUND         = 4
        PATH_NOT_FOUND         = 5
        FILE_EXTENSION_UNKNOWN = 6
        ERROR_EXECUTE_FAILED   = 7
        SYNCHRONOUS_FAILED     = 8
        NOT_SUPPORTED_BY_GUI   = 9
        OTHERS                 = 10.
    IF SY-SUBRC IS NOT INITIAL.
      MESSAGE E162(ALVHT).
      EXIT.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
