class ZCL_CN_ALV_GRID definition
  public
  inheriting from CL_GUI_ALV_GRID
  create public .

**************************************************************************
* Public section of class. *
**************************************************************************
public section.

  types:
    BEGIN OF ts_info_cnt,
        fname       TYPE fieldname,
        coltext     TYPE lvc_txtcol,
        statu       TYPE icon_d,
        count       TYPE i,
        count_t(10),
        ftext(106)  TYPE c,
      END OF ts_info_cnt .
  types:
    ty_info_cnt TYPE TABLE OF ts_info_cnt
                           WITH NON-UNIQUE SORTED KEY zfd COMPONENTS statu .
  types:
    BEGIN OF ts_gui_status,
        edit      TYPE char1,
        fcode(10),
      END OF ts_gui_status .
  types:
    ty_gui_status TYPE TABLE OF ts_gui_status .
  types:
*---------------------------------------
* History 관리
*---------------------------------------
    BEGIN OF ts_history,
        dkey(255).
        .INCLUDE TYPE ztcn00300.
    TYPES:END OF ts_history .
  types:
*-- History (Set Grid 시점에 사용)
    BEGIN OF ts_hist_type ,
        hst_gub(1),   "Set Grid 시점 사용 'X'
        hst_tabnm  TYPE tabname,
      END OF ts_hist_type .
  types:
    ty_history TYPE TABLE OF ts_history .
  types:
    ty_fcat TYPE TABLE OF lvc_s_fcat
                                            WITH NON-UNIQUE SORTED KEY zfd COMPONENTS fieldname .
  types:
    BEGIN OF ts_field,
        fieldname  TYPE fieldname,
        value(132),
        key        TYPE char1,
      END OF ts_field .
  types:
    tt_field TYPE TABLE OF ts_field .
  types:
*---------------------
* Alv Header Type
*---------------------
    BEGIN OF ts_header,
        typ   TYPE   char01,
        key   TYPE   char20,
        info  TYPE  char30,
        text  TYPE  char255,
        color TYPE sdydo_attribute,
        bold  TYPE char01,
      END OF ts_header .
  types:
    tt_header TYPE TABLE OF ts_header .
  types:
*----------------------------------------
* XML UPLOAD Type 선언
*----------------------------------------
**************************************************************************
* Protected section of class. *
**************************************************************************
    BEGIN OF ts_tcurr,
        waers  TYPE waers,
        factor TYPE i,
      END OF ts_tcurr .
  types:
    tt_tcurr TYPE TABLE OF ts_tcurr WITH EMPTY KEY .
  types:
    BEGIN OF ty_xml_itab,
        row    TYPE i,
        column TYPE string,
        cell   TYPE string,
        value  TYPE string,
      END OF  ty_xml_itab .
  types:
    ty_i_xml_itab TYPE STANDARD TABLE OF ty_xml_itab INITIAL SIZE 0 .
  types:
    BEGIN OF ty_xml_sheet_itab,
        row   TYPE i,
        cell  TYPE string,
        type  TYPE string,
        index TYPE i,
        value TYPE string,
      END OF   ty_xml_sheet_itab .
  types:
    ty_i_xml_sheet_itab TYPE STANDARD TABLE OF ty_xml_sheet_itab INITIAL SIZE 0 .
  types:
    BEGIN OF ty_xml_shared_str_itab,
        index TYPE i,
        value TYPE string,
      END OF   ty_xml_shared_str_itab .
  types:
    ty_i_xml_shared_str_itab TYPE STANDARD TABLE OF ty_xml_shared_str_itab INITIAL SIZE 0 .
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

  class-data AC_MSG_CONT type STRING value '계속 하시겠습니까?' ##NO_TEXT.
  class-data AC_MSG_DEL1 type STRING value '선택된 데이타를 삭제하려고 합니다.' ##NO_TEXT.
  class-data AC_MSG_DEL2 type STRING value '삭제하시겠습니까?' ##NO_TEXT.
  class-data AC_MSG_EXIT type STRING value '변경된 데이타가 있습니다.' ##NO_TEXT.
  class-data AC_MSG_EXIT2 type STRING value '화면에서 나가시겠습니까?' ##NO_TEXT.
  class-data AC_MSG_MSG1 type STRING value '계속 하시겠습니까?' ##NO_TEXT.
  class-data AC_MSG_REC1 type STRING value '선택된 데이타를 복구하려고 합니다.' ##NO_TEXT.
  class-data AC_MSG_REC2 type STRING value '복구하시겠습니까?' ##NO_TEXT.
  class-data AC_MSG_SAVE type STRING value '저장하시겠습니까?' ##NO_TEXT.
  class-data AC_MSG_TITLE type STRING value '저장확인' ##NO_TEXT.
  class-data AC_MSG_TITLE2 type STRING value 'Exit Confirm' ##NO_TEXT.
  class-data AC_MSG_TITLE3 type STRING value '삭제확인' ##NO_TEXT.
  class-data AC_MSG_TITLE4 type STRING value '복구확인' ##NO_TEXT.
  class-data AC_MSG_TITLE5 type STRING value 'Lost Confirm' ##NO_TEXT.
  class-data AC_MSG_UPD1 type STRING value '삭제 후 Upload 하시겠습니까?' ##NO_TEXT.
  class-data AC_MSG_UPD2 type STRING value '(Yes : 삭제  No : Append)' ##NO_TEXT.
  data AV_REPID type REPID .
  data AC_DATEFORMAT1904 type ABAP_BOOL .
  class-data AC_BLK type CHAR1 value '@' ##NO_TEXT.
  class-data AC_PATH_EXCEL type CHAR50 value 'Excel (*.XLSX)' ##NO_TEXT.

  methods CRT_DYN_TABLE
    importing
      !IT_FCAT type LVC_T_FCAT
      !IT_TKEY type ABAP_KEYDESCR_TAB optional
      !IV_UNIQUE type CHAR1 optional
      !IV_TYPE type CHAR1 optional
      !IV_TABLE_KIND type ABAP_TABLEKIND default CL_ABAP_TABLEDESCR=>TABLEKIND_STD
      !IV_EASY type CHAR1 optional
      !IV_ALV_COMM type CHAR1 optional
    exporting
      !EO_DYNT type DATA
      !ET_COMP type ABAP_COMPONENT_TAB
      !ERF_REF type ref to CL_ABAP_TABLEDESCR
      !ET_FCAT type LVC_T_FCAT .
  methods EXT_EXCL_DOWNLOAD
    importing
      !IV_TYPE type CHAR1 optional
      !IV_FILENAME type STRING optional
      !IT_FIELDCAT type LVC_T_FCAT optional
      !IT_HEADER type MDG_MDF_TS_FIELD_REPTEXT optional
      value(IT_DATA) type TABLE optional .
  methods EXT_EXCL_DOWNBIN
    importing
      !IT_FIELDCAT type LVC_T_FCAT optional
      !IT_HEADER type MDG_MDF_TS_FIELD_REPTEXT optional
      value(IT_DATA) type TABLE
    exporting
      value(EV_LENGTH) type INT4
      value(ET_BIN_TAB) type SDOKCNTBINS .
  methods FILE_OPEN_DIALOG
    importing
      !IV_TITLE type ANY optional
      !IV_DEFAULT_FILENAME type ANY optional
      !IV_DEFAULT_EXTENSION type ANY optional
      !IV_FILE_FILTER type ANY optional
      !IV_INIT_DIRECTORY type ANY optional
      !IV_EXCEL_CHK type ANY optional
    exporting
      !EV_FULLPATH type STRING
      !EV_PATH type STRING
      !EV_FILENAME type STRING
      !EV_EXCEL type CHAR1 .
  methods GET_ATTATCH_INFO
    importing
      !IS_OBJECT type SIBFLPORB
    returning
      value(RV_ICON) type ICON_D .
  class-methods GET_GUI_STATU
    importing
      !IV_MODE type CHAR1
      !IV_SAVE_INACT type CHAR1 optional
      !IV_DISP_ONLY type CHAR1 optional
      !IT_STATUS type TY_GUI_STATUS optional
    returning
      value(RT_FCODE) type /ACCGO/DGR_TT_UCOMM .
  methods GET_STRUC_DESCR
    importing
      !IV_TYPE type CHAR1
      !IV_STR_NM type STRUCNAME optional
      !IS_STRUC type ANY optional
    returning
      value(RT_FIELD) type DDFIELDS .
  methods DELTA_REFRESH_GRID
    importing
      !IV_MODIFIED type C optional
    changing
      !CT_DELTA_TABLE type LVC_T_MOCE .
  methods SET_ICON
    importing
      !IV_DCFLG type CHAR01
    returning
      value(RV_ICON) type ICON_D .
  class-methods SET_TIMESTAMP
    importing
      !IV_LOCAL type CHAR01 optional
      !IV_CREATE type CHAR01 optional
    changing
      !CS_DATA type ANY .
  methods SIMPLE_ALV
    importing
      value(IT_HDTXT) type MDG_MDF_TS_FIELD_REPTEXT optional
      value(IT_ITAB) type STANDARD TABLE
      !IT_NOOUT type FIELDNAME_T optional
      !IT_CURR type FIELDNAME_T optional
      !IV_TITLE type LVC_TITLE optional
      !IV_TOOLBAR type BOOLEAN optional
      !IV_CURR type FIELDNAME optional
      !IV_START_COLUMN type INT4 optional
      !IV_END_COLUMN type INT4 optional
      !IV_START_LINE type INT4 optional
      !IV_END_LINE type INT4 optional .
  methods COPY_CRT_ROW
    importing
      !IV_ROW type I
    changing
      !CS_DATA type ANY .
  methods BTN_ADD_ROW
    importing
      !IV_MULTI type CHAR1 optional
      !IV_INDEX type I optional
      !IT_DFTVL type TT_FIELD optional .
  methods BTN_DEL_ROW .
  methods BTN_EXCL_DOWNLOAD
    importing
      !IV_FILENAME type STRING optional
      !IS_LAYOUT type LVC_S_LAYO optional
      !IT_SORT type LVC_T_SORT optional
      !IT_FIELDCAT type LVC_T_FCAT optional
      !IT_HEADER type MDG_MDF_TS_FIELD_REPTEXT optional
      value(IT_DATA) type TABLE
      value(IV_DIALOG) type CHAR1 default 'X' .
  methods BTN_FILE_ATTATCH
    importing
      !IV_ROW type I optional
      !IV_DISPLAY type CHAR01 optional .
  methods BTN_HISTORY
    importing
      !IT_FCAT type LVC_T_FCAT
      !IV_TABNM type TABNAME .
  methods BTN_INFO .
  methods BTN_REC_ROW .
  methods CHECK_EXCEL_FILE
    importing
      !IV_FILENAME type ANY
    returning
      value(RV_SUBRC) type SY-SUBRC .
  methods CHK_DUP_DATA
    importing
      !IV_MSG_DEL type CHAR1 optional
      !IT_MANDT_FIELD type TT_FIELD optional
      !IT_KEY type ABAP_KEYDESCR_TAB optional
    returning
      value(RV_RETURN) type CHAR1 .
  methods CONSTRUCTOR
    importing
      !IRF_PARENT type ref to CL_GUI_CONTAINER default CL_GUI_CONTAINER=>DEFAULT_SCREEN
      !IV_NAME type STRING optional
      !IV_VARIANT type DISVARIANT-VARIANT optional
      !IT_DFTVL type TT_FIELD optional
      !IS_TOOLBTN type ZSCN00004 optional
      !IRF_HEAD type ref to CL_GUI_CONTAINER optional
      !IV_SCR_WR type CHAR30 optional
      !IT_HEADER type TT_HEADER optional
      !IV_AUTO_HEAD type CHAR1 optional
      !IV_LOCK_NM type CHAR100 optional
      !IV_TCODE type SY-TCODE optional
      !IT_INFO type TT_FIELD optional
      !IV_DICHG type CHAR1 optional
      !IV_CHANGED_SKIP type CHAR1 optional
      !IV_CELLS type CHAR1 default 'X'
      !IV_CELLC type CHAR1 default 'X'
      !IV_NO_GRID type CHAR1 optional .
  methods CRT_HIST_FIELD
    importing
      !IS_CELL type LVC_S_MODI .
  methods DISPLAY_GRID
    importing
      !IS_VARIANT type DISVARIANT
    changing
      !CT_DATA type TABLE .
  class-methods GET_DYNP_PARAM
    importing
      !IV_FNAME type FIELDNAME optional
      !IV_DYNNR type SY-DYNNR optional
    returning
      value(RT_PARAMS) type RSPARAMS_TT .
  class-methods GET_SEARCH_HELP
    importing
      !IV_FNAME type FIELDNAME
    exporting
      !EV_VALUE type ANY .
  methods EVT_AFTER_USER_COMMAND
    for event AFTER_USER_COMMAND of CL_GUI_ALV_GRID
    importing
      !E_UCOMM
      !E_SAVED
      !E_NOT_PROCESSED
      !SENDER .
  methods EVT_BEFORE_USER_COMMAND
    for event BEFORE_USER_COMMAND of CL_GUI_ALV_GRID
    importing
      !E_UCOMM
      !SENDER .
  methods EVT_BUTTON_CLIK
    for event BUTTON_CLICK of CL_GUI_ALV_GRID
    importing
      !ES_COL_ID
      !ES_ROW_NO
      !SENDER .
  methods EVT_DATA_CHANGED
    for event DATA_CHANGED of CL_GUI_ALV_GRID
    importing
      !ER_DATA_CHANGED
      !E_ONF4
      !E_ONF4_BEFORE
      !E_ONF4_AFTER
      !E_UCOMM
      !SENDER .
  methods EVT_DATA_CHANGED_FINISHED
    for event DATA_CHANGED_FINISHED of CL_GUI_ALV_GRID
    importing
      !E_MODIFIED
      !ET_GOOD_CELLS
      !SENDER .
  methods EVT_DOUBLE_CLICK
    for event DOUBLE_CLICK of CL_GUI_ALV_GRID
    importing
      !E_ROW
      !E_COLUMN
      !ES_ROW_NO
      !SENDER .
  methods EVT_HOTSPOT_CLICK
    for event HOTSPOT_CLICK of CL_GUI_ALV_GRID
    importing
      !E_ROW_ID
      !E_COLUMN_ID
      !ES_ROW_NO
      !SENDER .
  methods EVT_MENU_BUTTON
    for event MENU_BUTTON of CL_GUI_ALV_GRID
    importing
      !E_OBJECT
      !E_UCOMM
      !SENDER .
  methods EVT_ONDRAG
    for event ONDRAG of CL_GUI_ALV_GRID
    importing
      !E_ROW
      !E_COLUMN
      !ES_ROW_NO
      !E_DRAGDROPOBJ
      !SENDER .
  methods EVT_ONDROP
    for event ONDROP of CL_GUI_ALV_GRID
    importing
      !E_ROW
      !E_COLUMN
      !ES_ROW_NO
      !E_DRAGDROPOBJ
      !SENDER .
  methods EVT_ONDROPCOMPLETE
    for event ONDROPCOMPLETE of CL_GUI_ALV_GRID
    importing
      !E_ROW
      !E_COLUMN
      !ES_ROW_NO
      !E_DRAGDROPOBJ
      !SENDER .
  methods EVT_ONF4
    for event ONF4 of CL_GUI_ALV_GRID
    importing
      !E_FIELDNAME
      !E_FIELDVALUE
      !ES_ROW_NO
      !ER_EVENT_DATA
      !ET_BAD_CELLS
      !E_DISPLAY
      !SENDER .
  methods EVT_ON_DATA_CHANGED
    importing
      !IRF_DATA_CHANGED type ref to CL_ALV_CHANGED_DATA_PROTOCOL
      !IV_ONF4 type CHAR01
      !IV_ONF4_BEFORE type CHAR01
      !IV_ONF4_AFTER type CHAR01
      !IV_UCOMM type SY-UCOMM
      !IV_NAME type STRING .
  methods EVT_ON_F4
    importing
      !IV_FIELDNAME type FIELDNAME
      !IV_FIELDVALUE type LVC_VALUE optional
      !IS_ROW_NO type LVC_S_ROID optional
    exporting
      !EO_F4_LIST type ref to DATA
      !EV_MONTH_DISPLAY type CHAR1
      !EV_VALUE type DYNFNAM
    changing
      !CV_TITLE type CHAR50 default 'F4 Code List' .
  methods EVT_ON_TOOLBAR
    importing
      !IV_NAME type STRING
    changing
      !CT_ADD_TOOLBAR type TTB_BUTTON
      !CRF_OBJECT type ref to CL_ALV_EVENT_TOOLBAR_SET .
  methods EVT_TOOLBAR
    for event TOOLBAR of CL_GUI_ALV_GRID
    importing
      !E_OBJECT
      !E_INTERACTIVE
      !SENDER .
  methods EVT_TOP_OF_PAGE
    for event TOP_OF_PAGE of CL_GUI_ALV_GRID
    importing
      !E_DYNDOC_ID
      !TABLE_INDEX
      !SENDER .
  methods EVT_USER_COMMAND
    for event USER_COMMAND of CL_GUI_ALV_GRID
    importing
      !E_UCOMM
      !SENDER .
  methods FREE_GRID .
  methods GET_EDIT_MODE
    returning
      value(RV_EDIT_MODE) type CHAR1 .
  methods GET_FACTOR
    importing
      !IV_WAERS type WAERS optional
    returning
      value(RT_TCURR) type TT_TCURR .
  methods GET_FCAT
    importing
      value(IT_TABLE) type TABLE
    returning
      value(RT_FCAT) type LVC_T_FCAT .
  methods GET_FCAT_I
    importing
      !IS_STRU type ANY optional
      !IV_TYPNM type ANY optional
      !IV_CPROG type SY-CPROG default SY-CPROG
    returning
      value(RT_FCAT) type LVC_T_FCAT .
  methods GET_FIELD_FCAT
    returning
      value(RT_FCAT) type LVC_T_FCAT .
  methods GET_FILTER
    returning
      value(RT_FILTER) type LVC_T_FILT .
  methods GET_STATUS
    returning
      value(RV_STATUS) type SAP_BOOL .
  methods LOCK_COMMON
    importing
      !IV_TCODE type SY-TCODE optional
      !IV_JOBTYPE type CHAR100
      !IV_QUEUE type CHAR01 default 'E'
    returning
      value(RV_SUBRC) type SY-SUBRC .
  methods POP_TO_MSG
    importing
      !IV_TYPE type CHAR1 optional
      value(IV_TITLE) type STRING
      value(IV_TEXT1) type STRING
      value(IV_TEXT2) type STRING
      !IV_TEXT_BUTTON_1 type STRING default 'Yes'
      !IV_TEXT_BUTTON_2 type STRING default 'No'
      !IV_CANCEL_BUTTON type CHAR1 optional
    returning
      value(RV_ANSWER) type SAP_BOOL .
  methods REFRESH_GRID_DISPLAY
    importing
      !IV_SOFT_REFRESH type CHAR01 optional
      !IV_DELTA type CHAR1 optional
      !IV_DELTA_MODIFIED type CHAR1 optional
      value(IT_MOCE) type LVC_T_MOCE optional .
  methods RESET_STATUS
    returning
      value(RV_MODE) type CHAR1 .
  methods SET_INACTIVE_STATUS .
  methods SET_ACTIVE_STATUS .
  methods SET_CHANGE_MODE
    importing
      !IV_DICHG type CHAR1 optional
    changing
      !CV_LOCK type CHAR1 optional
      !CV_MODE type CHAR1 .
  methods SET_DATA_CHAGED_VALUE
    importing
      !IRF_DATA_CHANGED type ref to CL_ALV_CHANGED_DATA_PROTOCOL .
  methods SET_EDIT_MODE
    importing
      !IV_EDIT_MODE type CHAR1 optional .
  methods SET_GRID
    importing
      !IV_VARI type SLIS_VARI optional
      !IT_FCAT type LVC_T_FCAT optional
      !IS_HIST_TYPE type TS_HIST_TYPE optional
    changing
      !CT_DATA type TABLE optional
      !CV_MODE type CHAR1 optional .
  methods SET_LAYOUT
    changing
      !CS_LAYOUT type LVC_S_LAYO .
  methods SET_LAYOUT_TITLE
    importing
      !IV_TITLE type LVC_TITLE .
  methods SET_MSGTB
    importing
      !IV_DELETE type CHAR01 optional
      !IS_MSGTB type ZSCN00001
    changing
      !CS_DATA type ANY .
  methods SET_STATUS
    importing
      !IV_TYPE type CHAR1 optional .
  methods SET_TOOLBAR_BY_MODE
    importing
      !IV_EDIT_MODE type CHAR1
      value(IS_TOOLBTN) type ZSCN00004 optional
    changing
      value(CRF_OBJECT) type ref to CL_ALV_EVENT_TOOLBAR_SET .
  methods SHOW_MSGTB
    importing
      !IT_MSGTB type ZYCN00001 .
  methods XML_UPLOAD
    importing
      !IV_FILENAME type STRING
      !IV_BEG_ROW type I optional
      !IV_BEG_COL type I optional
      !IV_END_ROW type I optional
      !IV_END_COL type I optional
      !IV_SHEET_INDEX type I default 1
      !IT_FCAT type LVC_T_FCAT
    exporting
      !EV_SUBRC type SY-SUBRC
    changing
      !CT_DATA type TABLE
    exceptions
      DRM_ENCRYPTION
      CONVERSION_ERROR .
  methods CHECK_HANGUL
    importing
      !IV_VALUE type ANY
    returning
      value(RV_CHECK) type CHAR1 .
  methods DELETE_DATA_ROW
    importing
      !IT_ROWS type LVC_T_ROW .
  methods BTN_EXCL_UPLOAD
    importing
      !IV_FILENAME type STRING optional
      !IT_FCAT type LVC_T_FCAT optional
      !IT_DFTVL type TT_FIELD optional
      !IV_BEG_ROW type I default 2
      !IV_BEG_COL type I default 1
      !IV_END_ROW type I default 999999
      !IV_END_COL type I default 100
      !IV_SHEET_INDEX type I default 1
      !IV_NO_POP type CHAR1 optional
      !IV_DEL_DATA type CHAR1 optional
      !IV_CONV type CHAR1 optional
    changing
      !CT_DATA type TABLE
    returning
      value(RV_SUBRC) type SY-SUBRC
    exceptions
      DRM_ENCRYPTION
      CONVERSION_ERROR .
protected section.

  data AO_DATAREF type ref to DATA .
  data AO_OLDREF type ref to DATA .
  data AO_F4 type ref to DATA .
  data AS_FCAT type LVC_S_FCAT .
  data AS_FILTER type LVC_S_FILT .
  data AS_MSGTB type ZSCN00001 .
  data AS_SORT type LVC_S_SORT .
  data AS_VARIANT type DISVARIANT .
  data AT_DROPDOWN type LVC_T_DRAL .
  data AT_EX_TOOLBAR type UI_FUNCTIONS .
  data AT_FCAT_O type LVC_T_FCAT .
  data AT_FCAT type LVC_T_FCAT .
  data AT_FILTER type LVC_T_FILT .
  data AT_SORT type LVC_T_SORT .
  data AV_VARIANT type DISVARIANT-VARIANT .
  data AV_ERROR type CHAR1 .

  methods ON_SET_DEL_ROW_VALUE
    importing
      !IV_NAME type STRING
    changing
      !CS_DATA type ANY .
  methods ON_CHK_DEL_ROW
    importing
      !IV_NAME type STRING
    changing
      !CS_DATA type ANY
    returning
      value(RV_CHECK) type CHAR1 .
  methods ON_SET_ADD_ROW_VALUE
    importing
      !IV_NAME type STRING
    changing
      !CS_DATA type ANY .
  methods ON_SET_MSGTB
    importing
      !IV_NAME type STRING
      !IV_FIELDNAME type FIELDNAME optional
    changing
      !CS_DATA type ANY .
  methods ON_SET_LINE_STYLE_BY_ROW
    importing
      !IV_NAME type STRING
      !IS_DATA type ANY
      !IS_FCAT type LVC_S_FCAT
    changing
      !CS_CELLS type LVC_S_STYL .
  methods ON_CHK_DATA
    importing
      !IV_NAME type STRING
    changing
      !CS_DATA type ANY
      !CV_ERR type CHAR1 .
  methods ON_CHK_UPLOAD
    importing
      !IV_NAME type STRING
    changing
      !CP_DATA type ref to DATA .
  methods ON_CHK_UPLOAD_DATA
    importing
      !IV_NAME type STRING
    changing
      !CT_DATA type TABLE .
  methods ON_CHK_UPL_XML_COL
    importing
      !IV_NAME type ANY
      !IV_FIELDNAME type FIELDNAME optional
    changing
      !CV_COL_VAL type ANY
      !CS_DATA type ANY .
  methods ON_EVT_TOOLBAR_MODE
    importing
      !IV_NAME type STRING
    changing
      !CV_EDIT_MODE type CHAR1 .
  methods ON_EXCLUD_STD_TOOLBAR
    importing
      !IV_NAME type STRING
    changing
      !CT_EX_TOOLBAR type UI_FUNCTIONS .
  methods ON_INFO_TEXT
    importing
      !IV_NAME type STRING
      !IV_FNAME type FIELDNAME
      !IV_STATU type ICON_D
    changing
      !CV_TEXT type ANY .
  methods ON_SAVE_VARIANT
    importing
      !IV_NAME type STRING
    changing
      !CS_VARIANT type DISVARIANT
      !CV_DEFAULT type CHAR1
      !CV_SAVE type CHAR1 .
  methods ON_SET_ATTATCH_INFO
    importing
      !IV_NAME type STRING
      !IS_DATA type ANY optional
    changing
      !CS_OBJECT type SIBFLPORB
      !CV_ATTCH_FNAME type FIELDNAME .
  methods ON_SET_DNL_DATA
    importing
      !IV_NAME type STRING
    changing
      !CT_DATA type TABLE .
  methods ON_SET_DNL_FCAT
    importing
      !IV_NAME type STRING
    changing
      !CT_FCAT type LVC_T_FCAT .
  methods ON_SET_DROP_DOWN
    importing
      !IV_NAME type STRING
    changing
      !CT_DROP type LVC_T_DRAL .
  methods ON_SET_EVENT
    importing
      !IV_NAME type STRING .
  methods ON_SET_F4
    importing
      !IV_NAME type STRING
    changing
      value(CT_FIELD) type TT_FIELD .
  methods ON_SET_FCAT
    importing
      !IV_NAME type STRING
    changing
      !CT_FCAT type LVC_T_FCAT .
  methods ON_SET_FILTER
    importing
      !IV_NAME type STRING
    changing
      !CT_FILTER type LVC_T_FILT .
  methods ON_SET_LAYOUT
    importing
      !IV_NAME type STRING
    changing
      !CS_LAYOUT type LVC_S_LAYO .
  methods ON_SET_LINE_STYLE
    importing
      !IV_NAME type STRING
      !IT_FCAT type LVC_T_FCAT optional .
  methods ON_SET_SORT
    importing
      !IV_NAME type STRING
    changing
      value(CT_SORT) type LVC_T_SORT .
  methods ON_SET_UPL_FCAT
    importing
      !IV_NAME type STRING
    changing
      !CT_FCAT type LVC_T_FCAT .
**************************************************************************
* Private section of class. *
**************************************************************************
private section.

  data AV_TCODE type SY-TCODE .
  data ARF_DOCU type ref to CL_DD_DOCUMENT .
  data ARF_HEAD type ref to CL_GUI_CONTAINER .
  data AR_DFTVL type RSELOPTION .
  data AR_MASK type RSELOPTION .
  data AS_LAYOUT type LVC_S_LAYO .
  data AS_TOOLBTN type ZSCN00004 .
  data AT_DFTVL type TT_FIELD .
  data AT_DFTVL_ADD type TT_FIELD .
  data AT_HEADER type TT_HEADER .
  data AV_EDIT_MODE type CHAR1 .
  data AV_INPUT type INT4 value 0 ##NO_TEXT.
  data AV_LOCK_NM type CHAR100 .
  data AV_SCR_WR type CHAR30 .
  data AV_STATUS type SAP_BOOL .
  data AV_HST_TABNM type TABNAME .
  data AT_HISTORY type TY_HISTORY .
  data AT_KEY_LIST type TT_FIELD .
  data AT_INFO_TEXT type TY_INFO_CNT .
  data AT_INFO type TT_FIELD .
  data AV_DICHG type CHAR1 .
  data AR_KEY type RSELOPTION .
  data AV_DC_SKIP type CHAR1 .
  data AV_CELLS type CHAR1 .
  data AV_CELLC type CHAR1 .
  data AV_LOCK type CHAR1 .

  events SAVE_HIST .

  methods SET_FCAT_COMM
    changing
      !CT_FCAT type LVC_T_FCAT .
  class-methods MESSAGE_HANDLING
    importing
      !IV_MSG_TEXT type STRING
      !IV_MESSAGE_TYPE type CHAR01 optional
      !IV_ERR_POPUP type CHAR01 optional
    returning
      value(RS_MSG_RETURN) type BAPIRET2 .
  methods ALV_EXCLUD_STD_TOOLBAR .
  methods DEL_EXCL_SMW0_DOWNLOAD
    importing
      !IV_OBJID type ANY
      !IV_FILENAME type ANY optional .
  methods CELL_TO_INDEX
    importing
      !IV_CELL type STRING
    returning
      value(RV_INDEX) type I .
  methods CHECK_DRM
    importing
      !IV_FILENAME type STRING
    exporting
      !EV_SUBRC type SY-SUBRC
    returning
      value(RV_INVALID) type BOOLEAN
    exceptions
      DRM_ENCRYPTION .
  methods CONV_VALUE
    importing
      !IS_FCAT type LVC_S_FCAT
      !IV_TYPE type ESEBOOLE optional
      !IV_VALUE type ANY
    exporting
      !EV_SUBRC type SY-SUBRC
    changing
      !CV_VAL type ANY .
  methods CONVERT_CELL_TO_DATE_TIME
    importing
      !IV_INPUT type STRING
    returning
      value(RV_OUTPUT) type STRING .
  methods CONVERT_DEC_TIME_TO_HHMMSS
    importing
      !IV_DEC_TIME_STRING type STRING
    returning
      value(RV_TIME) type T .
  methods CONVERT_LONG_TO_DATE
    importing
      !IV_DATE_STRING type STRING
    returning
      value(RV_DATE) type D .
  methods CONV_NUMC
    importing
      !IV_VALUE type ANY
    returning
      value(RV_VALUE) type STRING .
  methods CORRECT_FORMAT_CONVERSION
    importing
      value(IV_SOURCE) type TEXT4096
      value(IS_DESC) type DFIES
    exporting
      !EV_TARGET type ANY
    returning
      value(RS_RETURN) type BAPIRET2 .
  methods DISPLAY_HEADER
    importing
      !IT_HEADER type TT_HEADER
      !IV_SCR_WR type CHAR30 optional .
  methods EVT_ON_TOP_OF_PAGE
    changing
      !CV_SCR_WR type CHAR30
      !CT_HEADER type TT_HEADER .
  methods EVT_SAVE_HIST
    for event SAVE_HIST of ZCL_CN_ALV_GRID .
  methods GET_EXE_CELLS_VALUE
    importing
      !IV_FILENAME type STRING
      !IV_SHEET_INDEX type I default 1
    exporting
      !ET_CELLS type TY_I_XML_ITAB
      !EV_SUBRC type SY-SUBRC
    exceptions
      DRM_ENCRYPTION
      INVALID_CELL_TYPE .
  methods INIT_INFO_TEXT .
  methods APPEND_F4_FIELD .
  methods SET_ERR_MSG
    importing
      !IV_FIELDNAME type FIELDNAME
      !IV_VALUE type ANY
      !IV_MSG type ANY
      !IV_MSG2 type ANY optional
      !IV_MSG3 type ANY optional
    changing
      !CS_DATA type ANY .
  methods SET_HIST_DATAKEY
    importing
      value(IS_DATA) type ANY
    exporting
      !EV_KEY type CHAR255
      !EV_KEYNM type CHAR255 .
  methods SET_LINE_STYLE
    importing
      !IV_NAME type STRING .
  methods CONV_UPLOAD_DATA
    importing
      !IT_FCAT type LVC_T_FCAT
      !IT_DFTVL type TT_FIELD optional
      value(IT_UPD) type TABLE
    changing
      !CT_DATA type TABLE .
ENDCLASS.



CLASS ZCL_CN_ALV_GRID IMPLEMENTATION.


  METHOD on_set_fcat.
*&---------------------------------------------------------------------*
*& Module      : CN
*& 생성자      : T0200335
*& 생성일      :  2020.12.30
*& Description :
*&---------------------------------------------------------------------*
*& Change History
*&---------------------------------------------------------------------*
*& 변경SEQ    변경자        변경일자             변경내용
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*

*-----------------------------------------------------
* 개별 프로그램에서  PROTECTED SECTION 에서 Redfefine 사용
* 그리드가 여러개 일 경우 GRID 명으로 구분 (IV_NAME)
* Alv Consturctor시 Display 되어지는 Table을 기준으로 Field Catalog가 생성되어지므로
* 각 필드별 속성을 변경 시키는 Method
* 참조 프로그램:YALV/YALV2
*-----------------------------------------------------
*=======================================================
*  PROTECTED SECTION.
*    METHODS:
**---------------------------
** Event Area
**---------------------------
*  ON_SET_FCAT      REDEFINITION.

*&---------------------------------------------------------------------*
*& Class Implementation
*&---------------------------------------------------------------------*
*CLASS lcl_cust_alv_grid IMPLEMENTATION.

*  METHOD ON_SET_F4.
*    CASE iv_name.
*      WHEN 'ALV_GRID'.
**--------------------------
** Modify Field Catalog
** 저장전 중복체크를 위한 Table key 세팅을 만드시!!
**--------------------------
*        PERFORM alv_grid_fcat_modify CHANGING ct_fcat.
*      WHEN 'GRID_DTL'.
*        PERFORM alv_dtl_fcat_modify CHANGING ct_fcat.
*    ENDCASE.
*  ENDMETHOD.


  ENDMETHOD.


  METHOD on_set_f4.
*&---------------------------------------------------------------------*
*& Module      : CN
*& 생성자      : T0200335
*& 생성일      :  2020.12.30
*& Description :
*&---------------------------------------------------------------------*
*& Change History
*&---------------------------------------------------------------------*
*& 변경SEQ    변경자        변경일자             변경내용
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*

*-----------------------------------------------------
* 개별 프로그램에서  PROTECTED SECTION 에서 Redfefine 사용
* 그리드가 여러개 일 경우 GRID 명으로 구분 (M_NAME)
* 사용하고자 하는 F4 Field를 등록
* 참조 프로그램:YALV/YALV2
*-----------------------------------------------------
*=======================================================
*  PROTECTED SECTION.
*    METHODS:
**---------------------------
** Event Area
**---------------------------
*  ON_SET_F4      REDEFINITION.

*&---------------------------------------------------------------------*
*& Class Implementation
*&---------------------------------------------------------------------*
*CLASS lcl_cust_alv_grid IMPLEMENTATION.

*  METHOD ON_SET_F4.
*
*  ENDMETHOD.

  ENDMETHOD.


  METHOD on_set_event.
*&---------------------------------------------------------------------*
*& Module      : CN
*& 생성자      : T0200335
*& 생성일      :  2020.12.30
*& Description :
*&---------------------------------------------------------------------*
*& Change History
*&---------------------------------------------------------------------*
*& 변경SEQ    변경자        변경일자             변경내용
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*

*-----------------------------------------------------
* 개별 프로그램에서  PROTECTED SECTION 에서 Redfefine 사용
* 그리드가 여러개 일 경우 GRID 명으로 구분 (M_NAME)
* ALV Event를 발생 시키는 Area
* 참조 프로그램:YALV,YALV2
*-----------------------------------------------------
*=======================================================
*  PROTECTED SECTION.
*    METHODS:
**---------------------------
** Event Area
**---------------------------
*  ON_SET_EVENT       REDEFINITION.

*&---------------------------------------------------------------------*
*& Class Implementation
*&---------------------------------------------------------------------*
*CLASS lcl_cust_alv_grid IMPLEMENTATION.

*  METHOD ON_SET_EVENT.
*        SET HANDLER :
*                      me->evt_toolbar                 FOR me,
*                      me->evt_data_changed            FOR me,
*                      me->evt_top_of_page             FOR me,
*                      me->evt_double_click            FOR me,
*                      me->evt_user_command            FOR me,
*                      me->evt_data_changed_finished   FOR me,
*                      me->evt_onf4                    FOR me.
*
*  ENDMETHOD.



  ENDMETHOD.


  METHOD on_set_drop_down.
*&---------------------------------------------------------------------*
*& Module      : CN
*& 생성자      : T0200335
*& 생성일      :  2020.12.30
*& Description :
*&---------------------------------------------------------------------*
*& Change History
*&---------------------------------------------------------------------*
*& 변경SEQ    변경자        변경일자             변경내용
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*

*-----------------------------------------------------
* 개별 프로그램에서  PROTECTED SECTION 에서 Redfefine 사용
* 그리드가 여러개 일 경우 GRID 명으로 구분 (M_NAME)
* 필드별 Dropdown List를 설정할 경우
* Field Catalog 영역에서도 반드시 2개필드를 각 상황에 맞게 설정
*  DRDN_HNDL'   '1',   "필드에 따라 변함
* 'DRDN_ALIAS'  'X',   "필수
* 참조 프로그램:YALV2
*-----------------------------------------------------
*=======================================================
*  PROTECTED SECTION.
*    METHODS:
**---------------------------
** Event Area
**---------------------------
*  ON_SET_DROP_DOWN    REDEFINITION.

*&---------------------------------------------------------------------*
*& Class Implementation
*&---------------------------------------------------------------------*
*CLASS lcl_cust_alv_grid IMPLEMENTATION.

*  METHOD ON_SET_DROP_DOWN.
*
*  ENDMETHOD.


  ENDMETHOD.


  METHOD ON_SET_DNL_FCAT.
*&---------------------------------------------------------------------*
*& Module      : CN
*& 생성자      : T0200335
*& 생성일      :  2020.12.30
*& Description :
*&---------------------------------------------------------------------*
*& Change History
*&---------------------------------------------------------------------*
*& 변경SEQ    변경자        변경일자             변경내용
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*

*-----------------------------------------------------
* 개별 프로그램에서  PROTECTED SECTION 에서 Redfefine 사용
* 그리드가 여러개 일 경우 GRID 명으로 구분 (IV_NAME)
* Alv Consturctor시 Display 되어지는 Table을 기준으로 Field Catalog가 생성되어지므로
* Upload 시 각 필드별 속성을 변경 시키는 Method
*-----------------------------------------------------
*=======================================================
*  PROTECTED SECTION.
*    METHODS:
**---------------------------
** Event Area
**---------------------------
*  ON_SET_UPL_FCAT      REDEFINITION.

*&---------------------------------------------------------------------*
*& Class Implementation
*&---------------------------------------------------------------------*
*CLASS lcl_cust_alv_grid IMPLEMENTATION.

*  METHOD ON_SET_F4.
*    CASE iv_name.
*      WHEN 'ALV_GRID'.
**--------------------------
** Modify Field Catalog
** 저장전 중복체크를 위한 Table key 세팅을 만드시!!
**--------------------------
*        PERFORM alv_grid_fcat_modify CHANGING ct_fcat.
*      WHEN 'GRID_DTL'.
*        PERFORM alv_dtl_fcat_modify CHANGING ct_fcat.
*    ENDCASE.
*  ENDMETHOD.


  ENDMETHOD.


  METHOD ON_SET_DNL_DATA.
*&---------------------------------------------------------------------*
*& Module      : CN
*& 생성자      : T0200335
*& 생성일      :  2020.12.30
*& Description :
*&---------------------------------------------------------------------*
*& Change History
*&---------------------------------------------------------------------*
*& 변경SEQ    변경자        변경일자             변경내용
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*

*-----------------------------------------------------
* 개별 프로그램에서  PROTECTED SECTION 에서 Redfefine 사용
* 그리드가 여러개 일 경우 GRID 명으로 구분 (IV_NAME)
* Alv Consturctor시 Display 되어지는 Table을 기준으로 Data 변경
*-----------------------------------------------------
*=======================================================
*  PROTECTED SECTION.
*    METHODS:
**---------------------------
** Event Area
**---------------------------
*  ON_SET_UPL_FCAT      REDEFINITION.

*&---------------------------------------------------------------------*
*& Class Implementation
*&---------------------------------------------------------------------*
*CLASS lcl_cust_alv_grid IMPLEMENTATION.

*  METHOD ON_SET_F4.
*    CASE iv_name.
*      WHEN 'ALV_GRID'.
**--------------------------
** Modify Field Catalog
** 저장전 중복체크를 위한 Table key 세팅을 만드시!!
**--------------------------
*        PERFORM alv_grid_fcat_modify CHANGING ct_fcat.
*      WHEN 'GRID_DTL'.
*        PERFORM alv_dtl_fcat_modify CHANGING ct_fcat.
*    ENDCASE.
*  ENDMETHOD.


  ENDMETHOD.


  METHOD ON_SET_DEL_ROW_VALUE.
*&---------------------------------------------------------------------*
*& Module      : CN
*& 생성자      : T0200335
*& 생성일      :  2020.12.30
*& Description :
*&---------------------------------------------------------------------*
*& Change History
*&---------------------------------------------------------------------*
*& 변경SEQ    변경자        변경일자             변경내용
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*

*-----------------------------------------------------
* 개별 프로그램에서  PROTECTED SECTION 에서 Redfefine 사용
* 그리드가 여러개 일 경우 GRID 명으로 구분 (M_NAME)
* 각 필드명 라인 Row Value
*-----------------------------------------------------
*=======================================================
*  PROTECTED SECTION.
*    METHODS:
**---------------------------
** Event Area
**---------------------------
*  ON_SET_ADD_ROW_VALUE REDEFINITION.

*&---------------------------------------------------------------------*
*& Class Implementation
*&---------------------------------------------------------------------*
*CLASS lcl_cust_alv_grid IMPLEMENTATION.

*  METHOD ON_SET_ADD_ROW_VALUE.
*
*  ENDMETHOD.

  ENDMETHOD.


  METHOD on_set_attatch_info.
  ENDMETHOD.


  METHOD ON_SET_ADD_ROW_VALUE.
*&---------------------------------------------------------------------*
*& Module      : CN
*& 생성자      : T0200335
*& 생성일      :  2020.12.30
*& Description :
*&---------------------------------------------------------------------*
*& Change History
*&---------------------------------------------------------------------*
*& 변경SEQ    변경자        변경일자             변경내용
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*

*-----------------------------------------------------
* 개별 프로그램에서  PROTECTED SECTION 에서 Redfefine 사용
* 그리드가 여러개 일 경우 GRID 명으로 구분 (M_NAME)
* 각 필드명 라인 Row Value
*-----------------------------------------------------
*=======================================================
*  PROTECTED SECTION.
*    METHODS:
**---------------------------
** Event Area
**---------------------------
*  ON_SET_ADD_ROW_VALUE REDEFINITION.

*&---------------------------------------------------------------------*
*& Class Implementation
*&---------------------------------------------------------------------*
*CLASS lcl_cust_alv_grid IMPLEMENTATION.

*  METHOD ON_SET_ADD_ROW_VALUE.
*
*  ENDMETHOD.

  ENDMETHOD.


  METHOD on_save_variant.
*&---------------------------------------------------------------------*
*& Module      : CN
*& 생성자      : T0200335
*& 생성일      :  2020.12.30
*& Description :
*&---------------------------------------------------------------------*
*& Change History
*&---------------------------------------------------------------------*
*& 변경SEQ    변경자        변경일자             변경내용
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*

*-----------------------------------------------------
* 개별 프로그램에서  PROTECTED SECTION 에서 Redfefine 사용
* 그리드가 여러개 일 경우 GRID 명으로 구분 (M_NAME)
* Variant를 Management 할 경우 개별에서 지정
* 참조 프로그램:YALV/YALV2
*-----------------------------------------------------
*=======================================================
*  PROTECTED SECTION.
*    METHODS:
**---------------------------
** Event Area
**---------------------------
*  ON_SAVE_VARIANT    REDEFINITION.

*&---------------------------------------------------------------------*
*& Class Implementation
*&---------------------------------------------------------------------*
*CLASS lcl_cust_alv_grid IMPLEMENTATION.

*  METHOD ON_SAVE_VARIANT.
*
*  ENDMETHOD.

  ENDMETHOD.


  METHOD on_info_text.
*&---------------------------------------------------------------------*
*& Module      : CN
*& 생성자      : T0200335
*& 생성일      :  2020.12.30
*& Description :
*&---------------------------------------------------------------------*
*& Change History
*&---------------------------------------------------------------------*
*& 변경SEQ    변경자        변경일자             변경내용
*&---------------------------------------------------------------------*


  ENDMETHOD.


  METHOD on_exclud_std_toolbar.
*&---------------------------------------------------------------------*
*& Module      : CN
*& 생성자      : T0200335
*& 생성일      :  2020.12.30
*& Description :
*&---------------------------------------------------------------------*
*& Change History
*&---------------------------------------------------------------------*
*& 변경SEQ    변경자        변경일자             변경내용
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*

*-----------------------------------------------------
* 개별 프로그램에서  PROTECTED SECTION 에서 Redfefine 사용
* 그리드가 여러개 일 경우 GRID 명으로 구분 (M_NAME)
* Construtor 시 생성되어진 Default Standard Toolbar 에서 제거 하고자 하는경우
* Ex)Sum,Total은 금액이나 수량이 없는 경우는 필요가 없으므로 이 영역에서 제거한다
* 참조 프로그램:YALV/YALV2
*-----------------------------------------------------
*=======================================================
*  PROTECTED SECTION.
*    METHODS:
**---------------------------
** Event Area
**---------------------------
*  ON_EXCLUD_STD_TOOLBAR     REDEFINITION.

*&---------------------------------------------------------------------*
*& Class Implementation
*&---------------------------------------------------------------------*
*CLASS lcl_cust_alv_grid IMPLEMENTATION.

*  METHOD on_exclud_std_toolbar.
*
*  ENDMETHOD.

  ENDMETHOD.


  METHOD ON_EVT_TOOLBAR_MODE.
*&---------------------------------------------------------------------*
*& Module      : CN
*& 생성자      : T0200335
*& 생성일      :  2020.12.30
*& Description :
*&---------------------------------------------------------------------*
*& Change History
*&---------------------------------------------------------------------*
*& 변경SEQ    변경자        변경일자             변경내용
*&---------------------------------------------------------------------*

  ENDMETHOD.


  METHOD on_chk_upl_xml_col.
*&---------------------------------------------------------------------*
*& Module      : CN
*& 생성자      : T0200335
*& 생성일      :  2021.03.05
*& Description :
*&---------------------------------------------------------------------*
*& Change History
*&---------------------------------------------------------------------*
*& 변경SEQ    변경자        변경일자             변경내용
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*

*-----------------------------------------------------
* 개별 프로그램에서  PROTECTED SECTION 에서 Redfefine 사용
* 그리드가 여러개 일 경우 GRID 명으로 구분 (IV_NAME)
* Upload Xml 변환시  Column 별 Value 를 체크
*-----------------------------------------------------
*=======================================================
*  PROTECTED SECTION.
*    METHODS:
**---------------------------
** Event Area
**---------------------------
*  ON_CHK_UPL_XML_COL     REDEFINITION.

*&---------------------------------------------------------------------*
*& Class Implementation
*&---------------------------------------------------------------------*
*CLASS lcl_cust_alv_grid IMPLEMENTATION.

*  METHOD ON_CHK_UPL_XML_COL.
*    CASE iv_name.
*      WHEN 'ALV_GRID'.
*        PERFORM chk_xml_col_data  USING    iv_fieldname
*                                  CHANGING cv_col_val cs_data.

*      WHEN 'GRID_DTL'.
*    ENDCASE.
*  ENDMETHOD.

  ENDMETHOD.


  METHOD ON_CHK_UPLOAD_DATA.
*&---------------------------------------------------------------------*
*& Module      : CN
*& 생성자      : T0200335
*& 생성일      :  2020.12.30
*& Description :
*&---------------------------------------------------------------------*
*& Change History
*&---------------------------------------------------------------------*
*& 변경SEQ    변경자        변경일자             변경내용
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*

  ENDMETHOD.


  METHOD on_chk_upload.
*&---------------------------------------------------------------------*
*& Module      : CN
*& 생성자      : T0200335
*& 생성일      :  2020.12.30
*& Description :
*&---------------------------------------------------------------------*
*& Change History
*&---------------------------------------------------------------------*
*& 변경SEQ    변경자        변경일자             변경내용
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*


  ENDMETHOD.


  METHOD ON_CHK_DEL_ROW.
*&---------------------------------------------------------------------*
*& Module      : CN
*& 생성자      : T0200335
*& 생성일      :  2020.12.30
*& Description :
*&---------------------------------------------------------------------*
*& Change History
*&---------------------------------------------------------------------*
*& 변경SEQ    변경자        변경일자             변경내용
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*

*-----------------------------------------------------
* 개별 프로그램에서  PROTECTED SECTION 에서 Redfefine 사용
* 그리드가 여러개 일 경우 GRID 명으로 구분 (M_NAME)
* 각 필드명 라인 Row Value
*-----------------------------------------------------
*=======================================================
*  PROTECTED SECTION.
*    METHODS:
**---------------------------
** Event Area
**---------------------------
*  ON_SET_ADD_ROW_VALUE REDEFINITION.

*&---------------------------------------------------------------------*
*& Class Implementation
*&---------------------------------------------------------------------*
*CLASS lcl_cust_alv_grid IMPLEMENTATION.

*  METHOD ON_SET_ADD_ROW_VALUE.
*
*  ENDMETHOD.

  ENDMETHOD.


  METHOD on_chk_data.
*&---------------------------------------------------------------------*
*& Module      : CN
*& 생성자      : T0200335
*& 생성일      :  2020.12.30
*& Description :
*&---------------------------------------------------------------------*
*& Change History
*&---------------------------------------------------------------------*
*& 변경SEQ    변경자        변경일자             변경내용
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*

*----------------------------------------------------
* 개별 프로그램에서  PROTECTED SECTION 에서 Redfefine 사용
* 그리드가 여러개 일 경우 GRID 명으로 구분 (IV_NAME)
* Save전 라인 별 데이타 체크 (CHK_DUP_DATA) 메소드 안에서 실행됨
* 참조 프로그램:YALV/YALV2
*-----------------------------------------------------
*=======================================================
*  PROTECTED SECTION.
*    METHODS:
**---------------------------
** Event Area
**---------------------------
*  ON_CHK_DATA    REDEFINITION.

*&---------------------------------------------------------------------*
*& Class Implementation
*&---------------------------------------------------------------------*
*CLASS lcl_cust_alv_grid IMPLEMENTATION.

*  METHOD ON_CHK_DATA
*
*  ENDMETHOD.

  ENDMETHOD.


  METHOD MESSAGE_HANDLING.

    DATA : LT_LINE TYPE TABLE OF SYMSGV,
           LV_LINE TYPE SYMSGV.

    DATA : LS_MSG   TYPE BAPIRET2,
           LV_MSG   TYPE STRING,
           LV_FNAME TYPE FIELDNAME,
           LV_CNT   TYPE N VALUE 1.

    FIELD-SYMBOLS <LV_VALUE> TYPE ANY.

    IF IV_MSG_TEXT IS INITIAL.
      EXIT.
    ELSE.
      LV_MSG = IV_MSG_TEXT.
    ENDIF.

*--------------------------------------------------------------------*
    IF LV_MSG IS NOT INITIAL. "split longtext message
*--------------------------------------------------------------------*

      SPLIT LV_MSG AT SPACE INTO TABLE LT_LINE.
      CLEAR LV_MSG.

      LOOP AT LT_LINE INTO LV_LINE.
        IF LV_MSG IS INITIAL.
          CONCATENATE LV_MSG LV_LINE INTO LV_MSG.
        ELSE.
          CONCATENATE LV_MSG LV_LINE INTO LV_MSG SEPARATED BY SPACE.
        ENDIF.

*-- Assign msgvalue field

        CLEAR LV_FNAME. UNASSIGN <LV_VALUE>.
        CONCATENATE 'LS_MSG-MESSAGE_V' LV_CNT INTO LV_FNAME.
        ASSIGN (LV_FNAME) TO <LV_VALUE>.
        CHECK <LV_VALUE> IS ASSIGNED.

        CL_ABAP_LIST_UTILITIES=>DYNAMIC_OUTPUT_LENGTH( EXPORTING FIELD = LV_MSG
                                                       RECEIVING LEN   = DATA(LV_LEN) ).

*-- Check over length

        IF LV_LEN >= 50.
          LV_MSG = LV_LINE.
          ADD 1 TO LV_CNT.
          CLEAR LV_FNAME. UNASSIGN <LV_VALUE>.
          CONCATENATE 'LS_MSG-MESSAGE_V' LV_CNT INTO LV_FNAME.
          ASSIGN (LV_FNAME) TO <LV_VALUE>.
        ENDIF.

*-- Concatenate split message

        CHECK <LV_VALUE> IS ASSIGNED.
        IF <LV_VALUE> IS INITIAL.
          CONCATENATE <LV_VALUE> LV_LINE '' INTO <LV_VALUE>.
        ELSE.
          CONCATENATE <LV_VALUE> LV_LINE '' INTO <LV_VALUE> SEPARATED BY SPACE.
        ENDIF.

      ENDLOOP.

    ENDIF.

    IF LS_MSG IS NOT INITIAL.
      MOVE-CORRESPONDING LS_MSG TO RS_MSG_RETURN.
    ENDIF.

*--------------------------------------------------------------------*
    IF IV_MESSAGE_TYPE IS NOT INITIAL. "Display message option
*--------------------------------------------------------------------*

      IF IV_ERR_POPUP = ABAP_TRUE. "Popup warning error message
*      DATA(LV_ANS) = zcl_cn_alv_grid=>POPUP_TO_CONFIRM( IV_TITLE         = TEXT-P01
*                                                    IV_QUESTION      = TEXT-P02
*                                                    IV_TEXT_BUTTON_1 = TEXT-P03
*                                                    IV_TEXT_BUTTON_2 = TEXT-PO4 ).
      ELSE.
        DATA(LV_ANS) = 'Y'.
      ENDIF.

*    IF LV_ANS = ykh_Y. "Display message
*      IF IV_MESSAGE_TYPE IS NOT INITIAL.
*        ZMACO_MC_MSG
*        'ZCO_01' IV_MESSAGE_TYPE 0 RS_MSG_RETURN-MESSAGE_V1
*                                   RS_MSG_RETURN-MESSAGE_V2
*                                   RS_MSG_RETURN-MESSAGE_V3
*                                   RS_MSG_RETURN-MESSAGE_V4
*                                   IV_MESSAGE_DISP.
*      ELSE.
*        ZMACO_MC_MSG
*        'ZCO_01' IV_MESSAGE_TYPE 0 RS_MSG_RETURN-MESSAGE_V1
*                                   RS_MSG_RETURN-MESSAGE_V2
*                                   RS_MSG_RETURN-MESSAGE_V3
*                                   RS_MSG_RETURN-MESSAGE_V4.
*      ENDIF.
*    ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD lock_common.
*&---------------------------------------------------------------------*
*& Module              : CN
*& Program ID          : ZCL_CN_ALV_GRID
*& T-CODE              : N/A
*& Referenced Program  : N/A
*& Created by          : T0200335
*& Created On          : 2020.12.23
*& Type                : CLASS
*& Description         : ZCL_CN_ALV_GRID
*&---------------------------------------------------------------------*
*& Change History
*&---------------------------------------------------------------------*
*& 변경SEQ    변경자        변경일자             변경내용
*&---------------------------------------------------------------------*

    DATA : lv_object    TYPE zscn00005-object.
    DATA : lt_enq      TYPE TABLE OF seqg3,
           lv_msg(200),
           lv_check.

*-----------------------
* Set Lock Name..
*-----------------------
    "Set T-Code
    IF iv_tcode IS INITIAL.
      DATA(lv_tcode) = sy-cprog.
    ELSE.
      lv_tcode = iv_tcode.
    ENDIF.

    CONCATENATE lv_tcode iv_jobtype INTO lv_object.

    CASE iv_queue.
      WHEN 'D'.
*=========================================
* 락 해제
*=========================================
        CALL FUNCTION 'DEQUEUE_EZLCN_QUEUE'
          EXPORTING
            mode_zscn00005 = 'E'
            object         = lv_object
          EXCEPTIONS
            OTHERS         = 1.
        rv_subrc = sy-subrc.
        IF sy-subrc NE 0.
          MESSAGE s024(zcn00) WITH lv_object DISPLAY LIKE 'E'.  "락해제 실패
          RETURN.
        ENDIF.
      WHEN 'E'.
*=========================================
* 락 설정
*=========================================
        CALL FUNCTION 'ENQUEUE_READ'
          EXPORTING
            gname                 = 'ZSCN00005'
          TABLES
            enq                   = lt_enq[]
          EXCEPTIONS
            communication_failure = 1
            system_failure        = 2
            OTHERS                = 3.

        LOOP AT lt_enq INTO DATA(ls_enq).
          IF lv_object    CP ls_enq-garg OR
             ls_enq-garg  CP lv_object.
            lv_check = 'X'.
            EXIT.
          ENDIF.
        ENDLOOP.

        CHECK lv_check IS INITIAL.
*------------------
* Read Enque Lock
*------------------
        CALL FUNCTION 'ENQUEUE_EZLCN_QUEUE'
          EXPORTING
            mode_zscn00005 = 'E'
            object         = lv_object
          EXCEPTIONS
            foreign_lock   = 1
            system_failure = 2
            OTHERS         = 3.

        IF sy-subrc = 0.
          CLEAR:rv_subrc.
          EXIT.
        ENDIF.

*------------------
* Message 처리
*------------------
        CALL FUNCTION 'MESSAGE_TEXT_BUILD'
          EXPORTING
            msgid               = sy-msgid
            msgnr               = sy-msgno
            msgv1               = sy-msgv1
            msgv2               = sy-msgv2
            msgv3               = sy-msgv3
            msgv4               = sy-msgv4
          IMPORTING
            message_text_output = lv_msg.

        "Get user Name
        SELECT SINGLE b~name_text AS name
          FROM usr21 AS a INNER JOIN adrp AS b
                                  ON a~persnumber = b~persnumber
          WHERE a~bname      = @sy-msgv1
            AND b~date_from <= @sy-datum
          INTO @DATA(lv_name).

        IF sy-subrc = 0.
          lv_msg = lv_msg && '(' && lv_name && ')'.
        ENDIF.

        rv_subrc = 8.
        MESSAGE lv_msg TYPE 'S'  DISPLAY LIKE 'E'.

    ENDCASE.

  ENDMETHOD.


  METHOD init_info_text.
*&---------------------------------------------------------------------*
*& Module      : CN
*& 생성자      : T0200335
*& 생성일      :  2020.12.30
*& Description :
*&---------------------------------------------------------------------*
*& Change History
*&---------------------------------------------------------------------*
*& 변경SEQ    변경자        변경일자             변경내용
*&---------------------------------------------------------------------*

    DEFINE _l_info_text.
      ls_info-statu = &1.
      ls_info-ftext = &2.
      APPEND ls_info TO at_info_text.
    end-OF-DEFINITION.

    DATA:ls_info TYPE ts_info_cnt.


    _l_info_text:icon_create                TEXT-c03,
                 icon_change                TEXT-c04,
                 icon_led_green             TEXT-c05,
                 icon_delete                TEXT-c06,
                 icon_system_save           TEXT-c08,
                 icon_xls                   TEXT-c09,
                 icon_system_undo           TEXT-c10,
                 icon_message_error_small   TEXT-c11,
                 icon_message_warning_small TEXT-c12,
                 icon_delete_row            TEXT-c07.

  ENDMETHOD.


  METHOD get_struc_descr.
*&---------------------------------------------------------------------*
*& Module              : CN
*& Program ID          : ZCL_CN_ALV_GRID
*& T-CODE              : N/A
*& Referenced Program  : N/A
*& Created by          : T0200335
*& Created On          : 2020.12.23
*& Type                : CLASS
*& Description         : CRT_DYN_TABLE
*&---------------------------------------------------------------------*
*& Change History
*&---------------------------------------------------------------------*
*& 변경SEQ    변경자        변경일자             변경내용
*&---------------------------------------------------------------------*

    DATA : lrf_str_desc TYPE REF TO CL_ABAP_STRUCTDESCR.

    CASE iv_type.
      WHEN 'N'.
        CHECK iv_str_nm IS NOT INITIAL.
*----------------------------------
* Get Field  By Structure Name..
*----------------------------------

*-- Check Struct Name..
        SELECT SINGLE tabname
          INTO @DATA(lv_tabnm)
          FROM dd02l
         WHERE tabname = @iv_str_nm
           AND as4local = 'A'.

        IF sy-subrc <> 0.
          MESSAGE s001(zcn00) WITH iv_str_nm DISPLAY LIKE 'E'.
          EXIT.
        ENDIF.

        rt_field   = CAST cl_abap_structdescr( cl_abap_typedescr=>describe_by_name( iv_str_nm ) )->get_ddic_field_list( ).
      WHEN 'S'.
*----------------------------------
* Get Field  By Structur Type
*----------------------------------
        CHECK is_struc IS SUPPLIED.

        lrf_str_desc ?= cl_abap_structdescr=>describe_by_data( is_struc ).
        rt_field = cl_salv_data_descr=>read_structdescr( lrf_str_desc ).

*        rt_field   = CAST cl_abap_structdescr( cl_abap_typedescr=>describe_by_data( is_struc ) )->get_ddic_field_list( ).
    ENDCASE.


  ENDMETHOD.


  METHOD get_status.
*&---------------------------------------------------------------------*
*& Module              : CN
*& Program ID          : ZCL_CN_ALV_GRID
*& T-CODE              : N/A
*& Referenced Program  : N/A
*& Created by          : T0200335
*& Created On          : 2020.12.23
*& Type                : CLASS
*& Description         : ZCL_CN_ALV_GRID
*&---------------------------------------------------------------------*
*& Change History
*&---------------------------------------------------------------------*
*& 변경SEQ    변경자        변경일자             변경내용
*&---------------------------------------------------------------------*

    DATA : lv_valid   TYPE char01,
           lv_refresh TYPE char01 VALUE 'X'.

*--------------------------------
* Check Changed Grid..
*--------------------------------
    CALL METHOD me->check_changed_data
      IMPORTING
        e_valid   = lv_valid
      CHANGING
        c_refresh = lv_refresh.

*--------------------------------
* Return Status..
*--------------------------------
    rv_status = av_status.


  ENDMETHOD.


  METHOD get_search_help.
*&---------------------------------------------------------------------*
*& Module              : CN
*& Program ID          : N/A
*& T-CODE              : N/A
*& Referenced Program  : N/A
*& Created by          : T0200335
*& Created On          : 2021.12.01
*& Type                : Method
*& Description :
*&---------------------------------------------------------------------*
*& Change History
*&---------------------------------------------------------------------*
*& 변경SEQ    변경자        변경일자             변경내용
*&---------------------------------------------------------------------*
*&    N      T0200335      2021.12.01            최초생성
*&---------------------------------------------------------------------*


    DATA: lt_param TYPE rsparams_tt,
          ls_param TYPE rsparams.

    DATA: lv_refld TYPE dfies-fieldname,
          lv_dyfld TYPE help_info-dynprofld,
          lv_month TYPE isellist-month.


    lv_dyfld = iv_fname.
    lv_refld = iv_fname+2(5).

    DATA(lv_dynnr) = COND syst_dynnr(
                          WHEN sy-dynnr = '1000' OR sy-dynnr = '3010' OR
                               sy-dynnr = '3020' OR sy-dynnr = '3030' OR
                               sy-dynnr = '3040'
                          THEN '1000' ELSE sy-dynnr ).

    CASE lv_refld.
      WHEN 'SPMON'.
        lt_param = ZCL_CN_ALV_GRID=>get_dynp_param( iv_fname = lv_refld iv_dynnr = lv_dynnr ).
        READ TABLE lt_param INTO ls_param INDEX 1.

        CASE iv_fname+0(1).
          WHEN 'S'.

            CASE iv_fname.
              WHEN 'S_SPMON-LOW'.
                lv_month = ls_param-low.
              WHEN OTHERS.
                lv_month = ls_param-high.
            ENDCASE.

          WHEN OTHERS.
            lv_month = ls_param-low.
        ENDCASE.

        IF lv_month IS INITIAL.
          lv_month = sy-datum+0(6).

        ELSE.
          CALL FUNCTION 'CONVERSION_EXIT_BCVMN_INPUT'
            EXPORTING
              input  = lv_month
            IMPORTING
              output = lv_month.
        ENDIF.

        CALL FUNCTION 'POPUP_TO_SELECT_MONTH'
          EXPORTING
            actual_month               = lv_month
          IMPORTING
            selected_month             = lv_month
          EXCEPTIONS
            factory_calendar_not_found = 1
            holiday_calendar_not_found = 2
            month_not_found            = 3
            OTHERS                     = 4.

        CHECK lv_month IS NOT INITIAL.

        ev_value = lv_month.

    ENDCASE.


  ENDMETHOD.


  METHOD get_gui_statu.
*&---------------------------------------------------------------------*
*& Module              : CN
*& Program ID          : ZCL_CN_ALV_GRID
*& T-CODE              : N/A
*& Referenced Program  : N/A
*& Created by          : T0200335
*& Created On          : 2020.12.23
*& Type                : CLASS
*& Description         : CRT_DYN_TABLE
*&---------------------------------------------------------------------*
*& Change History
*&---------------------------------------------------------------------*
*& 변경SEQ    변경자        변경일자             변경내용
*&---------------------------------------------------------------------*
*-- Excludeing Fcode
    CASE iv_mode.
      WHEN ''.       "Display mode
        rt_fcode = VALUE #( ( 'DISP' )
                            ( 'SAVE' ) ).
      WHEN OTHERS.   "Edit Mode
        rt_fcode = VALUE #( ( 'EDIT' ) ).
    ENDCASE.

*-- 무조건 Save 버튼 없앨 경우
    IF iv_save_inact = 'X'.
      rt_fcode = VALUE #( ( 'SAVE' ) ).
    ENDIF.

*-- Only Display Mode (Change <-> Edit 변경 없음)
    IF iv_disp_only = 'X'.
      rt_fcode = VALUE #( ( 'SAVE' )
                          ( 'EDIT' )
                          ( 'DISP' ) ).
    ENDIF.

    CHECK it_status IS NOT INITIAL.
*&---------------------------------------------------------------------------*
* IT_STATUS 는 Edit 구분에 'X' 로 되어있으면 Fcode는 Edit일때문 보여주고싶은것
*                          '' 로 되어있으면 Display 일때 보여주고 싶은것
*&---------------------------------------------------------------------------*
    LOOP AT it_status INTO DATA(ls_status).
      IF ls_status-edit = 'A'.  "A인 경우는 모드 상관없이 무조건
        rt_fcode = VALUE #( BASE rt_fcode ( ls_status-fcode ) ).
        CONTINUE.
      ENDIF.

      CASE iv_mode.
        WHEN ''.       "Display mode
          CHECK ls_status-edit = 'X'.
          rt_fcode = VALUE #( BASE rt_fcode ( ls_status-fcode ) ).
        WHEN 'X'.   "Edit Mode
          CHECK ls_status-edit = ''.
          rt_fcode = VALUE #( BASE rt_fcode ( ls_status-fcode ) ).
      ENDCASE.
    ENDLOOP.


  ENDMETHOD.


  METHOD get_filter.
*&---------------------------------------------------------------------*
*& Module              : CN
*& Program ID          : ZCL_CN_ALV_GRID
*& T-CODE              : N/A
*& Referenced Program  : N/A
*& Created by          : T0200335
*& Created On          : 2020.12.23
*& Type                : CLASS
*& Description         : ZCL_CN_ALV_GRID
*&---------------------------------------------------------------------*
*& Change History
*&---------------------------------------------------------------------*
*& 변경SEQ    변경자        변경일자             변경내용
*&---------------------------------------------------------------------*
    at_filter = rt_filter.

  ENDMETHOD.


  METHOD get_field_fcat.
*&---------------------------------------------------------------------*
*& Module              : CN
*& Program ID          : ZCL_CN_ALV_GRID
*& T-CODE              : N/A
*& Referenced Program  : N/A
*& Created by          : T0200335
*& Created On          : 2020.12.23
*& Type                : CLASS
*& Description         : ZCL_CN_ALV_GRID
*&---------------------------------------------------------------------*
*& Change History
*&---------------------------------------------------------------------*
*& 변경SEQ    변경자        변경일자             변경내용
*&---------------------------------------------------------------------*

    rt_fcat = at_fcat.

  ENDMETHOD.


  METHOD get_fcat_i.
*&---------------------------------------------------------------------*
*& Module              : CN
*& Program ID          : ZCL_CN_ALV_GRID
*& T-CODE              : N/A
*& Referenced Program  : N/A
*& Created by          : T0200335
*& Created On          : 2020.12.23
*& Type                : CLASS
*& Description         : ZCL_CN_ALV_GRID
*&---------------------------------------------------------------------*
*& Change History
*&---------------------------------------------------------------------*
*& 변경SEQ    변경자        변경일자             변경내용
*&---------------------------------------------------------------------*

    DEFINE _l_set_fcat.
      ls_fcat-ref_table = &1.
     ls_fcat-ref_field = &2.
     ls_fcat-no_out    = &3.
     APPEND ls_fcat TO &4.

    END-OF-DEFINITION.

    TYPES : BEGIN OF lty_ref,
              ref_table TYPE lvc_tname,
              fieldname TYPE lvc_fname,
            END OF lty_ref.

    DATA : lrf_cast    TYPE REF TO cx_sy_move_cast_error,
           lrf_root    TYPE REF TO cx_root,
           lrf_stdesc  TYPE REF TO cl_abap_structdescr,
           lrf_tadesc  TYPE REF TO cl_abap_tabledescr,
           lo_dref     TYPE REF TO data,
           ls_fcat     TYPE lvc_s_fcat,
           ls_desc     TYPE x030l,
           ls_dfies    TYPE dfies,
           lrf_elem    TYPE REF TO  cl_abap_elemdescr,
           lrf_stdesct TYPE REF TO  cl_abap_structdescr,
           lt_ref      TYPE TABLE OF lty_ref WITH NON-UNIQUE SORTED KEY zfd COMPONENTS fieldname,
           ls_ref      TYPE          lty_ref.

    DATA:lv_stname   TYPE dd02l-tabname,
         lv_type(30).

    FIELD-SYMBOLS:<ls_stru> TYPE any.

    IF is_stru IS SUPPLIED.

      TRY.
          lrf_stdesc ?= cl_abap_structdescr=>describe_by_data( is_stru ).
        CATCH cx_sy_move_cast_error INTO lrf_cast.
          RETURN.
        CATCH cx_root INTO lrf_root.
          RETURN.
      ENDTRY.

    ELSE.

      DATA(lv_case) = 'PROGRAM'.
      lv_type = iv_typnm.
      DATA(lv_obj)  = iv_cprog.

      CONCATENATE '\' lv_case '=' lv_obj '\TYPE=' lv_type INTO DATA(lv_src).

      CREATE DATA lo_dref TYPE (lv_src).

      ASSIGN lo_dref->* TO <ls_stru>.

      lrf_stdesc ?= cl_abap_structdescr=>describe_by_data( <ls_stru> ).

    ENDIF.

    IF lrf_stdesc->is_ddic_type( ) EQ abap_true.

*----------------------------------
*  Exists Dictionary Type
*----------------------------------
      lv_stname = lrf_stdesc->get_relative_name( ).


      CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
        EXPORTING
          i_buffer_active        = space
          i_structure_name       = lv_stname
          i_bypassing_buffer     = abap_true
        CHANGING
          ct_fieldcat            = rt_fcat
        EXCEPTIONS
          inconsistent_interface = 1
          program_error          = 2
          OTHERS                 = 3.

    ELSE.

*----------------------------------
*  Not Exists Dictionary Type
*----------------------------------
      DATA(lt_comtab) = lrf_stdesc->get_components( ).

      DATA(lt_fields_t) = lrf_stdesc->get_included_view( ).

      DELETE lt_comtab FROM 1 WHERE as_include NE 'X'.

      LOOP AT lt_comtab INTO DATA(ls_comtab).
        lrf_stdesct ?= ls_comtab-type.
        ls_ref-ref_table = lrf_stdesct->get_relative_name( ).
        APPEND ls_ref TO lt_ref.
      ENDLOOP.

      SORT lt_ref BY ref_table.
      DELETE ADJACENT DUPLICATES FROM lt_ref COMPARING ref_table.

      IF lt_ref IS NOT INITIAL.
        SELECT tabname AS ref_table
               fieldname
        INTO CORRESPONDING FIELDS OF TABLE lt_ref
        FROM dd03l
        FOR ALL ENTRIES IN lt_ref
        WHERE tabname = lt_ref-ref_table.
        SORT lt_ref BY fieldname.
      ENDIF.

      LOOP AT lt_fields_t INTO DATA(ls_fields).
        CLEAR: ls_fcat,
               ls_desc.

        ls_fcat-col_pos   = sy-tabix.
        ls_fcat-fieldname = ls_fields-name.
        ls_fcat-inttype   = ls_fields-type->type_kind.
        ls_fcat-decimals  = ls_fields-type->decimals.

        IF ls_fields-type->is_ddic_type( ) IS NOT INITIAL.
          DATA(lv_rel_name) = ls_fields-type->get_relative_name( ).

          IF lv_rel_name IS NOT INITIAL.
            ls_desc          = ls_fields-type->get_ddic_header( ).
            ls_fcat-rollname = ls_desc-tabname.
            ls_fcat-datatype = 'RAW' .
            ls_fcat-inttype  = abap_true.
            CASE ls_fields-name.
              WHEN 'CELLC'.
                _l_set_fcat :'ZSCN00003' 'CELLC' 'X' rt_fcat.
              WHEN 'CELLS'.
                _l_set_fcat :'ZSCN00003' 'CELLS' 'X' rt_fcat.
              WHEN 'MSGTB'.
                _l_set_fcat :'ZSCN00003' 'MSGTB' 'X' rt_fcat.
            ENDCASE.
          ENDIF.
        ELSE.
          ls_fcat-inttype  = ls_fields-type->type_kind.
          CASE ls_fcat-inttype.
            WHEN 'P'.
              ls_fcat-intlen   = ls_fields-type->length * 2 - 1.
            WHEN OTHERS.
              ls_fcat-intlen   = ls_fields-type->length.
          ENDCASE.
          ls_fcat-decimals = ls_fields-type->decimals.
        ENDIF.

        TRY.
            lrf_elem ?= ls_fields-type.
            ls_fcat-edit_mask = lrf_elem->edit_mask.
            IF lrf_elem->is_ddic_type( ) IS NOT INITIAL.
              ls_dfies          = lrf_elem->get_ddic_field( ).
              ls_fcat-convexit  = ls_dfies-convexit.
              ls_fcat-domname   = ls_dfies-domname.
              ls_fcat-datatype  = ls_dfies-datatype.
              ls_fcat-inttype   = ls_dfies-inttype.
              ls_fcat-outputlen = ls_dfies-outputlen.
              IF ls_dfies-intlen > ls_dfies-outputlen.
                ls_fcat-intlen    = ls_dfies-outputlen.
              ELSE.
                ls_fcat-intlen    = ls_dfies-intlen.
              ENDIF.
              ls_fcat-dd_outlen = ls_dfies-outputlen.
              ls_fcat-coltext   = ls_dfies-fieldtext.
              ls_fcat-scrtext_l = ls_dfies-scrtext_l.
              ls_fcat-scrtext_m = ls_dfies-scrtext_m.
              ls_fcat-scrtext_s = ls_dfies-scrtext_s.
              ls_fcat-decimals  = ls_dfies-decimals.
            ENDIF.

            READ TABLE lt_ref INTO ls_ref WITH TABLE KEY zfd COMPONENTS fieldname = ls_fields-name.
            IF sy-subrc EQ 0.
              ls_fcat-ref_table = ls_ref-ref_table.
              ls_fcat-ref_field = ls_ref-fieldname.
            ENDIF.

            CLEAR : ls_fcat-key.
            APPEND ls_fcat TO rt_fcat.
          CATCH cx_sy_move_cast_error INTO lrf_cast.
            lrf_tadesc  ?= ls_fields-type.
            DATA(lv_msg) = lrf_cast->get_text( ).
        ENDTRY.
      ENDLOOP.
    ENDIF.

  ENDMETHOD.


METHOD get_fcat.
*&---------------------------------------------------------------------*
*& Module              : CN
*& Program ID          : ZCL_CN_ALV_GRID
*& T-CODE              : N/A
*& Referenced Program  : N/A
*& Created by          : T0200335
*& Created On          : 2020.12.23
*& Type                : CLASS
*& Description         : ZCL_CN_ALV_GRID
*&---------------------------------------------------------------------*
*& Change History
*&---------------------------------------------------------------------*
*& 변경SEQ    변경자        변경일자             변경내용
*&---------------------------------------------------------------------*


  DATA:lrf_aggregations TYPE REF TO cl_salv_aggregations.

  DATA:lv_helo_id TYPE string,
       lo_data    TYPE REF TO data.

  FIELD-SYMBOLS : <lt_table> TYPE STANDARD TABLE,
                  <ls_line>  TYPE any.

*  CLEAR : at_fcat.

  CREATE DATA lo_data LIKE it_table.
  ASSIGN lo_data->* TO <lt_table>.

*-------------------------------
* Create Field Catalog..
*-------------------------------
  TRY.
      cl_salv_table=>factory(
        IMPORTING r_salv_table = DATA(lo_salv) " Basis Class Simple ALV Tables
        CHANGING  t_table      = <lt_table>  ).

      DATA(lrf_columns)      = lo_salv->get_columns( ).

      DATA(lt_fcat) = cl_salv_controller_metadata=>get_lvc_fieldcatalog(
                                   EXPORTING r_columns      = lrf_columns         " ALV Column
                                             r_aggregations = lrf_aggregations ). "

    CATCH cx_salv_msg. " ALV: General Error Class with Message
    CATCH cx_root.

  ENDTRY.

*-------------------------------
* Set Ref_table & Ref_field..
*-------------------------------
  CREATE DATA lo_data LIKE LINE OF <lt_table>.
  ASSIGN lo_data->* TO <ls_line>.
  CHECK sy-subrc = 0.

  LOOP AT lt_fcat ASSIGNING FIELD-SYMBOL(<ls_fcat>).

    DATA(lv_tabix) = sy-tabix.
    ASSIGN COMPONENT <ls_fcat>-fieldname OF STRUCTURE <ls_line> TO FIELD-SYMBOL(<lv_fld>).

    CHECK <lv_fld> IS ASSIGNED.

    <ls_fcat>-col_id = lv_tabix.
    CASE <ls_fcat>-edit_mask(2).
      WHEN '=='.
        DATA(lv_conv) = <ls_fcat>-edit_mask.
        REPLACE FIRST OCCURRENCE OF '==' IN lv_conv WITH space.
        <ls_fcat>-convexit = lv_conv.
    ENDCASE.

    CLEAR:<ls_fcat>-key.   "Key는 각 로컬에서 설정

    <ls_fcat>-coltext = <ls_fcat>-reptext.
    DESCRIBE FIELD <lv_fld> HELP-ID lv_helo_id.
    SPLIT lv_helo_id AT '-' INTO <ls_fcat>-ref_table <ls_fcat>-ref_field.
    IF <ls_fcat>-ref_table IS INITIAL OR
       <ls_fcat>-ref_field IS INITIAL.
      CLEAR:<ls_fcat>-ref_table,<ls_fcat>-ref_field.
    ENDIF.

    CASE <ls_fcat>-fieldname.
      WHEN 'STATU'.
        <ls_fcat>-coltext = <ls_fcat>-reptext = TEXT-c13.
        <ls_fcat>-col_pos = 1.
        <ls_fcat>-icon    = 'X'.
        <ls_fcat>-just    = 'C'.
      WHEN 'CELLC' OR 'CELLS' OR 'MSGTB' OR 'DCFLG' .
        <ls_fcat>-no_out = <ls_fcat>-tech = 'X'.
    ENDCASE.
  ENDLOOP.


*..// return field-catalog.
  rt_fcat = lt_fcat.


ENDMETHOD.


  method ON_SET_FILTER.
*&---------------------------------------------------------------------*
*& Module      : CN
*& 생성자      : T0200335
*& 생성일      :  2020.12.30
*& Description :
*&---------------------------------------------------------------------*
*& Change History
*&---------------------------------------------------------------------*
*& 변경SEQ    변경자        변경일자             변경내용
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*

*-----------------------------------------------------
* 개별 프로그램에서  PROTECTED SECTION 에서 Redfefine 사용
* 그리드가 여러개 일 경우 GRID 명으로 구분 (IV_NAME)
* Filster 설정
* 참조 프로그램:YALV/YALV2
*-----------------------------------------------------
*=======================================================
*  PROTECTED SECTION.
*    METHODS:
**---------------------------
** Event Area
**---------------------------
*  ON_SET_FILTERT     REDEFINITION.

*&---------------------------------------------------------------------*
*& Class Implementation
*&---------------------------------------------------------------------*
*CLASS lcl_cust_alv_grid IMPLEMENTATION.

*  METHOD ON_SET_FILTER.
*    CASE iv_name.
*      WHEN 'ALV_GRID'.
** ct_filter = lt_filter
*      WHEN 'GRID_DTL'.
*    ENDCASE.
*  ENDMETHOD.


  endmethod.


  METHOD xml_upload.
*&---------------------------------------------------------------------*
*& Module      : CN
*& 생성자      : T0200335
*& 생성일      :  2020.12.30
*& Description :
*&---------------------------------------------------------------------*
*& Change History
*&---------------------------------------------------------------------*
*& 변경SEQ    변경자        변경일자             변경내용
*&---------------------------------------------------------------------*

    DATA:lv_col   TYPE i,
         lv_index TYPE i.


*-------------------------------------------
* Get Excel Cell Data
*-------------------------------------------
    me->get_exe_cells_value(
      EXPORTING
        iv_filename    = iv_filename      " Local Filename
        iv_sheet_index = iv_sheet_index
      IMPORTING
        et_cells    = DATA(lt_data)    " Cels
        ev_subrc    = ev_subrc
      EXCEPTIONS
        drm_encryption = 1             " DRM Encryption
        OTHERS         = 2
    ).

    IF ev_subrc IS NOT INITIAL.
      EXIT.
    ENDIF.

*-------------------------------------------
* Set Excel Cell Data To Internal Table
*-------------------------------------------
    DATA(lt_fcat) = it_fcat.

    SORT: lt_data BY row column.

    IF ( iv_beg_row - 1 ) > 0.
      DO ( iv_beg_row - 1 ) TIMES.
        IF sy-index > iv_end_row.
          EXIT.
        ENDIF.
        DELETE lt_data WHERE row = sy-index.  "라인삭제

      ENDDO.
    ENDIF.

    lv_col = 1 - iv_beg_col.


    LOOP AT lt_data INTO DATA(ls_data).

      AT NEW row.
        CLEAR:lv_index.
        APPEND INITIAL LINE TO ct_data.
        READ TABLE ct_data ASSIGNING FIELD-SYMBOL(<ls_trg>) INDEX ( lines( ct_data[] ) ).
        IF sy-subrc <> 0 OR <ls_trg> IS NOT ASSIGNED.
          CONTINUE.
        ELSE.
          lv_index = sy-tabix.
        ENDIF.

      ENDAT.

      DATA(lv_col_num) = me->cell_to_index( ls_data-column ) + lv_col.

      CHECK lv_col_num <= iv_end_col.

*-----------------------------
* Type..
*-----------------------------

      READ TABLE lt_fcat INTO DATA(ls_fcat) INDEX lv_col_num.
      IF sy-subrc <> 0.
        IF  ls_data-value IS NOT INITIAL.
          MESSAGE s000(zcn00) WITH TEXT-m01 DISPLAY LIKE 'E'.
          ev_subrc = 4.
          EXIT.
        ELSE.
          CONTINUE.  "bLANK
        ENDIF.
      ENDIF.

      CASE ls_fcat-inttype.
        WHEN cl_abap_typedescr=>typekind_int    OR
             cl_abap_typedescr=>typekind_int1   OR
             cl_abap_typedescr=>typekind_int2   OR
             cl_abap_typedescr=>typekind_int8   OR
             cl_abap_typedescr=>typekind_packed OR
             cl_abap_typedescr=>typekind_float.

        WHEN cl_abap_typedescr=>typekind_date   OR
             cl_abap_typedescr=>typekind_time. " Date Or Time..
          ls_data-value =  me->conv_numc( iv_value = ls_data-value ).

      ENDCASE.

*------------------------
*-- Get Colum Position
*------------------------
*      lv_col_num = lv_col_num + lv_col.  "시작 컬럼
      ASSIGN COMPONENT lv_col_num OF STRUCTURE <ls_trg> TO FIELD-SYMBOL(<lv_trg>).

      IF sy-subrc <> 0 OR <lv_trg> IS NOT ASSIGNED.
        CONTINUE.
      ELSE.
        TRY .
            <lv_trg> = ls_data-value.

*-------------------------
* Check Column Value
*-------------------------
            me->on_chk_upl_xml_col( EXPORTING iv_name      = me->m_name
                                              iv_fieldname = ls_fcat-fieldname
                                    CHANGING  cv_col_val   = <lv_trg>
                                              cs_data      = <ls_trg> ).

          CATCH cx_sy_conversion_no_number.
            CLEAR:<lv_trg>.
            me->set_err_msg( EXPORTING iv_fieldname = ls_fcat-fieldname
                                       iv_value     = ls_data-value
                                       iv_msg       = ls_fcat-coltext
                                       iv_msg2      = TEXT-e04
                             CHANGING   cs_data = <ls_trg> ).
*            ev_subrc = 5.  "ConversiON
*            RAISE conversion_error.

          CATCH cx_sy_conversion_overflow INTO DATA(lrf_oref).
            CLEAR:<lv_trg>.
            me->set_err_msg( EXPORTING iv_fieldname = ls_fcat-fieldname
                                       iv_value     = ls_data-value
                                       iv_msg       = ls_fcat-coltext
                                       iv_msg2      = TEXT-e04
                             CHANGING   cs_data = <ls_trg> ).
*            ev_subrc = 4.  "ConversiON
          CATCH cx_sy_conversion_error.
            CLEAR:<lv_trg>.
            me->set_err_msg( EXPORTING iv_fieldname = ls_fcat-fieldname
                                       iv_value     = ls_data-value
                                       iv_msg       = ls_fcat-coltext
                                       iv_msg2      = TEXT-e04
                             CHANGING   cs_data = <ls_trg> ).
*            ev_subrc = 4.  "ConversiON
          CATCH cx_root INTO DATA(lr_root).
            ev_subrc = 4.

            EXIT.
        ENDTRY.
      ENDIF.

      AT END OF row.
        IF <ls_trg> IS ASSIGNED AND <ls_trg> IS INITIAL AND
           lv_index IS NOT INITIAL.
          DELETE ct_data INDEX lv_index.
        ENDIF.
      ENDAT.

      CLEAR:lv_col_num.

    ENDLOOP.

  ENDMETHOD.


  METHOD SIMPLE_ALV.
*&---------------------------------------------------------------------*
*& Module              : CN
*& Program ID          : ZCL_CN_ALV_GRID
*& T-CODE              : N/A
*& Referenced Program  : N/A
*& Created by          : T0200335
*& Created On          : 2020.12.23
*& Type                : CLASS
*& Description         : CRT_DYN_TABLE
*&---------------------------------------------------------------------*
*& Change History
*&---------------------------------------------------------------------*
*& 변경SEQ    변경자        변경일자             변경내용
*&---------------------------------------------------------------------*
    TRY.
        cl_salv_table=>factory( IMPORTING r_salv_table = DATA(lr_table)
                                CHANGING  t_table      = it_itab[] ).
      CATCH cx_salv_msg.                                "#EC NO_HANDLER
    ENDTRY.

    DATA(lr_functions) = lr_table->get_functions( ).
    DATA(lr_columns)   = lr_table->get_columns( ).
    DATA(lr_disp_set) = lr_table->get_display_settings( ).

*--------------------
*Functions
*--------------------

    IF iv_toolbar EQ abap_on.
      lr_functions->set_all( ).
    ENDIF.
*--------------------
*Set Optimize
*--------------------

    lr_columns->set_optimize( abap_true ).
*--------------------
*Set Popup
*--------------------
    IF iv_start_column IS NOT INITIAL.
      lr_table->set_screen_popup( start_column = iv_start_column
                                  end_column   = iv_end_column
                                  start_line   = iv_start_line
                                  end_line     = iv_end_line ).
    ENDIF.
*--------------------
*Set Title
*--------------------
    IF iv_title IS NOT INITIAL.
      lr_disp_set->set_list_header( iv_title ).
    ENDIF.

    LOOP AT lr_columns->get( ) INTO DATA(ls_columns).
*--------------------
*Set Header Text
*--------------------
      IF it_hdtxt IS NOT INITIAL.
        READ TABLE it_hdtxt ASSIGNING FIELD-SYMBOL(<ls_hdtxt>) WITH KEY fieldname = ls_columns-columnname.
        IF sy-subrc = 0.
          lr_columns->get_column( ls_columns-columnname )->set_short_text( CONV #( <ls_hdtxt>-reptext ) ).
          lr_columns->get_column( ls_columns-columnname )->set_medium_text( CONV #( <ls_hdtxt>-reptext ) ).
          lr_columns->get_column( ls_columns-columnname )->set_long_text( CONV #( <ls_hdtxt>-reptext ) ).
        ENDIF.
      ENDIF.

*--------------------
*Set Noout Fields
*--------------------
      IF it_noout IS NOT INITIAL.
        READ TABLE it_noout ASSIGNING FIELD-SYMBOL(<ls_noout>) WITH KEY
                                      table_line = ls_columns-columnname.
        IF sy-subrc = 0.
          lr_columns->get_column( ls_columns-columnname )->set_technical( abap_on ).
        ENDIF.
      ENDIF.

*--------------------
*Set Currency Fields
*--------------------
      IF it_curr IS NOT INITIAL.
        READ TABLE it_curr ASSIGNING FIELD-SYMBOL(<ls_curr>) WITH KEY
                                     table_line = ls_columns-columnname.
        IF sy-subrc = 0.
          IF iv_curr IS NOT INITIAL.
            lr_columns->get_column( ls_columns-columnname )->set_currency_column( iv_curr ).
          ENDIF.
        ENDIF.
      ENDIF.
    ENDLOOP.

*--------------------
*-- Show SALV
*--------------------
    lr_table->display( ).

  ENDMETHOD.


  METHOD show_msgtb.
*&---------------------------------------------------------------------*
*& Module      : CN
*& 생성자      : T0200335
*& 생성일      :  2020.12.30
*& Description :
*&---------------------------------------------------------------------*
*& Change History
*&---------------------------------------------------------------------*
*& 변경SEQ    변경자        변경일자             변경내용
*&---------------------------------------------------------------------*

    CHECK it_msgtb IS NOT INITIAL.

*--------------------
* Message Initial
*--------------------
    CALL FUNCTION 'MESSAGES_INITIALIZE'.

*--------------------
* Message 저장
*--------------------
    LOOP AT it_msgtb INTO DATA(ls_msgtb).

      CALL FUNCTION 'MESSAGE_STORE'
        EXPORTING
          msgty                  = ls_msgtb-msgty
          arbgb                  = ls_msgtb-arbgb
          txtnr                  = ls_msgtb-txtnr
          msgv1                  = ls_msgtb-msgv1
          msgv2                  = ls_msgtb-msgv2
          msgv3                  = ls_msgtb-msgv3
          msgv4                  = ls_msgtb-msgv4
        EXCEPTIONS
          message_type_not_valid = 1
          not_active             = 2
          OTHERS                 = 3.

    ENDLOOP.

*-------------------------------
* Display Message
*-------------------------------
    CALL FUNCTION 'MESSAGES_SHOW'
      EXPORTING
        corrections_option = ''
        object             = TEXT-m05
        i_use_grid         = 'X'
      EXCEPTIONS
        inconsistent_range = 1
        no_messages        = 2
        OTHERS             = 3.

  ENDMETHOD.


  METHOD set_toolbar_by_mode.
*&---------------------------------------------------------------------*
*& Module      : CN
*& 생성자      : T0200335
*& 생성일      :  2020.12.30
*& Description :
*&---------------------------------------------------------------------*
*& Change History
*&---------------------------------------------------------------------*
*& 변경SEQ    변경자        변경일자             변경내용
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*

*-----------------------------------------------------------------------------------------
* ALV 공통으로 사용되는 Toolbar
* (화면전환,라인추가/라인삭제/Recovery/Excel Download/ Excel Upload)
* 단, 화면별로 보여지는 Tool이 다를 수 있으므로 Constuctor 에서 IS_TOOLBTN 으로 제어한다
* 그외 User Toolbar는 METHOD EVT_ON_TOOLBAR에서 정의
*-----------------------------------------------------------------------------------------
    DEFINE _l_add_toolbar.
      ls_add_toolbar-function    = &1.
      ls_add_toolbar-icon        = &2.
      ls_add_toolbar-quickinfo   = &3.
      ls_add_toolbar-butn_type   = &4.
      ls_add_toolbar-disabled    = &5.
      ls_add_toolbar-text        = &6.

      APPEND ls_add_toolbar TO lt_toolbar.
    END-OF-DEFINITION.


    DEFINE _l_add_toolbar.

      ls_button = VALUE #( butn_type = &1 function = &2 icon = &3  quickinfo = &4 text = &5 ).

      APPEND ls_button TO crf_object->mt_toolbar.

    END-OF-DEFINITION.

    DATA : lt_add_toolbar TYPE ttb_button,
           ls_button      TYPE stb_button,
           lv_text(50),
           lv_mline       TYPE i.

    IF is_toolbtn IS INITIAL.
      MOVE-CORRESPONDING as_toolbtn TO is_toolbtn.
    ENDIF.

    IF is_toolbtn-btn_info_text IS INITIAL.
      lv_text = TEXT-b10.
    ELSE.
      lv_text = is_toolbtn-btn_info_text.
    ENDIF.
*---------------------------------------------------
* Default Toolbar Button
*---------------------------------------------------
    _l_add_toolbar '3' '' '' '' ''.  " 구분자

    CASE iv_edit_mode.
      WHEN ''.

* Information
        IF is_toolbtn-btn_info IS NOT INITIAL.
          _l_add_toolbar '' 'INFO' icon_information lv_text lv_text .   "정보
          _l_add_toolbar '3' '' '' '' ''. "구분자
        ENDIF.

*-- History
        IF is_toolbtn-btn_hist  IS NOT INITIAL.
          _l_add_toolbar '' 'HIST' icon_history  TEXT-b09 TEXT-b09.
          _l_add_toolbar '3' '' '' '' ''. "구분자
        ENDIF.

*-- File Attatchment
        IF is_toolbtn-btn_gos IS NOT INITIAL.
          _l_add_toolbar '' 'FILE' icon_attachment TEXT-b11 TEXT-b11.         "File Attatchment
          _l_add_toolbar '3' '' '' '' ''. "구분자
        ENDIF.

*-- Excel DownLoad
        IF is_toolbtn-btn_exld IS NOT INITIAL.
          _l_add_toolbar '' 'EXDL' icon_xxl TEXT-b08 TEXT-b08.   "Exel Download
          _l_add_toolbar '3' '' '' '' ''. "구분자
        ENDIF.

      WHEN 'X'.
*-----------------------------
* Edit Mode 일경우
*-----------------------------
*        IF is_toolbtn-btn_dis  IS NOT INITIAL.
*          _l_add_toolbar '' 'CHAN' icon_display TEXT-b02 TEXT-b02.  "Display
*          _l_add_toolbar '3' '' '' '' ''. "구분자
*        ENDIF.

* Information
        IF is_toolbtn-btn_info IS NOT INITIAL.
          _l_add_toolbar '' 'INFO' icon_information lv_text  lv_text .   "정보
          _l_add_toolbar '3' '' '' '' ''. "구분자
        ENDIF.

*-- Insert Row
        IF is_toolbtn-btn_add  IS NOT INITIAL.
          _l_add_toolbar '' 'AROW' icon_insert_row  '' ''.
          _l_add_toolbar '3' '' '' '' ''. "구분자
        ENDIF.

*-- Multy Row
        IF is_toolbtn-btn_madd  IS NOT INITIAL.
          IF is_toolbtn-mlti_lines IS INITIAL.
            lv_mline = 10.
          ELSE.
            lv_mline = is_toolbtn-mlti_lines.
          ENDIF.

          DATA(lv_str1) = |{ TEXT-c01 } { lv_mline } { TEXT-c02 }|.   "Insert 10 Rows
          DATA(lv_str2) = |{ lv_mline } { TEXT-c02 }|.                "10 Rows
          _l_add_toolbar '' 'MAROW' icon_insert_multiple_lines  lv_str1 lv_str2.
          _l_add_toolbar '3' '' '' '' ''. "구분자
        ENDIF.

*-- Delete Row
        IF is_toolbtn-btn_del  IS NOT INITIAL.
          _l_add_toolbar '' 'DROW'  icon_delete_row  TEXT-b04 ''.     "Delete Row
          _l_add_toolbar '3' '' '' '' ''. "구분자
        ENDIF.

*-- Recovery Row
        IF is_toolbtn-btn_rec  IS NOT INITIAL.
          _l_add_toolbar '' 'RECO'  icon_system_undo  TEXT-b03 TEXT-b03. "Recovery Row
          _l_add_toolbar '3' '' '' '' ''. "구분자
        ENDIF.

*-- History
        IF is_toolbtn-btn_hist  IS NOT INITIAL.
          _l_add_toolbar '' 'HIST' icon_history  TEXT-b09 TEXT-b09.
          _l_add_toolbar '3' '' '' '' ''. "구분자
        ENDIF.

*-- File Attatchment
        IF is_toolbtn-btn_gos IS NOT INITIAL.
          _l_add_toolbar '' 'FILE' icon_attachment TEXT-b11 TEXT-b11.         "File Attatchment
          _l_add_toolbar '3' '' '' '' ''. "구분자
        ENDIF.

*-- Excel Upload
        IF is_toolbtn-btn_exlu IS NOT INITIAL.
          _l_add_toolbar '' 'EXUP' icon_xls TEXT-b05 TEXT-b06.         "Excel Upload
          _l_add_toolbar '3' '' '' '' ''. "구분자
        ENDIF.

*-- Excel DownLoad
        IF is_toolbtn-btn_exld IS NOT INITIAL.
          _l_add_toolbar '' 'EXDL' icon_xxl TEXT-b07 TEXT-b08.         "Excel Download
          _l_add_toolbar '3' '' '' '' ''. "구분자
        ENDIF.

    ENDCASE.

  ENDMETHOD.


  METHOD set_timestamp.
*&---------------------------------------------------------------------*
*& Module              : CN
*& Program ID          : ZCL_CN_ALV_GRID
*& T-CODE              : N/A
*& Referenced Program  : N/A
*& Created by          : T0200335
*& Created On          : 2020.12.23
*& Type                : CLASS
*& Description         : CRT_DYN_TABLE
*&---------------------------------------------------------------------*
*& Change History
*&---------------------------------------------------------------------*
*& 변경SEQ    변경자        변경일자             변경내용
*&---------------------------------------------------------------------*

    DATA: BEGIN OF ls_stamp.
            include TYPE zscn_timestamp.
    DATA:   erdat_l TYPE  erdat,
            erzet_l TYPE  erzet,
            aedat_l TYPE  aedat,
            aezet_l TYPE  aezet,
          END OF ls_stamp.

    MOVE-CORRESPONDING cs_data TO ls_stamp.

    GET TIME.
    ls_stamp-aenam   = sy-uname.
    ls_stamp-aedat   = sy-datum.
    ls_stamp-aezet   = sy-uzeit.

    IF iv_local IS NOT INITIAL.
      ls_stamp-aedat_l   = sy-datlo.
      ls_stamp-aezet_l   = sy-timlo.
    ENDIF.

    IF iv_create = 'X'.
      ls_stamp-erdat   = ls_stamp-aedat.
      ls_stamp-erzet   = ls_stamp-aezet.
      ls_stamp-ernam   = ls_stamp-aenam.
    ENDIF.

    MOVE-CORRESPONDING ls_stamp TO cs_data.

  ENDMETHOD.


  METHOD set_status.
*&---------------------------------------------------------------------*
*& Module      : CN
*& 생성자      : T0200335
*& 생성일      :  2020.12.30
*& Description :
*&---------------------------------------------------------------------*
*& Change History
*&---------------------------------------------------------------------*
*& 변경SEQ    변경자        변경일자             변경내용
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*

    DATA:lv_del.

    FIELD-SYMBOLS:<lt_data>  TYPE table,
                  <lt_old>   TYPE table,
                  <lt_msgtb> TYPE table.

    ASSIGN ao_dataref->* TO <lt_data>.
    CHECK sy-subrc = 0.

*--------------------------------
* History..
*--------------------------------
    IF iv_type IS INITIAL.
      RAISE EVENT save_hist.
    ENDIF.


    LOOP AT <lt_data> ASSIGNING FIELD-SYMBOL(<ls_data>).
*---------------------------------
* Set DCFLAG/Status..
*---------------------------------
      ASSIGN COMPONENT 'DCFLG' OF STRUCTURE <ls_data> TO FIELD-SYMBOL(<lv_dcflg>).
      CHECK sy-subrc = 0.

      ASSIGN COMPONENT 'STATU' OF STRUCTURE <ls_data> TO FIELD-SYMBOL(<lv_statu>).
      CHECK sy-subrc = 0.

      ASSIGN COMPONENT 'ZDELE' OF STRUCTURE <ls_data> TO FIELD-SYMBOL(<lv_del>).
      IF sy-subrc = 0.
        lv_del = <lv_del>.
      ENDIF.

*-----------------------
*-- Message가 있을 경우
*-----------------------
      ASSIGN COMPONENT 'MSGTB' OF STRUCTURE <ls_data> TO <lt_msgtb>.
      IF sy-subrc = 0.
        IF sy-subrc = 0 AND <lt_msgtb> IS NOT INITIAL AND <lv_statu> IS INITIAL.
          <lv_statu> = set_icon( 'M' ).       " ICON_MESSAGE_ERROR_SMALL.
          CONTINUE.
        ENDIF.
      ENDIF.

      IF iv_type    IS INITIAL OR
         <lv_statu> IS INITIAL.
*------------------------------
*-- Check Dcflag..
*------------------------------
        CASE <lv_dcflg>.
          WHEN 'C' OR 'T' OR 'U'.   "Create/Delete/Update
          WHEN 'T'.
          WHEN 'U'.
          WHEN OTHERS.
            IF lv_del IS NOT INITIAL.
              <lv_statu> = me->set_icon( 'D' ). "Delete
            ELSE.
              IF <lv_statu> IS INITIAL.  "<> icon_led_yellow.
                <lv_statu> = me->set_icon( '' ).    "Green
              ENDIF.
            ENDIF.
            CLEAR:<lv_dcflg>.
            CONTINUE.
        ENDCASE.

*--------------------
* Set Data Status..
*--------------------
        IF lv_del IS INITIAL.
          <lv_statu> = me->set_icon( 'S' ). "Save
        ELSE.
          <lv_statu> = me->set_icon( 'D' ). "Delete
        ENDIF.

        CLEAR:<lv_dcflg>.

      ENDIF.

    ENDLOOP.

*--------------------------------
* Clear ALV Status(Data Changed)
*--------------------------------
    CLEAR:av_status.


*---------------------------------------
* Save Type..
*---------------------------------------
    CHECK iv_type IS INITIAL.   "Save일경우

    ASSIGN ao_oldref->* TO <lt_old>.
    MOVE-CORRESPONDING <lt_data> TO <lt_old>.
*    <lt_old> = <lt_data>.


  ENDMETHOD.


  METHOD set_msgtb.
*&---------------------------------------------------------------------*
*& Module      : CN
*& 생성자      : T0200335
*& 생성일      :  2020.12.30
*& Description :
*&---------------------------------------------------------------------*
*& Change History
*&---------------------------------------------------------------------*
*& 변경SEQ    변경자        변경일자             변경내용
*&---------------------------------------------------------------------*
    FIELD-SYMBOLS: <lt_msgtb> TYPE zycn00001.

    CHECK is_msgtb IS NOT INITIAL.

    ASSIGN COMPONENT 'DCFLG' OF STRUCTURE cs_data TO FIELD-SYMBOL(<lv_dcflg>).
    CHECK sy-subrc = 0.

    ASSIGN COMPONENT 'STATU' OF STRUCTURE cs_data TO FIELD-SYMBOL(<lv_statu>).
    CHECK sy-subrc = 0.

    ASSIGN COMPONENT 'MSGTB' OF STRUCTURE cs_data TO <lt_msgtb>.
    CHECK sy-subrc = 0.

    CASE iv_delete.
      WHEN 'D'.
        DELETE <lt_msgtb> FROM 1 WHERE fieldname EQ is_msgtb-fieldname
                                    OR fieldname EQ '*'.
        IF <lt_msgtb> IS INITIAL.
          <lv_statu> = set_icon( <lv_dcflg> ).
        ENDIF.
      WHEN OTHERS.

        CHECK cs_data  IS NOT INITIAL.

        APPEND is_msgtb TO <lt_msgtb>.

        <lv_statu> = set_icon( 'M' ).  .

        "개별 프로그램에서 Redefine Message Line..
        me->on_set_msgtb( EXPORTING iv_name      = me->m_name
                                    iv_fieldname = is_msgtb-fieldname
                          CHANGING  cs_data      = cs_data ).
    ENDCASE.

  ENDMETHOD.


  METHOD set_line_style.
*&---------------------------------------------------------------------*
*& Module      : CN
*& 생성자      : T0200335
*& 생성일      :  2020.12.30
*& Description :
*&---------------------------------------------------------------------*
*& Change History
*&---------------------------------------------------------------------*
*& 변경SEQ    변경자        변경일자             변경내용
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*


    DATA:ls_cells TYPE lvc_s_styl,
         lv_del.

    FIELD-SYMBOLS:<lt_cells> TYPE lvc_t_styl,
                  <lt_msgtb> TYPE table,
                  <lt_data>  TYPE table.

    ASSIGN ao_dataref->* TO <lt_data>.

    CHECK sy-subrc = 0.

*---------------------
*-- Default Value..
*---------------------

    DATA(lr_dftvl) = ar_dftvl.
    lr_dftvl = VALUE #( BASE lr_dftvl FOR wa_dftvl IN at_dftvl_add
                                          ( sign = 'I' option = 'EQ' low = wa_dftvl-fieldname ) ).  "추가로 들어오는 Default value..

*---------------------------------------
*-- Edit 인것만.. Style 열어주기
*---------------------------------------
    DATA(lt_fcat) = VALUE lvc_t_fcat( FOR wa_fcat IN at_fcat WHERE
                                     ( edit IS NOT INITIAL )
                                     ( CORRESPONDING #( wa_fcat ) ) ).


    LOOP AT <lt_data> ASSIGNING FIELD-SYMBOL(<ls_data>).


      ASSIGN COMPONENT 'DCFLG' OF STRUCTURE <ls_data> TO FIELD-SYMBOL(<lv_dcflg>).
      CHECK sy-subrc = 0.

      ASSIGN COMPONENT 'CELLS' OF STRUCTURE <ls_data> TO <lt_cells>.
      CHECK sy-subrc = 0.

      CLEAR:lv_del, <lt_cells>.
      ASSIGN COMPONENT 'ZDELE' OF STRUCTURE <ls_data> TO FIELD-SYMBOL(<lv_zdele>).
      IF sy-subrc = 0.
        lv_del = <lv_zdele>.
      ENDIF.

      LOOP AT lt_fcat INTO DATA(ls_fcat).
        ls_cells-fieldname = ls_fcat-fieldname.

        IF lv_del = 'X' OR <lv_dcflg> = 'T'.  "삭제일경우는 무조건 DELETE
          ls_cells-style = cl_gui_alv_grid=>mc_style_disabled.
          INSERT ls_cells INTO TABLE <lt_cells>.
          CONTINUE.
        ELSE.

          CASE ls_fcat-key.
            WHEN 'X'.
*--------------------------------------------------------
* Key일 경우 default Value가 들어올 경우는 Disabled..
*--------------------------------------------------------
              CASE <lv_dcflg>.
                WHEN 'C'. "신규필드인 경우
                  IF ( lr_dftvl[] IS NOT INITIAL AND ls_fcat-fieldname IN lr_dftvl ).    "Default Value Check..
                    ls_cells-style = cl_gui_alv_grid=>mc_style_disabled.
                  ELSE.
                    ls_cells-style = cl_gui_alv_grid=>mc_style_enabled.
                  ENDIF.
                WHEN OTHERS.
                  ls_cells-style = cl_gui_alv_grid=>mc_style_disabled.
              ENDCASE.
            WHEN OTHERS.
              ls_cells-style = cl_gui_alv_grid=>mc_style_enabled.
          ENDCASE.
        ENDIF.

        "User 컬럼 /Row 별  라인세팅
        me->on_set_line_style_by_row( EXPORTING iv_name  = me->m_name
                                                is_data  = <ls_data>
                                                is_fcat  = ls_fcat
                                      CHANGING  cs_cells = ls_cells ).

        INSERT ls_cells INTO TABLE <lt_cells>.
      ENDLOOP.

    ENDLOOP.

*===========================
* User Line Sytle
*===========================
    me->on_set_line_style( EXPORTING iv_name = me->m_name
                                     it_fcat = at_fcat ).


*    REFRESH:at_dftvl_add.  "추가 Default는 상위Header 별로 달라지므로 Clear..

  ENDMETHOD.


  METHOD set_layout_title.
*&---------------------------------------------------------------------*
*& Module      : CN
*& 생성자      : T0200335
*& 생성일      :  2020.12.30
*& Description :
*&---------------------------------------------------------------------*
*& Change History
*&---------------------------------------------------------------------*
*& 변경SEQ    변경자        변경일자             변경내용
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*

*--------------------------------------------
* Set Layout Title..
*--------------------------------------------

    DATA:ls_layout TYPE lvc_s_layo.

*----------------------
* Get Current Lyaout
*----------------------
    me->get_frontend_layout( IMPORTING es_layout = ls_layout ).

    ls_layout-smalltitle =  'X'.
    ls_layout-grid_title = iv_title.

*----------------------
* set Lyaout
*----------------------
    me->set_frontend_layout( ls_layout ).

  ENDMETHOD.


  METHOD set_layout.
*&---------------------------------------------------------------------*
*& Module      : CN
*& 생성자      : T0200335
*& 생성일      :  2020.12.30
*& Description :
*&---------------------------------------------------------------------*
*& Change History
*&---------------------------------------------------------------------*
*& 변경SEQ    변경자        변경일자             변경내용
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*

    cs_layout-sel_mode = 'D'.
    cs_layout-cwidth_opt = 'X'.
    cs_layout-no_author  = 'X'.

    IF av_cellc IS NOT INITIAL.  "필드가 많을 경우 속도 저하로 인하여 사용할 경우만 세팅
      cs_layout-ctab_fname = zcn00_cellc.
    ENDIF.

    IF av_cells IS NOT INITIAL. "필드가 많을 경우 속도 저하로 인하여 사용할 경우만 세팅
      cs_layout-stylefname = zcn00_cells.
    ENDIF.

  ENDMETHOD.


  METHOD set_inactive_status.
*&---------------------------------------------------------------------*
*& Module              : CN
*& Program ID          : ZCL_CN_ALV_GRID
*& T-CODE              : N/A
*& Referenced Program  : N/A
*& Created by          : T0200335
*& Created On          : 2020.12.23
*& Type                : CLASS
*& Description         : ZCL_CN_ALV_GRID
*&---------------------------------------------------------------------*
*& Change History
*&---------------------------------------------------------------------*
*& 변경SEQ    변경자        변경일자             변경내용
*&---------------------------------------------------------------------*

    CHECK av_edit_mode = 'X'.

    CLEAR:av_status .

  ENDMETHOD.


  METHOD set_icon.
*&---------------------------------------------------------------------*
*& Module              : CN
*& Program ID          : ZCL_CN_ALV_GRID
*& T-CODE              : N/A
*& Referenced Program  : N/A
*& Created by          : T0200335
*& Created On          : 2020.12.23
*& Type                : CLASS
*& Description         : SET_ICON
*&---------------------------------------------------------------------*
*& Change History
*&---------------------------------------------------------------------*
*& 변경SEQ    변경자        변경일자             변경내용
*&---------------------------------------------------------------------*
    CASE iv_dcflg.
      WHEN 'C'. " Insert(신규)
        rv_icon = icon_create.
      WHEN 'U'. " Change(변경)
        rv_icon = icon_change.
      WHEN 'E'. "Single Red
        rv_icon = icon_led_red.
      WHEN 'F'. " Failure
        rv_icon = icon_failure.
      WHEN 'O'.
        rv_icon = icon_led_green.
      WHEN 'Y'.  "Yellow
        rv_icon = icon_led_yellow.
      WHEN 'T'. " Line Delete
        rv_icon = icon_delete_row.
      WHEN 'D'. " Delete (zdele)
        rv_icon = icon_delete.
      WHEN 'S'. "Save
        rv_icon = icon_system_save.
      WHEN 'X'.  "Excel
        rv_icon = icon_xls.
      WHEN 'R'. "Recovery
        rv_icon = icon_system_undo.
      WHEN 'M'. "Message Error
        rv_icon = icon_message_error_small.
      WHEN 'N'.  "Inactive
        rv_icon = icon_led_inactive.
      WHEN 'Z'.  "Complete
        rv_icon = icon_complete.

      WHEN OTHERS.
        rv_icon =  icon_led_green.
    ENDCASE.

  ENDMETHOD.


  METHOD set_hist_datakey.
*&---------------------------------------------------------------------*
*& Module      : CN
*& 생성자      : T0200335
*& 생성일      :  2020.12.30
*& Description :
*&---------------------------------------------------------------------*
*& Change History
*&---------------------------------------------------------------------*
*& 변경SEQ    변경자        변경일자             변경내용
*&---------------------------------------------------------------------*

    LOOP AT at_key_list INTO DATA(ls_key).
      ASSIGN COMPONENT ls_key-fieldname OF STRUCTURE is_data  TO FIELD-SYMBOL(<lv_key>).
      CHECK sy-subrc = 0.
      IF <lv_key> IS INITIAL.
        <lv_key> = me->ac_blk.        "Blank 일경우 @ 로 표현
      ENDIF.
      CONCATENATE ev_key <lv_key> '/'  INTO ev_key.

      CONCATENATE ev_keynm ls_key-fieldname '/'  INTO ev_keynm.

      AT LAST.
        "Set Data Key
        DATA(lv_strlen) = CONV i( strlen( ev_key ) - 1 ).
        ev_key+lv_strlen(1) = ''.

        "Set Key Name
        lv_strlen = CONV i( strlen( ev_keynm ) - 1 ).
        ev_keynm+lv_strlen(1) = ''.

      ENDAT.
    ENDLOOP.

  ENDMETHOD.


  METHOD set_grid.
*&---------------------------------------------------------------------*
*& Module      : CN
*& 생성자      : T0200335
*& 생성일      :  2020.12.30
*& Description :
*&---------------------------------------------------------------------*
*& Change History
*&---------------------------------------------------------------------*
*& 변경SEQ    변경자        변경일자             변경내용
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*

    DATA:lv_mode, lv_lock.

    DATA:lo_data TYPE REF TO data.

    FIELD-SYMBOLS:<lt_old> TYPE table.

    CLEAR:at_ex_toolbar, at_fcat, at_sort, at_filter, as_layout, at_dropdown.

    CREATE DATA ao_oldref LIKE ct_data.
    ASSIGN ao_oldref->* TO <lt_old>.
    CHECK sy-subrc = 0.

    GET REFERENCE OF ct_data INTO ao_dataref.

*---------------------------
* Set Status & Dcflag..
*---------------------------
    me->set_status( 'X' ).

*----------------------------------
*--  Default Layout
*----------------------------------
    me->set_layout( CHANGING cs_layout = as_layout ).

    "*--  User Layout Set
    me->on_set_layout(  EXPORTING iv_name   = me->m_name
                        CHANGING  cs_layout = as_layout ).

*---------------------------
* Set Field Catalog
*---------------------------
    IF it_fcat IS INITIAL.
      at_fcat = me->get_fcat( it_table = ct_data ).
    ELSE.
      at_fcat = it_fcat.
    ENDIF.

*----------------------------------
*--  Exclude Toolbar
*----------------------------------
    me->alv_exclud_std_toolbar( ).

*-----------------------
* Set Combo Box.
*-----------------------
    me->on_set_drop_down( EXPORTING iv_name = me->m_name
                          CHANGING  ct_drop = at_dropdown ).   "User Settint..

    me->set_drop_down_table( it_drop_down_alias = at_dropdown ).

*-- ---------------------------
* Append  F4 Field
*-- ---------------------------
    me->append_f4_field( ).

*-- ---------------------------
* Set Sort
*-- ---------------------------
    on_set_sort( EXPORTING iv_name = me->m_name
                 CHANGING ct_sort  = at_sort ).

*-- ---------------------------
* Set filter
*-- ---------------------------
    on_set_filter( EXPORTING iv_name    = me->m_name
                   CHANGING  ct_filter  = at_filter ).

*---------------------------------
* Set ALV Event
*---------------------------------
*----------------------
*-- Dynamic History Talbe..
*----------------------
    IF is_hist_type-hst_gub = 'X'.  "Set Grid 시점에 사용하겠다는 표시
      av_hst_tabnm = is_hist_type-hst_tabnm.
    ENDIF.

    IF av_hst_tabnm IS NOT INITIAL.
      as_toolbtn-btn_hist = 'X'.
      <lt_old> = ct_data.
    ELSE.
      CLEAR:as_toolbtn-btn_hist.
    ENDIF.

    me->on_set_event(  EXPORTING iv_name = me->m_name ).


    IF av_hst_tabnm IS NOT INITIAL.
      SET HANDLER me->evt_save_hist   FOR me.
    ENDIF.


    CASE av_dichg.
      WHEN 'X'.
        CALL METHOD me->set_change_mode(
          EXPORTING
            iv_dichg = 'X'
          CHANGING
            cv_mode  = lv_mode
            cv_lock  = lv_lock ).
        IF lv_lock = 'X'.
          CLEAR:av_dichg , cv_mode.
        ELSE.
          me->raise_event( EXPORTING i_ucomm = 'CHAN' ).
          cv_mode = 'X'.
        ENDIF.
    ENDCASE.

*---------------------------------
* Set Variant
*---------------------------------
    as_variant = VALUE disvariant( report    = sy-cprog
                                   username  = sy-uname
                                   variant   = iv_vari ).
*---------------------------
* Set Field Cat..
*---------------------------
    me->set_fcat_comm(  CHANGING  ct_fcat = at_fcat ).

*-- Key List
    DATA(lt_fcat) = at_fcat.
    SORT:lt_fcat BY col_pos.
    at_key_list = VALUE #( FOR wa_fcat IN lt_fcat WHERE
                                ( key IS NOT INITIAL )
                                ( CORRESPONDING #( wa_fcat ) ) ).
    SORT:at_key_list BY fieldname.
*--------------------------------
* Set Line Style..
*--------------------------------
    me->set_line_style( EXPORTING iv_name = me->m_name ).


*---------------------------------
* Display Data by ALV
*---------------------------------
    me->display_grid( EXPORTING is_variant = as_variant
                      CHANGING  ct_data    = ct_data ).


    me->set_ready_for_input( av_input ).

*---------------------------------
* Set Information Header Display
*---------------------------------
    CHECK arf_docu IS BOUND.

    SET HANDLER me->evt_top_of_page        FOR me.

    CALL METHOD me->list_processing_events
      EXPORTING
        i_event_name = 'TOP_OF_PAGE'
        i_dyndoc_id  = arf_docu.

  ENDMETHOD.


  METHOD set_fcat_comm.
*&---------------------------------------------------------------------*
*& Module      : CN
*& 생성자      : T0200335
*& 생성일      :  2020.12.30
*& Description :
*&---------------------------------------------------------------------*
*& Change History
*&---------------------------------------------------------------------*
*& 변경SEQ    변경자        변경일자             변경내용
*&---------------------------------------------------------------------*
*&   U01  / KTGT0220021 / 2022.07.20 / 사용자 툴팁값 유지
*&---------------------------------------------------------------------*
    at_fcat_o = at_fcat.  "값 비교를 위함

*---------------------------
* User Set Fieldcatalog
*---------------------------
    me->on_set_fcat(  EXPORTING iv_name = me->m_name
                      CHANGING  ct_fcat = at_fcat ).

*---------------------------
* User Set Fieldcatalog
*---------------------------
    SORT:at_fcat_o BY fieldname.

    LOOP AT ct_fcat ASSIGNING FIELD-SYMBOL(<ls_fcat>).

      <ls_fcat>-tooltip = COND #( WHEN <ls_fcat>-tooltip IS INITIAL THEN <ls_fcat>-coltext ELSE <ls_fcat>-tooltip ).  "U01 IN
*      <ls_fcat>-tooltip = <ls_fcat>-coltext. "U01 OUT

      "Original 값과 비교하여 같을 경우만 변경(로컬에서 변경했을 경웅 수정불가
      READ TABLE at_fcat_o INTO DATA(ls_fcat_o) WITH KEY fieldname = <ls_fcat>-fieldname BINARY SEARCH.
      CHECK sy-subrc = 0.


      IF <ls_fcat>-scrtext_m = ls_fcat_o-scrtext_m.
        <ls_fcat>-scrtext_m = <ls_fcat>-coltext.
      ENDIF.

      IF <ls_fcat>-scrtext_l = ls_fcat_o-scrtext_l.
        <ls_fcat>-scrtext_l = <ls_fcat>-coltext.
      ENDIF.

      IF <ls_fcat>-scrtext_s = ls_fcat_o-scrtext_s.
        <ls_fcat>-scrtext_s = <ls_fcat>-coltext.
      ENDIF.

      IF <ls_fcat>-reptext = ls_fcat_o-reptext.
        <ls_fcat>-reptext   = <ls_fcat>-coltext.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD set_err_msg.
*&---------------------------------------------------------------------*
*& Module      : CN
*& 생성자      : T0200335
*& 생성일      :  2020.12.30
*& Description :
*&---------------------------------------------------------------------*
*& Change History
*&---------------------------------------------------------------------*
*& 변경SEQ    변경자        변경일자             변경내용
*&---------------------------------------------------------------------*

    FIELD-SYMBOLS: <lt_msgtb> TYPE zycn00001.

    ASSIGN COMPONENT 'MSGTB' OF STRUCTURE cs_data TO <lt_msgtb>.
    CHECK sy-subrc = 0.

    _g_set_msgtb:'' cs_data iv_fieldname 'ZCN00' '000' iv_msg iv_msg2 iv_msg3.


  ENDMETHOD.


  METHOD set_edit_mode.
*&---------------------------------------------------------------------*
*& Module      : CN
*& 생성자      : T0200335
*& 생성일      :  2020.12.30
*& Description :
*&---------------------------------------------------------------------*
*& Change History
*&---------------------------------------------------------------------*
*& 변경SEQ    변경자        변경일자             변경내용
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*

    av_edit_mode = iv_edit_mode.

*-----------------------
* Edit Mode 전환
*-----------------------
    CASE iv_edit_mode.
      WHEN abap_true.
        av_input = 1.
      WHEN OTHERS.
        av_input = 0.
    ENDCASE.


  ENDMETHOD.


  METHOD SET_DATA_CHAGED_VALUE.
*&---------------------------------------------------------------------*
*& Module      : CN
*& 생성자      : T0200335
*& 생성일      :  2020.12.30
*& Description :
*&---------------------------------------------------------------------*
*& Change History
*&---------------------------------------------------------------------*
*& 변경SEQ    변경자        변경일자             변경내용
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
    DATA:ls_timestamp TYPE zscn_timestamp.

    FIELD-SYMBOLS:<lt_data>     TYPE table,
                  <lt_mod_rows> TYPE table.


    ASSIGN irf_data_changed->mp_mod_rows->* TO <lt_mod_rows>.
    CHECK sy-subrc = 0 AND <lt_mod_rows> IS NOT INITIAL.

    ASSIGN ao_dataref->* TO <lt_data>.

    DATA(lt_fcat) = CORRESPONDING ty_fcat( at_fcat ).


    CLEAR:av_error.
*=============================================
* Row별 Data Changed..
*=============================================
    LOOP AT <lt_mod_rows> ASSIGNING FIELD-SYMBOL(<ls_mod_rows>).

      DATA(lv_tabix) = sy-tabix.
      DATA(lt_cells) = VALUE lvc_t_modi( FOR wa_cell IN irf_data_changed->mt_mod_cells WHERE
                                          ( tabix = lv_tabix )
                                          ( wa_cell ) ).
      CHECK lt_cells IS NOT INITIAL.
      READ TABLE lt_cells INTO DATA(ls_cells) INDEX 1.
      CHECK sy-subrc = 0.

      READ TABLE <lt_data> ASSIGNING FIELD-SYMBOL(<ls_data>) INDEX ls_cells-row_id.
      IF sy-subrc <> 0.
        me->copy_crt_row( EXPORTING iv_row = ls_cells-row_id CHANGING cs_data = <ls_mod_rows> ).
        CONTINUE.
      ENDIF.

      ASSIGN COMPONENT 'DCFLG' OF STRUCTURE <ls_data> TO FIELD-SYMBOL(<lv_dcflg>).
      CHECK sy-subrc = 0.

      ASSIGN COMPONENT 'STATU' OF STRUCTURE <ls_data> TO FIELD-SYMBOL(<lv_statu>).
      CHECK sy-subrc = 0.

*--------------------------------
* Cell 별 Loop.
*--------------------------------
      LOOP AT lt_cells INTO ls_cells.

        av_error = COND #( WHEN ls_cells-error IS NOT INITIAL THEN abap_true ELSE abap_false ).
        CHECK av_error IS INITIAL.
        _g_set_msgtb:'D' <ls_data>  ls_cells-fieldname 'ZCN00' '000' '' '' '' .  "Msg 삭제

        CASE <lv_dcflg>.
          WHEN 'C'.  "Create는 Skip
            CONTINUE.
        ENDCASE.

*---------------------------
* History Table
*---------------------------
        IF av_hst_tabnm IS NOT INITIAL.
          me->crt_hist_field( EXPORTING is_cell = ls_cells ).
        ENDIF.

      ENDLOOP.

      CHECK <lv_dcflg> IS INITIAL.
*---------------------------------------
* Modify Cell..
*---------------------------------------
*-- Set Status..
      ASSIGN COMPONENT 'STATU' OF STRUCTURE <ls_data> TO <lv_statu>.
      CHECK sy-subrc = 0.

*-- Data Changed Flag
      <lv_dcflg> = 'U'.

      <lv_statu> = me->set_icon( 'U' ).

*-----------------------------
* Set Time Stamp..
*-----------------------------
      CHECK line_exists( lt_fcat[ KEY zfd COMPONENTS fieldname = zcn00_aenam ] ).

      zcl_cn_alv_grid=>set_timestamp( CHANGING  cs_data   = ls_timestamp ).


      ASSIGN COMPONENT 'AEDAT' OF STRUCTURE <ls_data> TO FIELD-SYMBOL(<lv_aedat>).
      CHECK sy-subrc = 0.
      <lv_aedat> = ls_timestamp-aedat.

      ASSIGN COMPONENT 'AENAM' OF STRUCTURE <ls_data> TO FIELD-SYMBOL(<lv_aenam>).
      CHECK sy-subrc = 0.
      <lv_aenam> = ls_timestamp-aenam.

      ASSIGN COMPONENT 'AEZET' OF STRUCTURE <ls_data> TO FIELD-SYMBOL(<lv_aezet>).
      CHECK sy-subrc = 0.
      <lv_aezet> = ls_timestamp-aezet.

    ENDLOOP.


  ENDMETHOD.


  METHOD set_change_mode.
*&---------------------------------------------------------------------*
*& Module              : CN
*& Program ID          : ZCL_CN_ALV_GRID
*& T-CODE              : N/A
*& Referenced Program  : N/A
*& Created by          : T0200335
*& Created On          : 2020.12.23
*& Type                : CLASS
*& Description         : ZCL_CN_ALV_GRID
*&---------------------------------------------------------------------*
*& Change History
*&---------------------------------------------------------------------*
*& 변경SEQ    변경자        변경일자             변경내용
*&---------------------------------------------------------------------*

    DATA:lv_queue.

*-----------------------------------------------
    "Lock 설정이 되었을 경우만 Lock CHeck
*-----------------------------------------------
    IF av_lock_nm IS NOT INITIAL.
      "Lock 여부
      CASE cv_mode.
        WHEN ''.  " Display -> Edit
          lv_queue = 'E'.
        WHEN 'X'. " Edit    -> Display
          lv_queue = 'D'.
      ENDCASE.

      CASE me->lock_common( iv_tcode   = CONV #( sy-cprog )
                            iv_jobtype = CONV #( av_lock_nm )
                            iv_queue   = lv_queue  ).
        WHEN '0'.
        WHEN OTHERS.
          cv_lock = 'X'.
      ENDCASE.
    ENDIF.

    CHECK cv_lock IS INITIAL and iv_dichg is INITIAL.
*-----------------------------------------------
*   Lo
*-----------------------------------------------
    CALL METHOD me->raise_event
      EXPORTING
        i_ucomm = 'CHAN'.

    cv_mode = me->get_edit_mode( ).

*---------------------------
* Set Status & Dcflag..
*---------------------------
    me->set_status( 'X' ).


    me->refresh_grid_display( ).

  ENDMETHOD.


  METHOD set_active_status.
*&---------------------------------------------------------------------*
*& Module              : CN
*& Program ID          : ZCL_CN_ALV_GRID
*& T-CODE              : N/A
*& Referenced Program  : N/A
*& Created by          : T0200335
*& Created On          : 2020.12.23
*& Type                : CLASS
*& Description         : ZCL_CN_ALV_GRID
*&---------------------------------------------------------------------*
*& Change History
*&---------------------------------------------------------------------*
*& 변경SEQ    변경자        변경일자             변경내용
*&---------------------------------------------------------------------*
    CHECK av_edit_mode = 'X'.

    av_status = 'X'.

  ENDMETHOD.


  METHOD reset_status.
*&---------------------------------------------------------------------*
*& Module              : CN
*& Program ID          : ZCL_CN_ALV_GRID
*& T-CODE              : N/A
*& Referenced Program  : N/A
*& Created by          : T0200335
*& Created On          : 2020.12.23
*& Type                : CLASS
*& Description         : ZCL_CN_ALV_GRID
*&---------------------------------------------------------------------*
*& Change History
*&---------------------------------------------------------------------*
*& 변경SEQ    변경자        변경일자             변경내용
*&---------------------------------------------------------------------*

    CHECK av_edit_mode = 'X'.  "화면이 Edit 일경우 Reset

*------------------
*-- Lock 해제
*------------------
    me->lock_common( iv_tcode   = CONV #( sy-cprog )
                     iv_jobtype = CONV #( av_lock_nm )
                     iv_queue   = 'D'  ).

    CLEAR:av_status, av_input, av_edit_mode, at_history.

    rv_mode = av_edit_mode.


  ENDMETHOD.


  METHOD refresh_grid_display.
*&---------------------------------------------------------------------*
*& Module              : CN
*& Program ID          : ZCL_CN_ALV_GRID
*& T-CODE              : N/A
*& Referenced Program  : N/A
*& Created by          : T0200335
*& Created On          : 2020.12.23
*& Type                : CLASS
*& Description         : ZCL_CN_ALV_GRID
*&---------------------------------------------------------------------*
*& Change History
*&---------------------------------------------------------------------*
*& 변경SEQ    변경자        변경일자             변경내용
*&---------------------------------------------------------------------*

    DATA:ls_stable       TYPE lvc_s_stbl,
         lv_soft_refresh TYPE char01.

    CHECK me IS BOUND.

    IF iv_delta = 'X' AND it_moce IS NOT INITIAL.
      DATA(LT_MOCE) = IT_MOCE.
      me->delta_refresh_grid( EXPORTING iv_modified    = iv_delta_modified
                              CHANGING  ct_delta_table = Lt_moce ).
      EXIT.
    ENDIF.

    ls_stable = VALUE #( row = 'X' col = 'X' ).

*-------------------------
* Set Field Catalog
*-------------------------
    DATA(lt_fcat) = at_fcat.

    me->on_set_fcat(  EXPORTING iv_name = me->m_name
                      CHANGING  ct_fcat = at_fcat ).

*-------------------------
* Set Line Style..
*-------------------------
    me->set_line_style( EXPORTING iv_name = me->m_name ).


    me->on_set_drop_down( EXPORTING iv_name = me->m_name
                          CHANGING  ct_drop = at_dropdown ).   "User Settint..

    me->set_drop_down_table( it_drop_down_alias = at_dropdown ).

    CHECK ao_dataref IS NOT INITIAL.

    IF lt_fcat <> at_fcat.
      CALL METHOD me->set_frontend_fieldcatalog
        EXPORTING
          it_fieldcatalog = at_fcat.
    ENDIF.

*-------------------------
* Set Refresh Data..
*-------------------------
    CALL METHOD me->refresh_table_display
      EXPORTING
        is_stable      = ls_stable        " With Stable Rows/Columns
        i_soft_refresh = iv_soft_refresh  " Without Sort, Filter, etc.
      EXCEPTIONS
        finished       = 1                " Display was Ended (by Export)
        OTHERS         = 2.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    me->set_ready_for_input( av_input ).

    IF as_layout-cwidth_opt IS NOT INITIAL.
      me->optimize_all_cols( ).
    ENDIF.

    me->on_set_layout(  EXPORTING iv_name  = m_name
                        CHANGING cs_layout = as_layout ).


  ENDMETHOD.


  METHOD pop_to_msg.
*&---------------------------------------------------------------------*
*& Module              : CN
*& Program ID          : ZCL_CN_ALV_GRID
*& T-CODE              : N/A
*& Referenced Program  : N/A
*& Created by          : T0200335
*& Created On          : 2020.12.23
*& Type                : CLASS
*& Description         : ZCL_CN_ALV_GRID
*&---------------------------------------------------------------------*
*& Change History
*&---------------------------------------------------------------------*
*& 변경SEQ    변경자        변경일자             변경내용
*&---------------------------------------------------------------------*
    DATA : lv_ans.

    CLEAR:rv_answer.

    CASE iv_title.
      WHEN ac_msg_title. "저장확인
        iv_title = TEXT-p01.
      WHEN ac_msg_title2."Exit Confirm
        iv_title = TEXT-p02.
      WHEN ac_msg_title3."삭제확인
        iv_title = TEXT-p03.
      WHEN ac_msg_title4."복구확인
        iv_title = TEXT-p04.
      WHEN ac_msg_title5."Lost Confirm   X
        iv_title = TEXT-p05.
    ENDCASE.

    CASE iv_text1.
      WHEN ac_msg_del1. "'선택된 데이타를 삭제하려고 합니다.'
        iv_text1 = TEXT-p06.
      WHEN ac_msg_del2. "'삭제하시겠습니까?'
        iv_text1 = TEXT-p07.
      WHEN ac_msg_exit. "'변경된 데이타가 있습니다.'
        iv_text1 = TEXT-p08.
      WHEN ac_msg_exit2."'화면에서 나가시겠습니까?'
        iv_text1 = TEXT-p09.
      WHEN ac_msg_msg1. "'계속 하시겠습니까?'
        iv_text1 = TEXT-p10.
      WHEN ac_msg_rec1. "'선택된 데이타를 복구하려고 합니다.'
        iv_text1 = TEXT-p11.
      WHEN ac_msg_rec2. "'복구하시겠습니까?'
        iv_text1 = TEXT-p12.
      WHEN ac_msg_save ."'저장하시겠습니까?'
        iv_text1 = TEXT-p13.
      WHEN ac_msg_upd1.
        iv_text1 = TEXT-p15.
    ENDCASE.

    CASE iv_text2.
      WHEN ac_msg_del1. "'선택된 데이타를 삭제하려고 합니다.'
        iv_text2 = TEXT-p06.
      WHEN ac_msg_del2. "'삭제하시겠습니까?'
        iv_text2 = TEXT-p07.
      WHEN ac_msg_exit. "'변경된 데이타가 있습니다.'
        iv_text2 = TEXT-p08.
      WHEN ac_msg_exit2."'화면에서 나가시겠습니까?'
        iv_text2 = TEXT-p09.
      WHEN ac_msg_msg1. "'계속 하시겠습니까?'
        iv_text2 = TEXT-p10.
      WHEN ac_msg_rec1. "'선택된 데이타를 복구하려고 합니다.'
        iv_text2 = TEXT-p11.
      WHEN ac_msg_rec2. "'복구하시겠습니까?'
        iv_text2 = TEXT-p12.
      WHEN ac_msg_save ."'저장하시겠습니까?'
        iv_text2 = TEXT-p13.
      WHEN ac_msg_upd2.
        iv_text2 = TEXT-p16.
    ENDCASE.


    CONCATENATE iv_text1 iv_text2 INTO DATA(lv_msg) SEPARATED BY space.

    CALL FUNCTION 'POPUP_TO_CONFIRM'
      EXPORTING
        titlebar              = iv_title
        text_question         = lv_msg
        text_button_1         = iv_text_button_1
        icon_button_1         = 'ICON_OKAY'
        text_button_2         = iv_text_button_2
        icon_button_2         = 'ICON_INCOMPLETE'
        default_button        = '2'
        display_cancel_button = iv_cancel_button
        popup_type            = 'ICON_MESSAGE_QUESTION'
      IMPORTING
        answer                = lv_ans
      EXCEPTIONS
        text_not_found        = 1
        OTHERS                = 2.

    CHECK sy-subrc EQ 0.

    CASE lv_ans.
      WHEN '1'.
*----------------
* Yes
*----------------
        rv_answer = 'X'.
      WHEN '2'.
*----------------
* No
*----------------
        rv_answer = ''.
      WHEN 'A'.
*----------------
* Cancel
*----------------
        rv_answer = 'C'.
    ENDCASE.

  ENDMETHOD.


  METHOD ON_SET_UPL_FCAT.
*&---------------------------------------------------------------------*
*& Module      : CN
*& 생성자      : T0200335
*& 생성일      :  2020.12.30
*& Description :
*&---------------------------------------------------------------------*
*& Change History
*&---------------------------------------------------------------------*
*& 변경SEQ    변경자        변경일자             변경내용
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*

*-----------------------------------------------------
* 개별 프로그램에서  PROTECTED SECTION 에서 Redfefine 사용
* 그리드가 여러개 일 경우 GRID 명으로 구분 (IV_NAME)
* Alv Consturctor시 Display 되어지는 Table을 기준으로 Field Catalog가 생성되어지므로
* Upload 시 각 필드별 속성을 변경 시키는 Method
*-----------------------------------------------------
*=======================================================
*  PROTECTED SECTION.
*    METHODS:
**---------------------------
** Event Area
**---------------------------
*  ON_SET_UPL_FCAT      REDEFINITION.

*&---------------------------------------------------------------------*
*& Class Implementation
*&---------------------------------------------------------------------*
*CLASS lcl_cust_alv_grid IMPLEMENTATION.

*  METHOD ON_SET_F4.
*    CASE iv_name.
*      WHEN 'ALV_GRID'.
**--------------------------
** Modify Field Catalog
** 저장전 중복체크를 위한 Table key 세팅을 만드시!!
**--------------------------
*        PERFORM alv_grid_fcat_modify CHANGING ct_fcat.
*      WHEN 'GRID_DTL'.
*        PERFORM alv_dtl_fcat_modify CHANGING ct_fcat.
*    ENDCASE.
*  ENDMETHOD.


  ENDMETHOD.


  METHOD on_set_sort.
*&---------------------------------------------------------------------*
*& Module      : CN
*& 생성자      : T0200335
*& 생성일      :  2020.12.30
*& Description :
*&---------------------------------------------------------------------*
*& Change History
*&---------------------------------------------------------------------*
*& 변경SEQ    변경자        변경일자             변경내용
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*

*----------------------------------------------------
* 개별 프로그램에서  PROTECTED SECTION 에서 Redfefine 사용
* 그리드가 여러개 일 경우 GRID 명으로 구분 (IV_NAME)
* 필드 별 Sort 설정
* 참조 프로그램:YALV/YALV2
*-----------------------------------------------------
*=======================================================
*  PROTECTED SECTION.
*    METHODS:
**---------------------------
** Event Area
**---------------------------
*  ON_SET_SORT    REDEFINITION.

*&---------------------------------------------------------------------*
*& Class Implementation
*&---------------------------------------------------------------------*
*CLASS lcl_cust_alv_grid IMPLEMENTATION.

*  METHOD ON_SET_SORT.
*
*  ENDMETHOD.
  ENDMETHOD.


  METHOD ON_SET_MSGTB.
*&---------------------------------------------------------------------*
*& Module      : CN
*& 생성자      : T0200335
*& 생성일      :  2020.12.30
*& Description :
*&---------------------------------------------------------------------*
*& Change History
*&---------------------------------------------------------------------*
*& 변경SEQ    변경자        변경일자             변경내용
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*

*-----------------------------------------------------
* 개별 프로그램에서  PROTECTED SECTION 에서 Redfefine 사용
* 그리드가 여러개 일 경우 GRID 명으로 구분 (M_NAME)
* Message Talbe Append시 라인스타일 조정가능
*-----------------------------------------------------
*=======================================================
*  PROTECTED SECTION.
*    METHODS:
**---------------------------
** Event Area
**---------------------------
*  ON_SET_MSGTB REDEFINITION.

*&---------------------------------------------------------------------*
*& Class Implementation
*&---------------------------------------------------------------------*
*CLASS lcl_cust_alv_grid IMPLEMENTATION.

*  METHOD ON_SET_MSGTB.
*
*  ENDMETHOD.

  ENDMETHOD.


  METHOD on_set_line_style_by_row.
*&---------------------------------------------------------------------*
*& Module      : CN
*& 생성자      : T0200335
*& 생성일      :  2020.12.30
*& Description :
*&---------------------------------------------------------------------*
*& Change History
*&---------------------------------------------------------------------*
*& 변경SEQ    변경자        변경일자             변경내용
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*

*-----------------------------------------------------
* 개별 프로그램에서  PROTECTED SECTION 에서 Redfefine 사용
* 그리드가 여러개 일 경우 GRID 명으로 구분 (M_NAME)
* 각 필드명 라인별로 라인 스타일 세팅
*-----------------------------------------------------
*=======================================================
*  PROTECTED SECTION.
*    METHODS:
**---------------------------
** Event Area
**---------------------------
*  ON_SET_LINE_STYLE_BY_ROW  REDEFINITION.

*&---------------------------------------------------------------------*
*& Class Implementation
*&---------------------------------------------------------------------*
*CLASS lcl_cust_alv_grid IMPLEMENTATION.

*  METHOD ON_SET_LINE_STYLE_BY_ROW.
*
*  ENDMETHOD.

  ENDMETHOD.


  METHOD on_set_line_style.
*&---------------------------------------------------------------------*
*& Module      : CN
*& 생성자      : T0200335
*& 생성일      :  2020.12.30
*& Description :
*&---------------------------------------------------------------------*
*& Change History
*&---------------------------------------------------------------------*
*& 변경SEQ    변경자        변경일자             변경내용
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*

*-----------------------------------------------------
* 개별 프로그램에서  PROTECTED SECTION 에서 Redfefine 사용
* 그리드가 여러개 일 경우 GRID 명으로 구분 (IV_NAME)
* 각 필드의 Style 및 Color를 지정하고 싶을 경우
* 참조 프로그램:YALV/YALV2
*-----------------------------------------------------
*=======================================================
*  PROTECTED SECTION.
*    METHODS:
**---------------------------
** Event Area
**---------------------------
*  ON_SET_LINE_STYLE    REDEFINITION.

*&---------------------------------------------------------------------*
*& Class Implementation
*&---------------------------------------------------------------------*
*CLASS lcl_cust_alv_grid IMPLEMENTATION.

*  METHOD ON_SET_LINE_STYLE.
*
*  ENDMETHOD.

  ENDMETHOD.


  METHOD on_set_layout.
*&---------------------------------------------------------------------*
*& Module      : CN
*& 생성자      : T0200335
*& 생성일      :  2020.12.30
*& Description :
*&---------------------------------------------------------------------*
*& Change History
*&---------------------------------------------------------------------*
*& 변경SEQ    변경자        변경일자             변경내용
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*

*-----------------------------------------------------
* 개별 프로그램에서  PROTECTED SECTION 에서 Redfefine 사용
* 그리드가 여러개 일 경우 GRID 명으로 구분 (IV_NAME)
* Alv Consturctor시 Layout이 Default로 설정되므로 Default 외 다른 Layout을 설정할 경우
* 참조 프로그램:YALV/YALV2
*-----------------------------------------------------
*=======================================================
*  PROTECTED SECTION.
*    METHODS:
**---------------------------
** Event Area
**---------------------------
*  ON_SET_LAYOUT     REDEFINITION.

*&---------------------------------------------------------------------*
*& Class Implementation
*&---------------------------------------------------------------------*
*CLASS lcl_cust_alv_grid IMPLEMENTATION.

*  METHOD ON_SET_LAYOUT.
*    CASE iv_name.
*      WHEN 'ALV_GRID'.
** cs_layout-xxx = 'xx'.
*      WHEN 'GRID_DTL'.
*    ENDCASE.
*  ENDMETHOD.


  ENDMETHOD.


  METHOD get_factor.
*&---------------------------------------------------------------------*
*& Module              : CN
*& Program ID          : ZCL_CN_ALV_GRID
*& T-CODE              : N/A
*& Referenced Program  : N/A
*& Created by          : T0200335
*& Created On          : 2020.12.23
*& Type                : CLASS
*& Description         : ZCL_CN_ALV_GRID
*&---------------------------------------------------------------------*
*& Change History
*&---------------------------------------------------------------------*
*& 변경SEQ    변경자        변경일자             변경내용
*&---------------------------------------------------------------------*

    DATA: lt_tcurr  TYPE tt_tcurr,
          lr_waers  TYPE rseloption,
          lv_factor TYPE isoc_factor.

    IF iv_waers IS NOT INITIAL.
      lr_waers = VALUE #( ( sign = 'I' option = 'EQ' low = iv_waers ) ).
    ENDIF.

*-------------------------
* Get Currency
*-------------------------
    SELECT DISTINCT waers
      INTO CORRESPONDING FIELDS OF TABLE lt_tcurr
      FROM tcurc
     WHERE waers IN lr_waers
     ORDER BY waers.

*-------------------------
* Set Factor..
*-------------------------
    LOOP AT lt_tcurr ASSIGNING FIELD-SYMBOL(<ls_tcurr>).
      CALL FUNCTION 'CURRENCY_CONVERTING_FACTOR'
        EXPORTING
          currency          = <ls_tcurr>-waers
        IMPORTING
          factor            = lv_factor
        EXCEPTIONS
          too_many_decimals = 1
          OTHERS            = 2.

      <ls_tcurr>-factor = lv_factor.
    ENDLOOP.

    DELETE lt_tcurr FROM 1 WHERE factor = 1.  "1인것은 삭제
    rt_tcurr = lt_tcurr.

  ENDMETHOD.


  METHOD DEL_EXCL_SMW0_DOWNLOAD.
*&---------------------------------------------------------------------*
*& Module              : CN
*& Program ID          : ZCL_CN_ALV_GRID
*& T-CODE              : N/A
*& Referenced Program  : N/A
*& Created by          : T0200335
*& Created On          : 2020.12.23
*& Type                : CLASS
*& Description         : CRT_DYN_TABLE
*&---------------------------------------------------------------------*
*& Change History
*&---------------------------------------------------------------------*
*& 변경SEQ    변경자        변경일자             변경내용
*&---------------------------------------------------------------------*
*    CONSTANTS :lc_path_excel(50) TYPE c  VALUE 'Excel (*.XLSX)'.
    DATA : ls_wwwdata_item TYPE wwwdatatab.

    DATA : lt_mime TYPE TABLE OF w3mime .

    DATA : lv_filename     TYPE string,
           lv_path         TYPE string,
           lv_up_path      TYPE string,
           lv_dn_path      TYPE string,
           lv_fname        TYPE string,
           lv_return       TYPE abap_bool,
           lv_fullpath     TYPE string,
           lv_filesize(10),
           lv_size         TYPE i,
           lt_content_bin  TYPE bbpt_att_cont,
           lv_rc           TYPE i.

    DATA : lo_excel    TYPE ole2_object,
           lo_books    TYPE ole2_object,
           lo_workbook TYPE ole2_object,
           lo_sheets   TYPE ole2_object,
           lo_sheet    TYPE ole2_object.

    DATA:lv_dest TYPE localfile.

    lv_fname = COND #( WHEN iv_filename IS NOT INITIAL THEN iv_filename
                       ELSE iv_objid ).

    DATA(lv_gui) = zcl_cn_abap_util=>check_gui_info( ) .

*---------------------------------------------
* Select WWW 오브젝트 저장에 대한 INDX-유형
*---------------------------------------------
    SELECT SINGLE relid,
                  objid,
                  checkout,
                  checknew,
                  chname,
                  tdate,
                  ttime,
                  text
      FROM  wwwdata
      INTO CORRESPONDING FIELDS OF @ls_wwwdata_item
     WHERE objid = @iv_objid.   "<-- SMW0 Object 명

    IF sy-subrc NE 0.
      MESSAGE s011(zcn00) DISPLAY LIKE 'E' WITH TEXT-e08.  "011 &1 데이타가 없습니다.
      EXIT.
    ENDIF.

*-------------------------------
* Get Import Web Objects
*-------------------------------
    CALL FUNCTION 'WWWDATA_IMPORT'
      EXPORTING
        key               = ls_wwwdata_item
      TABLES
        mime              = lt_mime
      EXCEPTIONS
        wrong_object_type = 1
        import_error      = 2
        OTHERS            = 3.
*-------------------------------
* File Read
*-------------------------------
    CALL FUNCTION 'WWWPARAMS_READ'
      EXPORTING
        relid = ls_wwwdata_item-relid
        objid = ls_wwwdata_item-objid
        name  = 'filesize'
      IMPORTING
        value = lv_filesize.

*-------------------------------
* File Open Dialog
*-------------------------------
    TRY.
        CALL METHOD cl_gui_frontend_services=>get_upload_download_path
          CHANGING
            upload_path                 = lv_up_path
            download_path               = lv_dn_path
          EXCEPTIONS
            cntl_error                  = 1
            error_no_gui                = 2
            not_supported_by_gui        = 3
            gui_upload_download_path    = 4
            upload_download_path_failed = 5
            OTHERS                      = 6.

        CONCATENATE lv_fname '.XLSX' INTO   lv_fname.

        CALL METHOD cl_gui_frontend_services=>file_save_dialog
          EXPORTING
            window_title      = 'Download'
            default_extension = 'XLSX'
            default_file_name = lv_fname
            initial_directory = lv_dn_path
            file_filter       = CONV #( ac_path_excel )
          CHANGING
            filename          = lv_filename
            path              = lv_path
            fullpath          = lv_fullpath.

      CATCH cx_root INTO DATA(ls_root).

    ENDTRY.
    CHECK lv_filename IS NOT INITIAL.

    CASE lv_gui.
      WHEN 'X'.   "Sapgui


*-------------------------------
* Check File Exists
*-------------------------------
        CALL METHOD cl_gui_frontend_services=>file_exist
          EXPORTING
            file                 = lv_filename
          RECEIVING
            result               = lv_return
          EXCEPTIONS
            cntl_error           = 1
            error_no_gui         = 2
            wrong_parameter      = 3
            not_supported_by_gui = 4
            OTHERS               = 5.

        IF lv_return EQ abap_true.
*-------------------------------
* Delete File
*-------------------------------
          CLEAR : lv_return .
          CALL METHOD cl_gui_frontend_services=>file_delete
            EXPORTING
              filename             = lv_filename
            CHANGING
              rc                   = lv_rc
            EXCEPTIONS
              file_delete_failed   = 1
              cntl_error           = 2
              error_no_gui         = 3
              file_not_found       = 4
              access_denied        = 5
              unknown_error        = 6
              not_supported_by_gui = 7
              wrong_parameter      = 8
              OTHERS               = 9.
          IF lv_rc NE 0 .
            MESSAGE s000(zcn00) WITH TEXT-e09  DISPLAY LIKE 'E'.   "파일이 열려 있습니다.
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
    lv_size = lv_filesize.
    CALL FUNCTION 'GUI_DOWNLOAD'
      EXPORTING
        filename     = lv_filename
        filetype     = 'BIN'
        bin_filesize = lv_size
      TABLES
        data_tab     = lt_mime.

    CHECK lv_gui = 'X'.
*---------------------------------
* Excel 실행
*---------------------------------
    CALL METHOD cl_gui_frontend_services=>execute
      EXPORTING
        document               = lv_filename
      EXCEPTIONS
        cntl_error             = 1
        error_no_gui           = 2
        bad_parameter          = 3
        file_not_found         = 4
        path_not_found         = 5
        file_extension_unknown = 6
        error_execute_failed   = 7
        synchronous_failed     = 8
        not_supported_by_gui   = 9
        OTHERS                 = 10.
    IF sy-subrc IS NOT INITIAL.
      MESSAGE e162(alvht).
      EXIT.
    ENDIF.

  ENDMETHOD.


METHOD delta_refresh_grid.

  CHECK ct_delta_table IS NOT INITIAL.

  CALL METHOD me->set_delta_table
    EXPORTING
      modified    = iv_modified
    CHANGING
      delta_table = ct_delta_table
    EXCEPTIONS
      error       = 1
      OTHERS      = 2.

*  CALL METHOD me->check_changed_data.

ENDMETHOD.


  METHOD delete_data_row.

*&---------------------------------------------------------------------*
*& Module      : CN
*& 생성자: .T0200335
*& 생성일:  2020.12.30
*& Description :
*&---------------------------------------------------------------------*
*& Change History
*&---------------------------------------------------------------------*
*& 변경SEQ    변경자        변경일자             변경내용
*&---------------------------------------------------------------------*
    DATA:lo_data    TYPE REF TO data,
         ls_history TYPE ts_history.

    FIELD-SYMBOLS:<lt_data> TYPE table.

    ASSIGN ao_dataref->* TO <lt_data>.


    CHECK sy-subrc = 0.

*----------------------------------
* 삭제수행
*----------------------------------
    LOOP AT it_rows INTO DATA(ls_rows).

      READ TABLE <lt_data> ASSIGNING FIELD-SYMBOL(<ls_data>) INDEX ls_rows-index.

      CHECK sy-subrc EQ 0.
*---------------------------------
* Set DCFLAG/Status..
*---------------------------------
      ASSIGN COMPONENT 'DCFLG' OF STRUCTURE <ls_data> TO FIELD-SYMBOL(<lv_dcflg>).
      CHECK sy-subrc = 0.

      ASSIGN COMPONENT 'STATU' OF STRUCTURE <ls_data> TO FIELD-SYMBOL(<lv_statu>).
      CHECK sy-subrc = 0.

      ASSIGN COMPONENT 'ZDELE' OF STRUCTURE <ls_data> TO FIELD-SYMBOL(<lv_del>).
      CHECK sy-subrc = 0 AND <lv_del> IS INITIAL.

      CASE <lv_dcflg>.
        WHEN 'C'.
          "신규일 경우는 바로 삭제
          <lv_dcflg> = 'X'.
        WHEN OTHERS.
          "기존일경우는 라인 삭제 이미지
          <lv_dcflg> = 'T'.
          <lv_statu> = me->set_icon( 'T' ). "Line 삭제 표시
          <lv_del>   = 'X'.
          av_status  = 'X'.                                 "Changed Status..
      ENDCASE.


*--------------------------------------
* On Set Delete Row Value
*--------------------------------------
      me->on_set_del_row_value( EXPORTING iv_name = me->m_name
                                CHANGING cs_data  = <ls_data> ).

      CHECK <lv_dcflg> = 'T' .

      zcl_cn_alv_grid=>set_timestamp( CHANGING cs_data = <ls_data> ).

      CHECK  av_hst_tabnm IS NOT INITIAL.  "History Table

*---------------------------------
* Set History Field..
*---------------------------------
      ls_history-hfldnm    = TEXT-m04.
      ls_history-aedat     = sy-datum.
      ls_history-aezet     = sy-uzeit.
      ls_history-hval_b    = '*'.
      ls_history-hval_a    = '*'.
      ls_history-hchg_type = 'D'.

      me->set_hist_datakey( EXPORTING is_data  = <ls_data>
                            IMPORTING ev_key   = ls_history-dkey
                                      ev_keynm = ls_history-hkeynm ).
*---------------------------------
*  Append history
*---------------------------------
      APPEND ls_history TO at_history.
    ENDLOOP.

*-----------------------------------------
* 신규는 생성전이므로 삭제
*-----------------------------------------
    LOOP AT <lt_data> ASSIGNING <ls_data>.
      DATA(lv_index) = sy-tabix.
      ASSIGN COMPONENT 'DCFLG' OF STRUCTURE <ls_data> TO <lv_dcflg>.
      CHECK sy-subrc = 0 AND <lv_dcflg> = 'X'.
      DELETE <lt_data> INDEX lv_index.

    ENDLOOP.


  ENDMETHOD.


  METHOD crt_hist_field.
*&---------------------------------------------------------------------*
*& Module              : CN
*& Program ID          : ZCL_CN_ALV_GRID
*& T-CODE              : N/A
*& Referenced Program  : N/A
*& Created by          : T0200335
*& Created On          : 2020.12.23
*& Type                : CLASS
*& Description         : CRT_DYN_TABLE
*&---------------------------------------------------------------------*
*& Change History
*&---------------------------------------------------------------------*
*& 변경SEQ    변경자        변경일자             변경내용
*&---------------------------------------------------------------------*

    DATA:ls_history LIKE LINE OF at_history,
         lo_data    TYPE REF TO data.

    FIELD-SYMBOLS : <lt_data>     TYPE table.

*----------------------------
* Alv Befor Data..
*----------------------------
    ASSIGN ao_dataref->* TO <lt_data>.
    CREATE DATA lo_data LIKE LINE OF <lt_data>.

    READ TABLE <lt_data> ASSIGNING FIELD-SYMBOL(<ls_data>) INDEX is_cell-row_id.
    CHECK sy-subrc = 0.

    ASSIGN COMPONENT is_cell-fieldname OF STRUCTURE <ls_data> TO FIELD-SYMBOL(<lv_val>).
    CHECK sy-subrc = 0.

*---------------------------------
* Set History Field..
*---------------------------------
    ls_history-hfldnm    = is_cell-fieldname.
    ls_history-aedat     = sy-datum.
    ls_history-aezet     = sy-uzeit.
    ls_history-hval_b    = <lv_val>.
    ls_history-hval_a    = is_cell-value.
    ls_history-dkey      = space.
    ls_history-hchg_type = 'C' .

*---------------------------------
*  Conv Value..
*---------------------------------
    DATA(lt_fcat) = CORRESPONDING ty_fcat( at_fcat ).

    DATA(ls_fcat) = VALUE #( lt_fcat[ KEY zfd COMPONENTS fieldname = is_cell-fieldname ] OPTIONAL ).

    me->conv_value( EXPORTING  is_fcat     = ls_fcat
                               iv_type   = 'X'
                               iv_value   = ls_history-hval_b
                    CHANGING   cv_val  = ls_history-hval_b ).

    me->conv_value( EXPORTING  is_fcat     = ls_fcat
                               iv_type   = 'X'
                               iv_value   = ls_history-hval_a
                    CHANGING   cv_val  = ls_history-hval_a ).

*---------------------------------
*  Set Data Key..
*---------------------------------
    me->set_hist_datakey( EXPORTING is_data  = <ls_data>
                          IMPORTING ev_key   = ls_history-dkey
                                    ev_keynm = ls_history-hkeynm ).

*---------------------------------
*  Append history
*---------------------------------
    APPEND ls_history TO at_history.

  ENDMETHOD.


  METHOD crt_dyn_table.
*&---------------------------------------------------------------------*
*& Module              : CN
*& Program ID          : ZCL_CN_ALV_GRID
*& T-CODE              : N/A
*& Referenced Program  : N/A
*& Created by          : T0200335
*& Created On          : 2020.12.23
*& Type                : CLASS
*& Description         : CRT_DYN_TABLE
*&---------------------------------------------------------------------*
*& Change History
*&---------------------------------------------------------------------*
*& 변경SEQ    변경자        변경일자             변경내용
*&---------------------------------------------------------------------*

*--------------------------------------------------------------------------------------------
* Create Dynmic Table
* Dynamic Table 생성시 REF_TABLE/Ref_field 를 되도록 입력 하거나 Rollname (둘중에 하나는 필수)
* IV_ALV_COMM = 'X' 일시 자동으로 생성 MSGTB CELLC CELLS 는  ZSCN00003를 Ref_table Ref_field로 사용
*
* 사용법
*    ASSIGN eo_dynt->* TO <lt_tab>  "으로 로컬에서 사용
*
*--------------------------------------------------------------------------------------------
    DEFINE _l_append_fcat.
      READ TABLE it_fcat WITH KEY fieldname = &1 TRANSPORTING NO FIELDS.
      IF sy-subrc <> 0.
      CLEAR:ls_fcat.
      ls_fcat-fieldname = &1.
      ls_fcat-ref_table = &2.
      ls_fcat-ref_field = &3.
      ls_fcat-no_out    = &4.
      ls_fcat-col_pos   = &5.

      APPEND ls_fcat TO lt_fcat.
      ENDIF.
    end-OF-DEFINITION.

    DATA:lo_data TYPE REF TO data.

    DATA : ls_comp       TYPE         abap_componentdescr,
           lt_comp       TYPE         abap_component_tab,
           lrf_sdescr    TYPE REF TO  cl_abap_structdescr,
           lt_tkey       TYPE         abap_keydescr_tab,
           lrf_elemdescr TYPE REF TO  cl_abap_elemdescr,
           lrf_tdescr    TYPE REF TO  cl_abap_tabledescr.

    DATA : ls_fcat  TYPE lvc_s_fcat,
           lv_msg   TYPE string.

    DATA : lv_len       TYPE i,
           lv_dec       TYPE i,
           lv_comptype  TYPE comptype,
           lv_rollname  TYPE lvc_roll,
           ls_comm      TYPE zscn00003,
           lv_ref_field TYPE lvc_rfname,
           lv_typekind  TYPE abap_typekind.

    DATA(lt_fcat) = it_fcat.

*-----------------------------------------------------------------
* ALV 공통 (DCFLG/MSGTB/CELLC/CELLS)
*---------------------------------------------------------------
    IF iv_alv_comm = 'X'.
      CLEAR:ls_comm.
      _l_append_fcat:'DCFLG' 'ZSCN00003'      'DCFLG'   'X'   999,
                     'STATU' 'ZSCN00003'      'STATU'   'X'   999,
                     'CELLC' 'ZSCN00003'      'CELLC'   'X'   999,
                     'CELLS' 'ZSCN00003'      'CELLS'   'X'   999,
                     'ZDELE' 'ZSCN_TIMESTAMP' 'ZDELE'   'X'   999,
                     'MSGTB' 'ZSCN00003'      'MSGTB'   'X'   999.
    ENDIF.

    et_fcat = lt_fcat.

*-----------------------------------------------------------------
* Simple Create Dynamic Table.. (호출 제한 (Dump 발생 有)
*-----------------------------------------------------------------
    IF iv_easy = 'X'.
      CALL METHOD cl_alv_table_create=>create_dynamic_table
        EXPORTING
          it_fieldcatalog = lt_fcat
        IMPORTING
          ep_table        = lo_data.

      CHECK lo_data IS BOUND.
      eo_dynt = lo_data.
      EXIT.
    ENDIF.

*---------------------------------------------------------------
* Get Table Field Info..
*---------------------------------------------------------------
    DATA(lt_temp) = lt_fcat.
    SORT lt_temp BY ref_table.
    DELETE ADJACENT DUPLICATES FROM lt_temp COMPARING ref_table.
    DELETE lt_temp WHERE ref_table IS INITIAL.

    IF lt_temp IS NOT INITIAL.
      SELECT fieldname, rollname, comptype
        INTO TABLE @DATA(lt_dd03l)
      FROM dd03l
        FOR ALL ENTRIES IN @lt_temp
      WHERE tabname     =  @lt_temp-ref_table.
      SORT lt_dd03l BY fieldname.
      FREE:lt_temp.
    ENDIF.

*---------------------------------------------------------------
* Get Data Element Info..
*---------------------------------------------------------------
    lt_temp = lt_fcat.
    SORT lt_temp BY rollname.
    DELETE ADJACENT DUPLICATES FROM lt_temp COMPARING rollname.
    DELETE lt_temp WHERE rollname IS INITIAL.

    IF lt_temp IS NOT INITIAL.
      SELECT rollname, refkind
      INTO TABLE @DATA(lt_dd04l)
      FROM dd04l
      FOR ALL ENTRIES IN @lt_temp
      WHERE rollname  EQ @lt_temp-rollname.
      SORT lt_dd04l BY rollname.
      FREE:lt_temp.
    ENDIF.

*---------------------------------------------------------------
* Set Field Catalog
*---------------------------------------------------------------
    lt_tkey = it_tkey.

    LOOP AT lt_fcat INTO ls_fcat.

      CLEAR:ls_comp,lv_len, lv_dec,lv_typekind.

      ls_comp-name = ls_fcat-fieldname.

      IF ls_fcat-ref_field IS INITIAL.
        lv_ref_field = ls_fcat-fieldname.
      ELSE.
        lv_ref_field = ls_fcat-ref_field.
      ENDIF.

      READ TABLE lt_dd03l INTO DATA(ls_dd03l) WITH KEY fieldname = lv_ref_field
                                              BINARY SEARCH.
      IF sy-subrc NE 0.
        READ TABLE lt_dd04l INTO DATA(ls_dd04l) WITH KEY rollname = ls_fcat-rollname
                                                BINARY SEARCH.
      ENDIF.

      CLEAR:lv_rollname, lv_comptype.
      lv_rollname = COND #( WHEN ls_dd03l-rollname IS NOT INITIAL THEN ls_dd03l-rollname
                            ELSE ls_dd04l-rollname ).
      lv_comptype = COND #( WHEN ls_dd03l-comptype IS NOT INITIAL THEN ls_dd03l-comptype
                            ELSE ls_dd04l-refkind ).

      CLEAR: ls_dd03l, ls_dd04l.
      IF lv_rollname IS NOT INITIAL.
        CASE lv_comptype.
          WHEN 'L' OR 'S'.
            ls_comp-type   ?= cl_abap_elemdescr=>describe_by_name( lv_rollname ).
            APPEND ls_comp TO lt_comp.
            CONTINUE.
          WHEN OTHERS.
            lrf_elemdescr  ?= cl_abap_elemdescr=>describe_by_name( lv_rollname ).
            ls_comp-type = lrf_elemdescr.
            APPEND ls_comp TO lt_comp.
            CONTINUE.
        ENDCASE.
      ELSE.
        CASE ls_fcat-inttype.
          WHEN 'N' OR 'C'.
            IF ls_fcat-intlen NE 0.
              lv_len      = ls_fcat-intlen.
            ELSE.
              lv_len      = ls_fcat-outputlen.
            ENDIF.
          WHEN OTHERS.
            lv_len      = ls_fcat-intlen / 2.
        ENDCASE.
        lv_typekind     = ls_fcat-inttype.
        lv_dec          = ls_fcat-decimals.
      ENDIF.

      TRY.
          CASE lv_typekind.
            WHEN 'I'.
              ls_comp-type = cl_abap_elemdescr=>get_i( ).
            WHEN 'F'.
              ls_comp-type = cl_abap_elemdescr=>get_f( ).
            WHEN 'XSTRING'.
              ls_comp-type = cl_abap_elemdescr=>get_xstring( ).
            WHEN 'C' OR 'X' .
              ls_comp-type = cl_abap_elemdescr=>get_c( p_length   = lv_len ).
            WHEN 'D'.
              ls_comp-type = cl_abap_elemdescr=>get_d( ).
            WHEN 'T'.
              ls_comp-type = cl_abap_elemdescr=>get_t( ).
            WHEN 'g'.
              ls_comp-type = cl_abap_elemdescr=>get_string( ).
            WHEN 'N'.
              ls_comp-type = cl_abap_elemdescr=>get_n( p_length   = lv_len ).
            WHEN 'P'.
              ls_comp-type = cl_abap_elemdescr=>get_p( p_length   = lv_len
                                                       p_decimals = lv_dec ).
            WHEN 'b' OR 's'.
              ls_comp-type = cl_abap_elemdescr=>get_p( p_length   = lv_len
                                                       p_decimals = lv_dec ).
            WHEN OTHERS.
              DELETE lt_tkey FROM 1 WHERE name = ls_fcat-fieldname.
          ENDCASE.
        CATCH cx_root INTO data(lrf_root).
          lv_msg = lrf_root->get_text( ).
          CONCATENATE ls_fcat-fieldname '-' lv_msg INTO lv_msg.
          CONTINUE.
      ENDTRY.

*------------------------------------------
*   Append Data
*------------------------------------------
      IF ls_comp-type IS NOT INITIAL.
        ls_comp-name = ls_fcat-fieldname.
        APPEND ls_comp TO lt_comp.
      ENDIF.
    ENDLOOP.


    CHECK lt_comp IS NOT INITIAL.
*============================================================================
* Create Dyanmic Table..
*============================================================================

*--------------------------------
*-- Create a New Type
*--------------------------------
    lrf_sdescr = cl_abap_structdescr=>create( lt_comp ).

*--------------------------------
*-- New Table type
*--------------------------------
    CASE iv_table_kind.
      WHEN cl_abap_tabledescr=>tablekind_sorted OR cl_abap_tabledescr=>tablekind_hashed.
        IF lines( lt_comp ) LT lines( lt_tkey ).
          RETURN.
        ENDIF.
        lrf_tdescr = cl_abap_tabledescr=>create( p_line_type  = lrf_sdescr
                                                 p_table_kind = iv_table_kind
                                                 p_key        = lt_tkey
                                                 p_unique     = iv_unique ).
      WHEN OTHERS.
        lrf_tdescr = cl_abap_tabledescr=>create( p_line_type  = lrf_sdescr
                                                 p_table_kind = cl_abap_tabledescr=>tablekind_std
                                                 p_unique     = abap_false ).
    ENDCASE.

    CREATE DATA lo_data TYPE HANDLE lrf_tdescr.

    erf_ref = lrf_tdescr.
    et_comp = lt_comp.
    eo_dynt = lo_data.

  ENDMETHOD.


  METHOD correct_format_conversion.

    DATA : lv_field_type      TYPE c,
           lv_decimals        TYPE i,
           lv_decimals_target TYPE i,
           lv_strlen          TYPE i,
           lv_last_decimal    TYPE i,
           lv_hlpvz           TYPE i.

    FIELD-SYMBOLS : <lv_type_x> TYPE text4096.

    CONSTANTS : lc_ptext(13)       TYPE c VALUE '1234567890 -+',
                lc_darl_number(12) TYPE c VALUE '1234567890 ',
                lc_minus(1)        TYPE c VALUE '-',
                lc_null(8)         TYPE c VALUE '00:00:00'.

    DEFINE _perpare_number.
      CLEAR sy-subrc.
      WHILE sy-subrc = 0.
        IF &1 = '.'.
          IF iv_source CA &1.
            IF sy-fdpos > lv_last_decimal.
              lv_last_decimal = sy-fdpos + 1.
            ENDIF.
          ENDIF.
        ENDIF.
        REPLACE &1 WITH space INTO iv_source.
      ENDWHILE.
    END-OF-DEFINITION.


    DEFINE _return.
      IF sy-subrc IS NOT INITIAL.
        rs_return = VALUE #( type       = &1
                             id         = &2
                             number     = &3
                             message_v1 = &4
                             message_v2 = &5
                             message_v3 = &6
                             message_v4 = &7 ).
      ENDIF.
    END-OF-DEFINITION.

    CHECK iv_source IS NOT INITIAL.

    IF is_desc IS INITIAL.
      DESCRIBE FIELD ev_target TYPE lv_field_type.
    ELSE.
      lv_field_type = is_desc-inttype.
    ENDIF.

    TRY.
        CASE lv_field_type.

*--------------------------------------------------------------------*
          WHEN 'C'. "Char. type
*--------------------------------------------------------------------*

            ev_target = iv_source.
            IF is_desc-lowercase EQ space.
              ev_target = |{ ev_target CASE = UPPER }|.
            ENDIF.

*--------------------------------------------------------------------*
          WHEN 'D'. "Date type
*--------------------------------------------------------------------*

            REPLACE ALL OCCURRENCES OF '.' IN iv_source WITH space.
            REPLACE ALL OCCURRENCES OF '/' IN iv_source WITH space.
            REPLACE ALL OCCURRENCES OF '-' IN iv_source WITH space.
            CALL FUNCTION 'CONVERT_DATE_TO_INTERNAL'
              EXPORTING
                date_external       = iv_source
                accept_initial_date = abap_true
              IMPORTING
                date_internal       = ev_target
              EXCEPTIONS
                error_message       = 4
                OTHERS              = 4.
            _return sy-msgty sy-msgid sy-msgno sy-msgv1 sy-msgv2 sy-msgv3
            sy-msgv4.

*--------------------------------------------------------------------*
          WHEN 'T'. "Time type
*--------------------------------------------------------------------*

            IF NOT iv_source IS INITIAL AND iv_source <> lc_null.
              CALL FUNCTION 'CONVERT_TIME_INPUT'
                EXPORTING
                  input         = iv_source
                IMPORTING
                  output        = ev_target
                EXCEPTIONS
                  error_message = 4
                  OTHERS        = 4.
              _return sy-msgty sy-msgid sy-msgno sy-msgv1 sy-msgv2 sy-msgv3
              sy-msgv4.
            ENDIF.

*--------------------------------------------------------------------*
          WHEN 'X'. "Type X
*--------------------------------------------------------------------*

            ASSIGN iv_source TO <lv_type_x>.
            IF sy-subrc <> 0.
              _return 'E' 'ZCO_01' '0' 'Numeric Type Error' '' '' ''.
            ENDIF.
            ev_target = <lv_type_x>.

*--------------------------------------------------------------------*
          WHEN 'N'. "Numeric Type
*--------------------------------------------------------------------*

            REPLACE ALL OCCURRENCES OF '.' IN iv_source WITH space.
            CONDENSE iv_source NO-GAPS.
            IF iv_source CN lc_darl_number.
              _return 'E' 'ZCO_01' '0' 'Numeric Type Error' '' '' ''.
            ELSE.
              ev_target = iv_source. " * ( 10 ** ( LV_DECIMALS_TARGET ) ).
            ENDIF.

*--------------------------------------------------------------------*
          WHEN 'I'. "Integer Type
*--------------------------------------------------------------------*

            DESCRIBE FIELD ev_target DECIMALS lv_decimals_target.

            lv_strlen = strlen( iv_source ).
            _perpare_number : '.', ',', ';', '/'.
            IF NOT lv_last_decimal IS INITIAL.
              lv_hlpvz = lv_strlen - 1.
              IF iv_source+lv_hlpvz(1) = lc_minus.
                lv_decimals = lv_strlen - lv_last_decimal - 1.
              ELSE.
                lv_decimals = lv_strlen - lv_last_decimal.
              ENDIF.
            ENDIF.
            CONDENSE iv_source NO-GAPS.

            IF strlen( iv_source ) > is_desc-leng.
              _return 'E' 'ZCO_01' '000' 'Field length overflow' '' '' ''.
            ELSEIF iv_source CN lc_darl_number.
              _return 'E' 'ZCO_01' '0' 'Integer Type Error' '' '' ''.
            ELSE.
              lv_decimals_target = lv_decimals_target - lv_decimals.
              ev_target = iv_source * ( 10 ** ( lv_decimals_target ) ).
            ENDIF.

*--------------------------------------------------------------------*
          WHEN 'P'. "Amount/Qty Type
*--------------------------------------------------------------------*

            IF iv_source(1) = lc_minus.
              DATA lv_minus TYPE xfeld.
              lv_minus = abap_true.
              SHIFT iv_source BY 1 PLACES LEFT.
            ENDIF.

            IF iv_source EQ '-'.
              CLEAR iv_source.
            ENDIF.

            DESCRIBE FIELD ev_target DECIMALS lv_decimals_target.

            lv_strlen = strlen( iv_source ).
            _perpare_number : '.', ',', ';', '/'.
            IF NOT lv_last_decimal IS INITIAL.
              lv_hlpvz = lv_strlen - 1.
              IF iv_source+lv_hlpvz(1) = lc_minus.
                lv_decimals = lv_strlen - lv_last_decimal - 1.
              ELSE.
                lv_decimals = lv_strlen - lv_last_decimal.
              ENDIF.
            ENDIF.

            CONDENSE iv_source NO-GAPS.
            "Check overflow
            IF strlen( iv_source ) > is_desc-leng.
              _return 'E' 'ZCO_O1' '000' 'Field length overflow' '' '' ''.
              RETURN.
            ENDIF.

            DESCRIBE FIELD ev_target LENGTH DATA(lv_len_target) IN BYTE MODE.
            lv_len_target = ( lv_len_target * 2 ) - 1.
            DATA(lv_len_source) = strlen( iv_source ).
            IF lv_len_source > lv_len_target.
              lv_decimals_target =
              lv_decimals_target - lv_len_target + lv_len_source.
              iv_source = iv_source(lv_len_target).
            ENDIF.
            IF iv_source CN lc_ptext.
              CASE is_desc-datatype.
                WHEN 'CURR'. _return 'E' 'ZCO_01' '0' 'Currency Type Error' '' '' ''.
                WHEN 'QUAN'. _return 'E' 'ZCO_01' '0' 'Quantity Type Error' '' '' ''.
                WHEN OTHERS. _return 'E' 'ZCO_01' '0' 'Only numbers can be entered.' '' '' ''.
              ENDCASE.
            ELSE.
              ev_target = iv_source / ( 10 ** ( lv_decimals_target ) ).

              lv_decimals_target = lv_decimals_target - lv_decimals.
              ev_target = ev_target * ( 10 ** ( lv_decimals_target ) ).
            ENDIF.

            IF NOT lv_minus IS INITIAL.
              ev_target = ev_target * -1.
            ENDIF.

*--------------------------------------------------------------------*
          WHEN 'F'. "Flip Type
*--------------------------------------------------------------------*

            CALL FUNCTION 'CHAR_FLTP_CONVERSION'
              EXPORTING
                string = iv_source
              IMPORTING
                flstr  = ev_target
              EXCEPTIONS
                OTHERS = 4.
            _return sy-msgty sy-msgid sy-msgno
                    sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
          WHEN OTHERS. "Etc
            ev_target = iv_source.

        ENDCASE.

        IF rs_return IS INITIAL AND strlen( iv_source ) > is_desc-leng.
*            WRITE 'ERROR'.
*            _RETURN 'E' 'ZCO_01' '000' 'Field length overflow' '' '' ''.
        ENDIF.

*--Dynamic Conversion Exit
*        IF IS_DESC-CONVEXIT IS NOT INITIAL AND RS_RETURN IS INITIAL.
*          zcl_cn_alv_grid=>CONVERSION_EXIT_VALUE( EXPORTING IV_INPUT        = EV_TARGET
*                                                        IV_CONVEXIT     = IS_DESC-CONVEXIT
*                                                        IV_CONVERT_TYPE = ykh_I
*                                              IMPORTING EV_OUTPUT       = EV_TARGET ).
*        ENDIF.

      CATCH cx_root INTO DATA(lo_root).
        DATA(ls_return) = zcl_cn_alv_grid=>message_handling( lo_root->get_text( ) ).
        rs_return = VALUE #( BASE rs_return message_v1 = ls_return-message_v1
                                            message_v2 = ls_return-message_v2
                                            message_v3 = ls_return-message_v3
                                            message_v4 = ls_return-message_v4 ).
    ENDTRY.

  ENDMETHOD.


  METHOD copy_crt_row.
*&---------------------------------------------------------------------*
*& Module              : CN
*& Program ID          : ZCL_CN_ALV_GRID
*& T-CODE              : N/A
*& Referenced Program  : N/A
*& Created by          : T0200335
*& Created On          : 2020.12.23
*& Type                : CLASS
*& Description         : CRT_DYN_TABLE
*&---------------------------------------------------------------------*
*& Change History
*&---------------------------------------------------------------------*
*& 변경SEQ    변경자        변경일자             변경내용
*&---------------------------------------------------------------------**-------------------------------------------------------------------------------------------------------
* Line Insert Method
* 일반적으로 자동으로 호출 되어져 라인추가 생성
* 단, 개별에서 호출될 경우도 있음 (Parent & child 일 경우 Default Field가 달라지므로 개별에서 호출 가능)
* 참조프로그램:YALV2
*-------------------------------------------------------------------------------------------------------
    DATA:lo_data      TYPE REF TO data,
         lv_row       TYPE i,
         lt_dftvl     TYPE tt_field,
         ls_timestamp TYPE zscn_timestamp.

    DATA: ls_row_id TYPE lvc_s_row,
          ls_col_id TYPE lvc_s_col,
          ls_row_no TYPE lvc_s_roid.

    FIELD-SYMBOLS:<lt_data> TYPE table.

*    CHECK sy-uname = 'T0200335' OR sy-uname = 'T0210035'.

    ASSIGN ao_dataref->* TO <lt_data>.

    CREATE DATA lo_data LIKE LINE OF <lt_data>.
    ASSIGN lo_data->* TO FIELD-SYMBOL(<ls_data>).
    CHECK sy-subrc = 0.
*
    lt_dftvl = at_dftvl .

    READ TABLE <lt_data> INDEX iv_row ASSIGNING <ls_data>.
    CHECK sy-subrc <> 0.

*---------------------------------
* Set DCFLAG/Status..
*---------------------------------
    ASSIGN COMPONENT 'DCFLG' OF STRUCTURE cs_data TO FIELD-SYMBOL(<lv_dcflg>).
    CHECK sy-subrc = 0.

    <lv_dcflg> = 'C'. "Create

    ASSIGN COMPONENT 'STATU' OF STRUCTURE cs_data TO FIELD-SYMBOL(<lv_statu>).
    CHECK sy-subrc = 0.

    <lv_statu> = me->set_icon( 'C' ). "Create "신규

*---------------------------------
* Set Timestamp..
*---------------------------------
    zcl_cn_alv_grid=>set_timestamp( EXPORTING iv_create = 'X'
                                     CHANGING  cs_data   = ls_timestamp ).

    MOVE-CORRESPONDING ls_timestamp TO cs_data.

*---------------------------------
* Set Default Field..
*---------------------------------
    LOOP AT lt_dftvl INTO DATA(ls_field).
      ASSIGN COMPONENT ls_field-fieldname OF STRUCTURE cs_data TO FIELD-SYMBOL(<lv_val>).
      CHECK sy-subrc = 0.
      <lv_val> = ls_field-value.
    ENDLOOP.


*--------------------------------------
* On Set Add Row Value
*--------------------------------------
    me->on_set_add_row_value( EXPORTING iv_name = me->m_name
                              CHANGING cs_data  = cs_data ).

    MOVE-CORRESPONDING cs_data TO <ls_data>.

    INSERT <ls_data> INTO <lt_data> INDEX iv_row.

  ENDMETHOD.


  METHOD conv_value.
*&---------------------------------------------------------------------*
*& Module      : CN
*& 생성자      : T0200335
*& 생성일      :  2020.12.30
*& Description :
*&---------------------------------------------------------------------*
*& Change History
*&---------------------------------------------------------------------*
*& 변경SEQ    변경자        변경일자             변경내용
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
    DATA:lv_typename  TYPE lvc_roll.

    CHECK iv_value IS NOT INITIAL.

    IF is_fcat-rollname IS NOT INITIAL.
      lv_typename = is_fcat-rollname.
    ELSE.
      lv_typename = is_fcat-domname.
    ENDIF.

    CHECK lv_typename IS NOT INITIAL.


    CALL FUNCTION 'EHPRC_CP_LB06_CONV_EXIT_CALL'
      EXPORTING
        i_input       = iv_value
        i_flg_int2ext = iv_type
        i_typename    = lv_typename
      IMPORTING
        e_output      = cv_val
      EXCEPTIONS
        conv_error    = 1
        OTHERS        = 2.

  ENDMETHOD.


  METHOD conv_upload_data.
*&---------------------------------------------------------------------*
*& Module      : CN
*& 생성자      : T0200335
*& 생성일      :  2020.12.30
*& Description :
*&---------------------------------------------------------------------*
*& Change History
*&---------------------------------------------------------------------*
*& 변경SEQ    변경자        변경일자             변경내용
*&---------------------------------------------------------------------*

    CONSTANTS:lc_char(4) VALUE 'CHAR'.

    DATA:lo_data      TYPE REF TO data,
         ls_timestamp TYPE zscn_timestamp.

    FIELD-SYMBOLS:<ls_data> TYPE any.


    CREATE DATA lo_data LIKE LINE OF ct_data.
    ASSIGN lo_data->* TO <ls_data>.

*-------------------------------
* Set Currency / Conversion Exit
*-------------------------------
    DATA(lt_tcurr) = me->get_factor( ).

    "Currency Field
    DATA(lt_fcat_c) = VALUE lvc_t_fcat( FOR wa_fcat IN it_fcat WHERE
                                        ( cfieldname IS NOT INITIAL )
                                        ( CORRESPONDING #( wa_fcat ) ) ).
    "Conversion Exit
    DATA(lt_fcat_ex) = VALUE lvc_t_fcat( FOR wa_fcat IN it_fcat WHERE
                                        ( edit_mask IS NOT INITIAL OR convexit IS NOT INITIAL )
                                        ( CORRESPONDING #( wa_fcat ) ) ).

    "Chart Upper 용
    DATA(lt_fcat_u) = VALUE lvc_t_fcat( FOR wa_fcat IN it_fcat WHERE
                                       ( datatype = lc_char AND convexit IS INITIAL AND lowercase IS INITIAL )
                                       ( CORRESPONDING #( wa_fcat ) ) ).

*---------------------------
* Default value..
*---------------------------
    DATA(lt_dftvl) = at_dftvl.
    APPEND LINES OF: at_dftvl_add TO lt_dftvl,
                     it_dftvl     TO lt_dftvl.

*---------------------------
* Set Display Data..
*---------------------------
    zcl_cn_alv_grid=>set_timestamp( EXPORTING iv_create = 'X'
                                     CHANGING cs_data  = ls_timestamp ).

    LOOP AT it_upd ASSIGNING FIELD-SYMBOL(<ls_upd>).

      MOVE-CORRESPONDING <ls_upd> TO <ls_data>.
*---------------------
* Default Value..
*---------------------
      LOOP AT lt_dftvl INTO DATA(ls_dftvl).
        ASSIGN COMPONENT ls_dftvl-fieldname OF STRUCTURE <ls_data> TO FIELD-SYMBOL(<lv_value>).
        CHECK sy-subrc = 0.
        <lv_value> = ls_dftvl-value.
        UNASSIGN <lv_value>.
      ENDLOOP.


*---------------------
* Set Currency Factor
*---------------------
      LOOP AT lt_fcat_c INTO DATA(ls_fcat).
        ASSIGN COMPONENT ls_fcat-fieldname OF STRUCTURE <ls_data> TO <lv_value>.
        CHECK sy-subrc = 0 AND <lv_value> IS NOT INITIAL.
        ASSIGN COMPONENT ls_fcat-cfieldname OF STRUCTURE <ls_data> TO FIELD-SYMBOL(<lv_waers>).
        IF sy-subrc = 0 AND <lv_waers> IS NOT INITIAL.
          READ TABLE lt_tcurr INTO DATA(ls_tcurr) WITH KEY waers = <lv_waers> BINARY SEARCH.
          IF sy-subrc = 0 AND ls_tcurr-factor IS NOT INITIAL AND ls_tcurr-factor NE 1. " Factor ##
            <lv_value> = CONV #( <lv_value> / ls_tcurr-factor ).
          ENDIF.
        ENDIF.
      ENDLOOP.

*---------------------
* Set Chart Upper Case
*---------------------
      LOOP AT lt_fcat_u INTO ls_fcat.
        ASSIGN COMPONENT ls_fcat-fieldname OF STRUCTURE <ls_data> TO <lv_value>.
        CHECK sy-subrc = 0 AND <lv_value> IS NOT INITIAL.
        <lv_value> = |{ <lv_value>  CASE = UPPER }| .
      ENDLOOP.

*---------------------------
* Set Conversion Exit I/O
*---------------------------
      LOOP AT lt_fcat_ex INTO ls_fcat.

        IF ls_fcat-edit_mask IS NOT INITIAL OR ls_fcat-convexit IS NOT INITIAL.
          ASSIGN COMPONENT ls_fcat-fieldname OF STRUCTURE <ls_upd> TO FIELD-SYMBOL(<lv_in_val>).  "Excel Value
          <lv_in_val> = |{ <lv_in_val>  CASE = UPPER }| .

          ASSIGN COMPONENT ls_fcat-fieldname OF STRUCTURE <ls_data> TO <lv_value>.                "Display Value

          me->conv_value( EXPORTING is_fcat       = ls_fcat
                                    iv_type     = ''
                                    iv_value     = <lv_in_val>
                          IMPORTING ev_subrc      = DATA(lv_subrc)
                          CHANGING  cv_val       = <lv_value> ).
        ENDIF.
      ENDLOOP.

*---------------------------
* Set StatuS / DCFLG
*---------------------------
      ASSIGN COMPONENT 'DCFLG' OF STRUCTURE <ls_data> TO FIELD-SYMBOL(<lv_dcflg>).
      IF sy-subrc = 0.
        <lv_dcflg> = 'C'.  "Create
      ENDIF.
      ASSIGN COMPONENT 'STATU' OF STRUCTURE <ls_data> TO FIELD-SYMBOL(<lv_statu>).
      IF sy-subrc = 0 AND <lv_statu> IS INITIAL.
        <lv_statu> = set_icon( 'X' ).  "Excel
      ENDIF.

*---------------------------
*-- TimeStamp..
*---------------------------
      MOVE-CORRESPONDING ls_timestamp TO <ls_data>.

*---------------------------
*-- Append Display Data..
*---------------------------
      APPEND <ls_data> TO ct_data.

    ENDLOOP.



  ENDMETHOD.


  METHOD conv_numc.
*&---------------------------------------------------------------------*
*& Module      : CN
*& 생성자      : T0200335
*& 생성일      :  2020.12.30
*& Description :
*&---------------------------------------------------------------------*
*& Change History
*&---------------------------------------------------------------------*
*& 변경SEQ    변경자        변경일자             변경내용
*&---------------------------------------------------------------------*

    DATA : lv_len TYPE i,
           lv_pos TYPE i,
           lv_chc TYPE char1,
           lv_chn TYPE i.

    lv_len = strlen( iv_value ).

    DO lv_len TIMES.
      lv_chc = iv_value+lv_pos(1).
      lv_pos = lv_pos + 1.
      TRY .
          lv_chn = lv_chc.
        CATCH cx_sy_conversion_no_number INTO DATA(lrf_con).
          CONTINUE.
        CATCH cx_root INTO DATA(lrf_root).
          CONTINUE.
      ENDTRY.

      CONCATENATE rv_value lv_chc INTO rv_value.
    ENDDO.

  ENDMETHOD.


  METHOD CONVERT_LONG_TO_DATE.
*&---------------------------------------------------------------------*
*& Module      : CN
*& 생성자      : T0200335
*& 생성일      :  2020.12.30
*& Description :
*&---------------------------------------------------------------------*
*& Change History
*&---------------------------------------------------------------------*
*& 변경SEQ    변경자        변경일자             변경내용
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*

    DATA lv_num_days TYPE i.

    CONSTANTS: lc_init_date_1900 TYPE d VALUE '18991231',
               lc_init_date_1904 TYPE d VALUE '19040101'.

    lv_num_days = floor( iv_date_string ).

    IF me->ac_dateformat1904 = abap_false.
      " 1900 based
      rv_date = lc_init_date_1900.  "'18991231'.
      IF iv_date_string > 59.
        " Microsoft thinks the year 1900 is a leap year... it is not!
        rv_date = rv_date + lv_num_days - 1.
      ELSE.
        " From 1899-12-31 to 1900-02-28 Microsoft guesses the correct date
        rv_date = rv_date + lv_num_days.
      ENDIF.
      " 1904 based
    ELSE.
      rv_date = lc_init_date_1904.  "'19040101'.
      rv_date = rv_date + lv_num_days.
    ENDIF.

  ENDMETHOD.


  METHOD convert_dec_time_to_hhmmss.
*&---------------------------------------------------------------------*
*& Module      : CN
*& 생성자      : T0200335
*& 생성일      :  2020.12.30
*& Description :
*&---------------------------------------------------------------------*
*& Change History
*&---------------------------------------------------------------------*
*& 변경SEQ    변경자        변경일자             변경내용
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*


    DATA:
      lv_dec_time   TYPE decfloat16,
      lv_hour       TYPE i,
      lv_hour_str   TYPE string,
      lv_minute     TYPE i,
      lv_minute_str TYPE string,
      lv_second     TYPE decfloat16.

    TRY.
        lv_dec_time = iv_dec_time_string.
      CATCH cx_root.
        " Cannot convert string to dec float... leaving undone
        rv_time = iv_dec_time_string.
        EXIT.
    ENDTRY.

    lv_dec_time = frac( lv_dec_time ). " Make sure that only the fraction is considered

    " Thanks to Excel, we have to round at this point to be compliant
    lv_dec_time = round( val = lv_dec_time dec = 15 ).

    lv_dec_time = lv_dec_time * 24.
    lv_hour = floor( lv_dec_time ).
    lv_dec_time = ( lv_dec_time - lv_hour ) * 60.
    lv_minute = floor( lv_dec_time ).
    lv_second = round( val = ( ( lv_dec_time - lv_minute ) * 60 ) dec = 3 ).
    IF lv_second >= 60.
      lv_second = 0.
      lv_minute = lv_minute + 1.
    ENDIF.

    IF lv_hour < 10.
      lv_hour_str = '0' && lv_hour.
    ELSE.
      lv_hour_str = lv_hour.
    ENDIF.
    cl_abap_string_utilities=>del_trailing_blanks( CHANGING str = lv_hour_str ).

    IF lv_minute < 10.
      lv_minute_str = '0' && lv_minute.
    ELSE.
      lv_minute_str = lv_minute.
    ENDIF.
    cl_abap_string_utilities=>del_trailing_blanks( CHANGING str = lv_minute_str ).

************* add by DongBin.Park
    DATA: lv_second_str TYPE string.

    IF lv_second < 10.
      lv_second_str = '0' && lv_second.
    ELSE.
      lv_second_str = lv_second.
    ENDIF.
    cl_abap_string_utilities=>del_trailing_blanks( CHANGING str = lv_second_str ).

*************
    rv_time = lv_hour_str && lv_minute_str &&  lv_second_str .

  ENDMETHOD.


  METHOD CONVERT_CELL_TO_DATE_TIME.
*&---------------------------------------------------------------------*
*& Module      : CN
*& 생성자      : T0200335
*& 생성일      :  2020.12.30
*& Description :
*&---------------------------------------------------------------------*
*& Change History
*&---------------------------------------------------------------------*
*& 변경SEQ    변경자        변경일자             변경내용
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*

 data:
        lv_date_str  type string,
        lv_time_str  type string,
        lv_date_time type decfloat34,
        lv_date type d,
        lv_time type t.

  try.
    lv_date_time = iv_input.
  catch cx_root.
    "Not able to interpret as dec value
    exit.
  endtry.

  lv_date_str = floor( lv_date_time ).

  if lv_date_str ne '0'.
    lv_date = convert_long_to_date( lv_date_str ).
  endif.

  lv_time_str = frac( lv_date_time ).
  if lv_time_str ne '0'.
    lv_time = convert_dec_time_to_hhmmss( lv_time_str ).
  endif.


    CONCATENATE lv_date lv_time INTO rv_output.

  ENDMETHOD.


  METHOD constructor.
*&---------------------------------------------------------------------*
*& Module              : CN
*& Program ID          : ZCL_CN_ALV_GRID
*& T-CODE              : N/A
*& Referenced Program  : N/A
*& Created by          : T0200335
*& Created On          : 2020.12.23
*& Type                : CLASS
*& Description         : CONSTRUCTOR
*&---------------------------------------------------------------------*
*& Change History
*&---------------------------------------------------------------------*
*& 변경SEQ    변경자        변경일자             변경내용
*&---------------------------------------------------------------------*
    DATA:lv_mode,
         lv_lock.

    CHECK iv_no_grid IS INITIAL.  "alv 용 일경우만 생성

    CALL METHOD super->constructor
      EXPORTING
        i_parent          = irf_parent       " Parent Container
        i_name            = iv_name          " Name
      EXCEPTIONS
        error_cntl_create = 1                " Error when creating the control
        error_cntl_init   = 2                " Error While Initializing Control
        error_cntl_link   = 3                " Error While Linking Control
        error_dp_create   = 4                " Error While Creating DataProvider Control
        OTHERS            = 5.

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

*- ALV Name
    DATA(lv_name) = me->get_name( ).

    IF lv_name IS INITIAL.
      m_name = iv_name.
    ENDIF.

*-- Default Value (Insert Row)
    at_dftvl = it_dftvl.

    ar_dftvl = VALUE #( FOR wa_dftvl IN at_dftvl
                        ( sign = 'I' option = 'EQ' low = wa_dftvl-fieldname ) ).
*-- Variant
    av_variant = iv_variant.

*-- Edit Mode (초반 Display Mode)
    av_edit_mode = ''.

*-- 기본 User Toolbar ( 라인추가/라인삭제/엑셀/Recovery)
    as_toolbtn = is_toolbtn. "기본툴바 버튼
    IF as_toolbtn-btn_madd = 'X' AND
       as_toolbtn-mlti_lines IS INITIAL.
      as_toolbtn-mlti_lines = 10.
    ENDIF.

*-- Program ID
    av_repid = av_tcode = sy-cprog.

    IF iv_tcode IS NOT INITIAL.
      av_tcode = iv_tcode.
    ENDIF.

*-- Lock Type
    av_lock_nm = iv_lock_nm.  "Lock Job Type

*-- Direct Change
    av_dichg  = iv_dichg.
*      IF av_dichg = 'X'.
*        CALL METHOD me->set_change_mode(
*          CHANGING
*            cv_mode = lv_mode
*            cv_lock = av_lock ).
*        IF av_lock = 'X'.
*          CLEAR:av_dichg.
*        ENDIF.
*      ENDIF.
*----------------------------
*-- Hisoty Table Name
*----------------------------
    IF is_toolbtn-hist_tabnm IS NOT INITIAL.
      SELECT SINGLE tabname
        INTO @av_hst_tabnm
        FROM dd02l
       WHERE tabname  = @is_toolbtn-hist_tabnm
         AND as4local = 'A'.
    ENDIF.

*----------------------------
*-- Information 버튼
*----------------------------
    me->init_info_text( ).

    IF is_toolbtn-btn_info = 'X'.
      APPEND LINES OF it_info TO at_info.
      at_info = VALUE #( BASE at_info ( fieldname = 'STATU' ) ).
    ENDIF.

    av_dc_skip = iv_changed_skip.  "공통 Data Changed Skip

    av_cells = iv_cells.
    av_cellc = iv_cellc.
*-------------------------------------------------
* Create Header Information
*-------------------------------------------------
    CONSTANTS:lc_alv_grid(50) VALUE 'ALV_GRID'.

    CHECK irf_head IS BOUND.  "Header Conatiner가 있을 경우

    arf_head  = irf_head.

    arf_docu = NEW cl_dd_document( style = lc_alv_grid ).

    "Selection-screen값 자동 설정 "2022.05.01이후 사용가능(cts이관전)
*    IF iv_auto_head = 'X'.
*      at_header    = zcl_cn_apv_comm=>get_pgm_parm_info( ).
*      DATA(lt_header) = it_header.
*      DELETE lt_header WHERE typ = ''.  "주석부분 넘기기
*      APPEND LINES OF lt_header TO at_header.
*    ELSE.
*      at_header = it_header.
*    ENDIF.
*
*    av_scr_wr = iv_scr_wr.

  ENDMETHOD.


    METHOD chk_dup_data.
*&---------------------------------------------------------------------*
*& Module              : CN
*& Program ID          : ZCL_CN_ALV_GRID
*& T-CODE              : N/A
*& Referenced Program  : N/A
*& Created by          : T0200335
*& Created On          : 2020.12.23
*& Type                : CLASS
*& Description         : CRT_DYN_TABLE
*&---------------------------------------------------------------------*
*& Change History
*&---------------------------------------------------------------------*
*& 변경SEQ    변경자        변경일자             변경내용
*&---------------------------------------------------------------------*
    DATA:lo_data  TYPE REF TO data,
         lo_sdata TYPE REF TO data,
         lt_tkey  TYPE abap_keydescr_tab.

    FIELD-SYMBOLS:<lt_dup>   TYPE SORTED TABLE,
                  <lt_msgtb> TYPE zycn00001,
                  <ls_dup>   TYPE any.

    FIELD-SYMBOLS:<lt_data> TYPE table.

    CLEAR:rv_return.

*-----------------------------------------
* Make Key Table..
*-----------------------------------------
    DATA(lt_fcat) = at_fcat.

    ASSIGN ao_dataref->* TO <lt_data>.
    CHECK sy-subrc = 0.

    IF it_key IS INITIAL.
      SORT lt_fcat BY col_pos.

      DELETE lt_fcat WHERE key IS INITIAL.
      lt_tkey = CORRESPONDING #( lt_fcat MAPPING name = fieldname ).
    ELSE.
      lt_tkey = it_key.
    ENDIF.
    me->crt_dyn_table( EXPORTING it_fcat = lt_fcat
                                 it_tkey = lt_tkey
                                 iv_table_kind = cl_abap_tabledescr=>tablekind_sorted
                       IMPORTING eo_dynt = lo_data ).

    CHECK lo_data IS BOUND.

    ASSIGN lo_data->* TO <lt_dup>.

    CREATE DATA lo_sdata LIKE LINE OF <lt_dup>.
    ASSIGN lo_sdata->* TO <ls_dup>.

*-----------------------------------------
* Original 및 Update 모드일 경우 Key Append..
*-----------------------------------------
    LOOP AT <lt_data> ASSIGNING FIELD-SYMBOL(<ls_tab>).

      ASSIGN COMPONENT 'DCFLG' OF STRUCTURE <ls_tab> TO FIELD-SYMBOL(<lv_dcflg>).
      CHECK sy-subrc = 0.

      CHECK <lv_dcflg> <> 'C' AND <lv_dcflg> <> 'T'.  "신규/삭제가 아닌 건은 Key 세팅

      MOVE-CORRESPONDING <ls_tab> TO <ls_dup>.
      READ TABLE <lt_dup> FROM <ls_dup> TRANSPORTING NO FIELDS.
      "Append Data
      INSERT <ls_dup> INTO TABLE <lt_dup>.
    ENDLOOP.

*-----------------------------------------
* 중복 Key Check..
*-----------------------------------------
    lt_fcat = at_fcat.
    SORT lt_fcat BY fieldname.

    LOOP AT <lt_data> ASSIGNING <ls_tab>.

      ASSIGN COMPONENT 'DCFLG' OF STRUCTURE <ls_tab> TO <lv_dcflg>.
      CHECK sy-subrc = 0 AND <lv_dcflg> IS NOT INITIAL.

      ASSIGN COMPONENT 'STATU' OF STRUCTURE <ls_tab> TO FIELD-SYMBOL(<lv_statu>).

      ASSIGN COMPONENT 'MSGTB' OF STRUCTURE <ls_tab> TO <lt_msgtb>.
      CHECK sy-subrc = 0.

      DELETE <lt_msgtb> FROM 1 WHERE fieldname EQ '*'.  "* 데이타 삭제

      "IV_MSG_DEL = 'X'인 경우는 무조건 메세지 Table CLEAR
      IF iv_msg_del = 'X'.
        CLEAR: <lt_msgtb>.
        <lv_statu> = set_icon( <lv_dcflg> ).
      ENDIF.


      CASE <lv_dcflg>.
        WHEN 'C'.                "신규일 경우만 Key Check...
*-------------------------------
* Create Line
*-------------------------------
          MOVE-CORRESPONDING <ls_tab> TO <ls_dup>.
          READ TABLE <lt_dup> FROM <ls_dup> TRANSPORTING NO FIELDS.
          IF sy-subrc = 0.
            "중복에러
            _g_set_msgtb:'' <ls_tab> '*' 'ZCN00' '008' '' '' '' .  "데이타가 중복되었습니다.
            rv_return = 'X'.
          ELSE.
            "Append Data
            INSERT <ls_dup> INTO TABLE <lt_dup>.
          ENDIF.

        WHEN 'U'.
        WHEN 'T'.
          CLEAR:<lt_msgtb>.
          CONTINUE.
        WHEN OTHERS.
          CONTINUE.
      ENDCASE.

*-----------------------------
* 필수 체크
*-----------------------------
      LOOP AT it_mandt_field INTO DATA(ls_field).
        ASSIGN COMPONENT ls_field-fieldname OF STRUCTURE <ls_tab> TO FIELD-SYMBOL(<lv_field>).
        IF sy-subrc = 0 AND <lv_field> IS INITIAL.
          READ TABLE lt_fcat INTO DATA(ls_fcat) WITH KEY fieldname = ls_field-fieldname BINARY SEARCH.
          _g_set_msgtb:'' <ls_tab> ls_field-fieldname 'ZCN00' '009' ls_fcat-coltext '' '' .  "&1 은(는) 필수입니다.
          rv_return = 'X'.
        ENDIF.
      ENDLOOP.

*-----------------------------
* Check Save Data
*-----------------------------
      me->on_chk_data( EXPORTING iv_name = me->m_name
                       CHANGING cs_data = <ls_tab>
                                cv_err  = rv_return ).

      IF <lt_msgtb> IS INITIAL.
        <lv_statu> = set_icon( <lv_dcflg> ).
      ELSE.
        rv_return = 'X'.
      ENDIF.
    ENDLOOP.


  ENDMETHOD.


  METHOD check_hangul.

    DATA: lv_value TYPE string.

    lv_value = iv_value.

    TRANSLATE lv_value TO UPPER CASE.
    CONDENSE lv_value..

    IF lv_value CO zcn00_alpha.
      CLEAR : rv_check.
    ELSE.
      rv_check = 'X'. "한글 포함됨.
    ENDIF.

  ENDMETHOD.


  METHOD check_excel_file.
*&---------------------------------------------------------------------*
*& Module              : CN
*& Program ID          : ZCL_CN_ALV_GRID
*& T-CODE              : N/A
*& Referenced Program  : N/A
*& Created by          : T0200335
*& Created On          : 2020.12.23
*& Type                : CLASS
*& Description         : CRT_DYN_TABLE
*&---------------------------------------------------------------------*
*& Change History
*&---------------------------------------------------------------------*
*& 변경SEQ    변경자        변경일자             변경내용
*&---------------------------------------------------------------------*

    DATA:lv_name TYPE string.

*-------------------------------------
* Excel 체크
*-------------------------------------
    CALL FUNCTION 'SO_SPLIT_FILE_AND_PATH'
      EXPORTING
        full_name     = iv_filename
      IMPORTING
        stripped_name = lv_name
      EXCEPTIONS
        x_error       = 1
        OTHERS        = 2.

    SPLIT lv_name   AT '.' INTO TABLE DATA(lt_file).
    DESCRIBE TABLE lt_file LINES DATA(lv_cnt).
    READ TABLE lt_file INTO DATA(ls_file) INDEX lv_cnt.
    CASE ls_file.
      WHEN 'XLSX'.
      WHEN OTHERS.
        rv_subrc = 4.
    ENDCASE.

  ENDMETHOD.


  METHOD check_drm.
*&---------------------------------------------------------------------*
*& Module      : CN
*& 생성자      : T0200335
*& 생성일      :  2020.12.30
*& Description :
*&---------------------------------------------------------------------*
*& Change History
*&---------------------------------------------------------------------*
*& 변경SEQ    변경자        변경일자             변경내용
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*

    DATA: lv_length TYPE i.

    DATA: lt_data TYPE stringtab.
    TRY.

        CALL FUNCTION 'GUI_UPLOAD'
          EXPORTING
            filename                = iv_filename
            filetype                = 'ASC'
            codepage                = '4110'
          IMPORTING
            filelength              = lv_length
          TABLES
            data_tab                = lt_data
          EXCEPTIONS
            file_open_error         = 1
            file_read_error         = 2
            no_batch                = 3
            gui_refuse_filetransfer = 4
            invalid_type            = 5
            no_authority            = 6
            unknown_error           = 7
            bad_data_format         = 8
            header_not_allowed      = 9
            separator_not_allowed   = 10
            header_too_long         = 11
            unknown_dp_error        = 12
            access_denied           = 13
            dp_out_of_memory        = 14
            disk_full               = 15
            dp_timeout              = 16
            OTHERS                  = 17.


        IF sy-subrc <> 0 AND lt_data IS INITIAL.
          rv_invalid = 'X'.
          ev_subrc = sy-subrc.
          EXIT.
        ELSE.
          CLEAR:sy-msgid, sy-msgno, sy-msgty, sy-msgv1, sy-msgv2, sy-msgv3, sy-msgv4.
        ENDIF.

        READ TABLE lt_data INTO DATA(ls_data) INDEX 1.

        FIND REGEX '# DRMONE  This Document is encrypted and protected by Fasoo DRM' IN ls_data IN CHARACTER MODE.
        IF sy-subrc = 0.
          rv_invalid = abap_true.
          ev_subrc = 4.
          EXIT.
        ENDIF.

      CATCH cx_root INTO DATA(lo_root) .
        DATA(lv_msg) = lo_root->get_text( ).
        rv_invalid = abap_true.
        MESSAGE s001(zcn00) WITH lv_msg DISPLAY LIKE 'E'.
        EXIT.
    ENDTRY.
  ENDMETHOD.


  METHOD CELL_TO_INDEX.
*&---------------------------------------------------------------------*
*& Module      : CN
*& 생성자      : T0200335
*& 생성일      :  2020.12.30
*& Description :
*&---------------------------------------------------------------------*
*& Change History
*&---------------------------------------------------------------------*
*& 변경SEQ    변경자        변경일자             변경내용
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
    CONSTANTS: lc_abclen  TYPE i VALUE 26,
               lc_abc(26) TYPE c VALUE 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'.
    DATA: lv_len TYPE i.
    DATA: lv_colval.
    DATA: lv_abci TYPE i.
    DATA: lv_digit TYPE i.

    lv_len = strlen( iv_cell ) - 1.
    CLEAR rv_index.
    CLEAR lv_digit.
    WHILE lv_len >= 0.
      lv_colval = iv_cell+lv_digit(1).
      FIND lv_colval IN lc_abc MATCH OFFSET lv_abci.
      rv_index = rv_index + ( lv_abci + 1 ) * ipow( base = lc_abclen exp = lv_len ).
      lv_len = lv_len - 1.
      lv_digit = lv_digit + 1.
    ENDWHILE.

  ENDMETHOD.


  METHOD btn_rec_row.
*&---------------------------------------------------------------------*
*& Module              : CN
*& Program ID          : ZCL_CN_ALV_GRID
*& T-CODE              : N/A
*& Referenced Program  : N/A
*& Created by          : T0200335
*& Created On          : 2020.12.23
*& Type                : CLASS
*& Description         : CRT_DYN_TABLE
*&---------------------------------------------------------------------*
*& Change History
*&---------------------------------------------------------------------*
*& 변경SEQ    변경자        변경일자             변경내용
*&---------------------------------------------------------------------*
    DATA : lt_rows TYPE lvc_t_row,
           ls_rows TYPE lvc_s_row.

    DATA:lo_data      TYPE REF TO data,
         ls_history   TYPE ts_history,
         ls_timestamp TYPE zscn_timestamp.


    FIELD-SYMBOLS:<lt_data> TYPE table.

    ASSIGN ao_dataref->* TO <lt_data>.

    CHECK sy-subrc = 0.

*--------------------------------
* 삭제하기 위하여 선택한 줄
*--------------------------------
    CALL METHOD me->get_selected_rows
      IMPORTING
        et_index_rows = lt_rows.


    CALL METHOD cl_gui_cfw=>flush.

*----------------------------------
* 선택된 대상이 없을 경우 에러
*----------------------------------
    IF lt_rows[] IS INITIAL.
      MESSAGE s005(zcn00) DISPLAY LIKE 'E'.     "라인을 선택해 주십시오.
      EXIT.
    ENDIF.

    LOOP AT lt_rows INTO ls_rows.

      READ TABLE <lt_data> ASSIGNING FIELD-SYMBOL(<ls_data>) INDEX ls_rows-index.
      CHECK sy-subrc = 0.

      ASSIGN COMPONENT 'ZDELE' OF STRUCTURE <ls_data> TO FIELD-SYMBOL(<lv_del>).
      CHECK sy-subrc = 0 AND <lv_del> IS INITIAL.
      CHECK <lv_del> IS ASSIGNED AND <lv_del> IS INITIAL.
      MESSAGE s000(zcn00) WITH TEXT-e02 DISPLAY LIKE 'E'.
      RETURN.
    ENDLOOP.

*---------------------------
* 삭제 Confirm..
*---------------------------
    CASE me->pop_to_msg( iv_title = zcl_cn_alv_grid=>ac_msg_title4   "복구확인
                         iv_text1 = zcl_cn_alv_grid=>ac_msg_rec1     "선택된 데이타를 복구하려고 합니다.'
                         iv_text2 = zcl_cn_alv_grid=>ac_msg_rec2 ).  "복구하시겠습니까?'
      WHEN abap_true.
      WHEN OTHERS.
        EXIT.
    ENDCASE.


*----------------------------------
* Recovery 수행
*----------------------------------
    LOOP AT lt_rows INTO ls_rows.

      READ TABLE <lt_data> ASSIGNING <ls_data> INDEX ls_rows-index.
      CHECK sy-subrc EQ 0.

*---------------------------------
* Set DCFLAG/Status..
*---------------------------------
      ASSIGN COMPONENT 'ZDELE' OF STRUCTURE <ls_data> TO <lv_del>.
      CHECK sy-subrc = 0 AND <lv_del> = 'X'.

      ASSIGN COMPONENT 'DCFLG' OF STRUCTURE <ls_data> TO FIELD-SYMBOL(<lv_dcflg>).
      CHECK sy-subrc = 0.

      ASSIGN COMPONENT 'STATU' OF STRUCTURE <ls_data> TO FIELD-SYMBOL(<lv_statu>).
      CHECK sy-subrc = 0.

      CLEAR:<lv_del>.

      <lv_dcflg> = 'U'.  "Update 모드로 변경
      <lv_statu> = me->set_icon( 'U' ). "Line 삭제 표시

*---------------------------------
* Set Timestamp..
*---------------------------------
      zcl_cn_alv_grid=>set_timestamp( EXPORTING iv_create = ''
                                      CHANGING  cs_data   = <ls_data> ).

      "Changed Status..
      av_status  = 'X'.

      CHECK av_hst_tabnm IS NOT INITIAL.
*---------------------------------
* Set History Field..
*---------------------------------
      ls_history-hfldnm    =  TEXT-m06. " 'Line Recovery'.
      ls_history-aedat     = sy-datum.
      ls_history-aezet     = sy-uzeit.
      ls_history-hval_b    = '*'.
      ls_history-hval_a    = '*'.
      ls_history-hchg_type = 'R'.

      me->set_hist_datakey( EXPORTING is_data  = <ls_data>
                            IMPORTING ev_key   = ls_history-dkey
                                      ev_keynm = ls_history-hkeynm ).
*---------------------------------
*  Append history
*---------------------------------
      APPEND ls_history TO at_history.

    ENDLOOP.

  ENDMETHOD.


  METHOD btn_info.
*&---------------------------------------------------------------------*
*& Module              : CN
*& Program ID          : ZCL_CN_ALV_GRID
*& T-CODE              : N/A
*& Referenced Program  : N/A
*& Created by          : T0200335
*& Created On          : 2020.12.23
*& Type                : CLASS
*& Description         : CRT_DYN_TABLE
*&---------------------------------------------------------------------*
*& Change History
*&---------------------------------------------------------------------*
*& 변경SEQ    변경자        변경일자             변경내용
*&---------------------------------------------------------------------*
    CONSTANTS:lc_icon(4) VALUE 'ICON'.

    TYPES : BEGIN OF lts_info,
              total TYPE char20,
              count TYPE char10,
            END OF lts_info.

    DATA : lt_info       TYPE TABLE OF lts_info.


    DATA : ls_info_cnt TYPE ts_info_cnt,
           lt_info_cnt TYPE ty_info_cnt,
           lt_fcat     TYPE ty_fcat.
    DATA : lv_statu     TYPE icon_d VALUE 'STATU'.

    FIELD-SYMBOLS: <lt_data>  TYPE STANDARD TABLE,
                   <lv_statu> TYPE any,
                   <ls_fcat>  TYPE lvc_s_fcat.

    SORT:at_info BY fieldname.
    DELETE ADJACENT DUPLICATES FROM at_info COMPARING fieldname.

    ASSIGN ao_dataref->* TO <lt_data>.
    CHECK <lt_data> IS ASSIGNED.

*-------------------------
* Total Count
*-------------------------
    DATA(lv_count) = lines( <lt_data> ).

*-- Icon = 'X' 인것만
    lt_fcat = VALUE #( FOR wa_fcat IN at_fcat WHERE
                     ( domname = lc_icon or icon = 'X' )
                     ( CORRESPONDING #( wa_fcat ) ) ).

*-------------------------
*  Count by Status
*-------------------------
    LOOP AT <lt_data> ASSIGNING FIELD-SYMBOL(<ls_data>).
      LOOP AT at_info INTO DATA(ls_info).
*-------------------------
*-- 필드 Description
*-------------------------
        AT NEW fieldname.
          ls_info_cnt-coltext = VALUE #( lt_fcat[ KEY zfd COMPONENTS fieldname = ls_info-fieldname ]-coltext OPTIONAL ).
          IF ls_info_cnt-coltext IS INITIAL.
            CONTINUE.
          ENDIF.
        ENDAT.

*-------------------------
* Status Icon Value
*-------------------------
        ASSIGN COMPONENT ls_info-fieldname OF STRUCTURE <ls_data> TO FIELD-SYMBOL(<lv_value>).
        lv_statu = COND #( WHEN <lv_value> IS ASSIGNED THEN <lv_value> ELSE icon_led_green ).

*-- Collect
        ls_info_cnt-count = 1.
        ls_info_cnt-statu = <lv_value>.
        ls_info_cnt-fname = ls_info-fieldname.
        COLLECT ls_info_cnt INTO lt_info_cnt.
      ENDLOOP.
      CLEAR:ls_info_cnt.
    ENDLOOP.

*-------------------------
* Set Status Text..
*-------------------------
    LOOP AT lt_info_cnt ASSIGNING FIELD-SYMBOL(<ls_info>).
      <ls_info>-ftext = VALUE #( at_info_text[ KEY zfd COMPONENTS statu = <ls_info>-statu ]-ftext OPTIONAL ).

*--------------------------------------
*  Get Description By Local Status..
*--------------------------------------
      on_info_text( EXPORTING iv_name  = me->m_name
                              iv_fname = <ls_info>-fname
                              iv_statu = <ls_info>-statu
                    CHANGING  cv_text  = <ls_info>-ftext ).

    ENDLOOP.

    IF lt_info_cnt[] IS INITIAL.
      MESSAGE s011(zcn00) DISPLAY LIKE 'W'  WITH TEXT-t01."011  &1 데이타가 없습니다.
      EXIT.
    ENDIF.

    SORT:lt_info_cnt BY fname statu.
*--------------------------------------
*  Display Status Popup
*--------------------------------------
    call function 'ZFCN_ALV_BLOCK_POPUP'
      EXPORTING
        iv_title = TEXT-t01  "Icon 범주
        iv_cnt   = lv_count
        it_data  = lt_info_cnt.


  ENDMETHOD.


  METHOD btn_history.
*&---------------------------------------------------------------------*
*& Module              : CN
*& Program ID          : ZCL_CN_ALV_GRID
*& T-CODE              : N/A
*& Referenced Program  : N/A
*& Created by          : T0200335
*& Created On          : 2020.12.23
*& Type                : CLASS
*& Description         : CRT_DYN_TABLE
*&---------------------------------------------------------------------*
*& Change History
*&---------------------------------------------------------------------*
*& 변경SEQ    변경자        변경일자             변경내용
*&---------------------------------------------------------------------*
    DATA:lr_key TYPE rseloption,
         lv_key TYPE char255.

    DATA : lt_rows TYPE lvc_t_row.
    FIELD-SYMBOLS:<lt_data> TYPE table.

    CHECK iv_tabnm IS NOT INITIAL.


    ASSIGN ao_dataref->* TO <lt_data>.
*--------------------------------
*   키별 선택
*--------------------------------
    CALL METHOD me->get_selected_rows
      IMPORTING
        et_index_rows = lt_rows.


    CALL METHOD cl_gui_cfw=>flush.
*----------------------------------
* 선택된 대상
*----------------------------------
    IF lt_rows IS NOT INITIAL.
      LOOP AT lt_rows INTO DATA(ls_rows).
        READ TABLE <lt_data> ASSIGNING FIELD-SYMBOL(<ls_data>) INDEX ls_rows-index.
        CHECK <ls_data> IS ASSIGNED.

        ASSIGN COMPONENT 'DCFLG' OF STRUCTURE <ls_data> TO FIELD-SYMBOL(<lv_dcflg>).
        CHECK <lv_dcflg> <> 'C'. "Create가 아닌건
        CLEAR:lv_key.
        me->set_hist_datakey( EXPORTING is_data  = <ls_data>
                              IMPORTING ev_key   = lv_key ).
        lr_key = VALUE #( BASE lr_key ( sign = 'I' option = 'EQ' low = lv_key ) ).

      ENDLOOP.

    ELSE.
*-------------------------------------
* 선택된 대상이 없을 경우 전체 데이타
*-------------------------------------
      LOOP AT <lt_data> ASSIGNING <ls_data>.

        ASSIGN COMPONENT 'DCFLG' OF STRUCTURE <ls_data> TO <lv_dcflg>.
        CHECK <lv_dcflg> <> 'C'. "Create가 아닌건
        CLEAR:lv_key.
        me->set_hist_datakey( EXPORTING is_data  = <ls_data>
                              IMPORTING ev_key   = lv_key ).
        lr_key = VALUE #( BASE lr_key ( sign = 'I' option = 'EQ' low = lv_key ) ).
      ENDLOOP.
    ENDIF.

    EXPORT: history_tab  FROM it_fcat TO MEMORY ID 'ZHIST',
            history_key  FROM lr_key  TO MEMORY ID 'ZHIST_D'.

    CHECK lr_key IS NOT INITIAL.
*-----------------------------------
* Submit History..
*-----------------------------------
    SUBMIT zrcn0001 WITH p_tabnm EQ iv_tabnm
                         AND RETURN.

  ENDMETHOD.


  METHOD btn_file_attatch.
*&---------------------------------------------------------------------*
*& Module              : CN
*& Program ID          : ZCL_CN_ALV_GRID
*& T-CODE              : N/A
*& Referenced Program  : N/A
*& Created by          : T0200335
*& Created On          : 2020.12.23
*& Type                : CLASS
*& Description         : CRT_DYN_TABLE
*&---------------------------------------------------------------------*
*& Change History
*&---------------------------------------------------------------------*
*& 변경SEQ    변경자        변경일자             변경내용
*&---------------------------------------------------------------------*
    DATA: lt_rows            TYPE lvc_t_row,
          lt_atta            TYPE TABLE OF gos_s_atta,
          lv_icon            TYPE icon_d,
          lv_cnt             TYPE i,
          lv_instid          TYPE sibfboriid,
          lv_attch_fname(30),
          ls_cell            TYPE lvc_s_modi,
          ls_object_data     TYPE sibflporb,
          ls_object          TYPE sibflporb.

    FIELD-SYMBOLS:<lt_data> TYPE table.

    ASSIGN ao_dataref->* TO <lt_data>.

    CHECK sy-subrc = 0.

*--------------------------------
* 삭제하기 위하여 선택한 줄
*--------------------------------
    CALL METHOD me->get_selected_rows
      IMPORTING
        et_index_rows = lt_rows.


    CALL METHOD cl_gui_cfw=>flush.

*----------------------------------
* 선택된 대상이 없을 경우 에러
*----------------------------------
    IF lt_rows[] IS INITIAL AND iv_row IS INITIAL.
      MESSAGE s005(zcn00) DISPLAY LIKE 'E'.     "라인을 선택해 주십시오.
      EXIT.
    ENDIF.
*----------------------------------
* 한개의 라인만 선택
*----------------------------------
    IF iv_row IS INITIAL AND lines( lt_rows ) <> 1.
      MESSAGE s019(zcn00) DISPLAY LIKE 'E'.     "한개의 라인만 선택하십시오.
      EXIT.
    ENDIF.

    IF iv_row IS INITIAL.
      READ TABLE lt_rows INTO DATA(ls_rows) INDEX 1.
    ELSE.
      ls_rows-index = iv_row.
    ENDIF.
    CHECK sy-subrc = 0.

    READ TABLE <lt_data> ASSIGNING FIELD-SYMBOL(<ls_data>) INDEX ls_rows-index.
    CHECK <ls_data> IS ASSIGNED.

*-- 삭제데이타 첨부 불가
    ASSIGN COMPONENT 'ZDELE' OF STRUCTURE <ls_data> TO FIELD-SYMBOL(<lv_del>).
    IF sy-subrc = 0 AND <lv_del> IS NOT INITIAL .
      MESSAGE s000(zcn00) WITH TEXT-e05 DISPLAY LIKE 'E'.  "삭제된 데이타는 첨부가 불가합니다.
      RETURN.
    ENDIF.

    ASSIGN COMPONENT 'DCFLG' OF STRUCTURE <ls_data> TO FIELD-SYMBOL(<lv_dcflg>).
    CHECK sy-subrc = 0.

    ASSIGN COMPONENT 'STATU' OF STRUCTURE <ls_data> TO FIELD-SYMBOL(<lv_statu>).
    CHECK sy-subrc = 0.

    ASSIGN COMPONENT 'INSTID' OF STRUCTURE <ls_data> TO FIELD-SYMBOL(<lv_instid>).
    CHECK sy-subrc = 0.

    CLEAR: ls_object.
*-------------------------------------
* 개별 로직 적용 및 Object Set
*-------------------------------------
    me->on_set_attatch_info( EXPORTING iv_name        = me->m_name
                                    is_data        = <ls_data>
                          CHANGING  cs_object      = ls_object
                                    cv_attch_fname = lv_attch_fname ).
    CHECK ls_object IS NOT INITIAL.

    IF lv_attch_fname IS INITIAL.   "파일명 생략시 공통
      _g_set_value:lv_attch_fname 'AFILE'.
    ENDIF.

*-------------------------------------
* Display Mode일 경우 Popup 후 Exit
*-------------------------------------
    IF me->av_edit_mode IS INITIAL OR IV_DISPLAY EQ 'X'.
      CALL FUNCTION 'GOS_ATTACHMENT_LIST_POPUP'
        EXPORTING
          is_object = ls_object
          ip_mode   = 'D'.         "Display
      EXIT.
    ENDIF.

*==============================================================================
* Edit Mode일 경우
*==============================================================================
    CALL FUNCTION 'GOS_ATTACHMENT_LIST_POPUP'
      EXPORTING
        is_object = ls_object
        ip_mode   = 'C'.         "Create/Display/Edi

*----------------------
* Get File Info..
*----------------------
    ASSIGN COMPONENT lv_attch_fname OF STRUCTURE <ls_data> TO FIELD-SYMBOL(<lv_icon>).

    CHECK sy-subrc = 0.
    <lv_icon> =  me->get_attatch_info( EXPORTING is_object = ls_object ).

*----------------------------
* 생성일 경우 확인 후 Exit
*----------------------------
    CASE <lv_dcflg>.
      WHEN 'C'.
*-- 생성일 경우 Clear 및 문서 Key 세팅  후 Exit.
        IF <lv_icon> IS INITIAL.
          CLEAR:<lv_instid>.
        ELSE.
          <lv_instid> = ls_object-instid.
        ENDIF.
        EXIT.
    ENDCASE.

*----------------------
* Data 수정일 경우
*----------------------
    IF ( <lv_instid> IS INITIAL     AND <lv_icon> IS INITIAL ) OR    "문서변경 없음
       ( <lv_instid> IS NOT INITIAL AND <lv_icon> IS NOT INITIAL ).  "문서변경 없음 (문서가 추가되는것은 Db에 영향이 없음)
      EXIT.
    ENDIF.

*---------------------------------
* Data 변경(첨부수정 및 첨부삭제)
*---------------------------------
    av_status = 'X'.


    <lv_dcflg> = 'U'.
    <lv_statu> = me->set_icon( 'U' ).

    " 첨부 Object Key
    IF <lv_icon> IS NOT INITIAL.
      <lv_instid> = ls_object-instid.
    ELSE.
      CLEAR:<lv_instid>.
    ENDIF.

    "Set Data History
    IF av_hst_tabnm IS NOT INITIAL.
      ls_cell = VALUE #( row_id    = ls_rows-index
                         fieldname = 'INSTID'
                         value     = <lv_instid> ).
      me->crt_hist_field( EXPORTING is_cell = ls_cell ).
    ENDIF.


    me->refresh_grid_display( ).

  ENDMETHOD.


  METHOD btn_excl_upload.
*&---------------------------------------------------------------------*
*& Module              : CN
*& Program ID          : ZCL_CN_ALV_GRID
*& T-CODE              : N/A
*& Referenced Program  : N/A
*& Created by          : T0200335
*& Created On          : 2020.12.23
*& Type                : CLASS
*& Description         : CRT_DYN_TABLE
*&---------------------------------------------------------------------*
*& Change History
*&---------------------------------------------------------------------*
*& 변경SEQ    변경자        변경일자             변경내용
*&---------------------------------------------------------------------*
    DATA:lv_filename  TYPE string,
         lv_exit,
         ls_timestamp TYPE zscn_timestamp,
         lo_data      TYPE REF TO data,
         lo_dataref   TYPE REF TO data,
         lt_rows      TYPE lvc_t_row,
         lt_fcat      TYPE lvc_t_fcat.

    FIELD-SYMBOLS:<lt_upd>  TYPE table,
                  <ls_data> TYPE any.

    IF zcl_cn_abap_util=>check_gui_info( ) IS INITIAL.  "WebGui
      MESSAGE s000(zcn00) WITH TEXT-e10 DISPLAY LIKE 'E'.  "웹화면에서는 업로드가 불가합니다.
      EXIT.
    ENDIF.

    IF iv_filename IS INITIAL.
*------------------------------
* Open File
*------------------------------
      file_open_dialog( EXPORTING iv_default_extension = 'XLSX'
                        IMPORTING ev_fullpath  = lv_filename ).

      CHECK lv_filename IS NOT INITIAL.

*-----------------------------
* 삭제 Confirm Popup Message
*-----------------------------
      IF iv_no_pop IS INITIAL AND as_toolbtn-exlu_no_pop IS INITIAL.

        DATA(lv_ans) =  me->pop_to_msg(  iv_title = zcl_cn_alv_grid=>ac_msg_title3   "삭제확인
                                         iv_text1 = zcl_cn_alv_grid=>ac_msg_upd1     "삭제 후 Upload 하시겠습니까?
                                         iv_text2 = zcl_cn_alv_grid=>ac_msg_upd2
                                         iv_cancel_button    = 'X' ).  "Yes:삭제  No:Append  Cancel

        CHECK lv_ans <> 'C'.  "Cancel 이 아닐경우
      ENDIF.

      IF it_fcat[] IS INITIAL.
*        lt_fcat = at_fcat.
        me->get_frontend_fieldcatalog( IMPORTING et_fieldcatalog =  lt_fcat ).
      ELSE.
        lt_fcat = it_fcat.
      ENDIF.

      DELETE lt_fcat WHERE no_out    = 'X'
                        OR tech      = 'X'
                        OR fieldname = zcn00_statu.

    ELSE.
*------------------------------------------------
* 외부호출
*------------------------------------------------
      lt_fcat = me->get_fcat( it_table = ct_data ).

*      GET REFERENCE OF ct_data INTO ao_dataref.
      lv_filename = iv_filename.
      lv_exit = 'X'.
    ENDIF.

    GET REFERENCE OF ct_data INTO lo_dataref.

*------------------------------------------------
* 개별호출
*------------------------------------------------
    me->on_set_upl_fcat(  EXPORTING iv_name = me->m_name
                          CHANGING  ct_fcat = lt_fcat ).


*------------------------------
* Create Excel Internal Table
*------------------------------
    me->crt_dyn_table( EXPORTING it_fcat     = lt_fcat
                                 iv_alv_comm = 'X'
                       IMPORTING eo_dynt     = lo_data ).

    CHECK lo_data IS BOUND.

    ASSIGN lo_data->* TO <lt_upd>.
    CHECK sy-subrc = 0.

*------------------------------
* Get Upload Excel Data..
*------------------------------
    DATA(lv_sapgui) = zcl_cn_abap_util=>check_gui_info( ).

    me->xml_upload( EXPORTING iv_filename = lv_filename
                              iv_beg_row  = iv_beg_row
                              iv_beg_col  = iv_beg_col
                              iv_end_row  = iv_end_row
                              iv_end_col  = iv_end_col
                              iv_sheet_index = iv_sheet_index
                              it_fcat     = lt_fcat
                    IMPORTING ev_subrc    = rv_subrc
                    CHANGING  ct_data     = <lt_upd>  ).

    CHECK rv_subrc IS INITIAL.

    IF lv_exit IS NOT INITIAL AND iv_conv IS INITIAL.
      MOVE-CORRESPONDING <lt_upd> TO ct_data.
      EXIT.
    ENDIF.

    CHECK <lt_upd> IS NOT INITIAL.

*-----------------------------------
* Move Excel Data to Display Data..
*-----------------------------------
    IF lv_ans = 'X' OR as_toolbtn-exlu_del = 'X' OR iv_del_data = 'X'.  "삭제

*----------------------------------
* 삭제 수행
*----------------------------------
      DESCRIBE TABLE ct_data LINES DATA(lv_lines).
      DO lv_lines TIMES.
        lt_rows = VALUE #( BASE lt_rows ( index = sy-index ) ).
      ENDDO.
      me->delete_data_row( EXPORTING it_rows = lt_rows ) .

    ENDIF.

*-------------------------------
* Excel Alv Conversion
*-------------------------------
    me->conv_upload_data(  EXPORTING it_fcat  = lt_fcat
                                     it_dftvl = it_dftvl
                                     it_upd   = <lt_upd>
                           CHANGING  ct_data  = ct_data ).

*-------------------------------
* Excel Check Value (개별호출)
*-------------------------------
    "내부호출시 사용
    on_chk_upload(  EXPORTING iv_name = me->m_name
                    CHANGING  cp_data = lo_dataref ).

    "업로드 외부 호출시 사용
    on_chk_upload_data(  EXPORTING iv_name = me->m_name
                         CHANGING  ct_data = ct_data ).

    av_status = 'X'.

    MESSAGE s010(zcn00).   "Excel File을 성공적으로 Upload 했습니다.

  ENDMETHOD.


  METHOD btn_excl_download.
*&---------------------------------------------------------------------*
*& Module              : CN
*& Program ID          : ZCL_CN_ALV_GRID
*& T-CODE              : N/A
*& Referenced Program  : N/A
*& Created by          : T0200335
*& Created On          : 2020.12.23
*& Type                : CLASS
*& Description         : CRT_DYN_TABLE
*&---------------------------------------------------------------------*
*& Change History
*&---------------------------------------------------------------------*
*& 변경SEQ    변경자        변경일자             변경내용
*&---------------------------------------------------------------------*
*&   U01  / KTGT0220021 / 2022.08.18 / SORT,LAYOUT 속성 반영
*&---------------------------------------------------------------------*
*-----------------------------------------------------
* Excel Downloa Method (XML방식)
*-----------------------------------------------------
    CONSTANTS lc_temp_dir TYPE string VALUE 'Z:\'.

    DATA: lv_xml        TYPE xstring,
          lo_dref       TYPE REF TO data,
          lr_tabdescr   TYPE REF TO cl_abap_structdescr,
          lr_data       TYPE REF TO data,

          lt_fieldcat   TYPE lvc_t_fcat,
          ls_fieldcat   TYPE lvc_s_fcat,

          lt_header     TYPE mdg_mdf_ts_field_reptext,
          ls_header     TYPE LINE OF mdg_mdf_ts_field_reptext,

          lv_filename   TYPE string,
          lv_xml_stream TYPE xml_rawdata..

    DATA: lv_default_extension TYPE string,
          lv_initial_directory TYPE string,
          lv_length            TYPE i,
          lv_default_file_name TYPE string,
          lv_mask              TYPE char255,
          lv_mask1             TYPE string.

    FIELD-SYMBOLS:<lt_data> TYPE table.

*--------------------------------------------------------------------*
*-- Set fieldcatalog for Data table
*--------------------------------------------------------------------*
    CREATE DATA lr_data LIKE LINE OF it_data[].

    CLEAR lt_fieldcat.

    IF it_header[] IS NOT INITIAL.
      lt_header[] = it_header[].
    ENDIF.

    lr_tabdescr ?= cl_abap_structdescr=>describe_by_data_ref( lr_data ).
    DATA(lt_dfies) = cl_salv_data_descr=>read_structdescr( lr_tabdescr ).


    LOOP AT lt_dfies INTO DATA(ls_dfies).

      ls_fieldcat = CORRESPONDING lvc_s_fcat( ls_dfies ).

      "- User Define Fieldcatalog
      READ TABLE it_fieldcat INTO DATA(ls_user_fcat)  WITH KEY fieldname = ls_fieldcat-fieldname.
      IF sy-subrc = 0.
        MOVE-CORRESPONDING ls_user_fcat TO ls_fieldcat.
        ls_fieldcat-reptext   = ls_user_fcat-coltext.
        ls_fieldcat-scrtext_m = ls_user_fcat-reptext.
        ls_fieldcat-scrtext_l = ls_user_fcat-reptext.
        ls_fieldcat-scrtext_s = ls_user_fcat-reptext.
      ENDIF.

      "- User Define HederText
      IF lt_header IS NOT INITIAL.
        READ TABLE lt_header INTO ls_header
        WITH KEY fieldname = ls_fieldcat-fieldname BINARY SEARCH.
        IF sy-subrc  = 0.
          ls_fieldcat-reptext   = ls_header-reptext.
          ls_fieldcat-coltext   = ls_header-reptext.
          ls_fieldcat-scrtext_m = ls_header-reptext.
          ls_fieldcat-scrtext_l = ls_header-reptext.
          ls_fieldcat-scrtext_s = ls_header-reptext.
        ENDIF.
      ENDIF.

      CASE ls_fieldcat-edit_mask(2).
        WHEN '=='.
          DATA(lv_conv) = ls_fieldcat-edit_mask.
          REPLACE FIRST OCCURRENCE OF '==' IN lv_conv WITH space.
          ls_fieldcat-convexit = lv_conv.
      ENDCASE.
      CASE ls_dfies-fieldname.
        WHEN 'STATU'.
          ls_fieldcat-no_out = 'X'.
      ENDCASE.

      IF ls_fieldcat-checkbox = 'X'.
        CLEAR:ls_fieldcat-checkbox.
      ENDIF.

      APPEND ls_fieldcat TO lt_fieldcat.
      CLEAR : ls_dfies, ls_user_fcat, ls_fieldcat.

    ENDLOOP.

*--------------------------------------------------------------------*
*-- Changing Fieldcat ..
*--------------------------------------------------------------------*
    me->on_set_dnl_data( EXPORTING iv_name = me->m_name
                         CHANGING  ct_data  = it_data ).

*--------------------------------------------------------------------*
*-- Changing Fieldcat ..
*--------------------------------------------------------------------*
    me->on_set_dnl_fcat( EXPORTING iv_name = me->m_name
                         CHANGING  ct_fcat  = lt_fieldcat ).



*--------------------------------------------------------------------*
*-- Set Result Table
*--------------------------------------------------------------------*
    GET REFERENCE OF it_data[] INTO lo_dref.
    TRY .
        DATA(lrf_result_data) = cl_salv_ex_util=>factory_result_data_table(
                                r_data                      = lo_dref
                                s_layout                    = is_layout "U01 ADD
                                t_sort                      = it_sort   "U01 ADD
                                t_fieldcatalog              = lt_fieldcat[] ).
      CATCH cx_sy_dyn_call_illegal_type.
        EXIT.
    ENDTRY.


*--------------------------------------------------------------------*
*-- Transform result data to XML data
*--------------------------------------------------------------------*
    CALL METHOD cl_salv_bs_tt_util=>if_salv_bs_tt_util~transform
      EXPORTING
        xml_type      = '10'
        xml_version   = cl_salv_bs_a_xml_base=>get_version( )
        r_result_data = lrf_result_data
        xml_flavour   = if_salv_bs_c_tt=>c_tt_xml_flavour_export
        gui_type      = if_salv_bs_xml=>c_gui_type_gui
      IMPORTING
        xml           = lv_xml.

*--------------------------------------------------------------------*
*-- Check available Speadsheet Formats
*--------------------------------------------------------------------*
    DATA(lt_xml_choice) = cl_salv_export_xml_dialog=>get_gui_spreadsheet_formats(  ).
    IF lt_xml_choice IS NOT INITIAL.
      DATA(ls_xml_choice) = lt_xml_choice[ xml_type = '10' ].
    ENDIF.

    CHECK ls_xml_choice IS NOT INITIAL.

*--------------------------------------------------------------------*
* Filedownload
*--------------------------------------------------------------------*

*-- Get default extension
    CALL METHOD cl_alv_bds=>create_mask_for_filefilter
      EXPORTING
        i_frontend          = ls_xml_choice-frontend
      IMPORTING
        e_default_extension = lv_default_extension
      CHANGING
        c_mask              = lv_mask.

    lv_mask1 = lv_mask.

    DATA: lv_application TYPE string.
    IF cl_salv_gui_environment_info=>s_gui_type-name EQ cl_salv_gui_environment_info=>cs_gui_type-windows.

      lv_application = cl_salv_bs_xml_utils=>get_pc_application(
                                          ls_xml_choice-frontend ).
    ELSE.
      IF ls_xml_choice-frontend EQ cl_alv_bds=>mc_mhtml_frontend.
        lv_default_extension = cl_alv_bds=>mc_excel_extension.
        lv_mask1 = TEXT-r05.  "'Excel (*.XLS)|*.XLS'.
      ENDIF.
    ENDIF.

*------------------------------
*-- File full path
*------------------------------
    IF iv_filename IS NOT INITIAL.
      lv_default_file_name =  iv_filename && '.' && lv_default_extension.
    ELSE.
      lv_default_file_name = sy-cprog && lv_default_extension.
    ENDIF.

*--------------------------------
*--  Download File
*--------------------------------
    IF cl_gui_object=>www_active IS INITIAL.

      CALL FUNCTION 'XML_EXPORT_DIALOG'
        EXPORTING
          i_xml                      = lv_xml
          i_default_extension        = lv_default_extension
          i_initial_directory        = lv_initial_directory
          i_default_file_name        = lv_default_file_name
          i_mask                     = lv_mask1
          i_application              = lv_application
        EXCEPTIONS
          application_not_executable = 1
          OTHERS                     = 2.

    ELSE.

      lv_filename = lc_temp_dir && lv_default_file_name.

      CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
        EXPORTING
          buffer        = lv_xml
        IMPORTING
          output_length = lv_length
        TABLES
          binary_tab    = lv_xml_stream.

      CALL METHOD cl_gui_frontend_services=>gui_download
        EXPORTING
          bin_filesize = lv_length
          filetype     = 'BIN'
          filename     = lv_filename
        CHANGING
          data_tab     = lv_xml_stream
        EXCEPTIONS
          OTHERS       = 1.

    ENDIF.


  ENDMETHOD.


  METHOD btn_del_row.
*&---------------------------------------------------------------------*
*& Module              : CN
*& Program ID          : ZCL_CN_ALV_GRID
*& T-CODE              : N/A
*& Referenced Program  : N/A
*& Created by          : T0200335
*& Created On          : 2020.12.23
*& Type                : CLASS
*& Description         : CRT_DYN_TABLE
*&---------------------------------------------------------------------*
*& Change History
*&---------------------------------------------------------------------*
*& 변경SEQ    변경자        변경일자             변경내용
*&---------------------------------------------------------------------*
*-------------------------------------------------------------------------------------------------------
* 라인을 삭제하는 Method
* 일반적으로 자동으로 호출 되어져 라인삭제
* 단, 개별에서 호출될 경우도 있음 (특수 필드의 조건에 따라 삭제불가 되어져야 하므로)
* 참조프로그램:YALV2
*-------------------------------------------------------------------------------------------------------
    DATA : lt_rows  TYPE lvc_t_row.

    FIELD-SYMBOLS:<lt_data> TYPE table.

    ASSIGN ao_dataref->* TO <lt_data>.

    CHECK sy-subrc = 0.

*--------------------------------
* 삭제하기 위하여 선택한 줄
*--------------------------------
    CALL METHOD me->get_selected_rows
      IMPORTING
        et_index_rows = lt_rows.


    CALL METHOD cl_gui_cfw=>flush.

*----------------------------------
* 선택된 대상이 없을 경우 에러
*----------------------------------
    IF lt_rows[] IS INITIAL.
      MESSAGE s005(zcn00) DISPLAY LIKE 'E'.     "라인을 선택해 주십시오.
      EXIT.
    ENDIF.

    LOOP AT lt_rows INTO DATA(ls_rows).

      READ TABLE <lt_data> ASSIGNING FIELD-SYMBOL(<ls_data>) INDEX ls_rows-index.
      CHECK <ls_data> IS ASSIGNED.

     "삭제시 삭제 여부 체크
      DATA(lv_check) = me->on_chk_del_row( EXPORTING iv_name = me->m_name
                                           CHANGING cs_data  = <ls_data> ).

      IF lv_check = 'X'.
        RETURN.
      ENDIF.

      ASSIGN COMPONENT 'ZDELE' OF STRUCTURE <ls_data> TO FIELD-SYMBOL(<lv_del>).
      CHECK sy-subrc = 0 AND <lv_del> IS NOT INITIAL .
      MESSAGE s000(zcn00) WITH TEXT-e03 DISPLAY LIKE 'E'.
      RETURN.
    ENDLOOP.


*---------------------------
* 삭제 Confirm..
*---------------------------
    CASE me->pop_to_msg( iv_title = zcl_cn_alv_grid=>ac_msg_title3   "삭제확인
                         iv_text1 = zcl_cn_alv_grid=>ac_msg_del1     "선택된 데이타를 삭제하려고 합니다.'
                         iv_text2 = zcl_cn_alv_grid=>ac_msg_del2 ).  "'삭제하시겠습니까?'
      WHEN abap_true.
      WHEN OTHERS.
        EXIT.
    ENDCASE.


*----------------------------------
* 삭제 수행
*----------------------------------
    me->delete_data_row( EXPORTING it_rows = lt_rows ) .


  ENDMETHOD.


  METHOD btn_add_row.
*&---------------------------------------------------------------------*
*& Module              : CN
*& Program ID          : ZCL_CN_ALV_GRID
*& T-CODE              : N/A
*& Referenced Program  : N/A
*& Created by          : T0200335
*& Created On          : 2020.12.23
*& Type                : CLASS
*& Description         : CRT_DYN_TABLE
*&---------------------------------------------------------------------*
*& Change History
*&---------------------------------------------------------------------*
*& 변경SEQ    변경자        변경일자             변경내용
*&---------------------------------------------------------------------**-------------------------------------------------------------------------------------------------------
* Line Insert Method
* 일반적으로 자동으로 호출 되어져 라인추가 생성
* 단, 개별에서 호출될 경우도 있음 (Parent & child 일 경우 Default Field가 달라지므로 개별에서 호출 가능)
* 참조프로그램:YALV2
*-------------------------------------------------------------------------------------------------------
    DATA:lo_data      TYPE REF TO data,
         lv_row       TYPE i,
         lt_dftvl     TYPE tt_field,
         ls_timestamp TYPE zscn_timestamp.

    DATA: ls_row_id TYPE lvc_s_row,
          ls_col_id TYPE lvc_s_col,
          ls_row_no TYPE lvc_s_roid.

    FIELD-SYMBOLS:<lt_data> TYPE table.

    CLEAR:at_dftvl_add.  "추가 Default는 상위Header 별로 달라지므로 Clear..

    ASSIGN ao_dataref->* TO <lt_data>.

    CREATE DATA lo_data LIKE LINE OF <lt_data>.
    ASSIGN lo_data->* TO FIELD-SYMBOL(<ls_data>).
    CHECK sy-subrc = 0.

    lt_dftvl = at_dftvl .
*---------------------------------
* Set DCFLAG/Status..
*---------------------------------
    ASSIGN COMPONENT 'DCFLG' OF STRUCTURE <ls_data> TO FIELD-SYMBOL(<lv_dcflg>).
    CHECK sy-subrc = 0.

    <lv_dcflg> = 'C'. "Create

    ASSIGN COMPONENT 'STATU' OF STRUCTURE <ls_data> TO FIELD-SYMBOL(<lv_statu>).
    CHECK sy-subrc = 0.

    <lv_statu> = me->set_icon( 'C' ). "Create "신규

*---------------------------------
* Set Timestamp..
*---------------------------------
    zcl_cn_alv_grid=>set_timestamp( EXPORTING iv_create = 'X'
                                     CHANGING  cs_data   = ls_timestamp ).

    MOVE-CORRESPONDING ls_timestamp TO <ls_data>.

*---------------------------------
* Set Default Field..
*---------------------------------
    APPEND LINES OF it_dftvl TO :lt_dftvl,
                                 at_dftvl_add .

    LOOP AT lt_dftvl INTO DATA(ls_field).
      ASSIGN COMPONENT ls_field-fieldname OF STRUCTURE <ls_data> TO FIELD-SYMBOL(<lv_val>).
      CHECK sy-subrc = 0.
      <lv_val> = ls_field-value.
    ENDLOOP.

*-----------------------
* Row Count 설정
*-----------------------
    IF iv_multi = 'X'.
      lv_row = as_toolbtn-mlti_lines.
    ELSE.
      lv_row = 1.
    ENDIF.

*------------------------
* Current Row For Insert
*------------------------
    me->get_current_cell( IMPORTING e_row = DATA(lv_index) ).

    IF lv_index IS INITIAL.
      lv_index = 1.
    ENDIF.

    IF iv_index IS NOT INITIAL.   "User 가 지정한 라인에 넣고 싶을 경우
      lv_index = iv_index.
    ENDIF.

*--------------------------------------
* On Set Add Row Value
*--------------------------------------

*--------------------------------------
* Inser Row..
*--------------------------------------
    DO lv_row TIMES.

      me->on_set_add_row_value( EXPORTING iv_name = me->m_name
                                CHANGING cs_data  = <ls_data> ).

      INSERT <ls_data> INTO <lt_data> INDEX lv_index.

    ENDDO.

*---------------------------
*-- Set Currented Cell
*---------------------------
    ls_row_id-index = lv_index .
    me->set_current_cell_via_id( is_row_id    = ls_row_id
                                 is_column_id = ls_col_id
                                 is_row_no    = ls_row_no ).

*-- Set Changed Status
    av_status = 'X'.

  ENDMETHOD.


  METHOD APPEND_F4_FIELD.
*&---------------------------------------------------------------------*
*& Module      : CN
*& 생성자      : T0200335
*& 생성일      :  2020.12.30
*& Description :
*&---------------------------------------------------------------------*
*& Change History
*&---------------------------------------------------------------------*
*& 변경SEQ    변경자        변경일자             변경내용
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*

    DEFINE _l_append_f4.
      CLEAR ls_lvc_f4.
      ls_lvc_f4-fieldname  = &1.
      ls_lvc_f4-register   = 'X'.
      ls_lvc_f4-getbefore  = ' '.
      ls_lvc_f4-chngeafter = 'X'.
      INSERT ls_lvc_f4 INTO TABLE lt_lvc_f4.
    END-OF-DEFINITION.

    DATA:lt_field TYPE tt_field.

    DATA : ls_lvc_f4 TYPE lvc_s_f4,
           lt_lvc_f4 TYPE lvc_t_f4.

    on_set_f4( EXPORTING iv_name = me->m_name
               CHANGING  ct_field = lt_field ).

    LOOP AT lt_field INTO DATA(ls_field).
      _l_append_f4:ls_field-fieldname.
    ENDLOOP.

    me->register_f4_for_fields( it_f4 = lt_lvc_f4[] ).

  ENDMETHOD.


  METHOD alv_exclud_std_toolbar.
*&---------------------------------------------------------------------*
*& Module      : CN
*& 생성자      : T0200335
*& 생성일      :  2020.12.30
*& Description :
*&---------------------------------------------------------------------*
*& Change History
*&---------------------------------------------------------------------*
*& 변경SEQ    변경자        변경일자             변경내용
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*

*------------------------------------------------------------------------------
* ALV 표준 Toolbar 세팅
* Method SEG_GRID 시 자동으로 호출되서 생성되어짐
*------------------------------------------------------------------------------

    DEFINE _l_set_toolbar.

      ls_exclude = &1.
      IF ls_exclude EQ cl_gui_alv_grid=>mc_fc_excl_all.
      APPEND ls_exclude TO lt_exclude.
      ELSE.
      DELETE TABLE lt_exclude FROM ls_exclude.
      ENDIF.

    END-OF-DEFINITION.

    DATA: lt_exclude TYPE ui_functions,
          lt_user_ex TYPE ui_functions,
          ls_exclude TYPE ui_func.

*------------------------------------------
* Prepare All Toolbar
*------------------------------------------
    lt_exclude = VALUE ui_functions( ( cl_gui_alv_grid=>mc_fc_detail                    )
                                   ( cl_gui_alv_grid=>mc_fc_check                     )
                                   ( cl_gui_alv_grid=>mc_fc_refresh                   )
                                   ( cl_gui_alv_grid=>mc_fc_loc_cut                   )
                                   ( cl_gui_alv_grid=>mc_fc_loc_copy                  )
                                   ( cl_gui_alv_grid=>mc_fc_loc_paste                 )
                                   ( cl_gui_alv_grid=>mc_fc_loc_paste_new_row         )
                                   ( cl_gui_alv_grid=>mc_fc_loc_undo                  )
                                   ( cl_gui_alv_grid=>mc_fc_loc_paste                 )
                                   ( cl_gui_alv_grid=>mc_fc_loc_append_row            )
                                   ( cl_gui_alv_grid=>mc_fc_loc_insert_row            )
                                   ( cl_gui_alv_grid=>mc_fc_loc_delete_row            )
                                   ( cl_gui_alv_grid=>mc_fc_loc_copy_row              )
                                   ( cl_gui_alv_grid=>mc_fc_sort_asc                  )
                                   ( cl_gui_alv_grid=>mc_fc_sort_dsc                  )
                                   ( cl_gui_alv_grid=>mc_fc_find                      )
                                   ( cl_gui_alv_grid=>mc_fc_filter                    )
                                   ( cl_gui_alv_grid=>mc_fc_sum                       )
                                   ( cl_gui_alv_grid=>mc_fc_average                   )
                                   ( cl_gui_alv_grid=>mc_fc_minimum                   )
                                   ( cl_gui_alv_grid=>mc_fc_maximum                   )
                                   ( cl_gui_alv_grid=>mc_fc_subtot                    )
                                   ( cl_gui_alv_grid=>mc_fc_print                     )
                                   ( cl_gui_alv_grid=>mc_fc_views                     )
                                   ( cl_gui_alv_grid=>mc_fc_view_grid                 )
                                   ( cl_gui_alv_grid=>mc_fc_view_excel                )
                                   ( cl_gui_alv_grid=>mc_fc_view_crystal              )
                                   ( cl_gui_alv_grid=>mc_fc_call_xxl                  )
                                   ( cl_gui_alv_grid=>mc_fc_word_processor            )
                                   ( cl_gui_alv_grid=>mc_fc_pc_file                   )
                                   ( cl_gui_alv_grid=>mc_fc_send                      )
                                   ( cl_gui_alv_grid=>mc_fc_to_office                 )
                                   ( cl_gui_alv_grid=>mc_fc_call_abc                  )
                                   ( cl_gui_alv_grid=>mc_fc_expcrdesig                )
                                   ( cl_gui_alv_grid=>mc_fc_expcrtempl                )
                                   ( cl_gui_alv_grid=>mc_fc_html                      )
                                   ( cl_gui_alv_grid=>mc_fc_url_copy_to_clipboard     )
                                   ( cl_gui_alv_grid=>mc_fc_current_variant           )
                                   ( cl_gui_alv_grid=>mc_fc_load_variant              )
                                   ( cl_gui_alv_grid=>mc_fc_maintain_variant          )
                                   ( cl_gui_alv_grid=>mc_fc_save_variant              )
                                   ( cl_gui_alv_grid=>mc_fc_variant_admin             )
                                   ( cl_gui_alv_grid=>mc_fc_graph                     )
*                                  ( cl_gui_alv_grid=>cl_gui_alv_grid=>mc_fc_excl_all )
                                   ( cl_gui_alv_grid=>mc_fc_info                      ) ).

    SORT lt_exclude.

*------------------------------------------
* Set Show Toolbar
*------------------------------------------
    _l_set_toolbar: cl_gui_alv_grid=>mc_fc_sort_asc,
                    cl_gui_alv_grid=>mc_fc_sort_dsc,
                    cl_gui_alv_grid=>mc_fc_find,
                    cl_gui_alv_grid=>mc_fc_filter,
                    cl_gui_alv_grid=>mc_fc_detail,
*                    cl_gui_alv_grid=>mc_fc_loc_copy,
*                    cl_gui_alv_grid=>mc_fc_call_xxl,
                    cl_gui_alv_grid=>mc_fc_average,
                    cl_gui_alv_grid=>mc_fc_sum,
*                    cl_gui_alv_grid=>mc_fc_minimum,
*                    cl_gui_alv_grid=>mc_fc_maximum,
                    cl_gui_alv_grid=>mc_fc_subtot,
                    cl_gui_alv_grid=>mc_fc_current_variant,
                    cl_gui_alv_grid=>mc_fc_load_variant,
                    cl_gui_alv_grid=>mc_fc_maintain_variant,
                    cl_gui_alv_grid=>mc_fc_save_variant,
                    cl_gui_alv_grid=>mc_fc_variant_admin.

    clear:at_ex_toolbar.

    at_ex_toolbar = lt_exclude.

*--------------------------------------
*-- 제거하고자 하는 User Set Toolbar
*--------------------------------------
    me->on_exclud_std_toolbar( EXPORTING iv_name       = me->m_name
                               CHANGING  ct_ex_toolbar = lt_user_ex ).


    APPEND LINES OF lt_user_ex TO at_ex_toolbar.

  ENDMETHOD.


  METHOD DISPLAY_GRID.
*&---------------------------------------------------------------------*
*& Module              : CN
*& Program ID          : ZCL_CN_ALV_GRID
*& T-CODE              : N/A
*& Referenced Program  : N/A
*& Created by          : T0200335
*& Created On          : 2020.12.23
*& Type                : CLASS
*& Description         : CRT_DYN_TABLE
*&---------------------------------------------------------------------*
*& Change History
*&---------------------------------------------------------------------*
*& 변경SEQ    변경자        변경일자             변경내용
*&---------------------------------------------------------------------*
    DATA : lv_save    TYPE char01,
           ls_variant TYPE disvariant,
           lv_default TYPE char01.

*---------------------------
* On Save Variant ..
*---------------------------
    MOVE-CORRESPONDING is_variant TO ls_variant.

    me->on_save_variant(  EXPORTING iv_name    = me->m_name
                          CHANGING  cs_variant = ls_variant
                                    cv_save    = lv_save
                                    cv_default = lv_default ).
*---------------------------
* Dispaly ALv Grid..
*---------------------------
    CALL METHOD me->set_table_for_first_display
      EXPORTING
        i_bypassing_buffer            = abap_true
        i_save                        = lv_save  "Save Manage Layout
        i_default                     = lv_default
        is_variant                    = ls_variant
        is_layout                     = as_layout
        it_toolbar_excluding          = at_ex_toolbar
      CHANGING
        it_outtab                     = ct_data
        it_fieldcatalog               = at_fcat
        it_sort                       = at_sort
        it_filter                     = at_filter
      EXCEPTIONS
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        OTHERS                        = 4.

  ENDMETHOD.


  METHOD get_exe_cells_value.
*&---------------------------------------------------------------------*
*& Module      : CN
*& 생성자: .T0200335
*& 생성일:  2020.12.30
*& Description : GET_EXCEL_CELLS
*&---------------------------------------------------------------------*
*& Change History
*&---------------------------------------------------------------------*
*& 변경SEQ    변경자        변경일자             변경내용
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*

    CONSTANTS:lc_dot VALUE '.'.

    DATA:
*===============================================
      "xlsx
*===============================================
      lv_xstring             TYPE xstring,
      lrf_xlsx               TYPE REF TO cl_xlsx_document,
      lrf_workbook           TYPE REF TO cl_xlsx_workbookpart,
      lrf_collection         TYPE REF TO cl_openxml_partcollection,
      lrf_worksheet          TYPE REF TO cl_xlsx_worksheetpart,
      lrf_shared_st          TYPE REF TO cl_xlsx_sharedstringspart,
      lrf_style              TYPE REF TO cl_xlsx_stylespart,

      "ixml
      lrf_ixml_factory       TYPE REF TO if_ixml,
      lrf_streamfactory      TYPE REF TO if_ixml_stream_factory,
      lrf_stream             TYPE REF TO if_ixml_istream,
      lrf_document           TYPE REF TO if_ixml_document,
      lrf_parser             TYPE REF TO if_ixml_parser,
      lrf_node               TYPE REF TO if_ixml_node,
      lrf_node_r             TYPE REF TO if_ixml_node,
      lrf_att_child          TYPE REF TO if_ixml_node,
      lrf_ref_ixml_elem      TYPE REF TO if_ixml_element,
      lrf_nodes              TYPE REF TO if_ixml_node_collection,
      lrf_node_iterator      TYPE REF TO if_ixml_node_iterator,
      lrf_node_iterator_r    TYPE REF TO if_ixml_node_iterator,
      lrf_att                TYPE REF TO if_ixml_named_node_map,
*===============================================
      "paser error
      "Openxml itab
*===============================================
      ls_xml_itab            TYPE ty_xml_itab,
      lt_xml_sheet_itab      TYPE ty_i_xml_sheet_itab,
      ls_xml_sheet           TYPE ty_xml_sheet_itab,
      ls_xml_shared_str      TYPE ty_xml_shared_str_itab,
      lt_xml_shared_str_itab TYPE ty_i_xml_shared_str_itab,
      lv_row_str             TYPE string.

*===============================================
    "log
*===============================================
    DATA lt_xml_itab TYPE  ty_i_xml_itab .
    DATA lv_max_row TYPE i.

**************************************************************************
    TYPES:
*---------------------------------------------------------------------
*          "* protected components of class ZCL_XLSX_SPREADSHEET
*          "* do not include other source files here!!!
*---------------------------------------------------------------------
      BEGIN OF lty_ooxml_worksheet.
    TYPES name        TYPE string.
    TYPES id          TYPE string.
    TYPES location    TYPE string.
    TYPES END OF lty_ooxml_worksheet .
    TYPES:
      lty_ooxml_worksheets TYPE STANDARD TABLE OF lty_ooxml_worksheet .

    DATA: lv_workbook_xml     TYPE xstring.
    DATA: lt_ooxml_worksheets TYPE lty_ooxml_worksheets.
    DATA: ls_df1904           TYPE string.


*-----------------------------------------
*-- Read the content of XLSX file
*-----------------------------------------
    TRY.

        DATA(lv_invalid) = me->check_drm( EXPORTING iv_filename = iv_filename
                                          IMPORTING ev_subrc    = ev_subrc ).
        IF ev_subrc IS NOT INITIAL.
          CASE ev_subrc.
            WHEN 12.
              MESSAGE s000(zcn00) WITH TEXT-e06 DISPLAY LIKE 'E'. "Upload 파일이 Open 되어 데이타를 Upload 하지 못했습니다.(폴더명/파일명:한글확인)
            WHEN OTHERS.
              IF lv_invalid IS NOT INITIAL.
                MESSAGE s001(zcn00) WITH TEXT-e01 DISPLAY LIKE 'E'. "DRM 문서(암호화)
              ENDIF.
          ENDCASE.
          EXIT.
        ENDIF.

        lv_xstring     = cl_openxml_helper=>load_local_file( im_file_name = iv_filename ).


      CATCH cx_root INTO DATA(lo_root) .
        DATA(lv_msg) = lo_root->get_text( ).
        ev_subrc = 1.
        MESSAGE s001(zcn00) WITH lv_msg DISPLAY LIKE 'E'.
        EXIT.
    ENDTRY.


    CHECK lv_xstring IS NOT INITIAL.
    TRY.
        lrf_xlsx = cl_xlsx_document=>load_document( lv_xstring ).
      CATCH cx_openxml_format.

    ENDTRY.


    IF lrf_xlsx IS NOT  BOUND.
      ev_subrc = 9.
      MESSAGE s000(zcn00) WITH TEXT-e07 DISPLAY LIKE 'E'. "엑셀파일 형식을 확인 부탁드립니다(XLSX만 가능합니다)
      EXIT.
    ENDIF.

    TRY.
        lrf_workbook    = lrf_xlsx->get_workbookpart( ).
        lrf_collection  = lrf_workbook->get_worksheetparts( ).

        TRY.

            lv_workbook_xml =  lrf_workbook->get_data( ).

*---------------------------------------------------------------------
            "Get WorkSheet List
*---------------------------------------------------------------------
            CALL TRANSFORMATION xl_get_worksheets
                SOURCE XML lv_workbook_xml
                RESULT worksheets = lt_ooxml_worksheets.

*---------------------------------------------------------------------
            "Get Date Format
*---------------------------------------------------------------------
            CALL TRANSFORMATION xl_get_date_format
              SOURCE XML lv_workbook_xml
              RESULT dateformat_1904 = ls_df1904.

            IF ls_df1904 = '1'.
              me->ac_dateformat1904 = abap_true.
            ELSE.
              me->ac_dateformat1904 = abap_false.
            ENDIF.

          CATCH cx_ofi_doc cx_root.
*---------------------------------------------------------------------
            "data msg_string type string.
            "msg_string = 'File  ' && workbook_folder && workbook_filename && ' not found in package'.
            "message msg_string type 'I' display like 'E'.
*            MESSAGE E009(XL_INTG_CORE) WITH WORKBOOK_FILENAME WORKBOOK_FOLDER.
*---------------------------------------------------------------------
        ENDTRY.

        IF lt_ooxml_worksheets IS INITIAL.
          EXIT.
        ENDIF.

*---------------------------------------------------------------------
        "Get Selected Sheet !!!
*---------------------------------------------------------------------
        READ TABLE lt_ooxml_worksheets INTO DATA(ls_ooxml_worksheets) INDEX iv_sheet_index.


        TRY .
            lrf_worksheet ?= lrf_workbook->get_part_by_id( iv_id = ls_ooxml_worksheets-location ).
          CATCH cx_openxml_not_found. " Part not found
          CATCH cx_openxml_format.    " Not a valid OpenXML/OPC Package

        ENDTRY.


        IF lrf_worksheet IS BOUND.

          lrf_ixml_factory  = cl_ixml=>create( ).
          lrf_streamfactory = lrf_ixml_factory->create_stream_factory( ).
          lrf_stream        = lrf_streamfactory->create_istream_xstring( lrf_worksheet->get_data( ) ).

          lrf_document = lrf_ixml_factory->create_document( ).
          lrf_parser   = lrf_ixml_factory->create_parser( stream_factory = lrf_streamfactory
                                                        istream        = lrf_stream
                                                        document       = lrf_document ).
          IF lrf_parser->parse( ) EQ 0.

            lrf_ref_ixml_elem = lrf_document->get_root_element( ).
            lrf_nodes         = lrf_ref_ixml_elem->get_elements_by_tag_name( name = 'row' ).
            lrf_node_iterator = lrf_nodes->create_iterator( ).
            lrf_node          = lrf_node_iterator->get_next( ).
            WHILE lrf_node IS NOT INITIAL.
              lrf_att       = lrf_node->get_attributes( ).

              lrf_att_child = lrf_att->get_named_item( 'r' ).
              ls_xml_sheet-row = lrf_att_child->get_value( ).

              "node
              lrf_node_iterator_r = lrf_node->get_children( )->create_iterator( ).
              lrf_node_r          = lrf_node_iterator_r->get_next( ).
              WHILE lrf_node_r IS NOT INITIAL.
                lrf_att            = lrf_node_r->get_attributes( ).
                lrf_att_child      = lrf_att->get_named_item( 'r' ).
                ls_xml_sheet-cell = lrf_att_child->get_value( ).

                lrf_att_child = lrf_att->get_named_item( 't' ).
                IF lrf_att_child IS BOUND.
                  ls_xml_sheet-type = lrf_att_child->get_value( ).
                ENDIF.

                lrf_att_child = lrf_att->get_named_item( 's' ).
                IF lrf_att_child IS BOUND.
                  ls_xml_sheet-index = lrf_att_child->get_value( ).
                ENDIF.

                IF lrf_node_r IS BOUND.

*------------------------------------------------------------------------------------
                  "Office Open XML (OOXML) - Spreadsheet Content Overview
                  "Ref.Site :  http://officeopenxml.com/SScontentOverview.php
*------------------------------------------------------------------------------------
                  CASE ls_xml_sheet-type.
                    WHEN space
                      OR 'b'    "for boolean   ( Return Type => 0 or 1 )
                      OR 'd'    "for date      ( Return Type => Type TimeStamp  )
                      OR 'e'    "for error     ( Return Type => Text )
                      OR 'n'    "for number    ( Return Type => Type Numberic  )
                      OR 'str'. "for formula   ( Return Type => Value Text )

                      "When Fomular is exist, Get Only Value.!!!
                      IF lrf_node_r->get_last_child( ) IS BOUND.

                        "Get value of child

                        DATA(lrf_val_list) = lrf_node_r->get_children( )->create_iterator( ).
                        DATA(lrf_val) = lrf_val_list->get_next( ).
                        WHILE lrf_val IS NOT INITIAL.

*                            DATA(lv_val_name) = lrf_val->get_name( ).

                          IF lrf_val->get_name( ) = 'v'.  "Value
                            ls_xml_sheet-value = lrf_val->get_value( ).
*                            ELSE.       "'f'.  "fomular
*                              DATA(LV_VAL) = LRF_VAL->GET_VALUE( ).
                          ENDIF.

                          lrf_val = lrf_val_list->get_next( ).

                        ENDWHILE.

                      ELSE.

                        ls_xml_sheet-value = lrf_node_r->get_value( ).

                      ENDIF.

                      IF ls_xml_sheet-value CA lc_dot .  " Check Exist Dot(.)
                        DATA(lv_val_type) = lrf_node_r->get_type( ).
                        IF lv_val_type = '4'.  "Number Check
                          TRY.

                              DATA(lv_float) = |{ CONV decfloat34( ls_xml_sheet-value ) NUMBER = USER }|.

                              cl_abap_decfloat=>read_decfloat16(
                                EXPORTING
                                  string = lv_float              " Character Format
                                IMPORTING
                                  value  = DATA(lv_fval)
                                  rc     = DATA(lv_rc)           " Return Code
                              ).
                              IF lv_rc = 0 AND lv_fval IS NOT INITIAL.
                                ls_xml_sheet-value = lv_fval.
                              ENDIF.
                            CATCH cx_sy_conversion_overflow.     " System exception for overflow in conversion
                            CATCH cx_abap_decfloat_invalid_char. " Invalid Character when Importing a DECFLOAT Value
                            CATCH cx_abap_decfloat_parse_err.    " Error Importing a DECFLOAT Value
                          ENDTRY.
                        ENDIF.
                      ENDIF.


                    WHEN 's'.  ""for Shared String
                      ls_xml_sheet-index = lrf_node_r->get_value( ).

                    WHEN OTHERS.
                      RAISE invalid_cell_type.

                  ENDCASE.

                  APPEND ls_xml_sheet TO lt_xml_sheet_itab.
                ENDIF.
                CLEAR: ls_xml_sheet-cell,
                       ls_xml_sheet-index,
                       ls_xml_sheet-type,
                       ls_xml_sheet-value.
                lrf_node_r = lrf_node_iterator_r->get_next( ).
              ENDWHILE.
              lrf_node = lrf_node_iterator->get_next( ).
            ENDWHILE.
          ENDIF.
        ENDIF.

        lrf_shared_st  = lrf_workbook->get_sharedstringspart( ).


*-------------------------------------------------------------------------------------
* Excel Style Format
*-------------------------------------------------------------------------------------
        TYPES:BEGIN OF lts_struc_numfmtid.
        TYPES id          TYPE i.
        TYPES formatcode  TYPE string.
        TYPES END OF lts_struc_numfmtid .
        TYPES:lty_numfmtids TYPE STANDARD TABLE OF lts_struc_numfmtid .

        DATA: lt_cellxfs        TYPE STANDARD TABLE OF string,
              lt_numfmtids      TYPE  lty_numfmtids,
              lt_custom_numfmts TYPE  lty_numfmtids.


        DATA ls_format TYPE lts_struc_numfmtid.
        CONSTANTS :
          lc_formatcd1  TYPE string VALUE 'General',
          lc_formatcd2  TYPE string VALUE '0',
          lc_formatcd3  TYPE string VALUE '0.00',
          lc_formatcd4  TYPE string VALUE '#,##0',
          lc_formatcd5  TYPE string VALUE '#,##0.00',
          lc_formatcd6  TYPE string VALUE '0%',
          lc_formatcd7  TYPE string VALUE '0.00%',
          lc_formatcd8  TYPE string VALUE '0.00E+00',
          lc_formatcd9  TYPE string VALUE '# ?/?',
          lc_formatcd10 TYPE string VALUE '# ??/??',
          lc_formatcd11 TYPE string VALUE 'mm-dd-yy',
          lc_formatcd12 TYPE string VALUE 'd-mmm-yy',
          lc_formatcd13 TYPE string VALUE 'd-mmm',
          lc_formatcd14 TYPE string VALUE 'mmm-yy',
          lc_formatcd15 TYPE string VALUE 'h:mm AM/PM',
          lc_formatcd16 TYPE string VALUE 'h:mm:ss AM/PM',
          lc_formatcd17 TYPE string VALUE 'h:mm',
          lc_formatcd18 TYPE string VALUE 'h:mm:ss',
          lc_formatcd19 TYPE string VALUE 'm/d/yy h:mm',
          lc_formatcd20 TYPE string VALUE '#,##0;(#,##0)',
          lc_formatcd21 TYPE string VALUE '#,##0;[Red](#,##0)',
          lc_formatcd22 TYPE string VALUE '#,##0.00;(#,##0.00)',
          lc_formatcd23 TYPE string VALUE '#,##0.00;[Red](#,##0.00)',
          lc_formatcd24 TYPE string VALUE 'mm:ss',
          lc_formatcd25 TYPE string VALUE '[h]:mm:ss',
          lc_formatcd26 TYPE string VALUE 'mmss.0',
          lc_formatcd27 TYPE string VALUE '##0.0E+0',
          lc_formatcd28 TYPE string VALUE '@'.

        CONSTANTS:
          lc_formatcd29 TYPE string VALUE 'mm.dd.yyyy',
          lc_formatcd30 TYPE string VALUE 'yyyy.mm.dd'.
*-------------------------------------------------------------------------------------------------------------
        " Filling all OOXML predifended, language independend number formats !Not a straight numbering!
        " According to 'Office Open XML Part 4 - Markup Language Reference, 3.8.30'
*-------------------------------------------------------------------------------------------------------------
        ls_format-id  = 0.
        ls_format-formatcode = lc_formatcd1. "`General`.
        APPEND ls_format TO lt_numfmtids.
        ls_format-id = 1.
        ls_format-formatcode = lc_formatcd2. "`0`.
        APPEND ls_format TO lt_numfmtids.
        ls_format-id = 2.
        ls_format-formatcode = lc_formatcd3. "`0.00`.
        APPEND ls_format TO lt_numfmtids.
        ls_format-id = 3.
        ls_format-formatcode = lc_formatcd4. "`#,##0`.
        APPEND ls_format TO lt_numfmtids.
        ls_format-id = 4.
        ls_format-formatcode = lc_formatcd5. "`#,##0.00`.
        APPEND ls_format TO lt_numfmtids.
        ls_format-id = 9.
        ls_format-formatcode = lc_formatcd6. "`0%`.
        APPEND ls_format TO lt_numfmtids.
        ls_format-id = 10.
        ls_format-formatcode = lc_formatcd7. "`0.00%`.
        APPEND ls_format TO lt_numfmtids.
        ls_format-id = 11.
        ls_format-formatcode = lc_formatcd8. "`0.00E+00`.
        APPEND ls_format TO lt_numfmtids.
        ls_format-id = 12.
        ls_format-formatcode = lc_formatcd9. "`# ?/?`.
        APPEND ls_format TO lt_numfmtids.
        ls_format-id = 13.
        ls_format-formatcode = lc_formatcd10. "`# ??/??`.
        APPEND ls_format TO lt_numfmtids.
        ls_format-id = 14.
        ls_format-formatcode = lc_formatcd11. "`mm-dd-yy`.
        APPEND ls_format TO lt_numfmtids.
        ls_format-id = 15.
        ls_format-formatcode = lc_formatcd12. "`d-mmm-yy`.
        APPEND ls_format TO lt_numfmtids.
        ls_format-id = 16.
        ls_format-formatcode = lc_formatcd13. "`d-mmm`.
        APPEND ls_format TO lt_numfmtids.
        ls_format-id = 17.
        ls_format-formatcode = lc_formatcd14. "`mmm-yy`.
        APPEND ls_format TO lt_numfmtids.
        ls_format-id = 18.
        ls_format-formatcode = lc_formatcd15. "`h:mm AM/PM`.
        APPEND ls_format TO lt_numfmtids.
        ls_format-id = 19.
        ls_format-formatcode = lc_formatcd16. "`h:mm:ss AM/PM`.
        APPEND ls_format TO lt_numfmtids.
        ls_format-id = 20.
        ls_format-formatcode = lc_formatcd17. "`h:mm`.
        APPEND ls_format TO lt_numfmtids.
        ls_format-id = 21.
        ls_format-formatcode = lc_formatcd18. "`h:mm:ss`.
        APPEND ls_format TO lt_numfmtids.
        ls_format-id = 22.
        ls_format-formatcode = lc_formatcd19. "`m/d/yy h:mm`.
        APPEND ls_format TO lt_numfmtids.
        ls_format-id = 37.
        ls_format-formatcode = lc_formatcd20. "`#,##0;(#,##0)`.
        APPEND ls_format TO lt_numfmtids.
        ls_format-id = 38.
        ls_format-formatcode = lc_formatcd21. "`#,##0;[Red](#,##0)`.
        APPEND ls_format TO lt_numfmtids.
        ls_format-id = 39.
        ls_format-formatcode = lc_formatcd22. "`#,##0.00;(#,##0.00)`.
        APPEND ls_format TO lt_numfmtids.
        ls_format-id = 40.
        ls_format-formatcode = lc_formatcd23. "`#,##0.00;[Red](#,##0.00)`.
        APPEND ls_format TO lt_numfmtids.
        ls_format-id = 45.
        ls_format-formatcode = lc_formatcd24. "`mm:ss`.
        APPEND ls_format TO lt_numfmtids.
        ls_format-id = 46.
        ls_format-formatcode = lc_formatcd25. "`[h]:mm:ss`.
        APPEND ls_format TO lt_numfmtids.
        ls_format-id = 47.
        ls_format-formatcode = lc_formatcd26. "`mmss.0`.
        APPEND ls_format TO lt_numfmtids.
        ls_format-id = 48.
        ls_format-formatcode = lc_formatcd27. "`##0.0E+0`.
        APPEND ls_format TO lt_numfmtids.
        ls_format-id = 49.
        ls_format-formatcode = lc_formatcd28. "`@`.
        APPEND ls_format TO lt_numfmtids.

        ls_format-id = 50.
        ls_format-formatcode = lc_formatcd29. "`@`.
        APPEND ls_format TO lt_numfmtids.

        ls_format-id = 51.
        ls_format-formatcode = lc_formatcd30. "`@`.
        APPEND ls_format TO lt_numfmtids.

        TRY.
            lrf_style = lrf_workbook->get_stylespart( ).
            DATA(lv_styles_xml) = lrf_style->get_data( ).
            CALL TRANSFORMATION xl_get_cellxfs
              SOURCE XML lv_styles_xml
              RESULT numfmids = lt_cellxfs.


            CALL TRANSFORMATION xl_get_numfmtids
              SOURCE XML lv_styles_xml
              RESULT numfmts = lt_custom_numfmts.

          CATCH cx_openxml_not_found cx_openxml_format.
            "Style XML file not found.
        ENDTRY.


        APPEND LINES OF lt_custom_numfmts TO lt_numfmtids.

      CATCH cx_openxml_not_found
            cx_openxml_format.

    ENDTRY.

*-------------------------------------------------------------------------------------------------------------
    "Get Shared String
*-------------------------------------------------------------------------------------------------------------
    IF lrf_shared_st IS BOUND.
      lrf_ixml_factory  = cl_ixml=>create( ).
      lrf_streamfactory = lrf_ixml_factory->create_stream_factory( ).
      lrf_stream        = lrf_streamfactory->create_istream_xstring( lrf_shared_st->get_data( ) ).

      lrf_document      = lrf_ixml_factory->create_document( ).
      lrf_parser        = lrf_ixml_factory->create_parser( stream_factory = lrf_streamfactory
                                                         istream        = lrf_stream
                                                         document       = lrf_document ).
      IF lrf_parser->parse( ) EQ 0.

        lrf_ref_ixml_elem = lrf_document->get_root_element( ).
        lrf_nodes         = lrf_ref_ixml_elem->get_elements_by_tag_name( name = 'si' ).
        lrf_node_iterator = lrf_nodes->create_iterator( ).

        lrf_node = lrf_node_iterator->get_next( ).
        CLEAR sy-tabix.
        WHILE lrf_node IS NOT INITIAL.
          ls_xml_shared_str-index = sy-tabix.
          ls_xml_shared_str-value = lrf_node->get_value( ).
          APPEND ls_xml_shared_str TO lt_xml_shared_str_itab.
          CLEAR ls_xml_shared_str.
          lrf_node = lrf_node_iterator->get_next( ).
        ENDWHILE.
      ENDIF.
    ENDIF.

    SORT:lt_xml_shared_str_itab BY index .
*-------------------------------------------------------------------------------------------------------------
    "Mapping Shared String & Date-Type Convert.
*-------------------------------------------------------------------------------------------------------------
    LOOP AT lt_xml_sheet_itab INTO ls_xml_sheet.
      ls_xml_itab-row  = ls_xml_sheet-row.
      ls_xml_itab-cell = ls_xml_sheet-cell.
      "get column
      ls_xml_itab-column = ls_xml_itab-cell.
      lv_row_str         = ls_xml_itab-row.
      CONDENSE lv_row_str NO-GAPS.
      REPLACE lv_row_str IN ls_xml_itab-column WITH space.

      IF ls_xml_sheet-type EQ 's'.
        READ TABLE lt_xml_shared_str_itab INTO ls_xml_shared_str  WITH KEY index = ls_xml_sheet-index BINARY SEARCH.
        IF sy-subrc EQ 0.
          ls_xml_itab-value = ls_xml_shared_str-value.
        ENDIF.
      ELSE.

        IF ls_xml_sheet-index IS NOT INITIAL.
          READ TABLE lt_cellxfs INTO DATA(lv_style_id) INDEX ( ls_xml_sheet-index + 1 ).
          IF lv_style_id <> 0.
            READ TABLE lt_numfmtids INTO DATA(ls_numfmtid) WITH KEY id = lv_style_id.

            REPLACE REGEX '"[^"]*"' IN ls_numfmtid-formatcode WITH ''.
            REPLACE REGEX '\[Red\]' IN ls_numfmtid-formatcode WITH ''.

*-------------------------------------------------------------------------------------------------------------
            "Check date-type from Format
*-------------------------------------------------------------------------------------------------------------
            IF cl_abap_matcher=>matches( pattern = '.*(y+|m+|d+|h+|s+).*' text = ls_numfmtid-formatcode ) = abap_true.
              "Date Format
              ls_xml_itab-value = me->convert_cell_to_date_time( ls_xml_sheet-value ).
            ENDIF.

          ENDIF.

        ENDIF.

        IF ls_xml_itab-value IS INITIAL.
          ls_xml_itab-value = ls_xml_sheet-value.
        ENDIF.
      ENDIF.
      APPEND ls_xml_itab TO lt_xml_itab.
      CLEAR: ls_xml_sheet,
             ls_xml_shared_str,
             ls_xml_itab.
    ENDLOOP.


    SORT lt_xml_itab BY row DESCENDING.
    CLEAR ls_xml_itab.
    READ TABLE lt_xml_itab INDEX 1 INTO ls_xml_itab.

    lv_max_row =  ls_xml_itab-row.


    et_cells = lt_xml_itab.
  ENDMETHOD.


  method GET_EDIT_MODE.
*&---------------------------------------------------------------------*
*& Module              : CN
*& Program ID          : ZCL_CN_ALV_GRID
*& T-CODE              : N/A
*& Referenced Program  : N/A
*& Created by          : T0200335
*& Created On          : 2020.12.23
*& Type                : CLASS
*& Description         : ZCL_CN_ALV_GRID
*&---------------------------------------------------------------------*
*& Change History
*&---------------------------------------------------------------------*
*& 변경SEQ    변경자        변경일자             변경내용
*&---------------------------------------------------------------------*

    rv_edit_mode = av_edit_mode.

  endmethod.


  method GET_DYNP_PARAM.
    DATA : lt_dynfld TYPE TABLE OF dynpread,
           ls_dynfld TYPE dynpread.

    DATA : ls_param TYPE          rsparams,
           lv_tabix TYPE          sy-tabix.

    DATA: lr_selname TYPE RANGE OF rsscr_name,
          ls_selname LIKE LINE OF lr_selname.

*-------------------------------
* Get Parameter Value
*-------------------------------
    CALL FUNCTION 'RS_REFRESH_FROM_SELECTOPTIONS'
      EXPORTING
        curr_report     = sy-cprog
      TABLES
        selection_table = rt_params
      EXCEPTIONS
        not_found       = 1
        no_report       = 2
        OTHERS          = 3.

    CHECK sy-subrc EQ 0.

    IF iv_dynnr NE '1000'.

      ls_dynfld-fieldname = iv_fname.
      APPEND ls_dynfld TO lt_dynfld.

    ELSE.

      IF iv_fname IS NOT INITIAL.
        DATA(lv_low)   = '*' && iv_fname && '*'.
        lr_selname = VALUE #( ( sign = 'E' option = 'CP' low = lv_low ) ).
        DELETE rt_params WHERE selname IN lr_selname.
      ENDIF.


      LOOP AT rt_params INTO ls_param.
        CASE ls_param-kind.
          WHEN 'P'.
            lt_dynfld = VALUE #( BASE lt_dynfld ( fieldname = ls_param-selname ) ).
          WHEN 'S'.
            lt_dynfld = VALUE #( BASE lt_dynfld ( fieldname = ls_param-selname && '-LOW' )
                                            ( fieldname = ls_param-selname && '-HIGH' ) ).
        ENDCASE.
      ENDLOOP.

    ENDIF.

    CHECK lt_dynfld IS NOT INITIAL.

    CALL FUNCTION 'DYNP_VALUES_READ'
      EXPORTING
        dyname               = sy-cprog
        dynumb               = iv_dynnr
      TABLES
        dynpfields           = lt_dynfld
      EXCEPTIONS
        invalid_abapworkarea = 1
        invalid_dynprofield  = 2
        invalid_dynproname   = 3
        invalid_dynpronummer = 4
        invalid_request      = 5
        no_fielddescription  = 6
        invalid_parameter    = 7
        undefind_error       = 8
        double_conversion    = 9
        stepl_not_found      = 10
        OTHERS               = 11.

    CHECK sy-subrc EQ 0.

    SORT lt_dynfld BY fieldname.

    LOOP AT rt_params ASSIGNING FIELD-SYMBOL(<ls_param>).
      lv_tabix = sy-tabix.
      CASE <ls_param>-kind.
        WHEN 'P'.
          READ TABLE lt_dynfld INTO ls_dynfld WITH KEY fieldname = <ls_param>-selname BINARY SEARCH.
          IF sy-subrc = 0 AND
            <ls_param>-low <> ls_dynfld-fieldvalue.
            <ls_param>-low = ls_dynfld-fieldvalue.
          ENDIF.
        WHEN 'S'.
          DATA(lv_fieldname) = <ls_param>-selname && '-LOW'.
          READ TABLE lt_dynfld INTO ls_dynfld WITH KEY fieldname = lv_fieldname BINARY SEARCH.
          IF sy-subrc = 0 AND <ls_param>-low <> ls_dynfld-fieldvalue.
            <ls_param>-low = ls_dynfld-fieldvalue.
          ENDIF.

          lv_fieldname = <ls_param>-selname && '-HIGH'.
          READ TABLE lt_dynfld INTO ls_dynfld WITH KEY fieldname = lv_fieldname BINARY SEARCH.
          IF sy-subrc = 0 AND <ls_param>-high <> ls_dynfld-fieldvalue.
            <ls_param>-high = ls_dynfld-fieldvalue.
          ENDIF.
      ENDCASE.

    ENDLOOP.
  endmethod.


  METHOD get_attatch_info.
*&---------------------------------------------------------------------*
*& Module              : CN
*& Program ID          : ZCL_CN_ALV_GRID
*& T-CODE              : N/A
*& Referenced Program  : N/A
*& Created by          : T0200335
*& Created On          : 2020.12.23
*& Type                : CLASS
*& Description         : CRT_DYN_TABLE
*&---------------------------------------------------------------------*
*& Change History
*&---------------------------------------------------------------------*
*& 변경SEQ    변경자        변경일자             변경내용
*&---------------------------------------------------------------------*
    DATA lt_atta   TYPE TABLE OF gos_s_atta.

*--------------------------------------
* Set Attatch Info..
*--------------------------------------
    CALL FUNCTION 'GOS_API_GET_ATTA_LIST'
      EXPORTING
        is_object = is_object
      IMPORTING
        et_atta   = lt_atta
      EXCEPTIONS
        error     = 1.

    CLEAR rv_icon.

    CHECK NOT lt_atta[] IS INITIAL.

    DATA(lv_cnt) = lines( lt_atta[] ).

    READ TABLE lt_atta INTO DATA(ls_atta) INDEX lv_cnt.

    ls_atta-tech_type = |{ ls_atta-tech_type  CASE = UPPER }| .

    CASE ls_atta-tech_type.
      WHEN 'PPTX' OR 'PPT'.
        rv_icon = icon_ppt.                 "MS PowerPoint presentation
      WHEN 'PDF'.
        rv_icon = icon_pdf.                 "Adobe Acrobat document
      WHEN 'XLSX' OR 'XLS'.
        rv_icon = icon_xls.                 "MS Excel worksheet
      WHEN 'TXT'.
        rv_icon = icon_wri.                 "Notepad document
      WHEN 'DOCX' OR 'DOC'.
        rv_icon = icon_doc.                 "MS Word document
      WHEN 'JPG'.
        rv_icon = icon_jpg.                 "JPEG Image
      WHEN OTHERS.
        rv_icon = icon_attachment.         "Display attachment
    ENDCASE.

  ENDMETHOD.


  METHOD free_grid.
*&---------------------------------------------------------------------*
*& Module              : CN
*& Program ID          : ZCL_CN_ALV_GRID
*& T-CODE              : N/A
*& Referenced Program  : N/A
*& Created by          : T0200335
*& Created On          : 2020.12.23
*& Type                : CLASS
*& Description         : ZCL_CN_ALV_GRID
*&---------------------------------------------------------------------*
*& Change History
*&---------------------------------------------------------------------*
*& 변경SEQ    변경자        변경일자             변경내용
*&---------------------------------------------------------------------*

    me->free( ).
    cl_gui_cfw=>dispatch( ).

  ENDMETHOD.


  METHOD file_open_dialog.
*&---------------------------------------------------------------------*
*& Module              : CN
*& Program ID          : ZCL_CN_ALV_GRID
*& T-CODE              : N/A
*& Referenced Program  : N/A
*& Created by          : T0200335
*& Created On          : 2020.12.23
*& Type                : CLASS
*& Description         : CRT_DYN_TABLE
*&---------------------------------------------------------------------*
*& Change History
*&---------------------------------------------------------------------*
*& 변경SEQ    변경자        변경일자             변경내용
*&---------------------------------------------------------------------*
    DATA : lt_file_table TYPE  filetable,
           lv_rc         TYPE i.


    CLEAR : lt_file_table[], lv_rc, ev_fullpath, ev_path, ev_filename.
    cl_gui_frontend_services=>file_open_dialog(
      EXPORTING
        window_title            = CONV string( iv_title  )
        default_extension       = CONV string( iv_default_extension  )
        default_filename        = CONV string( iv_default_filename  )
        file_filter             = CONV string( iv_file_filter  )
        initial_directory       = CONV string( iv_init_directory  )
      CHANGING
        file_table              = lt_file_table
        rc                      = lv_rc
      EXCEPTIONS
        file_open_dialog_failed = 1
        cntl_error              = 2
        error_no_gui            = 3
        not_supported_by_gui    = 4
        OTHERS                  = 5 ).

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      RETURN.
    ELSE.
      IF lines( lt_file_table ) > 0.
        ev_fullpath  = lt_file_table[ 1 ]-filename.
      ENDIF.
    ENDIF.

    CHECK ev_filename IS REQUESTED OR ev_excel IS REQUESTED.

    CALL FUNCTION 'SO_SPLIT_FILE_AND_PATH'
      EXPORTING
        full_name     = ev_fullpath
      IMPORTING
        stripped_name = ev_filename
        file_path     = ev_path
      EXCEPTIONS
        x_error       = 1
        OTHERS        = 2.

    SPLIT ev_filename   AT '.' INTO TABLE DATA(lt_file).
    DESCRIBE TABLE lt_file LINES DATA(lv_cnt).
    READ TABLE lt_file INTO DATA(ls_file) INDEX lv_cnt.
    CASE ls_file.
      WHEN 'XLS' OR 'XLSX'.
        ev_excel = 'X'.
    ENDCASE.

  ENDMETHOD.


  METHOD ext_excl_download.
*&---------------------------------------------------------------------*
*& Module              : CN
*& Program ID          : ZCL_CN_ALV_GRID
*& T-CODE              : N/A
*& Referenced Program  : N/A
*& Created by          : T0200335
*& Created On          : 2020.12.23
*& Type                : CLASS
*& Description         : CRT_DYN_TABLE
*&---------------------------------------------------------------------*
*& Change History
*&---------------------------------------------------------------------*
*& 변경SEQ    변경자        변경일자             변경내용
*&---------------------------------------------------------------------*
*-----------------------------------------------------
* Excel Downloa Method (XML방식)
*-----------------------------------------------------
    CONSTANTS lc_temp_dir TYPE string VALUE 'Z:\'.

    DATA: lv_xml       TYPE xstring,
          lo_dref      TYPE REF TO data,
          lr_tabdescr  TYPE REF TO cl_abap_structdescr,
          lr_data      TYPE REF TO data,

          lt_fieldcat  TYPE lvc_t_fcat,
          ls_fieldcat  TYPE lvc_s_fcat,

          lt_header    TYPE mdg_mdf_ts_field_reptext,
          ls_header    TYPE LINE OF mdg_mdf_ts_field_reptext,

          l_filename   TYPE string,
          l_xml_stream TYPE xml_rawdata..

*--------------------------------------------------------------------*
*-- Set fieldcatalog for Data table
*--------------------------------------------------------------------*
    IF iv_type IS INITIAL.
      CREATE DATA lr_data LIKE LINE OF it_data[].

      CLEAR lt_fieldcat.

      IF it_header[] IS NOT INITIAL.
        lt_header[] = it_header[].
      ENDIF.

      lr_tabdescr ?= cl_abap_structdescr=>describe_by_data_ref( lr_data ).
      DATA(lt_dfies) = cl_salv_data_descr=>read_structdescr( lr_tabdescr ).

      LOOP AT lt_dfies INTO DATA(ls_dfies).

        ls_fieldcat = CORRESPONDING lvc_s_fcat( ls_dfies ).

        "- User Define Fieldcatalog
        READ TABLE it_fieldcat INTO DATA(ls_user_fcat)  WITH KEY fieldname = ls_fieldcat-fieldname.
        IF sy-subrc = 0.
          MOVE-CORRESPONDING ls_user_fcat TO ls_fieldcat.
          ls_fieldcat-reptext   = ls_user_fcat-coltext.
          ls_fieldcat-scrtext_m = ls_user_fcat-reptext.
          ls_fieldcat-scrtext_l = ls_user_fcat-reptext.
          ls_fieldcat-scrtext_s = ls_user_fcat-reptext.
        ENDIF.

        "- User Define HederText
        IF lt_header IS NOT INITIAL.
          READ TABLE lt_header INTO ls_header
          WITH KEY fieldname = ls_fieldcat-fieldname BINARY SEARCH.
          IF sy-subrc  = 0.
            ls_fieldcat-reptext   = ls_header-reptext.
            ls_fieldcat-coltext   = ls_header-reptext.
            ls_fieldcat-scrtext_m = ls_header-reptext.
            ls_fieldcat-scrtext_l = ls_header-reptext.
            ls_fieldcat-scrtext_s = ls_header-reptext.
          ENDIF.
        ENDIF.

        CASE ls_fieldcat-edit_mask(2).
          WHEN '=='.
            DATA(lv_conv) = ls_fieldcat-edit_mask.
            REPLACE FIRST OCCURRENCE OF '==' IN lv_conv WITH space.
            ls_fieldcat-convexit = lv_conv.
        ENDCASE.
        CASE ls_dfies-fieldname.
          WHEN 'STATU'.
            ls_fieldcat-no_out = 'X'.
        ENDCASE.

        IF ls_fieldcat-checkbox = 'X'.
          CLEAR:ls_fieldcat-checkbox.
        ENDIF.

        APPEND ls_fieldcat TO lt_fieldcat.
        CLEAR : ls_dfies, ls_user_fcat, ls_fieldcat.

      ENDLOOP.

*--------------------------------------------------------------------*
*-- Set Result Table
*--------------------------------------------------------------------*
      GET REFERENCE OF it_data[] INTO lo_dref.
      TRY .
          DATA(l_result_data) = cl_salv_ex_util=>factory_result_data_table(
                                  r_data                      = lo_dref
                                  t_fieldcatalog              = lt_fieldcat[] ).
        CATCH cx_sy_dyn_call_illegal_type.
          EXIT.
      ENDTRY.


*--------------------------------------------------------------------*
*-- Transform result data to XML data
*--------------------------------------------------------------------*
      CALL METHOD cl_salv_bs_tt_util=>if_salv_bs_tt_util~transform
        EXPORTING
          xml_type      = '10'
          xml_version   = cl_salv_bs_a_xml_base=>get_version( )
          r_result_data = l_result_data
          xml_flavour   = if_salv_bs_c_tt=>c_tt_xml_flavour_export
          gui_type      = if_salv_bs_xml=>c_gui_type_gui
        IMPORTING
          xml           = lv_xml.

*--------------------------------------------------------------------*
*-- Check available Speadsheet Formats
*--------------------------------------------------------------------*
      DATA(lt_xml_choice) =
             cl_salv_export_xml_dialog=>get_gui_spreadsheet_formats(  ).
      IF lt_xml_choice IS NOT INITIAL.
        DATA(ls_xml_choice) = lt_xml_choice[ xml_type = '10' ].
      ENDIF.

      CHECK ls_xml_choice IS NOT INITIAL.

*--------------------------------------------------------------------*
      "Filedownload : Ref.source
*--------------------------------------------------------------------*
**  CL_SALV_EXPORT_XML_DIALOG=>DOWNLOAD(
**                    EXPORTING
**                      S_XML_CHOICE = LS_XML_CHOICE
**                      XML          = LV_XML ).
*--------------------------------------------------------------------*

      DATA: lv_default_extension TYPE string,
            lv_initial_directory TYPE string,
            lv_length            TYPE i,
            lv_default_file_name TYPE string,
            lv_mask              TYPE char255,
            lv_mask1             TYPE string.

*  ... get default extension
      CALL METHOD cl_alv_bds=>create_mask_for_filefilter
        EXPORTING
          i_frontend          = ls_xml_choice-frontend
        IMPORTING
          e_default_extension = lv_default_extension
        CHANGING
          c_mask              = lv_mask.

      lv_mask1 = lv_mask.

      DATA: lv_application TYPE string.
      IF cl_salv_gui_environment_info=>s_gui_type-name EQ cl_salv_gui_environment_info=>cs_gui_type-windows.

        lv_application = cl_salv_bs_xml_utils=>get_pc_application(
                                            ls_xml_choice-frontend ).
      ELSE.
        IF ls_xml_choice-frontend EQ cl_alv_bds=>mc_mhtml_frontend.
          lv_default_extension = cl_alv_bds=>mc_excel_extension.
          lv_mask1 = TEXT-r05.  "'Excel (*.XLS)|*.XLS'.
        ENDIF.
      ENDIF.

      "- File full path
      IF iv_filename IS NOT INITIAL.
        CONCATENATE iv_filename '.' lv_default_extension
               INTO lv_default_file_name.
      ELSE.
        CONCATENATE sy-cprog lv_default_extension
               INTO lv_default_file_name.
      ENDIF.

    ENDIF.

*  ... call Filedownload Dialog and download file
    IF cl_gui_object=>www_active IS INITIAL.

      CALL FUNCTION 'XML_EXPORT_DIALOG'
        EXPORTING
          i_xml                      = lv_xml
          i_default_extension        = lv_default_extension
          i_initial_directory        = lv_initial_directory
          i_default_file_name        = lv_default_file_name
          i_mask                     = lv_mask1
          i_application              = lv_application
        EXCEPTIONS
          application_not_executable = 1
          OTHERS                     = 2.

    ELSE.

      l_filename = lc_temp_dir && lv_default_file_name.

      CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
        EXPORTING
          buffer        = lv_xml
        IMPORTING
          output_length = lv_length
        TABLES
          binary_tab    = l_xml_stream.

      CALL METHOD cl_gui_frontend_services=>gui_download
        EXPORTING
          bin_filesize = lv_length
          filetype     = 'BIN'
          filename     = l_filename
        CHANGING
          data_tab     = l_xml_stream
        EXCEPTIONS
          OTHERS       = 1.

    ENDIF.

  ENDMETHOD.


  METHOD ext_excl_downbin.

    DATA: lv_xml      TYPE xstring,
          lo_dref     TYPE REF TO data,
          lr_tabdescr TYPE REF TO cl_abap_structdescr,
          lr_data     TYPE REF TO data,

          lt_fieldcat TYPE lvc_t_fcat,
          ls_fieldcat TYPE lvc_s_fcat,

          lt_header   TYPE mdg_mdf_ts_field_reptext,
          ls_header   TYPE LINE OF mdg_mdf_ts_field_reptext,

          l_filename  TYPE string,
          lv_length   TYPE i,
          l_bin_tab   TYPE sdokcntbins.

*--------------------------------------------------------------------*
*-- Set fieldcatalog for Data table
*--------------------------------------------------------------------*

    CREATE DATA lr_data LIKE LINE OF it_data[].

    CLEAR lt_fieldcat.

    IF it_header[] IS NOT INITIAL.
      lt_header[] = it_header[].
    ENDIF.

    lr_tabdescr ?= cl_abap_structdescr=>describe_by_data_ref( lr_data ).
    DATA(lt_dfies) = cl_salv_data_descr=>read_structdescr( lr_tabdescr ).

    LOOP AT lt_dfies INTO DATA(ls_dfies).

      ls_fieldcat = CORRESPONDING lvc_s_fcat( ls_dfies ).

      "- User Define Fieldcatalog
      READ TABLE it_fieldcat INTO DATA(ls_user_fcat)  WITH KEY fieldname = ls_fieldcat-fieldname.
      IF sy-subrc = 0.
        MOVE-CORRESPONDING ls_user_fcat TO ls_fieldcat.
        ls_fieldcat-reptext   = ls_user_fcat-coltext.
        ls_fieldcat-scrtext_m = ls_user_fcat-reptext.
        ls_fieldcat-scrtext_l = ls_user_fcat-reptext.
        ls_fieldcat-scrtext_s = ls_user_fcat-reptext.
      ENDIF.

      "- User Define HederText
      IF lt_header IS NOT INITIAL.
        READ TABLE lt_header INTO ls_header
        WITH KEY fieldname = ls_fieldcat-fieldname BINARY SEARCH.
        IF sy-subrc  = 0.
          ls_fieldcat-reptext   = ls_header-reptext.
          ls_fieldcat-coltext   = ls_header-reptext.
          ls_fieldcat-scrtext_m = ls_header-reptext.
          ls_fieldcat-scrtext_l = ls_header-reptext.
          ls_fieldcat-scrtext_s = ls_header-reptext.
        ENDIF.
      ENDIF.

      CASE ls_fieldcat-edit_mask(2).
        WHEN '=='.
          DATA(lv_conv) = ls_fieldcat-edit_mask.
          REPLACE FIRST OCCURRENCE OF '==' IN lv_conv WITH space.
          ls_fieldcat-convexit = lv_conv.
      ENDCASE.
      CASE ls_dfies-fieldname.
        WHEN 'STATU'.
          ls_fieldcat-no_out = 'X'.
      ENDCASE.

      IF ls_fieldcat-checkbox = 'X'.
        CLEAR:ls_fieldcat-checkbox.
      ENDIF.

      APPEND ls_fieldcat TO lt_fieldcat.
      CLEAR : ls_dfies, ls_user_fcat, ls_fieldcat.

    ENDLOOP.

*--------------------------------------------------------------------*
*-- Set Result Table
*--------------------------------------------------------------------*
    GET REFERENCE OF it_data[] INTO lo_dref.
    TRY .
        DATA(l_result_data) = cl_salv_ex_util=>factory_result_data_table(
                                r_data                      = lo_dref
                                t_fieldcatalog              = lt_fieldcat[] ).
      CATCH cx_sy_dyn_call_illegal_type.
        EXIT.
    ENDTRY.


*--------------------------------------------------------------------*
*-- Transform result data to XML data
*--------------------------------------------------------------------*
    CALL METHOD cl_salv_bs_tt_util=>if_salv_bs_tt_util~transform
      EXPORTING
        xml_type      = '10'
        xml_version   = cl_salv_bs_a_xml_base=>get_version( )
        r_result_data = l_result_data
        xml_flavour   = if_salv_bs_c_tt=>c_tt_xml_flavour_export
        gui_type      = if_salv_bs_xml=>c_gui_type_gui
      IMPORTING
        xml           = lv_xml.

*--------------------------------------------------------------------*
*-- Check available Speadsheet Formats
*--------------------------------------------------------------------*
    DATA(lt_xml_choice) =
           cl_salv_export_xml_dialog=>get_gui_spreadsheet_formats(  ).
    IF lt_xml_choice IS NOT INITIAL.
      DATA(ls_xml_choice) = lt_xml_choice[ xml_type = '10' ].
    ENDIF.

    CHECK ls_xml_choice IS NOT INITIAL.

    CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
      EXPORTING
        buffer        = lv_xml
      IMPORTING
        output_length = lv_length
      TABLES
        binary_tab    = l_bin_tab.

    ev_length = lv_length.
    et_bin_tab = l_bin_tab.


  ENDMETHOD.


  METHOD evt_user_command.
*&---------------------------------------------------------------------*
*& Module              : CN
*& Program ID          : ZCL_CN_ALV_GRID
*& T-CODE              : N/A
*& Referenced Program  : N/A
*& Created by          : T0200335
*& Created On          : 2020.12.23
*& Type                : CLASS
*& Description         : ZCL_CN_ALV_GRID
*&---------------------------------------------------------------------*
*& Change History
*&---------------------------------------------------------------------*
*& 변경SEQ    변경자        변경일자             변경내용
*&---------------------------------------------------------------------*
*&   U01  / KTGT0220021 / 2022.08.18 / SORT,LAYOUT 속성 파라메터 추가
*&   U02  / KTGT0220021 / 2022.12.05 / SORT,LAYOUT 속성 파라메터 제거
*&---------------------------------------------------------------------*
*-----------------------------------------------------
* 개별 프로그램에서  Public Section 에서 Redfefine 사용
* 그리드가 여러개 일 경우 GRID 명으로 구분 (M_NAME)
* 참조 프로그램:YALV/YALV2
*-----------------------------------------------------
*=======================================================
*  PUBLIC SECTION.
*    METHODS:
**---------------------------
** Event Area
**---------------------------
*  EVT_USER_COMMAND       REDEFINITION.

*&---------------------------------------------------------------------*
*& Class Implementation
*&---------------------------------------------------------------------*
*CLASS lcl_cust_alv_grid IMPLEMENTATION.

*  METHOD EVT_USER_COMMAND.
*
*    PERFORM EVT_USER_COMMAND      USING E_UCOMM
*
*  ENDMETHOD.

    DATA:lv_fname TYPE string,
         lv_mode.

    FIELD-SYMBOLS:<lt_data> TYPE table.


    CASE e_ucomm.
      WHEN 'CHAN'.
*---------------------------
*-- Mode 전환
*---------------------------
        CASE av_edit_mode.
          WHEN 'X'.
            me->set_edit_mode( '' ).
          WHEN OTHERS.
            me->set_edit_mode( 'X' ).
        ENDCASE.
        EXIT.
      WHEN 'AROW'.
*---------------------------
* Inser Row..
*---------------------------
        me->btn_add_row( ).
      WHEN 'MAROW'.
*---------------------------
* Multi Inser Row..
*---------------------------
        me->btn_add_row( iv_multi = 'X' ).
      WHEN 'DROW'.
*---------------------------
* Delete Row..
*---------------------------
        me->btn_del_row( ).
      WHEN 'RECO'.
*---------------------------
* Recovery Row..
*---------------------------
        me->btn_rec_row( ).
      WHEN 'EXUP'.
*---------------------------
* Excel Upload
*---------------------------
        ASSIGN ao_dataref->* TO <lt_data>.
        CHECK sy-subrc = 0.

        me->btn_excl_upload( CHANGING  ct_data     = <lt_data> ).

      WHEN 'EXDL'.
*---------------------------
* Excel DownLoad
*---------------------------
        ASSIGN ao_dataref->* TO <lt_data>.
        CHECK sy-subrc = 0.

        CONCATENATE av_repid '' INTO lv_fname.
        me->btn_excl_download( EXPORTING iv_filename = lv_fname
*                                         is_layout   = as_layout  "U1 ADD   "U2 OUT
*                                         it_sort     = at_sort    "U1 ADD   "U2 OUT
                                         it_fieldcat = at_fcat
                                         it_data     = <lt_data> ).
        EXIT.
      WHEN 'INFO'.
        me->btn_info( ).
        EXIT.

      WHEN 'HIST'.
        me->btn_history( EXPORTING it_fcat = at_fcat
                                   iv_tabnm = av_hst_tabnm ).
        EXIT.
      WHEN 'FILE'.
        me->btn_file_attatch( ).
        EXIT.
    ENDCASE.

*--------------------------------
* Refresh ALv
*--------------------------------
    me->refresh_grid_display( ).

  ENDMETHOD.


  METHOD evt_top_of_page.
*&---------------------------------------------------------------------*
*& Module              : CN
*& Program ID          : ZCL_CN_ALV_GRID
*& T-CODE              : N/A
*& Referenced Program  : N/A
*& Created by          : T0200335
*& Created On          : 2020.12.23
*& Type                : CLASS
*& Description         : ZCL_CN_ALV_GRID
*&---------------------------------------------------------------------*
*& Change History
*&---------------------------------------------------------------------*
*& 변경SEQ    변경자        변경일자             변경내용
*&---------------------------------------------------------------------*

*-----------------------------------
* Display Header Document..
*-----------------------------------
    me->display_header( it_header   = at_header
                        iv_scr_wr   = av_scr_wr ).

  ENDMETHOD.


  METHOD evt_toolbar.
*&---------------------------------------------------------------------*
*& Module              : CN
*& Program ID          : ZCL_CN_ALV_GRID
*& T-CODE              : N/A
*& Referenced Program  : N/A
*& Created by          : T0200335
*& Created On          : 2020.12.23
*& Type                : CLASS
*& Description         : ZCL_CN_ALV_GRID
*&---------------------------------------------------------------------*
*& Change History
*&---------------------------------------------------------------------*
*& 변경SEQ    변경자        변경일자             변경내용
*&---------------------------------------------------------------------*
*-----------------------------------------------------------------------------------------
* ALV 공통으로 사용되는 Toolbar
* (화면전환,라인추가/라인삭제/Recovery/Excel Download/ Excel Upload)
* 단, 화면별로 보여지는 Tool이 다를 수 있으므로 Constuctor 에서 IS_TOOLBTN 으로 제어한다
* 그외 User Toolbar는 METHOD EVT_ON_TOOLBAR에서 정의
*-----------------------------------------------------------------------------------------
    DEFINE _l_add_toolbar.
      ls_add_toolbar-function    = &1.
      ls_add_toolbar-icon        = &2.
      ls_add_toolbar-quickinfo   = &3.
      ls_add_toolbar-butn_type   = &4.
      ls_add_toolbar-disabled    = &5.
      ls_add_toolbar-text        = &6.

      APPEND ls_add_toolbar TO lt_toolbar.
    END-OF-DEFINITION.


    DEFINE _l_add_toolbar.

      ls_button = VALUE #( butn_type = &1 function = &2 icon = &3  quickinfo = &4 text = &5 disabled = &6 ).

      APPEND ls_button TO e_object->mt_toolbar.

    END-OF-DEFINITION.

    DATA : lt_add_toolbar TYPE ttb_button,
           lv_edit_mode,
           ls_button      TYPE stb_button.

*---------------------------------------------------
* Default Toolbar Button
*---------------------------------------------------
    _l_add_toolbar '3' '' '' '' '' ''.  " 구분자

    lv_edit_mode = av_edit_mode.

*----------------------------------------
* Basic Toolbar Mode 변경
*----------------------------------------
    me->on_evt_toolbar_mode( EXPORTING iv_name = me->m_name
                             CHANGING  cv_edit_mode = lv_edit_mode ).

*----------------------------------------
* 기본 Toolbar (추가)
*----------------------------------------
    me->set_toolbar_by_mode( EXPORTING iv_edit_mode = lv_edit_mode
                                       is_toolbtn   = as_toolbtn
                             CHANGING  crf_object    = e_object ).


*----------------------------------------
* User Toolbar (추가)
*----------------------------------------
    me->evt_on_toolbar( EXPORTING iv_name        = me->m_name
                        CHANGING  ct_add_toolbar = lt_add_toolbar
                                  crf_object     = e_object ).


*=============================================
* User 추가 버튼
*=============================================
    LOOP AT lt_add_toolbar INTO DATA(ls_toolbar).
      _l_add_toolbar '' ls_toolbar-function ls_toolbar-icon  ls_toolbar-quickinfo ls_toolbar-text ls_toolbar-disabled.
      _l_add_toolbar '3' '' '' '' '' ''. "구분자

    ENDLOOP.

  ENDMETHOD.


  METHOD evt_save_hist.
*&---------------------------------------------------------------------*
*& Module      : CN
*& 생성자      : T0200335
*& 생성일      :  2020.12.30
*& Description :
*&---------------------------------------------------------------------*
*& Change History
*&---------------------------------------------------------------------*
*& 변경SEQ    변경자        변경일자             변경내용
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*

    DATA:lo_data      TYPE REF TO data,
         lo_sdata     TYPE REF TO data,
         ls_save      TYPE ztcn00300,
         lt_save      TYPE TABLE OF ztcn00300,
         ls_timestamp TYPE zscn_timestamp,
         lt_tkey      TYPE abap_keydescr_tab.

    FIELD-SYMBOLS:<lt_data>     TYPE table,
                  <lt_old>      TYPE table,
                  <lt_hist>     TYPE SORTED TABLE,
                  <lt_hist_old> TYPE SORTED TABLE,
                  <ls_hist>     TYPE any.

    CHECK av_hst_tabnm IS NOT INITIAL AND at_history IS NOT INITIAL.

    SORT at_history BY dkey hfldnm aezet DESCENDING.
    DELETE ADJACENT DUPLICATES FROM at_history  COMPARING dkey hfldnm.

*-----------------------------------------
* Make Key Table..
*-----------------------------------------
    DATA(lt_fcat) = CORRESPONDING ty_fcat( at_fcat ).
    SORT lt_fcat BY col_pos.

    DELETE lt_fcat WHERE key IS INITIAL.

*-- Create Dynamic Table..
    lt_tkey = CORRESPONDING #( lt_fcat MAPPING name = fieldname ).
    me->crt_dyn_table( EXPORTING it_fcat = at_fcat
                                 it_tkey = lt_tkey
                                 iv_table_kind = cl_abap_tabledescr=>tablekind_sorted
                       IMPORTING eo_dynt = lo_data ).

    CHECK lo_data IS BOUND.

    ASSIGN lo_data->* TO <lt_hist>.
    CREATE DATA lo_sdata LIKE LINE OF <lt_hist>.
    ASSIGN lo_sdata->* TO <ls_hist>.

    CREATE DATA lo_data LIKE <lt_hist>.
    ASSIGN lo_data->* TO <lt_hist_old>.
*-----------------------------------------------
* Get ALV Data..
*-----------------------------------------------
    ASSIGN: ao_dataref->* TO <lt_data>,   "현재데이타
            ao_oldref->*  TO  <lt_old>.    "최초데이타

* Excel 제외 Data Move..
    LOOP AT <lt_data> ASSIGNING FIELD-SYMBOL(<ls_data>).
      ASSIGN COMPONENT 'STATU' OF STRUCTURE <ls_data> TO FIELD-SYMBOL(<lv_statu>).
      CHECK <lv_statu> <> icon_xls.
      MOVE-CORRESPONDING <ls_data> TO <ls_hist>.
      INSERT <ls_hist> INTO TABLE <lt_hist>.
    ENDLOOP.

*-- 최초 데이타 Move
    MOVE-CORRESPONDING <lt_old> TO <lt_hist_old>.

    SORT:at_history BY dkey aezet.

    zcl_cn_alv_grid=>set_timestamp( EXPORTING iv_create = 'X'
                                    CHANGING  cs_data   = ls_timestamp ).

*-----------------------------------------
* Make Key Table..
*-----------------------------------------
    LOOP AT at_history ASSIGNING FIELD-SYMBOL(<ls_history>).

*-----------------------------
* Read bY Data Key..
*-----------------------------
      AT NEW dkey.
        CLEAR:<ls_hist>.

        SPLIT <ls_history>-dkey AT '/' INTO TABLE DATA(lt_result).
        LOOP AT at_key_list INTO DATA(ls_key).
          DATA(lv_line) = sy-tabix.
          ASSIGN COMPONENT ls_key-fieldname OF STRUCTURE <ls_hist> TO FIELD-SYMBOL(<lv_val>).
          READ TABLE lt_result INTO DATA(ls_result) INDEX lv_line.
          CHECK sy-subrc = 0 .
          IF ls_result = me->ac_blk.   "Blank 를 @ 로 표시함
            CLEAR:ls_result.
          ENDIF.
          <lv_val> = ls_result.
          UNASSIGN:<lv_val>.
        ENDLOOP.
        CLEAR:lt_result.

*------------------------
* Read Current Data..
*------------------------
        READ TABLE <lt_hist> FROM <ls_hist> ASSIGNING <ls_data>.
        CHECK sy-subrc = 0.
        ASSIGN COMPONENT 'DCFLG' OF STRUCTURE <ls_data> TO FIELD-SYMBOL(<lv_dcflg>).
      ENDAT.

      CASE <ls_history>-hchg_type.
        WHEN 'R'.
*-----------------------------
*  Recovery
*-----------------------------
          CHECK <lv_dcflg> IS NOT INITIAL AND <lv_dcflg> <> 'T'.

          READ TABLE <lt_hist_old> FROM <ls_hist> ASSIGNING <ls_data>.       "Old Data가 이전에 삭제였는지 확인
          ASSIGN COMPONENT 'ZDELE' OF STRUCTURE <ls_data> TO FIELD-SYMBOL(<lv_del>).
          CHECK <lv_del> = 'X'.

        WHEN 'D'.
*-----------------------------
*  Delete Field
*-----------------------------

          CHECK <lv_dcflg> = 'T'.  "삭제
        WHEN OTHERS.
*-----------------------------
* Changed Old Value
*-----------------------------
          READ TABLE <lt_hist_old> FROM <ls_hist> ASSIGNING <ls_data>.
          ASSIGN COMPONENT <ls_history>-hfldnm OF STRUCTURE <ls_data> TO <lv_val>.  "변경전 Value

          DATA(ls_fcat) = VALUE #( lt_fcat[ KEY zfd COMPONENTS fieldname = <ls_history>-hfldnm ] OPTIONAL ).

          me->conv_value( EXPORTING  is_fcat     = ls_fcat
                                     iv_type   = 'X'
                                     iv_value   = <lv_val>
                          CHANGING   cv_val  = <lv_val> ).

          <ls_history>-hval_b = <lv_val>.
      ENDCASE.

      MOVE-CORRESPONDING <ls_history> TO ls_save.
      MOVE-CORRESPONDING ls_timestamp TO ls_save.

      ls_save-htabnm = av_hst_tabnm.           "Table Name
      ls_save-hprogid  = av_repid.               "Program ID
      ls_save-hdatakey = <ls_history>-dkey.      "Set Data Key

      APPEND ls_save TO lt_save.
    ENDLOOP.

    TRY.
        INSERT ztcn00300 FROM TABLE lt_save.

      CATCH cx_sy_sql_error INTO DATA(lrf_sql).
        DATA(lv_msg) = lrf_sql->get_text( ).
    ENDTRY.


    CLEAR:at_history.

  ENDMETHOD.


  METHOD evt_on_top_of_page.
*&---------------------------------------------------------------------*
*& Module      : CN
*& 생성자      : T0200335
*& 생성일      :  2020.12.30
*& Description :
*&---------------------------------------------------------------------*
*& Change History
*&---------------------------------------------------------------------*
*& 변경SEQ    변경자        변경일자             변경내용
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*

*-----------------------------------------------------
* 개별 프로그램에서  Public Section 에서 Redfefine 사용
* 그리드가 여러개 일 경우 GRID 명으로 구분 (M_NAME)
* 참조 프로그램:YALV/YALV2
*-----------------------------------------------------
*=======================================================
*  PUBLIC SECTION.
*    METHODS:
**---------------------------
** Event Area
**---------------------------
*  EVT_ON_TOP_OF_PAGE      REDEFINITION.

*&---------------------------------------------------------------------*
*& Class Implementation
*&---------------------------------------------------------------------*
*CLASS lcl_cust_alv_grid IMPLEMENTATION.

*  METHOD EVT_ON_TOP_OF_PAGE.
*
*    PERFORM EVT_ON_TOP_OF_PAGE  CHANGING  CV_SCR_WR CT_HEADER.
*
*  ENDMETHOD.

  ENDMETHOD.


  METHOD evt_on_toolbar.
*&---------------------------------------------------------------------*
*& Module              : CN
*& Program ID          : ZCL_CN_ALV_GRID
*& T-CODE              : N/A
*& Referenced Program  : N/A
*& Created by          : T0200335
*& Created On          : 2020.12.23
*& Type                : CLASS
*& Description         : ZCL_CN_ALV_GRID
*&---------------------------------------------------------------------*
*& Change History
*&---------------------------------------------------------------------*
*& 변경SEQ    변경자        변경일자             변경내용
*&---------------------------------------------------------------------*

*-----------------------------------------------------
* 개별 프로그램에서  Public Section 에서 Redfefine 사용
* 그리드가 여러개 일 경우 GRID 명으로 구분 (M_NAME)
* 참조 프로그램:YALV/YALV2
*-----------------------------------------------------
*=======================================================
*  PUBLIC SECTION.
*    METHODS:
**---------------------------
** Event Area
**---------------------------
*  EVT_ON_TOOLBAR      REDEFINITION.

*&---------------------------------------------------------------------*
*& Class Implementation
*&---------------------------------------------------------------------*
*CLASS lcl_cust_alv_grid IMPLEMENTATION.

*  METHOD EVT_ON_TOOLBAR.
*
*    PERFORM EVT_ON_TOOLBAR   USING    IV_NAME
*                            CHANGING  CT_ADD_TOOLBAR.
*
*  ENDMETHOD.

  ENDMETHOD.


  METHOD evt_on_f4.
*&---------------------------------------------------------------------*
*& Module              : CN
*& Program ID          : ZCL_CN_ALV_GRID
*& T-CODE              : N/A
*& Referenced Program  : N/A
*& Created by          : T0200335
*& Created On          : 2020.12.23
*& Type                : CLASS
*& Description         : ZCL_CN_ALV_GRID
*&---------------------------------------------------------------------*
*& Change History
*&---------------------------------------------------------------------*
*& 변경SEQ    변경자        변경일자             변경내용
*&---------------------------------------------------------------------*

*-----------------------------------------------------
* 개별 프로그램에서  Public Section 에서 Redfefine 사용
* 그리드가 여러개 일 경우 GRID 명으로 구분 (M_NAME)
* 참조 프로그램:YALV/YALV2
*-----------------------------------------------------
*=======================================================
*  PUBLIC SECTION.
*    METHODS:
**---------------------------
** Event Area
**---------------------------
*  EVT_ON_F4       REDEFINITION.
*
*&---------------------------------------------------------------------*
*& Class Implementation
*&---------------------------------------------------------------------*
*CLASS lcl_cust_alv_grid IMPLEMENTATION.
*
*  METHOD EVT_ON_F4.
*
*    PERFORM EVT_ON_F4    USING    IV_FIELDNAME
*                         CHANGING EP_F4_LIST
*                                  EV_MONTH_DISPLAY
*                                  CV_TITLE.
*
*  ENDMETHOD.


  ENDMETHOD.


  METHOD evt_on_data_changed.
*&---------------------------------------------------------------------*
*& Module              : CN
*& Program ID          : ZCL_CN_ALV_GRID
*& T-CODE              : N/A
*& Referenced Program  : N/A
*& Created by          : T0200335
*& Created On          : 2020.12.23
*& Type                : CLASS
*& Description         : ZCL_CN_ALV_GRID
*&---------------------------------------------------------------------*
*& Change History
*&---------------------------------------------------------------------*
*& 변경SEQ    변경자        변경일자             변경내용
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*

*-----------------------------------------------------
* 개별 프로그램에서  Public Section 에서 Redfefine 사용
* 그리드가 여러개 일 경우 GRID 명으로 구분 (M_NAME)
* 참조 프로그램:YALV/YALV2
*-----------------------------------------------------
*=======================================================
*  PUBLIC SECTION.
*    METHODS:
**---------------------------
** Event Area
**---------------------------
*  EVT_ON_DATA_CHANGED       REDEFINITION.

*&---------------------------------------------------------------------*
*& Class Implementation
*&---------------------------------------------------------------------*
*CLASS lcl_cust_alv_grid IMPLEMENTATION.

*  METHOD EVT_ON_DATA_CHANGED.
*
*    PERFORM EVT_ON_DATA_CHANGED  USING IRF_DATA_CHANGED
*                                       IV_ONF4
*                                       IV_ONF4_BEFORE
*                                       IV_ONF4_AFTER
*                                       IV_UCOMM
*                                       IV_NAME .
*
*  ENDMETHOD.

  ENDMETHOD.


  METHOD evt_onf4.
*&---------------------------------------------------------------------*
*& Module              : CN
*& Program ID          : ZCL_CN_ALV_GRID
*& T-CODE              : N/A
*& Referenced Program  : N/A
*& Created by          : T0200335
*& Created On          : 2020.12.23
*& Type                : CLASS
*& Description         : ZCL_CN_ALV_GRID
*&---------------------------------------------------------------------*
*& Change History
*&---------------------------------------------------------------------*
*& 변경SEQ    변경자        변경일자             변경내용
*&---------------------------------------------------------------------*
*---------------------------------------------------------
* F4 Event Area
* 개별 프로그램 필드별 F4 로직은 EVT_ON_F4 메소드 참조
*---------------------------------------------------------
    DATA : lt_return        TYPE TABLE OF ddshretval,
           lt_scr_fields    TYPE TABLE OF dynpread,
           ls_lvc_modi      TYPE          lvc_s_modi,
           ls_scr_fields    TYPE          dynpread,
           lv_value         TYPE dynfnam,
           lv_month_display,
           lo_data          TYPE REF TO data,
           lv_title(50),
           lv_month         TYPE spmon.

    FIELD-SYMBOLS : <lv_f4tab>   TYPE lvc_t_modi,
                    <lt_f4_list> TYPE table.

*--------------------------------
* 개별 Program Area
*--------------------------------
    me->evt_on_f4( EXPORTING iv_fieldname     = e_fieldname
                             iv_fieldvalue    = e_fieldvalue
                             is_row_no        = es_row_no
                   IMPORTING ev_month_display = lv_month_display
                             eo_f4_list       = lo_data
                             ev_value         = lv_value
                   CHANGING  cv_title         = lv_title ).

    IF lv_month_display = 'X'.
*-----------------------------------
* Display Month (Year/Month F4))
*-----------------------------------
      zcl_cn_alv_grid=>get_search_help( EXPORTING iv_fname  = 'S_SPMON'
                                        IMPORTING ev_value = lv_month  ).

      CALL FUNCTION 'CONVERSION_EXIT_PERI6_OUTPUT'
        EXPORTING
          input  = lv_month
        IMPORTING
          output = lv_value.

    ELSE.
*-----------------------------------
* Display Code List
*-----------------------------------

      IF  lo_data IS BOUND.

      ASSIGN lo_data->* TO <lt_f4_list>.


      er_event_data->m_event_handled = abap_true.


      CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
        EXPORTING
          retfield     = e_fieldname
          dynpprog     = sy-cprog
          dynpnr       = sy-dynnr
          window_title = lv_title
          value_org    = 'S'
        TABLES
          value_tab    = <lt_f4_list>
          return_tab   = lt_return
        EXCEPTIONS
          OTHERS       = 0.

      READ TABLE lt_return INTO DATA(ls_return) INDEX 1.

      CHECK sy-subrc EQ 0.

      lv_value = ls_return-fieldval.

      ls_scr_fields-fieldname  = e_fieldname.
      ls_scr_fields-fieldvalue = lv_value.
      ls_scr_fields-stepl      = es_row_no-row_id.
      APPEND ls_scr_fields TO lt_scr_fields.

      CALL FUNCTION 'DYNP_VALUES_UPDATE'
        EXPORTING
          dyname               = av_repid
          dynumb               = sy-dynnr
        TABLES
          dynpfields           = lt_scr_fields
        EXCEPTIONS
          invalid_abapworkarea = 1
          invalid_dynprofield  = 2
          invalid_dynproname   = 3
          invalid_dynpronummer = 4
          invalid_request      = 5
          no_fielddescription  = 6
          undefind_error       = 7
          OTHERS               = 8.

      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.

      CHECK e_display = space.
      CLEAR ls_lvc_modi.

      IF sy-subrc NE 0.
        RETURN.
      ENDIF.

   ENDIF.

    ENDIF.

*-----------------------------------------
* F4 Feild Assign
*-----------------------------------------
    IF e_display IS INITIAL.

      ASSIGN er_event_data->m_data->* TO <lv_f4tab>.
      ls_lvc_modi-row_id    = es_row_no-row_id.
      ls_lvc_modi-fieldname = e_fieldname.
      ls_lvc_modi-value     = lv_value.
      APPEND ls_lvc_modi TO <lv_f4tab>.
    ENDIF.

    er_event_data->m_event_handled = abap_true.


  ENDMETHOD.


  METHOD EVT_ONDROPCOMPLETE.
*&---------------------------------------------------------------------*
*& Module              : CN
*& Program ID          : ZCL_CN_ALV_GRID
*& T-CODE              : N/A
*& Referenced Program  : N/A
*& Created by          : T0200335
*& Created On          : 2020.12.23
*& Type                : CLASS
*& Description         : CRT_DYN_TABLE
*&---------------------------------------------------------------------*
*& Change History
*&---------------------------------------------------------------------*
*& 변경SEQ    변경자        변경일자             변경내용
*&---------------------------------------------------------------------*
*-----------------------------------------------------
* 개별 프로그램에서  Public Section 에서 Redfefine 사용
* 그리드가 여러개 일 경우 GRID 명으로 구분 (M_NAME)
* 참조 프로그램:YALV/YALV2
*-----------------------------------------------------
*=======================================================
*  PUBLIC SECTION.
*    METHODS:
**---------------------------
** Event Area
**---------------------------
*  EVT_ONDROPCOMPLETE       REDEFINITION.

*&---------------------------------------------------------------------*
*& Class Implementation
*&---------------------------------------------------------------------*
*CLASS lcl_cust_alv_grid IMPLEMENTATION.

*  METHOD EVT_ONDROPCOMPLETE.
*
*    PERFORM EVT_ONDROPCOMPLETE  USING E_ROW
*                                      E_COLUMN
*                                      ES_ROW_NO
*                                      E_DRAGDROPOBJ
*
*  ENDMETHOD.


  ENDMETHOD.


  METHOD EVT_ONDROP.
*&---------------------------------------------------------------------*
*& Module              : CN
*& Program ID          : ZCL_CN_ALV_GRID
*& T-CODE              : N/A
*& Referenced Program  : N/A
*& Created by          : T0200335
*& Created On          : 2020.12.23
*& Type                : CLASS
*& Description         : CRT_DYN_TABLE
*&---------------------------------------------------------------------*
*& Change History
*&---------------------------------------------------------------------*
*& 변경SEQ    변경자        변경일자             변경내용
*&---------------------------------------------------------------------*
*-----------------------------------------------------
* 개별 프로그램에서  Public Section 에서 Redfefine 사용
* 그리드가 여러개 일 경우 GRID 명으로 구분 (M_NAME)
* 참조 프로그램:YALV/YALV2
*-----------------------------------------------------
*=======================================================
*  PUBLIC SECTION.
*    METHODS:
**---------------------------
** Event Area
**---------------------------
*  EVT_ONDROP       REDEFINITION.

*&---------------------------------------------------------------------*
*& Class Implementation
*&---------------------------------------------------------------------*
*CLASS lcl_cust_alv_grid IMPLEMENTATION.

*  METHOD EVT_ONDROP.
*
*    PERFORM EVT_ONDROP          USING E_ROW
*                                      E_COLUMN
*                                      ES_ROW_NO
*                                      E_DRAGDROPOBJ
*
*  ENDMETHOD.


  ENDMETHOD.


  METHOD evt_ondrag.
*&---------------------------------------------------------------------*
*& Module              : CN
*& Program ID          : ZCL_CN_ALV_GRID
*& T-CODE              : N/A
*& Referenced Program  : N/A
*& Created by          : T0200335
*& Created On          : 2020.12.23
*& Type                : CLASS
*& Description         : CRT_DYN_TABLE
*&---------------------------------------------------------------------*
*& Change History
*&---------------------------------------------------------------------*
*& 변경SEQ    변경자        변경일자             변경내용
*&---------------------------------------------------------------------*
*-----------------------------------------------------
* 개별 프로그램에서  Public Section 에서 Redfefine 사용
* 그리드가 여러개 일 경우 GRID 명으로 구분 (M_NAME)
* 참조 프로그램:YALV/YALV2
*-----------------------------------------------------
*=======================================================
*  PUBLIC SECTION.
*    METHODS:
**---------------------------
** Event Area
**---------------------------
*  EVT_ONDRAG       REDEFINITION.

*&---------------------------------------------------------------------*
*& Class Implementation
*&---------------------------------------------------------------------*
*CLASS lcl_cust_alv_grid IMPLEMENTATION.

*  METHOD EVT_ONDRAG.
*
*    PERFORM EVT_ONDRAG          USING E_ROW
*                                      E_COLUMN
*                                      ES_ROW_NO
*                                      E_DRAGDROPOBJ
*
*  ENDMETHOD.

  ENDMETHOD.


  METHOD EVT_MENU_BUTTON.
*&---------------------------------------------------------------------*
*& Module              : CN
*& Program ID          : ZCL_CN_ALV_GRID
*& T-CODE              : N/A
*& Referenced Program  : N/A
*& Created by          : T0200335
*& Created On          : 2020.12.23
*& Type                : CLASS
*& Description         : CRT_DYN_TABLE
*&---------------------------------------------------------------------*
*& Change History
*&---------------------------------------------------------------------*
*& 변경SEQ    변경자        변경일자             변경내용
*&---------------------------------------------------------------------*
*-----------------------------------------------------
* 개별 프로그램에서  Public Section 에서 Redfefine 사용
* 그리드가 여러개 일 경우 GRID 명으로 구분 (M_NAME)
* 참조 프로그램:YALV/YALV2
*-----------------------------------------------------
*=======================================================
*  PUBLIC SECTION.
*    METHODS:
**---------------------------
** Event Area
**---------------------------
*  EVT_MENU_BUTTON       REDEFINITION.

*&---------------------------------------------------------------------*
*& Class Implementation
*&---------------------------------------------------------------------*
*CLASS lcl_cust_alv_grid IMPLEMENTATION.

*  METHOD EVT_MENU_BUTTON.
*
*    PERFORM EVT_MENU_BUTTON     USING E_OBJECT
*                                      E_UCOMM.
*
*  ENDMETHOD.

  ENDMETHOD.


  METHOD evt_hotspot_click.
*&---------------------------------------------------------------------*
*& Module              : CN
*& Program ID          : ZCL_CN_ALV_GRID
*& T-CODE              : N/A
*& Referenced Program  : N/A
*& Created by          : T0200335
*& Created On          : 2020.12.23
*& Type                : CLASS
*& Description         : CRT_DYN_TABLE
*&---------------------------------------------------------------------*
*& Change History
*&---------------------------------------------------------------------*
*& 변경SEQ    변경자        변경일자             변경내용
*&---------------------------------------------------------------------*

*-----------------------------------------------------
* 개별 프로그램에서  Public Section 에서 Redfefine 사용
* 그리드가 여러개 일 경우 GRID 명으로 구분 (M_NAME)
* 참조 프로그램:YALV/YALV2
*-----------------------------------------------------
*=======================================================
*  PUBLIC SECTION.
*    METHODS:
**---------------------------
** Event Area
**---------------------------
*  EVT_DOUBLE_CLICK       REDEFINITION.

*&---------------------------------------------------------------------*
*& Class Implementation
*&---------------------------------------------------------------------*
*CLASS lcl_cust_alv_grid IMPLEMENTATION.

*  METHOD EVT_HOTSPOT_CLICK.
*
*    PERFORM EVT_HOTSPOT_CLICK    USING E_ROW_ID
*                                      E_COLUMN_ID
*                                      ES_ROW_NO.
*
*  ENDMETHOD.



  ENDMETHOD.


  METHOD evt_double_click.
*&---------------------------------------------------------------------*
*& Module              : CN
*& Program ID          : ZCL_CN_ALV_GRID
*& T-CODE              : N/A
*& Referenced Program  : N/A
*& Created by          : T0200335
*& Created On          : 2020.12.23
*& Type                : CLASS
*& Description         : CRT_DYN_TABLE
*&---------------------------------------------------------------------*
*& Change History
*&---------------------------------------------------------------------*
*& 변경SEQ    변경자        변경일자             변경내용
*&---------------------------------------------------------------------*
*-----------------------------------------------------
* 개별 프로그램에서  Public Section 에서 Redfefine 사용
* 그리드가 여러개 일 경우 GRID 명으로 구분 (M_NAME)
* 참조 프로그램:YALV/YALV2
*-----------------------------------------------------
*=======================================================
*  PUBLIC SECTION.
*    METHODS:
**---------------------------
** Event Area
**---------------------------
*  EVT_DOUBLE_CLICK       REDEFINITION.

*&---------------------------------------------------------------------*
*& Class Implementation
*&---------------------------------------------------------------------*
*CLASS lcl_cust_alv_grid IMPLEMENTATION.

*  METHOD EVT_DOUBLE_CLICK.
*
*    PERFORM EVT_DOUBLE_CLICK    USING E_ROW
*                                      E_COLUMN
*                                      ES_ROW_NO.
*
*  ENDMETHOD.


  ENDMETHOD.


  METHOD EVT_DATA_CHANGED_FINISHED.
*&---------------------------------------------------------------------*
*& Module              : CN
*& Program ID          : ZCL_CN_ALV_GRID
*& T-CODE              : N/A
*& Referenced Program  : N/A
*& Created by          : T0200335
*& Created On          : 2020.12.23
*& Type                : CLASS
*& Description         : CRT_DYN_TABLE
*&---------------------------------------------------------------------*
*& Change History
*&---------------------------------------------------------------------*
*& 변경SEQ    변경자        변경일자             변경내용
*&---------------------------------------------------------------------*
*-----------------------------------------------------
* 개별 프로그램에서  Public Section 에서 Redfefine 사용
* 그리드가 여러개 일 경우 GRID 명으로 구분 (M_NAME)
* 참조 프로그램:YALV/YALV2
*-----------------------------------------------------
*=======================================================
*  PUBLIC SECTION.
*    METHODS:
**---------------------------
** Event Area
**---------------------------
*  EVT_DATA_CHANGED_FINISHED       REDEFINITION.

*&---------------------------------------------------------------------*
*& Class Implementation
*&---------------------------------------------------------------------*
*CLASS lcl_cust_alv_grid IMPLEMENTATION.

*  METHOD EVT_DATA_CHANGED_FINISHED.
*
*    PERFORM EVT_ON_DATA_CHANGED  USING IRF_DATA_CHANGED
*                                       IV_ONF4
*                                       IV_ONF4_BEFORE
*                                       IV_ONF4_AFTER
*                                       IV_UCOMM
*                                       IV_NAME .
*
*  ENDMETHOD.



  ENDMETHOD.


  METHOD evt_data_changed.
*&---------------------------------------------------------------------*
*& Module              : CN
*& Program ID          : EVT_DATA_CHANGED
*& T-CODE              : N/A
*& Referenced Program  : N/A
*& Created by          : T0200335
*& Created On          : 2020.12.23
*& Type                : CLASS
*& Description         : EVT_DATA_CHANGED
*&---------------------------------------------------------------------*
*& Change History
*&---------------------------------------------------------------------*
*& 변경SEQ    변경자        변경일자             변경내용
*&---------------------------------------------------------------------*

    FIELD-SYMBOLS:<lt_mod_rows> TYPE table.

*--------------------------------------------------------
* Data가 변경이 일어날 경우 공통으로 status를 처리 하기 위해
* 개별에서 실제적으로 사용되는 Event영역은
* METHOD EVT_ON_DATA_CHAGED 영역임..
*--------------------------------------------------------
*---------------------------
* Set Data Changed Flag
*---------------------------
    ASSIGN er_data_changed->mp_mod_rows->* TO <lt_mod_rows>.
    CHECK sy-subrc = 0 AND <lt_mod_rows> IS NOT INITIAL.

    av_status = 'X'.   "Set Check Changed

    IF av_dc_skip IS INITIAL.   "공통 Data Changed Skip (Local Data Changed만 쓰고 싶을 경우)
      me->set_data_chaged_value( EXPORTING irf_data_changed =  er_data_changed ).
    ENDIF.

*---------------------------
* User Alv Data Changed..
*---------------------------
    me->evt_on_data_changed( EXPORTING irf_data_changed = er_data_changed
                                       iv_onf4          = e_onf4
                                       iv_onf4_before   = e_onf4_before
                                       iv_onf4_after    = e_onf4_after
                                       iv_ucomm         = e_ucomm
                                       iv_name          = me->m_name ).

  ENDMETHOD.


  METHOD evt_button_clik.
*&---------------------------------------------------------------------*
*& Module              : CN
*& Program ID          : ZCL_CN_ALV_GRID
*& T-CODE              : N/A
*& Referenced Program  : N/A
*& Created by          : T0200335
*& Created On          : 2020.12.23
*& Type                : CLASS
*& Description         : CRT_DYN_TABLE
*&---------------------------------------------------------------------*
*& Change History
*&---------------------------------------------------------------------*
*& 변경SEQ    변경자        변경일자             변경내용
*&---------------------------------------------------------------------*
*-----------------------------------------------------
* 개별 프로그램에서  Public Section 에서 Redfefine 사용
* 그리드가 여러개 일 경우 GRID 명으로 구분 (M_NAME)
* 참조 프로그램:YALV/YALV2
*-----------------------------------------------------
*=======================================================
*  PUBLIC SECTION.
*    METHODS:
**---------------------------
** Event Area
**---------------------------
*  EVT_BUTTON_CLIK        REDEFINITION.

*&---------------------------------------------------------------------*
*& Class Implementation
*&---------------------------------------------------------------------*
*CLASS lcl_cust_alv_grid IMPLEMENTATION.

*  METHOD EVT_BUTTON_CLIK.
*
*    PERFORM EVT_BUTTON_CLIK USING    e_col_id
*                                     es_row_no .
*
*  ENDMETHOD.


  ENDMETHOD.


  METHOD EVT_BEFORE_USER_COMMAND.
*&---------------------------------------------------------------------*
*& Module              : CN
*& Program ID          : ZCL_CN_ALV_GRID
*& T-CODE              : N/A
*& Referenced Program  : N/A
*& Created by          : T0200335
*& Created On          : 2020.12.23
*& Type                : CLASS
*& Description         : CRT_DYN_TABLE
*&---------------------------------------------------------------------*
*& Change History
*&---------------------------------------------------------------------*
*& 변경SEQ    변경자        변경일자             변경내용
*&---------------------------------------------------------------------*
*-----------------------------------------------------
* 개별 프로그램에서  Public Section 에서 Redfefine 사용
* 그리드가 여러개 일 경우 GRID 명으로 구분 (M_NAME)
* 참조 프로그램:YALV/YALV2
*-----------------------------------------------------
*=======================================================
*  PUBLIC SECTION.
*    METHODS:
**---------------------------
** Event Area
**---------------------------
*  EVT_BEFORE_USER_COMMAND        REDEFINITION.

*&---------------------------------------------------------------------*
*& Class Implementation
*&---------------------------------------------------------------------*
*CLASS lcl_cust_alv_grid IMPLEMENTATION.

*  METHOD EVT_BEFORE_USER_COMMAND.
*
*    PERFORM EVT_BEFORE_USER_COMMAND USING e_ucomm.
*
*  ENDMETHOD.



  ENDMETHOD.


  METHOD evt_after_user_command.
*&---------------------------------------------------------------------*
*& Module              : CN
*& Program ID          : ZCL_CN_ALV_GRID
*& T-CODE              : N/A
*& Referenced Program  : N/A
*& Created by          : T0200335
*& Created On          : 2020.12.23
*& Type                : CLASS
*& Description         : CRT_DYN_TABLE
*&---------------------------------------------------------------------*
*& Change History
*&---------------------------------------------------------------------*
*& 변경SEQ    변경자        변경일자             변경내용
*&---------------------------------------------------------------------*
*-----------------------------------------------------
* 개별 프로그램에서  Public Section 에서 Redfefine 사용
* 그리드가 여러개 일 경우 GRID 명으로 구분 (M_NAME)
* 참조 프로그램:YALV/YALV2
*-----------------------------------------------------
*=======================================================
*  PUBLIC SECTION.
*    METHODS:
**---------------------------
** Event Area
**---------------------------
*  EVT_AFTER_USER_COMMAND        REDEFINITION.

*&---------------------------------------------------------------------*
*& Class Implementation
*&---------------------------------------------------------------------*
*CLASS lcl_cust_alv_grid IMPLEMENTATION.

*  METHOD EVT_AFTER_USER_COMMAND .
*
*    PERFORM EVT_AFTER_USER_COMMAND  USING    e_ucomm
*                                             e_saved
*                                             e_not_processed
*
*  ENDMETHOD.


  ENDMETHOD.


  METHOD display_header.
*&---------------------------------------------------------------------*
*& Module      : CN
*& 생성자      : T0200335
*& 생성일      :  2020.12.30
*& Description :
*&---------------------------------------------------------------------*
*& Change History
*&---------------------------------------------------------------------*
*& 변경SEQ    변경자        변경일자             변경내용
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*

    DATA : lrf_table     TYPE REF TO cl_dd_table_element,
           lrf_col_key   TYPE REF TO cl_dd_area,
           lrf_col_info  TYPE REF TO cl_dd_area,
           lrf_col_text  TYPE REF TO cl_dd_area,
           lrf_col_ctext TYPE REF TO cl_dd_area,
           lrf_col_ckey  TYPE REF TO cl_dd_area.

    DEFINE _l_doc_area.
      CALL METHOD lrf_table->add_column
        EXPORTING
          width  = &1
        IMPORTING
          column = &2.
    END-OF-DEFINITION.

    DEFINE _l_set_header.
      lv_key = &1.
      lv_info = &2.
*-- Text
      CALL METHOD lrf_col_key->add_text
        EXPORTING
          text         = lv_key
          sap_emphasis = lv_strong.
      CALL METHOD lrf_col_key->new_line.

*-- Parameter
      CALL METHOD lrf_col_info->add_text
        EXPORTING
          text = lv_info
          sap_emphasis = lv_usr_strong.

      CALL METHOD lrf_col_info->new_line.

*-- description
      CALL METHOD lrf_col_text->add_text
        EXPORTING
          text = lv_text
          sap_emphasis = lv_usr_strong.

      CALL METHOD lrf_col_text->new_line.

    END-OF-DEFINITION.

    DEFINE _l_set_header_cmt.
      lv_key = &1.
*-- Text
      CALL METHOD lrf_col_ckey->add_text
        EXPORTING
          text         = lv_key
          sap_emphasis = lv_usr_strong
          sap_color    = &2.   "cl_dd_area=>list_group.

      CALL METHOD lrf_col_ckey->new_line.


    END-OF-DEFINITION.
    DEFINE _l_conv_date.
      CALL FUNCTION 'CONVERT_DATE_TO_EXTERNAL'
       EXPORTING
         date_internal                  = &1
       IMPORTING
         date_external                  = &2
       EXCEPTIONS
         date_internal_is_invalid       = 1
         OTHERS                         = 2.
    end-OF-DEFINITION.

    DATA : lv_line_len   TYPE i,
           lv_key        TYPE sdydo_text_element,
           lv_info       TYPE sdydo_text_element,
           lv_text       TYPE sdydo_text_element,
           lv_cmt1       TYPE sdydo_text_element,
           lv_cmt2       TYPE sdydo_text_element,
           lv_bool       TYPE sap_bool,
           lv_strong     TYPE sdydo_attribute,
           lv_usr_strong TYPE sdydo_attribute,
           lt_header     TYPE tt_header.

    DATA:lv_date(20),
         lv_time(20).

    DATA: ls_header TYPE ts_header.

    FIELD-SYMBOLS: <lt_data> TYPE STANDARD TABLE.

*--------------------------------------------------------------------*
    lt_header = it_header.

    CALL METHOD arf_docu->initialize_document.

    lv_line_len     = arf_docu->act_gui_properties-fontsize + 9.

    IF lv_line_len GT 21.
      lv_line_len = lv_line_len + 1.
    ENDIF.

    lv_strong = cl_dd_document=>strong.

    CALL METHOD arf_docu->add_table
      EXPORTING
        no_of_columns = 3
        with_heading  = ''
        width         = '100%'
        border        = '0'
      IMPORTING
        table         = lrf_table.

* Column Split
    DATA : lv_split TYPE string,
           lt_split TYPE TABLE OF string.

    DATA : lv_width TYPE sdydo_value.

*---------------------
* Set Column 1
*---------------------
    SPLIT iv_scr_wr AT ':' INTO TABLE lt_split.

    READ TABLE lt_split INTO lv_split INDEX 1.
    IF sy-subrc = 0.
      CONCATENATE lv_split '%' INTO lv_width.
    ELSE.
      CONCATENATE '20' '%' INTO lv_width.
    ENDIF.
    _l_doc_area lv_width lrf_col_key.
*---------------------
* Set Column 2
*---------------------
    READ TABLE lt_split INTO lv_split INDEX 2.
    IF sy-subrc = 0.
      CONCATENATE lv_split '%' INTO lv_width.
    ELSE.
      CONCATENATE '10' '%' INTO lv_width.
    ENDIF.
    _l_doc_area lv_width lrf_col_info.

*---------------------
* Set Column 3
*---------------------
    READ TABLE lt_split INTO lv_split INDEX 3.
    IF sy-subrc = 0.
      CONCATENATE lv_split '%' INTO lv_width.
    ELSE.
      CONCATENATE '70' '%' INTO lv_width.
    ENDIF.
    _l_doc_area lv_width lrf_col_text.


*---------------------
* Set Head
*---------------------
    LOOP AT lt_header INTO ls_header.
      DATA(lv_index) = sy-tabix.
      CASE ls_header-typ.
        WHEN 'C'.  "추가 주석
          CONTINUE.
        WHEN OTHERS.
          IF ls_header-bold = 'X'.
            lv_usr_strong = cl_dd_document=>strong.
          ELSE.
            CLEAR:lv_usr_strong.
          ENDIF.

          lv_text = ls_header-text.
          _l_set_header ls_header-key ls_header-info .
      ENDCASE.
      DELETE lt_header INDEX lv_index.
    ENDLOOP.


*-------------------------------------------------------*
*-- Information
*-------------------------------------------------------*

    lv_usr_strong = cl_dd_document=>strong.
    CLEAR:lv_text.
    _l_set_header '' ''.

    _l_conv_date:sy-datlo lv_date.

    CONCATENATE sy-timlo+0(2)  sy-timlo+2(2)  sy-timlo+4(2) INTO lv_time SEPARATED BY ':'.

    CONCATENATE lv_date  lv_time  sy-uname INTO lv_text SEPARATED BY '/'.

    IF sy-tcode(1) <> 'Z'.
      DATA(lv_tcode) = av_repid.
    ELSE.
      lv_tcode = av_tcode.
    ENDIF.
    _l_set_header TEXT-s01  lv_tcode.

    CLEAR:lv_text.
    _l_set_header '' ''.


*-------------------------------------------------------*
*-- Add Information
*-------------------------------------------------------*
    IF lt_header[] IS NOT INITIAL.
      CALL METHOD arf_docu->add_table
        EXPORTING
          no_of_columns = 1
          with_heading  = ''
          width         = '100%'
          border        = '0'
        IMPORTING
          table         = lrf_table.

      lv_width = 100.
      _l_doc_area lv_width lrf_col_ckey.

      CLEAR:lv_usr_strong.

      LOOP AT lt_header INTO ls_header.
        IF ls_header-bold = 'X'.
          lv_usr_strong = cl_dd_document=>strong.
        ELSE.
          CLEAR:lv_usr_strong.
        ENDIF.
        _l_set_header_cmt:  ls_header-text ls_header-color.
      ENDLOOP.

    ENDIF.

*-------------------------------------------------------*
*-- Display Viewer
*-------------------------------------------------------*
    DATA(lrf_html) = NEW cl_gui_html_viewer( parent = arf_head ).

    CALL METHOD arf_docu->merge_document.

    arf_docu->html_control = lrf_html.

*  display document.
    CALL METHOD arf_docu->display_document
      EXPORTING
        reuse_control      = 'X'
        parent             = arf_head
      EXCEPTIONS
        html_display_error = 1.

  ENDMETHOD.
ENDCLASS.
