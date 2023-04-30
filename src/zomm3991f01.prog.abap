*&---------------------------------------------------------------------*
*& Include          ZOMM3991F01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form INITIALIZATION
*&---------------------------------------------------------------------*
FORM initialization.

  sscrfields-functxt_01 = TEXT-u01.
  sscrfields-functxt_02 = TEXT-u02.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form FILE_OPEN
*&---------------------------------------------------------------------*
FORM file_open CHANGING cv_file.

  DATA: lt_filetable TYPE filetable,
        ls_filetable TYPE file_table.

  DATA lv_rc TYPE i.

  CALL METHOD cl_gui_frontend_services=>file_open_dialog
    EXPORTING
      file_filter             = cl_gui_frontend_services=>filetype_excel
    CHANGING
      file_table              = lt_filetable[]
      rc                      = lv_rc
    EXCEPTIONS
      file_open_dialog_failed = 1
      cntl_error              = 2
      error_no_gui            = 3
      not_supported_by_gui    = 4
      OTHERS                  = 5.

  READ TABLE lt_filetable INTO ls_filetable INDEX 1.

  cv_file = ls_filetable-filename.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CREATE_OBJ
*&---------------------------------------------------------------------*
FORM create_obj.

  CREATE OBJECT grf_grid.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CHECK_AUTHORITY
*&---------------------------------------------------------------------*
FORM check_authority.

* 권한체크 룰(추후 적용)
  CHECK 1 <> 1.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form UPLOAD_DATA
*&---------------------------------------------------------------------*
FORM upload_data.

  DATA: lv_file TYPE string.

*  DATA: LS_COLL TYPE TY_COLL.

  DATA: lv_tabname TYPE tabname.
*  FIELD-SYMBOLS: <LT_EXCEL> TYPE TABLE.

  CONSTANTS: lc_excel_do TYPE tabname VALUE 'GT_EXCEL_DO',
             lc_excel_im TYPE tabname VALUE 'GT_EXCEL_IM'.

  CONSTANTS: lc_int_date(8) VALUE '00000000'.

  CLEAR: gt_excel_do, gt_excel_im, gt_disp, gt_coll.

  IF p_file IS INITIAL.
    MESSAGE s017 WITH TEXT-f06 DISPLAY LIKE 'E'.  "업로드 파일 을(를) 입력하세요.
    LEAVE LIST-PROCESSING.
  ENDIF.

  lv_file = p_file.

  CASE 'X'.
    WHEN p_dopo.
      lv_tabname = lc_excel_do.
    WHEN p_impo.
      lv_tabname = lc_excel_im.
  ENDCASE.

  ASSIGN (lv_tabname) TO <gt_excel>.

  grf_grid->btn_excl_upload( EXPORTING iv_filename = CONV #( lv_file )
                                       iv_beg_row  = 5
                                       iv_beg_col  = 2
                             CHANGING  ct_data     = <gt_excel> ).

  CHECK <gt_excel> IS NOT INITIAL.

  MOVE-CORRESPONDING <gt_excel> TO gt_disp.

  SORT gt_disp BY cntr_no cntr_rev cntr_item.

ENDFORM.
**&---------------------------------------------------------------------*
**& Form CURRENCY_CONV_TO_INTERNAL
**&---------------------------------------------------------------------*
*FORM CURRENCY_CONV_TO_INTERNAL USING    IV_WAERS
*                               CHANGING CV_PRICE.
*
*  DATA: LV_NETPR TYPE BAPICURR-BAPICURR.
*
*  LV_NETPR = CV_PRICE.
*
*  CALL FUNCTION 'BAPI_CURRENCY_CONV_TO_INTERNAL'
*    EXPORTING
*      CURRENCY             = IV_WAERS
*      AMOUNT_EXTERNAL      = LV_NETPR
*      MAX_NUMBER_OF_DIGITS = 15
*    IMPORTING
*      AMOUNT_INTERNAL      = CV_PRICE.
*
*ENDFORM.
*&---------------------------------------------------------------------*
*& Form CHECK_EXIT
*&---------------------------------------------------------------------*
FORM check_exit.

*  CHECK GRF_GRID->POP_TO_MSG( IV_TYPE  = 'A'
*                              IV_TITLE = ZCL_CN_ALV_GRID=>AC_MSG_TITLE2    "Exit Confirm
*                              IV_TEXT1 = CONV #( '저장되지 않은 데이터는 사라집니다.' )
*                              IV_TEXT2 = ZCL_CN_ALV_GRID=>AC_MSG_EXIT2 )   "화면에서 나가시겠습니까?
*                              EQ ABAP_TRUE. "YES

  CASE gv_ok_code.
    WHEN 'EXIT'.
      LEAVE PROGRAM.
    WHEN OTHERS.
      LEAVE TO SCREEN 0.
  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form BTN_TEST_SAVE
*&---------------------------------------------------------------------*
FORM btn_test_save.

  PERFORM create_po USING 'X'.

  grf_grid->refresh_grid_display( ).

ENDFORM.
**&---------------------------------------------------------------------*
**& Form CHECK_DATA
**&---------------------------------------------------------------------*
*FORM CHECK_DATA.
*
*  LOOP AT GT_DISP ASSIGNING FIELD-SYMBOL(<LS_DISP>).
*
**    PERFORM CHECK_MAND_VALUE USING <LS_DISP>.
*
** 계약번호, 차수, 품목 기준으로 중복라인 체크
*    READ TABLE GT_COLL WITH KEY CNTR_NO   = <LS_DISP>-CNTR_NO
*                                CNTR_REV  = <LS_DISP>-CNTR_REV
**                                CNTR_ITEM = <LS_DISP>-CNTR_ITEM
*                                BINARY SEARCH
*                                INTO DATA(LS_COLL).
*    IF LS_COLL-COUNT > 1.
*      PERFORM CONV_MSG_V1 USING TEXT-M01 "AS-IS 계약번호, 차수, 품목 기준으로 중복라인이 있습니다.
*                          CHANGING <LS_DISP>-MSGTB.
*
*    ENDIF.
*
** 문서유형 체크
*    READ TABLE GT_T161 WITH KEY BSART = <LS_DISP>-BSART
*                                BINARY SEARCH
*                                TRANSPORTING NO FIELDS.
*    IF SY-SUBRC <> 0.
*      PERFORM CONV_MSG_V1 USING TEXT-M02 "문서유형을 점검하세요.
*                          CHANGING <LS_DISP>-MSGTB.
*    ENDIF.
*
** 조직코드 체크
*    READ TABLE GT_ORG WITH KEY EKGRP = <LS_DISP>-EKGRP
*                               BINARY SEARCH
*                               TRANSPORTING NO FIELDS.
*    IF SY-SUBRC <> 0.
*      PERFORM CONV_MSG_V1 USING TEXT-M03 "조직코드를 점검하세요.
*                          CHANGING <LS_DISP>-MSGTB.
*    ENDIF.
*
** 발주담당자 체크
*    IF <LS_DISP>-ORDER IS NOT INITIAL.
*      READ TABLE GT_USER WITH KEY EMPLOY_NO = <LS_DISP>-ORDER
*                                  BINARY SEARCH
*                                  INTO DATA(LS_USER).
*      IF SY-SUBRC <> 0.
*        PERFORM CONV_MSG_V1 USING TEXT-M04 "발주담당자를 확인하세요.
*                            CHANGING <LS_DISP>-MSGTB.
*      ENDIF.
*    ENDIF.
*
*** 플랜트 체크
**    IF <LS_DISP>-WERKS IS NOT INITIAL.
**      READ TABLE GT_T001W WITH KEY WERKS = <LS_DISP>-WERKS
**                                   BINARY SEARCH
**                                   TRANSPORTING NO FIELDS.
**      IF SY-SUBRC <> 0.
**        PERFORM CONV_MSG_V1 USING TEXT-M11 "플랜트를 점검하세요.
**                            CHANGING <LS_DISP>-MSGTB.
**      ENDIF.
**    ENDIF.
*
*    IF NOT <LS_DISP>-MSGTB[] IS INITIAL.
*      <LS_DISP>-DCFLG = 'E'.
*      <LS_DISP>-STATU = GRF_GRID->SET_ICON( 'E' ).
*    ENDIF.
*
**      CLEAR <LS_DISP>-MESSAGE.
*  ENDLOOP.
*
*ENDFORM.
**&---------------------------------------------------------------------*
**& Form CONV_MSG_V1
**&---------------------------------------------------------------------*
**& text
**&---------------------------------------------------------------------*
*FORM CONV_MSG_V1  USING IV_MSGTX
*                  CHANGING EV_MSGTB TYPE ZYCN00001.
*
**  DATA : LS_CONV TYPE BAPIRET2.
*
*  CONSTANTS: LC_FIELD TYPE FIELDNAME VALUE 'CNTR_NO',
*             LC_MSGID TYPE SY-MSGID VALUE 'ZMM01',
*             LC_MSGNO TYPE SY-MSGNO VALUE '000'.
*
*  CLEAR GS_MSGTB.
*
*  GS_MSGTB = VALUE #( FIELDNAME = LC_FIELD
*                      MSGTY = 'E'
*                      ARBGB = LC_MSGID
*                      TXTNR = LC_MSGNO
*                      MSGV1 = IV_MSGTX ).
*
*  APPEND GS_MSGTB TO EV_MSGTB.
*
*ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_CHECK_DATA
*&---------------------------------------------------------------------*
FORM get_check_data.

  DATA lr_bsart TYPE RANGE OF t161-bsart.

  CHECK gt_disp IS NOT INITIAL.

  CLEAR: gt_t161, gt_org, gt_user.

* 문서유형 체크
  IF p_dopo = 'X'.
    APPEND VALUE #( sign = 'I' option = 'EQ' low = gc_bsart_psm1 ) TO lr_bsart.
    APPEND VALUE #( sign = 'I' option = 'EQ' low = gc_bsart_psm2 ) TO lr_bsart.
    APPEND VALUE #( sign = 'I' option = 'EQ' low = gc_bsart_psic ) TO lr_bsart.

  ELSE.
    zcl_mm_common=>common_config(
      EXPORTING
        is_common = VALUE #( m = 'E1' d = 'E1000' s = 'EE130' )
*       IT_WHERE  = VALUE #(
*                                                           ( FIELD = 1 VALUE = 'ZOMM3010' )
*                                                           ( FIELD = 2 VALUE = 'EX01' )
*                                                           ( FIELD = 3 VALUE = SY-UNAME )
*                                                         )
      IMPORTING
        et_outtab = DATA(lt_config) ).

    LOOP AT lt_config INTO DATA(ls_config).
      APPEND VALUE #( sign = 'I' option = 'EQ' low = ls_config-field1 ) TO lr_bsart.
    ENDLOOP.

  ENDIF.

  CLEAR : gt_config.
  zcl_mm_common=>common_config(
    EXPORTING
      is_common =
                  VALUE #( m = 'A1' d = 'A1000' s = 'IM001' )
    IMPORTING
      et_outtab = gt_config ).
  SORT gt_config BY field1.

  SELECT bsart
    INTO CORRESPONDING FIELDS OF TABLE @gt_t161
    FROM t161
   WHERE bstyp = 'F'
     AND bsart IN @lr_bsart.

  SORT gt_t161 BY bsart.

* 조직코드 체크
  SELECT ekgrp,
         ekorg
    INTO CORRESPONDING FIELDS OF TABLE @gt_org
    FROM zsvcmm_org
   WHERE ekorg = @p_ekorg.

  SORT gt_org BY ekgrp ekorg.

* User Info.
  SELECT user_id,
         employ_no,
         department,
         company
    INTO CORRESPONDING FIELDS OF TABLE @gt_user
    FROM zsvmm_user_info
   WHERE company = @p_bukrs.

  SORT gt_user BY employ_no.


* 플랜트 체크
  SELECT bwkey
    FROM t001k
   WHERE bukrs = @p_bukrs
    INTO TABLE @DATA(lt_t001k).
  SORT lt_t001k BY bwkey.
  DELETE ADJACENT DUPLICATES FROM lt_t001k COMPARING bwkey.
  IF NOT lt_t001k[] IS INITIAL.
    SELECT werks
      INTO CORRESPONDING FIELDS OF TABLE @gt_t001w
      FROM t001w
       FOR ALL ENTRIES IN @lt_t001k
     WHERE bwkey = @lt_t001k-bwkey.
  ENDIF.

  SORT gt_t001w BY werks.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form BTN_ERR_DOWNLOAD
*&---------------------------------------------------------------------*
FORM btn_err_download.

  CONSTANTS: lc_extention  TYPE string VALUE '.XLSX',
             lc_default    TYPE string VALUE 'XLSX',
             lc_path_excel TYPE string VALUE 'Excel (*.XLSX)'.

  DATA: lv_objid        TYPE wwwdata-objid,
        ls_wwwdata_item TYPE wwwdatatab,
        lv_fname_exe    TYPE string,
        lv_filename     TYPE rlgrap-filename,
        lv_doim(6)      TYPE c,
        lv_fname        TYPE string VALUE 'PO Migration Error List',
        lv_path         TYPE string VALUE 'C:\TEMP',
        lv_up_path      TYPE string,
        lv_dn_path      TYPE string.

  DATA: lv_cnt TYPE i.

  DATA: lv_full   TYPE string,
        lv_action TYPE i.

*-----------------------------
* SAP 웹저장소(SMW0) 오브젝트
*-----------------------------
  CASE 'X'.
    WHEN p_dopo.
      lv_objid = gc_objid_do.
      lv_doim = '국내'(m21).
    WHEN p_impo.
      lv_objid = gc_objid_im.
      lv_doim = '수입'(m22).
  ENDCASE.

  SELECT SINGLE relid,
                objid,
                checkout,
                checknew,
                chname,
                tdate,
                ttime,
                text
    INTO CORRESPONDING FIELDS OF @ls_wwwdata_item
    FROM wwwdata
   WHERE objid = @lv_objid.

  IF ls_wwwdata_item IS INITIAL.
    "SAP 웹저장소(SMW0)에 오브젝트 &이 등록되지 않았습니다.
    MESSAGE s002 WITH TEXT-m14 lv_objid TEXT-m15 DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

*-----------------------------
* Error Data 추출
*-----------------------------
  PERFORM make_error_data.

  IF gt_error IS INITIAL.
    MESSAGE s000 WITH TEXT-m16 DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

*-----------------------------
* File Open Dialog
*-----------------------------
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

      CONCATENATE lv_doim lv_fname lc_extention INTO lv_fname.

      CALL METHOD cl_gui_frontend_services=>file_save_dialog
        EXPORTING
          window_title              = 'DOWNLOAD'
          default_extension         = lc_default  "XLSX
          default_file_name         = lv_fname
          initial_directory         = lv_dn_path
          file_filter               = CONV #( lc_path_excel )
        CHANGING
          filename                  = lv_fname_exe
          path                      = lv_path
          fullpath                  = lv_full
          user_action               = lv_action
        EXCEPTIONS
          cntl_error                = 1
          error_no_gui              = 2
          not_supported_by_gui      = 3
          invalid_default_file_name = 4
          OTHERS                    = 5.
    CATCH cx_root INTO DATA(ls_root).

  ENDTRY.

  IF lv_action = cl_gui_frontend_services=>action_cancel.
    "취소했습니다.
    MESSAGE s000 WITH TEXT-m17.
    EXIT.
  ENDIF.

  lv_filename = lv_fname_exe.

  CALL FUNCTION 'DOWNLOAD_WEB_OBJECT'
    EXPORTING
      key         = ls_wwwdata_item
      destination = lv_filename.

* 엑셀파일 실행
  CREATE OBJECT go_excel 'EXCEL.APPLICATION'.

  SET PROPERTY OF go_excel 'VISIBLE' = 0.
  CALL METHOD OF go_excel 'WORKBOOKS' = go_workbook.

  CALL METHOD OF go_workbook 'OPEN'
    EXPORTING
      #1 = lv_full.

* Excel Save안하고 종료
*  SET PROPERTY OF GO_WORKBOOK 'SAVED' = 0. "0저장 1 그냥 종료

  CALL METHOD OF go_excel 'WORKSHEETS' = go_worksheet
    EXPORTING #1 = 1.

  CALL METHOD OF go_worksheet 'ACTIVATE'.
  SET PROPERTY OF go_worksheet 'NAME' = 'Error List'.

* 엑셀 라인추가
  IF gt_error IS NOT INITIAL.
    lv_cnt = lines( gt_error ) - 1.
    PERFORM line_insert USING 5 lv_cnt.
  ENDIF.

* 데이터 라인 생성
  DATA lv_rc TYPE i.
  CALL METHOD cl_gui_frontend_services=>clipboard_export
    IMPORTING
      data                 = gt_error
    CHANGING
      rc                   = lv_rc
    EXCEPTIONS
      cntl_error           = 1
      error_no_gui         = 2
      not_supported_by_gui = 3
      OTHERS               = 4.

  CALL METHOD OF go_excel 'CELLS' = go_cell1
    EXPORTING
      #1 = 5
      #2 = 2.

  CALL METHOD OF go_excel 'CELLS' = go_cell2
    EXPORTING
      #1 = 5
      #2 = 2.

  CALL METHOD OF go_excel 'RANGE' = go_range
    EXPORTING
      #1 = go_cell1
      #2 = go_cell2.

  CALL METHOD OF go_range 'SELECT'.
  CALL METHOD OF go_worksheet 'PASTE'.

  SET PROPERTY OF go_excel 'VISIBLE' = 1.
*  CALL METHOD OF GO_EXCEL 'SaveAs' EXPORTING #1 = LV_FULL.
  CALL METHOD OF go_excel 'CLOSE'.
  CALL METHOD OF go_excel 'QUIT'.

*  LV_FILENAME = LV_FULL.
*  CALL FUNCTION 'WS_FILE_DELETE'
*    EXPORTING
*      FILE = LV_FILENAME.

  FREE OBJECT: go_cell1, go_cell2, go_range, go_worksheet, go_workbook, go_excel.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form MAKE_ERROR_DATA
*&---------------------------------------------------------------------*
FORM make_error_data.

  IF p_dopo = 'X'.
    PERFORM make_error_data_do.
  ELSE.
    PERFORM make_error_data_im.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form MAKE_ERROR_DATA_DO
*&---------------------------------------------------------------------*
FORM make_error_data_do.

  DATA: ls_error_do TYPE ty_error_do,
        ls_line     TYPE ty_line.

  CLEAR gt_error.

  LOOP AT gt_disp INTO DATA(ls_disp).

    IF ls_disp-message IS INITIAL.
      CONTINUE.
    ENDIF.

    "단가
    PERFORM currency_conv_to_external USING    ls_disp-waers
                                      CHANGING ls_disp-netpr.

    "품목가감액
    PERFORM currency_conv_to_external USING    ls_disp-waers
                                      CHANGING ls_disp-miuwr.

    "오류 Data만 다운로드
    MOVE-CORRESPONDING ls_disp TO ls_error_do.

    CONCATENATE
      ls_error_do-cntr_no     "계약번호
      ls_error_do-cntr_rev    "계약차수
      ls_error_do-cntr_item   "품목
      ls_error_do-bsart       "문서유형
      ls_error_do-ekgrp       "구매그룹
      ls_error_do-title       "발주명
      ls_error_do-order       "발주담당
      ls_error_do-exper       "지출발의담당
      ls_error_do-qmper       "검수담당자
      ls_error_do-absgr       "세금계산서발행방식
      ls_error_do-mtype       "임가공발주
      ls_error_do-lifnr       "공급업체
      ls_error_do-verkf       "담당자
      ls_error_do-bedat       "계약체결일
      ls_error_do-zterm       "지급조건
      ls_error_do-inco1       "인도조건
      ls_error_do-waers       "통화
      ls_error_do-dppct       "선급비율
      ls_error_do-zreal_cost  "실비정산여부

    INTO ls_line SEPARATED BY cl_abap_char_utilities=>horizontal_tab.

    CONCATENATE
    ls_line
    ls_error_do-matnr       "자재
    ls_error_do-bwtar       "평가유형
    ls_error_do-meins       "단위
    ls_error_do-menge       "수량
    ls_error_do-netpr       "단가
    ls_error_do-peinh       "가격단위
    ls_error_do-mwskz       "세금코드
    ls_error_do-miuwr       "품목가감액
    ls_error_do-zprice_change   "조정사유코드
    ls_error_do-zprice_reason   "조정사유상세
    ls_error_do-werks       "플랜트
    ls_error_do-lgort       "창고
    ls_error_do-eindt       "납품일
    ls_error_do-umwrk       "입고플랜트
    ls_error_do-sc_vender   "SC 업체
    ls_error_do-sakto       "G/L 계정
    ls_error_do-kostl       "코스트센터
    ls_error_do-wbsno       "WBS
    ls_error_do-vbeln       "영업오더
    ls_error_do-vbelp       "품번
    ls_error_do-idnlf        "공급업체 자재코드
    ls_error_do-charg      "배치

    ls_disp-message
  INTO ls_line SEPARATED BY cl_abap_char_utilities=>horizontal_tab.

    APPEND ls_line TO gt_error.
    CLEAR: ls_error_do, ls_line, ls_disp.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form MAKE_ERROR_DATA_IM
*&---------------------------------------------------------------------*
FORM make_error_data_im.

  DATA: ls_error_im TYPE ty_error_im,
        ls_line     TYPE ty_line.

  CLEAR gt_error.

  LOOP AT gt_disp INTO DATA(ls_disp).

    IF ls_disp-message IS INITIAL.
      CONTINUE.
    ENDIF.


    "단가
    PERFORM currency_conv_to_external USING    ls_disp-waers
                                      CHANGING ls_disp-netpr.

    "품목가감액
    PERFORM currency_conv_to_external USING    ls_disp-waers
                                      CHANGING ls_disp-miuwr.

    "오류 Data만 다운로드
    MOVE-CORRESPONDING ls_disp TO ls_error_im.

    CONCATENATE
      ls_error_im-cntr_no     "계약번호
      ls_error_im-cntr_rev    "계약차수
      ls_error_im-cntr_item   "품목
      ls_error_im-bsart       "문서유형
      ls_error_im-ekgrp       "구매그룹
      ls_error_im-title       "발주명
      ls_error_im-order       "발주담당
      ls_error_im-exper       "지출발의담당
      ls_error_im-lifnr       "공급업체
      ls_error_im-verkf       "담당자
      ls_error_im-bedat       "계약체결일
      ls_error_im-zterm       "지급조건
      ls_error_im-inco1       "인도조건
      ls_error_im-waers       "통화
      ls_error_im-dppct    "선급비율
      ls_error_im-zeinspect   "수입검사여부
      ls_error_im-zemanage2   "관리번호
      ls_error_im-herkl       "원산지
      ls_error_im-zeshiptype  "선적구분
      ls_error_im-zededline   "선적기한
      ls_error_im-inco2_l     "선적항
      ls_error_im-inco3_l     "도착항

    INTO ls_line SEPARATED BY cl_abap_char_utilities=>horizontal_tab.

    CONCATENATE
      ls_line
      ls_error_im-matnr       "자재
      ls_error_im-bwtar       "평가유형
      ls_error_im-meins       "단위
      ls_error_im-menge       "수량
      ls_error_im-netpr       "단가
      ls_error_im-peinh       "가격단위
      ls_error_im-miuwr       "품목가감액
      ls_error_im-zprice_change   "조정사유코드
      ls_error_im-zprice_reason   "조정사유상세
      ls_error_im-werks       "플랜트
      ls_error_im-lgort       "창고
      ls_error_im-eindt       "납품일
      ls_error_im-sakto       "G/L 계정
      ls_error_im-kostl       "코스트센터
      ls_error_im-wbsno       "WBS
      ls_disp-message
    INTO ls_line SEPARATED BY cl_abap_char_utilities=>horizontal_tab.

    APPEND ls_line TO gt_error.
    CLEAR: ls_error_im, ls_line, ls_disp.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form LINE_INSERT
*&---------------------------------------------------------------------*
FORM line_insert USING iv_row
                       iv_cnt.

  DATA: lv_line TYPE i.

*-----------------------------
* 다중 LINE 복사
*-----------------------------

  lv_line = iv_row.

  DO iv_cnt TIMES.

    ADD 1 TO lv_line.

*..LINE(ROW) 선택
    CALL METHOD OF go_excel 'ROWS' = go_row EXPORTING #1 = iv_row.
    CALL METHOD OF go_row 'SELECT'.
    CALL METHOD OF go_excel 'SELECTION' = go_buffer.
    CALL METHOD OF go_buffer 'COPY'.

*..LINE INSERT & PASTE
    CALL METHOD OF go_excel 'ROWS' = go_row EXPORTING #1 = lv_line.
    CALL METHOD OF go_row 'SELECT'.
    CALL METHOD OF go_excel 'SELECTION' = go_buffer.
    CALL METHOD OF go_buffer 'ENTIREROW' = go_buffer.
    CALL METHOD OF go_buffer 'INSERT' = go_buffer.

  ENDDO.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CURRENCY_CONV_TO_EXTERNAL
*&---------------------------------------------------------------------*
FORM currency_conv_to_external USING    iv_waers
                               CHANGING cv_price.

  DATA: lv_netpr TYPE bapicurr-bapicurr.

  CALL FUNCTION 'BAPI_CURRENCY_CONV_TO_EXTERNAL'
    EXPORTING
      currency        = iv_waers
      amount_internal = cv_price
    IMPORTING
      amount_external = lv_netpr.

  cv_price = lv_netpr.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CREATE_PO_START
*&---------------------------------------------------------------------*
FORM create_po_start.


*-----------------------------
* Validation Check
*-----------------------------

* Confirm
  CHECK grf_grid->pop_to_msg( iv_type  = 'A'
                              iv_title = zcl_cn_alv_grid=>ac_msg_title    "저장확인
                              iv_text1 = CONV #( 'PO를 생성하시겠습니까?' )
                              iv_text2 = space ) EQ abap_true. "YES

  PERFORM create_po USING space.

*-----------------------------
* Refresh
*-----------------------------
*  SORT GT_DISP BY ROW_NO.

  grf_grid->refresh_grid_display( ).

ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_DATA_FOR_BAPI
*&---------------------------------------------------------------------*
FORM get_data_for_bapi.

  DATA(lt_tmp) = gt_disp[].

  CLEAR gt_zsvbmminfoprice.

*> 정보레코드 추출 (단, 플랜트, 범주 적용 불필요하여 적용하지 않음.)
  SORT lt_tmp BY lifnr matnr.
  DELETE ADJACENT DUPLICATES FROM lt_tmp COMPARING lifnr matnr.
  IF NOT lt_tmp[] IS INITIAL.
    SELECT infnr, lifnr, matnr
      FROM zsvbmminfoprice
       FOR ALL ENTRIES IN @lt_tmp
     WHERE lifnr = @lt_tmp-lifnr
       AND matnr = @lt_tmp-matnr
       AND purchasingorganization = @p_ekorg
      INTO CORRESPONDING FIELDS OF TABLE @gt_zsvbmminfoprice.
    FREE lt_tmp.
    SORT gt_zsvbmminfoprice BY lifnr matnr.
  ENDIF.

ENDFORM.
*&----------------------------------------------------------*
*& Form HEADER_INFO
*&----------------------------------------------------------*
FORM header_info USING    is_disp    TYPE ty_disp
                CHANGING  cs_poheader TYPE zsmm_poheader
                          cs_poheaderx TYPE zsmm_poheaderx.

  CONSTANTS : lc_impo(4) VALUE 'IMPO'.

  IF p_impo = abap_true.
    DATA(lv_incov) = lc_impo.
  ELSE.
    CLEAR : lv_incov.
  ENDIF.

*  DATA: LV_PO_FIELD(50) TYPE C,
*        LV_IF_FIELD(50) TYPE C.

**********************************************************************
*1. I/F 데이터 변환
**********************************************************************
  cs_poheader  = VALUE #(
                  ebeln            =  space
                  bukrs            =  p_bukrs   "회사 코드
                  esart             =  is_disp-bsart  "구매문서유형
                  ekorg            =  p_ekorg  "구매 조직
                  bkgrp            =  is_disp-ekgrp  "구매 그룹
                  everk             =  is_disp-verkf  "업체담당자
                  ebdat             =  is_disp-bedat  "계약체결일
                  zterm             =  is_disp-zterm  "지급 조건 키
                  inco1             =  is_disp-inco1  "인도 조건(파트 1)
                  waers             =  is_disp-waers  "통화 키
                  dppcnt            =  is_disp-dppct  "선급비율
                  absgr             =  is_disp-absgr  "세금계산서 발행방식(1정발행,2 역발행)
                  incov              = lv_incov
                  inco2_l           =  is_disp-inco2_l  "선적항
                  inco3_l             =  is_disp-inco3_l  "도착항

                  unsez             =  is_disp-cntr_no  "
*                  HERKL             =  IS_DISP-HERKL  "원산지

*            DPPCNT             = IS_DISP-DPPCT  "선금비율
                        ).

  "공급업체 계정 번호
  _g_conv_data_ext_to_sap is_disp-lifnr '' cs_poheader-elifn.

  "선급비율 존재 시 TODAY 를 선급만기일로 지정.
  IF NOT is_disp-dppct IS INITIAL.
    cs_poheader-dptyp = 'V'.  "임의선급 지정
  ENDIF.

*> 'X' 필드 설정을 위함.
  DATA : lv_tab   TYPE ddobjname VALUE 'ZSMM_POHEADER',
         lt_dfies TYPE TABLE OF dfies.
  CALL FUNCTION 'DDIF_FIELDINFO_GET'
    EXPORTING
      tabname   = lv_tab
    TABLES
      dfies_tab = lt_dfies.

**poheaderx
  "생성 모드 시 값 존재하면 모두 'X'설정.
  LOOP AT lt_dfies INTO DATA(ls_dfies).
    DATA(lv_field) = 'CS_POHEADER-' && ls_dfies-fieldname.
    ASSIGN (lv_field) TO FIELD-SYMBOL(<lrf_field>).

    IF <lrf_field> IS NOT INITIAL.
      DATA(lv_fieldx) =  'CS_POHEADERX-' && ls_dfies-fieldname.
      ASSIGN (lv_fieldx) TO FIELD-SYMBOL(<lrf_fieldx>).
      <lrf_fieldx> = abap_true.
    ENDIF.

  ENDLOOP.
  CLEAR : lv_field, lv_fieldx.

ENDFORM.
*&----------------------------------------------------------*
*& Form HEADEROTHER_INFO
*&----------------------------------------------------------*
FORM headerother_info USING    is_disp       TYPE ty_disp
                      CHANGING cs_headeradd  TYPE zsmm_poheaderother
                               cs_headeraddx TYPE zsmm_poheaderotherx.

  DATA: lv_tab   TYPE ddobjname VALUE 'ZSMM_POHEADEROTHER',
        lt_dfies TYPE TABLE OF dfies.

* HEADERADD
  cs_headeradd = VALUE #( "/BOFU/BCSD_SUBJ   = IS_DISP-TITLE "발주명
                          zorder_person     = is_disp-order "발주담당
                           zorder_department     = is_disp-zorder_department "발부부서
                          zexpen_person = is_disp-exper "지출발의담당
                          zexpen_department     = is_disp-zexpen_department "지출발의부서
                          zreal_cost = is_disp-zreal_cost "실비정산여부
                          zqm_person = is_disp-qmper "검수담당자
                          zqm_department     = is_disp-zqm_department "검수부서

                          zeinspect = is_disp-zeinspect "수입검사여부
                          zemanage2 = is_disp-zemanage2 "관리번호
                          zeshiptype = is_disp-zeshiptype "선적구분
                          zededline = is_disp-zededline "선적기한
                        ).

  "발주명은 계약번호-차수+발주명(AS-IS)
  cs_headeradd-/bofu/bcsd_subj = is_disp-cntr_no && '-' && is_disp-cntr_rev && '-' && is_disp-title.
* HEADERADDX

  CONSTANTS: lc_to_field(30)  TYPE c VALUE 'CS_HEADERADD-',
             lc_to_fieldx(30) TYPE c VALUE 'CS_HEADERADDX-'.

  DATA: lv_field(100)  TYPE c,
        lv_fieldx(100) TYPE c.

  CALL FUNCTION 'DDIF_FIELDINFO_GET'
    EXPORTING
      tabname   = lv_tab
    TABLES
      dfies_tab = lt_dfies.

  LOOP AT lt_dfies INTO DATA(ls_dfies).
    lv_field = lc_to_field && ls_dfies-fieldname.
    ASSIGN (lv_field) TO FIELD-SYMBOL(<lrf_field>).

    IF <lrf_field> IS NOT INITIAL.
      lv_fieldx =  lc_to_fieldx && ls_dfies-fieldname.
      ASSIGN (lv_fieldx) TO FIELD-SYMBOL(<lrf_fieldx>).

      <lrf_fieldx> = abap_true.
    ENDIF.

  ENDLOOP.

ENDFORM.
*&----------------------------------------------------------*
*& Form ITEM_INFO
*&----------------------------------------------------------*
FORM item_info TABLES ct_item  STRUCTURE zsmm_poitem
                      ct_itemx STRUCTURE zsmm_poitemx
               USING  is_disp  TYPE ty_disp.

  DATA: lv_amt TYPE bapicurr-bapicurr.

  DATA: ls_item  TYPE zsmm_poitem,
        ls_itemx TYPE zsmm_poitemx.

  DATA: lv_tab   TYPE ddobjname VALUE 'ZSMM_POITEM',
        lt_dfies TYPE TABLE OF dfies.

* ITEM
  ls_item-ebelp = is_disp-ebelp.    "PO 품목

  "범주
  IF NOT is_disp-mtype IS INITIAL.
    ls_item-pstyp = '3'. "임가공발주면...
  ENDIF.

  ls_item-matnr = is_disp-matnr.    "자재
  ls_item-bwtar = is_disp-bwtar.    "평가유형
  ls_item-bstme = is_disp-meins.    "단위
  ls_item-bstmg = is_disp-menge.    "수량

  "단가
  CALL FUNCTION 'BAPI_CURRENCY_CONV_TO_EXTERNAL'
    EXPORTING
      currency        = is_disp-waers
      amount_internal = is_disp-netpr
    IMPORTING
      amount_external = lv_amt.
  ls_item-net_price = lv_amt.

  ls_item-epein = is_disp-peinh.  "가격단위
  ls_item-mwskz = is_disp-mwskz.  "세금코드
  ls_item-zprice_change =  is_disp-zprice_change.   "조정사유코드
  ls_item-zprice_reason =  is_disp-zprice_reason.   "조정사유상세
  ls_item-ewerk = is_disp-werks.  "플랜트
  ls_item-lgort = is_disp-lgort.  "저장위치
  ls_item-idnlf = is_disp-idnlf.  "공급업체 자재번호
  ls_item-charg = is_disp-charg.  "배치번호.

  ls_item-supp_vendor = is_disp-sc_vender.
  IF is_disp-sc_vender IS NOT INITIAL.
    ls_item-lblkz = abap_true..
  ENDIF.

  IF is_disp-wbsno IS NOT INITIAL.   "WBS
    ls_item-knttp = 'P'.    "계정지정범주
  ELSE.

    IF is_disp-vbeln IS NOT INITIAL.  "영업오더
      ls_item-knttp = 'M'.    "계정지정범주
      ls_item-pstyp = ''.    "품목범주
    ELSE.

      IF is_disp-kostl IS NOT INITIAL.  "코스트센터
        ls_item-knttp = 'K'.    "계정지정범주
      ENDIF.
    ENDIF.
  ENDIF.

* ITEMX
  CALL FUNCTION 'DDIF_FIELDINFO_GET'
    EXPORTING
      tabname   = lv_tab
    TABLES
      dfies_tab = lt_dfies.

  ls_itemx-ebelp = ls_item-ebelp.

  LOOP AT lt_dfies INTO DATA(ls_dfies).

    CHECK sy-tabix > 1.   "키 값은 제외

    DATA(lv_field) = 'LS_ITEM-' && ls_dfies-fieldname.
    ASSIGN (lv_field) TO FIELD-SYMBOL(<lrf_field>).

    IF <lrf_field> IS NOT INITIAL.
      DATA(lv_fieldx) =  'LS_ITEMX-' && ls_dfies-fieldname.
      ASSIGN (lv_fieldx) TO FIELD-SYMBOL(<lrf_fieldx>).

      <lrf_fieldx> = abap_true.
    ENDIF.

  ENDLOOP.

  APPEND: ls_item  TO ct_item,
          ls_itemx TO ct_itemx.

  CLEAR: lv_field, lv_fieldx.

ENDFORM.
*&----------------------------------------------------------*
*& Form addrdelivery_info
*&----------------------------------------------------------*
*& text
*&----------------------------------------------------------*
*&      --> IT_POADDRDELIVERY
*&----------------------------------------------------------*
FORM addrdelivery_info  TABLES
                   ct_poaddrdelivery STRUCTURE zsmm_poaddrdelivery
             USING is_disp TYPE ty_disp.

  DATA : lv_werks LIKE t001w-werks,
         ls_t001w LIKE t001w,
         lv_lifnr TYPE  lifnr,
         ls_lfa1  LIKE  lfa1.

*> 임가공 일때만 설정.
  IF NOT is_disp-mtype IS INITIAL.
    CASE p_bukrs.
      WHEN '1101'.
        lv_werks = is_disp-umwrk.
        CLEAR : ls_t001w.
        CALL FUNCTION 'T001W_READ'
          EXPORTING
            werks    = lv_werks
          IMPORTING
            struct   = ls_t001w
          EXCEPTIONS
            no_entry = 1
            OTHERS   = 2.
    ENDCASE.

    APPEND VALUE #( ebelp = is_disp-ebelp
                               addr_no = ls_t001w-adrnr )
*                              name2 = ls_item-name2 )
                         TO ct_poaddrdelivery.

  ELSEIF  is_disp-sc_vender IS NOT INITIAL.
    lv_lifnr = is_disp-sc_vender.
    CALL FUNCTION 'LFA1_READ_SINGLE'
      EXPORTING
        id_lifnr            = lv_lifnr
      IMPORTING
        es_lfa1             = ls_lfa1
      EXCEPTIONS
        not_found           = 1
        input_not_specified = 2
        lifnr_blocked       = 3
        OTHERS              = 4.

    APPEND VALUE #( ebelp   = is_disp-ebelp
                    land1   = ls_lfa1-land1
                    supp_vendor = is_disp-sc_vender
                    lblkz   = 'X' ) TO ct_poaddrdelivery.

  ENDIF.

*  ct_poaddrdelivery[]
*   ebelp    =  ebelp  "구매 문서 번호
*   name1  =  name1  "이름 1
*   name2  =  name2   "수행장소
*   city1     =  city1  "도시
*   city2     =  city2  "지역
*   street   =  street  "도로 주소
*   land1    =  land1  "국가 키
*   ekunnr  =  ekunnr  "고객
*   lblkz  =  lblkz   "외주 공급업체

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SCHEDULE_INFO
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM schedule_info  TABLES ct_poschedule STRUCTURE zsmm_poschedule
                           ct_poschedulex STRUCTURE zsmm_poschedulex
                        USING  is_disp  TYPE ty_disp.

**   schedule
  DATA ls_schedule LIKE zsmm_poschedule.

**   schedulex
  DATA : lv_tab       TYPE ddobjname VALUE 'ZSMM_POSCHEDULE',
         lt_dfies     TYPE TABLE OF dfies,
         ls_schedulex TYPE zsmm_poschedulex.

*  DATA: LV_PO_FIELD(50) TYPE C.

  CONSTANTS: lc_int_date(8) VALUE '00000000'.

  CALL FUNCTION 'DDIF_FIELDINFO_GET'
    EXPORTING
      tabname   = lv_tab
    TABLES
      dfies_tab = lt_dfies.


  CLEAR: ls_schedule, ls_schedulex.


  ls_schedule-ebelp = is_disp-ebelp. "구매 문서 품목 번호
  ls_schedule-eeind = is_disp-eindt. "납품일
  ls_schedule-etmen = is_disp-menge. "예정 수량 (po item 수량)

  IF ls_schedule-eeind = lc_int_date.
    ls_schedule-eeind = space.
  ENDIF.

  CLEAR : ls_schedulex.
  ls_schedulex-ebelp = ls_schedule-ebelp.

  LOOP AT lt_dfies INTO DATA(ls_dfies).
    CHECK sy-tabix > 2." 키값은 제외.
    DATA(lv_field) = 'LS_SCHEDULE-' && ls_dfies-fieldname.
    ASSIGN (lv_field) TO FIELD-SYMBOL(<lrf_field>).

    IF <lrf_field> IS NOT INITIAL.
      DATA(lv_fieldx) =  'LS_SCHEDULEX-' && ls_dfies-fieldname.
      ASSIGN (lv_fieldx) TO FIELD-SYMBOL(<lrf_fieldx>).
      <lrf_fieldx> = abap_true.
    ENDIF.

  ENDLOOP.

  APPEND ls_schedule TO ct_poschedule.
  APPEND ls_schedulex TO ct_poschedulex.

  CLEAR : lv_field, lv_fieldx.

ENDFORM.
*&----------------------------------------------------------*
*& Form ACCOUNT_INFO
*&----------------------------------------------------------*
FORM account_info TABLES ct_account  STRUCTURE zsmm_poaccount
                         ct_accountx STRUCTURE zsmm_poaccountx
                  USING  is_disp     TYPE ty_disp.

*  CONSTANTS: LC_INT_DATE(8) VALUE '00000000'.

  DATA: ls_account  TYPE zsmm_poaccount,
        ls_accountx TYPE zsmm_poaccountx.

  DATA : lv_tab   TYPE ddobjname VALUE 'ZSMM_POACCOUNT',
         lt_dfies TYPE TABLE OF dfies.

* ACCOUNT
  ls_account-ebelp = is_disp-ebelp.
  ls_account-saknr = is_disp-sakto.
  ls_account-kostl = is_disp-kostl.


  IF is_disp-wbsno IS INITIAL.
    ls_account-ps_posid = space.
  ELSE.
    ls_account-ps_posid = is_disp-wbsno. "CONV 안함. (EXCEL 이 EXTERNAL 임)
*    CALL FUNCTION 'CONVERSION_EXIT_ABPSP_OUTPUT'
*      EXPORTING
*        INPUT  = IS_DISP-WBSNO
*      IMPORTING
*        OUTPUT = LS_ACCOUNT-PS_POSID.
  ENDIF.
*
*  LS_ACCOUNT-VBELN = IS_DISP-VBELN.
*  LS_ACCOUNT-VBELP = IS_DISP-VBELP.


* ACCOUNTX
  CALL FUNCTION 'DDIF_FIELDINFO_GET'
    EXPORTING
      tabname   = lv_tab
    TABLES
      dfies_tab = lt_dfies.

  ls_accountx-ebelp = ls_account-ebelp.

  LOOP AT lt_dfies INTO DATA(ls_dfies).

    CHECK sy-tabix > 2.   "키 값은 제외

    DATA(lv_field) = 'LS_ACCOUNT-' && ls_dfies-fieldname.
    ASSIGN (lv_field) TO FIELD-SYMBOL(<lrf_field>).

    IF <lrf_field> IS NOT INITIAL.
      DATA(lv_fieldx) =  'LS_ACCOUNTX-' && ls_dfies-fieldname.
      ASSIGN (lv_fieldx) TO FIELD-SYMBOL(<lrf_fieldx>).

      <lrf_fieldx> = abap_true.
    ENDIF.

  ENDLOOP.

  APPEND: ls_account  TO ct_account,
          ls_accountx TO ct_accountx.

  CLEAR: lv_field, lv_fieldx.

ENDFORM.
*&----------------------------------------------------------*
*& Form CONDITION_INFO
*&----------------------------------------------------------*
FORM condition_info TABLES ct_cond  STRUCTURE zsmm_pocond
                           ct_condx STRUCTURE zsmm_pocondx
                    USING  is_disp  TYPE ty_disp.

*  DATA: LS_COND  TYPE ZSMM_POCOND,
*        LS_CONDX TYPE ZSMM_POCONDX.

  DATA: lv_amt    TYPE bapicurr-bapicurr,
        lv_change,
        lv_kscha  TYPE kscha,
        lv_zaehk  TYPE zsmm_pocond-zaehk,
        lv_stunr  TYPE zsmm_pocond-stunr.
*        LV_LIFNR TYPE EKKO-LIFNR.

  CONSTANTS : lc_pbxx(4) VALUE 'PBXX',
              lc_zcpr(4) VALUE 'ZCPR',
              lc_00(4)   VALUE '0.00'.


*  LV_AMT = IS_DISP-NETPR.
  CALL FUNCTION 'BAPI_CURRENCY_CONV_TO_EXTERNAL'
    EXPORTING
      currency        = is_disp-waers
      amount_internal = is_disp-netpr
    IMPORTING
      amount_external = lv_amt.

  lv_kscha = lc_pbxx.

  "정보레코드 존재여부에 따라 SEQ 결정.
  READ TABLE gt_zsvbmminfoprice WITH KEY lifnr = is_disp-lifnr
                                         matnr = is_disp-matnr
                                BINARY SEARCH
                                TRANSPORTING NO FIELDS.
  IF sy-subrc EQ 0.
    lv_zaehk = '02'.
  ELSE.
    lv_zaehk = '01'.
  ENDIF.

  lv_change = 'I'.
  lv_stunr = '001'.

***   condition
  APPEND VALUE #( kposn = is_disp-ebelp
                           stunr = lv_stunr
                           zaehk = lv_zaehk
                           kscha = lv_kscha
                           cond_value = lv_amt
                           waers = is_disp-waers
                           kmein = is_disp-meins
                           kpein = is_disp-peinh
                           kumza = 1
                           kumne = 1
                           change = lv_change ) "무조건적으로 UPDATE 임.. (기본 PBXX 제공으로 U 로만 진행)
                   TO ct_cond.
**  conditionx
  APPEND VALUE #( kposn = is_disp-ebelp
                           stunr = lv_stunr
                           zaehk = 'X'
                           kscha = 'X'
                           cond_value = 'X'
                           kmein = 'X'
                           kpein = 'X'
                           kumza = 'X'
                           kumne = 'X'
                           waers = 'X'
                           change = 'X' )
                   TO ct_condx.

*  품목 가감액.
  IF is_disp-miuwr IS NOT INITIAL AND
     is_disp-miuwr NE lc_00.
    CALL FUNCTION 'BAPI_CURRENCY_CONV_TO_EXTERNAL'
      EXPORTING
        currency        = is_disp-waers
        amount_internal = is_disp-miuwr
      IMPORTING
        amount_external = lv_amt.

    lv_kscha = lc_zcpr.
    ADD 1 TO lv_stunr.

***   CONDITION
    APPEND VALUE #( kposn = is_disp-ebelp
                             stunr = lv_stunr
                             zaehk = '01'
                             kscha = lv_kscha
                             cond_value = lv_amt
                             waers = is_disp-waers
                             kmein = is_disp-meins
                             kpein = is_disp-peinh
                             kumza = 1
                             kumne = 1
                             change = 'I' )
                     TO ct_cond.
**  CONDITIONX
    APPEND VALUE #( kposn = is_disp-ebelp
                             stunr = '001'
                             zaehk = 'X'
                             kscha = 'X'
                             cond_value = 'X'
                             kmein = 'X'
                             kpein = 'X'
                             kumza = 'X'
                             kumne = 'X'
                             waers = 'X'
                             change = 'X' )
                     TO ct_condx.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CREATE_PO
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM create_po USING iv_testrun .

  DATA : ls_header        TYPE zsmm_poheader,
         ls_headerx       TYPE  zsmm_poheaderx,
         ls_headerother   TYPE  zsmm_poheaderother,
         ls_headerotherx  TYPE  zsmm_poheaderotherx,
         lv_auth          TYPE  bapiflag,
         lv_price_from_po TYPE  bapiflag VALUE 'X',
         lv_purchaseorder LIKE  bapimepoheader-po_number,
         ls_result_header LIKE  bapimepoheader,
         lt_return        LIKE TABLE OF bapiret2,
         lt_item          LIKE TABLE OF  zsmm_poitem,
         lt_itemx         LIKE TABLE OF  zsmm_poitemx,
         lt_addrdelivery  LIKE TABLE OF  zsmm_poaddrdelivery,
         lt_schedule      LIKE TABLE OF  zsmm_poschedule,
         lt_schedulex     LIKE TABLE OF  zsmm_poschedulex,
         lt_account       LIKE TABLE OF  zsmm_poaccount,
         lt_accountx      LIKE TABLE OF  zsmm_poaccountx,
         lt_cond          LIKE TABLE OF  zsmm_pocond,
         lt_condx         LIKE TABLE OF  zsmm_pocondx,
         lt_textheader    LIKE TABLE OF  zsmm_potextheader.
*         LT_PARTNER      LIKE TABLE OF  ZSMM_POPARTNER,
*         LT_COMPONENTS   LIKE TABLE OF  ZSMM_POCOMPONENTS,
*         LT_COMPONENTSX  LIKE TABLE OF  ZSMM_POCOMPONENTSX,
*         LT_SHIPPING     LIKE TABLE OF  ZSMM_POSHIPPING,
*         LT_SHIPPINGX    LIKE TABLE OF  ZSMM_POSHIPPINGX.

  DATA: BEGIN OF ls_result,
          cntr_no     TYPE ze_cntr_no,                 "AS-IS 계약번호
          cntr_rev    TYPE ze_cntr_rev,                "AS-IS 차수
          ebeln       TYPE ekko-ebeln,
          return      TYPE bapiret2_t,
          error_exist TYPE c,
        END OF ls_result,
        lt_result LIKE TABLE OF ls_result.

  DATA lv_ebelp TYPE ekpo-ebelp.

  DATA(lt_coll) = gt_coll.

  DATA: lv_total       TYPE i,
        lv_succe       TYPE i,
        lv_error       TYPE i,
        lv_error_exist TYPE c.

  PERFORM get_data_for_bapi.

*-----------------------------
* AS-IS 계약번호, 차수별 계약 생성
*-----------------------------
  SORT: gt_disp BY cntr_no cntr_rev cntr_item.

  SORT lt_coll BY cntr_no cntr_rev.
  DELETE ADJACENT DUPLICATES FROM lt_coll COMPARING cntr_no cntr_rev.

  LOOP AT lt_coll INTO DATA(ls_coll).
    CLEAR: ls_header, ls_headerx, ls_headerother, ls_headerotherx, lv_auth,
           lv_price_from_po, lv_purchaseorder, ls_result_header, lt_return,
           lt_item,lt_itemx, lt_addrdelivery, lt_schedule, lt_schedulex,
           lt_account, lt_accountx, lt_cond, lt_condx, lt_textheader,
           ls_result, lv_ebelp, lv_error_exist.
*           LT_PARTNER, LT_COMPONENTS, LT_COMPONENTSX, LT_SHIPPING, LT_SHIPPINGX.

    ls_result-cntr_no = ls_coll-cntr_no.
    ls_result-cntr_rev = ls_coll-cntr_rev.

    READ TABLE gt_disp WITH KEY cntr_no   = ls_coll-cntr_no
                                cntr_rev  = ls_coll-cntr_rev
                                BINARY SEARCH
                                TRANSPORTING NO FIELDS.

    LOOP AT gt_disp ASSIGNING FIELD-SYMBOL(<ls_disp>) FROM sy-tabix.

      IF <ls_disp>-cntr_no   <> ls_coll-cntr_no  OR
         <ls_disp>-cntr_rev  <> ls_coll-cntr_rev.
        EXIT.
      ENDIF.

      IF <ls_disp>-statu     = icon_red_light.
        ls_result-error_exist = 'X'.
        lv_error_exist = 'X'.
        CONTINUE.
      ENDIF.

* Header Data
      IF ls_header IS INITIAL.
        PERFORM header_info USING    <ls_disp>
                            CHANGING ls_header ls_headerx.

        PERFORM headerother_info USING    <ls_disp>
                                 CHANGING ls_headerother ls_headerotherx.

*          PERFORM TEXTHEADER_INFO TABLES LT_TEXTHEADER USING <LS_DISP>.
      ENDIF.

* Item Data
*      LV_EBELP = LV_EBELP + 10.
*      <LS_DISP>-EBELP = LV_EBELP.
      <ls_disp>-ebelp = <ls_disp>-cntr_item.

      PERFORM item_info TABLES lt_item lt_itemx
                        USING  <ls_disp>.


*Addresses for Inward Delivery (Item)
      PERFORM addrdelivery_info TABLES lt_addrdelivery
                                    USING <ls_disp>.

*Delivery Schedule
      PERFORM schedule_info TABLES lt_schedule lt_schedulex
                            USING <ls_disp>.

* Account Assignment
      PERFORM account_info TABLES lt_account lt_accountx
                           USING  <ls_disp>.

* Conditions
      PERFORM condition_info TABLES lt_cond lt_condx
                             USING  <ls_disp>.

    ENDLOOP.

*  po create.
    IF ls_result-error_exist IS INITIAL.
      CALL FUNCTION 'ZFMM_PO_CREATE'
        EXPORTING
          is_poheader       = ls_header
          is_poheaderx      = ls_headerx
          is_poheaderother  = ls_headerother
          is_poheaderotherx = ls_headerotherx
          iv_auth           = lv_auth
          iv_price_from_po  = lv_price_from_po
          iv_testrun        = iv_testrun
        IMPORTING
          ev_purchaseorder  = lv_purchaseorder
          ev_header         = ls_result_header
        TABLES
          et_return         = lt_return
          it_poitem         = lt_item
          it_poitemx        = lt_itemx
          it_poaddrdelivery = lt_addrdelivery
          it_poschedule     = lt_schedule
          it_poschedulex    = lt_schedulex
          it_poaccount      = lt_account
          it_poaccountx     = lt_accountx
          it_pocond         = lt_cond
          it_pocondx        = lt_condx
          it_potextheader   = lt_textheader.
*        IT_POPARTNER      = LT_PARTNER
*        IT_POCOMPONENTS   = LT_COMPONENTS
*        IT_POCOMPONENTSX  = LT_COMPONENTSX
*        IT_POSHIPPING     = LT_SHIPPING
*        IT_POSHIPPINGX    = LT_SHIPPINGX.
    ENDIF.

**-----------------------------
** Return
**-----------------------------
    IF lv_purchaseorder IS NOT INITIAL.
      ls_result-ebeln = lv_purchaseorder.
*
*      DO 10 TIMES.
*        ZCL_MM_COMMON=>DATA_EXIST_CHECK(
*        EXPORTING  IS_TABLE =  VALUE #(
*                          TABLE = 'EKKO' FIELD = 'EBELN' )
*                            IT_WHERE = VALUE #(
*                            ( FIELD = 'EBELN'
*                              VALUE = LV_PURCHASEORDER )
*                                                             )
*         IMPORTING EV_SUBRC = DATA(LV_SUBRC) ).
*        IF LV_SUBRC EQ 0.
*          EXIT.
*        ELSE.
*          WAIT UP TO 1 SECONDS.
*        ENDIF.
*      ENDDO.
    ENDIF.

    ls_result-return[] = lt_return[].
    APPEND ls_result TO lt_result.
  ENDLOOP.

  DATA(lt_tmp_result) = lt_result[].
  SORT lt_tmp_result BY ebeln.
  DELETE ADJACENT DUPLICATES FROM lt_tmp_result COMPARING ebeln.
  IF NOT lt_tmp_result[] IS INITIAL.
    SELECT ebeln
      FROM ekko
       FOR ALL ENTRIES IN @lt_tmp_result
     WHERE ebeln = @lt_tmp_result-ebeln
      INTO TABLE @DATA(lt_ekko).
    FREE lt_tmp_result.
    SORT lt_ekko BY ebeln.
  ENDIF.

  ls_result-cntr_no = ls_coll-cntr_no.
  ls_result-cntr_rev = ls_coll-cntr_rev.

  SORT lt_result BY cntr_no cntr_rev.

  LOOP AT gt_disp ASSIGNING <ls_disp>.

    "계약 번호/차수 기준으로 결과 검색..
    READ TABLE lt_result INTO ls_result
                         WITH KEY cntr_no = <ls_disp>-cntr_no
                                  cntr_rev = <ls_disp>-cntr_rev
                        BINARY SEARCH.

    IF ls_result-error_exist IS INITIAL.
      CLEAR: <ls_disp>-message.

      _g_add_1 lv_total.
    ELSE.
      CONTINUE.
    ENDIF.

    IF NOT iv_testrun IS INITIAL.
      READ TABLE ls_result-return WITH KEY type = 'E'
                                  TRANSPORTING NO FIELDS.
      IF sy-subrc EQ 0.
        _g_add_1 lv_error.
        <ls_disp>-statu = icon_red_light.

        PERFORM conv_msg_v2 TABLES ls_result-return
                            USING 'E'
                            CHANGING <ls_disp>-msgtb
                                     <ls_disp>-message.
      ELSE.
        _g_add_1 lv_succe.
      ENDIF.

    ELSE.
      "PO 번호가 있을때
      IF NOT ls_result-ebeln IS INITIAL.
        "실제 TABLE 에 있으면 성공으로 간주하여 PO 번호 표시
        READ TABLE lt_ekko WITH KEY ebeln = ls_result-ebeln
                           BINARY SEARCH
                           TRANSPORTING NO FIELDS.
        IF sy-subrc EQ 0.
          <ls_disp>-ebeln = ls_result-ebeln.
          <ls_disp>-statu = icon_green_light.
          _g_add_1 lv_succe.

          "실제 TABLE 에 없으면 RETURN 메시지 전체 표시.
        ELSE.
          PERFORM conv_msg_v2 TABLES ls_result-return
                              USING space
                              CHANGING <ls_disp>-msgtb
                                       <ls_disp>-message. "의미.. X
          <ls_disp>-statu = icon_red_light.
          <ls_disp>-message = 'Commit 오류: 전체 메시지를 확인하세요.'(m20).

          _g_add_1 lv_error.
        ENDIF.
        "PO 번호가 없으면 ERROR 메시지만 표시.
      ELSE.
        PERFORM conv_msg_v2 TABLES ls_result-return
                            USING 'E'
                            CHANGING <ls_disp>-msgtb
                                     <ls_disp>-message.
        <ls_disp>-statu = icon_red_light.

        _g_add_1 lv_error.
      ENDIF.
    ENDIF.
  ENDLOOP.

  IF lv_error_exist EQ 'X'.
    MESSAGE i000 WITH 'Error 건은 제외하였으니 다시 Upload 바립니다!'(m25).
  ENDIF.

*--> 결과 메시지 출력.
  IF iv_testrun IS INITIAL.
    IF lv_total = lv_succe.
      MESSAGE s009 WITH lv_total.
    ELSE.
      MESSAGE s014 WITH lv_total lv_succe lv_error.
    ENDIF.
  ELSE.
    MESSAGE s014 WITH lv_total lv_succe lv_error.
  ENDIF.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form CONV_MSG_V1
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM conv_msg_v2  TABLES   it_return STRUCTURE bapiret2
                  USING iv_msgty_cond
                  CHANGING ev_msgtb TYPE zycn00001
                           ev_message.

  CONSTANTS: lc_field TYPE fieldname VALUE 'ZMBLNO',
             lc_msgid TYPE sy-msgid VALUE 'ZMM01',
             lc_msgno TYPE sy-msgno VALUE '001'.

  DATA ls_msgtb TYPE zscn00001.

  LOOP AT it_return INTO DATA(ls_return).
    CLEAR: ls_msgtb.

    "특정 메시지 타입만 표시하기 위함.
    IF NOT iv_msgty_cond IS INITIAL AND ls_return-type NE iv_msgty_cond.
      CONTINUE.
    ENDIF.

    ls_msgtb = VALUE #( fieldname = lc_field
                        msgty = ls_return-type
                        arbgb = ls_return-id
                        txtnr = ls_return-number
                        msgv1 = ls_return-message_v1
                        msgv2 = ls_return-message_v2
                        msgv3 = ls_return-message_v3
                        msgv4 = ls_return-message_v4  ).

    IF ls_msgtb-arbgb IS INITIAL.
      ls_msgtb-arbgb = lc_msgid.
      ls_msgtb-txtnr = lc_msgno.
      ls_msgtb-msgv1 = ls_return-message.
    ENDIF.

    "대표 메시지만 표시. (가능한 마지막 메시지를 표시)
    MESSAGE ID ls_msgtb-arbgb
            TYPE 'S'
            NUMBER ls_msgtb-txtnr
            WITH ls_msgtb-msgv1
                 ls_msgtb-msgv2
                 ls_msgtb-msgv3
                 ls_msgtb-msgv4
            INTO ev_message.

    APPEND ls_msgtb TO ev_msgtb.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form PROCESSING_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM processing_data .

  DATA: ls_coll TYPE ty_coll.

  DATA : ls_makt TYPE makt.

  CHECK gt_disp IS NOT INITIAL.

  LOOP AT gt_disp ASSIGNING FIELD-SYMBOL(<ls_disp>).

* Display Data
    <ls_disp>-row_key = sy-tabix.

    <ls_disp>-statu = icon_yellow_light.
    CLEAR: <ls_disp>-message, <ls_disp>-msgtb.

    "Conversion
    _g_conv_data_ext_to_sap <ls_disp>-lifnr '' <ls_disp>-lifnr.
    _g_conv_data_ext_to_sap <ls_disp>-bedat '' <ls_disp>-bedat.
    _g_conv_data_ext_to_sap <ls_disp>-waers '' <ls_disp>-waers.

    _g_conv_data_ext_to_sap <ls_disp>-matnr '' <ls_disp>-matnr.
    _g_conv_data_ext_to_sap <ls_disp>-meins '' <ls_disp>-meins.
    _g_conv_data_ext_to_sap <ls_disp>-menge <ls_disp>-meins <ls_disp>-menge.
    _g_conv_data_ext_to_sap <ls_disp>-netpr <ls_disp>-waers <ls_disp>-netpr.
    _g_conv_data_ext_to_sap <ls_disp>-miuwr <ls_disp>-waers <ls_disp>-miuwr.
    _g_conv_data_ext_to_sap <ls_disp>-eindt '' <ls_disp>-eindt.
    _g_conv_data_ext_to_sap <ls_disp>-sc_vender '' <ls_disp>-sc_vender.
    _g_conv_data_ext_to_sap <ls_disp>-sakto '' <ls_disp>-sakto.
    _g_conv_data_ext_to_sap <ls_disp>-kostl '' <ls_disp>-kostl.

    CLEAR : ls_makt.
    CALL FUNCTION 'MAKT_SINGLE_READ'
      EXPORTING
        matnr = <ls_disp>-matnr
        spras = sy-langu
      IMPORTING
        wmakt = ls_makt.
    IF ls_makt-maktx IS NOT INITIAL.
      <ls_disp>-maktx = ls_makt-maktx.
    ENDIF.

    "WBS CONV 안함(BAPI 수행 시 EXTERNAL 한 값으로 전달해야 함)
*    _G_CONV_DATA_EXT_TO_SAP <LS_DISP>-WBSNO '' <LS_DISP>-WBSNO.

    "필수 체크
    PERFORM check_data_01 CHANGING <ls_disp>.

* AS-IS 계약번호, 차수 별 Collect
    CLEAR ls_coll.
    MOVE-CORRESPONDING <ls_disp> TO ls_coll.
    ls_coll-count = 1.
    COLLECT ls_coll INTO gt_coll.

  ENDLOOP.

  SORT gt_coll BY cntr_no cntr_rev.

  PERFORM get_check_data.

  PERFORM processing_data_etc.

  DESCRIBE TABLE gt_disp LINES DATA(lv_tcnt).
  MESSAGE s011(zmm01) WITH lv_tcnt.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form CHECK_DATA_01
*&---------------------------------------------------------------------*
FORM check_data_01 CHANGING cs_disp TYPE ty_disp.

  CONSTANTS: lc_msgid TYPE sy-msgid VALUE 'ZMM01',
             lc_msgno TYPE sy-msgno VALUE '017'.

  DATA: ls_msgtb TYPE zscn00001.

  DEFINE _l_set_message.

    IF &1 IS INITIAL.
      ls_msgtb-msgv1 = &2.
      APPEND ls_msgtb TO cs_disp-msgtb.
    ENDIF.

  END-OF-DEFINITION.

  ls_msgtb-arbgb = lc_msgid.
  ls_msgtb-txtnr = lc_msgno.
  ls_msgtb-msgty = 'E'.

**********************************************************************
*> HEADER 필수 사항
**********************************************************************
*> 공통
  _l_set_message cs_disp-cntr_no '계약번호'.
  _l_set_message cs_disp-cntr_item '품목'.
  _l_set_message cs_disp-bsart '문서유형'.
  _l_set_message cs_disp-ekgrp '구매그룹'.
  _l_set_message cs_disp-order '발주담당자'.
  _l_set_message cs_disp-lifnr '공급업체'.
  _l_set_message cs_disp-bedat '계약체결일'.
  _l_set_message cs_disp-zterm '지급조건'.
  _l_set_message cs_disp-inco1 '인도조건'.
  _l_set_message cs_disp-waers '통화'.

*> 국내발주 전용 필수 체크
  IF p_dopo = 'X'.
    _l_set_message cs_disp-absgr '세금계산서발행방식'.

*> 수입발주 전용 필수 체크
  ELSE.
    _l_set_message cs_disp-zeshiptype '선적구분'.
    _l_set_message cs_disp-zededline '선적기한'.
    _l_set_message cs_disp-inco2_l '선적항'.
    _l_set_message cs_disp-inco3_l '도착항'.
  ENDIF.

**********************************************************************
*> ITEM 필수 사항
**********************************************************************
*> 공통
  "자재 필수
  _l_set_message cs_disp-matnr '자재'.
  _l_set_message cs_disp-meins '단위'.
  _l_set_message cs_disp-menge '수량'.
  _l_set_message cs_disp-netpr '단가'.
  _l_set_message cs_disp-peinh '가격단위'.
  _l_set_message cs_disp-werks '플랜트'.
  _l_set_message cs_disp-lgort '창고'.

  IF p_dopo = 'X'..
    _l_set_message cs_disp-eindt '납품일'.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form PROCESSING_DATA_ETC
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM processing_data_etc .

  LOOP AT gt_disp ASSIGNING FIELD-SYMBOL(<ls_disp>).

    "값 정합성 체크
    PERFORM check_data_02 CHANGING <ls_disp>.

    "ERROR 존재 시 대표 1건 표시
    IF NOT <ls_disp>-msgtb[] IS INITIAL.
      READ TABLE <ls_disp>-msgtb INTO DATA(ls_msgtb) INDEX 1.
      MESSAGE ID ls_msgtb-arbgb
              TYPE 'S'
              NUMBER ls_msgtb-txtnr
              WITH ls_msgtb-msgv1
                   ls_msgtb-msgv2
                   ls_msgtb-msgv3
                   ls_msgtb-msgv4
              INTO <ls_disp>-message.

      <ls_disp>-statu  = icon_red_light.
    ENDIF.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CHECK_DATA_02
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- <LS_DISP>
*&---------------------------------------------------------------------*
FORM check_data_02  CHANGING cs_disp TYPE ty_disp.


  CONSTANTS: lc_msgid     TYPE sy-msgid VALUE 'ZMM01',
             lc_msgno_022 TYPE sy-msgno VALUE '022',
             lc_msgno_039 TYPE sy-msgno VALUE '039',
             lc_msgno_005 TYPE sy-msgno VALUE '005'.

  DATA: ls_msgtb TYPE zscn00001.
*        LV_MSGV2 TYPE BAPIRET2-MESSAGE.

  DEFINE _l_set_message.
    CLEAR ls_msgtb.
    ls_msgtb-arbgb = lc_msgid.
    ls_msgtb-txtnr = &1.
    ls_msgtb-msgty = 'E'.

    ls_msgtb-msgv1 = &2.
    ls_msgtb-msgv2 = &3.
    ls_msgtb-msgv3 = &4.
    ls_msgtb-msgv4 = &5.

    APPEND ls_msgtb TO cs_disp-msgtb.

  END-OF-DEFINITION.

  "문서유형
  IF NOT cs_disp-bsart IS INITIAL.
    READ TABLE gt_t161 INTO DATA(ls_t161)
                       WITH KEY bsart = cs_disp-bsart
                       BINARY SEARCH.
    IF sy-subrc NE 0.
      _l_set_message lc_msgno_022 '문서유형' cs_disp-bsart '' ''.
    ENDIF.
  ENDIF.

  "조직코드
  IF NOT cs_disp-ekgrp IS INITIAL.
    READ TABLE gt_org INTO DATA(ls_org)
                       WITH KEY ekgrp = cs_disp-ekgrp
                       BINARY SEARCH.
    IF sy-subrc NE 0.
      _l_set_message lc_msgno_022 '구매그룹' cs_disp-ekgrp '' ''.
    ENDIF.
  ENDIF.

  "발주담당
  IF NOT cs_disp-order IS INITIAL.
    READ TABLE gt_user INTO DATA(ls_user)
                       WITH KEY employ_no = cs_disp-order
                       BINARY SEARCH.
    IF sy-subrc NE 0.
      _l_set_message lc_msgno_022 '발주담당' cs_disp-order '' ''.
    ELSE.
      cs_disp-zorder_department = ls_user-department.
    ENDIF.
  ENDIF.

  "지출발의담당
  IF NOT cs_disp-exper IS INITIAL.
    READ TABLE gt_user INTO ls_user
                       WITH KEY employ_no = cs_disp-exper
                       BINARY SEARCH.
    IF sy-subrc NE 0.
      _l_set_message lc_msgno_022 '지출발의담당' cs_disp-exper '' ''.
    ELSE.
      cs_disp-zexpen_department = ls_user-department.
    ENDIF.
  ENDIF.

  "검수담당자
  IF NOT cs_disp-qmper IS INITIAL.
    READ TABLE gt_user INTO ls_user
                       WITH KEY employ_no = cs_disp-qmper
                       BINARY SEARCH.
    IF sy-subrc NE 0.
      _l_set_message lc_msgno_022 '검수담당자' cs_disp-qmper '' ''.
    ELSE.
      cs_disp-zqm_department = ls_user-department.
    ENDIF.
  ENDIF.

  "플랜트
  IF NOT cs_disp-werks IS INITIAL.
    READ TABLE gt_t001w INTO DATA(ls_t001w)
                       WITH KEY werks = cs_disp-werks
                       BINARY SEARCH.
    IF sy-subrc NE 0.
      _l_set_message lc_msgno_022 '플랜트' cs_disp-werks '' ''.
    ENDIF.
  ENDIF.

  IF p_impo = abap_true.
*     선적항.
    IF cs_disp-inco2_l IS NOT INITIAL.
      READ TABLE gt_config INTO DATA(ls_config)
      WITH KEY field1 = cs_disp-inco2_l BINARY SEARCH.
      IF sy-subrc EQ 0.
        cs_disp-inco2_l =  ls_config-field1
                                       && '-('
                                       && ls_config-field2
                                       && '-'
                                       && ls_config-field4 && ')'.

      ELSE.
        _l_set_message lc_msgno_005 '선적항' '' '' ''.
      ENDIF.
    ENDIF.

*      도착항.
    IF cs_disp-inco3_l IS NOT INITIAL.
      READ TABLE gt_config INTO ls_config
      WITH KEY field1 = cs_disp-inco3_l BINARY SEARCH.
      IF sy-subrc EQ 0.
        cs_disp-inco3_l =  ls_config-field1
                                       && '-('
                                       && ls_config-field2
                                       && '-'
                                       && ls_config-field4 && ')'.

      ELSE.
        _l_set_message lc_msgno_005 '도착항' '' '' ''.
      ENDIF.
    ENDIF.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_F4_LISTBOX
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM set_f4_listbox .

*> 회사코드 List box
  PERFORM set_f4_bukrs_listbox.

*> 구매조직 List box
  PERFORM set_f4_ekorg_listbox.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_F4_BUKRS_LISTBOX
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM set_f4_bukrs_listbox .

  DATA: lv_name TYPE  vrm_id,
        lt_list TYPE  vrm_values.

  CONSTANTS: lc_scr_name TYPE screen-name VALUE 'P_BUKRS'.

  lv_name =  lc_scr_name.

  zcl_mm_common=>common_config(
    EXPORTING
      is_common = VALUE #( m = 'A1' d = 'A1000' s = 'AA100' )
      it_where  = VALUE #(
                           ( field = 1 value = 'BUKRS' )
                         )
    IMPORTING
      et_outtab = DATA(lt_config) ).

  SELECT a~field2 AS key, a~field3 AS text
    FROM @lt_config AS a
   WHERE a~field2 NE @space
    INTO CORRESPONDING FIELDS OF TABLE @lt_list.

  SORT  lt_list  BY  key.

  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      id     = lv_name
      values = lt_list.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_F4_EKORG_LISTBOX
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM set_f4_ekorg_listbox .

  DATA: lv_name TYPE  vrm_id,
        lt_list TYPE  vrm_values.

  CONSTANTS: lc_scr_name TYPE screen-name VALUE 'P_EKORG'.

*> List box 생성.
  lv_name =  lc_scr_name.

  SELECT ekorg AS key, ekotx AS text
    FROM t024e
   WHERE ekorg NE @space
*     AND BUKRS IN @LR_BUKRS
    ORDER BY ekorg ASCENDING
    INTO CORRESPONDING FIELDS OF TABLE @lt_list.

  SORT  lt_list  BY  key.

  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      id     = lv_name
      values = lt_list.

ENDFORM.
