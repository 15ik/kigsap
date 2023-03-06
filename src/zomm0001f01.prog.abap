*&---------------------------------------------------------------------*
*& Include          ZOMM0001F01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form select_ztmm00001
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> GT_DISP
*&---------------------------------------------------------------------*
FORM select_ztmm00001 TABLES ct_disp LIKE gt_disp.

  SELECT  zmain_cat,
                zmidd_cat,
                zsmal_cat,
                zmidd_cat_dec,
                zsmal_cat_dec,
                field_name1,
                field_name2,
                field_name3,
                field_name4,
                field_name5,
                field_name6,
                field_name7,
                field_name8,
                field_name9,
                field_name10,
                field_name11,
                field_name12,
                field_name13,
                reftable1,
                fieldname1,
                reftable2,
                fieldname2,
                reftable3,
                fieldname3,
                reftable4,
                fieldname4,
                reftable5,
                fieldname5,
                reftable6,
                fieldname6,
                reftable7,
                fieldname7,
                reftable8,
                fieldname8,
                reftable9,
                fieldname9,
                reftable10,
                fieldname10,
                reftable11,
                fieldname11,
                reftable12,
                fieldname12,
                reftable13,
                fieldname13,
*          ernam,
*          erdat,
*          erzet,
*          aenam,
*          aedat,
*          aezet,
          zdele
    FROM ztmm00001
   WHERE zmain_cat EQ @p_main
    INTO CORRESPONDING FIELDS OF TABLE @ct_disp.

  IF sy-subrc NE 0.
    ct_disp[] = VALUE #( ( zmain_cat = p_main ) ).
    MESSAGE s000 WITH 'Config를 등록하세요'(m07).
  ENDIF.

  SORT:gt_disp BY zmain_cat zmidd_cat zsmal_cat.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CHECK_EXIT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM check_exit .

*--------------------------------
* 화면 OFF전 변경 데이타 확인
*--------------------------------

  CASE gv_ok_code.
    WHEN 'EXIT'.
      LEAVE TO SCREEN 0.
*      LEAVE PROGRAM.
    WHEN OTHERS.
      grf_grid_dtl->check_changed_data(  ).
      IF gv_change IS NOT INITIAL.
        DATA : lv_msg1 TYPE string, lv_msg2 TYPE string.
        lv_msg1 = zcl_cn_alv_grid=>ac_msg_exit && '..' && zcl_cn_alv_grid=>ac_msg_save && '.'.
        lv_msg2 = TEXT-m27.
        CHECK grf_grid_dtl->pop_to_msg( iv_type  = 'A'
                                    iv_title = zcl_cn_alv_grid=>ac_msg_exit  "변경정보 확인
                                    iv_text1 = lv_msg1 "계속하시겠습니까?
                                    iv_text2 = lv_msg2 ) EQ abap_true. " YES
        PERFORM save_ztmm00002.
        CLEAR : gv_change.
      ENDIF.

      LEAVE TO SCREEN 0.
  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form check_authority
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM check_authority .

  CHECK p_main IS NOT INITIAL.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form initialization
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM initialization .

*------------------------------
* Set Variant
*------------------------------
  gs_variant-report   = sy-cprog.
  gs_variant-username = sy-uname.

  CLEAR : gv_sel_node.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form create_alv_grid
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM create_alv_grid .

  CREATE OBJECT go_document
    EXPORTING
      background_color = cl_dd_area=>col_textarea.

  PERFORM make_top_txt USING go_document. "Top 내용 구성

  CALL METHOD go_document->display_document
    EXPORTING
      parent             = grf_head
    EXCEPTIONS
      html_display_error = 1.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form select_detail_fcat
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> PS_TREE
*&---------------------------------------------------------------------*
FORM select_detail_fcat USING is_tree TYPE ts_tree.

  DATA : lv_count(2)  VALUE 1.

  FIELD-SYMBOLS : <lv_1>, <lv_2>, <lv_3>.

  CONSTANTS : lc_ztmm00001(9) VALUE 'ZTMM00001'.

  SELECT SINGLE zmain_cat,
                          zmidd_cat,
                          zsmal_cat,
                          zmidd_cat_dec,
                          zsmal_cat_dec,
                          field_name1,
                          field_name2,
                          field_name3,
                          field_name4,
                          field_name5,
                          field_name6,
                          field_name7,
                          field_name8,
                          field_name9,
                          field_name10,
                          field_name11,
                          field_name12,
                          field_name13,
                          reftable1,
                          fieldname1,
                          reftable2,
                          fieldname2,
                          reftable3,
                          fieldname3,
                          reftable4,
                          fieldname4,
                          reftable5,
                          fieldname5,
                          reftable6,
                          fieldname6,
                          reftable7,
                          fieldname7,
                          reftable8,
                          fieldname8,
                          reftable9,
                          fieldname9,
                          reftable10,
                          fieldname10,
                          reftable11,
                          fieldname11,
                          reftable12,
                          fieldname12,
                          reftable13,
                          fieldname13
     FROM ztmm00001
     INTO CORRESPONDING FIELDS OF @gs_fcat_dtl
   WHERE zmain_cat = @is_tree-zmain_cat
      AND zmidd_cat = @is_tree-zmidd_cat
      AND zsmal_cat = @is_tree-zsmal_cat.

  CLEAR : gt_pos_name[].
*  참조테이블 확인.
  DO 13 TIMES.
    DATA(lv_field1) = 'GS_FCAT_DTL-FIELD_NAME' && lv_count.
    IF lv_field1 IS NOT INITIAL. ASSIGN (lv_field1) TO <lv_1>.  ENDIF.
    DATA(lv_field2) = 'GS_FCAT_DTL-REFTABLE' && lv_count.
    IF lv_field2 IS NOT INITIAL. ASSIGN (lv_field2) TO <lv_2>.  ENDIF.
    DATA(lv_field3) = 'GS_FCAT_DTL-FIELDNAME' && lv_count.
    IF lv_field3 IS NOT INITIAL. ASSIGN (lv_field3) TO <lv_3>.  ENDIF.


*    참조가 없으면 임의지정.
    IF <lv_1> IS NOT INITIAL.
      IF <lv_2> IS INITIAL.
        <lv_2> = lc_ztmm00001.
        <lv_3> = 'FIELDNAME' && lv_count.
      ENDIF.

*    중복되는 필드가 있으면 카운터처리.
      CLEAR : gs_pos_name.
      READ TABLE gt_pos_name INTO gs_pos_name WITH KEY name = <lv_3>.
      IF sy-subrc EQ 0.

        gs_pos_name-pos = lv_count.
        gs_pos_name-name = <lv_3>.
        gs_pos_name-name_c =  gs_pos_name-name && '_' && lv_count.
        APPEND gs_pos_name TO gt_pos_name.

        <lv_3> = gs_pos_name-name_c.
      ELSE.

*   카운터 처리된 필드 집계.
        gs_pos_name-pos = lv_count.
        gs_pos_name-name = <lv_3>.
        gs_pos_name-name_c = <lv_3>.
        APPEND gs_pos_name TO gt_pos_name.
      ENDIF.

    ENDIF.

    ADD 1 TO lv_count.
**    CLEAR : lv_check.
  ENDDO.

  SORT gt_pos_name BY pos.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form select_detail
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> PS_TREE
*&---------------------------------------------------------------------*
FORM select_detail USING is_tree TYPE ts_tree.

  SELECT zmain_cat,
              zmidd_cat,
              zsmal_cat,
              field1,
              field2,
              field3,
              field4,
              field5,
              field6,
              field7,
              field8,
              field9,
              field10,
              field11,
              field12,
              field13,
              erdat,
              erzet,
              ernam,
              aedat,
              aezet,
              aenam,
              zdele
     FROM ztmm00002
     INTO CORRESPONDING FIELDS OF TABLE  @gt_disp_dtl
   WHERE zmain_cat = @is_tree-zmain_cat
      AND zmidd_cat = @is_tree-zmidd_cat
      AND zsmal_cat = @is_tree-zsmal_cat.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form btn_del_row
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM btn_del_row .

  DATA : lt_rows   TYPE  lvc_t_row,
         lt_row_no TYPE  lvc_t_roid.

  DATA : lt_disp_dtl TYPE TABLE OF ts_disp_dtl.
*         lt_00002    TYPE STANDARD TABLE OF ztmm00002.

  CALL METHOD grf_grid_dtl->get_selected_rows
    IMPORTING
      et_index_rows = lt_rows
      et_row_no     = lt_row_no.
  IF lt_rows[] IS NOT INITIAL.

    CHECK grf_grid_dtl->pop_to_msg( iv_type  = 'A'
                                iv_title = zcl_cn_alv_grid=>ac_msg_title3 "삭제확인
                                iv_text1 = zcl_cn_alv_grid=>ac_msg_del1  "삭제하시겠습니까?
                                iv_text2 = zcl_cn_alv_grid=>ac_msg_del2 ) EQ abap_true. " YES


    SORT lt_rows BY index DESCENDING.
    LOOP AT lt_rows INTO DATA(ls_rows).

      READ TABLE gt_disp_dtl INTO DATA(ls_dtl) INDEX ls_rows-index.
      APPEND ls_dtl TO lt_disp_dtl.

      DELETE gt_disp_dtl INDEX ls_rows-index.

      DELETE <gt_ta> INDEX  ls_rows-index.
    ENDLOOP.

*    IF lt_disp_dtl[] IS NOT INITIAL.
*      SORT lt_disp_dtl BY zmain_cat zmidd_cat zsmal_cat field1 field2 field3 field4 field5 field6 field7
*                                                                             field8 field9 field10 field11 field12 field13.
*      DELETE ADJACENT DUPLICATES FROM lt_disp_dtl COMPARING zmain_cat zmidd_cat zsmal_cat
*                                                                                                   field1 field2 field3 field4 field5 field6 field7
*                                                                                                   field8 field9 field10 field11 field12 field13.
*
*      CLEAR : lt_00002[].
*      SELECT zmain_cat,
*                  zmidd_cat,
*                  zsmal_cat,
*                  field1,
*                  field2,
*                  field3
*         INTO CORRESPONDING FIELDS OF TABLE  @lt_00002
*         FROM ztmm00002 FOR ALL ENTRIES IN @lt_disp_dtl
*       WHERE zmain_cat = @lt_disp_dtl-zmain_cat
*          AND zmidd_cat = @lt_disp_dtl-zmidd_cat
*          AND zsmal_cat = @lt_disp_dtl-zsmal_cat
*          AND field1 =  @lt_disp_dtl-field1
*          AND field2 =  @lt_disp_dtl-field2
*          AND field3 =  @lt_disp_dtl-field3.
*      IF lt_00002[] IS NOT INITIAL.
*        DELETE ztmm00002 FROM TABLE lt_00002.
*        IF sy-subrc EQ 0.
*          COMMIT WORK.
    gv_change = 'X'.
    MESSAGE s000 WITH '삭제되었습니다 ( 삭제 후에도 반드시 저장해 주시기 바랍니다 )'(m06).
*        ELSE.
**          ROLLBACK WORK.
*          MESSAGE s000 WITH '삭제 실패'(m08) DISPLAY LIKE 'E'.
*        ENDIF.
*      ENDIF.
*    ENDIF.
    CALL METHOD grf_grid_dtl->refresh_grid_display( ).
  ELSE.
    MESSAGE s000 WITH '삭제할 라인을 선택하세요'(m09)  DISPLAY LIKE 'E'.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form select_define_field
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> GT_DE_FCAT
*&---------------------------------------------------------------------*
FORM select_define_field TABLES ct_de_fcat LIKE gt_de_fcat.

  DATA : ls_de_fcat TYPE ts_de_fcat.

  FIELD-SYMBOLS :<lv_1>.
  DATA : lv_field  TYPE c LENGTH 30.
  DATA : lv_count TYPE c LENGTH 2 VALUE 1.

  IF gv_sel_node IS INITIAL.
    MESSAGE s000 WITH '소분류를 클릭하세요.'(m17)  DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  READ TABLE gt_tree INTO DATA(ls_tree) INDEX gv_sel_node.

  SELECT SINGLE
               field_name1,   field_name2,  field_name3,   field_name4, field_name5,
               field_name6,   field_name7,  field_name8,   field_name9, field_name10,
               field_name11, field_name12, field_name13,
              reftable1,   reftable2,  reftable3,   reftable4,  reftable5,
              reftable6,   reftable7,  reftable8,   reftable9,  reftable10,
              reftable11, reftable12, reftable13,
              fieldname1,   fieldname2,  fieldname3,   fieldname4, fieldname5,
              fieldname6,   fieldname7,  fieldname8,   fieldname9, fieldname10,
              fieldname11, fieldname12, fieldname13
     INTO  @DATA(ls_disp)
     FROM ztmm00001
   WHERE zmain_cat = @ls_tree-zmain_cat
      AND  zmidd_cat = @ls_tree-zmidd_cat
      AND  zsmal_cat = @ls_tree-zsmal_cat.
  IF sy-subrc EQ 0.

    DO 13 TIMES.
      CLEAR : ls_de_fcat.
      CONCATENATE '필드' lv_count INTO lv_field.
      ls_de_fcat-field = lv_field.

*      field name.
      UNASSIGN : <lv_1>. CLEAR : lv_field.
      CONCATENATE 'LS_DISP-FIELD_NAME' lv_count INTO lv_field.
      ASSIGN (lv_field) TO <lv_1>.
      ls_de_fcat-field_name = <lv_1>.

*      REFTABLE.
      UNASSIGN : <lv_1>.  CLEAR : lv_field.
      CONCATENATE 'LS_DISP-REFTABLE' lv_count INTO lv_field.
      ASSIGN (lv_field) TO <lv_1>.
      ls_de_fcat-reftable = <lv_1>.

*      FIELDNAME.
      UNASSIGN : <lv_1>. CLEAR : lv_field.
      CONCATENATE 'LS_DISP-FIELDNAME' lv_count INTO lv_field.
      ASSIGN (lv_field) TO <lv_1>.
      ls_de_fcat-fieldname = <lv_1>.

      APPEND ls_de_fcat TO ct_de_fcat.

      ADD 1 TO lv_count.
    ENDDO.


  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form variant_check
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- LV_ERR
*&---------------------------------------------------------------------*
FORM variant_check CHANGING ev_err.

  DATA : lv_name  TYPE dd02l-tabname,
         lv_field TYPE ddftx-fieldname.

  LOOP AT gt_de_fcat INTO DATA(ls_de).

*    IF gt_disp_dtl[] IS NOT INITIAL.
*      IF ls_de-field_name IS INITIAL.
*        pv_err = 'E'.
*        MESSAGE s000 WITH '필드명을  삭제할 수 없습니다.' DISPLAY LIKE 'E'.
*        EXIT.
*      ENDIF.
*    ENDIF.

    CHECK ls_de-field_name IS NOT INITIAL.

    IF ls_de-reftable IS NOT INITIAL.
      zcl_mm_common=>data_exist_check(
                                    EXPORTING  is_table =  VALUE #( table = 'DD02L' field = 'TABNAME' )
                                                        it_where = VALUE #( ( field = 'TABNAME' value = ls_de-reftable )
                                                                                         )
                                     IMPORTING ev_subrc = DATA(lv_subrc)
                                                       ev_return = lv_name ).
      IF lv_subrc NE 0.
        ev_err = 'E'.
        MESSAGE s000 WITH '존재하지 않는 테이블이 있습니다.'(m10) DISPLAY LIKE 'E'.
        EXIT.
      ENDIF.

      IF ls_de-fieldname IS INITIAL.
        ev_err = 'E'.
        MESSAGE s000 WITH '테이블에 존재하지 않는 필드가 있습니다.'(m11)  DISPLAY LIKE 'E'.
        EXIT.
      ENDIF.
    ENDIF.

    IF ls_de-fieldname IS NOT INITIAL.

      zcl_mm_common=>data_exist_check(
                                    EXPORTING  is_table =  VALUE #( table = 'DDFTX' field = 'FIELDNAME' )
                                                        it_where = VALUE #( ( field = 'TABNAME' value = lv_name )
                                                                                       ( field = 'FIELDNAME' value = ls_de-fieldname )
                                                                                       ( field = 'DDLANGUAGE' value = sy-langu )  )
                                     IMPORTING ev_subrc = lv_subrc
                                                       ev_return = lv_field ).
      IF lv_subrc <> 0.
        ev_err = 'E'.
        MESSAGE s000 WITH '테이블에 존재하지 않는 필드가 있습니다.'(m11)  DISPLAY LIKE 'E'.
        EXIT.
      ENDIF.
    ENDIF.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form save_field_define
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM save_field_define .

  DATA : lt_00001 LIKE TABLE OF ztmm00001 WITH HEADER LINE.

  CLEAR : lt_00001[], lt_00001.

  FIELD-SYMBOLS :<lv_f>.
  DATA : lv_field  TYPE c LENGTH 30.
  DATA : lv_count TYPE c LENGTH 2 VALUE 1.

  READ TABLE gt_tree INTO DATA(ls_tree) INDEX gv_sel_node.

  lt_00001-zmain_cat = ls_tree-zmain_cat.
  lt_00001-zmidd_cat = ls_tree-zmidd_cat.
  lt_00001-zsmal_cat = ls_tree-zsmal_cat.

  SELECT SINGLE zmidd_cat_dec, zsmal_cat_dec,
                           erdat, erzet, ernam, aedat, aezet, aenam
     INTO ( @lt_00001-zmidd_cat_dec, @lt_00001-zsmal_cat_dec,
               @lt_00001-erdat, @lt_00001-erzet, @lt_00001-ernam,
               @lt_00001-aedat, @lt_00001-aezet, @lt_00001-aenam )
     FROM ztmm00001
   WHERE zmain_cat = @ls_tree-zmain_cat
      AND  zmidd_cat = @ls_tree-zmidd_cat
      AND  zsmal_cat = @ls_tree-zsmal_cat.

  IF lt_00001-erdat IS INITIAL.
    lt_00001-erdat = sy-datum.
    lt_00001-erzet = sy-uzeit.
    lt_00001-ernam = sy-uname.
  ELSE.
    lt_00001-aedat = sy-datum.
    lt_00001-aezet = sy-uzeit.
    lt_00001-aenam = sy-uname.
  ENDIF.

  LOOP AT gt_de_fcat INTO DATA(ls_de).
    CHECK ls_de-field_name IS NOT INITIAL.

    lv_count = sy-tabix.
*      field name.
    CLEAR : lv_field.
    CONCATENATE 'LT_00001-FIELD_NAME' lv_count INTO lv_field.
    ASSIGN (lv_field) TO <lv_f>.
    <lv_f> = ls_de-field_name.

*      REFTABLE.
    UNASSIGN : <lv_f>.  CLEAR : lv_field.
    CONCATENATE 'LT_00001-REFTABLE' lv_count INTO lv_field.
    ASSIGN (lv_field) TO <lv_f>.
    <lv_f> =  ls_de-reftable.

*      FIELDNAME.
    UNASSIGN : <lv_f>. CLEAR : lv_field.
    CONCATENATE 'LT_00001-FIELDNAME' lv_count INTO lv_field.
    ASSIGN (lv_field) TO <lv_f>.
    <lv_f> = ls_de-fieldname.

  ENDLOOP.

  APPEND lt_00001.

  IF lt_00001[] IS NOT INITIAL.
    MODIFY ztmm00001 FROM TABLE @lt_00001.
    IF sy-subrc EQ 0.
      MESSAGE s000 WITH '필드 속성이 정의되었습니다.'(m12).
      COMMIT WORK AND WAIT.
      CLEAR gs_tree.
      READ TABLE gt_tree INTO gs_tree  INDEX gv_sel_node.
      PERFORM detail_screen USING gs_tree.

    ENDIF.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form check_zmain_cat
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> PA_MAIN
*&---------------------------------------------------------------------*
FORM check_zmain_cat USING iv_main.

  SELECT SINGLE ddtext INTO @DATA(lv_main)
    FROM dd07t
   WHERE domname = @gc_main
        AND domvalue_l = @iv_main
        AND ddlanguage  = @sy-langu.
  IF sy-subrc NE 0.
    MESSAGE e000 WITH '대분류 값이 없습니다.'(m13) DISPLAY LIKE 'E'.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form save_ztmm00001
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LS_00001
*&---------------------------------------------------------------------*
FORM save_ztmm00001 USING is_00001 TYPE ztmm00001
                                      CHANGING ev_err.

  CLEAR : ev_err.
  _g_time_er is_00001.
  INSERT ztmm00001 FROM @is_00001.
  IF sy-subrc EQ 0.
    COMMIT WORK.
    MESSAGE s000 WITH '분류 키가 생성되었습니다.'(m14).
  ELSE.
    ev_err = 'X'.
    ROLLBACK WORK.
    MESSAGE s000 WITH '분류 키 생성 실패.'(m15) DISPLAY LIKE 'E'.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form save_ztmm00002
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM save_ztmm00002 .

  DATA : lt_00002 TYPE STANDARD TABLE OF ztmm00002,
         ls_00002 TYPE ztmm00002.
*         ls_dup2  TYPE ztmm00002.

  DATA : ls_field  TYPE ts_field, ls_field2 TYPE ts_field.

*  빈값 체크.
  LOOP AT gt_disp_dtl INTO DATA(ls_disp_dtl).
    IF ls_disp_dtl-field1 IS INITIAL AND
        ls_disp_dtl-field2 IS INITIAL AND
        ls_disp_dtl-field3 IS INITIAL  AND
        ls_disp_dtl-field4 IS INITIAL  AND
        ls_disp_dtl-field5  IS INITIAL AND
        ls_disp_dtl-field6  IS INITIAL AND
        ls_disp_dtl-field7 IS INITIAL  AND
        ls_disp_dtl-field8 IS INITIAL  AND
        ls_disp_dtl-field9 IS INITIAL  AND
        ls_disp_dtl-field10 IS INITIAL  AND
        ls_disp_dtl-field11 IS INITIAL  AND
        ls_disp_dtl-field12 IS INITIAL  AND
        ls_disp_dtl-field13 IS INITIAL .
      DELETE gt_disp_dtl INDEX sy-tabix.
      DELETE <gt_ta> INDEX  sy-tabix.
      CONTINUE.
    ENDIF.
    CLEAR: ls_00002.
    MOVE-CORRESPONDING ls_disp_dtl TO ls_00002.
    APPEND ls_00002 TO lt_00002.
  ENDLOOP.

*  중복체크.
  DATA(lt_dup) = lt_00002[].
  IF lt_dup[] IS NOT INITIAL.
    SORT lt_dup BY  field1 field2 field3 field4 field5 field6 field7
                        field8 field9 field10.
    LOOP AT lt_dup INTO DATA(ls_dup).
      MOVE-CORRESPONDING ls_dup TO ls_field.
      IF ls_field = ls_field2.
        DATA(lv_check) = 'X'.
        EXIT.
      ENDIF.
      ls_field2 = ls_field.
    ENDLOOP.
  ENDIF.

  IF lv_check IS NOT  INITIAL.
    MESSAGE s008  DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.


  IF lt_00002[] IS INITIAL.
    IF gv_change IS INITIAL.
      MESSAGE s011 DISPLAY LIKE 'E'.
      EXIT.

    ELSE. " 삭제 후 저장 시 추가 로직 적용 후 종료
      PERFORM delete_save.

      EXIT.
    ENDIF.

  ENDIF.

  SELECT SINGLE
            erdat,
            erzet,
            ernam,
            aedat,
            aezet,
            aenam
     INTO  @DATA(ls_time)
    FROM ztmm00002
    WHERE zmain_cat = @ls_00002-zmain_cat
         AND zmidd_cat = @ls_00002-zmidd_cat
         AND zsmal_cat = @ls_00002-zsmal_cat.

  SORT lt_00002 BY zmain_cat zmidd_cat zsmal_cat field1 field2 field3 field4 field5 field6 field7
                                                                             field8 field9 field10.
  DELETE ADJACENT DUPLICATES FROM lt_00002 COMPARING zmain_cat zmidd_cat zsmal_cat
                                                                                            field1 field2 field3 field4 field5 field6 field7
                                                                                            field8 field9 field10.
  IF lt_00002[] IS NOT INITIAL.

    LOOP AT lt_00002 INTO ls_00002.
      IF ls_time-erdat IS INITIAL.
        _g_time_er ls_00002.
      ELSE.
        MOVE-CORRESPONDING ls_time TO ls_00002.
        _g_time_ae ls_00002.
      ENDIF.
      MODIFY lt_00002 FROM ls_00002.
    ENDLOOP.

    DELETE FROM ztmm00002 WHERE zmain_cat = @ls_00002-zmain_cat
                                              AND zmidd_cat = @ls_00002-zmidd_cat
                                              AND zsmal_cat = @ls_00002-zsmal_cat.

    INSERT ztmm00002 FROM TABLE @lt_00002.
    IF sy-subrc EQ 0.
      CLEAR : gv_change.
      COMMIT WORK AND WAIT.
      MESSAGE s002.
    ELSE.
      ROLLBACK WORK.
      MESSAGE s003.
    ENDIF.
  ENDIF.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form delete_save
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM delete_save .

  IF gv_sel_node IS INITIAL.  "세부사항 클릭확인.
    MESSAGE s000 WITH TEXT-m05 DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  READ TABLE gt_tree INTO DATA(ls_tree) INDEX gv_sel_node.

  SELECT SINGLE
            erdat,
            erzet,
            ernam,
            aedat,
            aezet,
            aenam
     INTO  @DATA(ls_time)
    FROM ztmm00002
    WHERE zmain_cat = @ls_tree-zmain_cat
         AND zmidd_cat = @ls_tree-zmidd_cat
         AND zsmal_cat = @ls_tree-zsmal_cat.
  IF sy-subrc EQ 0.
    DELETE FROM ztmm00002 WHERE zmain_cat = @ls_tree-zmain_cat
                                                AND zmidd_cat = @ls_tree-zmidd_cat
                                                AND zsmal_cat = @ls_tree-zsmal_cat.
    IF sy-subrc EQ 0.   "삭제후 바로 종료.
      CLEAR : gv_change.
      COMMIT WORK.
      MESSAGE s002.
    ELSE.
      ROLLBACK WORK.
      MESSAGE s003.
    ENDIF.

  ELSE.

    CLEAR : gv_change.
    MESSAGE s002.
  ENDIF.

ENDFORM.
