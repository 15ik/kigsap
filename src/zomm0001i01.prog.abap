*&---------------------------------------------------------------------*
*& Include          ZOMM0001I01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.

  gv_ok_code = ok_code.
  CLEAR : ok_code.

  CASE gv_ok_code.
    WHEN 'BACK' OR 'EXIT' OR 'CANC'.
      PERFORM check_exit.

  ENDCASE.


ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0200  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0200 INPUT.

  DATA: gt_nodes     TYPE lvc_t_nkey,
        gv_node      TYPE lvc_nkey,
        gv_next_node TYPE lvc_nkey.

  DATA : gs_00001 TYPE ztmm00001,
         gs_disp  TYPE ts_disp.

  gv_ok_code = ok_code.
  CLEAR : ok_code, gs_00001, gs_disp.

  CASE gv_ok_code.
    WHEN 'ENTE'.

      CASE gv_ncheck.
        WHEN 'C'.   "생성모드
          IF gs_tree_add-cat IS INITIAL OR gs_tree_add-dec IS INITIAL.
            MESSAGE s000 WITH '분류 코드 또는 내역을 입력하세요'(m22) DISPLAY LIKE 'E'.
            EXIT.
          ENDIF.

          CALL METHOD grf_tree->get_selected_nodes
            CHANGING
              ct_selected_nodes = gt_nodes.

          READ TABLE gt_nodes INTO gv_node INDEX 1.
          READ TABLE gt_tree INTO gs_tree INDEX gv_node.
          MOVE-CORRESPONDING gs_tree TO gs_disp.

*      중분류, 소분류 구분.
          IF gs_disp-zmidd_cat IS NOT INITIAL. " 소분류
            MOVE-CORRESPONDING gs_tree TO gs_00001.
            gs_00001-zsmal_cat = gs_disp-zsmal_cat = gs_tree_add-cat.
            gs_00001-zsmal_cat_dec = gs_disp-zsmal_cat_dec =  gs_tree_add-dec.

            SELECT SINGLE zmain_cat
               INTO @DATA(gv_cat)
              FROM ztmm00001
             WHERE zmain_cat = @gs_00001-zmain_cat
                AND zmidd_cat = @gs_00001-zmidd_cat
                AND zsmal_cat = @gs_00001-zsmal_cat.
            IF sy-subrc EQ 0.
              CLEAR : gs_tree_add.
              MESSAGE s000 WITH '같은 분류가 이미 존재합니다.'(m23) DISPLAY LIKE 'E'.
              LEAVE TO SCREEN 0.
            ENDIF.

*            노트에 추가.
            PERFORM add_smal_line USING  gs_disp
                                             gv_node
                                    CHANGING gv_next_node.

          ELSE. " 중분류

            MOVE-CORRESPONDING gs_tree TO gs_00001.
            gs_00001-zmidd_cat = gs_disp-zmidd_cat = gs_tree_add-cat.
            gs_00001-zmidd_cat_dec = gs_disp-zmidd_cat_dec = gs_tree_add-dec.
            SELECT SINGLE zmain_cat
               INTO @gv_cat
              FROM ztmm00001
             WHERE zmain_cat = @gs_00001-zmain_cat
                AND zmidd_cat = @gs_00001-zmidd_cat.
            IF sy-subrc EQ 0.

              CLEAR : gs_tree_add.
              MESSAGE s000 WITH '같은 분류가 이미 존재합니다.'(m23)  DISPLAY LIKE 'E'.
              LEAVE TO SCREEN 0.
            ENDIF.

            PERFORM add_midd_line USING gs_disp
                                             gv_node
                                    CHANGING gv_next_node.

          ENDIF.

*           저장.
          PERFORM save_ztmm00001 USING gs_00001
                                                    CHANGING gv_err.

          CHECK gv_err IS INITIAL.

*            메인 테이블에 넣기.
          APPEND gs_disp TO gt_disp.
          SORT gt_disp BY zmain_cat zmidd_cat zsmal_cat.

        WHEN 'M'.  "변경 모드
          DATA : gv_node_text   TYPE lvc_value.

          CALL METHOD grf_tree->get_selected_nodes
            CHANGING
              ct_selected_nodes = gt_nodes.
          READ TABLE gt_nodes INTO gv_node INDEX 1.

          READ TABLE gt_tree INTO gs_tree INDEX gv_node.

          gs_00001-zmain_cat = gs_tree-zmain_cat.
          gs_00001-zmidd_cat = gs_tree-zmidd_cat.
          gs_00001-zsmal_cat = gs_tree-zsmal_cat.
          gs_00001-zmidd_cat_dec = gs_tree-zmidd_cat_dec.
          gs_00001-zsmal_cat_dec = gs_tree-zsmal_cat_dec.

          IF gs_00001-zsmal_cat_dec IS NOT INITIAL.  "소분류

            UPDATE  ztmm00001
                 SET  zsmal_cat_dec = @gs_tree_add-dec,
                       aedat = @sy-datum,
                       aezet = @sy-uzeit,
                       aenam = @sy-uname
            WHERE zmain_cat = @gs_tree-zmain_cat
                 AND zmidd_cat = @gs_tree-zmidd_cat
                 AND zsmal_cat = @gs_tree-zsmal_cat.
            IF sy-subrc EQ 0.
              LOOP AT gt_disp INTO gs_disp.
                IF gs_disp-zmain_cat = gs_tree-zmain_cat AND
                   gs_disp-zmidd_cat = gs_tree-zmidd_cat AND
                  gs_disp-zsmal_cat = gs_tree-zsmal_cat.
                  gs_disp-zsmal_cat_dec = gs_tree_add-dec.
                  MODIFY gt_disp FROM gs_disp.
                ENDIF.
              ENDLOOP.
              MESSAGE s000 WITH '해당 분류가 변경되었습니다.'(m24) DISPLAY LIKE 'S'.
              COMMIT WORK AND WAIT.
            ELSE.
              ROLLBACK WORK.
              MESSAGE s000 WITH '분류 키 변경 실패.'(m25) DISPLAY LIKE 'E'.
              EXIT.
            ENDIF.

          ELSE.  "중분류

            UPDATE  ztmm00001
                 SET  zmidd_cat_dec = @gs_tree_add-dec,
                       aedat = @sy-datum,
                       aezet = @sy-uzeit,
                       aenam = @sy-uname
            WHERE zmain_cat = @gs_tree-zmain_cat
                 AND zmidd_cat = @gs_tree-zmidd_cat.
            IF sy-subrc EQ 0.
              LOOP AT gt_disp INTO gs_disp.
                IF gs_disp-zmain_cat = gs_tree-zmain_cat AND
                   gs_disp-zmidd_cat = gs_tree-zmidd_cat .
                  gs_disp-zmidd_cat_dec = gs_tree_add-dec.
                  MODIFY gt_disp FROM gs_disp.
                ENDIF.
              ENDLOOP.
              MESSAGE s000 WITH '해당 분류가 변경되었습니다.'(m24) DISPLAY LIKE 'S'.
              COMMIT WORK AND WAIT.
            ELSE.
              ROLLBACK WORK.
              MESSAGE s000 WITH '분류 키 변경 실패.'(m25) DISPLAY LIKE 'E'.
              EXIT.
            ENDIF.

            gs_00001-zmidd_cat_dec = gs_tree_add-dec.
          ENDIF.


          CONCATENATE gs_tree_add-cat gs_tree_add-dec
                          INTO gv_node_text SEPARATED BY space.

          CALL METHOD grf_tree->change_node
            EXPORTING
              i_node_key    = gv_node
              i_outtab_line = gs_00001
              i_node_text   = gv_node_text
              i_u_node_text = 'X'.

      ENDCASE.

      CALL METHOD grf_tree->frontend_update.
      LEAVE TO SCREEN 0.

    WHEN 'CANC'.
      CLEAR : gs_tree_add.
      LEAVE TO SCREEN 0.
  ENDCASE.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0300  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0300 INPUT.

  grf_de_grid->check_changed_data(  ).

  IF gv_err IS NOT INITIAL.
    CLEAR : gv_err.
    EXIT.
  ENDIF.

  gv_ok_code = ok_code.
  CLEAR : ok_code.

  CASE gv_ok_code.
    WHEN 'ENTE3'.

*    체크로직.
      CHECK gt_de_fcat[] IS NOT INITIAL.
      PERFORM variant_check CHANGING gv_err.
      IF gv_err IS NOT INITIAL.
        CLEAR : gv_err.
        EXIT.
      ENDIF.

*    저장.
      PERFORM save_field_define.
      LEAVE TO SCREEN 0.

    WHEN 'CANC3'.
      LEAVE TO SCREEN 0.

  ENDCASE.

ENDMODULE.
