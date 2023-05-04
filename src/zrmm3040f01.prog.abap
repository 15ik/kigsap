*&---------------------------------------------------------------------*
*& Include          ZRMM3040F01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form INITIALIZATION
*&---------------------------------------------------------------------*
FORM initialization.

** 사용자 기본값
*  PERFORM set_init_values.
*
** 플랜트 기본값
*  s_werks[] = VALUE #( ( sign = 'I' option = 'EQ' low = space ) ).
*
** 내외자구분 기본값
*  IF p_kalsk IS INITIAL.
*    p_kalsk = 'DO'.  "국내
*  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_INIT_VALUES
*&---------------------------------------------------------------------*
FORM set_init_values.

** 회사코드 기본값
*  SELECT SINGLE employ_no, department, company, company_name
*    FROM zsvmm_user_info
*   WHERE user_id EQ @sy-uname
*    INTO @DATA(ls_user_info).
*
*  IF sy-subrc EQ 0.
*    p_bukrs = ls_user_info-company.
*
*    s_order[] = VALUE #( ( sign = 'I' option = 'EQ' low = ls_user_info-employ_no ) ).  "계약담당자
*    s_depto[] = VALUE #( ( sign = 'I' option = 'EQ' low = ls_user_info-department ) ). "계약부서
*
*  ENDIF.
*
** 구매조직 기본값
*  zcl_mm_common=>common_config(
*    EXPORTING
*      is_common = VALUE #( m = 'A1' d = 'A1000' s = 'AA100' )
*      it_where  = VALUE #( ( field = 2 value = p_bukrs ) )
*    IMPORTING
*      et_outtab = DATA(lt_config) ).
*
*  READ TABLE lt_config INTO DATA(ls_config) INDEX 1.
*
*  IF sy-subrc EQ 0.
*    p_ekorg  = ls_config-field4.
*  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_LIST_BOX
*&---------------------------------------------------------------------*
FORM set_list_box.

* 회사코드
  PERFORM set_list_box_bukrs USING 'P_BUKRS'.

* 내외자구분
  PERFORM set_list_box_kalsk USING 'P_KALSK'.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_LIST_BOX_BUKRS
*&---------------------------------------------------------------------*
FORM set_list_box_bukrs USING iv_fname.

*  DATA: lv_name TYPE vrm_id,
*        lt_list TYPE vrm_values.     "Key, Text
*
** 회사코드
*  lv_name = iv_fname.
*
*  zcl_mm_common=>common_config(
*    EXPORTING
*      is_common = VALUE #( m = 'A1' d = 'A1000' s = 'AA100' )
*      it_where  = VALUE #( ( field = 1 value = 'BUKRS' ) )
*    IMPORTING
*      et_outtab = DATA(lt_config) ).
*
*  lt_list = CORRESPONDING #( lt_config MAPPING key = field2  text = field3  ).
*
*  _g_set_values: lv_name lt_list.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_LIST_BOX_KALSK
*&---------------------------------------------------------------------*
FORM set_list_box_kalsk USING iv_fname.

  DATA: lv_name TYPE vrm_id,
        lt_list TYPE vrm_values,     "Key, Text
        ls_list TYPE vrm_value.

  DEFINE _l_append_values.

    CLEAR: ls_list.
    ls_list-key  = &1.
    ls_list-text = &2.
    APPEND ls_list TO lt_list.

  END-OF-DEFINITION.

  lv_name = iv_fname.

  SELECT kalsk,
         kalsb
    FROM tmkkt
   WHERE spras = @sy-langu
     AND kalsk IS NOT INITIAL
    INTO TABLE @DATA(lt_help).

  LOOP AT lt_help INTO DATA(ls_help).

    _l_append_values: ls_help-kalsk  ls_help-kalsb.

  ENDLOOP.

  _g_set_values: lv_name lt_list.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_F4_EKGRP
*&---------------------------------------------------------------------*
FORM set_f4_ekgrp USING iv_scr_name.

  DATA: lt_return     TYPE TABLE OF ddshretval,
        lt_dynpfields TYPE TABLE OF dynpread.

  CONSTANTS: lc_title(15) TYPE c VALUE '구매그룹 정보',
             lc_retfield  TYPE fieldname VALUE 'EKGRP'.

  FIELD-SYMBOLS: <lv_scr_value> TYPE any.
  ASSIGN (iv_scr_name) TO <lv_scr_value>.


* 회사코드, 구매조직 체크
  PERFORM dynp_values_read USING 'P_BUKRS' CHANGING p_bukrs.

  IF p_bukrs IS INITIAL.
    MESSAGE s017 WITH TEXT-f01 DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  PERFORM dynp_values_read USING 'P_EKORG' CHANGING p_ekorg.

  IF p_ekorg IS INITIAL.
    MESSAGE s017 WITH TEXT-f02 DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.


* Get Data
  SELECT DISTINCT ekgrp, eknam
    FROM zsvcmm_org
   WHERE bukrs = @p_bukrs
     AND ekorg = @p_ekorg
    INTO TABLE @DATA(lt_org_info).

  SORT lt_org_info BY ekgrp.

* Search Help
  PERFORM f4if_int_table_value_request TABLES lt_org_info
                                              lt_return
                                       USING  lc_title
                                              lc_retfield
                                              iv_scr_name.

* Return
  READ TABLE lt_return INTO DATA(ls_return) INDEX 1.

  IF sy-subrc = 0.
    READ TABLE lt_org_info WITH KEY ekgrp = ls_return-fieldval
                           BINARY SEARCH
                           INTO DATA(ls_org_info).

    <lv_scr_value> = ls_org_info-ekgrp.

    lt_dynpfields = VALUE #( ( fieldname = iv_scr_name fieldvalue = ls_org_info-ekgrp ) ).

    PERFORM dynp_values_update TABLES lt_dynpfields.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_F4_EXPRF
*&---------------------------------------------------------------------*
FORM set_f4_exprf USING iv_scr_name.

  DATA: lt_return     TYPE TABLE OF ddshretval,
        lt_dynpfields TYPE TABLE OF dynpread.

  CONSTANTS: lc_title(15) TYPE c VALUE '단가유형',
             lc_retfield  TYPE fieldname VALUE 'EXPRF'.

  FIELD-SYMBOLS: <lv_scr_value> TYPE any.
  ASSIGN (iv_scr_name) TO <lv_scr_value>.


* 회사코드 체크
  PERFORM dynp_values_read USING 'P_BUKRS' CHANGING p_bukrs.

  IF p_bukrs IS INITIAL.
    MESSAGE s017 WITH TEXT-f01 DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

* 회사코드의 국가키
  SELECT SINGLE land1
    INTO @gv_land1
    FROM t001
   WHERE bukrs = @p_bukrs.


* Get Data
  SELECT a~land1,
         a~exprf,
         b~bezei
    FROM t616 AS a INNER JOIN t616t AS b
                           ON b~spras = @sy-langu
                          AND b~land1 = a~land1
                          AND b~exprf = a~exprf
   WHERE a~land1 = @gv_land1
    INTO TABLE @DATA(lt_t616).

  SORT lt_t616 BY exprf.

* Search Help
  PERFORM f4if_int_table_value_request TABLES lt_t616
                                              lt_return
                                       USING  lc_title
                                              lc_retfield
                                              iv_scr_name.

* Return
  READ TABLE lt_return INTO DATA(ls_return) INDEX 1.

  IF sy-subrc = 0.
    READ TABLE lt_t616 WITH KEY exprf = ls_return-fieldval
                                BINARY SEARCH
                                INTO DATA(ls_t616).

    <lv_scr_value> = ls_t616-exprf.

    lt_dynpfields = VALUE #( ( fieldname = iv_scr_name fieldvalue = ls_t616-exprf ) ).

    PERFORM dynp_values_update TABLES lt_dynpfields.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_F4_PERSN
*&---------------------------------------------------------------------*
FORM set_f4_persn USING iv_scr_name.
*
*  DATA: lt_return     TYPE TABLE OF ddshretval,
*        lt_dynpfields TYPE TABLE OF dynpread.
*
*  CONSTANTS: lc_title(15) TYPE c VALUE '담당자 정보',
*             lc_retfield  TYPE fieldname VALUE 'EMPLOY_NO'.
*
*  FIELD-SYMBOLS: <lv_scr_value> TYPE any.
*  ASSIGN (iv_scr_name) TO <lv_scr_value>.
*
** Get Data
*  SELECT employ_no, employ_name, department, depart_name
*    FROM zsvmm_user_info
*   WHERE company EQ @p_bukrs
*    INTO TABLE @DATA(lt_user_info).
*
*  SORT lt_user_info BY employ_no.
*
** Search Help
*  PERFORM f4if_int_table_value_request TABLES lt_user_info
*                                              lt_return
*                                       USING  lc_title
*                                              lc_retfield
*                                              iv_scr_name.
*
** Return
*  READ TABLE lt_return INTO DATA(ls_return) INDEX 1.
*
*  IF sy-subrc = 0.
*    READ TABLE lt_user_info WITH KEY employ_no = ls_return-fieldval
*                            BINARY SEARCH
*                            INTO DATA(ls_user_info).
*
*    <lv_scr_value> = ls_user_info-employ_no.
*
*    lt_dynpfields = VALUE #( ( fieldname = iv_scr_name fieldvalue = ls_user_info-employ_no ) ).
*
*    PERFORM dynp_values_update TABLES lt_dynpfields.
*  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_F4_DEPAT
*&---------------------------------------------------------------------*
FORM set_f4_depat USING iv_scr_name.

*  DATA: lt_return     TYPE TABLE OF ddshretval,
*        lt_dynpfields TYPE TABLE OF dynpread.
*
*  CONSTANTS: lc_title(15) TYPE c VALUE '부서 정보',
*             lc_retfield  TYPE fieldname VALUE 'DEPARTMENT'.
*
*  FIELD-SYMBOLS: <lv_scr_value> TYPE any.
*  ASSIGN (iv_scr_name) TO <lv_scr_value>.
*
** Get Data
*  SELECT DISTINCT department, depart_name
*    FROM zsvmm_user_info
*   WHERE company EQ @p_bukrs
*    INTO TABLE @DATA(lt_dept_info).
*
*  SORT lt_dept_info BY department.
*
** Search Help
*  PERFORM f4if_int_table_value_request TABLES lt_dept_info
*                                              lt_return
*                                       USING  lc_title
*                                              lc_retfield
*                                              iv_scr_name.
*
** Return
*  READ TABLE lt_return INTO DATA(ls_return) INDEX 1.
*
*  IF sy-subrc = 0.
*    READ TABLE lt_dept_info WITH KEY department = ls_return-fieldval
*                            BINARY SEARCH
*                            INTO DATA(ls_dept_info).
*
*    <lv_scr_value> = ls_dept_info-department.
*
*    lt_dynpfields = VALUE #( ( fieldname = iv_scr_name fieldvalue = ls_dept_info-department ) ).
*
*    PERFORM dynp_values_update TABLES lt_dynpfields.
*  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form DYNP_VALUES_READ
*&---------------------------------------------------------------------*
FORM dynp_values_read USING    iv_scr_name
                      CHANGING ev_scr_value.

  DATA: lt_dynpfields TYPE TABLE OF dynpread,
        ls_dynpfields TYPE dynpread.

  ls_dynpfields = VALUE #( fieldname  = iv_scr_name ).
  APPEND ls_dynpfields TO lt_dynpfields.

  CALL FUNCTION 'DYNP_VALUES_READ'
    EXPORTING
      dyname               = sy-cprog
      dynumb               = sy-dynnr
    TABLES
      dynpfields           = lt_dynpfields
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

  IF sy-subrc = 0.
    SORT lt_dynpfields BY fieldname.

    READ TABLE lt_dynpfields WITH KEY fieldname = iv_scr_name
                             BINARY SEARCH
                             INTO ls_dynpfields.
    IF sy-subrc = 0.
      ev_scr_value = ls_dynpfields-fieldvalue.
    ENDIF.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form DYNP_VALUES_UPDATE
*&---------------------------------------------------------------------*
FORM dynp_values_update TABLES it_dynpfields STRUCTURE dynpread.

  CALL FUNCTION 'DYNP_VALUES_UPDATE'
    EXPORTING
      dyname               = sy-cprog
      dynumb               = sy-dynnr
    TABLES
      dynpfields           = it_dynpfields
    EXCEPTIONS
      invalid_abapworkarea = 1
      invalid_dynprofield  = 2
      invalid_dynproname   = 3
      invalid_dynpronummer = 4
      invalid_request      = 5
      no_fielddescription  = 6
      undefind_error       = 7
      OTHERS               = 8.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F4IF_INT_TABLE_VALUE_REQUEST
*&---------------------------------------------------------------------*
FORM f4if_int_table_value_request TABLES it_help_tab
                                         ct_return
                                  USING  iv_title
                                         iv_retfield
                                         iv_scr_name.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      window_title      = iv_title
      retfield          = iv_retfield        "더블클릭하면 가져올 값
      dynpprog          = sy-cprog
      dynpnr            = sy-dynnr
      dynprofield       = iv_scr_name        "Retern Field가 실제로 복사될 화면 필드
      value_org         = 'S'
    TABLES
      value_tab         = it_help_tab
      return_tab        = ct_return
    EXCEPTIONS
      field_not_found   = 1
      no_help_for_field = 2
      inconsistent_help = 3
      no_values_found   = 4
      OTHERS            = 5.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_SEL_SCREEN
*&---------------------------------------------------------------------*
FORM set_sel_screen.

*  DATA: lv_io_do1,
*        lv_io_do2.
*
*  CHECK sy-dynnr = gc_dynnr_1000.
*
** 사용자 아이디에 따른 필드 처리.
*  zcl_mm_common=>common_config(
*    EXPORTING
*      is_common = VALUE #( m = 'A1' d = 'A1010' s = 'AB100' )
*      it_where  = VALUE #( ( field = 1 value = sy-cprog )
*                                      ( field = 3 value = sy-uname )  )
*    IMPORTING
*      et_outtab = DATA(lt_config) ).
*  DATA(ls_config) = VALUE #( lt_config[ 1 ] OPTIONAL ).
*
*  IF p_kalsk = gc_kalsk_do.
*    lv_io_do1 = 1.
*
*    IF p_bukrs = gc_bukrs_1101.
*      lv_io_do2 = 1.
*    ELSE.
*      lv_io_do2 = 0.
*    ENDIF.
*
*    "유효단가품목(Only) 체크
*    p_valid = 'X'.
*
*  ELSE.
*    lv_io_do1 = 0.
*    lv_io_do2 = 0.
*
*    "유효단가품목(Only) 체크 해제
*    p_valid = ''.
*  ENDIF.
*
*  LOOP AT SCREEN.
*
*    CASE screen-group1.
*
*      WHEN 'DO1'.
*        screen-active = lv_io_do1.
*
*      WHEN 'DO2'.
*        screen-active = lv_io_do2.
*
*      WHEN OTHERS.
*    ENDCASE.
*
**    IF ls_config-field2 IS NOT INITIAL.
**      CASE ls_config-field2.
***        WHEN 'EX01'.
**        WHEN 'EX02'.
**          IF screen-name = 'P_BUKRS' OR
**              screen-name = 'P_EKORG'.
**            screen-input = 0.
**          ENDIF.
**        WHEN 'EX03'.
**          IF screen-name = 'P_BUKRS' OR
**              screen-name = 'P_EKORG' OR
**              screen-name+0(7) = 'S_DEPTO'.
**            screen-input = 0.
**          ENDIF.
**      ENDCASE.
**
**    ELSE.
**      IF screen-name = 'P_BUKRS'  OR
**          screen-name = 'P_EKORG' OR
***              SCREEN-NAME+0(7) = 'S_ORDER' OR
**          screen-name+0(7) = 'S_DEPTO'.
**        screen-input = 0.
**      ENDIF.
**
**    ENDIF.
*
*    MODIFY SCREEN.
*  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CHECK_AUTHORITY
*&---------------------------------------------------------------------*
FORM check_authority.

* 권한체크 룰(추후 적용)
  CHECK 1 <> 1.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_DATA
*&---------------------------------------------------------------------*
FORM get_data.

* 회사코드의 국가키
  SELECT SINGLE land1
    INTO @gv_land1
    FROM t001
   WHERE bukrs = @p_bukrs.

*--------------------------------
* 국내/외자 구매정보레코드 Data
*--------------------------------
  CLEAR gt_disp.

  SELECT c~bukrs,
         a~infnr,
         b~esokz,
         a~lifnr,
         g~name1,
         a~matnr,
         h~maktx,
         b~ekorg,
         c~ekotx,
         b~werks,
         b~ekgrp,
         d~eknam,
         b~bstae,
         e~taxim,
         b~mwskz,
         b~webre,
         b~inco1,
         b~exprf,
         a~urzla,
         b~uebto,
         b~verid,
         p~validprice AS valid,
         CASE f~kalsk WHEN 'DO' THEN p~conditionratevalue
                      WHEN 'IM' THEN b~netpr
                      ELSE 0 END AS netpr,
         p~conditionratevalueunit AS waers,
         p~conditionquantity AS peinh,
         p~conditionvaliditystartdate AS datab,
         p~conditionvalidityenddate AS datbi,
         p~conditionquantityunit AS kmein
    INTO CORRESPONDING FIELDS OF TABLE @gt_disp
    FROM eina AS a INNER JOIN eine AS b
                           ON b~infnr = a~infnr
                   INNER JOIN mara AS m
                           ON m~matnr = a~matnr
                    LEFT JOIN t024e AS c
                           ON c~ekorg = b~ekorg
                    LEFT JOIN t024 AS d
                           ON d~ekgrp = b~ekgrp
                    LEFT JOIN mlan AS e
                           ON e~matnr = a~matnr
                          AND e~aland = @gv_land1
                    LEFT JOIN lfm1 AS f
                           ON f~lifnr = a~lifnr
                          AND f~ekorg = b~ekorg
                    LEFT JOIN lfa1 AS g
                           ON g~lifnr = a~lifnr
                    LEFT JOIN makt AS h
                           ON h~spras = @sy-langu
                          AND h~matnr = a~matnr
                    LEFT JOIN zsvbmminfoprice AS p
                           ON p~infnr = a~infnr
                          AND p~matnr = a~matnr
                          AND p~lifnr = a~lifnr
                          AND p~purchasingorganization       = b~ekorg
                          AND p~purchasinginforecordcategory = b~esokz
                          AND p~plant = b~werks
   WHERE
*     c~bukrs  = @p_bukrs
*     AND b~ekorg  = @p_ekorg
*     AND f~kalsk  = @p_kalsk
*     AND
                      a~lifnr IN @s_lifnr
     AND a~matnr IN @s_matnr
     AND a~infnr IN @s_infnr
     AND b~esokz IN @s_esokz
     AND b~ekgrp IN @s_ekgrp
     AND b~werks IN @s_werks
     AND m~matkl IN @s_matkl
     AND b~exprf IN @s_exprf
     AND b~bstae IN @s_bstae
     AND p~conditionvaliditystartdate IN @s_datab
     AND p~conditionvalidityenddate IN @s_datbi.

*--------------------------------
* TEXT 및 기타 Data
*--------------------------------
  PERFORM get_other_data.

*--------------------------------
* 첨부파일 Data
*--------------------------------
  PERFORM get_attach_data.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form PROCESSING_DATA
*&---------------------------------------------------------------------*
FORM processing_data.

  DATA: lt_valid_y TYPE TABLE OF ty_disp,
        lt_valid_n TYPE TABLE OF ty_disp.

  DATA: lv_tabix TYPE sy-tabix.

*  DATA: LV_BAPI_CURR TYPE BAPICURR-BAPICURR.

  CHECK gt_disp IS NOT INITIAL.

*--------------------------------
* 유효단가품목(Only)
*--------------------------------
  IF p_valid = 'X'.
    DELETE gt_disp WHERE valid <> 'Y'.
  ENDIF.

*--------------------------------
* 정보레코드가 복수라인인 경우는 VALIDPRICE=Y인 값만 선택
* 모두 Y가 아닌 경우는 CONDITIONVALIDITYSTARTDATE가 가장 늦은 날짜 라인을 선택함
* 유효단가(Only)체크, 계약일자&계약만료일 입력된 경우 아래 로직 상관없이 출력
*--------------------------------
  IF p_kalsk = gc_kalsk_do AND p_valid IS INITIAL." AND s_bedat[] IS INITIAL AND s_datbi[] IS INITIAL.

    lt_valid_y = gt_disp.
    DELETE lt_valid_y WHERE valid <> 'Y'.   "VALIDPRICE = 'Y'
    SORT lt_valid_y BY infnr.

    lt_valid_n = gt_disp.
    DELETE lt_valid_n WHERE valid = 'Y'.    "VALIDPRICE <> 'Y'
    SORT lt_valid_n BY infnr ASCENDING datab DESCENDING.
    DELETE ADJACENT DUPLICATES FROM lt_valid_n COMPARING infnr datab.

    CLEAR gt_disp.
    gt_disp = lt_valid_y.

    LOOP AT lt_valid_n INTO DATA(ls_data_n).
      READ TABLE lt_valid_y WITH KEY infnr = ls_data_n-infnr
                                     BINARY SEARCH
                                     TRANSPORTING NO FIELDS.
      IF sy-subrc = 0.
        CONTINUE.
      ELSE.
        APPEND ls_data_n TO gt_disp.
      ENDIF.

    ENDLOOP.

  ENDIF.

  SORT gt_disp BY infnr.

*--------------------------------
* Processing Data
*--------------------------------
  LOOP AT gt_disp ASSIGNING FIELD-SYMBOL(<ls_disp>).

    lv_tabix = sy-tabix.

*    IF s_cntrno[] IS NOT INITIAL OR
*       s_order[]  IS NOT INITIAL OR
*       s_depto[]  IS NOT INITIAL OR
*       s_bedat[]  IS NOT INITIAL.
*      DELETE gt_disp INDEX lv_tabix.
*      CONTINUE.
*    ENDIF.

* 계약담당자, 계약부서, 계약일자
*    READ TABLE gt_ifmax WITH KEY infnr = <ls_disp>-infnr
*                                 BINARY SEARCH
*                                 INTO DATA(ls_ifmax).
*    IF sy-subrc = 0.
*      <ls_disp>-cntr_no                = ls_ifmax-cntr_no.
*      <ls_disp>-cntr_rev               = ls_ifmax-cntr_rev.
*      <ls_disp>-zorder_person          = ls_ifmax-zorder_person.
*      <ls_disp>-zorder_person_name     = ls_ifmax-zorder_person_name.
*      <ls_disp>-zorder_department      = ls_ifmax-zorder_department.
*      <ls_disp>-zorder_department_name = ls_ifmax-zorder_department_name.
*      <ls_disp>-title                  = ls_ifmax-title.
*
*    ELSE.
*      IF s_cntrno[] IS NOT INITIAL OR
*         s_order[]  IS NOT INITIAL OR
*         s_depto[]  IS NOT INITIAL OR
*         s_bedat[]  IS NOT INITIAL.
*        DELETE gt_disp INDEX lv_tabix.
*        CONTINUE.
*      ENDIF.
*    ENDIF.

* 범주
    PERFORM domain_value_get USING    'ESOKZ'
                                      <ls_disp>-esokz
                             CHANGING <ls_disp>-esokz_text.

* 플랜트
    READ TABLE gt_t001w WITH KEY werks = <ls_disp>-werks
                                 BINARY SEARCH
                                 INTO DATA(ls_t001w).
    IF sy-subrc = 0.
      <ls_disp>-werks_text = ls_t001w-name1.
    ENDIF.

* 확인관리
    READ TABLE gt_t163m WITH KEY bstae = <ls_disp>-bstae
                                 BINARY SEARCH
                                 INTO DATA(ls_t163m).
    IF sy-subrc = 0.
      <ls_disp>-bsbez = ls_t163m-bsbez.
    ENDIF.

* 자제세금지시자
    READ TABLE gt_tmkm1t WITH KEY taxim = <ls_disp>-taxim
                                  BINARY SEARCH
                                  INTO DATA(ls_tmkm1t).
    IF sy-subrc = 0.
      <ls_disp>-taxib = ls_tmkm1t-taxib.
    ENDIF.

* 세금코드
    READ TABLE gt_t007s WITH KEY mwskz = <ls_disp>-mwskz
                                 BINARY SEARCH
                                 INTO DATA(ls_t007s).
    IF sy-subrc = 0.
      <ls_disp>-text1 = ls_t007s-text1.
    ENDIF.

* 인도조건
    READ TABLE gt_tinct WITH KEY inco1 = <ls_disp>-inco1
                                 BINARY SEARCH
                                 INTO DATA(ls_tinct).
    IF sy-subrc = 0.
      <ls_disp>-bezei = ls_tinct-bezei.
    ENDIF.

* 단가유형
    READ TABLE gt_t616t WITH KEY exprf = <ls_disp>-exprf
                                 BINARY SEARCH
                                 INTO DATA(ls_t616t).
    IF sy-subrc = 0.
      <ls_disp>-exprf_text = ls_t616t-bezei.
    ENDIF.

* 인쇄교체비
*    IF p_bukrs = gc_bukrs_1101.
*      READ TABLE gt_zpri WITH KEY matnr = <ls_disp>-matnr
*                                  BINARY SEARCH
*                                  INTO DATA(ls_zpri).
*      IF sy-subrc = 0.
*        <ls_disp>-kbetr = ls_zpri-kbetr.
*
**        LV_BAPI_CURR = LS_ZPRI-KBETR.
**
**        CALL FUNCTION 'BAPI_CURRENCY_CONV_TO_INTERNAL'
**          EXPORTING
**            CURRENCY             = <LS_DISP>-WAERS
**            AMOUNT_EXTERNAL      = LV_BAPI_CURR
**            MAX_NUMBER_OF_DIGITS = 15
**          IMPORTING
**            AMOUNT_INTERNAL      = <LS_DISP>-KBETR.
*      ENDIF.
*    ENDIF.

* 첨부파일
    READ TABLE gt_atta WITH KEY instid_a = <ls_disp>-infnr
                                TRANSPORTING NO FIELDS.
    IF sy-subrc = 0.
      <ls_disp>-afile = icon_attachment.
    ENDIF.

    CLEAR: ls_t001w, ls_t163m, ls_tmkm1t, ls_t007s, ls_tinct, ls_t616t. "ls_zpri.

  ENDLOOP.

*--------------------------------
* 인쇄교체비(Only)
*--------------------------------
*  IF p_zpri = 'X'.
*    DELETE gt_disp WHERE kbetr = 0.
*  ENDIF.


  DESCRIBE TABLE gt_disp LINES DATA(lv_tcnt).
  MESSAGE s011(zmm01) WITH lv_tcnt.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_OTHER_DATA
*&---------------------------------------------------------------------*
FORM get_other_data.

*  DATA: lv_kalsm LIKE t007s-kalsm.
*
*  CONSTANTS: lc_tax(3)     TYPE c VALUE 'TAX',
*             lc_kschl_zpri TYPE konp-kschl VALUE 'ZPRI'.
*
*  CLEAR: gt_t001w, gt_t163m, gt_tmkm1t, gt_t007s, gt_tinct, gt_t616t, gt_zpri.
*
*  IF gt_disp IS NOT INITIAL.
*
** 계약담당자, 계약부서, 계약일자
**> 구매정보레코드 I/F 수신 결과(MAX 차수)
*    SELECT  a~cntr_no,
*           a~cntr_rev,
*           a~cntr_item_lno,
*           a~infnr,
*           a~timestamp,
*           b~zorder_person,
*           c~user_nm AS zorder_person_name,
*           b~zorder_department,
*           d~orgn_nm AS zorder_department_name,
*           b~bedat,
*           b~/bofu/bcsd_subj AS title
*      INTO CORRESPONDING FIELDS OF TABLE @gt_ifmax
*      FROM ztmm30113 AS a INNER JOIN ztmm30110 AS b
*                                  ON b~cntr_no   = a~cntr_no
*                                 AND b~cntr_rev  = a~cntr_rev
*                                 AND b~timestamp = a~timestamp
*                           LEFT JOIN ztcn00002 AS c
*                                  ON c~emp_no = b~zorder_person
*                                 AND c~bukrs  = b~bukrs
*                           LEFT JOIN ztcn00001 AS d
*                                  ON d~orgn_cd = b~zorder_department
*                                 AND d~bukrs   = b~bukrs
*       FOR ALL ENTRIES IN @gt_disp
*     WHERE a~infnr = @gt_disp-infnr
*       AND b~bukrs = @gt_disp-bukrs
*       AND b~cntr_no           IN @s_cntrno
*       AND b~zorder_person     IN @s_order
*       AND b~zorder_department IN @s_depto
*       AND b~bedat             IN @s_bedat.
*
*    LOOP AT gt_ifmax ASSIGNING FIELD-SYMBOL(<ls_ifmax>).
*      <ls_ifmax>-times = <ls_ifmax>-timestamp. "숫자로 변환
*    ENDLOOP.
*
**    SORT gt_ifmax BY infnr cntr_no DESCENDING cntr_rev DESCENDING.
**    DELETE ADJACENT DUPLICATES FROM gt_ifmax COMPARING infnr cntr_no.
*
*    SORT gt_ifmax BY infnr ASCENDING times DESCENDING.  "타임의 내림차순 정렬
*    DELETE ADJACENT DUPLICATES FROM gt_ifmax COMPARING infnr.
*
** 플랜트
*    SELECT werks,
*           name1
*      INTO CORRESPONDING FIELDS OF TABLE @gt_t001w
*      FROM t001w
*       FOR ALL ENTRIES IN @gt_disp
*     WHERE werks = @gt_disp-werks.
*
*    SORT gt_t001w BY werks.
*
** 확인관리
*    SELECT bstae,
*           bsbez
*      INTO CORRESPONDING FIELDS OF TABLE @gt_t163m
*      FROM t163m
*       FOR ALL ENTRIES IN @gt_disp
*     WHERE spras = @sy-langu
*       AND bstae = @gt_disp-bstae.
*
*    SORT gt_t163m BY bstae.
*
** 자재세금지시자
*    SELECT taxim,
*           taxib
*      INTO CORRESPONDING FIELDS OF TABLE @gt_tmkm1t
*      FROM tmkm1t
*       FOR ALL ENTRIES IN @gt_disp
*     WHERE spras = @sy-langu
*       AND land1 = @gv_land1
*       AND taxim = @gt_disp-taxim.
*
*    SORT gt_tmkm1t BY taxim.
*
** 세금코드
*    lv_kalsm = lc_tax && gv_land1.
*
*    SELECT mwskz,
*           text1
*      INTO CORRESPONDING FIELDS OF TABLE @gt_t007s
*      FROM t007s
*       FOR ALL ENTRIES IN @gt_disp
*     WHERE spras = @sy-langu
*       AND kalsm = @lv_kalsm
*       AND mwskz = @gt_disp-mwskz.
*
*    SORT gt_t007s BY mwskz.
*
** 인도조건
*    SELECT inco1,
*           bezei
*      INTO CORRESPONDING FIELDS OF TABLE @gt_tinct
*      FROM tinct
*       FOR ALL ENTRIES IN @gt_disp
*     WHERE spras = @sy-langu
*       AND inco1 = @gt_disp-inco1.
*
*    SORT gt_tinct BY inco1.
*
** 단가유형
*    SELECT exprf,
*           bezei
*      INTO CORRESPONDING FIELDS OF TABLE @gt_t616t
*      FROM t616t
*       FOR ALL ENTRIES IN @gt_disp
*     WHERE spras = @sy-langu
*       AND land1 = @gv_land1
*       AND exprf = @gt_disp-exprf.
*
*    SORT gt_t616t BY exprf.
*
** 인쇄교체비
*    IF p_bukrs = gc_bukrs_1101.
*      SELECT matnr,
*             kbetr
*        INTO CORRESPONDING FIELDS OF TABLE @gt_zpri
*        FROM zsvcmm_a445_c
*         FOR ALL ENTRIES IN @gt_disp
*       WHERE matnr = @gt_disp-matnr
*         AND kschl = @lc_kschl_zpri   "'ZPRI'
*         AND ( ( datab <= @sy-datum AND datbi >= @sy-datum )
*             OR validprice = 'Y' )
*         AND loevm_ko = ''.
*
*      SORT gt_zpri BY matnr.
*    ENDIF.
*
*  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_ATTACH_DATA
*&---------------------------------------------------------------------*
FORM get_attach_data.

  DATA: lt_atta TYPE SORTED TABLE OF ty_atta WITH UNIQUE KEY instid_a.

  CLEAR gt_atta.

  lt_atta = CORRESPONDING #( gt_disp DISCARDING DUPLICATES MAPPING instid_a = infnr ).

  IF lt_atta IS NOT INITIAL.

    SELECT typeid_a,
           instid_a
      INTO TABLE @gt_atta
      FROM srgbtbrel
       FOR ALL ENTRIES IN @lt_atta
     WHERE instid_a = @lt_atta-instid_a
       AND typeid_a = @gc_gos_typeid
       AND catid_a  = @gc_gos_catid.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form DOMAIN_VALUE_GET
*&---------------------------------------------------------------------*
FORM domain_value_get USING    iv_domname
                               iv_domvalue
                      CHANGING cv_ddtext.

  DATA: lv_domname  TYPE dd07v-domname,
        lv_domvalue TYPE dd07v-domvalue_l,
        lv_ddtext   TYPE dd07v-ddtext.

  CLEAR cv_ddtext.

  lv_domname  = iv_domname.
  lv_domvalue = iv_domvalue.

  CALL FUNCTION 'DOMAIN_VALUE_GET'
    EXPORTING
      i_domname  = lv_domname
      i_domvalue = lv_domvalue
    IMPORTING
      e_ddtext   = lv_ddtext
    EXCEPTIONS
      not_exist  = 1
      OTHERS     = 2.

  IF sy-subrc = 0.
    cv_ddtext = lv_ddtext.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CHECK_EXIT
*&---------------------------------------------------------------------*
FORM check_exit.

  CASE gv_ok_code.
    WHEN 'EXIT'.
      LEAVE PROGRAM.
    WHEN OTHERS.
      LEAVE TO SCREEN 0.
  ENDCASE.

ENDFORM.
