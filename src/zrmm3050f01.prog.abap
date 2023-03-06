*&---------------------------------------------------------------------*
*& Include          ZRMM3050F01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form INITIALIZATION
*&---------------------------------------------------------------------*
FORM initialization.

* 예외처리 유저 점검
  zcl_mm_common=>common_config(
    EXPORTING
      is_common = VALUE #( m = 'A1' d = 'A1010' s = 'AB100' )
      it_where  = VALUE #( ( field = 1 value = sy-repid )
                           ( field = 2 value = 'EX01' )
                           ( field = 3 value = sy-uname ) )
    IMPORTING
      et_outtab = DATA(lt_config) ).

  READ TABLE lt_config INTO DATA(ls_config) INDEX 1.

  IF sy-subrc EQ 0.
    gv_exc_user = 'X'.
  ENDIF.

* 사용자 기본값
*  IF GV_EXC_USER IS INITIAL.
*  PERFORM set_init_values.
*  ENDIF.

* 생성일 기본값 (3개월 전)
  DATA: lv_sdate TYPE sy-datum.

  CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
    EXPORTING
      date      = sy-datum
      signum    = '-'
      days      = 0
      months    = 3
      years     = 0
    IMPORTING
      calc_date = lv_sdate.

  s_bedat[] = VALUE #( ( sign = 'I' option = 'BT' low = lv_sdate high = sy-datum ) ).

* 결재 상태 기본값
*  IF p_wfsts IS INITIAL.
*    p_wfsts = 'A'.  "ALL
*  ENDIF.

* 내외자구분 기본값
  IF p_kalsk IS INITIAL.
    p_kalsk = 'DO'.  "국내
  ENDIF.

  "[U1 변경시작 2022.08.24].
  DATA lv_field1 TYPE field_name.

  lv_field1 = sy-uname.

  CLEAR gv_mail.

  "[U2 변경시작 2022.10.14].
  CONSTANTS : lv_puord TYPE ztmm00002-zmidd_cat VALUE 'PUORD',
              lv_10050 TYPE ztmm00002-zsmal_cat VALUE '10050'.
  "[U2 변경종료 2022.10.14].

  SELECT SINGLE field1
    INTO lv_field1
    FROM ztmm00002
   WHERE zmain_cat = 'C1'
    "[U2 변경시작 2022.10.14].
     AND zmidd_cat = lv_puord
     AND zsmal_cat = lv_10050
*     AND ZMIDD_CAT = 'PUORD'
*     AND ZSMAL_CAT = '10050'
      "[U2 변경종료 2022.10.14].
     AND field1    = lv_field1.

  IF sy-subrc = 0.
    gv_mail = abap_true.
  ENDIF.
  "[U1 변경종료 2022.08.24].

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_INIT_VALUES
*&---------------------------------------------------------------------*
FORM set_init_values.

* 회사코드 기본값
*  SELECT SINGLE company, company_name
*    FROM zsvmm_user_info
*   WHERE user_id EQ @sy-uname
*    INTO @DATA(ls_user_info).
*
*  IF sy-subrc EQ 0.
*    p_bukrs = ls_user_info-company.
*  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CHECK_AUTHORITY
*&---------------------------------------------------------------------*
FORM check_authority.

* 권한체크 룰(추후 적용)
  CHECK 1 <> 1.

*  call function 'ZFMM_AUTH_CHECK'
*    EXPORTING
*      iv_user = sy-uname.
**     IV_OBJECT                   = 'ZMM_COMMON'
**     IV_BUKRS                    =
**     IV_EKORG                    =
**     IV_LGORT                    =
**     IV_WERKS                    =
**     IV_ZEXSPA                   =
**     IV_ZPODEP                   =
**     IV_ZPRDEP                   =
** TABLES
**     IT_BUKRS                    =
**     IT_EKORG                    =
**     IT_LGORT                    =
**     IT_WERKS                    =
**     IT_ZEXSPA                   =
**     IT_ZPODEP                   =
**     IT_ZPRDEP                   =
** EXCEPTIONS
**     NO_ID_DATA_FOUND            = 1
**     AUTHORIZATION_FAILURE       = 2
**     NO_INPUT_AUTH_VALUE         = 3
**     NO_DATA_FOUND               = 4
**     MANDATORYFIELDISMISS        = 5
**     OTHERS  = 6
*  IF sy-subrc <> 0.
*    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 DISPLAY LIKE 'E'.
*    LEAVE LIST-PROCESSING.
*  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_DATA
*&---------------------------------------------------------------------*
FORM get_data.

*--------------------------------
* 검색조건 설정
*--------------------------------
* 결재 상태 검색조건 설정
*  IF p_wfsts EQ 'A'.
*    CLEAR gr_frgke.
*  ELSE.
*    gr_frgke = VALUE #( ( sign = 'I' option = 'EQ' low = p_wfsts ) ).
*  ENDIF.

* 품목범주 검색조건 설정(External -> Internal)
  IF s_epstp IS INITIAL.
    CLEAR gr_pstyp.
  ELSE.
    gr_pstyp = s_epstp[].

    LOOP AT gr_pstyp INTO DATA(ls_pstyp).
      PERFORM item_category_input USING ls_pstyp-low  CHANGING ls_pstyp-low.
      PERFORM item_category_input USING ls_pstyp-high CHANGING ls_pstyp-high.

      MODIFY gr_pstyp FROM ls_pstyp.
    ENDLOOP.
  ENDIF.

* 구매오더 삭제 검색조건 설정
  IF p_loekz = 'X'.
    CLEAR gr_loekz.
  ELSE.
    gr_loekz = VALUE #( ( sign = 'I' option = 'EQ' low = space ) ).
  ENDIF.

*--------------------------------
* Data 추출
*--------------------------------
  CASE 'X'.
    WHEN p_ra.  "개요
      PERFORM get_data_ra.

      PERFORM processing_data_ra.

    WHEN p_rb.  "상세
      PERFORM get_data_rb.

      PERFORM processing_data_rb.

    WHEN OTHERS.
  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form ITEM_CATEGORY_INPUT
*&---------------------------------------------------------------------*
FORM item_category_input USING iv_epstp
                         CHANGING cv_pstyp.

  CALL FUNCTION 'ME_ITEM_CATEGORY_INPUT'
    EXPORTING
      epstp     = iv_epstp
    IMPORTING
      pstyp     = cv_pstyp
    EXCEPTIONS
      not_found = 1
      OTHERS    = 2.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_DATA_RA
*&---------------------------------------------------------------------*
FORM get_data_ra.

  CLEAR gt_disp_ra.

  SELECT a~bukrs,
         a~ekorg,
         a~frgke,
*         c~wf_status,

         a~lifnr,
*         c~name1,
         a~ebeln,
         b~ebelp,
         a~bsart,
*         c~batxt,
*         c~title,
*         c~totalvalue AS tag_amt,
*         c~taxamount AS tax_amt,
         a~waers,
*         c~zterm,
*         c~inco1,
         a~bedat,
         a~verkf,
         a~ihrez,
         a~unsez,
         a~kdatb,
         a~kdate,
         a~gwldt,
         a~submi,
         a~absgr,
         a~ekgrp,
         a~dppct,

         a~inco2_l, "INCO_SITE
         a~inco3_l,

         a~zorder_person,
*         E~EMPLOY_NAME AS ZORDER_PERSON_NAME,
         a~zorder_department,
*         E~DEPART_NAME AS ZORDER_DEPARTMENT_NAME,
         a~zexpen_person,
*         F~EMPLOY_NAME AS ZEXPEN_PERSON_NAME,
         a~zexpen_department,
*         F~DEPART_NAME AS ZEXPEN_DEPARTMENT_NAME,

         a~zcoop_qm,
         a~zqm_person,
*         G~EMPLOY_NAME AS ZQM_PERSON_NAME,
         a~zqm_department,
*         G~DEPART_NAME AS ZQM_DEPARTMENT_NAME,
         a~zreal_cost,
         a~zcontract_deposit,
         a~zcontract_guarn,
         a~zcont_gua_type,
         a~zcon_kdatb,
         a~zcon_kdate,
         a~zprepay_deposit,
         a~zprepay_grarn,
         a~zprep_gua_type,
         a~zpay_kdatb,
         a~zpay_kdate,
         a~zdefect_deposit,
         a~zdefect_guarn,
         a~zdefec_gua_type,
         a~zdef_base_date,
         a~zdef_kdatb,
         a~late_rate,

         a~zemanage2,
         a~zeshiptype,
         a~zededline,
         a~herkl,
         a~zeinspect
    INTO CORRESPONDING FIELDS OF TABLE @gt_disp_ra
    FROM ekko AS a INNER JOIN ekpo AS b
                              ON b~ebeln = a~ebeln
*                           INNER JOIN zsvcmm_po_tax AS c
*                           ON c~ebeln = a~ebeln
*                          AND c~bukrs = a~bukrs
                             LEFT JOIN lfm1 AS d
                                    ON d~lifnr = a~lifnr
                                   AND d~ekorg = a~ekorg

*                    LEFT JOIN ZSVMM_USER_INFO AS E
*                           ON E~EMPLOY_NO  = A~ZORDER_PERSON
*                          AND E~DEPARTMENT = A~ZORDER_DEPARTMENT
*                    LEFT JOIN ZSVMM_USER_INFO AS F
*                           ON F~EMPLOY_NO  = A~ZEXPEN_PERSON
*                          AND F~DEPARTMENT = A~ZEXPEN_DEPARTMENT
*                    LEFT JOIN ZSVMM_USER_INFO AS G
*                           ON G~EMPLOY_NO  = A~ZQM_PERSON
*                          AND G~DEPARTMENT = A~ZQM_DEPARTMENT
   WHERE a~bukrs  = @p_bukrs
     AND a~bstyp  = 'F'
     AND a~frgke IN @gr_frgke
     AND a~lifnr IN @s_lifnr
     AND a~bsart IN @s_bsart
     AND a~ekgrp IN @s_ekgrp
     AND b~werks IN @s_werks
     AND a~ebeln IN @s_ebeln
     AND b~konnr IN @s_konnr
     AND a~zorder_person     IN @s_order
     AND a~zorder_department IN @s_depto
     AND a~zexpen_person     IN @s_expen
     AND a~zexpen_department IN @s_depte
     AND a~zemanage2 IN @s_mange
     AND a~bedat IN @s_bedat
     AND b~matnr IN @s_matnr
     AND b~matkl IN @s_matkl
     AND b~pstyp IN @gr_pstyp
     AND b~knttp IN @s_knttp
     AND b~loekz IN @gr_loekz
     AND d~kalsk  = @p_kalsk.

  DATA(lt_tmp) = gt_disp_ra[].
  SORT lt_tmp BY ebeln.
  DELETE ADJACENT DUPLICATES FROM lt_tmp COMPARING ebeln.
  IF NOT lt_tmp[] IS INITIAL.

*    IF sy-langu NE '3'.
*      SELECT wf_status,
*           name1,
*           ebeln,
*           batxt,
*           title,
*           zterm,
*           inco1,
*           totalvalue AS tag_amt,
*           taxamount AS tax_amt
*        FROM zsvcmm_po_tax_en
*         FOR ALL ENTRIES IN @lt_tmp
*       WHERE ebeln = @lt_tmp-ebeln
*        INTO TABLE @DATA(lt_po_tax).
*
*    ELSE.
      SELECT wf_status,
           name1,
           ebeln,
           batxt,
           title,
           zterm,
           inco1,
           totalvalue AS tag_amt,
           taxamount AS tax_amt
        FROM zsvcmm_po_tax
         FOR ALL ENTRIES IN @lt_tmp
       WHERE ebeln = @lt_tmp-ebeln
        INTO TABLE @DATA(lt_po_tax).

*    ENDIF.

    SORT lt_po_tax BY ebeln.
    FREE lt_tmp.
  ENDIF.

  LOOP AT gt_disp_ra ASSIGNING FIELD-SYMBOL(<ls_disp_ra>).
    READ TABLE lt_po_tax INTO DATA(ls_po_tax)
                         WITH KEY ebeln = <ls_disp_ra>-ebeln
                         BINARY SEARCH.
    IF sy-subrc EQ 0.
      MOVE-CORRESPONDING ls_po_tax TO <ls_disp_ra>.
    ENDIF.
  ENDLOOP.

  SORT gt_disp_ra BY ebeln.

  DELETE ADJACENT DUPLICATES FROM gt_disp_ra COMPARING ebeln.

*--------------------------------
* TEXT 및 기타 Data
*--------------------------------
  PERFORM get_other_data_ra.

*--------------------------------
* 첨부파일 Data
*--------------------------------
  PERFORM get_attach_data.

*--------------------------------
* 전자결재 Data
*--------------------------------
*  PERFORM get_apv_data.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_OTHER_DATA_RA
*&---------------------------------------------------------------------*
FORM get_other_data_ra.

  CLEAR: gt_t052u, gt_tinct, gt_t024, gt_ekpa.
*  CLEAR: gt_user_ord, gt_user_exp, gt_user_qm.
*  CLEAR: gt_dept_ord, gt_dept_exp, gt_dept_qm.

  IF gt_disp_ra IS NOT INITIAL.

*   지급조건 TEXT
    SELECT zterm,
           text1
      INTO CORRESPONDING FIELDS OF TABLE @gt_t052u
      FROM t052u
       FOR ALL ENTRIES IN @gt_disp_ra
     WHERE spras = @sy-langu
       AND zterm = @gt_disp_ra-zterm.

    SORT gt_t052u BY zterm.

* 인도조건 TEXT
    SELECT inco1,
           bezei
      INTO CORRESPONDING FIELDS OF TABLE @gt_tinct
      FROM tinct
       FOR ALL ENTRIES IN @gt_disp_ra
     WHERE spras = @sy-langu
       AND inco1 = @gt_disp_ra-inco1.

    SORT gt_tinct BY inco1.

* 구매그룹명
    SELECT ekgrp,
           eknam
      INTO CORRESPONDING FIELDS OF TABLE @gt_t024
      FROM t024
       FOR ALL ENTRIES IN @gt_disp_ra
     WHERE ekgrp = @gt_disp_ra-ekgrp.

    SORT gt_t024 BY ekgrp.

* 공동수급업체
    SELECT a~ebeln,
           a~parvw,
           a~lifn2,
           b~name1
      INTO CORRESPONDING FIELDS OF TABLE @gt_ekpa
      FROM ekpa AS a INNER JOIN lfa1 AS b
                             ON a~lifn2 = b~lifnr
       FOR ALL ENTRIES IN @gt_disp_ra
     WHERE ebeln = @gt_disp_ra-ebeln.

    SORT gt_ekpa BY ebeln parvw.

** 발주담당자
*    SELECT DISTINCT a~emp_no,
*                    a~user_nm
*      FROM ztcn00002 AS a INNER JOIN @gt_disp_ra AS b
*                                  ON a~emp_no = b~zorder_person
*     WHERE a~bukrs = @p_bukrs
*      INTO CORRESPONDING FIELDS OF TABLE @gt_user_ord.
*
*    SORT gt_user_ord BY emp_no.
*
** 지출발의담당자
*    SELECT DISTINCT a~emp_no,
*                    a~user_nm
*      FROM ztcn00002 AS a INNER JOIN @gt_disp_ra AS b
*                                  ON a~emp_no = b~zexpen_person
*     WHERE a~bukrs = @p_bukrs
*      INTO CORRESPONDING FIELDS OF TABLE @gt_user_exp.
*
*    SORT gt_user_exp BY emp_no.
*
** 검수담당자
*    SELECT DISTINCT a~emp_no,
*                    a~user_nm
*      FROM ztcn00002 AS a INNER JOIN @gt_disp_ra AS b
*                                  ON a~emp_no = b~zqm_person
*     WHERE a~bukrs = @p_bukrs
*      INTO CORRESPONDING FIELDS OF TABLE @gt_user_qm.
*
*    SORT gt_user_qm BY emp_no.
*
** 발주부서
*    SELECT DISTINCT a~orgn_cd,
*                    a~orgn_nm
*      FROM ztcn00001 AS a INNER JOIN @gt_disp_ra AS b
*                                  ON a~orgn_cd = b~zorder_department
*     WHERE a~bukrs = @p_bukrs
*      INTO CORRESPONDING FIELDS OF TABLE @gt_dept_ord.
*
*    SORT gt_dept_ord BY orgn_cd.
*
** 지출발의부서
*    SELECT DISTINCT a~orgn_cd,
*                    a~orgn_nm
*      FROM ztcn00001 AS a INNER JOIN @gt_disp_ra AS b
*                                  ON a~orgn_cd = b~zexpen_department
*     WHERE a~bukrs = @p_bukrs
*      INTO CORRESPONDING FIELDS OF TABLE @gt_dept_exp.
*
*    SORT gt_dept_exp BY orgn_cd.
*
** 검수부서
*    SELECT DISTINCT a~orgn_cd,
*                    a~orgn_nm
*      FROM ztcn00001 AS a INNER JOIN @gt_disp_ra AS b
*                                  ON a~orgn_cd = b~zqm_department
*     WHERE a~bukrs = @p_bukrs
*      INTO CORRESPONDING FIELDS OF TABLE @gt_dept_qm.
*
*    SORT gt_dept_qm BY orgn_cd.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_ATTACH_DATA
*&---------------------------------------------------------------------*
FORM get_attach_data.

  DATA: lt_atta TYPE SORTED TABLE OF ty_atta WITH UNIQUE KEY instid_a.

  CLEAR gt_atta.

  lt_atta = CORRESPONDING #( gt_disp_ra DISCARDING DUPLICATES MAPPING instid_a = ebeln ).

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
*& Form PROCESSING_DATA_RA
*&---------------------------------------------------------------------*
FORM processing_data_ra.

  DATA: lt_po_header_texts TYPE TABLE OF bapiekkotx.

  CONSTANTS: lc_kaps(4) VALUE 'KAPS'.

  CHECK gt_disp_ra IS NOT INITIAL.

* 결재 상태 ICON 제외할 문서유형(KAPS)
  zcl_mm_common=>common_config(
    EXPORTING
      is_common = VALUE #( m = 'C1' d = 'PUORD' s = '1050' )
    IMPORTING
      et_outtab = DATA(lt_config) ).

  APPEND VALUE #( field1 = 'CD01' ) TO lt_config. "공사/용역 추가
  SORT lt_config BY field1.

  DELETE lt_config WHERE field2 <> lc_kaps.


  LOOP AT gt_disp_ra ASSIGNING FIELD-SYMBOL(<ls_disp>).

* 결재문서 ICON
    READ TABLE lt_config WITH KEY field1 = <ls_disp>-bsart
                                  BINARY SEARCH
                                  TRANSPORTING NO FIELDS.
*    IF sy-subrc <> 0.
*      READ TABLE gt_apv WITH KEY wfobject = <ls_disp>-ebeln
*                                 BINARY SEARCH
*                                 INTO DATA(ls_apv).
*      IF sy-subrc EQ 0.
*        <ls_disp>-apv_stat = icon_viewer_optical_archive.
*      ENDIF.
*    ENDIF.

* 세금계산서 발행방식
    CASE <ls_disp>-absgr.
      WHEN '1'.
        <ls_disp>-absgr_text = '정발행'.
      WHEN '2'.
        <ls_disp>-absgr_text = '역발행'.
      WHEN OTHERS.
        CLEAR: <ls_disp>-absgr, <ls_disp>-absgr_text.
    ENDCASE.

* 지급조건 TEXT
    READ TABLE gt_t052u WITH KEY zterm = <ls_disp>-zterm
                                 BINARY SEARCH
                                 INTO DATA(ls_t052u).
    IF sy-subrc = 0.
      <ls_disp>-vtext = ls_t052u-text1.
    ENDIF.

* 인도조건 TEXT
    READ TABLE gt_tinct WITH KEY inco1 = <ls_disp>-inco1
                                 BINARY SEARCH
                                 INTO DATA(ls_tinct).
    IF sy-subrc = 0.
      <ls_disp>-bezei = ls_tinct-bezei.
    ENDIF.

* 구매그룹명
    READ TABLE gt_t024 WITH KEY ekgrp = <ls_disp>-ekgrp
                                BINARY SEARCH
                                INTO DATA(ls_t024).
    IF sy-subrc = 0.
      <ls_disp>-eknam = ls_t024-eknam.
    ENDIF.

* 계약금액(VAT포함)
    <ls_disp>-tot_amt = <ls_disp>-tag_amt + <ls_disp>-tax_amt.

* 인코텀스장소
    IF p_kalsk = gc_kalsk_do.
      <ls_disp>-inco_site = <ls_disp>-inco2_l.
    ENDIF.

* 헤더텍스트(특약사항, 지출발의조건)
    CLEAR lt_po_header_texts.

    CALL FUNCTION 'BAPI_PO_GETDETAIL'
      EXPORTING
        purchaseorder   = <ls_disp>-ebeln
        items           = ' '
        header_texts    = 'X'
      TABLES
        po_header_texts = lt_po_header_texts.

    IF lt_po_header_texts IS NOT INITIAL.

      LOOP AT lt_po_header_texts INTO DATA(ls_po_header_texts).
        CASE ls_po_header_texts-text_id.
          WHEN 'K00'.
            CONCATENATE <ls_disp>-text_k00 ls_po_header_texts-text_line
                   INTO <ls_disp>-text_k00 SEPARATED BY space.
          WHEN 'K01'.
            CONCATENATE <ls_disp>-text_k01 ls_po_header_texts-text_line
                   INTO <ls_disp>-text_k01 SEPARATED BY space.
          WHEN OTHERS.
            CONTINUE.
        ENDCASE.
      ENDLOOP.

    ENDIF.

* 공동수급업체
    PERFORM get_ekpa_lifn2 USING:
      <ls_disp>-ebeln 'C1' CHANGING <ls_disp>-lifn2_c1 <ls_disp>-name1_c1,
      <ls_disp>-ebeln 'C2' CHANGING <ls_disp>-lifn2_c2 <ls_disp>-name1_c2,
      <ls_disp>-ebeln 'C3' CHANGING <ls_disp>-lifn2_c3 <ls_disp>-name1_c3,
      <ls_disp>-ebeln 'C4' CHANGING <ls_disp>-lifn2_c4 <ls_disp>-name1_c4,
      <ls_disp>-ebeln 'C5' CHANGING <ls_disp>-lifn2_c5 <ls_disp>-name1_c5.

* 담당자, 부서 정보
*    PERFORM set_person_info CHANGING <ls_disp>.

* 첨부파일
    READ TABLE gt_atta WITH KEY typeid_a = gc_gos_typeid
                                instid_a = <ls_disp>-ebeln
                                TRANSPORTING NO FIELDS.
    IF sy-subrc = 0.
      <ls_disp>-afile = icon_attachment.
    ENDIF.

    CLEAR: ls_t052u, ls_tinct, ls_t024.

  ENDLOOP.

  DESCRIBE TABLE gt_disp_ra LINES DATA(lv_tcnt).
  MESSAGE s011(zmm01) WITH lv_tcnt.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_DATA_RB
*&---------------------------------------------------------------------*
FORM get_data_rb.

  CLEAR gt_disp_rb.

  IF sy-langu NE '3'.
*    PERFORM get_data_rb_en.
  ELSE.
    PERFORM get_data_rb_ko.
  ENDIF.

  SORT gt_disp_rb BY ebeln ebelp.

  DELETE ADJACENT DUPLICATES FROM gt_disp_rb COMPARING ebeln ebelp.

*--------------------------------
* TEXT 및 기타 Data
*--------------------------------
  PERFORM get_other_data_rb.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_DATA_RB_KO
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM get_data_rb_ko .

  CONSTANTS: lc_etenr TYPE eket-etenr VALUE '0001'.

  SELECT a~bukrs,
         a~frgke,
         t~wf_status,
         a~lifnr,
         d~name1,
         a~bsart,
         d~batxt,
         a~ebeln,
         b~ebelp,
         b~loekz,
         d~title,
         b~matnr,
         b~txz01,
         b~bwtar,
         c~charg,
         b~menge,
         b~meins,
*         B~NETPR,
         b~brtwr AS brtwr_cal,
         e~pbxx_unitprice,
         e~pb00_unitprice,
         e~pbxx,
         e~pb00,
         a~waers,
         b~peinh,
         b~bprme,
         e~zcpr,
*         e~zprice_change AS change,
*         e~zprice_reason AS reason,
         b~mwskz,
         e~ztax,
         c~eindt,
         b~werks,
         b~lgort,
         b~matkl,

*         b~zinspection,
*         b~zqm_department,

         b~banfn,
         b~bnfpo,
         b~konnr,
         b~ktpnr,
         b~emlif,
         f~glaccount,
         f~wbselementexternalid AS wbselement,
         f~costcenter,
         f~orderid,
         f~salesorder,
         f~salesorderitem,
         f~masterfixedasset AS fixedasset,
         f~fundscenter,           "자금관리센터
         f~commitmentitem,     "약정항목


         e~zpri,
         e~zsup_unitprice,
         e~zsup,
         e~zsum_unitprice,
         e~zsum,
         e~zsur_unitprice,
         e~zsur,
         e~navs,
         b~bstae,

         e~zcu1_rate,
         e~zcu1,
         e~zfr1_rate,
         e~zfr1,
         e~zin1_rate,
         e~zin1,
         e~zot1_rate,
         e~zot1,

         b~retpo,
         b~repos,
         b~pstyp,
         b~knttp,
         b~elikz,
         b~uebto,
         b~webre
    INTO CORRESPONDING FIELDS OF TABLE @gt_disp_rb
    FROM ekko AS a INNER JOIN ekpo AS b
                           ON b~ebeln = a~ebeln
                   INNER JOIN eket AS c
                           ON c~ebeln = b~ebeln
                          AND c~ebelp = b~ebelp
                          AND c~etenr = @lc_etenr
                   INNER JOIN zsvcmm_po_tax AS t
                           ON t~ebeln = a~ebeln
                          AND t~bukrs = a~bukrs
                    LEFT JOIN zsvcmm_po_tax AS d
                           ON d~ebeln = a~ebeln
                          AND d~bukrs = a~bukrs
                    LEFT JOIN zsvcmm_price AS e
                           ON e~ebeln = b~ebeln
                          AND e~ebelp = b~ebelp
                    LEFT JOIN cpoacctasstp AS f
                           ON f~purchaseorder     = b~ebeln
                          AND f~purchaseorderitem = b~ebelp
                    LEFT JOIN lfm1 AS g
                           ON g~lifnr = a~lifnr
                          AND g~ekorg = a~ekorg
   WHERE a~bukrs  = @p_bukrs
     AND a~bstyp  = 'F'
     AND a~frgke IN @gr_frgke
     AND a~lifnr IN @s_lifnr
     AND a~bsart IN @s_bsart
     AND a~ekgrp IN @s_ekgrp
     AND b~werks IN @s_werks
     AND a~ebeln IN @s_ebeln
     AND b~konnr IN @s_konnr
     AND a~zorder_person     IN @s_order
     AND a~zorder_department IN @s_depto
     AND a~zexpen_person     IN @s_expen
     AND a~zexpen_department IN @s_depte
     AND a~zemanage2 IN @s_mange
     AND a~bedat IN @s_bedat
     AND b~matnr IN @s_matnr
     AND b~matkl IN @s_matkl
     AND b~pstyp IN @gr_pstyp
     AND b~knttp IN @s_knttp
     AND b~loekz IN @gr_loekz
     AND g~kalsk  = @p_kalsk.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_OTHER_DATA_RB
*&---------------------------------------------------------------------*
FORM get_other_data_rb.

  CONSTANTS: lc_kalsm      TYPE kalsm_d VALUE 'TAXKR',
             lc_ktopl_k000 TYPE skat-ktopl VALUE '1000'.

  CLEAR: gt_taxcode, gt_t001w, gt_t001l, gt_t023t, "gt_dept_qm, gt_33022
         gt_skat, gt_prps, gt_cskt, gt_aufk, gt_anla, gt_stxl.

  IF gt_disp_rb IS NOT INITIAL.

* 세금코드명
    SELECT taxcode,
           taxcodename
      INTO CORRESPONDING FIELDS OF TABLE @gt_taxcode
      FROM cmmtaxcodevh
       FOR ALL ENTRIES IN @gt_disp_rb
     WHERE taxcode = @gt_disp_rb-mwskz
       AND taxcalculationprocedure = @lc_kalsm.

    SORT gt_taxcode BY taxcode.

* 플랜트명
    SELECT werks,
           name1
      INTO CORRESPONDING FIELDS OF TABLE @gt_t001w
      FROM t001w
       FOR ALL ENTRIES IN @gt_disp_rb
     WHERE werks = @gt_disp_rb-werks.

    SORT gt_t001w BY werks.

* 저장위치명
    SELECT werks,
           lgort,
           lgobe
      INTO CORRESPONDING FIELDS OF TABLE @gt_t001l
      FROM t001l
       FOR ALL ENTRIES IN @gt_disp_rb
     WHERE werks = @gt_disp_rb-werks
       AND lgort = @gt_disp_rb-lgort.

    SORT gt_t001l BY werks lgort.

* 자재그룹명
    SELECT matkl,
           wgbez
      INTO CORRESPONDING FIELDS OF TABLE @gt_t023t
      FROM t023t
       FOR ALL ENTRIES IN @gt_disp_rb
     WHERE spras = @sy-langu
       AND matkl = @gt_disp_rb-matkl.

    SORT gt_t023t BY matkl.

* 검수부서
*    SELECT DISTINCT
*           a~orgn_cd,
*           a~orgn_nm
*      FROM ztcn00001 AS a INNER JOIN @gt_disp_rb AS b
*                                  ON a~orgn_cd = b~zqm_department
*     WHERE a~bukrs = @p_bukrs
*      INTO CORRESPONDING FIELDS OF TABLE @gt_dept_qm.
*
*    SORT gt_dept_qm BY orgn_cd.

* GL 계정
    SELECT saknr,
           txt20
      INTO CORRESPONDING FIELDS OF TABLE @gt_skat
      FROM skat
       FOR ALL ENTRIES IN @gt_disp_rb
     WHERE spras = @sy-langu
       AND ktopl = @lc_ktopl_k000
       AND saknr = @gt_disp_rb-glaccount.

    SORT gt_skat BY saknr.

* WBS 요소
    SELECT a~pspnr,
           a~post1
      FROM prps AS a INNER JOIN @gt_disp_rb AS b
                             ON a~pspnr = b~wbselement
     WHERE a~pbukr = @p_bukrs
      INTO CORRESPONDING FIELDS OF TABLE @gt_prps.

    SORT gt_prps BY pspnr.

* 코스트센터
    SELECT kostl,
           kokrs,
           ktext
      INTO CORRESPONDING FIELDS OF TABLE @gt_cskt
      FROM cskt
       FOR ALL ENTRIES IN @gt_disp_rb
     WHERE spras = @sy-langu
       AND kokrs = @gt_disp_rb-bukrs
       AND kostl = @gt_disp_rb-costcenter.

    SORT gt_cskt BY kostl kokrs.

* Internal Order
    SELECT aufnr,
           ktext
      INTO CORRESPONDING FIELDS OF TABLE @gt_aufk
      FROM aufk
       FOR ALL ENTRIES IN @gt_disp_rb
     WHERE aufnr = @gt_disp_rb-orderid.

    SORT gt_aufk BY aufnr.

* 자산번호
    SELECT anln1,
           bukrs,
           txt50
      INTO CORRESPONDING FIELDS OF TABLE @gt_anla
      FROM anla
       FOR ALL ENTRIES IN @gt_disp_rb
     WHERE bukrs = @gt_disp_rb-bukrs
       AND anln1 = @gt_disp_rb-fixedasset.

    SORT gt_anla BY anln1 bukrs.

* 납품 텍스트
    SELECT a~relid,
           a~tdobject,
           a~tdname,
           a~tdid,
           a~tdspras
      FROM stxl AS a INNER JOIN @gt_disp_rb AS b
                             ON left( a~tdname, 10 ) = b~ebeln
                            AND right( a~tdname, 5 ) = b~ebelp
     WHERE a~relid    = 'TX'
       AND a~tdobject = @gc_tab_ekpo
       AND a~tdid     = 'F04'
      INTO CORRESPONDING FIELDS OF TABLE @gt_stxl.

    SORT gt_stxl BY tdname.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form PROCESSING_DATA_RB
*&---------------------------------------------------------------------*
FORM processing_data_rb.

  DATA: lv_bapi_curr TYPE bapicurr-bapicurr.

  CONSTANTS: lc_bstae_0004 TYPE ekpo-bstae VALUE '0004'.

  DEFINE _l_curr_conv.
    CLEAR lv_bapi_curr.
    lv_bapi_curr = &1.
  CALL FUNCTION 'BAPI_CURRENCY_CONV_TO_INTERNAL'
    EXPORTING
      currency             = &2
      amount_external      = lv_bapi_curr
      max_number_of_digits = 15
    IMPORTING
      amount_internal      = &1.
  END-OF-DEFINITION.


  CHECK gt_disp_rb IS NOT INITIAL.

  LOOP AT gt_disp_rb ASSIGNING FIELD-SYMBOL(<ls_disp>).

* Set Text Data
    PERFORM set_text_data_rb CHANGING <ls_disp>.

*    특성정보.
*    PERFORM set_char_data_rb CHANGING <ls_disp>.

* 품목단가
    CLEAR lv_bapi_curr.

    IF <ls_disp>-pbxx_unitprice IS INITIAL.
      lv_bapi_curr = <ls_disp>-pb00_unitprice.
    ELSE.
      lv_bapi_curr = <ls_disp>-pbxx_unitprice.
    ENDIF.

    CALL FUNCTION 'BAPI_CURRENCY_CONV_TO_INTERNAL'
      EXPORTING
        currency             = <ls_disp>-waers
        amount_external      = lv_bapi_curr
        max_number_of_digits = 15
      IMPORTING
        amount_internal      = <ls_disp>-netpr.

* 품목금액
    IF <ls_disp>-pbxx IS INITIAL.
      <ls_disp>-brtwr = <ls_disp>-pb00.
    ELSE.
      <ls_disp>-brtwr = <ls_disp>-pbxx.
    ENDIF.

* 임가공 가공단가, 가공비(KGC)
*    IF <ls_disp>-bukrs = gc_bukrs_2101.
*
*      IF <ls_disp>-zsup_unitprice IS NOT INITIAL.
*        _l_curr_conv <ls_disp>-zsup_unitprice <ls_disp>-waers.
*      ENDIF.
*
*      IF <ls_disp>-zsum_unitprice IS NOT INITIAL.
*        _l_curr_conv <ls_disp>-zsum_unitprice <ls_disp>-waers.
*      ENDIF.
*
*      IF <ls_disp>-zsur_unitprice IS NOT INITIAL.
*        _l_curr_conv <ls_disp>-zsur_unitprice <ls_disp>-waers.
*      ENDIF.

*    ENDIF.

* 조정금액(품목금액+품목가감액), 조정단가(조정금액/품목수량)
*    IF <ls_disp>-zcpr IS NOT INITIAL.
*      <ls_disp>-adj_amt = <ls_disp>-brtwr + <ls_disp>-zcpr.
*
*      IF <ls_disp>-menge <> 0.
*        <ls_disp>-adj_net = <ls_disp>-adj_amt / <ls_disp>-menge.
*      ENDIF.
*    ENDIF.

* VAT(ZTAX+NAVS)
    <ls_disp>-ztax = <ls_disp>-ztax + <ls_disp>-navs.

* 공급가액
    <ls_disp>-total_amt = <ls_disp>-brtwr_cal.

* 계약금액(VAT포함)(품목금액+조정금액+VAT)
    <ls_disp>-cont_amt = <ls_disp>-brtwr_cal  + <ls_disp>-ztax.

* 품목삭제
    IF <ls_disp>-loekz IS NOT INITIAL.
      <ls_disp>-loekz = 'X'.
    ENDIF.

* 불공제여부
    IF <ls_disp>-navs <> 0.
      <ls_disp>-navs_text = 'Y'.
    ENDIF.

* 납품서생성
    IF <ls_disp>-bstae = lc_bstae_0004.
      <ls_disp>-bstae = 'Y'.
    ELSE.
      CLEAR <ls_disp>-bstae.
    ENDIF.

* 무상품목
    IF <ls_disp>-repos = 'X'.
      CLEAR <ls_disp>-repos.
    ELSE.
      <ls_disp>-repos = 'X'.
    ENDIF.

  ENDLOOP.

  DESCRIBE TABLE gt_disp_rb LINES DATA(lv_tcnt).
  MESSAGE s011(zmm01) WITH lv_tcnt.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_TEXT_DATA_RB
*&---------------------------------------------------------------------*
FORM set_text_data_rb CHANGING cs_disp TYPE ty_disp_rb.

* 세금코드명
  READ TABLE gt_taxcode WITH KEY taxcode = cs_disp-mwskz
                                 BINARY SEARCH
                                 INTO DATA(ls_taxcode).
  IF sy-subrc = 0.
    cs_disp-mwskz_text = ls_taxcode-taxcodename.
  ENDIF.

* 플랜트명
  READ TABLE gt_t001w WITH KEY werks = cs_disp-werks
                               BINARY SEARCH
                               INTO DATA(ls_t001w).
  IF sy-subrc = 0.
    cs_disp-werks_text = ls_t001w-name1.
  ENDIF.

* 저장위치명
  READ TABLE gt_t001l WITH KEY werks = cs_disp-werks
                               lgort = cs_disp-lgort
                               BINARY SEARCH
                               INTO DATA(ls_t001l).
  IF sy-subrc = 0.
    cs_disp-lgobe = ls_t001l-lgobe.
  ENDIF.

* 자재그룹명
  READ TABLE gt_t023t WITH KEY matkl = cs_disp-matkl
                               BINARY SEARCH
                               INTO DATA(ls_t023t).
  IF sy-subrc = 0.
    cs_disp-wgbez = ls_t023t-wgbez.
  ENDIF.


* GL 계정명
  READ TABLE gt_skat WITH KEY saknr = cs_disp-glaccount
                              BINARY SEARCH
                              INTO DATA(ls_skat).
  IF sy-subrc = 0.
    cs_disp-glaccount_text = ls_skat-txt20.
  ENDIF.

* WBS 요소명
  READ TABLE gt_prps WITH KEY pspnr = cs_disp-wbselement
                              BINARY SEARCH
                              INTO DATA(ls_prps).
  IF sy-subrc = 0.
    cs_disp-wbselement_text = ls_prps-post1.
  ENDIF.

* 코스트센터명
  READ TABLE gt_cskt WITH KEY kostl = cs_disp-costcenter
                              kokrs = cs_disp-bukrs
                              BINARY SEARCH
                              INTO DATA(ls_cskt).
  IF sy-subrc = 0.
    cs_disp-costcenter_text = ls_cskt-ktext.
  ENDIF.

* Internal Order명
  READ TABLE gt_aufk WITH KEY aufnr = cs_disp-orderid
                              BINARY SEARCH
                              INTO DATA(ls_aufk).
  IF sy-subrc = 0.
    cs_disp-orderid_text = ls_aufk-ktext.
  ENDIF.

* 자산명
  READ TABLE gt_anla WITH KEY anln1 = cs_disp-fixedasset
                              bukrs = cs_disp-bukrs
                              BINARY SEARCH
                              INTO DATA(ls_anla).
  IF sy-subrc = 0.
    cs_disp-fixedasset_text = ls_anla-txt50.
  ENDIF.

* 품목범주(Internal -> External)
  PERFORM item_category_output USING    cs_disp-pstyp
                               CHANGING cs_disp-epstp
                                        cs_disp-ptext.

* 아이템텍스트: 납품 텍스트
  READ TABLE gt_stxl WITH KEY tdname = cs_disp-ebeln && cs_disp-ebelp
                              BINARY SEARCH
                              TRANSPORTING NO FIELDS.
  IF sy-subrc = 0.
    cs_disp-text_f04 = icon_select_detail.
  ENDIF.

  CLEAR: ls_taxcode, ls_t001w, ls_t001l, ls_t023t, "ls_dept_qm,
         ls_skat, ls_prps, ls_cskt, ls_aufk, ls_anla.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form ITEM_CATEGORY_OUTPUT
*&---------------------------------------------------------------------*
FORM item_category_output USING iv_pstyp
                          CHANGING cv_epstp
                                   cv_ptext.

  CALL FUNCTION 'ME_ITEM_CATEGORY_OUTPUT'
    EXPORTING
      pstyp     = iv_pstyp
    IMPORTING
      epstp     = cv_epstp
      ptext     = cv_ptext
    EXCEPTIONS
      not_found = 1
      OTHERS    = 2.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form GET_EKPA_LIFN2
*&---------------------------------------------------------------------*
FORM get_ekpa_lifn2 USING iv_ebeln
                             iv_parvw
                    CHANGING cv_lifn2
                             cv_name1.

  READ TABLE gt_ekpa WITH KEY ebeln = iv_ebeln
                              parvw = iv_parvw
                              BINARY SEARCH
                              INTO DATA(ls_ekpa).
  IF sy-subrc = 0.
    cv_lifn2 = ls_ekpa-lifn2.
    cv_name1 = ls_ekpa-name1.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SUBMIT_ZRMM3020
*&---------------------------------------------------------------------*
FORM submit_zrmm3020.

  DATA: lr_bedat TYPE RANGE OF ekko-bedat,
        lr_ebeln TYPE RANGE OF ekko-ebeln.

  CLEAR: gt_rows.

  CALL METHOD grf_grid->get_selected_rows
    IMPORTING
      et_index_rows = gt_rows.
  DELETE gt_rows WHERE rowtype IS NOT INITIAL.

  IF gt_rows IS INITIAL.
    MESSAGE i006 DISPLAY LIKE 'E'.  "선택된 데이타가 없습니다.
    EXIT.
  ENDIF.

*-----------------------------
* 구매 실적 Report(ZRMM3020) 이동
*-----------------------------
  LOOP AT gt_rows INTO DATA(ls_rows).
    CASE 'X'.
      WHEN p_ra.
        READ TABLE gt_disp_ra INTO DATA(ls_disp_ra) INDEX ls_rows-index.
        lr_ebeln = VALUE #( BASE lr_ebeln ( sign = 'I' option = 'EQ' low = ls_disp_ra-ebeln ) ).

      WHEN p_rb.
        READ TABLE gt_disp_rb INTO DATA(ls_disp_rb) INDEX ls_rows-index.
        lr_ebeln = VALUE #( BASE lr_ebeln ( sign = 'I' option = 'EQ' low = ls_disp_rb-ebeln ) ).
    ENDCASE.
  ENDLOOP.

  CHECK lr_ebeln IS NOT INITIAL.

  lr_bedat = VALUE #( ( sign = 'I' option = 'BT' low = '10000101' high = '99991231' ) ).

  SUBMIT zrmm3020 WITH p_bukrs  = p_bukrs
                  WITH s_bedat IN lr_bedat
                  WITH s_ebeln IN lr_ebeln
                  WITH p_ra     = 'X'
                  WITH p_rb     = ''
                  AND RETURN.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CLOSE_ORDER_MULTI
*&---------------------------------------------------------------------*
FORM close_order_multi.

  DATA: lv_subrc TYPE sy-subrc.

  DATA: lt_return  TYPE TABLE OF bapiret2,
        lt_poitem  TYPE TABLE OF zsmm_poitem,
        lt_poitemx TYPE TABLE OF zsmm_poitemx.

  CLEAR gt_rows.

  CALL METHOD grf_grid->get_selected_rows
    IMPORTING
      et_index_rows = gt_rows.
  DELETE gt_rows WHERE rowtype IS NOT INITIAL.

  IF gt_rows IS INITIAL.
    MESSAGE i006 DISPLAY LIKE 'E'.  "선택된 데이타가 없습니다.
    EXIT.
  ENDIF.

*-----------------------------
* Validation Check
*-----------------------------
* 변경 불가능 문서유형
  zcl_mm_common=>common_config(
    EXPORTING
      is_common = VALUE #( m = 'C1' d = 'PUORD' s = '1050' )
    IMPORTING
      et_outtab = DATA(lt_config) ).

  SORT lt_config BY field1.

  LOOP AT gt_rows INTO DATA(ls_rows).

    READ TABLE gt_disp_rb INTO DATA(ls_disp) INDEX ls_rows-index.

* 상신중일 경우 처리 불가
    IF ls_disp-frgke = 'D'.
      lv_subrc = 4.
      MESSAGE i000 WITH TEXT-m01 DISPLAY LIKE 'E'.  "상신중인 구매오더가 포함되어 있습니다.
      EXIT.
    ENDIF.

* 변경 불가능 문서유형
    DELETE lt_config WHERE field2 = 'SAP'.  "SAP에서 생성된 문서는 변경 가능

    READ TABLE lt_config WITH KEY field1 = ls_disp-bsart
                                  BINARY SEARCH
                                  TRANSPORTING NO FIELDS.
    IF sy-subrc = 0.
      lv_subrc = 4.
      MESSAGE i000 WITH TEXT-m02 DISPLAY LIKE 'E'.  "변경 불가능한 구매오더 문서유형이 포함되어 있습니다.
      EXIT.
    ENDIF.

  ENDLOOP.

*-----------------------------
* 납품완료 처리
*-----------------------------
  CHECK lv_subrc IS INITIAL.

  LOOP AT gt_rows INTO ls_rows.

    CLEAR: lt_return, lt_poitem, lt_poitemx.

    READ TABLE gt_disp_rb INTO ls_disp INDEX ls_rows-index.

    lt_poitem  = VALUE #( ( ebelp = ls_disp-ebelp
                            elikz = 'X' ) ).

    lt_poitemx = VALUE #( ( ebelp = ls_disp-ebelp
                            elikz = 'X' ) ).

    call function 'ZFMM_PO_CHANGE'
      EXPORTING
        iv_ebeln   = ls_disp-ebeln
      TABLES
        et_return  = lt_return
        it_poitem  = lt_poitem
        it_poitemx = lt_poitemx.

  ENDLOOP.

*-----------------------------
* Refresh
*-----------------------------
  PERFORM get_data.

  grf_grid->refresh_grid_display( ).

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CLOSE_ORDER_SINGLE
*&---------------------------------------------------------------------*
FORM close_order_single.

  CLEAR gt_rows.

  CALL METHOD grf_grid->get_selected_rows
    IMPORTING
      et_index_rows = gt_rows.
  DELETE gt_rows WHERE rowtype IS NOT INITIAL.

  IF gt_rows IS INITIAL.
    MESSAGE i006 DISPLAY LIKE 'E'.  "선택된 데이타가 없습니다.
    EXIT.
  ENDIF.

  IF lines( gt_rows ) > 1.
    MESSAGE i007 DISPLAY LIKE 'E'.  "1 개의 라인을 선택하십시오.
    EXIT.
  ENDIF.

*-----------------------------
* Validation Check
*-----------------------------
  READ TABLE gt_rows INTO DATA(ls_rows) INDEX 1.

  READ TABLE gt_disp_rb INTO DATA(ls_disp) INDEX ls_rows-index.

* 변경 불가능 문서유형
  zcl_mm_common=>common_config(
    EXPORTING
      is_common = VALUE #( m = 'C1' d = 'PUORD' s = '1050' )
    IMPORTING
      et_outtab = DATA(lt_config) ).

  SORT lt_config BY field1.

* 상신중일 경우 처리 불가
  IF ls_disp-frgke = 'D'.
    MESSAGE i000 WITH TEXT-m01 DISPLAY LIKE 'E'.  "상신중인 구매오더가 포함되어 있습니다.
    RETURN.
  ENDIF.

* 변경 불가능 문서유형
  DELETE lt_config WHERE field2 = 'SAP'.  "SAP에서 생성된 문서는 변경 가능

  READ TABLE lt_config WITH KEY field1 = ls_disp-bsart
                                BINARY SEARCH
                                TRANSPORTING NO FIELDS.
  IF sy-subrc = 0.
    MESSAGE i000 WITH TEXT-m02 DISPLAY LIKE 'E'.  "변경 불가능한 구매오더 문서유형이 포함되어 있습니다.
    RETURN.
  ENDIF.

*-----------------------------
* 납품완료 처리(With 종료 사유)
*-----------------------------
  gv_text_mode = gc_mode_change.

  gs_close_key-ebeln = ls_disp-ebeln.
  gs_close_key-ebelp = ls_disp-ebelp.

  PERFORM popup_item_text_f04 USING ls_disp-ebeln
                                    ls_disp-ebelp.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form POPUP_ITEM_TEXT_F04
*&---------------------------------------------------------------------*
FORM popup_item_text_f04 USING iv_ebeln
                               iv_ebelp.

  DATA: lv_id     TYPE thead-tdid,
        lv_name   TYPE thead-tdname,
        lv_object TYPE thead-tdobject,
        lt_tline  TYPE TABLE OF tline.

  DATA: ls_text TYPE ty_text.

  CLEAR gt_text_f04.

  lv_id     = 'F04'.
  lv_name   = iv_ebeln && iv_ebelp.
  lv_object = gc_tab_ekpo.

  CALL FUNCTION 'READ_TEXT'
    EXPORTING
      id                      = lv_id
      language                = sy-langu
      name                    = lv_name
      object                  = lv_object
    TABLES
      lines                   = lt_tline
    EXCEPTIONS
      id                      = 1
      language                = 2
      name                    = 3
      not_found               = 4
      object                  = 5
      reference_check         = 6
      wrong_access_to_archive = 7
      OTHERS                  = 8.

  IF sy-subrc = 0.
    LOOP AT lt_tline INTO DATA(ls_tline).
      ls_text-line = ls_tline-tdline.
      APPEND ls_text TO gt_text_f04.
    ENDLOOP.
  ENDIF.

* 팝업 호출
  CALL SCREEN '0300' STARTING AT 05 05.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form submit_po_change
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM submit_po_change.

  CONSTANTS: lc_tcode_zomm3521     TYPE sy-tcode VALUE 'ZOMM3521',
             lc_tcode_zomm3521_01  TYPE sy-tcode VALUE 'ZOMM3521_01',
             lc_tcode_zommg3521    TYPE sy-tcode VALUE 'ZOMMG3521',
             lc_tcode_zommg3521_01 TYPE sy-tcode VALUE 'ZOMMG3521_01'.

  CLEAR: gt_rows.

  CALL METHOD grf_grid->get_selected_rows
    IMPORTING
      et_index_rows = gt_rows.
  DELETE gt_rows WHERE rowtype IS NOT INITIAL.

  IF gt_rows IS INITIAL.
    MESSAGE i006 DISPLAY LIKE 'E'.  "선택된 데이타가 없습니다.
    EXIT.
  ENDIF.

  IF lines( gt_rows ) > 1.
    MESSAGE i007 DISPLAY LIKE 'E'.  "1 개의 라인을 선택하십시오.
    EXIT.
  ENDIF.

*-----------------------------
* 구매오더 변경(ZOMM3511, ZOMM3521) 이동
*-----------------------------
  READ TABLE gt_rows INTO DATA(ls_rows) INDEX 1.

  READ TABLE gt_disp_ra INTO DATA(ls_disp) INDEX ls_rows-index.

* 담당자 / 담당부서
*  SELECT SINGLE employ_no, department
*    FROM zsvmm_user_info
*   WHERE user_id EQ @sy-uname
*    INTO @DATA(ls_user_info).

*  zcl_mmg_common=>common_g_config(
*    EXPORTING
*      is_common = VALUE #( m = 'A1' d = 'A1000' s = 'AA100' )
*      it_where  = VALUE #( ( field = 2 value = ls_disp-bukrs ) )
*    IMPORTING
*      et_outtab = DATA(lt_config) ).
*  READ TABLE lt_config INTO DATA(ls_config) INDEX 1.
*
*  IF ls_config-field9 = 'T1'.

  CASE ls_disp-bsart.
      WHEN 'PSM1' OR 'PSM2'.
*        SUBMIT zomm3511 WITH p_bukrs = ls_disp-bukrs
*                        WITH p_ekorg = ls_disp-ekorg
*                        WITH p_persn = ls_user_info-employ_no
*                        WITH p_depat = ls_user_info-department
*                        WITH p_ebeln = ls_disp-ebeln
*                        WITH p_mode  = 'C'
*                        AND RETURN.

    WHEN 'PSIM' OR 'PSIN'.
*        SUBMIT zomm3521 WITH p_bukrs = ls_disp-bukrs
*                        WITH p_ekorg = ls_disp-ekorg
*                        WITH p_persn = ls_user_info-employ_no
*                        WITH p_depat = ls_user_info-department
*                        WITH p_ebeln = ls_disp-ebeln
*                        WITH p_mode  = 'C'
*                        WITH p_tcode = lc_tcode_zomm3521
*                        AND RETURN.

    WHEN 'PSIC'.
       CASE p_kalsk.
          WHEN gc_kalsk_do. "국내: 계약참조 공사용역 구매오더 변경
*            SUBMIT zomm3521 WITH p_bukrs = ls_disp-bukrs
*                            WITH p_ekorg = ls_disp-ekorg
*                            WITH p_persn = ls_user_info-employ_no
*                            WITH p_depat = ls_user_info-department
*                            WITH p_ebeln = ls_disp-ebeln
*                            WITH p_mode  = 'C'
*                            WITH p_tcode = lc_tcode_zomm3521_01
*                            AND RETURN.
*
          WHEN gc_kalsk_im. "해외: 외자 PO 변경
*            SUBMIT zomm3521 WITH p_bukrs = ls_disp-bukrs
*                            WITH p_ekorg = ls_disp-ekorg
*                            WITH p_persn = ls_user_info-employ_no
*                            WITH p_depat = ls_user_info-department
*                            WITH p_ebeln = ls_disp-ebeln
*                            WITH p_mode  = 'C'
*                            WITH p_tcode = lc_tcode_zomm3521
*                            AND RETURN.
      ENDCASE.

    WHEN OTHERS.
      MESSAGE i000 WITH TEXT-m02 DISPLAY LIKE 'E'.  "변경 불가능한 구매오더 문서유형이 포함되어 있습니다.
      RETURN.
  ENDCASE.

*  ELSEIF ls_config-field9 = 'T2'.
*    DATA : lv_tcode TYPE sy-tcode.
*
*    CASE ls_disp-bsart.
*      WHEN 'PSIC'.
*        lv_tcode = lc_tcode_zommg3521_01.
*      WHEN 'PSM1' OR 'PSM2' OR 'PSIM' OR 'PSIN'.
*        lv_tcode = lc_tcode_zommg3521.
*      WHEN OTHERS.
*        MESSAGE i000 WITH TEXT-m02 DISPLAY LIKE 'E'.  "변경 불가능한 구매오더 문서유형이 포함되어 있습니다.
*        RETURN.
*    ENDCASE.

*    SUBMIT zommg3521 WITH p_bukrs = ls_disp-bukrs
*                    WITH p_ekorg = ls_disp-ekorg
*                    WITH p_persn = ls_user_info-employ_no
*                    WITH p_depat = ls_user_info-department
*                    WITH p_ebeln = ls_disp-ebeln
*                    WITH p_mode  = 'C'
*                    WITH p_tcode = lv_tcode
*                    AND RETURN.
*  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form POPUP_CHANGE_MASS_ORDER
*&---------------------------------------------------------------------*
FORM popup_change_mass_order.

  CLEAR : gt_rows,
  gs_mass.

  CALL METHOD grf_grid->get_selected_rows
    IMPORTING
      et_index_rows = gt_rows.
  DELETE gt_rows WHERE rowtype IS NOT INITIAL.

  IF gt_rows IS INITIAL.
    MESSAGE i006 DISPLAY LIKE 'E'.  "선택된 데이타가 없습니다.
    EXIT.
  ENDIF.

  IF p_ra = abap_true. " 헤더변경 시

    IF line_exists( gt_rows[ 2 ] ).  "1건씩 가능
      MESSAGE i007 DISPLAY LIKE 'E'.
      EXIT.
    ENDIF.

*    SELECT SINGLE employ_no
*      FROM zsvmm_user_info
*      INTO @DATA(lv_user)
*    WHERE user_id EQ @sy-uname.

    READ TABLE gt_rows INTO DATA(ls_row) INDEX 1.
    READ TABLE gt_disp_ra INTO DATA(ls_disp_ra) INDEX ls_row-index.
*    IF lv_user NE ls_disp_ra-zorder_person.  "담당자가 다른 경우 변경 불가
*      MESSAGE s000 WITH TEXT-m03 DISPLAY LIKE 'E'.
*      DATA(lv_err) = abap_true.
*      EXIT.
*    ENDIF.

*    IF ls_disp_ra-bsart+0(2) = 'PK'. " 캡스에서 생성된 PO 는 변경 불가
*      MESSAGE s000 WITH TEXT-m02 DISPLAY LIKE 'E'.
*      lv_err = abap_true.
*      EXIT.
*    ENDIF.

    SELECT SINGLE ebeln, absgr, zemanage2
      FROM ekko
      INTO @DATA(ls_ekko)
    WHERE ebeln  EQ @ls_disp_ra-ebeln.
    IF ls_ekko-absgr IS NOT INITIAL.
      gs_mass-absgr = ls_ekko-absgr.
      SELECT SINGLE absgr_txt
          INTO @gs_mass-atext
        FROM t165m
        WHERE spras = @sy-langu
           AND absgr = @gs_mass-absgr.
      gs_mass-abs = 'X'.
    ENDIF.
    IF ls_ekko-zemanage2 IS NOT INITIAL.
      gs_mass-zemanage2 = ls_ekko-zemanage2.
      gs_mass-zma = 'X'.
    ENDIF.

    IF gs_mass IS INITIAL.
*      lv_err = 'X'.
      MESSAGE s000 WITH TEXT-m04 DISPLAY LIKE 'E'.
    ENDIF.
  ELSE.
    READ TABLE gt_rows INTO ls_row INDEX 1.
    READ TABLE gt_disp_rb INTO DATA(ls_disp_rb) INDEX ls_row-index.
    gs_mass-werks = ls_disp_rb-werks.
  ENDIF.

*  CHECK lv_err IS  INITIAL.
*-----------------------------
* 발주 일괄 변경
*-----------------------------

  CALL SCREEN '0400' STARTING AT 05 05.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form POPUP_PO_HISTORY
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
*FORM popup_po_history .
*
*  CLEAR gt_rows.
*
*  CALL METHOD grf_grid->get_selected_rows
*    IMPORTING
*      et_index_rows = gt_rows.
*  DELETE gt_rows WHERE rowtype IS NOT INITIAL.
*
*  IF gt_rows IS INITIAL.
*    MESSAGE i006 DISPLAY LIKE 'E'.  "선택된 데이타가 없습니다.
*    EXIT.
*  ENDIF.
*
*  READ TABLE gt_rows INTO DATA(ls_rows) INDEX 1.
*  READ TABLE gt_disp_ra INTO DATA(ls_disp) INDEX ls_rows-index.
*
*  PERFORM select_history USING ls_disp.
*  IF gt_his_head[] IS INITIAL.
*    MESSAGE s079.
*    EXIT.
*  ENDIF.
*
*  CALL SCREEN '0500' STARTING AT 05 05.
*
*ENDFORM.
*&---------------------------------------------------------------------*
*& Form popup_comp_display
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM popup_comp_display .

  CLEAR gt_rows.

  CALL METHOD grf_grid->get_selected_rows
    IMPORTING
      et_index_rows = gt_rows.
  DELETE gt_rows WHERE rowtype IS NOT INITIAL.

  IF gt_rows IS INITIAL.
    MESSAGE i006 DISPLAY LIKE 'E'.  "선택된 데이타가 없습니다.
    EXIT.
  ENDIF.

  IF line_exists( gt_rows[ 2 ] ).
    MESSAGE i007 DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  READ TABLE gt_rows INTO DATA(ls_rows) INDEX 1.
  READ TABLE gt_disp_rb INTO DATA(ls_disp) INDEX ls_rows-index.

  PERFORM select_sub_comp USING ls_disp.
  IF gt_subcomp[] IS INITIAL.
    MESSAGE s006.
    EXIT.
  ENDIF.

  CALL SCREEN '0600' STARTING AT 05 05.
*                                  ENDING AT 20 100.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form select_sub_comp
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LS_DISP
*&---------------------------------------------------------------------*
FORM select_sub_comp USING is_disp TYPE ty_disp_rb.

  DATA : lv_matnr TYPE  makt-matnr,
         ls_makt  TYPE  makt.

  CLEAR : gt_subcomp[], gt_subcomp_p[].

  SELECT
             a~reservation AS rsnum,
             a~reservationitem AS rspos,
             a~material AS matnr,
             b~maktx AS idntx,
             a~assembly AS assem,
             a~entryunit AS erfme,
             CASE a~debitcreditcode WHEN 'S' THEN a~requiredquantity * -1
                                                                 ELSE a~requiredquantity END AS bdmng,
             a~materialcomponentisphantomitem AS phitem,"가상품목지시자
             CASE a~materialcomponentisphantomitem WHEN 'X' THEN '@K1@'END AS icon,
             a~orderpathvalue AS aufwg,
             a~orderlevelvalue AS aufst,
             a~assemblyorderpathvalue AS bauwg
     FROM ippsubcontrcomp AS a INNER JOIN makt AS b
                                                    ON a~material = b~matnr
                                                  AND b~spras = @sy-langu
     INTO CORRESPONDING FIELDS OF TABLE @gt_subcomp_p
  WHERE a~purchasingdocument = @is_disp-ebeln
      AND a~purchasingdocumentitem = @is_disp-ebelp.

  LOOP AT gt_subcomp_p ASSIGNING FIELD-SYMBOL(<ls_com>).

    CLEAR : lv_matnr.

    IF <ls_com>-bauwg IS NOT INITIAL.  "하위자재들
      <ls_com>-idnrk = <ls_com>-matnr.
      CLEAR  : <ls_com>-matnr.

    ELSE.   "상위 자재.

      CLEAR : ls_makt, <ls_com>-idntx.

      CALL FUNCTION 'MAKT_SINGLE_READ'
        EXPORTING
          matnr = <ls_com>-matnr
          spras = sy-langu
        IMPORTING
          wmakt = ls_makt.

      <ls_com>-maktx = ls_makt-maktx.

    ENDIF.

  ENDLOOP.

  gt_subcomp[] = CORRESPONDING #( gt_subcomp_p[] ).

  DELETE gt_subcomp WHERE bauwg  IS NOT INITIAL.
  SORT gt_subcomp_p BY rspos.
  SORT gt_subcomp BY rspos.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form comp_expand
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM comp_expand .

  DATA : lv_index TYPE sy-tabix.

  DELETE gt_subcomp WHERE bauwg IS NOT INITIAL.

  LOOP AT gt_subcomp INTO DATA(ls_comp) WHERE icon = gc_icon_coll.
    ls_comp-icon = gc_icon_expa.
    MODIFY gt_subcomp FROM ls_comp INDEX sy-tabix
                                   TRANSPORTING icon .
  ENDLOOP.

  IF gv_expand = abap_true.
    CLEAR : gv_expand.
  ELSE.

    LOOP AT gt_subcomp INTO ls_comp WHERE icon = gc_icon_expa.
      DATA(lv_tabix) = sy-tabix.
      CLEAR : lv_index.

      LOOP AT gt_subcomp_p INTO DATA(ls_comp_p) WHERE bauwg = ls_comp-aufwg.
        ADD 1 TO lv_index.
        DATA(lv_row) = lv_index + lv_tabix.

        INSERT ls_comp_p  INTO  gt_subcomp  INDEX lv_row.
      ENDLOOP.

      ls_comp-icon = gc_icon_coll.
      MODIFY gt_subcomp FROM ls_comp INDEX sy-tabix
                                     TRANSPORTING icon .
    ENDLOOP.

    gv_expand = abap_true.
  ENDIF.

  grf_subc_grid->refresh_grid_display( ).

ENDFORM.
*&---------------------------------------------------------------------*
*& Form alv_his_head_fcat_modify
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- CT_FCAT
*&---------------------------------------------------------------------*
FORM alv_his_head_fcat_modify CHANGING ct_fcat TYPE lvc_t_fcat.

  DEFINE _l_set_fcat.
    <ls_fcat>-col_pos    = &1.
    <ls_fcat>-key        = &2.
    <ls_fcat>-no_out     = &3.
    <ls_fcat>-coltext    = &4.
    <ls_fcat>-just       = &5.
    <ls_fcat>-tech       = &6.
    <ls_fcat>-cfieldname = &7.
    <ls_fcat>-qfieldname = &8.
    <ls_fcat>-outputlen  = &9.
  END-OF-DEFINITION.

  SORT ct_fcat BY fieldname.

*----------------------------------------
* Set Modify Field..
*----------------------------------------
  LOOP AT ct_fcat ASSIGNING FIELD-SYMBOL(<ls_fcat>).

    CASE <ls_fcat>-fieldname.
      WHEN 'ERDAT'.  "변경일
        _l_set_fcat: 01  'X'  ''  TEXT-c71  'C'  ''  ''  ''  '10'.
      WHEN 'ERZET'.  "변경시간
        _l_set_fcat: 01  'X'  ''  TEXT-c75  'C'  ''  ''  ''  '10'.
      WHEN 'EBELN'.  "구매문서
        _l_set_fcat: 02  'X'  ''  TEXT-c05  'C'  ''  ''  ''  '10'.
      WHEN 'CHANGE_REASON'.  "변경사유
        _l_set_fcat: 03  ''  ''  TEXT-c72  ''  ''  ''  ''  '15'.
      WHEN 'TITLE'.  "제목
        _l_set_fcat: 04  ''  ''  TEXT-c08  ''  ''  ''  ''  '20'.
      WHEN 'ZTERM'.  "지급조건
        _l_set_fcat: 05  ''  ''  TEXT-c13  'C'  ''  ''  ''  '08'.

      WHEN 'INCO1'.  "인도조건
        _l_set_fcat: 06  ''  ''  TEXT-c15  'C'  ''  ''  ''  '08'.
      WHEN 'DPPCT'.  "선급비율
        _l_set_fcat: 07  ''  ''  TEXT-c30  'C'  ''  ''  ''  '07'.
      WHEN 'HERKL'.  "원산지
        _l_set_fcat: 08  ''  ''  TEXT-c66  'C'  ''  ''  ''  '08'.
      WHEN 'SHIPTYPE'.  "선적구분
        _l_set_fcat: 09  ''  ''  TEXT-c64  'C'  ''  ''  ''  '08'.
      WHEN 'ZEDEDLINE'.  "선적기한
        _l_set_fcat: 10  ''  ''  TEXT-c65  'C'  ''  ''  ''  '10'.
      WHEN 'INCO2_L'.  "선적항
        _l_set_fcat: 11  ''  ''  TEXT-c61  ''  ''  ''  ''  '12'.
      WHEN 'INCO3_L'.  "도착항
        _l_set_fcat: 12  ''  ''  TEXT-c62  ''  ''  ''  ''  '12'.

      WHEN OTHERS.
        <ls_fcat>-tech = 'X'.
    ENDCASE.

    <ls_fcat>-scrtext_s = <ls_fcat>-coltext.
    <ls_fcat>-scrtext_m = <ls_fcat>-coltext.
    <ls_fcat>-scrtext_l = <ls_fcat>-coltext.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form alv_his_item_fcat_modify
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- CT_FCAT
*&---------------------------------------------------------------------*
FORM alv_his_item_fcat_modify CHANGING ct_fcat TYPE lvc_t_fcat.

  DEFINE _l_set_fcat.
    <ls_fcat>-col_pos    = &1.
    <ls_fcat>-key        = &2.
    <ls_fcat>-no_out     = &3.
    <ls_fcat>-coltext    = &4.
    <ls_fcat>-just       = &5.
    <ls_fcat>-tech       = &6.
    <ls_fcat>-cfieldname = &7.
    <ls_fcat>-qfieldname = &8.
    <ls_fcat>-outputlen  = &9.
  END-OF-DEFINITION.

  SORT ct_fcat BY fieldname.

*----------------------------------------
* Set Modify Field..
*----------------------------------------
  LOOP AT ct_fcat ASSIGNING FIELD-SYMBOL(<ls_fcat>).

    CASE <ls_fcat>-fieldname.
      WHEN 'ERDAT'.  "변경일
        _l_set_fcat: 01  'X'  ''  TEXT-c71  'C'  ''  ''  ''  '10'.
      WHEN 'ERZET'.  "변경시간
        _l_set_fcat: 01  'X'  ''  TEXT-c75  'C'  ''  ''  ''  '10'.
      WHEN 'EBELN'.  "구매문서
        _l_set_fcat: 02  'X'  ''  TEXT-c05  'C'  ''  ''  ''  '10'.
      WHEN 'EBELP'.  "품목
        _l_set_fcat: 03  'X'  ''  TEXT-d06  'C'  ''  ''  ''  '5'.
      WHEN 'MATNR'.  "자재
        _l_set_fcat: 04  ''  ''  TEXT-d08  'C'  ''  ''  ''  '12'.
      WHEN 'TXZ01'.  "내역
        _l_set_fcat: 05  ''  ''  TEXT-d09  'C'  ''  ''  ''  '20'.


      WHEN 'MWSKZ'.  "부가세코드
        _l_set_fcat: 06  ''  ''  TEXT-c10  'C'  ''  ''  ''  '6'.
      WHEN 'BWTAR'.  "평가유형
        _l_set_fcat: 07  ''  ''  TEXT-d10  'C'  ''  ''  ''  '10'.
      WHEN 'MENGE'.  "수량
        _l_set_fcat: 08  ''  ''  TEXT-d11  'R'  ''  ''  'MEINS'  '10'.
      WHEN 'MEINS'.  "단위
        _l_set_fcat: 09  ''  ''  TEXT-d12  'C'  ''  ''  ''  '4'.
      WHEN 'NETPR'.  "단가
        _l_set_fcat: 10  ''  ''  TEXT-d13  'R'  ''  'WAERS'  ''  '10'.
        <ls_fcat>-no_zero       = abap_true.

      WHEN 'WAERS'.  "통화
        _l_set_fcat: 11  ''  ''  TEXT-c12  'C'  ''  ''  ''  '4'.
      WHEN 'PEINH'.  "가격단위
        _l_set_fcat: 12  ''  ''  TEXT-d15  'C'  ''  ''  ''  '10'.
      WHEN 'BRTWR'.  "공급가액
        _l_set_fcat: 13  ''  ''  TEXT-c09  'R'  ''  'WAERS'  ''  '10'.
        <ls_fcat>-no_zero       = abap_true.
      WHEN 'ZTAX'.  "부가세
        _l_set_fcat: 14  ''  ''  TEXT-c10  'R'  ''  'WAERS'  ''  '08'.
        <ls_fcat>-no_zero       = abap_true.
      WHEN 'ZPRI'.  "인쇄교체비
        _l_set_fcat: 15  ''  ''  TEXT-d42  'R'  ''  'WAERS'  ''  '10'.
        <ls_fcat>-no_zero       = abap_true.

      WHEN 'ZCPR'.  "품목가감액
        _l_set_fcat: 16  ''  ''  TEXT-d17  'C'  ''  'WAERS'  ''  '10'.
        <ls_fcat>-no_zero       = abap_true.
      WHEN 'ZPRICE_CHANGE'.  "가격조정사유
        _l_set_fcat: 17  ''  ''  TEXT-d20  'C'  ''  ''  ''  '12'.
      WHEN 'ZPRICE_REASON'.  "조정사유상세
        _l_set_fcat: 18  ''  ''  TEXT-d21  'C'  ''  ''  ''  '12'.
      WHEN 'DLV_ADDR'.  "입고플랜트
        _l_set_fcat: 19  ''  ''  TEXT-c74  'C'  ''  ''  ''  '12'.
      WHEN 'ZQM_DEPARTMENT'.  "검수부서
        _l_set_fcat: 20  ''  ''  TEXT-c73  'C'  ''  ''  ''  '10'.

      WHEN 'ZINSPECTION'.  "사전검사
        _l_set_fcat: 21  ''  ''  TEXT-d99  'C'  ''  ''  ''  '10'.
      WHEN 'AD_NAME2'.  "수행장소
        _l_set_fcat: 22  ''  ''  TEXT-d98  'C'  ''  ''  ''  '12'.
      WHEN 'CHARG'.  "배치
        _l_set_fcat: 23  ''  ''  TEXT-d75  'C'  ''  ''  ''  '8'.
      WHEN 'EINDT'.  "납품일
        _l_set_fcat: 24  ''  ''  TEXT-d25  'C'  ''  ''  ''  '8'.
      WHEN 'WERKS'.  "플랜트
        _l_set_fcat: 25  ''  ''  TEXT-d26  'C'  ''  ''  ''  '8'.

      WHEN 'LGORT'.  "창고
        _l_set_fcat: 26  ''  ''  TEXT-d27  'C'  ''  ''  ''  '5'.
      WHEN 'LOEKZ'.  "삭제
        _l_set_fcat: 27  ''  ''  TEXT-d76  'C'  ''  ''  ''  '4'.
      WHEN 'SAKTO'.  "G/L계정번호
        _l_set_fcat: 28  ''  ''  TEXT-d35  'C'  ''  ''  ''  '10'.
      WHEN 'KOSTL'.  "코스트센터
        _l_set_fcat: 29  ''  ''  TEXT-d37  'C'  ''  ''  ''  '10'.
      WHEN 'ANLN1'.  "주요자산번호
        _l_set_fcat: 30  ''  ''  TEXT-d41  'C'  ''  ''  ''  '10'.

      WHEN 'AUFNR'.  "오더번호
        _l_set_fcat: 31  ''  ''  TEXT-d38  'C'  ''  ''  ''  '10'.
      WHEN 'WBSELEMENT'.  "WBS
        _l_set_fcat: 32  ''  ''  TEXT-d36  'C'  ''  ''  ''  '12'.
*        <ls_fcat>-convexit       = lc_abpsp.
      WHEN 'VBELN'.  "영업오더
        _l_set_fcat: 33  ''  ''  TEXT-d39  'C'  ''  ''  ''  '10'.
      WHEN 'VBELP'.  "영업품목
        _l_set_fcat: 34  ''  ''  TEXT-d40  'C'  ''  ''  ''  '8'.
      WHEN 'BANFN'.  "구매요청
        _l_set_fcat: 35  ''  ''  TEXT-d31  'C'  ''  ''  ''  '10'.

      WHEN 'BNFPO'.  "구매요청품목
        _l_set_fcat: 36  ''  ''  TEXT-d32  'C'  ''  ''  ''  '10'.
      WHEN OTHERS.
        <ls_fcat>-tech = 'X'.
    ENDCASE.

    <ls_fcat>-scrtext_s = <ls_fcat>-coltext.
    <ls_fcat>-scrtext_m = <ls_fcat>-coltext.
    <ls_fcat>-scrtext_l = <ls_fcat>-coltext.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form set_grid_sub_comp
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM set_grid_sub_comp .

  IF grf_subc_con IS INITIAL.

* Creating Docking container instance
    PERFORM create_container_sub_comp.
*--------------------------------
* Create Alv Grid
*--------------------------------
    PERFORM create_alv_grid_sub_comp.

*--------------------------------
* Dislay Grid..
*--------------------------------
    grf_subc_grid->set_grid( CHANGING ct_data = gt_subcomp ).

  ELSE.
    grf_subc_grid->refresh_grid_display( ).
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form create_container_sub_comp
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM create_container_sub_comp .

  CREATE OBJECT grf_subc_con
    EXPORTING
      container_name = 'SUBC'
    EXCEPTIONS
      OTHERS         = 1.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form create_alv_grid_sub_comp
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM create_alv_grid_sub_comp .

  DATA: ls_toolbtn TYPE zscn00004.

*--------------------------------------------------
* User Toolbar Button (Default Setting).
*--------------------------------------------------
*  ls_toolbtn-btn_rec    = 'X'.       "Recovery Row
  ls_toolbtn-btn_exld   = 'X'.       "Excel Download
*  ls_toolbtn-btn_exlu   = 'X'.       "Excel Upload
*  ls_toolbtn-mlti_lines = gv_mrow.   "Multi Row

  CREATE OBJECT grf_subc_grid
    EXPORTING
      iv_name    = 'ALV_SUBC'   "다수의 그리드일 경우 식별하기 위함..
      irf_parent = grf_subc_con
      is_toolbtn = ls_toolbtn.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form alv_sub_comp_fcat_modify
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- CT_FCAT
*&---------------------------------------------------------------------*
FORM alv_sub_comp_fcat_modify CHANGING ct_fcat TYPE lvc_t_fcat.

  DEFINE _l_set_fcat.
    <ls_fcat>-col_pos    = &1.
    <ls_fcat>-key        = &2.
    <ls_fcat>-no_out     = &3.
    <ls_fcat>-coltext    = &4.
    <ls_fcat>-just       = &5.
    <ls_fcat>-tech       = &6.
    <ls_fcat>-cfieldname = &7.
    <ls_fcat>-qfieldname = &8.
    <ls_fcat>-outputlen  = &9.
  END-OF-DEFINITION.

  SORT ct_fcat BY fieldname.

*----------------------------------------
* Set Modify Field..
*----------------------------------------
  LOOP AT ct_fcat ASSIGNING FIELD-SYMBOL(<ls_fcat>).

    CASE <ls_fcat>-fieldname.
      WHEN 'RSPOS'.
        _l_set_fcat: 01  ''  ''  ''  ''  ''  ''  ''  '3'.
        <ls_fcat>-no_out = abap_true.
      WHEN 'ICON'.  " 줄이기/늘리기
        _l_set_fcat: 02  ''  '' TEXT-f09  'C'  ''  ''  ''  '5'.
      WHEN 'MATNR'.  "자재코드
        _l_set_fcat: 03  ''  ''  TEXT-d08  'C'  ''  ''  ''  '12'.
      WHEN 'MAKTX'.
        _l_set_fcat: 04  ''  ''  TEXT-d09  ''  ''  ''  ''  '12'.
      WHEN 'IDNRK'.  "품목
        _l_set_fcat: 05  ''  ''  TEXT-d06  'C'  ''  ''  ''  '10'.
      WHEN 'IDNTX'.  "품목 내역
        _l_set_fcat: 06  ''  ''  TEXT-d09  ''  ''  ''  ''  '10'.
      WHEN 'ERFME'.  "단위
        _l_set_fcat: 07  ''  ''  TEXT-d12  'C'  ''  ''  ''  '5'.
      WHEN 'BDMNG'.  "소요량
        _l_set_fcat: 08  ''  ''  TEXT-f10  'R'  ''  ''  'ERFME'  '8'.

      WHEN OTHERS.
        <ls_fcat>-tech = 'X'.
    ENDCASE.

    <ls_fcat>-scrtext_s = <ls_fcat>-coltext.
    <ls_fcat>-scrtext_m = <ls_fcat>-coltext.
    <ls_fcat>-scrtext_l = <ls_fcat>-coltext.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form check_exit
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM check_exit.

  CASE gv_ok_code.
    WHEN 'EXIT'.
      LEAVE PROGRAM.
    WHEN OTHERS.
      LEAVE TO SCREEN 0.
  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form transfer_person
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM transfer_person.

  DATA: lt_return         TYPE TABLE OF bapiret2.
*  DATA: ls_poheaderother  TYPE zsmm_poheaderother,
*  ls_poheaderotherx TYPE zsmm_poheaderotherx.

*-----------------------------
* 발주담당자 변경
*-----------------------------
  LOOP AT gt_rows INTO DATA(ls_rows).

*    CLEAR: lt_return, ls_poheaderother, ls_poheaderotherx.
*
*    READ TABLE gt_disp_ra INTO DATA(ls_disp) INDEX ls_rows-index.

*    CASE abap_true.
*      WHEN gs_trans-order.
*        ls_poheaderother  = VALUE #( zorder_person     = gs_trans-employ_no
*                                     zorder_department = gs_trans-department ).
*
*        ls_poheaderotherx = VALUE #( zorder_person     = 'X'
*                                     zorder_department = 'X' ).
*
*      WHEN gs_trans-expen.
*        ls_poheaderother  = VALUE #( zexpen_person     = gs_trans-employ_no
*                                     zexpen_department = gs_trans-department ).
*
*        ls_poheaderotherx = VALUE #( zexpen_person     = 'X'
*                                     zexpen_department = 'X' ).
*
*      WHEN gs_trans-qm.
*        ls_poheaderother  = VALUE #( zqm_person     = gs_trans-employ_no
*                                     zqm_department = gs_trans-department ).
*
*        ls_poheaderotherx = VALUE #( zqm_person     = 'X'
*                                     zqm_department = 'X' ).
*
*    ENDCASE.

*    call function 'ZFMM_PO_CHANGE'
*      EXPORTING
*        iv_ebeln          = ls_disp-ebeln
*        is_poheaderother  = ls_poheaderother
*        is_poheaderotherx = ls_poheaderotherx
*      TABLES
*        et_return         = lt_return.

  ENDLOOP.

*-----------------------------
* Refresh
*-----------------------------
  PERFORM get_data.

  grf_grid->refresh_grid_display( ).

  LEAVE TO SCREEN 0.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form change_mass_order
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM change_mass_order.

  DATA: lt_return  TYPE TABLE OF bapiret2,
        lt_poitem  TYPE TABLE OF zsmm_poitem,
        lt_poitemx TYPE TABLE OF zsmm_poitemx.

  DATA: ls_poitem         TYPE zsmm_poitem,
        ls_poitemx        TYPE zsmm_poitemx,
        ls_poheader       TYPE  zsmm_poheader,
        ls_poheaderx      TYPE  zsmm_poheaderx.
*        ls_poheaderother  TYPE  zsmm_poheaderother,
*        ls_poheaderotherx TYPE  zsmm_poheaderotherx.

  DATA: lt_message TYPE TABLE OF bapiret2.

  DATA : lv_err.

  PERFORM req_field_check CHANGING lv_err.
  CHECK lv_err IS INITIAL.

* Confirm
  CHECK grf_grid->pop_to_msg( iv_type  = 'A'
                              iv_title = zcl_cn_alv_grid=>ac_msg_title    "저장확인
                              iv_text1 = CONV #( '변경 하시겠습니까?' )
                              iv_text2 = space ) EQ abap_true. "YES

*-----------------------------
* 발주 변경
*-----------------------------
  LOOP AT gt_rows INTO DATA(ls_rows).

    CLEAR: lt_return, lt_poitem, lt_poitemx, ls_poitem, ls_poitemx,
               ls_poheader, ls_poheaderx.
*               ls_poheaderother, ls_poheaderotherx.

*    헤더 변경 사항.
    CASE abap_true.
      WHEN p_ra.
        READ TABLE gt_disp_ra INTO DATA(ls_disp_ra) INDEX ls_rows-index.

        CASE abap_true.
          WHEN gs_mass-abs.
            ls_poheader-absgr = gs_mass-absgr.
            ls_poheaderx-absgr = 'X'.
          WHEN gs_mass-zma.
*            ls_poheaderother-zemanage2 = gs_mass-zemanage2.
*            ls_poheaderotherx-zemanage2 = 'X'.
        ENDCASE.

        call function 'ZFMM_PO_CHANGE'
          EXPORTING
            iv_ebeln          = ls_disp_ra-ebeln
            is_poheader       = ls_poheader
            is_poheaderx      = ls_poheaderx
*            is_poheaderother  = ls_poheaderother
*            is_poheaderotherx = ls_poheaderotherx
          TABLES
            et_return         = lt_return.

*      아이템 변경사항.
      WHEN p_rb.
        READ TABLE gt_disp_rb INTO DATA(ls_disp) INDEX ls_rows-index.

        ls_poitem-ebelp = ls_poitemx-ebelp = ls_disp-ebelp.

        IF gs_mass-bstae IS NOT INITIAL OR gs_mass-bstae_del = 'X'.
          ls_poitem-bstae  = gs_mass-bstae.
          ls_poitemx-bstae = 'X'.
        ENDIF.

        IF gs_mass-uebto IS NOT INITIAL OR gs_mass-uebto_del = 'X'.
          ls_poitem-uebto  = gs_mass-uebto.
          ls_poitemx-uebto = 'X'.
        ENDIF.

        IF gs_mass-lgort IS NOT INITIAL.
          ls_poitem-lgort  = gs_mass-lgort.
          ls_poitemx-lgort = 'X'.
        ENDIF.

        IF gs_mass-lifnr IS NOT INITIAL.
          ls_poitem-supp_vendor  = gs_mass-lifnr.
          ls_poitemx-supp_vendor = 'X'.
        ENDIF.

        APPEND: ls_poitem  TO lt_poitem,
                ls_poitemx TO lt_poitemx.

        call function 'ZFMM_PO_CHANGE'
          EXPORTING
            iv_ebeln   = ls_disp-ebeln
          TABLES
            et_return  = lt_return
            it_poitem  = lt_poitem
            it_poitemx = lt_poitemx.
    ENDCASE.

    LOOP AT lt_return INTO DATA(ls_return).
      IF ls_return-type = 'E'.
        APPEND ls_return TO lt_message.
      ENDIF.
    ENDLOOP.

  ENDLOOP.

  IF lt_message IS NOT INITIAL.
    PERFORM show_bapi_message TABLES lt_message.
  ENDIF.

*-----------------------------
* Refresh
*-----------------------------
  PERFORM get_data.

  grf_grid->refresh_grid_display( ).

  LEAVE TO SCREEN 0.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form req_field_check
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- LV_ERR
*&---------------------------------------------------------------------*
FORM req_field_check CHANGING cv_err.

  CASE abap_true.
    WHEN p_ra.

      IF gs_mass-absgr IS NOT INITIAL.
        SELECT SINGLE absgr_txt
            INTO @gs_mass-atext
          FROM t165m
          WHERE spras = @sy-langu
             AND absgr = @gs_mass-absgr.
        IF sy-subrc NE 0.
          cv_err = 'X'.
          MESSAGE s022(zmm01) WITH TEXT-c28 gs_mass-absgr DISPLAY LIKE 'E'.
        ENDIF.
      ENDIF.

*      IF gs_mass-zemanage2 IS NOT INITIAL.
*        SELECT SINGLE zemanage2
*            INTO @DATA(lv_zemanage2)
*          FROM ekko
*          WHERE zemanage2 = @gs_mass-zemanage2.
*        IF sy-subrc EQ 0.
*          cv_err = 'X'.
*          MESSAGE s039(zmm01)  DISPLAY LIKE 'E'.
*        ENDIF.
*      ENDIF.

      CASE abap_true.
        WHEN gs_mass-abs.
          IF gs_mass-absgr IS INITIAL.
            cv_err = 'X'.
            MESSAGE s028(zmm01) WITH TEXT-c28  DISPLAY LIKE 'E'.
            EXIT.
          ENDIF.
        WHEN gs_mass-zma.
          IF gs_mass-zemanage2 IS INITIAL.
            cv_err = 'X'.
            MESSAGE s028(zmm01) WITH TEXT-c63  DISPLAY LIKE 'E'.
            EXIT.
          ENDIF.
      ENDCASE.
    WHEN p_rb.
      IF gs_mass IS INITIAL.
        cv_err = 'X'.
        MESSAGE s021 DISPLAY LIKE 'E'.  "처리 가능한 데이타가 없습니다.
        EXIT.
      ENDIF.
  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SHOW_BAPI_MESSAGE
*&---------------------------------------------------------------------*
FORM show_bapi_message TABLES it_return STRUCTURE bapiret2.

* Message Initialize
  CALL FUNCTION 'MESSAGES_INITIALIZE'
    EXCEPTIONS
      log_not_active       = 1
      wrong_identification = 2
      OTHERS               = 3.

  LOOP AT it_return INTO DATA(ls_return).
    CALL FUNCTION 'MESSAGE_STORE'
      EXPORTING
        arbgb                  = ls_return-id
        msgty                  = ls_return-type
        msgv1                  = ls_return-message_v1
        msgv2                  = ls_return-message_v2
        msgv3                  = ls_return-message_v3
        msgv4                  = ls_return-message_v4
        txtnr                  = ls_return-number
        zeile                  = sy-tabix
      EXCEPTIONS
        message_type_not_valid = 1
        not_active             = 2.
  ENDLOOP.

* Message Popup
  CALL FUNCTION 'MESSAGES_SHOW'
    EXPORTING
      i_use_grid         = 'X'
      object             = 'Error Message'
    EXCEPTIONS
      inconsistent_range = 1
      no_messages        = 2
      OTHERS             = 3.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_F4_BSART
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> P_
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form SET_F4_BSART
*&---------------------------------------------------------------------*
FORM set_f4_bsart USING iv_scr_name.

  DATA: lt_return     TYPE TABLE OF ddshretval,
        lt_dynpfields TYPE TABLE OF dynpread.

  CONSTANTS: lc_title(15) TYPE c VALUE '구매 문서 유형',
             lc_retfield  TYPE fieldname VALUE 'BSART'.

  FIELD-SYMBOLS: <lv_scr_value> TYPE any.
  ASSIGN (iv_scr_name) TO <lv_scr_value>.

* Get Data
  SELECT bstyp,
         bsart,
         batxt
    FROM t161t
   WHERE spras = @sy-langu
     AND bstyp = 'F'        "구매오더만
    INTO TABLE @DATA(lt_t161t).

  SORT lt_t161t BY bstyp bsart.

* Search Help
  PERFORM f4if_int_table_value_request TABLES lt_t161t
                                              lt_return
                                       USING  lc_title
                                              lc_retfield
                                              iv_scr_name.

* Return
  READ TABLE lt_return INTO DATA(ls_return) INDEX 1.

  IF sy-subrc = 0.
    SORT lt_t161t BY bsart.

    READ TABLE lt_t161t WITH KEY bsart = ls_return-fieldval
                        BINARY SEARCH
                        INTO DATA(ls_t161t).

    <lv_scr_value> = ls_t161t-bsart.

    lt_dynpfields = VALUE #( ( fieldname = iv_scr_name fieldvalue = ls_t161t-bsart ) ).

    PERFORM dynp_values_update TABLES lt_dynpfields.
  ENDIF.

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
*& Form SET_F4_EKGRP
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> P_
*&---------------------------------------------------------------------*
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


* 회사코드 체크
  PERFORM dynp_values_read USING 'P_BUKRS' CHANGING p_bukrs.

  IF p_bukrs IS INITIAL.
    MESSAGE s017 WITH TEXT-f01 DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

* Get Data
  SELECT DISTINCT ekgrp, eknam
    FROM zsvcmm_org
   WHERE bukrs = @p_bukrs
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
*& Form DYNP_VALUES_READ
*&---------------------------------------------------------------------*
FORM dynp_values_read USING iv_scr_name
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
*& Form SET_F4_EPSTP
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> P_
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form SET_F4_EPSTP
*&---------------------------------------------------------------------*
FORM set_f4_epstp USING iv_scr_name.

  DATA: lt_return     TYPE TABLE OF ddshretval,
        lt_dynpfields TYPE TABLE OF dynpread.

  CONSTANTS: lc_title(15) TYPE c VALUE '품목범주',
             lc_retfield  TYPE fieldname VALUE 'EPSTP'.

  FIELD-SYMBOLS: <lv_scr_value> TYPE any.
  ASSIGN (iv_scr_name) TO <lv_scr_value>.

* Get Data
  SELECT epstp,
         ptext
    FROM t163y
   WHERE spras = @sy-langu
   ORDER BY pstyp
    INTO TABLE @DATA(lt_t163y).

* Search Help
  PERFORM f4if_int_table_value_request TABLES lt_t163y
                                              lt_return
                                       USING  lc_title
                                              lc_retfield
                                              iv_scr_name.

* Return
  READ TABLE lt_return INTO DATA(ls_return) INDEX 1.

  IF sy-subrc = 0.
    SORT lt_t163y BY epstp.

    READ TABLE lt_t163y WITH KEY epstp = ls_return-fieldval
                        BINARY SEARCH
                        INTO DATA(ls_t163y).

    <lv_scr_value> = ls_t163y-epstp.

    lt_dynpfields = VALUE #( ( fieldname = iv_scr_name fieldvalue = ls_t163y-epstp ) ).

    PERFORM dynp_values_update TABLES lt_dynpfields.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_LIST_BOX
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
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

  DATA: lv_name TYPE vrm_id,
        lt_list TYPE vrm_values.     "Key, Text

* 회사코드
  lv_name = iv_fname.

  zcl_mm_common=>common_config(
    EXPORTING
      is_common = VALUE #( m = 'A1' d = 'A1000' s = 'AA100' )
      it_where  = VALUE #( ( field = 1 value = 'BUKRS' ) )
    IMPORTING
      et_outtab = DATA(lt_config) ).

  lt_list = CORRESPONDING #( lt_config MAPPING key = field2  text = field3  ).

  _g_set_values: lv_name lt_list.

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
*& Form SET_SEL_SCREEN
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form SET_SEL_SCREEN
*&---------------------------------------------------------------------*
FORM set_sel_screen.

  CHECK sy-dynnr = gc_dynnr_1000.

  LOOP AT SCREEN.

    CASE screen-group1.
      WHEN 'EXC'.
*        SCREEN-REQUIRED = '2'.

        IF gv_exc_user IS INITIAL.
          screen-input = 0.
        ELSE.
          screen-input = 1.
        ENDIF.

      WHEN 'MNG'. "관리번호
        IF p_bukrs = gc_bukrs_1101 AND p_kalsk = gc_kalsk_im.
          screen-active = 1.
        ELSE.
          screen-active = 0.
        ENDIF.

      WHEN OTHERS.
    ENDCASE.

    "[U1 변경시작 2022.08.24].
    IF screen-group1 = 'MAL'.
      IF gv_mail = abap_true.
        screen-active = 1.
      ELSE.
        screen-active = 0.
      ENDIF.
    ENDIF.
    "[U1 변경종료 2022.08.24].

    MODIFY SCREEN.
  ENDLOOP.

ENDFORM.
