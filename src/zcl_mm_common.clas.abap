class ZCL_MM_COMMON definition
  public
  final
  create public .

public section.

  types:
    BEGIN OF  ts_ztmm00002,
        field1  TYPE  field_name,
        field2  TYPE  field_name,
        field3  TYPE  field_name,
        field4  TYPE  field_name,
        field5  TYPE  field_name,
        field6  TYPE  field_name,
        field7  TYPE  field_name,
        field8  TYPE  field_name,
        field9  TYPE  field_name,
        field10 TYPE  field_name,
        field11 TYPE  field_name,
        field12 TYPE  field_name,
        field13 TYPE  field_name,
      END OF ts_ztmm00002 .
  types:
    tt_ztmm00002 TYPE TABLE OF ts_ztmm00002 .
  types:
    BEGIN OF ts_config,
        m TYPE ztmm00001-zmain_cat,
        d TYPE ztmm00001-zmidd_cat,
        s TYPE ztmm00001-zsmal_cat,
      END OF ts_config .
  types:
    BEGIN OF ts_table,
        table TYPE reftable,
        field TYPE field_name,
      END OF ts_table .
  types:
    BEGIN OF ts_field,
        field  TYPE field_name,
        value  TYPE char40,
        option TYPE char2,
      END OF ts_field .
  types:
    tt_field TYPE TABLE OF ts_field .
  types:
    BEGIN OF ts_field_ex,
        field TYPE field_name,
        exp   TYPE char2,
        value TYPE char40,
      END OF ts_field_ex .
  types:
    tt_field_ex TYPE TABLE OF ts_field_ex .
  types:
    ty_rsel_info TYPE TABLE OF  rsel_info .

  methods CHECK_AUTHORITY
    importing
      value(IV_MODE) type CHAR02
      value(IV_USER) type USR02-BNAME
      value(IV_ACTVT) type CHAR02 optional
      value(IS_INPUT) type ZSMM_COMM_AUTH
    exporting
      value(EV_RET_TXT) type CHAR50
    returning
      value(RV_SUBRC) type SY-SUBRC .
  class-methods GET_SEARCH_HELP
    importing
      !IV_FNAME type FIELDNAME
      value(IV_FILT_VAL_A) type CHAR20 optional
      value(IV_FILT_VAL_B) type CHAR20 optional
      value(IV_FILT_VAL_C) type CHAR20 optional
    exporting
      !EV_VALUE type ANY .
  class-methods GET_DYNP_PARAM
    importing
      !IV_FNAME type FIELDNAME optional
      !IV_DYNNR type SY-DYNNR optional
    returning
      value(RT_PARAMS) type RSPARAMS_TT .
  methods GET_MATNR
    importing
      value(IV_MODE) type CHAR02 optional
      value(IV_WERKS) type WERKS_D optional
      value(IV_MATNR) type MATNR
    exporting
      value(ET_DATA) type ZYMM_COMM_MATNR
    returning
      value(RV_SUBRC) type SY-SUBRC .
  methods GET_LGORT
    importing
      value(IV_MODE) type CHAR02 optional
      value(IV_WERKS) type WERKS_D optional
      value(IV_LGORT) type LGORT_D
    exporting
      value(ET_DATA) type ZYMM_COMM_LGORT
    returning
      value(RV_SUBRC) type SY-SUBRC .
  class-methods DATA_EXIST_CHECK
    importing
      value(IV_MODE) type CHAR02 optional
      value(IS_TABLE) type TS_TABLE
      value(IT_WHERE) type TT_FIELD
    exporting
      value(EV_SUBRC) type SY-SUBRC
      value(EV_RETURN) type ANY .
  class-methods COMMON_CONFIG
    importing
      value(IV_MODE) type CHAR02 optional
      value(IS_COMMON) type TS_CONFIG
      value(IT_WHERE) type TT_FIELD optional
    exporting
      value(ET_OUTTAB) type TT_ZTMM00002 .
  class-methods SHOW_BAPI_MESSAGE
    importing
      !IT_RETURN type BAPIRET2_T optional
      !IT_RETURN2 type MEWI_T_RETURN optional
    returning
      value(RS_RETURN) type BAL_S_EXCM .
  methods CONV_DATA_SAP_TO_EAI
    importing
      !IV_VALUE type ANY
      !IV_UNIT type ANY optional
    exporting
      value(EV_VALUE) type ANY .
  methods CONV_STRUCTURE_SAP_TO_EAI
    importing
      !IS_STRUCTURE type ANY
    exporting
      value(ES_STRUCTURE) type ANY .
  class-methods CONV_DATA_EAI_TO_SAP
    importing
      !IV_VALUE type ANY
      !IV_UNIT type ANY optional
    exporting
      value(EV_VALUE) type ANY .
  methods CONV_STRUCTURE_EAI_TO_SAP
    importing
      !IS_STRUCTURE type ANY
    exporting
      value(ES_STRUCTURE) type ANY .
  methods BACK_JOB_PROCESS
    importing
      !IV_CPROG type CPROG default SY-CPROG
      !IT_RSPARAMS type TY_RSPARAMS optional
    changing
      value(CV_JOBNAME) type BTCJOB optional
    returning
      value(RV_SUBRC) type SY-SUBRC .
  methods GET_HTML_SYTLE
    importing
      !IV_STYLE_TYPE type CHAR01 default 'A'
      !IV_SUBJECT type STRING default '결재상신'
    returning
      value(RV_STRING) type STRING .
  methods SET_VALUE_UPDATE
    importing
      !IV_FNAME type ANY
      !IV_VALUE type ANY
      !IV_DYNNR type SYST_DYNNR .
  class-methods CALL_TRANSACTION_BP
    importing
      !IV_LIFNR type LIFNR .
  methods CHECK_AUTH_LGORT
    importing
      value(IV_BUKRS) type BUKRS
      value(IV_WERKS) type WERKS_D
      value(IT_LGORT) type RANGES_LGORT_TT
    returning
      value(RV_SUBRC) type SY-SUBRC .
*    exporting
*      value(ES_EACC_DATA) type ZDTV3T_AP_HEAD
  methods GET_EACC_INV_SEQ
    importing
      value(IV_BUKRS) type BUKRS
      value(IV_BUPLA) type BUPLA optional
      value(IV_STCD2) type STCD2 optional
      value(IV_BUDAT) type BUDAT optional
      value(IV_MWSKZ) type MWSKZ optional
      value(IV_DMBTR) type DMBTR optional
    returning
      value(RV_SUBRC) type SY-SUBRC .
  class-methods GET_PGM_PARAMETER
    importing
      !IV_REPID type REPID default SY-CPROG
    exporting
      !ET_SELTAB type TY_RSPARAMS
      !ET_SELINFO type TY_RSPARAMS
      !ET_SELTEXT type FC00_T_SEL_SCREEN_TEXTPOOL
    returning
      value(RV_SUBRC) type SUBRC .
protected section.
private section.
ENDCLASS.



CLASS ZCL_MM_COMMON IMPLEMENTATION.


  METHOD back_job_process.
    DATA : lt_rsparams TYPE TABLE OF rsparams,
           ls_rsparams TYPE rsparams,
           lv_jobcount TYPE btcjobcnt.

    TRY.

        IF it_rsparams[] IS INITIAL.
          rv_subrc = zcl_mm_common=>get_pgm_parameter( EXPORTING iv_repid  = iv_cprog
                                                     IMPORTING et_seltab = lt_rsparams ).
        ELSE.
          lt_rsparams[] = it_rsparams[].
        ENDIF.

        IF lt_rsparams[] IS INITIAL.
          rv_subrc = 12.
          EXIT.
        ENDIF.

        IF cv_jobname IS INITIAL.
          CONCATENATE 'JOB_' iv_cprog '_' sy-datum sy-uzeit INTO cv_jobname.
        ENDIF.


        DATA lc_start TYPE sy-uzeit.

        lc_start = sy-uzeit + 5.


        CALL FUNCTION 'JOB_OPEN'
          EXPORTING
            jobname          = cv_jobname
            sdlstrtdt        = sy-datum
            sdlstrttm        = lc_start    "sy-uzeit
          IMPORTING
            jobcount         = lv_jobcount
          EXCEPTIONS
            cant_create_job  = 1
            invalid_job_data = 2
            jobname_missing  = 3
            OTHERS           = 4.

        IF sy-subrc IS NOT INITIAL.
          rv_subrc = sy-subrc. EXIT.
        ENDIF.

        DATA(ls_opt) = VALUE ctu_params( racommit = abap_on ).

        SUBMIT (iv_cprog) WITH SELECTION-TABLE lt_rsparams
                    USER sy-uname
                   VIA JOB cv_jobname
                     NUMBER lv_jobcount
                    AND RETURN.

        IF sy-subrc IS INITIAL.
          CALL FUNCTION 'JOB_CLOSE'
            EXPORTING
              jobcount             = lv_jobcount
              jobname              = cv_jobname
              strtimmed            = abap_true
            EXCEPTIONS
              cant_start_immediate = 1
              invalid_startdate    = 2
              jobname_missing      = 3
              job_close_failed     = 4
              job_nosteps          = 5
              job_notex            = 6
              lock_failed          = 7
              invalid_target       = 8
              invalid_time_zone    = 9
              OTHERS               = 10.
          IF sy-subrc IS NOT INITIAL.
            rv_subrc = sy-subrc.
            IF sy-batch IS NOT INITIAL.

            ENDIF.
            EXIT.
          ENDIF.

        ENDIF.

      CATCH cx_root INTO DATA(lrf_root).
        rv_subrc = 11.
    ENDTRY.
  ENDMETHOD.


  METHOD call_transaction_bp.

*&--------------------------------------------------------------------*
*& Module              : MM
*& Program ID          : ZCL_MM_COMMON => CALL_TRANSACTION_BP
*& T-CODE              :
*& Referenced Program  : N/A
*& Created by          : T0210053
*& Created On          : 2021.07.28
*& Type                : Common method
*& Description         : BP 공급업체 VIEW 화면 이동 (더블클릭/HOTSPOT 등 활용)
*&--------------------------------------------------------------------*
*& Change History
*&--------------------------------------------------------------------*
*& 변경SEQ    변경자        변경일자             변경내용
*&--------------------------------------------------------------------*
*&    N       T0210053      2021.07.28          최초생성
*&--------------------------------------------------------------------*

    DATA: lv_request  TYPE REF TO cl_bupa_navigation_request,
          lv_options  TYPE REF TO cl_bupa_dialog_joel_options,
          lv_bp_guid  TYPE bu_partner_guid,
          ls_bp_role  TYPE bus_roles,
          lv_fole(10) VALUE 'FLVN01'.

    CHECK iv_lifnr NE space.

    ls_bp_role-role = lv_fole.  "공급업체

*     Get BP number linked to the selected vendor
    DATA(lo_bp_vendor) = cvi_ka_bp_vendor=>get_instance( ).
    lv_bp_guid = lo_bp_vendor->get_assigned_bp_for_vendor( iv_lifnr ).

*     Create a request/option.
    CREATE OBJECT lv_request.
    CREATE OBJECT lv_options.

*     Fill the request fields.
    CALL METHOD lv_request->set_maintenance_id( lv_request->gc_maintenance_id_partner ).
    CALL METHOD lv_request->set_partner_guid( lv_bp_guid ).
    CALL METHOD lv_request->set_bupa_partner_role( ls_bp_role ).
    CALL METHOD lv_request->set_bupa_activity( lv_request->gc_activity_display ).

*     Fill option - Navigation disable..
    CALL METHOD lv_options->set_navigation_disabled( 'X' ).
    CALL METHOD lv_options->set_activity_switching_off( 'X' ).
    CALL METHOD lv_options->set_navigate_on_first_tab( 'X' ).

*     Start the maintenance.
    CALL METHOD cl_bupa_dialog_joel=>start_with_navigation
      EXPORTING
        iv_request = lv_request
        iv_options = lv_options
      EXCEPTIONS
        OTHERS     = 1.
  ENDMETHOD.


  METHOD check_authority.
*&--------------------------------------------------------------------*
*& Module              : MM
*& Program ID          : ZCL_MM_COMMON => CHECK_AUTHORITY
*& T-CODE              :
*& Referenced Program  : N/A
*& Created by          : T0210052
*& Created On          : 2021.03.05
*& Type                : Common method
*& Description         : Authority Check
*& 참고사항
*& --> 1. 기능추가시 CASE 문에 WHEN 을 통해 추가 할것.
*& --> 2. 다른것은 건들지 말고  WHEN 구문옆에 생성자 이력 남겨둘것
*&--------------------------------------------------------------------*
*& Change History
*&--------------------------------------------------------------------*
*& 변경SEQ    변경자        변경일자             변경내용
*&--------------------------------------------------------------------*
*&    N       T0210052      2021.03.05          최초생성
*&--------------------------------------------------------------------*
***--------------------------------------------------------------------***
*** P1 : Plant 일반
***--------------------------------------------------------------------***

*-
    CLEAR: ev_ret_txt.
*-
    CASE iv_mode.
***--------------------------------------------------------------------***
*** Plant 일반  [ Create : T0210052 ]
***--------------------------------------------------------------------***
      WHEN 'P1'. "Plant 일반  TDB
        CONSTANTS: lc_object    TYPE ust12-objct VALUE 'M_BSET_WRK',
                   lc_err_p1_01 TYPE char50 VALUE '플랜트'.

        DATA : lr_werks TYPE peg_t_werks,
               lt_werks TYPE TABLE OF t001w-werks.

        APPEND VALUE #( sign   = 'I'
                        option = 'EQ'
                        low    = is_input-werks
                        high   = space )
                  TO lr_werks.

        SELECT werks INTO TABLE @lt_werks
                     FROM t001w
                    WHERE werks IN @lr_werks.

        rv_subrc = sy-subrc.

        IF sy-subrc <> 0.
          "[ &1 ] &2 를 확인하세요!
          MESSAGE s040(zmm01) WITH lc_err_p1_01 is_input-werks INTO ev_ret_txt.
          EXIT.
        ENDIF.

        LOOP AT lt_werks INTO DATA(ls_plant).
          IF iv_actvt IS INITIAL.
            AUTHORITY-CHECK OBJECT lc_object FOR USER iv_user
                                ID 'ACTVT' DUMMY
                                ID 'WERKS' FIELD ls_plant.
          ELSE.
            AUTHORITY-CHECK OBJECT lc_object FOR USER iv_user
                                ID 'ACTVT' FIELD iv_actvt
                                ID 'WERKS' FIELD ls_plant.
          ENDIF.

          rv_subrc = sy-subrc.
          IF rv_subrc <> 0.
            "[ &1 ] 권한이 없습니다.
            MESSAGE s041(zmm01) WITH lc_err_p1_01 INTO ev_ret_txt.
            EXIT.
          ENDIF.

        ENDLOOP.

***--------------------------------------------------------------------***
***
***--------------------------------------------------------------------***
    ENDCASE.

  ENDMETHOD.


  METHOD check_auth_lgort.

*   예외 ID 체크.
    zcl_mm_common=>common_config(
      EXPORTING
        is_common = VALUE #( m = 'D1' d = 'D9000' s = 'D9006' )
        it_where  = VALUE #( ( field = 1 value = sy-tcode )
                                        ( field = 2 value = sy-uname ) )
      IMPORTING
        et_outtab = DATA(lt_config) ).
    IF lt_config[] IS NOT INITIAL.
      rv_subrc = 0.
      EXIT.
    ENDIF.

*  권한체크할 대상 취합
    SELECT lgort FROM t001l INTO TABLE @DATA(lt_lgort)
     WHERE werks = @iv_werks
        AND lgort IN @it_lgort.
    IF sy-subrc NE 0.
      MESSAGE s000(zmm01) WITH TEXT-m01 DISPLAY LIKE 'E'.
      rv_subrc = sy-subrc.
      EXIT.
    ENDIF.

*   부여된 권한 정보 취합.
*    SELECT lgort INTO TABLE @DATA(lt_auth) FROM zcbmmslocauth
*      WHERE bukrs = @iv_bukrs
*        AND werks = @iv_werks
*       AND user_id = @sy-uname.
*    SORT lt_auth BY lgort.

*    LOOP AT lt_lgort INTO DATA(ls_lgort).
*      READ TABLE lt_auth WITH KEY lgort = ls_lgort-lgort BINARY SEARCH
*      TRANSPORTING NO FIELDS.
*      IF sy-subrc NE 0.
*        MESSAGE s001(zmm01) WITH ls_lgort-lgort TEXT-m02 DISPLAY LIKE 'E'.
*        rv_subrc = sy-subrc.
*        EXIT.
*      ENDIF.
*    ENDLOOP.

  ENDMETHOD.


  METHOD common_config.
*&--------------------------------------------------------------------*
*& Module              : MM
*& Program ID          : ZCL_MM_COMMON => COMMON_CONFIG
*& T-CODE              :
*& Referenced Program  : N/A
*& Created by          : T0210054
*& Created On          : 2021.02.08
*& Type                : Common method
*& Description         : MM 공통 config 조회
*& 참고사항
*& --> 1. MM 공통 config 조회
*&--------------------------------------------------------------------*
*& Change History
*&--------------------------------------------------------------------*
*& 변경SEQ    변경자        변경일자             변경내용
*&--------------------------------------------------------------------*
*&    N       T0210054      2021.02.08          최초생성
*&--------------------------------------------------------------------*

*    is_common 은 필수 - 공통 config(zomm0010)의 왼쪽 트리화면의 내용 ( M- 대분류, D- 중분류, S- 소분류  )
*    이외에 추가 field에 대한 조건문 추가는 it_where 로 처리 ( 테이블 형태로 입력 .  field = ( 필드의 순번) . value = (값) )
*       field의 순번 값과 value 값을 정확하게 입력하지않으면 조회 불가.

** 참조예시.
**  zcl_mm_common=>common_config(
**                                EXPORTING  is_common =  VALUE #( m = 'A1' d = 'A1010' s = 'AB100' )
**                                                   it_where = VALUE #( ( field = 1 value = 'ZOMM3010' )
**                                                                                  ( field = 3 value = 'T0200278' ) )
**                                 IMPORTING et_outtab = DATA(lt_config) ).


    DATA: lv_where(500).

    IF is_common-m IS INITIAL OR is_common-d IS INITIAL OR  is_common-s IS INITIAL.
      EXIT.
    ENDIF.

*   추가 조건 설정
    LOOP AT it_where INTO DATA(ls_where).
      CONDENSE ls_where-field.
      IF sy-tabix = 1.
        CONCATENATE 'FIELD' ls_where-field ` EQ `   '''' ls_where-value ''''
              INTO lv_where.
      ELSE.
        CONCATENATE  lv_where ` AND ` 'FIELD' ls_where-field ` EQ `    '''' ls_where-value ''''
                        INTO lv_where .
      ENDIF.
    ENDLOOP.

*    조회
*       추가 조건 설정 추가.
    TRY.
        SELECT  field1,  field2,  field3,  field4,  field5,  field6,
                     field7,  field8,  field9, field10, field11, field12, field13
          FROM ztmm00002
          INTO CORRESPONDING FIELDS OF TABLE @et_outtab
         WHERE zmain_cat = @is_common-m
            AND zmidd_cat = @is_common-d
            AND zsmal_cat = @is_common-s
            AND (lv_where).

      CATCH cx_sy_dynamic_osql_error.
    ENDTRY.

  ENDMETHOD.


  METHOD conv_data_eai_to_sap.
*&--------------------------------------------------------------------*
*& Module              : MM
*& Program ID          : ZCL_MM_COMMON => CONV_DATA_EAI_TO_SAP
*& T-CODE              :
*& Referenced Program  : N/A
*& Created by          : T0210053
*& Created On          : 2021.02.25
*& Type                : Common method
*& Description         : EAI 데이터 TYPE 을 SAP TYPE 으로 수신받기 위해 변환.
*&                       (값 단위로 변환)
*&--------------------------------------------------------------------*
*& Change History
*&--------------------------------------------------------------------*
*& 변경SEQ    변경자        변경일자             변경내용
*&--------------------------------------------------------------------*
*&    N       T0210053      2021.02.25          최초생성
*&--------------------------------------------------------------------*

    DATA: lrf_descr_ref  TYPE REF TO cl_abap_typedescr,
          lv_dataelement TYPE dd04l-rollname,
          lv_alpha_func  TYPE rs38l_fnam,
          lv_fun_ref     TYPE REF TO cx_root,
          lv_move_ref    TYPE REF TO cx_root,
          lv_msg         TYPE string.

    DATA: lv_datatype TYPE dd04l-datatype,
          lv_convexit TYPE dd04l-convexit.

    DATA: lv_ref_val    TYPE REF TO data.
    FIELD-SYMBOLS <lv_alpha_val> TYPE any.

    CONSTANTS: lc_type_curr TYPE dd04l-datatype VALUE 'CURR',
               lc_waers(5)  TYPE c VALUE '통화'.

    lrf_descr_ref = cl_abap_typedescr=>describe_by_data( ev_value ).

    DATA(lv_type) = lrf_descr_ref->type_kind.
    DATA(lv_absolute_name) = lrf_descr_ref->absolute_name.

    lv_dataelement = lv_absolute_name+6(*).

    "데이터 유형 및 ALPHA CONV 의 우선순위는 (DOMAIN -> DATA ELEMENT 임)
    SELECT SINGLE domname, rollname, datatype, convexit
      FROM dd04l
     WHERE rollname = @lv_dataelement
       AND as4local = 'A'
      INTO @DATA(ls_dd04l).
    IF sy-subrc EQ 0.
      SELECT SINGLE domname, datatype, convexit
        FROM dd01l
       WHERE domname = @ls_dd04l-domname
         AND as4local = 'A'
        INTO @DATA(ls_dd01l).

      IF sy-subrc EQ 0.
        lv_datatype = ls_dd01l-datatype.
        lv_convexit = ls_dd01l-convexit.
      ELSE.
        lv_datatype = ls_dd04l-datatype.
        lv_convexit = ls_dd04l-convexit.
      ENDIF.
    ENDIF.

    CASE lv_type.
      WHEN 'P'.  "숫자 타입
        DATA: lv_p_string TYPE string.
        CLEAR lv_p_string.
        lv_p_string = iv_value.
        REPLACE ALL OCCURRENCES OF ',' IN lv_p_string WITH space.
        CONDENSE lv_p_string NO-GAPS.

        "통화만 변환함. (수량은 소수점 2자리 형태라 그대로 전송)
        IF lv_datatype = lc_type_curr.
          IF iv_unit IS INITIAL.
            MESSAGE s005(zmm01) WITH lc_waers.
            EXIT.
          ENDIF.

          CALL FUNCTION 'CURRENCY_AMOUNT_IDOC_TO_SAP'
            EXPORTING
              currency    = CONV tcurc-waers( iv_unit )
              idoc_amount = lv_p_string
            IMPORTING
              sap_amount  = ev_value.
        ELSE.
          TRY.
              ev_value = lv_p_string.
            CATCH cx_sy_conversion_overflow INTO lv_move_ref.
              lv_msg = lv_move_ref->get_text( ).
              MESSAGE s000(zmm01) WITH lv_msg.
          ENDTRY.
        ENDIF.

      WHEN 'D'. "DATE 값이 없으면 공백으로 RETURN
        DATA: lv_del_special_in  TYPE fist-searchw,
              lv_del_special_out TYPE fist-searchw.

        CLEAR: lv_del_special_in, lv_del_special_out.

        lv_del_special_in = iv_value.

        IF NOT lv_del_special_in IS INITIAL.
          "특수문자 제거하여 숫자로만 구성함.
          CALL FUNCTION 'SF_SPECIALCHAR_DELETE'
            EXPORTING
              with_specialchar    = lv_del_special_in
            IMPORTING
              without_specialchar = lv_del_special_out.

          TRY.
              ev_value = lv_del_special_out.
            CATCH cx_sy_conversion_overflow INTO lv_move_ref.
              lv_msg = lv_move_ref->get_text( ).
              MESSAGE s000(zmm01) WITH lv_msg.
          ENDTRY.

        ENDIF.

      WHEN OTHERS.  "기타

        "전송 필드의 DATA ELEMENT 속성으로 변환
        IF NOT ls_dd04l-rollname IS INITIAL.
          CREATE DATA lv_ref_val TYPE (ls_dd04l-rollname).

          ASSIGN lv_ref_val->* TO <lv_alpha_val>.

          "ALPHA CONV 필요 하면 적용
          IF NOT lv_convexit IS INITIAL.
            CONCATENATE 'CONVERSION_EXIT_' lv_convexit '_INPUT'
            INTO lv_alpha_func.

            TRY.
                CALL FUNCTION lv_alpha_func
                  EXPORTING
                    input  = iv_value
                  IMPORTING
                    output = ev_value.
                "변환 Function 없으면 그대로 전송.
              CATCH cx_root INTO lv_fun_ref.
                lv_msg = lv_fun_ref->get_text( ).
                MESSAGE s000(zmm01) WITH lv_msg.
            ENDTRY.
          ELSE.
            ev_value = iv_value.
          ENDIF.
          "전송 필드의 DATA ELEMENT 가 지정되지 않으면 단순 MOVE
        ELSE.
          TRY.
              ev_value = iv_value.
            CATCH cx_sy_conversion_overflow INTO lv_move_ref.
              lv_msg = lv_move_ref->get_text( ).
              MESSAGE s000(zmm01) WITH lv_msg.
          ENDTRY.
        ENDIF.
    ENDCASE.

  ENDMETHOD.


  METHOD conv_data_sap_to_eai.
*&--------------------------------------------------------------------*
*& Module              : MM
*& Program ID          : ZCL_MM_COMMON => CONV_DATA_SAP_TO_EAI
*& T-CODE              :
*& Referenced Program  : N/A
*& Created by          : T0210053
*& Created On          : 2021.02.25
*& Type                : Common method
*& Description         : SAP 데이터 TYPE 을 EAI 전송하기 위해 CHAR 로 변환.
*&                      (값 단위로 변환)
*&--------------------------------------------------------------------*
*& Change History
*&--------------------------------------------------------------------*
*& 변경SEQ    변경자        변경일자             변경내용
*&--------------------------------------------------------------------*
*&    N       T0210053      2021.02.25          최초생성
*&--------------------------------------------------------------------*
*> CDS 참조 시 DATA ELEMENT 지정 안되면 별도로 CONV 해야 함.
*> 아래의 로직 사용 불가.

    DATA: lrf_descr_ref  TYPE REF TO cl_abap_typedescr,
          lv_dataelement TYPE dd04l-rollname,
          lv_alpha_func  TYPE rs38l_fnam,
          lv_fun_ref     TYPE REF TO cx_root,
          lv_msg         TYPE string.

    CONSTANTS: lc_type_curr TYPE dd04l-datatype VALUE 'CURR',
               lc_waers(5)  TYPE c VALUE '통화'.

    lrf_descr_ref = cl_abap_typedescr=>describe_by_data( iv_value ).

    DATA(lv_type) = lrf_descr_ref->type_kind.
    DATA(lv_absolute_name) = lrf_descr_ref->absolute_name.

    lv_dataelement = lv_absolute_name+6(*).

    SELECT SINGLE rollname, datatype, convexit
      FROM dd04l
     WHERE rollname = @lv_dataelement
       AND as4local = 'A'
      INTO @DATA(ls_dd04l).

    CASE lv_type.
      WHEN 'P'.  "숫자 타입

        "통화만 변환함. (수량은 소수점 2자리 형태라 그대로 전송)
        IF ls_dd04l-datatype = lc_type_curr.
          IF iv_unit IS INITIAL.
            MESSAGE s005(zmm01) WITH lc_waers.
            EXIT.
          ENDIF.

          CALL FUNCTION 'CURRENCY_AMOUNT_SAP_TO_IDOC'
            EXPORTING
              currency    = CONV tcurc-waers( iv_unit )
              sap_amount  = iv_value
            IMPORTING
              idoc_amount = ev_value.
        ELSE.
          ev_value = iv_value.
        ENDIF.

        CONDENSE ev_value.

        "minus 금액 (앞으로 표시)
        CALL FUNCTION 'CLOI_PUT_SIGN_IN_FRONT'
          CHANGING
            value = ev_value.

      WHEN 'D'. "DATE 값이 없으면 공백으로 RETURN
        IF iv_value IS INITIAL.
          CLEAR ev_value.
        ELSE.
          ev_value = iv_value.
        ENDIF.

      WHEN OTHERS.  "기타

        "ALPHA CONV 필요 하면 적용
        IF NOT ls_dd04l-convexit IS INITIAL.
          CONCATENATE 'CONVERSION_EXIT_' ls_dd04l-convexit '_OUTPUT'
          INTO lv_alpha_func.

          TRY.
              CALL FUNCTION lv_alpha_func
                EXPORTING
                  input  = iv_value
                IMPORTING
                  output = ev_value.
              "변환 Function 없으면 그대로 전송.
            CATCH cx_root INTO lv_fun_ref.
              lv_msg = lv_fun_ref->get_text( ).
              MESSAGE s000(zmm01) WITH lv_msg.
          ENDTRY.
        ELSE.
          ev_value = iv_value.
        ENDIF.

    ENDCASE.
  ENDMETHOD.


  METHOD conv_structure_eai_to_sap.
*&--------------------------------------------------------------------*
*& Module              : MM
*& Program ID          : ZCL_MM_COMMON => CONV_DATA_EAI_TO_SAP
*& T-CODE              :
*& Referenced Program  : N/A
*& Created by          : T0210053
*& Created On          : 2021.02.25
*& Type                : Common method
*& Description         : EAI 데이터 TYPE 을 SAP TYPE으로 수신받기 위해 변환.
*&                       (구조체 단위로 변환)
*&--------------------------------------------------------------------*
*& Change History
*&--------------------------------------------------------------------*
*& 변경SEQ    변경자        변경일자             변경내용
*&--------------------------------------------------------------------*
*&    N       T0210053      2021.02.25          최초생성
*&--------------------------------------------------------------------*

    DATA: lv_in_field  TYPE char100,
          lv_out_field TYPE char100.

    FIELD-SYMBOLS: <lv_in_value>  TYPE any,
                   <lv_out_value> TYPE any.

    DATA: BEGIN OF ls_tmp,
            rollname TYPE dd04l-rollname,
          END OF ls_tmp,
          lt_tmp LIKE TABLE OF ls_tmp.


    DATA: lrf_descr_ref  TYPE REF TO cl_abap_typedescr,
          lv_dataelement TYPE dd04l-rollname,
          lv_alpha_func  TYPE rs38l_fnam,
          lv_fun_ref     TYPE REF TO cx_root,
          lv_move_ref    TYPE REF TO cx_root,
          lv_msg         TYPE string.

    DATA: lv_ref_val    TYPE REF TO data.

    CONSTANTS: lc_in_str_name(20)  TYPE c VALUE 'IS_STRUCTURE-',
               lc_out_str_name(20) TYPE c VALUE 'ES_STRUCTURE-'.

    DATA: lo_struct TYPE REF TO cl_abap_structdescr,
          lt_comp   TYPE abap_component_tab,
          ls_comp   TYPE abap_componentdescr.

*> 수신된 구조체의 필드들을 ITAB 으로 전환
    lo_struct ?= cl_abap_typedescr=>describe_by_data( es_structure ).
    lt_comp = lo_struct->get_components( ).

*> GET DATA ELEMENT
    LOOP AT lt_comp INTO ls_comp.
      lrf_descr_ref = ls_comp-type.

      DATA(lv_absolute_name) = lrf_descr_ref->absolute_name.

      ls_tmp-rollname = lv_absolute_name+6(*).
      APPEND ls_tmp TO lt_tmp.
    ENDLOOP.

    SORT lt_tmp BY rollname.
    DELETE ADJACENT DUPLICATES FROM lt_tmp COMPARING rollname.
    IF NOT lt_tmp[] IS INITIAL.
      SELECT rollname, datatype, convexit
        FROM dd04l
        FOR ALL ENTRIES IN @lt_tmp
       WHERE rollname = @lt_tmp-rollname
        INTO TABLE @DATA(lt_dd04l).
      SORT lt_dd04l BY rollname.
    ENDIF.

*> 전체 필드에 대해 순차적으로...
    LOOP AT lt_comp INTO ls_comp.
      CLEAR: lrf_descr_ref, ls_tmp, lv_in_field, lv_out_field.

*> GET 필드 속성
      lrf_descr_ref = ls_comp-type.

      lv_absolute_name = lrf_descr_ref->absolute_name.
      DATA(lv_type) = lrf_descr_ref->type_kind.

      ls_tmp-rollname = lv_absolute_name+6(*).

*> GET 정확한 속성(P 안에서도 DEC, CURR 이 나뉘는 것처럼...)
      READ TABLE lt_dd04l INTO DATA(ls_dd04l)
                          WITH KEY rollname = ls_tmp-rollname
                          BINARY SEARCH.

*> SET VALUE
      CONCATENATE lc_out_str_name ls_comp-name INTO lv_out_field.
      ASSIGN (lv_out_field) TO <lv_out_value>.

      CONCATENATE lc_in_str_name ls_comp-name INTO lv_in_field.
      ASSIGN (lv_in_field) TO <lv_in_value>.
      IF sy-subrc NE 0.
        CONTINUE.
      ENDIF.


*> 속성 별 데이터 변환 시작
      CASE lv_type.
        WHEN 'P'.  "숫자 타입
          DATA: lv_p_string TYPE string.
          CLEAR lv_p_string.
          lv_p_string = <lv_in_value>.
          REPLACE ALL OCCURRENCES OF ',' IN lv_p_string WITH space.
          CONDENSE lv_p_string NO-GAPS.

          TRY.
              <lv_out_value> = lv_p_string.
            CATCH cx_sy_conversion_overflow INTO lv_move_ref.
              lv_msg = lv_move_ref->get_text( ).
              MESSAGE s000(zmm01) WITH lv_msg.
          ENDTRY.

        WHEN 'D'. "DATE 값이 없으면 공백으로 RETURN
          DATA: lv_del_special_in  TYPE fist-searchw,
                lv_del_special_out TYPE fist-searchw.

          CLEAR: lv_del_special_in, lv_del_special_out.

          lv_del_special_in = <lv_in_value>.

          IF NOT lv_del_special_in IS INITIAL.
            "특수문자 제거하여 숫자로만 구성함.
            CALL FUNCTION 'SF_SPECIALCHAR_DELETE'
              EXPORTING
                with_specialchar    = lv_del_special_in
              IMPORTING
                without_specialchar = lv_del_special_out.

            TRY.
                <lv_out_value> = lv_del_special_out.
              CATCH cx_sy_conversion_overflow INTO lv_move_ref.
                lv_msg = lv_move_ref->get_text( ).
                MESSAGE s000(zmm01) WITH lv_msg.
            ENDTRY.

          ENDIF.

        WHEN OTHERS.  "기타

          "ALPHA CONV 필요 하면 적용
          IF NOT ls_dd04l-convexit IS INITIAL.
            CONCATENATE 'CONVERSION_EXIT_' ls_dd04l-convexit '_OUTPUT'
            INTO lv_alpha_func.

            TRY.
                CALL FUNCTION lv_alpha_func
                  EXPORTING
                    input  = <lv_in_value>
                  IMPORTING
                    output = <lv_out_value>.
                "변환 Function 없으면 그대로 전송.
              CATCH cx_root INTO lv_fun_ref.
                lv_msg = lv_fun_ref->get_text( ).
                MESSAGE s000(zmm01) WITH lv_msg.
            ENDTRY.
          ELSE.
            TRY.
                <lv_out_value> = <lv_in_value>.
              CATCH cx_sy_conversion_overflow INTO lv_move_ref.
                lv_msg = lv_move_ref->get_text( ).
                MESSAGE s000(zmm01) WITH lv_msg.
            ENDTRY.
          ENDIF.
      ENDCASE.

      UNASSIGN: <lv_in_value>, <lv_out_value>.
    ENDLOOP.
  ENDMETHOD.


  METHOD conv_structure_sap_to_eai.
*&--------------------------------------------------------------------*
*& Module              : MM
*& Program ID          : ZCL_MM_COMMON => CONV_STRUCTURE_SAP_TO_EAI
*& T-CODE              :
*& Referenced Program  : N/A
*& Created by          : T0210053
*& Created On          : 2021.02.25
*& Type                : Common method
*& Description         : SAP 데이터 TYPE 을 EAI 전송하기 위해 CHAR 로 변환.
*&                       (구조체 단위로 변환)
*&--------------------------------------------------------------------*
*& Change History
*&--------------------------------------------------------------------*
*& 변경SEQ    변경자        변경일자             변경내용
*&--------------------------------------------------------------------*
*&    N       T0210053      2021.02.25          최초생성
*&--------------------------------------------------------------------*


    DATA: lv_in_field  TYPE char100,
          lv_out_field TYPE char100.

    FIELD-SYMBOLS: <lv_in_value>  TYPE any,
                   <lv_out_value> TYPE any.

    DATA: BEGIN OF ls_tmp,
            rollname TYPE dd04l-rollname,
          END OF ls_tmp,
          lt_tmp LIKE TABLE OF ls_tmp.


    DATA: lrf_descr_ref  TYPE REF TO cl_abap_typedescr,
          lv_dataelement TYPE dd04l-rollname,
          lv_alpha_func  TYPE rs38l_fnam,
          lv_fun_ref     TYPE REF TO cx_root,
          lv_msg         TYPE string.

    CONSTANTS: lc_in_str_name(20)  TYPE c VALUE 'IS_STRUCTURE-',
               lc_out_str_name(20) TYPE c VALUE 'ES_STRUCTURE-'.

    DATA: lo_struct TYPE REF TO cl_abap_structdescr,
          lt_comp   TYPE abap_component_tab,
          ls_comp   TYPE abap_componentdescr.

*> 수신된 구조체의 필드들을 ITAB 으로 전환
    lo_struct ?= cl_abap_typedescr=>describe_by_data( is_structure ).
    lt_comp = lo_struct->get_components( ).

*> GET DATA ELEMENT
    LOOP AT lt_comp INTO ls_comp.
      lrf_descr_ref = ls_comp-type.

      DATA(lv_absolute_name) = lrf_descr_ref->absolute_name.

      ls_tmp-rollname = lv_absolute_name+6(*).
      APPEND ls_tmp TO lt_tmp.
    ENDLOOP.

    SORT lt_tmp BY rollname.
    DELETE ADJACENT DUPLICATES FROM lt_tmp COMPARING rollname.
    IF NOT lt_tmp[] IS INITIAL.
      SELECT rollname, datatype, convexit
        FROM dd04l
        FOR ALL ENTRIES IN @lt_tmp
       WHERE rollname = @lt_tmp-rollname
        INTO TABLE @DATA(lt_dd04l).
      SORT lt_dd04l BY rollname.
    ENDIF.

*> 전체 필드에 대해 순차적으로...
    LOOP AT lt_comp INTO ls_comp.
      CLEAR: lrf_descr_ref, ls_tmp, lv_in_field, lv_out_field.

*> GET 필드 속성
      lrf_descr_ref = ls_comp-type.

      lv_absolute_name = lrf_descr_ref->absolute_name.
      DATA(lv_type) = lrf_descr_ref->type_kind.

      ls_tmp-rollname = lv_absolute_name+6(*).

*> GET 정확한 속성(P 안에서도 DEC, CURR 이 나뉘는 것처럼...)
      READ TABLE lt_dd04l INTO DATA(ls_dd04l)
                          WITH KEY rollname = ls_tmp-rollname
                          BINARY SEARCH.

*> SET VALUE
      CONCATENATE lc_in_str_name ls_comp-name INTO lv_in_field.
      ASSIGN (lv_in_field) TO <lv_in_value>.
      IF sy-subrc NE 0.
        CONTINUE.
      ENDIF.

      CONCATENATE lc_out_str_name ls_comp-name INTO lv_out_field.
      ASSIGN (lv_out_field) TO <lv_out_value>.
      IF sy-subrc NE 0.
        CONTINUE.
      ENDIF.

*> 속성 별 데이터 변환 시작
      CASE lv_type.
        WHEN 'P'.  "숫자 타입
          <lv_out_value> = <lv_in_value>.

          CONDENSE <lv_out_value>.

          "minus 금액 (앞으로 표시)
          CALL FUNCTION 'CLOI_PUT_SIGN_IN_FRONT'
            CHANGING
              value = <lv_out_value>.

        WHEN 'D'. "DATE 값이 없으면 공백으로 RETURN
          IF <lv_in_value> IS INITIAL.
            CLEAR <lv_out_value>.
          ELSE.
            <lv_out_value> = <lv_in_value>.
          ENDIF.

        WHEN OTHERS.  "기타

          "ALPHA CONV 필요 하면 적용
          IF NOT ls_dd04l-convexit IS INITIAL.
            CONCATENATE 'CONVERSION_EXIT_' ls_dd04l-convexit '_OUTPUT'
            INTO lv_alpha_func.

            TRY.
                CALL FUNCTION lv_alpha_func
                  EXPORTING
                    input  = <lv_in_value>
                  IMPORTING
                    output = <lv_out_value>.
                "변환 Function 없으면 그대로 전송.
              CATCH cx_root INTO lv_fun_ref.
                lv_msg = lv_fun_ref->get_text( ).
                MESSAGE s000(zmm01) WITH lv_msg.
            ENDTRY.
          ELSE.
            <lv_out_value> = <lv_in_value>.
          ENDIF.

      ENDCASE.

      UNASSIGN: <lv_in_value>, <lv_out_value>.
    ENDLOOP.

  ENDMETHOD.


  METHOD data_exist_check.
*&--------------------------------------------------------------------*
*& Module              : MM
*& Program ID          : ZCL_MM_COMMON => DATA_EXIST_CHECK
*& T-CODE              :
*& Referenced Program  : N/A
*& Created by          : T0210054
*& Created On          : 2021.02.05
*& Type                : Common method
*& Description         : data check
*& 참고사항
*& --> 1. 데이터 존재 유무 체크 & 텍스트 정보 조회
*&--------------------------------------------------------------------*
*& Change History
*&--------------------------------------------------------------------*
*& 변경SEQ    변경자        변경일자             변경내용
*&--------------------------------------------------------------------*
*&    N       T0210054      2021.02.05          최초생성
*&--------------------------------------------------------------------*

*    조회하는 테이블 및 필드명을 정확히 입력할 것 ( 그렇지 않으면 덤프 발생)
*     정상이면 lv_subrc = 0 및 필드 값 리턴, 없으면 lv_subrc = 4, 이외의 모든 오류는 lv_subrc = 8.

***  참조예시
*       is-table ( table : 찾을 테이블명 field : 찾을 필드명 )
*        it_where ( field : 조건 필드 value : 조건 값 )
**  DATA : lv_value TYPE mara-ersda.
**  zcl_mm_common=>data_exist_check(
**                                EXPORTING  is_table =  VALUE #( table = 'MARD' field = 'ERSDA' )
**                                                    it_where = VALUE #( ( field = 'MATNR' value = '00000000100000001' )
**                                                                                     )
**                                 IMPORTING ev_subrc = DATA(lv_subrc)
**                                                  ev_return = lv_value ).

    DATA: lv_where(500).
    DATA: : lo_data        TYPE REF TO data.
    FIELD-SYMBOLS : <ls_value>  TYPE any.

    IF is_table-table IS INITIAL OR is_table-field IS INITIAL OR  it_where[] IS INITIAL.
      ev_subrc = 8.
      EXIT.
    ENDIF.

*    조건문 설정.
    LOOP AT it_where INTO DATA(ls_where).
      IF ls_where-option IS INITIAL.
        ls_where-option = 'EQ'.
      ENDIF.
      ls_where-value =  '''' && ls_where-value && ''''.

      IF sy-tabix = 1.
        CONCATENATE ls_where-field ls_where-option  ls_where-value
              INTO lv_where SEPARATED BY space.
      ELSE.
        CONCATENATE  lv_where 'AND'  ls_where-field ls_where-option   ls_where-value
                        INTO lv_where SEPARATED BY space.
      ENDIF.
    ENDLOOP.

    TRY."    동적 구성.
        SELECT SINGLE (is_table-field)
          FROM (is_table-table)
        WHERE  (lv_where)
           INTO NEW @DATA(ls_value).
        ev_subrc = sy-subrc.

        ASSIGN ls_value->* TO FIELD-SYMBOL(<lv_value>).
        ev_return = <lv_value>.

      CATCH cx_sy_dynamic_osql_error.
        ev_subrc = 8.

    ENDTRY.



*    DATA: lv_where(500).
*
*    DATA: : lo_dref        TYPE REF TO data,
*            lo_struc       TYPE REF TO data,
*            lo_type        TYPE REF TO cl_abap_typedescr,
*            lo_struct_type TYPE REF TO cl_abap_structdescr,
*            lo_itab_type   TYPE REF TO cl_abap_tabledescr,
*            lo_comp_tab    TYPE cl_abap_structdescr=>component_table.
*
*    FIELD-SYMBOLS : <lt_outtab> TYPE ANY TABLE,
*                    <ls_struc>  TYPE any,
*                    <ls_value>  TYPE any.
*
*    IF is_table-table IS INITIAL OR is_table-field IS INITIAL OR  it_where[] IS INITIAL.
*      ev_subrc = 8.
*      EXIT.
*    ENDIF.
*
*    CLEAR : lo_dref, lo_struc, lo_itab_type, lo_struct_type, lo_comp_tab.
*
*    TRY.
**    동적 구성.
*
*        CALL METHOD cl_abap_structdescr=>describe_by_name
*          EXPORTING
*            p_name         = is_table-table
*          RECEIVING
*            p_descr_ref    = lo_type
*          EXCEPTIONS
*            type_not_found = 1
*            OTHERS         = 2.
*        CHECK sy-subrc EQ 0.
*
*        lo_struct_type ?=  lo_type.
**        lo_struct_type ?= cl_abap_typedescr=>describe_by_name( is_table-table ).
*        lo_comp_tab = lo_struct_type->get_components( ).
*
*        lo_struct_type    = cl_abap_structdescr=>create( lo_comp_tab ).
*        lo_itab_type   = cl_abap_tabledescr=>create( lo_struct_type ).
*
*        CREATE DATA lo_struc LIKE lo_struct_type.
*        ASSIGN lo_struc->* TO <ls_struc>.
*
*        CREATE DATA lo_dref TYPE HANDLE lo_itab_type.
*        ASSIGN lo_dref->* TO <lt_outtab>.
*
**    조건.
*        LOOP AT it_where INTO DATA(ls_where).
*          IF sy-tabix = 1.
*            CONCATENATE ls_where-field ` EQ `  '''' ls_where-value ''''
*                  INTO lv_where.
*          ELSE.
*            CONCATENATE  lv_where ` AND ` ls_where-field ` EQ `  '''' ls_where-value ''''
*                            INTO lv_where .
*          ENDIF.
*        ENDLOOP.
*
**    select 문.
*        SELECT (is_table-field)
*             INTO CORRESPONDING FIELDS OF TABLE <lt_outtab>
*        FROM (is_table-table) UP TO 1 ROWS
*     WHERE  (lv_where).
*        ev_subrc = sy-subrc.
*        LOOP AT <lt_outtab> ASSIGNING <ls_struc>.
*          ASSIGN COMPONENT is_table-field OF STRUCTURE <ls_struc> TO <ls_value>.
*          IF sy-subrc EQ 0.
*            ev_return = <ls_value>.
*          ELSE.
*            ev_subrc = 8.
*          ENDIF.
*          EXIT.
*        ENDLOOP.
*
*      CATCH cx_sy_dynamic_osql_error.
*        ev_subrc = 8.
*
*    ENDTRY.

  ENDMETHOD.


  METHOD get_dynp_param.
*&--------------------------------------------------------------------*
*& Module              : MM
*& Program ID          : ZCL_MM_COMMON => GET_DYNP_PARAM
*& T-CODE              :
*& Referenced Program  : N/A
*& Created by          : T0210052
*& Created On          : 2021.02.16
*& Type                : Common method
*& Description         : DYNP_VALUES_READ (Search help)
*&--------------------------------------------------------------------*
*& Change History
*&--------------------------------------------------------------------*
*& 변경SEQ    변경자        변경일자             변경내용
*&--------------------------------------------------------------------*
*&    N       T0210052      2021.02.16          최초생성
*&--------------------------------------------------------------------*

    DATA : lt_dynp TYPE TABLE OF dynpread,
           ls_dynp TYPE dynpread.

    DATA : ls_param TYPE          rsparams,
           lv_tabix TYPE          sy-tabix.

    DATA: lr_selname TYPE RANGE OF rsscr_name,
          ls_selname LIKE LINE OF lr_selname.

    DATA : lv_st TYPE fieldname.

    CONSTANTS: lc_1000 TYPE sy-dynnr VALUE '1000'.

*--------------------------------------------------------------------*

* Get Parameter Value
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

    "-1000 Screen #
    IF iv_dynnr NE lc_1000.

      ls_dynp-fieldname = iv_fname.
      APPEND ls_dynp TO lt_dynp.

    ELSE. "-1000 Screen

      IF iv_fname IS NOT INITIAL.

        ls_selname-sign   = 'E'.
        ls_selname-option = 'CP'.
        ls_selname-low    = '*' && iv_fname && '*'.
        APPEND ls_selname TO lr_selname.

        DELETE rt_params WHERE selname IN lr_selname.

      ENDIF.

      LOOP AT rt_params INTO ls_param.
        CASE ls_param-kind.
          WHEN 'S'.
            CONCATENATE ls_param-selname '-LOW' INTO ls_dynp-fieldname.
            APPEND ls_dynp TO lt_dynp.
            CONCATENATE ls_param-selname '-HIGH' INTO ls_dynp-fieldname.
            APPEND ls_dynp TO lt_dynp.
          WHEN 'P'.
            ls_dynp-fieldname = ls_param-selname.
            APPEND ls_dynp TO lt_dynp.
        ENDCASE.
      ENDLOOP.

    ENDIF.

    CHECK lt_dynp IS NOT INITIAL.

    CALL FUNCTION 'DYNP_VALUES_READ'
      EXPORTING
        dyname               = sy-cprog
        dynumb               = iv_dynnr
      TABLES
        dynpfields           = lt_dynp
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

    SORT lt_dynp BY fieldname.

    LOOP AT rt_params INTO ls_param.
      lv_tabix = sy-tabix.
      CASE ls_param-kind.
        WHEN 'S'.
          CONCATENATE ls_param-selname '-LOW' INTO ls_dynp-fieldname.
          READ TABLE lt_dynp INTO ls_dynp WITH KEY fieldname = ls_dynp-fieldname BINARY SEARCH.
*          IF LS_PARAM-LOW IS INITIAL.
          IF ls_param-low NE ls_dynp-fieldvalue
             AND sy-subrc EQ 0.
            ls_param-low = ls_dynp-fieldvalue.
          ENDIF.

          CONCATENATE ls_param-selname '-HIGH' INTO ls_dynp-fieldname.
          READ TABLE lt_dynp INTO ls_dynp WITH KEY fieldname = ls_dynp-fieldname BINARY SEARCH.
          IF ls_param-high NE ls_dynp-fieldvalue
             AND sy-subrc EQ 0.
            ls_param-high = ls_dynp-fieldvalue.
          ENDIF.

        WHEN 'P'.
          ls_dynp-fieldname = ls_param-selname.
          READ TABLE lt_dynp INTO ls_dynp WITH KEY fieldname = ls_dynp-fieldname BINARY SEARCH.
          IF ls_param-low NE ls_dynp-fieldvalue
             AND sy-subrc EQ 0.
            ls_param-low = ls_dynp-fieldvalue.
          ENDIF.
      ENDCASE.
      MODIFY rt_params FROM ls_param INDEX lv_tabix.
    ENDLOOP.

    IF iv_dynnr NE lc_1000.
      LOOP AT lt_dynp INTO ls_dynp.
        CLEAR : ls_param.
        SPLIT ls_dynp-fieldname AT '-' INTO lv_st ls_param-selname.
        ls_param-low = ls_dynp-fieldvalue.
        APPEND ls_param TO rt_params.
      ENDLOOP.
    ENDIF.

  ENDMETHOD.


  METHOD GET_EACC_INV_SEQ.

*    CONSTANTS: LC_I     TYPE C VALUE 'I',
*               LC_EQ(2) TYPE C VALUE 'EQ'.
*
*    DATA: LR_TYPE_CODE   TYPE RANGE OF ZDTV3T_AP_HEAD-TYPE_CODE,
*          LV_CHARGETOTAL TYPE ZDTV3T_AP_HEAD-CHARGETOTAL.
*
*    CLEAR: ES_EACC_DATA, RV_SUBRC.
*
**> 세금코드 별 영세/면세 구분자 검색
*    SELECT SINGLE MWSKZ, SATAX_TYPE
*      FROM ZTFI40019
*     WHERE BUKRS = @IV_BUKRS
*       AND MWSKZ = @IV_MWSKZ
*      INTO @DATA(LS_ZTFI40019).
*
**> 영세/면세 별 가용 전자세금유형 검색
*    ZCL_MM_COMMON=>COMMON_CONFIG(
*       EXPORTING  IS_COMMON =  VALUE #( M = 'E1' D = 'E1010' S = 'E1018' )
*                                       IT_WHERE = VALUE #(
*                                                  ( FIELD = 1 VALUE = IV_BUKRS )
*                                                         )
*       IMPORTING ET_OUTTAB = DATA(LT_CONFIG_E1018) ).
*
*    "세금 구분이 반드시 존재하는 것만 대상임.
*    CHECK NOT LT_CONFIG_E1018 IS INITIAL.
*
*    LOOP AT LT_CONFIG_E1018 INTO DATA(LS_CONFIG_E1018).
*      IF LS_CONFIG_E1018-FIELD4 IS INITIAL.
*        CONTINUE.
*      ENDIF.
*      "영세 또는 면세는 영세 또는 면세에 해당하는 유형분류를 Include 조건으로 정의
*      IF LS_ZTFI40019-SATAX_TYPE EQ '02' OR LS_ZTFI40019-SATAX_TYPE EQ '03'.
*        IF LS_CONFIG_E1018-FIELD2 = LS_ZTFI40019-SATAX_TYPE.
*          APPEND VALUE #( SIGN = 'I'
*                          OPTION = 'EQ'
*                          LOW = LS_CONFIG_E1018-FIELD4 ) TO LR_TYPE_CODE.
*        ENDIF.
*        "영세 또는 면세가 아닌 경우는 영세/면세에 해당하는 유형분류를  Exclude 조건으로 정의
*      ELSE.
*        APPEND VALUE #( SIGN = 'I'
*                        OPTION = 'NE'
*                        LOW = LS_CONFIG_E1018-FIELD4 ) TO LR_TYPE_CODE.
*      ENDIF.
*
*
*    ENDLOOP.
*
*    LV_CHARGETOTAL = IV_DMBTR.
*
*    SELECT SINGLE A~INV_SEQ, A~IP_ID
*             FROM ZDTV3T_AP_HEAD AS A INNER JOIN ZDTV3T_AP_EXT AS B
*               ON A~BUKRS        = B~BUKRS          AND
*                  A~ISSUE_DATE   = B~ISSUE_DATE     AND
*                  A~BUPLA        = B~BUPLA          AND
*                  A~INV_SEQ      = B~INV_SEQ
*            WHERE A~BUKRS        = @IV_BUKRS
*              AND A~BUPLA        = @IV_BUPLA
*              AND A~SU_ID        = @IV_STCD2
*              AND A~ISSUE_DATE   = @IV_BUDAT
*              AND B~USE_DOC      = @SPACE
*              AND A~CHARGETOTAL  = @LV_CHARGETOTAL
*              AND A~TYPE_CODE IN @LR_TYPE_CODE
*      INTO CORRESPONDING FIELDS OF @ES_EACC_DATA.
*
*    RV_SUBRC = SY-SUBRC.

  ENDMETHOD.


  METHOD get_html_sytle.
*&--------------------------------------------------------------------*
*& Module              : MM
*& Program ID          : ZCL_MM_COMMON => GET_HTML_SYTLE
*& T-CODE              :
*& Referenced Program  : N/A
*& Created by          : T0210128
*& Created On          : 2021.03.30
*& Type                : Common method
*& Description         : 전자결재 HTML Style Template
*&--------------------------------------------------------------------*
*& Change History
*&--------------------------------------------------------------------*
*& 변경SEQ    변경자        변경일자             변경내용
*&--------------------------------------------------------------------*
*&    N       T0210128      2021.03.30         최초생성
*&--------------------------------------------------------------------*

    DATA lt_html TYPE w3html_tab.

    CASE iv_style_type.
      WHEN 'A'.
        lt_html = VALUE #(
*        ( LINE = '<p style="font-family:"맑은 고딕","Malgun Gothic", dotum, 돋움, Arial: font-size: 16px;                               ' )
*        ( LINE = '         line-height:19.2px; margin-top:0px; margin-bottom:0px;">                                                     ' )
*        ( LINE = '<meta charset="utf-8">                                                                                                ' )
*        ( LINE = |<title>{ IV_SUBJECT }</title>|                                                                                          )
*        ( LINE = '</p>                                                                                                                  ' )

        ( line = '<style type="text/css">                                                                                               ' )
        ( line = 'html .report_wrap,                                                                                                    ' )
        ( line = 'body .report_wrap,                                                                                                    ' )
        ( line = 'div .report_wrap,                                                                                                     ' )
        ( line = 'section .report_wrap,                                                                                                 ' )
        ( line = 'header .report_wrap,                                                                                                  ' )

        ( line = '.report_wrap table {border-collapse:collapse; border-spacing:0;}                                                      ' )
        ( line = '.report_wrap a {color:#0668ed; text-decoration:none; cursor: pointer}                                                  ' )
        ( line = '.report_wrap {word-break:keep-all; min-width:900px}                                                                   ' )
        ( line = '.report_wrap header {position:relative; background:#fefafd; font-weight:bold;                                         ' )
        ( line = '                     text-align:center; margin-bottom:12px; padding:10px; border:1px solid #ccc;                      ' )
        ( line = '                     -webkit-box-shadow:4px 4px 0 rgba(0, 0, 0, .05);                                                 ' )
        ( line = '                     -moz-box-shadow:4px 4px 0 rgba(0, 0, 0, .05);                                                    ' )
        ( line = '                     box-shadow:4px 4px 0 rgba(0, 0, 0, .05);}                                                        ' )
        ( line = '.report_wrap header h2 {position:relative;font-size:24px; font-weight:bold;letter-spacing:5px; margin-bottom:10px;}   ' )
        ( line = '.report_wrap header h4 {position:relative;font-size:15px; font-weight:bold;margin-top:4px;}                           ' )
        ( line = '.report_wrap .contents {margin:20px 20px 0;}                                                                          ' )
        ( line = '.report_wrap .contents article {border:1px solid #ccc; padding:0 20px 20px;                                           ' )
        ( line = '                                -webkit-box-shadow:4px 4px 0 rgba(0, 0, 0, .05);                                      ' )
        ( line = '                                -moz-box-shadow:4px 4px 0 rgba(0, 0, 0, .05); }                                       ' )
        ( line = '                                 box-shadow:4px 4px 0 rgba(0, 0, 0, .05);}                                            ' )
        ( line = '.report_wrap .contents h3 {position:relative; line-height:10px; font-size:10px; font-weight:bold;}                    ' )
        ( line = '.report_wrap .contents h3 span {position:absolute; right:0; bottom:0px; font-size:12px; font-weight:normal;}          ' )
        ( line = '.report_wrap .contents article section {margin-top:20px;}                                                             ' )
        ( line = '.report_wrap section table {width:100%; box-sizing:border-box;}                                                       ' )
        ( line = '.report_wrap section th, article td {border:1px solid #ccc; padding:5px 6px; min-height:12px; letter-spacing:-0.05em} ' )
        ( line = '.report_wrap item td {border:1px solid #ccc; padding:5px 6px; min-height:12px; letter-spacing:-0.05em}                ' )
        ( line = '.report_wrap section tr:first-child th, .report_wrap section tr:first-child td {border-top:1px solid#1e5784;}         ' )
        ( line = '.report_wrap section th {text-align:center; font-weithg:normal;font-size:12px; background:#effafd}                    ' )
        ( line = '.report_wrap section th.head1 {badckground:#f3ffff}                                                                   ' )
        ( line = '.report_wrap section td {font-size:13px; word-break: break-all;}                                                      ' )
        ( line = '.report_wrap section .tc th {text-align:center;}                                                                      ' )
        ( line = '.report_wrap section .tc td {text-align:center;}                                                                      ' )
        ( line = '.report_wrap section .tl {text-align:left !inportant;}                                                                ' )
        ( line = '.report_wrap section .tr {text-align:right !inportant;}                                                               ' )
        ( line = '.report_wrap .tc {text-align:center;}                                                                                 ' )
        ( line = '.report_wrap .tl {text-align:left !inportant;}                                                                        ' )
        ( line = '.report_wrap .tr {text-align:right !inportant;}                                                                       ' )
        ( line = '.report_wrap section .db_line {border-right:3px double #ccc;}                                                         ' )

        ( line = 'footer {background:rul)img/logo_system.png) no-repeat 5px center; padding-left:160px;}                                ' )
        ( line = 'footer div {height:46px; line-height:46px; font-size:11px; color:#777;}                                               ' )
        ( line = 'P {font-size : 15px; padding-left:20px;}                                                                              ' )
        ( line = '</style>                                                                                                              ' ) ).

      WHEN OTHERS.
    ENDCASE.


    "- Convert itab to string.
    CLEAR: rv_string.
    CALL FUNCTION 'CONVERT_TABLE_TO_STRING'
      EXPORTING
        i_tabline_length = 255
      IMPORTING
        e_string         = rv_string
      TABLES
        it_table         = lt_html.

    CONDENSE rv_string.  "공백 축소


  ENDMETHOD.


  METHOD get_lgort.
*&--------------------------------------------------------------------*
*& Module              : MM
*& Program ID          : ZCL_MM_COMMON => GET_LGORT
*& T-CODE              :
*& Referenced Program  : N/A
*& Created by          : T0210052
*& Created On          : 2021.01.24
*& Type                : Common method
*& Description         : 저장위치 유효성 체크 및 유형별 값 제공
*&--------------------------------------------------------------------*
*& Change History
*&--------------------------------------------------------------------*
*& 변경SEQ    변경자        변경일자             변경내용
*&--------------------------------------------------------------------*
*&    N       T0210052      2021.01.24          최초생성
*&--------------------------------------------------------------------*
    DATA : lr_werks TYPE RANGE OF t001l-werks.

    CLEAR: et_data.

*-
    CLEAR: lr_werks, lr_werks[].
    lr_werks[] = VALUE #( BASE lr_werks[] ( sign = 'I' option = 'EQ' low = iv_werks ) ).

*-
    CASE iv_mode.
      WHEN space.
        SELECT werks, lgort, lgobe FROM t001l WHERE werks IN @lr_werks AND lgort = @iv_lgort
        INTO CORRESPONDING FIELDS OF TABLE @et_data.
        rv_subrc = sy-subrc.

      WHEN OTHERS.
    ENDCASE.

  ENDMETHOD.


  METHOD GET_MATNR.
*&--------------------------------------------------------------------*
*& Module              : MM
*& Program ID          : ZCL_MM_COMMON => GET_MATNR
*& T-CODE              :
*& Referenced Program  : N/A
*& Created by          : T0210052
*& Created On          : 2021.02.25
*& Type                : Common method
*& Description         : 자재코드 유효성 체크 및 유형별 값 제공
*&--------------------------------------------------------------------*
*& Change History
*&--------------------------------------------------------------------*
*& 변경SEQ    변경자        변경일자             변경내용
*&--------------------------------------------------------------------*
*&    N       T0210052      2021.02.25         최초생성
*&--------------------------------------------------------------------*
    DATA : LR_WERKS TYPE RANGE OF MARC-WERKS.

    CLEAR: ET_DATA.

*-
    CLEAR: LR_WERKS, LR_WERKS[].
    IF IV_WERKS IS NOT INITIAL.
      LR_WERKS[] = VALUE #( BASE LR_WERKS[] ( SIGN = 'I' OPTION = 'EQ' LOW = IV_WERKS ) ).
    ENDIF.

*-
    CASE IV_MODE.
      WHEN SPACE.
        SELECT A~WERKS, A~MATNR, A~SOBSL, A~BESKZ, B~MAKTX, C~MEINS, D~NAME1, A~EKGRP,
               A~MMSTA, E~DEINK, F~MTSTB, C~LVORM AS LVORM_MARA, A~LVORM AS LVORM_MARC
               FROM MARC AS A INNER JOIN MAKT AS B
                                      ON A~MATNR = B~MATNR
                                     AND B~SPRAS = @SY-LANGU
                              INNER JOIN MARA AS C
                                      ON A~MATNR = C~MATNR
                            LEFT OUTER JOIN T001W AS D
                                      ON A~WERKS = D~WERKS AND D~SPRAS = @SY-LANGU
                            LEFT OUTER JOIN T141      AS E
                                      ON A~MMSTA = E~MMSTA
                            LEFT OUTER JOIN T141T     AS F
                                      ON A~MMSTA = F~MMSTA AND F~SPRAS = @SY-LANGU
               WHERE A~WERKS IN @LR_WERKS AND A~MATNR = @IV_MATNR
        INTO CORRESPONDING FIELDS OF TABLE @ET_DATA.
        RV_SUBRC = SY-SUBRC.

      WHEN OTHERS.
    ENDCASE.
  ENDMETHOD.


  method GET_PGM_PARAMETER.
  endmethod.


  METHOD get_search_help.
*&--------------------------------------------------------------------*
*& Module              : MM
*& Program ID          : ZCL_MM_COMMON => GET_SEARCH_HELP
*& T-CODE              :
*& Referenced Program  : N/A
*& Created by          : T0210052
*& Created On          : 2021.02.16
*& Type                : Common method
*& Description         : Screen SearchHelp
*& 참고사항
*& --> 1. 기능추가시 CASE 문에 WHEN 을 통해 추가 할것.
*& --> 2. 다른것은 건들지 말고  WHEN 구문옆에 생성자 이력 남겨둘것
*&--------------------------------------------------------------------*
*& Change History
*&--------------------------------------------------------------------*
*& 변경SEQ    변경자        변경일자             변경내용
*&--------------------------------------------------------------------*
*&    N       T0210052      2021.02.16          최초생성
*&--------------------------------------------------------------------*
    TYPES: BEGIN OF lts_value,
             kyfld TYPE char10,
             descr TYPE text40,
           END OF lts_value.

    DATA: lt_field  TYPE apb_lpd_t_dfies, "TABLE OF DFIES,
          lt_return TYPE TABLE OF ddshretval.

    DATA: lt_param TYPE rsparams_tt,
          ls_param TYPE rsparams.

    DATA: lv_refld TYPE dfies-fieldname,
          lv_dyfld TYPE help_info-dynprofld,
          lv_month TYPE isellist-month.

    DATA: lt_data TYPE REF TO data.

    FIELD-SYMBOLS: <lt_data> TYPE STANDARD TABLE.

    CONSTANTS: lc_kyfld    TYPE fieldname VALUE 'KYFLD',
               lc_sufld    TYPE fieldname VALUE 'SUFLD',
               lc_pa_bukrs TYPE fieldname VALUE 'P_BUKRS'.

    DEFINE _l_set_val.

      lt_field = VALUE #( BASE lt_field
                 ( fieldname = &1 inttype = &2 leng = &3 intlen = &3 outputlen = &3
                   reptext = &4 offset = &5 ) ).

    END-OF-DEFINITION.

*******************************************************************************************************

    lv_dyfld = iv_fname.
    lv_refld = iv_fname+2(5).

    DATA(lv_dynnr) = COND syst_dynnr(
                          WHEN sy-dynnr = '1000' OR sy-dynnr = '3010' OR
                               sy-dynnr = '3020' OR sy-dynnr = '3030' OR
                               sy-dynnr = '3040'
                          THEN '1000' ELSE sy-dynnr ).

    CASE lv_refld.
***-----------------------------------------------------------------------------------------------***
      WHEN 'BERID'. "MRP 영역   "MDLV Filtering        2021.02.18
        CREATE DATA lt_data TYPE TABLE OF lts_value.
        ASSIGN lt_data->* TO <lt_data>.

        DATA : lr_berty TYPE RANGE OF berty.
        CLEAR: lr_berty, lr_berty[].

        IF iv_filt_val_a IS NOT INITIAL.
          lr_berty[] = VALUE #( ( sign = 'I' option = 'EQ'
                                   low = iv_filt_val_a(10) high ='' ) ).
        ENDIF.

        SELECT berid AS kyfld
               bertx AS descr
          INTO CORRESPONDING FIELDS OF TABLE <lt_data>
          FROM mdlv
         WHERE berty IN lr_berty.

        _l_set_val : 'KYFLD' 'C' '16' 'MRP 영역'  '0',
                     'DESCR' 'C' '40' '내역'      '16'.

        DATA(lt_map) = VALUE icl_dselc_t( ( fldname = lc_kyfld dyfldname = lv_dyfld ) ).
***-----------------------------------------------------------------------------------------------***
      WHEN 'SPMON'. "YYYYMM    "T0210052  년/월 형식   2021.02.16

        lt_param = zcl_mm_common=>get_dynp_param( iv_fname = lv_refld iv_dynnr = lv_dynnr ). " iv_dynnr ##
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
***-----------------------------------------------------------------------------------------------***
    ENDCASE.

*******************************************************************************************************

    CHECK <lt_data> IS ASSIGNED
      AND <lt_data> IS NOT INITIAL.

    "-Call F4 Search Help
    CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
      EXPORTING
        retfield        = lv_refld
        dynpprog        = sy-cprog
        dynpnr          = sy-dynnr
        dynprofield     = lv_dyfld
        value_org       = 'S'
      TABLES
        value_tab       = <lt_data>
        field_tab       = lt_field
        dynpfld_mapping = lt_map
        return_tab      = lt_return
      EXCEPTIONS
        parameter_error = 1
        no_values_found = 2
        OTHERS          = 3.

    CHECK lt_return IS NOT INITIAL.
    SORT lt_return BY fieldname.
    READ TABLE lt_return INTO DATA(ls_return) WITH KEY fieldname = lc_kyfld BINARY SEARCH.
    ev_value = ls_return-fieldval.

  ENDMETHOD.


  METHOD set_value_update.
*&--------------------------------------------------------------------*
*& Module              : MM
*& Program ID          : ZCL_MM_COMMON => SET_VALUE_UPDATE
*& T-CODE              :
*& Referenced Program  : N/A
*& Created by          : T0210054
*& Created On          : 2021.07.01
*& Type                : Common method
*& Description         : Screen field update
*& 참고사항
*& --> 1. Screen field update
*&--------------------------------------------------------------------*
*& Change History
*&--------------------------------------------------------------------*
*& 변경SEQ    변경자        변경일자             변경내용
*&--------------------------------------------------------------------*
*&    N       T0210054      2021.07.01          최초생성
*&--------------------------------------------------------------------*

    DATA : lt_dynp TYPE TABLE OF dynpread,
           ls_dynp TYPE dynpread.

    ls_dynp-fieldname  = iv_fname.
    ls_dynp-fieldvalue = iv_value.
    APPEND ls_dynp TO lt_dynp.


    CALL FUNCTION 'DYNP_VALUES_UPDATE'
      EXPORTING
        dyname               = sy-cprog     "주프로그램
        dynumb               = iv_dynnr     "화면번호
      TABLES
        dynpfields           = lt_dynp  "현재화면필드 (값포함)
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

  ENDMETHOD.


  METHOD show_bapi_message.
*&--------------------------------------------------------------------*
*& Module              : MM
*& Program ID          : ZCL_MM_COMMON => SHOW_BAPI_MESSAGE
*& T-CODE              :
*& Referenced Program  : N/A
*& Created by          : T0210053
*& Created On          : 2021.02.25
*& Type                : Common method
*& Description         : 표준 BAPI 에서 RETURN 되는 메시지를 POPUP 형태로 출력
*&--------------------------------------------------------------------*
*& Change History
*&--------------------------------------------------------------------*
*& 변경SEQ    변경자        변경일자             변경내용
*&--------------------------------------------------------------------*
*&    N       T0210053      2021.02.25          최초생성
*&--------------------------------------------------------------------*

    DATA : ls_return TYPE bapiret2,
           lv_text   TYPE c LENGTH 10,
           lv_zeile  TYPE i,
           lt_msgtab TYPE bapiret2_t.

    CONSTANTS : lc_text(10) VALUE 'Row No.'.

*1> BAPIRETURN -> BAPIRET2 으로 변환.
    LOOP AT it_return2 INTO DATA(ls_return2).
      CLEAR ls_return.
      CALL FUNCTION 'BALW_RETURN_TO_RET2'
        EXPORTING
          return_in = ls_return2
        IMPORTING
          return_ou = ls_return.
      APPEND ls_return TO lt_msgtab.
    ENDLOOP.

*2> BAPIRET2 사용
    IF NOT it_return[] IS INITIAL.
      APPEND LINES OF it_return[] TO lt_msgtab[].
    ENDIF.

    CHECK lt_msgtab[] IS NOT INITIAL.

    CALL FUNCTION 'MESSAGES_INITIALIZE'
      EXCEPTIONS
        log_not_active       = 1
        wrong_identification = 2
        OTHERS               = 3.

    LOOP AT lt_msgtab INTO ls_return.
      IF ls_return-row IS INITIAL.
        lv_zeile = sy-tabix.
      ELSE.
        lv_text  = lc_text.
        lv_zeile = ls_return-row.
      ENDIF.

      CALL FUNCTION 'MESSAGE_STORE'
        EXPORTING
          msgty                  = ls_return-type
          arbgb                  = ls_return-id
          txtnr                  = ls_return-number
          msgv1                  = ls_return-message_v1
          msgv2                  = ls_return-message_v2
          msgv3                  = ls_return-message_v3
          msgv4                  = ls_return-message_v4
          zeile                  = lv_zeile
        EXCEPTIONS
          message_type_not_valid = 1
          not_active             = 2
          OTHERS                 = 3.
      CLEAR ls_return.

    ENDLOOP.

    CALL FUNCTION 'MESSAGES_SHOW'
      EXPORTING
        show_linno_text     = lv_text
        show_linno_text_len = 5
      IMPORTING
        e_exit_command      = rs_return
      EXCEPTIONS
        inconsistent_range  = 1
        no_messages         = 2
        OTHERS              = 3.

  ENDMETHOD.
ENDCLASS.
