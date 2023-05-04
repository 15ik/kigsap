*&---------------------------------------------------------------------*
*& Include          ZRMM3040TOP
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
* 주의: ALV Display Table은 반드시 Types 선언으로 해주세요
*       인터널 테이블은 Header Line Table 금지
*----------------------------------------------------------------------*

TABLES: zsvbmminfoprice.
*  ztmm30110.

*----------------------------------------------------------------------*
* Local Class Define..
*----------------------------------------------------------------------*
CLASS: lcl_cust_alv_grid DEFINITION DEFERRED.

*----------------------------------------------------------------------*
* Constants
*----------------------------------------------------------------------*

CONSTANTS: gc_dynnr_1000 TYPE sy-dynnr VALUE '1000'.

CONSTANTS: gc_bukrs_1101 TYPE t001-bukrs VALUE '1101'.

CONSTANTS: gc_kalsk_do TYPE lfm1-kalsk VALUE 'DO',  "국내
           gc_kalsk_im TYPE lfm1-kalsk VALUE 'IM'.  "해외

* Condition Type
CONSTANTS: gc_cond_type TYPE zsvbmminfoprice-conditiontype VALUE 'PB00',
           gc_confirm   TYPE eine-exprf VALUE 'CP'.

* ALV 관련 상수
CONSTANTS: gc_emphsz_c300 TYPE lvc_emphsz VALUE 'C300', "노란색
           gc_emphsz_c500 TYPE lvc_emphsz VALUE 'C500'. "초록색

* GOS 관련 상수
CONSTANTS: gc_gos_typeid TYPE swo_objtyp VALUE 'BUS3003', "SWO1 등록한 OBJECT TYPE
           gc_gos_catid  TYPE sibfcatid  VALUE 'BO'.      "Category of Objects "Instances of BOR Object Types

*----------------------------------------------------------------------*
* Type
*----------------------------------------------------------------------*
TYPES: BEGIN OF ty_data,
         bukrs                  TYPE t024e-bukrs,                                     "회사코드
         infnr                  TYPE eina-infnr,                                      "구매정보레코드
         esokz                  TYPE eine-esokz,                                      "범주
         esokz_text             TYPE val_text,
         lifnr                  TYPE eina-lifnr,                                      "공급업체
         name1                  TYPE lfa1-name1,                                      "공급업체명
         matnr                  TYPE eina-matnr,                                      "자재
         maktx                  TYPE makt-maktx,                                      "내역
         ekorg                  TYPE eine-ekorg,                                      "구매조직
         ekotx                  TYPE t024e-ekotx,
         werks                  TYPE eine-werks,                                      "플랜트
         werks_text             TYPE t001w-name1,
         ekgrp                  TYPE eine-ekgrp,                                      "구매그룹
         eknam                  TYPE t024-eknam,
         bstae                  TYPE eine-bstae,                                      "확인관리
         bsbez                  TYPE t163m-bsbez,
         taxim                  TYPE mlan-taxim,                                      "자재세금지시자
         taxib                  TYPE tmkm1t-taxib,
         mwskz                  TYPE eine-mwskz,                                      "세금코드
         text1                  TYPE t007s-text1,
         webre                  TYPE eine-webre,                                      "GR기준IV
         inco1                  TYPE eine-inco1,                                      "인도조건
         bezei                  TYPE tinct-bezei,
         exprf                  TYPE eine-exprf,                                      "단가유형
         exprf_text             TYPE t616t-bezei,
         urzla                  TYPE eina-urzla,                                      "원산지
         uebto                  TYPE eine-uebto,                                      "초과납품허용
         verid                  TYPE eine-verid,                                      "생산버젼
*         kbetr                  TYPE zsvcmm_a445_c-kbetr,                             "인쇄교체비(KSCHL = 'ZPRI')
         valid                  TYPE zsvbmminfoprice-validprice,
         netpr                  TYPE zsvbmminfoprice-conditionratevalue,              "단가
         waers                  TYPE zsvbmminfoprice-conditionratevalueunit,          "통화
         peinh                  TYPE zsvbmminfoprice-conditionquantity,               "가격단위
         kmein                  TYPE zsvbmminfoprice-conditionquantityunit,        "발주단위
         datab                  TYPE zsvbmminfoprice-conditionvaliditystartdate,      "시작일
         datbi                  TYPE zsvbmminfoprice-conditionvalidityenddate,        "종료일
*         cntr_no                TYPE ztmm30113-cntr_no,
*         cntr_rev               TYPE ztmm30113-cntr_rev,
*         zorder_person          TYPE ztmm30110-zorder_person,                         "계약담당자
*         zorder_person_name     TYPE zsvmm_user_info-employ_name,
*         zorder_department      TYPE ztmm30110-zorder_department,                     "계약부서
*         zorder_department_name TYPE zsvmm_user_info-depart_name,
*         title                  TYPE ztmm30110-/bofu/bcsd_subj,
       END OF ty_data.

"*- Alv Dispaly
TYPES: BEGIN OF ty_disp.
         INCLUDE TYPE zscn00003.  "필수!! ALV Changed Common Struc.. (STATU/CELLS/CELLC/MSGTB/DCFLG)
         INCLUDE TYPE ty_data.    "Table 에 ZDELE가 없을 경우 필드 추가 필수
"Dcflg=> C:Create U:Update T:라인삭제 '':Original
TYPES:   cdlst(30),
         zdele,                  "필수필드임 (테이블에 있을경우는 생략)
       END OF ty_disp.

* 첨부파일 정보
TYPES: BEGIN OF ty_atta,
         typeid_a TYPE sibftypeid,
         instid_a TYPE sibfboriid,
       END OF ty_atta.

* 구매정보레코드 I/F 수신 결과
*TYPES: BEGIN OF ty_ifmax,
*         cntr_no                TYPE ztmm30113-cntr_no,
*         cntr_rev               TYPE ztmm30113-cntr_rev,
*         cntr_item_lno          TYPE ztmm30113-cntr_item_lno,
*         infnr                  TYPE ztmm30113-infnr,
*         timestamp              TYPE ztmm30113-timestamp,
*         times                  TYPE n LENGTH 15,
*         zorder_person          TYPE ztmm30110-zorder_person,                  "계약담당자
*         zorder_person_name     TYPE zsvmm_user_info-employ_name,
*         zorder_department      TYPE ztmm30110-zorder_department,              "계약부서
*         zorder_department_name TYPE zsvmm_user_info-depart_name,
*         bedat                  TYPE ztmm30110-bedat,
*         title                  TYPE ztmm30110-/bofu/bcsd_subj,
*       END OF ty_ifmax.

* 단가 이력 조회
TYPES: BEGIN OF ty_cond_hist,
         infnr      TYPE zsvbmminfoprice-infnr,                        "구매정보레코드
         esokz      TYPE zsvbmminfoprice-purchasinginforecordcategory, "구매정보레코드 범주
         esokz_text TYPE val_text,
         lifnr      TYPE zsvbmminfoprice-lifnr,                        "공급업체
         name1      TYPE zsvbmminfoprice-name1,                        "업체명
         matnr      TYPE zsvbmminfoprice-matnr,                        "자재
         maktx      TYPE zsvbmminfoprice-materialdescription,          "자재내역
         ekorg      TYPE zsvbmminfoprice-purchasingorganization,       "구매조직
         werks      TYPE zsvbmminfoprice-plant,                        "플랜트
         netpr      TYPE zsvbmminfoprice-conditionratevalue,           "금액
         peinh      TYPE zsvbmminfoprice-conditionquantity,            "가격단위
         waers      TYPE zsvbmminfoprice-conditionratevalueunit,       "통화
         datab      TYPE zsvbmminfoprice-conditionvaliditystartdate,   "유효 시작일
         datbi      TYPE zsvbmminfoprice-conditionvalidityenddate,     "유효 종료일
       END OF ty_cond_hist.

* 인쇄교체비 이력 조회
*TYPES: BEGIN OF ty_zpri_hist,
*         matnr TYPE zsvcmm_a445_c-matnr,        "자재
*         datab TYPE zsvcmm_a445_c-datab,        "유효 시작일
*         datbi TYPE zsvcmm_a445_c-datbi,        "유효 종료일
*         kbetr TYPE zsvcmm_a445_c-kbetr,        "금액
*         kpein TYPE zsvcmm_a445_c-kpein,        "가격단위
*         waers TYPE zsvcmm_a445_c-currency,     "통화
*       END OF ty_zpri_hist.

*----------------------------------------------------------------------*
* Class
*----------------------------------------------------------------------*

*---------------------
*-- ALV Object
*---------------------
DATA: grf_docking_con TYPE REF TO cl_gui_docking_container.

DATA: grf_head TYPE REF TO cl_gui_container,    "Header Information Object
      grf_body TYPE REF TO cl_gui_container.    "ALV Object

DATA: grf_grid TYPE REF TO lcl_cust_alv_grid.   " ALV Grid

* 단가 이력 조회 Popup
DATA: grf_cont_hist TYPE REF TO cl_gui_custom_container,
      grf_grid_hist TYPE REF TO lcl_cust_alv_grid.        " ALV Grid

* 인쇄교체비 이력 조회 Popup
DATA: grf_cont_zpri TYPE REF TO cl_gui_custom_container,
      grf_grid_zpri TYPE REF TO lcl_cust_alv_grid.        " ALV Grid

*----------------------------------------------------------------------*
* Global Data
*----------------------------------------------------------------------*
DATA: gt_disp TYPE TABLE OF ty_disp,
      gt_atta TYPE HASHED TABLE OF ty_atta WITH UNIQUE KEY typeid_a instid_a.  "첨부파일 정보

DATA: gv_land1 TYPE t001-land1.

*DATA: gt_ifmax TYPE TABLE OF ty_ifmax.

DATA: gt_t001w  TYPE TABLE OF t001w,
      gt_t163m  TYPE TABLE OF t163m,
      gt_tmkm1t TYPE TABLE OF tmkm1t,
      gt_t007s  TYPE TABLE OF t007s,
      gt_tinct  TYPE TABLE OF tinct,
      gt_t616t  TYPE TABLE OF t616t.
*      gt_zpri   TYPE TABLE OF zsvcmm_a445_c.

DATA: gt_cond_hist TYPE TABLE OF ty_cond_hist.
*      GV_YEAR      TYPE T5A4A-DLYYR.            "구매단가 이력 조회 기간

*DATA: gt_zpri_hist TYPE TABLE OF ty_zpri_hist.

DATA: ok_code    TYPE sy-ucomm,
      gv_ok_code TYPE sy-ucomm,
*      GV_MODE    TYPE C VALUE '',    "Display :D  Edit:E (화면편집)
      gv_mrow    TYPE i VALUE 10.    "Multi Row

* Selected Rows
DATA: gt_rows TYPE lvc_t_row.

*----------------------------------------------------------------------*
* FIELD-SYMBOLS     <G(V,T,O,S)_XXXX>   Local : <L(V,T,O,S)_XXXX>
*----------------------------------------------------------------------*



*----------------------------------------------------------------------*
* Macros
*----------------------------------------------------------------------*
DEFINE _g_set_value.

  &1 = &2.

END-OF-DEFINITION.

DEFINE _g_set_values.

  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      id     = &1
      values = &2.

END-OF-DEFINITION.
