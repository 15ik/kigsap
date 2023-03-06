*&---------------------------------------------------------------------*
*& Include          ZRMM3050TOP
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
* 주의: ALV Display Table은 반드시 Types 선언으로 해주세요
*       인터널 테이블은 Header Line Table 금지
*----------------------------------------------------------------------*

TABLES: ekko, ekpo, t163y.

*----------------------------------------------------------------------*
* Local Class Define..
*----------------------------------------------------------------------*
CLASS: lcl_cust_alv_grid DEFINITION DEFERRED.

*----------------------------------------------------------------------*
* Constants
*----------------------------------------------------------------------*

CONSTANTS: gc_dynnr_1000 TYPE sy-dynnr VALUE '1000'.

CONSTANTS: gc_bukrs_1101 TYPE bukrs VALUE '1101',
           gc_bukrs_2101 TYPE bukrs VALUE '2101',
           gc_bukrs_3101 TYPE bukrs VALUE '3101'.

CONSTANTS: gc_kalsk_do TYPE lfm1-kalsk VALUE 'DO', "국내
           gc_kalsk_im TYPE lfm1-kalsk VALUE 'IM'.  "해외

CONSTANTS: gc_tab_ekpo TYPE tabname VALUE 'EKPO'.

CONSTANTS: gc_mode_change VALUE 'C', "Change
           gc_mode_display VALUE 'D'.   "Display

* ALV 관련 상수
CONSTANTS: gc_emphsz_c300 TYPE lvc_emphsz VALUE 'C300', "노란색
           gc_emphsz_c500 TYPE lvc_emphsz VALUE 'C500'. "초록색

CONSTANTS: gc_tech_x1(2) TYPE c VALUE 'X ',
           gc_tech_x2(2) TYPE c VALUE ' X'.

* GOS 관련 상수
CONSTANTS: gc_gos_typeid TYPE swo_objtyp VALUE 'BUS2012', "SWO1 등록한 OBJECT TYPE
           gc_gos_catid  TYPE sibfcatid  VALUE 'BO'.      "Category of Objects "Instances of BOR Object Types


CONSTANTS: gc_icon_expa TYPE icon-id VALUE '@K1@',
           gc_icon_coll TYPE icon-id  VALUE '@K2@'.

*----------------------------------------------------------------------*
* Type
*----------------------------------------------------------------------*
* 개요
TYPES: BEGIN OF ty_data_ra,
         apv_stat               TYPE icon-id,
         bukrs                  TYPE ekko-bukrs,                   "회사코드
         ekorg                  TYPE ekko-ekorg,
         frgke                  TYPE ekko-frgke,                   "결재상태
         wf_status              TYPE char20,                       "결재상태명

         lifnr                  TYPE ekko-lifnr,                   "공급업체
         name1                  TYPE zsvcmm_po_tax-name1,          "업체명
         ebeln                  TYPE ekko-ebeln,                   "구매오더
         ebelp                  TYPE ekpo-ebelp,
         bsart                  TYPE ekko-bsart,                   "문서유형
         batxt                  TYPE zsvcmm_po_tax-batxt,          "문서유형내역
         title                  TYPE zsvcmm_po_tax-title,          "발주명
         tag_amt                TYPE zsvcmm_po_tax-totalvalue,     "공급가액
         tax_amt                TYPE zsvcmm_po_tax-taxamount,      "부가세
         tot_amt                TYPE zsvcmm_po_tax-totalvalue,     "계약금액(공급가액+부가세)
         waers                  TYPE ekko-waers,                   "통화
         zterm                  TYPE zsvcmm_po_tax-zterm,          "지급조건
         vtext                  TYPE tvzbt-vtext,                  "지급조건내역
         inco1                  TYPE zsvcmm_po_tax-inco1,          "인도조건
         bezei                  TYPE tinct-bezei,                  "인도조건내역
         bedat                  TYPE ekko-bedat,                   "발주생성일
         text_k00               TYPE tdline,                       "특약사항
         text_k01               TYPE tdline,                       "지출발의조건
         verkf                  TYPE ekko-verkf,                   "파트너담당
         ihrez                  TYPE ekko-ihrez,                   "접수/거부
         unsez                  TYPE ekko-unsez,                   "참조
         kdatb                  TYPE ekko-kdatb,                   "계약시작일
         kdate                  TYPE ekko-kdate,                   "계약종료일
         gwldt                  TYPE ekko-gwldt,                   "대금지급예정일
         submi                  TYPE ekko-submi,                   "계약서명담당
         absgr                  TYPE ekko-absgr,                   "세금계산서발행방식
         absgr_text             TYPE val_text,
         ekgrp                  TYPE ekko-ekgrp,                   "구매그룹
         eknam                  TYPE t024-eknam,
         dppct                  TYPE ekko-dppct,                   "선급비율
         inco_site              TYPE ekko-inco2_l,                 "인코텀스장소

         inco2_l                TYPE ekko-inco2_l,                 "선적항
         inco3_l                TYPE ekko-inco3_l,                 "도착항

         zorder_person          TYPE ekko-zorder_person,           "발주담당자
*         zorder_person_name     TYPE zsvmm_user_info-employ_name,
         zorder_department      TYPE ekko-zorder_department,       "발주부서
*         zorder_department_name TYPE zsvmm_user_info-depart_name,
         zexpen_person          TYPE ekko-zexpen_person,           "지출발의담당자
*         zexpen_person_name     TYPE zsvmm_user_info-employ_name,
         zexpen_department      TYPE ekko-zexpen_department,       "지출발의부서
*         zexpen_department_name TYPE zsvmm_user_info-depart_name,

         zcoop_qm               TYPE ekko-zcoop_qm,                "공동검수여부
         zqm_person             TYPE ekko-zqm_person,              "검수담당자
*         zqm_person_name        TYPE zsvmm_user_info-employ_name,
         zqm_department         TYPE ekko-zqm_department,          "검수부서
*         zqm_department_name    TYPE zsvmm_user_info-depart_name,
         zreal_cost             TYPE ekko-zreal_cost,              "실비정산
         zcontract_deposit      TYPE ekko-zcontract_deposit,       "계약이행보증율
         zcontract_guarn        TYPE ekko-zcontract_guarn,         "계약이행보증금
         zcont_gua_type         TYPE ekko-zcont_gua_type,          "계약이행 보증유형
         zcon_kdatb             TYPE ekko-zcon_kdatb,              "계약이행 보증시작
         zcon_kdate             TYPE ekko-zcon_kdate,              "계약이행보증 종료
         zprepay_deposit        TYPE ekko-zprepay_deposit,         "선급이행보증율
         zprepay_grarn          TYPE ekko-zprepay_grarn,           "선급이행보증금
         zprep_gua_type         TYPE ekko-zprep_gua_type,          "선급이행 보증유형
         zpay_kdatb             TYPE ekko-zpay_kdatb,              "선급이행 보증시작
         zpay_kdate             TYPE ekko-zpay_kdate,              "선급이행보증 종료
         zdefect_deposit        TYPE ekko-zdefect_deposit,         "하자이행보증율
         zdefect_guarn          TYPE ekko-zdefect_guarn,           "하자이행보증금
         zdefec_gua_type        TYPE ekko-zdefec_gua_type,         "하자이행 보증유형
         zdef_base_date         TYPE ekko-zdef_base_date,          "하자이행 보증기준
         zdef_kdatb             TYPE ekko-zdef_kdatb,              "하자보증기간
         late_rate              TYPE ekko-late_rate,               "지체상금율
         lifn2_c1               TYPE ekpa-lifn2,                   "공동수급업체1
         name1_c1               TYPE lfa1-name1,
         lifn2_c2               TYPE ekpa-lifn2,                   "공동수급업체2
         name1_c2               TYPE lfa1-name1,
         lifn2_c3               TYPE ekpa-lifn2,                   "공동수급업체3
         name1_c3               TYPE lfa1-name1,
         lifn2_c4               TYPE ekpa-lifn2,                   "공동수급업체4
         name1_c4               TYPE lfa1-name1,
         lifn2_c5               TYPE ekpa-lifn2,                   "공동수급업체5
         name1_c5               TYPE lfa1-name1,

         zemanage2              TYPE ekko-zemanage2,               "관리번호
         zeshiptype             TYPE ekko-zeshiptype,              "선적구분
         zededline              TYPE ekko-zededline,               "선적기한
         herkl                  TYPE ekko-herkl,                   "원산지
         zeinspect              TYPE ekko-zeinspect,               "수입검사여부
       END OF ty_data_ra.

* 상세
TYPES: BEGIN OF ty_data_rb,
         bukrs               TYPE ekko-bukrs,
         frgke               TYPE ekko-frgke,                          "결재상태
         wf_status           TYPE char20,                              "결재상태명
         lifnr               TYPE ekko-lifnr,                          "공급업체
         name1               TYPE zsvcmm_po_tax-name1,                 "업체명
         bsart               TYPE ekko-bsart,                          "문서유형
         batxt               TYPE zsvcmm_po_tax-batxt,                 "문서유형내역
         ebeln               TYPE ekko-ebeln,                          "구매오더
         ebelp               TYPE ekpo-ebelp,                          "품목
         loekz               TYPE ekpo-loekz,
         title               TYPE zsvcmm_po_tax-title,                 "발주명
         matnr               TYPE ekpo-matnr,                          "자재
         txz01               TYPE ekpo-txz01,                          "내역
         bwtar               TYPE ekpo-bwtar,                          "평가유형
         charg               TYPE eket-charg,                          "배치(YJP 전용)
         menge               TYPE ekpo-menge,                          "수량
         meins               TYPE ekpo-meins,                          "단위
         brtwr_cal           TYPE komp-netwr,                      "가감액, 인쇄교체비 계산 후 품목금액
         netpr               TYPE ekpo-netpr,                          "단가
         waers               TYPE ekko-waers,                          "통화
         pbxx_unitprice      TYPE zsvcmm_price-pbxx_unitprice,
         pb00_unitprice      TYPE zsvcmm_price-pb00_unitprice,
         pbxx                TYPE zsvcmm_price-pbxx,
         pb00                TYPE zsvcmm_price-pb00,
         peinh               TYPE ekpo-peinh,                          "가격단위
         bprme               TYPE ekpo-bprme,                          "OPU
         brtwr               TYPE ekpo-brtwr,                          "품목금액
         zcpr                TYPE p LENGTH 15 DECIMALS 2, " zsvcmm_price-zcpr,  "품목가감액
         adj_net             TYPE ekpo-netpr,                          "조정단가(조정금액/품목수량)
         adj_amt             TYPE ekpo-brtwr,                          "조정금액(품목금액+품목가감액)
*         change              TYPE zsvcmm_price-zprice_change,          "조정사유코드
         change_text         TYPE val_text,
*         reason              TYPE zsvcmm_price-zprice_reason,          "조정사유상세
         total_amt           TYPE p LENGTH 15 DECIMALS 2, " 공급가액
         mwskz               TYPE ekpo-mwskz,                          "세금코드
         mwskz_text          TYPE t007s-text1,
         ztax                TYPE zsvcmm_price-ztax,                   "VAT(ZTAX+NAVS)
         cont_amt            TYPE ekpo-brtwr,                          "계약금액(VAT포함)(품목금액+조정금액+VAT)
         eindt               TYPE eket-eindt,                          "납품일
         werks               TYPE ekpo-werks,                          "플랜트
         werks_text          TYPE t001w-name1,
         lgort               TYPE ekpo-lgort,                          "창고
         lgobe               TYPE t001l-lgobe,
         matkl               TYPE ekpo-matkl,                          "자재그룹
         wgbez               TYPE t023t-wgbez,

*         zinspection         TYPE ekpo-zinspection,                    "검사여부
*         zqm_department      TYPE ekpo-zqm_department,                 "검수부서
*         zqm_department_name TYPE zsvmm_user_info-depart_name,

         banfn               TYPE eban-banfn,                          "구매요청
         bnfpo               TYPE eban-bnfpo,                          "구매요청품목
         konnr               TYPE ekpo-konnr,                          "계약번호
         ktpnr               TYPE ekpo-ktpnr,                          "계약품목
         emlif               TYPE ekpo-emlif,
         glaccount           TYPE cpoacctasstp-glaccount,              "G/L계정
         glaccount_text      TYPE skat-txt20,
         wbselement          TYPE cpoacctasstp-wbselementexternalid,   "WBS
         wbselement_text     TYPE prps-post1,
         costcenter          TYPE cpoacctasstp-costcenter,             "코스트센터
         costcenter_text     TYPE cskt-ktext,
         orderid             TYPE cpoacctasstp-orderid,                "오더
         orderid_text        TYPE aufk-ktext,
         salesorder          TYPE cpoacctasstp-salesorder,             "영업오더
         salesorderitem      TYPE cpoacctasstp-salesorderitem,         "영업오더품목
         fixedasset          TYPE cpoacctasstp-masterfixedasset,       "자산번호
         fixedasset_text     TYPE anla-txt50,
         fundscenter         TYPE cpoacctasstp-fundscenter,          "자금관리센터
         commitmentitem      TYPE cpoacctasstp-commitmentitem, "약정항목

         zpri                TYPE zsvcmm_price-zpri,                   "인쇄교체비
         zsup_unitprice      TYPE ekpo-brtwr,                          "임가공 가공단가 "ZSVCMM_PRICE-ZSUP_UNITPRICE,
         zsup                TYPE zsvcmm_price-zsup,                   "임가공 가공비
         zsum_unitprice      TYPE ekpo-brtwr,                          "임가공 원료단가 "ZSVCMM_PRICE-ZSUM_UNITPRICE,
         zsum                TYPE zsvcmm_price-zsum,                   "임가공 원료비
         zsur_unitprice      TYPE ekpo-brtwr,                          "임가공재료품단가 "ZSVCMM_PRICE-ZSUR_UNITPRICE,
         zsur                TYPE zsvcmm_price-zsur,                   "임가공재료품비

         navs                TYPE zsvcmm_price-navs,                   "불공제여부
         navs_text,
         bstae               TYPE ekpo-bstae,                          "납품서생성

         zcu1_rate           TYPE zsvcmm_price-zcu1_rate,              "관세율_ST
         zcu1                TYPE zsvcmm_price-zcu1,                   "관세금액_ST
         zfr1_rate           TYPE zsvcmm_price-zfr1_rate,              "운임율_ST
         zfr1                TYPE zsvcmm_price-zfr1,                   "운임금액_ST
         zin1_rate           TYPE zsvcmm_price-zin1_rate,              "보험율_ST
         zin1                TYPE zsvcmm_price-zin1,                   "보험금액_ST
         zot1_rate           TYPE zsvcmm_price-zot1_rate,              "기타부대비율_ST
         zot1                TYPE zsvcmm_price-zot1,                   "기타부대비금액_ST

         retpo               TYPE ekpo-retpo,                          "반품품목
         repos               TYPE ekpo-repos,                          "무상품목
         pstyp               TYPE ekpo-pstyp,                          "품목범주(Internal)
         epstp               TYPE t163y-epstp,                         "품목범주(External)
         ptext               TYPE t163y-ptext,
         knttp               TYPE ekpo-knttp,                          "계정지정범주
         elikz               TYPE ekpo-elikz,                          "납품완료
         uebto               TYPE ekpo-uebto,                          "초과납품
         webre               TYPE ekpo-webre,                          "GR기준IV

         text_f04            TYPE icon-id,                             "납품 텍스트

*         price_stamp         TYPE ztmm33022-price_stamp,
*         sales_price         TYPE ztmm33022-sales_price,
*         konwa               TYPE ztmm33022-konwa,
*         cons_fo             TYPE ztmm33022-cons_fo,
*         unit_sz             TYPE ztmm33022-unit_sz,
*         unit_un             TYPE ztmm33022-unit_un,
*         box_qty             TYPE ztmm33022-box_qty,
*         box_unit            TYPE ztmm33022-box_unit,
*         set_qty             TYPE ztmm33022-set_qty,
*         set_unit            TYPE ztmm33022-set_unit,
*         add_qty             TYPE ztmm33022-add_qty,
*         add_unit            TYPE ztmm33022-add_unit,
*         produce_no1         TYPE ztmm33022-produce_no1,
*         produce_no2         TYPE ztmm33022-produce_no2,
*         history_tr1         TYPE ztmm33022-history_tr1,
*         history_tr2         TYPE ztmm33022-history_tr2,
*         com_item            TYPE ztmm33022-com_item,

       END OF ty_data_rb.

* 아이템 텍스트
TYPES: BEGIN OF ty_text,
         line TYPE tdline,
       END OF ty_text.

"*- Alv Dispaly
TYPES: BEGIN OF ty_disp_ra.
         include TYPE zscn00003.  "필수!! ALV Changed Common Struc.. (STATU/CELLS/CELLC/MSGTB/DCFLG)
         INCLUDE TYPE ty_data_ra.    "Table 에 ZDELE가 없을 경우 필드 추가 필수
"Dcflg=> C:Create U:Update T:라인삭제 '':Original
TYPES: cdlst(30),
         zdele,                  "필수필드임 (테이블에 있을경우는 생략)
       END OF ty_disp_ra.

TYPES: BEGIN OF ty_disp_rb.
         include TYPE zscn00003.  "필수!! ALV Changed Common Struc.. (STATU/CELLS/CELLC/MSGTB/DCFLG)
         INCLUDE TYPE ty_data_rb.    "Table 에 ZDELE가 없을 경우 필드 추가 필수
"Dcflg=> C:Create U:Update T:라인삭제 '':Original
TYPES: cdlst(30),
         zdele,                  "필수필드임 (테이블에 있을경우는 생략)
       END OF ty_disp_rb.

* 첨부파일 정보
TYPES: BEGIN OF ty_atta,
         typeid_a TYPE sibftypeid,
         instid_a TYPE sibfboriid,
       END OF ty_atta.

* 결재 진행 정보
*TYPES: BEGIN OF ty_apv,
*         apvifkey  TYPE ztmm30031-apvifkey,
*         fiid      TYPE ztmm30031-fiid,
*         wfobject  TYPE ztmm30032-wfobject,
*         apvstatus TYPE ztmm30031-apvstatus,
*       END OF ty_apv.

* 공동수급업체
TYPES: BEGIN OF ty_ekpa,
         ebeln TYPE ekpa-ebeln,
         parvw TYPE ekpa-parvw,
         lifn2 TYPE ekpa-lifn2,
         name1 TYPE lfa1-name1,
       END OF ty_ekpa.

* 담당자이관 Popup
TYPES: BEGIN OF ty_trans,
         order,
         expen,
         qm ,
*         employ_no     TYPE zsvmm_user_info-employ_no,
*         employ_name   TYPE zsvmm_user_info-employ_name,
*         department    TYPE zsvmm_user_info-department,
*         depart_name   TYPE zsvmm_user_info-depart_name,
*
*         b_employ_no   TYPE zsvmm_user_info-employ_no,
*         b_employ_name TYPE zsvmm_user_info-employ_name,
       END OF ty_trans.

* 일괄변경 Popup
TYPES: BEGIN OF ty_mass,
         bstae     TYPE ekpo-bstae,   "확인관리
         bstae_del TYPE db2decision,
         uebto     TYPE ekpo-uebto,   "초과납품허용한도
         uebto_del TYPE db2decision,
         lgort     TYPE lgort_d,
         werks     TYPE werks_d,
         lifnr     TYPE lfa1-lifnr,
         abs, "변경 체크
         zma, "변경 체크
         absgr     TYPE mepo1229-absgr,
         atext     TYPE t165m-absgr_txt,
         zemanage2 TYPE ekko-zemanage2,
       END OF ty_mass.

*TYPES: BEGIN OF ty_his_head.
*         include structure ztmm33020.
*TYPES: END OF ty_his_head.
*
*TYPES: BEGIN OF ty_his_item.
*         include structure ztmm33021.
*TYPES: END OF ty_his_item.

TYPES : BEGIN OF ty_sub_comp.
TYPES :
  rsnum  TYPE ippsubcontrcomp-reservation,
  rspos  TYPE ippsubcontrcomp-reservationitem,
  matnr  TYPE ippsubcontrcomp-material,
  maktx  TYPE makt-maktx,
  assem  TYPE ippsubcontrcomp-assembly,
  idnrk  TYPE ippsubcontrcomp-material,
  idntx  TYPE makt-maktx,
  icon   TYPE icon-id,
  erfme  TYPE  ippsubcontrcomp-entryunit,
  bdmng  TYPE mseg-lbkum, " ippsubcontrcomp-requiredquantity,  "소요량
  phitem TYPE ippsubcontrcomp-materialcomponentisphantomitem,  "가상 품목 지시자
  aufwg  TYPE ippsubcontrcomp-orderpathvalue,  "오더경로
  aufst  TYPE ippsubcontrcomp-orderlevelvalue,   "오더레벨
  bauwg  TYPE ippsubcontrcomp-assemblyorderpathvalue,   "하위레벨
  comq   TYPE ippsubcontrcomp-materialcomporiginalquantity.
  include TYPE zscn00003.
TYPES : END OF ty_sub_comp.

*---------------------
*-- Redefine
*---------------------
*TYPES: BEGIN OF ty_user,
*         emp_no  TYPE ztcn00002-emp_no,
*         user_nm TYPE ztcn00002-user_nm,
*       END OF ty_user.
*
*TYPES: BEGIN OF ty_dept,
*         orgn_cd TYPE ztcn00001-orgn_cd,
*         orgn_nm TYPE ztcn00001-orgn_nm,
*       END OF ty_dept.

*----------------------------------------------------------------------*
* Class
*----------------------------------------------------------------------*

*---------------------
*-- ALV Object
*---------------------
DATA: grf_docking_con TYPE REF TO cl_gui_docking_container.

DATA: grf_head TYPE REF TO cl_gui_container, "Header Information Object
      grf_body TYPE REF TO cl_gui_container.    "ALV Object

DATA: grf_grid TYPE REF TO lcl_cust_alv_grid. " ALV Grid

DATA: grf_his_h_con TYPE REF TO cl_gui_custom_container.
DATA: grf_his_h_grid TYPE REF TO lcl_cust_alv_grid. " ALV Grid
DATA: grf_his_i_con TYPE REF TO cl_gui_custom_container.
DATA: grf_his_i_grid TYPE REF TO lcl_cust_alv_grid. " ALV Grid

DATA: grf_subc_con TYPE REF TO cl_gui_custom_container.
DATA: grf_subc_grid TYPE REF TO lcl_cust_alv_grid. " ALV Grid

* Text Editor
DATA : grf_cont_text TYPE REF TO cl_gui_custom_container,
       grf_text_edit TYPE REF TO cl_gui_textedit.

*----------------------------------------------------------------------*
* Global Data
*----------------------------------------------------------------------*
DATA: gt_disp_ra TYPE TABLE OF ty_disp_ra,
      gt_disp_rb   TYPE TABLE OF ty_disp_rb,
      gt_atta      TYPE HASHED TABLE OF ty_atta WITH UNIQUE KEY typeid_a instid_a,  "첨부파일 정보
*      gt_apv       TYPE TABLE OF ty_apv,   "결재 진행 정보
*      gt_his_head  TYPE TABLE OF ty_his_head,
*      gt_his_item  TYPE TABLE OF ty_his_item,
      gt_subcomp   TYPE TABLE OF ty_sub_comp,
      gt_subcomp_p TYPE TABLE OF ty_sub_comp.

DATA: gt_text_f04 TYPE TABLE OF ty_text, "납품 텍스트
      gv_text_mode. "C:변경, D:조회

DATA: gs_trans TYPE ty_trans.

DATA: gs_mass TYPE ty_mass.

DATA: gt_t052u TYPE TABLE OF t052u, "지급조건
      gt_tinct TYPE TABLE OF tinct,   "인도조건
      gt_t024  TYPE TABLE OF t024,    "구매그룹
      gt_ekpa  TYPE TABLE OF ty_ekpa.

DATA: gt_skat TYPE TABLE OF skat, "G/L계정
      gt_prps TYPE TABLE OF prps,   "WBS
      gt_cskt TYPE TABLE OF cskt,   "코스트센터
      gt_aufk TYPE TABLE OF aufk,   "오더
      gt_anla TYPE TABLE OF anla.   "자산번호

DATA: gt_taxcode TYPE TABLE OF cmmtaxcodevh,
      gt_t001w   TYPE TABLE OF t001w,
      gt_t001l   TYPE TABLE OF t001l,
      gt_t023t   TYPE TABLE OF t023t,
      gt_stxl    TYPE TABLE OF stxl.
*      gt_33022   TYPE TABLE OF ztmm33022.

*DATA: gt_user_ord TYPE TABLE OF ty_user,
*      gt_user_exp TYPE TABLE OF ty_user,
*      gt_user_qm  TYPE TABLE OF ty_user,
*      gt_dept_ord TYPE TABLE OF ty_dept,
*      gt_dept_exp TYPE TABLE OF ty_dept,
*      gt_dept_qm  TYPE TABLE OF ty_dept.

DATA: gr_frgke TYPE RANGE OF ekko-frgke,
      gr_pstyp TYPE RANGE OF ekpo-pstyp,
      gr_loekz TYPE RANGE OF ekpo-loekz.

DATA: gv_tech TYPE char02. "국내/해외 필드 숨김

DATA: gs_close_key TYPE ekpo_key.

DATA: ok_code TYPE sy-ucomm,
      gv_ok_code TYPE sy-ucomm,
*      GV_MODE    TYPE C VALUE '',    "Display :D  Edit:E (화면편집)
      gv_mrow    TYPE i VALUE 10,    "Multi Row
      gv_expand.

* Selected Rows
DATA: gt_rows TYPE lvc_t_row.

DATA: gv_exc_user TYPE c.

DATA gv_mail TYPE c.

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
