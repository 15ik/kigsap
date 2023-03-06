*&---------------------------------------------------------------------*
*& Include          ZRMM3020TOP
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
* 주의: ALV Display Table은 반드시 Types 선언으로 해주세요
*       인터널 테이블은 Header Line Table 금지
*----------------------------------------------------------------------*

TABLES: ekko, ekpo, ekbe, lfa1, mbew.

*----------------------------------------------------------------------*
* Local Class Define..
*----------------------------------------------------------------------*
CLASS: lcl_cust_alv_grid DEFINITION DEFERRED.

*----------------------------------------------------------------------*
* Constants
*----------------------------------------------------------------------*

CONSTANTS: gc_dynnr_1000 TYPE sy-dynnr VALUE '1000'.

CONSTANTS: gc_cuky_krw TYPE tcurc-waers VALUE 'KRW',
           gc_ktopl_k000 TYPE skat-ktopl  VALUE '1000'.

CONSTANTS: gc_1000 TYPE char04 VALUE '1000'.

CONSTANTS: gc_criteria_all TYPE fieldname VALUE 'ALL',
           gc_criteria_lifnr TYPE fieldname VALUE 'LIFNR',
           gc_criteria_matnr TYPE fieldname VALUE 'MATNR',
           gc_criteria_matkl TYPE fieldname VALUE 'MATKL',
           gc_criteria_order TYPE fieldname VALUE 'ORDER',
           gc_criteria_depto TYPE fieldname VALUE 'DEPTO'.

* ALV 관련 상수
CONSTANTS: gc_emphsz_c300 TYPE lvc_emphsz VALUE 'C300', "노란색
           gc_emphsz_c500 TYPE lvc_emphsz VALUE 'C500', "초록색
           gc_emphsz_c700 TYPE lvc_emphsz VALUE 'C700'. "빨간색

* GOS 관련 상수
CONSTANTS: gc_gos_typeid_po TYPE swo_objtyp VALUE 'BUS2012', "SWO1 등록한 OBJECT TYPE
           gc_gos_typeid_co TYPE swo_objtyp VALUE 'BUS2014',
           gc_gos_catid     TYPE sibfcatid  VALUE 'BO'.      "Category of Objects "Instances of BOR Object Types

*----------------------------------------------------------------------*
* Type
*----------------------------------------------------------------------*
TYPES: BEGIN OF ty_data,
         ebeln                  TYPE ekko-ebeln,                    "구매오더번호
         ebelp                  TYPE ekpo-ebelp,                    "구매오더품번
         zemanage2              TYPE ekko-zemanage2,                "관리번호

         apv_stat               TYPE icon-id,                       "결재진행
         status                 TYPE icon-id,

         bukrs                  TYPE ekko-bukrs,
         ekorg                  TYPE ekko-ekorg,
         bedat                  TYPE ekko-bedat,                    "발주일
         bstyp                  TYPE ekko-bstyp,                    "구매문서범주
         bsart                  TYPE ekko-bsart,                    "구매문서유형
         batxt                  TYPE t161t-batxt,
         lifnr                  TYPE ekko-lifnr,                    "공급업체
         lifnr_text             TYPE lfa1-name1,                    "업체명
         kalsk                  TYPE lfm1-kalsk,                    "스키마그룹
         kalsb                  TYPE tmkkt-kalsb,
         matkl                  TYPE ekpo-matkl,                    "자재그룹
         wgbez                  TYPE t023t-wgbez60,                 "자재그룹명
         matnr                  TYPE ekpo-matnr,                    "자재
         txz01                  TYPE ekpo-txz01,                    "자재내역
         bwtar                  TYPE ekpo-bwtar,                    "평가유형
         bklas                  TYPE mbew-bklas,                    "평가클래스

         budat                  TYPE ekbe-budat,                    "전기일
         elikz                  TYPE ekpo-elikz,                    "납품완료지시자
         elikz_icon             TYPE icon-id,
         menge                  TYPE p DECIMALS 3,                  "발주수량
         menge_gr_sum(16)       TYPE p DECIMALS 3,                  "입고수량합
         menge_iv_sum(16)       TYPE p DECIMALS 3,                  "송장수량합
         menge_op_gr_sum(16)    TYPE p DECIMALS 3,                  "미입고수량합
         menge_op_sum(16)       TYPE p DECIMALS 3,                  "미착수량합
         meins                  TYPE ekpo-meins,                    "단위

         netpr                  TYPE ekpo-netpr,                    "단가
         netwr                  TYPE bseg-dmbtr,                    "발주금액
         wrbtr_gr_sum(16)       TYPE p DECIMALS 2,                  "입고금액합(전표)
         wrbtr_iv_sum(16)       TYPE p DECIMALS 2,                  "송장금액합(전표)
         wrbtr_op_sum(16)       TYPE p DECIMALS 2,                  "미착금액합(전표)
         waers                  TYPE ekko-waers,                    "통화
         dmbtr_gr_sum(16)       TYPE p DECIMALS 2,                  "입고금액합(KRW)
         dmbtr_iv_sum(16)       TYPE p DECIMALS 2,                  "송장금액합(KRW)
         dmbtr_op_sum(16)       TYPE p DECIMALS 2,                  "미착금액합(KRW)
         dmbtr_grir_sum(16)     TYPE p DECIMALS 2,                  "GR/IR반제차이(KRW)

         actex_sum(16)          TYPE p DECIMALS 2,                  "실비금액

         zterm                  TYPE ekko-zterm,                    "지급조건
         vtext                  TYPE tvzbt-vtext,
         infnr                  TYPE ekpo-infnr,                    "정보레코드
         zorder_person          TYPE ekko-zorder_person,            "발주자
*         zorder_person_name     TYPE zsvmm_user_info-employ_name,
         zorder_department      TYPE ekko-zorder_department,        "발주부서
*         zorder_department_name TYPE zsvmm_user_info-depart_name,

         konnr                  TYPE ekpo-konnr,                    "계약번호
         ktpnr                  TYPE ekpo-ktpnr,

         knttp                  TYPE ekpo-knttp,                    "계정지정범주
         knttx                  TYPE t163i-knttx,
         sakto                  TYPE ekkn-sakto,                    "GL 계정
         sakto_text             TYPE skat-txt20,
         prctr                  TYPE ekkn-prctr,                    "손익센터
         prctr_text             TYPE cepct-ktext,
         kostl                  TYPE ekkn-kostl,                    "코스트센터
         kostl_text             TYPE cskt-ktext,
         fistl                  TYPE ekkn-fistl,                    "자금 관리 센터
         fistl_text             TYPE fmfctrt-bezeich,
         fipos                   TYPE ekkn-fipos, "약정항목


         vbeln                  TYPE ekkn-vbeln,                    "판매오더번호
         vbelp                  TYPE ekkn-vbelp,                    "품목

         anln1                  TYPE ekkn-anln1,                    "자산번호
         anln1_text             TYPE anla-txt50,
         pspnr                  TYPE ekkn-ps_psp_pnr,               "WBS 요소
         pspnr_text             TYPE prps-post1,
         aufnr                  TYPE ekkn-aufnr,                    "Internal Order
         aufnr_text             TYPE aufk-ktext,

         retpo                  TYPE ekpo-retpo,                    "반품품목
         repos                  TYPE ekpo-repos,                    "송장수령(무상품목 지시자로 사용)

         gjahr_ekbe             TYPE ekbe-gjahr,
         belnr_ekbe             TYPE ekbe-belnr,
         menge_ekbe             TYPE ekbe-menge,
         wrbtr_ekbe             TYPE ekbe-wrbtr,
         dmbtr_ekbe             TYPE ekbe-dmbtr,
         arewr_ekbe             TYPE ekbe-arewr,
         waers_ekbe             TYPE ekbe-waers,

         werks                  TYPE ekpo-werks,                    "플랜트
         charg                  TYPE eket-charg,                    "배치
         vgabe                  TYPE ekbe-vgabe,                    "트랜잭셕
         shkzg                  TYPE ekbe-shkzg,                    "차/대구분
         bwart                  TYPE ekbe-bwart,                    "이동유형
         pstyp                  TYPE ekpo-pstyp,                    "품목범주
         ekgrp                  TYPE ekko-ekgrp,                    "구매그룹

*U2(T2)> LOEKZ 추가(Include deleted PO 조건 시 화면 표시위함 - Start
         loekz                  TYPE ekpo-loekz,                    "삭제지시자
*U2(T2)> LOEKZ 추가(Include deleted PO 조건 시 화면 표시위함 - End
       END OF ty_data.

* 계정지정 정보
TYPES: BEGIN OF ty_ekkn,
         ebeln      TYPE ekkn-ebeln,              "구매오더번호
         ebelp      TYPE ekkn-ebelp,              "품목
         kokrs      TYPE ekkn-kokrs,              "관리회계영역

         sakto      TYPE ekkn-sakto,              "GL 계정
         sakto_text TYPE skat-txt20,
         prctr      TYPE ekkn-prctr,              "손익센터
         prctr_text TYPE cepct-ktext,
         kostl      TYPE ekkn-kostl,              "코스트센터
         kostl_text TYPE cskt-ktext,
         fistl      TYPE ekkn-fistl,              "자금 관리 센터
         fistl_text TYPE fmfctrt-bezeich,
         fipos      TYPE ekkn-fipos,              "자금 관리 센터


         vbeln      TYPE ekkn-vbeln,              "판매오더번호
         vbelp      TYPE ekkn-vbelp,              "품목

         anln1      TYPE ekkn-anln1,              "자산번호
         anln1_text TYPE anla-txt50,
         pspnr      TYPE ekkn-ps_psp_pnr,         "WBS 요소
         pspnr_text TYPE prps-post1,
         aufnr      TYPE ekkn-aufnr,              "Internal Order
         aufnr_text TYPE aufk-ktext,
       END OF ty_ekkn.

* 업체별 SUM
TYPES: BEGIN OF ty_lifnr_sum,
         lifnr               TYPE ekko-lifnr,     "공급업체
         lifnr_text          TYPE lfa1-name1,     "업체명
         kalsk               TYPE lfm1-kalsk,     "스키마그룹
         kalsb               TYPE tmkkt-kalsb,

         menge               TYPE p DECIMALS 3,   "발주수량
         meins               TYPE ekpo-meins,     "발주단위
         waers               TYPE ekko-waers,     "통화
         netwr               TYPE bseg-dmbtr,     "총가격

         menge_op_gr_sum(16) TYPE p DECIMALS 3,   "미입고수량합
         menge_gr_sum(16)    TYPE p DECIMALS 3,   "입고수량합
         wrbtr_gr_sum(16)    TYPE p DECIMALS 2,   "입고금액합(전표)
         dmbtr_gr_sum(16)    TYPE p DECIMALS 2,   "입고금액합(KRW)

         menge_iv_sum(16)    TYPE p DECIMALS 3,   "송장수량합
         wrbtr_iv_sum(16)    TYPE p DECIMALS 2,   "송장금액합(전표)
         dmbtr_iv_sum(16)    TYPE p DECIMALS 2,   "송장금액합(KRW)

         menge_op_sum(16)    TYPE p DECIMALS 3,   "미착수량합
         wrbtr_op_sum(16)    TYPE p DECIMALS 2,   "미착금액합(전표)
         dmbtr_op_sum(16)    TYPE p DECIMALS 2,   "미착금액합(KRW)
         dmbtr_grir_sum(16)  TYPE p DECIMALS 2,   "GR/IR반제차이(KRW)

*         WAERS_EKBE          LIKE EKBE-WAERS,
       END OF ty_lifnr_sum.

* 자재별 SUM
TYPES: BEGIN OF ty_matnr_sum,
         matkl               TYPE ekpo-matkl,     "자재그룹
         wgbez               TYPE t023t-wgbez60,  "자재그룹명
         matnr               TYPE ekpo-matnr,     "자재
         txz01               TYPE ekpo-txz01,     "자재내역
         kalsk               TYPE lfm1-kalsk,     "스키마그룹
         kalsb               TYPE tmkkt-kalsb,

         menge               TYPE p DECIMALS 3,   "발주수량
         meins               TYPE ekpo-meins,     "발주단위
         waers               TYPE ekko-waers,     "통화
         netwr               TYPE bseg-dmbtr,     "총가격

         menge_op_gr_sum(16) TYPE p DECIMALS 3,   "미입고수량합
         menge_gr_sum(16)    TYPE p DECIMALS 3,   "입고수량합
         wrbtr_gr_sum(16)    TYPE p DECIMALS 2,   "입고금액합(전표)
         dmbtr_gr_sum(16)    TYPE p DECIMALS 2,   "입고금액합(KRW)

         menge_iv_sum(16)    TYPE p DECIMALS 3,   "송장수량합
         wrbtr_iv_sum(16)    TYPE p DECIMALS 2,   "송장금액합(전표)
         dmbtr_iv_sum(16)    TYPE p DECIMALS 2,   "송장금액합(KRW)

         menge_op_sum(16)    TYPE p DECIMALS 3,   "미착수량합
         wrbtr_op_sum(16)    TYPE p DECIMALS 2,   "미착금액합(전표)
         dmbtr_op_sum(16)    TYPE p DECIMALS 2,   "미착금액합(KRW)
         dmbtr_grir_sum(16)  TYPE p DECIMALS 2,   "GR/IR반제차이(KRW)

*         WAERS_EKBE          LIKE EKBE-WAERS,
       END OF ty_matnr_sum.

* 자재그룹별 SUM
TYPES: BEGIN OF ty_matkl_sum,
         matkl               TYPE ekpo-matkl,     "자재그룹
         wgbez               TYPE t023t-wgbez60,  "자재그룹명
         kalsk               TYPE lfm1-kalsk,     "스키마그룹
         kalsb               TYPE tmkkt-kalsb,

         menge               TYPE p DECIMALS 3,   "발주수량
         meins               TYPE ekpo-meins,     "발주단위
         waers               TYPE ekko-waers,     "통화
         netwr               TYPE bseg-dmbtr,     "총가격

         menge_op_gr_sum(16) TYPE p DECIMALS 3,   "미입고수량합
         menge_gr_sum(16)    TYPE p DECIMALS 3,   "입고수량합
         wrbtr_gr_sum(16)    TYPE p DECIMALS 2,   "입고금액합(전표)
         dmbtr_gr_sum(16)    TYPE p DECIMALS 2,   "입고금액합(KRW)

         menge_iv_sum(16)    TYPE p DECIMALS 3,   "송장수량합
         wrbtr_iv_sum(16)    TYPE p DECIMALS 2,   "송장금액합(전표)
         dmbtr_iv_sum(16)    TYPE p DECIMALS 2,   "송장금액합(KRW)

         menge_op_sum(16)    TYPE p DECIMALS 3,   "미착수량합
         wrbtr_op_sum(16)    TYPE p DECIMALS 2,   "미착금액합(전표)
         dmbtr_op_sum(16)    TYPE p DECIMALS 2,   "미착금액합(KRW)
         dmbtr_grir_sum(16)  TYPE p DECIMALS 2,   "GR/IR반제차이(KRW)

*         WAERS_EKBE          LIKE EKBE-WAERS,
       END OF ty_matkl_sum.

* 발주담당자별 SUM
TYPES: BEGIN OF ty_order_sum,
         zorder_person       TYPE ekko-zorder_person,           "발주담당자
*         zorder_person_name  TYPE zsvmm_user_info-employ_name,
         kalsk               TYPE lfm1-kalsk,                   "스키마그룹
         kalsb               TYPE tmkkt-kalsb,

         menge               TYPE p DECIMALS 3,                 "발주수량
         meins               TYPE ekpo-meins,                   "발주단위
         waers               TYPE ekko-waers,                   "통화
         netwr               TYPE bseg-dmbtr,                   "총가격

         menge_op_gr_sum(16) TYPE p DECIMALS 3,                 "미입고수량합
         menge_gr_sum(16)    TYPE p DECIMALS 3,                 "입고수량합
         wrbtr_gr_sum(16)    TYPE p DECIMALS 2,                 "입고금액합(전표)
         dmbtr_gr_sum(16)    TYPE p DECIMALS 2,                 "입고금액합(KRW)

         menge_iv_sum(16)    TYPE p DECIMALS 3,                 "송장수량합
         wrbtr_iv_sum(16)    TYPE p DECIMALS 2,                 "송장금액합(전표)
         dmbtr_iv_sum(16)    TYPE p DECIMALS 2,                 "송장금액합(KRW)

         menge_op_sum(16)    TYPE p DECIMALS 3,                 "미착수량합
         wrbtr_op_sum(16)    TYPE p DECIMALS 2,                 "미착금액합(전표)
         dmbtr_op_sum(16)    TYPE p DECIMALS 2,                 "미착금액합(KRW)
         dmbtr_grir_sum(16)  TYPE p DECIMALS 2,                 "GR/IR반제차이(KRW)

*         WAERS_EKBE          LIKE EKBE-WAERS,
       END OF ty_order_sum.

* 발주부서별 SUM
TYPES: BEGIN OF ty_depto_sum,
         zorder_department      TYPE ekko-zorder_department,        "발주부서
*         zorder_department_name TYPE zsvmm_user_info-depart_name,
         kalsk                  TYPE lfm1-kalsk,                    "스키마그룹
         kalsb                  TYPE tmkkt-kalsb,

         menge                  TYPE p DECIMALS 3,                  "발주수량
         meins                  TYPE ekpo-meins,                    "발주단위
         waers                  TYPE ekko-waers,                    "통화
         netwr                  TYPE bseg-dmbtr,                    "총가격

         menge_op_gr_sum(16)    TYPE p DECIMALS 3,                  "미입고수량합
         menge_gr_sum(16)       TYPE p DECIMALS 3,                  "입고수량합
         wrbtr_gr_sum(16)       TYPE p DECIMALS 2,                  "입고금액합(전표)
         dmbtr_gr_sum(16)       TYPE p DECIMALS 2,                  "입고금액합(KRW)

         menge_iv_sum(16)       TYPE p DECIMALS 3,                  "송장수량합
         wrbtr_iv_sum(16)       TYPE p DECIMALS 2,                  "송장금액합(전표)
         dmbtr_iv_sum(16)       TYPE p DECIMALS 2,                  "송장금액합(KRW)

         menge_op_sum(16)       TYPE p DECIMALS 3,                  "미착수량합
         wrbtr_op_sum(16)       TYPE p DECIMALS 2,                  "미착금액합(전표)
         dmbtr_op_sum(16)       TYPE p DECIMALS 2,                  "미착금액합(KRW)
         dmbtr_grir_sum(16)     TYPE p DECIMALS 2,                  "GR/IR반제차이(KRW)

*         WAERS_EKBE             LIKE EKBE-WAERS,
       END OF ty_depto_sum.

* 계정별 SUM
TYPES: BEGIN OF ty_bklas_sum,
         bklas            TYPE mbew-bklas,     "평가클래스
         bkbez            TYPE t025t-bkbez,    "평가클래스 내역
         konts            TYPE t030-konts,     "G/L계정번호
         txt20            TYPE skat-txt20,

         dmbtr_op_sum(16) TYPE p DECIMALS 2,   "미착금액합(KRW)
         waers            TYPE ekko-waers,
       END OF ty_bklas_sum.

"*- Alv Dispaly
TYPES: BEGIN OF ty_disp.
         include TYPE zscn00003.  "필수!! ALV Changed Common Struc.. (STATU/CELLS/CELLC/MSGTB/DCFLG)
         INCLUDE TYPE ty_data.    "Table 에 ZDELE가 없을 경우 필드 추가 필수
"Dcflg=> C:Create U:Update T:라인삭제 '':Original
TYPES: cdlst(30),
         zdele,                  "필수필드임 (테이블에 있을경우는 생략)
       END OF ty_disp.

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

DATA: gt_fcat TYPE lvc_t_fcat.

* 계정별 미착현황 Popup
DATA: grf_cont_bklas TYPE REF TO cl_gui_custom_container,
      grf_grid_bklas TYPE REF TO lcl_cust_alv_grid.        " ALV Grid

*----------------------------------------------------------------------*
* Global Data
*----------------------------------------------------------------------*
DATA: gr_kalsk TYPE RANGE OF lfm1-kalsk,
      gr_loekz TYPE RANGE OF eloek,
      gr_retpo TYPE RANGE OF ekpo-retpo,
      gr_repos TYPE RANGE OF ekpo-repos,
      gr_bstyp TYPE RANGE OF ekpo-bstyp.

DATA: gt_disp TYPE TABLE OF ty_disp,
      gt_data TYPE TABLE OF ty_disp,
      gt_all  TYPE TABLE OF ty_disp.

DATA: gt_atta TYPE HASHED TABLE OF ty_atta WITH UNIQUE KEY typeid_a instid_a. "첨부파일 정보
*      gt_apv  TYPE TABLE OF ty_apv.   "결재 진행 정보

DATA: gt_lifnr_sum TYPE TABLE OF ty_lifnr_sum,
      gt_matnr_sum TYPE TABLE OF ty_matnr_sum,
      gt_matkl_sum TYPE TABLE OF ty_matkl_sum,
      gt_order_sum TYPE TABLE OF ty_order_sum,
      gt_depto_sum TYPE TABLE OF ty_depto_sum,
      gt_bklas_sum TYPE TABLE OF ty_bklas_sum.

DATA: gt_lfa1 TYPE TABLE OF lfa1,
      gt_tmkkt     TYPE TABLE OF tmkkt,
      gt_makt      TYPE TABLE OF makt,
      gt_t161t     TYPE TABLE OF t161t,
      gt_t052u     TYPE TABLE OF t052u,
      gt_t163i     TYPE TABLE OF t163i,
      gt_ekkn      TYPE TABLE OF ty_ekkn,
      gt_t023t     TYPE TABLE OF t023t.

DATA: gv_criteria TYPE fieldname,
      gv_tech     TYPE char05.

DATA: ok_code TYPE sy-ucomm,
      gv_ok_code TYPE sy-ucomm,
*      GV_MODE    TYPE C VALUE '',    "Display :D  Edit:E (화면편집)
      gv_mrow    TYPE i VALUE 10.    "Multi Row

*U1> 입고기준/송장기준 검색 기능 추가
DATA: GT_GET_BASIC_PO TYPE TABLE OF EXP_PO_ITEM.

*'KRW' 대신 Local Currency 로 적용
DATA: GV_LOCAL_WAERS TYPE T001-WAERS.

* Selected Rows
*DATA: GT_ROWS TYPE LVC_T_ROW.

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
