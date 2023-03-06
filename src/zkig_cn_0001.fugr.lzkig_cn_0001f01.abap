*----------------------------------------------------------------------*
***INCLUDE LZFGMM6040F01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form header_info
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM header_info TABLES ct_return STRUCTURE bapiret2
                              USING is_poheader TYPE zsmm_poheader
                                       is_poheaderx TYPE zsmm_poheaderx.

**poheader
  gs_poheader  = VALUE #(
                         po_number            =  is_poheader-ebeln
                         comp_code            =  is_poheader-bukrs  "회사 코드
                         doc_type             =  is_poheader-esart   "구매 문서 유형
                         creat_date           =  sy-datum
                         created_by           =  sy-uname
                         vendor               =  is_poheader-elifn  "공급업체 계정 번호
                         langu                =  is_poheader-spras  "언어 키
                         item_intvl           =  is_poheader-intvl  "품목 번호 구간
                         pmnttrms             =  is_poheader-zterm  "지급 조건 키
                         purch_org            =  is_poheader-ekorg  "구매 조직
                         pur_group            =  is_poheader-bkgrp  "구매 그룹
                         currency             =  is_poheader-waers  "통화 키
                         doc_date             =  is_poheader-ebdat  "구매 문서 증빙일
                         vper_start           =  is_poheader-kdatb  "계약 시작일
                         vper_end             =  is_poheader-kdate  "계약 종료일
                         warranty             =  is_poheader-gwldt  "대금지급 예정일
                         quotation            =  is_poheader-angnr  "견적 번호
                         quot_date            =  is_poheader-ihran  "견적 제출일
                         ref_1                =  is_poheader-ihrez  "참조
                         sales_pers           =  is_poheader-everk  "공급업체 사무실의 영업 담당자
                         suppl_plnt           =  is_poheader-reswk  "공급(출고)플랜트
                         incoterms1           =  is_poheader-inco1  "인도 조건(파트 1)
                         incoterms2           =  is_poheader-inco2  "인코텀스(파트 2)
                         collect_no           =  is_poheader-submi  "계약서명 담당
                         diff_inv             =  is_poheader-lifre  "다른 송장 발행처
                         our_ref              =  is_poheader-unsez  "참조
                         reason_cancel        =  is_poheader-absgr  "세금계산서 발행방식
                         retention_type       =  is_poheader-rettp  "유보 지시자
                         retention_percentage =  is_poheader-retpz  "유보(%)
                         downpay_type         =  is_poheader-dptyp  "선금 지시자
                         downpay_amount       =  is_poheader-dpamt  "선금 금액(BAPI)
                         downpay_percent      =  is_poheader-dppcnt  "선금 비율
                         shiptype             =  is_poheader-versart  "출하 유형
                         shipcond             =  is_poheader-vsbed  "출하 조건
                         incotermsv           =  is_poheader-incov  "인코텀스 버전
                         incoterms2l          =  is_poheader-inco2_l    "인코텀스 장소 1
                         incoterms3l          =  is_poheader-inco3_l ). "인코텀스 장소 2

**poheaderx
  PERFORM headerx_info USING is_poheaderx.

*아이템의 선금금액or 비율에 따른 값 입력.
  IF gs_poheader-downpay_amount IS NOT INITIAL OR
     gs_poheader-downpay_percent IS NOT INITIAL.
    gs_poheader-downpay_duedate  = sy-datum.
    gs_poheaderx-downpay_duedate  = abap_true.
  ENDIF.

*  필수 필드체크.
  IF gs_poheader-comp_code IS INITIAL OR
     gs_poheader-doc_type IS INITIAL OR
    gs_poheader-purch_org IS INITIAL OR
    gs_poheader-pur_group IS INITIAL.
    _g_return TEXT-m01.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form headerx_info
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> IV_POHEADERX
*&---------------------------------------------------------------------*
FORM headerx_info USING is_poheaderx TYPE zsmm_poheaderx.

  gs_poheaderx  = VALUE #(
                         po_number            =  is_poheaderx-ebeln"구매 문서 번호
                         comp_code            =  is_poheaderx-bukrs"회사 코드
                         doc_type             =  is_poheaderx-esart  "구매 문서 유형
                         creat_date           =  abap_true
                         created_by           =  abap_true
                         vendor               =  is_poheaderx-elifn  "공급업체 계정 번호
                         langu                =  is_poheaderx-spras  "언어 키
                         item_intvl           =  is_poheaderx-intvl  "품목 번호 구간
                         pmnttrms             =  is_poheaderx-zterm  "지급 조건 키
                         purch_org            =  is_poheaderx-ekorg  "구매 조직
                         pur_group            =  is_poheaderx-bkgrp  "구매 그룹
                         currency             =  is_poheaderx-waers  "통화 키
                         doc_date             =  is_poheaderx-ebdat  "구매 문서 증빙일
                         vper_start           =  is_poheaderx-kdatb  "계약 시작일
                         vper_end             =  is_poheaderx-kdate  "계약 종료일
                         warranty             =  is_poheaderx-gwldt  "대금지급 예정일
                         quotation            =  is_poheaderx-angnr  "견적 번호
                         quot_date            =  is_poheaderx-ihran  "견적 제출일
                         ref_1                =  is_poheaderx-ihrez  "참조
                         sales_pers           =  is_poheaderx-everk  "공급업체 사무실의 영업 담당자
                         suppl_plnt           =  is_poheaderx-reswk  "공급(출고)플랜트
                         incoterms1           =  is_poheaderx-inco1  "인도 조건(파트 1)
                         incoterms2           =  is_poheaderx-inco2  "인코텀스(파트 2)
                         collect_no           =  is_poheaderx-submi  "계약서명 담당
                         diff_inv             =  is_poheaderx-lifre  "다른 송장 발행처
                         our_ref              =  is_poheaderx-unsez  "참조
                         reason_cancel        =  is_poheaderx-absgr  "세금계산서 발행방식
                         retention_type       =  is_poheaderx-rettp  "유보 지시자
                         retention_percentage =  is_poheaderx-retpz  "유보(%)
                         downpay_type         =  is_poheaderx-dptyp  "선금 지시자
                         downpay_amount       =  is_poheaderx-dpamt  "선금 금액(BAPI)
                         downpay_percent      =  is_poheaderx-dppcnt  "선금 비율
                         shiptype             =  is_poheaderx-versart  "출하 유형
                         shipcond             =  is_poheaderx-vsbed  "출하 조건
                         incotermsv           =  is_poheaderx-incov  "인코텀스 버전
                         incoterms2l          =  is_poheaderx-inco2_l    "인코텀스 장소 1
                         incoterms3l          =  is_poheaderx-inco3_l ). "인코텀스 장소 2

ENDFORM.
*&---------------------------------------------------------------------*
*& Form item_info
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> IT_POITEM
*&      --> IT_POITEMX
*&---------------------------------------------------------------------*
FORM item_info TABLES ct_poitem STRUCTURE zsmm_poitem
                         ct_poitemx STRUCTURE zsmm_poitemx.

**   item
  gt_poitem[] = CORRESPONDING #( ct_poitem[] MAPPING
                    po_item                  = ebelp "구매 문서 품목 번호
*                 DELETE_IND
                    short_text               = txz01  "내역
                    material_long           = matnr  "자재 번호
                    plant                       = ewerk  "플랜트
                    stge_loc                 = lgort  "저장 위치
                    trackingno              = bednr  "요청 추적 번호
                    matl_group             = matkl  "자재 그룹
                    info_rec               = infnr  "정보레코드
                    vend_mat               = idnlf  "공급업체가 사용하는 자재 번호
                    quantity                  = bstmg  "구매 오더 수량
                    po_unit                   = bstme  "구매 오더 단위
*                   PO_UNIT_ISO
                    orderpr_un              = bbprm  "오더 가격 단위(구매)
*                   ORDERPR_UN_ISO
                    conv_num1              = bpumz  "오더 가격 단위를 오더 단위로 환산하기 위한 분자
                    conv_den1              = bpumn  "오더 가격 단위에서 오더 단위로 환산하는 분모
                    net_price                = net_price  "bapi에 대한 통화 금액(9 소수 자릿수)
                    price_unit               = epein  "가격단위
                    gr_pr_time              = webaz  "입고소요일수 (일)
                    tax_code                = mwskz  "부가가치세 코드
                    agreement           = evrtn    "계약번호
                    agmt_item             = evrtp   "품번
                    info_upd              = info_upd "정보레코드 갱신
                    est_price                = schpr  "지시자: 추정 가격
                    over_dlv_tol            = uebto  "초과 납품 허용 한도
                    unlimited_dlv           = uebtk  "지시자: 무제한 초과 납품 허용
                    under_dlv_tol           = untto  "미달 납품 허용 한도
                    val_type                  = bwtar  "평가 유형
*                   NO_MORE_GR
*                   FINAL_INV
                    item_cat                  = pstyp  "구매 문서의 품목 범주
                    acctasscat              = knttp  "계정 지정 범주
                    distrib                      = vrtkz  "복수 계정 지정에 대한 분배 지시자
                    part_inv                   = twrkz  "분할송장 지시자
                    gr_ind                     = wepos  "입고 지시자
                    gr_non_val              = weunb  "비평가 입고
                    ir_ind                      = repos  "송장 수령 지시자
                    free_item                = umson  "무상 품목
                    gr_basediv              = webre  "지시자: 입고 기준 송장 검증
                    ackn_reqd               = kzabs  "발주 확인 필요
                    acknowl_no             = labnr  "오더 확인 번호
                    shipping                  = evers  "출하 지시
                    customer                 = ekunnr  "고객
                    plan_del                   = eplif  "계획 납품 소요 시간(일)
                    conf_ctrl                  = bstae  "확인 관리 키
                    funds_ctr                 = fistl  "자금 관리 센터
                    cmmt_item               = fipos  "약정 항목
*                   PRICEDATE
*                   PRICE_DATE
                    incoterms1               = inco1  "인도 조건(파트 1)
                    incoterms2               = inco2  "인코텀스(파트 2)
                    pre_vendor               = kolif  "이전 공급업체
                    supp_vendor             = supp_vendor
                    sc_vendor                = lblkz  "외주 공급업체
                    preq_no                   = banfn  "구매 요청 번호
                    preq_item                 = bnfpo  "구매 요청 품목 번호
                    ret_item                    = retpo  "반품 품목
                    order_reason            = bsgru  "오더 사유
                    batch                        = charg  "배치 번호
                    vendrbatch                = lichn  "공급업체 배치 번호

                    po_price                    = po_price  "가격 채택: 1 = 총액, 2 = 단가
                    suppl_stloc                = reslo  "재고 운송 오더에 대한 출고 저장 위치
                    retention_percentage  = retpz  "유보(%)
                    downpay_type             = dptyp  "선금 지시자
                    downpay_amount        = dpamt  "선금 금액(bapi)
                    downpay_percent        = dppcnt  "선금 비율
*                   DOWNPAY_DUEDATE
                    incoterms2l                 = inco2_l  "인코텀스 장소 1
                    incoterms3l                 = inco3_l  "인코텀스 장소 2
                    gl_account                  = saknr  "g/l 계정 번호
                    costcenter                  = kostl  "코스트 센터
                    qual_insp                = insmk "재고유형
                    wbs_element               = ps_posid ). "작업 분석 구조 요소(wbs 요소)


**   itemx
  PERFORM itemx_info TABLES ct_poitemx.

*아이템의 선금금액or 비율에 따른 값 입력.
  LOOP AT gt_poitem INTO DATA(ls_item).

    IF ls_item-downpay_amount IS NOT INITIAL OR
       ls_item-downpay_percent IS NOT INITIAL.
      ls_item-downpay_duedate  = sy-datum.

      READ TABLE gt_poitemx INTO DATA(ls_itemx) WITH KEY po_item = ls_item-po_item.
      IF sy-subrc EQ 0.
        DATA(lv_tabix) = sy-tabix.
        ls_itemx-downpay_duedate  = abap_true.
        MODIFY gt_poitemx FROM ls_itemx INDEX lv_tabix
                                   TRANSPORTING downpay_duedate.
      ENDIF.


    ENDIF.

    MODIFY gt_poitem FROM ls_item.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form itemx_info
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> CT_POITEMX
*&---------------------------------------------------------------------*
FORM itemx_info TABLES ct_poitemx STRUCTURE zsmm_poitemx.

  gt_poitemx[] = CORRESPONDING #( ct_poitemx[] MAPPING

                    po_item                  = ebelp  "구매 문서 품목 번호
*                   DELETE_IND
                    short_text               = txz01  "내역
                    material_long           = matnr  "자재 번호
                    plant                       = ewerk  "플랜트
                    stge_loc                 = lgort  "저장 위치
                    trackingno              = bednr  "요청 추적 번호
                    matl_group             = matkl  "자재 그룹
                    vend_mat               = idnlf  "공급업체가 사용하는 자재 번호
                    quantity                  = bstmg  "구매 오더 수량
                    po_unit                   = bstme  "구매 오더 단위
*                   PO_UNIT_ISO
                    orderpr_un              = bbprm  "오더 가격 단위(구매)
*                   ORDERPR_UN_ISO
                    conv_num1              = bpumz  "오더 가격 단위를 오더 단위로 환산하기 위한 분자
                    conv_den1              = bpumn  "오더 가격 단위에서 오더 단위로 환산하는 분모
                    net_price                = net_price  "bapi에 대한 통화 금액(9 소수 자릿수)
                    price_unit               = epein  "가격단위
                    gr_pr_time              = webaz  "입고소요일수 (일)
                    tax_code                = mwskz  "부가가치세 코드
                    agreement             = evrtn    "계약번호
                    agmt_item              = evrtp   "품번
                   info_upd                = info_upd
                    est_price                = schpr  "지시자: 추정 가격
                    over_dlv_tol            = uebto  "초과 납품 허용 한도
                    unlimited_dlv           = uebtk  "지시자: 무제한 초과 납품 허용
                    under_dlv_tol           = untto  "미달 납품 허용 한도
                    val_type                  = bwtar  "평가 유형
*                   NO_MORE_GR
*                   FINAL_INV
                    item_cat                  = pstyp  "구매 문서의 품목 범주
                    acctasscat              = knttp  "계정 지정 범주
                    distrib                      = vrtkz  "복수 계정 지정에 대한 분배 지시자
                    part_inv                   = twrkz  "분할송장 지시자
                    gr_ind                     = wepos  "입고 지시자
                    gr_non_val              = weunb  "비평가 입고
                    ir_ind                      = repos  "송장 수령 지시자
                    free_item                = umson  "무상 품목
                    gr_basediv              = webre  "지시자: 입고 기준 송장 검증
                    ackn_reqd               = kzabs  "발주 확인 필요
                    acknowl_no             = labnr  "오더 확인 번호
                    shipping                  = evers  "출하 지시
                    customer                 = ekunnr  "고객
                    plan_del                   = eplif  "계획 납품 소요 시간(일)
                    conf_ctrl                  = bstae  "확인 관리 키
                    funds_ctr                 = fistl  "자금 관리 센터
                    cmmt_item               = fipos  "약정 항목
*                   PRICEDATE
*                   PRICE_DATE
                    incoterms1               = inco1  "인도 조건(파트 1)
                    incoterms2               = inco2  "인코텀스(파트 2)
                    pre_vendor               = kolif  "이전 공급업체
                    sc_vendor                = lblkz  "외주 공급업체
                    preq_no                   = banfn  "구매 요청 번호
                    preq_item                 = bnfpo  "구매 요청 품목 번호
                    ret_item                    = retpo  "반품 품목
                    order_reason            = bsgru  "오더 사유
                    batch                        = charg  "배치 번호
                    vendrbatch                = lichn  "공급업체 배치 번호

                    po_price                    = po_price  "가격 채택: 1 = 총액, 2 = 단가
                    suppl_stloc                = reslo  "재고 운송 오더에 대한 출고 저장 위치
                    retention_percentage  = retpz  "유보(%)
                    downpay_type             = dptyp  "선금 지시자
                    downpay_amount        = dpamt  "선금 금액(bapi)
                    downpay_percent        = dppcnt  "선금 비율
*                   DOWNPAY_DUEDATE
                    incoterms2l                 = inco2_l  "인코텀스 장소 1
                    incoterms3l                 = inco3_l  "인코텀스 장소 2
                    gl_account                  = saknr  "g/l 계정 번호
                    costcenter                  = kostl  "코스트 센터
                    wbs_element               = ps_posid "작업 분석 구조 요소(wbs 요소)
                    qual_insp                = insmk  "재고유형
                    ).


ENDFORM.
*&---------------------------------------------------------------------*
*& Form addrdelivery_info
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> IT_POADDRDELIVERY
*&---------------------------------------------------------------------*
FORM addrdelivery_info TABLES ct_poaddrdelivery STRUCTURE zsmm_poaddrdelivery.

  gt_poaddrdelivery[] = CORRESPONDING #( ct_poaddrdelivery[] MAPPING

                                  po_item    =  ebelp "구매 문서 번호
                                  addr_no   = addr_no
                                  name        =  name1  "이름 1
                                  name_2    =  name2  "이름 2
                                  city           =  city1  "도시
                                  district       =  city2  "지역
                                  street        =  street  "도로 주소
                                  country     =  land1  "국가 키
                                  customer   =  ekunnr  "고객
                                  supp_vendor = supp_vendor
                                  sc_vendor  =  lblkz  )."외주 공급업체

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SCHEDULE_info
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> IT_POSCHEDULE
*&      --> IT_POSCHEDULEX
*&---------------------------------------------------------------------*
FORM schedule_info TABLES ct_poschedule STRUCTURE zsmm_poschedule
                             ct_poschedulex STRUCTURE zsmm_poschedulex.

**   schedule
  gt_poschedule[] = CORRESPONDING #( ct_poschedule[] MAPPING

                            po_item            =  ebelp "구매 문서 품목 번호
                            sched_line        =  etenr  "일정 라인 번호
                            del_datcat_ext  =  lpein  "납품일 범주
                            delivery_date     =  eeind  "납품일
                            quantity             =  etmen  "예정 수량
                            preq_no            =  banfn  "구매 요청 번호
                            preq_item          =  bnfpo "구매 요청 품목 번호
                            po_date            =  etbdt "일정 라인 오더일
                            req_closed        =  ebakz ).  "구매요청 마감

**   schedulex
  gt_poschedulex[] = CORRESPONDING #( ct_poschedulex[] MAPPING

                            po_item            =  ebelp "구매 문서 품목 번호
                            sched_line        =  etenr  "일정 라인 번호
                            del_datcat_ext  =  lpein  "납품일 범주
                            delivery_date     =  eeind  "납품일
                            quantity             =  etmen  "예정 수량
                            preq_no            =  banfn  "구매 요청 번호
                            preq_item          =  bnfpo "구매 요청 품목 번호
                            po_date            =  etbdt "일정 라인 오더일
                            req_closed        =  ebakz ).  "구매요청 마감


ENDFORM.
*&---------------------------------------------------------------------*
*& Form account_info
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> IT_POACCOUNT
*&      --> IT_POACCOUNTX
*&---------------------------------------------------------------------*
FORM account_info TABLES ct_poaccount STRUCTURE zsmm_poaccount
                            ct_poaccountx STRUCTURE zsmm_poaccountx.

**   account
  gt_poaccount[] = CORRESPONDING #( ct_poaccount[] MAPPING

                          po_item        =   ebelp  " 구매 문서 품목 번호
                          serial_no       =   zekkn  " 계정 지정 순번
*                          DELETE_IND
*                          CREAT_DATE
                          quantity         =   menge  " 수량
                          distr_perc      =   vproz " 다중 계정 지정의 경우 분배 백분율
                          net_value      =   net_value  " BAPI에 대한 통화 금액(9 소수 자릿수)
                          gl_account    =   saknr " G/L 계정 번호
                          costcenter     =   kostl  " 코스트 센터
                          asset_no       =   anln1  " 주요 자산 번호
                          sub_number   =   anln2  " 자산 하위 번호
                          orderid           =   aufnr  " 오더 번호
                          gr_rcpt           =   wempf  " 자재 수령인
                          unload_pt       =   ablad  " 하역 지점
                          wbs_element  =   ps_posid " 작업 분석 구조 요소(WBS 요소)
                          network          =   nplnr  " 계정지정 네트워크번호
                          cmmt_item      =   fipos  " 약정 항목
                          "[U3 변경시작 2022.09.01]
                          delete_ind     = loekz
                          "[U3 변경종료 2022.09.01]
                          funds_ctr        =   fistl ). " 자금 관리 센터
*                          tax_code
*                          nond_itax

**   accountx
  gt_poaccountx[] = CORRESPONDING #( ct_poaccountx[] MAPPING

                          po_item        =   ebelp  " 구매 문서 품목 번호
                          serial_no       =   zekkn  " 계정 지정 순번
*                          DELETE_IND
*                          CREAT_DATE
                          quantity         =   menge  " 수량
                          distr_perc      =   vproz " 다중 계정 지정의 경우 분배 백분율
                          net_value      =   net_value  " BAPI에 대한 통화 금액(9 소수 자릿수)
                          gl_account    =   saknr " G/L 계정 번호
                          costcenter     =   kostl  " 코스트 센터
                          asset_no       =   anln1  " 주요 자산 번호
                          sub_number   =   anln2  " 자산 하위 번호
                          orderid           =   aufnr  " 오더 번호
                          gr_rcpt           =   wempf  " 자재 수령인
                          unload_pt       =   ablad  " 하역 지점
                          wbs_element  =   ps_posid " 작업 분석 구조 요소(WBS 요소)
                          network          =   nplnr  " 계정지정 네트워크번호
                          cmmt_item      =   fipos  " 약정 항목
                          "[U3 변경시작 2022.09.01]
                          delete_ind     = loekz
                          "[U3 변경종료 2022.09.01]
                          funds_ctr        =   fistl ). " 자금 관리 센터
*                          tax_code
*                          nond_itax


* 추가 고정필드 셋팅
  LOOP AT gt_poaccount INTO DATA(ls_acct).
    IF ls_acct IS NOT INITIAL.

      ls_acct-creat_date  = sy-datum.

      READ TABLE gt_poaccountx INTO DATA(ls_acctx) WITH KEY po_item = ls_acct-po_item
                                                                                               serial_no = ls_acct-serial_no.
      IF sy-subrc EQ 0.
        DATA(lv_tabix) = sy-tabix.
        ls_acctx-creat_date  = abap_true.
        MODIFY gt_poaccountx FROM ls_acctx INDEX lv_tabix
                                   TRANSPORTING creat_date.
      ENDIF.

      MODIFY gt_poaccount FROM ls_acct.

    ENDIF.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form condition_info
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> IT_POCOND
*&      --> IT_POCONDX
*&---------------------------------------------------------------------*
FORM condition_info TABLES ct_pocond STRUCTURE zsmm_pocond
                              ct_pocondx STRUCTURE zsmm_pocondx.

**   condition
  gt_pocond[] = CORRESPONDING #( ct_pocond[] MAPPING

                           itm_number  =   kposn  " 조건품목번호
                           cond_st_no  =   stunr  " 단계 번호
                           cond_count  =   zaehk  " 조건 카운터(짧은 길이)
                           cond_type    =   kscha  " 조건유형
                           cond_value  =   cond_value  " 조건 금액
                           currency       =   waers  " 통화 키
                           cond_unit     =   kmein  " 조건 단위
                           cond_p_unt  =   kpein  " 조건 가격결정 단위
                           applicatio      =   kappl  " 어플리케이션
                           numconvert  =   kumza  " 조건 단위를 기본 단위로 변환 시 사용할 분자
                           denominato  =   kumne  " 조건 단위를 기본 단위로 변환 시 사용할 분모
                           condtype      =   kntyp  " 조건 범주(예: 세금, 운임, 가격, 비용)
                           change_id     =   change ).  " 변경 유형


**   conditionx
  gt_pocondx[] = CORRESPONDING #( ct_pocondx[] MAPPING

                           itm_number  =   kposn  " 조건품목번호
                           cond_st_no  =   stunr  " 단계 번호
                           cond_count  =   zaehk  " 조건 카운터(짧은 길이)
                           cond_type    =   kscha  " 조건유형
                           cond_value  =   cond_value  " 조건 금액
                           currency       =   waers  " 통화 키
                           cond_unit     =   kmein  " 조건 단위
                           cond_p_unt  =   kpein  " 조건 가격결정 단위
                           applicatio      =   kappl  " 어플리케이션
                           numconvert  =   kumza  " 조건 단위를 기본 단위로 변환 시 사용할 분자
                           denominato  =   kumne  " 조건 단위를 기본 단위로 변환 시 사용할 분모
                           condtype      =   kntyp  " 조건 범주(예: 세금, 운임, 가격, 비용)
                           change_id     =   change ).  " 변경 유형

  LOOP AT gt_pocond INTO DATA(ls_cond).
    IF ls_cond IS NOT INITIAL.
      ls_cond-applicatio = 'M'.

      READ TABLE gt_pocondx INTO DATA(ls_pocondx) WITH KEY itm_number = ls_cond-itm_number
                                                                                               cond_st_no = ls_cond-cond_st_no.
      IF sy-subrc EQ 0.
        DATA(lv_tabix) = sy-tabix.
        ls_pocondx-applicatio  = abap_true.
        MODIFY gt_pocondx FROM ls_pocondx INDEX lv_tabix
                                   TRANSPORTING applicatio.
      ENDIF.

      MODIFY gt_pocond FROM ls_cond.
    ENDIF.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form textheader_info
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> IT_POTEXTHEADER
*&---------------------------------------------------------------------*
FORM textheader_info TABLES ct_potextheader STRUCTURE zsmm_potextheader.

**   textheader
  gt_potextheader[] = CORRESPONDING #( ct_potextheader[] MAPPING

                                po_number  =  ebeln " 구매 문서 번호
                                po_item        =  ebelp " 구매 문서 품목 번호
                                text_id         =  tdid " 텍스트 ID
                                text_form   =  tdformat " 태그열
                                text_line     =  tdline )." 텍스트 라인

ENDFORM.
*&---------------------------------------------------------------------*
*& Form PARTNER_info
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> IT_POPARTNER
*&---------------------------------------------------------------------*
FORM partner_info TABLES ct_popartner STRUCTURE zsmm_popartner.

**   partner
  gt_popartner[] = CORRESPONDING #( ct_popartner[] MAPPING

                            partnerdesc  =  pabez " 파트너기능의 특정언어내역
*                            LANGU
                            buspartno    =  gparn )." 비즈니스 파트너 번호(공급업체 마스터 레코드)
*                            DELETE_IND

  LOOP AT gt_popartner INTO DATA(ls_partner).
    IF ls_partner IS NOT INITIAL.
      ls_partner-langu = '3'. "KO
      MODIFY gt_popartner FROM ls_partner.
    ENDIF.
  ENDLOOP.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form components_info
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> IT_POCOMPONENTS
*&      --> IT_POCOMPONENTSX
*&---------------------------------------------------------------------*
FORM components_info TABLES ct_pocomponents STRUCTURE zsmm_pocomponents
                               ct_pocomponentsx STRUCTURE zsmm_pocomponentsx.


**   components
  gt_pocomponents[] = CORRESPONDING #( ct_pocomponents[] MAPPING

                                   po_item           =  ebelp  " 구매 문서 품목 번호
                                   sched_line       =  etenr  " 일정 라인 번호
                                   item_no           =  rspos  " 예약/종속 소요량에 대한 품목 번호
                                   material_long   =  matnr   " 자재 번호
                                   entry_quantity  =  menge  " 구성품목 소요량
                                   entry_uom       =  erfme  " 입력단위
                                   entry_uom_iso =  erfme_iso  " ISO 코드의 입력단위
                                   fixed_quan      =  fmeng " 고정 수량
                                   plant               =  werks " 플랜트
                                   req_date         =  bdter  " 구성부품 소요일
                                   change_id         =  change_id  " 변경유형
                                   batch              =  charg  " 배치 번호
                                   iss_st_loc       =  lgpro )."  출고 저장 장소

**   components
  gt_pocomponentsx[] = CORRESPONDING #( ct_pocomponentsx[] MAPPING

                                   po_item           =  ebelp  " 구매 문서 품목 번호
                                   sched_line       =  etenr  " 일정 라인 번호
                                   item_no           =  rspos  " 예약/종속 소요량에 대한 품목 번호
                                   material_long   =  matnr"  " 자재 번호
                                   entry_quantity  =  menge  " 구성품목 소요량
                                   entry_uom       =  erfme  " 입력단위
                                   entry_uom_iso =  erfme_iso  " ISO 코드의 입력단위
                                   fixed_quan      =  fmeng " 고정 수량
                                   plant               =  werks  " 플랜트
                                   req_date         =  bdter  " 구성부품 소요일
                                   change_id         =  change_id  " 변경유형
                                   batch              =  charg  " 배치 번호
                                   iss_st_loc       =  lgpro ). "  출고 저장 장소

ENDFORM.
*&---------------------------------------------------------------------*
*& Form shipping_info
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> IT_POSHIPPING
*&      --> IT_POSHIPPINGX
*&---------------------------------------------------------------------*
FORM shipping_info TABLES ct_poshipping STRUCTURE zsmm_poshipping
                             ct_poshippingx STRUCTURE zsmm_poshippingx.

**   shipping
  gt_poshipping[] = CORRESPONDING #( ct_poshipping[] MAPPING

                            po_item      =   ebelp  " 구매 문서 품목 번호
                            ship_point   =   vstel  " 출하 지점/입고 지점
                            ship_cond   =   vsbed " 출하 조건
                            dlv_prio       =   lprio  " 납품 우선순위
                            route          =   route  " 운송 경로
                            unload_pt   =   ablad )." 하역 지점

**   shippingx
  gt_poshippingx[] = CORRESPONDING #( ct_poshippingx[] MAPPING

                            po_item      =   ebelp  " 구매 문서 품목 번호
                            ship_point   =   vstel  " 출하 지점/입고 지점
                            ship_cond   =   vsbed " 출하 조건
                            dlv_prio       =   lprio  " 납품 우선순위
                            route          =   route  " 운송 경로
                            unload_pt   =   ablad )." 하역 지점


ENDFORM.
*&---------------------------------------------------------------------*
*& Form headerother_info
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> IS_POHEADEROTHER
*&      --> IS_POHEADEROTHERX
*&---------------------------------------------------------------------*
*FORM headerother_info USING is_poheaderother TYPE zsmm_poheaderother
*                                is_poheaderotherx TYPE zsmm_poheaderotherx.
*
*  CONSTANTS: lc_ext_str_name  TYPE bapiparex-structure VALUE 'BAPI_TE_MEPOHEADER',
*             lc_extx_str_name TYPE bapiparex-structure VALUE 'BAPI_TE_MEPOHEADERX'.
*
*  DATA: "ls_ci_ekko    TYPE bapi_te_mepoheader,
*    ls_ci_ekkox    TYPE bapi_te_mepoheaderx,
*    ls_extensionin TYPE bapiparex.
*
**  DATA :BEGIN OF LS_CI_EKKO,
**          PO_NUMBER         TYPE C LENGTH 10,
**          /BOFU/BCSD_SUBJ   TYPE C LENGTH   255,
**          ZORDER_PERSON     TYPE C LENGTH   11,
**          ZORDER_DEPARTMENT TYPE C LENGTH   10,
**          ZEXPEN_PERSON     TYPE C LENGTH   11,
**          ZEXPEN_DEPARTMENT TYPE C LENGTH   10,
**          ZQM_PERSON        TYPE C LENGTH   11,
**          ZQM_DEPARTMENT    TYPE C LENGTH   10,
**          ZCOOP_QM          TYPE C LENGTH   1,
**          ZREAL_COST        TYPE C LENGTH   1,
**          ZCONTRACT_DEPOSIT TYPE C LENGTH   1,
**          ZCONTRACT_GUARN    TYPE C LENGTH 5,
**          ZCONT_GUA_TYPE    TYPE C LENGTH 1,
**          ZCON_KDATB        TYPE C LENGTH 8,
**          ZCON_KDATE        TYPE C LENGTH 8,
**          ZPREPAY_DEPOSIT    TYPE C LENGTH 1,
**          ZPREPAY_GRARN      TYPE C LENGTH 5 ,
**          ZPREP_GUA_TYPE    TYPE C LENGTH 1,
**          ZPAY_KDATB        TYPE C LENGTH 8,
**          ZPAY_KDATE        TYPE C LENGTH 8,
**          ZDEFECT_DEPOSIT    TYPE C LENGTH 1,
**          ZDEFECT_GUARN      TYPE C LENGTH 5,
**          ZDEFEC_GUA_TYPE    TYPE C LENGTH 1,
**          ZDEF_KDATB        TYPE C LENGTH 5,
**          ZDEF_KDATE        TYPE C LENGTH 8,
**          ZDEF_BASE_DATE    TYPE C LENGTH 10,
**          LATE_RATE          TYPE C LENGTH 5,
**          EQUI_CODE          TYPE C LENGTH 40,
**          ZEMANAGE1          TYPE C LENGTH 1,
**          ZEMANAGE2          TYPE C LENGTH 6,
**          HERKL              TYPE C LENGTH 3,
**          ZESHIPTYPE        TYPE C LENGTH 1,
**          ZEDEDLINE          TYPE C LENGTH 8,
**          ZEINSPECT          TYPE C LENGTH 1,
**          ZCONTRACT_VAT_YN  TYPE C LENGTH 1,
**          ZPREPAY_VAT_YN    TYPE C LENGTH 1,
**          ZDEFECT_VAT_YN    TYPE C LENGTH 1,
**          ZDEFECT_COL_YN    TYPE C LENGTH 1,
**        END OF LS_CI_EKKO.
*  DATA: BEGIN OF ls_ci_ekko.
*          include structure zsmm_bapi_te_mepoheader.
*  DATA: END OF ls_ci_ekko.
*
**  poheaderother
*  CLEAR : ls_extensionin.
*  ls_extensionin-structure = lc_ext_str_name.
*  MOVE-CORRESPONDING is_poheaderother TO ls_ci_ekko.
*  PERFORM transfer_to_extensionin USING  ls_ci_ekko CHANGING ls_extensionin.
*
*  APPEND ls_extensionin TO gt_extensionin.
*
**  poheaderotherx
*  CLEAR : ls_extensionin.
*  ls_extensionin-structure = lc_extx_str_name.
*  MOVE-CORRESPONDING is_poheaderotherx TO ls_ci_ekkox.
*  PERFORM transfer_to_extensionin USING ls_ci_ekkox CHANGING ls_extensionin.
*
*  APPEND ls_extensionin TO gt_extensionin.
*
*
*ENDFORM.
*&---------------------------------------------------------------------*
*& Form transfer_to_extensionin
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LS_CI_EBANDB
*&      <-- LS_EXTENSIONIN
*&---------------------------------------------------------------------*
FORM transfer_to_extensionin USING is_bapi_extensionin TYPE any
                             CHANGING cs_bapiparex TYPE bapiparex.

  DATA lv_distance_characters TYPE i.

  FIELD-SYMBOLS <lv_any> TYPE any.

  DESCRIBE DISTANCE BETWEEN cs_bapiparex-structure AND cs_bapiparex-valuepart1
           INTO lv_distance_characters IN CHARACTER MODE.

  ASSIGN cs_bapiparex+lv_distance_characters(*) TO <lv_any>

  CASTING LIKE is_bapi_extensionin.

  <lv_any> = is_bapi_extensionin.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form itemother_info
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> IT_POITEM
*&      --> IT_POITEMX
*&---------------------------------------------------------------------*
FORM itemother_info TABLES ct_poitem STRUCTURE zsmm_poitem
                         ct_poitemx STRUCTURE zsmm_poitemx.

  CONSTANTS: lc_ext_str_name  TYPE bapiparex-structure VALUE 'BAPI_TE_MEPOITEM',
             lc_extx_str_name TYPE bapiparex-structure VALUE 'BAPI_TE_MEPOITEMX'.

  DATA: ls_ci_ekpo     TYPE bapi_te_mepoitem,
        ls_ci_ekpox    TYPE bapi_te_mepoitemx,
        ls_extensionin TYPE bapiparex.


*  poitem other
  LOOP AT ct_poitem INTO DATA(ls_poitem).
    CLEAR : ls_extensionin, ls_ci_ekpo.
    MOVE-CORRESPONDING ls_poitem TO ls_ci_ekpo.
    IF ls_ci_ekpo IS NOT INITIAL.
      ls_extensionin-structure = lc_ext_str_name.
      ls_ci_ekpo-po_item = ls_poitem-ebelp.
      PERFORM transfer_to_extensionin USING  ls_ci_ekpo CHANGING ls_extensionin.
      APPEND ls_extensionin TO gt_extensionin.
    ENDIF.
  ENDLOOP.

*  poitem otherx
  LOOP AT ct_poitemx INTO DATA(ls_poitemx).
    CLEAR : ls_extensionin, ls_ci_ekpox.
    MOVE-CORRESPONDING ls_poitemx TO ls_ci_ekpox.
    IF ls_ci_ekpox IS NOT INITIAL.
      ls_extensionin-structure = lc_extx_str_name.
      ls_ci_ekpox-po_item = ls_poitemx-ebelp.
      PERFORM transfer_to_extensionin USING ls_ci_ekpox CHANGING ls_extensionin.

      APPEND ls_extensionin TO gt_extensionin.
    ENDIF.
  ENDLOOP.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form account_profit
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> IT_POACCOUNTPROFITSEGMENT
*&---------------------------------------------------------------------*
FORM account_profit TABLES
          ct_poaccountprofitsegment STRUCTURE zsmm_poaccountprofitsegment.

  gt_poaccountprofitsegment[] = CORRESPONDING #(
  ct_poaccountprofitsegment[] MAPPING

                          po_item   = po_item  " 구매 문서 품목 번호
                          serial_no = serial_no  " 계정 지정 순번
                          fieldname = fieldname  " 필드이름
                          value     = value ).  " CO-PA 특성값

ENDFORM.
