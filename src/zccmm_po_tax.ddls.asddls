@AbapCatalog.sqlViewName: 'ZSVCMM_PO_TAX'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'PO 세금계산'
define view ZCCMM_PO_TAX as select from ekko
             left outer join ZCBMM_PO_TAX
             on ekko.knumv = ZCBMM_PO_TAX.PricingDocument
             left outer join ZCCMM_PO_TOTAL as c
             on c.ebeln = ekko.ebeln

           left outer join t161t
            on ekko.bsart = t161t.bsart
            and ekko.bstyp = t161t.bstyp
            and t161t.spras = $session.system_language
            left outer join lfa1
            on ekko.lifnr = lfa1.lifnr


{
           key ekko.ebeln,
           key PricingDocument,
           key ekko.bukrs,
           max(c.totval) as totalValue,
           max(ekko.ktwrt) as targetvalue,
           sum(ZCBMM_PO_TAX.ConditionAmount) as taxamount,
           max(ekko.waers) as currency,
           max(ekko.lifnr) as lifnr,
           max(lfa1.name1) as name1,
           max(ekko./bofu/bcsd_subj) as title,
           max(ekko.kdatb) as startdate,
           max(ekko.kdate) as enddate,
           max(ekko.bsart) as bsart,
           max(t161t.batxt) as batxt,

           max(ekko.zterm) as zterm,
           max(ekko.inco1) as INCO1,
           max(ekko.bstyp) as BSTYP,
           max(ekko.zorder_department) as ZORDER_DEPARTMENT,
           max(ekko.zorder_person) as ZORDER_PERSON,
           max(ekko.frgke) as FRGKE,
           case when ekko.frgke ='X' then '결재대기'
                when ekko.frgke ='D' then '상신중'
                when ekko.frgke ='P' then '승인완료'
                when ekko.frgke ='J' then '반려'
                when ekko.frgke ='' then '승인완료'
                end as wf_status,
           max(ekko.ekorg) as ekorg,
           max(ekko.frggr) as frggr,
           max(ekko.bedat) as bedat,
           max(ekko.verkf) as verkf,
           max(ekko.zexpen_person) as zexpen_person,
           max(ekko.zexpen_department) as zexpen_department,
           max(ekko.absgr) as absgr,
           max(ekko.zreal_cost) as ZREAL_COST,
           max(ekko.dppct) as dppct,
           max(ekko.ihrez) as ihrez,
           max(ekko.unsez) as unsez,
           max(ekko.gwldt) as gwldt,
           max(ekko.zcontract_deposit) as zcontract_deposit,
           max(ekko.zcontract_guarn) as ZCONTRACT_GUARN,
           max(ekko.zcont_gua_type) as ZCONT_GUA_TYPE,
           max(ekko.zcon_kdatb) as ZCON_KDATB,
           max(ekko.zcon_kdate) as ZCON_KDATE,
           max(ekko.zprepay_deposit) as ZPREPAY_DEPOSIT,
           max(ekko.zprepay_grarn) as ZPREPAY_GRARN,
           max(ekko.zprep_gua_type) as ZPREP_GUA_TYPE,
           max(ekko.zpay_kdatb) as ZPAY_KDATB,
           max(ekko.zpay_kdate) as ZPAY_KDATE,
           max(ekko.zdefect_deposit) as ZDEFECT_DEPOSIT,
           max(ekko.zdefect_guarn) as ZDEFECT_GUARN,
           max(ekko.zdefec_gua_type) as ZDEFEC_GUA_TYPE,
           max(ekko.zdef_kdatb) as ZDEF_KDATB,
           max(ekko.zdef_kdate) as ZDEF_KDATE,
           max(ekko.zdef_base_date) as ZDEF_BASE_DATE,
           max(ekko.late_rate) as LATE_RATE
    }
 group by PricingDocument,frgke,ekko.ebeln,ekko.bukrs
