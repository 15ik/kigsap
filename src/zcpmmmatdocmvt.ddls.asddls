@AbapCatalog.sqlViewName: 'ZSVPMMSTOCKMVT'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: '[재고수불부]-Matdoc 기간 수불 합계'
define view ZCPMMMATDOCMVT
with parameters
@Consumption.hidden: true
@Environment.systemField: #SYSTEM_DATE
P_StartDate:    sydate,
@Environment.systemField: #SYSTEM_DATE
P_EndDate:    sydate

 as select from matdoc as a inner join ztmm00002 as b
 on a.bwart = b.field1 and a.kzbew = b.field2 and a.bstaus_sg = b.field4 and a.bstaus_cg = b.field5 and a.shkzg = b.field6
 inner join marc as x on a.matnr = x.matnr and a.werks = x.werks
 left outer join mcha as k on a.matnr = k.matnr and a.werks = k.werks and a.charg = k.charg
 {
    key a.bukrs,
    key a.werks,
    key a.lgort,
    key a.matnr,
    key case when x.xchpf is null then ''
             when x.xchpf = '' then ''
        else a.charg end as charg,
    key a.budat,
    a.sobkz,
    case a.sobkz when 'O' then a.lifnr else cast('' as abap.char(10))  end as LIFNR,
    a.lbbsa_sid,
    a.bstaus_sg,
    a.bstaus_cg,
    a.bwart,
    case when k.bwtar is null then '' else k.bwtar end as bwtar,
    a.kzbew,
//    a.shkzg,
    b.field3 as movegroup,
    sum(a.dmbtr_stock) as dmbtr,
    a.waers,
    sum(a.stock_qty) as stock_qty,
    a.meins
}
//where budat between $parameters.P_StartDate and $parameters.P_EndDate
where a.budat between $parameters.P_StartDate and $parameters.P_EndDate
 and b.zmain_cat = 'D1' and b.zmidd_cat = 'D1300' and b.zsmal_cat = 'D1302'
group by a.bukrs,a.werks,lgort,a.matnr,a.charg,budat,lbbsa_sid,bstaus_sg,bwart,kzbew,meins,waers,b.field3,a.sobkz,a.lifnr,a.bstaus_cg, a.bwtar, a.bstaus_cg, x.xchpf, k.bwtar
