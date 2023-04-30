@AbapCatalog.sqlViewName: 'ZSVBMM_LASTGR'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: '구매문서의 최종 입고문서'
define view ZCBMM_LASTGR as select from matdoc {
    key ebeln,
    key ebelp,
    key mblnr,
    key mjahr,
    key zeile,
    werks,
    lgort_sid as lgort,
    matbf,
    budat
}
where record_type = 'MDOC'
  and cancelled = '' 
  and cancellation_type = '' 
  and reversal_movement = '' 
  //and bwart = '101'
  and blart = 'WE'
