@AbapCatalog.sqlViewName: 'ZSVBMMBATLAST'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: '[재고관리]자재 별 최종 BATCH NO'

define view ZCBMMBATNOLAST 
as 
select from ZCBMMBATCHECK 
       association [1]   to nriv as nriv  on nriv.object = 'BATCH_CLT'  and nriv.nrrangenr = '01'
{
  key matnr,
      max(charg) as CHARG
     
} where LEN = 10 and TYPECHECK = '000000000000000000000000000'
  group by matnr
