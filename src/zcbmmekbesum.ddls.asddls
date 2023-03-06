@AbapCatalog.sqlViewName: 'ZSVBMMEKBESUM'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: '[재고관리]PO 입고 이력 SUM'

define view ZCBMMEKBESUM
as
select from ekbe as A left outer join mara as B on A.matnr = B.matnr
{
  key A.ebeln,
  key A.ebelp,
  key A.matnr,
      sum(case when A.shkzg = 'S' then A.menge else A.menge * -1 end ) as MENGE,
      B.meins
} where A.vgabe = '1'
  group by A.ebeln, A.ebelp, A.matnr, B.meins
