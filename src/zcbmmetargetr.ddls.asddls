@AbapCatalog.sqlViewName: 'ZSVBMMTARGETR'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: '[재고수불부]-집계대상'

define view ZCBMMETARGETR
//배치 재고(창고)
with parameters
P_Bukrs: bukrs
as

select from mara as A inner join nsdm_e_mchb_diff   as B on A.matnr = B.matnr
                      inner join t001k as k on B.werks = k.bwkey
                      inner join mcha as j on B.matnr = j.matnr and B.werks = j.werks and B.charg = j.charg
                      inner join marc as x on B.matnr = x.matnr and B.werks = x.werks
{
  key k.bukrs,
  key B.werks,
  key B.lgort,
  key cast('' as abap.char(10)) as LIFNR,
  key A.matnr,
  key case when x.xchpf is null then ''
           when x.xchpf = '' then ''
      else
      case when B.charg is null then '' else B.charg end end as charg,
  key j.bwtar,
  key A.meins,
  A.matkl,
  A.prdha
} where k.bukrs = $parameters.P_Bukrs

//일반 재고(창고)
union all

select from mara as A inner join nsdm_e_mard_diff   as B on A.matnr = B.matnr
                      inner join t001k as k on B.werks = k.bwkey
                      inner join marc as x on B.matnr = x.matnr and B.werks = x.werks
{
  key k.bukrs,
  key B.werks,
  key B.lgort,
  key cast('' as abap.char(10)) as lifnr,
  key A.matnr,
  key cast('' as abap.char(10)) as charg,
  key cast('' as abap.char(10)) as bwtar,
  key A.meins,
  A.matkl,
  A.prdha
} where k.bukrs = $parameters.P_Bukrs
    and x.bwtty = ''

// 업체 배치재고
union all
select from mara as A inner join nsdm_e_mslb_diff as B on A.matnr = B.matnr
                      inner join t001k as k on B.werks = k.bwkey
                      inner join mcha as j on B.matnr = j.matnr and B.werks = j.werks and B.charg = j.charg
                      inner join marc as x on B.matnr = x.matnr and B.werks = x.werks
{
  key k.bukrs,
  key B.werks,
  key cast('' as abap.char(4)) as lgort,
  key case when B.lifnr is null then '' else B.lifnr end as lifnr,
  key A.matnr,
  key case when x.xchpf is null then ''
           when x.xchpf = '' then ''
      else
      case when B.charg is null then '' else B.charg end end as charg,
  key j.bwtar,
  key A.meins,
  A.matkl,
  A.prdha
} where k.bukrs = $parameters.P_Bukrs

// 업체 일반재고
union all
select from mara as A inner join nsdm_e_mslb_diff as B on A.matnr = B.matnr
                      inner join t001k as k on B.werks = k.bwkey
                      inner join marc as x on B.matnr = x.matnr and B.werks = x.werks
{
  key k.bukrs,
  key B.werks,
  key cast('' as abap.char(4)) as lgort,
  key case when B.lifnr is null then '' else B.lifnr end as lifnr,
  key A.matnr,
  key cast('' as abap.char(10)) as charg,
  key cast('' as abap.char(10)) as bwtar,
  key A.meins,
  A.matkl,
  A.prdha
} where k.bukrs = $parameters.P_Bukrs
    and B.charg = ''

// 업체 위탁 재고(일반)
union all
select from mara as A inner join nsdm_e_mkol_diff as B on A.matnr = B.matnr
                      inner join t001k as k on B.werks = k.bwkey
{
  key k.bukrs,
  key B.werks,
  key B.lgort,
  key case when B.lifnr is null then '' else B.lifnr end as lifnr,
  key A.matnr,
  key cast('' as abap.char(10)) as charg,
  key cast('' as abap.char(10)) as bwtar,
  key A.meins,
  A.matkl,
  A.prdha
} where k.bukrs = $parameters.P_Bukrs
    and B.charg = ''

// 업체 위탁 재고(배치)
union all
select from mara as A inner join nsdm_e_mkol_diff as B on A.matnr = B.matnr
                      inner join t001k as k on B.werks = k.bwkey
                      inner join mcha as j on B.matnr = j.matnr and B.werks = j.werks and B.charg = j.charg
                      inner join marc as x on B.matnr = x.matnr and B.werks = x.werks
{
  key k.bukrs,
  key B.werks,
  key B.lgort,
  key case when B.lifnr is null then '' else B.lifnr end as lifnr,
  key A.matnr,
  key case when x.xchpf is null then ''
           when x.xchpf = '' then ''
      else
      case when B.charg is null then '' else B.charg end end as charg,
  key j.bwtar,
  key A.meins,
  A.matkl,
  A.prdha
} where k.bukrs = $parameters.P_Bukrs


// 고객 판매 재고(배치)
union all
select from mara as A inner join nsdm_e_mska as B on A.matnr = B.matnr
                      inner join t001k as k on B.werks = k.bwkey
                      inner join mcha as j on B.matnr = j.matnr and B.werks = j.werks and B.charg = j.charg
                      inner join marc as x on B.matnr = x.matnr and B.werks = x.werks
{
  key k.bukrs,
  key B.werks,
  key B.lgort,
  key cast('' as abap.char(10)) as LIFNR,
  key A.matnr,
  key case when x.xchpf is null then ''
           when x.xchpf = '' then ''
      else
      case when B.charg is null then '' else B.charg end end as charg,
  key j.bwtar      ,
  key A.meins,
  A.matkl,
  A.prdha
} where k.bukrs = $parameters.P_Bukrs

// 고객 판매 일반재고
union //all
select from mara as A inner join nsdm_e_mska as B on A.matnr = B.matnr
                      inner join t001k as k on B.werks = k.bwkey
{
  key k.bukrs,
  key B.werks,
  key B.lgort,
  key cast('' as abap.char(10)) as LIFNR,
  key A.matnr,
  key cast('' as abap.char(10)) as charg,
  key cast('' as abap.char(10)) as bwtar,
  key A.meins,
  A.matkl,
  A.prdha
} where k.bukrs = $parameters.P_Bukrs
    and B.charg = ''
