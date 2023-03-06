@AbapCatalog.sqlViewName: 'ZSVCMM_BATFIND'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: '배치 속성 기준 BATCH NO 조회'

define view ZCCMM_BATCHFIND as select from mara as y
                               inner join  mch1 as Z on y.matnr = Z.matnr
                               left outer join ZCPMM_BATCHFIND as B on Z.matnr = B.MATNR and B.CHARG = Z.charg


 {
    Z.matnr,
    Z.charg as BATCH,
    max(case when Z.lwedt = '00000000' then cast('00000000' as abap.dats ) else Z.lwedt end) as lwedt,
    max(case when Z.hsdat <> '00000000'  then cast(Z.hsdat as abap.dats)
        else   case when B.HSDAT is not null and B.HSDAT <> '00000000' then cast(B.HSDAT as abap.dats) else cast('00000000' as abap.dats) end
        end) as hsdat,
    max(case when B.LICHN is null or B.LICHN = '' then
          case when Z.licha is null then cast('' as abap.char(20)) else cast(Z.licha as abap.char( 20 )) end
          else cast(B.LICHN as abap.char( 20 ))
         end )   as LICHN,
    max(case when Z.vfdat <> '00000000' then cast(Z.vfdat as abap.dats)
        else  case when B.VFDAT is not null and B.VFDAT <> '00000000' then cast(B.VFDAT as abap.dats) else cast('00000000' as abap.dats) end
        end) as VFdat,
    max(Z.lifnr) as lifnr,
    max(case when B.ZMAKER is null then '' else B.ZMAKER end) as Zmaker,
    max(case when B.waterratio is null then 0 else B.waterratio end ) as waterratiO,
    max(case when B.potency is null then 0 else B.potency end ) as potency        ,
    max(case when B.net is null then 0 else B.net end ) as net,
    max(case when B.custlot is null then '' else B.custlot end) as custlot,
    Z.lvorm
}
where y.xchpf ='X'
group by Z.matnr , Z.charg, Z.hsdat, Z.vfdat, Z.lvorm
