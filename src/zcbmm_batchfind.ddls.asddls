@AbapCatalog.sqlViewName: 'ZSVBMM_BATFIND'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: '[재고관리] 배치속성 조회 Base View'
define view ZCBMM_BATCHFIND
as
select from inob as A inner join kssk as b on A.cuobj = b.objek
                      inner join klah as C on b.clint = C.clint and b.klart = C.klart
                      inner join ksml as D on C.clint = D.clint and C.klart = D.klart
                      inner join cabn as E on D.imerk = E.atinn and D.adzhl = E.adzhl
                      inner join ausp as F on A.cuobj = F.objek and E.atinn = F.atinn
                            association [1]   to cabnt as cabnt  on $projection.atinn = cabnt.atinn
                                                                and $projection.adzhl = cabnt.adzhl
                                                                and cabnt.spras =  $session.system_language
{
  key rtrim(LEFT(A.objek,18),'') as MATNR,
  key SUBSTRING(A.objek,41,10) as CHARG,
      C.class,
      D.posnr,
      D.adzhl,
      E.atinn,
      E.atnam,
      E.atfor,
      cabnt.atbez,
      E.aterf,
      case E.atfor when 'CHAR' then F.atwrt
                   when 'DATE' then
                     case REPLACE(cast(FLTP_TO_DEC(F.atflv as abap.dec(10,0) ) as abap.char(12)),',','')
                          when '0' then ''
                          else REPLACE(cast(FLTP_TO_DEC(F.atflv as abap.dec(10,0) ) as abap.char(12)),',','')
                          end
      end as ATWRT,
      case E.atnam when 'ZPROD_DATE' then REPLACE(cast(FLTP_TO_DEC(F.atflv as abap.dec(10,0) ) as abap.char(12)),',','') end as HSDAT,
      case E.atnam when 'ZEXPIRY_DATE' then REPLACE(cast(FLTP_TO_DEC(F.atflv as abap.dec(10,0) ) as abap.char(12)),',','') end as VFDAT,
      case E.atnam when 'ZFMFRPN' then F.atwrt end as LICHN,
      case E.atnam when 'ZMM_MAKER' then F.atwrt end as ZMAKER,
      case E.atnam when 'ZWATER_CONTENT' then FLTP_TO_DEC(F.atflv as abap.dec(8,3))
                   when 'ZPPYJ_COMP_MOIST' then FLTP_TO_DEC(F.atflv as abap.dec(8,3))  end as waterratio,
      case E.atnam when 'ZPPYJ_COMP_RATIO' then FLTP_TO_DEC(F.atflv as abap.dec(8,3)) end as potency,
      case E.atnam when 'ZPPYJ_COMP_NET' then FLTP_TO_DEC(F.atflv as abap.dec(8,3)) end as net,
      case E.atnam when 'ZPPYJ_CUST_LOTNO' then F.atwrt end as custlot

} where A.klart ='023'
