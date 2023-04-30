@AbapCatalog.sqlViewName: 'ZSVBMMINBINFO'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: '납품 문서 정보 상태 관리'
define view ZCBMMINBINFO as select from ztmm40300 as A 
//inner join zttm01010 as I on A.bukrs = I.bukrs and A.zwerks = I.werks and A.zlgort = I.lgort                         
                         association [1]   to lfa1 as B  on $projection.lifnr = B.lifnr
                         association [1]   to t001w as C  on $projection.werks = C.werks
                         association [1]   to t001w as D  on $projection.zwerks = D.werks
                         association [1]   to t001l as E  on $projection.werks = E.werks and $projection.lgort = E.lgort   
                         association [1]   to t001l as F  on $projection.zwerks = F.werks and $projection.zlgort = F.lgort 
                         association [1]   to t001 as G  on $projection.bukrs = G.bukrs  
                         association [1]   to tvsbt as TVSBT  on $projection.vsbed = TVSBT.vsbed and TVSBT.spras =  $session.system_language                          
                         association [1]   to lfa1 as H  on $projection.sublifnr = H.lifnr                                                 

{
  key A.vbeln,
  A.bukrs,
  G.butxt,
  A.lfdat,
  A.lifnr,
  B.name1 as NAME1,
  A.sublifnr,  
  H.name1 as SUBNAME,
  A.ztrans,
  A.ztrfee,
  A.vsbed,
  tvsbt.vtext,
  A.werks,
  C.name1 as NAME2,
  A.lgort,
  E.lgobe as LGOBE1,
  A.zwerks,
  D.name1 as NAME3,
  A.zlgort,
  F.lgobe as LGOBE2,
  A.zstatus,
  A.ztflag,
  case A.zstatus when 'S' then '납품서생성완료(확정 전)'
                 when 'C' then '납품서최종확정완료'
                 when 'X' then '납품서전송완료(WMS)'
                 when 'E' then '납품 생성 오류 발생'
  end as ZSTATUSNAME,
  A.zmessage,
  A.zconfirm,
  A.loekz,
  A.zwmsifid,
  A.zwmstrcid,
  A.ztrmemo,
  A.zgrmemo,
  A.erdat,
  A.erzet,
  A.ernam,
  A.aedat,
  A.aezet,
  A.aenam
      
}  
where A.zlifnr = ''
//and ( I.zwms = 'X' or I.z3plt = 'X' ) and 
