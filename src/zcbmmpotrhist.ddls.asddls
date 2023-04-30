@AbapCatalog.sqlViewName: 'ZSVBMMPOTRHIST'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'PO 전송 이력 관리[WMS 전송]'
define view ZCBMMPOTRHIST  
with parameters 
P_BUKRS : abap.char(4)
as
select from cdhdr as A 
        inner join cdpos as B 
               on A.objectclas = B.objectclas and A.objectid = B.objectid and A.changenr = B.changenr 
        inner join ekko as D on B.objectid = D.ebeln
        inner join ekpo as E on D.ebeln = E.ebeln
//        inner join zttm01010 as i on E.werks = i.werks and E.lgort = i.lgort
//        left outer join ztmm40304 as F on A.objectid = F.ebeln 
        association [1]   to t001 as G  on $projection.bukrs = G.bukrs 
        association [1]   to lfa1 as H  on $projection.lifnr = H.lifnr         
               
{
    key A.objectclas,
    key cast(A.objectid as abap.char( 10 ) ) as EBELN,
    key A.changenr,
    D.bukrs,
    G.butxt,
    E.werks,
    D.lifnr,
    H.name1,
    D.bsart,
    D.aedat as ZAEDAT,
    A.username,
    A.udate as UDATE,
    A.utime as UTIME,
    A.tcode,
    A.change_ind,
//    COALESCE( F.zstatus, '' ) as ZSTATUS,
//    F.zstatus as ZSTATUS,
//    F.zmsg,
    cast('' as abap.char( 10 )) as ZVBELN,
//    F.erdat,
//    F.erzet,
//    F.ernam,
//    case when E.lblkz <> '' then ''
//    else
//       case when  i.zwms is null or i.zwms = '' then 
//         case when i.z3plt is null or i.z3plt = '' then '' else i.z3plt end   
//       else i.zwms end 
//   end as ZWMS,
   E.bstae,
   E.knttp    
}

where A.objectclas = 'EINKBELEG'
and   D.bukrs =  $parameters.P_BUKRS
and   D.frgrl = ''
and   D.bstyp = 'F'
and   A.change_ind = 'U'
//and   E.ebelp = '00010' or E.ebelp = '00001'
//and   E.elikz = ''

union all

select from cdhdr as A 
        inner join cdpos as B 
               on A.objectclas = B.objectclas and A.objectid = B.objectid and A.changenr = B.changenr 
        inner join ekko as D on B.objectid = D.ebeln
        inner join ekpo as E on D.ebeln = E.ebeln       
//        inner join zttm01010 as i on E.werks = i.werks and E.lgort = i.lgort         
//        left outer join ztmm40304 as F on A.objectid = F.ebeln 
        association [1]   to t001 as G  on $projection.bukrs = G.bukrs 
        association [1]   to lfa1 as H  on $projection.lifnr = H.lifnr           
               
{
    key A.objectclas,
    key cast(A.objectid as abap.char( 10 ) ) as EBELN,
    key A.changenr,
    D.bukrs,
    G.butxt,
    E.werks,    
    D.lifnr,
    H.name1,
    D.bsart,
    D.aedat as ZAEDAT,    
    A.username,    
    A.udate as UDATE,
    A.utime as UTIME,
    A.tcode,
    A.change_ind,
//    COALESCE( F.zstatus, '' ) as ZSTATUS,    
//    F.zstatus as ZSTATUS,
//    F.zmsg,    
    cast('' as abap.char( 10 )) as ZVBELN,
//    F.erdat,
//    F.erzet,
//    F.ernam,
//    case when E.lblkz <> '' then ''
//    else
//       case when  i.zwms is null or i.zwms = '' then 
//         case when i.z3plt is null or i.z3plt = '' then '' else i.z3plt end   
//       else i.zwms end 
//   end as ZWMS,
   E.bstae,
   E.knttp        
}

where A.objectclas = 'EINKBELEG'
and   D.bukrs =  $parameters.P_BUKRS
and   D.frgrl = ''
and   D.bstyp = 'F'
and   A.change_ind = 'I'
//and   E.ebelp = '00010' or E.ebelp = '00001'
//and   E.elikz = ''

