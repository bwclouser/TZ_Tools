PRO tz_tools

END

PRO read_tz_part,file,full

first={lhead:0L,outfmt:0L,mode:0L}
second={stamp_date:0LL,itime:0L,step:0L}
third={numpart:0L,nact:0L,idx_orgn:0L}

OPENR,lun,file,/GET_LUN,/SWAP_ENDIAN,/F77_UNFORMATTED
READU,lun,first
READU,lun,second
READU,lun,third

nAct=third.nact

flag=LONARR(nAct)
lr_start=LONARR(nAct)
lon=FLTARR(nAct)
lat=FLTARR(nAct)
pres=FLTARR(nAct)
temp=FLTARR(nAct)
idx_back=LONARR(nAct)

READU,lun,flag
READU,lun,lr_start
READU,lun,lon
READU,lun,lat
READU,lun,pres
READU,lun,temp
READU,lun,idx_back

FREE_LUN,lun

full={lhead:first.lhead,outfmt:first.outfmt,mode:first.mode,stamp_date:second.stamp_date,itime:second.itime,step:second.step, $
  numpart:third.numpart,nact:third.nact,idx_orgn:third.idx_orgn,flag:flag,lr_start:lr_start,lon:lon,lat:lat,pres:pres,temp:temp,idx_back:idx_back}

END

PRO write_part_000,dat,tb,te,nparcels





END