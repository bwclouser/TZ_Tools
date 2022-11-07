PRO tz_tools

END

PRO read_tz_part,file,full

first={lhead:0L,outfmt:0L,mode:0L}
second={stamp_date:0LL,itime:0L,step:0L}
third={numpart:0L,nact:0L,idx_orgn:0L,nact_lastO:0L,nact_lastNM:0L,nact_lastNH:0L}

OPENR,lun,file,/GET_LUN,/SWAP_ENDIAN,/F77_UNFORMATTED
READU,lun,first
READU,lun,second
READU,lun,third
;stop
nAct=third.nact
IF nAct NE 0 THEN BEGIN
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
ENDIF ELSE BEGIN
  flag=-1
  lr_start=-1
  lon=!VALUES.F_NAN
  lat=!VALUES.F_NAN
  pres=!VALUES.F_NAN
  temp=!VALUES.F_NAN
  idx_back=-1
ENDELSE
FREE_LUN,lun

full={lhead:first.lhead,outfmt:first.outfmt,mode:first.mode,stamp_date:second.stamp_date,itime:second.itime,step:second.step, $
  numpart:third.numpart,nact:third.nact,idx_orgn:third.idx_orgn,nact_lastO:third.nact_lastO,nact_lastNM:third.nact_lastNM,nact_lastNH:third.nact_lastNH,flag:flag,lr_start:lr_start,lon:lon,lat:lat,pres:pres,temp:temp,idx_back:idx_back}

END

PRO write_part_000,dat,tb,te,stampTime,nParcels

timeInd=WHERE(dat.t_utc GE tb AND dat.t_utc LE te)
nTimes=N_ELEMENTS(timeInd)
ones=lonarr(nParcels)+1L

IF timeInd[0] NE -1 THEN BEGIN
  
  nAct=LONG(nTimes*nParcels)
  
  rec1=[LONG(3),LONG(107),LONG(3)]
  rec2={stamptime:LONG64(stamptime),itime:LONG(0),step:LONG(450)}
  rec3=[LONG(nact),LONG(nact),1L,0L,0L,0L]
  
  rec4=LONARR(nAct)
  rec4[*]=127L
  
  rec5=LONG(dat[timeInd].t_utc-86400L)
  rec5=REFORM(rec5##ones,nAct)
  
  rec6=FLOAT(dat[timeInd].g_long_mms)
  rec6[WHERE(rec6 LT 0)]+=360.
  rec6=REFORM(rec6##ones,nAct)
  
  rec7=FLOAT(dat[timeInd].g_lat_mms)
  rec7=REFORM(rec7##ones,nAct)
  
  rec8=FLOAT(dat[timeInd].p_mms*100.)
  rec8=REFORM(rec8##ones,nAct)
  
  rec9=FLOAT(dat[timeInd].t_mms)
  rec9=REFORM(rec9##ones,nAct)
  
  rec10=LINDGEN(nAct)+1L
  
  OPENW,lun,'part_000',/GET_LUN,/SWAP_ENDIAN,/F77_UNFORMATTED
  
  WRITEU,lun,rec1
  WRITEU,lun,rec2
  WRITEU,lun,rec3
  WRITEU,lun,rec4
  WRITEU,lun,rec5
  WRITEU,lun,rec6
  WRITEU,lun,rec7
  WRITEU,lun,rec8
  WRITEU,lun,rec9
  WRITEU,lun,rec10
  
  FREE_LUN,lun
  
ENDIF ELSE BEGIN
  PRINT,'WARNING: No Valid Times'
ENDELSE

END

PRO load_one_bt,dir,index,lat,lon,pres,temp

files=FILE_SEARCH(dir,'part_*')

nFiles=N_ELEMENTS(files)

lat=FLTARR(nFiles)
lon=FLTARR(nFiles)
pres=FLTARR(nFiles)
temp=FLTARR(nFiles)

lat[*]=!VALUES.F_NAN
lon[*]=!VALUES.F_NAN
pres[*]=!VALUES.F_NAN
temp[*]=!VALUES.F_NAN

FOR i=0,nFiles-1 DO BEGIN
  
  file=files[i]
  read_tz_part,file,full
  k=WHERE(index EQ full.idx_back)
  ;print,k[0]
  IF k[0] NE -1 THEN BEGIN
    lat[i]=full.lat[k]
    lon[i]=full.lon[k]
    pres[i]=full.pres[k]
    temp[i]=full.temp[k]
  ENDIF
  
ENDFOR

END