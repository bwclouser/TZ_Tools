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

PRO write_part_000,dat,tb,te,stampTime,nParcels,doublesub=doublesub,padchi=padchi

IF KEYWORD_SET(doublesub) THEN sub=172800L ELSE sub=86400L

IF KEYWORD_SET(padchi) THEN BEGIN
  p500=WHERE(dat.p_mms LE 500.)
  tb=dat[p500[0]].t_utc
  te=dat[p500[N_ELEMENTS(p500)-1]].t_utc
  ;stop
ENDIF

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
  
  rec5=LONG(dat[timeInd].t_utc-sub)
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

PRO load_one_bt,dir,index,time,lat,lon,pres,temp,indexed=indexed

t0=SYSTIME(1)

files=FILE_SEARCH(dir,'part_*')
fs=FILE_INFO(files)
ind=WHERE(INDGEN(N_ELEMENTS(files)) LE 24 OR fs.size NE 132)
files=files[ind]
nFiles=N_ELEMENTS(files)

time=0L
lat=FLTARR(nFiles)
lon=FLTARR(nFiles)
pres=FLTARR(nFiles)
temp=FLTARR(nFiles)

lat[*]=!VALUES.F_NAN
lon[*]=!VALUES.F_NAN
pres[*]=!VALUES.F_NAN
temp[*]=!VALUES.F_NAN

IF NOT KEYWORD_SET(indexed) THEN BEGIN
  
  FOR i=0,nFiles-1 DO BEGIN
    
    file=files[i]
    read_tz_part,file,full
    k=WHERE(index EQ full.idx_back)
    ;print,k[0]
    IF i EQ 0 THEN time=86400L+full.lr_start[k]
    IF k[0] NE -1 THEN BEGIN
      ;dex=full.idx_back[k]
      lat[i]=full.lat[k]
      lon[i]=full.lon[k]
      pres[i]=full.pres[k]
      temp[i]=full.temp[k]
      ;stop
    ENDIF
    
  ENDFOR

ENDIF ELSE BEGIN
  
  OPENR,lun,indexed,/get_lun
  nact=0L
  val=0.

  oneline=LONARR(nFiles)
  POINT_LUN,lun,8L+(LONG(index)-1L)*LONG(nFiles)*4L
  READU,lun,oneline
  FREE_LUN,lun
  
  FOR i=0,nFiles-1 DO BEGIN
    
    file=files[i]
    
    OPENR,lun,file,/get_lun,/swap_endian
    
    IF oneline[i] NE -1 THEN BEGIN
      
      dex=oneline[i]

      POINT_LUN,lun,52
      READU,lun,nact

      IF i EQ 0L THEN BEGIN
        POINT_LUN,lun,80LL+1LL*(8LL+4LL*nact)+dex
        READU,lun,time
        time+=86400L
      ENDIF

      ;PRINT,file,80LL+2LL*(8LL+4LL*nact)+dex,80L+2L*(8L+4L*nact)+dex
      POINT_LUN,lun,80L+2L*(8L+4L*nact)+dex
      READU,lun,val
      lon[i]=val
      
      POINT_LUN,lun,80L+3L*(8L+4L*nact)+dex
      READU,lun,val
      lat[i]=val
      
      POINT_LUN,lun,80L+4L*(8L+4L*nact)+dex
      READU,lun,val
      pres[i]=val
      
      POINT_LUN,lun,80L+5L*(8L+4L*nact)+dex
      READU,lun,val
      temp[i]=val
      ;print,i
    ENDIF
    
    FREE_LUN,lun
    
  ENDFOR
  
ENDELSE
PRINT,SYSTIME(1)-t0

END

PRO load_one_block,dir,index0,index1,time,lat,lon,pres,temp,range=range,indexed=indexed

t0=SYSTIME(1)

files=FILE_SEARCH(dir,'part_*')
fs=FILE_INFO(files)
ind=WHERE(INDGEN(N_ELEMENTS(files)) LE 24 OR fs.size NE 132)
files=files[ind]
nFiles=N_ELEMENTS(files)

IF KEYWORD_SET(range) THEN BEGIN
  nBT=N_ELEMENTS(range)
ENDIF ELSE BEGIN
  nBT=index1-index0+1
  range=L64INDGEN(nBT)+index0
ENDELSE

time=LONARR(nBT)
lat=FLTARR(nFiles,nBT)
lon=FLTARR(nFiles,nBT)
pres=FLTARR(nFiles,nBT)
temp=FLTARR(nFiles,nBT)

lat[*]=!VALUES.F_NAN
lon[*]=!VALUES.F_NAN
pres[*]=!VALUES.F_NAN
temp[*]=!VALUES.F_NAN

IF NOT KEYWORD_SET(indexed) THEN BEGIN

  FOR i=0,nFiles-1 DO BEGIN

    file=files[i]
    read_tz_part,file,full
    
    IF i EQ 0 THEN BEGIN
      time=86400.+full.lr_start[range]
    ENDIF
    FOR j=0,nBT-1 DO BEGIN
      
      k=WHERE(range[j] EQ full.idx_back)
      
      IF k[0] NE -1 THEN BEGIN
        
        lat[i,j]=full.lat[k]
        lon[i,j]=full.lon[k]
        pres[i,j]=full.pres[k]
        temp[i,j]=full.temp[k]
        
      ENDIF
    ENDFOR
  ENDFOR
  
  ENDIF ELSE BEGIN
    
    OPENR,lun,indexed,/get_lun
    nact=0L

    oneblock=LONARR(nFiles,index1-index0+1L)
    POINT_LUN,lun,8L+(LONG(index0)-1L)*LONG(nFiles)*4L
    READU,lun,oneblock
    FREE_LUN,lun

    FOR i=0,nFiles-1 DO BEGIN

      file=files[i]

      OPENR,lun,file,/get_lun,/swap_endian
      goods=WHERE(oneblock[i,*] NE -1L)
      
      IF goods[0] NE -1 THEN BEGIN

        dex=oneblock[i,goods[0]]
        
        POINT_LUN,lun,52
        READU,lun,nact

        IF i EQ 0L THEN BEGIN
          POINT_LUN,lun,80L+1L*(8L+4L*nact)+dex
          READU,lun,time
          time+=86400L
        ENDIF


        vals=FLTARR(N_ELEMENTS(goods))

        POINT_LUN,lun,80L+2L*(8L+4L*nact)+dex
        READU,lun,vals
        lon[i,goods]=vals

        POINT_LUN,lun,80L+3L*(8L+4L*nact)+dex
        READU,lun,vals
        lat[i,goods]=vals

        POINT_LUN,lun,80L+4L*(8L+4L*nact)+dex
        READU,lun,vals
        pres[i,goods]=vals

        POINT_LUN,lun,80L+5L*(8L+4L*nact)+dex
        READU,lun,vals
        temp[i,goods]=vals

      ENDIF

      FREE_LUN,lun

    ENDFOR

   
  ENDELSE
  
  PRINT,SYSTIME(1)-t0

END

PRO correct_block,quant

nsize=N_ELEMENTS(quant[0,*])

FOR i=0,nsize-1 DO BEGIN
  
  x=FINITE(quant[*,i])
  j=WHERE(x EQ 1)
  IF N_ELEMENTS(j) GE 2 THEN BEGIN
    IF j[1]-j[0] NE 1 THEN BEGIN
      line=quant[j,i]
      quant[*,i]=!Values.F_NAN
      quant[0:N_ELEMENTS(j)-1,i]=line
    ENDIF
  ENDIF
  
ENDFOR

END

PRO make_avg_paths,dir,avp,avlat,avlon,avt,starts,nParcels

files=FILE_SEARCH(dir,'part_*')
nFiles=N_ELEMENTS(files)


read_tz_part,files[0],full000
nRels=full000.nact/nParcels

dex=DINDGEN(nRels+1)*nParcels+1
starts=86400.+REBIN(full000.lr_start,nRels)

avp=DBLARR(nRels,nFiles)
avt=DBLARR(nRels,nFiles)
avlat=DBLARR(nRels,nFiles)
avlon=DBLARR(nRels,nFiles)
avp[*,*]=!VALUES.F_NAN
avt[*,*]=!VALUES.F_NAN
avlat[*,*]=!VALUES.F_NAN
avlon[*,*]=!VALUES.F_NAN

FOR i=0,nFiles-1 DO BEGIN

  file=files[i]
  read_tz_part,file,full

  FOR j=0,nRels-1 DO BEGIN

    k=WHERE(full.idx_back LT dex[j+1])
    IF k[0] NE -1 THEN BEGIN
      avp[j,i]=mean(full.pres[k])/100d0
      avt[j,i]=mean(full.temp[k])
      avlat[j,i]=mean(full.lat[k])
      avlon[j,i]=mean(full.lon[k])
    ENDIF

  ENDFOR



ENDFOR



END

PRO stddev_calc,dir,stdz,stdlat,stdlon,nParcels,index=index
  
  R=8.3145d0
  g=9.81d0
  
  files=FILE_SEARCH(dir,'part_*')
  nFiles=N_ELEMENTS(files)
  
IF NOT KEYWORD_SET(index) THEN BEGIN
  
  read_tz_part,files[0],full000
  nRels=full000.nact/nParcels
  
  dex=DINDGEN(nRels+1)*nParcels+1
  
  stdz=DBLARR(nRels,nFiles)
  stdlat=DBLARR(nRels,nFiles)
  stdlon=DBLARR(nRels,nFiles)
  stdz[*,*]=!VALUES.F_NAN
  stdlat[*,*]=!VALUES.F_NAN
  stdlon[*,*]=!VALUES.F_NAN
  
  FOR i=0,nFiles-1 DO BEGIN
    
    file=files[i]
    read_tz_part,file,full
    nlp=-R*g/full.temp*ALOG(full.pres)
    
    FOR j=0,nRels-1 DO BEGIN
      
      k=WHERE(full.idx_back LT dex[j+1])
      IF k[0] NE -1 THEN BEGIN
        stdz[j,i]=stddev(full.pres[k])
        stdlat[j,i]=stddev(full.lat[k])
        stdlon[j,i]=stddev(full.lon[k])
      ENDIF
  
    ENDFOR
    
  ENDFOR
  
ENDIF ELSE BEGIN
  
ENDELSE

END

PRO indexTRAJ,dir

t0=SYSTIME(1)

;swap_endian to read TRACZILLA output
;Nact is at byte 52
;idx array is at 20+20+32+6*(8+4*Nact)+8

files=FILE_SEARCH(dir,'part_*')

oname=dir+'part.idx'

nparc=LONG(0)
nact=LONG(0)

fs=FILE_INFO(files)
ind=WHERE(INDGEN(N_ELEMENTS(files)) LE 24 OR fs.size NE 132)
files=files[ind]

n_files=LONG(N_ELEMENTS(files))
;stop
FOR i=0L,n_files-1L DO BEGIN
  
  file=files[i]
  
  IF i EQ 0L THEN BEGIN
    
    OPENR,lun,file,/get_lun,/swap_endian
    POINT_LUN,lun,48
    READU,lun,nparc
    
    towrite=MAKE_ARRAY(n_files,nparc,VALUE=-1L,/LONG)
    ;towrite=MAKE_ARRAY(nparc,n_files,VALUE=-1L,/LONG)
    ;oneline=LONARR(nparc)
    
    FREE_LUN,lun
    
  ENDIF

  OPENR,lun,file,/get_lun,/swap_endian
  POINT_LUN,lun,52
  READU,lun,nact  

  IF nact NE 0 THEN BEGIN
    
    oneline=LONARR(nact)
    
    POINT_LUN,lun,80LL+6LL*(8LL+4LL*LONG64(nact))
    READU,lun,oneline
    ;stop
    towrite[i,oneline-1LL]=4L*LINDGEN(nact)
    
  ENDIF ELSE BEGIN

  ENDELSE
  
  FREE_LUN,lun
  
  
  ;PRINT,files[i]
  
ENDFOR
;stop
OPENW,wlun,oname,/get_lun
WRITEU,wlun,nparc,n_files,towrite
FREE_LUN,wlun

PRINT,SYSTIME(1)-t0

END