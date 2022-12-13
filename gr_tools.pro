PRO gr_tools

END

PRO gr_movie,grpath,timei,timef,llbox,dt=dt,tz=tz,steptime=steptime

IF NOT KEYWORD_SET(steptime) THEN steptime=10800d0

grfiles=FILE_SEARCH(grpath,'*.nc')
FILE_MKDIR,'Working'
stimes=grfiles.substring(-19,-4)
days=stimes.substring(6,7)
inc=WHERE(days GE timei AND days LE timef)

grtimes=DBLARR(N_ELEMENTS(stimes))
FOR i=0,N_ELEMENTS(stimes)-1 DO BEGIN
  READS,stimes[i],yr,mo,dy,hr,mn,sc,FORMAT='(I4,I2,I2,x,I2,I2,I2,x)'
  grtimes[i]=(dy*24.*3600.+hr*3600.+mn*60.+sc)-15.*24.*3600.
ENDFOR

grfiles=grfiles[inc]
stimes=stimes[inc]
n_files=N_ELEMENTS(inc)

IF KEYWORD_SET(tz) THEN BEGIN
  
  RESTORE,tz
  time-=86400d0
  mxtime=MAX(time)
  
  ab=DINDGEN(2832D0*1248D0*13D0) MOD 13
  ab=TRANSPOSE(REFORM(ab,13,2832,1248),[1,2,0])
  
  nBT=N_ELEMENTS(time[0,*])
  
  lts=DBLARR(nBT)
  lns=DBLARR(nBT)
  time0=time[0,*]
  
  deltas=dblarr(nBT)
  
ENDIF

FOR i=n_files-735,0,-1 DO BEGIN
  
  gd=gridrad_v4_2_read_file(grfiles[i])
  gd=gridrad_filter_v4_2(gd)
  ;gd=gridrad_remove_clutter_v4_2(gd)
  ;time=stimes[i]
  ;READS,time,yr,mo,dy,hr,mn,sc,FORMAT='(I4,I2,I2,x,I2,I2,I2,x)'
  ;mo=FLOAT(mo)
  ;dytime=FLOAT(dy)+FLOAT(hr)/24.+FLOAT(mn)/(24.*60.)+FLOAT(sc)/(24.*3600.)
  x1=where(gd.z.values eq 10)
  x2=where(gd.z.values eq 22)
  ;hh=total(finite(gd.z_h.values[*,*,16:28]),3)
  zh=gd.z_h.values
  zh[zh<15]=!Values.F_NAN
  hh=MAX(FINITE(zh[*,*,x1:x2])*ab,DIM=3)+10d0
  
  map=map('Geographic',center_longitude=90,limit=llbox,font_size=16,label_show=0,dimensions=[840,840],title=stimes[i])
  mc=mapcontinents(/countries,limit=llbox)
  mu=mapcontinents(/usa,limit=llbox)
  
  IF KEYWORD_SET(tz) THEN BEGIN

    lts[*]=!Values.F_NAN
    lns[*]=!Values.F_NAN

    IF grtimes[i] LE mxtime THEN BEGIN

      FOR j=0,nBT-1 DO BEGIN
        x=WHERE(time[*,j] LE grtimes[i])

        IF x[0] NE -1 THEN BEGIN

          delta=(grtimes[i]-time[x[0],j])/steptime
          deltas[j]=delta
          ltsi=lat[x[0],j]+delta*(lat[x[0]-1,j]-lat[x[0],j])
          lnsi=lon[x[0],j]+delta*(lon[x[0]-1,j]-lon[x[0],j])-360.
          lts[j]=ltsi
          lns[j]=lnsi
          
          IF KEYWORD_SET(testint) THEN BEGIN
            x=WHERE(gd.x.values GE lnsi-0.03 AND gd.x.values LE lnsi+0.03)
            y=WHERE(gd.y.values GE ltsi-0.03 AND gd.y.values LE ltsi+0.03)
            IF y[0] NE -1 AND x[0] NE -1 THEN BEGIN
              isEcho=WHERE(hh[x[0]:x[N_ELEMENTS(x)-1],y[0]:y[N_ELEMENTS(y)-1]] GE 0)
            ENDIF
          ENDIF

        ENDIF
      ENDFOR
      good=WHERE(FINITE(lns))
      p=plot(lns[good],lts[good],'.',sym_filled=1,sym_size=1,vert_colors=BYTSCL(time0[good]+86400.,min=dt[0].t_utc,max=dt[N_ELEMENTS(dt)-1].t_utc),rgb_table=34,/overplot)
    ENDIF
    
  ENDIF
  
  IF KEYWORD_SET(dt) THEN BEGIN
    p=plot(dt.g_long_mms,dt.g_lat_mms,vert_colors=BYTSCL(dt.t_utc,min=dt[0].t_utc,max=dt[N_ELEMENTS(dt)-1].t_utc),rgb_table=34,thick=2,/overplot)
    pp=plot(dt.t_utc,dt.h2o_chiwis,vert_colors=BYTSCL(dt.t_utc,min=dt[0].t_utc,max=dt[N_ELEMENTS(dt)-1].t_utc),rgb_table=34,ylog=1,yrange=[0,60],position=[0.05,0.85,0.95,0.98],/current)
    p.select
  ENDIF
  
  ct=contour(hh,gd.x.values-360.,gd.y.values,rgb_table=38,c_value=[10,11,12,13,14,15,16,17,18,19,20,21,22],/fill,/overplot)
  cb=colorbar(target=ct,orientation=0,range=[10,22],position=[0.08,0.08,0.50,0.10],title='Altitude (km)',font_size=14,textpos=0,rgb_table=38)
  stop

 

  
  ;stop
  map.save,'Working/'+stimes[i]+'.png'
  map.close
ENDFOR

END
