000001 ID DIVISION.                                                             
000002                                                                          
000003 PROGRAM-ID.     W4079700.                                                
000004 AUTHOR.         MÅNS SAMUELSSON.                                         
000005 DATE-WRITTEN.   95/08/03.                                                
000006 DATE-COMPILED.                                                           
000007                                                                          
000008*    FUNKTION: (TILLÄGG )                                                 
000009*        KOMPLETTERAT AV SUSANNE OLSSON, DEC 1997.                        
000010*        GENERELL PROGRAMKOD FÖR UPPLÄGG AV SALDOLOGG                     
000011*        I DATABAS WDL9/WLLOGA.                                           
000012*                                                                         
000013*    FUNKTION:                                                            
000014*        KOPPLINGS PROGRAM FÖR RETURENS R32 OR                            
000015*                                                                         
000016*        PROGRAMMET UPPDATERAR WLKREE (WDA2)                              
000017*        PROGRAMMET UPPDATERAR WLARTC (WDK6)                              
000018*        PROGRAMMET UPPDATERAR WLARTS (WDK7)                              
000019*        PROGRAMMET UPPDATERAR WLINLE (WDL2)                              
000020*        PROGRAMMET UPPDATERAR WLZZAC (WDG6)                              
000021*        PROGRAMMET UPPDATERAR WLFILB (WDR8)                              
000022*        PROGRAMMET UPPDATERAR WLSAPA (WDR9)                              
000023*        PROGRAMMET UPPDATERAR WDR6                                       
000024*                                                                         
000025*    E-TRACKER: 1658417  DATE 2006-03-20                                  
000026*    E-TRACKER: 3846737  DATE 2006-08-29                                  
000027*                                                                         
000028*    E-TRACKER 10143271 - CHINA WAREHOUSE PROJECT-1                       
000029*                                                                         
000030*                                                                         
000031*    INDATA.                                                              
000032*        TRANSAKTION: W4T797                                              
000033*        MID:         W4I79701                                            
000034*                                                                         
000035                                                                          
000036     SKIP3                                                                
000037 ENVIRONMENT DIVISION.                                                    
000038     EJECT                                                                
000039 DATA DIVISION.                                                           
000040 WORKING-STORAGE SECTION.                                                 
000041                                                                          
000042*    -- CHECKED BY WY2000                                                 
000043 77  IDPGM                       PIC X(08)   VALUE 'W4079700'.            
000044                                                                          
000045*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
000046 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
000047                                                                          
000048 77  JA                          PIC X       VALUE 'J'.                   
000049 77  NEJ                         PIC X       VALUE 'N'.                   
000050 77  IDTRACK-QTY-SW              PIC X       VALUE 'N'.                   
000051     88  IDTRACK-QTY-DONE                    VALUE 'J'.                   
000052     88  IDTRACK-QTY-NOT-DONE                VALUE 'N'.                   
000053                                                                          
000054*    --- ARBETSFÄLT FÖR BERÄKNING AV DAT./TID                             
000055 77  WS-AAAAMMDD                 PIC 9(8)    VALUE ZERO.                  
000056 77  WS-TTMMSSTH                 PIC 9(8)    VALUE ZERO.                  
000057                                                                          
000058 77  WS-FLLSBOK                  PIC X       VALUE 'N'.                   
000059                                                                          
000060                                                                          
000061 01  KLAR-SW                     PIC X       VALUE 'J'.                   
000062     88  KLAR                                VALUE 'J'.                   
000063     88  EJ-KLAR                             VALUE 'N'.                   
000064                                                                          
000065 01  IX                          PIC S9(4)   VALUE +0  COMP SYNC.         
000066                                                                          
000067 01  FILLER.                                                              
000068     03  W-IDLEVNR-PIC9          PIC 9(5).                                
000069                                                                          
000070 01  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   VALUE +33 COMP SYNC.         
000071                                                                          
000072 01  WS-IDLOGLOP                 PIC S9(1)   VALUE +0 COMP-3.             
000073                                                                          
000074 01  WS-KVRETINL                 PIC S9(7)   VALUE +0 COMP-3.             
000075 01  WS-KVRETINL-SKR             PIC S9(7)   VALUE +0 COMP-3.             
000076 01  WS-KVRETINL-MXC             PIC S9(7)   VALUE +0 COMP-3.             
000077 01  WS-TEMP-RETMXC              PIC S9(7)   VALUE +0 COMP-3.             
000078 01  WS-TEMP-RETMXCB             PIC S9(7)   VALUE +0 COMP-3.             
000079 01  WS-TEMP-RETKVAR             PIC S9(7)   VALUE +0 COMP-3.             
000080 01  WS-KVLEVANM-BEKR            PIC S9(7)   VALUE +0 COMP-3.             
000081 01  WS-KVAVV-KVAL               PIC S9(7)   VALUE +0 COMP-3.             
000082 01  WS-KVAVV-KVANT              PIC S9(7)   VALUE +0 COMP-3.             
000083 01  WS-TIRETANK                 PIC S9(7)   VALUE +0 COMP-3.             
000084 01  WS-ARTC01-KDPRODSL          PIC S9(3)   VALUE +0 COMP-3.             
000085 01  WS-KDSORT                   PIC X(2)    VALUE SPACE.                 
000086 01  WS-SLAG-PRAVCOST            PIC S9(7)V9(2) VALUE +0 COMP-3.          
000087 01  WS-IDDC-SPAR                PIC  X(2)   VALUE SPACE.                 
000088 01  WS-KRE-DATUM                PIC 9(6)    VALUE ZERO.                  
000089 01  WS-KVLS                     PIC S9(7)   VALUE ZERO.                  
000090                                                                          
000091 01  WS-IDLOPNRM                 PIC  9(9)   VALUE ZERO.                  
000092 01  FILLER      REDEFINES WS-IDLOPNRM.                                   
000093     03  FILLER                  PIC  9(1).                               
000094     03  WS-IDLOPNRM-VV          PIC  9(2).                               
000095     03  FILLER                  PIC  9(6).                               
000096                                                                          
000097*     -- LOGG-TRANSAR                                                     
000098 01      WS-ZZAC01.                                                       
000099     03  WS-ZZAC01-LOGGPOST      PIC X(90)   VALUE SPACE.                 
000100     03  FILLER                  REDEFINES WS-ZZAC01-LOGGPOST.            
000101        04   WS-ZZAC01-LOGGPOST-1-3  PIC X(3).                            
000102        04   WS-ZZAC01-LOGGPOST-4-90 PIC X(87).                           
000103     03  WS-ZZAC01-SORTPOST      PIC X(36)   VALUE SPACE.                 
000104     EJECT                                                                
000105 01  MESSAGE-CODES.                                                       
000106     03  INF-UPDATE-DONE         PIC X(3)    VALUE '101'.                 
000107     EJECT                                                                
000108                                                                          
000109 01  TEST-IDDISTR        PIC 9(5)        COMP-3.                          
000110*01  FILLER  -COPY WWDIST35  -RED TEST-IDDISTR.                           
000111     EJECT                                                                
000112*01  FILLER  -COPY WWDIST79  -RED TEST-IDDISTR.                           
000113                                                                          
000114     EJECT                                                                
000115*      --- VALID IDDC CODES                                               
000116*                                                                         
000117*01    -COPY WWDCKONS                                                     
000118*01    -COPY WWDC99                                                       
000119*01    -COPY WWDC99 -PRE RET-                                             
000120*01    -COPY WWDC99 -PRE LEV-                                             
000121       EJECT                                                              
000122*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
000123 01  GENERELLA-SUBPROGRAM.                                                
000124     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
000125     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
000126     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
000127     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
000128     03  W418OKOD                PIC X(8)    VALUE 'W418OKOD'.            
000129     03  W009CIA                 PIC X(8)    VALUE 'W009CIA'.             
000130     03  W510AVG                 PIC X(8)    VALUE 'W510AVG '.            
000131     EJECT                                                                
000132*    ---  LÄNKAREA TILL W418OKOD                                          
000133 01  FILLER                      PIC X(16)   VALUE 'W418OKOD'.            
000134                                                                          
000135*01 -COPY W418OKOD           -PRE OKOD-.                                  
000136     EJECT                                                                
000137*    --- PARAMETRAR TILL W009CIA                                          
000138*01  -COPY W009CIA                                                        
000139     EJECT                                                                
000140*    --- PARAMETRAR TILL SUBPROGRAM WDATKONV                              
000141*                                                                         
000142 01  FILLER                      PIC X(16)   VALUE 'WDATAREA'.            
000143     SKIP3                                                                
000144*01 -COPY WDATAREA                                                        
000145     EJECT                                                                
000146 01  FILLER                      PIC X(16)   VALUE 'W211FEL '.            
000147     SKIP3                                                                
000148*01 -COPY W211FEL  -PRE W211FEL-                                          
000149*01 -COPY W211M108 -PRE M108-                                             
000150     SKIP3                                                                
000151*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
000152*                                                                         
000153 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
000154     SKIP3                                                                
000155*01  MID -COPY W4I79701                                                   
000156     EJECT                                                                
000157 01  FILLER                      PIC X(16)  VALUE 'MSGKOM -AREA'.         
000158     SKIP3                                                                
000159*01  -COPY WMSGKOM                                                        
000160     EJECT                                                                
000161 01  FILLER                      PIC X(16)  VALUE 'MSG/MOD-AREA'.         
000162     SKIP3                                                                
000163*01  -COPY WMSGAREA                                                       
000164     EJECT                                                                
000165 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
000166     SKIP3                                                                
000167*01  -COPY WMFSAREA                                                       
000168     EJECT                                                                
000169*    --- AREA FÖR W510AVG                                                 
000170 01  FILLER                    PIC X(16) VALUE 'W510AVGAREA*****'.        
000171                                                                          
000172*01  -COPY W510AVG                                                        
000173     EJECT                                                                
000174*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
000175*                                                                         
000176     EJECT                                                                
000177 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
000178     SKIP3                                                                
000179 01  NYCKLAR-TILL-DLI.                                                    
000180     03  W-IDLOPNRM-X.                                                    
000181         05  W-IDLOPNRM          PIC S9(9)   VALUE ZERO COMP-3.           
000182     03  W-IDLEVANM-X.                                                    
000183         05  W-IDDISTR           PIC S9(5)    VALUE ZERO COMP-3.          
000184         05  W-IDKUNDNR          PIC S9(7)    VALUE ZERO COMP-3.          
000185         05  W-IDRAPPNR          PIC  9(7)    VALUE ZERO.                 
000186     03  W-WDA211KY-X.                                                    
000187       04  W-IDARTNR-X.                                                   
000188         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
000189       04  W-IDRADNR-X.                                                   
000190         05  W-IDRADNR           PIC S9(5)   VALUE ZERO COMP-3.           
000191     SKIP2                                                                
000192     03  W-WDA3FSEQ-X.                                                    
000193         05  W-IDDC-X.                                                    
000194           07  W-IDDC-FSEQ       PIC  X(2)          VALUE SPACE.          
000195         05  W-IDDISTR-FSEQ      PIC S9(5)   COMP-3 VALUE ZERO.           
000196         05  W-IDKUNDNR-FSEQ     PIC S9(7)   COMP-3 VALUE ZERO.           
000197         05  W-IDRAPPNR-FSEQ     PIC  9(7)   VALUE ZERO.                  
000198                                                                          
000199     03  W-IDDC-B6-X.                                                     
000200         05 W-IDDC-B6                  PIC X(2).                          
000201     03  W-PRAVCOST-X.                                                    
000202         05 W-PRAVCOST          PIC S9(7)V9(2) VALUE ZERO COMP-3.         
000203                                                                          
000204     03  W-IDDC1-CN                    PIC  X(1)   VALUE '7'.             
000205     03  W-DAINLEV-X.                                                     
000206         05  W-DAINLEV           PIC 9(16).                               
000207                                                                          
000208     EJECT                                                                
000209     SKIP2                                                                
000210*    --- STATUS-KOD FRÅN IMS                                              
000211 01  STATUS-WS                   PIC XX.                                  
000212     88  SEGMENT-FINNS                       VALUE '  '.                  
000213     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
000214     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
000215     88  SEGMENT-SLUT                        VALUE 'GB'.                  
000216     SKIP2                                                                
000217 01  GODK-STATUSKODER.                                                    
000218     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
000219     SKIP3                                                                
000220 01  SSA1                        PIC X(64).                               
000221 01  SSA2                        PIC X(64).                               
000222 01  SSA3                        PIC X(64).                               
000223 01  SSA4                        PIC X(64).                               
000224     EJECT                                                                
000225*    --- IMS FUNKTIONSKODER                                               
000226*01  -COPY W0003                                                          
000227     EJECT                                                                
000228*    ---  DLI INPUT-OUTPUT AREA                                           
000229 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
000230     SKIP3                                                                
000231 01  DLI-IO-AREA.                                                         
000232     03  IO-AREA                 PIC X(200)  VALUE SPACE.                 
000233     SKIP3                                                                
000234     03  WLINLC11 REDEFINES IO-AREA.                                      
000235*        05  -COPY WDL611  -PRE INLC-                                     
000236     03  WLINLE21 REDEFINES IO-AREA.                                      
000237*        05  -COPY WDL221  -PRE INLE-                                     
000238     SKIP3                                                                
000239     03  WLINLE31 REDEFINES IO-AREA.                                      
000240*        05  -COPY WDL231  -PRE INLE-                                     
000241     SKIP3                                                                
000242     03  WLZZAC01 REDEFINES IO-AREA.                                      
000243*        05  -COPY WDG601  -PRE ZZAC01-                                   
000244     03  WLRETA01 REDEFINES IO-AREA.                                      
000245*        05  -COPY WDA301                                                 
000246 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDA201'.                      
000247 01  DLI-IO-WDA201.                                                       
000248*    03  -COPY WDA201                                                     
000249     EJECT                                                                
000250 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDA211'.                      
000251 01  DLI-IO-WDA211.                                                       
000252*    03  -COPY WDA211                                                     
000253 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDA222'.                      
000254 01  DLI-IO-WDA222.                                                       
000255*    03  -COPY WDA222                                                     
000256 01  FILLER                      PIC X(16)  VALUE 'DLI-IO-AREA3'.         
000257 01  DLI-IO-AREA3.                                                        
000258     03  IO-AREA3                PIC X(900)  VALUE SPACE.                 
000259     03  WLARTC01 REDEFINES IO-AREA3.                                     
000260*        05  -COPY WDK601   -PRE ARTC-                                    
000261     03  WLARTC11 REDEFINES IO-AREA3.                                     
000262*        05  -COPY WDK611   -PRE ARTC-                                    
000263     EJECT                                                                
000264*    ---  DLI INPUT-OUTPUT AREA 4                                         
000265 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA4'.        
000266     SKIP3                                                                
000267 01  DLI-IO-AREA4.                                                        
000268     03  IO-AREA4                PIC X(250)  VALUE SPACE.                 
000269     SKIP3                                                                
000270     03  WLFILB01 REDEFINES IO-AREA4.                                     
000271*        05  -COPY WDR801   -PRE EKO-                                     
000272         07  FILLER REDEFINES EKO-FIL-WDR801-DATA.                        
000273*        09  -COPY W510A06  -PRE   A06-                                   
000274         07  FILLER REDEFINES EKO-FIL-WDR801-DATA.                        
000275*        09  -COPY W510EKHA -PRE  EKO-                                    
000276     EJECT                                                                
000277*    ---  DLI INPUT-OUTPUT AREA 5                                         
000278 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK711'.                      
000279 01  DLI-IO-WDK711.                                                       
000280*    03  -COPY WDK711 -PRE ARTS-                                          
000281                                                                          
000282 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK728'.                      
000283 01  DLI-IO-WDK728.                                                       
000284*    03   -COPY WDK728                                                    
000285 01  FILLER             PIC X(16)  VALUE 'DLI-IO-WLLOGA01'.               
000286 01  DLI-IO-WLLOGA01.                                                     
000287*    03  WLLOGA01  -COPY WDL901                                           
000288     EJECT                                                                
000289 01  FILLER             PIC X(16)  VALUE 'DLI-IO-WLSAPA01'.               
000290 01  DLI-IO-WLSAPA01.                                                     
000291*    03  WLSAPA01  -COPY WDR901                                           
000292*    07  -COPY W510EKHA -RED FIL-WDR901-DATA                              
000293     EJECT                                                                
000294 01  FILLER             PIC X(16)  VALUE 'DLI-IO-WDR601'.                 
000295 01  DLI-IO-WDR601.                                                       
000296*    03  -COPY WDR601  -PRE LOG-                                          
000297*      05  -COPY W407R32A  -RED LOG-FIL-WDR601-DATA                       
000298                                                                          
000299 01  FILLER           PIC X(16)   VALUE 'WDB601 AREA LEV'.                
000300 01   DLI-IO-AREA-B601-LEV.                                               
000301*     03  -COPY WDB601 -PRE LEV-                                          
000302                                                                          
000303 01  FILLER           PIC X(16)   VALUE 'WDB601 AREA RET'.                
000304 01   DLI-IO-AREA-B601-RET.                                               
000305*     03  -COPY WDB601 -PRE RET-                                          
000306                                                                          
000307     EJECT                                                                
000308 LINKAGE SECTION.                                                         
000309                                                                          
000310*01  -COPY W0009   -PRE MSG-                                              
000311*01  -COPY W0009   -PRE DISP-                                             
000312*01  -COPY W0008  -PRE WDA2-                                              
000313     05  FILLER                  PIC X.                                   
000314     EJECT                                                                
000315*01  -COPY W0008  -PRE ARTC-                                              
000316     05  FILLER                  PIC X.                                   
000317     EJECT                                                                
000318*01  -COPY W0008  -PRE WDK7-                                              
000319     05  FILLER                  PIC X.                                   
000320     EJECT                                                                
000321*01  -COPY W0008  -PRE INLE-                                              
000322     05  FILLER                  PIC X.                                   
000323     EJECT                                                                
000324*01  -COPY W0008  -PRE ZZAC-                                              
000325     05  FILLER                  PIC X.                                   
000326     EJECT                                                                
000327*01  -COPY W0008  -PRE RETA-                                              
000328     05  FILLER                  PIC X.                                   
000329     EJECT                                                                
000330*01  -COPY W0008  -PRE FILB-                                              
000331     05  FILLER                  PIC X.                                   
000332     EJECT                                                                
000333*01  -COPY W0008  -PRE INLC-                                              
000334     05  FILLER                  PIC X.                                   
000335     EJECT                                                                
000336*01  -COPY W0008  -PRE WLLOGA-                                            
000337     05  FILLER                  PIC X.                                   
000338     EJECT                                                                
000339*01  -COPY W0008  -PRE WLSAPA-                                            
000340     05  FILLER                  PIC X.                                   
000341     EJECT                                                                
000342*01  -COPY W0008  -PRE WDR6-                                              
000343     05  FILLER                  PIC X.                                   
000344     EJECT                                                                
000345*01  -COPY W0008      -PRE WDB6-                                          
000346     05  FILLER                  PIC X.                                   
000347*01  -COPY W0008  -PRE 9305-                                              
000348     05  FILLER                  PIC X.                                   
000349*01  -COPY W0008  -PRE AVG-WDB6-                                          
000350     05  FILLER                  PIC X.                                   
000351     EJECT                                                                
000352                                                                          
000353 PROCEDURE DIVISION  USING MSG-PCB DISP-PCB                               
000354     WDA2-PCB ARTC-PCB WDK7-PCB                                           
000355     INLE-PCB ZZAC-PCB RETA-PCB FILB-PCB INLC-PCB WLLOGA-PCB              
000356     WLSAPA-PCB WDR6-PCB WDB6-PCB 9305-PCB AVG-WDB6-PCB.                  
000357 MAIN SECTION.                                                            
000358     ENTRY 'DLITCBL' USING MSG-PCB DISP-PCB                               
000359     WDA2-PCB ARTC-PCB WDK7-PCB                                           
000360     INLE-PCB ZZAC-PCB RETA-PCB FILB-PCB INLC-PCB WLLOGA-PCB              
000361     WLSAPA-PCB WDR6-PCB WDB6-PCB 9305-PCB AVG-WDB6-PCB.                  
000362                                                                          
000363     PERFORM IMS-GET-MSG                                                  
000364     IF SEGMENT-FINNS                                                     
000365       PERFORM IMS-GN-MSG                                                 
000366       PERFORM A-INIT                                                     
000367       MOVE +1 TO IX                                                      
000368       PERFORM UNTIL IX > MID-KVPOST                                      
000369         MOVE JA   TO KLAR-SW                                             
000370         PERFORM H-UPPDATERA                                              
000371         ADD +1 TO IX                                                     
000372       END-PERFORM                                                        
000373       PERFORM Z-FINIT                                                    
000374     END-IF                                                               
000375                                                                          
000376     MOVE ZERO TO RETURN-CODE                                             
000377     GOBACK                                                               
000378     .                                                                    
000379     EJECT                                                                
000380 A-INIT SECTION.                                                          
000381                                                                          
000382     MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W4I79701                    
000383     MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                   
000384     MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                  
000385                                                                          
000386     MOVE LOW-VALUE TO MSG-AREA                                           
000387                                                                          
000388     MOVE FUNCTION CURRENT-DATE (1:8) TO WS-AAAAMMDD                      
000389     MOVE FUNCTION CURRENT-DATE (9:8) TO WS-TTMMSSTH                      
000390                                                                          
000391     MOVE 'IDAG' TO DAT-KDDATFORM                                         
000392     CALL WDATKONV USING DAT-KDDATFORM                                    
000393                         DAT-I-TIDATUM                                    
000394                         DAT-O-TIDATUM                                    
000395                         DAT-KDSVAR                                       
000396     IF DAT-KDSVAR = 'F'                                                  
000397       MOVE 'FEL FRÅN DATKONV ' TO FELTEXT                                
000398       CALL ABEND USING RKOD-ABEND-UTAN-DUMP                              
000399     END-IF                                                               
000400     MOVE ZERO              TO EKO-FIL-IDSEKVNR                           
000401                                                                          
000402     .                                                                    
000403     EJECT                                                                
000404 H-UPPDATERA SECTION.                                                     
000405                                                                          
000406     MOVE MID-IDDISTR  (IX) TO W-IDDISTR                                  
000407                               W-IDDISTR-FSEQ                             
000408                               TEST-IDDISTR                               
000409     MOVE MID-IDKUNDNR (IX) TO W-IDKUNDNR                                 
000410                               W-IDKUNDNR-FSEQ                            
000411     MOVE MID-IDRAPPNR (IX) TO W-IDRAPPNR                                 
000412                               W-IDRAPPNR-FSEQ                            
000413     MOVE MID-IDARTNR  (IX) TO W-IDARTNR                                  
000414     MOVE MID-IDRADNR  (IX) TO W-IDRADNR                                  
000415                                                                          
000416     PERFORM IMS-GHU-KREE-KREE11                                          
000417                                                                          
000418     MOVE LEV-IDLOPNRM     TO W-IDLOPNRM                                  
000419     MOVE LEV-KVRETINL     TO WS-KVRETINL                                 
000420     MOVE LEV-KVRETINL-SKR TO WS-KVRETINL-SKR                             
000421     MOVE LEV-KVLEVANM-BEKR TO WS-KVLEVANM-BEKR                           
000422     MOVE LEV-KVAVV-KVAL    TO WS-KVAVV-KVAL                              
000423     MOVE LEV-KVAVV-KVANT   TO WS-KVAVV-KVANT                             
000424     MOVE LEV-IDDC          TO WS-IDDC                                    
000425                                    W-IDDC-B6                             
000426     PERFORM IMS-GU-WDB601-LEV                                            
000427                                                                          
000428     MOVE LEV-IDDC-RET      TO W-IDDC-FSEQ                                
000429                                    RET-WS-IDDC                           
000430                                    W-IDDC-B6                             
000431                                                                          
000432     PERFORM IMS-GU-WDB601-RET                                            
000433                                                                          
000434     IF LEV-IDARTNR = 100                                                 
000435       CONTINUE                                                           
000436     ELSE                                                                 
000437       MOVE LEV-KDANMORS      TO OKOD-KDANMORS                            
000438*--- ANROPA KONTROLL AV ORSAKSKODER                                       
000439       CALL W418OKOD USING OKOD-W418OKOD                                  
000440                                                                          
000441       IF OKOD-FL-SALDOBOK-RETUR = JA                                     
000442         MOVE JA                  TO WS-FLLSBOK                           
000443                                                                          
000444         IF (CDC   AND LEV-DALEVANM     < 19970520) AND                   
000445            (LEV-KDANMORS = '12' OR '22')                                 
000446           PERFORM IMS-GU-ARTC-ARTC01                                     
000447           MOVE ARTC-ART-KDPRODSL TO WS-ARTC01-KDPRODSL                   
000448           MOVE ARTC-ART-KDSORT   TO WS-KDSORT                            
000449           PERFORM IMS-GHNP-ARTC-ARTC11                                   
000450           MOVE NEJ                TO WS-FLLSBOK                          
000451         ELSE                                                             
000452           IF RET-DCS-CDC                                                 
000453             PERFORM HAA-UPPD-ARTC11                                      
000454           ELSE                                                           
000455             PERFORM HAD-UPPD-WDK711                                      
000456             PERFORM IMS-GU-ARTC-ARTC01                                   
000457             MOVE ARTC-ART-KDPRODSL TO WS-ARTC01-KDPRODSL                 
000458             MOVE ARTC-ART-KDSORT   TO WS-KDSORT                          
000459             PERFORM IMS-GHNP-ARTC-ARTC11                                 
000460           END-IF                                                         
000461         END-IF                                                           
000462       ELSE                                                               
000463         PERFORM IMS-GU-ARTC-ARTC01                                       
000464         MOVE ARTC-ART-KDPRODSL TO WS-ARTC01-KDPRODSL                     
000465         MOVE ARTC-ART-KDSORT   TO WS-KDSORT                              
000466         PERFORM IMS-GHNP-ARTC-ARTC11                                     
000467       END-IF                                                             
000468                                                                          
000469       IF RET-DCS-CDC OR                                                  
000470*FIX HB                                                                   
000471         (W-IDDISTR  = 2380    AND                                        
000480          W-IDKUNDNR = 1823    AND                                        
000481          W-IDRAPPNR = 968821)                                            
000482         IF W-IDDISTR  = 1958    AND                                      
000483            W-IDKUNDNR = 1926    AND                                      
000484            W-IDRAPPNR = 15150                                            
000485*FIX HB     DENNA LEV.ANM. HAR LEV-IDLOPNRM = 0 OCH FINNS INTE            
000486*           MED PÅ INLEV.HISTORIKEN (WDL2).                               
000487*           FÖR ATT KUNNA FORTS. UTAN ATT ABENDA HOPPAR PGM               
000488*           ÖVER DETTA STEG (I SAMRÅD MED JESSICA 240807)                 
000489           CONTINUE                                                       
000490         ELSE                                                             
000491           PERFORM HAB-UPPD-INLE                                          
000492         END-IF                                                           
000493       ELSE                                                               
000494         IF (W-IDDISTR  = 2364    AND                                     
000495             W-IDKUNDNR = 1353    AND                                     
000496             W-IDRAPPNR = 1689)                                           
000497*FIX CO     DESSA LEV.ANM. HAR LEV-IDLOPNRM SOM INTE LÄNGRE FINNS         
000498*           PÅ INLEV.HIST. WDL6 PGA ATT RETURERNA ÄR VÄLDIGT              
000499*           GAMLA OCH INLEV.HIST. HAR HUNNIT BLIR RENSAD.                 
000500*           FÖR ATT KUNNA FORTS. UTAN ATT ABENDA HOPPAR PGM.              
000501*           ÖVER DETTA STEG (I SAMRÅD MED SUSSI S. 11/08 '14)             
000502           CONTINUE                                                       
000503         ELSE                                                             
000504           PERFORM HAE-UPPD-INLC                                          
000505         END-IF                                                           
000506       END-IF                                                             
000507                                                                          
000508       IF WS-KVLEVANM-BEKR     =                                          
000509          WS-KVAVV-KVAL        +                                          
000510          WS-KVAVV-KVANT       +                                          
000511          WS-KVRETINL          +                                          
000512          WS-KVRETINL-SKR                                                 
000513         PERFORM HAC-UPPD-LOGG                                            
000514       ELSE                                                               
000515         IF RET-NDC-NA                                                    
000516           IF OKOD-FL-SALDOBOK-RETUR = JA                                 
000517             MOVE JA                     TO WS-FLLSBOK                    
000518             IF LEV-IDFTG = 53 OR 54                                      
000519               PERFORM HACI-EKOTRANS-A06                                  
000520             END-IF                                                       
000521           END-IF                                                         
000522         END-IF                                                           
000523         MOVE NEJ       TO KLAR-SW                                        
000524       END-IF                                                             
000525     END-IF                                                               
000526                                                                          
000527     PERFORM IMS-GET-KREE-KREE01                                          
000528     PERFORM S04-GNP-KREE-KREE11                                          
000529     PERFORM UNTIL SEGMENT-SAKNAS OR EJ-KLAR                              
000530         IF LEV-KVLEVANM-BEKR > +0 AND                                    
000531            LEV-KVLEVANM-BEKR NOT =                                       
000532            LEV-KVAVV-KVAL        +                                       
000533            LEV-KVAVV-KVANT       +                                       
000534            LEV-KVRETINL          +                                       
000535            LEV-KVRETINL-SKR                                              
000536           MOVE NEJ     TO KLAR-SW                                        
000537         END-IF                                                           
000538       PERFORM S04-GNP-KREE-KREE11                                        
000539     END-PERFORM                                                          
000540     IF KLAR                                                              
000541       PERFORM IMS-GHU-KREE-KREE01                                        
000542       IF ANM-KDLEVANM NOT = '7'                                          
000543         MOVE '7'     TO ANM-KDLEVANM                                     
000544         PERFORM IMS-REPL-KREE                                            
000545                                                                          
000546* OM DISTRIKT = 8111 OCH 8211, SKAPA LOGG USA/CAN                         
000547* OM DISTRIKT = 8200 OCH 8201, SKAPA LOGG KINA - FLYTTAS TILL CN-2        
000548**SUSSI  IF DIST35-NA-CDC-RETURN OR DIST35-CN-CDC-RETURNS                 
000549         IF DIST35-NA-CDC-RETURN                                          
000550           PERFORM HAF-SKAPA-WDR6-LOGG                                    
000551         END-IF                                                           
000552       END-IF                                                             
000553                                                                          
000554       PERFORM IMS-GHU-RETA-RETA01                                        
000555*START FIX + GODKÄNT GE I IMS-GHU-RETA-RETA01                             
000556       IF SEGMENT-FINNS                                                   
000557*END   FIX                                                                
000558         PERFORM UNTIL SEGMENT-SAKNAS OR SEGMENT-SLUT                     
000559           MOVE  9      TO RET-KDKOLSTA                                   
000560           MOVE '6'     TO RET-KDRETSTA                                   
000561           ACCEPT RET-TIKLAR FROM DATE                                    
000562           PERFORM IMS-REPL-RETA                                          
000563           PERFORM IMS-GHN-RETA-RETA01                                    
000564         END-PERFORM                                                      
000565       END-IF                                                             
000566     END-IF                                                               
000567                                                                          
000568     .                                                                    
000569     EJECT                                                                
000570 HAA-UPPD-ARTC11  SECTION.                                                
000571                                                                          
000572     PERFORM IMS-GU-ARTC-ARTC01                                           
000573     MOVE ARTC-ART-KDPRODSL TO WS-ARTC01-KDPRODSL                         
000574     PERFORM IMS-GHNP-ARTC-ARTC11                                         
000575                                                                          
000576     SUBTRACT MID-KVRETINL(IX)     FROM ARTC-CLAG-KVAKS-CDC               
000577*    SUBTRACT MID-KVAVV-KVANT(IX)  FROM ARTC-CLAG-KVAKS-CDC               
000578     ADD      MID-KVRETINL(IX)     TO   ARTC-CLAG-KVLS                    
000579     IF MID-KVRETINL(IX) NOT = 0                                          
000580       MOVE '-'                    TO LOGG-IDTECKEN-KVAKS                 
000581       MOVE '+'                    TO LOGG-IDTECKEN-KVLS                  
000582       MOVE MID-KVRETINL(IX)       TO LOGG-KVART-SALDO                    
000583       PERFORM S05-SKAPA-SALDOLOGG                                        
000584     END-IF                                                               
000585     SUBTRACT MID-KVRETINL-TRP(IX) FROM ARTC-CLAG-KVLS                    
000586     IF MID-KVRETINL-TRP(IX) NOT = 0                                      
000587       MOVE ' '                    TO LOGG-IDTECKEN-KVAKS                 
000588       MOVE '-'                    TO LOGG-IDTECKEN-KVLS                  
000589       MOVE MID-KVRETINL-TRP(IX)   TO LOGG-KVART-SALDO                    
000590       PERFORM S05-SKAPA-SALDOLOGG                                        
000591     END-IF                                                               
000592     PERFORM IMS-REPL-ARTC-ARTC                                           
000593                                                                          
000594     .                                                                    
000595     EJECT                                                                
000596 HAB-UPPD-INLE    SECTION.                                                
000597                                                                          
000598     IF WS-KVLEVANM-BEKR     =                                            
000599        WS-KVAVV-KVAL        +                                            
000600        WS-KVAVV-KVANT       +                                            
000601        WS-KVRETINL          +                                            
000602        WS-KVRETINL-SKR                                                   
000603       PERFORM IMS-GHU-INLE-INLE21                                        
000604                                                                          
000605       MOVE 'R32'           TO INLE-MOT-IDPTYP                            
000606       IF WS-KVAVV-KVAL  > +0                                             
000607         MOVE +1              TO INLE-MOT-KDAVVKV                         
000608       END-IF                                                             
000609       IF WS-KVAVV-KVANT > +0                                             
000610         MOVE +1              TO INLE-MOT-KDAVVANT                        
000611       END-IF                                                             
000612       IF MID-KVRETINL-TRP(IX)  > +0                                      
000613         MOVE +3              TO INLE-MOT-KDAVVANT                        
000614       END-IF                                                             
000615       COMPUTE INLE-MOT-KVANTMOT  = WS-KVRETINL + WS-KVRETINL-SKR         
000616       MOVE DAT-TIAAMMDD    TO INLE-MOT-TIUPPDAT                          
000617       PERFORM IMS-REPL-INLE                                              
000618       PERFORM IMS-GHNP-INLE-INLE31                                       
000619       PERFORM UNTIL SEGMENT-SAKNAS                                       
000620         PERFORM IMS-DLET-INLE                                            
000621         PERFORM IMS-GHNP-INLE-INLE31                                     
000622       END-PERFORM                                                        
000623     ELSE                                                                 
000624       IF MID-KVRETINL(IX) > ZERO                                         
000625          ACCEPT INLE-DEL-TIREGDAT FROM DATE                              
000626          MOVE WS-IDDC          TO INLE-DEL-IDDC                          
000627          MOVE MID-KVRETINL(IX) TO INLE-DEL-KVRAPP                        
000628          PERFORM IMS-ISRT-INLE-INLE31                                    
000629       END-IF                                                             
000630     END-IF                                                               
000631     .                                                                    
000632     EJECT                                                                
000633 HAC-UPPD-LOGG    SECTION.                                                
000634                                                                          
000635     IF RET-CDC-SE                                                        
000636       IF  ARTC-CLAG-VKART         = ZERO                                 
000637         PERFORM HACB-LOGG-092-M108                                       
000638       END-IF                                                             
000639     END-IF                                                               
000640                                                                          
000641*- 20120419 ENLIGT BO H SKALL KINA INTE BOKAS VID AVVIKELSER              
000642*- KOD 62.PGM W4079200 BOKAR ALDRIG UPP AKS OCH DÅ SKALL W4079700         
000643*- INTE HELLER BOKA NER AKS.VID SKR BLIR DET OCKSÅ SKROTORDER OCH         
000644*- FELAKTIGT - LS.                                                        
000645     IF OKOD-FL-SALDOBOK-RETUR = JA                                       
000646        OR (LEV-DCS-SDC AND NOT LEV-DCS-CHINA)                            
000647                                                                          
000648       IF OKOD-FL-SALDOBOK-RETUR = JA                                     
000649         MOVE JA                   TO WS-FLLSBOK                          
000650       END-IF                                                             
000651                                                                          
000652       IF WS-KVAVV-KVAL > +0 OR                                           
000653          WS-KVAVV-KVANT > +0                                             
000654           PERFORM HACD-SALDO-AVV                                         
000655       END-IF                                                             
000656                                                                          
000657       MOVE '302'                  TO EKH-KDEKHHT                         
000658       MOVE '302'                  TO EKH-KDEKSHT                         
000659                                                                          
000660       MOVE LEV-IDDC          TO EKH-IDDC-SEND                            
000661       MOVE LEV-IDDC-RET      TO EKH-IDDC-REC                             
000662                                                                          
000663       COMPUTE EKH-KVANTAL = LEV-KVAVV-KVAL  +                            
000664                             LEV-KVAVV-KVANT                              
000665                                                                          
000666       IF RET-NDC-NA                                                      
000667         IF LEV-IDFTG = 53 OR 54                                          
000668           PERFORM HACH-EKOTRANS-A06                                      
000669         END-IF                                                           
000670       ELSE                                                               
000671         IF EKH-KVANTAL > +0                                              
000672           PERFORM S01-EKOTRANS-WDR801-WDR901                             
000673         END-IF                                                           
000674       END-IF                                                             
000675       IF MID-KVRETINL-TRP(IX) > ZERO                                     
000676         MOVE 101                    TO EKH-KDEKHHT                       
000677         MOVE 101                    TO EKH-KDEKSHT                       
000678         MOVE RET-WS-IDDC            TO EKH-IDDC-SEND                     
000679         MOVE RET-WS-IDDC            TO EKH-IDDC-REC                      
000680         MOVE MID-KVRETINL-TRP(IX)   TO EKH-KVANTAL                       
000681         PERFORM S01-EKOTRANS-WDR801-WDR901                               
000682       END-IF                                                             
000683                                                                          
000684       IF LEV-KDANMORS = '74'                                             
000685                                                                          
000686         COMPUTE EKH-KVANTAL = LEV-KVRETINL+                              
000687                               LEV-KVRETINL-SKR                           
000688                                                                          
000689         IF EKH-KVANTAL > +0                                              
000690           PERFORM S07-EKOTRANS-WDR901-KOD74                              
000691         END-IF                                                           
000692       END-IF                                                             
000693     END-IF                                                               
000694     .                                                                    
000695     EJECT                                                                
000696 HACB-LOGG-092-M108 SECTION.                                              
000697                                                                          
000698     MOVE W-IDLOPNRM             TO M108-IDLOPNRM                         
000699     MOVE ARTC-CLAG-ADLAGOMR     TO M108-ADLAGOMR                         
000700     MOVE ARTC-CLAG-ADGANG       TO M108-ADGANG                           
000701     MOVE ARTC-CLAG-ADPLATS      TO M108-ADPLATS                          
000702                                                                          
000703     PERFORM S03-RED-W211FEL-GNRL                                         
000704     MOVE ZERO                   TO W211FEL-KDORDKL                       
000705     MOVE '108'                  TO W211FEL-IDFELKODX                     
000706     MOVE M108-M108              TO W211FEL-FELMED                        
000707                                                                          
000708     MOVE '092'                  TO WS-ZZAC01-LOGGPOST-1-3                
000709     MOVE W211FEL-FELMED         TO WS-ZZAC01-LOGGPOST-4-90               
000710     MOVE W211FEL-SORT-FLT       TO WS-ZZAC01-SORTPOST                    
000711     PERFORM S02-SKAPA-ZZAC01                                             
000712     .                                                                    
000713     EJECT                                                                
000714 HACH-EKOTRANS-A06 SECTION.                                               
000715                                                                          
000716* A06-TRANSEN ÄR FÖR USA / CANADA, SKICKAS TILL LAB                       
000717                                                                          
000718     MOVE LOW-VALUE            TO EKO-FIL-WDR801-DATA                     
000719     MOVE 'A06'                TO A06-IDPTYP                              
000720     IF LEV-KDANMORS = '98'                                               
000721*       BUYBACK                                                           
000722       MOVE 'I30'              TO A06-KDEKOHT                             
000723     ELSE                                                                 
000724       MOVE LEV-IDDC         TO LEV-WS-IDDC                               
000725       IF LEV-CDC-SE OR LEV-GOOD-DDC                                      
000726*         VOR  ELLER DIREKTLEVERANS                                       
000727         MOVE 'O20'            TO A06-KDEKOHT                             
000728       ELSE                                                               
000729*         NDC FROM RETAILERS                                              
000730         MOVE 'O10'            TO A06-KDEKOHT                             
000731       END-IF                                                             
000732     END-IF                                                               
000733     MOVE LEV-IDFTG       TO A06-IDFTG                                    
000734                                                                          
000735     MOVE LEV-IDDC        TO LEV-WS-IDDC                                  
000736     IF LEV-GOOD-DDC                                                      
000737       MOVE '11'               TO A06-IDDC-SEND                           
000738     ELSE                                                                 
000739       MOVE LEV-IDDC      TO A06-IDDC-SEND                                
000740     END-IF                                                               
000741                                                                          
000742     MOVE LEV-IDDC-RET    TO A06-IDDC-REC                                 
000743     MOVE W-IDDISTR            TO A06-IDDISTR                             
000744     MOVE W-IDKUNDNR           TO A06-IDKUNDNR                            
000745     MOVE W-IDRAPPNR           TO A06-IDRAPPNR                            
000746     MOVE FUNCTION CURRENT-DATE(1:8) TO A06-DARETILL                      
000747     MOVE W-IDARTNR                  TO A06-IDARTNR                       
000748     MOVE WS-ARTC01-KDPRODSL         TO A06-KDPRODSL                      
000749     MOVE ARTC-CLAG-KDPSLLOC         TO A06-KDPSLLOC                      
000750     MOVE LEV-KVLEVANM-BEKR     TO A06-KVLEVANM                           
000751     MOVE LEV-KDANMORS          TO A06-KDANMORS                           
000752     MOVE MID-KVRETINL  (IX)         TO A06-KVRETINL                      
000753     MOVE MID-KVRETINL-SKR(IX)       TO A06-KVRETINL-SKR                  
000754     MOVE ARTS-SLAG-PRAVCOST         TO A06-PRAVCOST                      
000755                                                                          
000756     MOVE IDPGM                      TO EKO-FIL-IDPGM                     
000757     ACCEPT EKO-FIL-TIREGDAT   FROM DATE                                  
000758     ACCEPT EKO-FIL-TIKLOCK    FROM TIME                                  
000759     ADD  +1                         TO EKO-FIL-IDSEKVNR                  
000760     MOVE 'W510'                     TO EKO-FIL-CT-IDSYSTEM               
000761     MOVE 'A06'                      TO EKO-FIL-CT-IDPTYP                 
000762     MOVE ' '                        TO EKO-FIL-CT-IDVTYP                 
000763     PERFORM IMS-ISRT-EKOTRANS                                            
000764     PERFORM UNTIL SEGMENT-FINNS                                          
000765       ADD +1  TO EKO-FIL-IDSEKVNR                                        
000766       PERFORM IMS-ISRT-EKOTRANS                                          
000767     END-PERFORM                                                          
000768                                                                          
000769* LX6-TRANSEN ANVÄNDS FÖR LAB-AVSTÄMNINGEN                                
000770                                                                          
000771     COMPUTE A06-KVRETINL = LEV-KVAVV-KVAL +                              
000772                            LEV-KVAVV-KVANT +                             
000773                            MID-KVRETINL-SKR(IX)                          
000774     IF A06-KVRETINL NOT = 0                                              
000775       MOVE 'LX6'                  TO A06-IDPTYP                          
000776       MOVE ZERO                   TO A06-KVLEVANM                        
000777       MOVE ZERO                   TO A06-KVRETINL-SKR                    
000778       ADD  +1                     TO EKO-FIL-IDSEKVNR                    
000779       PERFORM IMS-ISRT-EKOTRANS                                          
000780       PERFORM UNTIL SEGMENT-FINNS                                        
000781         ADD +1  TO EKO-FIL-IDSEKVNR                                      
000782         PERFORM IMS-ISRT-EKOTRANS                                        
000783       END-PERFORM                                                        
000784     END-IF                                                               
000785     .                                                                    
000786     EJECT                                                                
000787 HACI-EKOTRANS-A06 SECTION.                                               
000788                                                                          
000789* A06-TRANSEN ÄR FÖR USA / CANADA, SKICKAS TILL LAB                       
000790                                                                          
000791     MOVE LOW-VALUE            TO EKO-FIL-WDR801-DATA                     
000792     MOVE 'A06'                TO A06-IDPTYP                              
000793     IF LEV-KDANMORS = '98'                                               
000794*       BUYBACK                                                           
000795       MOVE 'I30'              TO A06-KDEKOHT                             
000796     ELSE                                                                 
000797       MOVE LEV-IDDC      TO LEV-WS-IDDC                                  
000798       IF LEV-CDC-SE  OR LEV-GOOD-DDC                                     
000799*         VOR ELLER DIR.LEV.                                              
000800         MOVE 'O20'            TO A06-KDEKOHT                             
000801       ELSE                                                               
000802*         NDC FROM RETAILERS                                              
000803         MOVE 'O10'            TO A06-KDEKOHT                             
000804       END-IF                                                             
000805     END-IF                                                               
000806     MOVE LEV-IDFTG       TO A06-IDFTG                                    
000807                                                                          
000808     MOVE LEV-IDDC        TO LEV-WS-IDDC                                  
000809     IF LEV-GOOD-DDC                                                      
000810       MOVE '11'               TO A06-IDDC-SEND                           
000811     ELSE                                                                 
000812       MOVE LEV-IDDC      TO A06-IDDC-SEND                                
000813     END-IF                                                               
000814                                                                          
000815     MOVE LEV-IDDC-RET    TO A06-IDDC-REC                                 
000816     MOVE W-IDDISTR            TO A06-IDDISTR                             
000817     MOVE W-IDKUNDNR           TO A06-IDKUNDNR                            
000818     MOVE W-IDRAPPNR           TO A06-IDRAPPNR                            
000819     MOVE FUNCTION CURRENT-DATE(1:8) TO A06-DARETILL                      
000820     MOVE W-IDARTNR                  TO A06-IDARTNR                       
000821     MOVE WS-ARTC01-KDPRODSL         TO A06-KDPRODSL                      
000822     MOVE ARTC-CLAG-KDPSLLOC         TO A06-KDPSLLOC                      
000823     MOVE LEV-KVLEVANM-BEKR     TO A06-KVLEVANM                           
000824     MOVE LEV-KDANMORS          TO A06-KDANMORS                           
000825     MOVE MID-KVRETINL  (IX)         TO A06-KVRETINL                      
000826     MOVE MID-KVRETINL-SKR (IX)      TO A06-KVRETINL-SKR                  
000827     MOVE ARTS-SLAG-PRAVCOST         TO A06-PRAVCOST                      
000828                                                                          
000829     MOVE IDPGM                      TO EKO-FIL-IDPGM                     
000830     ACCEPT EKO-FIL-TIREGDAT   FROM DATE                                  
000831     ACCEPT EKO-FIL-TIKLOCK    FROM TIME                                  
000832     ADD  +1                         TO EKO-FIL-IDSEKVNR                  
000833     MOVE 'W510'                     TO EKO-FIL-CT-IDSYSTEM               
000834     MOVE 'A06'                      TO EKO-FIL-CT-IDPTYP                 
000835     MOVE ' '                        TO EKO-FIL-CT-IDVTYP                 
000836     PERFORM IMS-ISRT-EKOTRANS                                            
000837     PERFORM UNTIL SEGMENT-FINNS                                          
000838       ADD +1  TO EKO-FIL-IDSEKVNR                                        
000839       PERFORM IMS-ISRT-EKOTRANS                                          
000840     END-PERFORM                                                          
000841                                                                          
000842* LX6-TRANSEN ANVÄNDS FÖR LAB-AVSTÄMNINGEN                                
000843                                                                          
000844     MOVE MID-KVRETINL-SKR(IX)     TO A06-KVRETINL                        
000845     IF A06-KVRETINL NOT = 0                                              
000846       MOVE 'LX6'                  TO A06-IDPTYP                          
000847       MOVE ZERO                   TO A06-KVLEVANM                        
000848       MOVE ZERO                   TO A06-KVRETINL-SKR                    
000849       ADD  +1                     TO EKO-FIL-IDSEKVNR                    
000850       PERFORM IMS-ISRT-EKOTRANS                                          
000851       PERFORM UNTIL SEGMENT-FINNS                                        
000852         ADD +1  TO EKO-FIL-IDSEKVNR                                      
000853         PERFORM IMS-ISRT-EKOTRANS                                        
000854       END-PERFORM                                                        
000855     END-IF                                                               
000856     .                                                                    
000857     EJECT                                                                
000858 HACD-SALDO-AVV SECTION.                                                  
000859                                                                          
000860     IF RET-DCS-CDC                                                       
000861       PERFORM IMS-GHU-ARTC-ARTC11                                        
000862       SUBTRACT WS-KVAVV-KVAL  FROM ARTC-CLAG-KVAKS-CDC                   
000863       SUBTRACT WS-KVAVV-KVANT FROM ARTC-CLAG-KVAKS-CDC                   
000864       IF WS-KVAVV-KVAL NOT = 0                                           
000865         MOVE '-'                TO LOGG-IDTECKEN-KVAKS                   
000866         MOVE ' '                TO LOGG-IDTECKEN-KVLS                    
000867         MOVE WS-KVAVV-KVAL      TO LOGG-KVART-SALDO                      
000868         PERFORM S05-SKAPA-SALDOLOGG                                      
000869       END-IF                                                             
000870       IF WS-KVAVV-KVANT NOT = 0                                          
000871         MOVE '-'                 TO LOGG-IDTECKEN-KVAKS                  
000872         MOVE ' '                 TO LOGG-IDTECKEN-KVLS                   
000873         MOVE WS-KVAVV-KVANT      TO LOGG-KVART-SALDO                     
000874         PERFORM S05-SKAPA-SALDOLOGG                                      
000875       END-IF                                                             
000876       PERFORM IMS-REPL-ARTC-ARTC                                         
000877     ELSE                                                                 
000878       PERFORM IMS-GHU-ARTS-WDK711                                        
000879                                                                          
000880**** DET KAN VARA SÅ ATT MAN HAR BYTT LAGER FÖR KUNDEN OCH DÅ             
000881**** ANVÄNDER MAN ETT LAGER DÄR DET INTE FINNS ETT AVERAGE COST           
000882       IF RET-DCS-CHINA                                                   
000883         IF ARTS-SLAG-PRAVCOST = ZERO                                     
000884           PERFORM IMS-GU-WDK711-CN                                       
000885           MOVE ARTS-SLAG-PRAVCOST TO WS-SLAG-PRAVCOST                    
000886           PERFORM IMS-GHU-ARTS-WDK711                                    
000887           MOVE WS-SLAG-PRAVCOST   TO ARTS-SLAG-PRAVCOST                  
000888         END-IF                                                           
000889       END-IF                                                             
000890****                                                                      
000891                                                                          
000892       SUBTRACT WS-KVAVV-KVAL  FROM ARTS-SLAG-KVAKS-SDC                   
000893       SUBTRACT WS-KVAVV-KVANT FROM ARTS-SLAG-KVAKS-SDC                   
000894       IF WS-KVAVV-KVAL NOT = 0                                           
000895         MOVE '-'                TO LOGG-IDTECKEN-KVAKS                   
000896         MOVE ' '                TO LOGG-IDTECKEN-KVLS                    
000897         MOVE WS-KVAVV-KVAL      TO LOGG-KVART-SALDO                      
000898         PERFORM S06-SKAPA-SALDOLOGG                                      
000899       END-IF                                                             
000900       IF WS-KVAVV-KVANT NOT = 0                                          
000901         MOVE '-'                TO LOGG-IDTECKEN-KVAKS                   
000902         MOVE ' '                TO LOGG-IDTECKEN-KVLS                    
000903         MOVE WS-KVAVV-KVANT     TO LOGG-KVART-SALDO                      
000904         PERFORM S06-SKAPA-SALDOLOGG                                      
000905       END-IF                                                             
000906       PERFORM IMS-REPL-ARTS-ARTS                                         
000907     END-IF                                                               
000908     .                                                                    
000909     EJECT                                                                
000910                                                                          
000911 HAD-UPPD-WDK711  SECTION.                                                
000912     PERFORM IMS-GHU-ARTS-WDK711                                          
000913                                                                          
000914**** DET KAN VARA SÅ ATT MAN HAR BYTT LAGER FÖR KUNDEN OCH DÅ             
000915**** ANVÄNDER MAN ETT LAGER DÄR DET INTE FINNS ETT AVERAGE COST           
000916     IF RET-DCS-CHINA                                                     
000917       IF ARTS-SLAG-PRAVCOST = ZERO                                       
000918         PERFORM IMS-GU-WDK711-CN                                         
000919         MOVE ARTS-SLAG-PRAVCOST TO WS-SLAG-PRAVCOST                      
000920         PERFORM IMS-GHU-ARTS-WDK711                                      
000921         MOVE WS-SLAG-PRAVCOST   TO ARTS-SLAG-PRAVCOST                    
000922       END-IF                                                             
000923     END-IF                                                               
000924****                                                                      
000925**** DET KAN VARA SÅ ATT MAN HAR BYTT LAGER FÖR KUNDEN OCH DÅ             
000926**** ANVÄNDER MAN ETT LAGER DÄR DET INTE FINNS ETT AVERAGE COST           
000927     IF RET-DCS-THAILAND                                                  
000928       IF LEV-PRARTSJK > ZERO                                             
000929         MOVE LEV-TIFAKT      TO WS-KRE-DATUM                             
000930         MOVE WS-KRE-DATUM(1:2)    TO AVG-TIAA                            
000931         MOVE WS-KRE-DATUM(3:2)    TO AVG-TIMM                            
000932         MOVE 021                  TO AVG-KDCALL                          
000933         MOVE ARTS-SLAG-PRAVCOST   TO AVG-PRAVCOST-OLD                    
000934         COMPUTE WS-KVLS = ARTS-SLAG-KVLS + ARTS-SLAG-KVEFRS              
000935         IF WS-KVLS < ZERO                                                
000936           MOVE ZERO               TO AVG-KVLS-OLD                        
000937         ELSE                                                             
000938           MOVE WS-KVLS            TO AVG-KVLS-OLD                        
000939         END-IF                                                           
000940         MOVE 0                    TO AVG-PRAVCOST-NEW                    
000941         MOVE LEV-PRARTSJK    TO AVG-PRARTNTO                             
000942         MOVE ZERO                 TO AVG-PRKURS                          
000943         MOVE LEV-IDDC-RET    TO AVG-IDDC                                 
000944         MOVE SPACE                TO AVG-KDVALISO                        
000945         MOVE MID-KVRETINL(IX)     TO AVG-KVANTMOT                        
000946         MOVE ARTC-CLAG-KDPSLLOC   TO AVG-KDPSLLOC                        
000947         MOVE ZEROS                TO AVG-KDPRODSL                        
000948                                      AVG-IDFKNGRP                        
000949         CALL W510AVG USING AVG-W510AVG 9305-PCB                          
000950                            AVG-WDB6-PCB                                  
000951         IF AVG-KDSVAR = ' '                                              
000952            MOVE AVG-PRAVCOST-NEW  TO ARTS-SLAG-PRAVCOST                  
000953         ELSE                                                             
000954            MOVE AVG-PRAVCOST-OLD  TO ARTS-SLAG-PRAVCOST                  
000955         END-IF                                                           
000956       END-IF                                                             
000957     END-IF                                                               
000958****                                                                      
000959                                                                          
000960     SUBTRACT MID-KVRETINL(IX)     FROM ARTS-SLAG-KVAKS-SDC               
000961     ADD      MID-KVRETINL(IX)     TO   ARTS-SLAG-KVLS                    
000962     IF MID-KVRETINL(IX) NOT = 0                                          
000963       MOVE '-'                    TO LOGG-IDTECKEN-KVAKS                 
000964       MOVE '+'                    TO LOGG-IDTECKEN-KVLS                  
000965       MOVE MID-KVRETINL(IX)       TO LOGG-KVART-SALDO                    
000966       PERFORM S06-SKAPA-SALDOLOGG                                        
000967     END-IF                                                               
000968     SUBTRACT MID-KVRETINL-SKR(IX) FROM ARTS-SLAG-KVAKS-SDC               
000969     IF MID-KVRETINL-SKR(IX) NOT = 0                                      
000970       MOVE '-'                    TO LOGG-IDTECKEN-KVAKS                 
000971       MOVE ' '                    TO LOGG-IDTECKEN-KVLS                  
000972       MOVE MID-KVRETINL-SKR(IX)   TO LOGG-KVART-SALDO                    
000973       PERFORM S06-SKAPA-SALDOLOGG                                        
000974     END-IF                                                               
000975     SUBTRACT MID-KVRETINL-TRP(IX) FROM ARTS-SLAG-KVLS                    
000976     IF MID-KVRETINL-TRP(IX) NOT = 0                                      
000977       MOVE ' '                    TO LOGG-IDTECKEN-KVAKS                 
000978       MOVE '-'                    TO LOGG-IDTECKEN-KVLS                  
000979       MOVE MID-KVRETINL-TRP(IX)   TO LOGG-KVART-SALDO                    
000980       PERFORM S06-SKAPA-SALDOLOGG                                        
000981     END-IF                                                               
000982                                                                          
000983     PERFORM IMS-REPL-ARTS-ARTS                                           
000984     IF RET-DCS-MEXICO AND ARTS-SLAG-IDDC-REF > SPACES                    
000985      MOVE 'N' TO IDTRACK-QTY-SW                                          
000986      MOVE ZERO TO WS-TEMP-RETMXC                                         
000987      MOVE ZERO TO WS-TEMP-RETMXCB                                        
000988      MOVE ZERO TO WS-TEMP-RETKVAR                                        
000989      MOVE 9999999999999999    TO W-DAINLEV                               
000990      MOVE MID-KVRETINL(IX)    TO WS-KVRETINL-MXC                         
000991      PERFORM IMS-GHNP-WDK728-LAST                                        
000992      COMPUTE WS-TEMP-RETKVAR = TRCK-KVANTMOT -                           
000993              TRCK-KVTRACK-KVAR                                           
000994      PERFORM UNTIL SEGMENT-SAKNAS OR IDTRACK-QTY-DONE                    
000995       IF TRCK-KVTRACK-KVAR <= TRCK-KVANTMOT AND                          
000996          WS-TEMP-RETKVAR NOT = ZERO                                      
000997        COMPUTE WS-TEMP-RETMXCB = WS-TEMP-RETMXCB +                       
000998                                  WS-TEMP-RETMXC                          
000999        COMPUTE WS-TEMP-RETMXC = WS-TEMP-RETMXC +                         
001000          (TRCK-KVANTMOT - TRCK-KVTRACK-KVAR)                             
001001        IF WS-TEMP-RETMXC <= WS-KVRETINL-MXC                              
001002         MOVE TRCK-KVANTMOT TO TRCK-KVTRACK-KVAR                          
001003         MOVE WS-TEMP-RETMXC TO TRK-KVANTMOT                              
001004         PERFORM IMS-REPL-WDK728                                          
001005        ELSE                                                              
001006         COMPUTE WS-TEMP-RETMXCB = WS-KVRETINL-MXC -                      
001007                                   WS-TEMP-RETMXCB                        
001008         ADD WS-TEMP-RETMXCB TO TRCK-KVTRACK-KVAR                         
001009         MOVE WS-TEMP-RETMXCB TO TRK-KVANTMOT                             
001010         PERFORM IMS-REPL-WDK728                                          
001011         MOVE 'J' TO IDTRACK-QTY-SW                                       
001012        END-IF                                                            
001013        MOVE TRCK-IDTRACK TO TRK-IDTRACK                                  
001014        MOVE TRCK-DAINLEV(1:8) TO TRK-DADATUM                             
001015        PERFORM IMS-ISRT-WDA222                                           
001016       END-IF                                                             
001017       IF WS-TEMP-RETMXC = WS-KVRETINL-MXC                                
001018        MOVE 'J' TO IDTRACK-QTY-SW                                        
001019       END-IF                                                             
001020       IF IDTRACK-QTY-NOT-DONE                                            
001021          PERFORM IMS-GHU-ARTS-WDK711                                     
001022          MOVE  TRCK-DAINLEV TO W-DAINLEV                                 
001023          PERFORM IMS-GHNP-WDK728-LAST                                    
001024          COMPUTE WS-TEMP-RETKVAR = TRCK-KVANTMOT -                       
001025                  TRCK-KVTRACK-KVAR                                       
001026       END-IF                                                             
001027      END-PERFORM                                                         
001028     END-IF                                                               
001029     .                                                                    
001030     EJECT                                                                
001031                                                                          
001032 HAE-UPPD-INLC SECTION.                                                   
001033     PERFORM IMS-GHU-INLC11                                               
001034     MOVE 'R32'                 TO INLC-INL-IDPTYP                        
001035     MOVE LEV-KVRETINL     TO INLC-INL-KVANTMOT                           
001036     MOVE LEV-KVRETINL-SKR TO INLC-INL-KVART-SKROT                        
001037     MOVE LEV-IDANSTNR-RET TO INLC-INL-IDUSER-003                         
001038     ACCEPT INLC-INL-TIINLINL FROM DATE                                   
001039     PERFORM IMS-REPL-INLC                                                
001040     .                                                                    
001041     EJECT                                                                
001042 HAF-SKAPA-WDR6-LOGG SECTION.                                             
001043                                                                          
001044*--- LOGGAR ALLA NDC-RETURER DISTR 8111 OCH 8211 SOM FÅR                  
001045*--- KDLEVANM = 7                                                         
001046                                                                          
001047     MOVE IDPGM                  TO LOG-FIL-IDPGM                         
001048     ACCEPT LOG-FIL-TIREGDAT FROM DATE                                    
001049     ACCEPT LOG-FIL-TIKLOCK FROM TIME                                     
001050     MOVE ZERO                   TO LOG-FIL-IDSEKVNR                      
001051     MOVE 'W407'                 TO LOG-FIL-CT-IDSYSTEM                   
001052     MOVE 'R32'                  TO LOG-FIL-CT-IDPTYP                     
001053     MOVE 'A'                    TO LOG-FIL-CT-IDVTYP                     
001054     ADD +1                      TO LOG-FIL-IDSEKVNR                      
001055                                                                          
001056     MOVE '7'                    TO R32-KDLEVANM                          
001057     MOVE ZERO                   TO R32-SUSTDTOT                          
001058     MOVE W-IDDISTR              TO R32-IDDISTR                           
001059     MOVE W-IDKUNDNR             TO R32-IDKUNDNR                          
001060     MOVE W-IDRAPPNR             TO R32-IDRAPPNR                          
001061                                                                          
001062     PERFORM IMS-ISRT-WDR601                                              
001063     IF SEGMENT-FINNS-REDAN                                               
001064        PERFORM UNTIL NOT SEGMENT-FINNS-REDAN                             
001065           ADD +1 TO LOG-FIL-IDSEKVNR                                     
001066           PERFORM IMS-ISRT-WDR601                                        
001067        END-PERFORM                                                       
001068     END-IF                                                               
001069     .                                                                    
001070     EJECT                                                                
001071 Z-FINIT          SECTION.                                                
001072                                                                          
001073     MOVE INF-UPDATE-DONE        TO MSG-KOM-IDMFSMED                      
001074                                                                          
001075     PERFORM IMS-ISRT-DISP-MSG                                            
001076     .                                                                    
001077     EJECT                                                                
001078 S01-EKOTRANS-WDR801-WDR901 SECTION.                                      
001079                                                                          
001080     MOVE LEV-IDDC-RET        TO RET-WS-IDDC                              
001081     IF RET-XDC-NON-VCC-OWNED OR RET-LDC-CN                               
001082       MOVE 'W4079700'             TO EKO-FIL-IDPGM                       
001083       ACCEPT EKO-FIL-TIREGDAT FROM DATE                                  
001084       ACCEPT EKO-FIL-TIKLOCK  FROM TIME                                  
001085       MOVE 1                      TO EKO-FIL-IDSEKVNR                    
001086       IF RET-NDC-CN OR RET-LDC-CN                                        
001087         MOVE 'W570'               TO EKO-FIL-IDCPYTXT(1:4)               
001088       ELSE                                                               
001089         IF RET-NDC-IN                                                    
001090           MOVE 'W515'             TO EKO-FIL-IDCPYTXT(1:4)               
001091         ELSE                                                             
001092           MOVE RET-DCS-KDTRADP    TO EKO-FIL-IDCPYTXT(1:4)               
001093         END-IF                                                           
001094       END-IF                                                             
001095       MOVE 'EKHA'                 TO EKO-FIL-IDCPYTXT(5:4)               
001096       PERFORM S01A-EKOTRANS-WDR801                                       
001097     ELSE                                                                 
001098       MOVE 'W4079700'             TO FIL-IDPGM                           
001099       MOVE WS-AAAAMMDD            TO FIL-DAREGDAT                        
001100       MOVE WS-TTMMSSTH            TO FIL-TIKLOCK                         
001101       MOVE 1                      TO FIL-IDSEKVNR                        
001102       MOVE 'W510EKHA'             TO FIL-IDCPYTXT                        
001103       MOVE MSG-SIGNON-USERID      TO FIL-IDUSER                          
001104       PERFORM S01B-EKOTRANS-WDR901                                       
001105     END-IF                                                               
001106     .                                                                    
001107     EJECT                                                                
001108 S01A-EKOTRANS-WDR801 SECTION.                                            
001109                                                                          
001110     MOVE EKH-KDEKHHT            TO EKO-EKH-KDEKHHT                       
001111     MOVE EKH-KDEKSHT            TO EKO-EKH-KDEKSHT                       
001112     MOVE EKH-IDDC-SEND          TO EKO-EKH-IDDC-SEND                     
001113     MOVE EKH-IDDC-REC           TO EKO-EKH-IDDC-REC                      
001114     MOVE EKH-KVANTAL            TO EKO-EKH-KVANTAL                       
001115                                                                          
001116     MOVE 'DET'                  TO EKO-EKH-KDEKNIVA                      
001117     MOVE W-IDDISTR              TO EKO-EKH-IDDISTR                       
001118     MOVE W-IDKUNDNR             TO EKO-EKH-IDKUNDNR                      
001119                                                                          
001120*    MOVE W-IDRAPPNR             TO EKO-EKH-IDVERGL                       
001121     MOVE 'VO'                   TO CIA-IDARTPRE-IN                       
001122     MOVE W-IDRAPPNR             TO CIA-IDARTBET-IN                       
001123     CALL W009CIA USING             CIA-W009CIA                           
001124     MOVE CIA-IDARTBET-UT        TO EKO-EKH-IDVERGL                       
001125                                                                          
001126     MOVE WS-AAAAMMDD            TO EKO-EKH-DAVERDAT                      
001127     MOVE WS-ARTC01-KDPRODSL     TO EKO-EKH-KDPRODSL                      
001128     MOVE 0                      TO EKO-EKH-KDPSLLOC                      
001129     MOVE W-IDARTNR              TO EKO-EKH-IDARTNR                       
001130     MOVE ' '                    TO EKO-EKH-FLLSBOK                       
001131                                                                          
001132     MOVE 1.00                   TO EKO-EKH-PRKURS                        
001133     MOVE 0                      TO EKO-EKH-PRARTNTO                      
001134     MOVE 0                      TO EKO-EKH-PRARTSJK                      
001135     MOVE 0                      TO EKO-EKH-PRHEMTAG                      
001136     MOVE ARTS-SLAG-PRAVCOST     TO EKO-EKH-PRARTSTD                      
001137     MOVE 0                      TO EKO-EKH-PRLANDCO                      
001138     MOVE 0                      TO EKO-EKH-PRINK                         
001139     MOVE 0                      TO EKO-EKH-PRDIRLON                      
001140     MOVE 0                      TO EKO-EKH-PRDMTRL                       
001141     MOVE 0                      TO EKO-EKH-PROVRPAL                      
001142     MOVE 0                      TO EKO-EKH-SUBEL                         
001143                                                                          
001144     MOVE LEV-KDANMORS      TO EKO-EKH-KDANMORS                           
001145     MOVE LEV-IDANALYS      TO EKO-EKH-IDANALYS                           
001146     MOVE LEV-IDKONTO       TO EKO-EKH-IDKONTO                            
001147     MOVE LEV-IDKST         TO EKO-EKH-IDKST                              
001148     MOVE ZERO                   TO EKO-EKH-BEVAT                         
001149                                    EKO-EKH-KDFRAKT                       
001150                                    EKO-EKH-SUVAT                         
001151     MOVE ZERO                   TO EKO-EKH-DAAVIDAT                      
001152                                    EKO-EKH-IDAVINR                       
001153                                    EKO-EKH-KDAVVTYP                      
001154                                    EKO-EKH-KDRT                          
001155                                    EKO-EKH-KVANTMOT                      
001156                                    EKO-EKH-KVAVIS                        
001157                                                                          
001158     MOVE MFS-IDTRANS            TO EKO-EKH-IDTRANS                       
001159     MOVE WS-KDSORT              TO EKO-EKH-KDSORT                        
001160     MOVE SPACE                  TO EKO-EKH-IDLEVNR                       
001161     MOVE SPACE                  TO EKO-EKH-FLDCET                        
001162     MOVE SPACE                  TO EKO-EKH-IDKUNDRF                      
001163     MOVE SPACE                  TO EKO-EKH-IDFAKT-EXP                    
001164     MOVE RET-DCS-KDVALISO       TO EKO-EKH-KDVALISO                      
001165     MOVE RET-DCS-KDTRADP        TO EKO-EKH-KDTRADP                       
001166                                                                          
001167     PERFORM IMS-ISRT-EKOTRANS                                            
001168     PERFORM UNTIL SEGMENT-FINNS                                          
001169       ADD +1  TO EKO-FIL-IDSEKVNR                                        
001170       PERFORM IMS-ISRT-EKOTRANS                                          
001171     END-PERFORM                                                          
001172     .                                                                    
001173     EJECT                                                                
001174 S01B-EKOTRANS-WDR901 SECTION.                                            
001175                                                                          
001176     MOVE 'W4079700'             TO FIL-IDPGM                             
001177     MOVE WS-AAAAMMDD            TO FIL-DAREGDAT                          
001178     MOVE WS-TTMMSSTH            TO FIL-TIKLOCK                           
001179     MOVE 1                      TO FIL-IDSEKVNR                          
001180     MOVE 'W510EKHA'             TO FIL-IDCPYTXT                          
001181     MOVE MSG-SIGNON-USERID      TO FIL-IDUSER                            
001182     MOVE 'DET'                  TO EKH-KDEKNIVA                          
001183     MOVE W-IDDISTR              TO EKH-IDDISTR                           
001184     MOVE W-IDKUNDNR             TO EKH-IDKUNDNR                          
001185                                                                          
001186*    MOVE W-IDRAPPNR             TO EKH-IDVERGL                           
001187     MOVE 'VO'                   TO CIA-IDARTPRE-IN                       
001188     MOVE W-IDRAPPNR             TO CIA-IDARTBET-IN                       
001189     CALL W009CIA USING             CIA-W009CIA                           
001190     MOVE CIA-IDARTBET-UT        TO EKH-IDVERGL                           
001191                                                                          
001192     MOVE WS-AAAAMMDD            TO EKH-DAVERDAT                          
001193     MOVE WS-ARTC01-KDPRODSL     TO EKH-KDPRODSL                          
001194     MOVE 0                      TO EKH-KDPSLLOC                          
001195     MOVE W-IDARTNR              TO EKH-IDARTNR                           
001196     MOVE ' '                    TO EKH-FLLSBOK                           
001197                                                                          
001198     MOVE 'SEK'                  TO EKH-KDVALISO                          
001199     MOVE 1.00                   TO EKH-PRKURS                            
001200     MOVE 0                      TO EKH-PRARTNTO                          
001201     MOVE 0                      TO EKH-PRARTSJK                          
001202     MOVE ARTC-CLAG-PRHEMTAG     TO EKH-PRHEMTAG                          
001203     MOVE ARTC-CLAG-PRARTSTD     TO EKH-PRARTSTD                          
001204     MOVE 0                      TO EKH-PRLANDCO                          
001205     MOVE 0                      TO EKH-PRINK                             
001206     MOVE 0                      TO EKH-PRDIRLON                          
001207     MOVE 0                      TO EKH-PRDMTRL                           
001208     MOVE 0                      TO EKH-PROVRPAL                          
001209     MOVE 0                      TO EKH-SUBEL                             
001210                                                                          
001211     MOVE LEV-KDANMORS      TO EKH-KDANMORS                               
001212     MOVE LEV-IDANALYS      TO EKH-IDANALYS                               
001213     MOVE LEV-IDKONTO       TO EKH-IDKONTO                                
001214     MOVE LEV-IDKST         TO EKH-IDKST                                  
001215     MOVE ZERO                   TO EKH-BEVAT                             
001216                                    EKH-KDFRAKT                           
001217                                    EKH-SUVAT                             
001218     MOVE ZERO                   TO EKH-DAAVIDAT                          
001219                                    EKH-IDAVINR                           
001220                                    EKH-KDAVVTYP                          
001221                                    EKH-KDRT                              
001222                                    EKH-KVANTMOT                          
001223                                    EKH-KVAVIS                            
001224                                                                          
001225     MOVE MFS-IDTRANS            TO EKH-IDTRANS                           
001226     MOVE WS-KDSORT              TO EKH-KDSORT                            
001227     MOVE 'SEPV'                 TO EKH-KDTRADP                           
001228     MOVE SPACE                  TO EKH-IDLEVNR                           
001229     MOVE SPACE                  TO EKH-FLDCET                            
001230     MOVE SPACE                  TO EKH-IDKUNDRF                          
001231     MOVE SPACE                  TO EKH-IDFAKT-EXP                        
001232                                                                          
001233     PERFORM IMS-ISRT-WDR901                                              
001234     PERFORM UNTIL SEGMENT-FINNS                                          
001235       ADD +1  TO FIL-IDSEKVNR                                            
001236       PERFORM IMS-ISRT-WDR901                                            
001237     END-PERFORM                                                          
001238     .                                                                    
001239     EJECT                                                                
001240 S02-SKAPA-ZZAC01 SECTION.                                                
001241                                                                          
001242     ACCEPT ZZAC01-TIAAMMDD      FROM DATE                                
001243     ACCEPT ZZAC01-TIKLOCK       FROM TIME                                
001244                                                                          
001245     ADD +1                      TO WS-IDLOGLOP                           
001246     MOVE WS-IDLOGLOP            TO ZZAC01-IDLOGLOP                       
001247                                                                          
001248     MOVE WS-ZZAC01-LOGGPOST     TO ZZAC01-LOGGPOST                       
001249     MOVE WS-ZZAC01-SORTPOST     TO ZZAC01-SORTPOST                       
001250                                                                          
001251     PERFORM IMS-ISRT-ZZAC-LOGG                                           
001252     PERFORM UNTIL SEGMENT-FINNS                                          
001253       ACCEPT ZZAC01-TIAAMMDD      FROM DATE                              
001254       ACCEPT ZZAC01-TIKLOCK       FROM TIME                              
001255       ADD +1                      TO WS-IDLOGLOP                         
001256       MOVE WS-IDLOGLOP            TO ZZAC01-IDLOGLOP                     
001257       PERFORM IMS-ISRT-ZZAC-LOGG                                         
001258     END-PERFORM                                                          
001259     .                                                                    
001260     EJECT                                                                
001261 S03-RED-W211FEL-GNRL SECTION.                                            
001262                                                                          
001263     MOVE ZERO                   TO W211FEL-SORT-FLT                      
001264     MOVE SPACE                  TO W211FEL-FILLER2                       
001265                                                                          
001266     MOVE 'R32'                  TO W211FEL-IDPTYP-S                      
001267     MOVE W-IDARTNR              TO W211FEL-SORTBGP                       
001268     MOVE 1                      TO W211FEL-KDFELMRK                      
001269     .                                                                    
001270     EJECT                                                                
001271 S04-GNP-KREE-KREE11     SECTION.                                         
001272                                                                          
001273     MOVE NEJ      TO OKOD-FL-RETILL                                      
001274                      OKOD-FL-INTERNUPPACKNING                            
001275     PERFORM UNTIL OKOD-FL-RETILL = 'J' OR SEGMENT-SAKNAS                 
001276                OR OKOD-FL-INTERNUPPACKNING = 'J'                         
001277        PERFORM IMS-GNP-KREE-KREE11                                       
001278        IF LEV-KDKREBEH(1:1) = 'Y'   OR                                   
001279           LEV-KDKREBEH(1:1) = 'J'   OR                                   
001280           LEV-KDKREBEH(1:1) = 'C'   OR                                   
001281           LEV-KDKREBEH      = 'D01' OR                                   
001282           LEV-KDKREBEH      = 'D02' OR                                   
001283           LEV-KDKREBEH      = 'D03'                                      
001284          MOVE LEV-KDANMORS TO OKOD-KDANMORS                              
001285          CALL W418OKOD USING OKOD-W418OKOD                               
001286        END-IF                                                            
001287     END-PERFORM                                                          
001288                                                                          
001289     .                                                                    
001290     EJECT                                                                
001291 S05-SKAPA-SALDOLOGG SECTION.                                             
001292                                                                          
001293     MOVE W-IDARTNR                TO LOGG-IDARTNR                        
001294                                                                          
001295     MOVE FUNCTION CURRENT-DATE (1:8) TO WS-AAAAMMDD                      
001296     COMPUTE LOGG-DAREGDAT-9KOMPL  = 99999999                             
001297                                   - WS-AAAAMMDD                          
001298     MOVE FUNCTION CURRENT-DATE (9:8) TO WS-TTMMSSTH                      
001299     COMPUTE LOGG-TIKLOCK-9KOMPL   = 999999999                            
001300                                   - WS-TTMMSSTH                          
001301     MOVE 9                        TO LOGG-IDSEKVNR                       
001302     MOVE WC-CDC-SE                TO LOGG-IDDC                           
001303     MOVE 'DISC'                   TO LOGG-IDHUVTYP                       
001304     MOVE 'DEV'                    TO LOGG-IDSUBTYP                       
001305     MOVE 'W4079700'               TO LOGG-IDPGM                          
001306     MOVE MFS-IDTRANS              TO LOGG-IDTRANS                        
001307     MOVE MSG-SIGNON-USERID        TO LOGG-IDUSER                         
001308     MOVE SPACE                    TO LOGG-REF                            
001309     MOVE W-IDDISTR                TO LOGG-IDDISTR                        
001310     MOVE W-IDKUNDNR               TO LOGG-IDKUNDNR                       
001311     MOVE W-IDRAPPNR               TO LOGG-IDRAPPNR                       
001312     MOVE ' '                      TO LOGG-IDTECKEN-KVAKS-PAV             
001313     MOVE ' '                      TO LOGG-IDTECKEN-KVEFRS                
001314                                                                          
001315     COMPUTE LOGG-KVAKS = ARTC-CLAG-KVAKS-CDC                             
001316                        + ARTC-CLAG-KVAKS-T                               
001317                                                                          
001318     MOVE ARTC-CLAG-KVAKS-PAV      TO LOGG-KVAKS-PAV                      
001319     MOVE ARTC-CLAG-KVEFRS         TO LOGG-KVEFRS                         
001320     MOVE ARTC-CLAG-KVLS           TO LOGG-KVLS                           
001321     MOVE 000000                   TO LOGG-DAREGDAT-LADD                  
001322                                                                          
001323     PERFORM IMS-ISRT-WDL901                                              
001324     IF SEGMENT-FINNS-REDAN                                               
001325       PERFORM UNTIL NOT SEGMENT-FINNS-REDAN                              
001326         SUBTRACT 1       FROM LOGG-IDSEKVNR                              
001327         PERFORM IMS-ISRT-WDL901                                          
001328       END-PERFORM                                                        
001329     END-IF                                                               
001330     .                                                                    
001331     EJECT                                                                
001332 S06-SKAPA-SALDOLOGG SECTION.                                             
001333                                                                          
001334     MOVE W-IDARTNR                TO LOGG-IDARTNR                        
001335                                                                          
001336     MOVE FUNCTION CURRENT-DATE (1:8) TO WS-AAAAMMDD                      
001337     COMPUTE LOGG-DAREGDAT-9KOMPL  = 99999999                             
001338                                   - WS-AAAAMMDD                          
001339     MOVE FUNCTION CURRENT-DATE (9:8) TO WS-TTMMSSTH                      
001340     COMPUTE LOGG-TIKLOCK-9KOMPL   = 999999999                            
001341                                   - WS-TTMMSSTH                          
001342     MOVE 9                        TO LOGG-IDSEKVNR                       
001343     MOVE ARTS-SLAG-IDDC           TO LOGG-IDDC                           
001344     MOVE 'DISC'                   TO LOGG-IDHUVTYP                       
001345     MOVE 'DEV'                    TO LOGG-IDSUBTYP                       
001346     MOVE 'W4079700'               TO LOGG-IDPGM                          
001347     MOVE MFS-IDTRANS              TO LOGG-IDTRANS                        
001348     MOVE MSG-SIGNON-USERID        TO LOGG-IDUSER                         
001349     MOVE SPACE                    TO LOGG-REF                            
001350     MOVE W-IDDISTR                TO LOGG-IDDISTR                        
001351     MOVE W-IDKUNDNR               TO LOGG-IDKUNDNR                       
001352     MOVE W-IDRAPPNR               TO LOGG-IDRAPPNR                       
001353     MOVE ' '                      TO LOGG-IDTECKEN-KVAKS-PAV             
001354     MOVE ' '                      TO LOGG-IDTECKEN-KVEFRS                
001355     MOVE ARTS-SLAG-KVAKS-SDC      TO LOGG-KVAKS                          
001356     MOVE ARTS-SLAG-KVAKS-PAV      TO LOGG-KVAKS-PAV                      
001357     MOVE ARTS-SLAG-KVEFRS         TO LOGG-KVEFRS                         
001358     MOVE ARTS-SLAG-KVLS           TO LOGG-KVLS                           
001359     MOVE 000000                   TO LOGG-DAREGDAT-LADD                  
001360                                                                          
001361     PERFORM IMS-ISRT-WDL901                                              
001362     IF SEGMENT-FINNS-REDAN                                               
001363       PERFORM UNTIL NOT SEGMENT-FINNS-REDAN                              
001364         SUBTRACT 1       FROM LOGG-IDSEKVNR                              
001365         PERFORM IMS-ISRT-WDL901                                          
001366       END-PERFORM                                                        
001367     END-IF                                                               
001368     .                                                                    
001369     EJECT                                                                
001370 S07-EKOTRANS-WDR901-KOD74 SECTION.                                       
001371                                                                          
001372     MOVE 'W4079700'             TO FIL-IDPGM                             
001373     MOVE WS-AAAAMMDD            TO FIL-DAREGDAT                          
001374     MOVE WS-TTMMSSTH            TO FIL-TIKLOCK                           
001375     MOVE 1                      TO FIL-IDSEKVNR                          
001376     MOVE 'W510EKHA'             TO FIL-IDCPYTXT                          
001377     MOVE MSG-SIGNON-USERID      TO FIL-IDUSER                            
001378     MOVE 'DET'                  TO EKH-KDEKNIVA                          
001379     MOVE W-IDDISTR              TO EKH-IDDISTR                           
001380     MOVE W-IDKUNDNR             TO EKH-IDKUNDNR                          
001381                                                                          
001382     MOVE 'VO'                   TO CIA-IDARTPRE-IN                       
001383     MOVE W-IDRAPPNR             TO CIA-IDARTBET-IN                       
001384     CALL W009CIA USING             CIA-W009CIA                           
001385     MOVE CIA-IDARTBET-UT        TO EKH-IDVERGL                           
001386                                                                          
001387     MOVE WS-AAAAMMDD            TO EKH-DAVERDAT                          
001388     MOVE WS-ARTC01-KDPRODSL     TO EKH-KDPRODSL                          
001389     MOVE 0                      TO EKH-KDPSLLOC                          
001390     MOVE W-IDARTNR              TO EKH-IDARTNR                           
001391     MOVE 'N'                    TO EKH-FLLSBOK                           
001392     MOVE 'SEK'                  TO EKH-KDVALISO                          
001393     MOVE 1.00                   TO EKH-PRKURS                            
001394                                                                          
001395     IF DIST79-DEALER-PRICE OR                                            
001396        DIST79-ECOM-PRICE                                                 
001397       MOVE LEV-PRARTBTO-LOC TO EKH-PRARTNTO                              
001398     ELSE                                                                 
001399       MOVE LEV-PRARTBTO     TO EKH-PRARTNTO                              
001400     END-IF                                                               
001401                                                                          
001402     MOVE 0                      TO EKH-PRARTSJK                          
001403     MOVE ARTC-CLAG-PRHEMTAG     TO EKH-PRHEMTAG                          
001404     MOVE ARTC-CLAG-PRARTSTD     TO EKH-PRARTSTD                          
001405     MOVE 0                      TO EKH-PRLANDCO                          
001406     MOVE 0                      TO EKH-PRINK                             
001407     MOVE LEV-IDANALYS      TO EKH-IDANALYS                               
001408     MOVE LEV-IDKONTO       TO EKH-IDKONTO                                
001409     MOVE LEV-IDKST         TO EKH-IDKST                                  
001410     MOVE 0                      TO EKH-PRDIRLON                          
001411     MOVE 0                      TO EKH-PRDMTRL                           
001412     MOVE 0                      TO EKH-PROVRPAL                          
001413     MOVE 0                      TO EKH-SUBEL                             
001414     MOVE LEV-KDANMORS      TO EKH-KDANMORS                               
001415     MOVE ZERO                   TO EKH-BEVAT                             
001416                                    EKH-KDFRAKT                           
001417                                    EKH-SUVAT                             
001418     MOVE FUNCTION CURRENT-DATE (1:8) TO EKH-DAAVIDAT                     
001419     MOVE LEV-IDFAKT        TO EKH-IDAVINR                                
001420     MOVE 0                      TO EKH-KDAVVTYP                          
001421                                    EKH-KVAVIS                            
001422     MOVE ZERO                   TO EKH-KVANTMOT                          
001423     MOVE ZERO                   TO EKH-KDRT                              
001424     MOVE MFS-IDTRANS            TO EKH-IDTRANS                           
001425     MOVE WS-KDSORT              TO EKH-KDSORT                            
001426     MOVE SPACE                  TO EKH-KDTRADP                           
001427                                    EKH-IDLEVNR                           
001428                                    EKH-FLDCET                            
001429                                    EKH-IDKUNDRF                          
001430                                    EKH-IDFAKT-EXP                        
001431                                                                          
001432                                                                          
001433     PERFORM IMS-ISRT-WDR901                                              
001434     PERFORM UNTIL SEGMENT-FINNS                                          
001435       ADD +1  TO FIL-IDSEKVNR                                            
001436       PERFORM IMS-ISRT-WDR901                                            
001437     END-PERFORM                                                          
001438     .                                                                    
001439     EJECT                                                                
001440* --- IMS SEKTIONER ---                                                   
001441     SKIP3                                                                
001442 IMS-GET-MSG SECTION.                                                     
001443                                                                          
001444     MOVE '  QC' TO GODK-STATUSKODER                                      
001445     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
001446     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
001447     PERFORM IMS-STATUSKONTROLL                                           
001448     .                                                                    
001449     SKIP3                                                                
001450 IMS-GN-MSG SECTION.                                                      
001451                                                                          
001452     MOVE '  ' TO GODK-STATUSKODER                                        
001453     CALL CBLTDLI USING GN MSG-PCB MSG-KOM-WMSGKOM                        
001454     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
001455     PERFORM IMS-STATUSKONTROLL                                           
001456     .                                                                    
001457     SKIP3                                                                
001458 IMS-ISRT-DISP-MSG SECTION.                                               
001459                                                                          
001460     MOVE    '  '             TO GODK-STATUSKODER                         
001461     CALL    CBLTDLI          USING ISRT DISP-PCB MSG-KOM-WMSGKOM         
001462     MOVE    DISP-STATUS-CODE TO STATUS-WS                                
001463     PERFORM IMS-STATUSKONTROLL                                           
001464     .                                                                    
001465     EJECT                                                                
001466                                                                          
001467 IMS-GET-KREE-KREE01 SECTION.                                             
001468                                                                          
001469     STRING 'WDA201  (IDLEVANM =' W-IDLEVANM-X ')'                        
001470          DELIMITED BY SIZE INTO SSA1                                     
001471     MOVE '  ' TO GODK-STATUSKODER                                        
001472     CALL CBLTDLI USING GU  WDA2-PCB DLI-IO-WDA201 SSA1                   
001473     MOVE WDA2-STATUS-CODE TO STATUS-WS                                   
001474     PERFORM IMS-STATUSKONTROLL                                           
001475     .                                                                    
001476     SKIP3                                                                
001477 IMS-GNP-KREE-KREE11 SECTION.                                             
001478                                                                          
001479     MOVE 'WDA211   ' TO SSA1                                             
001480     MOVE '  GE' TO GODK-STATUSKODER                                      
001481     CALL CBLTDLI USING GHNP WDA2-PCB DLI-IO-WDA211 SSA1                  
001482     MOVE WDA2-STATUS-CODE TO STATUS-WS                                   
001483     PERFORM IMS-STATUSKONTROLL                                           
001484     .                                                                    
001485     SKIP3                                                                
001486 IMS-GHU-KREE-KREE01 SECTION.                                             
001487                                                                          
001488     STRING 'WDA201  (IDLEVANM =' W-IDLEVANM-X ')'                        
001489          DELIMITED BY SIZE INTO SSA1                                     
001490     MOVE '  ' TO GODK-STATUSKODER                                        
001491     CALL CBLTDLI USING GHU WDA2-PCB DLI-IO-WDA201 SSA1                   
001492     MOVE WDA2-STATUS-CODE TO STATUS-WS                                   
001493     PERFORM IMS-STATUSKONTROLL                                           
001494     .                                                                    
001495     SKIP3                                                                
001496 IMS-GHU-KREE-KREE11 SECTION.                                             
001497                                                                          
001498     STRING 'WDA201  (IDLEVANM =' W-IDLEVANM-X ')'                        
001499          DELIMITED BY SIZE INTO SSA1                                     
001500     STRING 'WDA211  (WDA211KY =' W-WDA211KY-X ')'                        
001501          DELIMITED BY SIZE INTO SSA2                                     
001502     MOVE '  ' TO GODK-STATUSKODER                                        
001503     CALL CBLTDLI USING GHU WDA2-PCB DLI-IO-WDA211 SSA1 SSA2              
001504     MOVE WDA2-STATUS-CODE TO STATUS-WS                                   
001505     PERFORM IMS-STATUSKONTROLL                                           
001506     .                                                                    
001507     SKIP3                                                                
001508 IMS-REPL-KREE      SECTION.                                              
001509                                                                          
001510     MOVE '  ' TO GODK-STATUSKODER                                        
001511     CALL CBLTDLI USING REPL WDA2-PCB DLI-IO-WDA201                       
001512     MOVE WDA2-STATUS-CODE TO STATUS-WS                                   
001513     PERFORM IMS-STATUSKONTROLL                                           
001514     .                                                                    
001515     EJECT                                                                
001516 IMS-ISRT-WDA222 SECTION.                                                 
001517                                                                          
001518     STRING 'WDA201  (IDLEVANM =' W-IDLEVANM-X ')'                        
001519         DELIMITED BY SIZE INTO SSA1                                      
001520     STRING 'WDA211  (WDA211KY =' W-WDA211KY-X ')'                        
001521         DELIMITED BY SIZE INTO SSA2                                      
001522     MOVE 'WDA222 '            TO SSA3                                    
001523     MOVE '  II'           TO GODK-STATUSKODER                            
001524     CALL CBLTDLI USING ISRT WDA2-PCB DLI-IO-WDA222 SSA1 SSA2 SSA3        
001525     MOVE WDA2-STATUS-CODE TO STATUS-WS                                   
001526     PERFORM IMS-STATUSKONTROLL                                           
001527     .                                                                    
001528 IMS-GU-ARTC-ARTC01 SECTION.                                              
001529                                                                          
001530     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
001531          DELIMITED BY SIZE INTO SSA1                                     
001532     MOVE '  ' TO GODK-STATUSKODER                                        
001533     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-AREA3 SSA1                     
001534     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
001535     PERFORM IMS-STATUSKONTROLL                                           
001536     .                                                                    
001537     SKIP3                                                                
001538 IMS-GHNP-ARTC-ARTC11 SECTION.                                            
001539                                                                          
001540     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
001541          DELIMITED BY SIZE INTO SSA1                                     
001542     MOVE 'WLARTC11 ' TO SSA2                                             
001543     MOVE '  ' TO GODK-STATUSKODER                                        
001544     CALL CBLTDLI USING GHNP ARTC-PCB DLI-IO-AREA3 SSA1 SSA2              
001545     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
001546     PERFORM IMS-STATUSKONTROLL                                           
001547     .                                                                    
001548     SKIP3                                                                
001549 IMS-GHU-ARTC-ARTC11 SECTION.                                             
001550                                                                          
001551     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
001552          DELIMITED BY SIZE INTO SSA1                                     
001553     MOVE 'WLARTC11 ' TO SSA2                                             
001554     MOVE '  ' TO GODK-STATUSKODER                                        
001555     CALL CBLTDLI USING GHU ARTC-PCB DLI-IO-AREA3 SSA1 SSA2               
001556     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
001557     PERFORM IMS-STATUSKONTROLL                                           
001558     .                                                                    
001559     SKIP3                                                                
001560 IMS-REPL-ARTC-ARTC SECTION.                                              
001561                                                                          
001562     MOVE '  ' TO GODK-STATUSKODER                                        
001563     CALL CBLTDLI USING REPL ARTC-PCB DLI-IO-AREA3                        
001564     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
001565     PERFORM IMS-STATUSKONTROLL                                           
001566     .                                                                    
001567     EJECT                                                                
001568 IMS-GHU-ARTS-WDK711 SECTION.                                             
001569                                                                          
001570     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
001571            DELIMITED BY SIZE INTO SSA1                                   
001572     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
001573            DELIMITED BY SIZE INTO SSA2                                   
001574     MOVE '  ' TO GODK-STATUSKODER                                        
001575     CALL CBLTDLI USING GHU WDK7-PCB DLI-IO-WDK711 SSA1 SSA2              
001576     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
001577     PERFORM IMS-STATUSKONTROLL                                           
001578     .                                                                    
001579     SKIP3                                                                
001580                                                                          
001581 IMS-GU-WDK711-CN   SECTION.                                              
001582     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
001583          DELIMITED BY SIZE INTO SSA1                                     
001584     STRING 'WDK711  (IDDC1    =' W-IDDC1-CN                              
001585                    '&PRAVCOST >' W-PRAVCOST-X ')'                        
001586             DELIMITED BY SIZE INTO SSA2                                  
001587     MOVE '  ' TO GODK-STATUSKODER                                        
001588     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-WDK711 SSA1 SSA2               
001589     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
001590     PERFORM IMS-STATUSKONTROLL                                           
001591     .                                                                    
001592     SKIP3                                                                
001593 IMS-REPL-ARTS-ARTS SECTION.                                              
001594                                                                          
001595     MOVE '  ' TO GODK-STATUSKODER                                        
001596     CALL CBLTDLI USING REPL WDK7-PCB DLI-IO-WDK711                       
001597     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
001598     PERFORM IMS-STATUSKONTROLL                                           
001599     .                                                                    
001600     EJECT                                                                
001601 IMS-GU-WDK711 SECTION.                                                   
001602                                                                          
001603     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
001604             DELIMITED BY SIZE INTO SSA1                                  
001605     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
001606             DELIMITED BY SIZE INTO SSA2                                  
001607     MOVE '  ' TO GODK-STATUSKODER                                        
001608     CALL  CBLTDLI  USING GU WDK7-PCB DLI-IO-WDK711 SSA1 SSA2             
001609     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
001610     PERFORM IMS-STATUSKONTROLL                                           
001611     .                                                                    
001612 IMS-GHNP-WDK728-LAST SECTION.                                            
001613                                                                          
001614     STRING 'WDK728  *L(DAINLEV < ' W-DAINLEV ')'                         
001615          DELIMITED BY SIZE INTO SSA1                                     
001616     MOVE '  GE' TO GODK-STATUSKODER                                      
001617     CALL CBLTDLI USING GHNP WDK7-PCB DLI-IO-WDK728 SSA1                  
001618     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
001619     PERFORM IMS-STATUSKONTROLL                                           
001620     .                                                                    
001621     EJECT                                                                
001622 IMS-REPL-WDK728 SECTION.                                                 
001623                                                                          
001624     MOVE '  ' TO GODK-STATUSKODER                                        
001625     CALL CBLTDLI USING REPL WDK7-PCB DLI-IO-WDK728                       
001626     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
001627     PERFORM IMS-STATUSKONTROLL                                           
001628     .                                                                    
001629     EJECT                                                                
001630 IMS-GHU-INLE-INLE21 SECTION.                                             
001631                                                                          
001632     STRING 'WLINLE01(IDARTNR  =' W-IDARTNR-X ')'                         
001633          DELIMITED BY SIZE INTO SSA1                                     
001634     MOVE 'WLINLE11 ' TO SSA2                                             
001635     STRING 'WLINLE21(IDLOPNRM =' W-IDLOPNRM-X ')'                        
001636          DELIMITED BY SIZE INTO SSA3                                     
001637     MOVE '  ' TO GODK-STATUSKODER                                        
001638     CALL CBLTDLI USING GHU  INLE-PCB DLI-IO-AREA SSA1 SSA2 SSA3          
001639     MOVE INLE-STATUS-CODE TO STATUS-WS                                   
001640     PERFORM IMS-STATUSKONTROLL                                           
001641     .                                                                    
001642     SKIP3                                                                
001643 IMS-GHNP-INLE-INLE31 SECTION.                                            
001644                                                                          
001645     MOVE 'WLINLE31 ' TO SSA1                                             
001646     MOVE '  GE' TO GODK-STATUSKODER                                      
001647     CALL CBLTDLI USING GHNP INLE-PCB DLI-IO-AREA SSA1                    
001648     MOVE INLE-STATUS-CODE TO STATUS-WS                                   
001649     PERFORM IMS-STATUSKONTROLL                                           
001650     .                                                                    
001651     SKIP3                                                                
001652 IMS-REPL-INLE        SECTION.                                            
001653                                                                          
001654     MOVE '  ' TO GODK-STATUSKODER                                        
001655     CALL CBLTDLI USING REPL INLE-PCB DLI-IO-AREA                         
001656     MOVE INLE-STATUS-CODE TO STATUS-WS                                   
001657     PERFORM IMS-STATUSKONTROLL                                           
001658     .                                                                    
001659     SKIP3                                                                
001660 IMS-GHU-INLC11      SECTION.                                             
001661                                                                          
001662     STRING 'WLINLC01(IDARTNR  =' W-IDARTNR-X ')'                         
001663          DELIMITED BY SIZE INTO SSA1                                     
001664     STRING 'WLINLC11(IDLOPNRM =' W-IDLOPNRM-X ')'                        
001665          DELIMITED BY SIZE INTO SSA2                                     
001666     MOVE '  ' TO GODK-STATUSKODER                                        
001667     CALL CBLTDLI USING GHU  INLC-PCB DLI-IO-AREA SSA1 SSA2               
001668     MOVE INLC-STATUS-CODE TO STATUS-WS                                   
001669     PERFORM IMS-STATUSKONTROLL                                           
001670     .                                                                    
001671     SKIP3                                                                
001672 IMS-REPL-INLC        SECTION.                                            
001673                                                                          
001674     MOVE '  ' TO GODK-STATUSKODER                                        
001675     CALL CBLTDLI USING REPL INLC-PCB DLI-IO-AREA                         
001676     MOVE INLC-STATUS-CODE TO STATUS-WS                                   
001677     PERFORM IMS-STATUSKONTROLL                                           
001678     .                                                                    
001679     SKIP3                                                                
001680 IMS-DLET-INLE        SECTION.                                            
001681                                                                          
001682     MOVE '  ' TO GODK-STATUSKODER                                        
001683     CALL CBLTDLI USING DLET INLE-PCB DLI-IO-AREA                         
001684     MOVE INLE-STATUS-CODE TO STATUS-WS                                   
001685     PERFORM IMS-STATUSKONTROLL                                           
001686     .                                                                    
001687     SKIP3                                                                
001688 IMS-ISRT-INLE-INLE31 SECTION.                                            
001689                                                                          
001690     STRING 'WLINLE01(IDARTNR  =' W-IDARTNR-X ')'                         
001691          DELIMITED BY SIZE INTO SSA1                                     
001692     MOVE 'WLINLE11 ' TO SSA2                                             
001693     STRING 'WLINLE21(IDLOPNRM =' W-IDLOPNRM-X ')'                        
001694          DELIMITED BY SIZE INTO SSA3                                     
001695     MOVE 'WLINLE31 ' TO SSA4                                             
001696     MOVE '  GE' TO GODK-STATUSKODER                                      
001697     CALL CBLTDLI USING ISRT INLE-PCB DLI-IO-AREA SSA1 SSA2               
001698                                                  SSA3 SSA4               
001699     MOVE INLE-STATUS-CODE TO STATUS-WS                                   
001700     PERFORM IMS-STATUSKONTROLL                                           
001701     .                                                                    
001702     SKIP3                                                                
001703 IMS-ISRT-ZZAC-LOGG SECTION.                                              
001704                                                                          
001705     MOVE 'WLZZAC01 ' TO SSA1                                             
001706     MOVE '  II' TO GODK-STATUSKODER                                      
001707     CALL CBLTDLI USING ISRT ZZAC-PCB DLI-IO-AREA SSA1                    
001708     MOVE ZZAC-STATUS-CODE TO STATUS-WS                                   
001709     PERFORM IMS-STATUSKONTROLL                                           
001710     .                                                                    
001711     EJECT                                                                
001712 IMS-GHU-RETA-RETA01 SECTION.                                             
001713                                                                          
001714     STRING 'WLRETA01(WDA3FSEQ =' W-WDA3FSEQ-X ')'                        
001715          DELIMITED BY SIZE INTO SSA1                                     
001716     MOVE '  GE' TO GODK-STATUSKODER                                      
001717     CALL CBLTDLI USING GHU  RETA-PCB DLI-IO-AREA SSA1                    
001718     MOVE RETA-STATUS-CODE TO STATUS-WS                                   
001719     PERFORM IMS-STATUSKONTROLL                                           
001720     .                                                                    
001721     SKIP3                                                                
001722 IMS-GHN-RETA-RETA01 SECTION.                                             
001723                                                                          
001724     STRING 'WLRETA01(WDA3FSEQ =' W-WDA3FSEQ-X ')'                        
001725          DELIMITED BY SIZE INTO SSA1                                     
001726     MOVE '  GEGB' TO GODK-STATUSKODER                                    
001727     CALL CBLTDLI USING GHN  RETA-PCB DLI-IO-AREA SSA1                    
001728     MOVE RETA-STATUS-CODE TO STATUS-WS                                   
001729     PERFORM IMS-STATUSKONTROLL                                           
001730     .                                                                    
001731     SKIP3                                                                
001732 IMS-REPL-RETA        SECTION.                                            
001733                                                                          
001734     MOVE '  ' TO GODK-STATUSKODER                                        
001735     CALL CBLTDLI USING REPL RETA-PCB DLI-IO-AREA                         
001736     MOVE RETA-STATUS-CODE TO STATUS-WS                                   
001737     PERFORM IMS-STATUSKONTROLL                                           
001738     .                                                                    
001739     SKIP3                                                                
001740 IMS-ISRT-EKOTRANS  SECTION.                                              
001741     MOVE 'WLFILB01 ' TO SSA1                                             
001742     MOVE '  II' TO GODK-STATUSKODER                                      
001743     CALL CBLTDLI USING ISRT FILB-PCB DLI-IO-AREA4 SSA1                   
001744     MOVE FILB-STATUS-CODE TO STATUS-WS                                   
001745     PERFORM IMS-STATUSKONTROLL                                           
001746     .                                                                    
001747     EJECT                                                                
001748 IMS-ISRT-WDL901 SECTION.                                                 
001749                                                                          
001750     MOVE 'WLLOGA01 ' TO SSA1                                             
001751     MOVE '  II' TO GODK-STATUSKODER                                      
001752     CALL CBLTDLI USING ISRT WLLOGA-PCB WLLOGA01 SSA1                     
001753     MOVE WLLOGA-STATUS-CODE TO STATUS-WS                                 
001754     PERFORM IMS-STATUSKONTROLL                                           
001755     .                                                                    
001756     EJECT                                                                
001757 IMS-ISRT-WDR901 SECTION.                                                 
001758                                                                          
001759     MOVE 'WLSAPA01 ' TO SSA1                                             
001760     MOVE '  II' TO GODK-STATUSKODER                                      
001761     CALL CBLTDLI USING ISRT WLSAPA-PCB WLSAPA01 SSA1                     
001762     MOVE WLSAPA-STATUS-CODE TO STATUS-WS                                 
001763     PERFORM IMS-STATUSKONTROLL                                           
001764     .                                                                    
001765     EJECT                                                                
001766 IMS-ISRT-WDR601 SECTION.                                                 
001767                                                                          
001768     MOVE 'WDR601  ' TO SSA1                                              
001769     MOVE '  II' TO GODK-STATUSKODER                                      
001770     CALL CBLTDLI USING ISRT WDR6-PCB DLI-IO-WDR601 SSA1                  
001771     MOVE WDR6-STATUS-CODE TO STATUS-WS                                   
001772     PERFORM IMS-STATUSKONTROLL                                           
001773     .                                                                    
001774     EJECT                                                                
001775 IMS-GU-WDB601-LEV    SECTION.                                            
001776     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
001777          DELIMITED BY SIZE INTO SSA1                                     
001778     MOVE '  GE' TO GODK-STATUSKODER                                      
001779     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601-LEV SSA1             
001780     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
001781     PERFORM IMS-STATUSKONTROLL                                           
001782     .                                                                    
001783     EJECT                                                                
001784 IMS-GU-WDB601-RET    SECTION.                                            
001785     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
001786          DELIMITED BY SIZE INTO SSA1                                     
001787     MOVE '  GE' TO GODK-STATUSKODER                                      
001788     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601-RET SSA1             
001789     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
001790     PERFORM IMS-STATUSKONTROLL                                           
001791     .                                                                    
001792     EJECT                                                                
001793 IMS-STATUSKONTROLL SECTION.                                              
001794                                                                          
001795     SET STATUS-IX TO 1                                                   
001796     SEARCH GODK-STATUS                                                   
001797       AT END                                                             
001798         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
001799         DELIMITED BY SIZE INTO FELTEXT                                   
001800         CALL FELLOG                                                      
001801       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
001802         CONTINUE                                                         
001803     END-SEARCH                                                           
001804     .                                                                    
