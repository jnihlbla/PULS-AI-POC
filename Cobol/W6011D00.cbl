000001 ID DIVISION.                                                             
000002                                                                          
000003 PROGRAM-ID.     W6011D00.                                                
000004 AUTHOR.         MÅNS SAMUELSSON.                                         
000005 DATE-WRITTEN.   94/11/20.                                                
000006 DATE-COMPILED.                                                           
000007                                                                          
000008*    FUNKTION:                                                            
000009*        LÄGGER UPP R40 OR PÅ REGISTER                                    
000010*                                                                         
000011*        PROGRAMMET UPPDATERAR W6LOPA (W6G1)                              
000012*        PROGRAMMET UPPDATERAR WLINLE (WDL2)                              
000013*        PROGRAMMET UPPDATERAR WLARTC (WDK6)                              
000014*        PROGRAMMET UPPDATERAR WDK7                                       
000015*        PROGRAMMET UPPDATERAR WLINLB (WDD9)                              
000016*        PROGRAMMET UPPDATERAR WLSAPA (WDR9)                              
000017*        PROGRAMMET UPPDATERAR WDR8                                       
000018*        PROGRAMMET LOGGAR SALDO FÖRÄNDRINGAR PÅ WLLOGA (WDL9)            
000019*        PROGRAMMET UPPDATERAR WDA9                                       
000020*        PROGRAMMET LÄSER      WDB2   (VIA WDB2BSEQ)                      
000021*                                                                         
000022*    INDATA.                                                              
000023*        TRANSAKTION: W6T11D                                              
000024*        MID:         W6I11D01                                            
000025*                                                                         
000026*    E-TRACKER 10143271 - CHINA WAREHOUSE PROJECT-1                       
000027*                                                                         
000028                                                                          
000029     SKIP3                                                                
000030 ENVIRONMENT DIVISION.                                                    
000031     EJECT                                                                
000032 DATA DIVISION.                                                           
000033 WORKING-STORAGE SECTION.                                                 
000034                                                                          
000035*    -- CHECKED BY WY2000                                                 
000036 77  IDPGM                       PIC X(08)   VALUE 'W6011D00'.            
000037                                                                          
000038*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
000039 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
000040                                                                          
000041 01  WS-IDLOPNRM                 PIC 9(9)    VALUE ZERO.                  
000042 01  W-0VVDLLLLK  REDEFINES WS-IDLOPNRM.                                  
000043     03 FILLER                   PIC 9(1).                                
000044     03 W-VVD                    PIC 9(3).                                
000045     03 W-LLLL                   PIC 9(4).                                
000046     03 W-K                      PIC 9(1).                                
000047                                                                          
000048 77  NOLL-RAKNARE                PIC S9(5)   VALUE ZERO COMP-3.           
000049 77  WS-SAP-X-IDORDNR            PIC X(7)    VALUE SPACE.                 
000050 77  JA                          PIC X       VALUE 'J'.                   
000051 77  NEJ                         PIC X       VALUE 'N'.                   
000052 77  W-DATE-AAMM                 PIC 9(4)      VALUE ZERO.                
000053                                                                          
000054 77  MID-IX                      PIC S9(9)  VALUE +1    COMP SYNC.        
000055                                                                          
000056*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
000057                                                                          
000058 77  W-IDTRANS                   PIC X(4)    VALUE SPACE.                 
000059     88  EGEN-MID                            VALUE '611D'.                
000060     88  GODK-MID                            VALUE '6100'.                
000061     EJECT                                                                
000062 01  W-HITTAD-SW                 PIC X(1)    VALUE SPACE.                 
000063     88  HITTAD-JA                           VALUE 'J'.                   
000064     88  HITTAD-NEJ                          VALUE 'N'.                   
000065     EJECT                                                                
000066 01  SPAR-ARTC01-IDLEVNR         PIC  X(5)   VALUE SPACE.                 
000067 01  SPAR-ARTC01-KDPRODSL        PIC S9(3)   VALUE +0 COMP-3.             
000068 01  SPAR-ARTC01-IDFKNGRP        PIC S9(5)   VALUE +0 COMP-3.             
000069 01  SPAR-ARTC11-KDPSLLOC        PIC S9(3)   VALUE +0 COMP-3.             
000070 01  SPAR-ARTC21-PRARTBES-PR     PIC S9(7)V9(2) VALUE +0 COMP-3.          
000071 01  SPAR-ARTC21-PRARTBEL-PR     PIC S9(7)V9(5) VALUE +0 COMP-3.          
000072 01  SPAR-ARTC21-KDVALISO        PIC X(3)       VALUE SPACE.              
000073 01  SPAR-PRKURS                 PIC S9(5)V9(5) VALUE +0   COMP-3.        
000074 01  W-REVALUTA                  PIC S9(5)      VALUE +0   COMP-3.        
000075 77  WS-PRAVCOST                 PIC S9(7)V9(2).                          
000076 01  WS-PRARTKALKYL              PIC S9(7)V9(5) VALUE +0 COMP-3.          
000077*                                                                         
000078*                                                                         
000079 01  WS-IDKONTO                  PIC S9(10).                              
000080 01  WS-IDKONTO2                 PIC S9(2).                               
000081*                                                                         
000082 01  WS-DAINLEV                  PIC 9(16)     VALUE ZERO.                
000083*                                                                         
000084 01  WS-IDLOGLOP                 PIC S9(1)      VALUE ZERO.               
000085*                                                                         
000086 01  DAGENS-DATUM                PIC 9(6)       VALUE ZERO.               
000087 01  DAGENS-TID                  PIC 9(8)       VALUE ZERO.               
000088 01  WLOGG-TID                   PIC S9(9)      VALUE ZERO.               
000089 01  LOGG-DATUM                  PIC S9(8)      VALUE ZERO.               
000090 01  WS-KDSORT                   PIC X(2)       VALUE SPACE.              
000091 01  WS-IDDISTR-DISP             PIC 9(4)       VALUE ZERO.               
000092 01  WS-PRKURS                   PIC S9(5)V9(5) VALUE +0   COMP-3.        
000093 01  WS-REVALUTA                 PIC S9(5)      VALUE +0   COMP-3.        
000094 01  WS-TIFAKT                   PIC S9(7)      VALUE ZERO.               
000095 01  W-RETULF                    PIC S9(3)V9(4) VALUE +0.                 
000096                                                                          
000097 01  WS-DAT.                                                              
000098*     -- DAAVIDAT TILL SAP                                                
000099  02     WS-DAAVIDAT             PIC 9(8)    VALUE ZERO.                  
000100  02     FILLER REDEFINES WS-DAAVIDAT.                                    
000101   03    WS-DAAVIDAT-SEKEL       PIC 9(2).                                
000102   03    WS-DAAVIDAT-YYMMDD      PIC 9(6).                                
000103     EJECT                                                                
000104     03  WS-DATE-YYMMDD            PIC 9(06).                             
000105     03  WS-DATE-FIRST REDEFINES WS-DATE-YYMMDD.                          
000106         05  WS-DATE-YYMM          PIC 9(04).                             
000107         05  WS-DATE-DD            PIC 9(02).                             
000108                                                                          
000109                                                                          
000110 01  FILLER                      PIC  X(16)  VALUE 'BYTES-TEST'.          
000111 01  TEST-IDARTNR                PIC  9(9)   COMP-3.                      
000112*01  FILLER  -COPY WWBYT01     -RED TEST-IDARTNR.                         
000113     EJECT                                                                
000114                                                                          
000115*01  FILLER  -COPY WWBYT02     -RED TEST-IDARTNR.                         
000116     EJECT                                                                
000117                                                                          
000118*01  FILLER  -COPY WWBYT16     -RED TEST-IDARTNR.                         
000119     EJECT                                                                
000120                                                                          
000121*      --- VALID IDDC CODES                                               
000122*                                                                         
000123*01    -COPY WWDCKONS                                                     
000124       EJECT                                                              
000125*01    -COPY WWDC99                                                       
000126       EJECT                                                              
000127*                                                                         
000128*01    -COPY W510CURR                                                     
000129       EJECT                                                              
000130*                                                                         
000131*                                                                         
000132 01  WS-TIAAAAMMDDTTMMSSTH       PIC 9(16)   VALUE ZERO.                  
000133 01  FILLER                      REDEFINES WS-TIAAAAMMDDTTMMSSTH.         
000134     03 WS-TISEKEL               PIC 9(2).                                
000135     03 WS-TIAAMMDDTTMMSSTH-DATE PIC 9(6).                                
000136     03 WS-TIAAMMDDTTMMSSTH-TIME PIC 9(8).                                
000137*                                                                         
000138 01  FLT-FOR-BER-AV-IDLOPNRM.                                             
000139     03 FLT-LGD                  PIC S9(1) COMP SYNC VALUE +7.            
000140     03 VAEGNINGSTAL             PIC 9(7) VALUE 2121212.                  
000141     03 VAEGNTAL-LGD             PIC S9 COMP SYNC VALUE +7.               
000142     03 MODUL-10-11              PIC 9(2) VALUE 10.                       
000143     03 ALT-A-B                  PIC X(1) VALUE 'B'.                      
000144*                                                                         
000145*    --- LOGG-TRANSAR                                                     
000146 01  WS-ZZAC01.                                                           
000147     03 WS-ZZAC01-LOGGPOST       PIC X(90)   VALUE SPACE.                 
000148     03 FILLER                   REDEFINES WS-ZZAC01-LOGGPOST.            
000149      04 WS-ZZAC01-LOGGPOST-1-3  PIC X(3).                                
000150      04 WS-ZZAC01-LOGGPOST-4-90 PIC X(87).                               
000151     03 WS-ZZAC01-SORTPOST       PIC X(36)   VALUE SPACE.                 
000152*    --- WDD9-FAELT                                                       
000153*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
000154 01  GENERELLA-SUBPROGRAM.                                                
000155     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
000156     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
000157     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
000158     03  CHECK                   PIC X(8)    VALUE 'CHECK   '.            
000159     03  WL01TIDZ                PIC X(8)    VALUE 'WL01TIDZ'.            
000160     03  W510AVG                 PIC X(8)    VALUE 'W510AVG '.            
000161     03  W510CURR                PIC X(8)    VALUE 'W510CURR'.            
000162     EJECT                                                                
000163 01  MESSAGE-CODES.                                                       
000164     03  INF-UPPDATE-DONE        PIC X(3)    VALUE '101'.                 
000165*01  -COPY W211400  -PRE W400-                                            
000166     EJECT                                                                
000167*01  -COPY W232232  -PRE W232-                                            
000168     EJECT                                                                
000169*01  -COPY WDATAREA                                                       
000170     EJECT                                                                
000171*01  -COPY WL01TIDZ -PRE TIDZ-                                            
000172     EJECT                                                                
000173*    --- AREOR FÖR MFS OCH SKÄRMHANTERING                                 
000174*                                                                         
000175 01  FILLER                      PIC X(16)   VALUE 'MID-AREA'.            
000176     SKIP3                                                                
000177*01  MID -COPY W6I11D01                                                   
000178     EJECT                                                                
000179 01  FILLER                      PIC X(16)   VALUE 'MFS-AREA'.            
000180     SKIP3                                                                
000181*01  -COPY WMSGKOM                                                        
000182     EJECT                                                                
000183*01  -COPY WMFSAREA                                                       
000184     EJECT                                                                
000185*01  -COPY WMSGAREA                                                       
000186     EJECT                                                                
000187*    --- AREA FÖR W510AVG                                                 
000188 01  FILLER                    PIC X(16) VALUE 'W510AVGAREA*****'.        
000189*01  -COPY W510AVG                                                        
000190     EJECT                                                                
000191*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
000192*                                                                         
000193     EJECT                                                                
000194 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
000195     SKIP3                                                                
000196 01  NYCKLAR-TILL-DLI.                                                    
000197     03  W-IDARTNR-WDA9-X.                                                
000198         05  W-IDARTNR-WDA9      PIC S9(9)   VALUE ZERO COMP-3.           
000199     03  W-IDDISTR-WDA9-X.                                                
000200         05  W-IDDISTR-WDA9      PIC S9(5)   VALUE ZERO COMP-3.           
000201     03  W-IDARTNR-X.                                                     
000202         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
000203     03  W-DAINLEV-X.                                                     
000204         05  W-DAINLEV           PIC 9(16)   VALUE ZERO.                  
000205     03  W-WDD901KY-X.                                                    
000206         05  W-IDARTNR-D9        PIC S9(9)   VALUE ZERO COMP-3.           
000207         05  W-IDDC-D9           PIC X(2)    VALUE SPACE.                 
000208     03  W-IDLEVNR-X.                                                     
000209         05  W-IDLEVNR           PIC  X(5)    VALUE SPACE.                
000210     03  W-IDDC-X.                                                        
000211         05  W-IDDC              PIC  X(2)    VALUE SPACE.                
000212     03  W-6017KEY-X.                                                     
000213         05  FILLER              PIC X(4)     VALUE '6017'.               
000214         05  FILLER              PIC X(26)    VALUE LOW-VALUE.            
000215     03  W-WDB2BSEQ-X.                                                    
000216         05  W-IDLEVNR-WDB2B     PIC  X(5)    VALUE SPACE.                
000217     03  W-IDLEVNR-PR-X.                                                  
000218         05  W-IDLEVNR-PR        PIC X(5) VALUE LOW-VALUE.                
000219     03  W-DAPRLIST-K7-N.                                                 
000220         05  W-DAPRLIST-K7       PIC 9(8)    VALUE ZERO.                  
000221     03  W-IDKR-X.                                                        
000222         05  W-IDKR              PIC 9(05)  VALUE ZERO.                   
000223     03  W-IDLAND-X.                                                      
000224         05  W-IDLAND            PIC X(2)    VALUE SPACE.                 
000225     03  W-IDLANDX2-X.                                                    
000226         05    W-IDLANDX2        PIC X(2)    VALUE SPACE.                 
000227     03  W-WDGX9305-X.                                                    
000228         05  W-IDHTYP            PIC X(4)    VALUE '9305'.                
000229         05  W-KDVALISO-HUV      PIC X(3)    VALUE SPACE.                 
000230         05  W-KDVALTYP          PIC X(1)    VALUE 'M'.                   
000231         05  FILLER              PIC X(22)   VALUE LOW-VALUE.             
000232     03  W-KDVALISO-X.                                                    
000233         05  W-KDVALISO-ROW      PIC X(3)    VALUE SPACE.                 
000234     03  W-TISTADA9-X.                                                    
000235         05  W-TISTADAT-9KOMPL   PIC S9(7)   VALUE ZERO COMP-3.           
000236     EJECT                                                                
000237                                                                          
000238                                                                          
000239     SKIP2                                                                
000240*    --- STATUS-KOD FRÅN IMS                                              
000241 01  STATUS-WS                   PIC XX.                                  
000242     88  SEGMENT-FINNS                       VALUE '  '.                  
000243     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
000244     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
000245     SKIP2                                                                
000246 01  GODK-STATUSKODER.                                                    
000247     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
000248     SKIP3                                                                
000249 01  SSA1                        PIC X(64).                               
000250 01  SSA2                        PIC X(64).                               
000251 01  SSA3                        PIC X(64).                               
000252     EJECT                                                                
000253*    --- IMS FUNKTIONSKODER                                               
000254*01  -COPY W0003                                                          
000255     EJECT                                                                
000256*    ---  DLI INPUT-OUTPUT AREA                                           
000257 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
000258     SKIP3                                                                
000259 01  DLI-IO-AREA.                                                         
000260     03  IO-AREA                 PIC X(150)  VALUE SPACE.                 
000261     SKIP3                                                                
000262     03  WLINLE01 REDEFINES IO-AREA.                                      
000263*        05  -COPY WDL201  -PRE INLE-                                     
000264     SKIP3                                                                
000265     03  WLINLE11 REDEFINES IO-AREA.                                      
000266*        05  -COPY WDL211  -PRE INLE-                                     
000267     SKIP3                                                                
000268     03  WLINLE22 REDEFINES IO-AREA.                                      
000269*        05  -COPY WDL223  -PRE INLE-                                     
000270     EJECT                                                                
000271     03  WLARTC01 REDEFINES IO-AREA.                                      
000272*        05  -COPY WDK601  -PRE ARTC01-                                   
000273     SKIP3                                                                
000274     03  WLZZAC01 REDEFINES IO-AREA.                                      
000275*        05  -COPY WDG601  -PRE ZZAC01-                                   
000276     EJECT                                                                
000277     03  WLINLB11 REDEFINES IO-AREA.                                      
000278*        05  -COPY WDD902  -PRE INLB11-                                   
000279     EJECT                                                                
000280 01  DLI-IO-AREA3.                                                        
000281     03  IO-AREA3                PIC X(150)  VALUE SPACE.                 
000282     SKIP3                                                                
000283     03  W6LOPA11 REDEFINES IO-AREA3.                                     
000284*        05  -COPY W6GX6018                                               
000285     EJECT                                                                
000286 01  DLI-IO-AREA4.                                                        
000287     03  IO-AREA4                PIC X(900)  VALUE SPACE.                 
000288     SKIP3                                                                
000289     03  WLARTC11 REDEFINES IO-AREA4.                                     
000290*        05  -COPY WDK611   -PRE ARTC11-                                  
000291 01  FILLER                      PIC X(16)  VALUE 'WLLOGA01'.             
000292*01  WLLOGA01  -COPY WDL901                                               
000293     EJECT                                                                
000294 01  FILLER                      PIC X(16) VALUE 'DLI-IO-WDR801'.         
000295 01  DLI-IO-WDR801.                                                       
000296*    05  -COPY WDR801  -PRE EKO-                                          
000297       07  FILLER REDEFINES EKO-FIL-WDR801-DATA.                          
000298         09  -COPY W510EKHA -PRE EKO-                                     
000299     EJECT                                                                
000300 01  FILLER             PIC X(16)  VALUE 'DLI-IO-WLSAPA01'.               
000301 01  DLI-IO-WLSAPA01.                                                     
000302*    03  WLSAPA01  -COPY WDR901                                           
000303*    07  -COPY W510EKHA  -RED FIL-WDR901-DATA                             
000304     EJECT                                                                
000305 01  FILLER             PIC X(16)  VALUE 'DLI-IO-WDA901  '.               
000306 01  DLI-IO-WDA901.                                                       
000307*    03  WDA901    -COPY WDA901                                           
000308     EJECT                                                                
000309 01  FILLER             PIC X(16)  VALUE 'DLI-IO-WDA911  '.               
000310 01  DLI-IO-WDA911.                                                       
000311*    03  WDA911    -COPY WDA911                                           
000312     EJECT                                                                
000313 01  FILLER             PIC X(16)  VALUE 'DLI-IO-WDK601  '.               
000314 01  DLI-IO-WDK601.                                                       
000315*    03  WDK601    -COPY WDK601 -PRE REM-                                 
000316     EJECT                                                                
000317 01  FILLER             PIC X(16)  VALUE 'DLI-IO-WDB201  '.               
000318 01  DLI-IO-WDB201.                                                       
000319*    03  WDB201    -COPY WDB201                                           
000320     EJECT                                                                
000321 01  FILLER             PIC X(16)  VALUE 'DLI-IO-WDK711  '.               
000322 01  DLI-IO-WDK711.                                                       
000323*    03  WDK711    -COPY WDK711                                           
000324     EJECT                                                                
000325 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK724'.                      
000326 01  DLI-IO-WDK724.                                                       
000327*        05  -COPY WDK724                                                 
000328                                                                          
000329 01  FILLER             PIC X(16)  VALUE 'DLI-IO-WDL601  '.               
000330 01  DLI-IO-WDL601.                                                       
000331*    03  WDL601    -COPY WDL601                                           
000332     EJECT                                                                
000333 01  FILLER             PIC X(16)  VALUE 'DLI-IO-WDL611  '.               
000334 01  DLI-IO-WDL611.                                                       
000335*    03  WDL611    -COPY WDL611                                           
000336     EJECT                                                                
000337 01  FILLER             PIC X(16)  VALUE 'DLI-IO-WDB601  '.               
000338 01  DLI-IO-WDB601.                                                       
000339*    03  WDB601    -COPY WDB601                                           
000340     EJECT                                                                
000341                                                                          
000342 01  FILLER               PIC X(16)   VALUE 'WDB617 AREA'.                
000343 01   DLI-IO-AREA-B617.                                                   
000344*     03  -COPY WDB617                                                    
000345     EJECT                                                                
000346                                                                          
000347 01  DLI-IO-AREA1.                                                        
000348     03  IO-AREA1                PIC X(900)  VALUE SPACE.                 
000349     03  W6H701 REDEFINES IO-AREA1.                                       
000350*        05 -COPY W6H701                                                  
000351                                                                          
000352 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA2'.        
000353 01  DLI-IO-AREA2.                                                        
000354     03  IO-AREA2                PIC X(400)  VALUE SPACE.                 
000355     03  W6H712 REDEFINES IO-AREA2.                                       
000356*        05 -COPY W6H712                                                  
000357     EJECT                                                                
000358                                                                          
000359 01  FILLER                    PIC X(16)  VALUE 'WDF101'.                 
000360*01  WLLEVA01 -COPY WDF101                                                
000361     EJECT                                                                
000362                                                                          
000363 01  FILLER                    PIC X(16)  VALUE 'WDF102'.                 
000364*01  WLLEVA11 -COPY WDF102 -PRE LEV-                                      
000365     EJECT                                                                
000366                                                                          
000367 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX9306'.                    
000368 01  DLI-IO-WDGX9306.                                                     
000369*    03  -COPY WDGX9306                                                   
000370     EJECT                                                                
000371 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX9308'.                    
000372 01  DLI-IO-WDGX9308.                                                     
000373*    03  -COPY WDGX9308                                                   
000374                                                                          
000375 LINKAGE SECTION.                                                         
000376                                                                          
000377*01  -COPY W0009   -PRE MSG-                                              
000378     EJECT                                                                
000379*01  -COPY W0009   -PRE DISP-                                             
000380     EJECT                                                                
000381*01  -COPY W0008  -PRE INLE-                                              
000382     05  FILLER                  PIC X.                                   
000383     EJECT                                                                
000384*01  -COPY W0008  -PRE ARTC-                                              
000385     05  FILLER                  PIC X.                                   
000386     EJECT                                                                
000387*01  -COPY W0008  -PRE ZZAC-                                              
000388     05  FILLER                  PIC X.                                   
000389     EJECT                                                                
000390*01  -COPY W0008  -PRE INLB-                                              
000391     05  FILLER                  PIC X(10).                               
000392     05  INLB-KFB-DAAVROP-AVS    PIC 9(6).                                
000393     EJECT                                                                
000394*01  -COPY W0008  -PRE LOPA-                                              
000395     05  FILLER                  PIC X.                                   
000396     EJECT                                                                
000397*01  -COPY W0008  -PRE LOGA-                                              
000398     05  FILLER                  PIC X.                                   
000399     EJECT                                                                
000400*01  -COPY W0008  -PRE WDR8-                                              
000401     05  FILLER                  PIC X.                                   
000402     EJECT                                                                
000403*01  -COPY W0008  -PRE SAPA-                                              
000404     05  FILLER                  PIC X.                                   
000405     EJECT                                                                
000406*01  -COPY W0008  -PRE WDA9-                                              
000407     05  FILLER                  PIC X.                                   
000408     EJECT                                                                
000409*01  -COPY W0008  -PRE WDB2B-                                             
000410     05  FILLER                  PIC X.                                   
000411     EJECT                                                                
000412*01  -COPY W0008  -PRE WDK7-                                              
000413     05  FILLER                  PIC X.                                   
000414     EJECT                                                                
000415*01  -COPY W0008  -PRE WDL6-                                              
000416     05  FILLER                  PIC X.                                   
000417     EJECT                                                                
000418*01  -COPY W0008  -PRE WDB6-                                              
000419     05  FILLER                  PIC X.                                   
000420     EJECT                                                                
000421*01  -COPY W0008  -PRE 9305-AVG-                                          
000422     05  FILLER                  PIC X.                                   
000423     EJECT                                                                
000424*01  -COPY W0008  -PRE AVG-WDB6-                                          
000425     05  FILLER                  PIC X.                                   
000426     EJECT                                                                
000427*01  -COPY W0008 -PRE W6H7-                                               
000428     05 FILLER                   PIC X.                                   
000429     EJECT                                                                
000430*01  -COPY W0008 -PRE LEV-                                                
000431     05  FILLER                  PIC X(5).                                
000440     EJECT                                                                
000441*01  -COPY W0008  -PRE 9305-                                              
000442     05  FILLER                  PIC X.                                   
000443                                                                          
000444 PROCEDURE DIVISION  USING MSG-PCB DISP-PCB INLE-PCB ARTC-PCB             
000445            ZZAC-PCB INLB-PCB LOPA-PCB LOGA-PCB WDR8-PCB SAPA-PCB         
000446            WDA9-PCB WDB2B-PCB WDK7-PCB WDL6-PCB WDB6-PCB                 
000447            9305-AVG-PCB AVG-WDB6-PCB                                     
000448            W6H7-PCB LEV-PCB 9305-PCB.                                    
000449 MAIN SECTION.                                                            
000450     ENTRY 'DLITCBL' USING MSG-PCB DISP-PCB INLE-PCB ARTC-PCB             
000451            ZZAC-PCB INLB-PCB LOPA-PCB LOGA-PCB WDR8-PCB SAPA-PCB         
000452            WDA9-PCB WDB2B-PCB WDK7-PCB WDL6-PCB WDB6-PCB                 
000453            9305-AVG-PCB AVG-WDB6-PCB                                     
000454            W6H7-PCB LEV-PCB 9305-PCB.                                    
000455                                                                          
000456     PERFORM IMS-GET-MSG                                                  
000457     IF SEGMENT-FINNS                                                     
000458       PERFORM IMS-GN-MSG                                                 
000459       PERFORM A-INIT                                                     
000460       PERFORM UNTIL MID-IX > MID-KVPOST                                  
000461         PERFORM B-UPPD-KVLS                                              
000462         PERFORM C-UPPD-BESTREST                                          
000463         PERFORM D-MEDDELANDE                                             
000464         PERFORM E-UPPD-HISTORIK                                          
000465         MOVE W-IDARTNR TO TEST-IDARTNR                                   
000466         IF BYT02-RENOV                                                   
000467           PERFORM F-UPPD-REMAN                                           
000468         END-IF                                                           
000469         ADD +1     TO MID-IX                                             
000470       END-PERFORM                                                        
000471       PERFORM Z-FINIT                                                    
000472     END-IF                                                               
000473                                                                          
000474     MOVE ZERO TO RETURN-CODE                                             
000475     GOBACK                                                               
000476     .                                                                    
000477     EJECT                                                                
000478 A-INIT SECTION.                                                          
000479                                                                          
000480     MOVE MSG-INDATA-MINUS-1-TRANSKOD  TO MID-W6I11D01                    
000481     MOVE MSG-IDTRANS-1  TO MFS-IDTRANS                                   
000482     MOVE MSG-KDMFSFOR-1 TO MFS-KDMFSFOR                                  
000483                                                                          
000484     MOVE MSG-KDTRTYP TO MFS-KDTRTYP                                      
000485     MOVE MSG-IDPFK   TO MFS-IDPFK                                        
000486     MOVE MFS-IDTRANS TO W-IDTRANS                                        
000487                                                                          
000488     ACCEPT DAGENS-DATUM       FROM DATE                                  
000489     ACCEPT DAGENS-TID         FROM TIME                                  
000490     MOVE MID-IDDC (1)           TO W-IDDC                                
000491     PERFORM IMS-GU-WDB601                                                
000492                                                                          
000493     MOVE '011'                  TO TIDZ-MSGI-KDCALL                      
000494     MOVE DCS-IDTIDZON           TO TIDZ-MSGI-IDTIDZON                    
000494     MOVE DCS-IDDC               TO TIDZ-MSGI-IDDC                        
000495     MOVE DAGENS-DATUM           TO TIDZ-MSGI-TILOKDAT                    
000496     MOVE DAGENS-TID             TO TIDZ-MSGI-TILOKTID                    
000497     CALL WL01TIDZ            USING TIDZ-MSGI-WL01TIDZ                    
000498                                                                          
000499     MOVE 'AAMMDD'               TO DAT-KDDATFORM                         
000500     MOVE TIDZ-MSGI-TILOKDAT     TO DAT-I-TIDATUM                         
000501     CALL WDATKONV            USING DAT-KDDATFORM                         
000502                                    DAT-I-TIDATUM                         
000503                                    DAT-O-TIDATUM                         
000504                                    DAT-KDSVAR                            
000505                                                                          
000506     ACCEPT WS-TIAAMMDDTTMMSSTH-DATE FROM DATE                            
000507     ACCEPT WS-TIAAMMDDTTMMSSTH-TIME FROM TIME                            
000508     MOVE FUNCTION CURRENT-DATE (1:2) TO WS-TISEKEL                       
000509     MOVE FUNCTION CURRENT-DATE (3:4) TO WS-DATE-YYMM                     
000510     MOVE 01                          TO WS-DATE-DD                       
000511     .                                                                    
000512     EJECT                                                                
000513                                                                          
000514 B-UPPD-KVLS          SECTION.                                            
000515     MOVE MID-IDDC(MID-IX)     TO WS-IDDC                                 
000516                                  W-IDDC                                  
000517     MOVE MID-IDARTNR (MID-IX) TO W-IDARTNR                               
000518     PERFORM IMS-GU-WLARTC01                                              
000519     MOVE ARTC01-ART-KDPRODSL  TO SPAR-ARTC01-KDPRODSL                    
000520     MOVE ARTC01-ART-IDFKNGRP  TO SPAR-ARTC01-IDFKNGRP                    
000521     MOVE ARTC01-ART-KDSORT    TO WS-KDSORT                               
000522     IF CDC                                                               
000523       PERFORM IMS-GHNP-WLARTC11                                          
000524       SUBTRACT MID-KVANTAL (MID-IX) FROM ARTC11-CLAG-KVLS                
000525       PERFORM IMS-REPL-WLARTC11                                          
000526     ELSE                                                                 
000527       PERFORM IMS-GNP-WLARTC11                                           
000528       MOVE ARTC11-CLAG-KDPSLLOC  TO SPAR-ARTC11-KDPSLLOC                 
000529       IF DCS-NDC-CN                                                      
000530       OR DCS-USA                                                         
000531         PERFORM DDAA-GET-PRARTBEL                                        
000532         PERFORM DDAB-GET-CURRENCY-RATE                                   
000533       END-IF                                                             
000534       PERFORM IMS-GHU-WDK711                                             
000535                                                                          
000536       IF DCS-NDC-CN                                                      
000537       OR DCS-USA                                                         
000538         IF MID-KVANTAL (MID-IX) > ZERO                                   
000539           IF DCS-NDC-CN                                                  
000540             MOVE 081                   TO AVG-KDCALL                     
000541           END-IF                                                         
000542           IF DCS-USA                                                     
000543             MOVE 080                   TO AVG-KDCALL                     
000544           END-IF                                                         
000545****  AVERAGE COST BERÄKNING SKALL TA HÄNSYN TILL EFR                     
000546           COMPUTE AVG-KVLS-OLD = SLAG-KVLS +                             
000547                   SLAG-KVEFRS                                            
000548                                                                          
000549           MOVE MID-IDLEVNR (MID-IX) TO W-IDLEVNR                         
000550           PERFORM IMS-GU-WLLEVA01                                        
000551           IF SEGMENT-SAKNAS                                              
000552             MOVE ZERO TO W-RETULF                                        
000553           ELSE                                                           
000554             PERFORM IMS-GU-WDB601                                        
000555             MOVE DCS-IDLANDX2 TO W-IDLAND                                
000556             PERFORM IMS-GNP-WLLEVA11                                     
000557             IF SEGMENT-FINNS                                             
000558               IF LEV-TULL-TITULF < DAGENS-DATUM                          
000559                 MOVE LEV-TULL-RETULF-1 TO W-RETULF                       
000560               ELSE                                                       
000561                 MOVE LEV-TULL-RETULF-2 TO W-RETULF                       
000562               END-IF                                                     
000563             END-IF                                                       
000564           END-IF                                                         
000565           MOVE W-RETULF              TO AVG-REMARKUP                     
000566                                                                          
000567           MOVE WS-TIFAKT             TO WS-DAAVIDAT-YYMMDD               
000568           MOVE WS-DAAVIDAT-YYMMDD(1:2) TO W-DATE-AAMM(1:2)               
000569           MOVE 01                      TO W-DATE-AAMM(3:2)               
000570           MOVE 'SEK'                 TO CURR-KDVALISO-HUV                
000571           IF DCS-NDC-CN                                                  
000572             MOVE 'CNY'               TO CURR-KDVALISO-ROW                
000573           END-IF                                                         
000574           IF DCS-USA                                                     
000575             MOVE 'USD'               TO CURR-KDVALISO-ROW                
000576           END-IF                                                         
000577           MOVE W-DATE-AAMM           TO CURR-TIAAMM                      
000578           MOVE 'A'                   TO CURR-KDVALTYP                    
000579           CALL W510CURR USING CURR-W510CURR 9305-PCB                     
000580           IF CURR-KDSVAR = ' '                                           
000581             MOVE CURR-PRKURS-NEW     TO WS-PRKURS                        
000582             MOVE CURR-REVALUTA-TO    TO WS-REVALUTA                      
000583           ELSE                                                           
000584             MOVE 1                   TO WS-PRKURS                        
000585             MOVE 1                   TO WS-REVALUTA                      
000586           END-IF                                                         
000587           MOVE WS-IDDC               TO W-IDDC                           
000588           PERFORM IMS-GU-WDB601                                          
000589           MOVE DCS-IDLANDX2          TO W-IDLANDX2                       
000590           IF SEGMENT-FINNS                                               
000591             PERFORM IMS-GNP-WDB617                                       
000592             IF SEGMENT-FINNS                                             
000593               COMPUTE WS-PRARTKALKYL        ROUNDED =                    
000594                      (ARTC11-CLAG-PRDIRLON *                             
000595                      PROC-REDIRLON * WS-REVALUTA / WS-PRKURS) +          
000596                      (ARTC11-CLAG-PRDMTRL *                              
000597                       PROC-REDMTRL * WS-REVALUTA / WS-PRKURS)            
000598             ELSE                                                         
000599               MOVE ZERO TO WS-PRARTKALKYL                                
000600             END-IF                                                       
000601           END-IF                                                         
000602           MOVE WS-PRARTKALKYL        TO AVG-PRARTNTO                     
000603                                                                          
000604           MOVE MID-KVANTAL (MID-IX)  TO AVG-KVANTMOT                     
000605           MOVE SLAG-PRAVCOST         TO AVG-PRAVCOST-OLD                 
000606           MOVE SPAR-PRKURS           TO AVG-PRKURS                       
000607           MOVE +0                    TO AVG-KVLEVART                     
000608           MOVE SPAR-ARTC21-KDVALISO  TO AVG-KDVALISO                     
000609           MOVE SPAR-ARTC21-PRARTBEL-PR TO AVG-PRARTBEL                   
000610           MOVE SPAR-ARTC11-KDPSLLOC  TO AVG-KDPSLLOC                     
000611           MOVE SPAR-ARTC01-KDPRODSL  TO AVG-KDPRODSL                     
000612           MOVE SPAR-ARTC01-IDFKNGRP  TO AVG-IDFKNGRP                     
000613           MOVE W-IDDC                TO AVG-IDDC                         
000614           MOVE +0                    TO AVG-PRAVCOST-NEW                 
000615           MOVE SPACE                 TO AVG-KDSVAR                       
000616           MOVE WS-TIAAMMDDTTMMSSTH-DATE(1:2) TO AVG-TIAA                 
000617           MOVE WS-TIAAMMDDTTMMSSTH-DATE(3:2) TO AVG-TIMM                 
000618                                                                          
000619           IF HITTAD-JA                                                   
000620             CALL W510AVG USING AVG-W510AVG                               
000621                                9305-AVG-PCB AVG-WDB6-PCB                 
000622             IF AVG-KDSVAR = SPACE                                        
000623               MOVE AVG-PRAVCOST-NEW TO WS-PRAVCOST                       
000624             ELSE                                                         
000625               IF AVG-KDSVAR = '4'                                        
000626                 MOVE SLAG-PRAVCOST TO WS-PRAVCOST                        
000627               ELSE                                                       
000628                 STRING 'FEL FRÅN W510AVG ' AVG-KDSVAR                    
000629                  DELIMITED BY SIZE INTO FELTEXT                          
000630                   CALL FELLOG                                            
000631               END-IF                                                     
000632             END-IF                                                       
000633             MOVE WS-PRAVCOST          TO   SLAG-PRAVCOST                 
000634           END-IF                                                         
000635         ELSE                                                             
000636           IF DCS-NDC-CN                                                  
000637             MOVE 041                 TO AVG-KDCALL                       
000638           END-IF                                                         
000639           IF DCS-USA                                                     
000640             MOVE 040                 TO AVG-KDCALL                       
000641           END-IF                                                         
000642**** AVERAGE COST BERÄKNING SKALL TA HÄNSYN TILL EFR                      
000643           COMPUTE AVG-KVLS-OLD = SLAG-KVLS +                             
000644                   SLAG-KVEFRS                                            
000645                                                                          
000646           MOVE MID-IDLEVNR (MID-IX) TO W-IDLEVNR                         
000647           PERFORM IMS-GU-WLLEVA01                                        
000648           IF SEGMENT-SAKNAS                                              
000649             MOVE ZERO TO W-RETULF                                        
000650           ELSE                                                           
000651             PERFORM IMS-GU-WDB601                                        
000652             MOVE DCS-IDLANDX2 TO W-IDLAND                                
000653             PERFORM IMS-GNP-WLLEVA11                                     
000654             IF SEGMENT-FINNS                                             
000655               IF LEV-TULL-TITULF < DAGENS-DATUM                          
000656                 MOVE LEV-TULL-RETULF-1 TO W-RETULF                       
000657               ELSE                                                       
000658                 MOVE LEV-TULL-RETULF-2 TO W-RETULF                       
000659               END-IF                                                     
000660             END-IF                                                       
000661           END-IF                                                         
000662           MOVE W-RETULF              TO AVG-REMARKUP                     
000663                                                                          
000670           MOVE WS-TIFAKT             TO WS-DAAVIDAT-YYMMDD               
000671           MOVE WS-DAAVIDAT-YYMMDD(1:2) TO W-DATE-AAMM(1:2)               
000672           MOVE 01                      TO W-DATE-AAMM(3:2)               
000673           MOVE 'SEK'                 TO CURR-KDVALISO-HUV                
000674           IF DCS-NDC-CN                                                  
000675             MOVE 'CNY'               TO CURR-KDVALISO-ROW                
000676           END-IF                                                         
000677           IF DCS-USA                                                     
000678             MOVE 'USD'               TO CURR-KDVALISO-ROW                
000679           END-IF                                                         
000680           MOVE W-DATE-AAMM           TO CURR-TIAAMM                      
000681           MOVE 'A'                   TO CURR-KDVALTYP                    
000682           CALL W510CURR USING CURR-W510CURR 9305-PCB                     
000683           IF CURR-KDSVAR = ' '                                           
000684             MOVE CURR-PRKURS-NEW     TO WS-PRKURS                        
000685             MOVE CURR-REVALUTA-TO    TO WS-REVALUTA                      
000686           ELSE                                                           
000687             MOVE 1                   TO WS-PRKURS                        
000688             MOVE 1                   TO WS-REVALUTA                      
000689           END-IF                                                         
000690           MOVE WS-IDDC               TO W-IDDC                           
000691           PERFORM IMS-GU-WDB601                                          
000692           MOVE DCS-IDLANDX2          TO W-IDLANDX2                       
000693           IF SEGMENT-FINNS                                               
000694             PERFORM IMS-GNP-WDB617                                       
000695             IF SEGMENT-FINNS                                             
000696               COMPUTE WS-PRARTKALKYL        ROUNDED =                    
000697                      (ARTC11-CLAG-PRDIRLON *                             
000698                      PROC-REDIRLON * WS-REVALUTA / WS-PRKURS) +          
000699                      (ARTC11-CLAG-PRDMTRL *                              
000700                       PROC-REDMTRL * WS-REVALUTA / WS-PRKURS)            
000701             ELSE                                                         
000702               MOVE ZERO TO WS-PRARTKALKYL                                
000703             END-IF                                                       
000704           END-IF                                                         
000705           MOVE WS-PRARTKALKYL        TO AVG-PRARTNTO                     
000706           COMPUTE AVG-KVANTMOT = MID-KVANTAL(MID-IX) * -1                
000707           MOVE SLAG-PRAVCOST         TO AVG-PRAVCOST-OLD                 
000708           MOVE SPAR-PRKURS           TO AVG-PRKURS                       
000709           MOVE +0                    TO AVG-KVLEVART                     
000710           MOVE SPAR-ARTC21-KDVALISO  TO AVG-KDVALISO                     
000711           MOVE SPAR-ARTC21-PRARTBEL-PR TO AVG-PRARTBEL                   
000712           MOVE SPAR-ARTC11-KDPSLLOC  TO AVG-KDPSLLOC                     
000713           MOVE SPAR-ARTC01-KDPRODSL  TO AVG-KDPRODSL                     
000714           MOVE SPAR-ARTC01-IDFKNGRP  TO AVG-IDFKNGRP                     
000715           MOVE W-IDDC                TO AVG-IDDC                         
000716           MOVE +0                    TO AVG-PRAVCOST-NEW                 
000717           MOVE SPACE                 TO AVG-KDSVAR                       
000718           MOVE WS-TIAAMMDDTTMMSSTH-DATE(1:2) TO AVG-TIAA                 
000719           MOVE WS-TIAAMMDDTTMMSSTH-DATE(3:2) TO AVG-TIMM                 
000720                                                                          
000721           IF HITTAD-JA                                                   
000722             CALL W510AVG USING AVG-W510AVG                               
000723                                9305-AVG-PCB AVG-WDB6-PCB                 
000724             IF AVG-KDSVAR = SPACE                                        
000725               MOVE AVG-PRAVCOST-NEW TO WS-PRAVCOST                       
000726             ELSE                                                         
000727               IF AVG-KDSVAR = '4'                                        
000728                 MOVE SLAG-PRAVCOST TO WS-PRAVCOST                        
000729               ELSE                                                       
000730                 STRING 'FEL FRÅN W510AVG ' AVG-KDSVAR                    
000731                  DELIMITED BY SIZE INTO FELTEXT                          
000732                   CALL FELLOG                                            
000733               END-IF                                                     
000734             END-IF                                                       
000735             MOVE WS-PRAVCOST          TO   SLAG-PRAVCOST                 
000736           END-IF                                                         
000737         END-IF                                                           
000738       END-IF                                                             
000739                                                                          
000740       SUBTRACT MID-KVANTAL (MID-IX) FROM SLAG-KVLS                       
000741       PERFORM IMS-REPL-WDK711                                            
000742     END-IF                                                               
000743                                                                          
000744     PERFORM BA-SKAPA-SALDOLOGG                                           
000745     .                                                                    
000746     EJECT                                                                
000747 BA-SKAPA-SALDOLOGG SECTION.                                              
000748     MOVE MID-IDARTNR (MID-IX)      TO LOGG-IDARTNR                       
000749     MOVE 9                         TO LOGG-IDSEKVNR                      
000750     MOVE MID-IDDC (MID-IX)         TO LOGG-IDDC                          
000751     MOVE 'MISC'                    TO LOGG-IDHUVTYP                      
000752     MOVE 'R40'                     TO LOGG-IDSUBTYP                      
000753     MOVE 'W6011D00'                TO LOGG-IDPGM                         
000754     IF W-IDTRANS = '6100'                                                
000755        MOVE W-IDTRANS              TO LOGG-IDTRANS                       
000756     ELSE                                                                 
000757        MOVE '6203'                 TO LOGG-IDTRANS                       
000758     END-IF                                                               
000759     MOVE MSG-SIGNON-USERID         TO LOGG-IDUSER                        
000760     MOVE SPACE                     TO LOGG-REF                           
000761     MOVE MID-IDLEVNR (MID-IX)      TO LOGG-IDLEVNR                       
000762     MOVE MID-IDORDNR (MID-IX)      TO LOGG-IDKR                          
000763     MOVE SPACE                     TO LOGG-IDTECKEN-KVAKS                
000764     MOVE SPACE                     TO LOGG-IDTECKEN-KVAKS-PAV            
000765     MOVE SPACE                     TO LOGG-IDTECKEN-KVEFRS               
000766     IF MID-KVANTAL (MID-IX) > ZERO                                       
000767       MOVE '-'                     TO LOGG-IDTECKEN-KVLS                 
000768       MOVE MID-KVANTAL (MID-IX)    TO LOGG-KVART-SALDO                   
000769     ELSE                                                                 
000770       MOVE '+'                     TO LOGG-IDTECKEN-KVLS                 
000771       COMPUTE LOGG-KVART-SALDO = MID-KVANTAL(MID-IX) * (-1)              
000772     END-IF                                                               
000773*                                                                         
000774     IF CDC                                                               
000775       COMPUTE LOGG-KVAKS = ARTC11-CLAG-KVAKS-CDC +                       
000776                            ARTC11-CLAG-KVAKS-T                           
000777                                                                          
000778       MOVE ARTC11-CLAG-KVAKS-PAV   TO LOGG-KVAKS-PAV                     
000779       MOVE ARTC11-CLAG-KVEFRS      TO LOGG-KVEFRS                        
000780       MOVE ARTC11-CLAG-KVLS        TO LOGG-KVLS                          
000781     ELSE                                                                 
000782       PERFORM IMS-GU-WDK711                                              
000783       MOVE SLAG-KVAKS-SDC          TO LOGG-KVAKS                         
000784       MOVE SLAG-KVAKS-PAV          TO LOGG-KVAKS-PAV                     
000785       MOVE SLAG-KVEFRS             TO LOGG-KVEFRS                        
000786       MOVE SLAG-KVLS               TO LOGG-KVLS                          
000787     END-IF                                                               
000788*                                                                         
000789     MOVE ZERO                      TO LOGG-DAREGDAT-LADD                 
000790     MOVE FUNCTION CURRENT-DATE(1:8) TO LOGG-DATUM                        
000791     COMPUTE LOGG-DAREGDAT-9KOMPL = 999999999 - LOGG-DATUM                
000792     ACCEPT WLOGG-TID FROM TIME                                           
000793     COMPUTE LOGG-TIKLOCK-9KOMPL = 999999999 - WLOGG-TID                  
000794                                                                          
000795     PERFORM IMS-ISRT-WDL9                                                
000796     IF SEGMENT-FINNS-REDAN                                               
000797       PERFORM UNTIL NOT SEGMENT-FINNS-REDAN                              
000798          ADD -1 TO LOGG-IDSEKVNR                                         
000799          PERFORM IMS-ISRT-WDL9                                           
000800       END-PERFORM                                                        
000801     END-IF                                                               
000802     .                                                                    
000803     EJECT                                                                
000804                                                                          
000805 C-UPPD-BESTREST SECTION.                                                 
000806                                                                          
000807     IF MID-FLUPPBR (MID-IX) = 1                                          
000808       IF MID-IDLEVNR (MID-IX) = '1001 ' OR '1003 ' OR '1004 ' OR         
000809                                 '1012 ' OR '1013 ' OR '2120 ' OR         
000810                      'BL3YA' OR 'BP2TH' OR 'BP2TD' OR 'BP2TC'            
000811         CONTINUE                                                         
000812       ELSE                                                               
000813         MOVE MID-IDARTNR (MID-IX) TO W-IDARTNR-D9                        
000814         MOVE MID-IDDC (MID-IX)    TO W-IDDC-D9                           
000815         MOVE MID-IDLEVNR (MID-IX) TO W-IDLEVNR                           
000816         PERFORM IMS-GHU-INLB11                                           
000817         IF SEGMENT-FINNS                                                 
000818           ADD MID-KVANTAL (MID-IX) TO INLB11-KVBR                        
000819           PERFORM IMS-REPL-INLB                                          
000820         END-IF                                                           
000821       END-IF                                                             
000822     END-IF                                                               
000823     .                                                                    
000824     EJECT                                                                
000825 D-MEDDELANDE              SECTION.                                       
000826                                                                          
000827     IF CDC                                                               
000828       IF MID-FLUPPBR (MID-IX) = 1                                        
000829         PERFORM DA-MEDDELANDE-TILL-W221                                  
000830       END-IF                                                             
000831*                                                                         
000832       IF MID-IDLEVNR (MID-IX) = '1001 ' OR '1003 ' OR '1004 ' OR         
000833                                 '1012 ' OR '1013 ' OR '2120 ' OR         
000834                        'BL3YA' OR 'BP2TH' OR 'BP2TD' OR 'BP2TC'          
000835          IF (MID-FLUPPBR (MID-IX) NOT = 1)                               
000836            PERFORM DB-MEDDELANDE-TILL-W232                               
000837          END-IF                                                          
000838       END-IF                                                             
000839*                                                                         
000840       PERFORM DC-LOGG-EKO-WDR9                                           
000841     ELSE                                                                 
000842       IF NDC-CN                                                          
000843         PERFORM DD-LOGG-EKO-WDR8-CN                                      
000844       END-IF                                                             
000845       IF NDC-US                                                          
000846         PERFORM DE-LOGG-EKO-WDR8-US                                      
000847       END-IF                                                             
000848     END-IF                                                               
000849     .                                                                    
000850     EJECT                                                                
000851 DA-MEDDELANDE-TILL-W221 SECTION.                                         
000852                                                                          
000853     MOVE ZERO                   TO W400-W211400                          
000854     MOVE '221'                  TO W400-IDTTYP                           
000855     MOVE '400'                  TO W400-IDPTYP                           
000856     MOVE MID-IDARTNR (MID-IX)   TO W400-IDARTNR                          
000857                                    W400-IDARTNR-S                        
000858     MOVE +1                     TO W400-KDCLAGER                         
000859                                    W400-KDCLAGER-S                       
000860     MOVE MID-KVANTAL (MID-IX)   TO W400-KVRETUR                          
000861     MOVE MID-IDLEVNR (MID-IX)   TO W400-IDLEVNR-INL                      
000862     MOVE MID-FLUPPBR  (MID-IX)  TO W400-FLUPPBR                          
000863     MOVE MID-IDORDNR (MID-IX)   TO W400-IDKONTO                          
000864                                                                          
000865     MOVE W400-W211400           TO WS-ZZAC01-LOGGPOST                    
000866     MOVE SPACE                  TO WS-ZZAC01-SORTPOST                    
000867     PERFORM S01-SKAPA-ZZAC01                                             
000868     .                                                                    
000869     EJECT                                                                
000870 DB-MEDDELANDE-TILL-W232 SECTION.                                         
000871                                                                          
000872     MOVE ZERO                   TO W232-W232232                          
000873     MOVE '232'                  TO W232-IDPTYP                           
000874     MOVE MID-IDARTNR (MID-IX)   TO W232-IDARTNR                          
000875     MOVE MID-KVANTAL (MID-IX)   TO W232-KVOI                             
000876     MOVE +113                   TO W232-KDOI                             
000877     MOVE 'O'                    TO W232-KDBEHX                           
000878     MOVE ZERO                   TO W232-KVOT                             
000879     MOVE DAT-TIAARP             TO W232-TIAAPP-AVBOK                     
000880                                                                          
000881     MOVE W232-W232232           TO WS-ZZAC01-LOGGPOST                    
000882     MOVE SPACE                  TO WS-ZZAC01-SORTPOST                    
000883     PERFORM S01-SKAPA-ZZAC01                                             
000884     .                                                                    
000885     EJECT                                                                
000886 DC-LOGG-EKO-WDR9 SECTION.                                                
000887                                                                          
000888     MOVE 'W6011D00'                  TO FIL-IDPGM                        
000889     MOVE FUNCTION CURRENT-DATE (1:8) TO FIL-DAREGDAT                     
000890                                         EKH-DAVERDAT                     
000891     MOVE FUNCTION CURRENT-DATE (9:8) TO FIL-TIKLOCK                      
000892     MOVE +1                          TO FIL-IDSEKVNR                     
000893     MOVE 'W510EKHA'                  TO FIL-IDCPYTXT                     
000894     MOVE MSG-SIGNON-USERID           TO FIL-IDUSER                       
000895     MOVE '102'                       TO EKH-KDEKHHT                      
000896     MOVE '106'                       TO EKH-KDEKSHT                      
000897     MOVE 'DET  '                     TO EKH-KDEKNIVA                     
000898     MOVE WC-CDC-SE                   TO EKH-IDDC-SEND                    
000899     MOVE SPACE                       TO EKH-IDDC-REC                     
000900     MOVE +0                          TO EKH-IDDISTR                      
000901                                         EKH-IDKUNDNR                     
000902*******************************                                           
000903*VÄNSTERJUSTERAT,SPACE I SLUTET,INGA INLEDANDE NOLLOR I DESSA FÄLT        
000904     MOVE ZERO TO NOLL-RAKNARE                                            
000905     MOVE MID-IDORDNR(MID-IX)        TO WS-SAP-X-IDORDNR                  
000906     INSPECT WS-SAP-X-IDORDNR TALLYING NOLL-RAKNARE                       
000907          FOR LEADING ZERO                                                
000908     ADD +1 TO NOLL-RAKNARE                                               
000909     UNSTRING WS-SAP-X-IDORDNR     INTO EKH-IDVERGL                       
000910          WITH POINTER NOLL-RAKNARE                                       
000911*******************************                                           
000912     MOVE SPAR-ARTC01-KDPRODSL        TO EKH-KDPRODSL                     
000913     MOVE ZERO                        TO EKH-KDPSLLOC                     
000914                                         EKH-PRARTNTO                     
000915                                         EKH-PRARTSJK                     
000916                                         EKH-PRHEMTAG                     
000917                                         EKH-PRINK                        
000918                                         EKH-PRLANDCO                     
000919                                         EKH-PRDIRLON                     
000920                                         EKH-PRDMTRL                      
000921                                         EKH-PROVRPAL                     
000922                                         EKH-SUBEL                        
000923     MOVE MID-IDARTNR(MID-IX)         TO EKH-IDARTNR                      
000924     MOVE SPACE                       TO EKH-FLLSBOK                      
000925     MOVE 'SEK'                       TO EKH-KDVALISO                     
000926     MOVE 1.00                        TO EKH-PRKURS                       
000927                                                                          
000928     MOVE ARTC11-CLAG-PRARTSTD        TO EKH-PRARTSTD                     
000929     COMPUTE EKH-KVANTAL = MID-KVANTAL(MID-IX) +                          
000930                           MID-KVART-SKROT-LDC                            
000931     MOVE '6203'                      TO EKH-IDTRANS                      
000932     MOVE SPACE                       TO EKH-BEVAT                        
000933                                         EKH-IDANALYS                     
000934                                         EKH-KDANMORS                     
000935     MOVE ZERO                        TO EKH-IDKONTO                      
000936*                                        EKH-KDBETVIL                     
000937                                         EKH-SUVAT                        
000938                                         EKH-KDFRAKT                      
000939                                                                          
000940                                                                          
000941     MOVE FUNCTION CURRENT-DATE (1:8) TO EKH-DAAVIDAT                     
000942     MOVE ZERO                        TO EKH-IDAVINR                      
000943     MOVE MID-IDLEVNR (MID-IX)        TO EKH-IDLEVNR                      
000944     MOVE ZERO                        TO EKH-KDAVVTYP                     
000945                                         EKH-KDRT                         
000946                                         EKH-KVANTMOT                     
000947                                         EKH-KVAVIS                       
000948     MOVE WS-KDSORT                   TO EKH-KDSORT                       
000949     MOVE SPACE                       TO EKH-KDTRADP                      
000950                                         EKH-IDKST                        
000951     MOVE SPACE                       TO EKH-FLDCET                       
000952     MOVE SPACE                       TO EKH-IDKUNDRF                     
000953     MOVE SPACE                       TO EKH-IDFAKT-EXP                   
000954                                                                          
000955                                                                          
000956     PERFORM IMS-ISRT-WLSAPA01                                            
000957                                                                          
000958     PERFORM UNTIL SEGMENT-FINNS                                          
000959       ADD +1 TO FIL-IDSEKVNR                                             
000960       PERFORM IMS-ISRT-WLSAPA01                                          
000961     END-PERFORM                                                          
000962     .                                                                    
000963     EJECT                                                                
000964 DD-LOGG-EKO-WDR8-CN SECTION.                                             
000965                                                                          
000966     MOVE 'W6011D00'                  TO EKO-FIL-IDPGM                    
000967     ACCEPT EKO-FIL-TIREGDAT FROM DATE                                    
000968     ACCEPT EKO-FIL-TIKLOCK  FROM TIME                                    
000969     MOVE +1                          TO EKO-FIL-IDSEKVNR                 
000970     MOVE 'W570EKHA'                  TO EKO-FIL-IDCPYTXT                 
000971     MOVE FUNCTION CURRENT-DATE(1:8)  TO EKO-EKH-DAVERDAT                 
000972     MOVE '102'                       TO EKO-EKH-KDEKHHT                  
000973     MOVE '106'                       TO EKO-EKH-KDEKSHT                  
000974     MOVE 'DET  '                     TO EKO-EKH-KDEKNIVA                 
000975     MOVE MID-IDDC(MID-IX)            TO EKO-EKH-IDDC-SEND                
000976     MOVE SPACE                       TO EKO-EKH-IDDC-REC                 
000977     MOVE +0                          TO EKO-EKH-IDDISTR                  
000978                                         EKO-EKH-IDKUNDNR                 
000979*******************************                                           
000980*VÄNSTERJUSTERAT,SPACE I SLUTET,INGA INLEDANDE NOLLOR I DESSA FÄLT        
000981     MOVE ZERO TO NOLL-RAKNARE                                            
000982     MOVE MID-IDORDNR(MID-IX)        TO WS-SAP-X-IDORDNR                  
000983     INSPECT WS-SAP-X-IDORDNR TALLYING NOLL-RAKNARE                       
000984          FOR LEADING ZERO                                                
000985     ADD +1 TO NOLL-RAKNARE                                               
000986     UNSTRING WS-SAP-X-IDORDNR     INTO EKO-EKH-IDVERGL                   
000987          WITH POINTER NOLL-RAKNARE                                       
000988*******************************                                           
000989     MOVE SPAR-ARTC01-KDPRODSL        TO EKO-EKH-KDPRODSL                 
000990     MOVE ZERO                        TO EKO-EKH-KDPSLLOC                 
000991                                         EKO-EKH-PRARTNTO                 
000992                                         EKO-EKH-PRARTSJK                 
000993                                         EKO-EKH-PRHEMTAG                 
000994                                         EKO-EKH-PRINK                    
000995                                         EKO-EKH-PRLANDCO                 
000996                                         EKO-EKH-PRDIRLON                 
000997                                         EKO-EKH-PRDMTRL                  
000998                                         EKO-EKH-PROVRPAL                 
000999                                         EKO-EKH-SUBEL                    
001000     MOVE MID-IDARTNR(MID-IX)         TO EKO-EKH-IDARTNR                  
001001     MOVE SPACE                       TO EKO-EKH-FLLSBOK                  
001002*    MOVE 'CNY'                       TO EKO-EKH-KDVALISO                 
001003*    MOVE 1.00                        TO EKO-EKH-PRKURS                   
001004                                                                          
001005     PERFORM DDAA-GET-PRARTBEL                                            
001006     PERFORM DDAB-GET-CURRENCY-RATE                                       
001007     IF SPAR-ARTC21-PRARTBEL-PR > ZERO                                    
001008       MOVE SPAR-ARTC21-PRARTBEL-PR   TO EKO-EKH-PRARTSTD                 
001009       MOVE SPAR-ARTC21-KDVALISO      TO EKO-EKH-KDVALISO                 
001010       MOVE SPAR-PRKURS               TO EKO-EKH-PRKURS                   
001011     ELSE                                                                 
001012       MOVE 0.1                       TO EKO-EKH-PRARTSTD                 
001013       MOVE 'XXX'                     TO EKO-EKH-KDVALISO                 
001014       MOVE 1.00                      TO EKO-EKH-PRKURS                   
001015     END-IF                                                               
001016     COMPUTE EKO-EKH-KVANTAL = MID-KVANTAL(MID-IX) +                      
001017                           MID-KVART-SKROT-LDC                            
001018     MOVE '6203'                      TO EKO-EKH-IDTRANS                  
001019     MOVE SPACE                       TO EKO-EKH-BEVAT                    
001020                                         EKO-EKH-IDANALYS                 
001021                                         EKO-EKH-KDANMORS                 
001022     MOVE ZERO                        TO EKO-EKH-IDKONTO                  
001023*                                        EKO-EKH-KDBETVIL                 
001024                                         EKO-EKH-SUVAT                    
001025                                         EKO-EKH-KDFRAKT                  
001026                                                                          
001027                                                                          
001028     MOVE FUNCTION CURRENT-DATE (1:8) TO EKO-EKH-DAAVIDAT                 
001029     MOVE ZERO                        TO EKO-EKH-IDAVINR                  
001030     MOVE MID-IDLEVNR (MID-IX)        TO EKO-EKH-IDLEVNR                  
001031     MOVE ZERO                        TO EKO-EKH-KDAVVTYP                 
001032                                         EKO-EKH-KDRT                     
001033                                         EKO-EKH-KVANTMOT                 
001034                                         EKO-EKH-KVAVIS                   
001035     MOVE WS-KDSORT                   TO EKO-EKH-KDSORT                   
001036     MOVE 'CN05'                      TO EKO-EKH-KDTRADP                  
001037     MOVE SPACE                       TO EKO-EKH-FLDCET                   
001038                                         EKO-EKH-IDKST                    
001039     MOVE SPACE                       TO EKO-EKH-IDKUNDRF                 
001040     MOVE SPACE                       TO EKO-EKH-IDFAKT-EXP               
001041                                                                          
001042                                                                          
001043*    PERFORM IMS-ISRT-WDR801                                              
001044                                                                          
001045*    PERFORM UNTIL SEGMENT-FINNS                                          
001046*      ADD +1 TO EKO-FIL-IDSEKVNR                                         
001047*      PERFORM IMS-ISRT-WDR801                                            
001048*    END-PERFORM                                                          
001049     .                                                                    
001050     EJECT                                                                
001051                                                                          
001052 DE-LOGG-EKO-WDR8-US SECTION.                                             
001053     MOVE 'W6011D00'                  TO EKO-FIL-IDPGM                    
001054     ACCEPT EKO-FIL-TIREGDAT FROM DATE                                    
001055     ACCEPT EKO-FIL-TIKLOCK  FROM TIME                                    
001056     MOVE +1                          TO EKO-FIL-IDSEKVNR                 
001057     MOVE 'W561EKHA'                  TO EKO-FIL-IDCPYTXT                 
001058     MOVE FUNCTION CURRENT-DATE(1:8)  TO EKO-EKH-DAVERDAT                 
001059     MOVE '102'                       TO EKO-EKH-KDEKHHT                  
001060     MOVE '106'                       TO EKO-EKH-KDEKSHT                  
001061     MOVE 'DET  '                     TO EKO-EKH-KDEKNIVA                 
001062     MOVE MID-IDDC(MID-IX)            TO EKO-EKH-IDDC-SEND                
001063     MOVE SPACE                       TO EKO-EKH-IDDC-REC                 
001064     MOVE +0                          TO EKO-EKH-IDDISTR                  
001065                                         EKO-EKH-IDKUNDNR                 
001066*******************************                                           
001067*VÄNSTERJUSTERAT,SPACE I SLUTET,INGA INLEDANDE NOLLOR I DESSA FÄLT        
001068     MOVE ZERO TO NOLL-RAKNARE                                            
001069     MOVE MID-IDORDNR(MID-IX)        TO WS-SAP-X-IDORDNR                  
001070     INSPECT WS-SAP-X-IDORDNR TALLYING NOLL-RAKNARE                       
001071          FOR LEADING ZERO                                                
001072     ADD +1 TO NOLL-RAKNARE                                               
001073     UNSTRING WS-SAP-X-IDORDNR     INTO EKO-EKH-IDVERGL                   
001074          WITH POINTER NOLL-RAKNARE                                       
001075*******************************                                           
001076     MOVE SPAR-ARTC01-KDPRODSL        TO EKO-EKH-KDPRODSL                 
001077     MOVE ZERO                        TO EKO-EKH-KDPSLLOC                 
001078                                         EKO-EKH-PRARTNTO                 
001079                                         EKO-EKH-PRARTSJK                 
001080                                         EKO-EKH-PRHEMTAG                 
001081                                         EKO-EKH-PRINK                    
001082                                         EKO-EKH-PRLANDCO                 
001083                                         EKO-EKH-PRDIRLON                 
001084                                         EKO-EKH-PRDMTRL                  
001085                                         EKO-EKH-PROVRPAL                 
001086                                         EKO-EKH-SUBEL                    
001087     MOVE MID-IDARTNR(MID-IX)         TO EKO-EKH-IDARTNR                  
001088     MOVE SPACE                       TO EKO-EKH-FLLSBOK                  
001089*    MOVE 'CNY'                       TO EKO-EKH-KDVALISO                 
001090*    MOVE 1.00                        TO EKO-EKH-PRKURS                   
001091                                                                          
001092     PERFORM DDAA-GET-PRARTBEL                                            
001093     PERFORM DDAB-GET-CURRENCY-RATE                                       
001094     IF SPAR-ARTC21-PRARTBEL-PR > ZERO                                    
001095       MOVE SPAR-ARTC21-PRARTBEL-PR   TO EKO-EKH-PRARTSTD                 
001096       MOVE SPAR-ARTC21-KDVALISO      TO EKO-EKH-KDVALISO                 
001097       MOVE SPAR-PRKURS               TO EKO-EKH-PRKURS                   
001098     ELSE                                                                 
001099       MOVE 0.1                       TO EKO-EKH-PRARTSTD                 
001100       MOVE 'XXX'                     TO EKO-EKH-KDVALISO                 
001101       MOVE 1.00                      TO EKO-EKH-PRKURS                   
001102     END-IF                                                               
001103     COMPUTE EKO-EKH-KVANTAL = MID-KVANTAL(MID-IX) +                      
001104                           MID-KVART-SKROT-LDC                            
001105     MOVE '6203'                      TO EKO-EKH-IDTRANS                  
001106     MOVE SPACE                       TO EKO-EKH-BEVAT                    
001107                                         EKO-EKH-IDANALYS                 
001108                                         EKO-EKH-KDANMORS                 
001109     MOVE ZERO                        TO EKO-EKH-IDKONTO                  
001110*                                        EKO-EKH-KDBETVIL                 
001111                                         EKO-EKH-SUVAT                    
001112                                         EKO-EKH-KDFRAKT                  
001113                                                                          
001114                                                                          
001115     MOVE FUNCTION CURRENT-DATE (1:8) TO EKO-EKH-DAAVIDAT                 
001116     MOVE ZERO                        TO EKO-EKH-IDAVINR                  
001117     MOVE MID-IDLEVNR (MID-IX)        TO EKO-EKH-IDLEVNR                  
001118     MOVE ZERO                        TO EKO-EKH-KDAVVTYP                 
001119                                         EKO-EKH-KDRT                     
001120                                         EKO-EKH-KVANTMOT                 
001121                                         EKO-EKH-KVAVIS                   
001122     MOVE WS-KDSORT                   TO EKO-EKH-KDSORT                   
001123     MOVE 'US01'                      TO EKO-EKH-KDTRADP                  
001124     MOVE SPACE                       TO EKO-EKH-FLDCET                   
001125                                         EKO-EKH-IDKST                    
001126     MOVE SPACE                       TO EKO-EKH-IDKUNDRF                 
001127     MOVE SPACE                       TO EKO-EKH-IDFAKT-EXP               
001128                                                                          
001129                                                                          
001130*    PERFORM IMS-ISRT-WDR801                                              
001131                                                                          
001132*    PERFORM UNTIL SEGMENT-FINNS                                          
001133*      ADD +1 TO EKO-FIL-IDSEKVNR                                         
001134*      PERFORM IMS-ISRT-WDR801                                            
001135*    END-PERFORM                                                          
001136     .                                                                    
001137     EJECT                                                                
001138 DDAA-GET-PRARTBEL SECTION.                                               
001139     MOVE +0                    TO SPAR-ARTC21-PRARTBEL-PR                
001140                                   SPAR-ARTC21-PRARTBES-PR                
001141                                                                          
001142     MOVE MID-IDORDNR(1)   TO W-IDKR                                      
001143     PERFORM IMS-GU-W6H712                                                
001144     IF SEGMENT-FINNS                                                     
001145       IF EK-PRARTBEL-PR > ZERO                                           
001146         MOVE EK-PRARTBEL-PR TO SPAR-ARTC21-PRARTBEL-PR                   
001147         MOVE EK-PRARTBEL-PR TO SPAR-ARTC21-PRARTBES-PR                   
001148         MOVE EK-KDVALISO    TO SPAR-ARTC21-KDVALISO                      
001149         MOVE EK-TIFAKT      TO WS-TIFAKT                                 
001150         MOVE JA TO W-HITTAD-SW                                           
001151       ELSE                                                               
001152         PERFORM DDAAA-GET-PRARTBEL                                       
001153       END-IF                                                             
001154     ELSE                                                                 
001155       PERFORM DDAAA-GET-PRARTBEL                                         
001156     END-IF                                                               
001157     .                                                                    
001158     EJECT                                                                
001159                                                                          
001160 DDAB-GET-CURRENCY-RATE SECTION.                                          
001161     MOVE WS-IDDC         TO W-IDDC                                       
001162     PERFORM IMS-GU-WDB601                                                
001163     MOVE DCS-KDVALISO      TO W-KDVALISO-HUV                             
001164     MOVE SPAR-ARTC21-KDVALISO TO W-KDVALISO-ROW                          
001165     IF W-KDVALISO-HUV = SPAR-ARTC21-KDVALISO                             
001166       MOVE 1 TO SPAR-PRKURS                                              
001167       MOVE 1 TO W-REVALUTA                                               
001168     ELSE                                                                 
001169       PERFORM IMS-GU-WDGX9306                                            
001170       IF SEGMENT-SAKNAS                                                  
001171         MOVE 1               TO SPAR-PRKURS                              
001172         MOVE 1               TO W-REVALUTA                               
001173       ELSE                                                               
001174         COMPUTE W-TISTADAT-9KOMPL =                                      
001175                 9999999 - WS-DATE-YYMMDD                                 
001176         PERFORM IMS-GNP-WDGX9308                                         
001177         IF SEGMENT-SAKNAS                                                
001178           PERFORM IMS-GNP-WDGX9308-FIRST                                 
001179           IF SEGMENT-SAKNAS                                              
001180             MOVE 1               TO SPAR-PRKURS                          
001181             MOVE 1               TO W-REVALUTA                           
001182           ELSE                                                           
001183             MOVE 9308-PRKURS   TO SPAR-PRKURS                            
001184             MOVE 9308-REVALUTA-TO TO W-REVALUTA                          
001185           END-IF                                                         
001186         ELSE                                                             
001187           MOVE 9308-PRKURS   TO SPAR-PRKURS                              
001188           MOVE 9308-REVALUTA-TO TO W-REVALUTA                            
001189         END-IF                                                           
001190       END-IF                                                             
001191     END-IF                                                               
001192     .                                                                    
001193     EJECT                                                                
001194                                                                          
001195 DDAAA-GET-PRARTBEL SECTION.                                              
001196*    -- WDK711                                                            
001197     PERFORM IMS-GU-WDK711-K7                                             
001198     IF SEGMENT-FINNS                                                     
001199                                                                          
001200*    -- WDK724                                                            
001201*      MOVE MID-IDLEVNR(MID-IX) TO W-IDLEVNR-PR                           
001202*      COMPUTE W-DAPRLIST-K7 = 99999999 - WS-TIAAMMDDTTMMSSTH-DATE        
001203       MOVE NEJ TO W-HITTAD-SW                                            
001204       PERFORM IMS-GNP-WDK724-FIRST                                       
001205                                                                          
001206       PERFORM UNTIL SEGMENT-SAKNAS OR HITTAD-JA                          
001207         IF SEGMENT-FINNS                                                 
001208           IF SPRL-IDLEVNR-PR = MID-IDLEVNR(MID-IX)                       
001209             IF SPRL-SUINLEV-PR > +0                                      
001210               MOVE SPRL-PRARTBEL-PR TO SPAR-ARTC21-PRARTBEL-PR           
001211               MOVE SPRL-PRARTBES-PR TO SPAR-ARTC21-PRARTBES-PR           
001212               MOVE SPRL-KDVALISO    TO SPAR-ARTC21-KDVALISO              
001213               MOVE JA TO W-HITTAD-SW                                     
001214             ELSE                                                         
001215               PERFORM IMS-GNP-WDK724-NEXT                                
001216             END-IF                                                       
001217           ELSE                                                           
001218             PERFORM IMS-GNP-WDK724-NEXT                                  
001219           END-IF                                                         
001220         END-IF                                                           
001221       END-PERFORM                                                        
001222     END-IF                                                               
001223     .                                                                    
001224     EJECT                                                                
001225                                                                          
001226 E-UPPD-HISTORIK           SECTION.                                       
001227                                                                          
001228     PERFORM IMS-GHU-W6LOPA11                                             
001229     IF NDC-CN                                                            
001230       MOVE 6018-IDLOPNRM-JP-AU TO WS-IDLOPNRM                            
001231     ELSE                                                                 
001232       MOVE 6018-IDLOPNRM       TO WS-IDLOPNRM                            
001233     END-IF                                                               
001234*                                                                         
001235     PERFORM S04-SKAPA-IDINLEV-IDLOPNRM                                   
001236                                                                          
001237     IF CDC                                                               
001238       PERFORM IMS-GU-WLINLE01                                            
001239       IF SEGMENT-SAKNAS                                                  
001240         MOVE W-IDARTNR           TO INLE-ART-IDARTNR                     
001241         PERFORM IMS-ISRT-WLINLE01                                        
001242       END-IF                                                             
001243*                                                                         
001244       MOVE WS-DAINLEV            TO INLE-INL-DAINLEV W-DAINLEV           
001245       PERFORM IMS-ISRT-WLINLE11                                          
001246       PERFORM UNTIL SEGMENT-FINNS                                        
001247         SUBTRACT +1 FROM WS-DAINLEV                                      
001248         MOVE WS-DAINLEV          TO INLE-INL-DAINLEV W-DAINLEV           
001249         PERFORM IMS-ISRT-WLINLE11                                        
001250       END-PERFORM                                                        
001251*                                                                         
001252       MOVE 'R40'                 TO INLE-RET-IDPTYP                      
001253       MOVE WS-IDLOPNRM           TO INLE-RET-IDLOPNRM                    
001254       MOVE MID-IDLEVNR (MID-IX)  TO INLE-RET-IDLEVNR                     
001255       MOVE MID-IDORDNR (MID-IX)  TO INLE-RET-IDORDNR                     
001256       MOVE MID-IDDC    (MID-IX)  TO INLE-RET-IDDC                        
001257       MOVE MID-KVANTAL (MID-IX)  TO INLE-RET-KVRETUR                     
001258       PERFORM IMS-ISRT-WLINLE23                                          
001259     ELSE                                                                 
001260       PERFORM IMS-GU-WDL601                                              
001261       IF SEGMENT-SAKNAS                                                  
001262         MOVE W-IDARTNR           TO ART-IDARTNR                          
001263         PERFORM IMS-ISRT-WDL601                                          
001264       END-IF                                                             
001265*                                                                         
001266       INITIALIZE INL-WDL611                                              
001267       MOVE WS-DAINLEV            TO INL-DAINLEV W-DAINLEV                
001268       MOVE 'R40'                 TO INL-IDPTYP                           
001269       MOVE WS-IDLOPNRM           TO INL-IDLOPNRM                         
001270       MOVE MID-IDLEVNR (MID-IX)  TO INL-IDLEVNR                          
001271       MOVE MID-IDORDNR (MID-IX)  TO INL-IDORDNR5                         
001272       MOVE MID-IDDC    (MID-IX)  TO INL-IDDC                             
001273       MOVE MID-KVANTAL (MID-IX)  TO INL-KVRETUR                          
001274       PERFORM IMS-ISRT-WDL611                                            
001275       PERFORM UNTIL SEGMENT-FINNS                                        
001276         SUBTRACT +1 FROM WS-DAINLEV                                      
001277         MOVE WS-DAINLEV            TO INL-DAINLEV W-DAINLEV              
001278         PERFORM IMS-ISRT-WDL611                                          
001279       END-PERFORM                                                        
001280     END-IF                                                               
001281     .                                                                    
001282     EJECT                                                                
001283 F-UPPD-REMAN SECTION.                                                    
001284     IF BYT02-RENOV                                                       
001285       IF BYT16-BYTES                                                     
001286         COMPUTE W-IDARTNR-WDA9 = W-IDARTNR +                             
001287                                  6000                                    
001288       ELSE                                                               
001289         COMPUTE W-IDARTNR-WDA9 = W-IDARTNR +                             
001290                                  1000                                    
001291       END-IF                                                             
001292     END-IF                                                               
001293     PERFORM IMS-GHU-WDA901                                               
001294                                                                          
001295     IF SEGMENT-SAKNAS                                                    
001296       PERFORM IMS-GU-WLARTC01-REM                                        
001297       MOVE W-IDARTNR-WDA9         TO UPB-IDARTNR                         
001298       MOVE REM-ART-IDFKNGRP       TO UPB-IDFKNGRP                        
001299       PERFORM IMS-ISRT-WDA901                                            
001300     END-IF                                                               
001301                                                                          
001302     MOVE MID-IDLEVNR(MID-IX)      TO W-IDLEVNR-WDB2B                     
001303     PERFORM IMS-GU-WDB201-BSEQ                                           
001304                                                                          
001305     IF SEGMENT-FINNS                                                     
001306** BARA AKTUELLT MED DISTRIKT: 99XX                                       
001307       MOVE GMT-IDDISTR              TO WS-IDDISTR-DISP                   
001308       PERFORM UNTIL WS-IDDISTR-DISP(1:2) = 99 OR SEGMENT-SAKNAS          
001309         PERFORM IMS-GN-WDB201-BSEQ                                       
001310         IF SEGMENT-FINNS                                                 
001311           MOVE GMT-IDDISTR          TO WS-IDDISTR-DISP                   
001312         END-IF                                                           
001313       END-PERFORM                                                        
001314                                                                          
001315       IF WS-IDDISTR-DISP(1:2) = 99                                       
001316                                                                          
001317         MOVE GMT-IDDISTR            TO W-IDDISTR-WDA9                    
001318         PERFORM IMS-GHU-WDA911                                           
001319                                                                          
001320         IF SEGMENT-SAKNAS                                                
001321           MOVE W-IDDISTR-WDA9       TO UPD-IDDISTR                       
001322           MOVE MID-KVANTAL (MID-IX) TO UPD-KVLS-REM                      
001323           MOVE ZERO                 TO UPD-DAREGDAT                      
001324                                        UPD-TIREGTID                      
001325           MOVE SPACE                TO UPD-IDUSER                        
001326           IF UPD-KVLS-REM < ZERO                                         
001327              MOVE ZERO TO UPD-KVLS-REM                                   
001328           END-IF                                                         
001329           PERFORM IMS-ISRT-WDA911                                        
001330         ELSE                                                             
001331           ADD MID-KVANTAL (MID-IX)  TO UPD-KVLS-REM                      
001332           IF UPD-KVLS-REM < ZERO                                         
001333              MOVE ZERO TO UPD-KVLS-REM                                   
001334           END-IF                                                         
001335           PERFORM IMS-REPL-WDA911                                        
001336         END-IF                                                           
001337       END-IF                                                             
001338     END-IF                                                               
001339     .                                                                    
001340     EJECT                                                                
001341 S01-SKAPA-ZZAC01 SECTION.                                                
001342                                                                          
001343     ACCEPT ZZAC01-TIAAMMDD      FROM DATE                                
001344     ACCEPT ZZAC01-TIKLOCK       FROM TIME                                
001345                                                                          
001346     ADD +1                      TO WS-IDLOGLOP                           
001347     MOVE WS-IDLOGLOP            TO ZZAC01-IDLOGLOP                       
001348                                                                          
001349     MOVE WS-ZZAC01-LOGGPOST     TO ZZAC01-LOGGPOST                       
001350     MOVE WS-ZZAC01-SORTPOST     TO ZZAC01-SORTPOST                       
001351                                                                          
001352     PERFORM IMS-ISRT-ZZAC01                                              
001353     PERFORM UNTIL SEGMENT-FINNS                                          
001354       IF WS-IDLOGLOP < +8                                                
001355         ADD +1 TO WS-IDLOGLOP                                            
001356         MOVE WS-IDLOGLOP   TO ZZAC01-IDLOGLOP                            
001357         PERFORM IMS-ISRT-ZZAC01                                          
001358       ELSE                                                               
001359         ACCEPT ZZAC01-TIAAMMDD      FROM DATE                            
001360         ACCEPT ZZAC01-TIKLOCK       FROM TIME                            
001361         MOVE +1 TO WS-IDLOGLOP                                           
001362         MOVE WS-IDLOGLOP   TO ZZAC01-IDLOGLOP                            
001363         PERFORM IMS-ISRT-ZZAC01                                          
001364       END-IF                                                             
001365     END-PERFORM                                                          
001366     .                                                                    
001367     EJECT                                                                
001368 S04-SKAPA-IDINLEV-IDLOPNRM SECTION.                                      
001369                                                                          
001370     IF DAT-TIAAVVD-GRP (3:3) = W-VVD                                     
001371       ADD +1                     TO W-LLLL                               
001372     ELSE                                                                 
001373       MOVE DAT-TIAAVVD-GRP (3:3) TO W-VVD                                
001374       IF NDC-CN                                                          
001375         MOVE +7001               TO W-LLLL                               
001376       ELSE                                                               
001377         IF NDC-US                                                        
001378           MOVE +6000             TO W-LLLL                               
001379         ELSE                                                             
001380           MOVE +1                TO W-LLLL                               
001381         END-IF                                                           
001382       END-IF                                                             
001383     END-IF                                                               
001384     CALL CHECK USING WS-IDLOPNRM (2:7) FLT-LGD                           
001385          VAEGNINGSTAL VAEGNTAL-LGD W-K MODUL-10-11 ALT-A-B               
001386                                                                          
001387     ACCEPT WS-TIAAMMDDTTMMSSTH-DATE FROM DATE                            
001388     ACCEPT WS-TIAAMMDDTTMMSSTH-TIME FROM TIME                            
001389     MOVE FUNCTION CURRENT-DATE (1:2) TO WS-TISEKEL                       
001390     COMPUTE WS-DAINLEV          = 9999999999999999                       
001391                                 - WS-TIAAAAMMDDTTMMSSTH                  
001392     END-COMPUTE                                                          
001393     .                                                                    
001394     EJECT                                                                
001395 Z-FINIT     SECTION.                                                     
001396     IF NDC-CN                                                            
001397       MOVE WS-IDLOPNRM TO 6018-IDLOPNRM-JP-AU                            
001398     ELSE                                                                 
001399       MOVE WS-IDLOPNRM TO 6018-IDLOPNRM                                  
001400     END-IF                                                               
001401*                                                                         
001402     PERFORM IMS-REPL-W6LOPA                                              
001403     MOVE INF-UPPDATE-DONE   TO MSG-KOM-IDMFSMED                          
001404                                                                          
001405     PERFORM IMS-ISRT-DISP-MSG                                            
001406     .                                                                    
001407     EJECT                                                                
001408* --- IMS SEKTIONER ---                                                   
001409     SKIP3                                                                
001410 IMS-GET-MSG SECTION.                                                     
001411                                                                          
001412     MOVE '  QC' TO GODK-STATUSKODER                                      
001413     CALL CBLTDLI USING GU MSG-PCB MSG-IO-AREA                            
001414     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
001415     PERFORM IMS-STATUSKONTROLL                                           
001416     .                                                                    
001417     SKIP3                                                                
001418 IMS-GN-MSG SECTION.                                                      
001419                                                                          
001420     MOVE '  ' TO GODK-STATUSKODER                                        
001421     CALL CBLTDLI USING GN MSG-PCB MSG-KOM-WMSGKOM                        
001422     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
001423     PERFORM IMS-STATUSKONTROLL                                           
001424     .                                                                    
001425     SKIP3                                                                
001426 IMS-ISRT-DISP-MSG   SECTION.                                             
001427                                                                          
001428     MOVE '  ' TO GODK-STATUSKODER                                        
001429     CALL CBLTDLI USING ISRT DISP-PCB MSG-KOM-WMSGKOM                     
001430     MOVE DISP-STATUS-CODE TO STATUS-WS                                   
001431     PERFORM IMS-STATUSKONTROLL                                           
001432     .                                                                    
001433     EJECT                                                                
001434                                                                          
001435 IMS-GU-WDK711-K7 SECTION.                                                
001436     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
001437          DELIMITED BY SIZE INTO SSA1                                     
001438     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
001439          DELIMITED BY SIZE INTO SSA2                                     
001440     MOVE '  GE' TO GODK-STATUSKODER                                      
001441     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-WDK711   SSA1 SSA2             
001442     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
001443     PERFORM IMS-STATUSKONTROLL                                           
001444     .                                                                    
001445     EJECT                                                                
001446                                                                          
001447 IMS-GNP-WDK724-FIRST SECTION.                                            
001448     MOVE 'WDK724  *F' TO SSA1                                            
001449     MOVE '  GE' TO GODK-STATUSKODER                                      
001450     CALL CBLTDLI USING GNP WDK7-PCB DLI-IO-WDK724 SSA1                   
001451     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
001452     PERFORM IMS-STATUSKONTROLL                                           
001453     .                                                                    
001454     SKIP3                                                                
001455                                                                          
001456 IMS-GNP-WDK724-NEXT SECTION.                                             
001457     MOVE 'WDK724   ' TO SSA1                                             
001458     MOVE '  GE' TO GODK-STATUSKODER                                      
001459     CALL CBLTDLI USING GNP WDK7-PCB DLI-IO-WDK724 SSA1                   
001460     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
001461     PERFORM IMS-STATUSKONTROLL                                           
001462     .                                                                    
001463     EJECT                                                                
001464                                                                          
001465 IMS-GU-WLARTC01-REM SECTION.                                             
001466                                                                          
001467     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-WDA9-X ')'                    
001468          DELIMITED BY SIZE INTO SSA1                                     
001469     MOVE '  ' TO GODK-STATUSKODER                                        
001470     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-WDK601 SSA1                    
001471     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
001472     PERFORM IMS-STATUSKONTROLL                                           
001473     .                                                                    
001474     SKIP3                                                                
001475 IMS-GU-WLARTC01     SECTION.                                             
001476                                                                          
001477     STRING 'WLARTC01(IDARTNR  =' W-IDARTNR-X ')'                         
001478          DELIMITED BY SIZE INTO SSA1                                     
001479     MOVE '  ' TO GODK-STATUSKODER                                        
001480     CALL CBLTDLI USING GU ARTC-PCB DLI-IO-AREA SSA1                      
001481     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
001482     PERFORM IMS-STATUSKONTROLL                                           
001483     .                                                                    
001484     SKIP3                                                                
001485 IMS-GNP-WLARTC11   SECTION.                                              
001486                                                                          
001487     MOVE 'WLARTC11 ' TO SSA1                                             
001488     MOVE '  ' TO GODK-STATUSKODER                                        
001489     CALL CBLTDLI USING GNP ARTC-PCB DLI-IO-AREA4 SSA1                    
001490     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
001491     PERFORM IMS-STATUSKONTROLL                                           
001492     .                                                                    
001493     SKIP3                                                                
001494 IMS-GHNP-WLARTC11   SECTION.                                             
001495                                                                          
001496     MOVE 'WLARTC11 ' TO SSA1                                             
001497     MOVE '  ' TO GODK-STATUSKODER                                        
001498     CALL CBLTDLI USING GHNP ARTC-PCB DLI-IO-AREA4 SSA1                   
001499     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
001500     PERFORM IMS-STATUSKONTROLL                                           
001501     .                                                                    
001502     SKIP3                                                                
001503 IMS-REPL-WLARTC11   SECTION.                                             
001504                                                                          
001505     MOVE '  ' TO GODK-STATUSKODER                                        
001506     CALL CBLTDLI USING REPL ARTC-PCB DLI-IO-AREA4 SSA1                   
001507     MOVE ARTC-STATUS-CODE TO STATUS-WS                                   
001508     PERFORM IMS-STATUSKONTROLL                                           
001509     .                                                                    
001510     EJECT                                                                
001511 IMS-GU-WDK711 SECTION.                                                   
001512     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
001513       DELIMITED BY SIZE INTO SSA1                                        
001514     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
001515       DELIMITED BY SIZE INTO SSA2                                        
001516     MOVE '  '                 TO GODK-STATUSKODER                        
001517     CALL CBLTDLI USING GU  WDK7-PCB DLI-IO-WDK711 SSA1 SSA2              
001518     MOVE WDK7-STATUS-CODE       TO STATUS-WS                             
001519     PERFORM IMS-STATUSKONTROLL                                           
001520     .                                                                    
001521 IMS-GHU-WDK711 SECTION.                                                  
001522     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
001523       DELIMITED BY SIZE INTO SSA1                                        
001524     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
001525       DELIMITED BY SIZE INTO SSA2                                        
001526     MOVE '  '                 TO GODK-STATUSKODER                        
001527     CALL CBLTDLI USING GHU WDK7-PCB DLI-IO-WDK711 SSA1 SSA2              
001528     MOVE WDK7-STATUS-CODE       TO STATUS-WS                             
001529     PERFORM IMS-STATUSKONTROLL                                           
001530     .                                                                    
001531 IMS-REPL-WDK711   SECTION.                                               
001532     MOVE '  ' TO GODK-STATUSKODER                                        
001533     CALL CBLTDLI USING REPL WDK7-PCB DLI-IO-WDK711                       
001534     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
001535     PERFORM IMS-STATUSKONTROLL                                           
001536     .                                                                    
001537     EJECT                                                                
001538 IMS-GU-WLINLE01   SECTION.                                               
001539                                                                          
001540     STRING 'WLINLE01(IDARTNR  =' W-IDARTNR-X ')'                         
001541          DELIMITED BY SIZE INTO SSA1                                     
001542     MOVE '  GE' TO GODK-STATUSKODER                                      
001543     CALL CBLTDLI USING GU   INLE-PCB DLI-IO-AREA SSA1                    
001544     MOVE INLE-STATUS-CODE TO STATUS-WS                                   
001545     PERFORM IMS-STATUSKONTROLL                                           
001546     .                                                                    
001547     SKIP3                                                                
001548 IMS-ISRT-WLINLE01 SECTION.                                               
001549                                                                          
001550     MOVE 'WLINLE01 ' TO SSA1                                             
001551     MOVE '  ' TO GODK-STATUSKODER                                        
001552     CALL CBLTDLI USING ISRT INLE-PCB DLI-IO-AREA SSA1                    
001553     MOVE INLE-STATUS-CODE TO STATUS-WS                                   
001554     PERFORM IMS-STATUSKONTROLL                                           
001555     .                                                                    
001556     SKIP3                                                                
001557 IMS-ISRT-WLINLE11 SECTION.                                               
001558                                                                          
001559     STRING 'WLINLE01(IDARTNR  =' W-IDARTNR-X ')'                         
001560          DELIMITED BY SIZE INTO SSA1                                     
001561     MOVE 'WLINLE11 ' TO SSA2                                             
001562     MOVE '  II' TO GODK-STATUSKODER                                      
001563     CALL CBLTDLI USING ISRT INLE-PCB DLI-IO-AREA SSA1 SSA2               
001564     MOVE INLE-STATUS-CODE TO STATUS-WS                                   
001565     PERFORM IMS-STATUSKONTROLL                                           
001566     .                                                                    
001567     SKIP3                                                                
001568 IMS-ISRT-WLINLE23 SECTION.                                               
001569                                                                          
001570     STRING 'WLINLE01(IDARTNR  =' W-IDARTNR-X ')'                         
001571          DELIMITED BY SIZE INTO SSA1                                     
001572     STRING 'WLINLE11(DAINLEV  =' W-DAINLEV-X ')'                         
001573          DELIMITED BY SIZE INTO SSA2                                     
001574     MOVE 'WLINLE23 ' TO SSA3                                             
001575     MOVE '  ' TO GODK-STATUSKODER                                        
001576     CALL CBLTDLI USING ISRT INLE-PCB DLI-IO-AREA SSA1 SSA2 SSA3          
001577     MOVE INLE-STATUS-CODE TO STATUS-WS                                   
001578     PERFORM IMS-STATUSKONTROLL                                           
001579     .                                                                    
001580     EJECT                                                                
001581 IMS-GU-WDL601    SECTION.                                                
001582     STRING 'WDL601  (IDARTNR  =' W-IDARTNR-X ')'                         
001583          DELIMITED BY SIZE INTO SSA1                                     
001584     MOVE '  GE'           TO GODK-STATUSKODER                            
001585     CALL CBLTDLI          USING GU WDL6-PCB DLI-IO-WDL601 SSA1           
001586     MOVE WDL6-STATUS-CODE TO STATUS-WS                                   
001587     PERFORM IMS-STATUSKONTROLL                                           
001588     .                                                                    
001589     EJECT                                                                
001590 IMS-ISRT-WDL601 SECTION.                                                 
001591     MOVE 'WDL601   ' TO SSA1                                             
001592     MOVE '  ' TO GODK-STATUSKODER                                        
001593     CALL CBLTDLI USING ISRT WDL6-PCB DLI-IO-WDL601 SSA1                  
001594     MOVE WDL6-STATUS-CODE TO STATUS-WS                                   
001595     PERFORM IMS-STATUSKONTROLL                                           
001596     .                                                                    
001597     SKIP3                                                                
001598 IMS-ISRT-WDL611 SECTION.                                                 
001599     STRING 'WDL601  (IDARTNR  =' W-IDARTNR-X ')'                         
001600          DELIMITED BY SIZE INTO SSA1                                     
001601     MOVE 'WDL611   ' TO SSA2                                             
001602     MOVE '  II' TO GODK-STATUSKODER                                      
001603     CALL CBLTDLI USING ISRT WDL6-PCB DLI-IO-WDL611 SSA1 SSA2             
001604     MOVE WDL6-STATUS-CODE TO STATUS-WS                                   
001605     PERFORM IMS-STATUSKONTROLL                                           
001606     .                                                                    
001607     SKIP3                                                                
001608 IMS-ISRT-ZZAC01 SECTION.                                                 
001609                                                                          
001610     MOVE 'WLZZAC01 ' TO SSA1                                             
001611     MOVE '  II' TO GODK-STATUSKODER                                      
001612     CALL CBLTDLI USING ISRT ZZAC-PCB DLI-IO-AREA SSA1                    
001613     MOVE ZZAC-STATUS-CODE TO STATUS-WS                                   
001614     PERFORM IMS-STATUSKONTROLL                                           
001615     .                                                                    
001616     EJECT                                                                
001617 IMS-GHU-INLB11 SECTION.                                                  
001618     STRING 'WLINLB01(WDD901KY =' W-WDD901KY-X ')'                        
001619          DELIMITED BY SIZE INTO SSA1                                     
001620     STRING 'WLINLB11(IDLEVNR  =' W-IDLEVNR-X ')'                         
001621          DELIMITED BY SIZE INTO SSA2                                     
001622     MOVE '  ' TO GODK-STATUSKODER                                        
001623     CALL CBLTDLI USING GHU INLB-PCB DLI-IO-AREA SSA1 SSA2                
001624     MOVE INLB-STATUS-CODE TO STATUS-WS                                   
001625     PERFORM IMS-STATUSKONTROLL                                           
001626     .                                                                    
001627     SKIP3                                                                
001628 IMS-REPL-INLB SECTION.                                                   
001629     MOVE '  ' TO GODK-STATUSKODER                                        
001630     CALL CBLTDLI USING REPL INLB-PCB DLI-IO-AREA                         
001631     MOVE INLB-STATUS-CODE TO STATUS-WS                                   
001632     PERFORM IMS-STATUSKONTROLL                                           
001633     .                                                                    
001634     SKIP3                                                                
001635 IMS-GHU-W6LOPA11 SECTION.                                                
001636     STRING 'W6LOPA01(W6GXKEY  =' W-6017KEY-X ')'                         
001637          DELIMITED BY SIZE INTO SSA1                                     
001638     MOVE 'W6LOPA11 ' TO SSA2                                             
001639     MOVE '  ' TO GODK-STATUSKODER                                        
001640     CALL CBLTDLI USING GHU LOPA-PCB DLI-IO-AREA3 SSA1 SSA2               
001641     MOVE LOPA-STATUS-CODE TO STATUS-WS                                   
001642     PERFORM IMS-STATUSKONTROLL                                           
001643     .                                                                    
001644     EJECT                                                                
001645 IMS-REPL-W6LOPA SECTION.                                                 
001646     MOVE '  ' TO GODK-STATUSKODER                                        
001647     CALL CBLTDLI USING REPL LOPA-PCB DLI-IO-AREA3                        
001648     MOVE LOPA-STATUS-CODE TO STATUS-WS                                   
001649     PERFORM IMS-STATUSKONTROLL                                           
001650     .                                                                    
001651     EJECT                                                                
001652 IMS-ISRT-WDL9   SECTION.                                                 
001653     MOVE 'WLLOGA01 ' TO SSA1                                             
001654     MOVE '  II' TO GODK-STATUSKODER                                      
001655     CALL CBLTDLI USING ISRT LOGA-PCB WLLOGA01 SSA1                       
001656     MOVE LOGA-STATUS-CODE TO STATUS-WS                                   
001657     PERFORM IMS-STATUSKONTROLL                                           
001658     .                                                                    
001659     SKIP3                                                                
001660 IMS-ISRT-WDR801   SECTION.                                               
001661     MOVE 'WDR801   ' TO SSA1                                             
001662     MOVE '  II' TO GODK-STATUSKODER                                      
001663     CALL CBLTDLI USING ISRT WDR8-PCB DLI-IO-WDR801 SSA1                  
001664     MOVE WDR8-STATUS-CODE TO STATUS-WS                                   
001665     PERFORM IMS-STATUSKONTROLL                                           
001666     .                                                                    
001667     EJECT                                                                
001668 IMS-ISRT-WLSAPA01 SECTION.                                               
001669     MOVE 'WLSAPA01 ' TO SSA1                                             
001670     MOVE '  II' TO GODK-STATUSKODER                                      
001671     CALL CBLTDLI USING ISRT SAPA-PCB WLSAPA01 SSA1                       
001672     MOVE SAPA-STATUS-CODE TO STATUS-WS                                   
001673     PERFORM IMS-STATUSKONTROLL                                           
001674     .                                                                    
001675     EJECT                                                                
001676                                                                          
001677 IMS-GHU-WDA901 SECTION.                                                  
001678     STRING 'WDA901  (IDARTNR  =' W-IDARTNR-WDA9-X ')'                    
001679          DELIMITED BY SIZE INTO SSA1                                     
001680     MOVE '  GE'           TO GODK-STATUSKODER                            
001681     CALL CBLTDLI USING GHU WDA9-PCB DLI-IO-WDA901 SSA1                   
001682     MOVE WDA9-STATUS-CODE TO STATUS-WS                                   
001683     PERFORM IMS-STATUSKONTROLL                                           
001684     .                                                                    
001685                                                                          
001686 IMS-GHU-WDA911 SECTION.                                                  
001687     STRING 'WDA901  (IDARTNR  =' W-IDARTNR-WDA9-X ')'                    
001688          DELIMITED BY SIZE INTO SSA1                                     
001689     STRING 'WDA911  (IDDISTR  =' W-IDDISTR-WDA9-X ')'                    
001690          DELIMITED BY SIZE INTO SSA2                                     
001691     MOVE '  GE'           TO GODK-STATUSKODER                            
001692     CALL CBLTDLI USING GHU WDA9-PCB DLI-IO-WDA911 SSA1                   
001693                                                   SSA2                   
001694     MOVE WDA9-STATUS-CODE TO STATUS-WS                                   
001695     PERFORM IMS-STATUSKONTROLL                                           
001696     .                                                                    
001697                                                                          
001698 IMS-ISRT-WDA901 SECTION.                                                 
001699     MOVE 'WDA901   '      TO SSA1                                        
001700     MOVE '  '             TO GODK-STATUSKODER                            
001701     CALL CBLTDLI USING ISRT WDA9-PCB DLI-IO-WDA901 SSA1                  
001702     MOVE WDA9-STATUS-CODE TO STATUS-WS                                   
001703     PERFORM IMS-STATUSKONTROLL                                           
001704     .                                                                    
001705                                                                          
001706 IMS-ISRT-WDA911 SECTION.                                                 
001707     MOVE 'WDA911   '      TO SSA1                                        
001708     MOVE '  '             TO GODK-STATUSKODER                            
001709     CALL CBLTDLI USING ISRT WDA9-PCB DLI-IO-WDA911 SSA1                  
001710     MOVE WDA9-STATUS-CODE TO STATUS-WS                                   
001711     PERFORM IMS-STATUSKONTROLL                                           
001712     .                                                                    
001713                                                                          
001714 IMS-REPL-WDA911 SECTION.                                                 
001715     MOVE '  '             TO GODK-STATUSKODER                            
001716     CALL CBLTDLI USING REPL WDA9-PCB DLI-IO-WDA911                       
001717     MOVE WDA9-STATUS-CODE TO STATUS-WS                                   
001718     PERFORM IMS-STATUSKONTROLL                                           
001719     .                                                                    
001720     EJECT                                                                
001721                                                                          
001722 IMS-GU-WDB201-BSEQ SECTION.                                              
001723     STRING 'WDB201  (WDB2BSEQ =' W-WDB2BSEQ-X ')'                        
001724          DELIMITED BY SIZE INTO SSA1                                     
001725     MOVE '  GE'            TO GODK-STATUSKODER                           
001726     CALL CBLTDLI USING GU WDB2B-PCB DLI-IO-WDB201 SSA1                   
001727     MOVE WDB2B-STATUS-CODE TO STATUS-WS                                  
001728     PERFORM IMS-STATUSKONTROLL                                           
001729     .                                                                    
001730     EJECT                                                                
001731                                                                          
001732 IMS-GN-WDB201-BSEQ SECTION.                                              
001733     STRING 'WDB201  (WDB2BSEQ =' W-WDB2BSEQ-X ')'                        
001734          DELIMITED BY SIZE INTO SSA1                                     
001735     MOVE '  GE'            TO GODK-STATUSKODER                           
001736     CALL CBLTDLI USING GN WDB2B-PCB DLI-IO-WDB201 SSA1                   
001737     MOVE WDB2B-STATUS-CODE TO STATUS-WS                                  
001738     PERFORM IMS-STATUSKONTROLL                                           
001739     .                                                                    
001740     EJECT                                                                
001741                                                                          
001742 IMS-GU-WDB601 SECTION.                                                   
001743     STRING 'WDB601  (IDDC     =' W-IDDC-X ')'                            
001744          DELIMITED BY SIZE INTO SSA1                                     
001745     MOVE '  GE'            TO GODK-STATUSKODER                           
001746     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-WDB601 SSA1                    
001747     MOVE WDB6-STATUS-CODE TO STATUS-WS                                   
001748     PERFORM IMS-STATUSKONTROLL                                           
001749     .                                                                    
001750     EJECT                                                                
001751                                                                          
001752 IMS-GNP-WDB617    SECTION.                                               
001753     MOVE 'WDB617   ' TO SSA1                                             
001754     MOVE '  GE' TO GODK-STATUSKODER                                      
001755     CALL CBLTDLI USING GNP WDB6-PCB DLI-IO-AREA-B617 SSA1                
001756     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
001757     PERFORM IMS-STATUSKONTROLL                                           
001758     .                                                                    
001759                                                                          
001760 IMS-GU-W6H712 SECTION.                                                   
001761     STRING 'W6H701  (IDKR     =' W-IDKR-X ')'                            
001762          DELIMITED BY SIZE INTO SSA1                                     
001763     MOVE 'W6H712  '      TO SSA2                                         
001764     MOVE '  GE' TO GODK-STATUSKODER                                      
001765     CALL CBLTDLI USING GU W6H7-PCB DLI-IO-AREA2 SSA1 SSA2                
001766     MOVE W6H7-STATUS-CODE TO STATUS-WS                                   
001767     PERFORM IMS-STATUSKONTROLL                                           
001768     .                                                                    
001769     SKIP2                                                                
001770                                                                          
001771 IMS-GU-WLLEVA01 SECTION.                                                 
001772     STRING 'WLLEVA01(IDLEVNR  =' W-IDLEVNR-X ')'                         
001773     DELIMITED BY SIZE INTO SSA1                                          
001774     MOVE '  GE' TO GODK-STATUSKODER                                      
001775     CALL CBLTDLI USING GU LEV-PCB WLLEVA01 SSA1                          
001776     MOVE LEV-STATUS-CODE TO STATUS-WS                                    
001777     PERFORM IMS-STATUSKONTROLL                                           
001778     .                                                                    
001779     SKIP3                                                                
001780                                                                          
001781 IMS-GNP-WLLEVA11 SECTION.                                                
001782     STRING 'WLLEVA11(IDLAND   =' W-IDLAND-X ')'                          
001783     DELIMITED BY SIZE INTO SSA1                                          
001784     MOVE '  GE' TO GODK-STATUSKODER                                      
001785     CALL CBLTDLI USING GNP LEV-PCB LEV-WLLEVA11 SSA1                     
001786     MOVE LEV-STATUS-CODE TO STATUS-WS                                    
001787     PERFORM IMS-STATUSKONTROLL                                           
001788     .                                                                    
001789     EJECT                                                                
001790                                                                          
001791 IMS-GU-WDGX9306 SECTION.                                                 
001792     STRING 'WDG201  (WDGXKEY  =' W-WDGX9305-X ')'                        
001793             DELIMITED BY SIZE INTO SSA1                                  
001794     STRING 'WDGX9306(KDVALISO =' W-KDVALISO-X ')'                        
001795             DELIMITED BY SIZE INTO SSA2                                  
001796     MOVE '  GE'   TO GODK-STATUSKODER                                    
001797     CALL CBLTDLI USING GU 9305-PCB DLI-IO-WDGX9306 SSA1 SSA2             
001798     MOVE 9305-STATUS-CODE       TO STATUS-WS                             
001799     PERFORM IMS-STATUSKONTROLL                                           
001800     .                                                                    
001801     SKIP3                                                                
001802                                                                          
001803 IMS-GNP-WDGX9308 SECTION.                                                
001804     STRING 'WDGX9308(TISTADA9 =' W-TISTADA9-X ')'                        
001805             DELIMITED BY SIZE INTO SSA1                                  
001806     MOVE '  GE'   TO GODK-STATUSKODER                                    
001807     CALL CBLTDLI USING GNP 9305-PCB DLI-IO-WDGX9308 SSA1                 
001808     MOVE 9305-STATUS-CODE       TO STATUS-WS                             
001809     PERFORM IMS-STATUSKONTROLL                                           
001810     .                                                                    
001811     SKIP3                                                                
001812                                                                          
001813 IMS-GNP-WDGX9308-FIRST SECTION.                                          
001814     MOVE 'WDGX9308*F' TO SSA1                                            
001815     MOVE '  GE'   TO GODK-STATUSKODER                                    
001816     CALL CBLTDLI USING GNP 9305-PCB DLI-IO-WDGX9308 SSA1                 
001817     MOVE 9305-STATUS-CODE       TO STATUS-WS                             
001818     PERFORM IMS-STATUSKONTROLL                                           
001819     .                                                                    
001820     SKIP3                                                                
001821                                                                          
001822 IMS-STATUSKONTROLL SECTION.                                              
001823                                                                          
001824     SET STATUS-IX TO 1                                                   
001825     SEARCH GODK-STATUS                                                   
001826       AT END                                                             
001827         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
001828         DELIMITED BY SIZE INTO FELTEXT                                   
001829         CALL FELLOG                                                      
001830       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
001831         CONTINUE                                                         
001832     END-SEARCH                                                           
001840     .                                                                    
