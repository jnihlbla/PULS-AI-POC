000001*COMPOPT STDSUB=YES                                                       
000002 ID DIVISION.                                                             
000003 PROGRAM-ID.     W411DLEV.                                                
000004 AUTHOR.         LASSI OLGRENER.                                          
000005 DATE-WRITTEN.   MARS -90.                                                
000006 DATE-COMPILED.                                                           
000007                                                                          
000008*    PROGRAMMET ÄR EN SUBMODUL TILL ETT MPP-PGM                           
000009*                                                                         
000010*    FUNKTION.                                                            
000011*      1 KONTROLL OM ORDERRAD SKA LEVERERAS IFRÅN VOLVO PARTS             
000012*        ELLER IFRÅN DIREKTLEVERANTÖR.                                    
000013*      . REDIRLEV I ARTREG STYR HUR EN ARTIKEL LEVERERAS:                 
000014*               0.00 = LEVERERAS IFRÅN PARTS                              
000015*               1.00 = LEVERERAS IFRÅN DIREKTLEVERERANTÖR                 
000016*          0.01-0.99 = LEVERERAS IFRÅN PARTS ELLER DIRLEVERANTÖR          
000017*                      BEROENDE PÅ BESTÄLLD KVANTITET(KVBEART-MIN)        
000018*                                                                         
000019*      2 TILLDELNING AV DC-TILLHÖRIGHET                                   
000020*                       ORDERSTATUS                                       
000021*                       BER. SKEPPNINGSDATUM                              
000022*                       TRANSPORTSKILLNAD MOT CDC (ANT. DAGAR)            
000023*                       TRANSPORTVÄG                                      
000024*                                                                         
000025*        PROGRAMMET LÄSER   WLLEVG (WDF2,ASEQ)DIREKTLEVERANSREG           
000026*        PROGRAMMET LÄSER   WLLEVF (WDF2)     DIREKTLEVERANSREG           
000027*        PROGRAMMET LÄSER   WLLEVA (WDF1)     LEVERANTÖRSREG              
000028*                                                                         
000029*                                                                         
000030*        LÄNKAREA: W411DLEV                                               
000031                                                                          
000032                                                                          
000033 ENVIRONMENT DIVISION.                                                    
000034                                                                          
000035 DATA DIVISION.                                                           
000036     EJECT                                                                
000037 WORKING-STORAGE SECTION.                                                 
000038                                                                          
000039*    -COPY WY2000W1                                                       
000040     SKIP3                                                                
000041 77  IDPGM                       PIC X(08)   VALUE 'W411DLEV'.            
000042 77  JA                          PIC X       VALUE 'J'.                   
000043 77  NEJ                         PIC X       VALUE 'N'.                   
000044 77  SPEC-FORBI                  PIC X       VALUE 'S'.                   
000045 77  PASSIV                      PIC X       VALUE 'P'.                   
000046 77  INDX-KL                     PIC S9(9)   VALUE +0  COMP SYNC.         
000047 77  IX                          PIC S9(9)  VALUE ZERO  COMP SYNC.        
000048 77  IX-DCCLEAR-MAX              PIC S9(9)  VALUE +99   COMP SYNC.        
000049 77  W-DISP                      PIC S9(8)   VALUE ZERO.                  
000050 77  W-KVAKS-SDC                 PIC 9(7)    VALUE ZERO.                  
000051 77  W-KVOKS-DAG                 PIC 9(7)    VALUE ZERO.                  
000052 77  W-KVOKS-BULK                PIC 9(7)    VALUE ZERO.                  
000053                                                                          
000054 01  GENERELLA-SUBPROGRAM.                                                
000055     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
000056     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
000057     03  WORKDAY                 PIC X(8)    VALUE 'WORKDAY '.            
000058     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
000059     EJECT                                                                
000060                                                                          
000061*      --- VALID IDDC CODES                                               
000062*01    -COPY WWDC99                                                       
000063*01    -COPY WWDCKONS                                                     
000064       EJECT                                                              
000065*      --- VALID IDDC CODES ALT2                                          
000066*01    -COPY WWDC99 -PRE ALT2-                                            
000067       EJECT                                                              
000068*      --- VALID IDDC CODES ALT3                                          
000069*01    -COPY WWDC99 -PRE ALT3-                                            
000070       EJECT                                                              
000071                                                                          
000072 01  FILLER                      PIC X(24) VALUE 'WORKDAY-START '.        
000073*01  -COPY WORKAREA                                                       
000074     EJECT                                                                
000075                                                                          
000076*    --- PARAMETRAR TILL ABEND                                            
000077                                                                          
000078 01  ERROR-TEXT                  PIC X(80).                               
000079 01  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   VALUE +16 COMP SYNC.         
000080 01  RKOD-ABEND-MED-DUMP         PIC S9(4)   VALUE +33 COMP SYNC.         
000081     EJECT                                                                
000082                                                                          
000083*    --- GENERELLA ARBETS-AREOR                                           
000084 01  DAGENS-DATUM                PIC 9(8).                                
000085 01  DAGENS-DAT                  PIC 9(6).                                
000086 01  W-DAG                       PIC S9(7)  COMP-3.                       
000087 01  W-DAG-KONS                  PIC S9(7)  VALUE 090101 COMP-3.          
000088 01  W-DAG-KONS2                 PIC S9(7)  VALUE 300101 COMP-3.          
000089 01  DAGENS-KLOCKA               PIC 9(8).                                
000090 01  WS-IDLEVNR                  PIC X(5).                                
000091 01  WS-STEERING-DC              PIC X(2).                                
000092     EJECT                                                                
000093                                                                          
000094*    --- ARBETSFÄLT FÖR AKTUELLA NYCKELVÄRDEN FRÅN SKÄRMEN                
000095                                                                          
000096 77  TRAEFF-SW                   PIC X       VALUE 'J'.                   
000097     88  TRAEFF-OK                           VALUE 'J'.                   
000098     88  EJ-TRAEFF                           VALUE 'N'.                   
000099                                                                          
000100 77  W-SDCLEV-SW                 PIC X       VALUE 'J'.                   
000101     88  SDCLEV-OK                           VALUE 'J'.                   
000102     88  EJ-SDCLEV                           VALUE 'N'.                   
000103                                                                          
000104 77  W-DELEV-FROM-DC             PIC X       VALUE 'N'.                   
000105     88  DELEV-DC-OK                         VALUE 'J'.                   
000106     88  EJ-DELEV-DC                         VALUE 'N'.                   
000107                                                                          
000108 77  W-HELG-SW                   PIC X       VALUE 'N'.                   
000109     88  HELG                                VALUE 'J'.                   
000110     88  EJ-HELG                             VALUE 'N'.                   
000111                                                                          
000112 77  W-DDGS-SW                   PIC X       VALUE 'N'.                   
000113     88  DDGS-OK                             VALUE 'J'.                   
000114     88  DDGS-NOT-OK                         VALUE 'N'.                   
000115                                                                          
000116 77  W-DIR-IDDC-SW               PIC X       VALUE 'N'.                   
000117     88  DIR-IDDC-OK                         VALUE 'J'.                   
000118     88  DIR-IDDC-NOT-OK                     VALUE 'N'.                   
000119                                                                          
000120*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
000121*                                                                         
000122 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
000123     SKIP3                                                                
000124*    --- STATUS-KOD FRÅN IMS                                              
000125 01  STATUS-WS                   PIC XX.                                  
000126     88  SEGMENT-FINNS                       VALUE '  '.                  
000127     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
000128     88  BAS-SLUT                            VALUE 'GB'.                  
000129     SKIP2                                                                
000130 01  GODK-STATUSKODER.                                                    
000131     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
000132     SKIP3                                                                
000133 01  SSA1                        PIC X(96).                               
000134 01  SSA2                        PIC X(96).                               
000135     EJECT                                                                
000136*    --- IMS FUNKTIONSKODER                                               
000137*01  -COPY W0003                                                          
000138     EJECT                                                                
000139 01  NYCKLAR-TILL-DLI.                                                    
000140     03  W-WDF2A1KY-MIN-X.                                                
000141         05  W-IDARTNR-MIN       PIC S9(9)   VALUE ZERO COMP-3.           
000142         05  W-IDLEVNR-MIN       PIC  X(5)   VALUE SPACE.                 
000143                                                                          
000144     03  W-WDF2A1KY-MAX-X.                                                
000145         05  W-IDARTNR-MAX       PIC S9(9)   VALUE ZERO COMP-3.           
000146         05  W-IDLEVNR-MAX       PIC  X(5)   VALUE SPACE.                 
000147                                                                          
000148     03  W-DASTADAT-X.                                                    
000149         05  W-DASTADAT          PIC 9(8)    VALUE ZERO.                  
000150                                                                          
000151     03  W-WDF201KY-X.                                                    
000152         05  W-IDLEVNR-WDF2      PIC  X(5)   VALUE SPACE.                 
000153         05  W-IDDIRGRP-WDF2     PIC X(10)   VALUE SPACE.                 
000154                                                                          
000155     03  W-IDDISTR-X.                                                     
000156         05  W-IDDISTR           PIC S9(5)   VALUE ZERO COMP-3.           
000157                                                                          
000158     03  W-IDKUNDNR-FOM-X.                                                
000159         05  W-IDKUNDNR-FOM      PIC S9(7)   VALUE ZERO COMP-3.           
000160     03  W-IDKUNDNR-TOM-X.                                                
000161         05  W-IDKUNDNR-TOM      PIC S9(7)   VALUE ZERO COMP-3.           
000162                                                                          
000163     03  W-WDF101KY-X.                                                    
000164         05  W-IDLEVNR-WDF1      PIC  X(5)   VALUE SPACE.                 
000165                                                                          
000166     03  W-WDF118KY-X.                                                    
000167         05  W-IDDISTR-WDF1      PIC S9(5)   VALUE ZERO COMP-3.           
000168         05  W-IDKUNDNR-WDF1     PIC S9(7)   VALUE ZERO COMP-3.           
000169         05  W-KDORDKL-WDF1      PIC S9(1)   VALUE ZERO COMP-3.           
000170                                                                          
000171     03  W-IDARTNR-X.                                                     
000172         05  W-IDARTNR           PIC S9(9)   VALUE +0  COMP-3.            
000173                                                                          
000174     03  W-IDDC-X.                                                        
000175         05 W-IDDC               PIC  X(2)   VALUE SPACE.                 
000176                                                                          
000177     03  W-IDDC-B6-X.                                                     
000178         05 W-IDDC-B6                  PIC X(2).                          
000179                                                                          
000180     EJECT                                                                
000181                                                                          
000182*    ---  DLI INPUT-OUTPUT AREA                                           
000183 01  FILLER         PIC X(16) VALUE 'DLI-IO-WLLEVG01'.                    
000184 01  DLI-IO-WLLEVG01.                                                     
000185*    03  -COPY WDF2A1                                                     
000186 01  FILLER         PIC X(16) VALUE 'DLI-IO-WLLEVF01'.                    
000187 01  DLI-IO-WLLEVF01.                                                     
000188*    03  -COPY WDF201                                                     
000189     EJECT                                                                
000190 01  FILLER         PIC X(16) VALUE 'DLI-IO-WLLEVF11'.                    
000191 01  DLI-IO-WLLEVF11.                                                     
000192*    03  -COPY WDF211                                                     
000193     EJECT                                                                
000194 01  FILLER         PIC X(16) VALUE 'DLI-IO-WLLEVA01'.                    
000195 01  DLI-IO-WLLEVA01.                                                     
000196*    03  -COPY WDF101 -PRE WDF1-                                          
000197     EJECT                                                                
000198 01  FILLER         PIC X(16) VALUE 'DLI-IO-WLLEVA18'.                    
000199 01  DLI-IO-WLLEVA18.                                                     
000200*    03  -COPY WDF118                                                     
000201     EJECT                                                                
000202 01  FILLER         PIC X(16) VALUE 'DLI-IO-WLARTS'.                      
000203     SKIP3                                                                
000204 01  DLI-IO-WLARTS.                                                       
000205*    03  -COPY WDK711                                                     
000206                                                                          
000207 01  FILLER               PIC X(16)   VALUE 'WDB601 AREA'.                
000208 01   DLI-IO-AREA-B601.                                                   
000209*     03  -COPY WDB601                                                    
000210                                                                          
000211 01  FILLER               PIC X(16)   VALUE 'WDR601 AREA'.                
000212 01  DLI-IO-WDR601.                                                       
000213*    03 -COPY WDR601                                                      
000214       05 FILLER REDEFINES FIL-WDR601-DATA.                               
000215*        07  -COPY W414BUMA                                               
000216 LINKAGE SECTION.                                                         
000217                                                                          
000218*01 -COPY W411DLEV                                                        
000219                                                                          
000220     EJECT                                                                
000221*01  -COPY W0008      -PRE LEVF-                                          
000222     05  FILLER                  PIC X.                                   
000223     EJECT                                                                
000224*01  -COPY W0008      -PRE LEVG-                                          
000225     05  FILLER                  PIC X.                                   
000226     EJECT                                                                
000227*01  -COPY W0008      -PRE LEVA-                                          
000228     05  FILLER                  PIC X.                                   
000229     EJECT                                                                
000230*01  -COPY W0008      -PRE ARTS-                                          
000231     05  FILLER                  PIC X.                                   
000232*01  -COPY W0008      -PRE WDB6-                                          
000233     05  FILLER                  PIC X.                                   
000234*01  -COPY W0008      -PRE FILA-                                          
000235     05  FILLER                  PIC X.                                   
000236     EJECT                                                                
000237 PROCEDURE DIVISION  USING DLEV-W411DLEV                                  
000238                           LEVF-PCB LEVG-PCB LEVA-PCB                     
000239                           ARTS-PCB WDB6-PCB FILA-PCB.                    
000240                                                                          
000241     MOVE FUNCTION CURRENT-DATE(1:8) TO DAGENS-DATUM                      
000242     ACCEPT DAGENS-DAT    FROM DATE                                       
000243     ACCEPT DAGENS-KLOCKA FROM TIME                                       
000244                                                                          
000245     MOVE DLEV-FLRESTN-IN            TO DLEV-FLRESTN-UT                   
000246     MOVE NEJ                        TO DLEV-FLSDCLEV-UT                  
000247     MOVE ZERO                       TO DLEV-KDORDBEK-UT                  
000248     MOVE SPACE                      TO DLEV-IDLEVNR-UT                   
000249     MOVE SPACE                      TO DLEV-IDDC-UT                      
000250     MOVE SPACE                      TO DLEV-KDORDSTA-UT                  
000251     MOVE ZERO                       TO DLEV-KDVIA-UT                     
000252     MOVE ZERO                       TO DLEV-KVDAGAR-DIFF-UT              
000253     MOVE ZERO                       TO DLEV-TISKEPPN-DDC-UT              
000254                                                                          
000255     IF  DLEV-REDIRLEV-IN > +0       AND                                  
000256             (DLEV-KDUART-IN = SPACE  OR                                  
000257              DLEV-IDKAMPRF-IN = 0    OR                                  
000258              DLEV-KDTPOTYP-IN = 0 OR +2)                                 
000259*                     AND                                                 
000260*             DLEV-FLFORBI-IN NOT = JA                                    
000261                      AND                                                 
000262              DLEV-FLFORBI-IN NOT = SPEC-FORBI                            
000263                                                                          
000264       MOVE DLEV-IDDC-ORD-IN         TO WS-IDDC                           
000265       MOVE DLEV-IDDC-IN             TO W-IDDC-B6                         
000266       PERFORM IMS-GU-WDB601                                              
000267       IF SEGMENT-FINNS AND                                               
000268         (DCS-SDC OR DCS-NDC)                                             
000269         MOVE 53 TO DLEV-KDORDBEK-UT                                      
000270       ELSE                                                               
000271         PERFORM A-DIREKTLEVERANS-KONTROLL                                
000272       END-IF                                                             
000273     END-IF                                                               
000274                                                                          
000275     IF (DLEV-FLFORBI-IN = JA OR NDC) AND                                 
000276        DDGS-NOT-OK                                                       
000277*    DDGS NOT ACCEPTED                                                    
000278        MOVE SPACE TO DLEV-IDLEVNR-UT                                     
000279                      DLEV-IDDC-UT                                        
000280        MOVE ZERO  TO DLEV-KDORDBEK-UT                                    
000281     END-IF                                                               
000282                                                                          
000283     IF DLEV-IDLEVNR-UT NOT = SPACE AND DLEV-IDDC-UT > SPACE              
000284        MOVE NEJ     TO DLEV-FLRESTN-UT                                   
000285        IF (DLEV-IDLEVNR-UT = '6492 '                                     
000286        OR  DLEV-IDLEVNR-UT = 'BZFFA')                                    
000287        AND DLEV-KDUART-IN = 'S'                                          
000288              MOVE 95     TO DLEV-KDORDBEK-UT                             
000289        ELSE                                                              
000290          IF DLEV-KDUART-IN NOT = SPACE OR                                
000291            (DLEV-KDTPOTYP-IN > +0 AND DLEV-KDTPOTYP-IN NOT = +2)         
000292             MOVE 21   TO DLEV-KDORDBEK-UT                                
000293             MOVE SPACE TO DLEV-IDLEVNR-UT                                
000294          ELSE                                                            
000295             MOVE DLEV-IDDC-UT  TO ALT2-WS-IDDC                           
000296             IF ALT2-GOOD-DDC                                             
000297                MOVE 95     TO DLEV-KDORDBEK-UT                           
000298             ELSE                                                         
000299                IF DLEV-KDORDKL-IN = +0 OR +1                             
000300                   MOVE 95   TO DLEV-KDORDBEK-UT                          
000301                END-IF                                                    
000302             END-IF                                                       
000303          END-IF                                                          
000304        END-IF                                                            
000305     END-IF                                                               
000306*                                                                         
000307     IF DLEV-KDCALL = 1 AND DLEV-REDIRLEV-IN > 0                          
000308       IF ((DLEV-KDORDBEK-UT NOT = 21 AND 53) AND                         
000309          (DELEV-DC-OK OR WS-STEERING-DC > SPACE))                        
000310         PERFORM S01-CREATE-WDR6-TRANS                                    
000311       END-IF                                                             
000312     ELSE                                                                 
000313       IF DLEV-KDCALL = 2 AND DLEV-REDIRLEV-IN > 0                        
000314         IF ((DLEV-KDORDBEK-UT NOT = 21 AND 53) AND                       
000315              DELEV-DC-OK)                                                
000316            PERFORM S01-CREATE-WDR6-TRANS                                 
000317         END-IF                                                           
000318       END-IF                                                             
000319     END-IF                                                               
000320*                                                                         
000321     MOVE ZERO TO RETURN-CODE                                             
000322     GOBACK                                                               
000323     .                                                                    
000324     EJECT                                                                
000325 A-DIREKTLEVERANS-KONTROLL SECTION.                                       
000326     MOVE LOW-VALUE        TO W-WDF2A1KY-MIN-X                            
000327     MOVE HIGH-VALUE       TO W-WDF2A1KY-MAX-X                            
000328     MOVE NEJ              TO TRAEFF-SW                                   
000329     MOVE NEJ              TO W-SDCLEV-SW                                 
000330     MOVE NEJ              TO W-DDGS-SW                                   
000331     MOVE NEJ              TO W-DELEV-FROM-DC                             
000332     MOVE SPACES           TO WS-STEERING-DC                              
000333     MOVE DAGENS-DATUM     TO W-DASTADAT                                  
000334     MOVE DLEV-IDARTNR-IN  TO W-IDARTNR-MIN                               
000335                              W-IDARTNR-MAX                               
000336     MOVE DLEV-IDDISTR-IN  TO W-IDDISTR                                   
000337     MOVE +0               TO W-IDKUNDNR-FOM                              
000338     MOVE +9999999         TO W-IDKUNDNR-TOM                              
000339                                                                          
000340     PERFORM IMS-GU-SEQA                                                  
000341     PERFORM UNTIL TRAEFF-OK OR SEGMENT-SAKNAS OR BAS-SLUT                
000342       MOVE SEQA-IDLEVNR  TO W-IDLEVNR-WDF2                               
000343       MOVE SEQA-IDDIRGRP TO W-IDDIRGRP-WDF2                              
000344       PERFORM IMS-GU-LEV                                                 
000345       IF SEGMENT-FINNS                                                   
000346         IF DAGENS-DATUM >= LEV-DASTADAT                                  
000347           MOVE LEV-IDLEVNR TO WS-IDLEVNR                                 
000348           PERFORM IMS-GET-DISTRIKT                                       
000349           IF SEGMENT-FINNS                                               
000350             IF DIR-IDDISTR-FOM = DIR-IDDISTR-TOM                         
000351               IF DIR-IDKUNDNR-FOM NOT > DLEV-IDKUNDNR-IN AND             
000352                  DIR-IDKUNDNR-TOM NOT < DLEV-IDKUNDNR-IN                 
000353                                                                          
000354                  PERFORM AA-BEHANDLA-DIREKTLEVERANS                      
000355               ELSE                                                       
000356                  MOVE DLEV-IDKUNDNR-IN TO W-IDKUNDNR-FOM                 
000357                                           W-IDKUNDNR-TOM                 
000358                  PERFORM IMS-GET-KUND                                    
000359                  IF SEGMENT-FINNS                                        
000360                    PERFORM AA-BEHANDLA-DIREKTLEVERANS                    
000361                  END-IF                                                  
000362               END-IF                                                     
000363             ELSE                                                         
000364               PERFORM AA-BEHANDLA-DIREKTLEVERANS                         
000365             END-IF                                                       
000366           END-IF                                                         
000367         END-IF                                                           
000368       END-IF                                                             
000369       MOVE +0               TO W-IDKUNDNR-FOM                            
000370       MOVE +9999999         TO W-IDKUNDNR-TOM                            
000371       PERFORM IMS-GN-SEQA                                                
000372     END-PERFORM                                                          
000373                                                                          
000374     IF SDCLEV-OK                                                         
000375        MOVE JA                        TO W-DELEV-FROM-DC                 
000376     ELSE                                                                 
000377        IF EJ-TRAEFF                                                      
000378          IF DLEV-REDIRLEV-IN = +1.00                                     
000379            MOVE 21                    TO DLEV-KDORDBEK-UT                
000380          END-IF                                                          
000381        ELSE                                                              
000382          MOVE NEJ                     TO TRAEFF-SW                       
000383          MOVE WS-IDLEVNR              TO W-IDLEVNR-WDF1                  
000384          PERFORM IMS-GU-WDF101                                           
000385          IF SEGMENT-FINNS                                                
000386            MOVE DLEV-KDORDKL-IN       TO W-KDORDKL-WDF1                  
000387            MOVE DLEV-IDDISTR-IN       TO W-IDDISTR-WDF1                  
000388            MOVE DLEV-IDKUNDNR-IN      TO W-IDKUNDNR-WDF1                 
000389            PERFORM IMS-GU-WDF118                                         
000390            IF SEGMENT-SAKNAS                                             
000391              MOVE 999999              TO W-IDKUNDNR-WDF1                 
000392              PERFORM IMS-GU-WDF118                                       
000393            END-IF                                                        
000394            IF SEGMENT-SAKNAS                                             
000395              MOVE 9999                TO W-IDDISTR-WDF1                  
000396              MOVE 999999              TO W-IDKUNDNR-WDF1                 
000397              PERFORM IMS-GU-WDF118                                       
000398            END-IF                                                        
000399            IF SEGMENT-FINNS                                              
000400              IF DSTY-IDDC = WC-CDC-SE                                    
000401                MOVE ZERO              TO DLEV-KDVIA-UT                   
000402                                          DLEV-TISKEPPN-DDC-UT            
000403                                          DLEV-KVDAGAR-DIFF-UT            
000404                MOVE DSTY-IDDC         TO DLEV-IDDC-UT                    
000405              ELSE                                                        
000406                MOVE DSTY-IDDC         TO DLEV-IDDC-UT                    
000407                MOVE DSTY-KDVIA        TO DLEV-KDVIA-UT                   
000408                MOVE DSTY-KVDAGAR-DIFF TO DLEV-KVDAGAR-DIFF-UT            
000409                PERFORM AB-BER-SKEPPNDAG                                  
000410              END-IF                                                      
000411              MOVE JA                  TO TRAEFF-SW                       
000412            END-IF                                                        
000413          END-IF                                                          
000414          IF EJ-TRAEFF                                                    
000415            IF DLEV-REDIRLEV-IN = +1.00                                   
000416              MOVE 21                    TO DLEV-KDORDBEK-UT              
000417            END-IF                                                        
000418          END-IF                                                          
000419        END-IF                                                            
000420     END-IF                                                               
000421     .                                                                    
000422     EJECT                                                                
000423                                                                          
000424 AA-BEHANDLA-DIREKTLEVERANS SECTION.                                      
000425                                                                          
000426     COMPUTE INDX-KL = DLEV-KDORDKL-IN + 1                                
000427     IF DLEV-KDTPOTYP-IN = +2                                             
000428        MOVE SPACE              TO ALT3-WS-IDDC                           
000429                                   WS-STEERING-DC                         
000430     ELSE                                                                 
000431        MOVE DIR-IDDC (INDX-KL) TO ALT3-WS-IDDC                           
000432                                   WS-STEERING-DC                         
000433     END-IF                                                               
000434                                                                          
000435*    MOVE DAGENS-DAT              TO TMP1-YYMMDD                          
000436*    MOVE DIR-TISTADAT(INDX-KL)   TO TMP2-YYMMDD                          
000437*    PERFORM WY2000P1                                                     
000438*    IF TMP1-YYMMDD <   TMP2-YYMMDD                                       
000439*      MOVE SPACE                 TO DIR-IDDC (INDX-KL)                   
000440*    END-IF                                                               
000441                                                                          
000442     MOVE +1 TO IX                                                        
000443     MOVE NEJ TO W-DIR-IDDC-SW                                            
000445     PERFORM UNTIL IX   > IX-DCCLEAR-MAX OR                               
                         DLEV-IDDC-CLEAR-IN(IX) = SPACE                         
000446       IF DIR-IDDC (INDX-KL) = DLEV-IDDC-CLEAR-IN(IX)                     
000450          MOVE JA TO W-DIR-IDDC-SW                                        
000451          MOVE IX-DCCLEAR-MAX TO IX                                       
000460       END-IF                                                             
000470       ADD +1 TO IX                                                       
000480     END-PERFORM                                                          
000481                                                                          
000482     IF  DIR-IDDC (INDX-KL) > '  '                                        
000483     AND (ALT3-SDC OR ALT3-LDC)                                           
000484     AND DIR-IDDC-OK                                                      
000485        PERFORM AAA-KOLLA-SDCLEV                                          
000486        IF SDCLEV-OK                                                      
000487           MOVE JA          TO TRAEFF-SW                                  
000488           MOVE JA          TO DLEV-FLSDCLEV-UT                           
000489           MOVE NEJ         TO DLEV-FLRESTN-UT                            
000490           MOVE W-IDDC      TO DLEV-IDDC-UT                               
000491*          IF DIR-FLDDGS (INDX-KL) = JA                                   
000492*             MOVE JA       TO W-DDGS-SW                                  
000493*          ELSE                                                           
000494*             MOVE NEJ      TO W-DDGS-SW                                  
000495*          END-IF                                                         
000496        ELSE                                                              
000497          IF DIR-KVBEART-MIN(INDX-KL) NOT = +0                            
000498            IF DLEV-KVBEART-Q-IN >= DIR-KVBEART-MIN (INDX-KL) OR          
000499               ((DIR-KDDDGS (INDX-KL) = 'N') AND                          
000500                 DLEV-KVBEART-Q-IN < DIR-KVBEART-MIN (INDX-KL))           
000501*             MOVE DAGENS-DAT               TO TMP1-YYMMDD                
000502*             MOVE DIR-TISTADAT (INDX-KL)   TO TMP2-YYMMDD                
000503*             PERFORM WY2000P1                                            
000504*             IF TMP1-YYMMDD >= TMP2-YYMMDD                               
000505                MOVE WS-IDLEVNR     TO DLEV-IDLEVNR-UT                    
000506                MOVE 'U'            TO DLEV-KDORDSTA-UT                   
000507                MOVE JA             TO TRAEFF-SW                          
000508                IF DIR-FLDDGS (INDX-KL) = JA                              
000509                   MOVE JA  TO W-DDGS-SW                                  
000510                END-IF                                                    
000511                MOVE SEQA-KVLS-DLEV TO DLEV-KVLS-DLEV-UT                  
000512                MOVE SEQA-TIINLMOT  TO DLEV-TIINLMOT-UT                   
000513                MOVE SEQA-TIREGDAT  TO DLEV-TIREGDAT-UT                   
000514*             END-IF                                                      
000515            END-IF                                                        
000516          END-IF                                                          
000517        END-IF                                                            
000518     ELSE                                                                 
000519        IF DIR-KVBEART-MIN(INDX-KL) NOT = +0                              
000520          IF DLEV-KVBEART-Q-IN >= DIR-KVBEART-MIN (INDX-KL)               
000521*           MOVE DAGENS-DAT               TO TMP1-YYMMDD                  
000522*           MOVE DIR-TISTADAT (INDX-KL)   TO TMP2-YYMMDD                  
000523*           PERFORM WY2000P1                                              
000524*           IF TMP1-YYMMDD >= TMP2-YYMMDD                                 
000525              MOVE WS-IDLEVNR     TO DLEV-IDLEVNR-UT                      
000526              MOVE 'U'            TO DLEV-KDORDSTA-UT                     
000527              MOVE JA             TO TRAEFF-SW                            
000528              IF DIR-FLDDGS (INDX-KL) = JA                                
000529                 MOVE JA    TO W-DDGS-SW                                  
000530                END-IF                                                    
000531              MOVE SEQA-KVLS-DLEV TO DLEV-KVLS-DLEV-UT                    
000532              MOVE SEQA-TIINLMOT  TO DLEV-TIINLMOT-UT                     
000533              MOVE SEQA-TIREGDAT  TO DLEV-TIREGDAT-UT                     
000534*           END-IF                                                        
000535          END-IF                                                          
000536        END-IF                                                            
000537     END-IF                                                               
000538     .                                                                    
000539     EJECT                                                                
000540                                                                          
000541 AAA-KOLLA-SDCLEV SECTION.                                                
000542     MOVE DLEV-IDARTNR-IN     TO W-IDARTNR                                
000543**   MOVE DLEV-IDDC-ORD-IN    TO W-IDDC                                   
000544     MOVE DIR-IDDC (INDX-KL)  TO W-IDDC                                   
000545                                                                          
000546     PERFORM IMS-GU-ARTS11                                                
000547     IF SEGMENT-FINNS                                                     
000548        IF SLAG-KVAKS-SDC < 0                                             
000549           MOVE 0              TO W-KVAKS-SDC                             
000550        ELSE                                                              
000551           MOVE SLAG-KVAKS-SDC TO W-KVAKS-SDC                             
000552        END-IF                                                            
000553        IF SLAG-KVOKS-DAG < 0                                             
000554           MOVE 0               TO W-KVOKS-DAG                            
000555        ELSE                                                              
000556           MOVE SLAG-KVOKS-DAG  TO W-KVOKS-DAG                            
000557        END-IF                                                            
000558        IF SLAG-KVOKS-BULK < 0                                            
000559           MOVE 0               TO W-KVOKS-BULK                           
000560        ELSE                                                              
000561           MOVE SLAG-KVOKS-BULK TO W-KVOKS-BULK                           
000562        END-IF                                                            
000563                                                                          
000564        COMPUTE W-DISP = SLAG-KVLS                                        
000565                       + W-KVAKS-SDC                                      
000566                       - W-KVOKS-DAG                                      
000567                       - W-KVOKS-BULK                                     
000568        IF W-DISP > 0                                                     
000569           COMPUTE W-DISP = W-DISP                                        
000570                          - SLAG-KVUTRS                                   
000571                          - SLAG-KVSPARR-KVAL                             
000572        END-IF                                                            
000573        IF W-DISP < 0                                                     
000574           MOVE 0              TO W-DISP                                  
000575        END-IF                                                            
000576        IF DLEV-KVBEART-Q-IN > W-DISP OR SLAG-KDLEVSP > 0                 
000577           MOVE NEJ            TO W-SDCLEV-SW                             
000578           IF DLEV-KDORDING-IN < 3                                        
000579             MOVE 'XX'        TO DLEV-KDOI-UT                             
000580             MOVE DIR-IDDC (INDX-KL) TO DLEV-IDDC-CLEAR (1)               
000581             IF DLEV-FLREFILL-IN NOT = JA  OR                             
000582                SLAG-KDREFSTA = 'P'                                       
000583                MOVE PASSIV    TO DLEV-FLLF(1)                            
000584                MOVE JA        TO DLEV-FLCLEAR(1)                         
000585             ELSE                                                         
000586                MOVE JA        TO DLEV-FLLF(1)                            
000587                MOVE JA        TO DLEV-FLCLEAR(1)                         
000588             END-IF                                                       
000589           END-IF                                                         
000590        ELSE                                                              
000591           IF DIR-KDDDGS (INDX-KL) = 'O'                                  
000592             MOVE JA           TO W-SDCLEV-SW                             
000593           ELSE                                                           
000594             IF DIR-KVBEART-MIN(INDX-KL) NOT = +0                         
000595               IF DLEV-KVBEART-Q-IN < DIR-KVBEART-MIN (INDX-KL)           
000596                 MOVE JA       TO W-SDCLEV-SW                             
000597               ELSE                                                       
000598                 MOVE NEJ      TO W-SDCLEV-SW                             
000599               END-IF                                                     
000600             ELSE                                                         
000601               MOVE JA         TO W-SDCLEV-SW                             
000602             END-IF                                                       
000603           END-IF                                                         
000604        END-IF                                                            
000605     ELSE                                                                 
000606        MOVE NEJ               TO W-SDCLEV-SW                             
000607        IF DLEV-KDORDING-IN < 3                                           
000608          MOVE DIR-IDDC (INDX-KL)  TO DLEV-IDDC-CLEAR (1)                 
000609          MOVE 'XX'           TO DLEV-KDOI-UT                             
000610          MOVE NEJ            TO DLEV-FLLF(1)                             
000611          MOVE JA             TO DLEV-FLCLEAR(1)                          
000612        END-IF                                                            
000613     END-IF                                                               
000614     .                                                                    
000615                                                                          
000616     EJECT                                                                
000617 AB-BER-SKEPPNDAG SECTION.                                                
000618                                                                          
000619     MOVE NEJ               TO W-HELG-SW                                  
000620     MOVE '11'              TO WORK-IDDC                                  
000621     MOVE 1                 TO WORK-KVWORKD                               
000622     MOVE DAGENS-DAT        TO WORK-TIAAMMDD-FOM                          
000623     MOVE 002               TO WORK-KDCALL                                
000624     CALL WORKDAY  USING       WORK-KDCALL                                
000625                               WORK-DATE-AREA                             
000626                               WORK-KDSVAR                                
000627     END-CALL                                                             
000628     IF WORK-KDSVAR-FEL                                                   
000629       MOVE ' FEL FRÅN WORKDAY (W411DLEV) 1' TO ERROR-TEXT                
000630       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
000631     END-IF                                                               
000632*    DISPLAY 'RRR*DLEV*FOM/TOM/WORKD 1 ='                                 
000633*                  WORK-TIAAMMDD-FOM '/'                                  
000634*                  WORK-TIAAMMDD-TOM '/'                                  
000635*                  WORK-KVWORKD                                           
000636     IF WORK-TIAAMMDD-FOM NOT = WORK-TIAAMMDD-TOM                         
000637        MOVE JA             TO W-HELG-SW                                  
000638     END-IF                                                               
000639                                                                          
000640     MOVE '11'              TO WORK-IDDC                                  
000641     COMPUTE DAGENS-KLOCKA = DAGENS-KLOCKA / 10000                        
000642     END-COMPUTE                                                          
000643     MOVE DSTY-KVDAGAR-LEV  TO WORK-KVWORKD                               
000644*    DISPLAY 'RRR*DLEV*KVDAGAR-LEV=' WORK-KVWORKD                         
000645     IF  DAGENS-KLOCKA > DSTY-TIMINUT-CUT                                 
000646     AND NOT HELG                                                         
000647       ADD 1                TO WORK-KVWORKD                               
000648     END-IF                                                               
000649     MOVE DAGENS-DAT        TO WORK-TIAAMMDD-FOM                          
000650     MOVE 002               TO WORK-KDCALL                                
000651     CALL WORKDAY  USING       WORK-KDCALL                                
000652                               WORK-DATE-AREA                             
000653                               WORK-KDSVAR                                
000654     END-CALL                                                             
000655     IF WORK-KDSVAR-FEL                                                   
000656       MOVE ' FEL FRÅN WORKDAY (W411DLEV) 2' TO ERROR-TEXT                
000657       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
000658     END-IF                                                               
000659*    DISPLAY 'RRR*DLEV*FOM/TOM/WORKD 2 ='                                 
000660*                  WORK-TIAAMMDD-FOM '/'                                  
000661*                  WORK-TIAAMMDD-TOM '/'                                  
000662*                  WORK-KVWORKD                                           
000663     MOVE WORK-TIAAMMDD-TOM TO DLEV-TISKEPPN-DDC-UT                       
000664*    DISPLAY 'RRR*DLEV*TISKEPPN=' DLEV-TISKEPPN-DDC-UT                    
000665     .                                                                    
000666     EJECT                                                                
000667 S01-CREATE-WDR6-TRANS   SECTION.                                         
000668     MOVE IDPGM                        TO  FIL-IDPGM                      
000669     ACCEPT FIL-TIREGDAT               FROM  DATE                         
000670     ACCEPT FIL-TIKLOCK                FROM  TIME                         
000671     MOVE ZERO                         TO FIL-IDSEKVNR                    
000672     MOVE 'W414'                       TO FIL-CT-IDSYSTEM                 
000673     MOVE 'A'                          TO FIL-CT-IDVTYP                   
000674     MOVE 'BUM'                        TO FIL-CT-IDPTYP                   
000675     MOVE SPACE                        TO FIL-WDR601-DATA                 
000676*    MOVE DLEV-IDLEVNR-UT              TO BUM-IDLEVNR                     
000677     MOVE WS-IDLEVNR                   TO BUM-IDLEVNR                     
000678     MOVE DLEV-IDDISTR-IN              TO BUM-IDDISTR                     
000679     MOVE DLEV-IDKUNDNR-IN             TO BUM-IDKUNDNR                    
000680     MOVE DLEV-IDKUNDRF-IN             TO BUM-IDKUNDRF                    
000681     MOVE DLEV-IDARTNR-IN              TO BUM-IDARTNR                     
000682     MOVE DLEV-KVBEART-Q-IN            TO BUM-KVBEART                     
000683     MOVE W-IDDC                       TO BUM-IDDC-STEER                  
000684                                                                          
000685     IF DELEV-DC-OK                                                       
000686       MOVE W-IDDC                     TO BUM-IDDC                        
000687     ELSE                                                                 
000688       MOVE SPACE                      TO BUM-IDDC                        
000689     END-IF                                                               
000690                                                                          
000691     ADD +1                            TO FIL-IDSEKVNR                    
000692     PERFORM IMS-ISRT-FILA-WDR6                                           
000693     PERFORM UNTIL SEGMENT-FINNS                                          
000694        ADD +1 TO FIL-IDSEKVNR                                            
000695        PERFORM IMS-ISRT-FILA-WDR6                                        
000696     END-PERFORM                                                          
000697     .                                                                    
000698     EJECT                                                                
000699 IMS-GU-SEQA SECTION.                                                     
000700                                                                          
000701     STRING 'WLLEVG01(WDF2A1KY>=' W-WDF2A1KY-MIN-X                        
000702                    '&WDF2A1KY<=' W-WDF2A1KY-MAX-X                        
000703                    '&DASTADAT<=' W-DASTADAT-X ')'                        
000704          DELIMITED BY SIZE INTO SSA1                                     
000705     MOVE '  GEGB' TO GODK-STATUSKODER                                    
000706     CALL CBLTDLI USING GU LEVG-PCB DLI-IO-WLLEVG01 SSA1                  
000707     MOVE LEVG-STATUS-CODE TO STATUS-WS                                   
000708     PERFORM IMS-STATUSKONTROLL                                           
000709     .                                                                    
000710     EJECT                                                                
000711 IMS-GN-SEQA SECTION.                                                     
000712                                                                          
000713     STRING 'WLLEVG01(WDF2A1KY>=' W-WDF2A1KY-MIN-X                        
000714                    '&WDF2A1KY<=' W-WDF2A1KY-MAX-X                        
000715                    '&DASTADAT<=' W-DASTADAT-X ')'                        
000716          DELIMITED BY SIZE INTO SSA1                                     
000717     MOVE '  GEGB' TO GODK-STATUSKODER                                    
000718     CALL CBLTDLI USING GN LEVG-PCB DLI-IO-WLLEVG01 SSA1                  
000719     MOVE LEVG-STATUS-CODE TO STATUS-WS                                   
000720     PERFORM IMS-STATUSKONTROLL                                           
000721     .                                                                    
000722     EJECT                                                                
000723 IMS-GU-LEV SECTION.                                                      
000724                                                                          
000725     STRING 'WLLEVF01(WDF201KY =' W-WDF201KY-X ')'                        
000726          DELIMITED BY SIZE INTO SSA1                                     
000727     MOVE '  ' TO GODK-STATUSKODER                                        
000728     CALL CBLTDLI USING GU LEVF-PCB DLI-IO-WLLEVF01 SSA1                  
000729     MOVE LEVF-STATUS-CODE TO STATUS-WS                                   
000730     PERFORM IMS-STATUSKONTROLL                                           
000731     .                                                                    
000732                                                                          
000733 IMS-GET-DISTRIKT SECTION.                                                
000734                                                                          
000735     STRING 'WLLEVF01(WDF201KY =' W-WDF201KY-X ')'                        
000736            DELIMITED BY SIZE INTO SSA1                                   
000737     STRING 'WLLEVF11(IDDISTRF<=' W-IDDISTR-X                             
000738                    '&IDDISTRT>=' W-IDDISTR-X                             
000739                    '&IDKUNDNF>=' W-IDKUNDNR-FOM-X                        
000740                    '&IDKUNDNT<=' W-IDKUNDNR-TOM-X ')'                    
000741            DELIMITED BY SIZE INTO SSA2                                   
000742     MOVE '  GE' TO GODK-STATUSKODER                                      
000743     CALL CBLTDLI USING GU LEVF-PCB DLI-IO-WLLEVF11 SSA1 SSA2             
000744     MOVE LEVF-STATUS-CODE TO STATUS-WS                                   
000745     PERFORM IMS-STATUSKONTROLL                                           
000746     .                                                                    
000747                                                                          
000748 IMS-GET-KUND SECTION.                                                    
000749                                                                          
000750     STRING 'WLLEVF01(WDF201KY =' W-WDF201KY-X ')'                        
000751            DELIMITED BY SIZE INTO SSA1                                   
000752     STRING 'WLLEVF11(IDDISTRF =' W-IDDISTR-X                             
000753                    '&IDDISTRT =' W-IDDISTR-X                             
000754                    '&IDKUNDNF<=' W-IDKUNDNR-FOM-X                        
000755                    '&IDKUNDNT>=' W-IDKUNDNR-TOM-X ')'                    
000756            DELIMITED BY SIZE INTO SSA2                                   
000757     MOVE '  GE' TO GODK-STATUSKODER                                      
000758     CALL CBLTDLI USING GU LEVF-PCB DLI-IO-WLLEVF11 SSA1 SSA2             
000759     MOVE LEVF-STATUS-CODE TO STATUS-WS                                   
000760     PERFORM IMS-STATUSKONTROLL                                           
000761     .                                                                    
000762     EJECT                                                                
000763 IMS-GU-WDF101 SECTION.                                                   
000764                                                                          
000765     STRING 'WLLEVA01(IDLEVNR  =' W-WDF101KY-X ')'                        
000766          DELIMITED BY SIZE INTO SSA1                                     
000767     MOVE '  GE' TO GODK-STATUSKODER                                      
000768     CALL CBLTDLI USING GU LEVA-PCB DLI-IO-WLLEVA01 SSA1                  
000769     MOVE LEVA-STATUS-CODE TO STATUS-WS                                   
000770     PERFORM IMS-STATUSKONTROLL                                           
000771     .                                                                    
000772                                                                          
000773 IMS-GU-WDF118 SECTION.                                                   
000774                                                                          
000775     STRING 'WLLEVA01(IDLEVNR  =' W-WDF101KY-X ')'                        
000776          DELIMITED BY SIZE INTO SSA1                                     
000777     STRING 'WLLEVA18(WDF118KY =' W-WDF118KY-X ')'                        
000778          DELIMITED BY SIZE INTO SSA2                                     
000779     MOVE '  GE' TO GODK-STATUSKODER                                      
000780     CALL CBLTDLI USING GU LEVA-PCB DLI-IO-WLLEVA18 SSA1 SSA2             
000781     MOVE LEVA-STATUS-CODE TO STATUS-WS                                   
000782     PERFORM IMS-STATUSKONTROLL                                           
000783     .                                                                    
000784     EJECT                                                                
000785                                                                          
000786 IMS-GU-ARTS11 SECTION.                                                   
000787                                                                          
000788     STRING 'WLARTS01(IDARTNR  =' W-IDARTNR-X ')'                         
000789            DELIMITED BY SIZE INTO SSA1                                   
000790     STRING 'WLARTS11(IDDC     =' W-IDDC-X ')'                            
000791            DELIMITED BY SIZE INTO SSA2                                   
000792     MOVE '  GE' TO GODK-STATUSKODER                                      
000793     CALL CBLTDLI USING GU ARTS-PCB DLI-IO-WLARTS SSA1 SSA2               
000794     MOVE ARTS-STATUS-CODE TO STATUS-WS                                   
000795     PERFORM IMS-STATUSKONTROLL                                           
000796     .                                                                    
000797     EJECT                                                                
000798                                                                          
000799 IMS-GU-WDB601    SECTION.                                                
000800     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
000801          DELIMITED BY SIZE INTO SSA1                                     
000802     MOVE '  GE' TO GODK-STATUSKODER                                      
000803     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
000804     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
000805     PERFORM IMS-STATUSKONTROLL                                           
000806     .                                                                    
000807 IMS-ISRT-FILA-WDR6 SECTION.                                              
000808     MOVE 'WLFILA01' TO SSA1                                              
000809     MOVE '  II' TO GODK-STATUSKODER                                      
000810     CALL CBLTDLI USING ISRT FILA-PCB DLI-IO-WDR601 SSA1                  
000811     MOVE FILA-STATUS-CODE TO STATUS-WS                                   
000812     PERFORM IMS-STATUSKONTROLL                                           
000813     .                                                                    
000814 IMS-STATUSKONTROLL SECTION.                                              
000815                                                                          
000816     SET STATUS-IX TO 1                                                   
000817     SEARCH GODK-STATUS                                                   
000818       AT END                                                             
000819         CALL FELLOG                                                      
000820       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
000821         CONTINUE                                                         
000822     END-SEARCH                                                           
000823     .                                                                    
000824     EJECT                                                                
000825*    -COPY WY2000P1                                                       
