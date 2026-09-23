000001*COMPOPT STDSUB=YES                                                       
000002 ID DIVISION.                                                             
000003     SKIP2                                                                
000004 PROGRAM-ID.     W411KREG.                                                
000005 AUTHOR.         LASSI OLGRENER.                                          
000006 DATE-WRITTEN.   MARS -90.                                                
000007                                                                          
000008     REMARKS.                                                             
000009*                                                                         
000010*    FUNKTION.                                                            
000011*        PROGRAMMET KOLLAR/HÄMTAR KUNDINFORMATION TILL                    
000012*        ORDERHUVUDREGISTRERING.                                          
000013*                                                                         
000014*        PROGRAMMET ÄR GEMENSAMT SUBPROGRAM                               
000015*                                                                         
000016*        PROGRAMMET LÄSER      WLGMTA (WDB2) KUNDINFO                     
000017*                              WLGMTB (WDB3) LAGERINFO                    
000018*                              WLGMTC (WDB5) FRAKTINFO                    
000019*                              WLBETC (WDB1) BETALARE                     
000020*                                                                         
000021*    LÄNKAREA: W411KREG-CTX                                               
000022     SKIP3                                                                
000023 ENVIRONMENT DIVISION.                                                    
000024     EJECT                                                                
000025 DATA DIVISION.                                                           
000026 WORKING-STORAGE SECTION.                                                 
000027*    -COPY WY2000W1                                                       
000028     SKIP3                                                                
000029 77  IDPGM                       PIC X(08)   VALUE 'W411KREG'.            
000030 77  FELTEXT                     PIC X(72)   VALUE SPACE.                 
000031 77  JA                          PIC X       VALUE 'J'.                   
000032 77  NEJ                         PIC X       VALUE 'N'.                   
000033 77  IX                          PIC S9(9)   COMP SYNC.                   
000034 77  IX-DCCLEAR-MAX              PIC 9(9)    VALUE 99.                    
000035 77  IX-MAX-IDDC-PREPLAN         PIC 9(9)    VALUE 8.                     
000036 77  DAGENS-DATUM                PIC 9(6).                                
000037 01  WS-TIME                         PIC  9(8).                           
000038 01  FILLER REDEFINES WS-TIME.                                            
000039     03 WS-HHMM                      PIC  9(4).                           
000040     03 FILLER                       PIC  9(4).                           
000041                                                                          
000042 77  ALLT-SW                     PIC X       VALUE 'J'.                   
000043     88  ALLT-OK                             VALUE 'J'.                   
000044                                                                          
000045 01  GENERELLA-SUBPROGRAM.                                                
000046     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
000047     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
000048     EJECT                                                                
000049                                                                          
000050 01  FILLER                      PIC  X(16) VALUE 'EG-LAND'.              
000051 01  TEST-IDLANDX2               PIC  X(2).                               
000052 01  FILLER REDEFINES TEST-IDLANDX2.                                      
000053*    03    -COPY WWLANDX2.                                                
000054     EJECT                                                                
000055*                                                                         
000056 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
000057     SKIP3                                                                
000058 01  NYCKLAR-TILL-DLI.                                                    
000059     03  W-IDGMT-X.                                                       
000060         05  W-IDDISTR           PIC S9(5)   VALUE ZERO COMP-3.           
000061         05  W-IDKUNDNR          PIC S9(7)   VALUE ZERO COMP-3.           
000062     03  W-WDB301KY-X.                                                    
000063         05  W-IDDC              PIC  X(2)   VALUE SPACE.                 
000064         05  W-IDDISTR-DC        PIC S9(5)   VALUE ZERO COMP-3.           
000065         05  W-IDKUNDNR-DC       PIC S9(7)   VALUE ZERO COMP-3.           
000066     03  W-WDB301KY-DEF-X.                                                
000067         05  W-IDDC-DEF          PIC  X(2)   VALUE SPACE.                 
000068         05  W-IDDISTR-DC-DEF    PIC S9(5)   VALUE ZERO COMP-3.           
000069         05  W-IDKUNDNR-DC-DEF   PIC S9(7)  VALUE +9999999 COMP-3.        
000070     03  W-WDB501KY-X.                                                    
000071         05  W-IDDC-FK           PIC  X(2)   VALUE SPACE.                 
000072         05  W-KDFRAKT           PIC S9(3)   VALUE ZERO COMP-3.           
000073         05  W-IDDISTR-FK        PIC S9(5)   VALUE ZERO COMP-3.           
000074         05  W-IDKUNDNR-FK       PIC S9(7)   VALUE ZERO COMP-3.           
000075     03  W-WDB501KY-DEF-X.                                                
000076         05  W-IDDC-FK-DEF       PIC  X(2)   VALUE SPACE.                 
000077         05  W-KDFRAKT-DEF       PIC S9(3)   VALUE ZERO COMP-3.           
000078         05  W-IDDISTR-FK-DEF    PIC S9(5)   VALUE ZERO COMP-3.           
000079         05  W-IDKUNDNR-FK-DEF   PIC S9(7)  VALUE +9999999 COMP-3.        
000080     03  W-WDB101KY-X.                                                    
000081         05  W-IDPARTNR          PIC  X(9)   VALUE SPACE.                 
000082         05  W-IDFTG             PIC  9(2)   VALUE ZERO.                  
000083     SKIP2                                                                
000084 01  STATUS-WS                   PIC XX.                                  
000085     88  SEGMENT-FINNS                       VALUE '  '.                  
000086     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
000087     SKIP2                                                                
000088 01  GODK-STATUSKODER.                                                    
000089     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
000090     SKIP3                                                                
000091 01  SSA1                        PIC X(96).                               
000092     EJECT                                                                
000093*    --- IMS FUNKTIONSKODER                                               
000094*01  -COPY W0003                                                          
000095     EJECT                                                                
000096 01  FILLER                      PIC X(16) VALUE 'DLI-IO-AREA-B1'.        
000097     SKIP3                                                                
000098 01  DLI-IO-AREA-B1.                                                      
000099*    03  -COPY WDB101                                                     
000100     EJECT                                                                
000101 01  FILLER                      PIC X(16) VALUE 'DLI-IO-AREA-B2'.        
000102     SKIP3                                                                
000103 01  DLI-IO-AREA-B2.                                                      
000104*    03  -COPY WDB201                                                     
000105     EJECT                                                                
000106 01  FILLER                      PIC X(16) VALUE 'DLI-IO-AREA-B3'.        
000107     SKIP3                                                                
000108 01  DLI-IO-AREA-B3.                                                      
000109*    03  -COPY WDB301                                                     
000110     EJECT                                                                
000111 01  FILLER                      PIC X(16) VALUE 'DLI-IO-AREA-B5'.        
000112     SKIP3                                                                
000113 01  DLI-IO-AREA-B5.                                                      
000114*    03  -COPY WDB501                                                     
000115     EJECT                                                                
000116 LINKAGE SECTION.                                                         
000117*   -COPY W411KREG                                                        
000118     EJECT                                                                
000119*01  -COPY W0008 -PRE GMTA-                                               
000120     05  FILLER                  PIC X.                                   
000121*01  -COPY W0008 -PRE GMTB-                                               
000122     05  FILLER                  PIC X.                                   
000123     EJECT                                                                
000124*01  -COPY W0008 -PRE GMTC-                                               
000125     05  FILLER                  PIC X.                                   
000126*01  -COPY W0008 -PRE BETC-                                               
000127     05  FILLER                  PIC X.                                   
000128     EJECT                                                                
000129 PROCEDURE DIVISION  USING KREG-W411KREG GMTA-PCB GMTB-PCB                
000130                                         GMTC-PCB BETC-PCB.               
000131 MAIN SECTION.                                                            
000132     PERFORM A-INIT                                                       
000133                                                                          
000134     PERFORM B-REDIGERA-GMT-INFO                                          
000135                                                                          
000136     IF ALLT-OK                                                           
000137       PERFORM C-REDIGERA-WDB101-INFO                                     
000138                                                                          
000139       IF ALLT-OK                                                         
000140         PERFORM D-REDIGERA-DC-INFO                                       
000141                                                                          
000142         IF ALLT-OK                                                       
000143           PERFORM E-REDIGERA-FRAKT-INFO                                  
000144         END-IF                                                           
000145       END-IF                                                             
000146     END-IF                                                               
000147                                                                          
000148     GOBACK                                                               
000149     .                                                                    
000150     EJECT                                                                
000151 A-INIT SECTION.                                                          
000152                                                                          
000153     ACCEPT DAGENS-DATUM       FROM DATE                                  
000154                                                                          
000155     MOVE JA                   TO KREG-IDDISTR-OK                         
000156                                  KREG-IDKUNDNR-OK                        
000157                                  KREG-IDDC-OK                            
000158                                  KREG-KDFRAKT-OK                         
000159                                  KREG-IDVAT-OK                           
000160                                  KREG-IDPARTNR-OK                        
000161                                  ALLT-SW                                 
000162     .                                                                    
000163     EJECT                                                                
000164 B-REDIGERA-GMT-INFO SECTION.                                             
000165                                                                          
000166     MOVE KREG-IDDISTR         TO W-IDDISTR                               
000167     MOVE KREG-IDKUNDNR        TO W-IDKUNDNR                              
000168                                                                          
000169     PERFORM IMS-GU-GMTA-WDB201                                           
000170     IF SEGMENT-SAKNAS                                                    
000171        MOVE NEJ               TO KREG-IDDISTR-OK                         
000172                                  KREG-IDKUNDNR-OK                        
000173                                  ALLT-SW                                 
000174     ELSE                                                                 
000175        MOVE DAGENS-DATUM   TO TMP1-YYMMDD                                
000176        MOVE GMT-TISTADAT   TO TMP2-YYMMDD                                
000177        MOVE GMT-TISTODAT   TO TMP3-YYMMDD                                
000178        PERFORM WY2000Q1                                                  
000179        IF (TMP1-YYMMDD < TMP3-YYMMDD OR TMP3-YYMMDD = +0)                
000180*        (TMP1-YYMMDD >= TMP3-YYMMDD AND KREG-IDSYSTEM = 'SOFT'))         
000181          AND                                                             
000182           ((TMP1-YYMMDD NOT < TMP2-YYMMDD) AND TMP2-YYMMDD > +0)         
000183          IF KREG-IDDC-TVS > ZERO                                         
000184             PERFORM BA-KOLLA-IDDC-TVS                                    
000185          END-IF                                                          
000186          IF ALLT-OK                                                      
000187             PERFORM BB-FLYTTA-GMT-DATA                                   
000188          ELSE                                                            
000189             MOVE NEJ          TO KREG-IDDC-OK                            
000190          END-IF                                                          
000191        ELSE                                                              
000192          MOVE NEJ            TO KREG-IDKUNDNR-OK                         
000193                                 ALLT-SW                                  
000194        END-IF                                                            
000195     END-IF                                                               
000196     .                                                                    
000197     EJECT                                                                
000198 BA-KOLLA-IDDC-TVS SECTION.                                               
000199                                                                          
000200     MOVE NEJ              TO ALLT-SW                                     
000201     IF (KREG-IDSYSTEM = 'LDCB' OR 'LYNB') AND KREG-KDORDKL = 3           
000202       MOVE +1             TO IX                                          
000203       PERFORM UNTIL ALLT-OK OR IX > IX-MAX-IDDC-PREPLAN                  
000204          IF KREG-IDDC-TVS = GMT-IDDC-PREPLAN(IX)                         
000205            MOVE JA        TO ALLT-SW                                     
000206          END-IF                                                          
000207          ADD 1 TO IX                                                     
000208       END-PERFORM                                                        
000209     END-IF                                                               
000210                                                                          
000211     MOVE +1               TO IX                                          
000212     PERFORM UNTIL ALLT-OK OR IX > IX-DCCLEAR-MAX OR                      
                        ( GMT-IDDC-VOR(IX)  = SPACE AND                         
                          GMT-IDDC-DAY(IX)  = SPACE AND                         
                          GMT-IDDC-BULK(IX) = SPACE                             
                        )                                                       
000213       IF KREG-KDORDKL = 0                                                
000214          IF KREG-IDDC-TVS = GMT-IDDC-VOR(IX)                             
000215            MOVE JA        TO ALLT-SW                                     
000216          END-IF                                                          
000217       END-IF                                                             
000218       IF KREG-KDORDKL = 1                                                
000219          IF KREG-IDDC-TVS = GMT-IDDC-DAY(IX)                             
000220            MOVE JA        TO ALLT-SW                                     
000221          END-IF                                                          
000222       END-IF                                                             
000223       IF KREG-KDORDKL > 1                                                
000224          IF KREG-IDDC-TVS = GMT-IDDC-BULK(IX)                            
000225            MOVE JA        TO ALLT-SW                                     
000226          END-IF                                                          
000227       END-IF                                                             
000228       ADD +1              TO IX                                          
000229     END-PERFORM                                                          
000230                                                                          
000231     IF  NOT ALLT-OK                                                      
000232     AND KREG-FLVORKO = JA                                                
000233     AND KREG-KDORDKL = 0                                                 
000234         IF  KREG-IDDC-TVS = GMT-IDDC-TVSVOR(1)                           
000235         OR  KREG-IDDC-TVS = GMT-IDDC-TVSVOR(2)                           
000236         OR  KREG-IDDC-TVS = GMT-IDDC-TVSVOR(3)                           
000237         OR  KREG-IDDC-TVS = GMT-IDDC-TVSVOR(4)                           
000238         OR  KREG-IDDC-TVS = GMT-IDDC-TVSVOR(5)                           
000239         OR  KREG-IDDC-TVS = GMT-IDDC-TVSVOR(6)                           
000240         OR  KREG-IDDC-TVS = GMT-IDDC-TVSVOR(7)                           
000241         OR  KREG-IDDC-TVS = GMT-IDDC-TVSVOR(8)                           
000242         OR  KREG-IDDC-TVS = GMT-IDDC-TVSVOR(9)                           
000243         OR  KREG-IDDC-TVS = GMT-IDDC-TVSVOR(10)                          
000244         OR  KREG-IDDC-TVS = GMT-IDDC-TVSVOR(11)                          
000245         OR  KREG-IDDC-TVS = GMT-IDDC-TVSVOR(12)                          
000246         OR  KREG-IDDC-TVS = GMT-IDDC-TVSVOR(13)                          
000247         OR  KREG-IDDC-TVS = GMT-IDDC-TVSVOR(14)                          
000248         OR  KREG-IDDC-TVS = GMT-IDDC-TVSVOR(15)                          
000249         OR  KREG-IDDC-TVS = GMT-IDDC-TVSVOR(16)                          
000250             MOVE JA       TO ALLT-SW                                     
000251         END-IF                                                           
000252     END-IF                                                               
000253     .                                                                    
000254     EJECT                                                                
000255 BB-FLYTTA-GMT-DATA SECTION.                                              
000256                                                                          
000257     MOVE GMT-IDDEPOT          TO KREG-IDDEPOT                            
000258     MOVE GMT-IDROUTE          TO KREG-IDROUTE                            
000259     MOVE GMT-IDZON            TO KREG-IDZON                              
000260                                                                          
000261     MOVE GMT-ADGMT            TO KREG-ADGMT                              
000262     MOVE GMT-BEGMT            TO KREG-BEGMT                              
000263     MOVE GMT-FLAUTORD         TO KREG-FLAUTORD                           
000264     MOVE GMT-FLNC             TO KREG-FLNC                               
000265     MOVE GMT-FLOKFAK-G        TO KREG-FLOKFAK-G                          
000266     MOVE GMT-FLOKFAK-N        TO KREG-FLOKFAK-N                          
000267     MOVE GMT-FLOKFAK-R        TO KREG-FLOKFAK-R                          
000268     MOVE GMT-FLOKFAK-K        TO KREG-FLOKFAK-K                          
000269     MOVE GMT-FLPRELRO         TO KREG-FLPRELRO                           
000270     MOVE GMT-FLPRERS          TO KREG-FLPRERS                            
000271     MOVE GMT-FLRESTN          TO KREG-FLRESTN                            
000272     MOVE GMT-FLORDTIL-KL1     TO KREG-FLORDTIL-KL1                       
000273     MOVE GMT-FLORDTIL-KL2     TO KREG-FLORDTIL-KL2                       
000274     MOVE GMT-FLORDTIL-KL3     TO KREG-FLORDTIL-KL3                       
000275     MOVE GMT-FLORDTIL-KL4     TO KREG-FLORDTIL-KL4                       
000276     MOVE GMT-IDSKYLT          TO KREG-IDSKYLT                            
000277     MOVE GMT-KDBEKALT         TO KREG-KDBEKALT                           
000278     MOVE GMT-KDGENFAK         TO KREG-KDGENFAK                           
000279     MOVE GMT-KDORDING         TO KREG-KDORDING                           
000280     MOVE GMT-KVDAGAR-DOW      TO KREG-KVDAGAR-DOW                        
000281     MOVE GMT-RESLATT          TO KREG-RESLATT                            
000282     MOVE GMT-IDRFTAB          TO KREG-IDRFTAB                            
000283     IF KREG-RESLATT = ZERO                                               
000284        MOVE '0'               TO KREG-IDRFTAB (3:1)                      
000285     END-IF                                                               
000286     IF KREG-IDDC-TVS > ZERO                                              
000287       MOVE KREG-IDDC-TVS      TO KREG-IDDC                               
000288     ELSE                                                                 
000291       IF KREG-KDORDKL = 0                                                
000292         MOVE GMT-IDDC-VOR(1)  TO KREG-IDDC                               
000293         IF GMT-IDDC-VOR(2) = SPACE                                       
000294           MOVE GMT-IDDC-VOR(1)     TO KREG-IDDC-TVS                      
000295         END-IF                                                           
000296       END-IF                                                             
000297       IF KREG-KDORDKL = 1                                                
000298         MOVE GMT-IDDC-DAY(1)  TO KREG-IDDC                               
000299         IF GMT-IDDC-DAY(2) = SPACE                                       
000300           MOVE GMT-IDDC-DAY(1)     TO KREG-IDDC-TVS                      
000301         END-IF                                                           
000302*!!NOTE!!                                                                 
000303* WHEN IDDC-DAY-ALT HAS A VALUE AND CURRENT TIME IS BETWEEN               
000304* GMT-TIHHMM-START&STOP THEN THE HOME DC GMT-IDDC-DAY(1) WILL BE          
000305* OVERRIDED BY GMT-IDDC-DAY-ALT                                           
000306         IF GMT-IDDC-DAY-ALT > SPACE AND KREG-KDORDKL = 1                 
000307           ACCEPT WS-TIME FROM TIME                                       
000308           IF WS-HHMM > GMT-TIHHMM-START AND                              
000309              WS-HHMM < GMT-TIHHMM-STOP                                   
000310             MOVE GMT-IDDC-DAY-ALT TO KREG-IDDC                           
000315           END-IF                                                         
000316         END-IF                                                           
000317       END-IF                                                             
000318       IF KREG-KDORDKL > 1                                                
000319         MOVE GMT-IDDC-BULK(1)  TO KREG-IDDC                              
000320         IF GMT-IDDC-BULK(2) = SPACE                                      
000321           MOVE GMT-IDDC-BULK(1)    TO KREG-IDDC-TVS                      
000322         END-IF                                                           
000323       END-IF                                                             
000324     END-IF                                                               
000325     MOVE GMT-REAVDRAG         TO KREG-REAVDRAG                           
000326     MOVE GMT-REEMBHNT         TO KREG-REEMBHNT                           
000328     .                                                                    
000329     EJECT                                                                
000330 C-REDIGERA-WDB101-INFO SECTION.                                          
000331                                                                          
000332     MOVE SPACE                TO KREG-ADBETRAD-1                         
000333                                  KREG-ADBETRAD-2                         
000334                                  KREG-BEBETRAD-1                         
000335                                  KREG-BEBETRAD-2                         
000336                                  KREG-KDKREDSP                           
000337                                  KREG-KDVALISO                           
000338     IF GMT-IDPARTNR NOT = SPACE                                          
000339       MOVE GMT-IDPARTNR       TO W-IDPARTNR                              
000340       MOVE GMT-IDFTG          TO W-IDFTG                                 
000341       PERFORM IMS-GU-BETC-WDB101                                         
000342       MOVE BET-ADBETRAD-1     TO KREG-ADBETRAD-1                         
000343       MOVE BET-ADBETRAD-2     TO KREG-ADBETRAD-2                         
000344       MOVE BET-BEBETRAD-1     TO KREG-BEBETRAD-1                         
000345       MOVE BET-BEBETRAD-2     TO KREG-BEBETRAD-2                         
000346       MOVE BET-KDKREDSP       TO KREG-KDKREDSP                           
000347       MOVE BET-KDVALISO       TO KREG-KDVALISO                           
000348                                                                          
000349       IF (KREG-KDFAKTYP-IN = 'K' OR 'N') OR                              
000350            (KREG-KDFAKTYP-IN = SPACE AND                                 
000351            (GMT-KDGENFAK = 'K' OR 'N'))                                  
000352         CONTINUE                                                         
000353       ELSE                                                               
000354          MOVE BET-IDLANDX2    TO TEST-IDLANDX2                           
000355          IF LANDX2-EU-EJ-SE AND BET-IDVAT = SPACE                        
000356            MOVE NEJ           TO KREG-IDVAT-OK                           
000357                                  ALLT-SW                                 
000358          END-IF                                                          
000359       END-IF                                                             
000360     ELSE                                                                 
000361       MOVE NEJ           TO KREG-IDPARTNR-OK                             
000362                             ALLT-SW                                      
000363     END-IF                                                               
000364     .                                                                    
000365     EJECT                                                                
000366 D-REDIGERA-DC-INFO SECTION.                                              
000367                                                                          
000368     IF KREG-IDDC-TVS > ZERO                                              
000369* ENTERED FROM 4221/4231 AND FOR TPO/KAMP-ORDERS,                         
000370*      OR FOR CUSTOMER WITH ONLY ONE DC IN CLEAR TABLE                    
000371       MOVE KREG-IDDC-TVS        TO W-IDDC                                
000372                                    W-IDDC-DEF                            
000373     ELSE                                                                 
000374       MOVE KREG-IDDC            TO W-IDDC                                
000375                                    W-IDDC-DEF                            
000376     END-IF                                                               
000377     MOVE KREG-IDDISTR           TO W-IDDISTR-DC                          
000378                                    W-IDDISTR-DC-DEF                      
000379     MOVE KREG-IDKUNDNR          TO W-IDKUNDNR-DC                         
000380     PERFORM IMS-GU-GMTB-WDB301                                           
000381     MOVE W-IDDC                 TO KREG-IDDC                             
000382     MOVE DC-KDTULLVE            TO KREG-KDTULLVE                         
000383     IF KREG-KDORDKL = 0 OR 1                                             
000384        MOVE DC-KDROPACK-DAG     TO KREG-KDROPACK                         
000385     ELSE                                                                 
000386        MOVE DC-KDROPACK-BULK    TO KREG-KDROPACK                         
000387     END-IF                                                               
000388     MOVE DC-KDMOMSIN            TO KREG-KDMOMSIN                         
000389                                                                          
000390     MOVE DC-KVLEDTIM-0          TO KREG-KVLEDTIM-0                       
000391     MOVE DC-KVLEDTIM-1          TO KREG-KVLEDTIM-1                       
000392     MOVE DC-KVLEDTIM-2          TO KREG-KVLEDTIM-2                       
000393     MOVE DC-KVLEDTIM-3          TO KREG-KVLEDTIM-3                       
000394     MOVE DC-KVLEDTIM-4          TO KREG-KVLEDTIM-4                       
000395     .                                                                    
000396     EJECT                                                                
000397 E-REDIGERA-FRAKT-INFO SECTION.                                           
000398                                                                          
000399     MOVE W-IDDC               TO W-IDDC-FK                               
000400                                  W-IDDC-FK-DEF                           
000401     IF KREG-KDFRAKT-IN = +0                                              
000402       IF KREG-KDORDKL = +0                                               
000403         MOVE DC-KDGENFRA-VOR  TO W-KDFRAKT                               
000404         IF KREG-FLVORFK = JA                                             
000405           MOVE DC-KDGENFRA-DO TO W-KDFRAKT                               
000406         END-IF                                                           
000407       ELSE                                                               
000408         IF KREG-KDORDKL = +1                                             
000409           MOVE DC-KDGENFRA-DO TO W-KDFRAKT                               
000410         ELSE                                                             
000411           MOVE DC-KDGENFRA-MO TO W-KDFRAKT                               
000412         END-IF                                                           
000413       END-IF                                                             
000414     ELSE                                                                 
000415        MOVE KREG-KDFRAKT-IN   TO W-KDFRAKT                               
000416     END-IF                                                               
000417     MOVE W-KDFRAKT            TO W-KDFRAKT-DEF                           
000418     MOVE KREG-IDDISTR         TO W-IDDISTR-FK                            
000419                                  W-IDDISTR-FK-DEF                        
000420     MOVE KREG-IDKUNDNR        TO W-IDKUNDNR-FK                           
000421                                                                          
000422     PERFORM IMS-GU-GMTC-WDB501                                           
000423     IF SEGMENT-SAKNAS                                                    
000424        MOVE NEJ               TO KREG-KDFRAKT-OK                         
000425                                  ALLT-SW                                 
000426     ELSE                                                                 
000427        MOVE FK-KDFRAKT        TO KREG-KDFRAKT                            
000428        MOVE FK-BEGMRK         TO KREG-BEGMRK                             
000429        MOVE FK-KDFDKRAV       TO KREG-KDFDKRAV                           
000430        MOVE FK-REFOERS        TO KREG-REFOERS                            
000431        MOVE FK-PRLEGKST       TO KREG-PRLEGKST                           
000432        MOVE FK-KDTRPKAT       TO KREG-KDTRPKAT                           
000433        IF KREG-KDORDKL = 0                                               
000434          MOVE FK-IDTRP-0      TO KREG-IDTRP                              
000435        END-IF                                                            
000436        IF KREG-KDORDKL = 1                                               
000437          MOVE FK-IDTRP-1      TO KREG-IDTRP                              
000438        END-IF                                                            
000439        IF KREG-KDORDKL = 2                                               
000440          MOVE FK-IDTRP-2      TO KREG-IDTRP                              
000441        END-IF                                                            
000442        IF KREG-KDORDKL = 3                                               
000443          MOVE FK-IDTRP-3      TO KREG-IDTRP                              
000444        END-IF                                                            
000445        IF KREG-KDORDKL = 4                                               
000446          MOVE FK-IDTRP-4      TO KREG-IDTRP                              
000447        END-IF                                                            
000448********* HÄMTA ALTERNATIV TRANSPORT *************                        
000449                                                                          
000450        ADD +1                 TO W-KDFRAKT                               
000451        ADD +1                 TO W-KDFRAKT-DEF                           
000452        PERFORM IMS-GU-GMTC-WDB501                                        
000453        IF SEGMENT-FINNS                                                  
000454          IF KREG-KDORDKL = 0                                             
000455            MOVE FK-IDTRP-0    TO KREG-IDTRP-ALT                          
000456          END-IF                                                          
000457          IF KREG-KDORDKL = 1                                             
000458            MOVE FK-IDTRP-1    TO KREG-IDTRP-ALT                          
000459          END-IF                                                          
000460          IF KREG-KDORDKL = 2                                             
000461            MOVE FK-IDTRP-2    TO KREG-IDTRP-ALT                          
000462          END-IF                                                          
000463          IF KREG-KDORDKL = 3                                             
000464            MOVE FK-IDTRP-3    TO KREG-IDTRP-ALT                          
000465          END-IF                                                          
000466          IF KREG-KDORDKL = 4                                             
000467            MOVE FK-IDTRP-4    TO KREG-IDTRP-ALT                          
000468          END-IF                                                          
000469        ELSE                                                              
000470          MOVE SPACE           TO KREG-IDTRP-ALT                          
000471        END-IF                                                            
000472     END-IF                                                               
000473     .                                                                    
000474     EJECT                                                                
000475* --- IMS SEKTIONER ---                                                   
000476     SKIP3                                                                
000477 IMS-GU-GMTA-WDB201 SECTION.                                              
000478                                                                          
000479     STRING 'WLGMTA01(IDGMT    =' W-IDGMT-X ')'                           
000480          DELIMITED BY SIZE INTO SSA1                                     
000481     MOVE '  GE'              TO GODK-STATUSKODER                         
000482     CALL CBLTDLI USING GU GMTA-PCB DLI-IO-AREA-B2 SSA1                   
000483     MOVE GMTA-STATUS-CODE    TO STATUS-WS                                
000484     PERFORM IMS-STATUSKONTROLL                                           
000485     .                                                                    
000486     SKIP3                                                                
000487 IMS-GU-GMTB-WDB301 SECTION.                                              
000488                                                                          
000489     STRING 'WLGMTB01(WDB301KY =' W-WDB301KY-X                            
000490                    '!WDB301KY =' W-WDB301KY-DEF-X ')'                    
000491          DELIMITED BY SIZE INTO SSA1                                     
000492     MOVE '    '              TO GODK-STATUSKODER                         
000493     CALL CBLTDLI USING GU GMTB-PCB DLI-IO-AREA-B3 SSA1                   
000494     MOVE GMTB-STATUS-CODE    TO STATUS-WS                                
000495     PERFORM IMS-STATUSKONTROLL                                           
000496     .                                                                    
000497     EJECT                                                                
000498 IMS-GU-GMTC-WDB501 SECTION.                                              
000499                                                                          
000500     STRING 'WLGMTC01(WDB501KY =' W-WDB501KY-X                            
000501                    '!WDB501KY =' W-WDB501KY-DEF-X ')'                    
000502          DELIMITED BY SIZE INTO SSA1                                     
000503     MOVE '  GE'              TO GODK-STATUSKODER                         
000504     CALL CBLTDLI USING GU GMTC-PCB DLI-IO-AREA-B5 SSA1                   
000505     MOVE GMTC-STATUS-CODE    TO STATUS-WS                                
000506     PERFORM IMS-STATUSKONTROLL                                           
000507     .                                                                    
000508     SKIP2                                                                
000509 IMS-GU-BETC-WDB101 SECTION.                                              
000510     STRING 'WLBETC01(WDB101KY =' W-WDB101KY-X ')'                        
000511          DELIMITED BY SIZE INTO SSA1                                     
000512     MOVE '    '              TO GODK-STATUSKODER                         
000513     CALL CBLTDLI USING GU BETC-PCB DLI-IO-AREA-B1 SSA1                   
000514     MOVE BETC-STATUS-CODE    TO STATUS-WS                                
000515     PERFORM IMS-STATUSKONTROLL                                           
000516     .                                                                    
000517     SKIP2                                                                
000518 IMS-STATUSKONTROLL SECTION.                                              
000519                                                                          
000520     SET STATUS-IX TO 1                                                   
000521     SEARCH GODK-STATUS                                                   
000522       AT END CALL FELLOG                                                 
000523       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
000524     END-SEARCH                                                           
000525     .                                                                    
000526     EJECT                                                                
000527*    -COPY WY2000Q1                                                       
