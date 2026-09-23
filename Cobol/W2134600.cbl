000001 ID DIVISION.                                                             
000002 PROGRAM-ID.     W2134600.                                                
000003 AUTHOR.         HELGEGREN PER-ANDERS.                                    
000004 DATE-WRITTEN.   03/08/29.                                                
000005 DATE-COMPILED.                                                           
000006                                                                          
000007                                                                          
000008*    FUNKTION:                                                            
000009*        NYTT GRÄNSSNITT INKÖP                                            
000010*        NYUPPLÄGG/ÄNDRING LEVERANTÖRER                                   
000011*                                                                         
000012*        PROGRAMMET UPPDATERAR WDF1                                       
000013*        PROGRAMMET LÄSER      WDK6A                                      
000014*                                                                         
000015                                                                          
000016     SKIP3                                                                
000017 ENVIRONMENT DIVISION.                                                    
000018     SKIP2                                                                
000019 INPUT-OUTPUT SECTION.                                                    
000020                                                                          
000021 FILE-CONTROL.                                                            
000022     SKIP2                                                                
000023*          --- FIL FRÅN INKÖP                                             
000024     SELECT A31481                     ASSIGN TO W21346D1.                
000025     SKIP2                                                                
000026 DATA DIVISION.                                                           
000027     SKIP3                                                                
000028 FILE SECTION.                                                            
000029     SKIP3                                                                
000030 FD  A31481                                                               
000031     RECORDING       F                                                    
000032     BLOCK CONTAINS  0.                                                   
000033                                                                          
000034*01  -COPY A31481      -L.                                                
000035     SKIP3                                                                
000036 WORKING-STORAGE SECTION.                                                 
000037                                                                          
000038 77  IDPGM                       PIC X(8)    VALUE 'W2134600'.            
000039 01  CHKP-VAR.                                                            
000040     03 CHKP-MSG-IO-AREA-LENGTH  PIC S9(9)   VALUE +32 COMP SYNC.         
000050     03 CHKP-MSG-IO-AREA         PIC X(32)   VALUE SPACE.                 
000051     03 CHKP-AREA-LENGTH         PIC S9(9)   VALUE +32 COMP SYNC.         
000052     03 CHKP-AREA                PIC X(32)   VALUE SPACE.                 
000053     03 CHKP-ANT                 PIC S9(3)   VALUE +0   COMP-3.           
000054     03 CHKP-MAX                 PIC S9(3)   VALUE +100 COMP-3.           
000055 77  JA                          PIC X       VALUE 'J'.                   
000056 77  NEJ                         PIC X       VALUE 'N'.                   
000057     SKIP2                                                                
000058 01  FELTEXT.                                                             
000059     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
000060     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
000061                                                                          
000062 01  W-A31481-KVPOST-IN          PIC S9(5)   COMP-3 VALUE ZERO.           
000063 01  W-KDVALLEV                  PIC S9(3)   COMP-3 VALUE ZERO.           
000064 01  W-KDSPRAK                   PIC S9(1)   COMP-3 VALUE ZERO.           
000065 01  WS-AA0101                   PIC 9(6).                                
000066 01  FILLER  REDEFINES WS-AA0101.                                         
000067     03  WS-AA                   PIC 9(2).                                
000068     03  WS-0101                 PIC 9(4).                                
000069                                                                          
000070 77  A31481-EOF-SW               PIC X       VALUE 'N'.                   
000071     88  END-OF-A31481                       VALUE 'J'.                   
000072                                                                          
000073 01  WS-SITEID                   PIC X(5)    VALUE SPACE.                 
000074     EJECT                                                                
000075 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
000076 01  FILLER REDEFINES DAGENS-DATUM.                                       
000077     03  DAGENS-DATUM-AAR        PIC 9(2).                                
000078     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
000079     03  DAGENS-DATUM-DAG        PIC 9(2).                                
000080     EJECT                                                                
000081 01  DYNAMISKA-SUBPROGRAM.                                                
000082*                                                                         
000083     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
000084     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
000085     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
000086     EJECT                                                                
000087*    --- PARAMETRAR TILL POSTSUM                                          
000088*                                                                         
000089*01  -COPY W0005   -PRE  POSTSUM-                                         
000090     EJECT                                                                
000091*01  -COPY W553LVAL                                                       
000092     EJECT                                                                
000093 01  IN-AREA-START               PIC X(24)   VALUE                        
000094                                             'IN-AREA-START'.             
000095     SKIP2                                                                
000096                                                                          
000097*01  AREA -COPY A31481     -PRE IN-                                       
000098     EJECT                                                                
000099 01  UT-AREA-START               PIC X(24)   VALUE                        
000100                                             'UT-AREA-START'.             
000101     SKIP2                                                                
000102                                                                          
000103*01  AREA -COPY A31481     -PRE UT-                                       
000104*                                                                         
000105     EJECT                                                                
000106 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
000107     SKIP3                                                                
000108 01  NYCKLAR-TILL-DLI.                                                    
000109     03  W-IDLEVNR-X.                                                     
000110         05  W-IDLEVNR           PIC X(5)    VALUE SPACE.                 
000111     03  W-IDLEVSUF-X.                                                    
000112         05  W-IDLEVSUF          PIC S9(1)   VALUE 1 COMP-3.              
000113     03  W-WDK6ASEQ-X.                                                    
000114         05  W-WDK6ASEQ          PIC X(5)    VALUE SPACE.                 
000115     SKIP2                                                                
000116*    --- STATUS-KOD FRÅN IMS                                              
000117 01  STATUS-WS                   PIC XX.                                  
000118     88  SEGMENT-FINNS                       VALUE '  '.                  
000119     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
000120     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
000121     88  SEGMENT-SLUT                        VALUE 'GB'.                  
000122     88  IMS-EJ-OK                           VALUE 'XD'.                  
000123     SKIP2                                                                
000124 01  GODK-STATUSKODER.                                                    
000125     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
000126     SKIP3                                                                
000127 01  SSA1                        PIC X(64).                               
000128 01  SSA2                        PIC X(64).                               
000129     EJECT                                                                
000130*    --- IMS FUNKTIONSKODER                                               
000131*01  -COPY W0003                                                          
000132     EJECT                                                                
000133*    ---  DLI INPUT-OUTPUT AREA                                           
000134                                                                          
000135 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDF101'.                      
000136 01  DLI-IO-WDF101.                                                       
000137*    03  -COPY WDF101                                                     
000138     EJECT                                                                
000139 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDF102'.                      
000140 01  DLI-IO-WDF102.                                                       
000141*    03  -COPY WDF102                                                     
000142     EJECT                                                                
000143 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDF106'.                      
000144 01  DLI-IO-WDF106.                                                       
000145*    03  -COPY WDF106                                                     
000146 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK601'.                      
000147 01  DLI-IO-WDK601.                                                       
000148*    03  -COPY WDK601                                                     
000149                                                                          
000150     EJECT                                                                
000151 LINKAGE SECTION.                                                         
000152                                                                          
000153*01  -COPY W0009   -PRE MSG-                                              
000154                                                                          
000155*01  -COPY W0008  -PRE WDF1-                                              
000156     05  FILLER                  PIC X.                                   
000157                                                                          
000158*01  -COPY W0008  -PRE WDK6A-                                             
000159     05  FILLER                  PIC X.                                   
000160     EJECT                                                                
000161 PROCEDURE DIVISION  USING MSG-PCB WDF1-PCB WDK6A-PCB.                    
000162 MAIN SECTION.                                                            
000163     ENTRY 'DLITCBL' USING MSG-PCB WDF1-PCB WDK6A-PCB.                    
000164                                                                          
000165     SKIP2                                                                
000166     PERFORM A-INIT                                                       
000167     PERFORM S01-LAES-A31481                                              
000168     PERFORM UNTIL END-OF-A31481                                          
000169       IF CHKP-ANT > CHKP-MAX                                             
000170         PERFORM X-TAG-CHECKPOINT                                         
000171       END-IF                                                             
000172                                                                          
000173       IF IN-SITEID NUMERIC AND IN-SITEID < '10000'                       
000174          MOVE IN-SITEID                 TO WS-SITEID                     
000175          MOVE ZERO                      TO TALLY                         
000176          INSPECT WS-SITEID TALLYING TALLY FOR LEADING ZEROES             
000177          IF TALLY = 5                                                    
000178             MOVE SPACE                  TO IN-SITEID                     
000179          ELSE                                                            
000180             MOVE WS-SITEID (TALLY + 1:) TO IN-SITEID                     
000181          END-IF                                                          
000182       END-IF                                                             
000183                                                                          
000184       IF IN-SITEID NOT = SPACE                                           
000185          MOVE IN-SITEID TO W-IDLEVNR                                     
000186                            W-WDK6ASEQ                                    
000187          PERFORM IMS-GET-WDF101                                          
000188          IF SEGMENT-FINNS                                                
000189            PERFORM B-AENDRA-LEV                                          
000190          ELSE                                                            
000191            IF IN-CDUPDAT NOT = 'D'                                       
000192              PERFORM CA-NYUPPLAGG                                        
000193            END-IF                                                        
000194          END-IF                                                          
000195       END-IF                                                             
000196       PERFORM S01-LAES-A31481                                            
000197     END-PERFORM                                                          
000198                                                                          
000199                                                                          
000200     PERFORM Z-FINIT                                                      
000201                                                                          
000202     MOVE ZERO TO RETURN-CODE                                             
000203     GOBACK                                                               
000204     .                                                                    
000205     EJECT                                                                
000206 A-INIT SECTION.                                                          
000207     SKIP2                                                                
000208                                                                          
000209     PERFORM IMS-RESTART                                                  
000210                                                                          
000211     OPEN INPUT A31481                                                    
000212                                                                          
000213     ACCEPT DAGENS-DATUM   FROM DATE                                      
000214     MOVE DAGENS-DATUM-AAR TO WS-AA                                       
000215     MOVE 0101             TO WS-0101                                     
000216                                                                          
000217                                                                          
000218     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
000219     .                                                                    
000220     EJECT                                                                
000221 B-AENDRA-LEV SECTION.                                                    
000222     SKIP2                                                                
000223                                                                          
000224     IF IN-CDUPDAT = 'D'                                                  
000225*       OBS BORTTAG                                                       
000226*       OBS FINNS ARTIKLAR KVAR PÅ WDK6 MED DENNA LEV ?                   
000227        PERFORM IMS-GET-WDK601-LEV                                        
000228        IF SEGMENT-FINNS                                                  
000229           MOVE IN-DATUPD  TO LEV-DATUM-BORT                              
000230           PERFORM IMS-REPL-WDF101                                        
000231        ELSE                                                              
000232           IF LEV-FLRSADR NOT = 'J'                                       
000233              PERFORM IMS-DLET-WDF101                                     
000234           END-IF                                                         
000235        END-IF                                                            
000236     ELSE                                                                 
000237        IF LEV-FLRSADR NOT = 'J'                                          
000238*          KOLLA FÖRST KDSPRAK / WDF101                                   
000239           PERFORM S20-KOLL-KDSPRAK                                       
000240           IF W-KDSPRAK NOT = LEV-KDSPRAK                                 
000241              MOVE W-KDSPRAK TO LEV-KDSPRAK                               
000242              PERFORM IMS-REPL-WDF101                                     
000243           END-IF                                                         
000244                                                                          
000245           PERFORM IMS-GET-WDF106                                         
000246                                                                          
000247           IF SEGMENT-FINNS                                               
000248              PERFORM BA-FYLL-I-ADR                                       
000249                                                                          
000250              PERFORM IMS-REPL-WDF106                                     
000251***********ELSE                                                           
000252************* HÄR BORDE LIGGA EN ISRT AV WDF106 ?                         
000253           END-IF                                                         
000254        END-IF                                                            
000255     END-IF                                                               
000256     .                                                                    
000257     EJECT                                                                
000258 BA-FYLL-I-ADR SECTION.                                                   
000259     SKIP2                                                                
000260                                                                          
000261*    MOVE 1                TO ADR-IDLEVSUF                                
000262     MOVE IN-SITENAME      TO ADR-BELEV-VCC                               
000263     MOVE IN-SITEADR1      TO ADR-ADLEV-RAD1                              
000264     IF IN-SITEADR1 = SPACE                                               
000265        MOVE IN-SITEADR    TO ADR-ADLEV-RAD1                              
000266     END-IF                                                               
000267     MOVE IN-SITEADR2      TO ADR-ADLEV-RAD2-VCC                          
000268     MOVE IN-SITEPADR      TO ADR-ADLEV-ORT-VCC                           
000269     MOVE IN-SITECTRY      TO ADR-ADLEVLND                                
000270     MOVE IN-PHONE         TO ADR-IDLEVTLF                                
000271*    MOVE SPACE            TO ADR-IDLEVTLX                                
000272     MOVE IN-FAX           TO ADR-IDLEVFAX                                
000273     MOVE IN-CDCTRY        TO ADR-IDLANDX2                                
000274     MOVE IN-VAT           TO ADR-IDVAT                                   
000275     .                                                                    
000276     EJECT                                                                
000277 CA-NYUPPLAGG  SECTION.                                                   
000278     SKIP2                                                                
000279                                                                          
000280     PERFORM S20-KOLL-KDSPRAK                                             
000281                                                                          
000282     MOVE +1                     TO W-KDVALLEV                            
000283     MOVE +1                     TO VAL-IX                                
000284     PERFORM UNTIL VAL-IX > VAL-IX-MAX                                    
000285        IF IN-CDCTRY = VAL-IDLANDX2 (VAL-IX)                              
000286           MOVE VAL-KDVALUTA (VAL-IX) TO W-KDVALLEV                       
000287           MOVE VAL-IX-MAX            TO VAL-IX                           
000288        END-IF                                                            
000289        ADD +1 TO VAL-IX                                                  
000290     END-PERFORM                                                          
000291                                                                          
000292     MOVE IN-SITEID              TO LEV-IDLEVNR   W-IDLEVNR               
000293                                                  W-WDK6ASEQ              
000294     MOVE ZERO                   TO LEV-KDLEVTYP                          
000295     MOVE W-KDSPRAK              TO LEV-KDSPRAK                           
000296     MOVE 'N'                    TO LEV-FLRSADR                           
000297     MOVE 1                      TO LEV-KDGK                              
000298     MOVE 3                      TO LEV-KVDAGAR-TTC1                      
000299     MOVE 3                      TO LEV-KVDAGAR-TTC2                      
000300     MOVE ZERO                   TO LEV-KVDAGAR-AVIAVV                    
000301     MOVE ZERO                   TO LEV-KVDAGAR-INLAVV                    
000302     MOVE ZERO                   TO LEV-KVVECKOR-LVAR                     
000303     MOVE 2                      TO LEV-KVVECKOR-LT                       
000304     MOVE 24                     TO LEV-KVVECKOR-AT                       
000305     MOVE ZERO                   TO LEV-IDLPKOLL                          
000306     MOVE 005                    TO LEV-IDANSK-PG (1)                     
000307     MOVE ZERO                   TO LEV-IDANSK-PG (2)                     
000308     MOVE ZERO                   TO LEV-IDANSK-PG (3)                     
000309     MOVE ZERO                   TO LEV-IDANSK-PG (4)                     
000310     MOVE 005                    TO LEV-IDANSK-PG (5)                     
000311     MOVE ZERO                   TO LEV-IDANSK-PG (6)                     
000312     MOVE ZERO                   TO LEV-IDANSK-PG (7)                     
000313     MOVE ZERO                   TO LEV-IDANSK-PG (8)                     
000314     MOVE ZERO                   TO LEV-TILEVDAG  (1)                     
000315     MOVE ZERO                   TO LEV-TILEVDAG  (2)                     
000316     MOVE ZERO                   TO LEV-TILEVDAG  (3)                     
000317     MOVE ZERO                   TO LEV-TILEVDAG  (4)                     
000318     MOVE ZERO                   TO LEV-TILEVDAG  (5)                     
000319     MOVE SPACE                  TO LEV-IDLEVNR-MOTSV                     
000320     MOVE ZERO                   TO LEV-DATUM-BORT                        
000321                                                                          
000322     PERFORM IMS-ISRT-WDF101                                              
000323                                                                          
000324     IF W-KDVALLEV > ZERO                                                 
000325       MOVE W-KDVALLEV           TO TULL-KDVALLEV                         
000326       MOVE WS-AA0101            TO TULL-TITULF                           
000327       MOVE 1.0812               TO TULL-RETULF-1                         
000328       MOVE 1.0812               TO TULL-RETULF-2                         
000329       MOVE 'SE'                 TO TULL-IDLANDX2                         
000330                                                                          
000331       PERFORM IMS-ISRT-WDF102                                            
000332     END-IF                                                               
000333                                                                          
000334     MOVE 1                TO ADR-IDLEVSUF                                
000335     MOVE IN-SITENAME      TO ADR-BELEV-VCC                               
000336     MOVE IN-SITEADR1      TO ADR-ADLEV-RAD1                              
000337     IF IN-SITEADR1 = SPACE                                               
000338        MOVE IN-SITEADR    TO ADR-ADLEV-RAD1                              
000339     END-IF                                                               
000340     MOVE IN-SITEADR2      TO ADR-ADLEV-RAD2-VCC                          
000341     MOVE IN-SITEPADR      TO ADR-ADLEV-ORT-VCC                           
000342     MOVE IN-SITECTRY      TO ADR-ADLEVLND                                
000343     MOVE IN-PHONE         TO ADR-IDLEVTLF                                
000344     MOVE SPACE            TO ADR-IDLEVTLX                                
000345     MOVE IN-FAX           TO ADR-IDLEVFAX                                
000346     MOVE IN-CDCTRY        TO ADR-IDLANDX2                                
000347     MOVE IN-VAT           TO ADR-IDVAT                                   
000348                                                                          
000349     PERFORM IMS-ISRT-WDF106                                              
000350     .                                                                    
000351     EJECT                                                                
000352 Z-FINIT SECTION.                                                         
000353                                                                          
000354                                                                          
000355     CLOSE A31481                                                         
000356     SKIP2                                                                
000357     MOVE 'S' TO POSTSUM-OPKOD                                            
000358     CALL POSTSUM USING POSTSUM-PARM                                      
000359     .                                                                    
000360     EJECT                                                                
000361 S01-LAES-A31481  SECTION.                                                
000362     SKIP2                                                                
000363     READ A31481 INTO IN-AREA                                             
000364     AT END                                                               
000365        SET END-OF-A31481 TO TRUE                                         
000366                                                                          
000367     NOT AT END                                                           
000368        MOVE 'A31481'   TO POSTSUM-FDNAMN                                 
000369        MOVE 'W21346D1' TO POSTSUM-DDNAMN2                                
000370        MOVE 'IN'       TO POSTSUM-TRANSTYP                               
000371        CALL POSTSUM USING POSTSUM-PARM                                   
000372                                                                          
000373        ADD 1 TO W-A31481-KVPOST-IN                                       
000374     END-READ                                                             
000375     .                                                                    
000376     EJECT                                                                
000377 S20-KOLL-KDSPRAK SECTION.                                                
000378     SKIP2                                                                
000379     MOVE ZERO         TO W-KDSPRAK                                       
000380     IF  IN-CDLANGUAGE = 'SE'                                             
000381       MOVE 0          TO W-KDSPRAK                                       
000382     ELSE                                                                 
000383       IF  IN-CDLANGUAGE = 'GB'                                           
000384         IF IN-CDCTRY = 'FR'                                              
000385           MOVE 2      TO W-KDSPRAK                                       
000386         ELSE                                                             
000387           MOVE 1      TO W-KDSPRAK                                       
000388         END-IF                                                           
000389       ELSE                                                               
000390         IF  IN-CDLANGUAGE = 'DE'                                         
000391           MOVE 4      TO W-KDSPRAK                                       
000392         ELSE                                                             
000393           IF  IN-CDLANGUAGE = 'FR'                                       
000394             MOVE 2    TO W-KDSPRAK                                       
000395           ELSE                                                           
000396             IF  IN-CDLANGUAGE = 'ES'                                     
000397               MOVE 3  TO W-KDSPRAK                                       
000398             ELSE                                                         
000399               MOVE 1  TO W-KDSPRAK                                       
000400             END-IF                                                       
000401           END-IF                                                         
000402         END-IF                                                           
000403       END-IF                                                             
000404     END-IF                                                               
000405     .                                                                    
000406     EJECT                                                                
000407 X-TAG-CHECKPOINT   SECTION.                                              
000408                                                                          
000409     DISPLAY ' ANT POST VID CHKP ' W-A31481-KVPOST-IN                     
000410* --- VID CHECKPOINTTAGGNING SÅ TAPPAR MAN GN-POSITION I BASEN            
000411* --- SPARA DATABASNYCKLAR OM DET BEHÖVS                                  
000412     PERFORM IMS-CHECKPOINT                                               
000413     MOVE ZERO TO CHKP-ANT                                                
000414* --- LÄS OM DATABAS OM DET BEHÖVS                                        
000415     .                                                                    
000416     EJECT                                                                
000417* --- IMS SEKTIONER ---                                                   
000418                                                                          
000419 IMS-GET-WDF101 SECTION.                                                  
000420                                                                          
000421     STRING 'WDF101  (IDLEVNR  =' W-IDLEVNR-X ')'                         
000422          DELIMITED BY SIZE INTO SSA1                                     
000423     MOVE '  GE' TO GODK-STATUSKODER                                      
000424     CALL CBLTDLI USING GHU WDF1-PCB DLI-IO-WDF101 SSA1                   
000425     MOVE WDF1-STATUS-CODE TO STATUS-WS                                   
000426     PERFORM IMS-STATUSKONTROLL                                           
000427     .                                                                    
000428     SKIP3                                                                
000429 IMS-ISRT-WDF101 SECTION.                                                 
000430                                                                          
000431     MOVE 'WDF101 ' TO SSA1                                               
000432     MOVE '  II' TO GODK-STATUSKODER                                      
000433     CALL CBLTDLI USING ISRT WDF1-PCB DLI-IO-WDF101 SSA1                  
000434     MOVE WDF1-STATUS-CODE TO STATUS-WS                                   
000435     PERFORM IMS-STATUSKONTROLL                                           
000436     ADD +1  TO CHKP-ANT                                                  
000437     .                                                                    
000438     SKIP3                                                                
000439 IMS-REPL-WDF101 SECTION.                                                 
000440                                                                          
000441     MOVE '  ' TO GODK-STATUSKODER                                        
000442     CALL CBLTDLI USING REPL WDF1-PCB DLI-IO-WDF101                       
000443     MOVE WDF1-STATUS-CODE TO STATUS-WS                                   
000444     PERFORM IMS-STATUSKONTROLL                                           
000445     ADD +1  TO CHKP-ANT                                                  
000446     .                                                                    
000447     SKIP3                                                                
000448 IMS-DLET-WDF101 SECTION.                                                 
000449                                                                          
000450     MOVE '  ' TO GODK-STATUSKODER                                        
000451     CALL CBLTDLI USING DLET WDF1-PCB DLI-IO-WDF101                       
000452     MOVE WDF1-STATUS-CODE TO STATUS-WS                                   
000453     PERFORM IMS-STATUSKONTROLL                                           
000454     ADD +1  TO CHKP-ANT                                                  
000455     .                                                                    
000456     EJECT                                                                
000457 IMS-ISRT-WDF102 SECTION.                                                 
000458                                                                          
000459     STRING 'WDF101  (IDLEVNR  =' W-IDLEVNR-X ')'                         
000460          DELIMITED BY SIZE INTO SSA1                                     
000461     MOVE 'WDF102 ' TO SSA2                                               
000462     MOVE '  II' TO GODK-STATUSKODER                                      
000470     CALL CBLTDLI USING ISRT WDF1-PCB DLI-IO-WDF102 SSA1 SSA2             
000471     MOVE WDF1-STATUS-CODE TO STATUS-WS                                   
000472     PERFORM IMS-STATUSKONTROLL                                           
000473     ADD +1  TO CHKP-ANT                                                  
000474     .                                                                    
000475     SKIP3                                                                
000476 IMS-GET-WDF106 SECTION.                                                  
000477                                                                          
000478     STRING 'WDF106  (IDLEVSUF =' W-IDLEVSUF-X ')'                        
000479          DELIMITED BY SIZE INTO SSA1                                     
000480     MOVE '  GE' TO GODK-STATUSKODER                                      
000490     CALL CBLTDLI USING GHNP WDF1-PCB DLI-IO-WDF106 SSA1                  
000491     MOVE WDF1-STATUS-CODE TO STATUS-WS                                   
000492     PERFORM IMS-STATUSKONTROLL                                           
000493     .                                                                    
000494     SKIP3                                                                
000495 IMS-ISRT-WDF106 SECTION.                                                 
000496                                                                          
000497     STRING 'WDF101  (IDLEVNR  =' W-IDLEVNR-X ')'                         
000498          DELIMITED BY SIZE INTO SSA1                                     
000499     MOVE 'WDF106 ' TO SSA2                                               
000500     MOVE '  II' TO GODK-STATUSKODER                                      
000501     CALL CBLTDLI USING ISRT WDF1-PCB DLI-IO-WDF106 SSA1 SSA2             
000502     MOVE WDF1-STATUS-CODE TO STATUS-WS                                   
000503     PERFORM IMS-STATUSKONTROLL                                           
000504     ADD +1  TO CHKP-ANT                                                  
000505     .                                                                    
000506     SKIP3                                                                
000507 IMS-REPL-WDF106 SECTION.                                                 
000508                                                                          
000509     MOVE '  ' TO GODK-STATUSKODER                                        
000510     CALL CBLTDLI USING REPL WDF1-PCB DLI-IO-WDF106                       
000511     MOVE WDF1-STATUS-CODE TO STATUS-WS                                   
000512     PERFORM IMS-STATUSKONTROLL                                           
000513     ADD +1  TO CHKP-ANT                                                  
000514     .                                                                    
000515     EJECT                                                                
000516 IMS-GET-WDK601-LEV SECTION.                                              
000517                                                                          
000518     STRING 'WDK601  (WDK6ASEQ =' W-WDK6ASEQ-X ')'                        
000519          DELIMITED BY SIZE INTO SSA1                                     
000520     MOVE '  GE' TO GODK-STATUSKODER                                      
000521     CALL CBLTDLI USING GU WDK6A-PCB DLI-IO-WDK601 SSA1                   
000522     MOVE WDK6A-STATUS-CODE TO STATUS-WS                                  
000523     PERFORM IMS-STATUSKONTROLL                                           
000524     .                                                                    
000525     EJECT                                                                
000526 IMS-RESTART SECTION.                                                     
000527     SKIP2                                                                
000528     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
000529     MOVE '  ' TO GODK-STATUSKODER                                        
000530     CALL CBLTDLI USING XRST MSG-PCB                                      
000531                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
000532                        CHKP-AREA-LENGTH CHKP-AREA                        
000533     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
000534     PERFORM IMS-STATUSKONTROLL                                           
000535     .                                                                    
000536     SKIP3                                                                
000537 IMS-CHECKPOINT SECTION.                                                  
000538     SKIP2                                                                
000539     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
000540     MOVE '  XD' TO GODK-STATUSKODER                                      
000541     CALL CBLTDLI USING CHKP MSG-PCB                                      
000542                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
000543                        CHKP-AREA-LENGTH CHKP-AREA                        
000544     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
000545     PERFORM IMS-STATUSKONTROLL                                           
000546                                                                          
000547     IF IMS-EJ-OK                                                         
000548       MOVE 'IMS-KONTROLLREGION EJ TILLGÄNGLIG' TO FELTEXT-STR            
000549       DISPLAY FELTEXT                                                    
000550       CALL FELLOG                                                        
000551     END-IF                                                               
000552     .                                                                    
000553     EJECT                                                                
000554 IMS-STATUSKONTROLL SECTION.                                              
000555     SKIP2                                                                
000556     SET STATUS-IX TO 1                                                   
000557     SEARCH GODK-STATUS                                                   
000558       AT END                                                             
000559         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
000560           DELIMITED BY SIZE INTO FELTEXT                                 
000561         DISPLAY FELTEXT                                                  
000562         CALL FELLOG                                                      
000563       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
000564         CONTINUE                                                         
000565     END-SEARCH                                                           
000566     .                                                                    
