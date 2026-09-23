000001 ID DIVISION.                                                             
000002 PROGRAM-ID.             W4791000.                                        
000003 AUTHOR.                 LASSI OLGRENER.                                  
000004     DATE-WRITTEN.       FEB 2000.                                        
000005                                                                          
000006                                                                          
000007     REMARKS.                                                             
000008*    FUNKTION:                                                            
000009*         PROGRAMMETS UPPGIFT ÄR ATT LISTA FÄRDIGA ORDER                  
000010*         PÅ ORDERREGISTREN (WDE4,WDE6).                                  
000011*                                                                         
000012*         MAN UTGÅR IFRÅN FIL W47909 SOM ÄR EN KOPIA AV WDE4.             
000013*         FILEN ÄR SORTERAD PÅ IDGMTREF+IDPRODNR+IDSEGM.                  
000014*         DETTA GÖR ATT POSTERNA ÄR GRUPPERADE PER PRODNR,                
000015*         01-SEGM FÖRST MED EV OLIKA PLKLST OCH SEN RESP. 11-SEGM.        
000016*                                                                         
000017*         BORTTAGNINGSREGEL:                                              
000018*         KONTROLL MOT VO-ROT:                                            
000019*                KVKOLLI = KVKOLLI-FAKT = KVKOLLI-LAST   OCH              
000020*                KVORDRAD = KVORDRAD-PACK                                 
000021*           SAMT                                                          
000022*                LEDTIDEN UPPFYLLD, DVS ORDERN HAR LEVT DEN TID           
000023*                SOM LEDTIDEN ANGER (LEDTIDEN ÄR DISTRIKTS-               
000024*                BEROENDE OCH HÄMTAS FRÅN TABELL-W479T011)                
000025*                VID JÄMFÖRELSE MOT LEDTIDEN ANVÄNDS DET SENASTE          
000026*                AV LASTNINGSDATUM RESP.FAKTURADATUM OM NÅGOT AV          
000027*                DEM ÄR SATT, ANNARS ANVÄNDS BEG. PACK.DAG.               
000028*                                                                         
000029*         KONTROLL MOT KOLLI:                                             
000030*                FAKTURANUMMER OCH FAKTURADATUM SKALL FINNAS              
000031*                (OM KONTROLL MOT KOLLI EJ ÄR UPPFYLLD SÅ                 
000032*                 AVBRYT MED ABEND)                                       
000033*    ABEND: 1/  FELAKTIG DATUMKONVERTERING AV DATUM: ÅÅMMDD               
000034*                                                                         
000035*           2/  DISTRIKT SAKNAS I LEDTIDSTABELL: NNNN                     
000036*                                                                         
000037*               -VISAR VILKET DISTRIKT SOM SAKNAS                         
000038*                                                                         
000039*           3/  VOLVOORDER EJ KLAR FÖR BORTTAG: NNNNN                     
000040*                                                                         
000041*               DENNA VOLVOORDER HAR NÅGOT KOLLI SOM SAKNAR               
000042*               FAKTURADATUM ELLER FAKTURANUMMER OCH KAN DÄRFÖR           
000043*               EJ TAS BORT                                               
000044*                                                                         
000045     EJECT                                                                
000046 ENVIRONMENT DIVISION.                                                    
000047 INPUT-OUTPUT SECTION.                                                    
000048 FILE-CONTROL.                                                            
000049                                                                          
000050     SELECT W47909 ASSIGN TO W47910D1.                                    
000051*------- WDE4-KOPIA                                                       
000052                                                                          
000053     SELECT W47910 ASSIGN TO W47910D2.                                    
000054*------- RENSNINGSPOSTER E4, Q1, Q2, Q3                                   
000055                                                                          
000056     SELECT W47912 ASSIGN TO W47910D3.                                    
000057*------- RENSNINGSPOSTER E6                                               
000058                                                                          
000059     SELECT W479A5 ASSIGN TO W47910D5.                                    
000060*------- RENSNINGSPOSTER A5                                               
000061     EJECT                                                                
000062 DATA DIVISION.                                                           
000063 FILE SECTION.                                                            
000064                                                                          
000065 FD  W47909                                                               
000066     RECORDING V                                                          
000067     BLOCK CONTAINS 0.                                                    
000068 01  -COPY W479E401 -L.                                                   
000069 01  -COPY W479E411 -L.                                                   
000070     SKIP2                                                                
000071                                                                          
000072 FD  W47910                                                               
000073     RECORDING F                                                          
000074     BLOCK CONTAINS 0.                                                    
000075 01  POST  -COPY W479010  -PRE UT-  -L.                                   
000076     SKIP2                                                                
000077                                                                          
000078 FD  W47912                                                               
000079     RECORDING F                                                          
000080     BLOCK CONTAINS 0.                                                    
000081 01  POST -COPY W479012   -PRE UT2-  -L.                                  
000082     SKIP2                                                                
000083                                                                          
000084 FD  W479A5                                                               
000085     RECORDING F                                                          
000086     BLOCK CONTAINS 0.                                                    
000087 01  POST -COPY W479A5    -PRE UT4-  -L.                                  
000088     EJECT                                                                
000089 WORKING-STORAGE SECTION.                                                 
000090     SKIP2                                                                
000091*    -COPY WY2000W4                                                       
000092     SKIP3                                                                
000093*    -COPY WY2000W1                                                       
000094     SKIP3                                                                
000095 77  IDPGM                   PIC X(8)   VALUE 'W4791000'.                 
000096 77  JA                      PIC X(1)   VALUE 'J'.                        
000097 77  NEJ                     PIC X(1)   VALUE 'N'.                        
000098 77  WS-LEDTID               PIC 9(2).                                    
000099 77  WS-IDPRODNR             PIC  9(7)  VALUE ZERO.                       
000100 77  LO-IX                   PIC S9(3)  VALUE  +0   COMP  SYNC.           
000101 77  INDX                    PIC S9(3)  VALUE  +0   COMP  SYNC.           
000102 77  MAX-INDX                PIC S9(3)  VALUE +200  COMP  SYNC.           
000103     EJECT                                                                
000104 01  WS-IDPLKLST-TAB.                                                     
000105   03  WS-IDPLKLST OCCURS 200  PIC S9(3)  VALUE +0 COMP-3.                
000106                                                                          
000107 01   TEST-IDDISTR               PIC S9(5)   VALUE +0 COMP-3.             
000108 01   FILLER  -COPY WWDIST19    -RED TEST-IDDISTR.                        
000109                                                                          
000110 01  DAGENS-DATUM-AADDD          PIC 9(5)    VALUE ZERO.                  
000111                                                                          
000112 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
000113 01  FILLER REDEFINES DAGENS-DATUM.                                       
000114     03  DAGENS-DATUM-AAR        PIC 9(2).                                
000115     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
000116     03  DAGENS-DATUM-DAG        PIC 9(2).                                
000117                                                                          
000118 01  ORDER-DATUM-AADDD   PIC 9(5).                                        
000119 01  FILLER              REDEFINES ORDER-DATUM-AADDD.                     
000120     03  ORDER-DAT-AA    PIC 9(2).                                        
000121     03  ORDER-DAT-DDD   PIC 9(3).                                        
000122                                                                          
000123 01  W47909-EOF-SW               PIC X       VALUE 'N'.                   
000124     88  END-OF-W47909                       VALUE 'J'.                   
000125                                                                          
000126 01  RENSNING-SW             PIC X   VALUE 'J'.                           
000127     88  RENSNING                    VALUE 'J'.                           
000128     88  RENSNING-EJ-AKTUELL         VALUE 'N'.                           
000129     EJECT                                                                
000130 01  DYNAMISKA-SUBPROGRAM.                                                
000131*                                                                         
000132     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
000133     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
000134     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
000135     03  DATKORT                 PIC X(8)    VALUE 'DATKORT'.             
000136     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
000137     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
000138     SKIP2                                                                
000139*    --- PARAMETRAR TILL ABEND                                            
000140                                                                          
000141 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
000142     SKIP2                                                                
000143 01  FELTEXT.                                                             
000144     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
000145     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
000146     EJECT                                                                
000147*    --- PARAMETRAR TILL DATKORT                                          
000148*                                                                         
000149 01  PROGRAM-NAMN                PIC X(6)    VALUE 'W47910'.              
000150     SKIP2                                                                
000151 01  DATUMKORT-ID                PIC X(6)    VALUE 'WDATUM'.              
000152     SKIP2                                                                
000153*01  -COPY WDATKORT                                                       
000154     EJECT                                                                
000155*    --- PARAMETRAR TILL WDATKONV                                         
000156 01  FILLER                  PIC X(16) VALUE 'WDATAREA'.                  
000157*01  -COPY WDATAREA                                                       
000158     EJECT                                                                
000159*    --- PARAMETRAR TILL POSTSUM                                          
000160*                                                                         
000161*01  -COPY W0005   -PRE  POSTSUM-                                         
000162     EJECT                                                                
000163 01  FILLER                  PIC X(16) VALUE 'LEDTIDSTABELL'.             
000164 01  LEDTIDSTABELL.                                                       
000165*    03 TABELL   -COPY W479T011.                                          
000166     SKIP2                                                                
000167     03 L-TAB    REDEFINES TABELL OCCURS 80                               
000168                 INDEXED BY TAB-IX.                                       
000169                                                                          
000170        05 TAB-IDDISTR-FROM     PIC 9(4).                                 
000171        05 FILLER               PIC X(1).                                 
000172        05 TAB-IDDISTR-TOM      PIC 9(4).                                 
000173        05 FILLER               PIC X(1).                                 
000174        05 TAB-KVVECKA          PIC 9(2).                                 
000175                                                                          
000176     EJECT                                                                
000177 01  IN-AREA-START               PIC X(24)   VALUE                        
000178                                 'IN-AREA-START  '.                       
000179     SKIP2                                                                
000180                                                                          
000181 01  IN-AREA                     PIC X(50).                               
000182*01  AREA -COPY W479E401    -PRE E401- -RED IN-AREA.                      
000183*01  AREA -COPY W479E411    -PRE E411- -RED IN-AREA.                      
000184     EJECT                                                                
000185 01  UT-AREA-START               PIC X(24)   VALUE                        
000186                                 'UT-AREA-START  '.                       
000187     SKIP2                                                                
000188                                                                          
000189*01  AREA -COPY W479010     -PRE UT-                                      
000190     EJECT                                                                
000191 01  UT2-AREA-START              PIC X(24)   VALUE                        
000192                                 'UT2-AREA-START  '.                      
000193     SKIP2                                                                
000194                                                                          
000195*01  AREA -COPY W479012     -PRE UT2-                                     
000196     EJECT                                                                
000197 01  UT4-AREA-START              PIC X(24)   VALUE                        
000198                                 'UT4-AREA-START  '.                      
000199     SKIP2                                                                
000200                                                                          
000201*01  AREA -COPY W479A5      -PRE UT4-                                     
000202     EJECT                                                                
000203 01  NYCKLAR-TILL-DLI.                                                    
000204     03  W-IDORDER-X.                                                     
000205         05  W-IDORDER           PIC S9(7)      COMP-3.                   
000206                                                                          
000207     03  W-IDDC-X.                                                        
000208         05  W-IDDC              PIC  X(2).                               
000209                                                                          
000210     03  W-IDPRODNR-X.                                                    
000211         05  W-IDPRODNR          PIC S9(7)      COMP-3.                   
000212                                                                          
000213     03  W-KDODELST-R.                                                    
000214         05  W-KDODELSTR         PIC X(1)       VALUE 'R'.                
000215                                                                          
000216     03  W-KDODELST-U.                                                    
000217         05  W-KDODELSTU         PIC X(1)       VALUE 'U'.                
000218                                                                          
000219     03  W-WDQ301KY-MIN.                                                  
000220         05  W-Q3-IDORDER-MIN    PIC S9(7)      COMP-3.                   
000221         05  W-Q3-IDDC-MIN       PIC  X(2).                               
000222         05  FILLER              PIC X(6)       VALUE LOW-VALUE.          
000223                                                                          
000224     03  W-WDQ301KY-MAX.                                                  
000225         05  W-Q3-IDORDER-MAX    PIC S9(7)      COMP-3.                   
000226         05  W-Q3-IDDC-MAX       PIC  X(2).                               
000227         05  FILLER              PIC X(6)       VALUE HIGH-VALUE.         
000228     EJECT                                                                
000229*        ARBETSAREOR TILL IMS-SEKTIONERNA                                 
000230*                                                                         
000231 01  IMS-WS.                                                              
000232   03  FILLER                    PIC X(8)    VALUE 'IMS-WS  '.            
000233     SKIP3                                                                
000234*                            *** STATUSKOD FRÅN IMS                       
000235   03  STATUS-WS                 PIC XX.                                  
000236*                                                                         
000237     88  SEGMENT-FINNS                       VALUE '  '.                  
000238     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
000239     88  BASEN-SLUT                          VALUE 'GB'.                  
000240     SKIP3                                                                
000241   03  GODK-STATUSKODER.                                                  
000242     05  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
000243     SKIP2                                                                
000244   03  SSA1                      PIC X(144).                              
000245   03  SSA2                      PIC X(64).                               
000246     EJECT                                                                
000247*01  -COPY W0003                                                          
000248     EJECT                                                                
000249 01  FILLER                PIC X(16)   VALUE 'DLI-IO-WDE601'.             
000250 01  DLI-IO-WDE601.                                                       
000251*  03  -COPY WDE601                                                       
000252     EJECT                                                                
000253 01  FILLER                PIC X(16)   VALUE 'DLI-IO-WDQ201'.             
000254 01  DLI-IO-WDQ201.                                                       
000255*  03  -COPY WDQ201                                                       
000256     EJECT                                                                
000257 01  FILLER                PIC X(16)   VALUE 'DLI-IO-WDQ221'.             
000258 01  DLI-IO-WDQ221.                                                       
000259*  03  -COPY WDQ221                                                       
000260     EJECT                                                                
000261 01  FILLER                PIC X(16)   VALUE 'DLI-IO-WDQ301'.             
000262 01  DLI-IO-WDQ301.                                                       
000263*  03  -COPY WDQ301                                                       
000264     EJECT                                                                
000265 LINKAGE SECTION.                                                         
000266     SKIP2                                                                
000267                                                                          
000268*01  -COPY W0008 -PRE WDE6-                                               
000269     05  FILLER   PIC X(1).                                               
000270     EJECT                                                                
000271*01  -COPY W0008 -PRE WDQ3-                                               
000272     05  FILLER   PIC X(1).                                               
000273     EJECT                                                                
000274*01  -COPY W0008 -PRE WDQ2-                                               
000275     05  FILLER   PIC X(1).                                               
000276     EJECT                                                                
000277 PROCEDURE DIVISION USING  WDE6-PCB WDQ3-PCB WDQ2-PCB.                    
000278                                                                          
000279     ENTRY 'DLITCBL' USING WDE6-PCB WDQ3-PCB WDQ2-PCB.                    
000280                                                                          
000281     PERFORM A-INIT                                                       
000282                                                                          
000283     PERFORM S01-LAES-W47909                                              
000284     PERFORM UNTIL END-OF-W47909                                          
000285                                                                          
000286        MOVE JA                   TO RENSNING-SW                          
000287        MOVE +1                   TO INDX                                 
000288        MOVE E401-IDPRODNR        TO WS-IDPRODNR                          
000289        PERFORM UNTIL END-OF-W47909                       OR              
000290                      E401-IDSEGM = 'WDE411'              OR              
000291                     (E401-IDPRODNR NOT = WS-IDPRODNR)                    
000292          PERFORM B-SPARA-E401-DATA                                       
000293                                                                          
000294          PERFORM S01-LAES-W47909                                         
000295        END-PERFORM                                                       
000296                                                                          
000297        PERFORM C-KOLLA-E6                                                
000298        IF RENSNING                                                       
000299          PERFORM D-KOLLA-Q3-Q2                                           
000300          IF RENSNING                                                     
000301            PERFORM E-SKRIV-WDE-POSTER                                    
000302            PERFORM UNTIL END-OF-W47909    OR                             
000303                          E401-IDSEGM = 'WDE401'                          
000304              PERFORM F-SKRIV-A5-POSTER                                   
000305                                                                          
000306              PERFORM S01-LAES-W47909                                     
000307            END-PERFORM                                                   
000308          END-IF                                                          
000309        END-IF                                                            
000310        IF RENSNING-EJ-AKTUELL                                            
000311          PERFORM UNTIL END-OF-W47909    OR                               
000312                        E401-IDSEGM = 'WDE401'                            
000313            PERFORM S01-LAES-W47909                                       
000314          END-PERFORM                                                     
000315        END-IF                                                            
000316     END-PERFORM                                                          
000317                                                                          
000318     PERFORM Z-FINIT                                                      
000319     MOVE ZERO                  TO RETURN-CODE                            
000320     GOBACK                                                               
000321     .                                                                    
000322     EJECT                                                                
000323 A-INIT SECTION.                                                          
000324                                                                          
000325     OPEN INPUT  W47909                                                   
000326     OPEN OUTPUT W47910                                                   
000327                 W47912                                                   
000328                 W479A5                                                   
000329     CALL DATKORT USING PROGRAM-NAMN DATUMKORT-ID DATUMKORT               
000330     MOVE D-AAR       TO DAGENS-DATUM-AAR                                 
000331     MOVE D-MAANAD    TO DAGENS-DATUM-MAANAD                              
000332     MOVE D-DAG       TO DAGENS-DATUM-DAG                                 
000333     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
000334                                                                          
000335**   OMVANDLA DAGENS-DATUM TILL ÅR, DAGNR                                 
000336     MOVE DAGENS-DATUM        TO DAT-I-TIDATUM                            
000337     MOVE 'AAMMDD'            TO DAT-KDDATFORM                            
000338                                                                          
000339     CALL WDATKONV USING   DAT-KDDATFORM,                                 
000340                           DAT-I-TIDATUM,                                 
000341                           DAT-O-TIDATUM,                                 
000342                           DAT-KDSVAR                                     
000343                                                                          
000344     IF DAT-KDSVAR-OK                                                     
000345       MOVE DAT-TIAADDD         TO DAGENS-DATUM-AADDD                     
000346     ELSE                                                                 
000347       MOVE 'FEL FRÅN WDATKONV I A-SECTION'                               
000348                         TO FELTEXT-STR                                   
000349       PERFORM S99-ABEND                                                  
000350     END-IF                                                               
000351     .                                                                    
000352     EJECT                                                                
000353 B-SPARA-E401-DATA SECTION.                                               
000354                                                                          
000355     IF INDX = +1                                                         
000356       MOVE E401-IDORDER       TO W-IDORDER                               
000357       MOVE E401-IDDC          TO W-IDDC                                  
000358       PERFORM UNTIL INDX > MAX-INDX                                      
000359         MOVE ZERO             TO WS-IDPLKLST (INDX)                      
000360         ADD +1                TO INDX                                    
000361       END-PERFORM                                                        
000362       MOVE +1                 TO INDX                                    
000363     END-IF                                                               
000364                                                                          
000365     MOVE E401-IDPLKLST        TO WS-IDPLKLST(INDX)                       
000366     ADD +1 TO INDX                                                       
000367     IF INDX > MAX-INDX                                                   
000368       MOVE 'FLER ÄN 100 IDPLKLST: UTÖKA MAX-INDX' TO FELTEXT-STR         
000369       PERFORM S99-ABEND                                                  
000370     END-IF                                                               
000371     .                                                                    
000372     EJECT                                                                
000373 C-KOLLA-E6 SECTION.                                                      
000374                                                                          
000375     MOVE   WS-IDPRODNR            TO W-IDPRODNR                          
000376                                                                          
000377******FIX                                                                 
000378     IF W-IDPRODNR = 0                                                    
000379*    OR W-IDPRODNR = 433387 >> PRODNR ATT SKIPPA VID ABEND                
000380        MOVE NEJ TO RENSNING-SW                                           
000381     ELSE                                                                 
000382       PERFORM IMS-GU-WDE601                                              
000383       IF VORD-KVKOLLI = VORD-KVKOLLI-FAKT  AND                           
000384          VORD-KVKOLLI = VORD-KVKOLLI-LAST  AND                           
000385          VORD-KVKOLLI = VORD-KVKOLPAC      AND                           
000386          VORD-KVORDRAD = VORD-KVORDRAD-PACK                              
000387                                                                          
000388         PERFORM CA-HAMTA-LEDTID                                          
000389         PERFORM CB-KOLLA-DATUM-MOT-LEDTID                                
000390       ELSE                                                               
000391          MOVE NEJ TO RENSNING-SW                                         
000392       END-IF                                                             
000393     END-IF                                                               
000394*SLUTFIX                                                                  
000395     .                                                                    
000396     EJECT                                                                
000397                                                                          
000398 CA-HAMTA-LEDTID  SECTION.                                                
000399                                                                          
000400     SET TAB-IX TO +1                                                     
000401     SEARCH    L-TAB                                                      
000402     AT END                                                               
000403       MOVE 'DISTRIKT SAKNAS I LEDTIDSTAB.'                               
000404                         TO FELTEXT-STR                                   
000405       PERFORM S99-ABEND                                                  
000406     WHEN  VORD-IDDISTR NOT < TAB-IDDISTR-FROM (TAB-IX)                   
000407                    AND NOT > TAB-IDDISTR-TOM (TAB-IX)                    
000408                                                                          
000409           MOVE TAB-KVVECKA    (TAB-IX)    TO WS-LEDTID                   
000410                                                                          
000411     END-SEARCH                                                           
000412     .                                                                    
000413     EJECT                                                                
000414 CB-KOLLA-DATUM-MOT-LEDTID SECTION.                                       
000415*****************************************************************         
000416*    DET MEST AKTUELLA AV LASTN.DATUM RESP. FAKTURADATUM ANVÄNDS*         
000417*    VID JÄMFÖRELSE MED LEDTID.                                 *         
000418*    OM DESSA SAKNAS -> ANVÄND BEG.PACKDAG    (KOLLI SAKNAS)    *         
000419*****************************************************************         
000420     SKIP3                                                                
000421     IF VORD-TILASTN-SK > ZERO  OR                                        
000422        VORD-TIFAKT-SK  > ZERO                                            
000423                                                                          
000424       MOVE VORD-TILASTN-SK         TO TMP1-YYMMDD                        
000425       MOVE VORD-TIFAKT-SK          TO TMP2-YYMMDD                        
000426       PERFORM WY2000P1                                                   
000427       IF TMP1-YYMMDD > TMP2-YYMMDD                                       
000428          MOVE VORD-TILASTN-SK      TO DAT-I-TIDATUM                      
000429       ELSE                                                               
000430          MOVE VORD-TIFAKT-SK       TO DAT-I-TIDATUM                      
000431       END-IF                                                             
000432     ELSE                                                                 
000433        MOVE VORD-TIUTSKR           TO DAT-I-TIDATUM                      
000434     END-IF                                                               
000435                                                                          
000436     MOVE 'AAMMDD'          TO DAT-KDDATFORM                              
000437     CALL  WDATKONV  USING     DAT-KDDATFORM,                             
000438                               DAT-I-TIDATUM,                             
000439                               DAT-O-TIDATUM,                             
000440                               DAT-KDSVAR                                 
000441                                                                          
000442     IF DAT-KDSVAR-FEL                                                    
000443       MOVE 'FEL FRÅN WDATKONV I CB-SECTION'                              
000444                         TO FELTEXT-STR                                   
000445       PERFORM S99-ABEND                                                  
000446     END-IF                                                               
000447                                                                          
000448     MOVE DAT-TIAADDD         TO ORDER-DATUM-AADDD                        
000449                                                                          
000450     COMPUTE ORDER-DATUM-AADDD = ORDER-DATUM-AADDD  +                     
000451             ( WS-LEDTID * 7 )                                            
000452*                                                                         
000453*    OBS ÅRSSKIFTE                                                        
000454*                                                                         
000455     IF ORDER-DAT-DDD  > 365                                              
000456        ADD 1    TO ORDER-DAT-AA                                          
000457        SUBTRACT 365 FROM ORDER-DAT-DDD                                   
000458     END-IF                                                               
000459                                                                          
000460     MOVE ORDER-DATUM-AADDD    TO TMP1-YYDDD                              
000461     MOVE DAGENS-DATUM-AADDD   TO TMP2-YYDDD                              
000462     PERFORM WY2000P4                                                     
000463     IF TMP1-YYDDD > TMP2-YYDDD                                           
000464        MOVE NEJ TO    RENSNING-SW                                        
000465     END-IF                                                               
000466     .                                                                    
000467     EJECT                                                                
000468 D-KOLLA-Q3-Q2 SECTION.                                                   
000469                                                                          
000470     MOVE W-IDORDER    TO W-Q3-IDORDER-MIN                                
000471                          W-Q3-IDORDER-MAX                                
000472     MOVE W-IDDC       TO W-Q3-IDDC-MIN                                   
000473                          W-Q3-IDDC-MAX                                   
000474     PERFORM IMS-GU-WDQ301                                                
000475     IF SEGMENT-SAKNAS                                                    
000476       PERFORM IMS-GU-WDQ201                                              
000477       IF SEGMENT-FINNS                                                   
000478         IF OHUV-FLKLAR = JA                                              
000479           PERFORM IMS-GNP-WDQ221                                         
000481           IF SEGMENT-FINNS                                               
000483              MOVE NEJ TO RENSNING-SW                                     
000484*FIX FÖR ATT KOLLA OM DENNA LÄSNING BEHÖVS                                
000485              DISPLAY 'Q221 RADER = ' OHUV-IDDISTR                        
000486                                      OHUV-IDKUNDNR                       
000487                                      OHUV-IDKUNDRF                       
000488                                      W-IDDC                              
000489                                      OHUV-TIREGDAT                       
000490           END-IF                                                         
000508         ELSE                                                             
000509           MOVE NEJ TO RENSNING-SW                                        
000510         END-IF                                                           
000511       END-IF                                                             
000512     ELSE                                                                 
000513       MOVE NEJ TO RENSNING-SW                                            
000514     END-IF                                                               
000515     .                                                                    
000516     EJECT                                                                
000517 E-SKRIV-WDE-POSTER SECTION.                                              
000518                                                                          
000519     MOVE '010'                    TO UT-IDPTYP                           
000520     MOVE W-IDORDER                TO UT-IDORDER                          
000521     MOVE E411-IDDISTR             TO UT-IDDISTR                          
000522                                      TEST-IDDISTR                        
000523     MOVE E411-IDKUNDNR            TO UT-IDKUNDNR                         
000524     MOVE E411-IDKUNDRF            TO UT-IDKUNDRF                         
000525     MOVE WS-IDPRODNR              TO UT-IDPRODNR                         
000526     MOVE W-IDDC                   TO UT-IDDC                             
000527     MOVE VORD-IDLEVNR             TO UT-IDLEVNR                          
000528                                                                          
000529     MOVE +1                      TO INDX                                 
000530     PERFORM UNTIL INDX > MAX-INDX OR                                     
000531                   WS-IDPLKLST (INDX) = ZERO                              
000532       MOVE WS-IDPLKLST (INDX)    TO UT-IDPLKLST                          
000533       PERFORM S11-SKRIV-W47910                                           
000534       ADD +1                     TO INDX                                 
000535     END-PERFORM                                                          
000536                                                                          
000537     MOVE WS-IDPRODNR             TO UT2-IDPRODNR                         
000538     PERFORM S12-SKRIV-W47912                                             
000539     .                                                                    
000540     EJECT                                                                
000541 F-SKRIV-A5-POSTER SECTION.                                               
000542                                                                          
000543     IF (NOT DIST19-SATS) AND E411-IDKUNDRF-RO > ZERO                     
000544       MOVE E411-IDDISTR        TO UT4-IDDISTR                            
000545       MOVE E411-IDKUNDNR       TO UT4-IDKUNDNR                           
000546       MOVE E411-IDKUNDRF-RO    TO UT4-IDKUNDRF                           
000547       MOVE E411-IDARTNR        TO UT4-IDARTNR                            
000548       MOVE E411-IDLOPNR-RO     TO UT4-IDLOPNR                            
000549                                                                          
000550       PERFORM S14-SKRIV-W479A5                                           
000551     END-IF                                                               
000552     .                                                                    
000553     EJECT                                                                
000554 Z-FINIT SECTION.                                                         
000555                                                                          
000556     CLOSE W47909                                                         
000557           W47910                                                         
000558           W47912                                                         
000559           W479A5                                                         
000560                                                                          
000561     MOVE 'S' TO POSTSUM-OPKOD                                            
000562     CALL POSTSUM USING POSTSUM-PARM                                      
000563     .                                                                    
000564     EJECT                                                                
000565 S01-LAES-W47909  SECTION.                                                
000566                                                                          
000567     READ W47909 INTO IN-AREA                                             
000568     AT END                                                               
000569        SET END-OF-W47909 TO TRUE                                         
000570                                                                          
000571     NOT AT END                                                           
000572        MOVE 'W47909'   TO POSTSUM-FDNAMN                                 
000573        MOVE 'W47910D1' TO POSTSUM-DDNAMN2                                
000574        MOVE 'IN-'      TO POSTSUM-TRANSTYP                               
000575        CALL POSTSUM USING POSTSUM-PARM                                   
000576     END-READ                                                             
000577     .                                                                    
000578     EJECT                                                                
000579 S11-SKRIV-W47910 SECTION.                                                
000580                                                                          
000581     WRITE UT-POST FROM UT-AREA                                           
000582                                                                          
000583     MOVE UT-IDPTYP TO POSTSUM-TRANSTYP                                   
000584     MOVE 'W47910' TO POSTSUM-FDNAMN                                      
000585     MOVE 'W47910D2' TO POSTSUM-DDNAMN2                                   
000586     CALL POSTSUM USING POSTSUM-PARM                                      
000587     .                                                                    
000588     EJECT                                                                
000589 S12-SKRIV-W47912 SECTION.                                                
000590                                                                          
000591     WRITE UT2-POST FROM UT2-AREA                                         
000592                                                                          
000593     MOVE 'E6'       TO POSTSUM-TRANSTYP                                  
000594     MOVE 'W47912'   TO POSTSUM-FDNAMN                                    
000595     MOVE 'W47910D3' TO POSTSUM-DDNAMN2                                   
000596     CALL POSTSUM USING POSTSUM-PARM                                      
000597     .                                                                    
000598     EJECT                                                                
000599 S14-SKRIV-W479A5 SECTION.                                                
000600                                                                          
000601     WRITE UT4-POST FROM UT4-AREA                                         
000602                                                                          
000603     MOVE 'A5'       TO POSTSUM-TRANSTYP                                  
000604     MOVE 'W479A5'   TO POSTSUM-FDNAMN                                    
000605     MOVE 'W47910D5' TO POSTSUM-DDNAMN2                                   
000606     CALL POSTSUM USING POSTSUM-PARM                                      
000607     .                                                                    
000608     EJECT                                                                
000609 S99-ABEND SECTION.                                                       
000610                                                                          
000611     SKIP2                                                                
000612     MOVE 'S' TO POSTSUM-OPKOD                                            
000613     CALL POSTSUM USING POSTSUM-PARM                                      
000614     CALL ABEND USING RKOD-ABEND-UTAN-DUMP                                
000615     .                                                                    
000616     EJECT                                                                
000617                                                                          
000618**************************************************************            
000619* IMS SEKTIONER                                              *            
000620*                                                            *            
000621**************************************************************            
000622 IMS-GU-WDE601          SECTION.                                          
000623                                                                          
000624     STRING 'WDE601  (IDPRODNR =' W-IDPRODNR-X  ')'                       
000625          DELIMITED BY SIZE INTO SSA1                                     
000626     MOVE  '    '            TO GODK-STATUSKODER                          
000627     CALL CBLTDLI USING GU WDE6-PCB DLI-IO-WDE601 SSA1                    
000628     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
000629     PERFORM IMS-STATUSKONTROLL                                           
000630     .                                                                    
000631     SKIP3                                                                
000632 IMS-GU-WDQ301      SECTION.                                              
000633                                                                          
000634     STRING 'WDQ301  (WDQ301KY>=' W-WDQ301KY-MIN                          
000635                    '&WDQ301KY<=' W-WDQ301KY-MAX                          
000636                    '&KDODELST =' W-KDODELST-U                            
000637                    '!WDQ301KY>=' W-WDQ301KY-MIN                          
000638                    '&WDQ301KY<=' W-WDQ301KY-MAX                          
000639                    '&KDODELST =' W-KDODELST-R ')'                        
000640            DELIMITED BY SIZE INTO SSA1                                   
000641     MOVE  '  GE'            TO GODK-STATUSKODER                          
000642     CALL CBLTDLI USING GU   WDQ3-PCB DLI-IO-WDQ301 SSA1                  
000643     MOVE WDQ3-STATUS-CODE TO STATUS-WS                                   
000644     PERFORM IMS-STATUSKONTROLL                                           
000645     .                                                                    
000646     EJECT                                                                
000647 IMS-GU-WDQ201      SECTION.                                              
000648                                                                          
000649     STRING 'WDQ201  (IDORDER  =' W-IDORDER-X ')'                         
000650            DELIMITED BY SIZE INTO SSA1                                   
000651     MOVE  '  GE'            TO GODK-STATUSKODER                          
000652     CALL CBLTDLI USING GU   WDQ2-PCB DLI-IO-WDQ201 SSA1                  
000653     MOVE WDQ2-STATUS-CODE TO STATUS-WS                                   
000654     PERFORM IMS-STATUSKONTROLL                                           
000655     .                                                                    
000656                                                                          
000657 IMS-GNP-WDQ221 SECTION.                                                  
000658                                                                          
000659     STRING 'WDQ212  (IDDC     =' W-IDDC-X ')'                            
000660            DELIMITED BY SIZE INTO SSA1                                   
000661     MOVE   'WDQ221'            TO SSA2                                   
000662     MOVE  '  GE'               TO GODK-STATUSKODER                       
000663     CALL CBLTDLI USING GNP  WDQ2-PCB DLI-IO-WDQ221 SSA1 SSA2             
000664     MOVE WDQ2-STATUS-CODE TO STATUS-WS                                   
000665     PERFORM IMS-STATUSKONTROLL                                           
000666     .                                                                    
000667     EJECT                                                                
000668 IMS-STATUSKONTROLL SECTION.                                              
000669                                                                          
000670      SET STATUS-IX TO 1                                                  
000671     SEARCH GODK-STATUS                                                   
000672        AT END CALL FELLOG                                                
000673        WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                 
000674     END-SEARCH                                                           
000675     .                                                                    
000676     EJECT                                                                
000677*    -COPY WY2000P1                                                       
000678     EJECT                                                                
000680*    -COPY WY2000P4                                                       
