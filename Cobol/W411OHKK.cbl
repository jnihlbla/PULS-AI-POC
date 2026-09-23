000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W411OHKK.                                                
000300 AUTHOR.         GÖRAN KJELLSON   GUIDE                                   
000400 DATE-WRITTEN.   NOVEMBER  2006                                           
000500                                                                          
000600     REMARKS.                                                             
000700*                                                                         
000800*    FUNKTION.                                                            
000900*        FÖR KDCALL = 1 (KONTROLL AV ORDER)                               
001000*        KONTROLL OM  EN ORDER SKALL HANTERAS MED TVINGANDE               
001100*        TILLÄG (KDBEH = 1)  FLORDTIL = JA                                
001200*        OM FLORDTIL SÄTTS = JA KOLLAS OM DET FINNS NÅGON ORDER           
001300*        ATT LÄGGA TILL TILL IDKUNDRF = AKTUELL ORDER ELLER BLANK         
001400*                                                                         
001500*        KONTROLL OM ORDERN BERÖRS AV DEN UTÖKADE TPO2-HANTERINGEN        
001600*        DVS. RADER SOM INTE KAN.LEVERERAS FRÅN SDC/LDC                   
001700*        GENERERAR EN TPO2 I STÄLLET FÖR EN CLEARING                      
001800*        DIREKT TILL DC 11 (KDBEH = 2)    IDSCHEMA > 0                    
001900*       (IDSCHEMA BORTTAGET HÖSTEN 2019 1453008)                          
002000*                                                                         
002100*                                                                         
002200*        FÖR KDCALL = 2 (KONTROLL ENDAST MOT DISTRIKT)                    
002300*                                                                         
002400*                                                                         
002500*    LÄNKAREA: W411OHKK                                                   
002600*    CHANGE LOG:                                                          
002700*    STORY 2373879 / REPLACE DIST35- 88 LEVELS OF IN,KR,                  
002800*     AE,CN CDC RETURNS TO DIST35-CDC-RETURNS-NON-VCC                     
002900                                                                          
003000                                                                          
003100 ENVIRONMENT DIVISION.                                                    
003200 DATA DIVISION.                                                           
003300 WORKING-STORAGE SECTION.                                                 
003400                                                                          
003500 77  IDPGM                       PIC X(08)   VALUE 'W411OHKK'.            
003600 77  JA                          PIC X       VALUE 'J'.                   
003700 77  YES                         PIC X       VALUE 'Y'.                   
003800 77  NEJ                         PIC X       VALUE 'N'.                   
003900 77  TPO2-HANTERING              PIC S9      VALUE +1.                    
004000 77  TVINGANDE-TILLAEGG          PIC S9      VALUE +2.                    
004100 77  FELTEXT                     PIC X(20)   VALUE SPACE.                 
004200                                                                          
004300 77  CURRENT-SECTION             PIC X(16)   VALUE 'MAIN'.                
004400 77  CURRENT-IMS-SECTION         PIC X(16)   VALUE SPACE.                 
004500                                                                          
004600 77  SW-STD-ORDER                PIC X(1)    VALUE 'N'.                   
004700     88 STD-ORDER                            VALUE 'J'.                   
004800                                                                          
004900 77  DISTRIKT-SW                 PIC X       VALUE 'N'.                   
005000     88  DISTRIKT-OK                         VALUE 'J'.                   
005100                                                                          
005200 77  TARGET-SW                   PIC X       VALUE 'N'.                   
005300     88  TARGET-OK                           VALUE 'J'.                   
005400                                                                          
005500 01  TEST-IDDISTR                PIC S9(5)   COMP-3.                      
005600                                                                          
005700*    *** NORDAMERIKA+PACIFIKDISTRIKT  *****                               
005800 01  FILLER REDEFINES TEST-IDDISTR.                                       
005900*    03     -COPY WWDIST07.                                               
006000                                                                          
006100*    *** SKROTDISTRIKT  ***********                                       
006200 01  FILLER REDEFINES TEST-IDDISTR.                                       
006300*    03     -COPY WWDIST18.                                               
006400                                                                          
006500*    *** SATSDISTRIKT   ***********                                       
006600 01  FILLER REDEFINES TEST-IDDISTR.                                       
006700*    03     -COPY WWDIST19.                                               
006800                                                                          
006900*    *** RETUR- REFILLDISTRIKT ****                                       
007000 01  FILLER REDEFINES TEST-IDDISTR.                                       
007100*    03     -COPY WWDIST35.                                               
007200                                                                          
007300                                                                          
007400*    --- SUBPROGRAM OCH PARAMETERAREOR                                    
007500 01  GENERELLA-SUBPROGRAM.                                                
007600     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
007700     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007800 01  GEMENSAMMA-SUBPROGRAM.                                               
007900                                                                          
008000*        LÄSNING AV KUNDREGISTRET                                         
008100     03  W411KREG                PIC X(8)    VALUE 'W411KREG'.            
008200     03  W413TAVG                PIC X(8)    VALUE 'W413TAVG'.            
008300                                                                          
008400                                                                          
008500*    --- PARAMETRAR TILL GEMENSAMMA SUBPROGRAM                            
008600 01 FILLER                       PIC X(8)    VALUE 'W411KREG'.            
008700*   -COPY W411KREG                                                        
008800                                                                          
008900 01 FILLER                       PIC X(8)    VALUE 'W413TAVG'.            
009000*   -COPY W413TAVG                                                        
009100                                                                          
009200*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
009300 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
009400                                                                          
009500 01  NYCKLAR-TILL-DLI.                                                    
009600     03  W-IDORDER-X.                                                     
009700         05  W-IDORDER           PIC S9(7)   VALUE +0 COMP-3.             
009800                                                                          
009900     03  W-WDQ2ESEQ-X.                                                    
010000         05  W-IDDISTR-WDQ2      PIC S9(5)   VALUE +0 COMP-3.             
010100         05  W-IDKUNDNR-WDQ2     PIC S9(7)   VALUE +0 COMP-3.             
010200         05  W-KDORDKL-WDQ2      PIC S9      VALUE +0 COMP-3.             
010300                                                                          
010400     03  W-IDGMTREF.                                                      
010500         05  W-IDDISTR-WDQ2C     PIC S9(5)   VALUE +0 COMP-3.             
010600         05  W-IDKUNDNR-WDQ2C    PIC S9(7)   VALUE +0 COMP-3.             
010700         05  W-IDORDNR-WDQ2C     PIC  9(7)   VALUE ZERO.                  
010800         05  FILLER              PIC  X(3)   VALUE SPACE.                 
010900                                                                          
011000     03  W-IDDC-X.                                                        
011100         05  W-IDDC-WDQ212       PIC X(2)    VALUE SPACE.                 
011200*                                                                         
011300*    --- STATUS-KOD FRÅN IMS                                              
011400 01  STATUS-WS                   PIC XX.                                  
011500     88  SEGMENT-FINNS                       VALUE '  '.                  
011600     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
011700     88  BASEN-SLUT                          VALUE 'GB'.                  
011800                                                                          
011900 01  GODK-STATUSKODER.                                                    
012000     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
012100                                                                          
012200 01  SSA1                        PIC X(64).                               
012300                                                                          
012400*    --- IMS FUNKTIONSKODER                                               
012500*01  -COPY W0003                                                          
012600                                                                          
012700*    ---  DLI INPUT-OUTPUT AREA                                           
012800 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
012900                                                                          
013000 01  FILLER                      PIC X(16)   VALUE 'WDQ201-AREA'.         
013100 01  DLI-IO-AREA-WDQ201.                                                  
013200*    03  -COPY WDQ201                                                     
013300                                                                          
013400 01  FILLER                      PIC X(16)   VALUE 'WDQ212-AREA'.         
013500 01  DLI-IO-AREA-WDQ212.                                                  
013600*    03  -COPY WDQ212                                                     
013700                                                                          
013800 01  FILLER                      PIC X(16)   VALUE 'WDQ2C-AREA '.         
013900 01  DLI-IO-AREA-WDQ2C.                                                   
014000*    03  -COPY WDQ2C1                                                     
014100                                                                          
014200 LINKAGE SECTION.                                                         
014300                                                                          
014400*   -COPY W411OHKK                                                        
014500                                                                          
014600*01  -COPY W0008      -PRE WDQ2-                                          
014700     05  FILLER                  PIC X.                                   
014800                                                                          
014900*01  -COPY W0008      -PRE WDQ2-UPD-                                      
015000     05  FILLER                  PIC X.                                   
015100                                                                          
015200*01  -COPY W0008      -PRE WDQ2C-                                         
015300     05  FILLER                  PIC X.                                   
015400                                                                          
015500 01  KREG-GMTA-PCB               PIC X.                                   
015600 01  KREG-GMTB-PCB               PIC X.                                   
015700 01  KREG-GMTC-PCB               PIC X.                                   
015800 01  KREG-BETC-PCB               PIC X.                                   
015900                                                                          
016000 01  TAVG-WDB2-PCB               PIC X.                                   
016100 01  TAVG-WDB3-PCB               PIC X.                                   
016200 01  TAVG-WDB5-PCB               PIC X.                                   
016300 01  TAVG-WDP7-PCB               PIC X.                                   
016400 01  TAVG-XXKB-PCB               PIC X.                                   
016500                                                                          
016600 PROCEDURE DIVISION  USING OHKK-W411OHKK                                  
016700                           WDQ2-PCB      WDQ2-UPD-PCB  WDQ2C-PCB          
016800                           KREG-GMTA-PCB KREG-GMTB-PCB                    
016900                           KREG-GMTC-PCB KREG-BETC-PCB                    
017000                           TAVG-WDB2-PCB TAVG-WDB3-PCB                    
017100                           TAVG-WDB5-PCB TAVG-WDP7-PCB                    
017200                           TAVG-XXKB-PCB.                                 
017300 MAIN SECTION.                                                            
017400                                                                          
017500     PERFORM A-INIT                                                       
017600                                                                          
017700     IF OHKK-KDCALL = 1 AND OHKK-FLSOFT = NEJ                             
017800        PERFORM B-KOLLA-DISTRIKT                                          
017900        IF DISTRIKT-OK                                                    
018000           PERFORM C-KOLLA-STD-ORDER                                      
018100                                                                          
018200           IF STD-ORDER                                                   
018300              PERFORM D-KOLLA-TVINGANDE-TILLAEGG                          
018400              IF OHKK-FLORDTIL = JA                                       
018500                 PERFORM E-KOLLA-TARGET-ORDER                             
018600              END-IF                                                      
018700           END-IF                                                         
018800        END-IF                                                            
018900     END-IF                                                               
019000                                                                          
019100     IF OHKK-KDCALL = 2                                                   
019200        PERFORM B-KOLLA-DISTRIKT                                          
019300     END-IF                                                               
019400                                                                          
019500     GOBACK                                                               
019600     .                                                                    
019700                                                                          
019800                                                                          
019900 A-INIT              SECTION.                                             
020000     MOVE 'A-INIT'               TO CURRENT-SECTION                       
020100                                                                          
020200     MOVE ZERO          TO OHKK-IDORDER                                   
020300     MOVE ZERO          TO OHKK-IDORDNR-UT                                
020400     MOVE NEJ           TO OHKK-FLORDTIL                                  
020500     .                                                                    
020600                                                                          
020700                                                                          
020800 B-KOLLA-DISTRIKT    SECTION.                                             
020900     MOVE 'B-KOLLA-DISTRIKT '    TO CURRENT-SECTION                       
021000                                                                          
021100     MOVE OHKK-IDDISTR         TO TEST-IDDISTR                            
021200     IF (DIST07-NA-CUSTOMERS AND                                          
021300        (OHKK-KDFRAKT NOT = 11) AND                                       
021400        (OHKK-KDFRAKT NOT = 0)) OR                                        
021500        DIST07-AUSTRALIEN        OR                                       
021600        DIST07-KINA              OR                                       
021700        DIST18-SKROT             OR                                       
021800        DIST18-SCRAP-NDC         OR                                       
021900        DIST19-SATS              OR                                       
022000        DIST35-RETUR             OR                                       
022100        DIST35-REFILL            OR                                       
022200        DIST35-NONVCC-REFILL     OR                                       
022300        DIST35-NONVCC-VCC-TRANSFER OR                                     
022400        DIST35-NONVCC-NONVCC-TRANSFER OR                                  
022500        DIST35-REFILL-INOM-NDC   OR                                       
022600        DIST35-REFILL-INOM-JP    OR                                       
022700        DIST35-REFILL-NA-JAP     OR                                       
022800        DIST35-NA-CDC-RETURN     OR                                       
022900        DIST35-CN-NDC-RETURNS    OR                                       
023000        DIST35-CDC-RETURNS-NON-VCC OR                                     
023100        DIST35-NA-TRANSFER       OR                                       
023200        DIST35-NA-NDC-RETURNS    OR                                       
023300        DIST35-PACIFIC-TRANSFER  OR                                       
023400        DIST35-CN-TRANSFER       OR                                       
023500        DIST35-ST-CDC            OR                                       
023600        DIST35-NL-SITTARD-OBJEKT                                          
023700                                                                          
023800        MOVE NEJ TO DISTRIKT-SW                                           
023900     ELSE                                                                 
024000        MOVE JA  TO DISTRIKT-SW                                           
024100                    OHKK-FLORDTIL                                         
024200     END-IF                                                               
024300     .                                                                    
024400                                                                          
024500                                                                          
024600 C-KOLLA-STD-ORDER   SECTION.                                             
024700     MOVE 'C-KOLLA-STD-ORDER'    TO CURRENT-SECTION                       
024800                                                                          
024900     MOVE NEJ       TO SW-STD-ORDER                                       
025000                       OHKK-FLORDTIL                                      
025100     MOVE LOW-VALUE TO W-WDQ2ESEQ-X                                       
025200                                                                          
025300     IF OHKK-IDSYSTEM NOT = 'LDC '         AND                            
025400       (OHKK-KDORDKL = 1 OR 2 OR 3 OR 4) AND                              
025500        OHKK-FLAUTFAK = NEJ                AND                            
025600        OHKK-FLAUTPAC = NEJ                AND                            
025700        OHKK-FLEMBORD = NEJ                AND                            
025800        OHKK-FLFORBI = NEJ                 AND                            
025900        OHKK-FLORDSPE = NEJ                AND                            
026000        OHKK-FLOVRLEV = NEJ                AND                            
026100       (OHKK-FLLSBOK = JA OR YES)          AND                            
026200        OHKK-FLVORKO = NEJ                 AND                            
026300                                                                          
026400        OHKK-IDANALYS = SPACE              AND                            
026500        OHKK-IDBIPREF = SPACE              AND                            
026600        OHKK-IDDEPT = ZERO                 AND                            
026700        OHKK-IDFTG = ZERO                  AND                            
026800        OHKK-IDKAMPRF = ZERO               AND                            
026900        OHKK-IDKONTO = ZERO                AND                            
027000        OHKK-IDKST = SPACE                 AND                            
027100                                                                          
027200       (OHKK-KDFAKTYP = 'K' OR 'R' OR '+') AND                            
027300        OHKK-KDTPOTYP = ZERO               AND                            
027400        OHKK-KDVRINFO = ZERO               AND                            
027500                                                                          
027600        OHKK-TITPO = ZERO                                                 
027700                                                                          
027800        MOVE JA TO SW-STD-ORDER                                           
027900     END-IF                                                               
028000                                                                          
028100     .                                                                    
028200                                                                          
028300 D-KOLLA-TVINGANDE-TILLAEGG  SECTION.                                     
028400     MOVE 'D-KOLLA-TVINGANDE'    TO CURRENT-SECTION                       
028500                                                                          
028600     MOVE OHKK-IDDISTR         TO KREG-IDDISTR                            
028700     MOVE OHKK-IDKUNDNR        TO KREG-IDKUNDNR                           
028800     MOVE OHKK-IDSYSTEM        TO KREG-IDSYSTEM                           
028900     MOVE SPACE                TO KREG-IDDC-TVS                           
029000     MOVE +0                   TO KREG-KDFRAKT-IN                         
029100     MOVE OHKK-KDORDKL         TO KREG-KDORDKL                            
029200     MOVE NEJ                  TO KREG-FLVORKO                            
029300                                  KREG-FLVORFK                            
029400                                                                          
029500     CALL W411KREG USING KREG-W411KREG                                    
029600                         KREG-GMTA-PCB KREG-GMTB-PCB                      
029700                         KREG-GMTC-PCB KREG-BETC-PCB                      
029800                                                                          
029900     IF  KREG-IDDISTR-OK = JA AND                                         
030000       ((OHKK-KDORDKL = 1 AND KREG-FLORDTIL-KL1 = JA) OR                  
030100        (OHKK-KDORDKL = 2 AND KREG-FLORDTIL-KL2 = JA) OR                  
030200        (OHKK-KDORDKL = 3 AND KREG-FLORDTIL-KL3 = JA) OR                  
030300        (OHKK-KDORDKL = 4 AND KREG-FLORDTIL-KL4 = JA))                    
030400        IF (OHKK-ADBETRAD-1     = KREG-ADBETRAD-1 OR                      
030500            OHKK-ADBETRAD-1     = ALL '+')         AND                    
030600           (OHKK-ADBETRAD-2     = KREG-ADBETRAD-2 OR                      
030700            OHKK-ADBETRAD-2     = ALL '+')         AND                    
030800           (OHKK-ADGMT          = KREG-ADGMT      OR                      
030900            OHKK-ADGMT          = ALL '+')         AND                    
031000           (OHKK-BEBETRAD-1     = KREG-BEBETRAD-1 OR                      
031100            OHKK-BEBETRAD-1     = ALL '+')         AND                    
031200           (OHKK-BEBETRAD-2     = KREG-BEBETRAD-2 OR                      
031300            OHKK-BEBETRAD-2     = ALL '+')         AND                    
031400           (OHKK-BEGMRK         = KREG-BEGMRK     OR                      
031500            OHKK-BEGMRK         = ALL '+')         AND                    
031600           (OHKK-BEGMT          = KREG-BEGMT      OR                      
031700            OHKK-BEGMT          = ALL '+')         AND                    
031800           (OHKK-KDFAKTYP       = KREG-KDGENFAK   OR                      
031900            OHKK-KDFAKTYP       = ALL '+')         AND                    
032000           OHKK-FLPRELRO        = KREG-FLPRELRO    AND                    
032100           OHKK-FLPRERS         = KREG-FLPRERS                            
032200                                                                          
032300           MOVE JA  TO OHKK-FLORDTIL                                      
032400                                                                          
032500           IF OHKK-IDORDNR-IN NOT = ZERO                                  
032600              MOVE OHKK-IDDISTR    TO W-IDDISTR-WDQ2C                     
032700              MOVE OHKK-IDKUNDNR   TO W-IDKUNDNR-WDQ2C                    
032800              MOVE OHKK-IDORDNR-IN TO W-IDORDNR-WDQ2C                     
032900                                                                          
033000              PERFORM IMS-06-GU-WDQ2C                                     
033100              IF SEGMENT-FINNS                                            
033200                 MOVE NEJ TO OHKK-FLORDTIL                                
033300              END-IF                                                      
033400           END-IF                                                         
033500        END-IF                                                            
033600     END-IF                                                               
033700     .                                                                    
033800                                                                          
033900                                                                          
034000 E-KOLLA-TARGET-ORDER  SECTION.                                           
034100     MOVE 'E-KOLLA-TARGET   '    TO CURRENT-SECTION                       
034200                                                                          
034300     MOVE OHKK-IDDISTR   TO TAVG-IDDISTR                                  
034400     MOVE OHKK-IDKUNDNR  TO TAVG-IDKUNDNR                                 
034500     MOVE OHKK-KDORDKL   TO TAVG-KDORDKL                                  
034600     MOVE OHKK-KDFRAKT   TO TAVG-KDFRAKT                                  
034700                                                                          
034800     CALL W413TAVG USING TAVG-W413TAVG                                    
034900                         TAVG-WDB2-PCB TAVG-WDB3-PCB                      
035000                         TAVG-WDB5-PCB TAVG-WDP7-PCB                      
035100                         TAVG-XXKB-PCB                                    
035200                                                                          
035300     IF TAVG-IDDC NOT = SPACE                                             
035400        MOVE '20'            TO TAVG-DATRPAVT(1:2)                        
035500        MOVE TAVG-KDFRAKT    TO OHKK-KDFRAKT                              
035600                                                                          
035700        MOVE OHKK-IDDISTR    TO W-IDDISTR-WDQ2                            
035800        MOVE OHKK-IDKUNDNR   TO W-IDKUNDNR-WDQ2                           
035900        MOVE OHKK-KDORDKL    TO W-KDORDKL-WDQ2                            
036000                                                                          
036100        MOVE NEJ TO TARGET-SW                                             
036200        PERFORM IMS-01-GU-WDQ201                                          
036300        IF SEGMENT-FINNS                                                  
036400           PERFORM UNTIL SEGMENT-SAKNAS OR BASEN-SLUT OR                  
036500                         TARGET-OK                                        
036600              IF OHUV-FLKLAR  = JA  AND                                   
036700                 OHUV-FLBORT = NEJ AND                                    
036800                (OHUV-FLRESTN = OHKK-FLRESTN OR                           
036900                (OHUV-FLRESTN = 'Y' OR 'J' AND                            
037000                 OHKK-FLRESTN = 'Y' OR 'J'))                              
037100                 MOVE OHUV-IDDC-PRIM     TO W-IDDC-WDQ212                 
037200                 PERFORM IMS-03-GNP-WDQ212                                
037300                 IF SEGMENT-FINNS                                         
037400                    IF ARB-DATRPAVT = TAVG-DATRPAVT AND                   
037500                       ARB-KDFRAKT  = TAVG-KDFRAKT  AND                   
037600                       ARB-KDROPACK NOT = '3'                             
037700                       MOVE JA TO TARGET-SW                               
037800                    ELSE                                                  
037900                       PERFORM IMS-02-GN-WDQ201                           
038000                    END-IF                                                
038100                 ELSE                                                     
038200                    PERFORM IMS-02-GN-WDQ201                              
038300                 END-IF                                                   
038400              ELSE                                                        
038500                 PERFORM IMS-02-GN-WDQ201                                 
038600              END-IF                                                      
038700           END-PERFORM                                                    
038800           IF SEGMENT-FINNS                                               
038900              MOVE OHUV-IDORDNR7 TO OHKK-IDORDNR-UT                       
039000              MOVE OHUV-IDORDER  TO OHKK-IDORDER                          
039100                                                                          
039200              IF OHKK-IDSYSTEM NOT = 'IMS '                               
039300                                                                          
039400                 MOVE OHUV-IDORDER  TO W-IDORDER                          
039500                 PERFORM IMS-04-GHU-WDQ201                                
039600                 MOVE NEJ              TO OHUV-FLKLAR                     
039700                 PERFORM IMS-05-REPL-WDQ201                               
039800                                                                          
039900                                                                          
040000                 PERFORM IMS-GHNP-WDQ212                                  
040100                 PERFORM UNTIL SEGMENT-SAKNAS                             
040200                    MOVE ARB-KDORDSTA TO ARB-KDORDSTA-O                   
040300                    MOVE 'E'          TO ARB-KDORDSTA                     
040400                    PERFORM IMS-REPL-WDQ212                               
040500                    PERFORM IMS-GHNP-WDQ212                               
040600                 END-PERFORM                                              
040700                                                                          
040800              END-IF                                                      
040900           ELSE                                                           
041000              MOVE ZERO          TO OHKK-IDORDER                          
041100                                    OHKK-IDORDNR-UT                       
041200           END-IF                                                         
041300        ELSE                                                              
041400           MOVE ZERO         TO OHKK-IDORDER                              
041500                                OHKK-IDORDNR-UT                           
041600        END-IF                                                            
041700     ELSE                                                                 
041800        MOVE ZERO         TO OHKK-IDORDER                                 
041900                             OHKK-IDORDNR-UT                              
042000                             OHKK-KDFRAKT                                 
042100     END-IF                                                               
042200                                                                          
042300     .                                                                    
042400                                                                          
042500 IMS-01-GU-WDQ201 SECTION.                                                
042600     MOVE 'IMS-01'               TO CURRENT-IMS-SECTION                   
042700                                                                          
042800     STRING 'WDQ201  (WDQ2ESEQ =' W-WDQ2ESEQ-X ')'                        
042900             DELIMITED BY SIZE INTO SSA1                                  
043000     MOVE '  GE'                 TO GODK-STATUSKODER                      
043100     CALL CBLTDLI USING GU WDQ2-PCB DLI-IO-AREA-WDQ201 SSA1               
043200     MOVE WDQ2-STATUS-CODE       TO STATUS-WS                             
043300     PERFORM IMS-STATUSKONTROLL                                           
043400     .                                                                    
043500                                                                          
043600 IMS-02-GN-WDQ201 SECTION.                                                
043700     MOVE 'IMS-02'               TO CURRENT-IMS-SECTION                   
043800                                                                          
043900     STRING 'WDQ201  (WDQ2ESEQ =' W-WDQ2ESEQ-X ')'                        
044000             DELIMITED BY SIZE INTO SSA1                                  
044100     MOVE '  GEGB'               TO GODK-STATUSKODER                      
044200     CALL CBLTDLI USING GN WDQ2-PCB DLI-IO-AREA-WDQ201 SSA1               
044300     MOVE WDQ2-STATUS-CODE       TO STATUS-WS                             
044400     PERFORM IMS-STATUSKONTROLL                                           
044500     .                                                                    
044600                                                                          
044700 IMS-03-GNP-WDQ212 SECTION.                                               
044800     MOVE 'IMS-03'               TO CURRENT-IMS-SECTION                   
044900                                                                          
045000     STRING 'WDQ212  (IDDC     =' W-IDDC-X ')'                            
045100             DELIMITED BY SIZE INTO SSA1                                  
045200     MOVE '  GE'                 TO GODK-STATUSKODER                      
045300     CALL CBLTDLI USING GNP WDQ2-PCB DLI-IO-AREA-WDQ212 SSA1              
045400     MOVE WDQ2-STATUS-CODE       TO STATUS-WS                             
045500     PERFORM IMS-STATUSKONTROLL                                           
045600     .                                                                    
045700                                                                          
045800 IMS-GHNP-WDQ212 SECTION.                                                 
045900     MOVE 'IMS-GHNP-WDQ212'      TO CURRENT-IMS-SECTION                   
046000                                                                          
046100     MOVE 'WDQ212'               TO SSA1                                  
046200     MOVE '  GE'                 TO GODK-STATUSKODER                      
046300     CALL CBLTDLI USING GHNP WDQ2-UPD-PCB DLI-IO-AREA-WDQ212 SSA1         
046400     MOVE WDQ2-UPD-STATUS-CODE   TO STATUS-WS                             
046500     PERFORM IMS-STATUSKONTROLL                                           
046600     .                                                                    
046700                                                                          
046800 IMS-REPL-WDQ212 SECTION.                                                 
046900     MOVE 'IMS-REPL-WDQ212'      TO CURRENT-IMS-SECTION                   
047000                                                                          
047100     MOVE '    '                 TO GODK-STATUSKODER                      
047200     CALL CBLTDLI USING REPL WDQ2-UPD-PCB DLI-IO-AREA-WDQ212              
047300     MOVE WDQ2-UPD-STATUS-CODE   TO STATUS-WS                             
047400     PERFORM IMS-STATUSKONTROLL                                           
047500     .                                                                    
047600                                                                          
047700 IMS-04-GHU-WDQ201 SECTION.                                               
047800     MOVE 'IMS-04'               TO CURRENT-IMS-SECTION                   
047900                                                                          
048000     STRING 'WDQ201  (IDORDER  =' W-IDORDER-X ')'                         
048100             DELIMITED BY SIZE INTO SSA1                                  
048200     MOVE '    '                 TO GODK-STATUSKODER                      
048300     CALL CBLTDLI USING GHU WDQ2-UPD-PCB DLI-IO-AREA-WDQ201 SSA1          
048400     MOVE WDQ2-UPD-STATUS-CODE   TO STATUS-WS                             
048500     PERFORM IMS-STATUSKONTROLL                                           
048600     .                                                                    
048700                                                                          
048800 IMS-05-REPL-WDQ201 SECTION.                                              
048900     MOVE 'IMS-05'               TO CURRENT-IMS-SECTION                   
049000                                                                          
049100     MOVE '    '                 TO GODK-STATUSKODER                      
049200     CALL CBLTDLI USING REPL WDQ2-UPD-PCB DLI-IO-AREA-WDQ201              
049300     MOVE WDQ2-UPD-STATUS-CODE   TO STATUS-WS                             
049400     PERFORM IMS-STATUSKONTROLL                                           
049500     .                                                                    
049600                                                                          
049700 IMS-06-GU-WDQ2C  SECTION.                                                
049800     MOVE 'IMS-06'               TO CURRENT-IMS-SECTION                   
049900                                                                          
050000     STRING 'WDQ2C1  (WDQ2C1KY =' W-IDGMTREF ')'                          
050100             DELIMITED BY SIZE INTO SSA1                                  
050200     MOVE '  GE'                 TO GODK-STATUSKODER                      
050300     CALL CBLTDLI USING GU WDQ2C-PCB DLI-IO-AREA-WDQ2C SSA1               
050400     MOVE WDQ2C-STATUS-CODE      TO STATUS-WS                             
050500     PERFORM IMS-STATUSKONTROLL                                           
050600     .                                                                    
050700 IMS-STATUSKONTROLL SECTION.                                              
050800                                                                          
050900     SET STATUS-IX TO 1                                                   
051000     SEARCH GODK-STATUS                                                   
051100       AT END CALL FELLOG                                                 
051200       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
051300     END-SEARCH                                                           
051400     .                                                                    
