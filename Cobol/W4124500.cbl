000100 ID DIVISION.                                                             
000200 PROGRAM-ID.                 W4124500.                                    
000300 AUTHOR.                     GÖRAN KJELLSON  GUIDE                        
000400     DATE-WRITTEN.           SEPTEMBER 2007                               
000500*                                                                         
000600     REMARKS.                                                             
000700*                                                                         
000800*    FUNKTION.                                                            
000900*                                                                         
001000*    LÄSER ORDERHUVUDETSREGISTRET (WDQ2) MED SB.                          
001100*    LÄSER DATABASEN OCH SKRIVER UT ORDER SOM ÄR LAGD                     
001200*    AKTUELL VECKA PÅ EN FIL.                                             
001210* CHANGE LOG:                                                             
001220* STORY 2373879 / REPLACE DIST35- 88 LEVELS OF IN,KR,                     
001230*  AE,CN CDC RETURNS TO DIST35-CDC-RETURNS-NON-VCC                        
001300                                                                          
001400                                                                          
001500 ENVIRONMENT DIVISION.                                                    
001600 INPUT-OUTPUT SECTION.                                                    
001700*                                                                         
001800 FILE-CONTROL.                                                            
001900     SKIP2                                                                
002000*    ---- UT-FIL W41245    OUTPUT                                         
002100                                                                          
002200     SELECT W41245           ASSIGN TO      W41245D1.                     
002300                                                                          
002400                                                                          
002500 DATA DIVISION.                                                           
002600                                                                          
002700 FILE SECTION.                                                            
002800                                                                          
002900 FD  W41245                                                               
003000     LABEL RECORD STANDARD                                                
003100     RECORDING F                                                          
003200     BLOCK CONTAINS 0.                                                    
003300                                                                          
003400*01  UT-AREA  -COPY  W41245     -L.                                       
003500                                                                          
003600                                                                          
003700 WORKING-STORAGE SECTION.                                                 
003800                                                                          
003900*    -- CHECKED BY WY2000                                                 
004000*    ---- GENERELLA KONSTANTER                                            
004100 77  JA                          PIC X       VALUE 'J'.                   
004200 77  NEJ                         PIC X       VALUE 'N'.                   
004210 77  YES                         PIC X       VALUE 'Y'.                   
004300 77  CURRENT-SECTION             PIC X(16)   VALUE 'MAIN'.                
004400 77  CURRENT-IMS-SECTION         PIC X(16)   VALUE SPACE.                 
004500                                                                          
004600 77  SW-TESTA-SKRIV              PIC X(1)    VALUE 'N'.                   
004610     88 OK-SKRIV                             VALUE 'J'.                   
004611                                                                          
004620 01  FELTEXT.                                                             
004800     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
004900     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
005000                                                                          
005100*    ---- EOF-SWITCHAR                                                    
005200 77  W41245-EOF                  PIC X       VALUE 'N'.                   
005300                                                                          
005400*    ---- ARBETSFÄLT                                                      
005500 01  CURR-AAR-VECKA              PIC 9(4).                                
005600 01  FILLER REDEFINES CURR-AAR-VECKA.                                     
005700     03  CURR-AAR                PIC 9(2).                                
005800     03  CURR-VECKA              PIC 9(2).                                
005900                                                                          
006000                                                                          
006100 01  WORK-AAR-VECKA              PIC 9(4).                                
006200                                                                          
006210 01  TEST-IDDISTR                PIC S9(5)   COMP-3.                      
006230                                                                          
006240** NORDAMERIKA+PACIFIKDISTRIKT  *****                                     
006250 01  FILLER REDEFINES TEST-IDDISTR.                                       
006260 03     -COPY WWDIST07.                                                   
006270                                                                          
006293** SKROTDISTRIKT  ***********                                             
006294 01  FILLER REDEFINES TEST-IDDISTR.                                       
006295 03     -COPY WWDIST18.                                                   
006296                                                                          
006297** SATSDISTRIKT   ***********                                             
006298 01  FILLER REDEFINES TEST-IDDISTR.                                       
006299 03     -COPY WWDIST19.                                                   
006300                                                                          
006301** RETUR- REFILLDISTRIKT ****                                             
006302 01  FILLER REDEFINES TEST-IDDISTR.                                       
006303 03     -COPY WWDIST35.                                                   
006310                                                                          
006320 01  W-IDDC-CLEAR                PIC X(2)  VALUE SPACE.                   
006400                                                                          
006500 01  RETURKODER.                                                          
006600   03  RKOD                      PIC S9(4) COMP SYNC VALUE ZERO.          
006700   03  RKOD-ABEND-UTAN-DUMP      PIC S9(4) COMP SYNC VALUE +16.           
006800                                                                          
006900*    ---- SUBPROGRAM OCH PARAMETERAREOR                                   
007000                                                                          
007100 01  DYNAMISKA-SUBPROGRAM.                                                
007200   03  POSTSUM                   PIC X(8)    VALUE 'POSTSUM '.            
007300   03  CBLTDLI                   PIC X(8)    VALUE 'CBLTDLI '.            
007400   03  FELLOG                    PIC X(8)    VALUE 'FELLOG  '.            
007500   03  DATKORT                   PIC X(8)    VALUE 'DATKORT '.            
007600   03  WDATKONV                  PIC X(8)    VALUE 'WDATKONV'.            
007700   03  ABEND                     PIC X(8)    VALUE 'ABEND   '.            
007800                                                                          
007900                                                                          
008000*    ---- PARAMETRAR TILL DATKORT                                         
008100 01  FILLER                      PIC X(8)    VALUE 'DATKORT'.             
008200 01  PROGRAM-NAMN                PIC X(6)    VALUE 'W41245'.              
008300 01  DATUMKORT-ID                PIC X(6)    VALUE 'WDATUM'.              
008400*01  -COPY WDATKORT                                                       
008500                                                                          
008600*    --- PARAMETRAR TILL WDATKONV                                         
008700*01  -COPY WDATAREA                                                       
008800                                                                          
008900                                                                          
009000*    ---- PARAMETRAR TILL POSTSUM                                         
009100*01  -COPY W0005      -PRE POSTSUM-.                                      
009200                                                                          
009300*    ---- UTAREA                                                          
009400                                                                          
009500*01  FILLER                      PIC X(8)    VALUE 'UT-AREA'.             
009600                                                                          
009700*01  -COPY W41245      -PRE UT-.                                          
009900                                                                          
010000                                                                          
010100*    ---- ARBETS-AREOR FÖR IMS-SEKTIONERNA.                               
010200                                                                          
010300 01  FILLER                      PIC X(8)   VALUE 'IMS-WS  '.             
010400                                                                          
010500*    ---- STATUSKOD FRÅN IMS                                              
010600                                                                          
010700 01  STATUS-WS                   PIC XX.                                  
010800     88  SEGMENT-FINNS                      VALUE '  '.                   
010810     88  SEGMENT-SAKNAS                     VALUE 'GE'.                   
010900     88  SEGMENT-SLUT                       VALUE 'GB'.                   
011000                                                                          
011100 01  GODK-STATUSKODER.                                                    
011200   03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                  
011300                                                                          
011400 01  SSA1                        PIC X(60).                               
011500                                                                          
011700*01      -COPY W0003.                                                     
011710                                                                          
011800 01  FILLER.                                                              
011810     03  W-WDQ301KY-MIN-X.                                                
011820      05  W-MIN-IDORDER       PIC  S9(7)    COMP-3.                       
011830      05  W-MIN-IDDC          PIC  X(2)     VALUE LOW-VALUE.              
011840      05  FILLER              PIC  X(6)     VALUE LOW-VALUE.              
011850                                                                          
011860     03  W-WDQ301KY-MAX-X.                                                
011870      05  W-MAX-IDORDER       PIC  S9(7)    COMP-3.                       
011880      05  W-MAX-IDDC          PIC  X(2)     VALUE HIGH-VALUE.             
011890      05  FILLER              PIC  X(6)     VALUE HIGH-VALUE.             
011891                                                                          
011900 01  FILLER                      PIC X(16)  VALUE                         
012000                                            'DLI-IO-AREA'.                
012100 01  DLI-IO-AREA.                                                         
012200*                                                                         
012420   03  IO-AREA               PIC X(5000)  VALUE SPACE.                    
012430     SKIP2                                                                
012440*  03  WLORQI01     -COPY WDQ201             -RED IO-AREA.                
012450     SKIP2                                                                
012460*  03  WLORQI12     -COPY WDQ212             -RED IO-AREA.                
012470     EJECT                                                                
012500 01  FILLER                   PIC X(16)   VALUE 'IO-AREA-WDQ301'.         
012510 01  DLI-IO-WDQ301.                                                       
012520     03  WDQ301.                                                          
012530*        05  -COPY WDQ301                                                 
012540     EJECT                                                                
012600                                                                          
012700 LINKAGE SECTION.                                                         
012800                                                                          
012900*    -COPY W0008 -PRE WDQ2-.                                              
013000    05  FILLER                   PIC X(1).                                
013010                                                                          
013020*    -COPY W0008 -PRE WDQ3-.                                              
013030    05  FILLER                   PIC X(1).                                
013100                                                                          
013200                                                                          
013300                                                                          
013400 PROCEDURE DIVISION  USING WDQ2-PCB  WDQ3-PCB.                            
013500     ENTRY 'DLITCBL' USING WDQ2-PCB  WDQ3-PCB.                            
013600                                                                          
013700 STYR SECTION.                                                            
013800     PERFORM A-INIT                                                       
013900     PERFORM IMS-01-GET-WDQ2                                              
014000     PERFORM UNTIL SEGMENT-SLUT                                           
014100       EVALUATE WDQ2-SEG-NAME-FB                                          
014200         WHEN 'WDQ201  '                                                  
014300           PERFORM B-SKAPA-UTPOST                                         
014310         WHEN 'WDQ212  '                                                  
014320           PERFORM C-UPPD-UTPOST                                          
014400       END-EVALUATE                                                       
014500       PERFORM IMS-01-GET-WDQ2                                            
014600     END-PERFORM                                                          
014700                                                                          
014800     PERFORM Z-FINIT                                                      
014900     MOVE ZERO TO RETURN-CODE                                             
015000     GOBACK                                                               
015100     .                                                                    
015200                                                                          
015300                                                                          
015400 A-INIT SECTION.                                                          
015500     MOVE 'A-INIT          ' TO CURRENT-SECTION                           
015600                                                                          
015700     OPEN OUTPUT W41245                                                   
015800     MOVE 'W4124500'         TO POSTSUM-PROGNAMN                          
015900     MOVE 'W41245D1'         TO POSTSUM-DDNAMN2                           
016000     MOVE 'W41245  '         TO POSTSUM-FDNAMN                            
016100                                                                          
016200                                                                          
016300     CALL DATKORT USING PROGRAM-NAMN DATUMKORT-ID DATUMKORT               
016400                                                                          
016700     MOVE D-AAR              TO CURR-AAR                                  
016710     MOVE D-VECKA            TO CURR-VECKA                                
016800     .                                                                    
016900                                                                          
017000                                                                          
017100 B-SKAPA-UTPOST SECTION.                                                  
017200     MOVE 'B-SKAPA-UTPOST  ' TO CURRENT-SECTION                           
017300                                                                          
017310     IF OK-SKRIV                                                          
017320        PERFORM D-DAUTSKR-Q3                                              
017330        WRITE UT-AREA FROM UT-W41245                                      
017340        CALL POSTSUM USING POSTSUM-PARM                                   
017341        MOVE NEJ          TO SW-TESTA-SKRIV                               
017350     END-IF                                                               
017400     MOVE 'AAMMDD'        TO DAT-KDDATFORM                                
017500     MOVE OHUV-TIREGDAT   TO DAT-I-TIDATUM                                
017600                                                                          
017700     CALL WDATKONV  USING DAT-KDDATFORM                                   
017800                          DAT-I-TIDATUM                                   
017900                          DAT-O-TIDATUM                                   
018000                          DAT-KDSVAR                                      
018100     IF DAT-KDSVAR-FEL                                                    
018200        MOVE 'FEL FRÅN WDATKONV I A-SECTION'                              
018300                          TO FELTEXT                                      
018400        CALL ABEND USING RKOD-ABEND-UTAN-DUMP                             
018500     END-IF                                                               
018600     MOVE DAT-TIAAVV-GRP  TO WORK-AAR-VECKA                               
018700     IF WORK-AAR-VECKA = CURR-AAR-VECKA                                   
018801       PERFORM TESTA                                                      
018810       IF OK-SKRIV                                                        
018900         MOVE OHUV-IDDISTR             TO UT-IDDISTR                      
019000         MOVE OHUV-IDKUNDNR            TO UT-IDKUNDNR                     
019100         MOVE OHUV-IDKUNDRF            TO UT-IDKUNDRF                     
019200         MOVE OHUV-KDORDKL             TO UT-KDORDKL                      
019300         MOVE CURR-AAR-VECKA           TO UT-TIAAVV                       
019400         MOVE OHUV-TIREGDAT            TO UT-TIREGDAT                     
019410         MOVE OHUV-TIREGTID            TO UT-TIREGTID                     
019411         IF OHUV-KVORDTIL IS NUMERIC                                      
019420           IF OHUV-KVORDTIL > ZERO                                        
019500              MOVE 1                   TO UT-KVORDTIL-UTSKR               
019510           ELSE                                                           
019520              MOVE ZERO                TO UT-KVORDTIL-UTSKR               
019530           END-IF                                                         
019600           MOVE OHUV-KVORDTIL          TO UT-KVORDTIL                     
019610         ELSE                                                             
019611           MOVE 1                      TO UT-KVORDTIL-UTSKR               
019612           MOVE 1                      TO UT-KVORDTIL                     
019620         END-IF                                                           
019630         IF OHUV-BEKUNDRF = 'DL REL'                                      
019631           MOVE 2                      TO UT-KDTILTYP                     
019640         ELSE                                                             
019641           MOVE 1                      TO UT-KDTILTYP                     
019650         END-IF                                                           
019660         MOVE OHUV-IDORDER             TO UT-IDORDER                      
019661                                          W-MIN-IDORDER                   
019662                                          W-MAX-IDORDER                   
019670         MOVE OHUV-TIREGDAT-STO        TO UT-TIREGDAT-STO                 
019680         MOVE OHUV-TIREGTID-STO        TO UT-TIREGTID-STO                 
019690         MOVE OHUV-IDSYSTEM            TO UT-IDSYSTEM                     
019700         MOVE OHUV-IDDC-PRIM           TO W-IDDC-CLEAR                    
019710         MOVE ZERO                     TO UT-KDFRAKT                      
019720                                          UT-DAUTSKR                      
019730                                          UT-TIUTSTID                     
019740                                          UT-KVRADER                      
019800                                                                          
019900*        WRITE UT-AREA FROM UT-W41245                                     
020000*        CALL POSTSUM USING POSTSUM-PARM                                  
020100       END-IF                                                             
020110     END-IF                                                               
020200     .                                                                    
020210 C-UPPD-UTPOST  SECTION.                                                  
020211     IF W-IDDC-CLEAR = ARB-IDDC                                           
020212        AND OK-SKRIV                                                      
020213        MOVE ARB-KDFRAKT               TO UT-KDFRAKT                      
020214     END-IF                                                               
020220     .                                                                    
020300     EJECT                                                                
020310 TESTA  SECTION.                                                          
020311     MOVE NEJ                          TO SW-TESTA-SKRIV                  
020312     MOVE OHUV-IDDISTR                 TO TEST-IDDISTR                    
020313     IF DIST07-NA-CUSTOMERS      OR                                       
020314        DIST07-PACIFIC           OR                                       
020315        DIST07-KINA              OR                                       
020316        DIST07-INDIEN            OR                                       
020317        DIST07-KOREA             OR                                       
020318        DIST07-MALAYSIA          OR                                       
020319        DIST07-THAILAND          OR                                       
020320        DIST07-TAIWAN            OR                                       
020321        DIST07-TURKEY            OR                                       
020322        DIST07-S-AFRICA          OR                                       
020323        DIST07-MEXICO            OR                                       
020324        DIST07-BRAZIL            OR                                       
020325        DIST18-SKROT             OR                                       
020326        DIST18-SCRAP-NDC         OR                                       
020327        DIST19-SATS              OR                                       
020328        DIST35-RETUR             OR                                       
020329        DIST35-REFILL            OR                                       
020330        DIST35-NONVCC-REFILL     OR                                       
020331        DIST35-NONVCC-NONVCC-TRANSFER OR                                  
020332        DIST35-NONVCC-VCC-TRANSFER OR                                     
020333        DIST35-REFILL-INOM-NDC   OR                                       
020334        DIST35-REFILL-NA-JAP     OR                                       
020335        DIST35-NA-CDC-RETURN     OR                                       
020336        DIST35-NA-NDC-RETURNS    OR                                       
020337        DIST35-CN-NDC-RETURNS    OR                                       
020338        DIST35-CDC-RETURNS-NON-VCC OR                                     
020339        DIST35-NA-TRANSFER       OR                                       
020340        DIST35-PACIFIC-TRANSFER  OR                                       
020341        DIST35-REFILL-INOM-JP    OR                                       
020342        DIST35-CN-TRANSFER       OR                                       
020343        DIST35-ST-CDC            OR                                       
020344        DIST35-NL-SITTARD-OBJEKT                                          
020345        CONTINUE                                                          
020346     ELSE                                                                 
020347       IF OHUV-IDSYSTEM NOT = 'LDC '         AND                          
020348         (OHUV-KDORDKL = 1 OR 2 OR 3 OR 4) AND                            
020349          OHUV-FLAUTFAK = NEJ                AND                          
020350          OHUV-FLAUTPAC = NEJ                AND                          
020351          OHUV-FLEMBORD = NEJ                AND                          
020352          OHUV-FLFORBI = NEJ                 AND                          
020353          OHUV-FLORDSPE = NEJ                AND                          
020354          OHUV-FLOVRLEV = NEJ                AND                          
020355         (OHUV-FLLSBOK = JA OR YES)          AND                          
020356          OHUV-FLVORKO = NEJ                 AND                          
020357                                                                          
020358          OHUV-IDANALYS = SPACE              AND                          
020359          OHUV-IDBIPREF = SPACE              AND                          
020360          OHUV-IDDEPT = ZERO                 AND                          
020361          OHUV-IDFTG = ZERO                  AND                          
020362          OHUV-IDKAMPRF = ZERO               AND                          
020363          OHUV-IDKONTO = ZERO                AND                          
020364          OHUV-IDKST = SPACE                 AND                          
020365                                                                          
020366         (OHUV-KDFAKTYP = 'K' OR 'R' OR '+') AND                          
020367          OHUV-KDTPOTYP = ZERO               AND                          
020368          OHUV-KDVRINFO = ZERO               AND                          
020369          OHUV-TITPO = ZERO                                               
020370         MOVE JA                        TO SW-TESTA-SKRIV                 
020371       END-IF                                                             
020372     END-IF                                                               
020373     .                                                                    
020374 D-DAUTSKR-Q3  SECTION.                                                   
020375     PERFORM IMS-GU-WDQ301                                                
020376     IF SEGMENT-FINNS                                                     
020377       MOVE ODEL-DAUTSKR            TO UT-DAUTSKR                         
020378       MOVE ODEL-TIUTSTID           TO UT-TIUTSTID                        
020390       PERFORM UNTIL SEGMENT-SAKNAS OR SEGMENT-SLUT                       
020391        ADD ODEL-KVRADER            TO UT-KVRADER                         
020392        IF ODEL-DAUTSKR < UT-DAUTSKR OR                                   
020393           (ODEL-DAUTSKR = UT-DAUTSKR AND                                 
020394           ODEL-TIUTSTID < UT-TIUTSTID)                                   
020395           MOVE ODEL-DAUTSKR        TO UT-DAUTSKR                         
020396           MOVE ODEL-TIUTSTID       TO UT-TIUTSTID                        
020397        END-IF                                                            
020398        PERFORM IMS-GN-WDQ301                                             
020399       END-PERFORM                                                        
020400     END-IF                                                               
020410     .                                                                    
020500 Z-FINIT SECTION.                                                         
020600     MOVE 'Z-FINIT         ' TO CURRENT-SECTION                           
020700                                                                          
020710     IF OK-SKRIV                                                          
020711        PERFORM D-DAUTSKR-Q3                                              
020720        WRITE UT-AREA FROM UT-W41245                                      
020730        CALL POSTSUM USING POSTSUM-PARM                                   
020731        MOVE NEJ          TO SW-TESTA-SKRIV                               
020740     END-IF                                                               
020800     CLOSE W41245                                                         
020900     MOVE 'S' TO POSTSUM-OPKOD                                            
021000     CALL POSTSUM USING POSTSUM-PARM                                      
021100     .                                                                    
021200                                                                          
021300                                                                          
021400*    ---- IMS SEKTIONER                                                   
021500 IMS-01-GET-WDQ2 SECTION.                                                 
021600     MOVE 'IMS-01-GET-WDQ2 ' TO CURRENT-IMS-SECTION                       
021700                                                                          
021800     MOVE '  GAGKGB' TO GODK-STATUSKODER                                  
021900     CALL CBLTDLI USING GN WDQ2-PCB DLI-IO-AREA                           
022000     MOVE WDQ2-STATUS-CODE TO STATUS-WS                                   
022100     PERFORM IMS-STATUSKONTROLL                                           
022200     .                                                                    
022210 IMS-GU-WDQ301 SECTION.                                                   
022220                                                                          
022230     STRING 'WDQ301  (WDQ301KY>=' W-WDQ301KY-MIN-X                        
022240                    '&WDQ301KY<=' W-WDQ301KY-MAX-X ')'                    
022250          DELIMITED BY SIZE INTO SSA1                                     
022260     MOVE '  GBGE' TO GODK-STATUSKODER                                    
022270     CALL CBLTDLI USING GU WDQ3-PCB DLI-IO-WDQ301 SSA1                    
022280     MOVE WDQ3-STATUS-CODE TO STATUS-WS                                   
022290     PERFORM IMS-STATUSKONTROLL                                           
022291     .                                                                    
022292 IMS-GN-WDQ301 SECTION.                                                   
022293                                                                          
022294     STRING 'WDQ301  (WDQ301KY>=' W-WDQ301KY-MIN-X                        
022295                    '&WDQ301KY<=' W-WDQ301KY-MAX-X ')'                    
022296          DELIMITED BY SIZE INTO SSA1                                     
022297     MOVE '  GBGE' TO GODK-STATUSKODER                                    
022298     CALL CBLTDLI USING GN WDQ3-PCB DLI-IO-WDQ301 SSA1                    
022299     MOVE WDQ3-STATUS-CODE TO STATUS-WS                                   
022300     PERFORM IMS-STATUSKONTROLL                                           
022310     .                                                                    
022400                                                                          
022500 IMS-STATUSKONTROLL SECTION.                                              
022600                                                                          
022700     SET STATUS-IX TO 1                                                   
022800     SEARCH GODK-STATUS                                                   
022900       AT END CALL FELLOG                                                 
023000       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
023100     END-SEARCH                                                           
023200     .                                                                    
