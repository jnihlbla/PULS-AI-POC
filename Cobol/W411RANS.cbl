000100*COMPOPT STDSUB=YES                                                       
000200 ID DIVISION.                                                             
000300     SKIP2                                                                
000400 PROGRAM-ID.     W411RANS.                                                
000500 AUTHOR.         SVANTE BJÖRKBERG.                                        
000600 DATE-WRITTEN.   MAJ -90.                                                 
000700                                                                          
000800     REMARKS.                                                             
000900*                                                                         
000910*    OBS OBS !! SKALL SAMMA ÄNDRING IN I W440RROT?                        
000920*                                                                         
001000*    DETTA ÄR EN SUBMODUL SOM ANROPAS I ORDER-ENTRY FÖR                   
001100*    ATT BERÄKNA RANSONERINGSFAKTORN PÅ ORDER-RADEN SAMT PÅ               
001200*    ARTIKELN.                                                            
001300*                                                                         
001400*    DESSUTOM BERÄKNAS BEHOVET FÖR TPO FRAM TOM NÄSTA INLEVERANS.         
001500*                                                                         
001600*    REGISTER :    WLXXKM (WDR1) RANSONERINGSTABELL                       
001700*                  WLARTM (WDK9) ARTIKELREGITER                           
001800*                                                                         
001900*    LÄNKAREA :    W411RANS                                               
002000                                                                          
002100     SKIP3                                                                
002200 ENVIRONMENT DIVISION.                                                    
002300     EJECT                                                                
002400 DATA DIVISION.                                                           
002500 WORKING-STORAGE SECTION.                                                 
002501*    -COPY WY2000W3                                                       
002502     SKIP3                                                                
002503*    -COPY WY2000W1                                                       
002510     SKIP3                                                                
002600 01  IDPGM                       PIC X(08)   VALUE 'W411RANS'.            
002700 01  JA                          PIC X       VALUE 'J'.                   
002800 01  NEJ                         PIC X       VALUE 'N'.                   
002900 01  FILLER                      PIC X(8) VALUE 'AAAAAAAA'.               
003000 01  WS-BEL                      PIC S9(9)V9(5).                          
003100 01  WS-DISP                     PIC S9(9)V9(5).                          
003200 01  WS-ACC                      PIC S9(9)   VALUE +0  COMP SYNC.         
003300 01  WS-TILLGANGARNA             PIC S9(9)V9(5)        COMP-3.            
003400 01  WS-BEHOV                    PIC S9(9)V9(5)        COMP-3.            
003410 01  WS-KVPB-REF                 PIC S9(8)V9(1)        COMP-3.            
003500 01  FILLER                      PIC X(8) VALUE 'BBBBBBBB'.               
003600 01  WS-DAG-T-INL                PIC S9(4).                               
003700 01  WS-DAGENS-DAT               PIC 9(6).                                
003800 01  WS-FORSLUNDS-KONSTANT       PIC S9(3)V9(5).                          
003900 01  WS-GJORD-KORRIGERING        PIC S9(3)V9(5).                          
004000 01  WS-KORR-REL-FOM             PIC S9(3)V9(5).                          
004100 01  WS-KORR-REL-TOM             PIC S9(3)V9(5).                          
004200 01  WS-MAX-KORRIGERING          PIC S9(3)V9(5).                          
004300 01  FILLER                      PIC X(8) VALUE 'CCCCCCCC'.               
004400 01  WS-RERF-CX                  PIC S9(3)V9(5).                          
004500 01  WS-RERF-FOM                 PIC S9(3)V9(5).                          
004600 01  WS-RERF-TOM                 PIC S9(3)V9(5).                          
004700 01  WS-RERF-NY                  PIC S9(3)V9(5).                          
004800 01  WS-TIAAAAVV-NUM             PIC 9(6).                                
004900 01  WS-TIAAAAVV  REDEFINES WS-TIAAAAVV-NUM.                              
005000  03 WS-TISEKEL                  PIC 9(2).                                
005010  03 WS-TIAA-VECKA               PIC 9(2).                                
005100  03 WS-TIVV                     PIC 9(2).                                
005200     EJECT                                                                
005210*      --- VALID IDDC CODES                                               
005220*                                                                         
005230*01    -COPY WWDCKONS                                                     
005240       EJECT                                                              
005300*                                                                         
005400 01  GENERELLA-SUBPROGRAM.                                                
005500     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
005600     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
005700     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
005800     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
005900     03  WORKDAY                 PIC X(8)    VALUE 'WORKDAY '.            
006000     EJECT                                                                
006100                                                                          
006200*    --- PARAMETRAR TILL ABEND                                            
006300                                                                          
006400 01  ERROR-TEXT                  PIC X(80).                               
006500 01  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   VALUE +16 COMP SYNC.         
006600 01  RKOD-ABEND-MED-DUMP         PIC S9(4)   VALUE +33 COMP SYNC.         
006700     EJECT                                                                
006800                                                                          
006900*    --- ARBETS-AREOR TILL GENERELLA SUBPROGRAM                           
007000     SKIP3                                                                
007100 01  FILLER                      PIC X(8)  VALUE 'WDATAREA'.              
007200*01  -COPY WDATAREA                                                       
007300     EJECT                                                                
007400                                                                          
007500 01  FILLER                      PIC X(8)  VALUE 'WORKAREA'.              
007600*01  -COPY WORKAREA                                                       
007700     EJECT                                                                
007800                                                                          
007900*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
008000*                                                                         
008100 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
008200     SKIP2                                                                
008300*    --- STATUS-KOD FRÅN IMS                                              
008400 01  STATUS-WS                   PIC XX.                                  
008500     88  SEGMENT-FINNS                       VALUE '  '.                  
008600     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
008700     SKIP2                                                                
008800 01  GODK-STATUSKODER.                                                    
008900     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
009000     SKIP2                                                                
009100 01  SSA1                        PIC X(64).                               
009200 01  SSA2                        PIC X(64).                               
009300     EJECT                                                                
009400                                                                          
009500*    --- IMS FUNKTIONSKODER                                               
009600*01  -COPY W0003                                                          
009700     EJECT                                                                
009800                                                                          
009900 01  NYCKLAR-TILL-DLI.                                                    
010000                                                                          
010100     03  W-WDGXKEY-4455-X.                                                
010200         05  W-IDHTYP            PIC  X(4)   VALUE '4455'.                
010300         05  FILLER              PIC  X(26)  VALUE LOW-VALUE.             
010400                                                                          
010500     03  W-WDGXKEY-4456-X.                                                
010600         05  W-IDRFTAB           PIC  X(3).                               
010700         05  FILLER              PIC  X(7)   VALUE LOW-VALUE.             
010800                                                                          
010900     03  W-IDARTNR-X.                                                     
011000         05  W-IDARTNR           PIC  S9(9)  COMP-3.                      
011100                                                                          
011200     03  W-DABEHOV-MIN-X.                                                 
011300         05  W-DABEHOV-MIN       PIC   9(6).                              
011400                                                                          
011500     03  W-DABEHOV-MAX-X.                                                 
011600         05  W-DABEHOV-MAX       PIC   9(6).                              
011700     EJECT                                                                
011800                                                                          
011900*    ---  DLI INPUT-OUTPUT AREOR                                          
012000*                                                                         
012100 01  FILLER                      PIC X(16)  VALUE 'XXKM-AREA'.            
012200*01  WLXXKM11 -COPY WDGX4456.                                             
012300     EJECT                                                                
012400 01  FILLER                      PIC X(16)  VALUE 'K901-AREA'.            
012500*01  WLARTM01 -COPY WDK901.                                               
012600     EJECT                                                                
012700 01  FILLER                      PIC X(16)  VALUE 'K911-AREA'.            
012800*01  WLARTM11 -COPY WDK911.                                               
012900     EJECT                                                                
012910 01  FILLER                      PIC X(16)  VALUE 'K701-AREA'.            
012920*01  WLARTS01 -COPY WDK701.                                               
012930     EJECT                                                                
012940 01  FILLER                      PIC X(16)  VALUE 'K711-AREA'.            
012950*01  WLARTS11 -COPY WDK711.                                               
012960     EJECT                                                                
013000                                                                          
013100 LINKAGE SECTION.                                                         
013200*                                                                         
013300*   -COPY W411RANS                                                        
013400*                                                                         
013500     EJECT                                                                
013600*01  -COPY W0008      -PRE XXKM-                                          
013700     05  FILLER                  PIC X.                                   
013800     SKIP2                                                                
013810*01  -COPY W0008      -PRE ARTM-                                          
013820     05  FILLER                  PIC X.                                   
013830     SKIP2                                                                
013900*01  -COPY W0008      -PRE ARTS-                                          
014000     05  FILLER                  PIC X.                                   
014100     EJECT                                                                
014200 PROCEDURE DIVISION  USING RANS-W411RANS XXKM-PCB ARTM-PCB                
014300                           ARTS-PCB.                                      
014400     PERFORM A-INIT                                                       
014500     PERFORM B-SUMMERA-TPO-BEHOVET                                        
014600                                                                          
014700     IF RANS-KDERS       = 0                     AND                      
014800        RANS-REDIRLEV    = 0                     AND                      
014900        RANS-FLOVRLEV    = NEJ                   AND                      
015000        RANS-FLORDSPE    = NEJ                   AND                      
015100        RANS-FLEMBORD    = NEJ                   AND                      
015200        RANS-IDLEVNR     = SPACE                 AND                      
015300        RANS-KDORDKL     > 0                     AND                      
015400        RANS-FLFORBI     = 'N'                   AND                      
015500        RANS-IDKAMPRF    = 0                     AND                      
015600        RANS-TIRODAT     = 0                     AND                      
015700        RANS-KVBEART-Q   > 0                     AND                      
015800        RANS-BERADREF    NOT = 'W480      '                               
015900                                                                          
016000       PERFORM D-INLEDANDE-BERAKNINGAR                                    
016100       PERFORM F-BERAKNA-RANS-FAKT                                        
016200     ELSE                                                                 
016300                                                                          
016400       MOVE 1                            TO RANS-RERF-RAD-UT              
016500     END-IF                                                               
016600                                                                          
016700     GOBACK                                                               
016800     .                                                                    
016900     EJECT                                                                
017000 A-INIT                        SECTION.                                   
017100                                                                          
017200     MOVE ZERO                  TO RANS-SUTPO-PB-UT                       
017300                                   RANS-SUTPO-EJPB-UT                     
017400                                   RANS-RERF-ART-UT                       
017500                                   RANS-RERF-RAD-UT                       
017600                                                                          
017700     MOVE 0.9000                TO WS-FORSLUNDS-KONSTANT                  
017800     .                                                                    
017900     EJECT                                                                
018000 B-SUMMERA-TPO-BEHOVET                   SECTION.                         
018100                                                                          
018200     MOVE RANS-IDARTNR              TO W-IDARTNR                          
018300     PERFORM IMS-GU-ARTM01                                                
018400                                                                          
018500     IF SEGMENT-FINNS                                                     
018600                                                                          
018700       PERFORM BA-SUMMERA-TPOBEHOV                                        
018800     ELSE                                                                 
018900       MOVE RANS-IDARTNR            TO ART-IDARTNR                        
019000       MOVE 0                       TO ART-KVOFFERT                       
019100       MOVE 0                       TO ART-KVOKS-BULK                     
019200       MOVE 0                       TO ART-KVOKS-DAG                      
019300       MOVE 0                       TO ART-KVOKS-VOR                      
019400       MOVE 0                       TO ART-KVPREAVB-BULK                  
019500       MOVE 0                       TO ART-KVPREAVB-DAG                   
019600       MOVE 0                       TO ART-KVPREAVB-VOR                   
019700       MOVE 0                       TO ART-KVPRERO-BULK                   
019800       MOVE 0                       TO ART-KVPRERO-DAG                    
019900       MOVE 1                       TO ART-RERF-ART                       
020000       MOVE 0                       TO ART-SUTPO-TOT                      
020100     END-IF                                                               
020200     .                                                                    
020300     EJECT                                                                
020400 BA-SUMMERA-TPOBEHOV           SECTION.                                   
020500                                                                          
020600     PERFORM BAA-SATT-MIN-VECKA                                           
020700                                                                          
020800     IF RANS-TIDISPIN = 0                                                 
020900       MOVE W-DABEHOV-MIN      TO W-DABEHOV-MAX                           
021000     ELSE                                                                 
021100       PERFORM BAB-SATT-MAX-VECKA                                         
021200     END-IF                                                               
021300                                                                          
021400     IF W-DABEHOV-MIN <= W-DABEHOV-MAX                                    
021500       PERFORM BAC-SUMMERA-TPO-BEHOVET                                    
021600     END-IF                                                               
021700     .                                                                    
021800     EJECT                                                                
021900                                                                          
022000 BAA-SATT-MIN-VECKA            SECTION.                                   
022100                                                                          
022200     MOVE 'IDAG'               TO DAT-KDDATFORM                           
022300     CALL WDATKONV USING DAT-KDDATFORM                                    
022400                         DAT-I-TIDATUM                                    
022500                         DAT-O-TIDATUM                                    
022600                         DAT-KDSVAR                                       
022700                                                                          
022800     IF DAT-KDSVAR-OK                                                     
022900       MOVE DAT-TISEKEL        TO WS-TISEKEL                              
022910       MOVE DAT-TIAA-VECKA     TO WS-TIAA-VECKA                           
023000       MOVE DAT-TIVV           TO WS-TIVV                                 
023100       MOVE WS-TIAAAAVV-NUM    TO W-DABEHOV-MIN                           
023200                                                                          
023300     ELSE                                                                 
023400       MOVE 'FEL I WDATKONV, W411RANS (LÄGE 1)' TO ERROR-TEXT             
023500       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
023600     END-IF                                                               
023700     .                                                                    
023800     EJECT                                                                
023900 BAB-SATT-MAX-VECKA                     SECTION.                          
024000                                                                          
024100     MOVE RANS-TIDISPIN  TO DAT-I-TIDATUM                                 
024200     MOVE 'AAMMDD'       TO DAT-KDDATFORM                                 
024300     CALL WDATKONV USING DAT-KDDATFORM                                    
024400                         DAT-I-TIDATUM                                    
024500                         DAT-O-TIDATUM                                    
024600                         DAT-KDSVAR                                       
024700                                                                          
024800     IF DAT-KDSVAR-OK                                                     
024900       MOVE DAT-TISEKEL        TO WS-TISEKEL                              
024910       MOVE DAT-TIAA-VECKA     TO WS-TIAA-VECKA                           
025000       MOVE DAT-TIVV           TO WS-TIVV                                 
025100       MOVE WS-TIAAAAVV-NUM    TO W-DABEHOV-MAX                           
025200                                                                          
025300     ELSE                                                                 
025400     MOVE 'FEL I WDATKONV, W411RANS (LÄGE 2)' TO ERROR-TEXT               
025500     CALL ABEND USING RKOD-ABEND-MED-DUMP                                 
025600     END-IF                                                               
025700     .                                                                    
025800     EJECT                                                                
025900 BAC-SUMMERA-TPO-BEHOVET                  SECTION.                        
026000                                                                          
026100     PERFORM IMS-GNP-ARTM11                                               
026200                                                                          
026300     PERFORM UNTIL SEGMENT-SAKNAS                                         
026400       COMPUTE RANS-SUTPO-PB-UT =                                         
026500         RANS-SUTPO-PB-UT + ANT-SUTPO-PB                                  
026600       END-COMPUTE                                                        
026700                                                                          
026800       COMPUTE RANS-SUTPO-EJPB-UT =                                       
026900         RANS-SUTPO-EJPB-UT + ANT-SUTPO-EJPB                              
027000       END-COMPUTE                                                        
027100                                                                          
027200       PERFORM IMS-GNP-ARTM11                                             
027300     END-PERFORM                                                          
027400     .                                                                    
027500     EJECT                                                                
027600 D-INLEDANDE-BERAKNINGAR       SECTION.                                   
027700                                                                          
027800     ACCEPT WS-DAGENS-DAT FROM DATE                                       
027900                                                                          
028000     PERFORM DA-BERAKNA-DISP                                              
028100     PERFORM DC-BERAKNA-BEL                                               
028200     PERFORM DE-ADDERA-BESTALLT-TILL-BEL                                  
028300     .                                                                    
028400     EJECT                                                                
028500 DA-BERAKNA-DISP              SECTION.                                    
028600                                                                          
028700     COMPUTE WS-DISP        = RANS-KVLS                                   
028800                            - RANS-KVSPANT                                
028900                            - RANS-KVUTRS                                 
029000                            - RANS-KVRESS                                 
029100                            - ART-KVPREAVB-VOR                            
029200                            - ART-KVPREAVB-DAG                            
029300                                                                          
029400     IF WS-DISP < 0                                                       
029500       MOVE 0                  TO WS-DISP                                 
029600     END-IF                                                               
029700     .                                                                    
029800     EJECT                                                                
029900 DC-BERAKNA-BEL                SECTION.                                   
030000                                                                          
030100     PERFORM DCA-BER-DAGAR-FRAM-TILL-INL                                  
030110     PERFORM DCB-BER-DAGSBEHOV-REFILL                                     
030200                                                                          
030300     COMPUTE WS-BEL = WS-DAG-T-INL                                        
030400                    * (RANS-KVPB-SEP + RANS-KVPB-SATS) * 12 / 260         
030510                    + (WS-DAG-T-INL                                       
030520                    * (WS-KVPB-REF * 12 / 260))                           
030600                    + RANS-SUTPO-PB-UT                                    
030700                    + RANS-SUTPO-EJPB-UT                                  
030800                    + ART-KVOKS-DAG                                       
030900                    + ART-KVOKS-BULK                                      
031000                    - ART-KVPREAVB-DAG                                    
031100     .                                                                    
031200     EJECT                                                                
031300 DCA-BER-DAGAR-FRAM-TILL-INL SECTION.                                     
031400                                                                          
031500     IF RANS-TIDISPIN  = 0                                                
031600       MOVE 0                  TO WS-DAG-T-INL                            
031700                                                                          
031800     ELSE                                                                 
031801       MOVE RANS-TIDISPIN   TO TMP1-YYMMDD                                
031802       MOVE WS-DAGENS-DAT   TO TMP2-YYMMDD                                
031810       PERFORM WY2000P1                                                   
031900       IF TMP1-YYMMDD > TMP2-YYMMDD                                       
032000         MOVE WS-DAGENS-DAT TO WORK-TIAAMMDD-FOM                          
032100         MOVE RANS-TIDISPIN TO WORK-TIAAMMDD-TOM                          
032200         MOVE 001           TO WORK-KDCALL                                
032210         MOVE WC-CDC-SE     TO WORK-IDDC                                  
032300         CALL WORKDAY  USING WORK-KDCALL                                  
032400                             WORK-DATE-AREA                               
032500                             WORK-KDSVAR                                  
032600                                                                          
032700         IF WORK-KDSVAR-OK                                                
032800           COMPUTE WS-DAG-T-INL = WORK-KVWORKD - 2                        
032900                                                                          
033000         ELSE                                                             
033100           MOVE 'FEL I WDATKONV, W411RANS (LÄGE 3)' TO                    
033200                                                    ERROR-TEXT            
033300           CALL ABEND USING RKOD-ABEND-MED-DUMP                           
033400         END-IF                                                           
033500                                                                          
033600         IF WS-DAG-T-INL > 15                                             
033700           MOVE 15                 TO WS-DAG-T-INL                        
033800         END-IF                                                           
033900       ELSE                                                               
034000         MOVE 0                    TO WS-DAG-T-INL                        
034100       END-IF                                                             
034200     END-IF                                                               
034300     .                                                                    
034400     EJECT                                                                
034410 DCB-BER-DAGSBEHOV-REFILL                SECTION.                         
034412     MOVE ZERO                      TO WS-KVPB-REF                        
034413                                                                          
034414     PERFORM IMS-GU-ARTS01                                                
034415                                                                          
034416     IF SEGMENT-FINNS                                                     
034417*STA FIX PGA ABEND I 4375                                                 
034418     IF SART-IDARTNR NOT = 9463173                                        
034419*STO FIX PGA ABEND I 4375                                                 
034420                                                                          
034421       PERFORM IMS-GNP-ARTS11                                             
034422                                                                          
034423       PERFORM UNTIL SEGMENT-SAKNAS                                       
034424                                                                          
034425         IF  SLAG-IDLEVNR = '1441 ' OR 'BP2TW'                            
034426           COMPUTE WS-KVPB-REF      =                                     
034427             WS-KVPB-REF + SLAG-KVPB-REF                                  
034428           END-COMPUTE                                                    
034429         END-IF                                                           
034430                                                                          
034431         PERFORM IMS-GNP-ARTS11                                           
034432       END-PERFORM                                                        
034433     END-IF                                                               
034434     END-IF                                                               
034435     .                                                                    
034440     EJECT                                                                
034500 DE-ADDERA-BESTALLT-TILL-BEL             SECTION.                         
034600                                                                          
034700     IF RANS-KDORDBEH NOT = 7                                             
034800        IF RANS-KDTPOTYP = 0                                              
034900           ADD RANS-KVBEART-Q         TO WS-BEL                           
035000        END-IF                                                            
035100     END-IF                                                               
035200                                                                          
035300     IF WS-BEL < 1                                                        
035400       MOVE 1                  TO WS-BEL                                  
035500     END-IF                                                               
035600     .                                                                    
035700     EJECT                                                                
035800 F-BERAKNA-RANS-FAKT           SECTION.                                   
035900                                                                          
036000     PERFORM FA-BERAKNA-RANS-FAKT                                         
036100                                                                          
036200     PERFORM FD-SATT-RANS-FAKT-PA-ARTREG                                  
036300     PERFORM FE-KONTROLL-MOT-GRAENSVAERDEN                                
036400                                                                          
036500     IF WS-RERF-CX > 0    AND                                             
036600        WS-RERF-CX < 1                                                    
036700       MOVE WS-RERF-CX         TO WS-RERF-NY                              
036800                                                                          
036900       PERFORM FF-LAS-RANSONERINGSTABELL                                  
037000       PERFORM FG-SATT-RATT-RANS-FAKTOR                                   
037100                                                                          
037200       IF WS-RERF-NY NOT = WS-RERF-CX                                     
037300         PERFORM FH-KONTROLL-AV-KORRIGERAD-RERF                           
037400         MOVE WS-RERF-NY                   TO WS-RERF-CX                  
037500       END-IF                                                             
037600     END-IF                                                               
037700                                                                          
037800     MOVE WS-RERF-CX           TO RANS-RERF-RAD-UT                        
037900     .                                                                    
038000     EJECT                                                                
038100 FA-BERAKNA-RANS-FAKT   SECTION.                                          
038200                                                                          
038300     MOVE WS-BEL                         TO WS-BEHOV                      
038400                                                                          
038500     MOVE WS-DISP                        TO WS-TILLGANGARNA               
038600                                                                          
038700     COMPUTE WS-RERF-CX = WS-TILLGANGARNA / WS-BEHOV                      
038800     .                                                                    
038900     EJECT                                                                
039000 FD-SATT-RANS-FAKT-PA-ARTREG             SECTION.                         
039100                                                                          
039200     IF WS-RERF-CX > 9.9999                                               
039300       MOVE 9.9999             TO RANS-RERF-ART-UT                        
039400     ELSE                                                                 
039500       MOVE WS-RERF-CX         TO RANS-RERF-ART-UT                        
039600     END-IF                                                               
039700     .                                                                    
039800     EJECT                                                                
039900 FE-KONTROLL-MOT-GRAENSVAERDEN           SECTION.                         
040000                                                                          
040100     IF WS-RERF-CX > 1                                                    
040200       MOVE 1                  TO WS-RERF-CX                              
040300     END-IF                                                               
040400                                                                          
040500     IF WS-RERF-CX < 0                                                    
040600       MOVE 0                  TO WS-RERF-CX                              
040700     END-IF                                                               
040800     .                                                                    
040900     EJECT                                                                
041000 FF-LAS-RANSONERINGSTABELL              SECTION.                          
041100                                                                          
041200     MOVE RANS-IDRFTAB     TO W-IDRFTAB                                   
041300     PERFORM IMS-GU-XXKM11-GODK-GE                                        
041400                                                                          
041500     IF SEGMENT-SAKNAS                                                    
041600       MOVE 'INT'          TO W-IDRFTAB                                   
041700       PERFORM IMS-GU-XXKM11-GODK-EJ-GE                                   
041800     END-IF                                                               
041900     .                                                                    
042000     EJECT                                                                
042100 FG-SATT-RATT-RANS-FAKTOR               SECTION.                          
042200                                                                          
042300     MOVE 1                TO WS-ACC                                      
042400                                                                          
042500     IF RANS-KDTPOTYP > +0                                                
042600        MOVE +5            TO RANS-KDORDKL                                
042700     END-IF                                                               
042800                                                                          
042900     PERFORM UNTIL WS-ACC > 5                                             
043000       IF WS-RERF-CX >= 4456-RERF-TOM (RANS-KDORDKL, WS-ACC)              
043100         MOVE 4456-RERF-NY  (RANS-KDORDKL, WS-ACC) TO WS-RERF-NY          
043200         MOVE 4456-RERF-FOM (RANS-KDORDKL, WS-ACC) TO WS-RERF-FOM         
043300         MOVE 4456-RERF-TOM (RANS-KDORDKL, WS-ACC) TO WS-RERF-TOM         
043400         MOVE 5            TO WS-ACC                                      
043500       END-IF                                                             
043600                                                                          
043700       ADD 1               TO WS-ACC                                      
043800     END-PERFORM                                                          
043900     .                                                                    
044000     EJECT                                                                
044100 FH-KONTROLL-AV-KORRIGERAD-RERF  SECTION.                                 
044200                                                                          
044300     PERFORM FHA-SATT-MAX-KORRIGERING                                     
044400     PERFORM FHB-SATT-GJORD-KORRIGERING                                   
044500                                                                          
044600     IF WS-RERF-NY > WS-RERF-CX                                           
044700       PERFORM FHC-KORRIGERING-UPPAAT                                     
044800     ELSE                                                                 
044900       PERFORM FHD-KORRIGERING-NEDAAT                                     
045000     END-IF                                                               
045100     .                                                                    
045200     EJECT                                                                
045300 FHA-SATT-MAX-KORRIGERING                SECTION.                         
045400                                                                          
045500     IF WS-RERF-NY > WS-RERF-FOM                                          
045600       COMPUTE WS-KORR-REL-FOM = WS-RERF-NY - WS-RERF-FOM                 
045700     ELSE                                                                 
045800       COMPUTE WS-KORR-REL-FOM = WS-RERF-FOM - WS-RERF-NY                 
045900     END-IF                                                               
046000                                                                          
046100     IF WS-RERF-NY > WS-RERF-TOM                                          
046200       COMPUTE WS-KORR-REL-TOM = WS-RERF-NY - WS-RERF-TOM                 
046300     ELSE                                                                 
046400       COMPUTE WS-KORR-REL-TOM = WS-RERF-TOM - WS-RERF-NY                 
046500     END-IF                                                               
046600                                                                          
046700     IF WS-KORR-REL-FOM > WS-KORR-REL-TOM                                 
046800       MOVE WS-KORR-REL-FOM              TO WS-MAX-KORRIGERING            
046900     ELSE                                                                 
047000       MOVE WS-KORR-REL-TOM              TO WS-MAX-KORRIGERING            
047100     END-IF                                                               
047200     .                                                                    
047300     EJECT                                                                
047400 FHB-SATT-GJORD-KORRIGERING              SECTION.                         
047500                                                                          
047600     IF WS-RERF-NY > WS-RERF-CX                                           
047700       COMPUTE WS-GJORD-KORRIGERING = WS-RERF-NY - WS-RERF-CX             
047800     ELSE                                                                 
047900       COMPUTE WS-GJORD-KORRIGERING = WS-RERF-CX - WS-RERF-NY             
048000     END-IF                                                               
048100     .                                                                    
048200     EJECT                                                                
048300 FHC-KORRIGERING-UPPAAT                  SECTION.                         
048400                                                                          
048500     IF WS-MAX-KORRIGERING > WS-RERF-CX                                   
048600       MOVE WS-RERF-CX                 TO WS-MAX-KORRIGERING              
048700     END-IF                                                               
048800                                                                          
048900     COMPUTE WS-MAX-KORRIGERING =                                         
049000                  WS-MAX-KORRIGERING                                      
049100               * (WS-BEHOV - RANS-KVBEART-Q)                              
049200               / RANS-KVBEART-Q                                           
049300                                                                          
049400     IF WS-GJORD-KORRIGERING > WS-MAX-KORRIGERING                         
049500       COMPUTE WS-RERF-NY = WS-RERF-CX + WS-MAX-KORRIGERING               
049600     END-IF                                                               
049700                                          .                               
049800     PERFORM FHCA-KONTR-FOR-STOR-KORR-UPPAT                               
049900     .                                                                    
050000     EJECT                                                                
050100 FHCA-KONTR-FOR-STOR-KORR-UPPAT          SECTION.                         
050200                                                                          
050300     IF (RANS-KVBEART-Q * WS-RERF-NY) > WS-TILLGANGARNA                   
050400                                                                          
050500       COMPUTE WS-RERF-NY = WS-TILLGANGARNA / RANS-KVBEART-Q              
050600     END-IF                                                               
050700     .                                                                    
050800     EJECT                                                                
050900 FHD-KORRIGERING-NEDAAT                  SECTION.                         
051000                                                                          
051100     IF WS-RERF-CX + WS-MAX-KORRIGERING > WS-FORSLUNDS-KONSTANT           
051200       IF WS-RERF-CX > WS-FORSLUNDS-KONSTANT                              
051300         MOVE 0                          TO WS-MAX-KORRIGERING            
051400       ELSE                                                               
051500         COMPUTE WS-MAX-KORRIGERING =                                     
051600           WS-FORSLUNDS-KONSTANT - WS-RERF-CX                             
051700         END-COMPUTE                                                      
051800       END-IF                                                             
051900     END-IF                                                               
052000                                                                          
052100     COMPUTE WS-MAX-KORRIGERING = WS-MAX-KORRIGERING *                    
052200       (WS-BEL - RANS-KVBEART-Q) / RANS-KVBEART-Q                         
052300                                                                          
052400     IF WS-GJORD-KORRIGERING > WS-MAX-KORRIGERING                         
052500       COMPUTE WS-RERF-NY = WS-RERF-CX - WS-MAX-KORRIGERING               
052600     END-IF                                                               
052700     .                                                                    
052800     EJECT                                                                
052900 IMS-GU-ARTM01                 SECTION.                                   
053000                                                                          
053100     STRING 'WLARTM01(IDARTNR  =' W-IDARTNR-X ')'                         
053200            DELIMITED BY SIZE INTO SSA1                                   
053300     MOVE '  GE'               TO GODK-STATUSKODER                        
053400     CALL CBLTDLI USING GU  ARTM-PCB ART-WDK901 SSA1                      
053500     MOVE ARTM-STATUS-CODE     TO STATUS-WS                               
053600     PERFORM IMS-STATUSKONTROLL                                           
053700     .                                                                    
053800     SKIP2                                                                
053900 IMS-GNP-ARTM11                SECTION.                                   
054000                                                                          
054100     STRING 'WLARTM11(DABEHOV >=' W-DABEHOV-MIN-X                         
054200                    '&DABEHOV  <' W-DABEHOV-MAX-X ')'                     
054300            DELIMITED BY SIZE INTO SSA1                                   
054400     MOVE '  GE'               TO GODK-STATUSKODER                        
054500     CALL CBLTDLI USING GNP ARTM-PCB ANT-WDK911 SSA1                      
054600     MOVE ARTM-STATUS-CODE     TO STATUS-WS                               
054700     PERFORM IMS-STATUSKONTROLL                                           
054800     .                                                                    
054900     EJECT                                                                
054910 IMS-GU-ARTS01                 SECTION.                                   
054920                                                                          
054930     STRING 'WLARTS01(IDARTNR  =' W-IDARTNR-X ')'                         
054940            DELIMITED BY SIZE INTO SSA1                                   
054950     MOVE '  GE'               TO GODK-STATUSKODER                        
054960     CALL CBLTDLI USING GU  ARTS-PCB SART-WDK701 SSA1                     
054970     MOVE ARTS-STATUS-CODE     TO STATUS-WS                               
054980     PERFORM IMS-STATUSKONTROLL                                           
054990     .                                                                    
054991     SKIP2                                                                
054992 IMS-GNP-ARTS11                SECTION.                                   
054993                                                                          
054997     MOVE 'WLARTS11 ' TO SSA1                                             
054999     MOVE '  GE'               TO GODK-STATUSKODER                        
055000     CALL CBLTDLI USING GNP ARTS-PCB SLAG-WDK711 SSA1                     
055001     MOVE ARTS-STATUS-CODE     TO STATUS-WS                               
055002     PERFORM IMS-STATUSKONTROLL                                           
055003     .                                                                    
055004     EJECT                                                                
055010 IMS-GU-XXKM11-GODK-GE         SECTION.                                   
055100     SKIP2                                                                
055200     STRING 'WLXXKM01(WDGXKEY  =' W-WDGXKEY-4455-X ')'                    
055300            DELIMITED BY SIZE INTO SSA1                                   
055400     STRING 'WLXXKM11(WDGXKEY  =' W-WDGXKEY-4456-X ')'                    
055500            DELIMITED BY SIZE INTO SSA2                                   
055600     MOVE '  GE'               TO GODK-STATUSKODER                        
055700     CALL CBLTDLI USING GU  XXKM-PCB 4456-WDGX4456 SSA1 SSA2              
055800     MOVE XXKM-STATUS-CODE     TO STATUS-WS                               
055900     PERFORM IMS-STATUSKONTROLL                                           
056000     .                                                                    
056100     SKIP2                                                                
056200 IMS-GU-XXKM11-GODK-EJ-GE      SECTION.                                   
056300     SKIP2                                                                
056400     STRING 'WLXXKM01(WDGXKEY  =' W-WDGXKEY-4455-X ')'                    
056500            DELIMITED BY SIZE INTO SSA1                                   
056600     STRING 'WLXXKM11(WDGXKEY  =' W-WDGXKEY-4456-X ')'                    
056700            DELIMITED BY SIZE INTO SSA2                                   
056800     MOVE '  '                 TO GODK-STATUSKODER                        
056900     CALL CBLTDLI USING GU  XXKM-PCB 4456-WDGX4456 SSA1 SSA2              
057000     MOVE XXKM-STATUS-CODE     TO STATUS-WS                               
057100     PERFORM IMS-STATUSKONTROLL                                           
057200     .                                                                    
057300     SKIP2                                                                
057400 IMS-STATUSKONTROLL            SECTION.                                   
057500     SKIP2                                                                
057600     SET STATUS-IX             TO 1                                       
057700     SEARCH GODK-STATUS AT END CALL FELLOG                                
057800       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
057900     END-SEARCH                                                           
058000     .                                                                    
058010     EJECT                                                                
058100*    -COPY WY2000P1                                                       
