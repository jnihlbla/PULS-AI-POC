000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W2714200.                                                
000400*AUTHOR.         ANETTE HERMANSSON.                                       
000500*DATE-WRITTEN.   95/01/16.                                                
000600                                                                          
000700*    REMARKS                                                              
000800*                                                                         
000900*    FUNKTION:                                                            
001000*        BEVAKA PASSIVERING AV ARTIKEL                                    
001100*                                                                         
001200*        PROGRAMMET UPPDATERAR WDK7                                       
001300*                              WDL7                                       
001400*                                                                         
001500*        PROGRAM READS         WDK6                                       
001600*                              WDK7                                       
001700*                              WDL6                                       
001800*                              WDL7                                       
001900*                              WDB6                                       
002000*                                                                         
002100*    ABENDKODER:                                                          
002200*        U0016 -  . . . .                                                 
002300*        U1000 -  . . . .                                                 
002400*                                                                         
002500                                                                          
002600     SKIP3                                                                
002700 ENVIRONMENT DIVISION.                                                    
002800     SKIP2                                                                
002900 INPUT-OUTPUT SECTION.                                                    
003000                                                                          
003100 FILE-CONTROL.                                                            
003200     EJECT                                                                
003300 DATA DIVISION.                                                           
003400     SKIP3                                                                
003500 FILE SECTION.                                                            
003600     EJECT                                                                
003700 WORKING-STORAGE SECTION.                                                 
003800     SKIP2                                                                
003900*    -COPY WY2000W1                                                       
004000     SKIP3                                                                
004100*    -COPY WWDC99                                                         
004200     SKIP3                                                                
004300 77  IDPGM                       PIC X(8)    VALUE 'W2714200'.            
004400 01  CHKP-VAR.                                                            
004500 03  CHKP-MSG-IO-AREA-LENGTH     PIC S9(9)   VALUE +32 COMP SYNC.         
004600 03  CHKP-MSG-IO-AREA            PIC X(32)   VALUE SPACE.                 
004700 03  CHKP-AREA-LENGTH            PIC S9(9)   VALUE +32 COMP SYNC.         
004800 03  CHKP-AREA                   PIC X(32)   VALUE SPACE.                 
004900 03  CHKP-ANT                    PIC S9(3)   VALUE +0.                    
005000 03  CHKP-MAX                    PIC S9(3)   VALUE +100.                  
005100 77  YES                         PIC X       VALUE 'Y'.                   
005200 77  NOO                         PIC X       VALUE 'N'.                   
005300 77  STOPP                       PIC X       VALUE 'S'.                   
005400 77  JA                          PIC X       VALUE 'J'.                   
005500 77  NEJ                         PIC X       VALUE 'N'.                   
005600     88  WDK712-FINNS                        VALUE 'J'.                   
005700     SKIP2                                                                
005800 01  FELTEXT.                                                             
005900     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
006000     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
006100     EJECT                                                                
006200 01  ARBETSFAELT.                                                         
006300     03  FILLER                  PIC X(10)   VALUE 'ARBETSFÄLT'.          
006400*                                                                         
006500     03  IX1                     PIC 9(2)    VALUE ZERO.                  
006600     03  IX2                     PIC 9(2)    VALUE ZERO.                  
006700     03  MAX-IX1                 PIC 9(2)    VALUE 13.                    
006800     03  MAX-IX2                 PIC 9(2)    VALUE 10.                    
006900     03  WS-DC-FLAG              PIC X(1)    VALUE 'N'.                   
007000     03  WS-PASS-FLAG            PIC X(1)    VALUE 'N'.                   
007100     03  WS-ADLAGOMR-FLAG        PIC X(1)    VALUE 'N'.                   
007200     03  WS-ALL-ZERO             PIC X(1)    VALUE 'N'.                   
007300     03  WS-TIINLINL-VALID       PIC X(1)    VALUE 'N'.                   
007400     03  WS-WEEKS                PIC 9(5)    VALUE ZERO.                  
007500     03  TODAYS-DATE             PIC 9(6)    VALUE ZERO.                  
007600     03  WS-DATE                 PIC 9(6)    VALUE ZERO.                  
007700     03  WS-DATE-YYWWD           PIC 9(5)    VALUE ZERO.                  
007800     03  WS-KDPRODSL-601         PIC 9(2)    VALUE ZERO.                  
007900     03  WS-TIINLINL             PIC 9(6)    VALUE ZERO.                  
008000     03  WS-PRIS                 PIC S9(7)V9(2) VALUE ZERO COMP-3.        
008100*                                                                         
008200 01  WS-TABLE-ARRAY.                                                      
008300     03  WS-TABLE OCCURS 150 INDEXED BY IX.                               
008400       05  WS-TAB-IDDC           PIC X(2).                                
008500       05  WS-PASS-WDB613 OCCURS 13.                                      
008600           10 WS-KVVECKOR-LSALES PIC S9(3) COMP-3.                        
008700           10 WS-KVVECKOR-PUBV   PIC S9(3) COMP-3.                        
008800           10 WS-PRARTSTD        PIC 9(7).                                
008900           10 WS-VLARTNTO        PIC 9(8).                                
009000           10 WS-KDPRODSL        PIC S9(3) COMP-3.                        
009100           10 WS-ADLAGOMR OCCURS 10 TIMES PIC S9(3) COMP-3.               
009200                                                                          
009300 01  WS-VLARTNTO-JFR             PIC 9(8)V9(1) VALUE ZERO.                
009400 01  FILLER REDEFINES WS-VLARTNTO-JFR.                                    
009500     03  WS-VLARTNTO-HELTAL      PIC 9(8).                                
009600     03  WS-VLARTNTO-DECIMAL     PIC 9(1).                                
009700                                                                          
009800 01  DYNAMISKA-SUBPROGRAM.                                                
009900*                                                                         
010000     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
010100     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
010200     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
010300     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM '.            
010400     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
010500     03  DATKORT                 PIC X(8)    VALUE 'DATKORT'.             
010600     03  WZ20DAYS                PIC X(8)    VALUE 'WZ20DAYS'.            
010700     EJECT                                                                
010800*    --- PARAMETRAR TILL ABEND                                            
010900                                                                          
011000 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
011100 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
011200     SKIP2                                                                
011300     EJECT                                                                
011400*    --- PARAMETRAR TILL POSTSUM                                          
011500*                                                                         
011600*01  -COPY W0005   -PRE  POSTSUM-                                         
011700     EJECT                                                                
011800*    --- PARAMETRAR TILL WDATKONV                                         
011900*                                                                         
012000*01  -COPY WDATAREA                                                       
012100*                                                                         
012200*    --- PARAMETERS FOR WZ20DAYS SUBPROGRAM                               
012300*01  -COPY WZ20DAYS                                                       
012400*                                                                         
012500*    --- PARAMETRAR TILL DATKORT                                          
012600 01  PROGRAM-NAMN                PIC X(6)    VALUE 'W27142'.              
012700     SKIP2                                                                
012800 01  DATUMKORT-ID                PIC X(6)    VALUE 'WDATUM'.              
012900     SKIP2                                                                
013000*01  -COPY WDATKORT                                                       
013100     EJECT                                                                
013200 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
013300     SKIP3                                                                
013400 01  NYCKLAR-TILL-DLI.                                                    
013500     03  W-IDARTNR-X.                                                     
013600         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
013700     03  W-KDSEGKEY-X.                                                    
013800         05  W-KDSEGKEY          PIC X(1)    VALUE '1'.                   
013900     03  W-IDDC-X.                                                        
014000         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
014100     03  W-IDLAND-X.                                                      
014200         05  W-IDLAND            PIC X(2)    VALUE SPACE.                 
014300     03  W-IDDC-MIN-X.                                                    
014400         05  W-IDDC-MIN          PIC X(2)    VALUE SPACE.                 
014500     03  W-DAINLEV-MIN-X.                                                 
014600         05  W-DAINLEV-MIN       PIC 9(16)   VALUE ZERO.                  
014700     03  W-DAINLEV-MAX-X.                                                 
014800         05  W-DAINLEV-MAX       PIC 9(16)   VALUE                        
014900                                             9999999999999999.            
015000*                                                                         
015100*    --- STATUS-KOD FRÅN IMS                                              
015200 01  STATUS-WS                   PIC XX.                                  
015300     88  SEGMENT-FINNS                       VALUE '  '.                  
015400     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
015500     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
015600     88  SEGMENT-SLUT                        VALUE 'GB'.                  
015700     88  IMS-EJ-OK                           VALUE 'XD'.                  
015800     SKIP2                                                                
015900 01  GODK-STATUSKODER.                                                    
016000     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
016100     SKIP3                                                                
016200 01  SSA1                        PIC X(128).                              
016300 01  SSA2                        PIC X(64).                               
016400     EJECT                                                                
016500*    --- IMS FUNKTIONSKODER                                               
016600*01  -COPY W0003                                                          
016700     EJECT                                                                
016800*    ---  DLI INPUT-OUTPUT AREA                                           
016900 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK601'.                      
017000 01  DLI-IO-WDK601.                                                       
017100*    03  -COPY WDK601                                                     
017200*                                                                         
017300 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK611'.                      
017400 01  DLI-IO-WDK611.                                                       
017500*    03  -COPY WDK611                                                     
017600*                                                                         
017700 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK701'.                      
017800 01  DLI-IO-WDK701.                                                       
017900*    03  -COPY WDK701                                                     
018000*                                                                         
018100 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK711'.                      
018200 01  DLI-IO-WDK711.                                                       
018300*    03  -COPY WDK711                                                     
018400*                                                                         
018500 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK712'.                      
018600 01  DLI-IO-WDK712.                                                       
018700*    03  -COPY WDK712                                                     
018800*                                                                         
018900 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDL601'.                      
019000 01  DLI-IO-WDL601.                                                       
019100*    03  -COPY WDL601                                                     
019200*                                                                         
019300 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDL611'.                      
019400 01  DLI-IO-WDL611.                                                       
019500*    03  -COPY WDL611                                                     
019600*                                                                         
019700 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDL711'.                      
019800 01  DLI-IO-WDL711.                                                       
019900*    03  -COPY WDL711                                                     
020000*                                                                         
020100 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB601'.                      
020200 01  DLI-IO-WDB601.                                                       
020300*    03  -COPY WDB601                                                     
020400*                                                                         
020500 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB613'.                      
020600 01  DLI-IO-WDB613.                                                       
020700*    03  -COPY WDB613                                                     
020800*                                                                         
020900 LINKAGE SECTION.                                                         
021000                                                                          
021100*01  -COPY W0009  -PRE MSG-                                               
021200     EJECT                                                                
021300*01  -COPY W0008  -PRE WDK6-                                              
021400     05  FILLER                  PIC X.                                   
021500     EJECT                                                                
021600*01  -COPY W0008  -PRE WDK7-                                              
021700     05  FILLER                  PIC X.                                   
021800     EJECT                                                                
021900*01  -COPY W0008  -PRE WDK7-2-                                            
022000     05  FILLER                  PIC X.                                   
022100     EJECT                                                                
022200*01  -COPY W0008  -PRE WDL6-                                              
022300     05  FILLER                  PIC X.                                   
022400     EJECT                                                                
022500*01  -COPY W0008  -PRE WDL7-                                              
022600     05  FILLER                  PIC X.                                   
022700     EJECT                                                                
022800*01  -COPY W0008  -PRE WDB6-                                              
022900     05  FILLER                  PIC X.                                   
023000     EJECT                                                                
023100 PROCEDURE DIVISION  USING MSG-PCB  WDK6-PCB WDK7-PCB WDK7-2-PCB          
023200                           WDL6-PCB WDL7-PCB WDB6-PCB.                    
023300     ENTRY 'DLITCBL' USING MSG-PCB  WDK6-PCB WDK7-PCB WDK7-2-PCB          
023400                           WDL6-PCB WDL7-PCB WDB6-PCB.                    
023500                                                                          
023600     PERFORM A-INIT                                                       
023700                                                                          
023800     PERFORM IMS-GN-WDK701                                                
023900                                                                          
024000     PERFORM UNTIL SEGMENT-SLUT                                           
024100       IF CHKP-ANT > CHKP-MAX                                             
024200         PERFORM X-TAG-CHECKPOINT                                         
024300       END-IF                                                             
024400*                                                                         
024500       MOVE SART-IDARTNR TO W-IDARTNR                                     
024600       PERFORM IMS-GHNP-WDK711                                            
024700*                                                                         
024800       PERFORM UNTIL SEGMENT-SAKNAS                                       
024900         MOVE SART-IDARTNR          TO W-IDARTNR                          
025000         MOVE SLAG-IDDC             TO W-IDDC                             
025100                                       WS-IDDC                            
025200         IF SLAG-IDDC-REF = SPACE AND                                     
025300            (NDC-CN OR                                                    
025500             NDC-US)                                                      
025600*           SLAG-IDDC(1:1) = '7'                                          
025700           CONTINUE                                                       
025800         ELSE                                                             
025900           PERFORM B-SEARCH-CORRECT-DC                                    
026000           IF SLAG-KDREFSTA = 'A'                                         
026100                                                                          
026200             IF SDC OR LDC                                                
026600                 PERFORM IMS-GU-WDK601                                    
026700                 PERFORM IMS-GNP-WDK611                                   
026800                 IF SEGMENT-FINNS                                         
027200****PASSIVATE PARTS WITH DIRECT DELIVERY FOR ALL SDC AND LDC              
027300****IN EU EXCEPT DC 21                                                    
027400****                                                                      
027600                   IF (CLAG-REDIRLEV = 1.00                               
027700                   AND NOT SDC-NL)                                        
027800                     PERFORM E-UPPDAT-WDK711                              
027900                     MOVE 'N' TO WS-DC-FLAG                               
028000                   END-IF                                                 
028100                 END-IF                                                   
028300             END-IF                                                       
028400                                                                          
028500             MOVE 'YYMMDD'       TO DAYS-KDDATFMT1                        
028600             MOVE SLAG-TIREFSTA  TO WS-DATE                               
028700             MOVE WS-DATE        TO DAYS-TIDATE1                          
028800             PERFORM S01-GET-WEEKS-WZ20DAYS                               
028900             IF DAYS-KVDAYS > 180                                         
029000               IF WS-DC-FLAG = 'Y'                                        
029100                 PERFORM C-CHECK-PASSIVATION-RULES                        
029200               END-IF                                                     
029300             END-IF                                                       
029400           END-IF                                                         
029500         END-IF                                                           
029600         PERFORM IMS-GHNP-WDK711                                          
029700       END-PERFORM                                                        
029800*                                                                         
029900       PERFORM IMS-GN-WDK701                                              
030000     END-PERFORM                                                          
030100                                                                          
030200     MOVE ZERO TO RETURN-CODE                                             
030300     GOBACK                                                               
030400     .                                                                    
030500 A-INIT SECTION.                                                          
030600     INITIALIZE WS-TABLE-ARRAY                                            
030700                                                                          
030800     PERFORM AA-LOAD-WDB6-TABLE                                           
030900                                                                          
031000     PERFORM IMS-RESTART                                                  
031100     ACCEPT TODAYS-DATE FROM DATE                                         
031200     .                                                                    
031300 AA-LOAD-WDB6-TABLE SECTION.                                              
031400     PERFORM IMS-GU-WDB601                                                
031500     SET IX TO 1                                                          
031600     PERFORM UNTIL SEGMENT-SLUT                                           
031700       MOVE DCS-IDDC               TO WS-TAB-IDDC (IX)                    
031800*                                                                         
031900       PERFORM IMS-GNP-WDB613                                             
032000       MOVE +1 TO IX1                                                     
032100       PERFORM UNTIL SEGMENT-SAKNAS                                       
032200         MOVE PASS-KVVECKOR-LSALES TO WS-KVVECKOR-LSALES (IX IX1)         
032300         MOVE PASS-KVVECKOR-PUBV   TO WS-KVVECKOR-PUBV   (IX IX1)         
032400         MOVE PASS-PRARTSTD        TO WS-PRARTSTD        (IX IX1)         
032500         MOVE PASS-VLARTNTO        TO WS-VLARTNTO        (IX IX1)         
032600         MOVE PASS-KDPRODSL        TO WS-KDPRODSL        (IX IX1)         
032700         MOVE +1 TO IX2                                                   
032800         PERFORM UNTIL IX2 > MAX-IX2                                      
032900           MOVE PASS-ADLAGOMR (IX2) TO WS-ADLAGOMR (IX IX1 IX2)           
033000           ADD +1 TO IX2                                                  
033100         END-PERFORM                                                      
033200         PERFORM IMS-GNP-WDB613                                           
033300         ADD +1 TO IX1                                                    
033400       END-PERFORM                                                        
033500*                                                                         
033600       PERFORM IMS-GN-WDB601                                              
033700       SET IX UP BY 1                                                     
033800     END-PERFORM                                                          
033900     .                                                                    
034000 B-SEARCH-CORRECT-DC SECTION.                                             
034100     SET IX TO 1                                                          
034200     MOVE YES TO WS-DC-FLAG                                               
034300     SEARCH WS-TABLE                                                      
034400       AT END                                                             
034500         MOVE 'N'  TO WS-DC-FLAG                                          
034600       WHEN WS-TAB-IDDC (IX) = W-IDDC AND                                 
034700            WS-KVVECKOR-LSALES (IX 1) > 0                                 
034800         CONTINUE                                                         
034900     END-SEARCH                                                           
035000     .                                                                    
035100 C-CHECK-PASSIVATION-RULES SECTION.                                       
035200     MOVE +1  TO IX1                                                      
035300     MOVE NOO TO WS-PASS-FLAG                                             
035400*                                                                         
035500     PERFORM UNTIL IX1 > MAX-IX1 OR WS-PASS-FLAG = 'Y'                    
035600                OR WS-KVVECKOR-LSALES (IX IX1) = 0                        
035700       MOVE NOO TO WS-PASS-FLAG                                           
035800       PERFORM CA-CHECK-LSALES-RULES                                      
035900                                                                          
036000       IF WS-PASS-FLAG = 'Y'                                              
036100         PERFORM CB-CHECK-PUBV-RULES                                      
036200       END-IF                                                             
036300                                                                          
036400       IF WS-PASS-FLAG = 'Y'                                              
036500         PERFORM CC-CHECK-PRICE-RULES                                     
036600       END-IF                                                             
036700                                                                          
036800       IF WS-PASS-FLAG = 'Y'                                              
036900         PERFORM CD-CHECK-VOLUME-RULES                                    
037000       END-IF                                                             
037100                                                                          
037200       IF WS-PASS-FLAG = 'Y'                                              
037300         PERFORM CE-CHECK-PRODSL-RULES                                    
037400       END-IF                                                             
037500                                                                          
037600       IF WS-PASS-FLAG = 'Y'                                              
037700         PERFORM CF-CHECK-ADLAGOMR-RULES                                  
037800       END-IF                                                             
037900       ADD +1 TO IX1                                                      
038000     END-PERFORM                                                          
038100                                                                          
038200     IF WS-PASS-FLAG = 'Y'                                                
038300       PERFORM E-UPPDAT-WDK711                                            
038400     END-IF                                                               
038500     .                                                                    
038600 CA-CHECK-LSALES-RULES SECTION.                                           
038700     PERFORM IMS-GU-WDL711                                                
038800     IF SEGMENT-FINNS AND DC-TIREFEFT > 0                                 
038900       MOVE 'YYMMDD'       TO DAYS-KDDATFMT1                              
039000       MOVE DC-TIREFEFT    TO WS-DATE                                     
039100       MOVE WS-DATE        TO DAYS-TIDATE1                                
039200       PERFORM S01-GET-WEEKS-WZ20DAYS                                     
039300       IF WS-WEEKS >= WS-KVVECKOR-LSALES (IX IX1)                         
039400         MOVE YES TO WS-PASS-FLAG                                         
039500       ELSE                                                               
039600         MOVE NOO TO WS-PASS-FLAG                                         
039700       END-IF                                                             
039800     ELSE                                                                 
039900       PERFORM IMS-GU-WDL601                                              
040000       IF SEGMENT-FINNS                                                   
040100         PERFORM IMS-GNP-WDL611                                           
040200         MOVE ZERO TO WS-TIINLINL                                         
040300         PERFORM UNTIL SEGMENT-SAKNAS                                     
040400           IF INL-IDDC = W-IDDC                                           
040500             MOVE INL-TIINLINL TO TMP1-YYMMDD                             
040600             MOVE WS-TIINLINL  TO TMP2-YYMMDD                             
040700             PERFORM WY2000P1                                             
040800             IF TMP1-YYMMDD > TMP2-YYMMDD                                 
040900               MOVE INL-TIINLINL TO WS-TIINLINL                           
041000             END-IF                                                       
041100           END-IF                                                         
041200           PERFORM IMS-GNP-WDL611                                         
041300         END-PERFORM                                                      
041400         IF WS-TIINLINL  > 0                                              
041500           MOVE 'YYMMDD'       TO DAYS-KDDATFMT1                          
041600           MOVE WS-TIINLINL    TO WS-DATE                                 
041700           MOVE WS-DATE        TO DAYS-TIDATE1                            
041800           PERFORM S01-GET-WEEKS-WZ20DAYS                                 
041900           IF WS-WEEKS >= WS-KVVECKOR-LSALES (IX IX1)                     
042000             MOVE YES TO WS-PASS-FLAG                                     
042100           ELSE                                                           
042200             MOVE NOO TO WS-PASS-FLAG                                     
042300           END-IF                                                         
042400         END-IF                                                           
042500       END-IF                                                             
042600     END-IF                                                               
042700     .                                                                    
042800 CB-CHECK-PUBV-RULES SECTION.                                             
042900     PERFORM IMS-GU-WDB601-KY                                             
043000     MOVE DCS-IDLANDX2   TO W-IDLAND                                      
043100     PERFORM IMS-GU-WDK712                                                
043200     IF SEGMENT-FINNS AND LART-DAPUBL > 0                                 
043300       MOVE 'YYYYMMDD'     TO DAYS-KDDATFMT1                              
043400       MOVE LART-DAPUBL    TO DAYS-TIDATE1                                
043500       PERFORM S01-GET-WEEKS-WZ20DAYS                                     
043600       IF (WS-WEEKS >= WS-KVVECKOR-PUBV (IX IX1) OR                       
043700           WS-KVVECKOR-PUBV (IX IX1) = 0 )                                
043800         MOVE YES TO WS-PASS-FLAG                                         
043900       ELSE                                                               
044000         MOVE NOO TO WS-PASS-FLAG                                         
044100       END-IF                                                             
044200     ELSE                                                                 
044300       PERFORM IMS-GU-WDK601                                              
044400       IF SEGMENT-FINNS                                                   
044500         MOVE 'YYWWD'        TO DAYS-KDDATFMT1                            
044600         MOVE ART-TIFINLV    TO WS-DATE-YYWWD                             
044700         MOVE WS-DATE-YYWWD  TO DAYS-TIDATE1                              
044800         PERFORM S01-GET-WEEKS-WZ20DAYS                                   
044900         IF (WS-WEEKS >= WS-KVVECKOR-PUBV (IX IX1) OR                     
045000           WS-KVVECKOR-PUBV (IX IX1) = 0 )                                
045100           MOVE YES TO WS-PASS-FLAG                                       
045200         ELSE                                                             
045300           MOVE NOO TO WS-PASS-FLAG                                       
045400         END-IF                                                           
045500       END-IF                                                             
045600     END-IF                                                               
045700     .                                                                    
045800     EJECT                                                                
045900 CC-CHECK-PRICE-RULES SECTION.                                            
046000     PERFORM IMS-GU-WDK601                                                
046100     IF SEGMENT-FINNS                                                     
046200       MOVE ART-KDPRODSL TO WS-KDPRODSL-601                               
046300       PERFORM IMS-GNP-WDK611                                             
046400       IF SEGMENT-FINNS                                                   
046500         MOVE CLAG-PRARTSTD TO WS-PRIS                                    
046600       END-IF                                                             
046700       IF DCS-CHINA OR DCS-NDC-NA                                         
046800         MOVE LART-PRMATRL  TO WS-PRIS                                    
046900       END-IF                                                             
047000       IF (WS-PRIS >= WS-PRARTSTD(IX IX1) OR                              
047100           WS-PRARTSTD(IX IX1) = 0)                                       
047200         MOVE YES TO WS-PASS-FLAG                                         
047300       ELSE                                                               
047400         MOVE NOO TO WS-PASS-FLAG                                         
047500       END-IF                                                             
047600     END-IF                                                               
047700     .                                                                    
047800 CD-CHECK-VOLUME-RULES SECTION.                                           
047900     IF NDC-CN OR NDC-NA                                                  
048000       IF LART-VLARTNTO >  0                                              
048100         MOVE LART-VLARTNTO   TO WS-VLARTNTO-JFR                          
048200       ELSE                                                               
048300         MOVE CLAG-VLARTNTO   TO WS-VLARTNTO-JFR                          
048400       END-IF                                                             
048500     ELSE                                                                 
048600       MOVE CLAG-VLARTNTO     TO WS-VLARTNTO-JFR                          
048700     END-IF                                                               
048800                                                                          
048900     IF (WS-VLARTNTO-HELTAL >= WS-VLARTNTO(IX IX1) OR                     
049000         WS-VLARTNTO(IX IX1) = 0)                                         
049100       MOVE YES TO WS-PASS-FLAG                                           
049200     ELSE                                                                 
049300       MOVE NOO TO WS-PASS-FLAG                                           
049400     END-IF                                                               
049500     .                                                                    
049600 CE-CHECK-PRODSL-RULES SECTION.                                           
049700     IF (WS-KDPRODSL-601 = WS-KDPRODSL(IX IX1) OR                         
049800         WS-KDPRODSL(IX IX1) = 0)                                         
049900       MOVE YES TO WS-PASS-FLAG                                           
050000     ELSE                                                                 
050100       MOVE NOO TO WS-PASS-FLAG                                           
050200     END-IF                                                               
050300     .                                                                    
050400 CF-CHECK-ADLAGOMR-RULES SECTION.                                         
050500     MOVE NOO TO WS-ADLAGOMR-FLAG                                         
050600     MOVE +1 TO IX2                                                       
050700     PERFORM UNTIL IX2 > MAX-IX2 OR WS-ADLAGOMR-FLAG = 'Y'                
050800       IF SLAG-ADLAGOMR = WS-ADLAGOMR(IX IX1 IX2)                         
050900         AND WS-ADLAGOMR(IX IX1 IX2) > 0                                  
051000         MOVE YES TO WS-PASS-FLAG                                         
051100         MOVE YES TO WS-ADLAGOMR-FLAG                                     
051200       ELSE                                                               
051300         MOVE NOO TO WS-PASS-FLAG                                         
051400       END-IF                                                             
051500       ADD +1 TO IX2                                                      
051600     END-PERFORM                                                          
051700*                                                                         
051800     IF WS-ADLAGOMR-FLAG = 'N'                                            
051900       MOVE +1 TO IX2                                                     
052000       MOVE YES TO WS-ALL-ZERO                                            
052100       PERFORM UNTIL IX2 > MAX-IX2 OR WS-ALL-ZERO = 'N'                   
052200         IF WS-ADLAGOMR(IX IX1 IX2) = 0                                   
052300           MOVE YES TO WS-PASS-FLAG                                       
052400         ELSE                                                             
052500           MOVE NOO TO WS-PASS-FLAG                                       
052600           MOVE NOO TO WS-ALL-ZERO                                        
052700         END-IF                                                           
052800         ADD +1 TO IX2                                                    
052900       END-PERFORM                                                        
053000     END-IF                                                               
053100     .                                                                    
053200 E-UPPDAT-WDK711 SECTION.                                                 
053300*    PASSIVERA EN ARTIKEL PÅ ARTS-BASEN (WDK7)                            
053400     MOVE 'P'          TO SLAG-KDREFSTA                                   
053500     MOVE TODAYS-DATE  TO SLAG-TIREFSTA                                   
053600     MOVE NOO          TO SLAG-FLREFNYO                                   
053700     MOVE +0           TO SLAG-KVPB-REF                                   
053800                          SLAG-KVPB-HIST                                  
053900                          SLAG-KVPBREOI                                   
054000                          SLAG-KVPBREOI-HIST                              
054100                          SLAG-KVREFBER                                   
054200                          SLAG-KVREFOVL                                   
054300                          SLAG-KVREFPKT                                   
054400                          SLAG-TIREFMPB                                   
054500                          SLAG-TIREFPAF                                   
054600                          SLAG-TIREFPKT                                   
054700                          SLAG-TIREFSTO                                   
054800     IF SLAG-FLREFBEO = 'S'                                               
054900       CONTINUE                                                           
055000     ELSE                                                                 
055100       MOVE NOO        TO SLAG-FLREFBEO                                   
055200       IF SDC OR LDC                                                      
055300         IF CLAG-KDERS  = 09                                              
055400            MOVE STOPP TO SLAG-FLREFBEO                                   
055500         END-IF                                                           
055600       END-IF                                                             
055700     END-IF                                                               
055800                                                                          
055900     PERFORM IMS-REPL-WDK711                                              
056000     ADD +1 TO CHKP-ANT                                                   
056100     .                                                                    
056200 S01-GET-WEEKS-WZ20DAYS SECTION.                                          
056300     MOVE ZERO           TO DAYS-KVDAYS                                   
056400     MOVE 'YYMMDD'       TO DAYS-KDDATFMT2                                
056500     MOVE TODAYS-DATE    TO DAYS-TIDATE2                                  
056600                                                                          
056700     CALL WZ20DAYS USING DAYS-WZ20DAYS                                    
056800*                                                                         
056900     IF DAYS-KDRC = +0                                                    
057000       COMPUTE WS-WEEKS = DAYS-KVDAYS / 7                                 
057100     ELSE                                                                 
057200       MOVE 'ERROR FROM WZ20DAYS MODULE' TO FELTEXT-STR                   
057300       DISPLAY FELTEXT                                                    
057400       DISPLAY DAYS-KDRC                                                  
057500       CALL FELLOG                                                        
057600     END-IF                                                               
057700     .                                                                    
057800 S99-ABEND SECTION.                                                       
057900     MOVE 'S' TO POSTSUM-OPKOD                                            
058000     CALL POSTSUM USING POSTSUM-PARM                                      
058100     CALL ABEND USING RKOD-ABEND-UTAN-DUMP                                
058200     .                                                                    
058300 X-TAG-CHECKPOINT   SECTION.                                              
058400     MOVE SART-IDARTNR TO W-IDARTNR                                       
058500                                                                          
058600     PERFORM IMS-CHECKPOINT                                               
058700     MOVE ZERO TO CHKP-ANT                                                
058800                                                                          
058900     PERFORM IMS-GU-WDK701                                                
059000     .                                                                    
059100* --- IMS SEKTIONER ---                                                   
059200 IMS-GU-WDK601 SECTION.                                                   
059300     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X  ')'                        
059400          DELIMITED BY SIZE INTO SSA1                                     
059500     MOVE '  ' TO GODK-STATUSKODER                                        
059600     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK601 SSA1                    
059700     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
059800     PERFORM IMS-STATUSKONTROLL                                           
059900     .                                                                    
060000 IMS-GNP-WDK611 SECTION.                                                  
060100     MOVE 'WDK611' TO SSA1                                                
060200     MOVE '  GE'   TO GODK-STATUSKODER                                    
060300     CALL CBLTDLI USING GNP WDK6-PCB DLI-IO-WDK611 SSA1                   
060400     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
060500     PERFORM IMS-STATUSKONTROLL                                           
060600     .                                                                    
060700 IMS-GU-WDK701 SECTION.                                                   
060800     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X  ')'                        
060900          DELIMITED BY SIZE INTO SSA1                                     
061000     MOVE '  ' TO GODK-STATUSKODER                                        
061100     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-WDK701 SSA1                    
061200     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
061300     PERFORM IMS-STATUSKONTROLL                                           
061400     .                                                                    
061500 IMS-GN-WDK701 SECTION.                                                   
061600     MOVE 'WDK701'    TO SSA1                                             
061700     MOVE '  GBGE' TO GODK-STATUSKODER                                    
061800     CALL CBLTDLI USING GN WDK7-PCB DLI-IO-WDK701 SSA1                    
061900     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
062000     PERFORM IMS-STATUSKONTROLL                                           
062100     .                                                                    
062200 IMS-GHNP-WDK711 SECTION.                                                 
062300*    MOVE 'WDK711  *F'  TO SSA1                                           
062400     MOVE 'WDK711 ' TO SSA1                                               
062500     MOVE '  GE' TO GODK-STATUSKODER                                      
062600     CALL CBLTDLI USING GHNP WDK7-PCB DLI-IO-WDK711 SSA1                  
062700     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
062800     PERFORM IMS-STATUSKONTROLL                                           
062900     .                                                                    
063000 IMS-REPL-WDK711 SECTION.                                                 
063100     MOVE '  ' TO GODK-STATUSKODER                                        
063200     CALL CBLTDLI USING REPL WDK7-PCB DLI-IO-WDK711                       
063300     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
063400     PERFORM IMS-STATUSKONTROLL                                           
063500     .                                                                    
063600 IMS-GU-WDK712 SECTION.                                                   
063700     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X  ')'                        
063800          DELIMITED BY SIZE INTO SSA1                                     
063900     STRING 'WDK712  (IDLAND   =' W-IDLAND-X  ')'                         
064000          DELIMITED BY SIZE INTO SSA2                                     
064100     MOVE '  GE' TO GODK-STATUSKODER                                      
064200     CALL CBLTDLI USING GU WDK7-2-PCB DLI-IO-WDK712 SSA1 SSA2             
064300     MOVE WDK7-2-STATUS-CODE TO STATUS-WS                                 
064400     PERFORM IMS-STATUSKONTROLL                                           
064500     .                                                                    
064600 IMS-GU-WDL601 SECTION.                                                   
064700     STRING 'WDL601  (IDARTNR  =' W-IDARTNR-X ')'                         
064800          DELIMITED BY SIZE INTO SSA1                                     
064900     MOVE '  GE' TO GODK-STATUSKODER                                      
065000     CALL CBLTDLI USING GU WDL6-PCB DLI-IO-WDL601 SSA1                    
065100     MOVE WDL6-STATUS-CODE TO STATUS-WS                                   
065200     PERFORM IMS-STATUSKONTROLL                                           
065300     .                                                                    
065400 IMS-GNP-WDL611 SECTION.                                                  
065500     STRING 'WDL611  (DAINLEV >=' W-DAINLEV-MIN-X                         
065600                    '&DAINLEV <=' W-DAINLEV-MAX-X                         
065700                    '&IDDC     =' W-IDDC-X  ')'                           
065800          DELIMITED BY SIZE INTO SSA1                                     
065900     MOVE '  GE' TO GODK-STATUSKODER                                      
066000     CALL CBLTDLI USING GNP WDL6-PCB DLI-IO-WDL611 SSA1                   
066100     MOVE WDL6-STATUS-CODE TO STATUS-WS                                   
066200     PERFORM IMS-STATUSKONTROLL                                           
066300     .                                                                    
066400 IMS-GU-WDL711 SECTION.                                                   
066500     STRING 'WDL701  (IDARTNR  =' W-IDARTNR-X ')'                         
066600          DELIMITED BY SIZE INTO SSA1                                     
066700     STRING 'WDL711  (IDDC     =' W-IDDC-X ')'                            
066800          DELIMITED BY SIZE INTO SSA2                                     
066900     MOVE '  GE' TO GODK-STATUSKODER                                      
067000     CALL CBLTDLI USING GU WDL7-PCB DLI-IO-WDL711 SSA1 SSA2               
067100     MOVE WDL7-STATUS-CODE TO STATUS-WS                                   
067200     PERFORM IMS-STATUSKONTROLL                                           
067300     .                                                                    
067400 IMS-GU-WDB601    SECTION.                                                
067500     MOVE 'WDB601' TO SSA1                                                
067600     MOVE '  ' TO GODK-STATUSKODER                                        
067700     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-WDB601 SSA1                    
067800     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
067900     PERFORM IMS-STATUSKONTROLL                                           
068000     .                                                                    
068100 IMS-GU-WDB601-KY SECTION.                                                
068200     STRING 'WDB601  (IDDC     =' W-IDDC-X ')'                            
068300          DELIMITED BY SIZE INTO SSA1                                     
068400     MOVE '  ' TO GODK-STATUSKODER                                        
068500     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-WDB601 SSA1                    
068600     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
068700     PERFORM IMS-STATUSKONTROLL                                           
068800     .                                                                    
068900 IMS-GN-WDB601    SECTION.                                                
069000     MOVE 'WDB601' TO SSA1                                                
069100     MOVE '  GB' TO GODK-STATUSKODER                                      
069200     CALL CBLTDLI USING GN WDB6-PCB DLI-IO-WDB601 SSA1                    
069300     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
069400     PERFORM IMS-STATUSKONTROLL                                           
069500     .                                                                    
069600 IMS-GNP-WDB613 SECTION.                                                  
069700     MOVE 'WDB613  '       TO SSA1                                        
069800     MOVE '  GE' TO GODK-STATUSKODER                                      
069900     CALL CBLTDLI USING GNP WDB6-PCB DLI-IO-WDB613 SSA1                   
070000     MOVE WDB6-STATUS-CODE TO STATUS-WS                                   
070100     PERFORM IMS-STATUSKONTROLL                                           
070200     .                                                                    
070300     EJECT                                                                
070400 IMS-RESTART SECTION.                                                     
070500     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
070600     MOVE '  ' TO GODK-STATUSKODER                                        
070700     CALL CBLTDLI USING XRST MSG-PCB                                      
070800                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
070900                        CHKP-AREA-LENGTH CHKP-AREA                        
071000     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
071100     PERFORM IMS-STATUSKONTROLL                                           
071200     .                                                                    
071300     EJECT                                                                
071400 IMS-CHECKPOINT SECTION.                                                  
071500     MOVE SPACE TO CHKP-MSG-IO-AREA                                       
071600     MOVE '  XD' TO GODK-STATUSKODER                                      
071700     CALL CBLTDLI USING CHKP MSG-PCB                                      
071800                        CHKP-MSG-IO-AREA-LENGTH CHKP-MSG-IO-AREA          
071900                        CHKP-AREA-LENGTH CHKP-AREA                        
072000     MOVE MSG-STATUS-CODE TO STATUS-WS                                    
072100     PERFORM IMS-STATUSKONTROLL                                           
072200                                                                          
072300     IF IMS-EJ-OK                                                         
072400       MOVE 'IMS-KONTROLLREGION EJ TILLGÄNGLIG' TO FELTEXT-STR            
072500       DISPLAY FELTEXT                                                    
072600       CALL FELLOG                                                        
072700     END-IF                                                               
072800     .                                                                    
072900 IMS-STATUSKONTROLL SECTION.                                              
073000     SET STATUS-IX TO 1                                                   
073100     SEARCH GODK-STATUS                                                   
073200       AT END                                                             
073300         MOVE 'FELAKTIG IMS-RETURKOD' TO FELTEXT-STR                      
073400         DISPLAY FELTEXT                                                  
073500         CALL FELLOG                                                      
073600       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
073700         CONTINUE                                                         
073800     END-SEARCH                                                           
073900     .                                                                    
074000*    -COPY WY2000P1                                                       
