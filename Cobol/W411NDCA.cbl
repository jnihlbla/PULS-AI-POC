000100*COMPOPT STDSUB=YES                                                       
000200 ID DIVISION.                                                             
000300 PROGRAM-ID.     W411NDCA.                                                
000400 AUTHOR.         LASSI OLGRENER.                                          
000500 DATE-WRITTEN.   96/06/11.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*    FUNKTION:                                                            
000900*        PRELIMINÄR ALLOKERING AV ORDERRAD I NDC:ERNA                     
001000*        OLIKA REGLER GÄLLER BEROENDE PÅ ORDERKLASS                       
001100*        RADEN CLEARAS MELLAN OLIKA DC I SIN HELHET DVS INGEN             
001200*        RAD DELAS.                                                       
001300*                                                                         
001400*        PROGRAMMET LÄSER      WDK7                                       
001500*                              WDL6                                       
001600*                              WLUSEA (WDP7)                              
001610*    STORY 1640901 / ADDED CHECK FOR REFILL ORDERS TO BE ABLE TO          
001620*          PROCEED EVEN IF PUBLICATION WEEK > 2 WEEKS FROM TODAY          
001700                                                                          
001800 ENVIRONMENT DIVISION.                                                    
001900 DATA DIVISION.                                                           
002000                                                                          
002100 WORKING-STORAGE SECTION.                                                 
002200                                                                          
002300*    -COPY WY2000W1                                                       
002400                                                                          
002500 77  IDPGM                       PIC X(8)    VALUE 'W411NDCA'.            
002600 77  JA                          PIC X       VALUE 'J'.                   
002610 77  YES                         PIC X       VALUE 'Y'.                   
002700 77  NEJ                         PIC X       VALUE 'N'.                   
002800 77  CURRENT-SECTION             PIC X(16)   VALUE SPACE.                 
002900 77  CURRENT-IMS-SECTION         PIC X(16)   VALUE SPACE.                 
002910 77  CURRENT-KDORDKL             PIC 9       VALUE ZERO.                  
003000 77  W-DISP                      PIC S9(8)   VALUE ZERO.                  
003100 77  W-DISP-HOME                 PIC S9(8)   VALUE ZERO.                  
003110 77  W-DISP-KVANT                PIC S9(8).                               
003200 77  W-KVOKS-DAG                 PIC 9(7)    VALUE ZERO.                  
003300 77  W-KVOKS-BULK                PIC 9(7)    VALUE ZERO.                  
003400 77  W-KVRESS                    PIC 9(7)    VALUE ZERO.                  
003500 77  W-KVSPANT                   PIC 9(7)    VALUE ZERO.                  
003600 77  W-FLLF                      PIC X(1)    VALUE SPACE.                 
003700 77  WS-KVAVIS                   PIC 9(6)    VALUE ZERO.                  
003800 77  WS-TIBERANK                 PIC 9(6)    VALUE ZERO.                  
003900 77  WS-TIREGDAT                 PIC 9(6)    VALUE ZERO.                  
003910 77  WS-CDC-BLOCK                PIC X(1)    VALUE 'N' .                  
003920 77  WS-BULK-SW                  PIC X(1)    VALUE 'N' .                  
004000 77  W-IDDC-OI                   PIC X(2)    VALUE SPACE.                 
004100 77  W-IDDC-RO-BYTBAR            PIC X       VALUE 'N'.                   
004200 77  CURR-DC-IX                  PIC 9(1)    VALUE ZERO.                  
004300 77  CURR-DC-IX-MAX              PIC 9(1)    VALUE 6.                     
004400 77  FORSTA-IX                   PIC 9(1)    VALUE 1.                     
004500 77  SISTA-IX                    PIC 9(1)    VALUE 6.                     
004600 77  WS-IXDCCLEAR                PIC S9(5)   VALUE ZERO COMP-3.           
004700 77  WS-IXDCCLEAR-NEW            PIC S9(5)   VALUE ZERO COMP-3.           
004800                                                                          
004900 01  CLEAR-SW                    PIC X.                                   
005000     88  CLEARING                            VALUE 'J'.                   
005100     88  NO-CLEARING                         VALUE 'N'.                   
005200                                                                          
005300 77  AKTUELLT-LAND               PIC X       VALUE 'N'.                   
005400     88  AKTUELLT-LAND-KINA                  VALUE 'J'.                   
005500     88  AKTUELLT-EJ-KINA                    VALUE 'N'.                   
005510                                                                          
005520 77  WS-KVAKS                    PIC S9(8)   VALUE ZERO.                  
005530 77  WS-RETURNS                  PIC S9(8)   VALUE ZERO.                  
005540                                                                          
005550 77  WS-KDAKDISP                 PIC 9       VALUE 0.                     
005560     88  NO-AKS                              VALUE 0.                     
005570     88  FULL-AKS                            VALUE 1.                     
005580     88  LIMITED-AKS                         VALUE 2.                     
005590                                                                          
005600*      --- VALID IDDC CODES                                               
005700                                                                          
005800*01    -COPY WWDCKONS                                                     
005900                                                                          
006000*01    -COPY WWDC99                                                       
006100*01    -COPY WWDC99 -PRE CLEAR-                                           
006200                                                                          
006300                                                                          
006400 01  WS-TIHHMMSS                 PIC 9(6)    VALUE ZERO.                  
006500 01  FILLER REDEFINES WS-TIHHMMSS.                                        
006600     03  WS-TIHHMM               PIC 9(4).                                
006700     03  FILLER                  PIC 9(2).                                
006800                                                                          
006900 01  FELTEXT.                                                             
007000     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
007100     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
007200                                                                          
007300                                                                          
007400                                                                          
007500 01  TEST-IDDISTR                PIC S9(5)   COMP-3.                      
007600 01  FILLER REDEFINES TEST-IDDISTR.                                       
007700*    03   -COPY WWDIST07.                                                 
007800                                                                          
007900 01  FILLER REDEFINES TEST-IDDISTR.                                       
008000*    03   -COPY WWDIST18.                                                 
008100                                                                          
008200 01  FILLER REDEFINES TEST-IDDISTR.                                       
008300*    03   -COPY WWDIST35.                                                 
008400                                                                          
008500                                                                          
008600 01  DYNAMISKA-SUBPROGRAM.                                                
008700*                                                                         
008800     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
008900     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
009000     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
009100     03  WDAGKONV                PIC X(8)    VALUE 'WDAGKONV'.            
009200     03  W005INIT                PIC X(8)    VALUE 'W005INIT'.            
009210     03  WZ20DAYS                PIC X(8)    VALUE 'WZ20DAYS'.            
009300                                                                          
009310*                                                                         
009320*    --- PARAMETRAR TILL WZ20DAYS                                         
009330*    -COPY WZ20DAYS                                                       
009340                                                                          
009400*    --- PARAMETRAR TILL ABEND                                            
009500 01  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
009600 01  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
009700                                                                          
009800                                                                          
009900*    --- PARAMETRAR TILL WDAGAREA                                         
010000 01  FILLER                      PIC X(16) VALUE 'WDAGAREA'.              
010100*01  -COPY WDAGAREA                                                       
010200                                                                          
010300*    --- PARAMETRAR TILL W005INIT                                         
010400 01  FILLER                      PIC X(16) VALUE 'WMSGINIT'.              
010500*01  -COPY WMSGINIT                                                       
010600                                                                          
010700                                                                          
010800                                                                          
010900 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
011000                                                                          
011100 01  NYCKLAR-TILL-DLI.                                                    
011200     03  W-IDARTNR-X.                                                     
011300         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
011400     03  W-IDDC-X.                                                        
011500         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
011600     03  W-IDLAND-X.                                                      
011700         05  W-IDLAND            PIC X(2)    VALUE SPACE.                 
011800     03  W-IDLEVNR-1441-X.                                                
011900         05  W-IDLEVNR-1441      PIC X(5)    VALUE '1441 '.               
012000     03  W-IDPTYP-X.                                                      
012010         05  W-IDPTYP            PIC X(3)    VALUE '310'.                 
012020     03  W-KDRT-07-X.                                                     
012030         05  W-KDRT-07           PIC S9(3)   VALUE 7    COMP-3.           
012040     03  W-KDRT-77-X.                                                     
012050         05  W-KDRT-77           PIC S9(3)   VALUE 77   COMP-3.           
012060                                                                          
012100                                                                          
012200*    --- STATUS-KOD FRÅN IMS                                              
012300 01  STATUS-WS                   PIC XX.                                  
012400     88  SEGMENT-FINNS                       VALUE '  '.                  
012500     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
012600     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
012700                                                                          
012800 01  GODK-STATUSKODER.                                                    
012900     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
013000                                                                          
013100 01  SSA1                        PIC X(64).                               
013200 01  SSA2                        PIC X(64).                               
013300 01  SSA3                        PIC X(64).                               
013400                                                                          
013500                                                                          
013600*    --- IMS FUNKTIONSKODER                                               
013700*01  -COPY W0003                                                          
013800                                                                          
013900*    ---  DLI INPUT-OUTPUT AREA                                           
014000 01  FILLER                      PIC X(16) VALUE 'DLI-IO-WDK711'.         
014100 01  DLI-IO-WDK711.                                                       
014200*    03  -COPY WDK711                                                     
014300                                                                          
014400                                                                          
014500 01  FILLER                      PIC X(16) VALUE 'DLI-IO-WDK712'.         
014600 01  DLI-IO-WDK712.                                                       
014700*    03  -COPY WDK712                                                     
014800                                                                          
014900                                                                          
015000 01  FILLER                      PIC X(16) VALUE 'DLI-IO-WDK722'.         
015100 01  DLI-IO-WDK722.                                                       
015200*    03  -COPY WDK722                                                     
015300                                                                          
015400                                                                          
015500 01  FILLER                      PIC X(16) VALUE 'DLI-IO-WDL601'.         
015600 01  DLI-IO-WDL601.                                                       
015700*    03  -COPY WDL601                                                     
015710                                                                          
015720 01  FILLER                      PIC X(16) VALUE 'DLI-IO-WDL611'.         
015730 01  DLI-IO-WDL611.                                                       
015740*    03  -COPY WDL611                                                     
015800                                                                          
015900                                                                          
016000 01  FILLER                      PIC X(16) VALUE 'DLI-IO-WDB601'.         
016100 01  DLI-IO-WDB601.                                                       
016200*    03  -COPY WDB601                                                     
016300                                                                          
016400                                                                          
016500                                                                          
016600 LINKAGE SECTION.                                                         
016700*    -COPY W411NDCA                                                       
016800                                                                          
016900*    -COPY W411CLDC                                                       
017000                                                                          
017100*01  -COPY W0008  -PRE USEA-                                              
017200     05  FILLER                  PIC X.                                   
017300                                                                          
017400*01  -COPY W0008  -PRE WDK7-                                              
017500     05  FILLER                  PIC X.                                   
017600                                                                          
017700*01  -COPY W0008  -PRE WDL6-                                              
017800     05  FILLER                  PIC X.                                   
017900                                                                          
018000*01  -COPY W0008  -PRE WDB6-                                              
018100     05  FILLER                  PIC X.                                   
018110                                                                          
018111*    WORKARE JUST FOR TEST                                                
018120*    -COPY W411XDK7                                                       
018200                                                                          
018300                                                                          
018400                                                                          
018500 PROCEDURE DIVISION  USING NDCA-W411NDCA CLDC-W411CLDC                    
018600                           USEA-PCB WDK7-PCB WDL6-PCB WDB6-PCB            
018610                           XDK7-W411XDK7.                                 
018700                                                                          
018800 MAIN SECTION.                                                            
018900                                                                          
019000     PERFORM A-INIT                                                       
019100     MOVE 1  TO CURR-DC-IX                                                
019200     MOVE NDCA-IDARTNR TO W-IDARTNR                                       
019300                                                                          
019400     PERFORM UNTIL NO-CLEARING                                            
019500        PERFORM B-ALLOC-DC                                                
019600        PERFORM C-UPDATE-CLEARGROUP                                       
019700        ADD 1 TO CURR-DC-IX                                               
019800     END-PERFORM                                                          
019900                                                                          
020000     IF NDCA-IDDC-RO NOT = SPACE                                          
020100       MOVE SLAG-ADLAGOMR   TO NDCA-ADLAGOMR                              
020200       MOVE SLAG-ADGANG     TO NDCA-ADGANG                                
020300       MOVE SLAG-ADPLATS    TO NDCA-ADPLATS                               
020400     END-IF                                                               
020500                                                                          
020600     IF NDCA-IDDC-TVS = SPACE AND                                         
020700        NDCA-IDDC NOT = NDCA-IDDC-CLEAR                                   
020800                     IN NDCA-IDDC-CLEAR-GRP(FORSTA-IX)                    
020900       PERFORM E-NY-LOKALTID                                              
021000     END-IF                                                               
021100                                                                          
021200     PERFORM F-FIXA-KDOI                                                  
021300                                                                          
021400     MOVE ZERO TO RETURN-CODE                                             
021500     GOBACK                                                               
021600     .                                                                    
021700                                                                          
021800                                                                          
021900                                                                          
022000 A-INIT SECTION.                                                          
022100     MOVE 'A-INIT          ' TO CURRENT-SECTION                           
022200                                                                          
022300     PERFORM AA-JUSTERA-CLEAR-GRP                                         
022400                                                                          
022500     MOVE SPACE             TO NDCA-IDDC-RO                               
022600     MOVE ZERO              TO NDCA-KDORDBEK                              
022700                               NDCA-ADLAGOMR                              
022800                               NDCA-ADGANG                                
022900                               NDCA-ADPLATS                               
023000                               NDCA-KVPREAVB                              
023100                               NDCA-KVPRERO                               
023200                               W-DISP-HOME                                
023300                               W-DISP                                     
023400     MOVE 999999            TO WS-TIBERANK                                
023500     MOVE ZERO              TO WS-KVAVIS                                  
023600     MOVE NEJ               TO W-IDDC-RO-BYTBAR                           
023700     MOVE JA                TO CLEAR-SW                                   
023800     MOVE NDCA-IDDC         TO WS-IDDC                                    
023900     MOVE NDCA-IDDISTR      TO TEST-IDDISTR                               
024000     MOVE NDCA-TIREGDAT     TO WS-TIREGDAT                                
024001     MOVE 'N'               TO WS-CDC-BLOCK                               
024002     MOVE 'N'               TO WS-BULK-SW                                 
024003     MOVE 6                 TO SISTA-IX                                   
024010                                                                          
024030     MOVE NDCA-IDDC-CLEAR IN                                              
024040          NDCA-IDDC-CLEAR-GRP(1) TO W-IDDC                                
024110     .                                                                    
024200                                                                          
024300                                                                          
024400                                                                          
024500 AA-JUSTERA-CLEAR-GRP  SECTION.                                           
024600     MOVE 'AA-JUSTERA-CLEAR' TO CURRENT-SECTION                           
024700                                                                          
024800     MOVE 1 TO WS-IXDCCLEAR                                               
024900               WS-IXDCCLEAR-NEW                                           
025000                                                                          
025100     PERFORM UNTIL WS-IXDCCLEAR > SISTA-IX                                
025200        MOVE NDCA-IDDC-CLEAR IN                                           
025300             NDCA-IDDC-CLEAR-GRP(WS-IXDCCLEAR)                            
025400                             TO WS-IDDC                                   
025500        IF NDC                                                            
025600           MOVE NDCA-IDDC-CLEAR IN                                        
025700                NDCA-IDDC-CLEAR-GRP(WS-IXDCCLEAR)                         
025800                             TO                                           
025900                NDCA-IDDC-CLEAR IN                                        
026000                NDCA-IDDC-CLEAR-GRP(WS-IXDCCLEAR-NEW)                     
026100           ADD 1             TO WS-IXDCCLEAR-NEW                          
026200        END-IF                                                            
026300        ADD 1                TO WS-IXDCCLEAR                              
026400     END-PERFORM                                                          
026500                                                                          
026600     PERFORM UNTIL WS-IXDCCLEAR-NEW > SISTA-IX                            
026700        MOVE SPACE           TO                                           
026800             NDCA-IDDC-CLEAR IN                                           
026900             NDCA-IDDC-CLEAR-GRP(WS-IXDCCLEAR-NEW)                        
027000        ADD 1                TO WS-IXDCCLEAR-NEW                          
027100     END-PERFORM                                                          
027200                                                                          
027300     MOVE 1 TO WS-IXDCCLEAR-NEW                                           
027400     PERFORM UNTIL WS-IXDCCLEAR-NEW > SISTA-IX                            
027500          OR NDCA-IDDC-CLEAR IN                                           
027600             NDCA-IDDC-CLEAR-GRP(WS-IXDCCLEAR-NEW)                        
027700             = SPACE                                                      
027800                                                                          
027900        ADD 1                TO WS-IXDCCLEAR-NEW                          
028000     END-PERFORM                                                          
028100     COMPUTE SISTA-IX = WS-IXDCCLEAR-NEW - 1                              
028200     .                                                                    
028300                                                                          
028400                                                                          
028500                                                                          
028600 B-ALLOC-DC   SECTION.                                                    
028700     MOVE 'B-ALLOC-DC      ' TO CURRENT-SECTION                           
028800                                                                          
028900     MOVE NEJ TO CLEAR-SW                                                 
029000                                                                          
029100     IF NDCA-IDDC-TVS = SPACE                                             
029200       MOVE NDCA-IDDC-CLEAR                                               
029300         IN NDCA-IDDC-CLEAR-GRP(CURR-DC-IX)                               
029400                         TO W-IDDC                                        
029500                            W-IDDC-OI                                     
029600     ELSE                                                                 
029700       MOVE NDCA-IDDC-TVS      TO W-IDDC                                  
029800                                  W-IDDC-OI                               
029900     END-IF                                                               
030000                                                                          
030100     PERFORM IMS-GU-WDB601                                                
030200     MOVE DCS-IDLANDX2         TO W-IDLAND                                
030300                                                                          
030400     IF DCS-CHINA                                                         
030500        SET AKTUELLT-LAND-KINA TO TRUE                                    
030600     ELSE                                                                 
030700        SET AKTUELLT-EJ-KINA   TO TRUE                                    
030800     END-IF                                                               
030810                                                                          
030820     IF DCS-FLCLEAR-BULK = YES AND                                        
030830        NDCA-KDORDKL > 1       AND                                        
030831        CURR-DC-IX = 1                                                    
030840        MOVE 1                 TO CURRENT-KDORDKL                         
030841        MOVE 'Y'               TO WS-BULK-SW                              
030850     ELSE                                                                 
030851        IF WS-BULK-SW = 'N'                                               
030852          MOVE NDCA-KDORDKL    TO CURRENT-KDORDKL                         
030853        ELSE                                                              
030861           MOVE 1              TO CURRENT-KDORDKL                         
030870        END-IF                                                            
030880     END-IF                                                               
030900                                                                          
031000     PERFORM IMS-GU-WDK712                                                
031100     IF SEGMENT-FINNS                                                     
031200        IF LART-KDARTURS > SPACE                                          
031300           MOVE LART-KDARTURS  TO NDCA-KDARTURS                           
031400        END-IF                                                            
031500        IF LART-VKART > ZERO AND                                          
031510           LART-VKART NOT = NDCA-VKART                                    
031600           MOVE LART-VKART     TO NDCA-VKART                              
031610                                  NDCA-VKART-NTO                          
031700        END-IF                                                            
031800        IF LART-VLARTNTO > 0                                              
031900           MOVE LART-VLARTNTO  TO NDCA-VLARTNTO                           
032000        END-IF                                                            
032100     ELSE                                                                 
032200        MOVE ZERO              TO LART-DAPUBL                             
032300     END-IF                                                               
032400                                                                          
032401*THIS IS A FIX WHEN CDC-BLOCK IS THERE BUT NDC-PUBL WEEK IS               
032402*ZERO SO WE SHOULD NOT APPROVE THE ORDER                                  
032403*W411SPAR SENDS 67 CODE AND WE SHOULD GENERATE CODE 55 HERE SO WE         
032404*DONT UPDATE OKS VALUE IN W411NDCA                                        
032410     IF NDCA-DAPUBL = 99999999 AND LART-DAPUBL = ZERO                     
032420        MOVE 'Y'               TO WS-CDC-BLOCK                            
032430     END-IF                                                               
032440     MOVE LART-DAPUBL          TO NDCA-DAPUBL                             
032500     PERFORM S11-CHECK-KVSPANT                                            
032600     PERFORM IMS-GHU-WDK711                                               
032700     IF SEGMENT-FINNS                                                     
032800        IF CURR-DC-IX = 1                                                 
032900           MOVE W-IDDC         TO NDCA-IDDC-RO                            
033000                                                                          
033100           IF SLAG-KDREFSTA = 'P'                                         
033200              IF NDCA-KDORDKL > +1                                        
033300                 MOVE JA       TO W-IDDC-RO-BYTBAR                        
033400              END-IF                                                      
033500           END-IF                                                         
033600        END-IF                                                            
033700                                                                          
033800        IF SLAG-FLREFILL = NEJ                                            
033900           MOVE NEJ         TO W-FLLF                                     
034000        ELSE                                                              
034100           MOVE JA          TO W-FLLF                                     
034200        END-IF                                                            
034300                                                                          
034400        IF  CURR-DC-IX = 1                                                
034500        AND NDCA-FLORDSPE = JA                                            
034600           MOVE W-IDDC           TO NDCA-IDDC                             
034700        ELSE                                                              
034800           IF NDCA-IDDC-RO = SPACE AND CURR-DC-IX  > 1                    
034900              MOVE W-IDDC        TO NDCA-IDDC-RO                          
035000              IF  CURR-DC-IX < CURR-DC-IX-MAX                             
035100                 IF  NDCA-KDORDKL > +1                                    
035200                 AND SLAG-KDREFSTA = 'P'                                  
035300                    MOVE JA      TO W-IDDC-RO-BYTBAR                      
035400                 END-IF                                                   
035500              ELSE                                                        
035600                 IF  NDCA-KDORDKL > +1                                    
035700                 AND W-IDDC-RO-BYTBAR = JA                                
035800                 AND SLAG-KDREFSTA  NOT = 'P'                             
035900                    MOVE W-IDDC  TO NDCA-IDDC-RO                          
036000                    MOVE NEJ     TO W-IDDC-RO-BYTBAR                      
036100                 END-IF                                                   
036200              END-IF                                                      
036300           END-IF                                                         
036400        END-IF                                                            
036500                                                                          
036600        IF  CURR-DC-IX = 1                                                
036700        AND NDCA-FLORDSPE = JA                                            
036800           CONTINUE                                                       
036900        ELSE                                                              
036901          IF LART-DAPUBL > ZERO                                           
036910             MOVE SPACE             TO DAYS-TIDATE1                       
036920             MOVE 'YYMMDD'          TO DAYS-KDDATFMT1                     
036930             MOVE LART-DAPUBL(3:6)  TO DAYS-TIDATE2                       
037010             MOVE 'YYMMDD'          TO DAYS-KDDATFMT2                     
037030             MOVE 14                TO DAYS-KVDAYS                        
037040             CALL WZ20DAYS USING DAYS-WZ20DAYS                            
037050             IF DAYS-KDRC NOT = ZERO                                      
037060               MOVE 'WZ20DAYS ERROR ADDING 14 DAYS' TO FELTEXT-STR        
037070                CALL ABEND USING RKOD-ABEND-MED-DUMP                      
037080             END-IF                                                       
037100             MOVE DAYS-TIDATE1(1:6) TO TMP2-YYMMDD                        
037101          ELSE                                                            
037102             MOVE LART-DAPUBL(3:6)  TO TMP2-YYMMDD                        
037103          END-IF                                                          
037110          MOVE WS-TIREGDAT          TO TMP1-YYMMDD                        
037200          PERFORM WY2000P1                                                
037300          IF ((TMP1-YYMMDD < TMP2-YYMMDD )                                
037310          AND NOT DIST35-REFILL                                           
037320          AND NOT DIST35-NONVCC-REFILL                                    
037330          AND NOT DIST35-REFILL-NA-JAP                                    
037340          AND NOT DIST35-REFILL-INOM-JP                                   
037350          AND NOT DIST35-REFILL-INOM-NDC) OR                              
037400              WS-CDC-BLOCK = 'Y'          OR                              
037410             (SLAG-FLORDSP-EJRO = JA                                      
037500          AND NOT DIST18-SKROT-SDC                                        
037600          AND NOT DIST18-SCRAP-NDC-SC                                     
037700          AND NOT DIST35-JPAU-CDC-RETUR                                   
037800          AND NOT DIST35-NA-CDC-BB-RETURN                                 
037900          AND NOT DIST35-CN-CDC-RETUR                                     
038000          AND NOT DIST35-CN-NDC-RETURNS                                   
038100          AND NOT DIST35-NA-TRANSFER                                      
038200          AND NOT DIST35-NA-NDC-RETURNS                                   
038300          AND NOT DIST35-CN-TRANSFER                                      
038400          AND NOT DIST35-PACIFIC-TRANSFER)                                
038500             MOVE 55     TO NDCA-KDORDBEK                                 
038600             MOVE W-IDDC TO NDCA-IDDC                                     
038700          ELSE                                                            
038800                                                                          
038900             IF (NOT DIST18-SCRAP-NDC-QUAL) AND                           
039000                (NOT DIST18-SKROT-SDC) AND                                
039100                (NOT DIST18-SKROT-KVAL-SDC) AND                           
039200                (NOT DIST35-NA-CDC-QUAL-RETURN) AND                       
039300                (NOT DIST35-CN-CDC-RETUR-Q) AND                           
039400                                                                          
039500                (NOT DIST35-NA-TRANSFER)  AND                             
039600                (NOT DIST35-NA-NDC-RETURNS) AND                           
039700                (NOT DIST35-CN-TRANSFER) AND                              
039800                (NOT DIST35-CN-NDC-RETURNS) AND                           
039900                                                                          
040000*   VERKSTADSORDER KINA SKALL HA DAGORDERSERVICE                          
040100               ((SLAG-FLORDSP = JA AND (NDCA-KDORDKL = +1))               
040300             OR                                                           
040400              (SLAG-KDLEVSP > 0 AND (NDCA-KDORDKL = 0 OR 1)))             
040600                                                                          
040700                IF NDCA-IDDC-CLEAR                                        
040800                        IN NDCA-IDDC-CLEAR-GRP(CURR-DC-IX + 1)            
040900                                        = SPACE                           
041000                OR (CURR-DC-IX = 1 AND                                    
041100                      NDCA-IDDC-TVS NOT = SPACE)                          
041200                OR  CURR-DC-IX = CURR-DC-IX-MAX                           
041300                   PERFORM S02-FIXA-LEV-DC                                
041400                ELSE                                                      
041500                                                                          
041600                   IF CURR-DC-IX < CURR-DC-IX-MAX                         
041700                      MOVE JA        TO CLEAR-SW                          
041800                      MOVE 15        TO NDCA-KDORDBEK                     
041900                   END-IF                                                 
042000                END-IF                                                    
042100             ELSE                                                         
042200                PERFORM BA-KOLLA-DISP                                     
042300             END-IF                                                       
042400          END-IF                                                          
042500        END-IF                                                            
042600     ELSE                                                                 
042700        MOVE NEJ                TO W-FLLF                                 
042800        IF CURR-DC-IX = 1                                                 
042900           PERFORM BB-AVSLUTA-FORSTA                                      
043000        ELSE                                                              
043100           IF CURR-DC-IX = CURR-DC-IX-MAX                                 
043200              PERFORM BC-AVSLUTA-SISTA                                    
043300           ELSE                                                           
043400              PERFORM BD-AVSLUTA-MELLERSTA                                
043500           END-IF                                                         
043600        END-IF                                                            
043700     END-IF                                                               
043800     .                                                                    
043900                                                                          
044000                                                                          
044100 BA-KOLLA-DISP     SECTION.                                               
044200     MOVE 'BA-KOLLA-DISP   ' TO CURRENT-SECTION                           
044300                                                                          
044400     PERFORM S01-NOLLA-EV-MINUS-SALDON                                    
044410     PERFORM BAE-DECIDE-USE-OF-AKS                                        
044500*    IF NDCA-KDORDKL = +0                                                 
044510     IF CURRENT-KDORDKL = +0                                              
044600       PERFORM BAA-KOLLA-DISP-VOR                                         
044700     END-IF                                                               
044800*    IF NDCA-KDORDKL = +1                                                 
044810     IF CURRENT-KDORDKL = +1                                              
044900       PERFORM BAB-KOLLA-DISP-DAG                                         
045000     END-IF                                                               
045100*    IF NDCA-KDORDKL > +1                                                 
045110     IF CURRENT-KDORDKL > +1                                              
045500       PERFORM BAC-KOLLA-DISP-BULK                                        
045700     END-IF                                                               
045800     .                                                                    
045900                                                                          
046000                                                                          
046100                                                                          
046200 BAA-KOLLA-DISP-VOR SECTION.                                              
046300     MOVE 'BAA-DISP-VOR    ' TO CURRENT-SECTION                           
046400                                                                          
046500     IF CURR-DC-IX = 1 AND                                                
046600        NDCA-FLFORBI = JA                                                 
046700       IF SLAG-KVAKS-SDC > +0 OR SLAG-KVSPARR-KVAL > +0                   
046800         COMPUTE W-DISP = SLAG-KVLS                                       
046900                        - SLAG-KVSPARR-KVAL                               
047000         IF W-DISP < ZERO                                                 
047100           MOVE ZERO           TO W-DISP                                  
047200         END-IF                                                           
047210         PERFORM S13-ANPASSA-DISP-TILL-KVANT                              
047300         IF NDCA-KVBEART-Q > W-DISP                                       
047400           PERFORM S04-FIXA-VOR-RAD                                       
047500         ELSE                                                             
047600           MOVE NDCA-KVBEART-Q TO NDCA-KVPREAVB                           
047700           ADD NDCA-KVBEART-Q  TO SLAG-KVOKS-DAG                          
047800           PERFORM S12-REPL-WDK7                                          
047900           MOVE W-IDDC         TO NDCA-IDDC                               
048000         END-IF                                                           
048100       ELSE                                                               
048200         MOVE NDCA-KVBEART-Q   TO NDCA-KVPREAVB                           
048300         ADD NDCA-KVBEART-Q    TO SLAG-KVOKS-DAG                          
048400         PERFORM S12-REPL-WDK7                                            
048500         MOVE W-IDDC           TO NDCA-IDDC                               
048600       END-IF                                                             
048700     ELSE                                                                 
048800       PERFORM S07-BERAKN-DISP-VOR                                        
048900       IF NDCA-KVBEART-Q > W-DISP                                         
049000         IF CURR-DC-IX = 1                                                
049100            IF NDCA-IDDC-TVS = SPACE AND                                  
049200               NDCA-IDDC-CLEAR                                            
049300                    IN NDCA-IDDC-CLEAR-GRP(CURR-DC-IX + 1)                
049400                               NOT = SPACE                                
049500               MOVE JA          TO CLEAR-SW                               
049600               MOVE 15          TO NDCA-KDORDBEK                          
049700               MOVE W-DISP      TO W-DISP-HOME                            
049800            ELSE                                                          
049900              PERFORM S04-FIXA-VOR-RAD                                    
050000            END-IF                                                        
050100         ELSE                                                             
050200           IF CURR-DC-IX = SISTA-IX                                       
050300              PERFORM S04-FIXA-VOR-RAD                                    
050400           ELSE                                                           
050500              IF NDCA-IDDC-CLEAR                                          
050600                 IN NDCA-IDDC-CLEAR-GRP(CURR-DC-IX + 1) = SPACE           
050700                 PERFORM S04-FIXA-VOR-RAD                                 
050800              ELSE                                                        
050900                 MOVE JA        TO CLEAR-SW                               
051000                 IF NDCA-IDDC-RO = W-IDDC                                 
051100                    MOVE W-DISP TO W-DISP-HOME                            
051200                 END-IF                                                   
051300              END-IF                                                      
051400           END-IF                                                         
051500         END-IF                                                           
051600       ELSE                                                               
051700         ADD NDCA-KVBEART-Q    TO SLAG-KVOKS-DAG                          
051800         PERFORM S12-REPL-WDK7                                            
051900         MOVE NDCA-KVBEART-Q   TO NDCA-KVPREAVB                           
052000         MOVE W-IDDC           TO NDCA-IDDC                               
052100       END-IF                                                             
052200     END-IF                                                               
052300     .                                                                    
052400                                                                          
052500                                                                          
052600                                                                          
052700 BAB-KOLLA-DISP-DAG SECTION.                                              
052800     MOVE 'BAB-DISP-DAG    ' TO CURRENT-SECTION                           
052900                                                                          
052901**************************************************************            
052910*    IF DCS-FLCLEAR-BULK = YES AND NDCA-KDORDKL > 1                       
052920*    WE ALSO ENDS UP IN THIS SECTION TO GET THE SAME REFERRAL             
052930*    RULES FOR THIS ORDER. THATS WHY WE HAVE TO CHECK ON THAT             
052940*    SO WE CALL S05- OR S06- AND ALSO UPDATE THE RIGHT OKS                
052950**************************************************************            
052960                                                                          
052970     IF NDCA-KDORDKL = 1                                                  
053000        PERFORM S08-BERAKN-DISP-DAG                                       
053010     ELSE                                                                 
053011        PERFORM S09-BERAKN-DISP-BULK                                      
053020     END-IF                                                               
053100     IF CURR-DC-IX = FORSTA-IX                                            
053200        IF NDCA-KVBEART-Q > W-DISP AND                                    
053210           NDCA-KDORDKL = 1                                               
053300           PERFORM S10-CHECK-NEXT-REFILL                                  
053400        END-IF                                                            
053500        IF NDCA-KVBEART-Q > (W-DISP + WS-KVAVIS)                          
053600          IF NDCA-IDDC-CLEAR                                              
053700             IN NDCA-IDDC-CLEAR-GRP(CURR-DC-IX + 1) = SPACE               
053710          OR LART-FLREFERAL = JA                                          
053720            IF NDCA-KDORDKL = 1                                           
053800               PERFORM S05-FIXA-DAY-RAD                                   
053810            ELSE                                                          
053811               PERFORM S06-FIXA-BULK-RAD                                  
053820            END-IF                                                        
053900          ELSE                                                            
054000            MOVE JA            TO CLEAR-SW                                
054100            MOVE 15            TO NDCA-KDORDBEK                           
054200            MOVE W-DISP        TO W-DISP-HOME                             
054300          END-IF                                                          
054400        ELSE                                                              
054410          IF NDCA-KDORDKL = 1                                             
054500             ADD NDCA-KVBEART-Q TO SLAG-KVOKS-DAG                         
054510          ELSE                                                            
054511             ADD NDCA-KVBEART-Q TO SLAG-KVOKS-BULK                        
054520          END-IF                                                          
054600          PERFORM S12-REPL-WDK7                                           
054700          IF WS-KVAVIS = ZERO                                             
054800            MOVE NDCA-KVBEART-Q TO NDCA-KVPREAVB                          
054900            COMPUTE NDCA-KVPRERO = NDCA-KVBEART-Q                         
055000                                 - NDCA-KVPREAVB                          
055100          ELSE                                                            
055200            MOVE W-DISP        TO NDCA-KVPREAVB                           
055300            COMPUTE NDCA-KVPRERO = NDCA-KVBEART-Q                         
055400                                 - W-DISP                                 
055500          END-IF                                                          
055600          MOVE W-IDDC          TO NDCA-IDDC                               
055700        END-IF                                                            
055800     ELSE                                                                 
055900       IF NDCA-KVBEART-Q > W-DISP                                         
056000         IF CURR-DC-IX = SISTA-IX                                         
056110            IF NDCA-KDORDKL = 1                                           
056120               PERFORM S05-FIXA-DAY-RAD                                   
056130            ELSE                                                          
056140               PERFORM S06-FIXA-BULK-RAD                                  
056150            END-IF                                                        
056200         ELSE                                                             
056300            IF NDCA-IDDC-CLEAR                                            
056400               IN NDCA-IDDC-CLEAR-GRP(CURR-DC-IX + 1) = SPACE             
056510              IF NDCA-KDORDKL = 1                                         
056520                 PERFORM S05-FIXA-DAY-RAD                                 
056530              ELSE                                                        
056540                 PERFORM S06-FIXA-BULK-RAD                                
056550              END-IF                                                      
056600            ELSE                                                          
056700              MOVE JA          TO CLEAR-SW                                
056800              IF NDCA-IDDC-RO = W-IDDC                                    
056900                MOVE W-DISP    TO W-DISP-HOME                             
057000              END-IF                                                      
057100            END-IF                                                        
057200         END-IF                                                           
057300       ELSE                                                               
057410         IF NDCA-KDORDKL = 1                                              
057420            ADD NDCA-KVBEART-Q TO SLAG-KVOKS-DAG                          
057430         ELSE                                                             
057440            ADD NDCA-KVBEART-Q TO SLAG-KVOKS-BULK                         
057450         END-IF                                                           
057500         PERFORM S12-REPL-WDK7                                            
057600         MOVE NDCA-KVBEART-Q   TO NDCA-KVPREAVB                           
057700         MOVE ZERO             TO NDCA-KVPRERO                            
057800         MOVE W-IDDC           TO NDCA-IDDC                               
057900       END-IF                                                             
058000     END-IF                                                               
058100     .                                                                    
058200                                                                          
058300                                                                          
058400 BAC-KOLLA-DISP-BULK SECTION.                                             
058500     MOVE 'BAC-DISP-BULK   ' TO CURRENT-SECTION                           
058600                                                                          
058700     IF NDCA-KDERS > ZERO OR SLAG-KDREFSTA = 'P'                          
058800       PERFORM S09-BERAKN-DISP-BULK                                       
058900       IF CURR-DC-IX = FORSTA-IX                                          
059000          MOVE W-DISP            TO W-DISP-HOME                           
059100       ELSE                                                               
059200          IF CURR-DC-IX < SISTA-IX                                        
059300             IF NDCA-IDDC-RO  = W-IDDC                                    
059400                MOVE W-DISP      TO W-DISP-HOME                           
059500             END-IF                                                       
059600          END-IF                                                          
059700       END-IF                                                             
059800                                                                          
059900       IF (NDCA-KDERS > ZERO AND W-DISP > +0) OR                          
060000          (NDCA-KVBEART-Q NOT > W-DISP)                                   
060100         ADD NDCA-KVBEART-Q      TO SLAG-KVOKS-BULK                       
060200         PERFORM S12-REPL-WDK7                                            
060300         MOVE NDCA-KVBEART-Q     TO NDCA-KVPREAVB                         
060400*?LO KVPREAVB KAN VARA < RAD-ANTAL...SPELAR DET NGN ROLL???               
060500         MOVE W-IDDC             TO NDCA-IDDC                             
060600       ELSE                                                               
060700         IF CURR-DC-IX < SISTA-IX                                         
060800            IF NDCA-IDDC-CLEAR                                            
060900               IN NDCA-IDDC-CLEAR-GRP(CURR-DC-IX + 1) = SPACE             
060910            OR LART-FLREFERAL = JA                                        
061000               PERFORM S06-FIXA-BULK-RAD                                  
061100            ELSE                                                          
061200               MOVE JA           TO CLEAR-SW                              
061300               IF CURR-DC-IX = FORSTA-IX                                  
061400                  MOVE 15        TO NDCA-KDORDBEK                         
061500               END-IF                                                     
061600            END-IF                                                        
061700         ELSE                                                             
061800            PERFORM S06-FIXA-BULK-RAD                                     
061900         END-IF                                                           
062000       END-IF                                                             
062100     ELSE                                                                 
062200       ADD NDCA-KVBEART-Q        TO SLAG-KVOKS-BULK                       
062300       PERFORM S12-REPL-WDK7                                              
062400       MOVE NDCA-KVBEART-Q       TO NDCA-KVPREAVB                         
062500       MOVE W-IDDC               TO NDCA-IDDC                             
062600     END-IF                                                               
062700     .                                                                    
062800                                                                          
062900                                                                          
069110 BAE-DECIDE-USE-OF-AKS SECTION.                                           
069120     MOVE 'BAE-DECIDE-USE-O' TO CURRENT-SECTION                           
069130                                                                          
069131     IF NDCA-KDORDKL = 0 OR 1                                             
069140        MOVE DCS-KDAKDISP-DAG  TO WS-KDAKDISP                             
069141     ELSE                                                                 
069142        MOVE DCS-KDAKDISP-BULK TO WS-KDAKDISP                             
069143     END-IF                                                               
069150     IF NO-AKS                                                            
069160        MOVE ZERO              TO WS-KVAKS                                
069170     ELSE                                                                 
069180        IF FULL-AKS                                                       
069190           MOVE SLAG-KVAKS-SDC TO WS-KVAKS                                
069191        ELSE                                                              
069192           PERFORM BAEA-COMPUTE-LIMITED-AKS                               
069193        END-IF                                                            
069194     END-IF                                                               
069195     .                                                                    
069196     EJECT                                                                
069197 BAEA-COMPUTE-LIMITED-AKS SECTION.                                        
069198     MOVE 'BAEA-LIMITED-AKS' TO CURRENT-SECTION                           
069199                                                                          
069200     MOVE SLAG-KVAKS-SDC TO WS-KVAKS                                      
069201     MOVE ZERO           TO WS-RETURNS                                    
069202                                                                          
069203     PERFORM IMS-GU-WDL601                                                
069204     IF SEGMENT-FINNS                                                     
069205        PERFORM IMS-GNP-WDL611                                            
069206        PERFORM UNTIL SEGMENT-SAKNAS                                      
069207           COMPUTE WS-RETURNS = WS-RETURNS +                              
069208                   INL-KVAVIS - INL-KVANTMOT                              
069209           PERFORM IMS-GNP-WDL611                                         
069210        END-PERFORM                                                       
069211     END-IF                                                               
069212                                                                          
069213     COMPUTE WS-KVAKS = WS-KVAKS - WS-RETURNS                             
069214*    THE AKS-VALUE TO ADD MUST NEVER BE NEGATIVE                          
069215     IF WS-KVAKS < ZERO                                                   
069216        MOVE ZERO TO WS-KVAKS                                             
069217     END-IF                                                               
069218     .                                                                    
069219     EJECT                                                                
069220 BB-AVSLUTA-FORSTA SECTION.                                               
069300     MOVE 'BB-AVSL-FORSTA  ' TO CURRENT-SECTION                           
069400                                                                          
069500     IF NDCA-IDDC-TVS = SPACE                                             
069600        IF NDCA-IDDC-CLEAR                                                
069700           IN NDCA-IDDC-CLEAR-GRP(CURR-DC-IX + 1)                         
069800                                = SPACE                                   
069900           IF NDCA-KDERS < 20                                             
070000             MOVE 55           TO NDCA-KDORDBEK                           
070100             MOVE W-IDDC       TO NDCA-IDDC                               
070200           END-IF                                                         
070300        ELSE                                                              
070400           MOVE JA             TO CLEAR-SW                                
070500           MOVE 15             TO NDCA-KDORDBEK                           
070600        END-IF                                                            
070700     ELSE                                                                 
070800        IF NDC-CA  AND NDCA-FLFORBI = NEJ                                 
070900           MOVE 55             TO NDCA-KDORDBEK                           
071000        ELSE                                                              
071100           MOVE 53             TO NDCA-KDORDBEK                           
071200        END-IF                                                            
071300     END-IF                                                               
071400     .                                                                    
071500                                                                          
071600                                                                          
071700 BC-AVSLUTA-SISTA  SECTION.                                               
071800     MOVE 'BC-AVSL-SISTA   ' TO CURRENT-SECTION                           
071900                                                                          
072000      IF NDCA-IDDC-RO = SPACE                                             
072100         MOVE 55           TO NDCA-KDORDBEK                               
072200         MOVE NDCA-IDDC-CLEAR    IN NDCA-IDDC-CLEAR-GRP(FORSTA-IX)        
072300                           TO NDCA-IDDC                                   
072400      ELSE                                                                
072500         PERFORM S03-BACKA-TILL-DC-RO                                     
072600      END-IF                                                              
072700     .                                                                    
072800                                                                          
072900                                                                          
073000 BD-AVSLUTA-MELLERSTA SECTION.                                            
073100     MOVE 'BD-AVSL-MELLERST' TO CURRENT-SECTION                           
073200                                                                          
073300     IF NDCA-IDDC-CLEAR                                                   
073400          IN NDCA-IDDC-CLEAR-GRP(CURR-DC-IX + 1) = SPACE                  
073500        IF NDCA-IDDC-RO = SPACE                                           
073600           MOVE 55            TO NDCA-KDORDBEK                            
073700           MOVE NDCA-IDDC-CLEAR IN NDCA-IDDC-CLEAR-GRP(FORSTA-IX)         
073800                              TO NDCA-IDDC                                
073900        ELSE                                                              
074000           PERFORM S03-BACKA-TILL-DC-RO                                   
074100        END-IF                                                            
074200     ELSE                                                                 
074300        MOVE 15               TO NDCA-KDORDBEK                            
074400        MOVE JA               TO CLEAR-SW                                 
074500     END-IF                                                               
074600     .                                                                    
074700                                                                          
074800                                                                          
074900 E-NY-LOKALTID SECTION.                                                   
075000     MOVE 'E-NY-LOKALTID   ' TO CURRENT-SECTION                           
075100                                                                          
075200     MOVE NDCA-IDDC          TO WS-IDDC                                   
075300     MOVE NDCA-IDDC-CLEAR    IN NDCA-IDDC-CLEAR-GRP(FORSTA-IX)            
075400                             TO CLEAR-WS-IDDC                             
075500     MOVE ALL '+'            TO MSGI-WMSGINIT                             
075600     MOVE '001'              TO MSGI-KDCALL                               
075700     MOVE 'WIDDC   '         TO MSGI-IDUSER                               
075800     MOVE NDCA-IDDC          TO MSGI-IDUSER (6:2)                         
075900     MOVE 'W411'             TO MSGI-IDTRANS                              
076000                                                                          
076100     CALL W005INIT USING MSGI-WMSGINIT USEA-PCB                           
076200                                                                          
076300     MOVE MSGI-TILOKDAT      TO NDCA-TIREGDAT                             
076400     MOVE MSGI-TILOKTID      TO WS-TIHHMM                                 
076500     MOVE WS-TIHHMMSS        TO NDCA-TIREGTID                             
076600     .                                                                    
076700                                                                          
076800                                                                          
076900                                                                          
077000 F-FIXA-KDOI SECTION.                                                     
077100     MOVE 'F-FIXA-KDOI     ' TO CURRENT-SECTION                           
077200                                                                          
077300     IF NDCA-KDORDING = 3                                                 
077400       MOVE SPACE            TO NDCA-KDOI                                 
077500     ELSE                                                                 
077600       IF NDCA-KDOI NOT = 'RE'                                            
077700          IF NDCA-KDOI = 'XX'                                             
077800            PERFORM FA-KOLLA-KDOI                                         
077900          ELSE                                                            
078000            IF NDCA-IDDC-TVS NOT = SPACE OR                               
078100             NDCA-IDDC = NDCA-IDDC-CLEAR                                  
078200                         IN NDCA-IDDC-CLEAR-GRP(FORSTA-IX)                
078300              MOVE 'N1'        TO NDCA-KDOI                               
078400            ELSE                                                          
078500              IF NDCA-IDDC-RO = NDCA-IDDC-CLEAR IN                        
078600                                NDCA-IDDC-CLEAR-GRP(FORSTA-IX)            
078700                IF (NOT DIST35-NA-TRANSFER) AND                           
078800                   (NOT DIST35-NA-NDC-RETURNS) AND                        
078900                   (NOT DIST35-CN-TRANSFER) AND                           
079000                   (SLAG-FLORDSP = JA AND NDCA-KDORDKL = +1) OR           
079100                 (SLAG-KDLEVSP > 0 AND (NDCA-KDORDKL = 0 OR 1))           
079200                  MOVE 'N1'    TO NDCA-KDOI                               
079300                ELSE                                                      
079400                  MOVE 'N2'    TO NDCA-KDOI                               
079500                END-IF                                                    
079600              ELSE                                                        
079700                MOVE 'N1'      TO NDCA-KDOI                               
079800              END-IF                                                      
079900            END-IF                                                        
080000          END-IF                                                          
080100       END-IF                                                             
080200     END-IF                                                               
080300                                                                          
080400     .                                                                    
080500                                                                          
080600                                                                          
080700                                                                          
080800 FA-KOLLA-KDOI   SECTION.                                                 
080900     MOVE 'FA-KOLLA-KDOI   ' TO CURRENT-SECTION                           
081000                                                                          
081100     MOVE NDCA-IDARTNR    TO W-IDARTNR                                    
081200                                                                          
081300     MOVE NDCA-IDDC-CLEAR IN NDCA-IDDC-CLEAR-GRP(FORSTA-IX)               
081400                          TO W-IDDC                                       
081500                                                                          
081600     PERFORM IMS-GU-WDB601                                                
081700                                                                          
081800     IF DCS-NDC-PF OR DCS-NDC-CN                                          
081900        PERFORM IMS-GU-WDK711                                             
082000        IF SEGMENT-FINNS                                                  
082100           IF SLAG-IDLEVNR NOT = '1441 '                                  
082200              MOVE 'LO'      TO NDCA-KDOI                                 
082300           END-IF                                                         
082400        END-IF                                                            
082500     ELSE                                                                 
082600        IF DCS-USA                                                        
082700           MOVE 'LO'         TO NDCA-KDOI                                 
082800                                                                          
082900           PERFORM IMS-GU-WDK701                                          
083000           IF SEGMENT-FINNS                                               
083100              PERFORM IMS-GNP-WDK711-1441                                 
083200              PERFORM UNTIL SEGMENT-SAKNAS                                
083300                   OR NDCA-KDOI = 'XX'                                    
083400                 MOVE SLAG-IDDC TO WS-IDDC                                
083500                 IF NDC-US                                                
083600                    MOVE 'XX'   TO NDCA-KDOI                              
083700                 ELSE                                                     
083800                    PERFORM IMS-GNP-WDK711-1441                           
083900                 END-IF                                                   
084000              END-PERFORM                                                 
084100           END-IF                                                         
084200        ELSE                                                              
084300           IF DCS-CHINA                                                   
084400              MOVE 'LO'         TO NDCA-KDOI                              
084500                                                                          
084600              PERFORM IMS-GU-WDK701                                       
084700              IF SEGMENT-FINNS                                            
084800                 PERFORM IMS-GNP-WDK711-1441                              
084900                 PERFORM UNTIL SEGMENT-SAKNAS                             
085000                      OR NDCA-KDOI = 'XX'                                 
085100                    MOVE SLAG-IDDC TO WS-IDDC                             
085200                    IF NDC-CN                                             
085300                       MOVE 'XX'   TO NDCA-KDOI                           
085400                    ELSE                                                  
085500                       PERFORM IMS-GNP-WDK711-1441                        
085600                    END-IF                                                
085700                 END-PERFORM                                              
085800              END-IF                                                      
085900           END-IF                                                         
086000        END-IF                                                            
086100     END-IF                                                               
086200     .                                                                    
086300                                                                          
086400                                                                          
086500                                                                          
086600 C-UPDATE-CLEARGROUP       SECTION.                                       
086700     MOVE 'C-UPD-CLEARGROUP' TO CURRENT-SECTION                           
086800                                                                          
086900*    MED HJÄLP AV WS-IXDCCLEAR HÅLLER VI REDAN PÅ OM VI HAR ETT           
087000*    KINA NDC I CLEARING TABELLEN                                         
087100*    OCH NDCA-IXDCCLEAR HÅLLER POSITIONEN I CLEARGROUP TABELLEN           
087200*    FÖR ORDERINGÅNGEN                                                    
087300                                                                          
087400     IF NDCA-KDORDING < 3                                                 
087500        MOVE 1             TO WS-IXDCCLEAR                                
087600        PERFORM UNTIL WS-IXDCCLEAR > SISTA-IX OR                          
087700                      CLDC-IDDC(WS-IXDCCLEAR) = W-IDDC-OI                 
087800                                                                          
087900           ADD 1           TO WS-IXDCCLEAR                                
088000        END-PERFORM                                                       
088100                                                                          
088200                                                                          
088300        IF DIST35-REFILL-INOM-NDC OR DIST35-NONVCC-REFILL                 
088400           MOVE 'RE'      TO NDCA-KDOI                                    
088500                                                                          
088600           MOVE SPACE     TO NDCA-IDDC-CLEAR                              
088700                          IN NDCA-CLEARAREA(NDCA-IXDCCLEAR)               
088800           MOVE SPACE     TO NDCA-FLLF(NDCA-IXDCCLEAR)                    
088900           MOVE SPACE     TO NDCA-FLCLEAR(NDCA-IXDCCLEAR)                 
089000        ELSE                                                              
089100           MOVE 'XX'      TO NDCA-KDOI                                    
089200                                                                          
089300           MOVE W-IDDC-OI TO NDCA-IDDC-CLEAR                              
089400                          IN NDCA-CLEARAREA(NDCA-IXDCCLEAR)               
089500           MOVE W-FLLF    TO NDCA-FLLF(NDCA-IXDCCLEAR)                    
089600           IF CLEARING                                                    
089700              MOVE JA     TO NDCA-FLCLEAR(NDCA-IXDCCLEAR)                 
089800           ELSE                                                           
089900              MOVE NEJ    TO NDCA-FLCLEAR(NDCA-IXDCCLEAR)                 
090000           END-IF                                                         
090100        END-IF                                                            
090200        ADD 1             TO NDCA-IXDCCLEAR                               
090300     ELSE                                                                 
090400        MOVE SPACE           TO NDCA-KDOI                                 
090500     END-IF                                                               
090600     .                                                                    
090700                                                                          
090800                                                                          
090900                                                                          
091000 S01-NOLLA-EV-MINUS-SALDON SECTION.                                       
091100     MOVE 'S01-NOLLA-MINUS ' TO CURRENT-SECTION                           
091200                                                                          
091300     IF SLAG-KVOKS-DAG < +0                                               
091400       MOVE +0                 TO W-KVOKS-DAG                             
091500     ELSE                                                                 
091600       MOVE SLAG-KVOKS-DAG     TO W-KVOKS-DAG                             
091700     END-IF                                                               
091800     IF SLAG-KVOKS-BULK < +0                                              
091900       MOVE +0                 TO W-KVOKS-BULK                            
092000     ELSE                                                                 
092100       MOVE SLAG-KVOKS-BULK    TO W-KVOKS-BULK                            
092200     END-IF                                                               
092300     IF SLAG-KVRESS < +0                                                  
092400       MOVE +0                 TO W-KVRESS                                
092500     ELSE                                                                 
092600       MOVE SLAG-KVRESS        TO W-KVRESS                                
092700     END-IF                                                               
092800     .                                                                    
092900                                                                          
093000                                                                          
093100                                                                          
093200 S02-FIXA-LEV-DC SECTION.                                                 
093300     MOVE 'S02-FIXA-LEV-DC ' TO CURRENT-SECTION                           
093400                                                                          
093500     IF NDCA-KDERS < 20                                                   
093600       IF NDCA-KDORDKL = 0                                                
093700         MOVE 92                 TO NDCA-KDORDBEK                         
093800       ELSE                                                               
093900         IF NDCA-FLRESTN = JA AND NDCA-FLPRELRO = JA                      
094000           MOVE 99               TO NDCA-KDORDBEK                         
094100         END-IF                                                           
094200       END-IF                                                             
094300       IF NDCA-IDDC-RO = W-IDDC                                           
094400         IF NDCA-KDORDKL > 1                                              
094500            ADD NDCA-KVBEART-Q   TO SLAG-KVOKS-BULK                       
094600         ELSE                                                             
094700            ADD NDCA-KVBEART-Q   TO SLAG-KVOKS-DAG                        
094800         END-IF                                                           
094900         MOVE NDCA-KVBEART-Q     TO NDCA-KVPRERO                          
095000       ELSE                                                               
095100         MOVE W-DISP-HOME        TO NDCA-KVPREAVB                         
095200         COMPUTE NDCA-KVPRERO = NDCA-KVBEART-Q                            
095300                              - W-DISP-HOME                               
095400         MOVE NDCA-IDDC-RO       TO W-IDDC                                
095500         PERFORM IMS-GHU-WDK711                                           
095600         IF NDCA-KDORDKL > 1                                              
095700            ADD NDCA-KVBEART-Q   TO SLAG-KVOKS-BULK                       
095800         ELSE                                                             
095900            ADD NDCA-KVBEART-Q   TO SLAG-KVOKS-DAG                        
096000         END-IF                                                           
096100       END-IF                                                             
096200       PERFORM S12-REPL-WDK7                                              
096300       MOVE NDCA-IDDC-RO         TO NDCA-IDDC                             
096400     END-IF                                                               
096500     .                                                                    
096600                                                                          
096700                                                                          
096800                                                                          
096900 S03-BACKA-TILL-DC-RO SECTION.                                            
097000     MOVE 'S03-BACKA-DC    ' TO CURRENT-SECTION                           
097100                                                                          
097200     IF NDCA-KDERS < 20                                                   
097300       MOVE NDCA-IDDC-RO         TO W-IDDC                                
097400                                    NDCA-IDDC                             
097500       PERFORM IMS-GHU-WDK711                                             
097600       IF NDCA-KDORDKL = +0                                               
097700         MOVE 92                 TO NDCA-KDORDBEK                         
097800         ADD NDCA-KVBEART-Q      TO SLAG-KVOKS-DAG                        
097900       ELSE                                                               
098000         IF NDCA-KDORDKL = +1                                             
098100           IF NDCA-FLRESTN = JA AND NDCA-FLPRELRO = JA                    
098200             MOVE 99             TO NDCA-KDORDBEK                         
098300           ELSE                                                           
098400             IF NDCA-IDDC-RO = NDCA-IDDC-CLEAR                            
098500                            IN NDCA-IDDC-CLEAR-GRP(FORSTA-IX)             
098600               MOVE ZERO         TO NDCA-KDORDBEK                         
098700             END-IF                                                       
098800           END-IF                                                         
098900           ADD NDCA-KVBEART-Q    TO SLAG-KVOKS-DAG                        
099000         ELSE                                                             
099100           MOVE ZERO             TO NDCA-KDORDBEK                         
099200           ADD NDCA-KVBEART-Q    TO SLAG-KVOKS-BULK                       
099300         END-IF                                                           
099400       END-IF                                                             
099500       PERFORM S12-REPL-WDK7                                              
099600       MOVE W-DISP-HOME          TO NDCA-KVPREAVB                         
099700       COMPUTE NDCA-KVPRERO       = NDCA-KVBEART-Q                        
099800                                  - W-DISP-HOME                           
099900     END-IF                                                               
100000     .                                                                    
100100                                                                          
100200                                                                          
100300                                                                          
100400 S04-FIXA-VOR-RAD SECTION.                                                
100500     MOVE 'S04-FIXA-VOR-RAD' TO CURRENT-SECTION                           
100600                                                                          
100700     IF NDCA-KDERS < 20                                                   
100800       IF NDCA-IDDC-RO = W-IDDC                                           
100900         MOVE W-DISP             TO NDCA-KVPREAVB                         
101000         COMPUTE NDCA-KVPRERO = NDCA-KVBEART-Q                            
101100                              - W-DISP                                    
101200       ELSE                                                               
101300         MOVE NDCA-IDDC-RO       TO W-IDDC                                
101400         PERFORM IMS-GHU-WDK711                                           
101500         MOVE W-DISP-HOME        TO NDCA-KVPREAVB                         
101600         COMPUTE NDCA-KVPRERO = NDCA-KVBEART-Q                            
101700                              - W-DISP-HOME                               
101800       END-IF                                                             
101900       ADD NDCA-KVBEART-Q        TO SLAG-KVOKS-DAG                        
102000       PERFORM S12-REPL-WDK7                                              
102010       IF LART-FLREFERAL = JA                                             
102020          MOVE 98                TO NDCA-KDORDBEK                         
102030       ELSE                                                               
102100          MOVE 92                TO NDCA-KDORDBEK                         
102110       END-IF                                                             
102200       MOVE NDCA-IDDC-RO         TO NDCA-IDDC                             
102300     END-IF                                                               
102400     .                                                                    
102500                                                                          
102600                                                                          
102700                                                                          
102800 S05-FIXA-DAY-RAD SECTION.                                                
102900     MOVE 'S05-FIXA-DAY-RAD' TO CURRENT-SECTION                           
103000                                                                          
103100     IF NDCA-KDERS < 20                                                   
103200       IF NDCA-IDDC-RO = W-IDDC                                           
103300         IF NDCA-FLRESTN = JA AND NDCA-FLPRELRO = JA                      
103400           MOVE 99               TO NDCA-KDORDBEK                         
103500         END-IF                                                           
103600         MOVE W-DISP             TO NDCA-KVPREAVB                         
103700         COMPUTE NDCA-KVPRERO = NDCA-KVBEART-Q                            
103800                              - W-DISP                                    
103900       ELSE                                                               
104000         MOVE W-DISP-HOME        TO NDCA-KVPREAVB                         
104100         COMPUTE NDCA-KVPRERO = NDCA-KVBEART-Q                            
104200                              - W-DISP-HOME                               
104300         IF NDCA-FLRESTN = JA AND NDCA-FLPRELRO = JA                      
104400           MOVE 99               TO NDCA-KDORDBEK                         
104500         ELSE                                                             
104600           IF NDCA-IDDC-RO = NDCA-IDDC-CLEAR                              
104700                          IN NDCA-IDDC-CLEAR-GRP(FORSTA-IX)               
104800             MOVE ZERO           TO NDCA-KDORDBEK                         
104900           END-IF                                                         
105000         END-IF                                                           
105100         MOVE NDCA-IDDC-RO        TO W-IDDC                               
105200         PERFORM IMS-GHU-WDK711                                           
105300       END-IF                                                             
105400       ADD NDCA-KVBEART-Q        TO SLAG-KVOKS-DAG                        
105500       PERFORM S12-REPL-WDK7                                              
105600       MOVE NDCA-IDDC-RO         TO NDCA-IDDC                             
105700     END-IF                                                               
105800     .                                                                    
105900                                                                          
106000                                                                          
106100                                                                          
106200 S06-FIXA-BULK-RAD SECTION.                                               
106300     MOVE 'S06-FIX-BULK-RAD' TO CURRENT-SECTION                           
106320     MOVE NDCA-IDDISTR         TO TEST-IDDISTR                            
106400                                                                          
106501     IF NDCA-KDERS < 20  OR (NDCA-KDERS > 20 AND (DIST35-REFILL           
106502                         OR  DIST35-REFILL-INOM-NDC                       
106503                         OR  DIST35-NONVCC-REFILL                         
106504                         OR  DIST35-RETUR                                 
106505                         OR  DIST18-SKROT                                 
106506                         OR  DIST18-SCRAP-NDC))                           
106600       IF NDCA-IDDC-RO = W-IDDC                                           
106700         ADD NDCA-KVBEART-Q     TO SLAG-KVOKS-BULK                        
106800         PERFORM S12-REPL-WDK7                                            
106900         MOVE W-DISP            TO NDCA-KVPREAVB                          
107000         COMPUTE NDCA-KVPRERO = NDCA-KVBEART-Q                            
107100                              - W-DISP                                    
107200       ELSE                                                               
107300         MOVE NDCA-IDDC-RO      TO W-IDDC                                 
107400         PERFORM IMS-GHU-WDK711                                           
107500         ADD NDCA-KVBEART-Q     TO SLAG-KVOKS-BULK                        
107600         PERFORM S12-REPL-WDK7                                            
107700         MOVE W-DISP-HOME       TO NDCA-KVPREAVB                          
107800         COMPUTE NDCA-KVPRERO = NDCA-KVBEART-Q                            
107900                              - W-DISP-HOME                               
108000       END-IF                                                             
108100       IF NDCA-IDDC-RO = NDCA-IDDC-CLEAR                                  
108200                      IN NDCA-IDDC-CLEAR-GRP(FORSTA-IX)                   
108300         MOVE ZERO              TO NDCA-KDORDBEK                          
108400       END-IF                                                             
108500       MOVE NDCA-IDDC-RO        TO NDCA-IDDC                              
108600     END-IF                                                               
108700     .                                                                    
108800                                                                          
108900                                                                          
109000                                                                          
109100 S07-BERAKN-DISP-VOR SECTION.                                             
109200     MOVE 'S07-BERAKN-VOR  ' TO CURRENT-SECTION                           
109300                                                                          
109410*    IF DIST07-KINA                                                       
109420*    IF DIST07-KINA OR DIST07-USA-CUSTOMERS                               
109500        COMPUTE W-DISP = SLAG-KVLS                                        
109600                       + WS-KVAKS                                         
109700                       - W-KVOKS-DAG                                      
109800*    ELSE                                                                 
109900*       COMPUTE W-DISP = SLAG-KVLS                                        
110000*                      - W-KVOKS-DAG                                      
110100*    END-IF                                                               
110200     IF W-DISP > ZERO                                                     
110300       COMPUTE W-DISP = W-DISP                                            
110400                      - SLAG-KVUTRS                                       
110500                      - SLAG-KVSPARR-KVAL                                 
110600     END-IF                                                               
110700     IF W-DISP < ZERO                                                     
110800        MOVE ZERO          TO W-DISP                                      
110900     END-IF                                                               
110910     PERFORM S13-ANPASSA-DISP-TILL-KVANT                                  
111000     .                                                                    
111100                                                                          
111200                                                                          
111300                                                                          
111400 S08-BERAKN-DISP-DAG SECTION.                                             
111500     MOVE 'S08-BERAKN-DAG  ' TO CURRENT-SECTION                           
111600                                                                          
111710*    IF DIST07-KINA                                                       
111720*    IF DIST07-KINA OR DIST07-USA-CUSTOMERS                               
111800        COMPUTE W-DISP = SLAG-KVLS                                        
111900                       + WS-KVAKS                                         
112000                       - W-KVOKS-DAG                                      
112100*    ELSE                                                                 
112200*       COMPUTE W-DISP = SLAG-KVLS                                        
112300*                      - W-KVOKS-DAG                                      
112400*    END-IF                                                               
112500     IF W-DISP > ZERO                                                     
112600       COMPUTE W-DISP = W-DISP                                            
112700                      - W-KVRESS                                          
112800                      - SLAG-KVUTRS                                       
112900                      - SLAG-KVSPARR-KVAL                                 
113000                      - W-KVSPANT                                         
113100     END-IF                                                               
113200     IF W-DISP < ZERO                                                     
113300       MOVE ZERO               TO W-DISP                                  
113400     END-IF                                                               
113410     PERFORM S13-ANPASSA-DISP-TILL-KVANT                                  
113500     .                                                                    
113600                                                                          
113700                                                                          
113800                                                                          
113900 S09-BERAKN-DISP-BULK SECTION.                                            
114000     MOVE 'S09-BERAKN-BULK ' TO CURRENT-SECTION                           
114100                                                                          
114200     MOVE SLAG-KVLS            TO W-DISP                                  
114310*    IF DIST07-KINA                                                       
114320*    IF DIST07-KINA OR DIST07-USA-CUSTOMERS                               
114400        COMPUTE W-DISP = W-DISP                                           
114500                       + WS-KVAKS                                         
114600                       - W-KVOKS-DAG                                      
114700                       - W-KVOKS-BULK                                     
114800*    ELSE                                                                 
114900*       COMPUTE W-DISP = W-DISP                                           
115000*                      - W-KVOKS-DAG                                      
115100*                      - W-KVOKS-BULK                                     
115200*    END-IF                                                               
115300     IF W-DISP > ZERO                                                     
115400       COMPUTE W-DISP = W-DISP                                            
115500                      - W-KVRESS                                          
115600                      - SLAG-KVUTRS                                       
115700                      - SLAG-KVSPARR-KVAL                                 
115800                      - W-KVSPANT                                         
115900     END-IF                                                               
116000     IF W-DISP < 0                                                        
116100       MOVE ZERO               TO W-DISP                                  
116200     END-IF                                                               
116210     PERFORM S13-ANPASSA-DISP-TILL-KVANT                                  
116300     .                                                                    
116400                                                                          
116500                                                                          
116600                                                                          
116700 S10-CHECK-NEXT-REFILL SECTION.                                           
116800     MOVE 'S10-CHECK-REFILL' TO CURRENT-SECTION                           
116900                                                                          
117000     PERFORM IMS-GU-WDL601                                                
117100     IF SEGMENT-FINNS                                                     
117200       PERFORM IMS-GNP-WDL611-OKVAL                                       
117300       IF SEGMENT-FINNS                                                   
117400         PERFORM UNTIL SEGMENT-SAKNAS                                     
117500         MOVE INL-TIBERANK    TO TMP1-YYMMDD                              
117600         MOVE NDCA-TIREGDAT   TO TMP2-YYMMDD                              
117700         MOVE WS-TIBERANK     TO TMP3-YYMMDD                              
117800         PERFORM WY2000Q1                                                 
117900           IF INL-IDDC = W-IDDC                 AND                       
118000             (INL-IDPTYP = '310' OR 'R30')      AND                       
118100             (TMP1-YYMMDD >= TMP2-YYMMDD) AND                             
118200              TMP1-YYMMDD <  TMP3-YYMMDD                                  
118300                                                                          
118400             MOVE INL-TIBERANK TO WS-TIBERANK                             
118500             MOVE INL-KVAVIS   TO WS-KVAVIS                               
118600           END-IF                                                         
118700           PERFORM IMS-GNP-WDL611-OKVAL                                   
118800         END-PERFORM                                                      
118900       END-IF                                                             
119000     END-IF                                                               
119100     MOVE WS-TIBERANK   TO TMP1-YYMMDD                                    
119200     MOVE 999999        TO TMP2-YYMMDD                                    
119300     PERFORM WY2000P1                                                     
119400     IF TMP1-YYMMDD > ZERO AND TMP1-YYMMDD < TMP2-YYMMDD                  
119500       MOVE 001                  TO DAG-KDCALL                            
119600       MOVE NDCA-TIREGDAT        TO DAG-TIAAMMDD-FOM                      
119700       MOVE WS-TIBERANK          TO DAG-TIAAMMDD-TOM                      
119800                                                                          
119900       CALL WDAGKONV USING DAG-KDCALL DAG-DATUM-AREA                      
120000                           DAG-KDSVAR                                     
120100                                                                          
120200       IF DAG-KDSVAR = 'F'                                                
120300         MOVE 'FEL FRÅN DAGKONV BABA-SECTION' TO FELTEXT                  
120400         CALL ABEND USING RKOD-ABEND-UTAN-DUMP                            
120500       ELSE                                                               
120600         IF DAG-KVKALDAG NOT > NDCA-KVDAGAR-DOW                           
120700           MOVE 99              TO NDCA-KDORDBEK                          
120800         ELSE                                                             
120900           MOVE ZERO TO WS-KVAVIS                                         
121000         END-IF                                                           
121100       END-IF                                                             
121200     END-IF                                                               
121300     .                                                                    
121400                                                                          
121500                                                                          
121600                                                                          
121700 S11-CHECK-KVSPANT          SECTION.                                      
121800     MOVE 'S11-CHECK-KVSPAN' TO CURRENT-SECTION                           
121900                                                                          
122000     IF AKTUELLT-LAND-KINA                                                
122100        PERFORM IMS-GU-WDK722                                             
122200        IF SEGMENT-FINNS                                                  
122300           MOVE XLAG-KVSPANT      TO W-KVSPANT                            
122400        ELSE                                                              
122500           MOVE ZERO              TO W-KVSPANT                            
122600        END-IF                                                            
122700     ELSE                                                                 
122800        MOVE ZERO                 TO W-KVSPANT                            
122900     END-IF                                                               
123000     .                                                                    
123100                                                                          
123200                                                                          
123300 S12-REPL-WDK7 SECTION.                                                   
123400     MOVE 'S12-REPL-WDK7   ' TO CURRENT-SECTION                           
123500                                                                          
123600     IF NDCA-KDCALL = +1 OR +3                                            
123700        PERFORM IMS-REPL-WDK7                                             
123710        IF NDCA-KDCALL = +3                                               
123711           MOVE ZERO            TO XDK7-KDIDDC                            
123712           MOVE SLAG-IDDC       TO XDK7-IDDC                              
123713           MOVE SLAG-KVOKS-DAG  TO XDK7-KVOKS-DAG                         
123714           MOVE SLAG-KVOKS-BULK TO XDK7-KVOKS-BULK                        
123720        END-IF                                                            
123800     END-IF                                                               
123801                                                                          
123810     IF NDCA-KDCALL = +2                                                  
123820        MOVE ZERO            TO XDK7-KDIDDC                               
123830        MOVE SLAG-IDDC       TO XDK7-IDDC                                 
123840        MOVE SLAG-KVOKS-DAG  TO XDK7-KVOKS-DAG                            
123850        MOVE SLAG-KVOKS-BULK TO XDK7-KVOKS-BULK                           
123860     END-IF                                                               
123900     .                                                                    
124000                                                                          
124100                                                                          
124110 S13-ANPASSA-DISP-TILL-KVANT SECTION.                                     
124120                                                                          
124121     IF NDCA-KDSORT    = 'L ' AND                                         
124122        NDCA-KVQPACK-1 > ZERO                                             
124123                                                                          
124130       IF W-DISP < NDCA-KVBEART-Q                                         
124140          COMPUTE W-DISP-KVANT =                                          
124150                  W-DISP / NDCA-KVQPACK-1                                 
124160          COMPUTE W-DISP-KVANT =                                          
124170                  W-DISP-KVANT * NDCA-KVQPACK-1                           
124180          MOVE W-DISP-KVANT TO W-DISP                                     
124190       END-IF                                                             
124191     END-IF                                                               
124192     .                                                                    
124193     EJECT                                                                
124194                                                                          
124200                                                                          
124310* --- IMS SEKTIONER ---                                                   
124400 IMS-GU-WDK701 SECTION.                                                   
124500     MOVE 'IMS-GU-WDK701   ' TO CURRENT-IMS-SECTION                       
124600                                                                          
124700     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
124800          DELIMITED BY SIZE INTO SSA1                                     
124900     MOVE '  GE' TO GODK-STATUSKODER                                      
125000     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-WDK711 SSA1                    
125100     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
125200     PERFORM IMS-STATUSKONTROLL                                           
125300     .                                                                    
125400                                                                          
125500                                                                          
125600 IMS-GHU-WDK711 SECTION.                                                  
125700     MOVE 'IMS-GHU-WDK711  ' TO CURRENT-IMS-SECTION                       
125800                                                                          
125900     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
126000          DELIMITED BY SIZE INTO SSA1                                     
126100     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
126200          DELIMITED BY SIZE INTO SSA2                                     
126300     MOVE '  GE' TO GODK-STATUSKODER                                      
126400     CALL CBLTDLI USING GHU WDK7-PCB DLI-IO-WDK711 SSA1 SSA2              
126500     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
126600     PERFORM IMS-STATUSKONTROLL                                           
126700     .                                                                    
126800                                                                          
126900                                                                          
127000 IMS-GU-WDK711 SECTION.                                                   
127100     MOVE 'IMS-GU-WDK711   ' TO CURRENT-IMS-SECTION                       
127200                                                                          
127300     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
127400          DELIMITED BY SIZE INTO SSA1                                     
127500     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
127600          DELIMITED BY SIZE INTO SSA2                                     
127700     MOVE '  GE' TO GODK-STATUSKODER                                      
127800     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-WDK711 SSA1 SSA2               
127900     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
128000     PERFORM IMS-STATUSKONTROLL                                           
128100     .                                                                    
128200                                                                          
128300                                                                          
128400 IMS-GNP-WDK711-1441 SECTION.                                             
128500     MOVE 'IMS-GNP-WDK711  ' TO CURRENT-IMS-SECTION                       
128600                                                                          
128700     STRING 'WDK711  (IDDC     =' W-IDDC-X                                
128800                    '&IDLEVNR  =' W-IDLEVNR-1441-X ')'                    
128900          DELIMITED BY SIZE INTO SSA1                                     
129000     MOVE '  GE' TO GODK-STATUSKODER                                      
129100     CALL CBLTDLI USING GNP WDK7-PCB DLI-IO-WDK711 SSA1                   
129200     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
129300     PERFORM IMS-STATUSKONTROLL                                           
129400     .                                                                    
129500                                                                          
129600                                                                          
129700 IMS-GU-WDK712 SECTION.                                                   
129800     MOVE 'IMS-GU-WDK712   ' TO CURRENT-IMS-SECTION                       
129900                                                                          
130000     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
130100          DELIMITED BY SIZE INTO SSA1                                     
130200     STRING 'WDK712  (IDLAND   =' W-IDLAND-X ')'                          
130300          DELIMITED BY SIZE INTO SSA2                                     
130400     MOVE '  GE'              TO GODK-STATUSKODER                         
130500     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-WDK712 SSA1 SSA2               
130600     MOVE WDK7-STATUS-CODE    TO STATUS-WS                                
130700     PERFORM IMS-STATUSKONTROLL                                           
130800     .                                                                    
130900                                                                          
131000                                                                          
131100 IMS-GU-WDK722 SECTION.                                                   
131200     MOVE 'IMS-GU-WDK722   ' TO CURRENT-IMS-SECTION                       
131300                                                                          
131400     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
131500          DELIMITED BY SIZE INTO SSA1                                     
131600     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
131700          DELIMITED BY SIZE INTO SSA2                                     
131800     MOVE 'WDK722 '           TO SSA3                                     
131900     MOVE '  GE' TO GODK-STATUSKODER                                      
132000     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-WDK722 SSA1 SSA2 SSA3          
132100     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
132200     PERFORM IMS-STATUSKONTROLL                                           
132300     .                                                                    
132400                                                                          
132500                                                                          
132600 IMS-REPL-WDK7 SECTION.                                                   
132700     MOVE 'IMS-REPL-WDK7   ' TO CURRENT-IMS-SECTION                       
132800                                                                          
132900     MOVE '    ' TO GODK-STATUSKODER                                      
133000     CALL CBLTDLI USING REPL WDK7-PCB DLI-IO-WDK711                       
133100     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
133200     PERFORM IMS-STATUSKONTROLL                                           
133300     .                                                                    
133400                                                                          
133500                                                                          
133600 IMS-GU-WDL601 SECTION.                                                   
133700     MOVE 'IMS-GU-WDL601   ' TO CURRENT-IMS-SECTION                       
133800                                                                          
133900     STRING 'WDL601  (IDARTNR  =' W-IDARTNR-X ')'                         
134000          DELIMITED BY SIZE INTO SSA1                                     
134100     MOVE '  GE'              TO GODK-STATUSKODER                         
134200     CALL CBLTDLI USING GU WDL6-PCB DLI-IO-WDL601 SSA1                    
134300     MOVE WDL6-STATUS-CODE    TO STATUS-WS                                
134400     PERFORM IMS-STATUSKONTROLL                                           
134500     .                                                                    
134600                                                                          
134700                                                                          
134800 IMS-GNP-WDL611 SECTION.                                                  
134900     MOVE 'IMS-GNP-WDL611  '  TO CURRENT-IMS-SECTION                      
135000                                                                          
135100     STRING 'WDL611  (IDPTYP   =' W-IDPTYP-X                              
135200                    '&KDRT     =' W-KDRT-07-X                             
135300                    '!IDPTYP   =' W-IDPTYP-X                              
135400                    '&KDRT     =' W-KDRT-77-X ')'                         
135500          DELIMITED BY SIZE INTO SSA1                                     
135600     MOVE '  GE'              TO GODK-STATUSKODER                         
135700     CALL CBLTDLI USING GNP WDL6-PCB DLI-IO-WDL611 SSA1                   
135800     MOVE WDL6-STATUS-CODE    TO STATUS-WS                                
135810     PERFORM IMS-STATUSKONTROLL                                           
135820     .                                                                    
135830                                                                          
135840                                                                          
135850 IMS-GNP-WDL611-OKVAL SECTION.                                            
135860     MOVE 'IMS-GNP-WDL611-O' TO CURRENT-IMS-SECTION                       
135870                                                                          
135880     MOVE 'WDL611  '         TO SSA1                                      
135890     MOVE '  GE' TO GODK-STATUSKODER                                      
135891     CALL CBLTDLI USING GNP WDL6-PCB DLI-IO-WDL611 SSA1                   
135892     MOVE WDL6-STATUS-CODE   TO STATUS-WS                                 
135893     PERFORM IMS-STATUSKONTROLL                                           
135894     .                                                                    
135900 IMS-GU-WDB601 SECTION.                                                   
136000     MOVE 'IMS-GU-WDB601   ' TO CURRENT-IMS-SECTION                       
136100                                                                          
136200     STRING 'WDB601  (IDDC     =' W-IDDC-X ')'                            
136300          DELIMITED BY SIZE INTO SSA1                                     
136400     MOVE '  '                TO GODK-STATUSKODER                         
136500     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-WDB601 SSA1                    
136600     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
136700     PERFORM IMS-STATUSKONTROLL                                           
136800     .                                                                    
136900                                                                          
137000                                                                          
137100 IMS-STATUSKONTROLL SECTION.                                              
137200                                                                          
137300     SET STATUS-IX TO 1                                                   
137400     SEARCH GODK-STATUS                                                   
137500       AT END                                                             
137600         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
137700           DELIMITED BY SIZE INTO FELTEXT                                 
137800         DISPLAY FELTEXT                                                  
137900         CALL FELLOG                                                      
138000       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
138100         CONTINUE                                                         
138200     END-SEARCH                                                           
138300     .                                                                    
138400                                                                          
138500*    -COPY WY2000P1                                                       
138600*    -COPY WY2000Q1                                                       
