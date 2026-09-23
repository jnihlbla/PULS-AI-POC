000100 ID DIVISION.                                                             
000200 PROGRAM-ID.        W1220400.                                             
000300 AUTHOR.            P. DAHLÖF.                                            
000400 DATE-WRITTEN.      JULI 1987.                                            
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    LÄSER WDK6 MED SB                                                    
000800*          WDK7 MED DL1                                                   
000900*          WDD9 MED DL1                                                   
001000*          WDK9 MED DL1                                                   
001100*    SKAPAR 3 FILER  W12207 = MED ARTIKLAR SOM SKA SKALAS/RENSAS          
001200*                    - ARIKLAR MED KDERS > 20 SKALAS EFTER 3 ÅR           
001300*                      IDPTYP = S (UNDANTAG KDERS = 27 ELLER 28)          
001400*                    - ARIKLAR MED KDERS = 52 RENSAS EFTER 1 ÅR           
001500*                      IDPTYP = B                                         
001600*                      UNDANTAG ÄR DE ARTIKLAR SOM LIGGER PÅ              
001700*                      LEVERANSREGISTRET WDA2 GÖRS DOCK I ETT             
001800*                      ANNAT PROGRAM                                      
001900*                    W12205 MED LARMADE ARTIKLAR                          
002000*                      - SALDO CDC > 0                                    
002100*                      - SALDO SDC > 0                                    
002200*                      - TILLKOMMANDE I ERSÄTTNING                        
002300*                      - BESTÄLLNINGSREST                                 
002400*                      - DE ARTIKLAR SOM BORDE SKALAS/RENSAS MEN          
002500*                        LIGGER PÅ LEVERANSREGISTRET WDA2 GÖRS            
002600*                        DOCK I ETT ANNAT PROGRAM                         
002700*                    W12203 MED SAMTLIGA ARTIKLAR SOM REDAN ÄR            
002800*                           SKALADE = KDERS-UTG > 0                       
002900     EJECT                                                                
003000 ENVIRONMENT DIVISION.                                                    
003100 INPUT-OUTPUT SECTION.                                                    
003200 FILE-CONTROL.                                                            
003300     SKIP2                                                                
003400*- - - - - - - - - - - - - - - - -  LARMT-FIL W12205                      
003500     SELECT W12205           ASSIGN TO      W12204D1.                     
003600*- - - - - - - - - - - - -  BORTTAG/SKALN-FIL W12207                      
003700     SELECT W12207           ASSIGN TO      W12204D2.                     
003800*- - - - - - - - - - - - -  FIL MED 'GAMLA' SKALADE ARTIKLAR              
003900     SELECT W12203           ASSIGN TO      W12204D3.                     
004000     EJECT                                                                
004100 DATA DIVISION.                                                           
004200 FILE SECTION.                                                            
004300     SKIP2                                                                
004400 FD  W12205                                                               
004500     RECORDING F                                                          
004600     BLOCK CONTAINS 0.                                                    
004700*01  POST -COPY W12205  -PRE W12205-   -L.                                
004800     SKIP2                                                                
004900 FD  W12207                                                               
005000     RECORDING F                                                          
005100     BLOCK CONTAINS 0.                                                    
005200*01  POST -COPY W12207  -PRE W12207-   -L.                                
005300     EJECT                                                                
005400 FD  W12203                                                               
005500     RECORDING F                                                          
005600     BLOCK CONTAINS 0.                                                    
005700*01  POST -COPY W12203  -PRE W12203-   -L.                                
005800     EJECT                                                                
005900 WORKING-STORAGE SECTION.                                                 
006000                                                                          
006100*    -- CHECKED BY WY2000                                                 
006200     SKIP3                                                                
006300 77  PROGRAM-NAMN            PIC X(8)    VALUE 'W1220400'.                
006400 77  FELTEXT                 PIC X(25)   VALUE SPACE.                     
006500 77  JA                      PIC X       VALUE 'J'.                       
006600 77  NEJ                     PIC X       VALUE 'N'.                       
006700 77  SDC-IX                  PIC S9(3)   VALUE ZERO COMP-3.               
006800 77  SDC-IX-MAX              PIC S9(3)   VALUE +3 COMP-3.                 
006900 77  SPAR-KVAKS              PIC S9(7)   VALUE ZERO COMP-3.               
007000 77  SPAR-KVBR               PIC S9(7)   VALUE ZERO COMP-3.               
007100 77  SPAR-TIREGDAT           PIC 9(6)    VALUE ZERO.                      
007200 77  SW-KEMIDAT-OK           PIC X       VALUE SPACE.                     
007300 77  SPARAD-DAGENS-DATUM-AAVVD  PIC 9(5) VALUE ZERO.                      
007400 77  WS-ANT-ARTIKLAR         PIC 9(5)    VALUE ZERO.                      
007500                                                                          
007600*01  -COPY WWDCKONS                                                       
007700                                                                          
007800*01  -COPY WWPRODSL                                                       
007900                                                                          
008000 01  WS-TIREGDAT-SEKEL       PIC 9(8) VALUE ZERO.                         
008100 01  FILLER REDEFINES WS-TIREGDAT-SEKEL.                                  
008200     05  WS-SEKEL             PIC 9(2).                                   
008300     05  WS-TIREGDAT          PIC 9(6).                                   
008400                                                                          
008500 01  WS-KOLLDAT              PIC 9(8) VALUE ZERO.                         
008600 01  FILLER REDEFINES WS-KOLLDAT.                                         
008700     05  WS-KOLLDAT-AAR       PIC 9(4).                                   
008800     05  FILLER               PIC 9(4).                                   
008900                                                                          
009000 01  DAGENS-DATUM-AAAVVD      PIC 9(6) VALUE ZERO.                        
009100 01  FILLER REDEFINES DAGENS-DATUM-AAAVVD.                                
009200     05  DAGENS-AAA           PIC 9(3).                                   
009300     05  DAGENS-VV            PIC 99.                                     
009400     05  DAGENS-D             PIC  9.                                     
009500                                                                          
009600 01  WS-TIERSDAT-AAAVVD       PIC 9(6) VALUE ZERO.                        
009700 01  FILLER REDEFINES WS-TIERSDAT-AAAVVD.                                 
009800     03  W-TIERSDAT-AAA     PIC 9(3).                                     
009900     03  W-TIERSDAT-VV      PIC 9(2).                                     
010000     03  W-TIERSDAT-D       PIC  9.                                       
010100                                                                          
010200 77  W-TIERSDAT-SPARAD PIC 9(5)         VALUE ZERO.                       
010300 77  W-XX-ANTAL-AAA   PIC S9(3) COMP-3.                                   
010400 77  W-DIFF           PIC S9(3) COMP-3.                                   
010500 77  W-VECKOFAELT     PIC S9(2) COMP-3.                                   
010600 01  W-FLAGGA-ANTAL-AAA PIC X.                                            
010700    88  LAEGG-TILL-6-AAR           VALUE '6'.                             
010800    88  LAEGG-TILL-3-AAR           VALUE '3'.                             
010900    88  LAEGG-TILL-1-AAR           VALUE '1'.                             
011000                                                                          
011100 01  DYNAMISKA-SUBPROGRAM.                                                
011200     03  POSTSUM             PIC X(8)    VALUE 'POSTSUM'.                 
011300     03  CBLTDLI             PIC X(8)    VALUE 'CBLTDLI'.                 
011400     03  FELLOG              PIC X(8)    VALUE 'FELLOG '.                 
011500     03  WDATKONV            PIC X(8)    VALUE 'WDATKONV'.                
011600     EJECT                                                                
011700 01  FILLER                  PIC X(16)   VALUE 'BYTES-TESTER'.            
011800 01  TEST-IDARTNR            PIC S9(9)   COMP-3.                          
011900*01  FILLER -COPY WWBYT01 -RED TEST-IDARTNR                               
012000     EJECT                                                                
012100*01  FILLER -COPY WWDC99                                                  
012200     EJECT                                                                
012300*- - - - - - - - - - - - - - - - - PARAMETRAR TILL DATUMKORT              
012400     SKIP1                                                                
012500*01  -COPY WDATAREA                                                       
012600     EJECT                                                                
012700*- - - - - - - - - - - - - - - - - PARAMETRAR TILL POSTSUM                
012800     SKIP1                                                                
012900*01  -COPY W0005      -PRE POSTSUM-.                                      
013000     EJECT                                                                
013100*- - - - - - - - - - - - - - - - - UTAREA FÖR W12203-POST                 
013200*01  AREA -COPY W12203     -PRE UT03-.                                    
013300     EJECT                                                                
013400*- - - - - - - - - - - - - - - - - UTAREA FÖR W12205-POST                 
013500*01  AREA -COPY W12205     -PRE UT05-.                                    
013600     EJECT                                                                
013700*- - - - - - - - - - - - - - - - - UTAREA FÖR W12207-POST                 
013800*01  AREA -COPY W12207     -PRE UT07-.                                    
013900     EJECT                                                                
014000*- - - - - - - - - - - - - - - - - NYCKLAR                                
014100 01  W-WDD901KY-X.                                                        
014200   03 W-IDARTNR-D9    PIC S9(9) COMP-3 VALUE ZERO.                        
014300   03 W-IDDC-D9       PIC X(2)  VALUE SPACE.                              
014400 01  W-IDARTNR-X.                                                         
014500   03 W-IDARTNR       PIC S9(9) COMP-3 VALUE ZERO.                        
014600 01  W-IDDC-X.                                                            
014700   03 W-IDDC          PIC X(2)  VALUE SPACE.                              
014800                                                                          
014900 01    IMS-WS.                                                            
015000   03  FILLER           PIC X(8)   VALUE 'IMS-WS  '.                      
015100   03    STATUS-WS      PIC XX.                                           
015200     88  SEGMENT-FINNS             VALUE '  '.                            
015300     88  SEGMENT-SLUT              VALUE 'GB'.                            
015400     88  SEGMENT-SAKNAS            VALUE 'GE'.                            
015500                                                                          
015600   03    SSA1           PIC X(80).                                        
015700   03    SSA2           PIC X(80).                                        
015800                                                                          
015900   03    GODK-STATUSKODER.                                                
016000     05  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
016100     EJECT                                                                
016200*- - - - - - - - - - - - - - - - - IMS-CALL FUNKTIONER                    
016300*01      -COPY W0003.                                                     
016400     EJECT                                                                
016500*- - - - - - - - - - - - - - - - - IMS - COMM-AREA                        
016600 01  DLI-IO-AREA.                                                         
016700     03 IO-AREA             PIC X(900).                                   
016800*    03 ART-AREA  -COPY WDK601   -RED IO-AREA.                            
016900     EJECT                                                                
017000*    03 CLAG-AREA  -COPY WDK611   -RED IO-AREA.                           
017100     EJECT                                                                
017200 01  DLI-IO-AREA3.                                                        
017300     03 IO-AREA3            PIC X(100).                                   
017400*    03 AREA  -COPY WDK901 -PRE ARTM01- -RED IO-AREA3.                    
017500     EJECT                                                                
017600*    03 AREA  -COPY WDD902 -PRE INLB11- -RED IO-AREA3.                    
017700     EJECT                                                                
017800 01  DLI-IO-AREA4.                                                        
017900     03 IO-AREA4            PIC X(296).                                   
018000*    03 SLAG-AREA  -COPY WDK711  -RED IO-AREA4.                           
018100     EJECT                                                                
018200 LINKAGE SECTION.                                                         
018300*    -COPY W0008 -PRE WDK6-.                                              
018400          05  FILLER         PIC XX.                                      
018500     EJECT                                                                
018600*    -COPY W0008 -PRE ARTM-.                                              
018700          05  FILLER         PIC XX.                                      
018800     EJECT                                                                
018900*    -COPY W0008 -PRE INLB-.                                              
019000          05  FILLER         PIC XX.                                      
019100     EJECT                                                                
019200*    -COPY W0008 -PRE ARTS-.                                              
019300          05  FILLER         PIC XX.                                      
019400     EJECT                                                                
019500 PROCEDURE DIVISION  USING WDK6-PCB ARTM-PCB INLB-PCB ARTS-PCB.           
019600 MAIN SECTION.                                                            
019700     ENTRY 'DLITCBL' USING WDK6-PCB ARTM-PCB INLB-PCB ARTS-PCB.           
019800                                                                          
019900***  OBS  ALL INLÄSNING SKER TILL LARMFILS-UTAREAN  (UT05-)               
020000***       FÖR ATT EVENTUELLT BLI FLYTTADE TILL BORT/SKAL AREA             
020100                                                                          
020200     PERFORM A-INIT                                                       
020300                                                                          
020400     PERFORM IMS-GET-WDK6                                                 
020500     PERFORM UNTIL SEGMENT-SAKNAS OR SEGMENT-SLUT                         
020600***** FÖR TEST                                                            
020700       IF WS-ANT-ARTIKLAR < 5000                                          
020800***** FÖR TEST                                                            
020900       EVALUATE WDK6-SEG-NAME-FB                                          
021000                                                                          
021100       WHEN 'WDK601  '                                                    
021200         IF UT05-UTFIL-TYP = 'L'                                          
021300           PERFORM S11-SKRIV-LARMTRANS                                    
021400         ELSE                                                             
021500           IF UT05-UTFIL-TYP = 'S' OR 'B'                                 
021600             PERFORM S12-SKRIV-BORT-SKAL-TR                               
021700           END-IF                                                         
021800         END-IF                                                           
021900         PERFORM S01-SPACA-UTAREA                                         
022000         MOVE ART-IDARTNR     TO UT05-IDARTNR                             
022100         MOVE ART-IDLEVNR     TO UT05-IDLEVNR                             
022200         MOVE ART-FLERS       TO UT05-FLERS                               
022300         MOVE ART-KDPRODSL    TO UT05-KDPRODSL                            
022400                                 TEST-KDPRODSL                            
022500         MOVE ART-TIERSDAT    TO WS-TIERSDAT-AAAVVD                       
022600         MOVE ART-TIREGDAT    TO SPAR-TIREGDAT                            
022700                                                                          
022800         IF ART-KDERS-UTG > ZERO                                          
022900            PERFORM E-SKAPA-REDAN-SKALAD-FIL                              
023000         END-IF                                                           
023100                                                                          
023200       WHEN 'WDK611  '                                                    
023300         IF CLAG-KDERS > 20                                               
023400           IF CLAG-KDERS = 52                                             
023500              PERFORM B-KOLLA-KDERS-OCH-DATUM                             
023600           ELSE                                                           
023700             IF CLAG-KDERS = 27                                           
023800             OR CLAG-KDERS = 28                                           
023900               CONTINUE                                                   
024000             ELSE                                                         
024100               IF KDPRODSL-VOLVO-BYTES    OR                              
024200                  KDPRODSL-VOLVO-WHEELS                                   
024300                 CONTINUE                                                 
024400               ELSE                                                       
024500                 IF KDPRODSL-CHEMICAL                                     
024600                   PERFORM F-KOLLA-DATUM                                  
024700                   IF SW-KEMIDAT-OK = JA                                  
024800                     PERFORM B-KOLLA-KDERS-OCH-DATUM                      
024900                   END-IF                                                 
025000                 ELSE                                                     
025100                   PERFORM B-KOLLA-KDERS-OCH-DATUM                        
025200                 END-IF                                                   
025300               END-IF                                                     
025400             END-IF                                                       
025500           END-IF                                                         
025600         END-IF                                                           
025700       END-EVALUATE                                                       
025800***** FÖR TEST                                                            
025900       END-IF                                                             
026000***** FÖR TEST                                                            
026100       PERFORM IMS-GET-WDK6                                               
026200     END-PERFORM                                                          
026300                                                                          
026400     IF UT05-UTFIL-TYP = 'L'                                              
026500       PERFORM S11-SKRIV-LARMTRANS                                        
026600     ELSE                                                                 
026700       IF UT05-UTFIL-TYP = 'S' OR 'B'                                     
026800         PERFORM S12-SKRIV-BORT-SKAL-TR                                   
026900       END-IF                                                             
027000     END-IF                                                               
027100                                                                          
027200     PERFORM Z-FINIT                                                      
027300     MOVE ZERO TO RETURN-CODE                                             
027400     GOBACK                                                               
027500     .                                                                    
027600     EJECT                                                                
027700 A-INIT SECTION.                                                          
027800                                                                          
027900     OPEN OUTPUT W12205                                                   
028000                 W12207                                                   
028100                 W12203                                                   
028200                                                                          
028300     MOVE PROGRAM-NAMN TO POSTSUM-PROGNAMN                                
028400     PERFORM S01-SPACA-UTAREA                                             
028500     MOVE 'IDAG  ' TO DAT-KDDATFORM                                       
028600     CALL WDATKONV USING DAT-KDDATFORM                                    
028700                         DAT-I-TIDATUM                                    
028800                         DAT-O-TIDATUM                                    
028900                         DAT-KDSVAR                                       
029000     MOVE DAT-TIAAVVD TO DAGENS-DATUM-AAAVVD                              
029100                         SPARAD-DAGENS-DATUM-AAVVD                        
029200     .                                                                    
029300     EJECT                                                                
029400 B-KOLLA-KDERS-OCH-DATUM SECTION.                                         
029500                                                                          
029600     MOVE UT05-IDARTNR TO TEST-IDARTNR                                    
029700                                                                          
029800     IF CLAG-KDERS = 52                                                   
029900        MOVE '1' TO W-FLAGGA-ANTAL-AAA                                    
030000        PERFORM D-LAEGG-TILL-XX-ANTAL-AAA                                 
030100        IF DAGENS-DATUM-AAAVVD > WS-TIERSDAT-AAAVVD                       
030200           MOVE 'B' TO UT05-UTFIL-TYP                                     
030300           PERFORM BC-KOLLA-SALDON-FLERS                                  
030400           PERFORM BB-KOLLA-SDC                                           
030500           PERFORM BD-FLYTTA-TILL-UT05-AREA                               
030600****** FÖR TEST                                                           
030700           ADD +1 TO WS-ANT-ARTIKLAR                                      
030800****** FÖR TEST                                                           
030900        END-IF                                                            
031000        MOVE SPARAD-DAGENS-DATUM-AAVVD TO DAGENS-DATUM-AAAVVD             
031100     ELSE                                                                 
031200        MOVE '3' TO W-FLAGGA-ANTAL-AAA                                    
031300        PERFORM D-LAEGG-TILL-XX-ANTAL-AAA                                 
031400        IF DAGENS-DATUM-AAAVVD > WS-TIERSDAT-AAAVVD                       
031500           MOVE 'S' TO UT05-UTFIL-TYP                                     
031600           PERFORM BC-KOLLA-SALDON-FLERS                                  
031700           PERFORM BB-KOLLA-SDC                                           
031800           PERFORM BD-FLYTTA-TILL-UT05-AREA                               
031900****** FÖR TEST                                                           
032000           ADD +1 TO WS-ANT-ARTIKLAR                                      
032100****** FÖR TEST                                                           
032200        END-IF                                                            
032300        MOVE SPARAD-DAGENS-DATUM-AAVVD TO DAGENS-DATUM-AAAVVD             
032400     END-IF                                                               
032500     .                                                                    
032600     EJECT                                                                
032700 BB-KOLLA-SDC SECTION.                                                    
032800                                                                          
032900     MOVE UT05-IDARTNR TO W-IDARTNR                                       
033000     PERFORM IMS-GET-ARTS01                                               
033100     IF SEGMENT-FINNS                                                     
033200        PERFORM IMS-GET-ARTS11                                            
033300        PERFORM UNTIL SEGMENT-SAKNAS OR SEGMENT-SLUT                      
033400           MOVE SLAG-IDDC TO WS-IDDC                                      
033500           IF SDC                                                         
033600              MOVE +1 TO SDC-IX                                           
033700           ELSE                                                           
033800              IF NDC                                                      
033900                 MOVE +2 TO SDC-IX                                        
034000              ELSE                                                        
034100                 IF LDC                                                   
034200                    MOVE +3 TO SDC-IX                                     
034300                 END-IF                                                   
034400              END-IF                                                      
034500           END-IF                                                         
034600           IF SLAG-KVLS NOT = 0                                           
034700             MOVE 'L' TO UT05-UTFIL-TYP                                   
034800             MOVE JA TO UT05-SALDO-SDC(SDC-IX)                            
034900           END-IF                                                         
035000           IF SLAG-KVAKS-SDC NOT = 0                                      
035100             MOVE 'L' TO UT05-UTFIL-TYP                                   
035200             MOVE JA TO UT05-SALDO-SDC(SDC-IX)                            
035300           END-IF                                                         
035400           IF SLAG-KVAKS-PAV  NOT = 0                                     
035500             MOVE 'L' TO UT05-UTFIL-TYP                                   
035600             MOVE JA TO UT05-SALDO-SDC(SDC-IX)                            
035700           END-IF                                                         
035800           IF SLAG-KVOKS-BULK NOT = 0                                     
035900             MOVE 'L' TO UT05-UTFIL-TYP                                   
036000             MOVE JA TO UT05-SALDO-SDC(SDC-IX)                            
036100           END-IF                                                         
036200           IF SLAG-KVOKS-DAG NOT = 0                                      
036300             MOVE 'L' TO UT05-UTFIL-TYP                                   
036400             MOVE JA TO UT05-SALDO-SDC(SDC-IX)                            
036500           END-IF                                                         
036600           IF SLAG-KVEFRS NOT = 0                                         
036700             MOVE 'L' TO UT05-UTFIL-TYP                                   
036800             MOVE JA TO UT05-SALDO-SDC(SDC-IX)                            
036900           END-IF                                                         
037000           IF SLAG-KVBEART NOT = 0                                        
037100             MOVE 'L' TO UT05-UTFIL-TYP                                   
037200             MOVE JA TO UT05-SALDO-SDC(SDC-IX)                            
037300           END-IF                                                         
037400           PERFORM IMS-GET-ARTS11                                         
037500        END-PERFORM                                                       
037600     END-IF                                                               
037700     .                                                                    
037800     EJECT                                                                
037900 BC-KOLLA-SALDON-FLERS SECTION.                                           
038000     IF UT05-UTFIL-TYP = 'B'                                              
038100       IF UT05-FLERS = JA AND CLAG-KDERS = 52                             
038200          MOVE 'L'  TO UT05-UTFIL-TYP                                     
038300       END-IF                                                             
038400     END-IF                                                               
038500                                                                          
038600     IF CLAG-KVLS  NOT = 0                                                
038700       MOVE 'L'  TO UT05-UTFIL-TYP                                        
038800     END-IF                                                               
038900     IF CLAG-KVAKS-CDC  NOT = 0                                           
039000       MOVE 'L'  TO UT05-UTFIL-TYP                                        
039100     END-IF                                                               
039200     IF CLAG-KVAKS-PAV  NOT = 0                                           
039300       MOVE 'L'  TO UT05-UTFIL-TYP                                        
039400     END-IF                                                               
039500     IF CLAG-KVAKS-T  NOT = 0                                             
039600       MOVE 'L'  TO UT05-UTFIL-TYP                                        
039700     END-IF                                                               
039800     IF CLAG-KVEFRS NOT = 0                                               
039900       MOVE 'L'  TO UT05-UTFIL-TYP                                        
040000     END-IF                                                               
040100     IF CLAG-KVROS  NOT = 0                                               
040200       MOVE 'L'  TO UT05-UTFIL-TYP                                        
040300     END-IF                                                               
040400     IF CLAG-KVRESS  NOT = 0                                              
040500       MOVE 'L'  TO UT05-UTFIL-TYP                                        
040600     END-IF                                                               
040700                                                                          
040800     MOVE UT05-IDARTNR TO W-IDARTNR                                       
040900                          W-IDARTNR-D9                                    
041000     PERFORM IMS-GU-WLARTM01                                              
041100     IF SEGMENT-FINNS                                                     
041200       IF ARTM01-ART-SUTPO-TOT > 0                                        
041300          MOVE 'L' TO UT05-UTFIL-TYP                                      
041400       END-IF                                                             
041500       MOVE ARTM01-ART-SUTPO-TOT TO UT05-SUTPO-TOT                        
041600     ELSE                                                                 
041700       MOVE ZERO TO UT05-SUTPO-TOT                                        
041800     END-IF                                                               
041900                                                                          
042000     MOVE ZERO       TO SPAR-KVBR                                         
042100     MOVE WC-CDC-SE  TO W-IDDC-D9                                         
042200     PERFORM IMS-GET-WLINLB01                                             
042300     IF SEGMENT-FINNS                                                     
042400        PERFORM IMS-GET-WLINLB11                                          
042500        PERFORM UNTIL SEGMENT-SAKNAS                                      
042600           IF INLB11-KVBR > ZERO                                          
042700              ADD INLB11-KVBR   TO SPAR-KVBR                              
042800              MOVE 'L'          TO UT05-UTFIL-TYP                         
042900           END-IF                                                         
043000           PERFORM IMS-GET-WLINLB11                                       
043100        END-PERFORM                                                       
043200     END-IF                                                               
043300     .                                                                    
043400     EJECT                                                                
043500 BD-FLYTTA-TILL-UT05-AREA SECTION.                                        
043600                                                                          
043700     MOVE CLAG-IDANSK TO UT05-IDANSK                                      
043800     MOVE CLAG-KDERS  TO UT05-KDERS                                       
043900     MOVE CLAG-KVRESS TO UT05-KVRESS                                      
044000     COMPUTE SPAR-KVAKS =                                                 
044100                  CLAG-KVAKS-CDC + CLAG-KVAKS-PAV + CLAG-KVAKS-T          
044200     MOVE SPAR-KVAKS  TO UT05-KVAKS                                       
044300     MOVE CLAG-KVEFRS TO UT05-KVEFRS                                      
044400     MOVE CLAG-KVLS   TO UT05-KVLS                                        
044500     MOVE CLAG-KVROS  TO UT05-KVROS                                       
044600     MOVE SPAR-KVBR   TO UT05-KVBR                                        
044700     .                                                                    
044800     EJECT                                                                
044900 D-LAEGG-TILL-XX-ANTAL-AAA SECTION.                                       
045000                                                                          
045100     IF LAEGG-TILL-1-AAR                                                  
045200       ADD 1  TO W-TIERSDAT-AAA                                           
045300     ELSE                                                                 
045400       ADD 3  TO W-TIERSDAT-AAA                                           
045500     END-IF                                                               
045600     MOVE ZERO TO W-DIFF                                                  
045700     COMPUTE W-DIFF = W-TIERSDAT-AAA - DAGENS-AAA                         
045800     IF W-DIFF  > 50                                                      
045900       ADD 100 TO DAGENS-AAA                                              
046000     END-IF                                                               
046100     .                                                                    
046200     EJECT                                                                
046300 E-SKAPA-REDAN-SKALAD-FIL SECTION.                                        
046400                                                                          
046500     MOVE ART-IDARTNR   TO UT03-IDARTNR                                   
046600     MOVE ART-KDERS-UTG TO UT03-KDERS-UTG                                 
046700     MOVE ART-TIERSDAT  TO UT03-TIERSDAT                                  
046800     PERFORM S13-SKRIV-REDAN-SKALAD-FIL                                   
046900     .                                                                    
047000     EJECT                                                                
047100 F-KOLLA-DATUM SECTION.                                                   
047200                                                                          
047300     MOVE NEJ TO SW-KEMIDAT-OK                                            
047400                                                                          
047500     IF SPAR-TIREGDAT NOT = 0                                             
047600        MOVE SPAR-TIREGDAT TO WS-TIREGDAT                                 
047700        MOVE 'AAMMDD' TO DAT-KDDATFORM                                    
047800        MOVE WS-TIREGDAT TO DAT-I-TIDATUM                                 
047900        CALL WDATKONV USING DAT-KDDATFORM                                 
048000                            DAT-I-TIDATUM                                 
048100                            DAT-O-TIDATUM                                 
048200                            DAT-KDSVAR                                    
048300        IF DAT-KDSVAR-OK                                                  
048400           MOVE DAT-TISEKEL TO WS-SEKEL                                   
048500           MOVE WS-TIREGDAT-SEKEL TO WS-KOLLDAT                           
048600           IF WS-SEKEL = 19                                               
048700              CONTINUE                                                    
048800           ELSE                                                           
048900              IF WS-KOLLDAT-AAR > 2005                                    
049000                 MOVE JA TO SW-KEMIDAT-OK                                 
049100              END-IF                                                      
049200           END-IF                                                         
049300        ELSE                                                              
049400           MOVE 'FEL VID DATKONV' TO FELTEXT                              
049500           DISPLAY FELTEXT                                                
049600           CALL FELLOG                                                    
049700        END-IF                                                            
049800     END-IF                                                               
049900     .                                                                    
050000     EJECT                                                                
050100 S01-SPACA-UTAREA SECTION.                                                
050200                                                                          
050300     MOVE SPACE TO UT05-UTFIL-TYP                                         
050400                   UT07-UTFIL-TYP                                         
050500                                                                          
050600                   UT05-FLERS                                             
050700     MOVE +1 TO SDC-IX                                                    
050800     PERFORM UNTIL SDC-IX > SDC-IX-MAX                                    
050900        MOVE SPACE TO UT05-SALDO-SDC(SDC-IX)                              
051000        ADD +1 TO SDC-IX                                                  
051100     END-PERFORM                                                          
051200     MOVE ZERO  TO UT05-IDARTNR                                           
051300                   UT05-IDANSK                                            
051400                   UT05-KDERS                                             
051500                   UT05-KVRESS                                            
051600                   UT05-KVAKS                                             
051700                   UT05-SUTPO-TOT                                         
051800                   UT05-KVEFRS                                            
051900                   UT05-KVLS                                              
052000                   UT05-KVROS                                             
052100                   UT05-KVBR                                              
052200                   UT05-KDPRODSL                                          
052300     MOVE SPACE TO UT05-IDLEVNR                                           
052400     MOVE SPACE TO UT05-FLDISC                                            
052500                                                                          
052600     MOVE ZERO  TO UT03-IDARTNR                                           
052700                   UT03-KDERS-UTG                                         
052800                   UT03-TIERSDAT                                          
052900                                                                          
053000     MOVE ZERO  TO UT07-IDARTNR                                           
053100     MOVE SPACE TO UT07-IDLEVNR                                           
053200     .                                                                    
053300     EJECT                                                                
053400 S11-SKRIV-LARMTRANS SECTION.                                             
053500                                                                          
053600     WRITE W12205-POST FROM UT05-AREA                                     
053700                                                                          
053800     MOVE 'W12204D1' TO POSTSUM-DDNAMN2                                   
053900     MOVE 'W12205  ' TO POSTSUM-FDNAMN                                    
054000     MOVE 'LARM'     TO POSTSUM-TRANSTYP                                  
054100     CALL POSTSUM USING POSTSUM-PARM                                      
054200     .                                                                    
054300     EJECT                                                                
054400 S12-SKRIV-BORT-SKAL-TR SECTION.                                          
054500                                                                          
054600     MOVE UT05-UTFIL-TYP TO UT07-UTFIL-TYP                                
054700     MOVE UT05-IDARTNR   TO UT07-IDARTNR                                  
054800     MOVE UT05-IDLEVNR   TO UT07-IDLEVNR                                  
054900                                                                          
055000     WRITE W12207-POST FROM UT07-AREA                                     
055100                                                                          
055200     MOVE 'W12204D2'         TO POSTSUM-DDNAMN2                           
055300     MOVE 'W12207  '         TO POSTSUM-FDNAMN                            
055400     MOVE 'BOSK'             TO POSTSUM-TRANSTYP                          
055500     CALL POSTSUM USING POSTSUM-PARM                                      
055600     .                                                                    
055700     EJECT                                                                
055800 S13-SKRIV-REDAN-SKALAD-FIL SECTION.                                      
055900                                                                          
056000     WRITE W12203-POST FROM UT03-AREA                                     
056100                                                                          
056200     MOVE 'W12204D3' TO POSTSUM-DDNAMN2                                   
056300     MOVE 'W12205  ' TO POSTSUM-FDNAMN                                    
056400     MOVE 'REDAN'    TO POSTSUM-TRANSTYP                                  
056500     CALL POSTSUM USING POSTSUM-PARM                                      
056600     .                                                                    
056700     EJECT                                                                
056800 Z-FINIT SECTION.                                                         
056900                                                                          
057000     CLOSE W12205                                                         
057100           W12207                                                         
057200           W12203                                                         
057300     MOVE 'S'                TO POSTSUM-OPKOD                             
057400     CALL POSTSUM USING POSTSUM-PARM                                      
057500     .                                                                    
057600     EJECT                                                                
057700*- - - - - - - - - - - - - - - - - IMS SEKTION                            
057800 IMS-GET-WDK6 SECTION.                                                    
057900                                                                          
058000     MOVE '  GAGKGB' TO GODK-STATUSKODER                                  
058100     CALL CBLTDLI USING GN WDK6-PCB DLI-IO-AREA                           
058200     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
058300     PERFORM IMS-STATUSKONTROLL                                           
058400     .                                                                    
058500     SKIP3                                                                
058600 IMS-GU-WLARTM01 SECTION.                                                 
058700                                                                          
058800     STRING 'WLARTM01(IDARTNR  =' W-IDARTNR-X ')'                         
058900     DELIMITED BY SIZE INTO SSA1                                          
059000     MOVE '  GE' TO GODK-STATUSKODER                                      
059100     CALL CBLTDLI USING GU ARTM-PCB DLI-IO-AREA3 SSA1                     
059200     MOVE ARTM-STATUS-CODE TO STATUS-WS                                   
059300     PERFORM IMS-STATUSKONTROLL                                           
059400     .                                                                    
059500     EJECT                                                                
059600 IMS-GET-WLINLB01 SECTION.                                                
059700                                                                          
059800     STRING 'WLINLB01(WDD901KY =' W-WDD901KY-X ')'                        
059900     DELIMITED BY SIZE INTO SSA1                                          
060000     MOVE '  GE' TO GODK-STATUSKODER                                      
060100     CALL CBLTDLI USING GU INLB-PCB DLI-IO-AREA3 SSA1                     
060200     MOVE INLB-STATUS-CODE TO STATUS-WS                                   
060300     PERFORM IMS-STATUSKONTROLL                                           
060400     .                                                                    
060500     SKIP3                                                                
060600 IMS-GET-WLINLB11 SECTION.                                                
060700                                                                          
060800     MOVE 'WLINLB11 ' TO SSA1                                             
060900     MOVE '  GE' TO GODK-STATUSKODER                                      
061000     CALL CBLTDLI USING GNP INLB-PCB DLI-IO-AREA3 SSA1                    
061100     MOVE INLB-STATUS-CODE TO STATUS-WS                                   
061200     PERFORM IMS-STATUSKONTROLL                                           
061300     .                                                                    
061400     EJECT                                                                
061500 IMS-GET-ARTS01 SECTION.                                                  
061600                                                                          
061700     STRING 'WLARTS01(IDARTNR  =' W-IDARTNR-X ')'                         
061800     DELIMITED BY SIZE INTO SSA1                                          
061900     MOVE '  GE' TO GODK-STATUSKODER                                      
062000     CALL CBLTDLI USING GU ARTS-PCB DLI-IO-AREA4 SSA1                     
062100     MOVE ARTS-STATUS-CODE TO STATUS-WS                                   
062200     PERFORM IMS-STATUSKONTROLL                                           
062300     .                                                                    
062400     SKIP3                                                                
062500 IMS-GET-ARTS11 SECTION.                                                  
062600                                                                          
062700     MOVE 'WLARTS11 ' TO SSA1                                             
062800     MOVE '  GE' TO GODK-STATUSKODER                                      
062900     CALL CBLTDLI USING GNP ARTS-PCB DLI-IO-AREA4 SSA1                    
063000     MOVE ARTS-STATUS-CODE TO STATUS-WS                                   
063100     PERFORM IMS-STATUSKONTROLL                                           
063200     .                                                                    
063300     SKIP3                                                                
063400 IMS-STATUSKONTROLL SECTION.                                              
063500     SET STATUS-IX TO 1                                                   
063600     SEARCH GODK-STATUS                                                   
063700       AT END                                                             
063800         CALL FELLOG                                                      
063900     WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                             
064000       CONTINUE                                                           
064100     END-SEARCH                                                           
064200     .                                                                    
