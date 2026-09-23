000100 ID DIVISION.                                                             
000200 PROGRAM-ID.             W1145100.                                        
000300 AUTHOR.                 GÖRAN KJELLSON   GUIDE                           
000400     DATE-WRITTEN.       FEBRUARI 2004                                    
000500*                                                                         
000600     REMARKS.                                                             
000700*                                                                         
000800*    FUNKTION:                                                            
000900*                   IMS BMP/BATCH HUVUDPROGRAM.                           
001000*                                                                         
001100*         KONTROLL AV NYA ARTIKLAR UTAN FP-INSTRUKTION                    
001200*                                                                         
001300*         PROGRAMMET STYRS AV HÄNDELSETRANSAR (WDGX1142 TYP '4')          
001400*                                                                         
001500*         OM ARTIKELN FÅTT FÖRPACKNINGSKOD (CLAG-KDEMBKOD-2)              
001600*         ÄNDRAD TILL 20, 25, 50 ELLER 80                                 
001700*         SKALL HTR-TYPEN ÄNDRAS TILL '1'                                 
001800*                                                                         
001900*         PROGRAMMET UPPDATERAR  WDG2                                     
002000*                                NYA ARTIKLAR UTAN FP-INSTRUKTION         
002100*                                WDD2                                     
002200*                                NYA ARTIKLAR FRÅN PV OCH LV              
002300*                    LÄSER       WDK6                                     
002400*                                ARTIKELINFORMATION                       
002500                                                                          
002600 ENVIRONMENT DIVISION.                                                    
002700 DATA DIVISION.                                                           
002800 WORKING-STORAGE SECTION.                                                 
002900                                                                          
003000*    ---- GENERELLA KONSTANTER                                            
003100                                                                          
003200 77  JA                          PIC X       VALUE 'J'.                   
003300 77  NEJ                         PIC X       VALUE 'N'.                   
003400 77  CURR-SECTION                PIC X(16)   VALUE 'MAIN'.                
003500 77  CURR-IMS-SECTION            PIC X(16)   VALUE SPACE.                 
003600                                                                          
003700*    ---- ARBETSFÄLT                                                      
003800                                                                          
003900 01  DAGENS-DATUM.                                                        
004000     03  AAMMDD                  PIC 9(6)    VALUE ZERO.                  
004100                                                                          
004200     03  AAVV                    PIC 9(4)    VALUE ZERO.                  
004300     03  FILLER    REDEFINES AAVV.                                        
004400         05  AA                  PIC 9(2).                                
004500         05  VV                  PIC 9(2).                                
004600                                                                          
004700*    ---- SUBPROGRAM OCH PARAMETERAREOR                                   
004800                                                                          
004900 01  DYNAMISKA-SUBPROGRAM.                                                
005000   03  POSTSUM                   PIC X(8)    VALUE 'POSTSUM '.            
005100   03  CBLTDLI                   PIC X(8)    VALUE 'CBLTDLI '.            
005200   03  FELLOG                    PIC X(8)    VALUE 'FELLOG  '.            
005300   03  WDATKONV                  PIC X(8)    VALUE 'WDATKONV'.            
005400                                                                          
005500*    ---- PARAMETRAR TILL WDATKONV                                        
005600*01  -COPY WDATAREA.                                                      
005700                                                                          
005800*    ---- PARAMETRAR TILL POSTSUM                                         
005900*01  -COPY W0005       -PRE POSTSUM-.                                     
006000                                                                          
006100*    ---- ARBETS-AREOR FÖR IMS-SEKTIONERNA                                
006200*                                                                         
006300 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
006400*    ---- STATUSKOD FRÅN IMS                                              
006500                                                                          
006600 01  STATUS-WS                   PIC XX.                                  
006700     88  SEGMENT-FINNS                       VALUE '  '.                  
006800     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
006900                                                                          
007000 01  GODK-STATUSKODER.                                                    
007100   03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                  
007200                                                                          
007300 01  SSA1                        PIC X(64).                               
007400 01  SSA2                        PIC X(64).                               
007500                                                                          
007600*    ---- NYCKLAR OCH SÖKFÄLT TILL DLI                                    
007700 01  NYCKLAR-TILL-DLI.                                                    
007800                                                                          
007900   03  W-IDARTNR-X.                                                       
008000     05  W-IDARTNR               PIC S9(9)                COMP-3.         
008100                                                                          
008200   03  W-KDSEGKEY-X.                                                      
008300     05  W-KDSEGKEY              PIC X(1)    VALUE '1'.                   
008400                                                                          
008500   03  W-1141KEY-X.                                                       
008600     05  FILLER                  PIC X(04)  VALUE '1141'.                 
008700     05  FILLER                  PIC X(26)  VALUE LOW-VALUE.              
008800                                                                          
008900   03  W-1142KEY-X.                                                       
009000     05  FILLER                  PIC X(01)  VALUE '4'.                    
009100                                                                          
009200                                                                          
009300*01  -COPY W0003                                                          
009400                                                                          
009500 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDG2  '.                      
009600 01  DLI-IO-WDG2.                                                         
009700*    03  WDG2 -COPY WDGX1142                                              
009800                                                                          
009900 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK611'.                      
010000 01  DLI-IO-WDK611.                                                       
010100*    03  -COPY WDK611                                                     
010200                                                                          
010300 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDD201'.                      
010400 01  DLI-IO-WDD201.                                                       
010500*    03  -COPY WDD201  -PRE D2-                                           
010600                                                                          
010700                                                                          
010800 LINKAGE SECTION.                                                         
010900*01      -COPY W0009     -PRE MSG-                                        
011000*01      -COPY W0008     -PRE WDG2-                                       
011100      05 FILLER          PIC X.                                           
011200*01      -COPY W0008     -PRE WDK6-                                       
011300      05 FILLER          PIC X.                                           
011400*01      -COPY W0008     -PRE WDD2-                                       
011500      05 FILLER          PIC X.                                           
011600                                                                          
011700                                                                          
011800 PROCEDURE  DIVISION USING MSG-PCB WDG2-PCB WDK6-PCB WDD2-PCB.            
011900                                                                          
012000     ENTRY 'DLITCBL' USING MSG-PCB WDG2-PCB WDK6-PCB WDD2-PCB.            
012100                                                                          
012200     PERFORM A-INIT                                                       
012300     PERFORM IMS-01-GU-WDG201                                             
012400     PERFORM IMS-02-GHNP-WDG202                                           
012500                                                                          
012600     PERFORM UNTIL SEGMENT-SAKNAS                                         
012700        MOVE 'TOT'   TO POSTSUM-TRANSTYP                                  
012800        CALL POSTSUM USING POSTSUM-PARM                                   
012900        MOVE 1142-IDARTNR TO W-IDARTNR                                    
013000        PERFORM IMS-04-GU-WDK611                                          
013100                                                                          
013200        IF SEGMENT-FINNS                                                  
013300           IF (CLAG-KDEMBKOD-2 = 20 OR 25 OR 50 OR 80                     
013400              OR 30 OR 35 OR 40 OR 45)                                    
013500           OR (CLAG-BEFT = 93 OR 95 OR 98 OR 99)                          
013600              MOVE 'REPL'  TO POSTSUM-TRANSTYP                            
013700              CALL POSTSUM USING POSTSUM-PARM                             
013800              MOVE '1' TO 1142-KDSEGKEY                                   
013900              PERFORM IMS-03-REPL-WDG202                                  
014000              PERFORM IMS-05-GHU-WDD201                                   
014100              MOVE 2   TO D2-ART-KDANSKQ                                  
014200              MOVE AAMMDD TO D2-ART-TIINKOP                               
014300              PERFORM IMS-06-REPL-WDD201                                  
014400           END-IF                                                         
014500        ELSE                                                              
014510           PERFORM IMS-07-DLET-WDG202                                     
014600        END-IF                                                            
014700                                                                          
014800        PERFORM IMS-02-GHNP-WDG202                                        
014900     END-PERFORM                                                          
015000                                                                          
015100     PERFORM Z-FINIT                                                      
015200     MOVE ZERO TO RETURN-CODE                                             
015300     GOBACK                                                               
015400     .                                                                    
015500                                                                          
015600 A-INIT SECTION.                                                          
015700     MOVE 'A-INIT' TO CURR-SECTION                                        
015800                                                                          
015900     MOVE 'IDAG'  TO DAT-KDDATFORM                                        
016000     MOVE  ZERO   TO DAT-I-TIDATUM                                        
016100     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
016200                         DAT-O-TIDATUM DAT-KDSVAR                         
016300     IF DAT-KDSVAR-OK                                                     
016400       MOVE DAT-TIAAMMDD    TO  AAMMDD                                    
016500       MOVE DAT-TIAA-VECKA  TO  AA                                        
016600       MOVE DAT-TIVV        TO  VV                                        
016700     END-IF                                                               
016800                                                                          
016900     MOVE 'W11451'   TO  POSTSUM-PROGNAMN                                 
017000                         POSTSUM-FDNAMN                                   
017100     MOVE 'HTR 1142' TO  POSTSUM-DDNAMN2                                  
017200     .                                                                    
017300                                                                          
017400 Z-FINIT   SECTION.                                                       
017500     MOVE 'Z-FINIT' TO CURR-SECTION                                       
017600                                                                          
017700     MOVE 'S' TO POSTSUM-OPKOD                                            
017800     CALL POSTSUM USING POSTSUM-PARM                                      
017900     .                                                                    
018000                                                                          
018100 IMS-01-GU-WDG201     SECTION.                                            
018200     MOVE 'IMS-01'  TO CURR-IMS-SECTION                                   
018300                                                                          
018400     STRING 'WDG201  (WDGXKEY  =' W-1141KEY-X ')'                         
018500             DELIMITED BY SIZE INTO SSA1                                  
018600                                                                          
018700     MOVE '  GE'           TO GODK-STATUSKODER                            
018800     CALL CBLTDLI USING GU WDG2-PCB DLI-IO-WDG2 SSA1                      
018900     MOVE WDG2-STATUS-CODE TO STATUS-WS                                   
019000     PERFORM IMS-STATUSKONTROLL                                           
019100     .                                                                    
019200                                                                          
019300 IMS-02-GHNP-WDG202     SECTION.                                          
019400     MOVE 'IMS-02'  TO CURR-IMS-SECTION                                   
019500                                                                          
019600     STRING 'WDG202  (KDSEGKEY =' W-1142KEY-X ')'                         
019700             DELIMITED BY SIZE INTO SSA1                                  
019800                                                                          
019900     MOVE '  GE'        TO GODK-STATUSKODER                               
020000     CALL CBLTDLI USING GHNP WDG2-PCB DLI-IO-WDG2 SSA1                    
020100     MOVE WDG2-STATUS-CODE TO STATUS-WS                                   
020200     PERFORM IMS-STATUSKONTROLL                                           
020300     .                                                                    
020400                                                                          
020500 IMS-03-REPL-WDG202           SECTION.                                    
020600     MOVE 'IMS-03'  TO CURR-IMS-SECTION                                   
020700                                                                          
020800     MOVE '  ' TO GODK-STATUSKODER                                        
020900     CALL CBLTDLI USING REPL WDG2-PCB DLI-IO-WDG2                         
021000     MOVE WDG2-STATUS-CODE TO STATUS-WS                                   
021100     PERFORM IMS-STATUSKONTROLL                                           
021200     .                                                                    
021300                                                                          
021310 IMS-07-DLET-WDG202           SECTION.                                    
021320     MOVE 'IMS-07'  TO CURR-IMS-SECTION                                   
021330                                                                          
021340     MOVE '  ' TO GODK-STATUSKODER                                        
021350     CALL CBLTDLI USING DLET WDG2-PCB DLI-IO-WDG2                         
021360     MOVE WDG2-STATUS-CODE TO STATUS-WS                                   
021370     PERFORM IMS-STATUSKONTROLL                                           
021380     .                                                                    
021390                                                                          
021400 IMS-04-GU-WDK611      SECTION.                                           
021500     MOVE 'IMS-04'  TO CURR-IMS-SECTION                                   
021600                                                                          
021700     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
021800          DELIMITED BY SIZE INTO SSA1                                     
021900     STRING 'WDK611  (KDSEGKEY =' W-KDSEGKEY-X ')'                        
022000          DELIMITED BY SIZE INTO SSA2                                     
022100     MOVE '  GE'             TO GODK-STATUSKODER                          
022200     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK611 SSA1 SSA2               
022300     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
022400     PERFORM IMS-STATUSKONTROLL                                           
022500     .                                                                    
022600 IMS-05-GHU-WDD201      SECTION.                                          
022700     MOVE 'IMS-05'  TO CURR-IMS-SECTION                                   
022800                                                                          
022900     STRING 'WDD201  (IDARTNR  =' W-IDARTNR-X ')'                         
023000          DELIMITED BY SIZE INTO SSA1                                     
023100     MOVE '  '             TO GODK-STATUSKODER                            
023200     CALL CBLTDLI USING GHU WDD2-PCB DLI-IO-WDD201 SSA1                   
023300     MOVE WDD2-STATUS-CODE TO STATUS-WS                                   
023400     PERFORM IMS-STATUSKONTROLL                                           
023500     .                                                                    
023600 IMS-06-REPL-WDD201      SECTION.                                         
023700     MOVE 'IMS-06'  TO CURR-IMS-SECTION                                   
023800                                                                          
023900     MOVE '  '             TO GODK-STATUSKODER                            
024000     CALL CBLTDLI USING REPL WDD2-PCB DLI-IO-WDD201                       
024100     MOVE WDD2-STATUS-CODE TO STATUS-WS                                   
024200     PERFORM IMS-STATUSKONTROLL                                           
024300     .                                                                    
024400 IMS-STATUSKONTROLL SECTION.                                              
024500     SET STATUS-IX TO 1                                                   
024600     SEARCH GODK-STATUS                                                   
024700       AT END CALL FELLOG                                                 
024800       WHEN GODK-STATUS(STATUS-IX) = STATUS-WS CONTINUE                   
024900     END-SEARCH                                                           
025000     .                                                                    
