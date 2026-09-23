000100*COMPOPT STDSUB=YES                                                       
000200 ID DIVISION.                                                             
000300 PROGRAM-ID.     W200CDCU.                                                
000400 AUTHOR.         JOHAN NIHLBLAD.                                          
000500 DATE-WRITTEN.   JUNE 2023.                                               
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*                                                                         
000900*    FUNCTION:                                                            
001000*        SUBPROGRAM TO UPDATE FOR CDC:                                    
001100*                                                                         
001200*        PROGRAM READS     WDK6                                           
001300*        PROGRAM UPDATES   WDK6                                           
001400*                                                                         
001500                                                                          
001600     SKIP3                                                                
001700 ENVIRONMENT DIVISION.                                                    
001800     SKIP2                                                                
001900 INPUT-OUTPUT SECTION.                                                    
002000                                                                          
002100 FILE-CONTROL.                                                            
002200     SKIP2                                                                
002300 DATA DIVISION.                                                           
002400     SKIP3                                                                
002500 FILE SECTION.                                                            
002600     SKIP3                                                                
002700     EJECT                                                                
002800 WORKING-STORAGE SECTION.                                                 
002900                                                                          
003000 77  IDPGM                       PIC X(8)    VALUE 'W200CDCU'.            
003100 77  CURRENT-SECTION             PIC X(16)   VALUE SPACE.                 
003200 77  CURR-IMS-SECTION            PIC X(16)   VALUE SPACE.                 
003300 77  JA                          PIC X       VALUE 'J'.                   
003400 77  NEJ                         PIC X       VALUE 'N'.                   
003500 77  SW-IDANSK-UPPD              PIC X(1)    VALUE SPACE.                 
003600 77  WS-SPAR-IDANSK              PIC 9(3)    VALUE ZERO.                  
003700     EJECT                                                                
003800                                                                          
003900                                                                          
004000 01  WORKING-FIELDS.                                                      
004100*                                                                         
004200     03  INDX                    PIC  9(3)   VALUE ZERO.                  
004300                                                                          
004400     03  WS-IDPLANGR-AG          PIC 9(1) VALUE ZERO.                     
004500     03 WS-IDANSK                PIC 9(3) VALUE ZERO.                     
004600     03 WS-IDANSK-GRP            REDEFINES WS-IDANSK.                     
004700        05 WS-IDANSK-12          PIC 9(2).                                
004800        05 WS-IDANSK-3           PIC 9(1).                                
004900                                                                          
005000                                                                          
005100 01  TODAYS-DATE                 PIC 9(8)    VALUE ZERO.                  
005200*                                                                         
005300*                                                                         
005400 01  GENERAL-SUBPROGRAMS.                                                 
005500*                                                                         
005600     03  CBLTDLI                 PIC X(8)   VALUE 'CBLTDLI '.             
005700     03  ABEND                   PIC X(8)   VALUE 'ABEND'.                
005800     03  FELLOG                  PIC X(8)   VALUE 'FELLOG  '.             
005900     03  POSTSUM                 PIC X(8)   VALUE 'POSTSUM'.              
006000     03  WDATKONV                PIC X(8)   VALUE 'WDATKONV'.             
006100     SKIP2                                                                
006200***************************************************************           
006300*       C O P Y T E X T E R    (DYNAMISKA ANROP)                          
006400***************************************************************           
006500 01  FILLER                      PIC X(16) VALUE 'WDATAREA     '.         
006600*01   -COPY WDATAREA.                                                     
006700                                                                          
006800                                                                          
006900*    --- PARAMETERS TO ABEND                                              
007000                                                                          
007100 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
007200 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
007300 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
007400*                                                                         
007500*SWITCHES                                                                 
007600                                                                          
007700 77  INPUT-DATA-SW               PIC X(01)   VALUE 'J'.                   
007800     88  INPUT-DATA-OK                       VALUE 'J'.                   
007900     88  INPUT-DATA-FEL                      VALUE 'N'.                   
008000*                                                                         
008100                                                                          
008200*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
008300*                                                                         
008400 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
008500 01  KEYS-TO-DLI.                                                         
008600     03  W-IDARTNR-X.                                                     
008700         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
008800                                                                          
008900     03  W-IDLEVNR-X.                                                     
009000         05  W-IDLEVNR           PIC X(5)    VALUE SPACE.                 
009100                                                                          
009200*    --- IMS FUNCTION CODES                                               
009300*01  -COPY W0003                                                          
009400                                                                          
009500                                                                          
009600*    ---  DLI INPUT-OUTPUT AREA                                           
009700                                                                          
009800 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK601'.                      
009900 01  DLI-IO-WDK601.                                                       
010000*    03  -COPY WDK601                                                     
010100                                                                          
010200 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK611'.                      
010300 01  DLI-IO-WDK611.                                                       
010400*    03  -COPY WDK611                                                     
010500                                                                          
010600                                                                          
010700*    --- STATUS-CODE FROM IMS                                             
010800 01  STATUS-WS                   PIC XX.                                  
010900     88  SEGMENT-FOUND                       VALUE '  '.                  
011000     88  SEGMENT-FOUND-EXISTS                VALUE 'II'.                  
011100     88  SEGMENT-MISSING                     VALUE 'GE'.                  
011200     88  END-OF-BASE                         VALUE 'GB'.                  
011300     SKIP2                                                                
011400 01  GOOD-STATUSCODES.                                                    
011500     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
011600                                                                          
011700 01  ALL-SSA.                                                             
011800    03 SSA1                     PIC X(64).                                
011900    03 SSA2                     PIC X(64).                                
012000                                                                          
012100                                                                          
012200 LINKAGE SECTION.                                                         
012300*    -COPY W200CDCU                                                       
012400                                                                          
012500     EJECT                                                                
012600*01  -COPY W0008      -PRE WDK6-                                          
012700     05  FILLER                  PIC X.                                   
012800                                                                          
012900                                                                          
013000 PROCEDURE DIVISION  USING CDCU-W200CDCU WDK6-PCB.                        
013100                                                                          
013200     PERFORM A-INIT                                                       
013300     PERFORM B-VALIDATE-INPUT                                             
013400     IF INPUT-DATA-OK                                                     
013500      PERFORM C-GET-WDK6                                                  
013600      IF SW-IDANSK-UPPD = JA                                              
013700        PERFORM H-UPDATE                                                  
013800      END-IF                                                              
013900     END-IF                                                               
014000                                                                          
014100     GOBACK                                                               
014200     .                                                                    
014300     EJECT                                                                
014400 A-INIT SECTION.                                                          
014500     MOVE 'A-INIT         ' TO CURRENT-SECTION                            
014600                                                                          
014700                                                                          
014800*    INITIALIZE CDCU-UTDATA                                               
014900     MOVE JA                         TO INPUT-DATA-SW                     
015000                                                                          
015100     MOVE SPACES                     TO CDCU-KDSVAR                       
015200                                        CDCU-FEL-TEXT                     
015300                                        CDCU-IDMSG-ERROR                  
015400                                        CDCU-IDELMT-ERROR                 
015500                                                                          
015600     .                                                                    
015700     EJECT                                                                
015800                                                                          
015900 B-VALIDATE-INPUT SECTION.                                                
016000     MOVE 'B-VALIDATE-    ' TO CURRENT-SECTION                            
016100                                                                          
016200     IF  CDCU-IDARTNR     IS NUMERIC                                      
016300     AND CDCU-IDARTNR       > ZERO                                        
016400         MOVE CDCU-IDARTNR       TO W-IDARTNR                             
016500     ELSE                                                                 
016600         MOVE NEJ                TO INPUT-DATA-SW                         
016700*                                                                         
016800         SET  CDCU-KDSVAR-FEL     TO TRUE                                 
016900         MOVE '022'               TO CDCU-IDMSG-ERROR                     
017000         MOVE 'IDARTNR'           TO CDCU-IDELMT-ERROR                    
017100         MOVE 'INVALID PART     ' TO CDCU-FEL-TEXT                        
017200*                                                                         
017300     END-IF                                                               
017400     .                                                                    
017500 C-GET-WDK6              SECTION.                                         
017600     MOVE 'C-GET-WDK6    ' TO CURRENT-SECTION                             
017700                                                                          
017800     PERFORM IMS-GU-WDK601                                                
017900     IF SEGMENT-FOUND                                                     
018000       PERFORM IMS-GHNP-WDK611                                            
018100       IF  SEGMENT-FOUND                                                  
018200        PERFORM CA-CHECK-UPDATE                                           
018300       ELSE                                                               
018400        SET  CDCU-KDSVAR-FEL     TO TRUE                                  
018500        MOVE '025'               TO CDCU-IDMSG-ERROR                      
018600        MOVE 'IDARTNR'           TO CDCU-IDELMT-ERROR                     
018700        MOVE 'MISSING IN WDK611' TO CDCU-FEL-TEXT                         
018800       END-IF                                                             
018900     ELSE                                                                 
019000        SET  CDCU-KDSVAR-FEL     TO TRUE                                  
019100        MOVE '025'               TO CDCU-IDMSG-ERROR                      
019200        MOVE 'IDARTNR'           TO CDCU-IDELMT-ERROR                     
019300        MOVE 'MISSING IN WDK601' TO CDCU-FEL-TEXT                         
019400     END-IF                                                               
019500     .                                                                    
019600                                                                          
019700 CA-CHECK-UPDATE SECTION.                                                 
019800                                                                          
019900     MOVE NEJ TO SW-IDANSK-UPPD                                           
020000     MOVE CLAG-IDANSK             TO WS-SPAR-IDANSK                       
020100                                     WS-IDANSK                            
020200     MOVE +7                      TO WS-IDANSK-3                          
020300     MOVE WS-IDANSK               TO CLAG-IDANSK                          
020400     MOVE JA                      TO SW-IDANSK-UPPD                       
020500     IF WS-SPAR-IDANSK = 701 OR 702 OR 703                                
020600        MOVE +9                       TO CLAG-IDPLANGR-AG                 
020700     END-IF                                                               
020800     .                                                                    
020900                                                                          
021000 H-UPDATE SECTION.                                                        
021100                                                                          
021200     PERFORM IMS-REPL-WDK611                                              
021300     .                                                                    
021400                                                                          
021500 S01-DATE-CONVER-TO-AAVVD  SECTION.                                       
021600     MOVE 'S01-DATE-CONVER-TO-AAVVD' TO CURRENT-SECTION                   
021700                                                                          
021800     MOVE 'AAMMDD'          TO DAT-KDDATFORM                              
021900     CALL WDATKONV USING DAT-KDDATFORM,                                   
022000                         DAT-I-TIDATUM,                                   
022100                         DAT-O-TIDATUM,                                   
022200                         DAT-KDSVAR                                       
022300     .                                                                    
022400     EJECT                                                                
022500* ---                                                                     
022600* --- IMS SECTIONS  ---                                                   
022700* ---                                                                     
022800                                                                          
022900                                                                          
023000 IMS-GU-WDK601  SECTION.                                                  
023100     MOVE 'IMS-GU-WDK601 ' TO CURR-IMS-SECTION                            
023200                                                                          
023300     STRING 'WDK601  (IDARTNR  =' W-IDARTNR-X ')'                         
023400          DELIMITED BY SIZE INTO SSA1                                     
023500     MOVE '  GE' TO GOOD-STATUSCODES                                      
023600     CALL CBLTDLI USING GU WDK6-PCB DLI-IO-WDK601 SSA1                    
023700     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
023800     PERFORM IMS-STATUSCHECK                                              
023900     .                                                                    
024000     SKIP3                                                                
024100 IMS-GHNP-WDK611 SECTION.                                                 
024200     MOVE 'IMS-GNP-WDK611' TO CURR-IMS-SECTION                            
024300                                                                          
024400     MOVE 'WDK611    '      TO SSA1                                       
024500     MOVE '  GEGB' TO GOOD-STATUSCODES                                    
024600     CALL CBLTDLI USING GHNP WDK6-PCB DLI-IO-WDK611 SSA1                  
024700     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
024800     PERFORM IMS-STATUSCHECK                                              
024900     .                                                                    
025000     EJECT                                                                
025100 IMS-REPL-WDK611 SECTION.                                                 
025200     MOVE 'IMS-REPL-WDK611' TO CURR-IMS-SECTION                           
025300                                                                          
025400     MOVE '  ' TO GOOD-STATUSCODES                                        
025500     CALL CBLTDLI USING REPL WDK6-PCB DLI-IO-WDK611                       
025600     MOVE WDK6-STATUS-CODE TO STATUS-WS                                   
025700     PERFORM IMS-STATUSCHECK                                              
025800     .                                                                    
025900     EJECT                                                                
026000 IMS-STATUSCHECK SECTION.                                                 
026100                                                                          
026200     SET STATUS-IX TO 1                                                   
026300     SEARCH GOOD-STATUS                                                   
026400       AT END                                                             
026500         CALL FELLOG                                                      
026600       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
026700         CONTINUE                                                         
026800     END-SEARCH                                                           
026900     .                                                                    
