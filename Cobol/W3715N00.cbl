000010 PROCESS DYNAM                                                            
000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W3715N00.                                                
000300 AUTHOR.         STENING INGER.                                           
000400 DATE-WRITTEN.   20/10/05.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNCTION:                                                            
000800*        REPORT OF BINNED CORES AND DEVIATIONS FOR MAASTRICHT ORG.        
000900*                                                                         
001000*        THE PROGRAM READS     WDK7                                       
001100*                              BYART (DB2)                                
001200*                                                                         
001300*    ABENDCODES:                                                          
001400*        U0016 -  . . . .                                                 
001500*        U1000 -  . . . .                                                 
001600*                                                                         
001700                                                                          
001900 ENVIRONMENT DIVISION.                                                    
002000                                                                          
002100 INPUT-OUTPUT SECTION.                                                    
002200                                                                          
002300 FILE-CONTROL.                                                            
002500*          --- INPUT                                                      
002600     SELECT W3714X                     ASSIGN TO W3715ND1.                
002800*          --- OUTPUT                                                     
002900     SELECT W3715N                     ASSIGN TO W3715ND2.                
003000     EJECT                                                                
003100 DATA DIVISION.                                                           
003200                                                                          
003300 FILE SECTION.                                                            
003400                                                                          
003500 FD  W3714X                                                               
003600     RECORDING       F                                                    
003700     BLOCK CONTAINS  0.                                                   
003800                                                                          
003900*01  -COPY W3714X      -L.                                                
004000                                                                          
004100 FD  W3715N                                                               
004200     RECORDING       V                                                    
004300     BLOCK CONTAINS  0.                                                   
004400                                                                          
004500 01  OUT-RECORD    PIC X(999).                                            
004600     EJECT                                                                
004700 WORKING-STORAGE SECTION.                                                 
004800                                                                          
004900 77  IDPGM                       PIC X(8)    VALUE 'W3715N00'.            
005000 77  YES                         PIC X       VALUE 'J'.                   
005010 77  CURRENT-SECTION             PIC X(80)   VALUE SPACE.                 
005020 77  DBS-SECTION                 PIC X(80)   VALUE SPACE.                 
005030 77  CURRENT-DB-SEC              PIC X(80)   VALUE SPACE.                 
005100 77  NOO                         PIC X       VALUE 'N'.                   
005200                                                                          
005202 01  WS-IDARTNR-BYT              PIC S9(9) COMP-3 VALUE ZERO.             
005203 01  WS-IDARTNR                  PIC 9(9)    VALUE ZERO.                  
005204 01  FILLER REDEFINES WS-IDARTNR.                                         
005205   03  FILLER                    PIC 9(5).                                
005206   03  WS-ARTSIFFRA              PIC 9(1).                                
005207     88  ART-0                   VALUE 6.                                 
005208     88  ART-1                   VALUE 4  7.                              
005209     88  ART-2                   VALUE 5  8.                              
005210     88  ART-3                   VALUE 9.                                 
005211   03  FILLER                    PIC 9(3).                                
005212                                                                          
005300 77  W3714X-EOF-SW               PIC X       VALUE 'N'.                   
005400     88  END-OF-W3714X                       VALUE 'J'.                   
005500     EJECT                                                                
006200 01  TEST-IDARTNR                PIC 9(9)   COMP-3.                       
006300*01  FILLER -COPY WWBYT01   -RED TEST-IDARTNR                             
006400*01  FILLER -COPY WWBYT16   -RED TEST-IDARTNR                             
006500     EJECT                                                                
006600 01  GENERAL-SUBPROGRAMS.                                                 
006700*                                                                         
006800     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
006900     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
007000     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
007100     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
007200                                                                          
007300*    --- PARAMETERS FOR SUBPROGRAM ABEND                                  
007400                                                                          
007500 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
007600 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
007700 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
007800                                                                          
007900 01  ERROR-TEXT.                                                          
008000     03  FILLER                  PIC X(8)    VALUE 'ERR-TEXT'.            
008100     03  ERROR-TEXT-STR          PIC X(72)   VALUE SPACE.                 
008200     EJECT                                                                
008300*    --- PARAMETRAR TILL POSTSUM                                          
008400*                                                                         
008500*01  -COPY W0005   -PRE  POSTSUM-                                         
008600     EJECT                                                                
008700 01  WS-DAP-ID1.                                                          
008800     03  FILLER                  PIC X(133)  VALUE                        
008900                                 '¤DAPW3715N-091'.                        
008910 01  WS-DAP-ID2.                                                          
008920     03  FILLER                  PIC X(133)  VALUE                        
008930                                 '¤DAPW3715N    '.                        
009000 01  WS-HEAD.                                                             
009100     03 FILLER                   PIC X(09) VALUE 'PART NO'.               
009200     03 FILLER                   PIC X(01) VALUE ';'.                     
009210     03 FILLER                   PIC X(08) VALUE 'DISTRICT'.              
009220     03 FILLER                   PIC X(01) VALUE ';'.                     
009300     03 FILLER                   PIC X(08) VALUE 'CUSTOMER'.              
009400     03 FILLER                   PIC X(01) VALUE ';'.                     
009500     03 FILLER                   PIC X(09) VALUE 'REPORT NO'.             
009600     03 FILLER                   PIC X(01) VALUE ';'.                     
009710     03 FILLER                   PIC X(13) VALUE 'APPROVAL DATE'.         
009800     03 FILLER                   PIC X(01) VALUE ';'.                     
009900     03 FILLER                   PIC X(07) VALUE 'QTY ORG'.               
010000     03 FILLER                   PIC X(01) VALUE ';'.                     
010100     03 FILLER                   PIC X(08) VALUE 'QTY HAND'.              
010200     03 FILLER                   PIC X(01) VALUE ';'.                     
010300     03 FILLER                   PIC X(04) VALUE 'CODE'.                  
010400     03 FILLER                   PIC X(01) VALUE ';'.                     
010500     03 FILLER                   PIC X(03) VALUE 'SCR'.                   
010600     03 FILLER                   PIC X(01) VALUE ';'.                     
010700     03 FILLER                   PIC X(11) VALUE 'APPROVER ID'.           
010800     03 FILLER                   PIC X(01) VALUE ';'.                     
010900     03 FILLER                   PIC X(11) VALUE 'EU DISTRICT'.           
011000     03 FILLER                   PIC X(01) VALUE ';'.                     
011100     03 FILLER                   PIC X(09) VALUE 'MAX STOCK'.             
011200     03 FILLER                   PIC X(01) VALUE ';'.                     
011300     03 FILLER                   PIC X(07) VALUE 'STOCK'.                 
011400*                                                                         
011500 01  WS-LINE.                                                             
011600     03 ROW-IDARTNR-OBJ          PIC Z(08)9.                              
011700     03 FILLER                   PIC X(01) VALUE ';'.                     
011800     03 ROW-IDDISTR              PIC Z(04)9.                              
011900     03 FILLER                   PIC X(01) VALUE ';'.                     
012000     03 ROW-IDKUNDNR             PIC Z(06)9.                              
012100     03 FILLER                   PIC X(01) VALUE ';'.                     
012200     03 ROW-IDBYTRAP             PIC Z(06)9.                              
012300     03 FILLER                   PIC X(01) VALUE ';'.                     
012400     03 ROW-DAREGDAT-GODK        PIC 9(08).                               
012500     03 FILLER                   PIC X(01) VALUE ';'.                     
012600     03 ROW-KVRETUR-URSP         PIC Z(06)9.                              
012700     03 FILLER                   PIC X(01) VALUE ';'.                     
012800     03 ROW-KVRETUR-GODK         PIC Z(06)9.                              
012900     03 FILLER                   PIC X(01) VALUE ';'.                     
013000     03 ROW-KDBYTREF             PIC X(03).                               
013100     03 FILLER                   PIC X(01) VALUE ';'.                     
013110     03 ROW-FLSKROT              PIC X(01).                               
013300     03 FILLER                   PIC X(01) VALUE ';'.                     
013400     03 ROW-IDUSER               PIC X(08).                               
013500     03 FILLER                   PIC X(01) VALUE ';'.                     
013600     03 ROW-IDDISTR-RENOV        PIC Z(4)9.                               
013700     03 FILLER                   PIC X(01) VALUE ';'.                     
013800     03 ROW-KVLS-MAXCORE         PIC Z(06)9.                              
013900     03 FILLER                   PIC X(01) VALUE ';'.                     
014000     03 ROW-KVLS                 PIC Z(06)9.                              
014100*                                                                         
014200 01  IN-AREA-START               PIC X(24)   VALUE                        
014300                                 'IN-AREA-START  '.                       
014600*01  AREA -COPY W3714X     -PRE IN-                                       
014700     EJECT                                                                
015400*    --- AREAS FOR IMS-SECTIONS                                           
015500*                                                                         
015600     EJECT                                                                
015700 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
015900 01  KEYS-FOR-DLI.                                                        
016000     03  W-IDARTNR-X.                                                     
016100         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
016200     03  W-IDDC-X.                                                        
016210         05  W-IDDC              PIC X(02)   VALUE SPACE.                 
016300                                                                          
016400*    --- STATUS-KOD FRÅN IMS                                              
016500 01  STATUS-WS                   PIC XX.                                  
016600     88  SEGMENT-FOUND                       VALUE '  '.                  
016700     88  SEGMENT-FOUND-EXISTS                VALUE 'II'.                  
016800     88  SEGMENT-MISSING                     VALUE 'GE'.                  
016900                                                                          
017000 01  GOOD-STATUSCODES.                                                    
017100     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
017200                                                                          
017300 01  SSA1                        PIC X(64).                               
017400 01  SSA2                        PIC X(64).                               
017500     EJECT                                                                
017600*    --- WORK-AREAS FOR DB2-SECTIONS                                      
017700*                                                                         
017800 01  FILLER                      PIC X(16)   VALUE 'SQLCA-AREA'.          
017900       EXEC SQL INCLUDE SQLCA END-EXEC.                                   
018100*                                                                         
018200 01  FILLER                      PIC X(16)   VALUE 'SQLCODE-WS'.          
018300 01  DB2-WS.                                                              
018400     03  SQLCODE-WS              PIC  9(3)   VALUE ZERO.                  
018500         88  ROW-FOUND                       VALUE  000.                  
018600         88  ROW-NOTFOUND                    VALUE  100.                  
018700         88  RESOURCE-UNAVAILABLE            VALUE  904.                  
018800     03  GOOD-SQLCODECODES.                                               
018900         05  GOOD-SQLCODE OCCURS 5                                        
019000           INDEXED BY SQLCODE-IX PIC  9(3).                               
019100*                                                                         
019200     EJECT                                                                
019300*    --- DB2 HOST-COPYTEXT                                                
019400 01  FILLER                      PIC X(16)   VALUE 'DB2-WS'.              
019500*01  -COPY BYART -PRE BYART-                                              
019600     EJECT                                                                
019800*    --- DB2 DCL                                                          
019900 01  FILLER                  PIC X(16) VALUE 'BYART-AREA'.                
020000       EXEC SQL INCLUDE BYART END-EXEC.                                   
020100*                                                                         
020200*    --- IMS FUNCTION CODES                                               
020300*01  -COPY W0003                                                          
020400     EJECT                                                                
020500*    ---  DLI INPUT-OUTPUT AREA                                           
020600 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK711'.                      
020700 01  DLI-IO-WDK711.                                                       
020800*    03  -COPY WDK711                                                     
020900     EJECT                                                                
021000 LINKAGE SECTION.                                                         
021100                                                                          
021200                                                                          
021300*01  -COPY W0008  -PRE WDK7-                                              
021400     05  FILLER                  PIC X.                                   
021500     EJECT                                                                
021600 PROCEDURE DIVISION  USING WDK7-PCB.                                      
021700 MAIN SECTION.                                                            
021800     ENTRY 'DLITCBL' USING WDK7-PCB.                                      
021900                                                                          
022000                                                                          
022100     PERFORM A-INIT                                                       
022200                                                                          
022300     PERFORM S01-READ-W3714X                                              
022400     IF NOT END-OF-W3714X                                                 
022401        MOVE WS-DAP-ID1  TO OUT-RECORD                                    
022402        PERFORM S11-WRITE-W3715N                                          
022403                                                                          
022410        MOVE WS-DAP-ID2  TO OUT-RECORD                                    
022500        PERFORM S11-WRITE-W3715N                                          
022510                                                                          
022520        MOVE WS-HEAD     TO OUT-RECORD                                    
022530        PERFORM S11-WRITE-W3715N                                          
022600     END-IF                                                               
022700                                                                          
022800     PERFORM UNTIL END-OF-W3714X                                          
022900                                                                          
023000       PERFORM C-CREATE-OUTPUT-W3715N                                     
023100                                                                          
023200       PERFORM S01-READ-W3714X                                            
023300     END-PERFORM                                                          
023400                                                                          
023500                                                                          
023600     PERFORM Z-FINIT                                                      
023700                                                                          
023800     MOVE ZERO TO RETURN-CODE                                             
023900     GOBACK                                                               
024000     .                                                                    
024100     EJECT                                                                
024200 A-INIT SECTION.                                                          
024300                                                                          
024400     OPEN INPUT  W3714X                                                   
024500                                                                          
024600     OPEN OUTPUT W3715N                                                   
024700                                                                          
024900     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
025000     .                                                                    
025100     EJECT                                                                
025200 C-CREATE-OUTPUT-W3715N SECTION.                                          
025300                                                                          
025310     MOVE IN-IDARTNR-OBJ          TO ROW-IDARTNR-OBJ                      
025320     MOVE IN-IDDISTR              TO ROW-IDDISTR                          
025330     MOVE IN-IDKUNDNR             TO ROW-IDKUNDNR                         
025340     MOVE IN-IDBYTRAP             TO ROW-IDBYTRAP                         
025350     MOVE IN-DAREGDAT-GODK        TO ROW-DAREGDAT-GODK                    
025371     MOVE IN-KVRETUR-URSP         TO ROW-KVRETUR-URSP                     
025372     MOVE IN-KVRETUR-GODK         TO ROW-KVRETUR-GODK                     
025380     MOVE IN-KDBYTREF             TO ROW-KDBYTREF                         
025390     MOVE IN-FLSKROT              TO ROW-FLSKROT                          
025400     MOVE IN-IDUSER               TO ROW-IDUSER                           
026200                                                                          
026210     MOVE IN-IDARTNR-OBJ          TO WS-IDARTNR                           
026211                                     TEST-IDARTNR                         
026300     IF BYT01-BYTES                                                       
026400       IF BYT16-RADIO                                                     
026500         MOVE 3                   TO WS-ARTSIFFRA                         
026600       ELSE                                                               
026700         IF ART-0                                                         
026800           MOVE 0                 TO WS-ARTSIFFRA                         
026900         ELSE                                                             
027000           IF ART-1                                                       
027100             MOVE 1               TO WS-ARTSIFFRA                         
027200           ELSE                                                           
027300             IF ART-2                                                     
027400               MOVE 2             TO WS-ARTSIFFRA                         
027500             ELSE                                                         
027600               IF ART-3                                                   
027700                 MOVE 3           TO WS-ARTSIFFRA                         
027800               END-IF                                                     
027900             END-IF                                                       
028000           END-IF                                                         
028100         END-IF                                                           
028200       END-IF                                                             
028300     END-IF                                                               
028400                                                                          
028410                                                                          
028420     IF IN-IDARTNR-OBJ NOT = W-IDARTNR                                    
028500       MOVE WS-IDARTNR            TO WS-IDARTNR-BYT                       
028600       PERFORM DB2-SELECT-BYART                                           
028700       IF ROW-FOUND                                                       
028800         MOVE BYART-IDDISTR-RENOV TO ROW-IDDISTR-RENOV                    
028900         MOVE BYART-KVLS-MAXCORE  TO ROW-KVLS-MAXCORE                     
029000       ELSE                                                               
029100         MOVE ZERO                TO ROW-IDDISTR-RENOV                    
029200         MOVE ZERO                TO ROW-KVLS-MAXCORE                     
029300       END-IF                                                             
029400                                                                          
029500       MOVE IN-IDARTNR-OBJ        TO W-IDARTNR                            
029600       MOVE IN-IDDC               TO W-IDDC                               
029700       PERFORM IMS-GU-WDK711                                              
029800       IF SEGMENT-FOUND                                                   
029900          MOVE SLAG-KVLS          TO ROW-KVLS                             
030000       ELSE                                                               
030100          MOVE ZERO               TO ROW-KVLS                             
030200       END-IF                                                             
030210     END-IF                                                               
030300                                                                          
030520     MOVE WS-LINE                 TO OUT-RECORD                           
030600                                                                          
030700     PERFORM S11-WRITE-W3715N                                             
031300     .                                                                    
031400     EJECT                                                                
031500 Z-FINIT SECTION.                                                         
031600     CLOSE W3714X                                                         
031700           W3715N                                                         
031800                                                                          
031900     MOVE 'S' TO POSTSUM-OPKOD                                            
032000     CALL POSTSUM USING POSTSUM-PARM                                      
032100     .                                                                    
032200     EJECT                                                                
032300 S01-READ-W3714X SECTION.                                                 
032400     READ W3714X INTO IN-AREA                                             
032500     AT END                                                               
032600        MOVE HIGH-VALUE TO IN-AREA                                        
032700        SET END-OF-W3714X TO TRUE                                         
032800                                                                          
032900     NOT AT END                                                           
033000        MOVE 'W3714X'   TO POSTSUM-FDNAMN                                 
033100        MOVE 'W3715ND1' TO POSTSUM-DDNAMN2                                
033200        MOVE SPACE      TO POSTSUM-TRANSTYP                               
033300        CALL POSTSUM USING POSTSUM-PARM                                   
033400     END-READ                                                             
033500     .                                                                    
033600     EJECT                                                                
033700 S11-WRITE-W3715N SECTION.                                                
033800                                                                          
033900     WRITE OUT-RECORD                                                     
034000                                                                          
034100     MOVE SPACE      TO POSTSUM-TRANSTYP                                  
034200     MOVE 'W3715N'   TO POSTSUM-FDNAMN                                    
034300     MOVE 'W3715ND2' TO POSTSUM-DDNAMN2                                   
034400     CALL POSTSUM USING POSTSUM-PARM                                      
034500     .                                                                    
034600     EJECT                                                                
034700 S99-ABEND SECTION.                                                       
034800                                                                          
034900                                                                          
035000     MOVE 'S' TO POSTSUM-OPKOD                                            
035100     CALL POSTSUM USING POSTSUM-PARM                                      
035200     CALL ABEND USING RKOD-ABEND                                          
035300     .                                                                    
035400     EJECT                                                                
035500* --- IMS SECTIONS  ---                                                   
035600                                                                          
035700     EJECT                                                                
035800 IMS-GU-WDK711    SECTION.                                                
035900     MOVE 'IMS-GU-WDK711  '  TO DBS-SECTION                               
036000                                                                          
036100     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
036200          DELIMITED BY SIZE INTO SSA1                                     
036300     STRING 'WDK711  (IDDC     =' W-IDDC-X ')'                            
036400          DELIMITED BY SIZE INTO SSA2                                     
036500     MOVE '  GE'              TO GOOD-STATUSCODES                         
036600     CALL CBLTDLI USING GU WDK7-PCB DLI-IO-WDK711 SSA1 SSA2               
036700     MOVE WDK7-STATUS-CODE    TO STATUS-WS                                
036800     PERFORM IMS-STATUSCHECK                                              
036900     .                                                                    
037000     EJECT                                                                
037100 IMS-STATUSCHECK SECTION.                                                 
037200                                                                          
037300     SET STATUS-IX TO 1                                                   
037400     SEARCH GOOD-STATUS                                                   
037500       AT END                                                             
037600         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
037700           DELIMITED BY SIZE INTO ERROR-TEXT                              
037800         DISPLAY ERROR-TEXT                                               
037900         CALL FELLOG                                                      
038000       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
038100         CONTINUE                                                         
038200     END-SEARCH                                                           
038300     .                                                                    
038400 DB2-SELECT-BYART SECTION.                                                
038500     MOVE 'DB2-SELECT-BYART'     TO CURRENT-DB-SEC                        
038600                                                                          
038700     MOVE 000100                 TO GOOD-SQLCODECODES                     
038800     EXEC SQL SELECT                                                      
038900                  IDDISTR_RENOV,                                          
039000                  KVLS_MAXCORE                                            
039100              INTO                                                        
039200                  :BYART-IDDISTR-RENOV,                                   
039300                  :BYART-KVLS-MAXCORE                                     
039400              FROM BYART                                                  
039500             WHERE IDARTNR_BYT = :WS-IDARTNR-BYT                          
039600     END-EXEC                                                             
039700     MOVE SQLCODE                TO SQLCODE-WS                            
039800     PERFORM DB2-STATUS-KONTROLL                                          
039900     .                                                                    
040000     EJECT                                                                
040100 DB2-STATUS-KONTROLL SECTION.                                             
040200                                                                          
040300     SET SQLCODE-IX              TO 1                                     
040400     SEARCH GOOD-SQLCODE                                                  
040500       AT END                                                             
040600         STRING ' INVALID RETURN CODE FROM DB2: ' SQLCODE-WS              
040700             DELIMITED BY SIZE INTO ERROR-TEXT                            
040800         CALL FELLOG                                                      
040900       WHEN GOOD-SQLCODE (SQLCODE-IX) = SQLCODE-WS                        
041000         CONTINUE                                                         
041100     END-SEARCH                                                           
041200     .                                                                    
