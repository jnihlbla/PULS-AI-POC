000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W271G100.                                                
000300 AUTHOR.         ARUP DATTA.                                              
000400 DATE-WRITTEN.   18/08/01.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNCTION:                                                            
000800*        REFILL ORDERS                                                    
000900*                                                                         
001000*        THE PROGRAM READS     K711                                       
001100*                                                                         
001200*    ABENDCODES:                                                          
001300*        U0016 -  . . . .                                                 
001400*        U1000 -  . . . .                                                 
001500*                                                                         
001600                                                                          
001700     SKIP3                                                                
001800 ENVIRONMENT DIVISION.                                                    
001900     SKIP2                                                                
002000 INPUT-OUTPUT SECTION.                                                    
002100                                                                          
002200 FILE-CONTROL.                                                            
002300     SKIP2                                                                
002400*          --- REFILL ORDERS                                              
002500     SELECT W271G101                   ASSIGN TO W271G1D1.                
002600     SELECT W271G102                   ASSIGN TO W271G1D2.                
002700     EJECT                                                                
002800 DATA DIVISION.                                                           
002900     SKIP2                                                                
003000 FILE SECTION.                                                            
003100     SKIP3                                                                
003200 FD  W271G101                                                             
003300     RECORDING       F                                                    
003400     BLOCK CONTAINS  0.                                                   
003500                                                                          
003600*01  RECORD -COPY W271G1 -PRE  OUT1- -L.                                  
003700     EJECT                                                                
003800 FD  W271G102                                                             
003900     RECORDING       F                                                    
004000     BLOCK CONTAINS  0.                                                   
004100                                                                          
004200*01  RECORD -COPY W27111 -PRE  OUT2- -L.                                  
004300     EJECT                                                                
004400 WORKING-STORAGE SECTION.                                                 
004500                                                                          
004600 77  IDPGM                       PIC X(8)    VALUE 'W271G100'.            
004700 77  CURRENT-SECTION             PIC X(80)   VALUE SPACE.                 
004800 77  YES                         PIC X       VALUE 'J'.                   
004900 77  NOO                         PIC X       VALUE 'N'.                   
005000     EJECT                                                                
005100*    --- VALID IDDC CODES                                                 
005200*01  -COPY WWDC99                                                         
005300*01  -COPY WWDC99   -PRE REF-                                             
005400     EJECT                                                                
005500 01  TODAYS-DATE                 PIC 9(6)    VALUE ZERO.                  
005600 01  FILLER REDEFINES TODAYS-DATE.                                        
005700     03  TODAYS-DATE-YEAR        PIC 9(2).                                
005800     03  TODAYS-DATE-MONTH       PIC 9(2).                                
005900     03  TODAYS-DATE-DAY         PIC 9(2).                                
006000     EJECT                                                                
006100 01  GENERAL-SUBPROGRAMS.                                                 
006200*                                                                         
006300     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
006400     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
006500     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
006600     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
006700     SKIP2                                                                
006800*    --- PARAMETERS FOR SUBPROGRAM ABEND                                  
006900                                                                          
007000 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
007100 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
007200 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
007300     SKIP2                                                                
007400 01  ERROR-TEXT.                                                          
007500     03  FILLER                  PIC X(8)    VALUE 'ERR-TEXT'.            
007600     03  ERROR-TEXT-STR          PIC X(72)   VALUE SPACE.                 
007700     EJECT                                                                
007800*    --- PARAMETRAR TILL POSTSUM                                          
007900*                                                                         
008000*01  -COPY W0005   -PRE  POSTSUM-                                         
008100     EJECT                                                                
008200 01  OUT1-AREA-START              PIC X(24)   VALUE                       
008300                                 'OUT1-AREA-START  '.                     
008400     SKIP2                                                                
008500                                                                          
008600*01  AREA -COPY W271G1     -PRE OUT1-                                     
008700     EJECT                                                                
008800 01  OUT2-AREA-START              PIC X(24)   VALUE                       
008900                                 'OUT2-AREA-START  '.                     
009000     SKIP2                                                                
009100                                                                          
009200*01  AREA -COPY W27111     -PRE OUT2-                                     
009300     EJECT                                                                
009400*    --- AREAS FOR IMS-SECTIONS                                           
009500*                                                                         
009600     EJECT                                                                
009700 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
009800     SKIP3                                                                
009900 01  KEYS-FOR-DLI.                                                        
010000                                                                          
010100     03  W-IDARTNR-X.                                                     
010200         05  W-IDARTNR           PIC S9(9)   VALUE ZERO COMP-3.           
010300     03  W-IDDC-X.                                                        
010400         05  W-IDDC              PIC X(2)    VALUE SPACE.                 
010500     03  W-IDDC-REF-X.                                                    
010600         05  W-IDDC-REF          PIC X(2)    VALUE SPACE.                 
010700     03  W-IDDC-B6-X.                                                     
010800         05  W-IDDC-B6           PIC X(2)    VALUE SPACE.                 
010900     03  W-IDDC-B616-X.                                                   
011000         05  W-IDDC-B616         PIC X(2)    VALUE SPACE.                 
011100                                                                          
011200     SKIP2                                                                
011300*    --- STATUS-KOD FRÅN IMS                                              
011400 01  STATUS-WS                   PIC XX.                                  
011500     88  SEGMENT-FINNS                       VALUE '  '.                  
011600     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
011700     88  SEGMENT-SLUT                        VALUE 'GB'.                  
011800     SKIP2                                                                
011900 01  GOOD-STATUSCODES.                                                    
012000     03  GOOD-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
012100     SKIP3                                                                
012200 01  SSA1                        PIC X(64).                               
012300 01  SSA2                        PIC X(64).                               
012400     EJECT                                                                
012500*    --- IMS FUNCTION CODES                                               
012600*01  -COPY W0003                                                          
012700     EJECT                                                                
012800*    ---  DLI INPUT-OUTPUT AREA                                           
012900 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK701'.                      
013000 01  DLI-IO-WDK701.                                                       
013100*    03  -COPY WDK701                                                     
013200     EJECT                                                                
013300 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK711'.                      
013400 01  DLI-IO-WDK711.                                                       
013500*    03  -COPY WDK711                                                     
013600     EJECT                                                                
013700 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK701-2'.                    
013800 01  DLI-IO-WDK701-2.                                                     
013900*    03  -COPY WDK701 -PRE K7-                                            
014000     EJECT                                                                
014100 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDK711-2'.                    
014200 01  DLI-IO-WDK711-2.                                                     
014300*    03  -COPY WDK711 -PRE K7-                                            
014400     EJECT                                                                
014500 01  FILLER         PIC X(24) VALUE 'DLI-IO-WDB616'.                      
014600 01  DLI-IO-WDB616.                                                       
014700*    03  -COPY WDB616 -PRE B616-                                          
014800     EJECT                                                                
014900 LINKAGE SECTION.                                                         
015000*01  -COPY W0008  -PRE WDK7-                                              
015100     05  FILLER                  PIC X.                                   
015200*01  -COPY W0008  -PRE WDK7-2-                                            
015300     05  FILLER                  PIC X.                                   
015400*01  -COPY W0008  -PRE WDB6-                                              
015500     05  FILLER                  PIC X.                                   
015600     EJECT                                                                
015700 PROCEDURE DIVISION  USING WDK7-PCB WDK7-2-PCB WDB6-PCB.                  
015800 MAIN SECTION.                                                            
015900     ENTRY 'DLITCBL' USING WDK7-PCB WDK7-2-PCB WDB6-PCB.                  
016000                                                                          
016100     PERFORM A-INIT                                                       
016200                                                                          
016300     PERFORM IMS-GN-WDK701                                                
016400     PERFORM UNTIL SEGMENT-SLUT                                           
016500       MOVE SART-IDARTNR         TO W-IDARTNR                             
016600       PERFORM IMS-GNP-WDK711                                             
016700       PERFORM UNTIL SEGMENT-SAKNAS                                       
016800         MOVE SLAG-IDDC          TO WS-IDDC                               
016900         MOVE SLAG-IDDC-REF      TO REF-WS-IDDC                           
017000                                                                          
017100         IF (NDC-US                                                       
017200         AND REF-NDC-CN)                                                  
017300             PERFORM S01-CHECK-REFILL-IDDC                                
017400             IF SEGMENT-FINNS                                             
017500*               CHECK REFILLING DC-CN IS REFILLED FROM DC-US              
017600                IF  NDC-CN                                                
017700                AND REF-NDC-US                                            
017800                    PERFORM B-WRITE-OUTPUT                                
017900                END-IF                                                    
018000             END-IF                                                       
018100         ELSE                                                             
018200           IF (NDC-CN                                                     
018300           AND REF-NDC-US)                                                
018400               PERFORM S01-CHECK-REFILL-IDDC                              
018500               IF SEGMENT-FINNS                                           
018600*                 CHECK REFILLING DC-US IS REFILLED FROM DC-CN            
018700                  IF  NDC-US                                              
018800                  AND REF-NDC-CN                                          
018900                      PERFORM B-WRITE-OUTPUT                              
019000                  END-IF                                                  
019100               END-IF                                                     
019200           END-IF                                                         
019300         END-IF                                                           
019400         PERFORM IMS-GNP-WDK711                                           
019500       END-PERFORM                                                        
019600       PERFORM IMS-GN-WDK701                                              
019700     END-PERFORM                                                          
019800                                                                          
019900     PERFORM Z-FINIT                                                      
020000                                                                          
020100     MOVE ZERO TO RETURN-CODE                                             
020200     GOBACK                                                               
020300     .                                                                    
020400     EJECT                                                                
020500                                                                          
020600 A-INIT SECTION.                                                          
020700                                                                          
020800     OPEN OUTPUT W271G101                                                 
020900                 W271G102                                                 
021000                                                                          
021100     ACCEPT TODAYS-DATE  FROM DATE                                        
021200     MOVE IDPGM            TO POSTSUM-PROGNAMN                            
021300     .                                                                    
021400     EJECT                                                                
021500                                                                          
021600 B-WRITE-OUTPUT SECTION.                                                  
021700                                                                          
021800     MOVE SLAG-IDDC                      TO W-IDDC-B6                     
021900     MOVE SLAG-IDDC-REF                  TO W-IDDC-B616                   
022000                                                                          
022100     PERFORM IMS-GU-WDB616                                                
022200     IF SEGMENT-FINNS                                                     
022300        MOVE SART-IDARTNR                TO OUT1-IDARTNR                  
022400        MOVE SLAG-IDDC                   TO OUT1-IDDC                     
022500        PERFORM S11-WRITE-W271G101                                        
022600                                                                          
022700        MOVE SART-IDARTNR                TO OUT2-IDARTNR                  
022800        MOVE SLAG-IDDC                   TO OUT2-IDDC                     
022900        MOVE SLAG-IDPERSON-BUY           TO OUT2-IDPERSON-BUY             
023000        MOVE 'B'                         TO OUT2-KDREFTYP                 
023100        MOVE B616-REF-IDDISTR-REFILL     TO OUT2-IDDISTR                  
023200        MOVE ZERO                        TO OUT2-ADLAGOMR-CDC             
023300                                            OUT2-ADGANG-CDC               
023400                                            OUT2-ADPLATS-CDC              
023500        MOVE SLAG-ADLAGOMR               TO OUT2-ADLAGOMR-SDC             
023600        MOVE SLAG-ADGANG                 TO OUT2-ADGANG-SDC               
023700        MOVE SLAG-ADPLATS                TO OUT2-ADPLATS-SDC              
023800        MOVE ZERO                        TO OUT2-KVBEART                  
023900        MOVE 'P'                         TO OUT2-KDREFORS                 
024000        MOVE SLAG-IDLEVNR                TO OUT2-IDLEVNR                  
024100        MOVE '75'                        TO OUT2-KDREFTXT                 
024200        MOVE ZERO                        TO OUT2-KDFRAKT                  
024300                                            OUT2-KVBEART-CD               
024400                                            OUT2-ADLAGOMR-CD              
024500                                            OUT2-ADGANG-CD                
024600                                            OUT2-ADPLATS-CD               
024700                                            OUT2-IDKUNDNR                 
024800        MOVE SLAG-IDDC-REF               TO OUT2-IDDC-REF                 
024900        PERFORM S12-WRITE-W271G102                                        
025000     END-IF                                                               
025100     .                                                                    
025200     EJECT                                                                
025300 S01-CHECK-REFILL-IDDC  SECTION.                                          
025400                                                                          
025500     MOVE SLAG-IDDC-REF             TO W-IDDC-REF                         
025600     PERFORM IMS-GU-WDK711-REF                                            
025700     IF SEGMENT-FINNS                                                     
025800        MOVE K7-SLAG-IDDC           TO WS-IDDC                            
025900        MOVE K7-SLAG-IDDC-REF       TO REF-WS-IDDC                        
026000     END-IF                                                               
026100     .                                                                    
026200     EJECT                                                                
026300 Z-FINIT SECTION.                                                         
026400     CLOSE W271G101                                                       
026500           W271G102                                                       
026600                                                                          
026700     MOVE 'S'              TO POSTSUM-OPKOD                               
026800     CALL POSTSUM       USING POSTSUM-PARM                                
026900     .                                                                    
027000     EJECT                                                                
027100 S11-WRITE-W271G101 SECTION.                                              
027200                                                                          
027300     WRITE OUT1-RECORD   FROM OUT1-AREA                                   
027400                                                                          
027500     MOVE SPACE            TO POSTSUM-TRANSTYP                            
027600     MOVE 'W271G1'         TO POSTSUM-FDNAMN                              
027700     MOVE 'W271G1D1'       TO POSTSUM-DDNAMN2                             
027800     CALL POSTSUM       USING POSTSUM-PARM                                
027900     .                                                                    
028000     EJECT                                                                
028100 S12-WRITE-W271G102 SECTION.                                              
028200                                                                          
028300     WRITE OUT2-RECORD   FROM OUT2-AREA                                   
028400                                                                          
028500     MOVE SPACE            TO POSTSUM-TRANSTYP                            
028600     MOVE 'W271G1'         TO POSTSUM-FDNAMN                              
028700     MOVE 'W271G1D2'       TO POSTSUM-DDNAMN2                             
028800     CALL POSTSUM       USING POSTSUM-PARM                                
028900     .                                                                    
029000     EJECT                                                                
029100 S99-ABEND SECTION.                                                       
029200                                                                          
029300     SKIP2                                                                
029400     MOVE 'S' TO POSTSUM-OPKOD                                            
029500     CALL POSTSUM USING POSTSUM-PARM                                      
029600     CALL ABEND USING RKOD-ABEND                                          
029700     .                                                                    
029800     EJECT                                                                
029900* --- IMS SECTIONS  ---                                                   
030000                                                                          
030100     EJECT                                                                
030200 IMS-GN-WDK701 SECTION.                                                   
030300                                                                          
030400     MOVE 'WDK701'         TO SSA1                                        
030500     MOVE '  GBGE'         TO GOOD-STATUSCODES                            
030600     CALL CBLTDLI USING GN WDK7-PCB DLI-IO-WDK701 SSA1                    
030700     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
030800     PERFORM IMS-STATUSCHECK                                              
030900     .                                                                    
031000     EJECT                                                                
031100                                                                          
031200 IMS-GNP-WDK711 SECTION.                                                  
031300                                                                          
031400     MOVE 'WDK711 '        TO SSA1                                        
031500     MOVE '  GE'           TO GOOD-STATUSCODES                            
031600     CALL CBLTDLI USING GNP WDK7-PCB DLI-IO-WDK711 SSA1                   
031700     MOVE WDK7-STATUS-CODE TO STATUS-WS                                   
031800     PERFORM IMS-STATUSCHECK                                              
031900     .                                                                    
032000     EJECT                                                                
032100 IMS-GU-WDK711-REF SECTION.                                               
032200                                                                          
032300     STRING 'WDK701  (IDARTNR  =' W-IDARTNR-X ')'                         
032400          DELIMITED BY SIZE INTO SSA1                                     
032500     STRING 'WDK711  (IDDC     =' W-IDDC-REF-X ')'                        
032600          DELIMITED BY SIZE INTO SSA2                                     
032700     MOVE '  GE'           TO GOOD-STATUSCODES                            
032800     CALL CBLTDLI USING GU WDK7-2-PCB DLI-IO-WDK711-2 SSA1 SSA2           
032900     MOVE WDK7-2-STATUS-CODE TO STATUS-WS                                 
033000     PERFORM IMS-STATUSCHECK                                              
033100     .                                                                    
033200     EJECT                                                                
033300 IMS-GU-WDB616    SECTION.                                                
033400     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
033500          DELIMITED BY SIZE INTO SSA1                                     
033600     STRING 'WDB616  (IDDCREF  =' W-IDDC-B616-X ')'                       
033700          DELIMITED BY SIZE INTO SSA2                                     
033800     MOVE '  GE'              TO GOOD-STATUSCODES                         
033900     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-WDB616 SSA1 SSA2               
034000     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
034100     PERFORM IMS-STATUSCHECK                                              
034200     .                                                                    
034300     EJECT                                                                
034400                                                                          
034500 IMS-STATUSCHECK SECTION.                                                 
034600                                                                          
034700     SET STATUS-IX TO 1                                                   
034800     SEARCH GOOD-STATUS                                                   
034900       AT END                                                             
035000         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
035100           DELIMITED BY SIZE INTO ERROR-TEXT                              
035200         DISPLAY ERROR-TEXT                                               
035300         CALL FELLOG                                                      
035400       WHEN GOOD-STATUS (STATUS-IX) = STATUS-WS                           
035500         CONTINUE                                                         
035600     END-SEARCH                                                           
035700     .                                                                    
