000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W2220700.                                                
000300 AUTHOR.         ANDERSSON BERT.                                          
000400 DATE-WRITTEN.   13/01/10.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNCTION:                                                            
000800*      FAIR DISTRIBUTION FROM REFILL                                      
000900*      W22207 PERIODICALLY FILE EXTRACT FOR MODEL OF REQUIREMENTS         
001000*                                                                         
001100*                                                                         
001200                                                                          
001300     SKIP3                                                                
001400 ENVIRONMENT DIVISION.                                                    
001500     SKIP2                                                                
001600 INPUT-OUTPUT SECTION.                                                    
001700                                                                          
001800 FILE-CONTROL.                                                            
001900     SKIP2                                                                
002000*          --- W01160  WDK601 & WDK611                                    
002100     SELECT W01160                     ASSIGN TO W22207D1.                
002200     SKIP2                                                                
002300*          --- W22207  PERIODICALLY FILE EXTRACT                          
002400     SELECT W22207                     ASSIGN TO W22207D2.                
002500     EJECT                                                                
002600 DATA DIVISION.                                                           
002700     SKIP3                                                                
002800 FILE SECTION.                                                            
002900     SKIP3                                                                
003000 FD  W01160                                                               
003100     RECORDING       F                                                    
003200     BLOCK CONTAINS  0.                                                   
003300                                                                          
003400*01  W01160-POST       -COPY W01160      -L.                              
003500     SKIP3                                                                
003600 FD  W22207                                                               
003700     RECORDING       F                                                    
003800     BLOCK CONTAINS  0.                                                   
003900                                                                          
004000*01  W22207-POST  -COPY W22207           -L.                              
004100     EJECT                                                                
004200 WORKING-STORAGE SECTION.                                                 
004300*    -- CHECKED BY WY2000                                                 
004400     SKIP3                                                                
004500*    -COPY WY2000W3                                                       
004600     SKIP3                                                                
004700                                                                          
004800 77  FILLER                      PIC X(8)    VALUE 'WORKAREA'.            
004900 77  IDPGM                       PIC X(8)    VALUE 'W2220700'.            
005000 77  JA                          PIC X       VALUE 'J'.                   
005100 77  NEJ                         PIC X       VALUE 'N'.                   
005200     SKIP2                                                                
005300 01  ERROR-TEXT.                                                          
005400     03  FILLER                  PIC X(8)    VALUE 'ERRORTEX'.            
005500     03  ERROR-TEXT-STR          PIC X(72)   VALUE SPACE.                 
005600                                                                          
005700     SKIP2                                                                
005800 77  RESEASON-IX                 PIC S9(4)  VALUE +0    COMP SYNC.        
005900     SKIP2                                                                
006000                                                                          
006100 77  PART-SW                  PIC X       VALUE 'J'.                      
006200     88  PART-OK                          VALUE 'J'.                      
006300     88  PART-WRONG                       VALUE 'N'.                      
006400                                                                          
006500*    --- ARBETSFÄLT                                                       
006600 01  ARBETSFAELT.                                                         
006700     03  WS-DAT-TIAARP           PIC   9(4)  VALUE ZERO.                  
006800                                                                          
006900     03  WS-CURRENT-DATE.                                                 
007000         05  WS-DAGENS-TIAAAA    PIC 9(4)   VALUE ZERO.                   
007100         05  FILLER              PIC 9(4)   VALUE ZERO.                   
007200         05  FILLER              PIC 9(6)   VALUE ZERO.                   
007300                                                                          
007400     03  FILLER REDEFINES WS-CURRENT-DATE.                                
007500*-----   INKLUSIVE SEKEL                                                  
007600         05  WS-DAGENS-DATUM     PIC 9(8).                                
007700         05  WS-DAGENS-TID.                                               
007800             07 WS-DAGENS-TIMME  PIC 9(2).                                
007900             07 WS-DAGENS-MINUT  PIC 9(2).                                
008000             07 WS-DAGENS-SEKUND PIC 9(2).                                
008100*                                                                         
008200 01 FILLER                      PIC  X(16) VALUE 'ART-TAB'.               
008300******************************************************************        
008400*TABLE PERIODFIL                                                          
008500******************************************************************        
008600*                                                                         
008700 01 ART-TABLE.                                                            
008800    03 ART-WS-PERIOD.                                                     
008900       05   WS-IDARTNR           PIC X(9).                                
009000       05   WS-TIAARP            PIC 9(4).                                
009100       05   WS-KVPB-PLAN         PIC Z(6)9.9.                             
009200       05   WS-DAPBPLAN          PIC 9(8).                                
009300       05   WS-DASEASON          PIC 9(8).                                
009400       05   WS-RESEASON-PLAN     PIC 9.9(2).                              
009500       05   WS-KVPB-SATS         PIC Z(5)9.9.                             
009600*                                                                         
009700******************************************************************        
009800*                                                                         
009900*      --- VALID IDDC CODES                                               
010000*                                                                         
010100*01    -COPY WWDCKONS                                                     
010200       EJECT                                                              
010300*                                                                         
010400*01    -COPY WWPRODSL                                                     
010500       EJECT                                                              
010600                                                                          
010700 77  W01160-EOF-SW               PIC X       VALUE 'N'.                   
010800     88  END-OF-W01160                       VALUE 'Y'.                   
010900     EJECT                                                                
011000 01  GENERAL-SUBPROGRAMS.                                                 
011100*                                                                         
011200     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
011300     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
011400     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
011500     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
011700     EJECT                                                                
011800*    --- PARAMETRAR TILL POSTSUM                                          
011900*                                                                         
012000*01  -COPY W0005   -PRE  POSTSUM-                                         
012100     EJECT                                                                
012200*01  -COPY WDATAREA                                                       
012300     EJECT                                                                
013000*    ---  DLI INPUT-OUTPUT AREA                                           
013100                                                                          
013200     EJECT                                                                
013300 01  CLAG-AREA-START         PIC X(24) VALUE 'CLAG-AREA-START'.           
013400*01  AREA -COPY W01160       -PRE IN-                                     
013500*                                                                         
013600*                                                                         
013700     EJECT                                                                
013800 01  OUT-AREA-START          PIC X(24) VALUE 'OUT-AREA-START'.            
013900*01       -COPY W22207                                                    
014000*                                                                         
014100     EJECT                                                                
014200                                                                          
014300 LINKAGE SECTION.                                                         
014400                                                                          
015400 PROCEDURE DIVISION.                                                      
015800 MAIN SECTION.                                                            
016300                                                                          
016400     SKIP2                                                                
016500     PERFORM A-INIT                                                       
016600     PERFORM S01-READ-W01160                                              
016700     PERFORM UNTIL END-OF-W01160                                          
016800                                                                          
016900       PERFORM B-LAES-WRITE-INFO                                          
017000                                                                          
017100       PERFORM S01-READ-W01160                                            
017200     END-PERFORM                                                          
017300                                                                          
017400     PERFORM Z-FINIT                                                      
017500                                                                          
017600     MOVE ZERO TO RETURN-CODE                                             
017700     GOBACK                                                               
017800     .                                                                    
017900     EJECT                                                                
018000 A-INIT SECTION.                                                          
018100     SKIP2                                                                
018200                                                                          
018300     OPEN INPUT W01160                                                    
018400                                                                          
018500     OPEN OUTPUT W22207                                                   
018600                                                                          
018700     MOVE FUNCTION CURRENT-DATE  TO WS-CURRENT-DATE                       
018800                                                                          
018900     MOVE 'IDAG'             TO DAT-KDDATFORM                             
019000                                                                          
019100     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
019200                         DAT-O-TIDATUM DAT-KDSVAR                         
019300                                                                          
019400     IF DAT-KDSVAR-OK                                                     
019500        MOVE DAT-TIAARP          TO WS-DAT-TIAARP                         
019600     ELSE                                                                 
019700        CALL FELLOG                                                       
019800     END-IF                                                               
019900                                                                          
020000     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
020100     .                                                                    
020200     EJECT                                                                
020300                                                                          
020400 B-LAES-WRITE-INFO SECTION.                                               
020500                                                                          
020600       PERFORM BA-CHECK-PART                                              
020700       IF PART-OK                                                         
020800         MOVE IN-CLAG-IDARTNR  TO WS-IDARTNR                              
020900         MOVE WS-DAT-TIAARP    TO WS-TIAARP                               
021000                                                                          
021100         MOVE IN-CLAG-DAPBPLAN TO WS-DAPBPLAN                             
021200         MOVE IN-CLAG-DASEASON TO WS-DASEASON                             
021300                                                                          
021400         PERFORM BB-PBPLAN                                                
021500                                                                          
021600         PERFORM BC-RESEASON-PLAN                                         
021700         PERFORM BD-SATS-BEHOV                                            
021800                                                                          
021900         PERFORM BE-WRITE-TO-FILE                                         
022000       END-IF                                                             
022100     .                                                                    
022200     EJECT                                                                
022300                                                                          
022400 BA-CHECK-PART     SECTION.                                               
022500                                                                          
022600     MOVE NEJ       TO PART-SW                                            
022700                                                                          
022800     IF IN-CLAG-KDERS-UTG = 0                                             
022900       MOVE IN-CLAG-KDPRODSL     TO TEST-KDPRODSL                         
023000       IF KDPRODSL-VOLVO-BIMA                                             
023100                                                                          
023200         IF IN-CLAG-KDERS < 11                                            
023300           IF IN-CLAG-PRARTSTD > 0                                        
023400                                                                          
023500             IF IN-CLAG-KVPB-SEP > 0                                      
023600                                                                          
023700               PERFORM BAA-CLEAR-TABLE                                    
023800                                                                          
023900               MOVE JA           TO PART-SW                               
024000             ELSE                                                         
024100               MOVE NEJ          TO PART-SW                               
024200             END-IF                                                       
024300           END-IF                                                         
024400         END-IF                                                           
024500       END-IF                                                             
024600     END-IF                                                               
024700     .                                                                    
024800     EJECT                                                                
024900                                                                          
025000 BAA-CLEAR-TABLE   SECTION.                                               
025100                                                                          
025200     MOVE ZERO             TO WS-KVPB-PLAN                                
025300     MOVE ZERO             TO WS-DAPBPLAN                                 
025400     MOVE ZERO             TO WS-DASEASON                                 
025500     MOVE ZERO             TO WS-RESEASON-PLAN                            
025600     MOVE ZERO             TO WS-KVPB-SATS                                
025700                                                                          
025800     .                                                                    
025900     EJECT                                                                
026000                                                                          
026100 BB-PBPLAN  SECTION.                                                      
026200                                                                          
026300     IF  IN-CLAG-DAPBPLAN > ZERO                                          
026400                                                                          
026500       MOVE IN-CLAG-DAPBPLAN        TO WS-DAPBPLAN                        
026600     ELSE                                                                 
026700       MOVE ZERO                    TO WS-DAPBPLAN                        
026800     END-IF                                                               
026900                                                                          
027000     IF IN-CLAG-KVPB-PLAN > ZERO                                          
027100                                                                          
027200       MOVE IN-CLAG-KVPB-PLAN       TO WS-KVPB-PLAN                       
027300     ELSE                                                                 
027400       MOVE ZERO                    TO WS-KVPB-PLAN                       
027500     END-IF                                                               
027600                                                                          
027700     .                                                                    
027800     EJECT                                                                
027900                                                                          
028000 BC-RESEASON-PLAN       SECTION.                                          
028100                                                                          
028200     MOVE ZERO                     TO RESEASON-IX                         
028300     MOVE WS-TIAARP(3:2)           TO RESEASON-IX                         
028400                                                                          
028500     IF IN-CLAG-DASEASON > ZERO                                           
028600                                                                          
028700                                                                          
028800       MOVE IN-CLAG-RESEASON-PLAN (RESEASON-IX)                           
028900         TO WS-RESEASON-PLAN                                              
029000                                                                          
029100     ELSE                                                                 
030800         MOVE 1.00                 TO WS-RESEASON-PLAN                    
031000     END-IF                                                               
031100                                                                          
031200     .                                                                    
031300     EJECT                                                                
031400                                                                          
031500 BD-SATS-BEHOV  SECTION.                                                  
031600                                                                          
031700     MOVE IN-CLAG-KVPB-SATS    TO WS-KVPB-SATS                            
031800                                                                          
031900     .                                                                    
032000     EJECT                                                                
032100                                                                          
032200 BE-WRITE-TO-FILE     SECTION.                                            
032300                                                                          
032400      MOVE WS-IDARTNR                  TO OUT-IDARTNR                     
032500      MOVE WS-TIAARP                   TO OUT-TIAARP                      
032600      MOVE WS-KVPB-PLAN                TO OUT-KVPB-PLAN                   
032700                                                                          
032800      MOVE WS-DASEASON                 TO OUT-DASEASON                    
032900      MOVE WS-RESEASON-PLAN            TO OUT-RESEASON-PLAN               
033000                                                                          
033100      MOVE WS-DAPBPLAN                 TO OUT-DAPBPLAN                    
033200      MOVE WS-KVPB-SATS                TO OUT-KVPB-SATS                   
033300                                                                          
033400      PERFORM S11-WRITE-W22207                                            
033500                                                                          
033600     .                                                                    
033700     EJECT                                                                
033800                                                                          
033900 Z-FINIT SECTION.                                                         
034000                                                                          
034100     CLOSE W01160                                                         
034200                                                                          
034300           W22207                                                         
034400     SKIP2                                                                
034500     MOVE 'S' TO POSTSUM-OPKOD                                            
034600     CALL POSTSUM USING POSTSUM-PARM                                      
034700     .                                                                    
034800     EJECT                                                                
034900                                                                          
035000 S01-READ-W01160  SECTION.                                                
035100     SKIP2                                                                
035200     READ W01160 INTO IN-AREA                                             
035300     AT END                                                               
035400*       MOVE HIGH-VALUE TO CLAG-ID                                        
035500        SET END-OF-W01160 TO TRUE                                         
035600                                                                          
035700     NOT AT END                                                           
035800        MOVE 'W01160'   TO POSTSUM-FDNAMN                                 
035900        MOVE 'W22207D1' TO POSTSUM-DDNAMN2                                
036000        MOVE 'IN-'      TO POSTSUM-TRANSTYP                               
036100        CALL POSTSUM USING POSTSUM-PARM                                   
036200     END-READ                                                             
036300     .                                                                    
036400     EJECT                                                                
036500 S11-WRITE-W22207 SECTION.                                                
036600     SKIP2                                                                
036700     WRITE W22207-POST FROM OUT-W22207                                    
036800                                                                          
036900     MOVE 'OUT'      TO POSTSUM-TRANSTYP                                  
037000     MOVE 'W22207 ' TO POSTSUM-FDNAMN                                     
037100     MOVE 'W22207D2' TO POSTSUM-DDNAMN2                                   
037200     CALL POSTSUM USING POSTSUM-PARM                                      
037300     .                                                                    
037400     EJECT                                                                
