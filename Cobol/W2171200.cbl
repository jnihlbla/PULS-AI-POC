000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W2171200.                                                
000300 AUTHOR.         STENING INGER.                                           
000400 DATE-WRITTEN.   13/01/31.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*                                                                         
000800*    FUNCTION:                                                            
000900*        READ KVOI INFO FROM FILE W01186 (WDL7)                           
001000*                                                                         
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
002400*          --- OUTPUT FROM WDL701 AND WDL711                              
002500     SELECT W2171201                   ASSIGN TO W21712D1.                
002600     SKIP2                                                                
002700*          --- ORDERINGÅNG WDL7                                           
002800     SELECT W2171202                   ASSIGN TO W21712D2.                
002900     EJECT                                                                
003000 DATA DIVISION.                                                           
003100     SKIP3                                                                
003200 FILE SECTION.                                                            
003300     SKIP3                                                                
003400 FD  W2171201                                                             
003500     RECORDING       F                                                    
003600     BLOCK CONTAINS  0.                                                   
003700                                                                          
003800*01  -COPY W01186      -L.                                                
003900     SKIP3                                                                
004000 FD  W2171202                                                             
004100     RECORDING       F                                                    
004200     BLOCK CONTAINS  0.                                                   
004300                                                                          
004400*01  RECORD -COPY W2171201 -PRE  OUT-  -L.                                
004500     EJECT                                                                
004600 WORKING-STORAGE SECTION.                                                 
004700                                                                          
004800 77  IDPGM                       PIC X(8)    VALUE 'W2171200'.            
004900 77  CURRENT-SECTION             PIC X(30)  VALUE SPACE.                  
005000 77  YES                         PIC X       VALUE 'J'.                   
005100 77  NOO                         PIC X       VALUE 'N'.                   
005200 77  IX-YY                       PIC 9(2).                                
005300 77  IX-MM                       PIC 9(2).                                
005400 77  IX-WW                       PIC 9(2).                                
005500 77  IX-INNEV                    PIC 9(2).                                
005600 77  IX-RP                       PIC 9(2).                                
005700                                                                          
005800 77  W2171201-EOF-SW             PIC X       VALUE 'N'.                   
005900     88  END-OF-W2171201                     VALUE 'J'.                   
006000                                                                          
006100 01  DAGENS-PER              PIC 9(4)   VALUE ZERO.                       
006200 01  DAG-PER REDEFINES DAGENS-PER.                                        
006300         05 DAGENS-AA            PIC 9(2).                                
006400         05 DAGENS-PP            PIC 9(2).                                
006500 77  DAGENS-AAR                  PIC 9(4)    VALUE ZERO.                  
006600 77  DAGENS-VECKA                PIC 9(2)    VALUE ZERO.                  
006700                                                                          
006800 01  W-OUT-KVOI-YEAR             PIC S9(07)  VALUE +0.                    
006900 01  OI-ARTAL-0                  PIC 9(4).                                
007000 01  OI-ARTAL-1                  PIC 9(4).                                
007100 01  OI-ARTAL-2                  PIC 9(4).                                
007200 01  OI-ARTAL-3                  PIC 9(4).                                
007300 01  OI-ARTAL-4                  PIC 9(4).                                
007400 01  OI-ARTAL-5                  PIC 9(4).                                
007500     EJECT                                                                
007600 01  DAGENS-DATUM                PIC 9(8)    VALUE ZERO.                  
007700 01  FILLER REDEFINES DAGENS-DATUM.                                       
007800     03  DAGENS-DATUM-AAR        PIC 9(4).                                
007900     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
008000     03  DAGENS-DATUM-DAG        PIC 9(2).                                
008100     EJECT                                                                
008200*    -COPY WWDC99                                                         
008300     EJECT                                                                
008400                                                                          
008500 01  GENERAL-SUBPROGRAMS.                                                 
008600*                                                                         
008700     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
008800     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
008900     03  FELLOG                  PIC X(8)    VALUE 'FELLOG'.              
009000     03  WDATKONV                PIC X(8)    VALUE 'WDATKONV'.            
009100     SKIP2                                                                
009200 01  FELTEXT.                                                             
009300     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
009400     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
009500     EJECT                                                                
009600*    --- PARAMETERS TO ABEND                                              
009700                                                                          
009800 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
009900 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
010000 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
010100     SKIP2                                                                
010200 01  ERROR-TEXT.                                                          
010300     03  FILLER                  PIC X(10)   VALUE 'ERROR-TEXT'.          
010400     03  ERROR-TEXT-STR          PIC X(72)   VALUE SPACE.                 
010500     EJECT                                                                
010600*                                                                         
010700 01  PROGRAM-NAME                PIC X(6)    VALUE 'W21712'.              
010800                                                                          
010900     EJECT                                                                
011000*    --- PARAMETRAR TILL POSTSUM                                          
011100*                                                                         
011200*01  -COPY W0005   -PRE  POSTSUM-                                         
011300     EJECT                                                                
011400*    --- PARAMETRAR TILL SUBPROGRAM WDATKONV                              
011500*01 -COPY WDATAREA                                                        
011600     SKIP3                                                                
011700 01  IN-AREA-START               PIC X(24)   VALUE                        
011800                                 'IN-AREA-START  '.                       
011900     SKIP2                                                                
012000                                                                          
012100*01  DC-AREA -COPY W01186     -PRE IN-                                    
012200     EJECT                                                                
012300 01  OUT-AREA-START              PIC X(24)   VALUE                        
012400                                 'OUT-AREA-START  '.                      
012500     SKIP2                                                                
012600                                                                          
012700*01  AREA -COPY W2171201     -PRE OUT-                                    
012800     EJECT                                                                
012900 PROCEDURE DIVISION.                                                      
013000 MAIN SECTION.                                                            
013100     SKIP2                                                                
013200                                                                          
013300     PERFORM A-INIT                                                       
013400     PERFORM S01-READ-W2171201                                            
013500     PERFORM UNTIL END-OF-W2171201                                        
013600                                                                          
013700        PERFORM F-COMPUTE-ORDERINGANG                                     
013800                                                                          
013900        PERFORM S01-READ-W2171201                                         
014000                                                                          
014100     END-PERFORM                                                          
014200                                                                          
014300     PERFORM Z-FINIT                                                      
014400                                                                          
014500     MOVE ZERO TO RETURN-CODE                                             
014600     GOBACK                                                               
014700     .                                                                    
014800     EJECT                                                                
014900 A-INIT SECTION.                                                          
015000     MOVE ' A-INIT                 ' TO CURRENT-SECTION                   
015100                                                                          
015200     OPEN INPUT  W2171201                                                 
015300          OUTPUT W2171202                                                 
015400                                                                          
015500     ACCEPT DAGENS-DATUM FROM DATE                                        
015600                                                                          
015700     MOVE 'AAMMDD'           TO DAT-KDDATFORM                             
015800     MOVE DAGENS-DATUM       TO DAT-I-TIDATUM                             
015900                                                                          
016000     CALL WDATKONV USING DAT-KDDATFORM DAT-I-TIDATUM                      
016100                     DAT-O-TIDATUM DAT-KDSVAR                             
016200                                                                          
016300     IF DAT-KDSVAR-OK                                                     
016400       MOVE DAT-TISEKEL       TO DAGENS-AAR(1:2)                          
016500       MOVE DAT-TIAARP        TO DAGENS-PER                               
016600       MOVE DAT-TIVV          TO DAGENS-VECKA                             
016700     ELSE                                                                 
016800         STRING ' FEL FRÅN DATUMRUTIN WDATKONV '                          
016900         DELIMITED BY SIZE INTO FELTEXT                                   
017000         CALL FELLOG                                                      
017100     END-IF                                                               
017200                                                                          
017300     MOVE FUNCTION CURRENT-DATE (1:8) TO DAGENS-DATUM                     
017400     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
017500     .                                                                    
017600     EJECT                                                                
017700                                                                          
017800 F-COMPUTE-ORDERINGANG SECTION.                                           
017900     MOVE 'F-COMPUTE-ORDERINGANG   ' TO CURRENT-SECTION                   
018000                                                                          
018100     MOVE 0                          TO OUT-KVOI-12-RULL                  
018200                                        OUT-KVOI-YEAR-0                   
018300                                        OUT-KVOI-YEAR-1                   
018400                                        OUT-KVOI-YEAR-2                   
018500                                        OUT-KVOI-YEAR-3                   
018600                                        OUT-KVOI-YEAR-4                   
018700                                        OUT-KVOI-YEAR-5                   
018800                                                                          
018900     MOVE IN-DC-IDARTNR              TO OUT-IDARTNR                       
019000     MOVE IN-DC-IDDC                 TO OUT-IDDC                          
019100                                        WS-IDDC                           
019200     IF NDC-CN OR NDC-US                                                  
019300       MOVE +1                       TO IX-WW                             
019400       PERFORM UNTIL IX-WW > 53                                           
019500                                                                          
019600         IF IX-WW < IN-DC-TIVV (01)                                       
019700                                                                          
019800            COMPUTE OUT-KVOI-YEAR-0 = OUT-KVOI-YEAR-0                     
019900                                    + IN-DC-KVOI-RULL(IX-WW)              
020000                                    + IN-DC-KVOI-REF-RULL(IX-WW)          
020100         END-IF                                                           
020200                                                                          
020300         COMPUTE OUT-KVOI-12-RULL = OUT-KVOI-12-RULL                      
020400                                  + IN-DC-KVOI-RULL     (IX-WW)           
020500                                  + IN-DC-KVOI-REF-RULL (IX-WW)           
020600                                                                          
020700         ADD 1                       TO IX-WW                             
020800       END-PERFORM                                                        
020900                                                                          
021000       MOVE +1    TO IX-INNEV                                             
021100       PERFORM UNTIL IX-INNEV > +5                                        
021200         COMPUTE OUT-KVOI-12-RULL = OUT-KVOI-12-RULL                      
021300               + IN-DC-KVOI-INNEV     (IX-INNEV)                          
021310               + IN-DC-KVOI-PP-INNEV  (IX-INNEV)                          
021400               + IN-DC-KVOI-REF-INNEV (IX-INNEV)                          
021500                                                                          
021600         IF IN-DC-TIVV (IX-INNEV) <= DAT-TIVV                             
021700            IF IN-DC-KVOI-INNEV     (IX-INNEV) > +0                       
021710            OR IN-DC-KVOI-PP-INNEV  (IX-INNEV) > +0                       
021800            OR IN-DC-KVOI-REF-INNEV (IX-INNEV) > +0                       
021900               COMPUTE OUT-KVOI-YEAR-0  = OUT-KVOI-YEAR-0                 
022000                     + IN-DC-KVOI-INNEV     (IX-INNEV)                    
022010                     + IN-DC-KVOI-PP-INNEV  (IX-INNEV)                    
022100                     + IN-DC-KVOI-REF-INNEV (IX-INNEV)                    
022200            END-IF                                                        
022300         END-IF                                                           
022400                                                                          
022500         ADD +1                  TO IX-INNEV                              
022600       END-PERFORM                                                        
022700                                                                          
022800       MOVE +1    TO IX-YY                                                
022900       PERFORM UNTIL IX-YY > 5                                            
023000         MOVE +1  TO IX-MM                                                
023100         PERFORM UNTIL IX-MM > 12                                         
023200           COMPUTE W-OUT-KVOI-YEAR = W-OUT-KVOI-YEAR                      
023300                          + IN-DC-KVOI(IX-YY IX-MM)                       
023400                          + IN-DC-KVOI-REFILL(IX-YY IX-MM)                
023500                                                                          
023600           ADD 1                     TO IX-MM                             
023700         END-PERFORM                                                      
023800                                                                          
023900         IF IX-YY = +1                                                    
024000           MOVE W-OUT-KVOI-YEAR      TO OUT-KVOI-YEAR-1                   
024100         ELSE                                                             
024200           IF IX-YY = +2                                                  
024300            MOVE W-OUT-KVOI-YEAR     TO OUT-KVOI-YEAR-2                   
024400           ELSE                                                           
024500             IF IX-YY = +3                                                
024600               MOVE W-OUT-KVOI-YEAR  TO OUT-KVOI-YEAR-3                   
024700             ELSE                                                         
024800               IF IX-YY = +4                                              
024900                 MOVE W-OUT-KVOI-YEAR TO OUT-KVOI-YEAR-4                  
025000               ELSE                                                       
025100                 IF IX-YY = +5                                            
025200                   MOVE W-OUT-KVOI-YEAR TO OUT-KVOI-YEAR-5                
025300                 END-IF                                                   
025400               END-IF                                                     
025500             END-IF                                                       
025600           END-IF                                                         
025700         END-IF                                                           
025800         MOVE +0                        TO W-OUT-KVOI-YEAR                
025900         ADD +1                         TO IX-YY                          
026000       END-PERFORM                                                        
026100       PERFORM S11-WRITE-W2171202                                         
026200     END-IF                                                               
026300     .                                                                    
026400     EJECT                                                                
026500 Z-FINIT SECTION.                                                         
026600     MOVE 'Z-FINIT                 ' TO CURRENT-SECTION                   
026700                                                                          
026800     CLOSE W2171201                                                       
026900           W2171202                                                       
027000     SKIP2                                                                
027100     MOVE 'S' TO POSTSUM-OPKOD                                            
027200     CALL POSTSUM USING POSTSUM-PARM                                      
027300     .                                                                    
027400     EJECT                                                                
027500 S01-READ-W2171201  SECTION.                                              
027600     MOVE 'S01-READ-W2171201       ' TO CURRENT-SECTION                   
027700                                                                          
027800     READ W2171201 INTO IN-DC-AREA                                        
027900     AT END                                                               
028000        MOVE HIGH-VALUE TO IN-DC-AREA                                     
028100        SET END-OF-W2171201 TO TRUE                                       
028200                                                                          
028300     NOT AT END                                                           
028400        MOVE 'W21712' TO POSTSUM-FDNAMN                                   
028500        MOVE 'W21712D1' TO POSTSUM-DDNAMN2                                
028600        MOVE 'WDL7'    TO POSTSUM-TRANSTYP                                
028700        CALL POSTSUM USING POSTSUM-PARM                                   
028800     END-READ                                                             
028900     .                                                                    
029000     EJECT                                                                
029100 S11-WRITE-W2171202 SECTION.                                              
029200     MOVE 'S11-WRITE-W2171202      ' TO CURRENT-SECTION                   
029300                                                                          
029400     WRITE OUT-RECORD FROM OUT-AREA                                       
029500                                                                          
029600     MOVE 'W2171201' TO POSTSUM-TRANSTYP                                  
029700     MOVE 'W21712' TO POSTSUM-FDNAMN                                      
029800     MOVE 'W21712D2' TO POSTSUM-DDNAMN2                                   
029900     CALL POSTSUM USING POSTSUM-PARM                                      
030000     .                                                                    
030100     EJECT                                                                
030200 S99-ABEND SECTION.                                                       
030300                                                                          
030400     SKIP2                                                                
030500     MOVE 'S' TO POSTSUM-OPKOD                                            
030600     CALL POSTSUM USING POSTSUM-PARM                                      
030700     CALL ABEND USING RKOD-ABEND                                          
030800     .                                                                    
