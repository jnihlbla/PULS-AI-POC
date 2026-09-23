000100 ID DIVISION.                                                             
000200 PROGRAM-ID.    W5706B00.                                                 
000300                                                                          
000400*    AUTHOR.        ANDERS HENRIKSSON.                                    
000500*    DATE-WRITTEN   APRIL   2012.                                         
000600*                                                                         
000700*    FUNKTION:                                                            
000800*    MATCHA TVÅ FILER OCH SELECTERA UT HÄNDELSE 102-121                   
000810*    AND 102-131                                                          
000900     EJECT                                                                
001000 ENVIRONMENT DIVISION.                                                    
001100                                                                          
001200 INPUT-OUTPUT SECTION.                                                    
001300 FILE-CONTROL.                                                            
001400                                                                          
001500     SELECT W5104A  ASSIGN       TO W5706BD1.                             
001600                                                                          
001700     SELECT W57061A ASSIGN       TO W5706BD2.                             
001800                                                                          
001900     SELECT W57061  ASSIGN       TO W5706BD3.                             
002000                                                                          
002100     SELECT W5706A  ASSIGN       TO W5706BD4.                             
002200                                                                          
002300     EJECT                                                                
002400 DATA DIVISION.                                                           
002500 FILE SECTION.                                                            
002600                                                                          
002700 FD  W5104A                                                               
002800     RECORDING F                                                          
002900     BLOCK CONTAINS 0.                                                    
003000                                                                          
003100 01  INPOST1.                                                             
003200*    03   -COPY W57060    -L                                              
003300     SKIP2                                                                
003400                                                                          
003500 FD  W57061A                                                              
003600     RECORDING F                                                          
003700     BLOCK CONTAINS 0.                                                    
003800                                                                          
003900 01  INPOST2.                                                             
004000*    03   -COPY W57060    -L                                              
004100     SKIP2                                                                
004200                                                                          
004300 FD  W57061                                                               
004400     RECORDING F                                                          
004500     BLOCK CONTAINS 0.                                                    
004600                                                                          
004700*01  UTPOST1     -COPY W57060    -L                                       
004800     SKIP2                                                                
004900                                                                          
005000 FD  W5706A                                                               
005100     RECORDING F                                                          
005200     BLOCK CONTAINS 0.                                                    
005300                                                                          
005400*01  UTPOST2     -COPY W57060    -L                                       
005500     SKIP2                                                                
005600                                                                          
005700 WORKING-STORAGE SECTION.                                                 
005800     SKIP3                                                                
005900 77  IDPGM                   PIC X(8)      VALUE 'W5706B00'.              
006000 77  JA                      PIC X         VALUE 'J'.                     
006100 77  NEJ                     PIC X         VALUE 'N'.                     
006200 77  FELTEXT                 PIC X(80).                                   
006300 77  EOF-W5104A              PIC X         VALUE 'N'.                     
006400 77  EOF-W57061A             PIC X         VALUE 'N'.                     
006500 77  WS-IX                   PIC S9(3)   COMP-3 VALUE ZERO.               
006600 01  WS-VARIABLES.                                                        
006700     03  W-PRKURS            PIC S9(5)V9(2) VALUE +0   COMP-3.            
006800     03  W-REVALUTA          PIC S9(3)      VALUE +0   COMP-3.            
006900 01  W-DATE                  PIC 9(6)    VALUE ZERO.                      
007000 01  FILLER REDEFINES W-DATE.                                             
008000     03  W-DATE-AAMM         PIC 9(4).                                    
008100     03  W-DATE-DD           PIC 9(2).                                    
008200                                                                          
008300 01  SUBPROGRAM.                                                          
008400     03  ABEND               PIC X(8)    VALUE 'ABEND'.                   
008500     03  POSTSUM             PIC X(8)    VALUE 'POSTSUM'.                 
008600     03  CBLTDLI             PIC X(8)    VALUE 'CBLTDLI '.                
008700     03  FELLOG              PIC X(8)    VALUE 'FELLOG  '.                
008800                                                                          
010800*    --- PARAMETRAR TILL ABEND                                            
010900 77  RKOD-ABEND                   PIC S9(4)   COMP VALUE +0.              
011000 77  RKOD-ABEND-UTAN-DUMP         PIC S9(4)   COMP VALUE +16.             
011100 77  RKOD-ABEND-MED-DUMP          PIC S9(4)   COMP VALUE +1000.           
011200     EJECT                                                                
011300                                                                          
011400 01  W5104A-TRANSID.                                                      
011500     03  FILLER              PIC X(6) VALUE 'W5104A'.                     
011600     03  FILLER              PIC X(8) VALUE 'W5706BD1'.                   
011700     03  FILLER              PIC X(4) VALUE ' IN1'.                       
011800                                                                          
011900 01  W57061A-TRANSID.                                                     
012000     03  FILLER              PIC X(6) VALUE 'W57061'.                     
012100     03  FILLER              PIC X(8) VALUE 'W5706BD2'.                   
012200     03  FILLER              PIC X(4) VALUE ' IN2'.                       
012300                                                                          
012400 01  W57061-TRANSID.                                                      
012500     03  FILLER              PIC X(6) VALUE 'W57061'.                     
012600     03  FILLER              PIC X(8) VALUE 'W5706BD3'.                   
012700     03  FILLER              PIC X(4) VALUE ' UT1'.                       
012800                                                                          
012900 01  W5706A-TRANSID.                                                      
013000     03  FILLER              PIC X(6) VALUE 'W5706A'.                     
013100     03  FILLER              PIC X(8) VALUE 'W5706BD4'.                   
013200     03  FILLER              PIC X(4) VALUE ' UT2'.                       
013300                                                                          
013400     EJECT                                                                
013500*   -COPY W0005  -PRE POSTSUM-                                            
013600     EJECT                                                                
013700                                                                          
013800 01  FILLER                  PIC X(16)   VALUE 'IN-AREA1   '.             
013900 01  INAREA1.                                                             
014000*    03  -COPY W57060 -PRE IN1-                                           
014100     EJECT                                                                
014200                                                                          
014300 01  FILLER                  PIC X(16)   VALUE 'IN-AREA2   '.             
014400 01  INAREA2.                                                             
014500*    03  -COPY W57060 -PRE IN2-                                           
014600     EJECT                                                                
014700                                                                          
014800 01  FILLER                  PIC X(16)   VALUE 'UT-AREA1    '.            
014900 01  UTAREA1.                                                             
015000*    03  -COPY W57060 -PRE UT1-                                           
015100     EJECT                                                                
015200                                                                          
015300 01  FILLER                  PIC X(16)   VALUE 'UT-AREA2    '.            
015400 01  UTAREA2.                                                             
015500*    03  -COPY W57060 -PRE UT2-                                           
015600     EJECT                                                                
015700                                                                          
018200 LINKAGE SECTION.                                                         
018300                                                                          
018400 PROCEDURE DIVISION.                                                      
018500 MAIN SECTION.                                                            
018600                                                                          
018700     PERFORM A-INIT                                                       
018800                                                                          
018900     PERFORM S01-READ-W5104A-POST                                         
019000     PERFORM S02-READ-W57061A-POST                                        
019100                                                                          
019200     PERFORM UNTIL  EOF-W5104A = JA AND EOF-W57061A = JA                  
019300       PERFORM B-CHECK-MATCH                                              
019400     END-PERFORM                                                          
019500                                                                          
019600     PERFORM Z-END                                                        
019700                                                                          
019800     MOVE ZERO TO RETURN-CODE                                             
019900     GOBACK                                                               
020000     .                                                                    
020100     EJECT                                                                
020200                                                                          
020300 A-INIT SECTION.                                                          
020400     OPEN INPUT  W5104A                                                   
020500                 W57061A                                                  
020600          OUTPUT W57061                                                   
020700                 W5706A                                                   
020800                                                                          
020900     MOVE IDPGM                       TO POSTSUM-PROGNAMN                 
021000     .                                                                    
021100     EJECT                                                                
021200                                                                          
021300 B-CHECK-MATCH SECTION.                                                   
021400     PERFORM UNTIL  EOF-W57061A = JA                                      
021500       IF IN1-EKHT-IDVERGL < IN2-EKHT-IDVERGL                             
021600       AND EOF-W5104A = NEJ                                               
021700         PERFORM S04-CREATE-FILE-W5706A                                   
021800         PERFORM S01-READ-W5104A-POST                                     
021900       ELSE                                                               
022000                                                                          
022100         IF IN1-EKHT-IDVERGL = IN2-EKHT-IDVERGL                           
022200         AND EOF-W5104A = NEJ                                             
022300           IF (IN2-EKHT-KDEKHHT = '102'                                   
022400           AND IN2-EKHT-KDEKSHT = '121')                                  
022410           OR (IN2-EKHT-KDEKHHT = '102'                                   
022420           AND IN2-EKHT-KDEKSHT = '131')                                  
022500             PERFORM UNTIL (IN1-EKHT-IDVERGL NOT =                        
022600                           IN2-EKHT-IDVERGL)                              
022700                     OR EOF-W5104A = JA                                   
022800               PERFORM S03-CREATE-FILE-W57061-102                         
022900               PERFORM S01-READ-W5104A-POST                               
023000             END-PERFORM                                                  
023100           ELSE                                                           
023200             PERFORM S04-CREATE-FILE-W5706A                               
023300             PERFORM S01-READ-W5104A-POST                                 
023400           END-IF                                                         
023500         ELSE                                                             
023600                                                                          
023700           IF IN1-EKHT-IDVERGL > IN2-EKHT-IDVERGL                         
023800           AND EOF-W5104A = NEJ                                           
023900             PERFORM S03-CREATE-FILE-W57061                               
024000             PERFORM S02-READ-W57061A-POST                                
024100           ELSE                                                           
024200                                                                          
024300             IF EOF-W5104A = JA                                           
024400             AND EOF-W57061A = NEJ                                        
024500               PERFORM S03-CREATE-FILE-W57061                             
024600               PERFORM S02-READ-W57061A-POST                              
024700             END-IF                                                       
024800           END-IF                                                         
024900         END-IF                                                           
025000       END-IF                                                             
025100     END-PERFORM                                                          
025200                                                                          
025300     IF EOF-W57061A = JA                                                  
025400     AND EOF-W5104A = NEJ                                                 
025500       PERFORM UNTIL  EOF-W5104A = JA                                     
025600         PERFORM S04-CREATE-FILE-W5706A                                   
025700         PERFORM S01-READ-W5104A-POST                                     
025800       END-PERFORM                                                        
025900     END-IF                                                               
026000     .                                                                    
026100     EJECT                                                                
026200                                                                          
026300 Z-END SECTION.                                                           
026400     CLOSE W5104A                                                         
026500           W57061A                                                        
026600           W57061                                                         
026700           W5706A                                                         
026800                                                                          
026900     MOVE 'S'        TO POSTSUM-OPKOD                                     
027000     CALL POSTSUM USING POSTSUM-PARM                                      
027100     .                                                                    
027200     EJECT                                                                
027300                                                                          
027400 S01-READ-W5104A-POST SECTION.                                            
027500     READ W5104A INTO INAREA1                                             
027600     AT END                                                               
027700       MOVE JA TO EOF-W5104A                                              
027800     NOT AT END                                                           
027900       MOVE W5104A-TRANSID TO POSTSUM-TRANSID                             
028000       CALL POSTSUM USING POSTSUM-PARM                                    
028100     END-READ                                                             
028200     .                                                                    
028300     SKIP2                                                                
028400                                                                          
028500 S02-READ-W57061A-POST SECTION.                                           
028600     READ W57061A INTO INAREA2                                            
028700     AT END                                                               
028800       MOVE JA TO EOF-W57061A                                             
028900     NOT AT END                                                           
029000       MOVE W57061-TRANSID TO POSTSUM-TRANSID                             
029100       CALL POSTSUM USING POSTSUM-PARM                                    
029200     END-READ                                                             
029300     .                                                                    
029400     SKIP2                                                                
029500                                                                          
029600 S03-CREATE-FILE-W57061 SECTION.                                          
029700     MOVE INAREA2 TO UTAREA1                                              
029800     WRITE UTPOST1 FROM UTAREA1                                           
029900     MOVE W57061-TRANSID TO POSTSUM-TRANSID                               
030000     CALL POSTSUM USING POSTSUM-PARM                                      
030100     .                                                                    
030200     EJECT                                                                
030300                                                                          
030400 S03-CREATE-FILE-W57061-102 SECTION.                                      
030500     MOVE INAREA1 TO UTAREA1                                              
030600     MOVE IN2-EKHT-IDPGM         TO UT1-EKHT-IDPGM                        
030700     MOVE IN2-EKHT-TIREGDAT      TO UT1-EKHT-TIREGDAT                     
030800     MOVE IN2-EKHT-TIKLOCK       TO UT1-EKHT-TIKLOCK                      
030900     MOVE IN2-EKHT-IDCPYTXT      TO UT1-EKHT-IDCPYTXT                     
031000     MOVE IN2-EKHT-DAVERDAT      TO UT1-EKHT-DAVERDAT                     
031100     MOVE IN2-EKHT-KDEKSHT       TO UT1-EKHT-KDEKSHT                      
031200     MOVE IN2-EKHT-IDFAKT-EXP    TO UT1-EKHT-IDFAKT-EXP                   
031210     MOVE IN2-EKHT-CMD           TO UT1-EKHT-CMD                          
031300     WRITE UTPOST1 FROM UTAREA1                                           
031400     MOVE W57061-TRANSID TO POSTSUM-TRANSID                               
031500     CALL POSTSUM USING POSTSUM-PARM                                      
031600     .                                                                    
031700     EJECT                                                                
031800                                                                          
031900 S04-CREATE-FILE-W5706A SECTION.                                          
032000     MOVE INAREA1 TO UTAREA2                                              
032100     WRITE UTPOST2 FROM UTAREA2                                           
032200     MOVE W57061-TRANSID TO POSTSUM-TRANSID                               
032300     CALL POSTSUM USING POSTSUM-PARM                                      
032400     .                                                                    
032500     EJECT                                                                
