000100 ID DIVISION.                                                             
000200 PROGRAM-ID.    W5106B00.                                                 
000300                                                                          
000400*    AUTHOR.        ANDERS HENRIKSSON.                                    
000500*    DATE-WRITTEN   APRIL   2016.                                         
000600*                                                                         
000700*    FUNKTION:                                                            
000800*    MATCHA TVÅ FILER OCH SELECTERA UT HÄNDELSE 102-121                   
000900     EJECT                                                                
001000 ENVIRONMENT DIVISION.                                                    
001100                                                                          
001200 INPUT-OUTPUT SECTION.                                                    
001300 FILE-CONTROL.                                                            
001400                                                                          
001500     SELECT W51061C ASSIGN       TO W5106BD1.                             
001600                                                                          
001700     SELECT W51061A ASSIGN       TO W5106BD2.                             
001800                                                                          
001900     SELECT W51061  ASSIGN       TO W5106BD3.                             
002000                                                                          
002100     SELECT W5106A  ASSIGN       TO W5106BD4.                             
002200                                                                          
002300     EJECT                                                                
002400 DATA DIVISION.                                                           
002500 FILE SECTION.                                                            
002600                                                                          
002700 FD  W51061C                                                              
002800     RECORDING F                                                          
002900     BLOCK CONTAINS 0.                                                    
003000                                                                          
003100 01  INPOST1.                                                             
003200*    03   -COPY WDR901    -L                                              
003300     SKIP2                                                                
003400                                                                          
003500 FD  W51061A                                                              
003600     RECORDING F                                                          
003700     BLOCK CONTAINS 0.                                                    
003800                                                                          
003900 01  INPOST2.                                                             
004000*    03   -COPY WDR901    -L                                              
004100     SKIP2                                                                
004200                                                                          
004300 FD  W51061                                                               
004400     RECORDING F                                                          
004500     BLOCK CONTAINS 0.                                                    
004600                                                                          
004700*01  UTPOST1     -COPY WDR901    -L                                       
004800     SKIP2                                                                
004900                                                                          
005000 FD  W5106A                                                               
005100     RECORDING F                                                          
005200     BLOCK CONTAINS 0.                                                    
005300                                                                          
005400*01  UTPOST2     -COPY WDR901    -L                                       
005500     SKIP2                                                                
005600                                                                          
005700 WORKING-STORAGE SECTION.                                                 
005800     SKIP3                                                                
005900 77  IDPGM                   PIC X(8)      VALUE 'W5106B00'.              
006000 77  JA                      PIC X         VALUE 'J'.                     
006100 77  NEJ                     PIC X         VALUE 'N'.                     
006200 77  FELTEXT                 PIC X(80).                                   
006300 77  EOF-W51061C             PIC X         VALUE 'N'.                     
006400 77  EOF-W51061A             PIC X         VALUE 'N'.                     
006500 77  WS-IX                   PIC S9(3)   COMP-3 VALUE ZERO.               
006510 77  W-DATE-AAMM             PIC 9(4)    VALUE ZERO.                      
006520 77  WS-KDVALISO-HUV         PIC X(3)    VALUE 'SEK'.                     
006600                                                                          
006700 01  SUBPROGRAM.                                                          
006800     03  ABEND               PIC X(8)    VALUE 'ABEND'.                   
006900     03  POSTSUM             PIC X(8)    VALUE 'POSTSUM'.                 
007000     03  CBLTDLI             PIC X(8)    VALUE 'CBLTDLI '.                
007100     03  FELLOG              PIC X(8)    VALUE 'FELLOG  '.                
007110     03  W510CURR            PIC X(8)    VALUE 'W510CURR'.                
007200                                                                          
008300*    --- PARAMETRAR TILL ABEND                                            
008400 77  RKOD-ABEND                   PIC S9(4)   COMP VALUE +0.              
008500 77  RKOD-ABEND-UTAN-DUMP         PIC S9(4)   COMP VALUE +16.             
008600 77  RKOD-ABEND-MED-DUMP          PIC S9(4)   COMP VALUE +1000.           
008700     EJECT                                                                
008800                                                                          
008900 01  W51061C-TRANSID.                                                     
009000     03  FILLER              PIC X(6) VALUE 'W51061'.                     
009100     03  FILLER              PIC X(8) VALUE 'W5106BD1'.                   
009200     03  FILLER              PIC X(4) VALUE ' IN1'.                       
009300                                                                          
009400 01  W51061A-TRANSID.                                                     
009500     03  FILLER              PIC X(6) VALUE 'W51061'.                     
009600     03  FILLER              PIC X(8) VALUE 'W5106BD2'.                   
009700     03  FILLER              PIC X(4) VALUE ' IN2'.                       
009800                                                                          
009900 01  W51061-TRANSID.                                                      
010000     03  FILLER              PIC X(6) VALUE 'W51061'.                     
010100     03  FILLER              PIC X(8) VALUE 'W5106BD3'.                   
010200     03  FILLER              PIC X(4) VALUE ' UT1'.                       
010300                                                                          
010400 01  W5106A-TRANSID.                                                      
010500     03  FILLER              PIC X(6) VALUE 'W5106A'.                     
010600     03  FILLER              PIC X(8) VALUE 'W5106BD4'.                   
010700     03  FILLER              PIC X(4) VALUE ' UT2'.                       
010800                                                                          
010900     EJECT                                                                
011000*   -COPY W0005  -PRE POSTSUM-                                            
011100     EJECT                                                                
011110*   -COPY W510CURR                                                        
011120     EJECT                                                                
011200                                                                          
011300 01  FILLER                  PIC X(16)   VALUE 'IN-AREA1   '.             
011400 01  INAREA1.                                                             
011500*    03  -COPY WDR901 -PRE IN1-                                           
011510       05   -COPY W510EKHA   -PRE IN1- -RED IN1-FIL-WDR901-DATA           
011600     EJECT                                                                
011700                                                                          
011800 01  FILLER                  PIC X(16)   VALUE 'IN-AREA2   '.             
011900 01  INAREA2.                                                             
012000*    03  -COPY WDR901 -PRE IN2-                                           
012010       05   -COPY W510EKHA   -PRE IN2- -RED IN2-FIL-WDR901-DATA           
012100     EJECT                                                                
012200                                                                          
012300 01  FILLER                  PIC X(16)   VALUE 'UT-AREA1    '.            
012400 01  UTAREA1.                                                             
012500*    03  -COPY WDR901 -PRE UT1-                                           
012510       05   -COPY W510EKHA   -PRE UT1- -RED UT1-FIL-WDR901-DATA           
012600     EJECT                                                                
012700                                                                          
012800 01  FILLER                  PIC X(16)   VALUE 'UT-AREA2    '.            
012900 01  UTAREA2.                                                             
013000*    03  -COPY WDR901 -PRE UT2-                                           
013010       05   -COPY W510EKHA   -PRE UT2- -RED UT2-FIL-WDR901-DATA           
013100     EJECT                                                                
013200                                                                          
015500                                                                          
015600 LINKAGE SECTION.                                                         
015700*01  -COPY W0008  -PRE WDG2-                                              
015800     05  FILLER                  PIC X.                                   
015900                                                                          
016000 PROCEDURE DIVISION  USING WDG2-PCB.                                      
016100 MAIN SECTION.                                                            
016200     ENTRY 'DLITCBL' USING WDG2-PCB.                                      
016300                                                                          
016400     PERFORM A-INIT                                                       
016500                                                                          
016600     PERFORM S01-READ-W51061C-POST                                        
016700     PERFORM S02-READ-W51061A-POST                                        
016800                                                                          
016900     PERFORM UNTIL  EOF-W51061C = JA AND EOF-W51061A = JA                 
017000       PERFORM B-CHECK-MATCH                                              
017100     END-PERFORM                                                          
017200                                                                          
017300     PERFORM Z-END                                                        
017400                                                                          
017500     MOVE ZERO TO RETURN-CODE                                             
017600     GOBACK                                                               
017700     .                                                                    
017800     EJECT                                                                
017900                                                                          
018000 A-INIT SECTION.                                                          
018100     OPEN INPUT  W51061C                                                  
018200                 W51061A                                                  
018300          OUTPUT W51061                                                   
018400                 W5106A                                                   
018500                                                                          
018600     MOVE IDPGM           TO POSTSUM-PROGNAMN                             
018700     .                                                                    
018800     EJECT                                                                
018900                                                                          
019000 B-CHECK-MATCH SECTION.                                                   
019100     PERFORM UNTIL  EOF-W51061A = JA                                      
019200       IF IN1-EKH-IDVERGL < IN2-EKH-IDVERGL                               
019300       AND EOF-W51061C = NEJ                                              
019400         PERFORM S04-CREATE-FILE-W5106A                                   
019500         PERFORM S01-READ-W51061C-POST                                    
019600       ELSE                                                               
019700                                                                          
019800         IF IN1-EKH-IDVERGL = IN2-EKH-IDVERGL                             
019900         AND EOF-W51061C = NEJ                                            
020000           IF IN2-EKH-KDEKHHT = '102'                                     
020100           AND IN2-EKH-KDEKSHT = '121'                                    
020200             PERFORM UNTIL (IN1-EKH-IDVERGL NOT =                         
020300                           IN2-EKH-IDVERGL)                               
020400                     OR EOF-W51061C = JA                                  
020500               PERFORM S10-GET-CURRENCY-RATE                              
020600               MOVE 'SEK' TO IN1-EKH-KDVALISO                             
020700               PERFORM S03-CREATE-FILE-W51061-102                         
020800               PERFORM S01-READ-W51061C-POST                              
020900             END-PERFORM                                                  
021000           ELSE                                                           
021100             PERFORM S04-CREATE-FILE-W5106A                               
021200             PERFORM S01-READ-W51061C-POST                                
021300           END-IF                                                         
021400         ELSE                                                             
021500                                                                          
021600           IF IN1-EKH-IDVERGL > IN2-EKH-IDVERGL                           
021700           AND EOF-W51061C = NEJ                                          
021800             PERFORM S03-CREATE-FILE-W51061                               
021900             PERFORM S02-READ-W51061A-POST                                
022000           ELSE                                                           
022100                                                                          
022200             IF EOF-W51061C = JA                                          
022300             AND EOF-W51061A = NEJ                                        
022400               PERFORM S03-CREATE-FILE-W51061                             
022500               PERFORM S02-READ-W51061A-POST                              
022600             END-IF                                                       
022700           END-IF                                                         
022800         END-IF                                                           
022900       END-IF                                                             
023000     END-PERFORM                                                          
023100                                                                          
023200     IF EOF-W51061A = JA                                                  
023300     AND EOF-W51061C = NEJ                                                
023400       PERFORM UNTIL  EOF-W51061C = JA                                    
023500         PERFORM S04-CREATE-FILE-W5106A                                   
023600         PERFORM S01-READ-W51061C-POST                                    
023700       END-PERFORM                                                        
023800     END-IF                                                               
023900     .                                                                    
024000     EJECT                                                                
024100                                                                          
024200 Z-END SECTION.                                                           
024300     CLOSE W51061C                                                        
024400           W51061A                                                        
024500           W51061                                                         
024600           W5106A                                                         
024700                                                                          
024800     MOVE 'S'        TO POSTSUM-OPKOD                                     
024900     CALL POSTSUM USING POSTSUM-PARM                                      
025000     .                                                                    
025100     EJECT                                                                
025200                                                                          
025300 S01-READ-W51061C-POST SECTION.                                           
025400     READ W51061C INTO INAREA1                                            
025500     AT END                                                               
025600       MOVE JA TO EOF-W51061C                                             
025700     NOT AT END                                                           
025800       MOVE W51061C-TRANSID TO POSTSUM-TRANSID                            
025900       CALL POSTSUM USING POSTSUM-PARM                                    
026000     END-READ                                                             
026100     .                                                                    
026200     SKIP2                                                                
026300                                                                          
026400 S02-READ-W51061A-POST SECTION.                                           
026500     READ W51061A INTO INAREA2                                            
026600     AT END                                                               
026700       MOVE JA TO EOF-W51061A                                             
026800     NOT AT END                                                           
026900       MOVE W51061A-TRANSID TO POSTSUM-TRANSID                            
027000       CALL POSTSUM USING POSTSUM-PARM                                    
027100     END-READ                                                             
027200     .                                                                    
027300     SKIP2                                                                
027400                                                                          
027500 S03-CREATE-FILE-W51061 SECTION.                                          
027600     MOVE INAREA2 TO UTAREA1                                              
027700     WRITE UTPOST1 FROM UTAREA1                                           
027800     MOVE W51061-TRANSID TO POSTSUM-TRANSID                               
027900     CALL POSTSUM USING POSTSUM-PARM                                      
028000     .                                                                    
028100     EJECT                                                                
028200                                                                          
028300 S03-CREATE-FILE-W51061-102 SECTION.                                      
028400     MOVE INAREA1               TO UTAREA1                                
028500     MOVE IN2-FIL-IDPGM         TO UT1-FIL-IDPGM                          
028600     MOVE IN2-FIL-DAREGDAT      TO UT1-FIL-DAREGDAT                       
028700     MOVE IN2-FIL-TIKLOCK       TO UT1-FIL-TIKLOCK                        
028800     MOVE IN2-FIL-IDCPYTXT      TO UT1-FIL-IDCPYTXT                       
028900     MOVE IN2-EKH-DAVERDAT      TO UT1-EKH-DAVERDAT                       
029000     MOVE IN2-EKH-KDEKSHT       TO UT1-EKH-KDEKSHT                        
029100     WRITE UTPOST1 FROM UTAREA1                                           
029200     MOVE W51061-TRANSID        TO POSTSUM-TRANSID                        
029300     CALL POSTSUM USING POSTSUM-PARM                                      
029400     .                                                                    
029500     EJECT                                                                
029600                                                                          
029700 S04-CREATE-FILE-W5106A SECTION.                                          
029800     MOVE INAREA1            TO UTAREA2                                   
029810     MOVE IN1-FIL-IDCPYTXT   TO UT2-FIL-IDCPYTXT                          
029900     WRITE UTPOST2 FROM UTAREA2                                           
030000     MOVE W5106A-TRANSID     TO POSTSUM-TRANSID                           
030100     CALL POSTSUM USING POSTSUM-PARM                                      
030200     .                                                                    
030300     EJECT                                                                
030400 S10-GET-CURRENCY-RATE  SECTION.                                          
030410                                                                          
030500     MOVE IN1-EKH-DAVERDAT(3:2) TO W-DATE-AAMM(1:2)                       
030501     MOVE IN1-EKH-DAVERDAT(5:2) TO W-DATE-AAMM(3:2)                       
030502     MOVE W-DATE-AAMM           TO CURR-TIAAMM                            
030503     MOVE WS-KDVALISO-HUV       TO CURR-KDVALISO-HUV                      
030505     MOVE 'M'                   TO CURR-KDVALTYP                          
030506                                                                          
030510     IF IN1-FIL-IDCPYTXT = 'W570EKHA'                                     
030600       MOVE 'CNY'               TO IN1-EKH-KDVALISO                       
030601       MOVE 'CNY'               TO CURR-KDVALISO-ROW                      
030602       CALL W510CURR USING CURR-W510CURR WDG2-PCB                         
030603       IF CURR-KDSVAR = ' '                                               
030604         COMPUTE IN1-EKH-SUBEL   ROUNDED =                                
030605                 IN1-EKH-SUBEL * CURR-PRKURS-NEW                          
030606       END-IF                                                             
030610     END-IF                                                               
030611                                                                          
030620     IF IN1-FIL-IDCPYTXT = 'W561EKHA'                                     
030630       MOVE 'USD'               TO IN1-EKH-KDVALISO                       
030631       MOVE 'USD'               TO CURR-KDVALISO-ROW                      
030632       CALL W510CURR USING CURR-W510CURR WDG2-PCB                         
030633       IF CURR-KDSVAR = ' '                                               
030634         COMPUTE IN1-EKH-SUBEL   ROUNDED =                                
030635                 IN1-EKH-SUBEL * CURR-PRKURS-NEW                          
030636       END-IF                                                             
030640     END-IF                                                               
034200     .                                                                    
034300     EJECT                                                                
034400                                                                          
