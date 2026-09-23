000100 ID DIVISION.                                                             
000200 PROGRAM-ID.    W5156B00.                                                 
000300                                                                          
000400*    AUTHOR.        HÅKAN BOHLIN.                                         
000500*    DATE-WRITTEN   AUGUSTI 2017.                                         
000600*                                                                         
000700*    FUNKTION:                                                            
000800*    MATCHA TVÅ FILER OCH SELEKTERAR UT HÄNDELSE 102-121                  
000900     EJECT                                                                
001000 ENVIRONMENT DIVISION.                                                    
001100                                                                          
001200 INPUT-OUTPUT SECTION.                                                    
001300 FILE-CONTROL.                                                            
001400                                                                          
001500     SELECT W5104A  ASSIGN       TO W5156BD1.                             
001600                                                                          
001700     SELECT W51561A ASSIGN       TO W5156BD2.                             
001800                                                                          
001900     SELECT W51561  ASSIGN       TO W5156BD3.                             
002000                                                                          
002100     SELECT W5156A  ASSIGN       TO W5156BD4.                             
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
003200*    03   -COPY W51560    -L                                              
003300     SKIP2                                                                
003400                                                                          
003500 FD  W51561A                                                              
003600     RECORDING F                                                          
003700     BLOCK CONTAINS 0.                                                    
003800                                                                          
003900 01  INPOST2.                                                             
004000*    03   -COPY W51560    -L                                              
004100     SKIP2                                                                
004200                                                                          
004300 FD  W51561                                                               
004400     RECORDING F                                                          
004500     BLOCK CONTAINS 0.                                                    
004600                                                                          
004700*01  UTPOST1     -COPY W51560    -L                                       
004800     SKIP2                                                                
004900                                                                          
005000 FD  W5156A                                                               
005100     RECORDING F                                                          
005200     BLOCK CONTAINS 0.                                                    
005300                                                                          
005400*01  UTPOST2     -COPY W51560    -L                                       
005500     SKIP2                                                                
005600                                                                          
005700 WORKING-STORAGE SECTION.                                                 
005800     SKIP3                                                                
005900 77  IDPGM                   PIC X(8)      VALUE 'W5156B00'.              
006000 77  JA                      PIC X         VALUE 'J'.                     
006100 77  NEJ                     PIC X         VALUE 'N'.                     
006200 77  FELTEXT                 PIC X(80).                                   
006300 77  EOF-W5104A              PIC X         VALUE 'N'.                     
006400 77  EOF-W51561A             PIC X         VALUE 'N'.                     
006500 77  WS-IX                   PIC S9(3)   COMP-3 VALUE ZERO.               
006600 77  W-DATE-AAMM             PIC 9(4)    VALUE ZERO.                      
006700 77  WS-KDVALISO-HUV         PIC X(3)    VALUE 'SEK'.                     
006800                                                                          
006900 01  SUBPROGRAM.                                                          
007000     03  ABEND               PIC X(8)    VALUE 'ABEND'.                   
007100     03  POSTSUM             PIC X(8)    VALUE 'POSTSUM'.                 
007200     03  CBLTDLI             PIC X(8)    VALUE 'CBLTDLI '.                
007300     03  FELLOG              PIC X(8)    VALUE 'FELLOG  '.                
007400     03  W510CURR            PIC X(8)    VALUE 'W510CURR'.                
007500                                                                          
007600                                                                          
007700*    --- PARAMETRAR TILL ABEND                                            
007800 77  RKOD-ABEND                   PIC S9(4)   COMP VALUE +0.              
007900 77  RKOD-ABEND-UTAN-DUMP         PIC S9(4)   COMP VALUE +16.             
008000 77  RKOD-ABEND-MED-DUMP          PIC S9(4)   COMP VALUE +1000.           
008100     EJECT                                                                
008200                                                                          
008300 01  W5104A-TRANSID.                                                      
008400     03  FILLER              PIC X(6) VALUE 'W5104A'.                     
008500     03  FILLER              PIC X(8) VALUE 'W5156BD1'.                   
008600     03  FILLER              PIC X(4) VALUE ' IN1'.                       
008700                                                                          
008800 01  W51561A-TRANSID.                                                     
008900     03  FILLER              PIC X(6) VALUE 'W51561'.                     
009000     03  FILLER              PIC X(8) VALUE 'W5156BD2'.                   
009100     03  FILLER              PIC X(4) VALUE ' IN2'.                       
009200                                                                          
009300 01  W51561-TRANSID.                                                      
009400     03  FILLER              PIC X(6) VALUE 'W51561'.                     
009500     03  FILLER              PIC X(8) VALUE 'W5156BD3'.                   
009600     03  FILLER              PIC X(4) VALUE ' UT1'.                       
009700                                                                          
009800 01  W5156A-TRANSID.                                                      
009900     03  FILLER              PIC X(6) VALUE 'W5156A'.                     
010000     03  FILLER              PIC X(8) VALUE 'W5156BD4'.                   
010100     03  FILLER              PIC X(4) VALUE ' UT2'.                       
010200                                                                          
010300     EJECT                                                                
010400*   -COPY W0005  -PRE POSTSUM-                                            
010500     EJECT                                                                
010600                                                                          
010700*   -COPY W510CURR                                                        
010800     EJECT                                                                
010900                                                                          
011000 01  FILLER                  PIC X(16)   VALUE 'IN-AREA1   '.             
011100 01  INAREA1.                                                             
011200*    03  -COPY W51560 -PRE IN1-                                           
011300     EJECT                                                                
011400                                                                          
011500 01  FILLER                  PIC X(16)   VALUE 'IN-AREA2   '.             
011600 01  INAREA2.                                                             
011700*    03  -COPY W51560 -PRE IN2-                                           
011800     EJECT                                                                
011900                                                                          
012000 01  FILLER                  PIC X(16)   VALUE 'UT-AREA1    '.            
012100 01  UTAREA1.                                                             
012200*    03  -COPY W51560 -PRE UT1-                                           
012300     EJECT                                                                
012400                                                                          
012500 01  FILLER                  PIC X(16)   VALUE 'UT-AREA2    '.            
012600 01  UTAREA2.                                                             
012700*    03  -COPY W51560 -PRE UT2-                                           
012800     EJECT                                                                
012900                                                                          
013000 LINKAGE SECTION.                                                         
013100*01  -COPY W0008  -PRE WDG2-                                              
013200     05  FILLER                  PIC X.                                   
013300                                                                          
013400 PROCEDURE DIVISION  USING WDG2-PCB.                                      
013500 MAIN SECTION.                                                            
013600     ENTRY 'DLITCBL' USING WDG2-PCB.                                      
013700                                                                          
013800     PERFORM A-INIT                                                       
013900                                                                          
014000     PERFORM S01-READ-W5104A-POST                                         
014100     PERFORM S02-READ-W51561A-POST                                        
014200                                                                          
014300     PERFORM UNTIL  EOF-W5104A = JA AND EOF-W51561A = JA                  
014400       PERFORM B-CHECK-MATCH                                              
014500     END-PERFORM                                                          
014600                                                                          
014700     PERFORM Z-END                                                        
014800                                                                          
014900     MOVE ZERO TO RETURN-CODE                                             
015000     GOBACK                                                               
015100     .                                                                    
015200     EJECT                                                                
015300                                                                          
015400 A-INIT SECTION.                                                          
015500     OPEN INPUT  W5104A                                                   
015600                 W51561A                                                  
015700          OUTPUT W51561                                                   
015800                 W5156A                                                   
015900                                                                          
016000     MOVE IDPGM           TO POSTSUM-PROGNAMN                             
016100     .                                                                    
016200     EJECT                                                                
016300                                                                          
016400 B-CHECK-MATCH SECTION.                                                   
016500     PERFORM UNTIL  EOF-W51561A = JA                                      
016600       IF IN1-EKHT-IDVERGL < IN2-EKHT-IDVERGL                             
016700       AND EOF-W5104A = NEJ                                               
016800         PERFORM S04-CREATE-FILE-W5156A                                   
016900         PERFORM S01-READ-W5104A-POST                                     
017000       ELSE                                                               
017100                                                                          
017200         IF IN1-EKHT-IDVERGL = IN2-EKHT-IDVERGL                           
017300         AND EOF-W5104A = NEJ                                             
017400           IF IN2-EKHT-KDEKHHT = '102'                                    
017500           AND (IN2-EKHT-KDEKSHT = '121'                                  
017600           OR   IN2-EKHT-KDEKSHT = '131')                                 
017700             PERFORM UNTIL (IN1-EKHT-IDVERGL NOT =                        
017800                           IN2-EKHT-IDVERGL)                              
017900                     OR EOF-W5104A = JA                                   
018000               PERFORM S10-GET-CURRENCY-RATE                              
018100               PERFORM S03-CREATE-FILE-W51561-102                         
018200               PERFORM S01-READ-W5104A-POST                               
018300             END-PERFORM                                                  
018400           ELSE                                                           
018500             PERFORM S04-CREATE-FILE-W5156A                               
018600             PERFORM S01-READ-W5104A-POST                                 
018700           END-IF                                                         
018800         ELSE                                                             
018900                                                                          
019000           IF IN1-EKHT-IDVERGL > IN2-EKHT-IDVERGL                         
019100           AND EOF-W5104A = NEJ                                           
019200             PERFORM S03-CREATE-FILE-W51561                               
019300             PERFORM S02-READ-W51561A-POST                                
019400           ELSE                                                           
019500                                                                          
019600             IF EOF-W5104A = JA                                           
019700             AND EOF-W51561A = NEJ                                        
019800               PERFORM S03-CREATE-FILE-W51561                             
019900               PERFORM S02-READ-W51561A-POST                              
020000             END-IF                                                       
020100           END-IF                                                         
020200         END-IF                                                           
020300       END-IF                                                             
020400     END-PERFORM                                                          
020500                                                                          
020600     IF EOF-W51561A = JA                                                  
020700     AND EOF-W5104A = NEJ                                                 
020800       PERFORM UNTIL  EOF-W5104A = JA                                     
020900         PERFORM S04-CREATE-FILE-W5156A                                   
021000         PERFORM S01-READ-W5104A-POST                                     
021100       END-PERFORM                                                        
021200     END-IF                                                               
021300     .                                                                    
021400     EJECT                                                                
021500                                                                          
021600 Z-END SECTION.                                                           
021700     CLOSE W5104A                                                         
021800           W51561A                                                        
021900           W51561                                                         
022000           W5156A                                                         
022100                                                                          
022200     MOVE 'S'        TO POSTSUM-OPKOD                                     
022300     CALL POSTSUM USING POSTSUM-PARM                                      
022400     .                                                                    
022500     EJECT                                                                
022600                                                                          
022700 S01-READ-W5104A-POST SECTION.                                            
022800     READ W5104A INTO INAREA1                                             
022900     AT END                                                               
023000       MOVE JA TO EOF-W5104A                                              
023100     NOT AT END                                                           
023200       MOVE W5104A-TRANSID TO POSTSUM-TRANSID                             
023300       CALL POSTSUM USING POSTSUM-PARM                                    
023400     END-READ                                                             
023500     .                                                                    
023600     SKIP2                                                                
023700                                                                          
023800 S02-READ-W51561A-POST SECTION.                                           
023900     READ W51561A INTO INAREA2                                            
024000     AT END                                                               
024100       MOVE JA TO EOF-W51561A                                             
024200     NOT AT END                                                           
024300       MOVE W51561-TRANSID TO POSTSUM-TRANSID                             
024400       CALL POSTSUM USING POSTSUM-PARM                                    
024500     END-READ                                                             
024600     .                                                                    
024700     SKIP2                                                                
024800                                                                          
024900 S03-CREATE-FILE-W51561 SECTION.                                          
025000     MOVE INAREA2 TO UTAREA1                                              
025100     WRITE UTPOST1 FROM UTAREA1                                           
025200     MOVE W51561-TRANSID TO POSTSUM-TRANSID                               
025300     CALL POSTSUM USING POSTSUM-PARM                                      
025400     .                                                                    
025500     EJECT                                                                
025600                                                                          
025700 S03-CREATE-FILE-W51561-102 SECTION.                                      
025800     MOVE INAREA1 TO UTAREA1                                              
025900     MOVE IN2-EKHT-IDPGM         TO UT1-EKHT-IDPGM                        
026000     MOVE IN2-EKHT-TIREGDAT      TO UT1-EKHT-TIREGDAT                     
026100     MOVE IN2-EKHT-TIKLOCK       TO UT1-EKHT-TIKLOCK                      
026200     MOVE IN2-EKHT-IDCPYTXT      TO UT1-EKHT-IDCPYTXT                     
026300     MOVE IN2-EKHT-DAVERDAT      TO UT1-EKHT-DAVERDAT                     
026400     MOVE IN2-EKHT-KDEKSHT       TO UT1-EKHT-KDEKSHT                      
026500     WRITE UTPOST1 FROM UTAREA1                                           
026600     MOVE W51561-TRANSID TO POSTSUM-TRANSID                               
026700     CALL POSTSUM USING POSTSUM-PARM                                      
026800     .                                                                    
026900     EJECT                                                                
027000                                                                          
027100 S04-CREATE-FILE-W5156A SECTION.                                          
027200     MOVE INAREA1 TO UTAREA2                                              
027300     WRITE UTPOST2 FROM UTAREA2                                           
027400     MOVE W51561-TRANSID TO POSTSUM-TRANSID                               
027500     CALL POSTSUM USING POSTSUM-PARM                                      
027600     .                                                                    
027700     EJECT                                                                
027800 S10-GET-CURRENCY-RATE  SECTION.                                          
027900     MOVE IN1-EKHT-DAVERDAT(3:2)   TO W-DATE-AAMM(1:2)                    
028000     MOVE IN1-EKHT-DAVERDAT(5:2)   TO W-DATE-AAMM(3:2)                    
028100     MOVE W-DATE-AAMM              TO CURR-TIAAMM                         
028200     MOVE WS-KDVALISO-HUV          TO CURR-KDVALISO-HUV                   
028300     MOVE IN1-EKHT-KDVALISO        TO CURR-KDVALISO-ROW                   
028400     MOVE 'M'                      TO CURR-KDVALTYP                       
028500                                                                          
028600     CALL W510CURR USING CURR-W510CURR WDG2-PCB                           
028700     IF CURR-KDSVAR = ' '                                                 
028800        COMPUTE IN1-EKHT-SUBEL   ROUNDED =                                
028900                   IN1-EKHT-SUBEL / CURR-PRKURS-NEW                       
029000     END-IF                                                               
029100     .                                                                    
029200     EJECT                                                                
029300                                                                          
