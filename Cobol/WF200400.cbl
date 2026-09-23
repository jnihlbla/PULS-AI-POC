000100 PROCESS DYNAM                                                            
000200*        - THE OPTION ABOVE IS NEEDED TO LINK A BATCH-DB2-PGM             
000400*                                                                         
001150 ID DIVISION.                                                             
001200 PROGRAM-ID.     WF200400.                                                
001300 AUTHOR.         BO HAMMARIN.                                             
001400 DATE-WRITTEN.   APR 2002.                                                
001500 DATE-COMPILED.                                                           
001700                                                                          
001800*   PGM                                                                   
001900*   COMPLETES THE DOCUMENT HEADER WITH                                    
002000*   - MISCELLANEOUS TOTALS                                                
002100*   - MISCELLANEOUS DATA SUCH AS ADDRESSES, PAYMENT TERMS                 
002410*                                                                         
002423*   PGM UPDATES                                                           
002424*   - ROWS IN TABLE T01DHEA                                               
002425*                                                                         
002430*   PGM READS                                                             
002440*   - ROWS IN TABLE T01PROC                                               
002441*   - ROWS IN TABLE T01LSEL                                               
002442*   - ROWS IN TABLE T01SECO                                               
002443*   - ROWS IN TABLE T01RECO                                               
002450*   - ROWS IN TABLE T01FCUS                                               
002451*   - ROWS IN TABLE T01CURR                                               
002452*   - ROWS IN TABLE T01PATE                                               
002460*   - ROWS IN TABLE T01DLIN                                               
002500                                                                          
002700 ENVIRONMENT DIVISION.                                                    
002800                                                                          
002900 INPUT-OUTPUT SECTION.                                                    
003000                                                                          
003011 FILE-CONTROL.                                                            
003012                                                                          
003013 DATA DIVISION.                                                           
003014                                                                          
003015 FILE SECTION.                                                            
003016                                                                          
004000 WORKING-STORAGE SECTION.                                                 
004110*    -- CHECKED BY WY2000                                                 
004200 77  IDPGM                        PIC X(8)   VALUE 'WF200400'.            
004201 77  WS-DAGENS-DATUM              PIC X(8)   VALUE '00000000'.            
004202 77  WS-DAGENS-DATUM-NUM          PIC 9(8).                               
004203 77  WS-FELTEXT                   PIC X(50).                              
004210                                                                          
004230 01  WS-MISC-MULTIFETCH.                                                  
004240     03 WS-DATUM                  PIC X(8)   VALUE SPACE.                 
004250     03 WS-KLOCKAN                PIC 9(10)  VALUE ZERO.                  
004260     03 WS-MX1                    PIC S9(3)  COMP-3.                      
004270     03 WS-MULTIFETCH1            PIC S9(3)  COMP-3.                      
004271     03 WS-MX2                    PIC S9(3)  COMP-3.                      
004272     03 WS-MULTIFETCH2            PIC S9(3)  COMP-3.                      
004400                                                                          
004410 01  WS-MISC-TABELS.                                                      
004418     03  WS-FLRATE           OCCURS 100 PIC X(1).                         
004419     03  WS-FLDIRVAT         OCCURS 100 PIC X(1).                         
004420                                                                          
004421 01  WS-MISCELLANEOUS.                                                    
004422     03  WS-SUNTO-SERV            PIC S9(11)V9(02) COMP-3.                
004423     03  WS-SUBTO-SERV            PIC S9(11)V9(02) COMP-3.                
004424     03  WS-SUNTO-PART            PIC S9(11)V9(02) COMP-3.                
004425     03  WS-SUBTO-PART            PIC S9(11)V9(02) COMP-3.                
004426     03  WS-SUNTO-TOT             PIC S9(11)V9(02) COMP-3.                
004427     03  WS-SUBTO-TOT             PIC S9(11)V9(02) COMP-3.                
004428     03  WS-SUVAT-BILLIT-TOT      PIC S9(11)V9(02) COMP-3.                
004429     03  IND-X                    PIC S9(3)  COMP-3.                      
004430     03  WS-IDLEGSEL              PIC X(4).                               
004431     03  WS-DAEXDAT               PIC X(8).                               
004432     03  WS-TIEXTID               PIC S9(7) COMP-3.                       
004433     03  WS-KDVALISO              PIC X(3).                               
004434     03  WS-IDLANDX3-SEND         PIC X(3).                               
004435     03  WS-IDLEVNR               PIC X(5).                               
004436     03  WS-IDPARTNR              PIC X(9).                               
004437     03  WS-KDFINDOC              PIC X(4).                               
004438     03  WS-FLSOFT                PIC X(1).                               
004439     03  WS-FLFREE                PIC X(1).                               
004440     03  WS-FLPRIV                PIC X(1).                               
004441     03  WS-IDBREAK-1             PIC X(8).                               
004442     03  WS-IDBREAK-2             PIC X(8).                               
004443     03  WS-IDBREAK-2A            PIC X(8).                               
004444     03  WS-DASTADAT-KEY          PIC X(8).                               
004445     03  WS-DASTADAT-CREDIT       PIC X(8).                               
004446     03  WS2-DASTADAT-CREDIT      PIC X(8).                               
004447     03  WS-REVALUTA              PIC S9(5)        COMP-3.                
004448     03  WS-IDSPRAK               PIC X(2).                               
004449     03  WS-KDBETALV              PIC X(4).                               
004450     03  WS-BEBETVIL              PIC X(30).                              
004451     03  WS-BELEGRAD-1            PIC X(35).                              
004452     03  WS-BELEGRAD-2            PIC X(35).                              
004453     03  WS-ADLEG-STREET          PIC X(35).                              
004454     03  WS-ADLEG-BOX             PIC X(10).                              
004455     03  WS-ADLEG-CITY            PIC X(35).                              
004456     03  WS-ADLEG-PCODE           PIC X(10).                              
004457     03  WS-IDLANDX3-LEG          PIC X(3).                               
004458     03  WS-IDTFN-LEG             PIC X(20).                              
004459     03  WS-IDTFX-LEG             PIC X(20).                              
004460     03  WS-IDMAIL-LEG            PIC X(60).                              
004461     03  WS-BECONT-LEG            PIC X(35).                              
004462     03  WS-IDVAT-LEG             PIC X(17).                              
004463     03  WS-IDBG-LEG              PIC X(15).                              
004464     03  WS-IDPG-LEG              PIC X(15).                              
004465     03  WS-BERESPRA-1            PIC X(35).                              
004466     03  WS-BERESPRA-2            PIC X(35).                              
004467     03  WS-ADRESP-STREET         PIC X(35).                              
004468     03  WS-ADRESP-BOX            PIC X(10).                              
004469     03  WS-ADRESP-CITY           PIC X(35).                              
004470     03  WS-ADRESP-PCODE          PIC X(10).                              
004471     03  WS-IDLANDX3-RESP         PIC X(3).                               
004472     03  WS-IDTFN-RESP            PIC X(20).                              
004473     03  WS-IDTFX-RESP            PIC X(20).                              
004474     03  WS-IDMAIL-RESP           PIC X(60).                              
004475     03  WS-BECONT-RESP           PIC X(35).                              
004476     03  WS-IDVAT-RESP            PIC X(17).                              
004477     03  WS-IDBG-RESP             PIC X(15).                              
004478     03  WS-IDPG-RESP             PIC X(15).                              
004479     03  WS-BEBET-NAME1           PIC X(35).                              
004480     03  WS-BEBET-NAME2           PIC X(35).                              
004481     03  WS-ADBET-STREET          PIC X(35).                              
004482     03  WS-ADBET-BOX             PIC X(10).                              
004483     03  WS-ADBET-CITY            PIC X(35).                              
004484     03  WS-ADBET-PCODE           PIC X(10).                              
004485     03  WS-IDLANDX3-BET          PIC X(3).                               
004490     03  WS-IDVAT-BET             PIC X(17) VALUE SPACES.                 
004491     03  WS-KDTRADP               PIC X(4).                               
004492     03  WS-PRKURS                PIC S9(6)V9(5) COMP-3.                  
004494     03  WS-BETEXT-1              PIC X(50).                              
004495     03  WS-BETEXT-2              PIC X(50).                              
004496     03  WS-BETEXT-3              PIC X(50).                              
004497     03  WS-BETEXT-4              PIC X(50).                              
004498     03  WS-IDVAT-AGENT           PIC X(17).                              
004499     03  WS-PRKURS-CREDIT         PIC S9(6)V9(5) COMP-3.                  
004500     03  WS-REVALUTA-CREDIT       PIC S9(5)      COMP-3.                  
004501     03  WS-IDLANDX3-DDGS-REC     PIC X(3).                               
004502     03  WS-BETEXT-1A             PIC X(50).                              
004503     03  WS-BETEXT-2A             PIC X(50).                              
004504     03  WS-BETEXT-3A             PIC X(50).                              
004505     03  WS-BETEXT-4A             PIC X(50).                              
004510     EJECT                                                                
004520                                                                          
004530*01  -COPY WWLANDX2                                                       
004600                                                                          
006000 01  DYNAMISKA-SUBPROGRAM.                                                
006100*                                                                         
006300     03  ABEND                    PIC X(8)   VALUE 'ABEND   '.            
006310                                                                          
006400 01  RKOD-ABEND-DB2               PIC S9(4)  VALUE +998 COMP SYNC.        
006501     EJECT                                                                
006550*                                                                         
010602*        WORK-AREAS FOR DB2-SECTIONS                                      
010603*                                                                         
010604 01  FILLER                       PIC X(16)  VALUE 'PROC-TAB   '.         
010605*01  -COPY T01PROC    -PRE PROC-                                          
010606                                                                          
010607 01  FILLER                       PIC X(16)  VALUE 'DHEA-TAB   '.         
010608*01  -COPY T01DHEAT   -PRE DHEA-                                          
010609                                                                          
010610 01  FILLER                       PIC X(16)  VALUE 'DLIN-TAB   '.         
010611*01  -COPY T01DLINT   -PRE DLIN-                                          
010612                                                                          
010613 01  FILLER                       PIC X(16)  VALUE 'LSEL-TAB   '.         
010614*01  -COPY T01LSEL    -PRE LSEL-                                          
010615                                                                          
010616 01  FILLER                       PIC X(16)  VALUE 'RECO-TAB   '.         
010617*01  -COPY T01SECO    -PRE SECO-                                          
010618                                                                          
010619 01  FILLER                       PIC X(16)  VALUE 'SECO-TAB   '.         
010620*01  -COPY T01RECO    -PRE RECO-                                          
010621                                                                          
010622 01  FILLER                       PIC X(16)  VALUE 'FCUS-TAB   '.         
010623*01  -COPY T01FCUS    -PRE FCUS-                                          
010624                                                                          
010625 01  FILLER                       PIC X(16)  VALUE 'CURR-TAB   '.         
010626*01  -COPY T01CURR    -PRE CURR-                                          
010627                                                                          
010628 01  FILLER                       PIC X(16)  VALUE 'PATE-TAB   '.         
010629*01  -COPY T01PATE    -PRE PATE-                                          
010630     EJECT                                                                
010631                                                                          
010632 01  FILLER                       PIC X(16)  VALUE 'PROC-AREA'.           
010633       EXEC SQL INCLUDE T01PROC  END-EXEC.                                
010634                                                                          
010635 01  FILLER                       PIC X(16)  VALUE 'DHEA-AREA'.           
010636       EXEC SQL INCLUDE T01DHEA  END-EXEC.                                
010637                                                                          
010638 01  FILLER                       PIC X(16)  VALUE 'DLIN-AREA'.           
010639       EXEC SQL INCLUDE T01DLIN  END-EXEC.                                
010640                                                                          
010641 01  FILLER                       PIC X(16)  VALUE 'LSEL-AREA'.           
010642       EXEC SQL INCLUDE T01LSEL  END-EXEC.                                
010643                                                                          
010644 01  FILLER                       PIC X(16)  VALUE 'SECO-AREA'.           
010645       EXEC SQL INCLUDE T01SECO  END-EXEC.                                
010646                                                                          
010647 01  FILLER                       PIC X(16)  VALUE 'RECO-AREA'.           
010648       EXEC SQL INCLUDE T01RECO  END-EXEC.                                
010649                                                                          
010650 01  FILLER                       PIC X(16)  VALUE 'FCUS-AREA'.           
010651       EXEC SQL INCLUDE T01FCUS  END-EXEC.                                
010652                                                                          
010653 01  FILLER                       PIC X(16)  VALUE 'CURR-AREA'.           
010654       EXEC SQL INCLUDE T01CURR  END-EXEC.                                
010655                                                                          
010656 01  FILLER                       PIC X(16)  VALUE 'PATE-AREA'.           
010657       EXEC SQL INCLUDE T01PATE  END-EXEC.                                
010658     EJECT                                                                
010659                                                                          
010660 01  FILLER                       PIC X(16)  VALUE 'SQLCA-AREA'.          
010661       EXEC SQL INCLUDE SQLCA END-EXEC.                                   
010662*                        **** STATUS-CODE FROM DB2                        
010663                                                                          
010664 01  FILLER                       PIC X(16)  VALUE 'SQLCODE-WS'.          
010665 01  DB2-WS.                                                              
010666   03  SQLCODE-WS                 PIC 9(3)   VALUE ZERO.                  
010667     88  ROW-FOUND                           VALUE 000.                   
010668     88  ROW-MISSING                         VALUE 100.                   
010669   03  GOOD-SQLCODES.                                                     
010670     05  GOOD-SQLCODE OCCURS 5                                            
010671         INDEXED BY SQLCODE-IX    PIC 999.                                
010672     EJECT                                                                
010680                                                                          
010717 PROCEDURE DIVISION.                                                      
010718 MAIN SECTION.                                                            
011600     PERFORM B-EXECUTE                                                    
011900                                                                          
012400     PERFORM Z-FINISH                                                     
012600     MOVE ZERO TO RETURN-CODE                                             
012700     GOBACK                                                               
012800     .                                                                    
012900     EJECT                                                                
012910                                                                          
014985 B-EXECUTE SECTION.                                                       
014987     PERFORM DB2-OPEN-CRS-LSEL                                            
014988     PERFORM DB2-FETCH-CRS-LSEL                                           
014989     PERFORM UNTIL ROW-MISSING                                            
014990       PERFORM BA-INIT-AMOUNTS                                            
014991       MOVE FUNCTION CURRENT-DATE (1:8) TO WS-DAGENS-DATUM                
014992       MOVE WS-DAGENS-DATUM             TO WS-DAGENS-DATUM-NUM            
014993       PERFORM DB2-SELECT-PROC                                            
014994       IF PROC-KDBEH = 'P'                                                
014995         SUBTRACT 1 FROM WS-DAGENS-DATUM-NUM                              
014996         MOVE WS-DAGENS-DATUM-NUM       TO WS-DAGENS-DATUM                
014997       END-IF                                                             
014999       PERFORM DB2-SELECT-MAX-CURR                                        
015000       PERFORM DB2-OPEN-CRS-MISC                                          
015001       PERFORM DB2-FETCH-CRS-MISC                                         
015002       IF SQLERRD(3) > 0                                                  
015003         MOVE 000                       TO SQLCODE-WS                     
015004       END-IF                                                             
015005                                                                          
015006       PERFORM UNTIL ROW-MISSING                                          
015007         MOVE SQLERRD(3)                TO WS-MULTIFETCH1                 
015008         MOVE ZERO                      TO WS-MX1                         
015010         PERFORM UNTIL WS-MX1 = WS-MULTIFETCH1                            
015011           ADD +1                       TO WS-MX1                         
015012           PERFORM BA-INIT-AMOUNTS                                        
015013           PERFORM BB-BUILD-KEY                                           
015014           PERFORM DB2-OPEN-CRS-DLIN                                      
015015           PERFORM DB2-FETCH-CRS-DLIN                                     
015016           IF SQLERRD(3) > 0                                              
015017             MOVE 000                 TO SQLCODE-WS                       
015018           END-IF                                                         
015019                                                                          
015021           PERFORM UNTIL ROW-MISSING                                      
015022             MOVE SQLERRD(3)            TO WS-MULTIFETCH2                 
015023             MOVE ZERO                  TO WS-MX2                         
015025             PERFORM UNTIL WS-MX2 = WS-MULTIFETCH2                        
015026               ADD +1                   TO WS-MX2                         
015027               PERFORM BC-ACCUMULATE-AMOUNTS                              
015028               PERFORM BF-CHECK-NI-VAT-REG                                
015029               PERFORM BG-ITALY-VAT-REG-DIRTY-FIX                         
015030               PERFORM BG-ITALY-VAT-REG-DIRTY-CREDIT                      
015034             END-PERFORM                                                  
015035             IF WS-MULTIFETCH2 = 100                                      
015036               PERFORM DB2-FETCH-CRS-DLIN                                 
015037               IF SQLERRD(3) > 0                                          
015038                 MOVE 000                 TO SQLCODE-WS                   
015039               END-IF                                                     
015040             ELSE                                                         
015041               MOVE 100                 TO SQLCODE-WS                     
015042             END-IF                                                       
015043           END-PERFORM                                                    
015045           PERFORM DB2-CLOSE-CRS-DLIN                                     
015046                                                                          
015047           PERFORM BD-CHECK-RATE-CALC                                     
015049           PERFORM BE-BUILD-HEAD                                          
015050           PERFORM BH-CHECK-DDGS-TEXT                                     
015051*          PERFORM BH-CHECK-IPT-TEXT                                      
015052           PERFORM DB2-UPDATE-DHEA                                        
015053         END-PERFORM                                                      
015054         IF WS-MULTIFETCH1 = 100                                          
015055           PERFORM DB2-FETCH-CRS-MISC                                     
015056           IF SQLERRD(3) > 0                                              
015057             MOVE 000                     TO SQLCODE-WS                   
015058           END-IF                                                         
015059         ELSE                                                             
015060           MOVE 100                     TO SQLCODE-WS                     
015061         END-IF                                                           
015062       END-PERFORM                                                        
015063                                                                          
015064       PERFORM DB2-CLOSE-CRS-MISC                                         
015065       PERFORM DB2-FETCH-CRS-LSEL                                         
015070     END-PERFORM                                                          
015103     .                                                                    
015104     EJECT                                                                
015105                                                                          
015106 BA-INIT-AMOUNTS SECTION.                                                 
015109     MOVE ZERO TO WS-SUNTO-SERV                                           
015110                  WS-SUNTO-PART                                           
015111                  WS-SUBTO-SERV                                           
015112                  WS-SUBTO-PART                                           
015120                  WS-SUNTO-TOT                                            
015130                  WS-SUBTO-TOT                                            
015140                  WS-SUVAT-BILLIT-TOT                                     
015202     .                                                                    
015204                                                                          
015205 BB-BUILD-KEY SECTION.                                                    
015207     MOVE DHEA-IDLEGSEL      (WS-MX1) TO WS-IDLEGSEL                      
015208     MOVE DHEA-DAEXDAT       (WS-MX1) TO WS-DAEXDAT                       
015209     MOVE DHEA-TIEXTID       (WS-MX1) TO WS-TIEXTID                       
015210     MOVE DHEA-KDVALISO      (WS-MX1) TO WS-KDVALISO                      
015211     MOVE DHEA-IDLANDX3-SEND (WS-MX1) TO WS-IDLANDX3-SEND                 
015212     MOVE DHEA-IDLEVNR       (WS-MX1) TO WS-IDLEVNR                       
015213     MOVE DHEA-IDPARTNR      (WS-MX1) TO WS-IDPARTNR                      
015214     MOVE DHEA-KDFINDOC      (WS-MX1) TO WS-KDFINDOC                      
015215     MOVE DHEA-FLSOFT        (WS-MX1) TO WS-FLSOFT                        
015216     MOVE DHEA-FLFREE        (WS-MX1) TO WS-FLFREE                        
015217     MOVE DHEA-FLPRIV        (WS-MX1) TO WS-FLPRIV                        
015218     MOVE DHEA-IDBREAK-1     (WS-MX1) TO WS-IDBREAK-1                     
015219     MOVE DHEA-IDBREAK-2     (WS-MX1) TO WS-IDBREAK-2                     
015229     .                                                                    
015230                                                                          
015231 BC-ACCUMULATE-AMOUNTS SECTION.                                           
015232     IF DLIN-IDARTNR-FINANCE (WS-MX2) > SPACE                             
015233       COMPUTE WS-SUNTO-PART          = WS-SUNTO-PART          +          
015234                                        DLIN-SUNTO    (WS-MX2)            
015235       END-COMPUTE                                                        
015236       COMPUTE WS-SUBTO-PART          = WS-SUBTO-PART          +          
015237                                        DLIN-SUBTO    (WS-MX2)            
015238       END-COMPUTE                                                        
015239     ELSE                                                                 
015240       COMPUTE WS-SUNTO-SERV          = WS-SUNTO-SERV          +          
015241                                        DLIN-SUNTO    (WS-MX2)            
015242       END-COMPUTE                                                        
015243       COMPUTE WS-SUBTO-SERV          = WS-SUBTO-SERV          +          
015244                                        DLIN-SUBTO    (WS-MX2)            
015245       END-COMPUTE                                                        
015246     END-IF                                                               
015247                                                                          
015248     COMPUTE WS-SUNTO-TOT             = WS-SUNTO-TOT           +          
015249                                        DLIN-SUNTO    (WS-MX2)            
015250     END-COMPUTE                                                          
015251     .                                                                    
015252                                                                          
015253 BD-CHECK-RATE-CALC SECTION.                                              
015254     IF WS-FLRATE (WS-MX1) = 'J'                                          
015256       MOVE DHEA-IDPARTNR (WS-MX1) TO WS-IDPARTNR                         
015257       IF DHEA-KDFINDOC (WS-MX1) = 'CR'                                   
015258         IF DHEA-IDLEVNR (WS-MX1)(1:4) = '0000'                           
015259         OR DHEA-IDLEVNR (WS-MX1)(1:4) = '    '                           
015260           PERFORM DB2-SELECT-FCUS-CURR                                   
015261           COMPUTE DHEA-PRKURS (WS-MX1) ROUNDED = CURR-PRKURS /           
015262                  (DHEA-PRKURS (WS-MX1) * WS-REVALUTA)                    
015263         ELSE                                                             
015267           MOVE '20' TO WS-DASTADAT-CREDIT(1:2)                           
015268       MOVE DHEA-IDLEVNR (WS-MX1)(1:4) TO WS-DASTADAT-CREDIT(3:4)         
015269           MOVE '01' TO WS-DASTADAT-CREDIT(7:2)                           
015270           PERFORM DB2-SELECT-MAX-CURR-CREDIT                             
015271           PERFORM DB2-SELECT-FCUS-CURR-CREDIT                            
015272           PERFORM DB2-SELECT-CURR-CREDIT                                 
015273           COMPUTE DHEA-PRKURS (WS-MX1) ROUNDED = CURR-PRKURS /           
015274                   (WS-PRKURS-CREDIT * WS-REVALUTA)                       
015275         END-IF                                                           
015276       ELSE                                                               
015277         PERFORM DB2-SELECT-FCUS-CURR                                     
015278         COMPUTE DHEA-PRKURS (WS-MX1) ROUNDED = CURR-PRKURS /             
015279                (DHEA-PRKURS (WS-MX1) * WS-REVALUTA)                      
015280       END-IF                                                             
015281     END-IF                                                               
015282     .                                                                    
015283                                                                          
015284 BE-BUILD-HEAD SECTION.                                                   
015285     MOVE DHEA-IDSPRAK            (WS-MX1) TO WS-IDSPRAK                  
015286     MOVE DHEA-KDBETALV           (WS-MX1) TO WS-KDBETALV                 
015287     MOVE DHEA-BEBETVIL           (WS-MX1) TO WS-BEBETVIL                 
015288     MOVE DHEA-BELEGRAD-1         (WS-MX1) TO WS-BELEGRAD-1               
015289     MOVE DHEA-BELEGRAD-2         (WS-MX1) TO WS-BELEGRAD-2               
015290     MOVE DHEA-ADLEG-STREET       (WS-MX1) TO WS-ADLEG-STREET             
015291     MOVE DHEA-ADLEG-BOX          (WS-MX1) TO WS-ADLEG-BOX                
015292     MOVE DHEA-ADLEG-CITY         (WS-MX1) TO WS-ADLEG-CITY               
015293     MOVE DHEA-ADLEG-PCODE        (WS-MX1) TO WS-ADLEG-PCODE              
015294     MOVE DHEA-IDLANDX3-LEG       (WS-MX1) TO WS-IDLANDX3-LEG             
015295     MOVE DHEA-IDTFN-LEG          (WS-MX1) TO WS-IDTFN-LEG                
015296     MOVE DHEA-IDTFX-LEG          (WS-MX1) TO WS-IDTFX-LEG                
015297     MOVE DHEA-IDMAIL-LEG         (WS-MX1) TO WS-IDMAIL-LEG               
015298     MOVE DHEA-BECONT-LEG         (WS-MX1) TO WS-BECONT-LEG               
015299     MOVE DHEA-IDVAT-LEG          (WS-MX1) TO WS-IDVAT-LEG                
015300     MOVE DHEA-IDBG-LEG           (WS-MX1) TO WS-IDBG-LEG                 
015301     MOVE DHEA-IDPG-LEG           (WS-MX1) TO WS-IDPG-LEG                 
015302     MOVE DHEA-BERESPRA-1         (WS-MX1) TO WS-BERESPRA-1               
015303     MOVE DHEA-BERESPRA-2         (WS-MX1) TO WS-BERESPRA-2               
015304     MOVE DHEA-ADRESP-STREET      (WS-MX1) TO WS-ADRESP-STREET            
015305     MOVE DHEA-ADRESP-BOX         (WS-MX1) TO WS-ADRESP-BOX               
015306     MOVE DHEA-ADRESP-CITY        (WS-MX1) TO WS-ADRESP-CITY              
015307     MOVE DHEA-ADRESP-PCODE       (WS-MX1) TO WS-ADRESP-PCODE             
015308     MOVE DHEA-IDLANDX3-RESP      (WS-MX1) TO WS-IDLANDX3-RESP            
015309     MOVE DHEA-IDTFN-RESP         (WS-MX1) TO WS-IDTFN-RESP               
015310     MOVE DHEA-IDTFX-RESP         (WS-MX1) TO WS-IDTFX-RESP               
015311     MOVE DHEA-IDMAIL-RESP        (WS-MX1) TO WS-IDMAIL-RESP              
015312     MOVE DHEA-BECONT-RESP        (WS-MX1) TO WS-BECONT-RESP              
015313     MOVE DHEA-IDVAT-RESP         (WS-MX1) TO WS-IDVAT-RESP               
015314     MOVE DHEA-IDBG-RESP          (WS-MX1) TO WS-IDBG-RESP                
015315     MOVE DHEA-IDPG-RESP          (WS-MX1) TO WS-IDPG-RESP                
015316     MOVE DHEA-BEBET-NAME1        (WS-MX1) TO WS-BEBET-NAME1              
015317     MOVE DHEA-BEBET-NAME2        (WS-MX1) TO WS-BEBET-NAME2              
015318     MOVE DHEA-ADBET-STREET       (WS-MX1) TO WS-ADBET-STREET             
015319     MOVE DHEA-ADBET-BOX          (WS-MX1) TO WS-ADBET-BOX                
015320     MOVE DHEA-ADBET-PCODE        (WS-MX1) TO WS-ADBET-PCODE              
015321     MOVE DHEA-ADBET-CITY         (WS-MX1) TO WS-ADBET-CITY               
015322     MOVE DHEA-IDLANDX3-BET       (WS-MX1) TO WS-IDLANDX3-BET             
015330     MOVE DHEA-IDVAT-BET          (WS-MX1) TO WS-IDVAT-BET                
015332     MOVE DHEA-KDTRADP            (WS-MX1) TO WS-KDTRADP                  
015333     MOVE DHEA-PRKURS             (WS-MX1) TO WS-PRKURS                   
015334     MOVE DHEA-BETEXT-1           (WS-MX1) TO WS-BETEXT-1                 
015335     MOVE DHEA-BETEXT-2           (WS-MX1) TO WS-BETEXT-2                 
015336     MOVE DHEA-BETEXT-3           (WS-MX1) TO WS-BETEXT-3                 
015337     MOVE DHEA-BETEXT-4           (WS-MX1) TO WS-BETEXT-4                 
015338     MOVE DHEA-IDVAT-AGENT        (WS-MX1) TO WS-IDVAT-AGENT              
015345     .                                                                    
015346                                                                          
015347     EJECT                                                                
015348                                                                          
015349 BF-CHECK-NI-VAT-REG SECTION.                                             
015350**** TIS CUSTOMER OR NORTHERN IRELAND                                     
015351     IF DLIN-IDEXCUST-1(WS-MX2) = '11822004'                              
015352     OR (DLIN-IDEXCUST-1(WS-MX2) = '1378'                                 
015353     AND DLIN-IDEXCUST-2(WS-MX2) = '22004')                               
015354       MOVE 'XI 505345176'         TO DHEA-IDVAT-BET (WS-MX1)             
015360     END-IF                                                               
015361     .                                                                    
015362                                                                          
015364 BG-ITALY-VAT-REG-DIRTY-FIX SECTION.                                      
015366**** ITALY DIRTY FIX FOR VAT REG NO                                       
015367     IF   DLIN-KDFINDOC(WS-MX2)      = 'INV'                              
015368     AND  DLIN-IDLANDX3-SEND(WS-MX2) = 'IT'                               
015372     AND (DLIN-IDEXCUST-1(WS-MX2)    = '1822'                             
015373     OR   DLIN-IDEXCUST-1(WS-MX2)    = '1870'                             
015374     OR   DLIN-IDEXCUST-1(WS-MX2)    = '1871')                            
015375       MOVE 'SE556074308901'       TO DHEA-IDVAT-RESP(WS-MX1)             
015376     END-IF                                                               
015377     .                                                                    
015378                                                                          
015379 BG-ITALY-VAT-REG-DIRTY-CREDIT SECTION.                                   
015380**** ITALY DIRTY FIX FOR VAT REG NO                                       
015390     IF   DLIN-KDFINDOC(WS-MX2)      = 'CR'                               
015391     AND  DLIN-IDLANDX3-SEND(WS-MX2) = 'IT'                               
015392     AND  DLIN-IDLANDX3-REC(WS-MX2)  = 'IT'                               
015393     AND (DLIN-IDEXCUST-1(WS-MX2)    = '1822'                             
015394     OR   DLIN-IDEXCUST-1(WS-MX2)    = '1870'                             
015395     OR   DLIN-IDEXCUST-1(WS-MX2)    = '1871')                            
015396       MOVE 'SE556074308901'       TO DHEA-IDVAT-RESP(WS-MX1)             
015397     END-IF                                                               
015398     .                                                                    
015399     EJECT                                                                
015400                                                                          
015402 BH-CHECK-DDGS-TEXT SECTION.                                              
015403     MOVE DHEA-IDLANDX3-RESP(WS-MX1) TO LANDX2-IDLANDX2                   
015405     IF LANDX2-EU-IDLANDX2                                                
015406       MOVE DHEA-IDLANDX3-SEND(WS-MX1) TO LANDX2-IDLANDX2                 
015408       IF LANDX2-EU-IDLANDX2                                              
015410         IF WS-FLDIRVAT(WS-MX1) = 'J'                                     
015412           IF  DHEA-IDLEVNR(WS-MX1) > ' '                                 
015413           AND  DHEA-KDFINDOC(WS-MX1) = 'INV'                             
015415             MOVE DHEA-IDBREAK-2(WS-MX1) TO WS-IDBREAK-2A                 
015416             MOVE WS-IDBREAK-2A(6:2)     TO LANDX2-IDLANDX2               
015417             IF LANDX2-EU-IDLANDX2                                        
015418               MOVE DHEA-IDLANDX3-RESP(WS-MX1) TO                         
015419                                         WS-IDLANDX3-DDGS-REC             
015420               PERFORM DB2-SELECT-SECO-DDGS-TEXT                          
015421               MOVE WS-BETEXT-1A      TO WS-BETEXT-1                      
015422               MOVE WS-BETEXT-2A      TO WS-BETEXT-2                      
015423               MOVE WS-BETEXT-3A      TO WS-BETEXT-3                      
015424               MOVE WS-BETEXT-4A      TO WS-BETEXT-4                      
015425             ELSE                                                         
015426               CONTINUE                                                   
015427             END-IF                                                       
015428           ELSE                                                           
015429             IF DHEA-KDFINDOC(WS-MX1) = 'CR'                              
015430               MOVE DHEA-IDBREAK-2(WS-MX1) TO WS-IDBREAK-2A               
015431               MOVE WS-IDBREAK-2A(1:2)     TO LANDX2-IDLANDX2             
015432               IF LANDX2-EU-IDLANDX2                                      
015433                 MOVE DHEA-IDLANDX3-RESP(WS-MX1) TO                       
015434                                         WS-IDLANDX3-DDGS-REC             
015435                 PERFORM DB2-SELECT-SECO-DDGS-TEXT                        
015436                 MOVE WS-BETEXT-1A    TO WS-BETEXT-1                      
015437                 MOVE WS-BETEXT-2A    TO WS-BETEXT-2                      
015438                 MOVE WS-BETEXT-3A    TO WS-BETEXT-3                      
015439                 MOVE WS-BETEXT-4A    TO WS-BETEXT-4                      
015440               END-IF                                                     
015441             ELSE                                                         
015442               CONTINUE                                                   
015443             END-IF                                                       
015444           END-IF                                                         
015445         ELSE                                                             
015446           CONTINUE                                                       
015447         END-IF                                                           
015448       ELSE                                                               
015449         CONTINUE                                                         
015450       END-IF                                                             
015451     ELSE                                                                 
015452       CONTINUE                                                           
015453     END-IF                                                               
015454     .                                                                    
015455                                                                          
015456 BH-CHECK-IPT-TEXT SECTION.                                               
015457     IF WS-IDBREAK-1 = 'SERVEXT'                                          
015458     AND DLIN-IDEXCUST-1(WS-MX2) = '2278'                                 
015459       MOVE DHEA-IDLANDX3-RESP(WS-MX1) TO WS-IDLANDX3-DDGS-REC            
015460       PERFORM DB2-SELECT-SECO-DDGS-TEXT                                  
015461       MOVE WS-BETEXT-1A      TO WS-BETEXT-1                              
015462       MOVE WS-BETEXT-2A      TO WS-BETEXT-2                              
015463       MOVE WS-BETEXT-3A      TO WS-BETEXT-3                              
015470       MOVE WS-BETEXT-4A      TO WS-BETEXT-4                              
015473     ELSE                                                                 
015474       CONTINUE                                                           
015475     END-IF                                                               
015476     .                                                                    
015477                                                                          
015478 Z-FINISH SECTION.                                                        
015479     PERFORM DB2-CLOSE-CRS-LSEL                                           
015480     .                                                                    
015481     EJECT                                                                
015482                                                                          
015490* --- DB2 SECTIONS  ---                                                   
015500*                                                                         
017210                                                                          
017211 DB2-SELECT-PROC SECTION.                                                 
017213     EXEC SQL                                                             
017214     SELECT   T01PROC.KDBEH                                               
017215                                                                          
017216     INTO     :PROC-KDBEH                                                 
017217                                                                          
017220     FROM     T01PROC                                                     
017221                                                                          
017222     WHERE    T01PROC.IDSYSTEM = 'WF02'       AND                         
017223              T01PROC.IDLEGSEL = :WS-IDLEGSEL                             
017224     END-EXEC                                                             
017225                                                                          
017226     MOVE 'PROC'  TO WS-FELTEXT                                           
017227     MOVE 000     TO GOOD-SQLCODES                                        
017228     MOVE SQLCODE TO SQLCODE-WS                                           
017229     PERFORM DB2-STATUS-CHECK                                             
017230     .                                                                    
017231     EJECT                                                                
017232                                                                          
017233 DB2-SELECT-MAX-CURR SECTION.                                             
017234     EXEC SQL                                                             
017235     SELECT   MAX(T01CURR.DASTADAT)                                       
017236                                                                          
017237     INTO     :WS-DASTADAT-KEY                                            
017238                                                                          
017239     FROM     T01CURR                                                     
017240                                                                          
017241     WHERE    T01CURR.IDLEGSEL = :WS-IDLEGSEL                             
017242       AND   (T01CURR.DASTADAT < :WS-DAGENS-DATUM                         
017243        OR    T01CURR.DASTADAT = :WS-DAGENS-DATUM)                        
017244     END-EXEC                                                             
017245                                                                          
017246     MOVE 'MAX ' TO WS-FELTEXT                                            
017247     MOVE 000            TO GOOD-SQLCODES                                 
017248     MOVE SQLCODE        TO SQLCODE-WS                                    
017249     PERFORM DB2-STATUS-CHECK                                             
017250     .                                                                    
017251     EJECT                                                                
017252                                                                          
017253 DB2-SELECT-MAX-CURR-CREDIT SECTION.                                      
017254     EXEC SQL                                                             
017255     SELECT   MAX(T01CURR.DASTADAT)                                       
017256                                                                          
017257     INTO     :WS-DASTADAT-CREDIT                                         
017258                                                                          
017259     FROM     T01CURR                                                     
017260                                                                          
017261     WHERE    T01CURR.IDLEGSEL = :WS-IDLEGSEL                             
017262       AND   (T01CURR.DASTADAT < :WS2-DASTADAT-CREDIT                     
017263        OR    T01CURR.DASTADAT = :WS2-DASTADAT-CREDIT)                    
017264     END-EXEC                                                             
017265                                                                          
017266     MOVE 'MAX ' TO WS-FELTEXT                                            
017267     MOVE 000            TO GOOD-SQLCODES                                 
017268     MOVE SQLCODE        TO SQLCODE-WS                                    
017269     PERFORM DB2-STATUS-CHECK                                             
017270     .                                                                    
017271     EJECT                                                                
017272                                                                          
017273 DB2-SELECT-FCUS-CURR SECTION.                                            
017274     EXEC SQL                                                             
017275     SELECT   T01CURR.PRKURS                                              
017276             ,T01CURR.REVALUTA                                            
017277                                                                          
017278     INTO     :CURR-PRKURS                                                
017279             ,:WS-REVALUTA                                                
017280                                                                          
017281     FROM     T01FCUS                                                     
017282             ,T01CURR                                                     
017283                                                                          
017284     WHERE    T01FCUS.IDLEGSEL = :WS-IDLEGSEL                             
017285       AND    T01FCUS.IDPARTNR = :WS-IDPARTNR                             
017286       AND    T01FCUS.IDLEGSEL = T01CURR.IDLEGSEL                         
017287       AND    T01FCUS.KDVALISO = T01CURR.KDVALISO                         
017288       AND    T01CURR.DASTADAT = :WS-DASTADAT-KEY                         
017289     END-EXEC                                                             
017290                                                                          
017291     MOVE 'FCUS' TO WS-FELTEXT                                            
017292     MOVE 000            TO GOOD-SQLCODES                                 
017293     MOVE SQLCODE        TO SQLCODE-WS                                    
017294     PERFORM DB2-STATUS-CHECK                                             
017295     .                                                                    
017296     EJECT                                                                
017297                                                                          
017298 DB2-SELECT-CURR-CREDIT SECTION.                                          
017299     EXEC SQL                                                             
017300     SELECT   T01CURR.PRKURS                                              
017301             ,T01CURR.REVALUTA                                            
017302                                                                          
017303     INTO     :WS-PRKURS-CREDIT                                           
017304             ,:WS-REVALUTA-CREDIT                                         
017305                                                                          
017306     FROM     T01CURR                                                     
017307                                                                          
017308     WHERE    T01CURR.IDLEGSEL = :WS-IDLEGSEL                             
017309       AND    T01CURR.KDVALISO = :WS-KDVALISO                             
017310       AND    T01CURR.DASTADAT = :WS2-DASTADAT-CREDIT                     
017311     END-EXEC                                                             
017312                                                                          
017313     MOVE 'FCUS' TO WS-FELTEXT                                            
017314     MOVE 000            TO GOOD-SQLCODES                                 
017315     MOVE SQLCODE        TO SQLCODE-WS                                    
017316     PERFORM DB2-STATUS-CHECK                                             
017317     .                                                                    
017318     EJECT                                                                
017319                                                                          
017320 DB2-SELECT-FCUS-CURR-CREDIT SECTION.                                     
017321     EXEC SQL                                                             
017322     SELECT   T01CURR.PRKURS                                              
017323             ,T01CURR.REVALUTA                                            
017324                                                                          
017325     INTO     :CURR-PRKURS                                                
017326             ,:WS-REVALUTA                                                
017327                                                                          
017328     FROM     T01FCUS                                                     
017329             ,T01CURR                                                     
017330                                                                          
017331     WHERE    T01FCUS.IDLEGSEL = :WS-IDLEGSEL                             
017332       AND    T01FCUS.IDPARTNR = :WS-IDPARTNR                             
017333       AND    T01FCUS.IDLEGSEL = T01CURR.IDLEGSEL                         
017334       AND    T01FCUS.KDVALISO = T01CURR.KDVALISO                         
017335       AND    T01CURR.DASTADAT = :WS2-DASTADAT-CREDIT                     
017336     END-EXEC                                                             
017337                                                                          
017338     MOVE 'FCUS' TO WS-FELTEXT                                            
017339     MOVE 000            TO GOOD-SQLCODES                                 
017340     MOVE SQLCODE        TO SQLCODE-WS                                    
017341     PERFORM DB2-STATUS-CHECK                                             
017342     .                                                                    
017343     EJECT                                                                
017344                                                                          
017345 DB2-SELECT-SECO-DDGS-TEXT   SECTION.                                     
017346     EXEC SQL                                                             
017347     SELECT   T01SECO.BETEXT_1                                            
017348             ,T01SECO.BETEXT_2                                            
017349             ,T01SECO.BETEXT_3                                            
017350             ,T01SECO.BETEXT_4                                            
017351                                                                          
017352     INTO     :WS-BETEXT-1A                                               
017353             ,:WS-BETEXT-2A                                               
017354             ,:WS-BETEXT-3A                                               
017355             ,:WS-BETEXT-4A                                               
017356                                                                          
017357     FROM     T01SECO                                                     
017358                                                                          
017359     WHERE    T01SECO.IDLEGSEL = :WS-IDLEGSEL                             
017360       AND    T01SECO.IDLANDX3 = :WS-IDLANDX3-DDGS-REC                    
017361       AND    T01SECO.KDSTATUS = 1                                        
017362       AND    T01SECO.DADELDAT = '00000000'                               
017363     END-EXEC                                                             
017364                                                                          
017365     MOVE 'SECO' TO WS-FELTEXT                                            
017366     MOVE 000            TO GOOD-SQLCODES                                 
017367     MOVE SQLCODE        TO SQLCODE-WS                                    
017368     PERFORM DB2-STATUS-CHECK                                             
017369     .                                                                    
017370     EJECT                                                                
017371                                                                          
017372 DB2-OPEN-CRS-LSEL SECTION.                                               
017373     EXEC SQL DECLARE T01LSEL-CRS CURSOR FOR                              
017374     SELECT   T01LSEL.IDLEGSEL                                            
017375                                                                          
017376     FROM     T01LSEL                                                     
017377                                                                          
017378     WHERE    KDSTATUS = 1                                                
017379     END-EXEC                                                             
017380                                                                          
017381     EXEC SQL OPEN T01LSEL-CRS                                            
017382     END-EXEC                                                             
017383                                                                          
017384     MOVE 'LSEL' TO WS-FELTEXT                                            
017385     MOVE 000            TO GOOD-SQLCODES                                 
017386     MOVE SQLCODE        TO SQLCODE-WS                                    
017387     PERFORM DB2-STATUS-CHECK                                             
017388     .                                                                    
017389     EJECT                                                                
017390                                                                          
017391 DB2-FETCH-CRS-LSEL SECTION.                                              
017392     EXEC SQL FETCH T01LSEL-CRS INTO                                      
017393            :WS-IDLEGSEL                                                  
017394     END-EXEC                                                             
017395                                                                          
017396     MOVE 000100         TO GOOD-SQLCODES                                 
017397     MOVE SQLCODE        TO SQLCODE-WS                                    
017398     PERFORM DB2-STATUS-CHECK                                             
017399     .                                                                    
017400     EJECT                                                                
017401                                                                          
017402 DB2-CLOSE-CRS-LSEL SECTION.                                              
017403     EXEC SQL CLOSE T01LSEL-CRS                                           
017404     END-EXEC                                                             
017405     .                                                                    
017406     EJECT                                                                
017407                                                                          
017408 DB2-OPEN-CRS-MISC SECTION.                                               
017409     EXEC SQL DECLARE MISC-CRS CURSOR WITH ROWSET POSITIONING FOR         
017410     SELECT   T01DHEA.IDLEGSEL,                                           
017411              T01DHEA.DAEXDAT,                                            
017412              T01DHEA.TIEXTID,                                            
017413              T01DHEA.KDVALISO,                                           
017414              T01DHEA.IDLANDX3_SEND,                                      
017415              T01DHEA.IDLEVNR,                                            
017416              T01DHEA.IDPARTNR,                                           
017417              T01DHEA.KDFINDOC,                                           
017418              T01DHEA.FLSOFT,                                             
017419              T01DHEA.FLFREE,                                             
017420              T01DHEA.FLPRIV,                                             
017421              T01DHEA.IDBREAK_1,                                          
017422              T01DHEA.IDBREAK_2,                                          
017423              T01FCUS.IDSPRAK,                                            
017424              T01PATE.KDBETALV,                                           
017425              T01PATE.BEBETVIL,                                           
017426              T01LSEL.BELEGRAD_1,                                         
017427              T01LSEL.BELEGRAD_2,                                         
017428              T01LSEL.ADLEG_STREET,                                       
017429              T01LSEL.ADLEG_BOX,                                          
017430              T01LSEL.ADLEG_CITY,                                         
017431              T01LSEL.ADLEG_PCODE,                                        
017432              T01LSEL.IDLANDX3,                                           
017433              T01LSEL.IDTFN,                                              
017434              T01LSEL.IDTFX,                                              
017440              T01LSEL.IDMAIL,                                             
017450              T01LSEL.BECONT,                                             
017470              T01LSEL.IDBG,                                               
017480              T01LSEL.IDPG,                                               
017481              T01LSEL.IDVAT,                                              
017482              T01RECO.BERESPRA_1,                                         
017483              T01RECO.BERESPRA_2,                                         
017484              T01RECO.ADRESP_STREET,                                      
017485              T01RECO.ADRESP_BOX,                                         
017486              T01RECO.ADRESP_CITY,                                        
017487              T01RECO.ADRESP_PCODE,                                       
017488              T01RECO.IDLANDX3,                                           
017489              T01RECO.IDTFN,                                              
017490              T01RECO.IDTFX,                                              
017491              T01RECO.IDMAIL,                                             
017492              T01RECO.BECONT,                                             
017493              T01SECO.IDVAT,                                              
017498              T01RECO.IDBG,                                               
017499              T01RECO.IDPG,                                               
017500              T01FCUS.BEBET_NAME1,                                        
017501              T01FCUS.BEBET_NAME2,                                        
017502              T01FCUS.ADBET_STREET,                                       
017503              T01FCUS.ADBET_BOX,                                          
017504              T01FCUS.ADBET_CITY,                                         
017505              T01FCUS.ADBET_PCODE,                                        
017506              T01FCUS.IDLANDX3,                                           
017507              T01FCUS.IDVAT,                                              
017508              T01FCUS.KDTRADP,                                            
017509              T01FCUS.FLRATE,                                             
017510              T01FCUS.FLDIRVAT,                                           
017511              T01CURR.PRKURS,                                             
017512              T01SECO.BETEXT_1,                                           
017513              T01SECO.BETEXT_2,                                           
017514              T01SECO.BETEXT_3,                                           
017515              T01SECO.BETEXT_4,                                           
017516              T01RECO.IDVAT_AGENT                                         
017517                                                                          
017518     FROM     T01PROC,                                                    
017519              T01DHEA,                                                    
017520              T01LSEL,                                                    
017521              T01SECO,                                                    
017522              T01RECO,                                                    
017523              T01FCUS,                                                    
017524              T01CURR,                                                    
017525              T01PATE                                                     
017526                                                                          
017527     WHERE    T01PROC.IDSYSTEM = 'WF02'                                   
017528       AND    T01DHEA.IDLEGSEL = T01PROC.IDLEGSEL                         
017529       AND    T01DHEA.DAEXDAT  = T01PROC.DAEXDAT                          
017530       AND    T01DHEA.TIEXTID  = T01PROC.TIEXTID                          
017531       AND    T01DHEA.FLFREE   = 'N'                                      
017532       AND    T01LSEL.IDLEGSEL = T01DHEA.IDLEGSEL                         
017533       AND    T01LSEL.KDSTATUS = 1                                        
017534       AND    T01FCUS.IDLEGSEL = T01DHEA.IDLEGSEL                         
017535       AND    T01FCUS.IDPARTNR = T01DHEA.IDPARTNR                         
017536       AND    T01FCUS.KDSTATUS = 1                                        
017537       AND    T01FCUS.DADELDAT = '00000000'                               
017538       AND    T01SECO.IDLEGSEL = T01DHEA.IDLEGSEL                         
017539       AND    T01SECO.IDLANDX3 = T01DHEA.IDLANDX3_SEND                    
017540       AND    T01SECO.KDSTATUS = 1                                        
017541       AND    T01SECO.DADELDAT = '00000000'                               
017542       AND    T01RECO.IDLEGSEL = T01FCUS.IDLEGSEL                         
017543       AND    T01RECO.IDLANDX3 = T01FCUS.IDLANDX3                         
017544       AND    T01RECO.KDSTATUS = 1                                        
017545       AND    T01RECO.DADELDAT = '00000000'                               
017546       AND    T01PATE.IDLEGSEL = T01FCUS.IDLEGSEL                         
017547       AND    T01PATE.IDSPRAK  = T01FCUS.IDSPRAK                          
017548       AND    T01PATE.KDBETALV = T01FCUS.KDBETALV                         
017549       AND    T01CURR.IDLEGSEL = T01DHEA.IDLEGSEL                         
017550       AND    T01CURR.KDVALISO = T01DHEA.KDVALISO                         
017551       AND    T01CURR.DASTADAT = :WS-DASTADAT-KEY                         
017552     UNION                                                                
017553                                                                          
017554     SELECT   T01DHEA.IDLEGSEL,                                           
017555              T01DHEA.DAEXDAT,                                            
017556              T01DHEA.TIEXTID,                                            
017557              T01DHEA.KDVALISO,                                           
017558              T01DHEA.IDLANDX3_SEND,                                      
017559              T01DHEA.IDLEVNR,                                            
017560              T01DHEA.IDPARTNR,                                           
017561              T01DHEA.KDFINDOC,                                           
017562              T01DHEA.FLSOFT,                                             
017563              T01DHEA.FLFREE,                                             
017564              T01DHEA.FLPRIV,                                             
017565              T01DHEA.IDBREAK_1,                                          
017566              T01DHEA.IDBREAK_2,                                          
017567              T01FCUS.IDSPRAK,                                            
017568              ' '             ,                                           
017569              'FREE OF CHARGE',                                           
017570              T01LSEL.BELEGRAD_1,                                         
017571              T01LSEL.BELEGRAD_2,                                         
017572              T01LSEL.ADLEG_STREET,                                       
017573              T01LSEL.ADLEG_BOX,                                          
017574              T01LSEL.ADLEG_CITY,                                         
017575              T01LSEL.ADLEG_PCODE,                                        
017576              T01LSEL.IDLANDX3,                                           
017577              T01LSEL.IDTFN,                                              
017578              T01LSEL.IDTFX,                                              
017579              T01LSEL.IDMAIL,                                             
017580              T01LSEL.BECONT,                                             
017581              T01LSEL.IDBG,                                               
017582              T01LSEL.IDPG,                                               
017583              T01LSEL.IDVAT,                                              
017584              T01RECO.BERESPRA_1,                                         
017585              T01RECO.BERESPRA_2,                                         
017586              T01RECO.ADRESP_STREET,                                      
017587              T01RECO.ADRESP_BOX,                                         
017588              T01RECO.ADRESP_CITY,                                        
017589              T01RECO.ADRESP_PCODE,                                       
017590              T01RECO.IDLANDX3,                                           
017591              T01RECO.IDTFN,                                              
017592              T01RECO.IDTFX,                                              
017593              T01RECO.IDMAIL,                                             
017594              T01RECO.BECONT,                                             
017595              T01SECO.IDVAT,                                              
017596              T01RECO.IDBG,                                               
017597              T01RECO.IDPG,                                               
017598              T01FCUS.BEBET_NAME1,                                        
017599              T01FCUS.BEBET_NAME2,                                        
017600              T01FCUS.ADBET_STREET,                                       
017601              T01FCUS.ADBET_BOX,                                          
017602              T01FCUS.ADBET_CITY,                                         
017603              T01FCUS.ADBET_PCODE,                                        
017604              T01FCUS.IDLANDX3,                                           
017605              T01FCUS.IDVAT,                                              
017606              T01FCUS.KDTRADP,                                            
017607              T01FCUS.FLRATE,                                             
017608              T01FCUS.FLDIRVAT,                                           
017609              T01CURR.PRKURS,                                             
017610              T01SECO.BETEXT_1,                                           
017611              T01SECO.BETEXT_2,                                           
017612              T01SECO.BETEXT_3,                                           
017613              T01SECO.BETEXT_4,                                           
017614              T01RECO.IDVAT_AGENT                                         
017615                                                                          
017616     FROM     T01PROC,                                                    
017617              T01DHEA,                                                    
017618              T01LSEL,                                                    
017619              T01SECO,                                                    
017620              T01RECO,                                                    
017621              T01FCUS,                                                    
017622              T01CURR                                                     
017623                                                                          
017624     WHERE    T01PROC.IDSYSTEM = 'WF02'                                   
017625       AND    T01DHEA.IDLEGSEL = T01PROC.IDLEGSEL                         
017626       AND    T01DHEA.DAEXDAT  = T01PROC.DAEXDAT                          
017627       AND    T01DHEA.TIEXTID  = T01PROC.TIEXTID                          
017628       AND    T01DHEA.FLFREE   = 'J'                                      
017629       AND    T01LSEL.IDLEGSEL = T01DHEA.IDLEGSEL                         
017630       AND    T01LSEL.KDSTATUS = 1                                        
017631       AND    T01FCUS.IDLEGSEL = T01DHEA.IDLEGSEL                         
017632       AND    T01FCUS.IDPARTNR = T01DHEA.IDPARTNR                         
017633       AND    T01FCUS.KDSTATUS = 1                                        
017634       AND    T01FCUS.DADELDAT = '00000000'                               
017635       AND    T01SECO.IDLEGSEL = T01DHEA.IDLEGSEL                         
017636       AND    T01SECO.IDLANDX3 = T01DHEA.IDLANDX3_SEND                    
017637       AND    T01SECO.KDSTATUS = 1                                        
017638       AND    T01SECO.DADELDAT = '00000000'                               
017639       AND    T01RECO.IDLEGSEL = T01FCUS.IDLEGSEL                         
017640       AND    T01RECO.IDLANDX3 = T01FCUS.IDLANDX3                         
017641       AND    T01RECO.KDSTATUS = 1                                        
017642       AND    T01RECO.DADELDAT = '00000000'                               
017643       AND    T01CURR.IDLEGSEL = T01DHEA.IDLEGSEL                         
017644       AND    T01CURR.KDVALISO = T01DHEA.KDVALISO                         
017645       AND    T01CURR.DASTADAT = :WS-DASTADAT-KEY                         
017646     END-EXEC                                                             
017647                                                                          
017648     EXEC SQL OPEN MISC-CRS                                               
017649     END-EXEC                                                             
017650                                                                          
017651     MOVE 'CRS ' TO WS-FELTEXT                                            
017652     MOVE 000            TO GOOD-SQLCODES                                 
017653     MOVE SQLCODE        TO SQLCODE-WS                                    
017654     PERFORM DB2-STATUS-CHECK                                             
017655     .                                                                    
017656     EJECT                                                                
017657                                                                          
017658 DB2-OPEN-CRS-DLIN SECTION.                                               
017659     EXEC SQL DECLARE DLIN-CRS CURSOR WITH ROWSET POSITIONING FOR         
017660     SELECT  T01DLIN.IDARTNR_FINANCE,                                     
017661             T01DLIN.SUNTO,                                               
017662             T01DLIN.SUVAT_BILLIT,                                        
017663             T01DLIN.SUBTO,                                               
017664             T01DLIN.IDEXCUST_1,                                          
017665             T01DLIN.IDEXCUST_2,                                          
017666             T01DLIN.KDFINDOC,                                            
017667             T01DLIN.IDLANDX3_SEND,                                       
017668             T01DLIN.IDLANDX3_REC                                         
017669                                                                          
017670     FROM    T01DLIN                                                      
017671                                                                          
017672     WHERE   T01DLIN.IDLEGSEL      = :WS-IDLEGSEL                         
017673       AND   T01DLIN.DAEXDAT       = :WS-DAEXDAT                          
017674       AND   T01DLIN.TIEXTID       = :WS-TIEXTID                          
017675       AND   T01DLIN.KDVALISO      = :WS-KDVALISO                         
017676       AND   T01DLIN.IDLANDX3_SEND = :WS-IDLANDX3-SEND                    
017677       AND   T01DLIN.IDLEVNR       = :WS-IDLEVNR                          
017678       AND   T01DLIN.IDPARTNR      = :WS-IDPARTNR                         
017679       AND   T01DLIN.KDFINDOC      = :WS-KDFINDOC                         
017680       AND   T01DLIN.FLSOFT        = :WS-FLSOFT                           
017681       AND   T01DLIN.FLFREE        = :WS-FLFREE                           
017682       AND   T01DLIN.FLPRIV        = :WS-FLPRIV                           
017683       AND   T01DLIN.IDBREAK_1     = :WS-IDBREAK-1                        
017690       AND   T01DLIN.IDBREAK_2     = :WS-IDBREAK-2                        
017695     END-EXEC                                                             
017696                                                                          
017697     EXEC SQL OPEN DLIN-CRS                                               
017698     END-EXEC                                                             
017699                                                                          
017700     MOVE 'DLIN' TO WS-FELTEXT                                            
017701     MOVE 000            TO GOOD-SQLCODES                                 
017702     MOVE SQLCODE        TO SQLCODE-WS                                    
017703     PERFORM DB2-STATUS-CHECK                                             
017704     .                                                                    
017705     EJECT                                                                
017706                                                                          
017707 DB2-FETCH-CRS-MISC SECTION.                                              
017709     EXEC SQL FETCH NEXT ROWSET FROM MISC-CRS FOR 100 ROWS INTO           
017710            :DHEA-IDLEGSEL,                                               
017711            :DHEA-DAEXDAT,                                                
017712            :DHEA-TIEXTID,                                                
017713            :DHEA-KDVALISO,                                               
017714            :DHEA-IDLANDX3-SEND,                                          
017715            :DHEA-IDLEVNR,                                                
017716            :DHEA-IDPARTNR,                                               
017717            :DHEA-KDFINDOC,                                               
017718            :DHEA-FLSOFT,                                                 
017719            :DHEA-FLFREE,                                                 
017720            :DHEA-FLPRIV,                                                 
017721            :DHEA-IDBREAK-1,                                              
017730            :DHEA-IDBREAK-2,                                              
017740            :DHEA-IDSPRAK,                                                
017750            :DHEA-KDBETALV,                                               
017751            :DHEA-BEBETVIL,                                               
017760            :DHEA-BELEGRAD-1,                                             
017770            :DHEA-BELEGRAD-2,                                             
017780            :DHEA-ADLEG-STREET,                                           
017790            :DHEA-ADLEG-BOX,                                              
017800            :DHEA-ADLEG-CITY,                                             
017810            :DHEA-ADLEG-PCODE,                                            
017820            :DHEA-IDLANDX3-LEG,                                           
017830            :DHEA-IDTFN-LEG,                                              
017840            :DHEA-IDTFX-LEG,                                              
017850            :DHEA-IDMAIL-LEG,                                             
017860            :DHEA-BECONT-LEG,                                             
017880            :DHEA-IDBG-LEG,                                               
017890            :DHEA-IDPG-LEG,                                               
017891            :DHEA-IDVAT-LEG,                                              
017892            :DHEA-BERESPRA-1,                                             
017893            :DHEA-BERESPRA-2,                                             
017894            :DHEA-ADRESP-STREET,                                          
017895            :DHEA-ADRESP-BOX,                                             
017896            :DHEA-ADRESP-CITY,                                            
017897            :DHEA-ADRESP-PCODE,                                           
017898            :DHEA-IDLANDX3-RESP,                                          
017899            :DHEA-IDTFN-RESP,                                             
017900            :DHEA-IDTFX-RESP,                                             
017901            :DHEA-IDMAIL-RESP,                                            
017902            :DHEA-BECONT-RESP,                                            
017903            :DHEA-IDVAT-RESP,                                             
017904            :DHEA-IDBG-RESP,                                              
017905            :DHEA-IDPG-RESP,                                              
017906            :DHEA-BEBET-NAME1,                                            
017907            :DHEA-BEBET-NAME2,                                            
017908            :DHEA-ADBET-STREET,                                           
017909            :DHEA-ADBET-BOX,                                              
017910            :DHEA-ADBET-CITY,                                             
017911            :DHEA-ADBET-PCODE,                                            
017912            :DHEA-IDLANDX3-BET,                                           
017913            :DHEA-IDVAT-BET,                                              
017914            :DHEA-KDTRADP,                                                
017915            :WS-FLRATE,                                                   
017916            :WS-FLDIRVAT,                                                 
017917            :DHEA-PRKURS,                                                 
017918            :DHEA-BETEXT-1,                                               
017919            :DHEA-BETEXT-2,                                               
017920            :DHEA-BETEXT-3,                                               
017921            :DHEA-BETEXT-4,                                               
017922            :DHEA-IDVAT-AGENT                                             
017923     END-EXEC                                                             
017924                                                                          
017930     MOVE 000100         TO GOOD-SQLCODES                                 
018000     MOVE SQLCODE        TO SQLCODE-WS                                    
018200     PERFORM DB2-STATUS-CHECK                                             
018300     .                                                                    
018400     EJECT                                                                
019011                                                                          
019012 DB2-FETCH-CRS-DLIN SECTION.                                              
019015     EXEC SQL FETCH NEXT ROWSET FROM DLIN-CRS FOR 100 ROWS INTO           
019016            :DLIN-IDARTNR-FINANCE,                                        
019017            :DLIN-SUNTO,                                                  
019018            :DLIN-SUVAT-BILLIT,                                           
019019            :DLIN-SUBTO,                                                  
019020            :DLIN-IDEXCUST-1,                                             
019030            :DLIN-IDEXCUST-2,                                             
019040            :DLIN-KDFINDOC,                                               
019050            :DLIN-IDLANDX3-SEND,                                          
019060            :DLIN-IDLANDX3-REC                                            
019066     END-EXEC                                                             
019067                                                                          
019068     MOVE 000100         TO GOOD-SQLCODES                                 
019069     MOVE SQLCODE        TO SQLCODE-WS                                    
019070     PERFORM DB2-STATUS-CHECK                                             
019071     .                                                                    
019072     EJECT                                                                
019073                                                                          
019148 DB2-UPDATE-DHEA SECTION.                                                 
019150     EXEC SQL UPDATE T01DHEA                                              
019301     SET    IDSPRAK          = :WS-IDSPRAK,                               
019302            KDBETALV         = :WS-KDBETALV,                              
019303            BEBETVIL         = :WS-BEBETVIL,                              
019304            BELEGRAD_1       = :WS-BELEGRAD-1,                            
019305            BELEGRAD_2       = :WS-BELEGRAD-2,                            
019306            ADLEG_STREET     = :WS-ADLEG-STREET,                          
019307            ADLEG_BOX        = :WS-ADLEG-BOX,                             
019308            ADLEG_CITY       = :WS-ADLEG-CITY,                            
019309            ADLEG_PCODE      = :WS-ADLEG-PCODE,                           
019310            IDLANDX3_LEG     = :WS-IDLANDX3-LEG,                          
019311            IDTFN_LEG        = :WS-IDTFN-LEG,                             
019312            IDTFX_LEG        = :WS-IDTFX-LEG,                             
019313            IDMAIL_LEG       = :WS-IDMAIL-LEG,                            
019314            BECONT_LEG       = :WS-BECONT-LEG,                            
019315            IDVAT_LEG        = :WS-IDVAT-LEG,                             
019316            IDBG_LEG         = :WS-IDBG-LEG,                              
019317            IDPG_LEG         = :WS-IDPG-LEG,                              
019318            BERESPRA_1       = :WS-BERESPRA-1,                            
019319            BERESPRA_2       = :WS-BERESPRA-2,                            
019320            ADRESP_STREET    = :WS-ADRESP-STREET,                         
019321            ADRESP_BOX       = :WS-ADRESP-BOX,                            
019322            ADRESP_CITY      = :WS-ADRESP-CITY,                           
019323            ADRESP_PCODE     = :WS-ADRESP-PCODE,                          
019324            IDLANDX3_RESP    = :WS-IDLANDX3-RESP,                         
019325            IDTFN_RESP       = :WS-IDTFN-RESP,                            
019326            IDTFX_RESP       = :WS-IDTFX-RESP,                            
019327            IDMAIL_RESP      = :WS-IDMAIL-RESP,                           
019328            BECONT_RESP      = :WS-BECONT-RESP,                           
019329            IDVAT_RESP       = :WS-IDVAT-RESP,                            
019330            IDBG_RESP        = :WS-IDBG-RESP,                             
019331            IDPG_RESP        = :WS-IDPG-RESP,                             
019332            BEBET_NAME1      = :WS-BEBET-NAME1,                           
019333            BEBET_NAME2      = :WS-BEBET-NAME2,                           
019334            ADBET_STREET     = :WS-ADBET-STREET,                          
019335            ADBET_BOX        = :WS-ADBET-BOX,                             
019336            ADBET_CITY       = :WS-ADBET-CITY,                            
019337            ADBET_PCODE      = :WS-ADBET-PCODE,                           
019338            IDLANDX3_BET     = :WS-IDLANDX3-BET,                          
019339            IDVAT_BET        = :WS-IDVAT-BET,                             
019340            KDTRADP          = :WS-KDTRADP,                               
019341            PRKURS           = :WS-PRKURS,                                
019343            SUNTO_SERV       = :WS-SUNTO-SERV,                            
019344            SUNTO_PART       = :WS-SUNTO-PART,                            
019345            SUNTO_TOT        = :WS-SUNTO-TOT,                             
019346            SUBTO_SERV       = :WS-SUBTO-SERV,                            
019347            SUBTO_PART       = :WS-SUBTO-PART,                            
019348            SUVAT_BILLIT_TOT = :WS-SUVAT-BILLIT-TOT,                      
019349            SUBTO_TOT        = :WS-SUBTO-TOT,                             
019350            BETEXT_1         = :WS-BETEXT-1,                              
019351            BETEXT_2         = :WS-BETEXT-2,                              
019352            BETEXT_3         = :WS-BETEXT-3,                              
019353            BETEXT_4         = :WS-BETEXT-4,                              
019354            IDVAT_AGENT      = :WS-IDVAT-AGENT                            
019355                                                                          
019358     WHERE  T01DHEA.IDLEGSEL      = :WS-IDLEGSEL                          
019359       AND  T01DHEA.DAEXDAT       = :WS-DAEXDAT                           
019360       AND  T01DHEA.TIEXTID       = :WS-TIEXTID                           
019361       AND  T01DHEA.KDVALISO      = :WS-KDVALISO                          
019362       AND  T01DHEA.IDLANDX3_SEND = :WS-IDLANDX3-SEND                     
019363       AND  T01DHEA.IDLEVNR       = :WS-IDLEVNR                           
019364       AND  T01DHEA.IDPARTNR      = :WS-IDPARTNR                          
019365       AND  T01DHEA.KDFINDOC      = :WS-KDFINDOC                          
019366       AND  T01DHEA.FLSOFT        = :WS-FLSOFT                            
019367       AND  T01DHEA.FLFREE        = :WS-FLFREE                            
019368       AND  T01DHEA.FLPRIV        = :WS-FLPRIV                            
019369       AND  T01DHEA.IDBREAK_1     = :WS-IDBREAK-1                         
019370       AND  T01DHEA.IDBREAK_2     = :WS-IDBREAK-2                         
019371     END-EXEC                                                             
019372                                                                          
019373     MOVE 000            TO GOOD-SQLCODES                                 
019374     MOVE SQLCODE        TO SQLCODE-WS                                    
019375     PERFORM DB2-STATUS-CHECK                                             
019376     .                                                                    
019377     EJECT                                                                
019378                                                                          
019459 DB2-CLOSE-CRS-MISC SECTION.                                              
019460     EXEC SQL CLOSE MISC-CRS                                              
019461     END-EXEC                                                             
019462     .                                                                    
019463     EJECT                                                                
019464                                                                          
019465 DB2-CLOSE-CRS-DLIN SECTION.                                              
019466     EXEC SQL CLOSE DLIN-CRS                                              
019467     END-EXEC                                                             
019468     .                                                                    
019469     EJECT                                                                
019470                                                                          
019471 DB2-STATUS-CHECK SECTION.                                                
019472     SET SQLCODE-IX         TO 1                                          
019473     SEARCH GOOD-SQLCODE AT END                                           
019474           CALL ABEND USING RKOD-ABEND-DB2                                
019475        WHEN GOOD-SQLCODE (SQLCODE-IX) = SQLCODE-WS                       
019480           CONTINUE                                                       
019500     END-SEARCH                                                           
019600     .                                                                    
