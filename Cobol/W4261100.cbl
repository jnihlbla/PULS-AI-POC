000100 ID DIVISION.                                                             
000200                                                                          
000300 PROGRAM-ID.             W4261100.                                        
000400 AUTHOR.                 INGER NILSSON.                                   
000500 DATE-WRITTEN.           MAJ 1990.                                        
000600                                                                          
000700     REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION:                                                            
001000*        INFIL    FRÅN W42605                                             
001100*                                                                         
001200*        LISTA     FÖRDELNING FELGRUPPER, CDC                             
001300*                                                                         
001310*        LISTFILER FÖRDELNING FELGRUPPER, ALLA SDC:ER                     
001320*                                                                         
001400     EJECT                                                                
001500 ENVIRONMENT DIVISION.                                                    
001600                                                                          
001700 INPUT-OUTPUT SECTION.                                                    
001800                                                                          
001900 FILE-CONTROL.                                                            
002000     SELECT W42603       ASSIGN TO W42611D1.                              
002001*                        * INFIL                                          
002002                                                                          
002100     SELECT W42611-001   ASSIGN TO W42611D2.                              
002110*                        * LISTA   TILL CDC   PÅ SVENSKA                  
002120                                                                          
002200     SELECT W42611-001A  ASSIGN TO W42611D3.                              
002210*                        * LISTA   TILL CDC   PÅ ENGELSKA                 
002220                                                                          
002300     SELECT W42613-21    ASSIGN TO W42611D4.                              
002310*                        * LISTFIL TILL DC21  PÅ FLAMLÄNDSKA              
002320                                                                          
002400     SELECT W42614-21    ASSIGN TO W42611D5.                              
002410*                        * LISTFIL TILL DC21  PÅ ENGELSKA                 
002420                                                                          
002500     EJECT                                                                
002600 DATA DIVISION.                                                           
002700                                                                          
002800 FILE SECTION.                                                            
002900     SKIP3                                                                
003000 FD  W42603                                                               
003100     LABEL RECORD STANDARD                                                
003200     RECORDING F                                                          
003300     BLOCK CONTAINS 0.                                                    
003400*01  W42603-POST -COPY W4260504     -L                                    
003600     SKIP2                                                                
003610                                                                          
003700 FD  W42611-001                                                           
003800     LABEL RECORD STANDARD.                                               
003900 01  W42611-LISTA-S-CDC.                                                  
004000     03  RAD-SKIP-S-CDC     PIC S9(03) COMP-3.                            
004100     03  FILLER             PIC X(119).                                   
004200     SKIP2                                                                
004210                                                                          
004300 FD  W42611-001A                                                          
004400     LABEL RECORD STANDARD.                                               
004500 01  W42611-LISTA-GB-CDC.                                                 
004600     03  RAD-SKIP-GB-CDC    PIC S9(03) COMP-3.                            
004700     03  FILLER             PIC X(119).                                   
004800     EJECT                                                                
004810                                                                          
004900 FD  W42613-21                                                            
005000     LABEL RECORD STANDARD.                                               
005100 01  W42613-LISTA-NL-DC21.                                                
005200     03  RAD-SKIP-NL-DC21   PIC S9(03) COMP-3.                            
005300     03  FILLER             PIC X(119).                                   
005400     SKIP2                                                                
005410                                                                          
005500 FD  W42614-21                                                            
005600     LABEL RECORD STANDARD.                                               
005700 01  W42614-LISTA-GB-DC21.                                                
005800     03  RAD-SKIP-GB-DC21   PIC S9(03) COMP-3.                            
005900     03  FILLER             PIC X(119).                                   
006000     EJECT                                                                
006010                                                                          
006100 WORKING-STORAGE SECTION.                                                 
006200     SKIP2                                                                
006201                                                                          
006210*    -- CHECKED BY WY2000                                                 
006300*    ---- ARBETSVARIABLER                                                 
006400*                                                                         
006500 77  IDPGM                   PIC  X(08)  VALUE 'W4261100'.                
006600 77  INFIL-SLUT              PIC  X(03)  VALUE SPACE.                     
006700 77  W-IDDC                  PIC XX      VALUE SPACE.                     
006900 77  W-ANT-RADER             PIC S9(03)  COMP-3 VALUE +0.                 
007000 77  W-TIAARP                PIC  9(05).                                  
007100 77  W-REKVAREL              PIC  9(05).                                  
007200 77  W-SKRIV-TEXT            PIC  X(01)  VALUE 'J'.                       
007300 77  W-NY-SIDA               PIC  X(01).                                  
007400 01  W-TEXT.                                                              
007500     03 W-TEXT1-S.                                                        
007600        05 W-TEXT1-1-S       PIC X(06).                                   
007700        05 W-TEXT1-2-S       PIC X(07).                                   
007800     03 W-TEXT2-S            PIC X(15).                                   
007900     03 W-TEXT1-NL.                                                       
008000        05 W-TEXT1-1-NL      PIC X(10).                                   
008100        05 W-TEXT1-2-NL      PIC X(05).                                   
008200     03 W-TEXT2-NL           PIC X(15).                                   
008300     03 W-TEXT1-GB.                                                       
008400        05 W-TEXT1-1-GB      PIC X(10).                                   
008500        05 W-TEXT1-2-GB      PIC X(03).                                   
008600     03 W-TEXT2-GB           PIC X(15).                                   
008700*                                                                         
008800 77  JA                      PIC X(01)   VALUE 'J'.                       
008900 77  NEJ                     PIC X(01)   VALUE 'N'.                       
009000*      --- VALID IDDC CODES                                               
009010*                                                                         
009020*01    -COPY WWDC99                                                       
009030       EJECT                                                              
009100                                                                          
009200*    ---- INDEXFÄLT                                                       
009300                                                                          
009400 77  IX1                     PIC S9(3)   VALUE +0   COMP SYNC.            
009500     EJECT                                                                
009600*    ---- INFIL RELATIV STORLEK                                           
009700*01  W42603  -COPY W4260504        -PRE IN-                               
009900     EJECT                                                                
010000 01  RUBRIK1.                                                             
010100    03  RUBRIK1-S.                                                        
010200      05  FILLER           PIC S9(03) COMP-3 VALUE +1.                    
010300      05  FILLER           PIC X(23) VALUE 'VOLVO CAR PARTS     '.        
010400      05  FILLER           PIC X(74) VALUE 'W42611-001'.                  
010500      05  FILLER           PIC X(07) VALUE 'DATUM'.                       
010600      05  FILLER           PIC X(16) VALUE SPACE.                         
010700                                                                          
010800    03  RUBRIK1-NL.                                                       
010900      05  FILLER           PIC S9(03) COMP-3 VALUE +1.                    
010910      05  FILLER           PIC X(23) VALUE 'VOLVO CAR PARTS     '.        
011100      05  FILLER           PIC X(74) VALUE 'W42611-001'.                  
011200      05  FILLER           PIC X(07) VALUE ' DATUM'.                      
011300      05  FILLER           PIC X(16) VALUE SPACE.                         
011400                                                                          
011500    03  RUBRIK1-GB.                                                       
011600      05  FILLER           PIC S9(03) COMP-3 VALUE +1.                    
011610      05  FILLER           PIC X(23) VALUE 'VOLVO CAR PARTS     '.        
011800      05  FILLER           PIC X(74) VALUE 'W42611-001'.                  
011900      05  FILLER           PIC X(07) VALUE ' DATE'.                       
012000      05  FILLER           PIC X(16) VALUE SPACE.                         
012100                                                                          
012200 01  FILLER REDEFINES RUBRIK1.                                            
012300    03  RUBRIK-1 OCCURS 3.                                                
012400      05  FILLER                PIC S9(03) COMP-3.                        
012500      05  FILLER                PIC X(104).                               
012600      05  RUB1-DATUM            PIC 99B99B99.                             
012700      05  FILLER                PIC X(08).                                
012800   EJECT                                                                  
012900 01  RUBRIK2.                                                             
013000    03  RUBRIK2-S.                                                        
013100      05  FILLER                PIC S9(03) COMP-3 VALUE +2.               
013200      05  FILLER                PIC X(10) VALUE 'KVALITET'.               
013300      05  FILLER                PIC X(110)                                
013400                                VALUE 'FÖRDELNING FELGRUPPER'.            
013500                                                                          
013600    03  RUBRIK2-NL.                                                       
013700      05  FILLER                PIC S9(03) COMP-3 VALUE +2.               
013800      05  FILLER                PIC X(10) VALUE 'KWALITEIT'.              
013900      05  FILLER                PIC X(110)                                
014000                               VALUE 'DISTRIBUTIE FOUTGROEP'.             
014100                                                                          
014200    03  RUBRIK2-GB.                                                       
014300      05  FILLER                PIC S9(03) COMP-3 VALUE +2.               
014400      05  FILLER                PIC X(10) VALUE 'QUALITY'.                
014500      05  FILLER                PIC X(110)                                
014600                               VALUE 'DISTRIBUTION DEFECT GROUPS'.        
014700                                                                          
014800 01  FILLER REDEFINES RUBRIK2.                                            
014900    03  RUBRIK-2 OCCURS 3.                                                
015000      05  FILLER                PIC S9(03) COMP-3.                        
015100      05  FILLER                PIC X(120).                               
015200   EJECT                                                                  
015300 01  RUBRIK3.                                                             
015400    03  RUBRIK3-S.                                                        
015500      05  FILLER                PIC S9(03) COMP-3 VALUE +2.               
015600      03  FILLER                PIC X(64) VALUE SPACE.                    
015700      03  FILLER                PIC X(56) VALUE 'PERIOD'.                 
015800                                                                          
015900    03  RUBRIK3-NL.                                                       
016000      05  FILLER                PIC S9(03) COMP-3 VALUE +2.               
016100      03  FILLER                PIC X(64) VALUE SPACE.                    
016200      03  FILLER                PIC X(56) VALUE 'PERIODE'.                
016300                                                                          
016400    03  RUBRIK3-GB.                                                       
016500      05  FILLER                PIC S9(03) COMP-3 VALUE +2.               
016600      03  FILLER                PIC X(64) VALUE SPACE.                    
016700      03  FILLER                PIC X(56) VALUE 'PERIOD'.                 
016800                                                                          
016900 01  FILLER REDEFINES RUBRIK3.                                            
017000    03  RUBRIK-3 OCCURS 3.                                                
017100      05  FILLER                PIC S9(03) COMP-3.                        
017200      05  FILLER                PIC X(120).                               
017300   EJECT                                                                  
017400 01  RUBRIK-4.                                                            
017500   03  FILLER                PIC S9(03) COMP-3 VALUE +2.                  
017600   03  FILLER                PIC X(30) VALUE SPACE.                       
017700   03  FILLER OCCURS 13.                                                  
017800     05  RAD-TIAARP          PIC Z(06).                                   
017900   03  FILLER                PIC X(14) VALUE SPACE.                       
018000                                                                          
018100 01  RAD.                                                                 
018200   03  RAD-SKIP              PIC S9(03) COMP-3.                           
018300   03  RAD-TEXT1             PIC X(15).                                   
018400   03  RAD-TEXT2             PIC X(15).                                   
018500   03  FILLER OCCURS 13.                                                  
018600     05  RAD-REKVAREL        PIC Z(05)9.                                  
018700   03  FILLER                PIC X(16) VALUE SPACE.                       
018800     EJECT                                                                
018900*    ---- SUBPROGRAM OCH PARAMETER-AREOR                                  
019000     SKIP3                                                                
019100 01  DYNAMISKA-SUBPROGRAM.                                                
019200   03  WDATKONV              PIC X(8)    VALUE 'WDATKONV'.                
019300     SKIP2                                                                
019400*    ----  PARAMETRAR TILL DATUMKORT                                      
019500                                                                          
019600 01  DATUMKORT-ID            PIC X(6)    VALUE 'WDATUM'.                  
019700     SKIP3                                                                
019800*    -COPY WDATAREA                                                       
020000     EJECT                                                                
020100 PROCEDURE DIVISION.                                                      
020200                                                                          
020300     PERFORM A-INIT                                                       
020400                                                                          
020500     READ W42603 INTO IN-W42603 AT END                                    
020600                   MOVE 'EOF' TO INFIL-SLUT                               
020700     END-READ                                                             
020800                                                                          
020900     PERFORM UNTIL INFIL-SLUT = 'EOF'                                     
021000                                                                          
021100       PERFORM B-BEHANDLING                                               
021200                                                                          
021300       READ W42603 INTO IN-W42603 AT END                                  
021400                     MOVE 'EOF' TO INFIL-SLUT                             
021500       END-READ                                                           
021600     END-PERFORM                                                          
021700                                                                          
021800     PERFORM Z-FINIT                                                      
021900                                                                          
022000     MOVE ZERO            TO RETURN-CODE                                  
022100     GOBACK                                                               
022200     .                                                                    
022300     EJECT                                                                
022400 A-INIT SECTION.                                                          
022500     SKIP2                                                                
022600     OPEN  INPUT W42603                                                   
022700          OUTPUT W42611-001 W42611-001A W42613-21 W42614-21               
022800                                                                          
022900     MOVE 'IDAG'          TO DAT-KDDATFORM                                
023000     CALL WDATKONV     USING DAT-KDDATFORM                                
023100                             DAT-I-TIDATUM                                
023200                             DAT-O-TIDATUM                                
023300                             DAT-KDSVAR                                   
023400     .                                                                    
023500     EJECT                                                                
023600 B-BEHANDLING SECTION.                                                    
023700     SKIP2                                                                
023800     MOVE +1 TO IX1                                                       
023900     PERFORM UNTIL IX1 > +13                                              
024000        MOVE IN-TIAARP   (IX1)       TO W-TIAARP                          
024100        MOVE W-TIAARP                TO RAD-TIAARP   (IX1)                
024200        MOVE IN-REKVAREL (IX1)       TO W-REKVAREL                        
024300        MOVE W-REKVAREL              TO RAD-REKVAREL (IX1)                
024400        ADD +1                       TO IX1                               
024500     END-PERFORM                                                          
024600                                                                          
024610     MOVE IN-IDDC           TO WS-IDDC                                    
024700     EVALUATE TRUE                                                        
024800        WHEN  CDC-SE   PERFORM BA-SKRIV-FORDELNING-CDC                    
025000        WHEN  SDC-NL   PERFORM BB-SKRIV-FORDELNING-DC21                   
025010        WHEN  OTHER ,  CONTINUE                                           
025100     END-EVALUATE                                                         
025200                                                                          
025300     MOVE IN-IDDC                    TO W-IDDC                            
025400     .                                                                    
025500     EJECT                                                                
025600 BA-SKRIV-FORDELNING-CDC SECTION.                                         
025700     SKIP2                                                                
025800     MOVE NEJ                        TO W-NY-SIDA                         
025900     IF W-ANT-RADER     > +45                                             
026000     OR IN-IDDC NOT = W-IDDC                                              
026100        MOVE DAT-TIAAMMDD            TO RUB1-DATUM (01)                   
026200        MOVE RUBRIK-1 (01)           TO W42611-LISTA-S-CDC                
026300        WRITE W42611-LISTA-S-CDC AFTER PAGE                               
026400                                                                          
026500        MOVE DAT-TIAAMMDD            TO RUB1-DATUM (03)                   
026600        MOVE RUBRIK-1 (01)           TO W42611-LISTA-GB-CDC               
026700        WRITE W42611-LISTA-GB-CDC AFTER PAGE                              
026800                                                                          
026900        MOVE RUBRIK-2 (01)           TO W42611-LISTA-S-CDC                
027000        WRITE W42611-LISTA-S-CDC AFTER RAD-SKIP-S-CDC                     
027100                                                                          
027200        MOVE RUBRIK-2 (03)           TO W42611-LISTA-GB-CDC               
027300        WRITE W42611-LISTA-GB-CDC AFTER RAD-SKIP-GB-CDC                   
027400                                                                          
027500        MOVE RUBRIK-3 (01)           TO W42611-LISTA-S-CDC                
027600        WRITE W42611-LISTA-S-CDC AFTER RAD-SKIP-S-CDC                     
027700                                                                          
027800        MOVE RUBRIK-3 (03)           TO W42611-LISTA-GB-CDC               
027900        WRITE W42611-LISTA-GB-CDC AFTER RAD-SKIP-GB-CDC                   
028000                                                                          
028100        MOVE RUBRIK-4                TO W42611-LISTA-S-CDC                
028200        WRITE W42611-LISTA-S-CDC AFTER RAD-SKIP-S-CDC                     
028300                                                                          
028400        MOVE RUBRIK-4                TO W42611-LISTA-GB-CDC               
028500        WRITE W42611-LISTA-GB-CDC AFTER RAD-SKIP-GB-CDC                   
028600                                                                          
028700        MOVE +11                     TO W-ANT-RADER                       
028800        MOVE JA                      TO W-NY-SIDA                         
028900     END-IF                                                               
029000                                                                          
029100     IF IN-IDPTYP = 'GRP' OR 'TOT'                                        
029200        IF W-SKRIV-TEXT = NEJ                                             
029300           MOVE SPACE                TO W-TEXT1-S                         
029400                                        W-TEXT1-GB                        
029500           MOVE +1                   TO RAD-SKIP                          
029600           ADD  +1                   TO W-ANT-RADER                       
029700        ELSE                                                              
029800           MOVE NEJ                  TO W-SKRIV-TEXT                      
029900           IF IN-IDPTYP = 'GRP'                                           
030000              MOVE 'FEL GRUPP'       TO W-TEXT1-S                         
030100              MOVE 'DEF.GROUP'       TO W-TEXT1-GB                        
030200           ELSE                                                           
030300              MOVE 'TOT FELGUPP'     TO W-TEXT1-S                         
030400              MOVE 'TOT.DEF.GROUP'   TO W-TEXT1-GB                        
030500           END-IF                                                         
030600           IF W-NY-SIDA = JA                                              
030700              MOVE +2                TO RAD-SKIP                          
030800              ADD  +2                TO W-ANT-RADER                       
030900           ELSE                                                           
031000              MOVE +3                TO RAD-SKIP                          
031100              ADD  +3                TO W-ANT-RADER                       
031200           END-IF                                                         
031300        END-IF                                                            
031400        IF IN-KDKVAFG = +1                                                
031500           MOVE 'PRODUKT'            TO W-TEXT2-S                         
031600           MOVE 'PRODUCT'            TO W-TEXT2-GB                        
031700        ELSE                                                              
031800           IF IN-KDKVAFG = +2                                             
031900              MOVE 'ADMINSTRATION'   TO W-TEXT2-S                         
032000              MOVE 'ADMINISTRATION'   TO W-TEXT2-GB                       
032100           ELSE                                                           
032200              IF IN-KDKVAFG = +3                                          
032300                 MOVE 'FÖRPACKNING'  TO W-TEXT2-S                         
032400                 MOVE 'PACKAGE'      TO W-TEXT2-GB                        
032500              ELSE                                                        
032600                 IF IN-KDKVAFG = +4                                       
032700                    MOVE 'METOD'     TO W-TEXT2-S                         
032800                    MOVE 'METHOD'    TO W-TEXT2-GB                        
032900                 END-IF                                                   
033000              END-IF                                                      
033100           END-IF                                                         
033200        END-IF                                                            
033300     ELSE                                                                 
033400        MOVE JA                      TO W-SKRIV-TEXT                      
033500        IF IN-IDPTYP = 'OMR'                                              
033600           MOVE 'KONTR.OMR'          TO W-TEXT1-S                         
033700           MOVE IN-BEKVAOMR (01)     TO W-TEXT2-S                         
033800           MOVE 'INSP.AREA'          TO W-TEXT1-GB                        
033900           MOVE IN-BEKVAOMR (02)     TO W-TEXT2-GB                        
034000        ELSE                                                              
034100          IF IN-IDPTYP = 'CL '                                            
034200              MOVE 'LAGER'           TO W-TEXT1-1-S                       
034300              MOVE 'WAREHOUSE'       TO W-TEXT1-1-GB                      
034310                                                                          
034400              MOVE IN-IDDC           TO W-TEXT1-2-S                       
034410                                        W-TEXT1-2-GB                      
034420                                                                          
034500              MOVE SPACE             TO W-TEXT2-S                         
035000                                        W-TEXT2-GB                        
035100          END-IF                                                          
035200        END-IF                                                            
035300        MOVE +2                      TO RAD-SKIP                          
035400        ADD  +2                      TO W-ANT-RADER                       
035500     END-IF                                                               
035600                                                                          
035700     MOVE W-TEXT1-S                  TO RAD-TEXT1                         
035800     MOVE W-TEXT2-S                  TO RAD-TEXT2                         
035900     MOVE RAD                        TO W42611-LISTA-S-CDC                
036000     WRITE W42611-LISTA-S-CDC AFTER RAD-SKIP-S-CDC                        
036100                                                                          
036200     MOVE W-TEXT1-GB                 TO RAD-TEXT1                         
036300     MOVE W-TEXT2-GB                 TO RAD-TEXT2                         
036400     MOVE RAD                        TO W42611-LISTA-GB-CDC               
036500     WRITE W42611-LISTA-GB-CDC AFTER RAD-SKIP-GB-CDC                      
036600     .                                                                    
036700     EJECT                                                                
036800 BB-SKRIV-FORDELNING-DC21 SECTION.                                        
036900     SKIP2                                                                
037000     MOVE NEJ                        TO W-NY-SIDA                         
037100     IF W-ANT-RADER     > +45                                             
037200     OR IN-IDDC NOT = W-IDDC                                              
037300        MOVE DAT-TIAAMMDD            TO RUB1-DATUM (02)                   
037400        MOVE RUBRIK-1 (02)           TO W42613-LISTA-NL-DC21              
037500        WRITE W42613-LISTA-NL-DC21 AFTER PAGE                             
037600                                                                          
037700        MOVE DAT-TIAAMMDD            TO RUB1-DATUM (03)                   
037800        MOVE RUBRIK-1 (03)           TO W42614-LISTA-GB-DC21              
037900        WRITE W42614-LISTA-GB-DC21 AFTER PAGE                             
038000                                                                          
038100        MOVE RUBRIK-2 (02)           TO W42613-LISTA-NL-DC21              
038200        WRITE W42613-LISTA-NL-DC21 AFTER RAD-SKIP-NL-DC21                 
038300                                                                          
038400        MOVE RUBRIK-2 (03)           TO W42614-LISTA-GB-DC21              
038500        WRITE W42614-LISTA-GB-DC21 AFTER RAD-SKIP-GB-DC21                 
038600                                                                          
038700        MOVE RUBRIK-3 (02)           TO W42613-LISTA-NL-DC21              
038800        WRITE W42613-LISTA-NL-DC21 AFTER RAD-SKIP-NL-DC21                 
038900                                                                          
039000        MOVE RUBRIK-3 (03)           TO W42614-LISTA-GB-DC21              
039100        WRITE W42614-LISTA-GB-DC21 AFTER RAD-SKIP-GB-DC21                 
039200                                                                          
039300        MOVE RUBRIK-4                TO W42613-LISTA-NL-DC21              
039400        WRITE W42613-LISTA-NL-DC21 AFTER RAD-SKIP-NL-DC21                 
039500                                                                          
039600        MOVE RUBRIK-4                TO W42614-LISTA-GB-DC21              
039700        WRITE W42614-LISTA-GB-DC21 AFTER RAD-SKIP-GB-DC21                 
039800                                                                          
039900        MOVE +11                     TO W-ANT-RADER                       
040000        MOVE JA                      TO W-NY-SIDA                         
040100     END-IF                                                               
040200                                                                          
040300     IF IN-IDPTYP = 'GRP' OR 'TOT'                                        
040400        IF W-SKRIV-TEXT = NEJ                                             
040500           MOVE SPACE                TO W-TEXT1-NL                        
040600                                        W-TEXT1-GB                        
040700           MOVE +1                   TO RAD-SKIP                          
040800           ADD  +1                   TO W-ANT-RADER                       
040900        ELSE                                                              
041000           MOVE NEJ                  TO W-SKRIV-TEXT                      
041100           IF IN-IDPTYP = 'GRP'                                           
041200              MOVE 'FOUTGROEP'       TO W-TEXT1-NL                        
041300              MOVE 'DEF.GROUP'       TO W-TEXT1-GB                        
041400           ELSE                                                           
041500              MOVE 'TOT FOUTGROEP'   TO W-TEXT1-NL                        
041600              MOVE 'TOT.DEF.GROUP'   TO W-TEXT1-GB                        
041700           END-IF                                                         
041800           IF W-NY-SIDA = JA                                              
041900              MOVE +2                TO RAD-SKIP                          
042000              ADD  +2                TO W-ANT-RADER                       
042100           ELSE                                                           
042200              MOVE +3                TO RAD-SKIP                          
042300              ADD  +3                TO W-ANT-RADER                       
042400           END-IF                                                         
042500        END-IF                                                            
042600        IF IN-KDKVAFG = +1                                                
042700           MOVE 'PRODUKT'            TO W-TEXT2-NL                        
042800           MOVE 'PRODUCT'            TO W-TEXT2-GB                        
042900        ELSE                                                              
043000           IF IN-KDKVAFG = +2                                             
043100              MOVE 'ADMINISTRATIE'   TO W-TEXT2-NL                        
043200              MOVE 'ADMINISTRATION'  TO W-TEXT2-GB                        
043300           ELSE                                                           
043400              IF IN-KDKVAFG = +3                                          
043500                 MOVE 'VERPAKKING'   TO W-TEXT2-NL                        
043600                 MOVE 'PACKAGE'      TO W-TEXT2-GB                        
043700              ELSE                                                        
043800                 IF IN-KDKVAFG = +4                                       
043900                    MOVE 'METHODE'   TO W-TEXT2-NL                        
044000                    MOVE 'METHOD'    TO W-TEXT2-GB                        
044100                 END-IF                                                   
044200              END-IF                                                      
044300           END-IF                                                         
044400        END-IF                                                            
044500     ELSE                                                                 
044600        MOVE JA                      TO W-SKRIV-TEXT                      
044700        IF IN-IDPTYP = 'OMR'                                              
044800           MOVE 'MAGAZIJN-AREA'      TO W-TEXT1-NL                        
044900           MOVE IN-BEKVAOMR (01)     TO W-TEXT2-NL                        
045000           MOVE 'INSP.AREA'          TO W-TEXT1-GB                        
045100           MOVE IN-BEKVAOMR (02)     TO W-TEXT2-GB                        
045200        ELSE                                                              
045300          IF IN-IDPTYP = 'CL '                                            
045400              MOVE 'MAGAZIJN'        TO W-TEXT1-1-NL                      
045500              MOVE 'WAREHOUSE'       TO W-TEXT1-1-GB                      
045600              MOVE IN-IDDC           TO W-TEXT1-2-NL                      
045610                                        W-TEXT1-2-GB                      
045700              MOVE SPACE             TO W-TEXT2-NL                        
046200                                        W-TEXT2-GB                        
046300          END-IF                                                          
046400        END-IF                                                            
046500        MOVE +2                      TO RAD-SKIP                          
046600        ADD  +2                      TO W-ANT-RADER                       
046700     END-IF                                                               
046800                                                                          
046900     MOVE W-TEXT1-NL                 TO RAD-TEXT1                         
047000     MOVE W-TEXT2-NL                 TO RAD-TEXT2                         
047100     MOVE RAD                        TO W42613-LISTA-NL-DC21              
047200     WRITE W42613-LISTA-NL-DC21 AFTER RAD-SKIP-NL-DC21                    
047300                                                                          
047400     MOVE W-TEXT1-GB                 TO RAD-TEXT1                         
047500     MOVE W-TEXT2-GB                 TO RAD-TEXT2                         
047600     MOVE RAD                        TO W42614-LISTA-GB-DC21              
047700     WRITE W42614-LISTA-GB-DC21 AFTER RAD-SKIP-GB-DC21                    
047800     .                                                                    
047900     EJECT                                                                
048000 Z-FINIT SECTION.                                                         
048100     SKIP2                                                                
048200     CLOSE W42603 W42611-001 W42611-001A W42613-21 W42614-21              
048300     .                                                                    
048400     EJECT                                                                
