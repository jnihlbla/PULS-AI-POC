000100 ID DIVISION.                                                             
000200                                                                          
000300 PROGRAM-ID.             W4261000.                                        
000400 AUTHOR.                 INGER NILSSON.                                   
000500 DATE-WRITTEN.           MAJ 1990.                                        
000600                                                                          
000700     REMARKS.                                                             
000800*                                                                         
000900*    FUNKTION:                                                            
001000*        INFIL    FRÅN W42605                                             
001100*                                                                         
001200*        LISTA    KVALITETSINDEX FÖR CDC                                  
001300*                                                                         
001400*        LISTFILER  KVALITETSINDEX FÖR SDC:ERNA                           
001500*                                                                         
001600     EJECT                                                                
001700 ENVIRONMENT DIVISION.                                                    
001800                                                                          
001900 INPUT-OUTPUT SECTION.                                                    
002000                                                                          
002100 FILE-CONTROL.                                                            
002200     SELECT W42602      ASSIGN TO W42610D1.                               
002300*                        * INFIL                                          
002400                                                                          
002500     SELECT W42610-001  ASSIGN TO W42610D2.                               
002600*                        * LISTA   TILL CDC   PÅ SVENSKA                  
002700                                                                          
002800     SELECT W42610-001A ASSIGN TO W42610D3.                               
002900*                        * LISTA   TILL CDC   PÅ ENGELSKA                 
003000                                                                          
003100     SELECT W42611      ASSIGN TO W42610D4.                               
003200*                        * LISTFIL TILL DC21  PÅ FLAMLÄNDSKA              
003300                                                                          
003400     SELECT W42612      ASSIGN TO W42610D5.                               
003500*                        * LISTFIL TILL DC21  PÅ ENGELSKA                 
003600     EJECT                                                                
003700 DATA DIVISION.                                                           
003800                                                                          
003900 FILE SECTION.                                                            
004000     SKIP3                                                                
004100 FD  W42602                                                               
004200     LABEL RECORD STANDARD                                                
004300     RECORDING F                                                          
004400     BLOCK CONTAINS 0.                                                    
004500*01  W42602-POST -COPY W4260503   -L                                      
004600     SKIP2                                                                
004700                                                                          
004800 FD  W42610-001                                                           
004900     LABEL RECORD STANDARD.                                               
005000 01  W42610-LISTA-S-CDC.                                                  
005100     03  RAD-SKIP-S-CDC     PIC S9(03) COMP-3.                            
005200     03  FILLER             PIC X(119).                                   
005300     SKIP2                                                                
005400                                                                          
005500 FD  W42610-001A                                                          
005600     LABEL RECORD STANDARD.                                               
005700 01  W42610-LISTA-GB-CDC.                                                 
005800     03  RAD-SKIP-GB-CDC    PIC S9(03) COMP-3.                            
005900     03  FILLER             PIC X(119).                                   
006000     EJECT                                                                
006100                                                                          
006200 FD  W42611                                                               
006300     LABEL RECORD STANDARD.                                               
006400 01  W42611-LISTA-NL-DC21.                                                
006500     03  RAD-SKIP-NL-DC21   PIC S9(03) COMP-3.                            
006600     03  FILLER             PIC X(119).                                   
006700     SKIP2                                                                
006800                                                                          
006900 FD  W42612                                                               
007000     LABEL RECORD STANDARD.                                               
007100 01  W42612-LISTA-GB-DC21.                                                
007200     03  RAD-SKIP-GB-DC21   PIC S9(03) COMP-3.                            
007300     03  FILLER             PIC X(119).                                   
007400     EJECT                                                                
007500 WORKING-STORAGE SECTION.                                                 
007600     SKIP2                                                                
007601                                                                          
007610*    -- CHECKED BY WY2000                                                 
007700*    ---- ARBETSVARIABLER                                                 
007800*                                                                         
007900 77  IDPGM                   PIC  X(08)  VALUE 'W4261000'.                
008000 77  INFIL-SLUT              PIC  X(03)  VALUE SPACE.                     
008100 77  W-IDDC                  PIC XX      VALUE SPACE.                     
008200 77  W-TIAARP                PIC  9(05).                                  
008300 77  W-REKVAIND              PIC S9(05).                                  
008400*                                                                         
008500 77  JA                      PIC X(01)   VALUE 'J'.                       
008600 77  NEJ                     PIC X(01)   VALUE 'N'.                       
009000                                                                          
009010*      --- VALID IDDC CODES                                               
009020*                                                                         
009030*01    -COPY WWDC99                                                       
009030*01    -COPY WWDCKONS                                                     
009040       EJECT                                                              
009100*    ---- INDEXFÄLT                                                       
009200 77  IX1                     PIC S9(3)   VALUE +0   COMP SYNC.            
009300     EJECT                                                                
009400 01  RUBRIK1.                                                             
009500    03  RUBRIK1-S.                                                        
009600      05  FILLER          PIC S9(03) COMP-3 VALUE +1.                     
009700      05  FILLER          PIC X(23) VALUE 'VOLVO CAR PARTS     '.         
009800      05  FILLER          PIC X(74) VALUE 'W42610-001'.                   
009900      05  FILLER          PIC X(07) VALUE 'DATUM'.                        
010000      05  FILLER          PIC X(16) VALUE SPACE.                          
010100                                                                          
010200    03  RUBRIK1-NL.                                                       
010300      05  FILLER          PIC S9(03) COMP-3 VALUE +1.                     
010400      05  FILLER          PIC X(23) VALUE 'VOLVO CAR PARTS     '.         
010500      05  FILLER          PIC X(74) VALUE 'W42610-001'.                   
010600      05  FILLER          PIC X(07) VALUE 'DATUM'.                        
010700      05  FILLER          PIC X(16) VALUE SPACE.                          
010800                                                                          
010900    03  RUBRIK1-GB.                                                       
011000      05  FILLER          PIC S9(03) COMP-3 VALUE +1.                     
011100      05  FILLER          PIC X(23) VALUE 'VOLVO CAR PARTS     '.         
011200      05  FILLER          PIC X(74) VALUE 'W42610-001'.                   
011300      05  FILLER          PIC X(07) VALUE 'DATE'.                         
011400      05  FILLER          PIC X(16) VALUE SPACE.                          
011500                                                                          
011600 01  FILLER REDEFINES RUBRIK1.                                            
011700    03  RUBRIK-1 OCCURS 3.                                                
011800      05  FILLER                PIC S9(03) COMP-3.                        
011900      05  FILLER                PIC X(104).                               
012000      05  RUB1-DATUM            PIC 99B99B99.                             
012100      05  FILLER                PIC X(08).                                
012200     EJECT                                                                
012300 01  RUBRIK2.                                                             
012400    03  RUBRIK2-S.                                                        
012500      05  FILLER                PIC S9(03) COMP-3 VALUE +3.               
012600      05  FILLER                PIC X(120) VALUE 'KVALITETSINDEX'.        
012700                                                                          
012800    03  RUBRIK2-NL.                                                       
012900      05  FILLER                PIC S9(03) COMP-3 VALUE +3.               
013000      05  FILLER               PIC X(120) VALUE 'KWALITEITSINDEX'.        
013100                                                                          
013200    03  RUBRIK2-GB.                                                       
013300      05  FILLER                PIC S9(03) COMP-3 VALUE +3.               
013400      05  FILLER               PIC X(120) VALUE 'QUALITY INDEX'.          
013500                                                                          
013600 01  FILLER REDEFINES RUBRIK2.                                            
013700    03  RUBRIK-2 OCCURS 3.                                                
013800      05  FILLER                PIC S9(03) COMP-3.                        
013900      05  FILLER                PIC X(120).                               
014000     EJECT                                                                
014100 01  RUBRIK3.                                                             
014200    03  RUBRIK3-S.                                                        
014300      05  FILLER                PIC S9(03) COMP-3 VALUE +3.               
014400      05  FILLER                PIC X(61) VALUE SPACE.                    
014500      05  FILLER                PIC X(59) VALUE 'PERIOD'.                 
014600                                                                          
014700    03  RUBRIK3-NL.                                                       
014800      05  FILLER                PIC S9(03) COMP-3 VALUE +3.               
014900      05  FILLER                PIC X(61) VALUE SPACE.                    
015000      05  FILLER                PIC X(59) VALUE 'PERIODE'.                
015100                                                                          
015200    03  RUBRIK3-GB.                                                       
015300      05  FILLER                PIC S9(03) COMP-3 VALUE +3.               
015400      05  FILLER                PIC X(61) VALUE SPACE.                    
015500      05  FILLER                PIC X(59) VALUE 'PERIOD'.                 
015600                                                                          
015700 01  FILLER REDEFINES RUBRIK3.                                            
015800    03  RUBRIK-3 OCCURS 3.                                                
015900      05  FILLER                PIC S9(03) COMP-3.                        
016000      05  FILLER                PIC X(120).                               
016100     EJECT                                                                
016200 01  RUBRIK-4.                                                            
016300    03  FILLER                PIC S9(03) COMP-3 VALUE +3.                 
016400    03  FILLER                PIC X(27) VALUE SPACE.                      
016500    03  FILLER OCCURS 13.                                                 
016600      05  RAD-TIAARP          PIC Z(06).                                  
016700    03  FILLER                PIC X(15) VALUE SPACE.                      
016800                                                                          
016900 01  RAD.                                                                 
017000    03  FILLER                PIC S9(03) COMP-3 VALUE +2.                 
017100   03  RAD-TEXT.                                                          
017200     05 RAD-TEXT1            PIC X(10).                                   
017300     05 RAD-TEXT2            PIC X(02).                                   
017400   03  RAD-IDKVAOMR          PIC X(02).                                   
017500   03  RAD-BEKVAOMR          PIC X(13).                                   
017600   03  FILLER OCCURS 13.                                                  
017700     05  RAD-REKVAIND        PIC -(06).                                   
017800   03  FILLER                PIC X(15)   VALUE SPACE.                     
017900     EJECT                                                                
018000*    ---- INFIL RELATIV STORLEK                                           
018100*01  W42602  -COPY W4260503        -PRE IN-                               
018200     EJECT                                                                
018300*    ---- SUBPROGRAM OCH PARAMETER-AREOR                                  
018400     SKIP3                                                                
018500 01  DYNAMISKA-SUBPROGRAM.                                                
018600   03  WDATKONV              PIC X(8)    VALUE 'WDATKONV'.                
018700     SKIP2                                                                
018800*    ----  PARAMETRAR TILL DATUMKORT                                      
018900                                                                          
019000 01  DATUMKORT-ID            PIC X(6)    VALUE 'WDATUM'.                  
019100     SKIP3                                                                
019200*    -COPY WDATAREA                                                       
019300     EJECT                                                                
019400 PROCEDURE DIVISION.                                                      
019500                                                                          
019600     PERFORM A-INIT                                                       
019700                                                                          
019800     READ W42602 INTO IN-W42602 AT END                                    
019900                   MOVE 'EOF' TO INFIL-SLUT                               
020000     END-READ                                                             
020100                                                                          
020200     PERFORM UNTIL INFIL-SLUT = 'EOF'                                     
020300                                                                          
020400       PERFORM B-BEHANDLING                                               
020500                                                                          
020600       READ W42602 INTO IN-W42602 AT END                                  
020700                     MOVE 'EOF' TO INFIL-SLUT                             
020800       END-READ                                                           
020900     END-PERFORM                                                          
021000                                                                          
021100     PERFORM Z-FINIT                                                      
021200                                                                          
021300     MOVE ZERO            TO RETURN-CODE                                  
021400     GOBACK                                                               
021500     .                                                                    
021600     EJECT                                                                
021700 A-INIT SECTION.                                                          
021800     SKIP2                                                                
021900     OPEN  INPUT W42602                                                   
022000          OUTPUT W42610-001 W42610-001A W42611 W42612                     
022100                                                                          
022200     MOVE 'IDAG'          TO DAT-KDDATFORM                                
022300     CALL WDATKONV     USING DAT-KDDATFORM                                
022400                             DAT-I-TIDATUM                                
022500                             DAT-O-TIDATUM                                
022600                             DAT-KDSVAR                                   
022700     .                                                                    
022800     EJECT                                                                
022900 B-BEHANDLING SECTION.                                                    
023000     SKIP2                                                                
023100     MOVE +1 TO IX1                                                       
023200     PERFORM UNTIL IX1 > +13                                              
023300       MOVE IN-TIAARP   (IX1)   TO W-TIAARP                               
023400       MOVE W-TIAARP            TO RAD-TIAARP   (IX1)                     
023500       MOVE IN-REKVAIND (IX1)   TO W-REKVAIND                             
023600       MOVE W-REKVAIND          TO RAD-REKVAIND (IX1)                     
023700       ADD +1                   TO IX1                                    
023800     END-PERFORM                                                          
023900                                                                          
023910     MOVE IN-IDDC               TO WS-IDDC                                
024000     EVALUATE TRUE                                                        
024100       WHEN  CDC-SE                                                       
024200          PERFORM BA-SKRIV-KVALITETSINDEX-CDC                             
024300       WHEN  SDC-NL                                                       
024400          PERFORM BB-SKRIV-KVALITETSINDEX-DC21                            
024500       WHEN  OTHER                                                        
024600          CONTINUE                                                        
024700     END-EVALUATE                                                         
024800                                                                          
024900     MOVE IN-IDDC   TO W-IDDC                                             
025000     .                                                                    
025100     EJECT                                                                
025200 BA-SKRIV-KVALITETSINDEX-CDC SECTION.                                     
025300     SKIP2                                                                
025400     IF IN-IDDC NOT = W-IDDC                                              
025500       MOVE DAT-TIAAMMDD        TO RUB1-DATUM (01)                        
025600       MOVE RUBRIK-1 (01)       TO W42610-LISTA-S-CDC                     
025700       WRITE W42610-LISTA-S-CDC AFTER PAGE                                
025800                                                                          
025900       MOVE DAT-TIAAMMDD        TO RUB1-DATUM (03)                        
026000       MOVE RUBRIK-1 (03)       TO W42610-LISTA-GB-CDC                    
026100       WRITE W42610-LISTA-GB-CDC AFTER PAGE                               
026200                                                                          
026300       MOVE RUBRIK-2 (01)       TO W42610-LISTA-S-CDC                     
026400       WRITE W42610-LISTA-S-CDC AFTER RAD-SKIP-S-CDC                      
026500                                                                          
026600       MOVE RUBRIK-2 (03)       TO W42610-LISTA-GB-CDC                    
026700       WRITE W42610-LISTA-GB-CDC AFTER RAD-SKIP-GB-CDC                    
026800                                                                          
026900       MOVE RUBRIK-3 (01)       TO W42610-LISTA-S-CDC                     
027000       WRITE W42610-LISTA-S-CDC AFTER RAD-SKIP-S-CDC                      
027100                                                                          
027200       MOVE RUBRIK-3 (03)       TO W42610-LISTA-GB-CDC                    
027300       WRITE W42610-LISTA-GB-CDC AFTER RAD-SKIP-GB-CDC                    
027400                                                                          
027500       MOVE RUBRIK-4            TO W42610-LISTA-S-CDC                     
027600       WRITE W42610-LISTA-S-CDC AFTER RAD-SKIP-S-CDC                      
027700                                                                          
027800       MOVE RUBRIK-4            TO W42610-LISTA-GB-CDC                    
027900       WRITE W42610-LISTA-GB-CDC AFTER RAD-SKIP-GB-CDC                    
028000     END-IF                                                               
028100                                                                          
028200     IF IN-IDPTYP = 'OMR'                                                 
028300        IF IN-IDDC NOT = W-IDDC                                           
028400           MOVE 'KONTR.OMR'     TO RAD-TEXT                               
028500        ELSE                                                              
028600           MOVE SPACE           TO RAD-TEXT                               
028700        END-IF                                                            
028800        MOVE IN-IDKVAOMR        TO RAD-IDKVAOMR                           
028900        MOVE IN-BEKVAOMR (01)   TO RAD-BEKVAOMR                           
029000                                                                          
029100        MOVE RAD                   TO W42610-LISTA-S-CDC                  
029200        WRITE W42610-LISTA-S-CDC AFTER ADVANCING RAD-SKIP-S-CDC           
029300                                                                          
029400        IF IN-IDDC NOT = W-IDDC                                           
029500           MOVE 'INSP.AREA'     TO RAD-TEXT                               
029600        ELSE                                                              
029700           MOVE SPACE           TO RAD-TEXT                               
029800        END-IF                                                            
029900        MOVE IN-IDKVAOMR        TO RAD-IDKVAOMR                           
030000        MOVE IN-BEKVAOMR (02)   TO RAD-BEKVAOMR                           
030100                                                                          
030200        MOVE RAD                   TO W42610-LISTA-GB-CDC                 
030300        WRITE W42610-LISTA-GB-CDC                                         
030400                               AFTER ADVANCING RAD-SKIP-GB-CDC            
030500     ELSE                                                                 
030600       IF IN-IDPTYP = 'DC '                                               
030700          MOVE 'LAGER'          TO RAD-TEXT1                              
030800          MOVE IN-IDDC          TO RAD-TEXT2                              
030900          MOVE SPACE            TO RAD-IDKVAOMR                           
031000                                   RAD-BEKVAOMR                           
031100          MOVE RAD              TO W42610-LISTA-S-CDC                     
031200          WRITE W42610-LISTA-S-CDC                                        
031300                               AFTER ADVANCING RAD-SKIP-S-CDC             
031400                                                                          
031500          MOVE 'WAREHOUSE'      TO RAD-TEXT1                              
031600          MOVE RAD              TO W42610-LISTA-GB-CDC                    
031700          WRITE W42610-LISTA-GB-CDC                                       
031800                              AFTER ADVANCING RAD-SKIP-GB-CDC             
031900       ELSE                                                               
032000         IF IN-IDPTYP = 'BUF'                                             
032100            MOVE 'BUFFERT'             TO RAD-TEXT1                       
032200            EVALUATE TRUE                                                 
032300              WHEN CDC-SE MOVE WC-CDC-SE  TO RAD-TEXT2                    
032400              WHEN SDC-NL MOVE WC-SDC-NL  TO RAD-TEXT2                    
032500              WHEN OTHER  MOVE '--'    TO RAD-TEXT2                       
032600            END-EVALUATE                                                  
032700            MOVE SPACE            TO RAD-IDKVAOMR                         
032800                                     RAD-BEKVAOMR                         
032900            MOVE RAD              TO W42610-LISTA-S-CDC                   
033000            WRITE W42610-LISTA-S-CDC AFTER ADVANCING                      
033100                                            RAD-SKIP-S-CDC                
033200                                                                          
033300            MOVE 'BUFFERT'      TO RAD-TEXT1                              
033400            MOVE RAD              TO W42610-LISTA-GB-CDC                  
033500           WRITE W42610-LISTA-GB-CDC AFTER ADVANCING                      
033600                                            RAD-SKIP-GB-CDC               
033700         END-IF                                                           
033800       END-IF                                                             
033900     END-IF                                                               
034000     .                                                                    
034100     EJECT                                                                
034200 BB-SKRIV-KVALITETSINDEX-DC21 SECTION.                                    
034300     SKIP2                                                                
034400     IF IN-IDDC NOT = W-IDDC                                              
034500       MOVE DAT-TIAAMMDD        TO RUB1-DATUM (02)                        
034600       MOVE RUBRIK-1 (02)       TO W42611-LISTA-NL-DC21                   
034700       WRITE W42611-LISTA-NL-DC21 AFTER PAGE                              
034800                                                                          
034900       MOVE DAT-TIAAMMDD        TO RUB1-DATUM (03)                        
035000       MOVE RUBRIK-1 (03)       TO W42612-LISTA-GB-DC21                   
035100       WRITE W42612-LISTA-GB-DC21 AFTER PAGE                              
035200                                                                          
035300       MOVE RUBRIK-2 (02)       TO W42611-LISTA-NL-DC21                   
035400       WRITE W42611-LISTA-NL-DC21 AFTER RAD-SKIP-NL-DC21                  
035500                                                                          
035600       MOVE RUBRIK-2 (03)       TO W42612-LISTA-GB-DC21                   
035700       WRITE W42612-LISTA-GB-DC21 AFTER RAD-SKIP-GB-DC21                  
035800                                                                          
035900       MOVE RUBRIK-3 (02)       TO W42611-LISTA-NL-DC21                   
036000       WRITE W42611-LISTA-NL-DC21 AFTER RAD-SKIP-NL-DC21                  
036100                                                                          
036200       MOVE RUBRIK-3 (03)       TO W42612-LISTA-GB-DC21                   
036300       WRITE W42612-LISTA-GB-DC21 AFTER RAD-SKIP-GB-DC21                  
036400                                                                          
036500       MOVE RUBRIK-4            TO W42611-LISTA-NL-DC21                   
036600       WRITE W42611-LISTA-NL-DC21 AFTER RAD-SKIP-NL-DC21                  
036700                                                                          
036800       MOVE RUBRIK-4            TO W42612-LISTA-GB-DC21                   
036900       WRITE W42612-LISTA-GB-DC21 AFTER RAD-SKIP-GB-DC21                  
037000     END-IF                                                               
037100                                                                          
037200     IF IN-IDPTYP = 'OMR'                                                 
037300        IF IN-IDDC NOT = W-IDDC                                           
037400           MOVE 'INSP.AREA'     TO RAD-TEXT                               
037500        ELSE                                                              
037600           MOVE SPACE           TO RAD-TEXT                               
037700        END-IF                                                            
037800        MOVE IN-IDKVAOMR        TO RAD-IDKVAOMR                           
037900        MOVE IN-BEKVAOMR (01)   TO RAD-BEKVAOMR                           
038000                                                                          
038100        MOVE RAD                   TO W42611-LISTA-NL-DC21                
038200        WRITE W42611-LISTA-NL-DC21                                        
038210                               AFTER ADVANCING RAD-SKIP-NL-DC21           
038300                                                                          
038400        IF IN-IDDC NOT = W-IDDC                                           
038500           MOVE 'INSP.AREA'     TO RAD-TEXT                               
038600        ELSE                                                              
038700           MOVE SPACE           TO RAD-TEXT                               
038800        END-IF                                                            
038900        MOVE IN-IDKVAOMR        TO RAD-IDKVAOMR                           
039000        MOVE IN-BEKVAOMR (02)   TO RAD-BEKVAOMR                           
039100                                                                          
039200        MOVE RAD                   TO W42612-LISTA-GB-DC21                
039300        WRITE W42612-LISTA-GB-DC21                                        
039310                               AFTER ADVANCING RAD-SKIP-GB-DC21           
039400     ELSE                                                                 
039500       IF IN-IDPTYP = 'DC '                                               
039600          MOVE 'MAGAZIJN'       TO RAD-TEXT1                              
039700          MOVE IN-IDDC          TO RAD-TEXT2                              
039800          MOVE SPACE            TO RAD-IDKVAOMR                           
039900                                   RAD-BEKVAOMR                           
040000          MOVE RAD              TO W42611-LISTA-NL-DC21                   
040100         WRITE W42611-LISTA-NL-DC21                                       
040110                                AFTER ADVANCING RAD-SKIP-NL-DC21          
040200                                                                          
040300          MOVE 'WAREHOUSE'      TO RAD-TEXT1                              
040400          MOVE RAD              TO W42612-LISTA-GB-DC21                   
040500         WRITE W42612-LISTA-GB-DC21                                       
040510                                AFTER ADVANCING RAD-SKIP-GB-DC21          
040600       ELSE                                                               
040700         IF IN-IDPTYP = 'BUF'                                             
040800            MOVE 'BUFFERT'        TO RAD-TEXT1                            
040900            EVALUATE TRUE                                                 
041000              WHEN CDC-SE MOVE WC-CDC-SE     TO RAD-TEXT2                 
041100              WHEN SDC-NL MOVE WC-SDC-NL    TO RAD-TEXT2                  
041200              WHEN OTHER  MOVE '--'    TO RAD-TEXT2                       
041300            END-EVALUATE                                                  
041900            MOVE SPACE            TO RAD-IDKVAOMR                         
042000                                     RAD-BEKVAOMR                         
042100            MOVE RAD              TO W42611-LISTA-NL-DC21                 
042200            WRITE W42611-LISTA-NL-DC21 AFTER ADVANCING                    
042300                                             RAD-SKIP-NL-DC21             
042400                                                                          
042500            MOVE 'BUFFERT'       TO RAD-TEXT1                             
042600            MOVE RAD              TO W42612-LISTA-GB-DC21                 
042700            WRITE W42612-LISTA-GB-DC21 AFTER ADVANCING                    
042800                                             RAD-SKIP-GB-DC21             
042900         END-IF                                                           
043000       END-IF                                                             
043100     END-IF                                                               
043200     .                                                                    
043300     EJECT                                                                
043400 Z-FINIT SECTION.                                                         
043500     SKIP2                                                                
043600     CLOSE W42602 W42610-001 W42610-001A W42611 W42612                    
043700     .                                                                    
043800     EJECT                                                                
