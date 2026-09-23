000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W4128200.                                                
000300 AUTHOR.         ANDRE KJELL.                                             
000400 DATE-WRITTEN.   09/08/25.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*                                                                         
000800*    FUNCTION:                                                            
000900*        EXTRACT ORDER DATA FROM UPLOADED .CSV FILE AND                   
001000*        WRITE A FILE IN A FORMAT THAT IS EASIER TO PROCESS.              
001100*                                                                         
001200*        2013-03. NEW "MULTIPLE DEALER" FORMAT ADDED (ET 10191998)        
001300*                                                                         
001400*        Vinter 2017/18 "Proforma" FORMAT ADDED (Jira 793)                
001500*                                                                         
001600                                                                          
001700     EJECT                                                                
001800 ENVIRONMENT DIVISION.                                                    
001900 INPUT-OUTPUT SECTION.                                                    
002000 FILE-CONTROL.                                                            
002100                                                                          
002200*          --- CSV FILE                                                   
002300     SELECT W41281                     ASSIGN TO W41282D1.                
002400                                                                          
002500*          --- EXTRACTED ORDER DATA                                       
002600     SELECT W41282                     ASSIGN TO W41282D2.                
002700                                                                          
002800     EJECT                                                                
002900 DATA DIVISION.                                                           
003000 FILE SECTION.                                                            
003100                                                                          
003200 FD  W41281                                                               
003300     RECORDING       V                                                    
003400     BLOCK CONTAINS  0.                                                   
003500                                                                          
003600 01  FILLER          PIC X(496).                                          
003700                                                                          
003800                                                                          
003900 FD  W41282                                                               
004000     RECORDING       V                                                    
004100     BLOCK CONTAINS  0.                                                   
004200                                                                          
004300*01  RECORD -COPY W4128201 -PRE  OHDR-  -L.                               
004400*01  RECORD -COPY W4128202 -PRE  ODTA-  -L.                               
004500     EJECT                                                                
004600 WORKING-STORAGE SECTION.                                                 
004700                                                                          
004800 77  IDPGM                       PIC X(8)    VALUE 'W4128200'.            
004900 77  YES                         PIC X       VALUE 'J'.                   
005000 77  NOO                         PIC X       VALUE 'N'.                   
005100 77  CARR-RETURN                 PIC X       VALUE X'0D'.                 
005100 77  LOW-VAL                     PIC X       VALUE X'00'.                 
005200                                                                          
005300*    -- DIFFERENT TYPE OF INPUT FORMAT IN EXCEL FILE                      
005400 77  SPX-IDSYSTEM                PIC X(4)    VALUE 'SPX '.                
005500 77  DEALER-IDSYSTEM             PIC X(4)    VALUE 'XCEL'.                
005600 77  MULTI-DEALER-IDSYSTEM       PIC X(4)    VALUE 'MXCL'.                
005700 77  PROFORMA-IDSYSTEM           PIC X(4)    VALUE 'PXCL'.                
005800 77  REFILL-IDSYSTEM             PIC X(4)    VALUE 'RXCL'.                
005810 77  PREPLANNED-IDSYSTEM         PIC X(4)    VALUE 'PPXC'.                
005900 77  WS-FLFORBI                  PIC X(1)    VALUE SPACE.                 
006000                                                                          
006100*    -- NUMBER OF BLANK LINES BEFORE WE STOP READING.                     
006200*    -- FOR EACH TYPE OF INPUT FORMAT                                     
006300 77  MAX-BLANKS-SPX              PIC S9(1) COMP-3 VALUE 0.                
006400 77  MAX-BLANKS-DEALER           PIC S9(1) COMP-3 VALUE 0.                
006500 77  MAX-BLANKS-MULTI-DEALER     PIC S9(1) COMP-3 VALUE 1.                
006600 77  MAX-BLANKS-REFILL           PIC S9(1) COMP-3 VALUE 0.                
006700 77  RECENT-BLANK-LINES          PIC S9(1) COMP-3 VALUE 0.                
006800 77  MAX-BLANKS-PROFORMA         PIC S9(1) COMP-3 VALUE 0.                
006900                                                                          
007000 77  W41281-EOF-SW               PIC X       VALUE 'N'.                   
007100     88  END-OF-W41281                       VALUE 'J'.                   
007200                                                                          
007300 77  READ-SWITCH                 PIC X       VALUE 'J'.                   
007400     88  CONTINUE-READING                    VALUE 'J'.                   
007500     88  STOP-READING                        VALUE 'N'.                   
007600                                                                          
007700 77  CUSTOMER-STRUCTURE-SW       PIC X       VALUE 'N'.                   
007800     88  CUSTOMER-STRUCTURE                  VALUE 'J'.                   
007900     88  CUSTOMER-ZERO                       VALUE 'N'.                   
008000                                                                          
008100 77  PART-NO-LINE-SW             PIC X       VALUE 'N'.                   
008200     88  NOT-YET-PART-NO-LINE                VALUE 'N'.                   
008300     88  PART-NO-LINE-FOUND                  VALUE 'J'.                   
008400                                                                          
008500 77  HEADER-LINE-SW              PIC X       VALUE 'N'.                   
008600     88  NOT-YET-HEADER-LINE                 VALUE 'N'.                   
008700     88  HEADER-LINE-FOUND                   VALUE 'J'.                   
008800                                                                          
008900*    --- GENERAL LOOP INDICES AND OTHER COUNTERS                          
009000 77  IX                          PIC S9(4)   BINARY.                      
009100 77  START-IX                    PIC S9(4)   BINARY.                      
009200 77  CCOUNT                      PIC S9(4)   BINARY.                      
009300                                                                          
009400     EJECT                                                                
009500 01  GENERAL-SUBPROGRAMS.                                                 
009600*                                                                         
009700     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
009800     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
009900                                                                          
010000*    --- PARAMETERS TO ABEND                                              
010100 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
010200 77  RKOD-ABEND-NO-DUMP          PIC S9(4)   COMP VALUE +16.              
010300 77  RKOD-ABEND-WITH-DUMP        PIC S9(4)   COMP VALUE +1000.            
010400                                                                          
010500 01  ERROR-TEXT.                                                          
010600     03  FILLER                  PIC X(12)   VALUE 'ERROR-TEXT'.          
010700     03  ERROR-TEXT-STR          PIC X(72)   VALUE SPACE.                 
010800     EJECT                                                                
010900*    --- PARAMETERS TO POSTSUM                                            
011000*                                                                         
011100*01  -COPY W0005   -PRE  POSTSUM-                                         
011200                                                                          
011300     EJECT                                                                
011400 01  LINE-NO                     PIC S9(4)   BINARY VALUE ZERO.           
011500                                                                          
011600 01  CSV-CELLS-START             PIC X(24)   VALUE                        
011700                                 'CSV-CELLS-START        '.               
011800 01  CELL-WIDTH                  PIC S9(4) BINARY VALUE 25.               
011900 01  CELL-LIMIT                  PIC S9(4) BINARY VALUE 50.               
012000 01  CELLS.                                                               
012100     03 CELL  OCCURS 50          PIC X(25).                               
012200                                                                          
012300 01  CSV-LENGTH-START            PIC X(24)   VALUE                        
012400                                 'CSV-LENGTHS-START       '.              
012500 01  CELL-VALUE-LENGTHS.                                                  
012600     03 LENG  OCCURS 50          PIC S9(4)   BINARY.                      
012700                                                                          
012800 01  PART-NBRS-START             PIC X(24)   VALUE                        
012900                                 'PART-NBRS-START         '.              
013000 77  PX                          PIC S9(4)   BINARY.                      
013100 77  PX-MAX                      PIC S9(4)   BINARY.                      
013200 77  PX-LIMIT                    PIC S9(4)   BINARY VALUE 40.             
013300 01  PART-NBRS.                                                           
013400     03 TAB-IDARTNR OCCURS 40    PIC X(15).                               
013500                                                                          
013600     EJECT                                                                
013700 01  CSV-AREA-START              PIC X(24)   VALUE                        
013800                                 'CSV-AREA-START          '.              
013900 01  CSV-AREA.                                                            
014000     03 FILLER                   PIC X(496).                              
014100     EJECT                                                                
014200 01  OHDR-AREA-START             PIC X(24)   VALUE                        
014300                                 'OHDR-AREA-START         '.              
014400*01  -COPY W4128201                                                       
014500                                                                          
014600                                                                          
014700 01  ODTA-AREA-START             PIC X(24)   VALUE                        
014800                                 'ODTA-AREA-START         '.              
014900*01  -COPY W4128202                                                       
015000                                                                          
015100     EJECT                                                                
015200 PROCEDURE DIVISION.                                                      
015300 MAIN SECTION.                                                            
015400                                                                          
015500     PERFORM A-INIT                                                       
015600     PERFORM S01-READ-W41281                                              
015700     PERFORM UNTIL END-OF-W41281 OR NOT CONTINUE-READING                  
015800       PERFORM B-ANALYZE-CSV-RECORD                                       
015900       IF CONTINUE-READING                                                
016000         PERFORM S01-READ-W41281                                          
016100       END-IF                                                             
016200     END-PERFORM                                                          
016300                                                                          
016400     PERFORM Z-FINIT                                                      
016500                                                                          
016600     MOVE ZERO TO RETURN-CODE                                             
016700     GOBACK                                                               
016800     .                                                                    
016900     EJECT                                                                
017000 A-INIT SECTION.                                                          
017100                                                                          
017200     OPEN INPUT  W41281                                                   
017300     OPEN OUTPUT W41282                                                   
017400                                                                          
017500     MOVE SPACE TO OHDR-W4128201                                          
017600     MOVE SPACE TO ODTA-W4128202                                          
017700     .                                                                    
017800     EJECT                                                                
017900 B-ANALYZE-CSV-RECORD SECTION.                                            
018000                                                                          
018100*    -- DETERMIN IDSYSTEM                                                 
018200*    -- CHECK IF DEALER ORDER (VCCS... IN FIRST CELL ON LINE 1)           
018300     IF LINE-NO = 1                                                       
018400        MOVE 0 TO CCOUNT                                                  
018500        INSPECT CELL(1) TALLYING CCOUNT                                   
018600                FOR CHARACTERS BEFORE 'VCCS'                              
018700        IF CCOUNT < CELL-WIDTH                                            
018800          INSPECT CELL(1) TALLYING CCOUNT                                 
018900                  FOR CHARACTERS BEFORE 'Parts'                           
019000          IF CCOUNT < CELL-WIDTH                                          
019100*           -- Single dealer format                                       
019200            MOVE DEALER-IDSYSTEM TO OHDR-IDSYSTEM                         
019300*           -- WRITE A HEADER RECORD WITH IDSYSTEM                        
019400*           -- (BLANK MARKET NAME FOR THIS TYPE)                          
019500            MOVE 'N' TO WS-FLFORBI                                        
019600            PERFORM S11-WRITE-W41282-OHDR                                 
019700          ELSE                                                            
019800            MOVE 0 TO CCOUNT                                              
019900            INSPECT CELL(1) TALLYING CCOUNT                               
020000                    FOR CHARACTERS BEFORE 'Bypass'                        
020100            IF CCOUNT < CELL-WIDTH                                        
020200*             -- Multiple dealer orders                                   
020300              MOVE MULTI-DEALER-IDSYSTEM TO OHDR-IDSYSTEM                 
020400*             -- WRITE A HEADER RECDORD WITH IDSYSTEM                     
020500*             -- (BLANK MARKET NAME FOR THIS TYPE too)                    
020600              MOVE 'J' TO WS-FLFORBI                                      
020700              PERFORM S11-WRITE-W41282-OHDR                               
020800            ELSE                                                          
020900               MOVE 0 TO CCOUNT                                           
021000               INSPECT CELL(1) TALLYING CCOUNT                            
021100                       FOR CHARACTERS BEFORE 'Multi'                      
021200               IF CCOUNT < CELL-WIDTH                                     
021300*                -- Multiple dealer orders                                
021400                 MOVE MULTI-DEALER-IDSYSTEM TO OHDR-IDSYSTEM              
021500*                -- WRITE A HEADER RECDORD WITH IDSYSTEM                  
021600*                -- (BLANK MARKET NAME FOR THIS TYPE too)                 
021700                 MOVE 'N' TO WS-FLFORBI                                   
021800                 PERFORM S11-WRITE-W41282-OHDR                            
021900               ELSE                                                       
022000                 MOVE 0 TO CCOUNT                                         
022100                 INSPECT CELL(1) TALLYING CCOUNT                          
022200                         FOR CHARACTERS BEFORE 'Proforma'                 
022300                 IF CCOUNT < CELL-WIDTH                                   
022400*                -- Proforma orders                                       
022500                   MOVE PROFORMA-IDSYSTEM TO OHDR-IDSYSTEM                
022600*                -- WRITE A HEADER RECDORD WITH IDSYSTEM                  
022700*                -- (BLANK MARKET NAME FOR THIS TYPE too)                 
022800                   MOVE 'N' TO WS-FLFORBI                                 
022900                   PERFORM S11-WRITE-W41282-OHDR                          
023000                 ELSE                                                     
023100                   MOVE 0 TO CCOUNT                                       
023200                   INSPECT CELL(1) TALLYING CCOUNT                        
023300                           FOR CHARACTERS BEFORE 'Refill'                 
023400                   IF CCOUNT < CELL-WIDTH                                 
023500                     MOVE REFILL-IDSYSTEM TO OHDR-IDSYSTEM                
023600*                  -- WRITE A HEADER RECDORD WITH IDSYSTEM                
023700*                  -- (BLANK MARKET NAME FOR THIS TYPE too)               
023800                     MOVE 'N' TO WS-FLFORBI                               
023900                     PERFORM S11-WRITE-W41282-OHDR                        
024000                   ELSE                                                   
024100                     MOVE 0 TO CCOUNT                                     
024200                     INSPECT CELL(1) TALLYING CCOUNT                      
024300                             FOR CHARACTERS BEFORE 'Bypref'               
024400                     IF CCOUNT < CELL-WIDTH                               
024500                       MOVE REFILL-IDSYSTEM TO OHDR-IDSYSTEM              
024600*                    -- WRITE A HEADER RECDORD WITH IDSYSTEM              
024700*                    -- (BLANK MARKET NAME FOR THIS TYPE too)             
024800                       MOVE 'J' TO WS-FLFORBI                             
024900                       PERFORM S11-WRITE-W41282-OHDR                      
025010                     ELSE                                                 
025020                       MOVE 0 TO CCOUNT                                   
025030                       INSPECT CELL(1) TALLYING CCOUNT                    
025040                               FOR CHARACTERS BEFORE 'Preplanned'         
025050                       IF CCOUNT < CELL-WIDTH                             
025060                         MOVE PREPLANNED-IDSYSTEM TO OHDR-IDSYSTEM        
025070*                      -- WRITE A HEADER RECDORD WITH IDSYSTEM            
025080*                      -- (BLANK MARKET NAME FOR THIS TYPE too)           
025090                         MOVE 'N' TO WS-FLFORBI                           
025091                         PERFORM S11-WRITE-W41282-OHDR                    
025092                       END-IF                                             
025100                     END-IF                                               
025110                   END-IF                                                 
025200                 END-IF                                                   
025300               END-IF                                                     
025400            END-IF                                                        
025500                                                                          
025600          END-IF                                                          
025700        END-IF                                                            
025800     END-IF                                                               
025900                                                                          
026000*    -- CHECK IF SPX ORDER                                                
026100*    -- ("Newsletter" OR "Number" IN FIRST CELL ON LINE 5)                
026200*    -- WAIT FOR LINE 6 BEFORE WRITING A HEADER RECORD                    
026300     IF LINE-NO = 5                                                       
026400        MOVE 0 TO CCOUNT                                                  
026500        INSPECT CELL(1) TALLYING CCOUNT                                   
026600           FOR CHARACTERS BEFORE 'Newsletter'                             
026700        IF CCOUNT < CELL-WIDTH                                            
026800          MOVE SPX-IDSYSTEM  TO  OHDR-IDSYSTEM                            
026900        ELSE                                                              
027000          MOVE 0 TO CCOUNT                                                
027100          INSPECT CELL(1) TALLYING CCOUNT                                 
027200             FOR CHARACTERS BEFORE 'Number'                               
027300          IF CCOUNT < CELL-WIDTH                                          
027400            MOVE SPX-IDSYSTEM  TO  OHDR-IDSYSTEM                          
027500          END-IF                                                          
027600        END-IF                                                            
027700     END-IF                                                               
027800                                                                          
027900*    -- THE FOLLWING SECTIONS WILL ONLY BE PERFORMED AFTER                
028000*    -- ONE OF THE PREVIOUS CHECKS HAVE BEEN SATISFIED                    
028100     IF OHDR-IDSYSTEM = SPX-IDSYSTEM                                      
028200        PERFORM BA-SPX-CSV-ANALYZIS                                       
028300     END-IF                                                               
028400                                                                          
028500     IF OHDR-IDSYSTEM = DEALER-IDSYSTEM                                   
028600        PERFORM BB-DEALER-CSV-ANALYZIS                                    
028700     END-IF                                                               
028800                                                                          
028900     IF OHDR-IDSYSTEM = MULTI-DEALER-IDSYSTEM OR                          
028910                        PREPLANNED-IDSYSTEM                               
029000        PERFORM BC-MULTI-DEALER-CSV-ANALYZIS                              
029100     END-IF                                                               
029200                                                                          
029300     IF OHDR-IDSYSTEM = PROFORMA-IDSYSTEM                                 
029400        PERFORM BD-PROFORMA-CSV-ANALYZIS                                  
029500     END-IF                                                               
029600                                                                          
029700     IF OHDR-IDSYSTEM = REFILL-IDSYSTEM                                   
029800        PERFORM BE-REFILL-CSV-ANALYZIS                                    
029900     END-IF                                                               
030000     .                                                                    
030100     EJECT                                                                
030200 BA-SPX-CSV-ANALYZIS SECTION.                                             
030300                                                                          
030400*    -- LINES AFTER THE 'Volvo Dealer' HEADER SHOULD CONTAIN              
030500*    -- CUSTOMER(DISTRICT NUMBERS AND QUANTITIES                          
030600     IF PART-NO-LINE-FOUND                                                
030700       IF LENG(1) > 0                                                     
030800         MOVE ZERO TO RECENT-BLANK-LINES                                  
030900                                                                          
031000*        -- A LINE WITH CUSTOMER NBR (OR DISTRICT)                        
031100*        -- THE LINE CONTAINS ORDER LINE DATA                             
031200         IF CUSTOMER-STRUCTURE                                            
031300*          -- DISTRICT ALREADY SET                                        
031400           MOVE CELL(1) TO ODTA-IDKUNDNR                                  
031500         ELSE                                                             
031600           MOVE CELL(1) TO ODTA-IDDISTR                                   
031700           MOVE '0'     TO ODTA-IDKUNDNR                                  
031800         END-IF                                                           
031900                                                                          
032000*        -- NO REAL VALUE SET HERE.                                       
032100*        -- TIME-STAMP WILL BE SET IN NEXT PROGRAM TO                     
032200*        -- GET THE SAME VALUE AS IN RETURNING MAIL                       
032300         MOVE 'SPX'     TO ODTA-BERADREF                                  
032400         MOVE 'SPX'     TO ODTA-BEVARREF                                  
032500                                                                          
032600*        -- WRITE AN ORDER RECORD FOR ALL SAVED PART NBRS                 
032700*        -- IF A QUANTITY HAS BEEN SPECIFIED IN THE CORRESPONDING         
032800*        -- COLUMN                                                        
032900         MOVE 1 TO PX                                                     
033000         MOVE START-IX TO IX                                              
033100         PERFORM UNTIL PX > PX-MAX                                        
033200           IF CELL(IX) NOT = SPACE AND '0'                                
033300*            -- A NON-ZERO QUANTITY IS SPECIFIED FOR THIS PART            
033400             MOVE TAB-IDARTNR(PX) TO ODTA-IDARTNR                         
033500             MOVE CELL(IX)        TO ODTA-KVBEART                         
033600             MOVE WS-FLFORBI      TO ODTA-FLFORBI                         
033700             PERFORM S12-WRITE-W41282-ODTA                                
033800           END-IF                                                         
033900           ADD 1 TO PX   ADD 1 TO IX                                      
034000         END-PERFORM                                                      
034100       ELSE                                                               
034200         IF CELL(1) = SPACE                                               
034300           ADD 1 TO RECENT-BLANK-LINES                                    
034400           IF RECENT-BLANK-LINES > MAX-BLANKS-SPX                         
034500*          -- NO MORE CUSTOMERS/DISTRICTS IN INPUT FILE                   
034600*          -- SO WE CAN AS WELL STOP READING                              
034700             SET STOP-READING TO TRUE                                     
034800           END-IF                                                         
034900         END-IF                                                           
035000       END-IF                                                             
035100     END-IF                                                               
035200                                                                          
035300*    -- NO 'Volvo Dealer' HEADER YET                                      
035400*    -- INSPECT LINE FOR ORDER HEADER INFO                                
035500     IF NOT-YET-PART-NO-LINE                                              
035600       MOVE 1 TO CCOUNT                                                   
035700       INSPECT CELL(1) TALLYING CCOUNT                                    
035800          FOR LEADING SPACE                                               
035900                                                                          
036000       IF CCOUNT <= CELL-WIDTH - 5                                        
036100       AND CELL(1)(CCOUNT:6) = 'Market'                                   
036200          MOVE CELL(2) TO OHDR-BEMARKN                                    
036300*         -- TIME TO WRITE A HEADER RECORD                                
036400*         -- (WITH SYSTEM AND MARKET NAME)                                
036500          MOVE 'N' TO WS-FLFORBI                                          
036600          PERFORM S11-WRITE-W41282-OHDR                                   
036700       END-IF                                                             
036800                                                                          
036900       IF CCOUNT <= CELL-WIDTH - 7                                        
037000       AND CELL(1)(CCOUNT:8) = 'District'                                 
037100          MOVE CELL(2) TO ODTA-IDDISTR                                    
037200          IF ODTA-IDDISTR = SPACE                                         
037300*            -- NO DISTRICT - THEY COME LATER                             
037400*            -- AND CUSTOMER SHOULD BE ZERO                               
037500             SET CUSTOMER-ZERO         TO TRUE                            
037600          ELSE                                                            
037700*            -- A DISTRICT IS SPECIFIED.                                  
037800*            -- CUSTOMER NUMBERS COME LATER                               
037900             SET CUSTOMER-STRUCTURE TO TRUE                               
038000          END-IF                                                          
038100       END-IF                                                             
038200                                                                          
038300       IF CCOUNT <= CELL-WIDTH - 11                                       
038400       AND CELL(1)(CCOUNT:12) = 'Order placed'                            
038500*        -- CELL 8 CONTAINS "CL - x" WHERE x=ORDER CLASS                  
038600         MOVE 3 TO CCOUNT                                                 
038700         INSPECT CELL(8) TALLYING CCOUNT                                  
038800            FOR CHARACTERS BEFORE '- '                                    
038900         IF CCOUNT < CELL-WIDTH + 3                                       
039000           MOVE CELL(8)(CCOUNT:) TO ODTA-KDORDKL                          
039100         ELSE                                                             
039200*          -- CELL 8 CONTAINS ONLY CLASS                                  
039300           MOVE 1 TO CCOUNT                                               
039400           INSPECT CELL(8) TALLYING CCOUNT                                
039500               FOR LEADING SPACE                                          
039600           IF CCOUNT < CELL-WIDTH                                         
039700             MOVE CELL(8)(CCOUNT:) TO ODTA-KDORDKL                        
039800           END-IF                                                         
039900         END-IF                                                           
040000       END-IF                                                             
040100                                                                          
040200       IF CCOUNT <= CELL-WIDTH - 11                                       
040300       AND CELL(1)(CCOUNT:12) = 'Volvo Dealer'                            
040400*        -- CELLS 3 (OR 4 OR ...) AND HIGHER CONTAIN PART NUMBER          
040500*        -- SAVE THEM IN A TABLE FOR LATER USE                            
040600         MOVE 3 TO IX START-IX                                            
040700         IF LENG(IX) > CELL-WIDTH                                         
040800           MOVE CELL-WIDTH TO LENG(IX)                                    
040900         END-IF                                                           
041000         PERFORM UNTIL IX > 10                                            
041100         OR (LENG(IX) > 0 AND CELL(IX)(1:LENG(IX)) NUMERIC)               
041200           ADD 1 TO IX START-IX                                           
041300           IF LENG(IX) > CELL-WIDTH                                       
041400             MOVE CELL-WIDTH TO LENG(IX)                                  
041500           END-IF                                                         
041600         END-PERFORM                                                      
041700                                                                          
041800         MOVE 0 TO PX                                                     
041900         PERFORM UNTIL LENG(IX) = 0                                       
042000         OR CELL(IX)(1:LENG(IX)) NOT NUMERIC                              
042100         OR PX >= PX-LIMIT                                                
042200           ADD 1 TO PX                                                    
042300           MOVE CELL(IX) TO TAB-IDARTNR(PX)                               
042400           ADD 1 TO IX                                                    
042500           IF LENG(IX) > CELL-WIDTH                                       
042600             MOVE CELL-WIDTH TO LENG(IX)                                  
042700           END-IF                                                         
042800         END-PERFORM                                                      
042900         MOVE PX TO PX-MAX                                                
043000                                                                          
043100         SET PART-NO-LINE-FOUND TO TRUE                                   
043200       END-IF                                                             
043300                                                                          
043400     END-IF                                                               
043500     .                                                                    
043600                                                                          
043700     EJECT                                                                
043800 BB-DEALER-CSV-ANALYZIS SECTION.                                          
043900                                                                          
044000*    -- LINES AFTER THE 'Part No.' HEADER SHOULD CONTAIN                  
044100*    -- PART NUMBERS AND QUANTITIES                                       
044200     IF PART-NO-LINE-FOUND                                                
044300        IF LENG(1) > 0                                                    
044400          MOVE ZERO TO RECENT-BLANK-LINES                                 
044500                                                                          
044600*         -- A LINE WITH PART NUMBER                                      
044700*         -- THE LINE CONTAINS ORDER LINE DATA                            
044800          MOVE CELL(1) TO ODTA-IDARTNR                                    
044900          MOVE CELL(3) TO ODTA-KVBEART                                    
045000          IF LENG(5) > 0                                                  
045000            INSPECT CELL(5)                                               
045000               REPLACING ALL LOW-VAL BY SPACE                             
045000          END-IF                                                          
045000          MOVE CELL(5) TO ODTA-BERADREF                                   
045100                                                                          
045200          MOVE WS-FLFORBI      TO ODTA-FLFORBI                            
045300          PERFORM S12-WRITE-W41282-ODTA                                   
045400        ELSE                                                              
045500          ADD 1 TO RECENT-BLANK-LINES                                     
045600          IF RECENT-BLANK-LINES > MAX-BLANKS-DEALER                       
045700*           -- NO MORE PART NUMBERS IN INPUT FILE                         
045800*           -- SO WE CAN AS WELL STOP READING                             
045900            SET STOP-READING TO TRUE                                      
046000          END-IF                                                          
046100        END-IF                                                            
046200     END-IF                                                               
046300                                                                          
046400*    -- NO 'Part no.' HEADER YET                                          
046500*    -- INSPECT LINE FOR ORDER HEADER INFO                                
046600     IF NOT-YET-PART-NO-LINE                                              
046700       MOVE 1 TO CCOUNT                                                   
046800       INSPECT CELL(1) TALLYING CCOUNT                                    
046900          FOR LEADING SPACE                                               
047000                                                                          
047100       IF CCOUNT <= CELL-WIDTH - 7                                        
047200       AND CELL(1)(CCOUNT:8) = 'District'                                 
047300          MOVE CELL(2) TO ODTA-IDDISTR                                    
047400       END-IF                                                             
047500                                                                          
047600       IF CCOUNT <= CELL-WIDTH - 5                                        
047700       AND CELL(1)(CCOUNT:6) = 'Dealer'                                   
047800          MOVE CELL(2) TO ODTA-IDKUNDNR                                   
047900       END-IF                                                             
048000                                                                          
048100       IF CCOUNT <= CELL-WIDTH - 6                                        
048200       AND CELL(1)(CCOUNT:7) = 'Order c'                                  
048300          MOVE CELL(2) TO ODTA-KDORDKL                                    
048400       END-IF                                                             
048500                                                                          
048600       IF CCOUNT <= CELL-WIDTH - 6                                        
048700       AND CELL(1)(CCOUNT:7) = 'Order r'                                  
048800          MOVE CELL(2) TO ODTA-BEKUNDRF                                   
048900       END-IF                                                             
049000                                                                          
049100       IF CCOUNT <= CELL-WIDTH - 3                                        
049200       AND CELL(1)(CCOUNT:4) = 'Part'                                     
049300          SET PART-NO-LINE-FOUND TO TRUE                                  
049400       END-IF                                                             
049500     END-IF                                                               
049600     .                                                                    
049700                                                                          
049800     EJECT                                                                
049900 BC-MULTI-DEALER-CSV-ANALYZIS SECTION.                                    
050000                                                                          
050100*    -- A HEADER LINE WITH the TEXT 'District' IN COL 1 STARTS            
050200*    -- A SEQUENCE OF ORDER LINES WITH PART NUMBERS + QANTITIES           
050300*    -- AND OPTIONALLY ORDER HEAD DATA.                                   
050400     IF HEADER-LINE-FOUND                                                 
050500        IF LENG(7) > 0                                                    
050600          MOVE ZERO TO RECENT-BLANK-LINES                                 
050700                                                                          
050800*         -- A LINE WITH PART NUMBER                                      
050900*         -- THE LINE CONTAINS ORDER LINE DATA                            
051000          IF LENG(1) > 0 OR LENG(2) > 0 OR LENG(3) > 0                    
051100*           -- DISTRICT, CUSTOMER OR ORDER CLASS IS GIVEN                 
051200*           -- THIS INDICATES A NEW ORDER.                                
051300*           -- CLEAR PREVIOUS VALUES IF THE COLUMN IS BLANK.              
051400            IF LENG(1) > 0                                                
051500              MOVE CELL(1) TO ODTA-IDDISTR                                
051600            ELSE                                                          
051700              MOVE SPACE TO   ODTA-IDDISTR                                
051800            END-IF                                                        
051900            IF LENG(2) > 0                                                
052000              MOVE CELL(2) TO ODTA-IDKUNDNR                               
052100            ELSE                                                          
052200              MOVE SPACE TO   ODTA-IDKUNDNR                               
052300            END-IF                                                        
052400            IF LENG(3) > 0                                                
052500              MOVE CELL(3) TO ODTA-KDORDKL                                
052600            ELSE                                                          
052700              MOVE SPACE TO   ODTA-KDORDKL                                
052800            END-IF                                                        
052810            IF OHDR-IDSYSTEM = PREPLANNED-IDSYSTEM                        
052820*  FIX FÖR PREPLANNED                                                     
052830              MOVE '3'     TO ODTA-KDORDKL                                
052840            END-IF                                                        
052900                                                                          
053000            IF LENG(4) > 0                                                
053100              MOVE CELL(4) TO ODTA-KDFRAKT                                
053200            ELSE                                                          
053300              MOVE SPACE   TO ODTA-KDFRAKT                                
053400            END-IF                                                        
053500            IF LENG(5) > 0                                                
053600              MOVE CELL(5) TO ODTA-IDDC                                   
053700            ELSE                                                          
053800              MOVE SPACE   TO ODTA-IDDC                                   
053900            END-IF                                                        
054000            IF LENG(6) > 0                                                
054100              MOVE CELL(6) TO ODTA-BEKUNDRF                               
054200            ELSE                                                          
054300              MOVE SPACE   TO ODTA-BEKUNDRF                               
054400            END-IF                                                        
054500                                                                          
054600            IF LENG(11) > 0                                               
054700              MOVE CELL(11) TO ODTA-TITPO                                 
054710              MOVE SPACE    TO ODTA-TIREPDAT                              
054800            ELSE                                                          
054900              MOVE SPACE    TO ODTA-TITPO                                 
054910              MOVE SPACE    TO ODTA-TIREPDAT                              
055000            END-IF                                                        
055010            IF OHDR-IDSYSTEM = PREPLANNED-IDSYSTEM                        
055020*  FIX FÖR PREPLANNED                                                     
055021*  VI ANVÄNDER SAMMA KOLUMN I ARKET FÖR REPDAT OCH TPODAT                 
055022*  DÄRFÖR DETTA LILLA TRIXET MED DATUMFÄLTEN                              
055030              MOVE ODTA-TITPO  TO ODTA-TIREPDAT                           
055031              MOVE SPACE       TO ODTA-TITPO                              
055040            END-IF                                                        
055100            IF LENG(12) > 0                                               
055200              MOVE CELL(12) TO ODTA-BEGMT-RAD1                            
055300            ELSE                                                          
055400              MOVE SPACE    TO ODTA-BEGMT-RAD1                            
055500            END-IF                                                        
055600            IF LENG(13) > 0                                               
055700              MOVE CELL(13) TO ODTA-BEGMT-RAD2                            
055800            ELSE                                                          
055900              MOVE SPACE    TO ODTA-BEGMT-RAD2                            
056000            END-IF                                                        
056100            IF LENG(14) > 0                                               
056200              MOVE CELL(14) TO ODTA-ADGMT-GATA                            
056300            ELSE                                                          
056400              MOVE SPACE    TO ODTA-ADGMT-GATA                            
056500            END-IF                                                        
056600            IF LENG(15) > 0                                               
056700              MOVE CELL(15) TO ODTA-ADGMT-PADR                            
056800            ELSE                                                          
056900              MOVE SPACE    TO ODTA-ADGMT-PADR                            
057000            END-IF                                                        
057100            IF LENG(16) > 0                                               
057200              MOVE CELL(16) TO ODTA-ADGMT-LAND                            
057300            ELSE                                                          
057400              MOVE SPACE    TO ODTA-ADGMT-LAND                            
057500            END-IF                                                        
057600          END-IF                                                          
057700                                                                          
057800          MOVE CELL(7)      TO ODTA-IDARTNR                               
057900          MOVE CELL(8)      TO ODTA-KVBEART                               
058000          IF LENG(9) > 0                                                  
058000            INSPECT CELL(9)                                               
058000               REPLACING ALL LOW-VAL BY SPACE                             
058000          END-IF                                                          
058000          MOVE CELL(9)      TO ODTA-BERADREF                              
058100          MOVE CELL(10)     TO ODTA-BEVARREF                              
058200          MOVE WS-FLFORBI   TO ODTA-FLFORBI                               
058300                                                                          
058400          PERFORM S12-WRITE-W41282-ODTA                                   
058500        ELSE                                                              
058600          ADD 1 TO RECENT-BLANK-LINES                                     
058700          IF RECENT-BLANK-LINES > MAX-BLANKS-MULTI-DEALER                 
058800*           -- NO MORE PART NUMBERS IN INPUT FILE                         
058900*           -- SO WE CAN AS WELL STOP READING                             
059000            SET STOP-READING TO TRUE                                      
059100          END-IF                                                          
059200        END-IF                                                            
059300     END-IF                                                               
059400                                                                          
059500*    -- NO 'District' ETC. HEADER YET - SKIP THE LINE                     
059600     IF NOT-YET-HEADER-LINE                                               
059700       MOVE 1 TO CCOUNT                                                   
059800       INSPECT CELL(1) TALLYING CCOUNT                                    
059900          FOR LEADING SPACE                                               
060000                                                                          
060100       IF CCOUNT <= CELL-WIDTH - 7                                        
060200       AND CELL(1)(CCOUNT:8) = 'District'                                 
060300          SET HEADER-LINE-FOUND TO TRUE                                   
060400       END-IF                                                             
060500     END-IF                                                               
060600     .                                                                    
060700                                                                          
060800     EJECT                                                                
060900 BD-PROFORMA-CSV-ANALYZIS SECTION.                                        
061000                                                                          
061100*    -- A HEADER LINE WITH the TEXT 'District' IN COL 1 STARTS            
061200*    -- A SEQUENCE OF ORDER LINES WITH PART NUMBERS + QANTITIES           
061300*    -- AND OPTIONALLY ORDER HEAD DATA.                                   
061400     IF HEADER-LINE-FOUND                                                 
061500        IF LENG(8) > 0                                                    
061600          MOVE ZERO TO RECENT-BLANK-LINES                                 
061700                                                                          
061800*         -- A LINE WITH PART NUMBER                                      
061900*         -- THE LINE CONTAINS ORDER LINE DATA                            
062000          IF LENG(1) > 0 OR LENG(2) > 0 OR LENG(3) > 0                    
062100*           -- DISTRICT, CUSTOMER OR ORDER CLASS IS GIVEN                 
062200*           -- THIS INDICATES A NEW ORDER.                                
062300*           -- CLEAR PREVIOUS VALUES IF THE COLUMN IS BLANK.              
062400            IF LENG(1) > 0                                                
062500              MOVE CELL(1) TO ODTA-IDDISTR                                
062600            ELSE                                                          
062700              MOVE SPACE TO   ODTA-IDDISTR                                
062800            END-IF                                                        
062900            IF LENG(2) > 0                                                
063000              MOVE CELL(2) TO ODTA-IDKUNDNR                               
063100            ELSE                                                          
063200              MOVE SPACE TO   ODTA-IDKUNDNR                               
063300            END-IF                                                        
063400            IF LENG(3) > 0                                                
063500              MOVE CELL(3) TO ODTA-KDORDKL                                
063600            ELSE                                                          
063700              MOVE SPACE TO   ODTA-KDORDKL                                
063800            END-IF                                                        
063900                                                                          
064000            IF LENG(4) > 0                                                
064100              MOVE CELL(4) TO ODTA-KDFRAKT                                
064200            ELSE                                                          
064300              MOVE SPACE   TO ODTA-KDFRAKT                                
064400            END-IF                                                        
064500            IF LENG(5) > 0                                                
064600              MOVE CELL(5) TO ODTA-KDPROTYP                               
064700            ELSE                                                          
064800              MOVE SPACE   TO ODTA-KDPROTYP                               
064900            END-IF                                                        
065000            IF LENG(6) > 0                                                
065100              MOVE CELL(6) TO ODTA-KDFAKTYP                               
065200            ELSE                                                          
065300              MOVE SPACE   TO ODTA-KDFAKTYP                               
065400            END-IF                                                        
065500            IF LENG(7) > 0                                                
065600              MOVE CELL(7) TO ODTA-BEKUNDRF                               
065700            ELSE                                                          
065800              MOVE SPACE   TO ODTA-BEKUNDRF                               
065900            END-IF                                                        
066000                                                                          
066100            IF LENG(10) > 0                                               
066200              MOVE CELL(10) TO ODTA-IDKONTO                               
066300            ELSE                                                          
066400              MOVE SPACE    TO ODTA-IDKONTO                               
066500            END-IF                                                        
066600            IF LENG(11) > 0                                               
066700              MOVE CELL(11) TO ODTA-IDSKYLT                               
066800            ELSE                                                          
066900              MOVE SPACE    TO ODTA-IDSKYLT                               
067000            END-IF                                                        
067100            IF LENG(12) > 0                                               
067200              MOVE CELL(12) TO ODTA-FORFDAT                               
067300            ELSE                                                          
067400              MOVE SPACE    TO ODTA-FORFDAT                               
067500            END-IF                                                        
067600            IF LENG(14) > 0                                               
067700              MOVE CELL(14) TO ODTA-BEGMT-RAD1                            
067800            ELSE                                                          
067900              MOVE SPACE    TO ODTA-BEGMT-RAD1                            
068000            END-IF                                                        
068100            IF LENG(15) > 0                                               
068200              MOVE CELL(15) TO ODTA-BEGMT-RAD2                            
068300            ELSE                                                          
068400              MOVE SPACE    TO ODTA-BEGMT-RAD2                            
068500            END-IF                                                        
068600            IF LENG(16) > 0                                               
068700              MOVE CELL(16) TO ODTA-ADGMT-GATA                            
068800            ELSE                                                          
068900              MOVE SPACE    TO ODTA-ADGMT-GATA                            
069000            END-IF                                                        
069100            IF LENG(17) > 0                                               
069200              MOVE CELL(17) TO ODTA-ADGMT-PADR                            
069300            ELSE                                                          
069400              MOVE SPACE    TO ODTA-ADGMT-PADR                            
069500            END-IF                                                        
069600            IF LENG(18) > 0                                               
069700              MOVE CELL(18) TO ODTA-BEBETRAD-1                            
069800            ELSE                                                          
069900              MOVE SPACE    TO ODTA-BEBETRAD-1                            
070000            END-IF                                                        
070100            IF LENG(19) > 0                                               
070200              MOVE CELL(19) TO ODTA-BEBETRAD-2                            
070300            ELSE                                                          
070400              MOVE SPACE    TO ODTA-BEBETRAD-2                            
070500            END-IF                                                        
070600            IF LENG(20) > 0                                               
070700              MOVE CELL(20) TO ODTA-ADBETRAD-1                            
070800            ELSE                                                          
070900              MOVE SPACE    TO ODTA-ADBETRAD-1                            
071000            END-IF                                                        
071100            IF LENG(21) > 0                                               
071200              MOVE CELL(21) TO ODTA-ADBETRAD-2                            
071300            ELSE                                                          
071400              MOVE SPACE    TO ODTA-ADBETRAD-2                            
071500            END-IF                                                        
071600          END-IF                                                          
071700                                                                          
071800          MOVE CELL(8)      TO ODTA-IDARTNR                               
071900          MOVE CELL(9)      TO ODTA-KVBEART                               
072000          IF LENG(11) > 0                                                 
072000            INSPECT CELL(11)                                              
072000               REPLACING ALL LOW-VAL BY SPACE                             
072000          END-IF                                                          
072000          MOVE CELL(11)     TO ODTA-BERADREF                              
072100          MOVE WS-FLFORBI   TO ODTA-FLFORBI                               
072200                                                                          
072300          PERFORM S12-WRITE-W41282-ODTA                                   
072400        ELSE                                                              
072500          ADD 1 TO RECENT-BLANK-LINES                                     
072600          IF RECENT-BLANK-LINES > MAX-BLANKS-MULTI-DEALER                 
072700*           -- NO MORE PART NUMBERS IN INPUT FILE                         
072800*           -- SO WE CAN AS WELL STOP READING                             
072900            SET STOP-READING TO TRUE                                      
073000          END-IF                                                          
073100        END-IF                                                            
073200     END-IF                                                               
073300                                                                          
073400*    -- NO 'District' ETC. HEADER YET - SKIP THE LINE                     
073500     IF NOT-YET-HEADER-LINE                                               
073600       MOVE 1 TO CCOUNT                                                   
073700       INSPECT CELL(1) TALLYING CCOUNT                                    
073800          FOR LEADING SPACE                                               
073900                                                                          
074000       IF CCOUNT <= CELL-WIDTH - 7                                        
074100       AND CELL(1)(CCOUNT:8) = 'District'                                 
074200          SET HEADER-LINE-FOUND TO TRUE                                   
074300       END-IF                                                             
074400     END-IF                                                               
074500     .                                                                    
074600                                                                          
074700     EJECT                                                                
074800 BE-REFILL-CSV-ANALYZIS SECTION.                                          
074900                                                                          
075000*    -- A HEADER LINE WITH the TEXT 'District' IN COL 1 STARTS            
075100*    -- A SEQUENCE OF ORDER LINES WITH PART NUMBERS + QANTITIES           
075200*    -- AND OPTIONALLY ORDER HEAD DATA.                                   
075300     IF HEADER-LINE-FOUND                                                 
075400        IF LENG(7) > 0                                                    
075500          MOVE ZERO TO RECENT-BLANK-LINES                                 
075600                                                                          
075700*         -- A LINE WITH PART NUMBER                                      
075800*         -- THE LINE CONTAINS ORDER LINE DATA                            
075900          IF ((LENG(1) > 0 OR LENG(3) > 0) AND WS-FLFORBI = 'N')          
075910          OR ((LENG(1) > 0 OR LENG(3) > 0 OR LENG(5) > 0)                 
075920               AND WS-FLFORBI = 'J')                                      
076000*           -- DISTRICT, CUSTOMER OR ORDER CLASS IS GIVEN                 
076100*           -- THIS INDICATES A NEW ORDER.                                
076200*           -- CLEAR PREVIOUS VALUES IF THE COLUMN IS BLANK.              
076300            IF LENG(1) > 0                                                
076400              MOVE CELL(1) TO ODTA-IDDISTR                                
076500            ELSE                                                          
076600              MOVE SPACE TO   ODTA-IDDISTR                                
076700            END-IF                                                        
076800            IF LENG(2) > 0                                                
076900              MOVE CELL(2) TO ODTA-IDKUNDNR                               
077000            ELSE                                                          
077100              MOVE SPACE TO   ODTA-IDKUNDNR                               
077200            END-IF                                                        
077300            IF LENG(3) > 0                                                
077400              MOVE CELL(3) TO ODTA-KDORDKL                                
077500            ELSE                                                          
077600              MOVE SPACE TO   ODTA-KDORDKL                                
077700            END-IF                                                        
077800                                                                          
077900            IF LENG(4) > 0                                                
078000              MOVE CELL(4) TO ODTA-KDFRAKT                                
078100            ELSE                                                          
078200              MOVE SPACE   TO ODTA-KDFRAKT                                
078300            END-IF                                                        
078400            IF LENG(5) > 0                                                
078500              MOVE CELL(5) TO ODTA-IDDC                                   
078600            ELSE                                                          
078700              MOVE SPACE   TO ODTA-IDDC                                   
078800            END-IF                                                        
078900            IF LENG(6) > 0                                                
079000              MOVE CELL(6) TO ODTA-BEKUNDRF                               
079100            ELSE                                                          
079200              MOVE SPACE   TO ODTA-BEKUNDRF                               
079300            END-IF                                                        
079400                                                                          
079500            IF LENG(11) > 0                                               
079600              MOVE CELL(11) TO ODTA-TITPO                                 
079700            ELSE                                                          
079800              MOVE SPACE    TO ODTA-TITPO                                 
079900            END-IF                                                        
080000            IF LENG(12) > 0                                               
080100              MOVE CELL(12) TO ODTA-BEGMT-RAD1                            
080200            ELSE                                                          
080300              MOVE SPACE    TO ODTA-BEGMT-RAD1                            
080400            END-IF                                                        
080500            IF LENG(13) > 0                                               
080600              MOVE CELL(13) TO ODTA-BEGMT-RAD2                            
080700            ELSE                                                          
080800              MOVE SPACE    TO ODTA-BEGMT-RAD2                            
080900            END-IF                                                        
081000            IF LENG(14) > 0                                               
081100              MOVE CELL(14) TO ODTA-ADGMT-GATA                            
081200            ELSE                                                          
081300              MOVE SPACE    TO ODTA-ADGMT-GATA                            
081400            END-IF                                                        
081500            IF LENG(15) > 0                                               
081600              MOVE CELL(15) TO ODTA-ADGMT-PADR                            
081700            ELSE                                                          
081800              MOVE SPACE    TO ODTA-ADGMT-PADR                            
081900            END-IF                                                        
082000            IF LENG(16) > 0                                               
082100              MOVE CELL(16) TO ODTA-ADGMT-LAND                            
082200            ELSE                                                          
082300              MOVE SPACE    TO ODTA-ADGMT-LAND                            
082400            END-IF                                                        
082500          END-IF                                                          
082600                                                                          
082700          MOVE CELL(7)      TO ODTA-IDARTNR                               
082800          MOVE CELL(8)      TO ODTA-KVBEART                               
082900          IF LENG(9) > 0                                                  
082900            INSPECT CELL(9)                                               
082900               REPLACING ALL LOW-VAL BY SPACE                             
082900          END-IF                                                          
082900          MOVE CELL(9)      TO ODTA-BERADREF                              
083000          MOVE CELL(10)     TO ODTA-BEVARREF                              
083100          MOVE WS-FLFORBI   TO ODTA-FLFORBI                               
083200                                                                          
083300          PERFORM S12-WRITE-W41282-ODTA                                   
083400        ELSE                                                              
083500          ADD 1 TO RECENT-BLANK-LINES                                     
083600          IF RECENT-BLANK-LINES > MAX-BLANKS-REFILL                       
083700*           -- NO MORE PART NUMBERS IN INPUT FILE                         
083800*           -- SO WE CAN AS WELL STOP READING                             
083900            SET STOP-READING TO TRUE                                      
084000          END-IF                                                          
084100        END-IF                                                            
084200     END-IF                                                               
084300                                                                          
084400*    -- NO 'District' ETC. HEADER YET - SKIP THE LINE                     
084500     IF NOT-YET-HEADER-LINE                                               
084600       MOVE 1 TO CCOUNT                                                   
084700       INSPECT CELL(1) TALLYING CCOUNT                                    
084800          FOR LEADING SPACE                                               
084900                                                                          
084910       IF WS-FLFORBI = YES                                                
084911         IF CCOUNT <= CELL-WIDTH - 4                                      
084912         AND CELL(1)(CCOUNT:5) = 'To DC'                                  
084913            SET HEADER-LINE-FOUND TO TRUE                                 
084914         END-IF                                                           
084920       ELSE                                                               
085000         IF CCOUNT <= CELL-WIDTH - 7                                      
085100         AND CELL(1)(CCOUNT:8) = 'District'                               
085200            SET HEADER-LINE-FOUND TO TRUE                                 
085300         END-IF                                                           
085310       END-IF                                                             
085400     END-IF                                                               
085500     .                                                                    
085600                                                                          
085700     EJECT                                                                
085800 Z-FINIT SECTION.                                                         
085900                                                                          
086000     CLOSE W41281                                                         
086100           W41282                                                         
086200                                                                          
086300     MOVE 'S' TO POSTSUM-OPKOD                                            
086400     CALL POSTSUM USING POSTSUM-PARM                                      
086500     .                                                                    
086600                                                                          
086700     EJECT                                                                
086800 S01-READ-W41281  SECTION.                                                
086900                                                                          
087000     READ W41281 INTO CSV-AREA                                            
087100     AT END                                                               
087200        MOVE HIGH-VALUE TO CSV-AREA                                       
087300        SET END-OF-W41281 TO TRUE                                         
087400                                                                          
087500     NOT AT END                                                           
087600        PERFORM S01A-FETCH-CELL-DATA                                      
087700                                                                          
087800        MOVE 'W41281'   TO POSTSUM-FDNAMN                                 
087900        MOVE 'W41282D1' TO POSTSUM-DDNAMN2                                
088000        MOVE SPACE      TO POSTSUM-TRANSTYP                               
088100        CALL POSTSUM USING POSTSUM-PARM                                   
088200     END-READ                                                             
088300     .                                                                    
088400                                                                          
088500     EJECT                                                                
088600 S01A-FETCH-CELL-DATA SECTION.                                            
088700                                                                          
088800     ADD 1 TO LINE-NO                                                     
088900                                                                          
089000     MOVE 1 TO IX                                                         
089100     PERFORM UNTIL IX > CELL-LIMIT                                        
089200       MOVE SPACE TO CELL(IX)                                             
089300       MOVE ZERO  TO LENG(IX)                                             
089400       ADD 1 TO IX                                                        
089500     END-PERFORM                                                          
089600                                                                          
089700     UNSTRING CSV-AREA DELIMITED BY ';' OR CARR-RETURN                    
089800     INTO CELL(1)  COUNT IN LENG(1)                                       
089900          CELL(2)  COUNT IN LENG(2)                                       
090000          CELL(3)  COUNT IN LENG(3)                                       
090100          CELL(4)  COUNT IN LENG(4)                                       
090200          CELL(5)  COUNT IN LENG(5)                                       
090300          CELL(6)  COUNT IN LENG(6)                                       
090400          CELL(7)  COUNT IN LENG(7)                                       
090500          CELL(8)  COUNT IN LENG(8)                                       
090600          CELL(9)  COUNT IN LENG(9)                                       
090700          CELL(10) COUNT IN LENG(10)                                      
090800          CELL(11) COUNT IN LENG(11)                                      
090900          CELL(12) COUNT IN LENG(12)                                      
091000          CELL(13) COUNT IN LENG(13)                                      
091100          CELL(14) COUNT IN LENG(14)                                      
091200          CELL(15) COUNT IN LENG(15)                                      
091300          CELL(16) COUNT IN LENG(16)                                      
091400          CELL(17) COUNT IN LENG(17)                                      
091500          CELL(18) COUNT IN LENG(18)                                      
091600          CELL(19) COUNT IN LENG(19)                                      
091700          CELL(20) COUNT IN LENG(20)                                      
091800          CELL(21) COUNT IN LENG(21)                                      
091900          CELL(22) COUNT IN LENG(22)                                      
092000          CELL(23) COUNT IN LENG(23)                                      
092100          CELL(24) COUNT IN LENG(24)                                      
092200          CELL(25) COUNT IN LENG(25)                                      
092300          CELL(26) COUNT IN LENG(26)                                      
092400          CELL(27) COUNT IN LENG(27)                                      
092500          CELL(28) COUNT IN LENG(28)                                      
092600          CELL(29) COUNT IN LENG(29)                                      
092700          CELL(30) COUNT IN LENG(30)                                      
092800          CELL(31) COUNT IN LENG(31)                                      
092900          CELL(32) COUNT IN LENG(32)                                      
093000          CELL(33) COUNT IN LENG(33)                                      
093100          CELL(34) COUNT IN LENG(34)                                      
093200          CELL(35) COUNT IN LENG(35)                                      
093300          CELL(36) COUNT IN LENG(36)                                      
093400          CELL(37) COUNT IN LENG(37)                                      
093500          CELL(38) COUNT IN LENG(38)                                      
093600          CELL(39) COUNT IN LENG(39)                                      
093700          CELL(40) COUNT IN LENG(40)                                      
093800          CELL(41) COUNT IN LENG(41)                                      
093900          CELL(42) COUNT IN LENG(42)                                      
094000          CELL(43) COUNT IN LENG(43)                                      
094100          CELL(44) COUNT IN LENG(44)                                      
094200          CELL(45) COUNT IN LENG(45)                                      
094300          CELL(46) COUNT IN LENG(46)                                      
094400          CELL(47) COUNT IN LENG(47)                                      
094500          CELL(48) COUNT IN LENG(48)                                      
094600          CELL(49) COUNT IN LENG(49)                                      
094700          CELL(50) COUNT IN LENG(50)                                      
094800     ON OVERFLOW                                                          
094900       CONTINUE                                                           
095000     .                                                                    
095100                                                                          
095200     EJECT                                                                
095300 S11-WRITE-W41282-OHDR SECTION.                                           
095400                                                                          
095500     WRITE OHDR-RECORD  FROM OHDR-W4128201                                
095600                                                                          
095700     MOVE 'OHDR'     TO POSTSUM-TRANSTYP                                  
095800     MOVE 'W41282'   TO POSTSUM-FDNAMN                                    
095900     MOVE 'W41282D2' TO POSTSUM-DDNAMN2                                   
096000     CALL POSTSUM USING POSTSUM-PARM                                      
096100     .                                                                    
096200                                                                          
096300     EJECT                                                                
096400 S12-WRITE-W41282-ODTA SECTION.                                           
096500                                                                          
096600     WRITE ODTA-RECORD  FROM ODTA-W4128202                                
096700                                                                          
096800     MOVE 'ODTA'     TO POSTSUM-TRANSTYP                                  
096900     MOVE 'W41282'   TO POSTSUM-FDNAMN                                    
097000     MOVE 'W41282D2' TO POSTSUM-DDNAMN2                                   
097100     CALL POSTSUM USING POSTSUM-PARM                                      
097200     .                                                                    
097300                                                                          
