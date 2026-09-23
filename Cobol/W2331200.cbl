000100 ID DIVISION.                                                             
000200 SKIP2                                                                    
000300 PROGRAM-ID.         W2331200.                                            
000400 AUTHOR.             URBAN ZACKRISSON.                                    
000500 DATE-WRITTEN.       NOV 1984.                                            
000600     REMARKS.                                                             
000700                                                                          
000800            FUNKTION:                                                     
000900                     FILEN W23311 SORTERAS PER ARTIKEL OCH                
001000                     LEVERANTÖRNR                                         
001100                     DE SORTERADE POSTERNA BEHANDLAS ENLIGT               
001200                     FÖLJANDE:                                            
001300                                                                          
001400                     PT 310   KVAVIS ADDERAS TILL ANT-INLEV               
001500                     PT 320   KVMOTANT-KVAVIS ADD TILL ANT-INLEV          
001600                     PT 330   KVMOTANT ADDERAS TILL ANT-INLEV             
001700                     PT 340   KVMOTANT ADDERAS TILL ANT-INLEV             
001800                                                                          
001900                     DEN SORTERADE FILEN MATCHAS MOT W23319 OCH           
002000                     BEROENDE PÅ KDPRODSL FÖR ARTIKELN PÅ FILEN           
002100                     W23319 , SÄTTS KDSEKTOR TILL:                        
002200                                                                          
002300                     '6'   NÄR KDPRODSL = +15                             
002400                     '5'   NÄR KDPRODSL = +16 ELLER +17                   
002500                     '3'   ÖVRIGT                                         
002600                                  PER ARTIKEL OCH LEVERANTÖR              
002700                     SKRIVES MED DE SUMMERADE VÄRDENA(PT 073)             
002800                     PÅ W23313.                                           
002900            ABENDKODER:                                                   
003000                     U0016     -OM RETURKOD FRÅN SORT.                    
003100                     U1000     -OM ARTIKEL SAKNAR MOTSVARIGHET            
003200                                PÅ W23319                                 
003300     EJECT                                                                
003400 ENVIRONMENT DIVISION.                                                    
003500     SKIP2                                                                
003600 INPUT-OUTPUT SECTION.                                                    
003700                                                                          
003800 FILE-CONTROL.                                                            
003900     SKIP2                                                                
004000     SELECT  W23311  ASSIGN  TO UT-S-W23312D1.                            
004100     SELECT  W23319  ASSIGN  TO UT-S-W23312D2.                            
004200     SELECT  W23313  ASSIGN  TO UT-S-W23312D3.                            
004300     SELECT  SORTFIL ASSIGN  TO UT-S-W23312DS.                            
004400     EJECT                                                                
004500 DATA DIVISION.                                                           
004600     SKIP2                                                                
004700 FILE SECTION.                                                            
004800     SKIP3                                                                
004900 FD  W23311                                                               
005000     LABEL RECORD STANDARD                                                
005100     RECORDING F                                                          
005200     BLOCK CONTAINS 0.                                                    
005300                                                                          
005400*01      -COPY  W211310 -L                                                
005500     SKIP3                                                                
005600 FD  W23319                                                               
005700     LABEL RECORD STANDARD                                                
005800     RECORDING F                                                          
005900     BLOCK CONTAINS 0.                                                    
006000*01  W23319-POST   -COPY W23319     -L                                    
006100     SKIP3                                                                
006200 FD  W23313                                                               
006300     LABEL RECORD STANDARD                                                
006400     RECORDING F                                                          
006500     BLOCK CONTAINS 0.                                                    
006600*01  W23313-POST   -COPY A310G073 -L                                      
006700     SKIP3                                                                
006800 SD  SORTFIL                                                              
006900     RECORDING F.                                                         
007000*01  POST    -COPY   W211310 -PRE SORT-                                   
007100     EJECT                                                                
007200 WORKING-STORAGE SECTION.                                                 
007300                                                                          
007400*    -- CHECKED BY WY2000                                                 
007500 77  IDPGM                   PIC X(8)     VALUE 'W2331200'.               
007600 77  JA                      PIC X               VALUE 'J'.               
007700 77  NEJ                     PIC X               VALUE 'N'.               
007800 77  W23319-EOF              PIC X               VALUE 'N'.               
007900 77  SORT-EOF                PIC X               VALUE 'N'.               
008000 77  IDARTNR-WS              PIC S9(9).                                   
008100 77  IDLEVNR-WS              PIC X(5).                                    
008200 77  KDPKINR-WS              PIC 9(3).                                    
008300 77  KDSEKTOR-WS             PIC X.                                       
008400 01  TEST-IDLEVNR            PIC X(5).                                    
008500 01  TEST-KDPRODSL-SW        PIC 9(3)    VALUE ZERO.                      
008600     88  KDPRODSL-LYNC                   VALUE 31 THRU 39.                
008700     SKIP3                                                                
008800 01  DYNAMISKA-SUBPROGRAM.                                                
008900     03  ABEND               PIC X(8)    VALUE 'ABEND'.                   
009000     03  DATKORT             PIC X(8)    VALUE 'DATKORT'.                 
009100     03  POSTSUM             PIC X(8)    VALUE 'POSTSUM'.                 
009200     03  WDATKONV            PIC X(8)    VALUE 'WDATKONV'.                
009300     SKIP3                                                                
009400 01  RETURKODER.                                                          
009500     03  RKOD                  PIC S9(4) COMP SYNC VALUE ZERO.            
009600     03  RKOD-ABEND-UTAN-DUMP  PIC S9(4) COMP SYNC VALUE +16.             
009700     03  RKOD-ABEND-MED-DUMP   PIC S9(4) COMP SYNC VALUE +1000.           
009800     SKIP3                                                                
009900 01  DATUM-FALT.                                                          
010000     03  WS-AAMMDD           PIC 9(6).                                    
010100     03  FILLER REDEFINES WS-AAMMDD.                                      
010200         05  WS-AA           PIC 9(2).                                    
010300         05  WS-MM           PIC 9(2).                                    
010400         05  WS-DD           PIC 9(2).                                    
010500     EJECT                                                                
010600*01  -COPY WWKONLEV.                                                      
010700     EJECT                                                                
010800*01  -COPY W0005  -PRE POSTSUM-.                                          
010900     EJECT                                                                
011000*    --- PARAMETRAR TILL DATKORT                                          
011100*                                                                         
011200 01  DATUMKORT-ID            PIC X(6) VALUE 'WDATUM'.                     
011300     SKIP2                                                                
011400 01  PROGRAM-NAMN                PIC X(6)    VALUE 'W23312'.              
011500     SKIP2                                                                
011600*01  -COPY WDATKORT                                                       
011700     EJECT                                                                
011800*01  -COPY WDATAREA.                                                      
011900     EJECT                                                                
012000 01  SORTWS-AREA.                                                         
012100*    03  -COPY   W211310  -PRE SORTWS-.                                   
012200     EJECT                                                                
012300*01  AREA   -COPY   W23319      -PRE IN-.                                 
012400     EJECT                                                                
012500 01  A310-AREA.                                                           
012600*    03  -COPY   A310G073 -PRE A310-.                                     
012700     EJECT                                                                
012800 PROCEDURE DIVISION.                                                      
012900                                                                          
013000     PERFORM A-INIT                                                       
013100                                                                          
013200     SORT SORTFIL ASCENDING                                               
013300                           SORT-IDARTNR-S                                 
013400                           SORT-IDLEVNR-INL                               
013500     USING W23311                                                         
013600     OUTPUT PROCEDURE C-SKRIV-W23313                                      
013700                                                                          
013800     IF SORT-RETURN NOT = ZERO                                            
013900       DISPLAY '*** W2331200  -FEL VID SORTERING'                         
014000       CALL ABEND USING RKOD-ABEND-UTAN-DUMP                              
014100     ELSE                                                                 
014200       PERFORM  Z-FINIT                                                   
014300       MOVE +0 TO RETURN-CODE                                             
014400       GOBACK                                                             
014500     END-IF                                                               
014600     .                                                                    
014700     EJECT                                                                
014800 A-INIT SECTION.                                                          
014900     OPEN OUTPUT W23313                                                   
015000     OPEN INPUT  W23319                                                   
015100     CALL DATKORT USING PROGRAM-NAMN DATUMKORT-ID DATUMKORT               
015200     MOVE D-AAR    TO WS-AA                                               
015300     MOVE D-MAANAD TO WS-MM                                               
015400     MOVE D-DAG    TO WS-DD                                               
015500     MOVE +73      TO A310-PT                                             
015600     MOVE 'BP2TW'  TO A310-GSDB-FORB                                      
015700     MOVE ZERO     TO A310-FORBNR                                         
015800     MOVE +0       TO A310-INDEX-LEV                                      
015900                      A310-INDEX-KVAL                                     
016000                      IN-IDARTNR                                          
016100                      IN-KDPRODSL                                         
016200     SKIP2                                                                
016300     MOVE 'AAMMDD'  TO DAT-KDDATFORM                                      
016400     MOVE WS-AAMMDD TO DAT-I-TIDATUM                                      
016500     CALL WDATKONV USING                                                  
016600     DAT-KDDATFORM                                                        
016700     DAT-I-TIDATUM                                                        
016800     DAT-O-TIDATUM                                                        
016900     DAT-KDSVAR                                                           
017000     SKIP1                                                                
017100     IF DAT-KDSVAR-OK                                                     
017200       MOVE DAT-TIAAPP TO A310-PER                                        
017300     ELSE                                                                 
017400       DISPLAY 'W23312 - FEL FRÅN WDATKONV'                               
017500       CALL ABEND USING RKOD-ABEND-UTAN-DUMP                              
017600     END-IF                                                               
017700     .                                                                    
017800     EJECT                                                                
017900 C-SKRIV-W23313 SECTION.                                                  
018000     PERFORM S01-SORT-RETURN                                              
018100     MOVE SORT-IDARTNR-S   TO IDARTNR-WS                                  
018200     MOVE SORT-IDLEVNR-INL TO IDLEVNR-WS                                  
018300     MOVE SORT-KDPKINR     TO KDPKINR-WS                                  
018400     MOVE +0               TO A310-ANT-INLEV                              
018500     PERFORM S03-LAES-W23319-TILLS-LIKA                                   
018600     PERFORM S04-FLYTTA-KDSEKTOR                                          
018700     PERFORM UNTIL SORT-EOF = JA                                          
018800       PERFORM UNTIL (SORT-IDLEVNR-INL NOT = IDLEVNR-WS) OR               
018900                     (SORT-IDARTNR-S NOT = IDARTNR-WS) OR                 
019000                     (SORT-EOF = JA)                                      
019100*        IF NOT KDPRODSL-LYNC                                             
019200            IF SORT-IDPTYP = +310                                         
019300              ADD SORT-KVAVIS TO A310-ANT-INLEV                           
019400            ELSE                                                          
019500              ADD SORT-KVMOTANT TO A310-ANT-INLEV                         
019600              IF SORT-IDPTYP = +320                                       
019700                SUBTRACT SORT-KVAVIS FROM A310-ANT-INLEV                  
019800              END-IF                                                      
019900            END-IF                                                        
020000*        END-IF                                                           
020100         PERFORM S01-SORT-RETURN                                          
020200       END-PERFORM                                                        
020300                                                                          
020400*      IF NOT KDPRODSL-LYNC                                               
020500          PERFORM S02-SKRIV                                               
020600*      END-IF                                                             
020700                                                                          
020800       IF SORT-EOF = NEJ                                                  
020900         MOVE SORT-IDARTNR-S   TO IDARTNR-WS                              
021000         MOVE SORT-IDLEVNR-INL TO IDLEVNR-WS                              
021100         MOVE SORT-KDPKINR     TO KDPKINR-WS                              
021200         MOVE +0               TO A310-ANT-INLEV                          
021300                                                                          
021400         IF IDARTNR-WS NOT = IN-IDARTNR                                   
021500           MOVE ZERO           TO TEST-KDPRODSL-SW                        
021600           PERFORM S03-LAES-W23319-TILLS-LIKA                             
021700           PERFORM S04-FLYTTA-KDSEKTOR                                    
021800         END-IF                                                           
021900       END-IF                                                             
022000     END-PERFORM                                                          
022100     .                                                                    
022200     EJECT                                                                
022300 S01-SORT-RETURN SECTION.                                                 
022400     RETURN SORTFIL INTO SORTWS-AREA                                      
022500     AT END MOVE JA TO SORT-EOF.                                          
022600                                                                          
022700     IF SORT-EOF = NEJ                                                    
022800       MOVE 'W23311'      TO POSTSUM-FDNAMN                               
022900       MOVE 'W23312D1'    TO POSTSUM-DDNAMN2                              
023000       MOVE SORTWS-IDPTYP TO POSTSUM-TRANSTYP                             
023100       CALL POSTSUM USING POSTSUM-PARM                                    
023200     END-IF                                                               
023300     .                                                                    
023400     SKIP3                                                                
023500 S02-SKRIV SECTION.                                                       
023600     MOVE IDARTNR-WS   TO A310-ARTNR                                      
023700*    MOVE ZERO         TO TALLY                                           
023800*    INSPECT IDLEVNR-WS  TALLYING TALLY                                   
023900*            FOR CHARACTERS BEFORE INITIAL SPACE                          
024000*    IF TALLY = ZERO                                                      
024100*       MOVE ZERO      TO A310-LEVNUM                                     
024200*    ELSE                                                                 
024300*       MOVE IDLEVNR-WS (1:TALLY)  TO A310-LEVNUM                         
024400*    END-IF                                                               
024500     MOVE IDLEVNR-WS   TO A310-GSDB-LEV                                   
024600                          WS-IDLEVNR-KONCERN                              
024700                          TEST-IDLEVNR                                    
024800     INSPECT TEST-IDLEVNR REPLACING ALL SPACE BY ZERO                     
024900     MOVE ZERO         TO A310-LEVNUM                                     
025000     MOVE KDSEKTOR-WS  TO A310-KDSEKTOR                                   
025100     IF (IDLEVNR-WS = '14489') OR                                         
025200        (IDLEVNR-WS = 'DL7YA') OR                                         
025300        (IDLEVNR-WS = 'BQ8VA') OR                                         
025400        (KONCERN-LEV         ) OR                                         
025500        (TEST-IDLEVNR NUMERIC)                                            
025600***     DISPLAY 'LV-ART: ' IDARTNR-WS '   EJ UT PÅ W23313'                
025700        CONTINUE                                                          
025800     ELSE                                                                 
025900       WRITE W23313-POST FROM A310-AREA                                   
026000                                                                          
026100       MOVE 'W23313'      TO POSTSUM-FDNAMN                               
026200       MOVE 'W23312D3'    TO POSTSUM-DDNAMN2                              
026300       MOVE SORTWS-IDPTYP TO POSTSUM-TRANSTYP                             
026400       CALL POSTSUM USING POSTSUM-PARM                                    
026500     END-IF                                                               
026600     .                                                                    
026700     SKIP3                                                                
026800 S03-LAES-W23319-TILLS-LIKA SECTION.                                      
026900     SKIP2                                                                
027000      PERFORM UNTIL (IN-IDARTNR NOT < IDARTNR-WS) OR                      
027100                      (W23319-EOF = JA)                                   
027200        PERFORM S03A-LAES-W23319                                          
027300      END-PERFORM                                                         
027400      IF IN-IDARTNR = IDARTNR-WS                                          
027500        CONTINUE                                                          
027600      ELSE                                                                
027700        IF (IN-IDARTNR > IDARTNR-WS) AND (W23319-EOF = NEJ)               
027800          CALL ABEND USING RKOD-ABEND-MED-DUMP                            
027900        END-IF                                                            
028000      END-IF                                                              
028100     .                                                                    
028200     SKIP3                                                                
028300 S03A-LAES-W23319 SECTION.                                                
028400     SKIP2                                                                
028500     READ W23319 INTO IN-AREA                                             
028600       AT END                                                             
028700          MOVE JA TO W23319-EOF                                           
028800       NOT AT END                                                         
028900          MOVE IN-KDPRODSL  TO TEST-KDPRODSL-SW                           
029000     END-READ                                                             
029100     .                                                                    
029200     SKIP3                                                                
029300 S04-FLYTTA-KDSEKTOR SECTION.                                             
029400     SKIP2                                                                
029410                                                                          
029500     EVALUATE IN-KDPRODSL WHEN +17                                        
029600                                  MOVE '5' TO KDSEKTOR-WS                 
029700                          WHEN +16                                        
029800                                  MOVE '5' TO KDSEKTOR-WS                 
029900                          WHEN +15                                        
030000                                  MOVE '6' TO KDSEKTOR-WS                 
030100                          WHEN OTHER                                      
030200                                  MOVE '3' TO KDSEKTOR-WS                 
030300     END-EVALUATE                                                         
030400     .                                                                    
030500     SKIP3                                                                
030600 Z-FINIT SECTION.                                                         
030700     CLOSE W23313                                                         
030800           W23319                                                         
030900     MOVE 'S' TO POSTSUM-OPKOD                                            
031000     CALL POSTSUM USING POSTSUM-PARM                                      
031100     .                                                                    
031200     EJECT                                                                
