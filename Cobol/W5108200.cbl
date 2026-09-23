000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W5108200.                                                
000300 AUTHOR.         RANDI BERG.                                              
000400 DATE-WRITTEN.   98/11/10.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*                                                                         
000800*    FUNKTION:                                                            
000900*        SKICKAR LEVA1-TRANSAR TILL ON-DEMAND                             
001000*                                                                         
001100*                                                                         
001200*    ABENDKODER:                                                          
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
002400*          --- LEVA1-TRANSAR                                              
002500     SELECT W51080                     ASSIGN TO W51082D1.                
002600     SKIP2                                                                
002700*                                                                         
002800     SELECT LISTA                      ASSIGN TO W51082D2.                
002900     SKIP2                                                                
003000*          --- SORTERINGSFIL                                              
003100     SELECT SORTFIL                    ASSIGN TO W51082DS.                
003200     EJECT                                                                
003300 DATA DIVISION.                                                           
003400     SKIP3                                                                
003500 FILE SECTION.                                                            
003600     SKIP3                                                                
003700 FD  W51080                                                               
003800     RECORDING       F                                                    
003900     BLOCK CONTAINS  0.                                                   
004000                                                                          
004100*01  -COPY A432GSDB     -L.                                               
004200     SKIP3                                                                
004300 FD  LISTA                                                                
004400     RECORDING       F                                                    
004500     BLOCK CONTAINS  0.                                                   
004600     SKIP2                                                                
004700 01  LISTPOST                    PIC X(121).                              
004800     SKIP2                                                                
004900 SD  SORTFIL.                                                             
005000                                                                          
005100*01  POST -COPY A432GSDB     -PRE SORT-                                   
005200     EJECT                                                                
005300 WORKING-STORAGE SECTION.                                                 
005400                                                                          
005500                                                                          
005600*    -- CHECKED BY WY2000                                                 
005700 77  IDPGM                       PIC X(8)    VALUE 'W5108200'.            
005800 77  JA                          PIC X       VALUE 'J'.                   
005900 77  NEJ                         PIC X       VALUE 'N'.                   
006000                                                                          
006100 77  W51080-EOF-SW               PIC X       VALUE 'N'.                   
006200     88  END-OF-W51080                       VALUE 'J'.                   
006300                                                                          
006400 77  SORTFIL-EOF-SW              PIC X       VALUE 'N'.                   
006500     88  END-OF-SORTFIL                      VALUE 'J'.                   
006600     EJECT                                                                
006700                                                                          
006800 77  SIDA-SW             PIC X               VALUE 'J'.                   
006900     88  NY-SIDA                             VALUE 'J'.                   
007000 77  W-IDARTNR           PIC S9(10)          VALUE ZERO.                  
007100 77  W-DATUM             PIC X(8).                                        
007200                                                                          
007300 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
007400 01  FILLER REDEFINES DAGENS-DATUM.                                       
007500     03  DAGENS-DATUM-AAR        PIC 9(2).                                
007600     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
007700     03  DAGENS-DATUM-DAG        PIC 9(2).                                
007800     EJECT                                                                
007900 01  DYNAMISKA-SUBPROGRAM.                                                
008000*                                                                         
008100     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
008200     03  DATKORT                 PIC X(8)    VALUE 'DATKORT'.             
008300     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
008400     SKIP2                                                                
008500*    --- PARAMETRAR TILL ABEND                                            
008600                                                                          
008700 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
008800 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
008900 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
009000     SKIP2                                                                
009100 01  FELTEXT.                                                             
009200     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
009300     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
009400     EJECT                                                                
009500*    --- PARAMETRAR TILL DATKORT                                          
009600*                                                                         
009700 01  PROGRAM-NAMN                PIC X(6)    VALUE 'W51082'.              
009800     SKIP2                                                                
009900 01  DATUMKORT-ID                PIC X(6)    VALUE 'WDATUM'.              
010000     SKIP2                                                                
010100*01  -COPY WDATKORT                                                       
010200     EJECT                                                                
010300*    --- PARAMETRAR TILL POSTSUM                                          
010400*                                                                         
010500*01  -COPY W0005   -PRE  POSTSUM-                                         
010600     EJECT                                                                
010700 01  IN-AREA-START               PIC X(24)   VALUE                        
010800                                 'IN-AREA-START  '.                       
010900     SKIP2                                                                
011000*01  AREA -COPY A432GSDB    -PRE IN-                                      
011100     EJECT                                                                
011200 01  TITEL1.                                                              
011300     03  FILLER              PIC X(27)   VALUE                            
011400         ' W51082-001   VCCS        '.                                    
011500     03  FILLER              PIC X(24)   VALUE                            
011600         'SAP MM TRANSACTION WEEK:'.                                      
011700     03  W-AAVV              PIC X(4).                                    
011800     03  FILLER REDEFINES W-AAVV.                                         
011900         05  W-AAR           PIC X(2).                                    
012000         05  W-VECKA         PIC X(2).                                    
012100     03  FILLER              PIC X(7)   VALUE                             
012200         ' PART: '.                                                       
012300     03  W-ARTNR-RUBR        PIC Z(10).                                   
012400                                                                          
012500 01  RUBRIK1.                                                             
012600     03  FILLER              PIC X(34) VALUE                              
012700         ' SEQNO     VENDOR DELDATE PACKNO  '.                            
012800     03  FILLER              PIC X(45) VALUE                              
012900         '   QTY  ORDER PRICE      STD SUM    ORDER SUM'.                 
013000     03  FILLER              PIC X(12) VALUE                              
013100         ' INFREIGHT  '.                                                  
013200     EJECT                                                                
013300                                                                          
013400 01  BLANK-RAD              PIC X    VALUE SPACE.                         
013500 01  DETALJRAD.                                                           
013600     03  FILLER                 PIC X.                                    
013700     03  RAD-MRNR               PIC Z(8).                                 
013800     03  FILLER                 PIC X(3)  VALUE SPACE.                    
013900     03  RAD-LEVNR              PIC X(5).                                 
014000     03  FILLER                 PIC X(1)  VALUE SPACE.                    
014100     03  RAD-DATUM-AVS          PIC X(6).                                 
014200     03  FILLER                 PIC X(1)  VALUE SPACE.                    
014300     03  RAD-PACKNR             PIC Z(6).                                 
014400     03  FILLER                 PIC X(1)  VALUE SPACE.                    
014500     03  RAD-ANTAL              PIC Z(6)9.                                
014600     03  RAD-ANTAL-TKN          PIC X(1).                                 
014700     03  FILLER                 PIC X(1)  VALUE SPACE.                    
014800     03  RAD-PRIS-BEST          PIC Z(8)9.99.                             
014900     03  FILLER                 PIC X(1)  VALUE SPACE.                    
015000     03  RAD-BEL-STD            PIC Z(8)9.99.                             
015100     03  FILLER                 PIC X(1)  VALUE SPACE.                    
015200     03  RAD-BEL-BEST           PIC Z(8)9.99.                             
015300     03  FILLER                 PIC X(1)  VALUE SPACE.                    
015400     03  RAD-TULLFAKT           PIC 9.9999.                               
015500     EJECT                                                                
015600 01  SORTWS-AREA-START           PIC X(24)   VALUE                        
015700                                  'SORTWS-AREA-START  '.                  
015800     SKIP2                                                                
015900                                                                          
016000*01  AREA -COPY A432GSDB     -PRE SORTWS-                                 
016100 01  SORT-RETURN-X               PIC X(2)  VALUE SPACE.                   
016200     EJECT                                                                
016300 PROCEDURE DIVISION.                                                      
016400 MAIN SECTION.                                                            
016500     SKIP2                                                                
016600                                                                          
016700     PERFORM A-INIT                                                       
016800                                                                          
016900     SORT SORTFIL ASCENDING KEY SORT-ARTNR                                
017000                  INPUT PROCEDURE B-SORT-INPUT                            
017100                  OUTPUT PROCEDURE C-SORT-OUTPUT                          
017200                                                                          
017300     IF SORT-RETURN NOT = 0                                               
017400       MOVE SORT-RETURN TO SORT-RETURN-X                                  
017500       STRING 'RETURKOD ' SORT-RETURN-X ' FRÅN SORT'                      
017600       DELIMITED BY SIZE INTO FELTEXT-STR                                 
017700       DISPLAY FELTEXT                                                    
017800       MOVE RKOD-ABEND-UTAN-DUMP TO RKOD-ABEND                            
017900       PERFORM S99-ABEND                                                  
018000     ELSE                                                                 
018100       PERFORM Z-FINIT                                                    
018200                                                                          
018300       MOVE ZERO TO RETURN-CODE                                           
018400       GOBACK                                                             
018500     END-IF                                                               
018600                                                                          
018700     .                                                                    
018800     EJECT                                                                
018900 A-INIT SECTION.                                                          
019000                                                                          
019100     OPEN INPUT  W51080                                                   
019200          OUTPUT LISTA                                                    
019300     SKIP2                                                                
019400     CALL DATKORT USING PROGRAM-NAMN DATUMKORT-ID DATUMKORT               
019500     MOVE D-AAR     TO  W-AAR                                             
019600     MOVE D-VECKA   TO  W-VECKA                                           
019700     .                                                                    
019800     EJECT                                                                
019900 B-SORT-INPUT  SECTION.                                                   
020000                                                                          
020100     PERFORM S01-LAES-W51080                                              
020200     PERFORM UNTIL END-OF-W51080                                          
020300       MOVE IN-AREA TO SORTWS-AREA                                        
020400       PERFORM S31-SORT-RELEASE                                           
020500       PERFORM S01-LAES-W51080                                            
020600     END-PERFORM                                                          
020700     .                                                                    
020800     EJECT                                                                
020900 C-SORT-OUTPUT SECTION.                                                   
021000     SKIP2                                                                
021100                                                                          
021200     PERFORM S32-SORT-RETURN                                              
021300     MOVE SPACE TO DETALJRAD                                              
021400     PERFORM UNTIL END-OF-SORTFIL                                         
021500       PERFORM CA-FLYTTA-SKRIV-LISTA                                      
021600       PERFORM S32-SORT-RETURN                                            
021700     END-PERFORM                                                          
021800     .                                                                    
021900     EJECT                                                                
022000 CA-FLYTTA-SKRIV-LISTA SECTION.                                           
022100     IF IN-FTAG = 57                                                      
022200       MOVE IN-ARTNR     TO W-ARTNR-RUBR                                  
022300       MOVE IN-MRNR      TO RAD-MRNR                                      
022400       MOVE IN-GSDB      TO RAD-LEVNR                                     
022500       MOVE IN-DATUM-AVS TO W-DATUM                                       
022600       MOVE W-DATUM (3:6) TO RAD-DATUM-AVS                                
022700       MOVE IN-PACKNR    TO RAD-PACKNR                                    
022800       MOVE IN-ANTAL     TO RAD-ANTAL                                     
022900       IF IN-ANTAL < ZERO                                                 
023000          MOVE '-'       TO RAD-ANTAL-TKN                                 
023100       END-IF                                                             
023200       COMPUTE RAD-PRIS-BEST ROUNDED = IN-BEL-STD / IN-ANTAL              
023300       MOVE IN-BEL-STD  TO RAD-BEL-STD                                    
023400       MOVE IN-BEL-BEST TO RAD-BEL-BEST                                   
023500       MOVE IN-TULLFAKT TO RAD-TULLFAKT                                   
023600                                                                          
023700       IF IN-ARTNR = W-IDARTNR                                            
023800          MOVE NEJ TO SIDA-SW                                             
023900       ELSE                                                               
024000          MOVE JA TO SIDA-SW                                              
024100       END-IF                                                             
024200                                                                          
024300       PERFORM S21-SKRIV-LISTA                                            
024310     ELSE                                                                 
024320       CONTINUE                                                           
024330     END-IF                                                               
024400     MOVE IN-ARTNR TO W-IDARTNR                                           
024500     MOVE SPACE TO DETALJRAD                                              
024600     .                                                                    
024700     EJECT                                                                
024800 Z-FINIT SECTION.                                                         
024900     CLOSE W51080                                                         
025000           LISTA                                                          
025100     SKIP2                                                                
025200     MOVE 'S' TO POSTSUM-OPKOD                                            
025300     CALL POSTSUM USING POSTSUM-PARM                                      
025400     .                                                                    
025500     EJECT                                                                
025600 S01-LAES-W51080  SECTION.                                                
025700     READ W51080 INTO IN-AREA                                             
025800     AT END                                                               
025900        MOVE HIGH-VALUE TO IN-AREA                                        
026000        SET END-OF-W51080 TO TRUE                                         
026100                                                                          
026200     NOT AT END                                                           
026300        MOVE 'W51080' TO POSTSUM-FDNAMN                                   
026400        MOVE 'W51082D1' TO POSTSUM-DDNAMN2                                
026500        MOVE SPACE     TO POSTSUM-TRANSTYP                                
026600        CALL POSTSUM USING POSTSUM-PARM                                   
026700     END-READ                                                             
026800     .                                                                    
026900     EJECT                                                                
027000 S21-SKRIV-LISTA  SECTION.                                                
027100                                                                          
027200     IF NY-SIDA                                                           
027300       WRITE LISTPOST FROM TITEL1 AFTER PAGE                              
027400       WRITE LISTPOST FROM RUBRIK1 AFTER 2                                
027500     END-IF                                                               
027600                                                                          
027700                                                                          
027800     WRITE LISTPOST FROM DETALJRAD AFTER 1                                
027900     .                                                                    
028000     EJECT                                                                
028100                                                                          
028200 S31-SORT-RELEASE  SECTION.                                               
028300                                                                          
028400     RELEASE SORT-POST FROM SORTWS-AREA                                   
028500     .                                                                    
028600     EJECT                                                                
028700 S32-SORT-RETURN  SECTION.                                                
028800                                                                          
028900     RETURN SORTFIL INTO IN-AREA                                          
029000     AT END                                                               
029100         SET END-OF-SORTFIL TO TRUE                                       
029200     .                                                                    
029300     EJECT                                                                
029400 S99-ABEND SECTION.                                                       
029500                                                                          
029600     SKIP2                                                                
029700     MOVE 'S' TO POSTSUM-OPKOD                                            
029800     CALL POSTSUM USING POSTSUM-PARM                                      
029900     CALL ABEND USING RKOD-ABEND                                          
030000     .                                                                    
