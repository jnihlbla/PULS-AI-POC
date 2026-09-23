000100 ID  DIVISION.                                                            
000200     SKIP2                                                                
000300 PROGRAM-ID.    W3307200.                                                 
000400 AUTHOR.        RONNY STENHOLM                                            
000500 DATE-WRITTEN.  DEC 1989.                                                 
000600     REMARKS.                                                             
000700*                                                                         
000800*    FUNKTION:                                                            
000900*                                                                         
001000*        PROGRAMMET LÄSER EN SORTERAD FIL MED ARTIKELSTATISTIK            
001100*        OCH SKAPAR TRE FILER TILL OLIKA TYPER AV STATISTIK.              
001300*                                                                         
001400     EJECT                                                                
001500 ENVIRONMENT DIVISION.                                                    
001600     SKIP2                                                                
001700 INPUT-OUTPUT SECTION.                                                    
001800                                                                          
001900 FILE-CONTROL.                                                            
002000     SKIP2                                                                
002100*    --- INFIL:                                                           
002200                                                                          
002300     SELECT W33071                       ASSIGN TO W33072D1.              
002400                                                                          
002500     SKIP2                                                                
002600*    --- UTFILER:                                                         
002700     SELECT W33073                       ASSIGN TO W33072D2.              
002800     SKIP2                                                                
002900     SELECT W33077                       ASSIGN TO W33072D3.              
003000     SKIP2                                                                
003100     SELECT W33081                       ASSIGN TO W33072D4.              
003200     SKIP2                                                                
003300     EJECT                                                                
003400 DATA DIVISION.                                                           
003500     SKIP2                                                                
003600 FILE SECTION.                                                            
003700     SKIP3                                                                
003800 FD  W33071                                                               
003900     LABEL RECORD   STANDARD                                              
004000     RECORDING      F                                                     
004100     BLOCK CONTAINS 0.                                                    
004200     SKIP2                                                                
004300*    POST -COPY W33071 -PRE IN-  -L.                                      
004400*++INCLUDE W33071                                                         
004500     EJECT                                                                
004600 FD  W33073                                                               
004700     LABEL RECORD   STANDARD                                              
004800     RECORDING      F                                                     
004900     BLOCK CONTAINS 0.                                                    
005000     SKIP2                                                                
005100*01  POST  -COPY W33073 -PRE 73UT073- -L.                                 
005200*++INCLUDE W33073                                                         
005300     SKIP2                                                                
005400     EJECT                                                                
005500 FD  W33077                                                               
005600     LABEL RECORD   STANDARD                                              
005700     RECORDING      F                                                     
005800     BLOCK CONTAINS 0.                                                    
005900     SKIP2                                                                
006000*01  POST  -COPY W33077 -PRE 77UT077- -L.                                 
006100*++INCLUDE W33077                                                         
006200     SKIP2                                                                
006300     EJECT                                                                
006400 FD  W33081                                                               
006500     LABEL RECORD   STANDARD                                              
006600     RECORDING      F                                                     
006700     BLOCK CONTAINS 0.                                                    
006800     SKIP2                                                                
006900*01  POST  -COPY W33081 -PRE 81UT081- -L.                                 
007000*++INCLUDE W33081                                                         
007100     SKIP2                                                                
007200     EJECT                                                                
007300 WORKING-STORAGE SECTION.                                                 
007400     SKIP2                                                                
007401                                                                          
007410*    -- CHECKED BY WY2000                                                 
007500 77  PROGRAM-NAMN                PIC X(8)    VALUE 'W3307200'.            
007600 77  VA1                         PIC X(3)    VALUE 'VA1'.                 
007700 77  VA2                         PIC X(3)    VALUE 'VA2'.                 
007800 77  VP1                         PIC X(3)    VALUE 'VP1'.                 
007900 77  WS-IDTRANS                  PIC X(4)    VALUE ZERO.                  
008000     SKIP2                                                                
008100*    --- FLAGGOR                                                          
008200 77  W33071-EOF-SW               PIC X(1)    VALUE 'N'.                   
008300     88  END-OF-W33071                       VALUE 'J'.                   
008400     SKIP3                                                                
008500                                                                          
008600     EJECT                                                                
008700                                                                          
008800 01  DYNAMISKA-SUBPROGRAM.                                                
008900*                                                                         
009000     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM '.            
009100     SKIP2                                                                
009200*- - - - - - - - - - - - - - - - PARAMETRAR TILL POSTSUM                  
009300*                                                                         
009400*01  -COPY W0005 -PRE  POSTSUM-                                           
009500*++INCLUDE W0005                                                          
009600     EJECT                                                                
009700 01  UT-AREA-START                PIC X(24)   VALUE                       
009800                                            'UT-AREA-START  '.            
009900     SKIP3                                                                
010000 01  UT-AREA.                                                             
010100     03  FILLER                   PIC X(154).                             
010200     SKIP2                                                                
010300*01  FILLER  -PRE 73POST-    -COPY W33073 -RED UT-AREA                    
010400*++INCLUDE W33073                                                         
010500     EJECT                                                                
010600*01  FILLER  -PRE 77POST-    -COPY W33077 -RED UT-AREA                    
010700*++INCLUDE W33077                                                         
010800     EJECT                                                                
010900*01  FILLER  -PRE 81POST-    -COPY W33081 -RED UT-AREA                    
011000*++INCLUDE W33081                                                         
011100     EJECT                                                                
011200                                                                          
011300 01  IN-AREA-START                PIC X(24)   VALUE                       
011400                                            'IN-AREA-START  '.            
011500     SKIP3                                                                
011600 01  IN-AREA.                                                             
011700     03  FILLER                   PIC X(176).                             
011800     SKIP2                                                                
011900*01  AREA  -PRE 71POST-    -COPY W33071 -RED IN-AREA                      
012000*++INCLUDE W33071                                                         
012100     EJECT                                                                
012200 PROCEDURE DIVISION.                                                      
012300     SKIP2                                                                
012400 STYR SECTION.                                                            
012500     PERFORM A-INIT                                                       
012600     PERFORM S01-LAS-R071                                                 
012700     PERFORM UNTIL END-OF-W33071                                          
012800       PERFORM UNTIL 71POST-IDPTYP NOT = VA1                              
012900                     OR  END-OF-W33071                                    
013000         PERFORM B-FLYTTA-URVAL-VA1                                       
013100         PERFORM S22-SKRIV-FIL-R073                                       
013200         PERFORM S01-LAS-R071                                             
013300       END-PERFORM                                                        
013400       PERFORM UNTIL 71POST-IDPTYP NOT = VA2                              
013500                     OR  END-OF-W33071                                    
013600         PERFORM C-FLYTTA-URVAL-VA2                                       
013700         PERFORM S33-SKRIV-FIL-R077                                       
013800         PERFORM S01-LAS-R071                                             
013900       END-PERFORM                                                        
014000       PERFORM UNTIL 71POST-IDPTYP NOT = VP1                              
014100                     OR  END-OF-W33071                                    
014200         PERFORM D-FLYTTA-URVAL-VP1                                       
014300         PERFORM S44-SKRIV-FIL-R081                                       
014400         PERFORM S01-LAS-R071                                             
014500       END-PERFORM                                                        
014600     END-PERFORM                                                          
014700     PERFORM Z-FINIT                                                      
014800     MOVE ZERO TO RETURN-CODE                                             
014900     GOBACK                                                               
015000     .                                                                    
015100     SKIP2                                                                
015200 A-INIT SECTION.                                                          
015300     SKIP2                                                                
015400     OPEN INPUT W33071                                                    
015500     SKIP2                                                                
015600     OPEN OUTPUT W33073                                                   
015700                 W33077                                                   
015800                 W33081                                                   
015900     SKIP2                                                                
016000     MOVE PROGRAM-NAMN TO POSTSUM-PROGNAMN                                
016100     .                                                                    
016200     EJECT                                                                
016300                                                                          
016400 B-FLYTTA-URVAL-VA1 SECTION.                                              
016500                                                                          
016600     MOVE 71POST-IDUSER      TO 73POST-IDUSER                             
016700     MOVE 71POST-DAREGDAT    TO 73POST-DAREGDAT                           
016800     MOVE 71POST-TIREGTID    TO 73POST-TIREGTID                           
016900     MOVE 71POST-IDFSGURV    TO 73POST-IDFSGURV                           
017000     MOVE 71POST-IDPTYP      TO 73POST-IDPTYP                             
017100     MOVE 71POST-IDGTYP      TO 73POST-IDGTYP                             
017200     MOVE 71POST-IDARTNR     TO 73POST-IDARTNR                            
017300     MOVE 71POST-BEART-SVE TO 73POST-BEART-SVE                            
017400     MOVE 71POST-KDPRODSL    TO 73POST-KDPRODSL                           
017500     MOVE 71POST-BEPRODSL    TO 73POST-BEPRODSL                           
017600     MOVE 71POST-IDFKNGRP    TO 73POST-IDFKNGRP                           
017700     MOVE 71POST-BEFKNGRP    TO 73POST-BEFKNGRP                           
017800     MOVE 71POST-DAFSGVV     TO 73POST-DAFSGVV                            
017900     MOVE 71POST-PRARTSJK    TO 73POST-PRARTSJK                           
018000     MOVE 71POST-SULEVANT    TO 73POST-SULEVANT                           
018100     MOVE 71POST-SUARTFSG    TO 73POST-SUARTFSG                           
018200     .                                                                    
018300     EJECT                                                                
018400                                                                          
018500 C-FLYTTA-URVAL-VA2 SECTION.                                              
018600                                                                          
018700     MOVE 71POST-IDUSER         TO 77POST-IDUSER                          
018800     MOVE 71POST-DAREGDAT       TO 77POST-DAREGDAT                        
018900     MOVE 71POST-TIREGTID       TO 77POST-TIREGTID                        
019000     MOVE 71POST-IDFSGURV       TO 77POST-IDFSGURV                        
019100     MOVE 71POST-IDPTYP         TO 77POST-IDPTYP                          
019200     MOVE 71POST-IDGTYP         TO 77POST-IDGTYP                          
019300     MOVE 71POST-IDARTNR        TO 77POST-IDARTNR                         
019400     MOVE 71POST-BEART-SVE      TO 77POST-BEART-SVE                       
019500     MOVE 71POST-KDPRODSL       TO 77POST-KDPRODSL                        
019600     MOVE 71POST-BEPRODSL       TO 77POST-BEPRODSL                        
019700     MOVE 71POST-IDDISTR        TO 77POST-IDDISTR                         
019800     MOVE 71POST-IDKONCNR       TO 77POST-IDKONCNR                        
019900     MOVE 71POST-KDMARK-BUDG    TO 77POST-KDMARK-BUDG                     
020000     MOVE 71POST-BEMARK-BUDG    TO 77POST-BEMARK-BUDG                     
020100     MOVE 71POST-DAFSGVV        TO 77POST-DAFSGVV                         
020200     MOVE 71POST-PRARTSJK       TO 77POST-PRARTSJK                        
020300     MOVE 71POST-SULEVANT       TO 77POST-SULEVANT                        
020400     MOVE 71POST-SUARTFSG       TO 77POST-SUARTFSG                        
020500     .                                                                    
020600     EJECT                                                                
020700                                                                          
020800 D-FLYTTA-URVAL-VP1 SECTION.                                              
020900                                                                          
021000     MOVE 71POST-IDUSER         TO 81POST-IDUSER                          
021100     MOVE 71POST-DAREGDAT       TO 81POST-DAREGDAT                        
021200     MOVE 71POST-TIREGTID       TO 81POST-TIREGTID                        
021300     MOVE 71POST-IDFSGURV       TO 81POST-IDFSGURV                        
021400     MOVE 71POST-IDPTYP         TO 81POST-IDPTYP                          
021500     MOVE 71POST-IDGTYP         TO 81POST-IDGTYP                          
021600     MOVE 71POST-KDPRODSL       TO 81POST-KDPRODSL                        
021700     MOVE 71POST-BEPRODSL       TO 81POST-BEPRODSL                        
021800     MOVE 71POST-IDFKNGRP       TO 81POST-IDFKNGRP                        
021900     MOVE 71POST-BEFKNGRP       TO 81POST-BEFKNGRP                        
022000     MOVE 71POST-IDDISTR        TO 81POST-IDDISTR                         
022100     MOVE 71POST-IDKONCNR       TO 81POST-IDKONCNR                        
022200     MOVE 71POST-KDMARK-BUDG    TO 81POST-KDMARK-BUDG                     
022300     MOVE 71POST-BEMARK-BUDG    TO 81POST-BEMARK-BUDG                     
022400     MOVE 71POST-DAFSGVV        TO 81POST-DAFSGVV                         
022500     MOVE 71POST-PRARTSJK       TO 81POST-PRARTSJK                        
022600     MOVE 71POST-SULEVANT       TO 81POST-SULEVANT                        
022700     MOVE 71POST-SUARTFSG       TO 81POST-SUARTFSG                        
022800     .                                                                    
022900     EJECT                                                                
023000                                                                          
023100                                                                          
023200 S01-LAS-R071 SECTION.                                                    
023300     SKIP2                                                                
023400     READ W33071 INTO IN-AREA                                             
023500     AT END                                                               
023600         SET END-OF-W33071 TO TRUE                                        
023700     END-READ                                                             
023800     IF NOT END-OF-W33071                                                 
023900       MOVE SPACE TO POSTSUM-TRANSTYP                                     
024000       MOVE 'W33071' TO POSTSUM-FDNAMN                                    
024100       MOVE 'W33072D1' TO POSTSUM-DDNAMN2                                 
024200       CALL POSTSUM USING POSTSUM-PARM                                    
024300     END-IF                                                               
024400     .                                                                    
024500                                                                          
024600 S22-SKRIV-FIL-R073 SECTION.                                              
024700     SKIP2                                                                
024800     WRITE 73UT073-POST FROM UT-AREA                                      
024900     MOVE SPACE TO POSTSUM-TRANSTYP                                       
025000     MOVE 'W33073' TO POSTSUM-FDNAMN                                      
025100     MOVE 'W33072D2' TO POSTSUM-DDNAMN2                                   
025200     CALL POSTSUM USING POSTSUM-PARM                                      
025300     .                                                                    
025400                                                                          
025500 S33-SKRIV-FIL-R077 SECTION.                                              
025600     SKIP2                                                                
025700     WRITE 77UT077-POST FROM UT-AREA                                      
025800     MOVE SPACE TO POSTSUM-TRANSTYP                                       
025900     MOVE 'W33077' TO POSTSUM-FDNAMN                                      
026000     MOVE 'W33072D2' TO POSTSUM-DDNAMN2                                   
026100     CALL POSTSUM USING POSTSUM-PARM                                      
026200     .                                                                    
026300                                                                          
026400 S44-SKRIV-FIL-R081 SECTION.                                              
026500     SKIP2                                                                
026600     WRITE 81UT081-POST FROM UT-AREA                                      
026700     MOVE SPACE TO POSTSUM-TRANSTYP                                       
026800     MOVE 'W33081' TO POSTSUM-FDNAMN                                      
026900     MOVE 'W33072D2' TO POSTSUM-DDNAMN2                                   
027000     CALL POSTSUM USING POSTSUM-PARM                                      
027100     .                                                                    
027200                                                                          
027300 Z-FINIT SECTION.                                                         
027400     SKIP2                                                                
027500     CLOSE W33071                                                         
027600           W33073                                                         
027700           W33077                                                         
027800           W33081                                                         
027900     MOVE 'S' TO POSTSUM-OPKOD                                            
028000     CALL POSTSUM USING POSTSUM-PARM                                      
028100     .                                                                    
028200     SKIP2                                                                
028300     EJECT                                                                
