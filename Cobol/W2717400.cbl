000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W2717400.                                                
000300 AUTHOR.         STEFAN ANDREASSON.                                       
000400 DATE-WRITTEN.   MARS 1998.                                               
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*                                                                         
000900*                                                                         
001000*        PROGRAMMET SKRIVER UT EN LISTA ÖVER                              
001100*        MANUELLT SATTA SÄSONGER                                          
001200*        DÄR TOM DATUM KOMMER ATT UPPHÖRA INNAN NÄSTA                     
001300*        SÄSONGSBERÄKNING (EN GÅNG PER KVARTAL)                           
001400*        DETTA PROGRAM KÖRS TVÅ VECKOR INNAN SÄSONGSBERÄKNINGEN           
001500*                                                                         
001600*                                                                         
001700                                                                          
001800     EJECT                                                                
001900 ENVIRONMENT DIVISION.                                                    
002000     SKIP2                                                                
002100 INPUT-OUTPUT SECTION.                                                    
002200                                                                          
002300 FILE-CONTROL.                                                            
002400     SKIP2                                                                
002500*          ---                                                            
002600     SELECT W27174                     ASSIGN TO W27174D1.                
002700*          ---                                                            
002800     SELECT W271UT-ALL                 ASSIGN TO W27174D2.                
002900                                                                          
003000     EJECT                                                                
003100 DATA DIVISION.                                                           
003200     SKIP2                                                                
003300 FILE SECTION.                                                            
003400     SKIP3                                                                
003500                                                                          
003600 FD  W27174                                                               
003700     RECORDING       F                                                    
003800     BLOCK CONTAINS  0.                                                   
003900                                                                          
004000*01  POST  -COPY W27173    -PRE IN-    -L.                                
004100                                                                          
004200 FD  W271UT-ALL                                                           
004300     RECORDING V                                                          
004400     BLOCK CONTAINS  0 RECORDS.                                           
004500 01  ORDER-REPORT-REC-ALL    PIC X(120).                                  
004600                                                                          
004700     EJECT                                                                
004800 WORKING-STORAGE SECTION.                                                 
004900                                                                          
005000*    -- CHECKED BY WY2000                                                 
005100 77  IDPGM                       PIC X(8)    VALUE 'W2717400'.            
005200 77  JA                          PIC X       VALUE 'J'.                   
005300 77  NEJ                         PIC X       VALUE 'N'.                   
005400                                                                          
005500 77  W27174-EOF-SW               PIC X       VALUE 'N'.                   
005600     88  END-OF-W27174                       VALUE 'J'.                   
005700                                                                          
005800                                                                          
005900 01  ARBETSAREOR.                                                         
006000     03 WS-PAGE-ALL              PIC  9(4)  VALUE ZERO.                   
006100     03 WS-RADANT-ALL            PIC  9(2)  VALUE ZERO.                   
006200     03 WS-RED-DATUM.                                                     
006300       05  WS-RED-MM             PIC X(2)   VALUE SPACE.                  
006400       05  FILLER                PIC X      VALUE '/'.                    
006500       05  WS-RED-DD             PIC X(2)   VALUE SPACE.                  
006600       05  FILLER                PIC X      VALUE '/'.                    
006700       05  WS-RED-AA             PIC X(2)   VALUE SPACE.                  
006800     03 WS-ANTAL-ARTIKLAR-ALL    PIC 9(8)   VALUE ZERO.                   
006900                                                                          
007000                                                                          
007100 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
007200 01  FILLER REDEFINES DAGENS-DATUM.                                       
007300     03  DAGENS-DATUM-AAR        PIC 9(2).                                
007400     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
007500     03  DAGENS-DATUM-DAG        PIC 9(2).                                
007600                                                                          
007700                                                                          
007800 01  FELTEXT.                                                             
007900     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
008000     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
008100                                                                          
008200     EJECT                                                                
008300 01  W27174-AREA-START           PIC X(24)   VALUE                        
008400                                 'W27174-AREA-START  '.                   
008500                                                                          
008600*01  AREA -COPY W27173     -PRE IN-                                       
008700                                                                          
008800     EJECT                                                                
008900 01  W271UT-AREA-START           PIC X(24)   VALUE                        
009000                                 'W271UT-AREA-START  '.                   
009100                                                                          
009200 01  UT-AREOR.                                                            
009300     03 UT-RPT-PRINT-LINES.                                               
009400       05 UT-HEADING-1.                                                   
009500         10 FILLER               PIC X(1)  VALUE SPACE.                   
009600         10 FILLER               PIC X(10) VALUE 'VOLVO CAR'.             
009700         10 FILLER               PIC X(17) VALUE 'AFTERSALES'.            
009800         10 FILLER               PIC X(11) VALUE SPACE.                   
009900         10 UT-LIST-NAME         PIC X(12) VALUE SPACE.                   
010000         10 FILLER               PIC X(50) VALUE SPACE.                   
010100         10 UT-REPORT-DATE       PIC X(8)  VALUE SPACE.                   
010200         10 FILLER               PIC X(1)  VALUE SPACE.                   
010300         10 FILLER               PIC X(4)  VALUE 'PAGE'.                  
010400         10 FILLER               PIC X(1)  VALUE SPACE.                   
010500         10 UT-REPORT-PAGE       PIC ZZZ9.                                
010600       05 UT-HEADING-2.                                                   
010700         10 FILLER               PIC X(22) VALUE SPACE.                   
010800         10 FILLER               PIC X(09) VALUE 'MANUELLY'.              
010900         10 FILLER               PIC X(12) VALUE 'SEASONPARTS'.           
011000         10 FILLER               PIC X(10) VALUE 'THAT WILL'.             
011100         10 FILLER               PIC X(12) VALUE 'EXPIRE NEXT'.           
011200         10 FILLER               PIC X(14) VALUE 'RECALCULATION'.         
011300       05 UT-HEADING-3.                                                   
011400         10 FILLER               PIC X(1)  VALUE SPACE.                   
011500         10 FILLER               PIC X(5)  VALUE 'BUYER'.                 
011600         10 FILLER               PIC X(4)  VALUE SPACE.                   
011700         10 FILLER               PIC X(4)  VALUE 'PART'.                  
011800       05 UT-HEADING-4.                                                   
011900         10 FILLER               PIC X(1)  VALUE SPACE.                   
012000         10 FILLER               PIC X(4)  VALUE 'CODE'.                  
012100         10 FILLER               PIC X(3)  VALUE SPACE.                   
012200         10 FILLER               PIC X(6)  VALUE 'NUMBER'.                
012300         10 FILLER               PIC X(1)  VALUE SPACE.                   
012400         10 FILLER               PIC X(11) VALUE 'DESCRIPTION'.           
012500         10 FILLER               PIC X(10) VALUE SPACE.                   
012600         10 FILLER               PIC X(2)  VALUE 'DC'.                    
012700       05 UT-RAD-1.                                                       
012800         10 FILLER               PIC X(1)  VALUE SPACE.                   
012900         10 UT-IDPERSON-BUY      PIC ZZ9   VALUE ZERO.                    
013000         10 FILLER               PIC X     VALUE SPACE.                   
013100         10 UT-IDARTNR           PIC Z(8)9 VALUE ZERO.                    
013200         10 FILLER               PIC X(1)  VALUE SPACE.                   
013300         10 UT-BEART             PIC X(19) VALUE SPACE.                   
013400         10 FILLER               PIC X(2)  VALUE SPACE.                   
013500         10 UT-IDDC              PIC X(2)  VALUE SPACE.                   
013600                                                                          
013700     EJECT                                                                
013800 PROCEDURE DIVISION.                                                      
013900 MAIN SECTION.                                                            
014000                                                                          
014100     PERFORM A-INIT                                                       
014200     PERFORM B-SKAPA-POSTER                                               
014300     PERFORM Z-FINIT                                                      
014400                                                                          
014500     MOVE ZERO TO RETURN-CODE                                             
014600     GOBACK                                                               
014700     .                                                                    
014800                                                                          
014900     EJECT                                                                
015000 A-INIT SECTION.                                                          
015100                                                                          
015200     OPEN INPUT  W27174                                                   
015300                                                                          
015400     OPEN OUTPUT W271UT-ALL                                               
015500                                                                          
015600     ACCEPT DAGENS-DATUM     FROM DATE                                    
015700     MOVE DAGENS-DATUM-AAR   TO WS-RED-AA                                 
015800     MOVE DAGENS-DATUM-MAANAD                                             
015900                             TO WS-RED-MM                                 
016000     MOVE DAGENS-DATUM-DAG   TO WS-RED-DD                                 
016100     MOVE WS-RED-DATUM       TO UT-REPORT-DATE                            
016200     MOVE ZERO               TO WS-PAGE-ALL                               
016300                                WS-RADANT-ALL                             
016400     MOVE ZERO               TO WS-ANTAL-ARTIKLAR-ALL                     
016500     .                                                                    
016600                                                                          
016700     EJECT                                                                
016800 B-SKAPA-POSTER SECTION.                                                  
016900     PERFORM S01-LAES-W27174                                              
017000                                                                          
017100     PERFORM UNTIL END-OF-W27174                                          
017200       MOVE IN-IDPERSON-BUY  TO UT-IDPERSON-BUY                           
017300       MOVE IN-IDARTNR       TO UT-IDARTNR                                
017400       MOVE IN-BEART         TO UT-BEART                                  
017500       MOVE IN-IDDC          TO UT-IDDC                                   
017600       PERFORM BA-SKRIV-LISTA                                             
017700       PERFORM S01-LAES-W27174                                            
017800     END-PERFORM                                                          
017900     .                                                                    
018000                                                                          
018100     EJECT                                                                
018200 BA-SKRIV-LISTA SECTION.                                                  
018300     IF WS-RADANT-ALL = ZERO                                              
018400     OR WS-RADANT-ALL > 42                                                
018500       ADD 1                 TO WS-PAGE-ALL                               
018600       MOVE IN-IDDC          TO UT-IDDC                                   
018700       MOVE WS-PAGE-ALL      TO UT-REPORT-PAGE                            
018800       MOVE 'W27174-001'     TO UT-LIST-NAME                              
018900                                                                          
019000       MOVE UT-HEADING-1     TO ORDER-REPORT-REC-ALL                      
019100       WRITE ORDER-REPORT-REC-ALL AFTER PAGE                              
019200       MOVE UT-HEADING-2     TO ORDER-REPORT-REC-ALL                      
019300       WRITE ORDER-REPORT-REC-ALL                                         
019400       MOVE UT-HEADING-3     TO ORDER-REPORT-REC-ALL                      
019500       WRITE ORDER-REPORT-REC-ALL AFTER 2                                 
019600       MOVE UT-HEADING-4     TO ORDER-REPORT-REC-ALL                      
019700       WRITE ORDER-REPORT-REC-ALL                                         
019800       MOVE SPACE            TO ORDER-REPORT-REC-ALL                      
019900       WRITE ORDER-REPORT-REC-ALL                                         
020000       MOVE 10               TO WS-RADANT-ALL                             
020100                                                                          
020200     END-IF                                                               
020300                                                                          
020400     MOVE UT-RAD-1           TO ORDER-REPORT-REC-ALL                      
020500     WRITE ORDER-REPORT-REC-ALL                                           
020600     ADD 1                   TO WS-RADANT-ALL                             
020700     .                                                                    
020800                                                                          
020900     EJECT                                                                
021000 Z-FINIT SECTION.                                                         
021100     CLOSE W27174                                                         
021200           W271UT-ALL                                                     
021300     .                                                                    
021400                                                                          
021500     EJECT                                                                
021600 S01-LAES-W27174  SECTION.                                                
021700     READ W27174 INTO IN-AREA                                             
021800     AT END                                                               
021900        SET END-OF-W27174 TO TRUE                                         
022000                                                                          
022100     END-READ                                                             
022200     .                                                                    
