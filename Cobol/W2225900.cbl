000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W2225900.                                                
000300 AUTHOR.         STEFAN ANDREASSON.                                       
000400 DATE-WRITTEN.   APRIL 2000.                                              
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*                                                                         
000900*                                                                         
001000*        PROGRAMMET SKRIVER UT EN LISTA ÖVER                              
001100*        MANUELLT SATTA SÄSONGER                                          
001200*        DÄR TOM DATUM KOMMER ATT UPPHÖRA INNAN NÄSTA                     
001300*        SÄSONGSBERÄKNING (EN GÅNG VARANN MÅNAD)                          
001400*        DETTA PROGRAM KÖRS TRE VECKOR INNAN SÄSONGSBERÄKNINGEN           
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
002600     SELECT W22259                     ASSIGN TO W22259D1.                
002700*          ---                                                            
002800     SELECT W271UT-ALL                 ASSIGN TO W22259D2.                
002900                                                                          
003000     EJECT                                                                
003100 DATA DIVISION.                                                           
003200     SKIP2                                                                
003300 FILE SECTION.                                                            
003400     SKIP3                                                                
003500                                                                          
003600 FD  W22259                                                               
003700     RECORDING       F                                                    
003800     BLOCK CONTAINS  0.                                                   
003900                                                                          
004000*01  POST  -COPY W22258    -PRE IN-    -L.                                
004100                                                                          
004200 FD  W271UT-ALL                                                           
004300     RECORDING F                                                          
004400     BLOCK CONTAINS  0 RECORDS.                                           
004500 01  ORDER-REPORT-REC-ALL    PIC X(120).                                  
004600                                                                          
004700     EJECT                                                                
004800 WORKING-STORAGE SECTION.                                                 
004900                                                                          
005000*    -- CHECKED BY WY2000                                                 
005100 77  IDPGM                       PIC X(8)    VALUE 'W2225900'.            
005200 77  JA                          PIC X       VALUE 'J'.                   
005300 77  NEJ                         PIC X       VALUE 'N'.                   
005400                                                                          
005500 77  W22259-EOF-SW               PIC X       VALUE 'N'.                   
005600     88  END-OF-W22259                       VALUE 'J'.                   
005700                                                                          
005800                                                                          
005900 01  ARBETSAREOR.                                                         
006000     03 WS-PAGE-ALL              PIC  9(4)  VALUE ZERO.                   
006100     03 WS-RADANT-ALL            PIC  9(2)  VALUE ZERO.                   
006200     03 WS-RED-DATUM.                                                     
006210       05  WS-RED-AA             PIC X(2)   VALUE SPACE.                  
006220       05  FILLER                PIC X      VALUE '-'.                    
006300       05  WS-RED-MM             PIC X(2)   VALUE SPACE.                  
006400       05  FILLER                PIC X      VALUE '-'.                    
006500       05  WS-RED-DD             PIC X(2)   VALUE SPACE.                  
006800     03 WS-ANTAL-ARTIKLAR-ALL    PIC 9(8)   VALUE ZERO.                   
006810     03 WS-IDANSK                PIC S9(3)   VALUE ZERO COMP-3.           
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
008300 01  W22259-AREA-START           PIC X(24)   VALUE                        
008400                                 'W22259-AREA-START  '.                   
008500                                                                          
008600*01  AREA -COPY W22258     -PRE IN-                                       
008700                                                                          
008800     EJECT                                                                
008900 01  W271UT-AREA-START           PIC X(24)   VALUE                        
009000                                 'W271UT-AREA-START  '.                   
009100                                                                          
009200 01  UT-AREOR.                                                            
009300     03 UT-RPT-PRINT-LINES.                                               
009400       05 UT-HEADING-1.                                                   
009500         10 FILLER               PIC X(1)  VALUE SPACE.                   
009501         10 FILLER               PIC X(23)                                
009510                                 VALUE 'VOLVO CUSTOMER SERVICE'.          
009800         10 FILLER               PIC X(15) VALUE SPACE.                   
009900         10 UT-LIST-NAME         PIC X(12) VALUE SPACE.                   
010000         10 FILLER               PIC X(50) VALUE SPACE.                   
010100         10 UT-REPORT-DATE       PIC X(8)  VALUE SPACE.                   
010200         10 FILLER               PIC X(1)  VALUE SPACE.                   
010300         10 FILLER               PIC X(4)  VALUE 'PAGE'.                  
010400         10 FILLER               PIC X(1)  VALUE SPACE.                   
010500         10 UT-REPORT-PAGE       PIC ZZZ9.                                
010600       05 UT-HEADING-2.                                                   
010700         10 FILLER               PIC X(22) VALUE SPACE.                   
010800         10 FILLER               PIC X(09) VALUE 'MANUELLA'.              
010900         10 FILLER               PIC X(07) VALUE 'SÄSONGS'.               
010910         10 FILLER               PIC X(09) VALUE 'ARTIKLAR'.              
011000         10 FILLER               PIC X(11) VALUE 'SOM KOMMER'.            
011100         10 FILLER               PIC X(12) VALUE 'ATT UPPHÖRA'.           
011200         10 FILLER               PIC X(10) VALUE 'VID NÄSTA'.             
011210         10 FILLER               PIC X(09) VALUE 'OMRÄKNING'.             
011300       05 UT-HEADING-3.                                                   
011900         10 FILLER               PIC X(1)  VALUE SPACE.                   
012000         10 FILLER               PIC X(4)  VALUE 'ANSK'.                  
012100         10 FILLER               PIC X(3)  VALUE SPACE.                   
012200         10 FILLER               PIC X(6)  VALUE 'ARTNR'.                 
012300         10 FILLER               PIC X(1)  VALUE SPACE.                   
012400         10 FILLER               PIC X(11) VALUE 'BENÄMNING'.             
012700       05 UT-RAD-1.                                                       
012800         10 FILLER               PIC X(1)  VALUE SPACE.                   
012900         10 UT-IDANSK            PIC ZZ9   VALUE ZERO.                    
013000         10 FILLER               PIC X     VALUE SPACE.                   
013100         10 UT-IDARTNR           PIC Z(8)9 VALUE ZERO.                    
013200         10 FILLER               PIC X(1)  VALUE SPACE.                   
013300         10 UT-BEART             PIC X(19) VALUE SPACE.                   
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
015200     OPEN INPUT  W22259                                                   
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
016900     PERFORM S01-LAES-W22259                                              
017000                                                                          
017100     PERFORM UNTIL END-OF-W22259                                          
017200       MOVE IN-IDANSK        TO UT-IDANSK                                 
017300       MOVE IN-IDARTNR       TO UT-IDARTNR                                
017400       MOVE IN-BEART         TO UT-BEART                                  
017600       PERFORM BA-SKRIV-LISTA                                             
017700       PERFORM S01-LAES-W22259                                            
017800     END-PERFORM                                                          
017900     .                                                                    
018000                                                                          
018100     EJECT                                                                
018200 BA-SKRIV-LISTA SECTION.                                                  
018300     IF WS-RADANT-ALL = ZERO                                              
018400     OR WS-RADANT-ALL > 42                                                
018410     OR IN-IDANSK NOT = WS-IDANSK                                         
018420       MOVE IN-IDANSK        TO WS-IDANSK                                 
018500       ADD 1                 TO WS-PAGE-ALL                               
018700       MOVE WS-PAGE-ALL      TO UT-REPORT-PAGE                            
018800       MOVE 'W22259-001'     TO UT-LIST-NAME                              
018900                                                                          
019000       MOVE UT-HEADING-1     TO ORDER-REPORT-REC-ALL                      
019100       WRITE ORDER-REPORT-REC-ALL AFTER PAGE                              
019200       MOVE UT-HEADING-2     TO ORDER-REPORT-REC-ALL                      
019300       WRITE ORDER-REPORT-REC-ALL                                         
019400       MOVE UT-HEADING-3     TO ORDER-REPORT-REC-ALL                      
019500       WRITE ORDER-REPORT-REC-ALL AFTER 2                                 
019800       MOVE SPACE            TO ORDER-REPORT-REC-ALL                      
019900       WRITE ORDER-REPORT-REC-ALL                                         
020000       MOVE 09               TO WS-RADANT-ALL                             
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
021100     CLOSE W22259                                                         
021200           W271UT-ALL                                                     
021300     .                                                                    
021400                                                                          
021500     EJECT                                                                
021600 S01-LAES-W22259  SECTION.                                                
021700     READ W22259 INTO IN-AREA                                             
021800     AT END                                                               
021900        SET END-OF-W22259 TO TRUE                                         
022000                                                                          
022100     END-READ                                                             
022200     .                                                                    
