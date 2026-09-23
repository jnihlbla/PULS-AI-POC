000100 ID DIVISION.                                                             
000200                                                                          
000300 PROGRAM-ID.    W5128600.                                                 
000400                                                                          
000500*    AUTHOR.        BARSHARANI BISHOYE.                                   
000600*    DATE-WRITTEN   NOVEMBER 2019.                                        
000700*                                                                         
000800*    FUNKTION:                                                            
000900*               SKAPAR W51286 FÖR KURANSGRUPPER PER DC                    
001000*               TURNOVERVALUE PER PRODUCTGROUP                            
001100*               TURNOVERVALUE PER PRODUCTGROUP AND TURNOVERGROUP          
001200*                                                                         
001300     EJECT                                                                
001400 ENVIRONMENT DIVISION.                                                    
001500 INPUT-OUTPUT SECTION.                                                    
001600 FILE-CONTROL.                                                            
001700                                                                          
001800     SELECT W51284  ASSIGN       TO W51286D1.                             
001900                                                                          
002000     SELECT W51286  ASSIGN       TO W51286D2.                             
002100     EJECT                                                                
002200                                                                          
002300 DATA DIVISION.                                                           
002400 FILE SECTION.                                                            
002500                                                                          
002600 FD  W51284                                                               
002700     RECORDING F                                                          
002800     BLOCK CONTAINS 0.                                                    
002900                                                                          
003000*01  INTRANS     -COPY W51284    -L                                       
003100     SKIP2                                                                
003200                                                                          
003300 FD  W51286                                                               
003400     RECORDING V                                                          
003500     BLOCK CONTAINS 0.                                                    
003600                                                                          
003700 01  W51286-REC              PIC X(121).                                  
003800     EJECT                                                                
003900                                                                          
004000 WORKING-STORAGE SECTION.                                                 
004100     SKIP3                                                                
004200 77  IDPGM                   PIC X(8)      VALUE 'W5128600'.              
004300 77  JA                      PIC X         VALUE 'J'.                     
004400 77  NEJ                     PIC X         VALUE 'N'.                     
004500 77  EOF-W51284              PIC X         VALUE 'N'.                     
004600                                                                          
004800 01  W-MAXRADER              PIC S9(3)     VALUE +999 COMP-3.             
004900 01  W-SIDOR                 PIC S9(3)     VALUE +0  COMP-3.              
004910 01  WS-IDDC                 PIC X(2)      VALUE SPACE.                   
005000 01  WS-KDPSLLOC             PIC S9(2)     VALUE +0 COMP-3.               
005100 01  W-KUR5                  PIC S9(11)V99  COMP-3.                       
005200 01  W-KUR6                  PIC S9(11)V99  COMP-3.                       
005300 01  W-KUR56                 PIC S9(11)V99  COMP-3.                       
005400                                                                          
005500 01  SUBPROGRAM.                                                          
005600     03  POSTSUM             PIC X(8)    VALUE 'POSTSUM'.                 
005700                                                                          
005800 01  W51284-TRANSID.                                                      
005900     03  FILLER              PIC X(6) VALUE 'W51284'.                     
006000     03  FILLER              PIC X(8) VALUE 'W51286D1'.                   
006100     03  FILLER              PIC X(4) VALUE '4311'.                       
006200                                                                          
006300     EJECT                                                                
006400*   -COPY W0005  -PRE POSTSUM-                                            
006500     EJECT                                                                
006600                                                                          
006700 01  FILLER                  PIC X(16)   VALUE 'W51284-AREA'.             
006800 01  INAREA.                                                              
006900*    03  -COPY W51284     -PRE IN-                                        
007000     EJECT                                                                
007010                                                                          
007020 01  W001-DAP.                                                            
007030     03  FILLER                  PIC X(165)  VALUE SPACE.                 
007100                                                                          
007200*FOR SHELFLIFE                                                            
007300 01  TEXT-AREA.                                                           
007400     03  HEADER.                                                          
007500         05  FILLER          PIC X(7) VALUE 'PART.NO'.                    
007600         05  FILLER          PIC X(1)    VALUE '	'.                       
007700         05  FILLER          PIC X(7) VALUE 'SS CODE'.                    
007800         05  FILLER          PIC X(1)    VALUE '	'.                       
007900         05  FILLER          PIC X(5) VALUE 'STOCK'.                      
008000         05  FILLER          PIC X(1)    VALUE '	'.                       
008100         05  FILLER          PIC X(8) VALUE 'AVG.COST'.                   
008200         05  FILLER          PIC X(1)    VALUE '	'.                       
008300         05  FILLER          PIC X(7) VALUE 'STOCK 5'.                    
008400         05  FILLER          PIC X(1)    VALUE '	'.                       
008500         05  FILLER          PIC X(7) VALUE 'STOCK 6'.                    
008600         05  FILLER          PIC X(1)    VALUE '	'.                       
008700         05  FILLER          PIC X(10) VALUE 'STOCKVALUE'.                
008800         05  FILLER          PIC X(1)    VALUE '	'.                       
008900                                                                          
009000     03  ROW.                                                             
009100         05  ROW-IDARTNR     PIC Z(9).                                    
009200         05  FILLER          PIC X(1)    VALUE '	'.                       
009300         05  ROW-KDERS       PIC Z(3).                                    
009400         05  FILLER          PIC X(1)    VALUE '	'.                       
009500         05  ROW-KVLS        PIC Z(7).                                    
009600         05  FILLER          PIC X(1)    VALUE '	'.                       
009700         05  ROW-PRAVCOST    PIC Z(6)9.99-.                               
009800         05  FILLER          PIC X(1)    VALUE '	'.                       
009900         05  ROW-IN-STOCK5   PIC Z(7).                                    
010000         05  FILLER          PIC X(1)    VALUE '	'.                       
010100         05  ROW-IN-STOCK6   PIC Z(7).                                    
010200         05  FILLER          PIC X(1)    VALUE '	'.                       
010300         05  ROW-LVALUE      PIC Z(9)9.99-.                               
010400         05  FILLER          PIC X(1)    VALUE '	'.                       
010500                                                                          
010600                                                                          
010700 PROCEDURE DIVISION.                                                      
010800                                                                          
010900 MAIN SECTION.                                                            
011000                                                                          
011100     PERFORM A-INIT                                                       
011200                                                                          
011300     PERFORM S01-READ-W51284-POST                                         
011400     IF EOF-W51284 = JA                                                   
011500       CONTINUE                                                           
011600     ELSE                                                                 
011800       PERFORM B-CREATE-SHELFLIFE-POST                                    
011900     END-IF                                                               
012000                                                                          
012100     PERFORM Z-END                                                        
012200                                                                          
012300     MOVE ZERO TO RETURN-CODE                                             
012400     GOBACK                                                               
012500     .                                                                    
012600     EJECT                                                                
012700                                                                          
012800 A-INIT SECTION.                                                          
012900     OPEN INPUT  W51284                                                   
013000          OUTPUT W51286                                                   
013100                                                                          
013200     MOVE IDPGM           TO POSTSUM-PROGNAMN                             
013400     .                                                                    
013500     EJECT                                                                
013600                                                                          
013700 B-CREATE-SHELFLIFE-POST SECTION.                                         
013800     PERFORM UNTIL EOF-W51284 = JA                                        
013810       IF IN-IDDC NOT = WS-IDDC                                           
013811          PERFORM S02-SKRIV-DAP1                                          
013812          PERFORM S03-SKRIV-DAP2                                          
013813          WRITE W51286-REC FROM HEADER                                    
013814          MOVE IN-IDDC TO WS-IDDC                                         
013830       END-IF                                                             
013900       IF IN-KDPSLLOC = 11                                                
014000         PERFORM BA-COMPUTE-STOCKVALUE                                    
014100       END-IF                                                             
014200       PERFORM S01-READ-W51284-POST                                       
014300     END-PERFORM                                                          
014400     .                                                                    
014500     EJECT                                                                
014600                                                                          
014700 BA-COMPUTE-STOCKVALUE SECTION.                                           
014800     IF IN-STOCK-KURANS5 > +0                                             
014900     OR IN-STOCK-KURANS6 > +0                                             
015100       MOVE IN-IDARTNR         TO ROW-IDARTNR                             
015200       MOVE IN-PRAVCOST        TO ROW-PRAVCOST                            
015300       MOVE IN-STOCK-KURANS5   TO ROW-IN-STOCK5                           
015400       MOVE IN-STOCK-KURANS6   TO ROW-IN-STOCK6                           
015500       MOVE IN-KDERS           TO ROW-KDERS                               
015600       MOVE IN-KVLS-TOT        TO ROW-KVLS                                
015700       COMPUTE W-KUR5 ROUNDED = (IN-PRAVCOST * IN-STOCK-KURANS5)          
015800       COMPUTE W-KUR6 ROUNDED = (IN-PRAVCOST * IN-STOCK-KURANS6)          
015900       COMPUTE W-KUR56 = W-KUR5 + W-KUR6                                  
016000       MOVE W-KUR56            TO ROW-LVALUE                              
016400       IF W-KUR56 > 2000                                                  
016500          WRITE W51286-REC FROM ROW                                       
016600       END-IF                                                             
016800     END-IF                                                               
016900     .                                                                    
017000     EJECT                                                                
017100                                                                          
017200 Z-END    SECTION.                                                        
017300     CLOSE W51284                                                         
017400           W51286                                                         
017500                                                                          
017600     MOVE 'S'        TO POSTSUM-OPKOD                                     
017700     CALL POSTSUM USING POSTSUM-PARM                                      
017800     .                                                                    
017900     EJECT                                                                
018000                                                                          
018100 S01-READ-W51284-POST SECTION.                                            
018200     READ W51284 INTO INAREA                                              
018300     AT END                                                               
018400       MOVE JA TO EOF-W51284                                              
018500     NOT AT END                                                           
018600       MOVE W51284-TRANSID TO POSTSUM-TRANSID                             
018700       CALL POSTSUM USING POSTSUM-PARM                                    
018800     END-READ                                                             
018900     .                                                                    
018910 S02-SKRIV-DAP1 SECTION.                                                  
018920                                                                          
018930     MOVE ' ¤DAPW51286' TO W001-DAP                                       
018940     WRITE W51286-REC    FROM W001-DAP                                    
018950                                                                          
018960     MOVE SPACE TO W001-DAP                                               
018970     .                                                                    
018980                                                                          
018990 S03-SKRIV-DAP2 SECTION.                                                  
018991                                                                          
018992     STRING ' ¤DAP' IN-IDDC                                               
018993            DELIMITED BY SIZE INTO W001-DAP                               
018994     WRITE W51286-REC    FROM W001-DAP                                    
018995                                                                          
018996     MOVE SPACE TO W001-DAP                                               
018997     .                                                                    
019000     EJECT                                                                
