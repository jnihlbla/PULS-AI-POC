000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W2336600.                                                
000400*AUTHOR.         STEFAN KIHLBERG.                                         
000500*DATE-WRITTEN.   94/05/31.                                                
000600                                                                          
000700*                                                                         
000800*    FUNKTION:                                                            
000900*        LÄSER_ARTIKELFIL W91042 OCH FIL MED EMBLEM W91028                
001000*        OCH SKAPAR FILEN W23367 MED EMLEM FÖR CARPAC-ARTIKLAR            
001100*                                                                         
001200*                                                                         
001300*    ABENDKODER:                                                          
001400*        U0016 -  . . . .                                                 
001500*        U1000 -  . . . .                                                 
001600*                                                                         
001700                                                                          
001800     SKIP3                                                                
001900 ENVIRONMENT DIVISION.                                                    
002000     SKIP2                                                                
002100 INPUT-OUTPUT SECTION.                                                    
002200                                                                          
002300 FILE-CONTROL.                                                            
002400     SKIP2                                                                
002500*          --- ARTIKELFIL                                                 
002600     SELECT W91042                     ASSIGN TO W23366D1.                
002700     SKIP2                                                                
002800*          --- FIL MED EMBLEM                                             
002900     SELECT W91028                     ASSIGN TO W23366D2.                
003000     SKIP2                                                                
003100*          --- EMBLEM PER CARPAC-ARTIKEL                                  
003200     SELECT W23367                     ASSIGN TO W23366D3.                
003300     EJECT                                                                
003400 DATA DIVISION.                                                           
003500     SKIP3                                                                
003600 FILE SECTION.                                                            
003700     SKIP3                                                                
003800 FD  W91042                                                               
003900     RECORDING       F                                                    
004000     BLOCK CONTAINS  0.                                                   
004100     SKIP2                                                                
004200*01  -COPY W91042      -L.                                                
004300     SKIP3                                                                
004400 FD  W91028                                                               
004500     RECORDING       V                                                    
004600     BLOCK CONTAINS  0.                                                   
004700     SKIP2                                                                
004800*01  -COPY W91028L1      -L.                                              
004900     SKIP3                                                                
005000 FD  W23367                                                               
005100     RECORDING       F                                                    
005200     BLOCK CONTAINS  0.                                                   
005300     SKIP2                                                                
005400*01  POST -COPY W23367 -PRE  W23367-  -L.                                 
005500     EJECT                                                                
005600 WORKING-STORAGE SECTION.                                                 
005700     SKIP2                                                                
005800                                                                          
005900*    -- CHECKED BY WY2000                                                 
006000 77  IDPGM                       PIC X(8)    VALUE 'W2336600'.            
006100 77  JA                          PIC X       VALUE 'J'.                   
006200 77  NEJ                         PIC X       VALUE 'N'.                   
006300                                                                          
006400 77  W91042-EOF-SW               PIC X       VALUE 'N'.                   
006500     88  END-OF-W91042                       VALUE 'J'.                   
006600                                                                          
006700 77  W91028-EOF-SW               PIC X       VALUE 'N'.                   
006800     88  END-OF-W91028                       VALUE 'J'.                   
006900     EJECT                                                                
007000 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
007100 01  FILLER REDEFINES DAGENS-DATUM.                                       
007200     03  DAGENS-DATUM-AAR        PIC 9(2).                                
007300     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
007400     03  DAGENS-DATUM-DAG        PIC 9(2).                                
007500     EJECT                                                                
007600                                                                          
007700*01  -COPY WWPRODSL                                                       
007800                                                                          
007900 01  DYNAMISKA-SUBPROGRAM.                                                
008000*                                                                         
008100     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
008200     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
008300     SKIP2                                                                
008400*    --- PARAMETRAR TILL ABEND                                            
008500                                                                          
008600 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
008700 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
008800     SKIP2                                                                
008900 01  FELTEXT.                                                             
009000     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
009100     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
009200     EJECT                                                                
009300 01  ARBETSAREOR.                                                         
009400     03  SPAR-IDARTNR            PIC S9(9)   VALUE ZERO COMP-3.           
009500     03  IX                      PIC S9(9)   VALUE ZERO COMP-3.           
009600*    --- PARAMETRAR TILL POSTSUM                                          
009700*                                                                         
009800*01  -COPY W0005   -PRE  POSTSUM-                                         
009900     EJECT                                                                
010000 01  W91042-AREA-START           PIC X(24)   VALUE                        
010100                                 'W91042-AREA-START  '.                   
010200     SKIP2                                                                
010300                                                                          
010400*01  AREA -COPY W91042     -PRE W91042-                                   
010500     EJECT                                                                
010600 01  W91028-AREA-START           PIC X(24)   VALUE                        
010700                                 'W91028-AREA-START  '.                   
010800     SKIP2                                                                
010900                                                                          
011000*01  AREA -COPY W91028L1     -PRE W91028-                                 
011100     EJECT                                                                
011200 01  W23367-AREA-START           PIC X(24)   VALUE                        
011300                                 'W23367-AREA-START  '.                   
011400     SKIP2                                                                
011500                                                                          
011600*01  AREA -COPY W23367     -PRE W23367-                                   
011700     EJECT                                                                
011800 PROCEDURE DIVISION.                                                      
011900     SKIP2                                                                
012000                                                                          
012100     PERFORM A-INIT                                                       
012200     PERFORM S01-LAES-W91042                                              
012300     PERFORM S02-LAES-W91028                                              
012400     PERFORM UNTIL END-OF-W91042 OR END-OF-W91028                         
012500        IF W91028-IDARTNR = W91042-IDARTNR                                
012600           MOVE W91042-KDPRODSL  TO TEST-KDPRODSL                         
012700           IF KDPRODSL-VCBV                                               
012800              PERFORM B-NOLLSTALL                                         
012900              PERFORM C-FLYTTA-EMBLEM                                     
013000              PERFORM S01-LAES-W91042                                     
013100           ELSE                                                           
013200              PERFORM S02-LAES-W91028                                     
013300              PERFORM S01-LAES-W91042                                     
013400           END-IF                                                         
013500        ELSE                                                              
013600           IF W91028-IDARTNR > W91042-IDARTNR                             
013700              PERFORM S01-LAES-W91042                                     
013800           ELSE                                                           
013900              PERFORM S02-LAES-W91028                                     
014000           END-IF                                                         
014100        END-IF                                                            
014200     END-PERFORM                                                          
014300     PERFORM Z-FINIT                                                      
014400                                                                          
014500     MOVE ZERO TO RETURN-CODE                                             
014600     GOBACK                                                               
014700     .                                                                    
014800     EJECT                                                                
014900 A-INIT SECTION.                                                          
015000                                                                          
015100     OPEN INPUT  W91042                                                   
015200                 W91028                                                   
015300                                                                          
015400     OPEN OUTPUT W23367                                                   
015500     SKIP2                                                                
015600     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
015700     .                                                                    
015800     EJECT                                                                
015900 B-NOLLSTALL SECTION.                                                     
016000                                                                          
016100     MOVE SPACE TO W23367-AREA                                            
016200     .                                                                    
016300     EJECT                                                                
016400 C-FLYTTA-EMBLEM SECTION.                                                 
016500                                                                          
016600     MOVE 'VPCA'              TO W23367-FZ2RECT                           
016700     IF W91042-IDARTNR < 10000000                                         
016800        MOVE W91042-IDARTNR      TO W23367-IDARTNR                        
016900     ELSE                                                                 
017000        MOVE W91042-IDARTNR      TO W23367-IDARTNR-8                      
017100     END-IF                                                               
017200                                                                          
017300     MOVE W91028-IDARTNR TO SPAR-IDARTNR                                  
017400     PERFORM UNTIL END-OF-W91028                     OR                   
017500                   W91028-IDARTNR NOT = SPAR-IDARTNR                      
017600        MOVE W91028-BEEMBLEM    TO W23367-FZ2CAT                          
017700        PERFORM S11-SKRIV-W23367                                          
017800                                                                          
017900        PERFORM S02-LAES-W91028                                           
018000     END-PERFORM                                                          
018100     .                                                                    
018200     EJECT                                                                
018300 Z-FINIT SECTION.                                                         
018400     CLOSE W91042                                                         
018500           W91028                                                         
018600           W23367                                                         
018700     SKIP2                                                                
018800     MOVE 'S' TO POSTSUM-OPKOD                                            
018900     CALL POSTSUM USING POSTSUM-PARM                                      
019000     .                                                                    
019100     EJECT                                                                
019200 S01-LAES-W91042  SECTION.                                                
019300     SKIP2                                                                
019400     READ W91042 INTO W91042-AREA                                         
019500     AT END                                                               
019600        SET END-OF-W91042 TO TRUE                                         
019700                                                                          
019800     NOT AT END                                                           
019900        MOVE 'W91042' TO POSTSUM-FDNAMN                                   
020000        MOVE 'W23366D1' TO POSTSUM-DDNAMN2                                
020100        CALL POSTSUM USING POSTSUM-PARM                                   
020200     END-READ                                                             
020300     .                                                                    
020400     EJECT                                                                
020500 S02-LAES-W91028  SECTION.                                                
020600     SKIP2                                                                
020700     READ W91028 INTO W91028-AREA                                         
020800     AT END                                                               
020900        SET END-OF-W91028 TO TRUE                                         
021000                                                                          
021100     NOT AT END                                                           
021200        MOVE 'W91028' TO POSTSUM-FDNAMN                                   
021300        MOVE 'W23366D2' TO POSTSUM-DDNAMN2                                
021400        CALL POSTSUM USING POSTSUM-PARM                                   
021500     END-READ                                                             
021600     .                                                                    
021700     EJECT                                                                
021800 S11-SKRIV-W23367 SECTION.                                                
021900     SKIP2                                                                
022000     WRITE W23367-POST FROM W23367-AREA                                   
022100                                                                          
022200     MOVE 'W23367' TO POSTSUM-FDNAMN                                      
022300     MOVE 'W23366D3' TO POSTSUM-DDNAMN2                                   
022400     CALL POSTSUM USING POSTSUM-PARM                                      
022500     .                                                                    
022600     EJECT                                                                
