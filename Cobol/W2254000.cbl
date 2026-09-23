000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W2254000.                                                
000400*AUTHOR.         STEFAN KIHLBERG.                                         
000500*DATE-WRITTEN.   93/10/20.                                                
000600                                                                          
000700*    REMARKS                                                              
000800*                                                                         
000900*    FUNKTION:                                                            
001000*        LÄSER W91042.                                                    
001100*        SAMLAR ARTIKLAR MED RESTORDER.                                   
001200*        SKRIVER FIL MED W22541 MED ARTIKELNUMMER, ANSKAFFARE,            
001300*        RESTORDERSALDO OCH ANKOMSTSALDO.                                 
001400*                                                                         
001500*                                                                         
001600*    ABENDKODER:                                                          
001700*        U0016 -  . . . .                                                 
001800*        U1000 -  . . . .                                                 
001900*                                                                         
002000                                                                          
002100     SKIP3                                                                
002200 ENVIRONMENT DIVISION.                                                    
002300     SKIP2                                                                
002400 INPUT-OUTPUT SECTION.                                                    
002500                                                                          
002600 FILE-CONTROL.                                                            
002700     SKIP2                                                                
002800*          --- SAMTLIGA ARTIKLAR PÅ WDK601                                
002900     SELECT W91042                     ASSIGN TO W22540D1.                
003000*          --- ARTIKLAR MED RESTORDER                                     
003100     SELECT W22541                     ASSIGN TO W22540D2.                
003200     EJECT                                                                
003300 DATA DIVISION.                                                           
003400                                                                          
003500 FILE SECTION.                                                            
003600                                                                          
003700 FD  W91042                                                               
003800     RECORDING       F                                                    
003900     BLOCK CONTAINS  0.                                                   
004000     SKIP2                                                                
004100*01  POST -COPY W91042 -PRE W91042-   -L.                                 
004200 FD  W22541                                                               
004300     RECORDING       F                                                    
004400     BLOCK CONTAINS  0.                                                   
004500     SKIP2                                                                
004600*01  POST -COPY W22541 -PRE  W22541-  -L.                                 
004700     EJECT                                                                
004800 WORKING-STORAGE SECTION.                                                 
004900     SKIP2                                                                
005000                                                                          
005100*    -- CHECKED BY WY2000                                                 
005200 77  IDPGM                       PIC X(8)    VALUE 'W2254000'.            
005300 77  JA                          PIC X       VALUE 'J'.                   
005400 77  NEJ                         PIC X       VALUE 'N'.                   
005500                                                                          
005600 01  W91042-EOF-SW               PIC X       VALUE 'N'.                   
005700     88  END-OF-W91042                       VALUE 'J'.                   
005800     EJECT                                                                
005900                                                                          
006000 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
006100 01  FILLER REDEFINES DAGENS-DATUM.                                       
006200     03  DAGENS-DATUM-AAR        PIC 9(2).                                
006300     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
006400     03  DAGENS-DATUM-DAG        PIC 9(2).                                
006500     EJECT                                                                
006600                                                                          
006700*01  -COPY WWPRODSL                                                       
006800                                                                          
006900 01  DYNAMISKA-SUBPROGRAM.                                                
007000*                                                                         
007100     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
007200     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
007300     SKIP2                                                                
007400*    --- PARAMETRAR TILL ABEND                                            
007500                                                                          
007600 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
007700 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
007800     SKIP2                                                                
007900 01  FELTEXT.                                                             
008000     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
008100     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
008200     EJECT                                                                
008300*    --- PARAMETRAR TILL POSTSUM                                          
008400*                                                                         
008500*01  -COPY W0005   -PRE  POSTSUM-                                         
008600     EJECT                                                                
008700 01  W91042-AREA-START           PIC X(24)   VALUE                        
008800                                 'W91042-AREA-START  '.                   
008900     SKIP2                                                                
009000                                                                          
009100*01  AREA  -COPY W91042    -PRE W91042-                                   
009200     EJECT                                                                
009300 01  W22541-AREA-START           PIC X(24)   VALUE                        
009400                                 'W22541-AREA-START  '.                   
009500*01  AREA -COPY W22541     -PRE W22541-                                   
009600     EJECT                                                                
009700 PROCEDURE DIVISION.                                                      
009800                                                                          
009900     SKIP2                                                                
010000     PERFORM A-INIT                                                       
010100     PERFORM S01-LAES-W91042                                              
010200     PERFORM UNTIL END-OF-W91042                                          
010300        PERFORM B-BEHANDLA-ARTIKEL                                        
010400        PERFORM S01-LAES-W91042                                           
010500     END-PERFORM                                                          
010600     PERFORM Z-AVSLUTNING                                                 
010700                                                                          
010800     MOVE ZERO TO RETURN-CODE                                             
010900     GOBACK                                                               
011000     .                                                                    
011100     EJECT                                                                
011200                                                                          
011300                                                                          
011400                                                                          
011500 A-INIT SECTION.                                                          
011600                                                                          
011700     OPEN INPUT  W91042                                                   
011800          OUTPUT W22541                                                   
011900     SKIP2                                                                
012000     ACCEPT DAGENS-DATUM  FROM DATE                                       
012100     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
012200     .                                                                    
012300     EJECT                                                                
012400                                                                          
012500                                                                          
012600 B-BEHANDLA-ARTIKEL SECTION.                                              
012700                                                                          
012800     IF W91042-IDLEVNR NOT = '1441 ' AND NOT = '8888 '                    
012900             AND NOT = '10987' AND NOT = '19539' AND NOT = '16466'        
013000             AND NOT = 'BP2TW' AND NOT = 'BWLAA' AND NOT = 'BQ8VA'        
013100             AND NOT = 'S5S2A'                                            
013200        MOVE W91042-KDPRODSL              TO TEST-KDPRODSL                
013300        IF W91042-KDERS-UTG = 0 AND KDPRODSL-VOLVO-BIMA                   
013400           IF W91042-KVROS (1) > 0                                        
013500              MOVE W91042-IDARTNR         TO W22541-IDARTNR               
013600              MOVE W91042-IDANSK          TO W22541-IDANSK                
013700              MOVE W91042-KVROS (1)       TO W22541-KVROS-ART             
013800              MOVE W91042-KVAKS (1)       TO W22541-KVAKS-ART             
013900              COMPUTE W22541-KVLS-DISP-ART =                              
014000                   W91042-KVLS (1) -                                      
014100                   W91042-KVRESS (1)  -                                   
014200                   W91042-KVOKS (1)                                       
014300              IF W22541-KVLS-DISP-ART < ZERO                              
014400                 MOVE ZERO TO W22541-KVLS-DISP-ART                        
014500              END-IF                                                      
014600                                                                          
014700              PERFORM S11-SKRIV-W22541                                    
014800           END-IF                                                         
014900        END-IF                                                            
015000     END-IF                                                               
015100     .                                                                    
015200     EJECT                                                                
015300                                                                          
015400 Z-AVSLUTNING SECTION.                                                    
015500     CLOSE W91042                                                         
015600           W22541                                                         
015700     SKIP2                                                                
015800     MOVE 'S' TO POSTSUM-OPKOD                                            
015900     CALL POSTSUM USING POSTSUM-PARM                                      
016000     .                                                                    
016100     EJECT                                                                
016200                                                                          
016300                                                                          
016400 S01-LAES-W91042  SECTION.                                                
016500     READ W91042 INTO W91042-AREA                                         
016600     AT END                                                               
016700        SET END-OF-W91042 TO TRUE                                         
016800                                                                          
016900     NOT AT END                                                           
017000        MOVE 'W91042' TO POSTSUM-FDNAMN                                   
017100        MOVE 'W22540D1' TO POSTSUM-DDNAMN2                                
017200        CALL POSTSUM USING POSTSUM-PARM                                   
017300     END-READ                                                             
017400     .                                                                    
017500     EJECT                                                                
017600                                                                          
017700                                                                          
017800 S11-SKRIV-W22541 SECTION.                                                
017900     SKIP2                                                                
018000     WRITE W22541-POST FROM W22541-AREA                                   
018100                                                                          
018200     MOVE 'W22541' TO POSTSUM-FDNAMN                                      
018300     MOVE 'W22540D1' TO POSTSUM-DDNAMN2                                   
018400     CALL POSTSUM USING POSTSUM-PARM                                      
018500     .                                                                    
018600     EJECT                                                                
018700*S99-ABEND SECTION.                                                       
018800*    SKIP2                                                                
018900*    SKIP2                                                                
019000*    MOVE 'S' TO POSTSUM-OPKOD                                            
019100*    CALL POSTSUM USING POSTSUM-PARM                                      
019200*    CALL ABEND USING RKOD-ABEND-UTAN-DUMP                                
019300*    .                                                                    
019400     EJECT                                                                
