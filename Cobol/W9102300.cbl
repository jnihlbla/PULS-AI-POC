000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W9102300.                                                
000300 AUTHOR.         HELGEGREN PER-ANDERS.                                    
000400 DATE-WRITTEN.   11/11/25.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*                                                                         
000800*    FUNKTION:                                                            
000900*        LÄSER LAGERBAND                                                  
001000*        ERSÄTTNINGAR TILL ERC                                            
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
002500*          --- LAGERBAND                                                  
002600     SELECT W01160                     ASSIGN TO W91023D1.                
002700     SKIP2                                                                
002800*          --- ERSÄTTNINGAR TILL ERC                                      
002900     SELECT W91024                     ASSIGN TO W91023D2.                
003000     EJECT                                                                
003100 DATA DIVISION.                                                           
003200     SKIP3                                                                
003300 FILE SECTION.                                                            
003400     SKIP3                                                                
003500 FD  W01160                                                               
003600     RECORDING       F                                                    
003700     BLOCK CONTAINS  0.                                                   
003800                                                                          
003900*01  -COPY W01160      -L.                                                
004000     SKIP3                                                                
004100 FD  W91024                                                               
004200     RECORDING       F                                                    
004300     BLOCK CONTAINS  0.                                                   
004400                                                                          
004500*01  POST -COPY W91024 -PRE  UT-  -L.                                     
004600     EJECT                                                                
004700 WORKING-STORAGE SECTION.                                                 
004800                                                                          
004900 77  IDPGM                       PIC X(8)    VALUE 'W9102300'.            
005000 77  JA                          PIC X       VALUE 'J'.                   
005100 77  NEJ                         PIC X       VALUE 'N'.                   
005200                                                                          
005300 77  W01160-EOF-SW               PIC X       VALUE 'N'.                   
005400     88  END-OF-W01160                       VALUE 'J'.                   
005500     EJECT                                                                
005600 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
005700 01  FILLER REDEFINES DAGENS-DATUM.                                       
005800     03  DAGENS-DATUM-AAR        PIC 9(2).                                
005900     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
006000     03  DAGENS-DATUM-DAG        PIC 9(2).                                
006100     EJECT                                                                
006200*01  -COPY WWPRODSL                                                       
006300     EJECT                                                                
006400 01  DYNAMISKA-SUBPROGRAM.                                                
006500*                                                                         
006600     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
006700     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
006800     SKIP2                                                                
006900*    --- PARAMETRAR TILL ABEND                                            
007000                                                                          
007100 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
007200 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
007300 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
007400     SKIP2                                                                
007500 01  FELTEXT.                                                             
007600     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
007700     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
007800     EJECT                                                                
007900*    --- PARAMETRAR TILL POSTSUM                                          
008000*                                                                         
008100*01  -COPY W0005   -PRE  POSTSUM-                                         
008200     EJECT                                                                
008300 01  IN-AREA-START               PIC X(24)   VALUE                        
008400                                 'IN-AREA-START  '.                       
008500     SKIP2                                                                
008600                                                                          
008700*01  AREA -COPY W01160     -PRE IN-                                       
008800     EJECT                                                                
008900 01  UT-AREA-START               PIC X(24)   VALUE                        
009000                                 'UT-AREA-START  '.                       
009100     SKIP2                                                                
009200                                                                          
009300*01  AREA -COPY W91024     -PRE UT-                                       
009400     EJECT                                                                
009500 PROCEDURE DIVISION.                                                      
009600 MAIN SECTION.                                                            
009700     SKIP2                                                                
009800                                                                          
009900     PERFORM A-INIT                                                       
010000     PERFORM S01-LAES-W01160                                              
010100     PERFORM UNTIL END-OF-W01160                                          
010200       MOVE IN-CLAG-KDPRODSL     TO TEST-KDPRODSL                         
010300       IF KDPRODSL-PARTS-BYTES OR KDPRODSL-VCBV-PARTS OR                  
010400          KDPRODSL-SERVICES                                               
010500          IF (IN-CLAG-KDERS = 21 OR 22 OR 23 OR 27)                       
010600          OR (IN-CLAG-KDERS-UTG = 21 OR 22 OR 23 OR 27)                   
010700             IF (IN-CLAG-TIERSDAT < 50000 AND                             
010800                 IN-CLAG-TIERSDAT > 03000) OR                             
010900                (IN-CLAG-TIERSDAT = ZERO)                                 
011000                MOVE IN-CLAG-IDARTNR       TO UT-IDARTNR                  
011100                IF IN-CLAG-KDERS > ZERO                                   
011200                   MOVE IN-CLAG-KDERS      TO UT-KDERS                    
011300                ELSE                                                      
011400                   MOVE IN-CLAG-KDERS-UTG  TO UT-KDERS                    
011500                END-IF                                                    
011600                MOVE IN-CLAG-TIERSDAT      TO UT-TIERSDAT                 
011700                MOVE IN-CLAG-FLERS         TO UT-FLERS                    
011800                PERFORM S11-SKRIV-W91024                                  
011900             END-IF                                                       
012000          END-IF                                                          
012100       END-IF                                                             
012200       PERFORM S01-LAES-W01160                                            
012300     END-PERFORM                                                          
012400                                                                          
012500                                                                          
012600     PERFORM Z-FINIT                                                      
012700                                                                          
012800     MOVE ZERO TO RETURN-CODE                                             
012900     GOBACK                                                               
013000     .                                                                    
013100     EJECT                                                                
013200 A-INIT SECTION.                                                          
013300                                                                          
013400     OPEN INPUT  W01160                                                   
013500                                                                          
013600     OPEN OUTPUT W91024                                                   
013700     SKIP2                                                                
013800     ACCEPT DAGENS-DATUM  FROM DATE                                       
013900     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
014000     .                                                                    
014100     EJECT                                                                
014200 Z-FINIT SECTION.                                                         
014300     CLOSE W01160                                                         
014400           W91024                                                         
014500     SKIP2                                                                
014600     MOVE 'S' TO POSTSUM-OPKOD                                            
014700     CALL POSTSUM USING POSTSUM-PARM                                      
014800     .                                                                    
014900     EJECT                                                                
015000 S01-LAES-W01160  SECTION.                                                
015100     READ W01160 INTO IN-AREA                                             
015200     AT END                                                               
015300        MOVE HIGH-VALUE TO IN-AREA                                        
015400        SET END-OF-W01160 TO TRUE                                         
015500                                                                          
015600     NOT AT END                                                           
015700        MOVE 'W01160'   TO POSTSUM-FDNAMN                                 
015800        MOVE 'W91023D1' TO POSTSUM-DDNAMN2                                
015900        MOVE 'IN'       TO POSTSUM-TRANSTYP                               
016000        CALL POSTSUM USING POSTSUM-PARM                                   
016100     END-READ                                                             
016200     .                                                                    
016300     EJECT                                                                
016400 S11-SKRIV-W91024 SECTION.                                                
016500                                                                          
016600     WRITE UT-POST FROM UT-AREA                                           
016700                                                                          
016800     MOVE 'UT'       TO POSTSUM-TRANSTYP                                  
016900     MOVE 'W91024'   TO POSTSUM-FDNAMN                                    
017000     MOVE 'W91023D2' TO POSTSUM-DDNAMN2                                   
017100     CALL POSTSUM USING POSTSUM-PARM                                      
017200     .                                                                    
017300     EJECT                                                                
017400 S99-ABEND SECTION.                                                       
017500                                                                          
017600     SKIP2                                                                
017700     MOVE 'S' TO POSTSUM-OPKOD                                            
017800     CALL POSTSUM USING POSTSUM-PARM                                      
017900     CALL ABEND USING RKOD-ABEND                                          
018000     .                                                                    
