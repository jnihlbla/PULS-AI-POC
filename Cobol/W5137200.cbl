000100 ID DIVISION.                                                             
000200                                                                          
000300 PROGRAM-ID.     W5137200.                                                
000400 AUTHOR.         BO HAMMARIN, GDC-GROUP.                                  
000500 DATE-WRITTEN.   97/04/15.                                                
000600 DATE-COMPILED.                                                           
000700                                                                          
000800*                                                                         
000900*    FUNKTION:                                                            
001000*        PGM KOMPLETTERAR ARTIKELFIL FÖR ARTIKLAR KVAR ATT                
001100*        INVENTERA (W51374) MED INFO FRÅN LAGERFIL (W01172)               
001200*        OCH SKRIVER EN FIL (W5172) FÖR ALLA DC                           
001300*                                                                         
001400*    ABENDKODER:                                                          
001500*        U0016 -  UTAN DUMP                                               
001600*        U1000 -  MED DUMP                                                
001700                                                                          
001800     SKIP3                                                                
001900 ENVIRONMENT DIVISION.                                                    
002000     SKIP2                                                                
002100 INPUT-OUTPUT SECTION.                                                    
002200                                                                          
002300 FILE-CONTROL.                                                            
002400     SKIP2                                                                
002500*          --- ARTIKLAR KVAR ATT INVENTERA - SAMTLIGA DC                  
002600     SELECT W51374                     ASSIGN TO W51372D1.                
002700     SKIP2                                                                
002800*          --- DAGLIGT LAGERBAND                                          
002900     SELECT W01172                     ASSIGN TO W51372D2.                
003000     SKIP2                                                                
003100*          --- ARTIKLAR KVAR ATT INVENTERA - DC ALLA                      
003200     SELECT W51372                     ASSIGN TO W51372D3.                
003300     EJECT                                                                
003400 DATA DIVISION.                                                           
003500     SKIP3                                                                
003600 FILE SECTION.                                                            
003700     SKIP3                                                                
003800 FD  W51374                                                               
003900     LABEL RECORD    STANDARD                                             
004000     RECORDING       F                                                    
004100     BLOCK CONTAINS 0.                                                    
004200                                                                          
004300*01  POST -COPY W51371  -PRE  W51374-  -L.                                
004400     SKIP3                                                                
004500 FD  W01172                                                               
004600     RECORDING       F                                                    
004700     BLOCK CONTAINS  0.                                                   
004800                                                                          
004900*01  POST -COPY W011100 -PRE  W01172-   -L.                               
005000     SKIP3                                                                
005100 FD  W51372                                                               
005200     RECORDING       F                                                    
005300     BLOCK CONTAINS  0.                                                   
005400                                                                          
005500*01  POST -COPY W51371 -PRE  DCXX-  -L.                                   
005600     EJECT                                                                
005700 WORKING-STORAGE SECTION.                                                 
005800                                                                          
005900 77  IDPGM                       PIC X(8)    VALUE 'W5137200'.            
006000 77  JA                          PIC X       VALUE 'J'.                   
006100 77  NEJ                         PIC X       VALUE 'N'.                   
006200 77  W51374-EOF-SW               PIC X       VALUE 'N'.                   
006300     88  END-OF-W51374                       VALUE 'J'.                   
006400 77  W01172-EOF-SW               PIC X       VALUE 'N'.                   
006500     88  END-OF-W01172                       VALUE 'J'.                   
006800     EJECT                                                                
006900 01  DYNAMISKA-SUBPROGRAM.                                                
007000     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
007100     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
007200                                                                          
007300*    --- PARAMETRAR TILL ABEND                                            
007400                                                                          
007500 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
007600 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
007700 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
007800     EJECT                                                                
007900*    --- PARAMETRAR TILL POSTSUM                                          
008000*                                                                         
008100*01  -COPY W0005   -PRE  POSTSUM-                                         
008200     EJECT                                                                
008300 01  W51374-AREA-START           PIC X(24)   VALUE                        
008400                                 'W51374-AREA      '.                     
008500     SKIP2                                                                
008600                                                                          
008700*01  AREA -COPY W51371     -PRE W51374-                                   
008800     EJECT                                                                
008900 01  W01172-AREA-START           PIC X(24)   VALUE                        
009000                                 'W01172-AREA      '.                     
009100     SKIP2                                                                
009200                                                                          
009300*01  AREA -COPY W011100    -PRE W01172-                                   
009400     EJECT                                                                
009500 01  DCXX-AREA-START             PIC X(24)   VALUE 'DCXX-AREA'.           
009600                                                                          
009700*01  AREA -COPY W51371     -PRE DCXX-                                     
009800     EJECT                                                                
009900 PROCEDURE DIVISION.                                                      
010000                                                                          
010100     PERFORM A-INIT                                                       
010200                                                                          
010300     PERFORM S01-LAES-W01172                                              
010400     PERFORM S02-LAES-W51374                                              
010500                                                                          
010600     PERFORM UNTIL END-OF-W01172                                          
010700       PERFORM UNTIL END-OF-W51374 OR                                     
010800                     W51374-IDARTNR > W01172-IDARTNR                      
010900         IF W51374-IDARTNR = W01172-IDARTNR                               
011000           PERFORM B-SKAPA-UTPOST                                         
011100         END-IF                                                           
011200         PERFORM S02-LAES-W51374                                          
011300       END-PERFORM                                                        
011400       PERFORM S01-LAES-W01172                                            
011500     END-PERFORM                                                          
011600                                                                          
011700     PERFORM Z-FINIT                                                      
011800                                                                          
011900     MOVE ZERO TO RETURN-CODE                                             
012000     GOBACK                                                               
012100     .                                                                    
012200     EJECT                                                                
012300 A-INIT SECTION.                                                          
012400                                                                          
012500     OPEN INPUT  W51374                                                   
012600                 W01172                                                   
012700     OPEN OUTPUT W51372                                                   
012800                                                                          
012900     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
013000     .                                                                    
013100     EJECT                                                                
013200 B-SKAPA-UTPOST SECTION.                                                  
013300                                                                          
013400     MOVE W01172-KDVVKL            TO W51374-KDVVKL                       
013500     MOVE W01172-KDPRODSL          TO W51374-KDPRODSL                     
013700     MOVE W01172-IDFKNGRP          TO W51374-IDFKNGRP                     
013800     MOVE W51374-W51371            TO DCXX-W51371                         
013910     PERFORM S11-SKRIV-DCXX                                               
014000     .                                                                    
014100     EJECT                                                                
014200 Z-FINIT SECTION.                                                         
014300     CLOSE W51374                                                         
014400           W01172                                                         
014500           W51372                                                         
014600                                                                          
014700     MOVE 'S' TO POSTSUM-OPKOD                                            
014800     CALL POSTSUM USING POSTSUM-PARM                                      
014900     .                                                                    
015000     EJECT                                                                
015100 S01-LAES-W01172  SECTION.                                                
015200                                                                          
015300     READ W01172 INTO W01172-AREA                                         
015400     AT END                                                               
015500        MOVE +99999999  TO W01172-IDARTNR                                 
015600        SET END-OF-W01172 TO TRUE                                         
015700                                                                          
015800     NOT AT END                                                           
015900        MOVE 'LB'       TO POSTSUM-TRANSTYP                               
016000        MOVE 'W01172'   TO POSTSUM-FDNAMN                                 
016100        MOVE 'W51372D2' TO POSTSUM-DDNAMN2                                
016200        CALL POSTSUM USING POSTSUM-PARM                                   
016300     END-READ                                                             
016400     .                                                                    
016500     EJECT                                                                
016600 S02-LAES-W51374  SECTION.                                                
016700                                                                          
016800     READ W51374 INTO W51374-AREA                                         
016900     AT END                                                               
017000        MOVE +99999999   TO W51374-IDARTNR                                
017100        SET END-OF-W51374 TO TRUE                                         
017200                                                                          
017300     NOT AT END                                                           
017400        MOVE W51374-IDDC TO POSTSUM-TRANSTYP                              
017500        MOVE 'W51374'    TO POSTSUM-FDNAMN                                
017600        MOVE 'W51372D1'  TO POSTSUM-DDNAMN2                               
017700        CALL POSTSUM USING POSTSUM-PARM                                   
017800     END-READ                                                             
017900     .                                                                    
018000     EJECT                                                                
018100 S11-SKRIV-DCXX   SECTION.                                                
018200                                                                          
018300     WRITE DCXX-POST FROM DCXX-AREA                                       
018400                                                                          
018500     MOVE DCXX-IDDC  TO POSTSUM-TRANSTYP                                  
018600     MOVE 'W51372'   TO POSTSUM-FDNAMN                                    
018700     MOVE 'W51372D3' TO POSTSUM-DDNAMN2                                   
018800     CALL POSTSUM USING POSTSUM-PARM                                      
018900     .                                                                    
