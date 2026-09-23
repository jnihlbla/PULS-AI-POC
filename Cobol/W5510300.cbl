000100 ID DIVISION.                                                             
000200                                                                          
000300 PROGRAM-ID.             W5510300.                                        
000400 AUTHOR.                 INGER NILSSON.                                   
000500 DATE-WRITTEN.           OKT 1991.                                        
000600                                                                          
000700     REMARKS.                                                             
000800*            PROGRAMMET BYTTE NAMN 23/5 1995 FRÅN W4240300                
000900*                                                                         
001000*    FUNKTION:                                                            
001100*      INFIL    W55103D1  ARTIKELINFO FRÅN SPIS.                          
001200*                                                                         
001300*      UTFIL    W55103D2  FÖR PRISSÄTTNING AV FÖRPACKN.KOD                
001400*               W55103D4  FÖR PRISSÄTTNING PÅ FÖRPACKN.TYP                
001500     EJECT                                                                
001600 ENVIRONMENT DIVISION.                                                    
001700                                                                          
001800 INPUT-OUTPUT SECTION.                                                    
001900                                                                          
002000 FILE-CONTROL.                                                            
002100     SELECT W55159   ASSIGN TO W55103D1.                                  
002200     SELECT W55111   ASSIGN TO W55103D2.                                  
002300     SELECT W55113   ASSIGN TO W55103D4.                                  
002400     EJECT                                                                
002500 DATA DIVISION.                                                           
002600                                                                          
002700 FILE SECTION.                                                            
002800     SKIP3                                                                
002900 FD  W55159                                                               
003000     RECORDING F                                                          
003100     BLOCK CONTAINS 0.                                                    
003200*01  LB-W55159 -COPY W55159   -L                                          
003300     SKIP2                                                                
003400 FD  W55111                                                               
003500     RECORDING F                                                          
003600     BLOCK CONTAINS 0.                                                    
003700*01  UTPOST-11 -COPY W55159   -L                                          
003800     SKIP2                                                                
003900 FD  W55113                                                               
004000     RECORDING F                                                          
004100     BLOCK CONTAINS 0.                                                    
004200*01  UTPOST-13 -COPY W55159   -L                                          
004300     EJECT                                                                
004400 WORKING-STORAGE SECTION.                                                 
004500                                                                          
004600                                                                          
004700*    -- CHECKED BY WY2000                                                 
004800 77  IDPGM                   PIC  X(08)  VALUE 'W5510300'.                
004900                                                                          
005000 77  JA                      PIC  X      VALUE 'J'.                       
005100 77  NEJ                     PIC  X      VALUE 'N'.                       
005200 77  INFIL-SLUT              PIC  X      VALUE 'N'.                       
005300                                                                          
005400 01  DYNAMISKA-SUBPROGRAM.                                                
005500   03  ABEND                 PIC X(8)    VALUE 'ABEND   '.                
005600   03  POSTSUM               PIC X(8)    VALUE 'POSTSUM '.                
005700     SKIP2                                                                
005800* 01 -COPY W0005            -PRE POSTSUM-                                 
005900     EJECT                                                                
006000*    ----  AREA FÖR LAGERBANDS-POST                                       
006100*01  -COPY W55159    -PRE IN-                                             
006200     EJECT                                                                
006300*    ----  AREA FÖR UTPOST PRISSÄTTNING                                   
006400*01  -COPY W55159    -PRE UT1-                                            
006500     EJECT                                                                
006600 PROCEDURE DIVISION.                                                      
006700                                                                          
006800     PERFORM A-INIT                                                       
006900                                                                          
007000     PERFORM S01-LAS-ARTIKELFIL                                           
007100     PERFORM UNTIL INFIL-SLUT = JA                                        
007200                                                                          
007300       IF IN-BEFT = 00 OR 01 OR 03 OR 04 OR 05 OR                         
007400                    06 OR 07 OR 08 OR 09 OR 41 OR 42 OR                   
007500                    43 OR 44 OR 45 OR 47 OR 48 OR                         
007600                    98 OR 99                                              
007700***  MÅLERIARTIKLAR SKALL EJ VARA MED                                     
007800          CONTINUE                                                        
007900       ELSE                                                               
008000         IF IN-KDERS        > 20 AND                                      
008100            IN-KVBEHOVAR    = +0                                          
008200              CONTINUE                                                    
008300         ELSE                                                             
008400           PERFORM B-BEHANDLING                                           
008500         END-IF                                                           
008600       END-IF                                                             
008700       PERFORM S01-LAS-ARTIKELFIL                                         
008800     END-PERFORM                                                          
008900                                                                          
009000     PERFORM Z-FINIT                                                      
009100                                                                          
009200     MOVE ZERO            TO RETURN-CODE                                  
009300     GOBACK                                                               
009400     .                                                                    
009500     SKIP3                                                                
009600 A-INIT SECTION.                                                          
009700                                                                          
009800     OPEN  INPUT W55159                                                   
009900          OUTPUT W55111                                                   
010000                 W55113                                                   
010100                                                                          
010200     MOVE IDPGM           TO POSTSUM-PROGNAMN                             
010300     .                                                                    
010400     EJECT                                                                
010500 B-BEHANDLING SECTION.                                                    
010600                                                                          
010700     MOVE IN-IDARTNR                TO UT1-IDARTNR                        
010800     MOVE IN-IDLEVNR                TO UT1-IDLEVNR                        
010900     MOVE +1                        TO UT1-KDGK                           
011000     MOVE IN-FLIART                 TO UT1-FLIART                         
011100     MOVE IN-KVQPACK-0              TO UT1-KVQPACK-0                      
011200     MOVE IN-KVQPACK-1              TO UT1-KVQPACK-1                      
011300     MOVE IN-KVQPACK-2              TO UT1-KVQPACK-2                      
011400     MOVE IN-PRARTSJK               TO UT1-PRARTSJK                       
011500     MOVE IN-PRARTSTD               TO UT1-PRARTSTD                       
011600     MOVE IN-TIFINLV                TO UT1-TIFINLV                        
011700     MOVE IN-PRDIRLON               TO UT1-PRDIRLON                       
011800     MOVE IN-PRDMTRL                TO UT1-PRDMTRL                        
011900     MOVE IN-PROVRPAL               TO UT1-PROVRPAL                       
012000     MOVE IN-KDVTH                  TO UT1-KDVTH                          
012100     MOVE IN-IDFKNGRP               TO UT1-IDFKNGRP                       
012200     MOVE IN-KDPRODSL               TO UT1-KDPRODSL                       
012300     MOVE IN-KDFARLIG               TO UT1-KDFARLIG                       
012400     MOVE ZERO                      TO UT1-KVLASTB                        
012500     MOVE IN-KDERS                  TO UT1-KDERS                          
012600     MOVE IN-KVLS                   TO UT1-KVLS                           
012700     MOVE IN-KVBEHOVAR              TO UT1-KVBEHOVAR                      
012800     MOVE IN-KDFORP                 TO UT1-KDFORP                         
012900     MOVE IN-BEFT                   TO UT1-BEFT                           
013000     MOVE IN-IDARTNR-EMBQ0          TO UT1-IDARTNR-EMBQ0                  
013100     MOVE IN-IDARTNR-EMBQ1          TO UT1-IDARTNR-EMBQ1                  
013200     MOVE IN-IDARTNR-EMBQ2          TO UT1-IDARTNR-EMBQ2                  
013300     MOVE IN-IDARTNR-EMBQ3          TO UT1-IDARTNR-EMBQ3                  
013400     MOVE IN-IDARTNR-EMBQ4          TO UT1-IDARTNR-EMBQ4                  
013500     MOVE IN-ADLAGOMR               TO UT1-ADLAGOMR                       
013600     MOVE IN-ADGANG                 TO UT1-ADGANG                         
013700     MOVE IN-ADPLATS                TO UT1-ADPLATS                        
013800     IF (IN-KDGK = +0 OR +1 OR +2 OR +3) AND                              
013900        (UT1-KDFORPGP > 0 AND UT1-KDFORPUF > 0)                           
014000                                                                          
014100       WRITE UTPOST-11 FROM UT1-W55159                                    
014200                                                                          
014300       MOVE SPACE                   TO POSTSUM-TRANSTYP                   
014400       MOVE 'W55111'                TO POSTSUM-FDNAMN                     
014500       MOVE 'W55103D2'              TO POSTSUM-DDNAMN2                    
014600       CALL POSTSUM USING POSTSUM-PARM                                    
014700     ELSE                                                                 
014800       WRITE UTPOST-13 FROM UT1-W55159                                    
014900                                                                          
015000       MOVE SPACE                   TO POSTSUM-TRANSTYP                   
015100       MOVE 'W55113'                TO POSTSUM-FDNAMN                     
015200       MOVE 'W55103D4'              TO POSTSUM-DDNAMN2                    
015300       CALL POSTSUM USING POSTSUM-PARM                                    
015400     END-IF                                                               
015500     .                                                                    
015600     EJECT                                                                
015700 Z-FINIT SECTION.                                                         
015800                                                                          
015900     CLOSE W55159                                                         
016000           W55111                                                         
016100           W55113                                                         
016200                                                                          
016300     MOVE 'S'                 TO POSTSUM-OPKOD                            
016400     CALL POSTSUM USING POSTSUM-PARM                                      
016500     .                                                                    
016600     SKIP3                                                                
016700 S01-LAS-ARTIKELFIL SECTION.                                              
016800                                                                          
016900     READ W55159   INTO IN-W55159                                         
017000     AT END                                                               
017100       MOVE JA TO INFIL-SLUT                                              
017200     NOT AT END                                                           
017300       MOVE 'SPIS'           TO POSTSUM-TRANSTYP                          
017400       MOVE 'W55159'         TO POSTSUM-FDNAMN                            
017500       MOVE 'W55103D1'       TO POSTSUM-DDNAMN2                           
017600       CALL POSTSUM USING POSTSUM-PARM                                    
017700     END-READ                                                             
017800     .                                                                    
