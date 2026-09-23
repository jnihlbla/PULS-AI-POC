000100 ID DIVISION.                                                             
000200 PROGRAM-ID.     W2716400.                                                
000300 AUTHOR.         STEFAN ANDREASSON, FRONTEC.                              
000400 DATE-WRITTEN.   MAJ 1997.                                                
000500 DATE-COMPILED.                                                           
000600                                                                          
000700*    FUNKTION:                                                            
000800*                                                                         
000900*                PROGRAMMET LÄSER RESTORDER OCH SUMMERAR                  
001000*                ORDERRADER PER DC/ARTIKEL FÖR ATT SENARE                 
001100*                SKRIVA LISTAN 'TOPP 100 RESTORDER' PER DC                
001200*                I ORDERLINES ORDNING                                     
001300*                                                                         
001400*                                                                         
001500*                                                                         
001600                                                                          
001700     SKIP3                                                                
001800 ENVIRONMENT DIVISION.                                                    
001900     SKIP2                                                                
002000 INPUT-OUTPUT SECTION.                                                    
002100                                                                          
002200 FILE-CONTROL.                                                            
002300     SKIP2                                                                
002400     SELECT W27164IN                   ASSIGN TO W27164D1.                
002500*                                                                         
002600     SELECT W27164UT                   ASSIGN TO W27164D2.                
002700     SKIP2                                                                
002800     EJECT                                                                
002900 DATA DIVISION.                                                           
003000     SKIP3                                                                
003100 FILE SECTION.                                                            
003200     SKIP3                                                                
003300                                                                          
003400 FD  W27164IN                                                             
003500     RECORD CONTAINS 76 CHARACTERS                                        
003600     LABEL RECORD STANDARD                                                
003700     BLOCK CONTAINS  0.                                                   
003800                                                                          
003900 01  IN-POST                 PIC X(76).                                   
004000                                                                          
004100     SKIP3                                                                
004200                                                                          
004300 FD  W27164UT                                                             
004400     RECORD CONTAINS 80 CHARACTERS                                        
004500     LABEL RECORD STANDARD                                                
004600     BLOCK CONTAINS  0.                                                   
004700                                                                          
004800 01  UT-POST                 PIC X(80).                                   
004900                                                                          
005000*                                                                         
005100     EJECT                                                                
005200 WORKING-STORAGE SECTION.                                                 
005300*    -- CHECKED BY WY2000                                                 
005400 77  IDPGM                       PIC X(8)    VALUE 'W2716400'.            
005500 77  JA                          PIC X       VALUE 'J'.                   
005600                                                                          
005700 01  WS.                                                                  
005800     03  WS-IDARTNR              PIC 9(9)    VALUE ZERO.                  
005900     03  WS-ANTAL-ORDRAD         PIC 9(7)    VALUE ZERO.                  
006000     03  WS-ANTAL-QTY            PIC 9(7)    VALUE ZERO.                  
006100                                                                          
006200                                                                          
006300 77  W27164-EOF-SW               PIC X       VALUE 'N'.                   
006400     88  END-OF-W27164                       VALUE 'J'.                   
006500                                                                          
006600 01  IN-AREA-START               PIC X(24)   VALUE                        
006700                                             'IN-AREA-START'.             
006800     SKIP2                                                                
006900*01  AREA -COPY W27161     -PRE IN-                                       
007000                                                                          
007100     EJECT                                                                
007200 01  UT-AREA-START              PIC X(24)   VALUE                         
007300                                 'UT-AREA-START  '.                       
007400     SKIP2                                                                
007500*01  AREA -COPY W27162     -PRE UT-                                       
007600                                                                          
007700                                                                          
007800     EJECT                                                                
007900 PROCEDURE DIVISION.                                                      
008000                                                                          
008100     PERFORM A-INIT                                                       
008200                                                                          
008300     PERFORM S01-LAES-W27164IN                                            
008400                                                                          
008500     PERFORM UNTIL END-OF-W27164                                          
008600                                                                          
008700       MOVE ZERO             TO WS-ANTAL-ORDRAD                           
008800                                WS-ANTAL-QTY                              
008900                                UT-DARODAT                                
009000       MOVE IN-IDDC          TO UT-IDDC                                   
009100       MOVE IN-IDLANDX2      TO UT-IDLANDX2                               
009200       MOVE IN-IDARTNR       TO WS-IDARTNR                                
009300                                UT-IDARTNR                                
009400       MOVE IN-BEART         TO UT-BEART                                  
009500       MOVE IN-IDFKNGRP      TO UT-IDFKNGRP                               
009600       MOVE IN-FREEZECODE    TO UT-FREEZECODE                             
009700       MOVE IN-IDPERSON-BUY  TO UT-IDPERSON-BUY                           
009800       MOVE IN-KVBEART       TO UT-KVBEART                                
009900       MOVE IN-PRAVCOST      TO UT-PRAVCOST                               
010000       MOVE IN-KVPB-REF      TO UT-KVPB-REF                               
010100       MOVE IN-KVAKS-SDC     TO UT-KVAKS-SDC                              
010200       MOVE IN-ONHAND        TO UT-ONHAND                                 
010300       MOVE IN-AVAIL-CDC     TO UT-AVAIL-CDC                              
010400       MOVE IN-KDERS         TO UT-KDERS                                  
010500                                                                          
010600       PERFORM UNTIL END-OF-W27164                                        
010700       OR  IN-IDARTNR    NOT = WS-IDARTNR                                 
010800                                                                          
010900         ADD +1              TO WS-ANTAL-ORDRAD                           
011000         ADD IN-KVBEART-Q    TO WS-ANTAL-QTY                              
011100                                                                          
011200         PERFORM S01-LAES-W27164IN                                        
011300       END-PERFORM                                                        
011400                                                                          
011500       MOVE WS-ANTAL-ORDRAD  TO UT-ANTAL-ORDRAD                           
011600       MOVE WS-ANTAL-QTY     TO UT-ANTAL-QTY                              
011700                                                                          
011800       PERFORM S02-SKRIV-UTFIL                                            
011900                                                                          
012000     END-PERFORM                                                          
012100                                                                          
012200                                                                          
012300                                                                          
012400     PERFORM Z-FINIT                                                      
012500                                                                          
012600     MOVE ZERO TO RETURN-CODE                                             
012700     GOBACK                                                               
012800     .                                                                    
012900     EJECT                                                                
013000 A-INIT SECTION.                                                          
013100     SKIP2                                                                
013200                                                                          
013300     OPEN INPUT  W27164IN                                                 
013400     OPEN OUTPUT W27164UT                                                 
013500     .                                                                    
013600     EJECT                                                                
013700                                                                          
013800 Z-FINIT SECTION.                                                         
013900                                                                          
014000                                                                          
014100     CLOSE W27164IN                                                       
014200           W27164UT                                                       
014300     .                                                                    
014400     EJECT                                                                
014500 S01-LAES-W27164IN SECTION.                                               
014600     SKIP2                                                                
014700     READ W27164IN           INTO IN-AREA                                 
014800     AT END                                                               
014900        MOVE JA TO W27164-EOF-SW                                          
015000                                                                          
015100     END-READ                                                             
015200     .                                                                    
015300     EJECT                                                                
015400 S02-SKRIV-UTFIL SECTION.                                                 
015500     SKIP2                                                                
015600     WRITE UT-POST                    FROM UT-AREA                        
015700     .                                                                    
