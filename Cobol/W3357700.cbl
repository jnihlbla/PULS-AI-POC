000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W3357700.                                                
000400*AUTHOR.         INGVAR SKJELBRED.                                        
000500*DATE-WRITTEN.   95/10/10.                                                
000600                                                                          
000700                                                                          
000800*                                                                         
000900*    FUNKTION:                                                            
001000*        SLÅR IHOP FILERNA W33575 W33573                                  
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
002500*          --- SORTERAD INFIL W33575S                                     
002600     SELECT W33575                     ASSIGN TO W33577D1.                
002700     SKIP2                                                                
002800*          --- SORTERAD INFIL W33573S                                     
002900     SELECT W33573                     ASSIGN TO W33577D2.                
003000     SKIP2                                                                
003100*          --- IHOPSLAGEN FIL AV 33575 33573                              
003200     SELECT W33577                     ASSIGN TO W33577D3.                
003300     EJECT                                                                
003400 DATA DIVISION.                                                           
003500     SKIP3                                                                
003600 FILE SECTION.                                                            
003700     SKIP3                                                                
003800 FD  W33575                                                               
003900     RECORDING       F                                                    
004000     BLOCK CONTAINS  0.                                                   
004100                                                                          
004200*01  -COPY W33575      -L.                                                
004300     SKIP3                                                                
004400 FD  W33573                                                               
004500     RECORDING       F                                                    
004600     BLOCK CONTAINS  0.                                                   
004700                                                                          
004800*01  -COPY W33573      -L.                                                
004900     SKIP3                                                                
005000 FD  W33577                                                               
005100     RECORDING       F                                                    
005200     BLOCK CONTAINS  0.                                                   
005300                                                                          
005400*01  POST -COPY W33577 -PRE  UT-  -L.                                     
005500     EJECT                                                                
005600 WORKING-STORAGE SECTION.                                                 
005800                                                                          
005900*    -- CHECKED BY WY2000                                                 
006000 77  IDPGM                       PIC X(8)    VALUE 'W3357700'.            
006100 77  JA                          PIC X       VALUE 'J'.                   
006200 77  NEJ                         PIC X       VALUE 'N'.                   
007900                                                                          
008000 77  W33575-EOF-SW               PIC X       VALUE 'N'.                   
008100     88  END-OF-W33575                       VALUE 'J'.                   
008200                                                                          
008300 77  W33573-EOF-SW               PIC X       VALUE 'N'.                   
008400     88  END-OF-W33573                       VALUE 'J'.                   
009100     EJECT                                                                
009200 01  DYNAMISKA-SUBPROGRAM.                                                
009300*                                                                         
009400     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
009500     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
009600     SKIP2                                                                
009700*    --- PARAMETRAR TILL ABEND                                            
009800                                                                          
009900 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
010000 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
010100     SKIP2                                                                
010200 01  FELTEXT.                                                             
010300     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
010400     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
010500     EJECT                                                                
010600*    --- PARAMETRAR TILL POSTSUM                                          
010700*                                                                         
010800*01  -COPY W0005   -PRE  POSTSUM-                                         
010900     EJECT                                                                
011000 01  IN1-AREA-START              PIC X(24)   VALUE                        
011100                                 'IN1-AREA-START  '.                      
011200     SKIP2                                                                
011300                                                                          
011400*01  AREA -COPY W33575     -PRE IN1-                                      
011500     EJECT                                                                
011600 01  IN2-AREA-START              PIC X(24)   VALUE                        
011700                                 'IN2-AREA-START  '.                      
011900                                                                          
012000*01  AREA -COPY W33573     -PRE IN2-                                      
012100     EJECT                                                                
012200 01  UT-AREA-START               PIC X(24)   VALUE                        
012300                                 'UT-AREA-START  '.                       
012400     SKIP2                                                                
012500                                                                          
012600*01  AREA -COPY W33577     -PRE UT-                                       
012700     EJECT                                                                
012800 PROCEDURE DIVISION.                                                      
012900     SKIP2                                                                
013000                                                                          
013100     PERFORM A-INIT                                                       
013200     PERFORM S01-LAES-W33575                                              
013300     PERFORM S02-LAES-W33573                                              
013400     PERFORM UNTIL END-OF-W33575                                          
013500             AND   END-OF-W33573                                          
013600        PERFORM  B-KONTRL-IN-FILER                                        
013700     END-PERFORM                                                          
013800                                                                          
013900                                                                          
014000     PERFORM Z-FINIT                                                      
014100                                                                          
014200     MOVE ZERO TO RETURN-CODE                                             
014300     GOBACK                                                               
014400     .                                                                    
014500     EJECT                                                                
014600 A-INIT SECTION.                                                          
014700                                                                          
014800     OPEN INPUT  W33575                                                   
014900                 W33573                                                   
015000                                                                          
015100     OPEN OUTPUT W33577                                                   
015200                                                                          
015400     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
015500     PERFORM BB-INITIERA-UT-FIL                                           
015600     .                                                                    
015700     EJECT                                                                
015800 B-KONTRL-IN-FILER SECTION.                                               
015900******************************************************************        
016000***** FILER SOM SAKNAR DISTRIKT ÄR OINTRESSANTA OCH SKRIVS EJ UT**        
016100***** DVS NÄR DET FINNS EN POST I FIL1 SOM SAKNAR MATCHANDE FIL2**        
016200******************************************************************        
016300                                                                          
016400     IF END-OF-W33575                                                     
016500        PERFORM BA-SKAPA-NY-FIL                                           
016600        PERFORM S02-LAES-W33573                                           
016700     ELSE                                                                 
016800       IF END-OF-W33573                                                   
016900          PERFORM S01-LAES-W33575                                         
017000       ELSE                                                               
017100         IF IN1-IDPARTNR = IN2-IDPARTNR                                   
017200           PERFORM BA-SKAPA-NY-FIL                                        
017300           PERFORM S02-LAES-W33573                                        
017400         ELSE                                                             
017500           IF IN1-IDPARTNR > IN2-IDPARTNR                                 
017600              PERFORM BA-SKAPA-NY-FIL                                     
017700              PERFORM S02-LAES-W33573                                     
017800           ELSE                                                           
017900*****************************************************                     
018000**     IF IN1-IDPARTNR < IN2-IDPARTNR            ****                     
018100** POSTERN SAKNAR DISTRIKTNUMMER OCH GÅR DÅ INTE ****                     
018200** ATT ANVÄNDA TILL NÅGOT VETTIGT                ****                     
018300*****************************************************                     
018400             PERFORM S01-LAES-W33575                                      
018500           END-IF                                                         
018600         END-IF                                                           
018700       END-IF                                                             
018800     END-IF                                                               
018900     .                                                                    
019000     EJECT                                                                
019100 BA-SKAPA-NY-FIL SECTION.                                                 
019200                                                                          
019300     IF IN1-IDPARTNR = IN2-IDPARTNR                                       
019400        MOVE IN1-IDPROMR  TO UT-IDPROMR                                   
019500        MOVE IN1-IDPARTNR TO UT-IDPARTNR                                  
019700        MOVE IN1-KDKREDSP TO UT-KDKREDSP                                  
019800                                                                          
019900        MOVE IN2-IDDISTR TO UT-IDDISTR                                    
020000        MOVE IN2-BET-BEBETRAD-1 TO UT-BEBETRAD                            
020100        MOVE IN2-FLOKFAK-G TO UT-FLOKFAK-G                                
020200        MOVE IN2-FLOKFAK-K TO UT-FLOKFAK-K                                
020300        MOVE IN2-FLOKFAK-N TO UT-FLOKFAK-N                                
020400        MOVE IN2-FLOKFAK-R TO UT-FLOKFAK-R                                
020500        MOVE IN2-KDVALISO  TO UT-KDVALISO                                 
020600        MOVE IN2-REEMBHNT  TO UT-REEMBHNT                                 
020700     ELSE                                                                 
020800       IF IN1-IDPARTNR > IN2-IDPARTNR                                     
020900          MOVE IN2-IDDISTR TO UT-IDDISTR                                  
021000          MOVE IN2-IDPARTNR TO UT-IDPARTNR                                
021200                                                                          
021300          MOVE IN2-BET-BEBETRAD-1 TO UT-BEBETRAD                          
021400          MOVE IN2-FLOKFAK-G TO UT-FLOKFAK-G                              
021500          MOVE IN2-FLOKFAK-K TO UT-FLOKFAK-K                              
021600          MOVE IN2-FLOKFAK-N TO UT-FLOKFAK-N                              
021700          MOVE IN2-FLOKFAK-R TO UT-FLOKFAK-R                              
021800          MOVE IN2-KDVALISO  TO UT-KDVALISO                               
021900          MOVE IN2-REEMBHNT  TO UT-REEMBHNT                               
022000       END-IF                                                             
022100     END-IF                                                               
022200                                                                          
022300     PERFORM S11-SKRIV-W33577                                             
022400     PERFORM BB-INITIERA-UT-FIL                                           
022500                                                                          
022600     .                                                                    
022700     EJECT                                                                
022800 BB-INITIERA-UT-FIL SECTION.                                              
022900                                                                          
023000     MOVE SPACE        TO UT-IDPROMR                                      
023100     MOVE SPACE        TO UT-KDKREDSP                                     
023200     MOVE SPACE        TO UT-IDPARTNR                                     
023500     MOVE ZERO         TO UT-IDDISTR                                      
023600     MOVE SPACE        TO UT-BEBETRAD                                     
023700     MOVE SPACE        TO UT-FLOKFAK-G                                    
023800     MOVE SPACE        TO UT-FLOKFAK-K                                    
023900     MOVE SPACE        TO UT-FLOKFAK-N                                    
024000     MOVE SPACE        TO UT-FLOKFAK-R                                    
024100     MOVE ZERO         TO UT-KDVALISO                                     
024200     MOVE ZERO         TO UT-REEMBHNT                                     
024300                                                                          
024400     .                                                                    
024500     EJECT                                                                
024600 Z-FINIT SECTION.                                                         
024610                                                                          
024700     CLOSE W33575                                                         
024800           W33573                                                         
024900           W33577                                                         
025000                                                                          
025100     MOVE 'S' TO POSTSUM-OPKOD                                            
025200     CALL POSTSUM USING POSTSUM-PARM                                      
025300     .                                                                    
025400     EJECT                                                                
025500 S01-LAES-W33575  SECTION.                                                
025510                                                                          
025600     READ W33575 INTO IN1-AREA                                            
025700     AT END                                                               
025800        SET END-OF-W33575 TO TRUE                                         
026000     NOT AT END                                                           
026100        MOVE 'W33575'   TO POSTSUM-FDNAMN                                 
026200        MOVE 'W33577D1' TO POSTSUM-DDNAMN2                                
026300        MOVE 'FIL1'     TO POSTSUM-TRANSTYP                               
026400        CALL POSTSUM USING POSTSUM-PARM                                   
026800     END-READ                                                             
026900     .                                                                    
027000     EJECT                                                                
027100 S02-LAES-W33573  SECTION.                                                
027110                                                                          
027200     READ W33573 INTO IN2-AREA                                            
027300     AT END                                                               
027400        SET END-OF-W33573 TO TRUE                                         
027600     NOT AT END                                                           
027700        MOVE 'W33573'   TO POSTSUM-FDNAMN                                 
027800        MOVE 'W33577D2' TO POSTSUM-DDNAMN2                                
027900        MOVE 'FIL2'     TO POSTSUM-TRANSTYP                               
028000        CALL POSTSUM USING POSTSUM-PARM                                   
028400     END-READ                                                             
028500     .                                                                    
028600     EJECT                                                                
028700 S11-SKRIV-W33577 SECTION.                                                
028800                                                                          
028900     WRITE UT-POST FROM UT-AREA                                           
029000                                                                          
029100     MOVE 'UTFL'     TO POSTSUM-TRANSTYP                                  
029200     MOVE 'W33577'   TO POSTSUM-FDNAMN                                    
029300     MOVE 'W33577D3' TO POSTSUM-DDNAMN2                                   
029400     CALL POSTSUM USING POSTSUM-PARM                                      
029500     .                                                                    
