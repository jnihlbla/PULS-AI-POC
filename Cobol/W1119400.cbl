000100 ID DIVISION.                                                             
000200     SKIP2                                                                
000300 PROGRAM-ID.     W1119400.                                                
000400*AUTHOR.         BODIL LINDAHL.                                           
000500*DATE-WRITTEN.   97/12/16.                                                
000600                                                                          
000700*    REMARKS                                                              
000800*                                                                         
000900*    FUNKTION:                                                            
001000*        PROGRAMMET MATCHAR W01177(0) OCH W01177(-1).                     
001100*        FIL SKAPAS FARLIGT GODS.                                         
001200*        - ARTIKLAR MED FÖRÄNDRAD KDFARLIG TILL/FRÅN 4/6/7.               
001300     SKIP3                                                                
001400 ENVIRONMENT DIVISION.                                                    
001500     SKIP2                                                                
001600 INPUT-OUTPUT SECTION.                                                    
001700 FILE-CONTROL.                                                            
001800     SKIP2                                                                
001900*          --- LAGERBAND IN,AKTUELL VECKAS                                
002000     SELECT W01177-NEW                 ASSIGN TO W11194D1.                
002100     SKIP2                                                                
002200*          --- LAGERBAND IN,FÖRRA VECKANS                                 
002300     SELECT W01177-OLD                 ASSIGN TO W11194D2.                
002400     SKIP2                                                                
002500*          --- MEMOFIL/DAP FIL                                            
002600     SELECT W11194                     ASSIGN TO W11194D3.                
002700     EJECT                                                                
002800 DATA DIVISION.                                                           
002900                                                                          
003000 FILE SECTION.                                                            
003100     SKIP3                                                                
003200 FD  W01177-NEW                                                           
003300     RECORDING       F                                                    
003400     BLOCK CONTAINS  0.                                                   
003500*01  -COPY W011100      -L.                                               
003600     SKIP3                                                                
003700 FD  W01177-OLD                                                           
003800     RECORDING       F                                                    
003900     BLOCK CONTAINS  0.                                                   
004000*01  -COPY W011100      -L.                                               
004100     SKIP3                                                                
004200 FD  W11194                                                               
004300     RECORDING       V                                                    
004400     BLOCK CONTAINS  0.                                                   
004500 01  MEMO-POST           PIC X(84).                                       
004600     EJECT                                                                
004700 WORKING-STORAGE SECTION.                                                 
004800     SKIP2                                                                
004900                                                                          
005000                                                                          
005100*    -- CHECKED BY WY2000                                                 
005200 77  IDPGM                       PIC X(8)    VALUE 'W1119400'.            
005300 77  JA                          PIC X       VALUE 'J'.                   
005400 77  NEJ                         PIC X       VALUE 'N'.                   
005500 77  WS-RUBRIK-SKRIVEN           PIC X       VALUE SPACE.                 
005600                                                                          
005700 77  W01177-EOF-SW-NEW           PIC X       VALUE 'N'.                   
005800     88  END-OF-W01177-NEW                   VALUE 'J'.                   
005900                                                                          
006000 77  W01177-EOF-SW-OLD           PIC X       VALUE 'N'.                   
006100     88  END-OF-W01177-OLD                   VALUE 'J'.                   
006200                                                                          
006300 01  IDAG.                                                                
006400     03  WS-SEKEL                PIC 9(2).                                
006500     03  DAGENS-DATUM.                                                    
006600         05  DAGENS-SEKEL        PIC 9(1).                                
006700         05  FILLER              PIC 9(5).                                
006800                                                                          
006900 01  MEMO-AREA-START             PIC X(24)   VALUE                        
007000                                 'MEMO-AREA-START  '.                     
007100 01  MEMO-RAD.                                                            
007200     03  FILLER                  PIC X(80)  VALUE SPACE.                  
007300     EJECT                                                                
007400 01  MEMO-RUBRIK1.                                                        
007500*                                                                         
007600     03  FILLER                  PIC X(43)                                
007700             VALUE '  VCAS   ARTIKELFÖRÄNDRING FARLIGT GODS KOD'.         
007800     03  FILLER                  PIC X(11)   VALUE SPACE.                 
007900     03  MEMO-DATUM              PIC XXBXXBXX.                            
008000     03  FILLER                  PIC X(20)   VALUE SPACE.                 
008100     SKIP3                                                                
008200 01  MEMO-RUBRIK2.                                                        
008300     03  FILLER                  PIC X(17)   VALUE                        
008400                                '  ARTIKELNR.     '.                      
008500     03  FILLER                  PIC X(30)   VALUE                        
008600                                'SVENSK BENÄMNING              '.         
008700     03  FILLER                  PIC X(16)   VALUE                        
008800                                'FARLIGT GODS KOD'.                       
008900     03  FILLER                  PIC X(19)   VALUE SPACE.                 
009000     SKIP3                                                                
009100 01  MEMO-RUBRIK3.                                                        
009200     03  FILLER                  PIC X(47)   VALUE SPACE.                 
009300     03  FILLER                  PIC X(5)    VALUE                        
009400                                'NY   '.                                  
009500     03  FILLER                  PIC X(6)    VALUE                        
009600                                'GAMMAL'.                                 
009700     03  FILLER                  PIC X(24)   VALUE SPACE.                 
009800     SKIP3                                                                
009900 01  MEMO-DETALJRAD1.                                                     
010000     03  FILLER                  PIC X       VALUE ' '.                   
010100     03  MEMO-DET1-IDARTNR       PIC Z(9).                                
010200     03  FILLER                  PIC X(6)    VALUE SPACE.                 
010300     03  MEMO-DET1-BEART         PIC X(25).                               
010400     03  FILLER                  PIC X(6)    VALUE SPACE.                 
010500     03  MEMO-DET1-KDFARLIG-NY   PIC Z(1).                                
010600     03  FILLER                  PIC X(8)    VALUE SPACE.                 
010700     03  MEMO-DET1-KDFARLIG-OLD  PIC Z(1).                                
010800     03  FILLER                  PIC X(26)   VALUE SPACE.                 
010900     EJECT                                                                
011000 01  FELTEXT.                                                             
011100     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
011200     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
011300     EJECT                                                                
011400 01  DYNAMISKA-SUBPROGRAM.                                                
011500*                                                                         
011600     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
011700                                                                          
011800*01  -COPY W0005   -PRE  POSTSUM-                                         
011900     EJECT                                                                
012000 01  IN-AREA-NEW-START          PIC X(24)   VALUE                         
012100                                 'IN-AREA-NEW-START'.                     
012200 01  INN-AREA.                                                            
012300*   05  FILLER -COPY W011100  -PRE INN-                                   
012400     EJECT                                                                
012500 01  IN-AREA-OLD-START           PIC X(24)   VALUE                        
012600                                 'IN-AREA-OLD-START'.                     
012700 01  INO-AREA.                                                            
012800*   05  FILLER -COPY W011100  -PRE INO-                                   
012900     EJECT                                                                
013000 PROCEDURE DIVISION.                                                      
013100                                                                          
013200     PERFORM A-INIT                                                       
013300                                                                          
013400     PERFORM S01-LAES-W01177-NEW                                          
013500     PERFORM S02-LAES-W01177-OLD                                          
013600                                                                          
013700     PERFORM UNTIL END-OF-W01177-NEW AND                                  
013800                   END-OF-W01177-OLD                                      
013900                                                                          
014000        IF INN-IDARTNR = INO-IDARTNR                                      
014100           IF INN-KDFARLIG NOT = INO-KDFARLIG                             
014200              IF (INN-KDFARLIG = 4 OR 6 OR 7)                             
014300              OR (INO-KDFARLIG = 4 OR 6 OR 7)                             
014400                 PERFORM B-SKAPA-SKRIV-UTFIL                              
014500              END-IF                                                      
014600           END-IF                                                         
014700           PERFORM S01-LAES-W01177-NEW                                    
014800           PERFORM S02-LAES-W01177-OLD                                    
014900        ELSE                                                              
015000           IF INN-IDARTNR > INO-IDARTNR                                   
015100              PERFORM S02-LAES-W01177-OLD                                 
015200           ELSE                                                           
015300              PERFORM S01-LAES-W01177-NEW                                 
015400           END-IF                                                         
015500        END-IF                                                            
015600     END-PERFORM                                                          
015700                                                                          
015800     PERFORM Z-FINIT                                                      
015900     MOVE ZERO TO RETURN-CODE                                             
016000     GOBACK                                                               
016100     .                                                                    
016200     EJECT                                                                
016300 A-INIT SECTION.                                                          
016400     OPEN INPUT  W01177-NEW                                               
016500                 W01177-OLD                                               
016600     OPEN OUTPUT W11194                                                   
016700                                                                          
016800     MOVE NEJ TO WS-RUBRIK-SKRIVEN                                        
016900     ACCEPT DAGENS-DATUM FROM DATE                                        
017000     MOVE DAGENS-DATUM TO MEMO-DATUM                                      
017100     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
017200     .                                                                    
017300     SKIP3                                                                
017400 B-SKAPA-SKRIV-UTFIL SECTION.                                             
017500                                                                          
017600     IF WS-RUBRIK-SKRIVEN = NEJ                                           
017700        MOVE MEMO-RUBRIK1    TO MEMO-RAD                                  
017800        PERFORM S03-SKRIV-MEMO                                            
017900        MOVE MEMO-RUBRIK2    TO MEMO-RAD                                  
018000        PERFORM S03-SKRIV-MEMO                                            
018100        MOVE MEMO-RUBRIK3    TO MEMO-RAD                                  
018200        PERFORM S03-SKRIV-MEMO                                            
018300        MOVE JA TO WS-RUBRIK-SKRIVEN                                      
018400     END-IF                                                               
018500     MOVE INN-IDARTNR     TO MEMO-DET1-IDARTNR                            
018600     MOVE INN-BEART-SVE   TO MEMO-DET1-BEART                              
018700     MOVE INN-KDFARLIG    TO MEMO-DET1-KDFARLIG-NY                        
018800     MOVE INO-KDFARLIG    TO MEMO-DET1-KDFARLIG-OLD                       
018900     MOVE MEMO-DETALJRAD1 TO MEMO-RAD                                     
019000     PERFORM S03-SKRIV-MEMO                                               
019100     .                                                                    
019200     EJECT                                                                
019300 Z-FINIT SECTION.                                                         
019400                                                                          
019500     CLOSE W01177-NEW                                                     
019600           W01177-OLD                                                     
019700           W11194                                                         
019800                                                                          
019900     MOVE 'S' TO POSTSUM-OPKOD                                            
020000     CALL POSTSUM USING POSTSUM-PARM                                      
020100     .                                                                    
020200     EJECT                                                                
020300 S01-LAES-W01177-NEW SECTION.                                             
020400     READ W01177-NEW INTO INN-AREA                                        
020500     AT END                                                               
020600        MOVE 999999999 TO INN-IDARTNR                                     
020700        SET END-OF-W01177-NEW TO TRUE                                     
020800                                                                          
020900     NOT AT END                                                           
021000        MOVE 'W01177-NEW' TO POSTSUM-FDNAMN                               
021100        MOVE 'W11194D1'   TO POSTSUM-DDNAMN2                              
021200        MOVE 'LB N'       TO POSTSUM-TRANSTYP                             
021300        CALL POSTSUM USING POSTSUM-PARM                                   
021400     END-READ                                                             
021500     .                                                                    
021600     SKIP3                                                                
021700 S02-LAES-W01177-OLD SECTION.                                             
021800     READ W01177-OLD INTO INO-AREA                                        
021900     AT END                                                               
022000        MOVE 999999999 TO INO-IDARTNR                                     
022100        SET END-OF-W01177-OLD TO TRUE                                     
022200                                                                          
022300     NOT AT END                                                           
022400        MOVE 'W01177-OLD' TO POSTSUM-FDNAMN                               
022500        MOVE 'W11194D2'   TO POSTSUM-DDNAMN2                              
022600        MOVE 'LB O'       TO POSTSUM-TRANSTYP                             
022700        CALL POSTSUM USING POSTSUM-PARM                                   
022800     END-READ                                                             
022900     .                                                                    
023000     EJECT                                                                
023100 S03-SKRIV-MEMO SECTION.                                                  
023200     WRITE MEMO-POST FROM MEMO-RAD                                        
023300                                                                          
023400     MOVE 'MEMO  '   TO POSTSUM-FDNAMN                                    
023500     MOVE 'W11194D3' TO POSTSUM-DDNAMN2                                   
023600     MOVE SPACE      TO POSTSUM-TRANSTYP                                  
023700     CALL POSTSUM USING POSTSUM-PARM                                      
023800     .                                                                    
023900     EJECT                                                                
