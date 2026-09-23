000300 ID DIVISION.                                                             
000400 PROGRAM-ID.     W5400200.                                                
000800*AUTHOR.         KJELL.                                                   
000900*DATE-WRITTEN.   MAJ 1979.                                                
001000                                                                          
001200*        FUNKTION.                                                        
001300*                PROGRAMMET ÄR SUBPROGRAM TILL TRATTEN                    
001400*                OCH SKRIVER KONTROLLERADE OCH REDIGERADE                 
001500*                R24:OR PÅ UTFILEN.                                       
001600*                R24:ORNA GENOMGÅR KOPPLADE KONTROLLER.                   
001700                                                                          
001800     EJECT                                                                
001900 ENVIRONMENT DIVISION.                                                    
002000 INPUT-OUTPUT SECTION.                                                    
002100 FILE-CONTROL.                                                            
002200     SKIP2                                                                
002300*------------------------------ REDIGERADE TRANSAKTIONER                  
002400     SELECT  W09270  ASSIGN TO UT-S-W09206DY.                             
002500     EJECT                                                                
002600 DATA DIVISION.                                                           
002700 FILE SECTION.                                                            
002800 FD  W09270                                                               
002900     BLOCK CONTAINS 0                                                     
003000     RECORDING F                                                          
003100                           .                                              
003200     SKIP3                                                                
003300*01  POST    -COPY W540054    -PRE UT054- -L.                             
003500     EJECT                                                                
003600 WORKING-STORAGE SECTION.                                                 
003610                                                                          
003700*    -- CHECKED BY WY2000                                                 
003800 77  LCP-ONCTR-01                  PIC S9(8) COMP-3 VALUE ZERO.           
003900 77  IDPGM                   PIC X(8) VALUE 'W5400200'.                   
004300     SKIP2                                                                
004400 77  INDX                    PIC S9(4)   COMP SYNC.                       
004500 77  W-FELKOD                PIC X(3).                                    
004600     EJECT                                                                
004700 LINKAGE SECTION.                                                         
004800     SKIP2                                                                
004900 01  TRANS-PARM.                                                          
005000*                                                                         
005100     03  EOF-KOD             PIC X(3).                                    
005200*                                                                         
005300     03  FELTAB  OCCURS 100.                                              
005400         05  FELKOD          PIC X(3).                                    
005500         05  FELTEXT         PIC X(15).                                   
005600*                                                                         
005700     03  FILLER              PIC X(3).                                    
005800     EJECT                                                                
005900     03  INPOST              PIC X(100).                                  
006000*                                                                         
006100*    03  AREA -COPY W540054T -PRE INR24-  -RED INPOST                     
006300     EJECT                                                                
006400     03  UTAREA.                                                          
006500*        05  ID-DEL  -COPY W092W001  -PRE ID-.                            
006700     EJECT                                                                
006800         05  UTPOST          PIC X(214).                                  
006900*        05  AREA    -COPY W540054 -PRE UT054-  -RED UTPOST               
007100     EJECT                                                                
007200 PROCEDURE DIVISION USING TRANS-PARM.                                     
007300     SKIP2                                                                
007400     IF LCP-ONCTR-01 =  0                                                 
007500         ADD 1 TO LCP-ONCTR-01                                            
007600                               OPEN OUTPUT W09270                         
007610     END-IF                                                               
007700                                                                          
007800     IF EOF-KOD = 'EOF'                                                   
007900       CLOSE  W09270                                                      
008000       GOBACK                                                             
008100     END-IF                                                               
008200     PERFORM A-KOPPLADE-KONTROLLER                                        
008300                                                                          
008400     IF W-FELKOD NOT = SPACE                                              
008500       MOVE W-FELKOD TO FELKOD (1)                                        
008600       MOVE HIGH-VALUE TO FELKOD (2)                                      
008700     ELSE                                                                 
008800       PERFORM B-SKRIV-054-POST                                           
008900     END-IF                                                               
009000     GOBACK                                                               
009100     .                                                                    
009200     EJECT                                                                
009300 A-KOPPLADE-KONTROLLER SECTION.                                           
009400     SKIP2                                                                
009500     MOVE '1C7'  TO  W-FELKOD                                             
009600     IF INR24-KDUPPTYP = '0'                                              
009700     AND INR24-PRINK NOT NUMERIC                                          
009800     AND INR24-PRDIRLON NOT NUMERIC                                       
009900     AND INR24-PRDMTRL NOT NUMERIC                                        
010000     AND INR24-PROVRPAL NOT NUMERIC                                       
010100       MOVE  SPACE    TO  W-FELKOD                                        
010200     ELSE                                                                 
010300       IF INR24-KDUPPTYP = '1'                                            
010400       AND INR24-PRINK NUMERIC                                            
010500       AND INR24-PRDMTRL NOT NUMERIC                                      
010600       AND INR24-PRDIRLON NOT NUMERIC                                     
010700       AND INR24-PROVRPAL NOT NUMERIC                                     
010800         MOVE    SPACE   TO W-FELKOD                                      
010900*                                                                         
011000       ELSE                                                               
011100         IF INR24-KDUPPTYP = '2'                                          
011200         AND INR24-PRINK NUMERIC                                          
011300         AND INR24-PRDIRLON NUMERIC                                       
011400         AND INR24-PRDMTRL NUMERIC                                        
011500         AND INR24-PROVRPAL NUMERIC                                       
011600           MOVE    SPACE   TO  W-FELKOD                                   
011700*                                                                         
011800         ELSE                                                             
011900           IF INR24-KDUPPTYP = '3'                                        
012000           AND INR24-PRINK NOT NUMERIC                                    
012100           AND INR24-PRDIRLON NUMERIC                                     
012200           AND INR24-PRDMTRL NUMERIC                                      
012300           AND INR24-PROVRPAL NUMERIC                                     
012400             MOVE    SPACE   TO  W-FELKOD                                 
012500*                                                                         
012600           ELSE                                                           
012700             IF INR24-KDUPPTYP = '4'                                      
012800             AND INR24-PRINK NOT NUMERIC                                  
012900             AND INR24-PROVRPAL NOT NUMERIC                               
013000             AND INR24-PRDIRLON NUMERIC                                   
013100             AND INR24-PRDMTRL NUMERIC                                    
013200               MOVE    SPACE   TO  W-FELKOD                               
013300*                                                                         
013400             ELSE                                                         
013500               IF INR24-KDUPPTYP = '5'                                    
013600               AND INR24-PRINK NOT NUMERIC                                
013700               AND INR24-PRDMTRL NOT NUMERIC                              
013800               AND INR24-PRDIRLON NUMERIC                                 
013900               AND INR24-PROVRPAL NUMERIC                                 
014000                 MOVE    SPACE   TO  W-FELKOD                             
014100*                                                                         
014200               ELSE                                                       
014300                 IF INR24-KDUPPTYP = '6'                                  
014400                 AND INR24-PRINK NOT NUMERIC                              
014500                 AND INR24-PRDIRLON NOT NUMERIC                           
014600                 AND INR24-PRDMTRL NUMERIC                                
014700                 AND INR24-PROVRPAL NUMERIC                               
014800                   MOVE    SPACE   TO  W-FELKOD                           
014900*                                                                         
015000                 ELSE                                                     
015100                   IF INR24-KDUPPTYP = '7'                                
015200                   AND INR24-PRDMTRL NOT NUMERIC                          
015300                   AND INR24-PRINK NOT NUMERIC                            
015400                   AND INR24-PROVRPAL NOT NUMERIC                         
015500                   AND INR24-PRDIRLON NUMERIC                             
015600                     MOVE    SPACE   TO  W-FELKOD                         
015700*                                                                         
015800                   ELSE                                                   
015900                     IF INR24-KDUPPTYP = '8'                              
016000                     AND INR24-PRINK NOT NUMERIC                          
016100                     AND INR24-PRDIRLON NOT NUMERIC                       
016200                     AND INR24-PROVRPAL NOT NUMERIC                       
016300                     AND INR24-PRDMTRL NUMERIC                            
016400                       MOVE    SPACE   TO  W-FELKOD                       
016500*                                                                         
016600                     ELSE                                                 
016700                       IF INR24-KDUPPTYP = '9'                            
016800                       AND INR24-PRINK NOT NUMERIC                        
016900                       AND INR24-PRDIRLON NOT NUMERIC                     
017000                       AND INR24-PRDMTRL NOT NUMERIC                      
017100                       AND INR24-PROVRPAL NUMERIC                         
017200                         MOVE    SPACE   TO  W-FELKOD                     
017300                       END-IF                                             
017400                     END-IF                                               
017500                   END-IF                                                 
017600                 END-IF                                                   
017700               END-IF                                                     
017800             END-IF                                                       
017900           END-IF                                                         
018000         END-IF                                                           
018100       END-IF                                                             
018200     END-IF                                                               
018300     .                                                                    
018400     EJECT                                                                
018500 B-SKRIV-054-POST SECTION.                                                
018600     WRITE UT054-POST FROM UT054-AREA                                     
018700     .                                                                    
