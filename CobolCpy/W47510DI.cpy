000010*** EDIT ALLOWED                                                          
000100 S03-UPPDATERA-ORQI-KDORDSTA SECTION.                                     
000200                                                                          
000300     MOVE ZERO                 TO W-KVKOLLI                               
000400                                  W-KVKOLLI-FAKT                          
000500                                  W-KVKOLLI-LAST                          
000600                                  WS-IDPRODNR                             
000700     MOVE NEJ                  TO KDORDSTA-SW                             
000800                                                                          
000900     IF GOOD-DC                                                           
001000        PERFORM IMS-KOLLI-GNP-OHUVSEG                                     
001100        MOVE KOLLI-KKORD-IDDISTR  TO W-E4ASEQ-IDDISTR                     
001200        MOVE KOLLI-KKORD-IDKUNDNR TO W-E4ASEQ-IDKUNDNR                    
001300        MOVE KOLLI-KKORD-IDKUNDRF TO W-E4ASEQ-IDKUNDRF                    
001400        PERFORM IMS-KORD-GU-OHUVSEG                                       
001500                                                                          
001600        MOVE KORD-IDORDER         TO W-IDORDER                            
001700                                                                          
001800        PERFORM IMS-ORQI-GHU-OHUVSEG                                      
001900                                                                          
002000        MOVE +1 TO ORDSTA-IX                                              
002100        PERFORM UNTIL ORDSTA-IX > +3                                      
002200          IF OHUV-IDDC-CLEAR(ORDSTA-IX) = ARB-SKEPP-IDDC                  
002300             MOVE OHUV-KDORDSTA(ORDSTA-IX)                                
002400                                  TO WS-DC-KDORDSTA                       
002500             ADD +3               TO ORDSTA-IX                            
002600          ELSE                                                            
002700             IF ORDSTA-IX = +3                                            
002800                MOVE 'RADENS DC SAKNAS I DC-MATRISEN' TO FELTEXT          
002900                CALL ABEND USING RKOD-ABEND-MED-DUMP                      
003000             ELSE                                                         
003100               ADD +1 TO ORDSTA-IX                                        
003200             END-IF                                                       
003300          END-IF                                                          
003400        END-PERFORM                                                       
003500     ELSE                                                                 
003600        IF GOOD-DDC                                                       
003700           MOVE KORD-IDORDER         TO W-IDORDER                         
003800           MOVE ARB-SKEPP-IDDC     TO W-WDQ211-IDDC                       
003900           MOVE KOLLI-VORD-IDLEVNR TO W-WDQ211-IDLEVNR                    
004000           PERFORM IMS-ORQI-GHU-DIRLSEG                                   
004100           MOVE DIRL-KDORDSTA      TO WS-DC-KDORDSTA                      
004200        ELSE                                                              
004300           MOVE 'RADENS DC SAKNAS I DDC-COPYTEXTEN' TO FELTEXT            
004400           CALL ABEND USING RKOD-ABEND-MED-DUMP                           
004500        END-IF                                                            
004600     END-IF                                                               
004700                                                                          
004800     IF WS-DC-KDORDSTA = 'R*' OR 'U*'                                     
004900       MOVE JA                 TO KDORDSTA-SW                             
005000       MOVE  WS-DC-KDORDSTA    TO WS-KDORDSTA                             
005100     ELSE                                                                 
005200* SVAR FRÅN BASLÄSNINGEN  REF TILL E4 EJ Q2. FÖRSTA GÅNGEN SKALL          
005300* MAN ALLTID IN I SNURRAN.                                                
005400       PERFORM UNTIL KDORDSTA-KLAR                                        
005500                  OR SEGMENT-SAKNAS                                       
005600                  OR SEGMENT-SLUT                                         
005700         IF KORD-IDDC         = ARB-SKEPP-IDDC    AND                     
005800            KORD-IDPRODNR NOT = WS-IDPRODNR                               
005900           MOVE KORD-IDPRODNR  TO W-IDPRODNR                              
006000                                  WS-IDPRODNR                             
006100           PERFORM IMS-KOLLI-GET-VORDSEG                                  
006200                                                                          
006300           IF KOLLI-VORD-KVKOLLI-LAST = KOLLI-VORD-KVKOLLI OR             
006400              KOLLI-VORD-KVKOLLI-FAKT = KOLLI-VORD-KVKOLLI                
006500             COMPUTE W-KVKOLLI       = W-KVKOLLI +                        
006600                                       KOLLI-VORD-KVKOLLI                 
006700             COMPUTE W-KVKOLLI-FAKT  = W-KVKOLLI-FAKT +                   
006800                                       KOLLI-VORD-KVKOLLI-FAKT            
006900             COMPUTE W-KVKOLLI-LAST  = W-KVKOLLI-LAST +                   
007000                                       KOLLI-VORD-KVKOLLI-LAST            
007100           ELSE                                                           
007200            MOVE 'P*'          TO WS-KDORDSTA                             
007300            MOVE JA            TO KDORDSTA-SW                             
007400           END-IF                                                         
007500                                                                          
007600         END-IF                                                           
007700                                                                          
007800         PERFORM IMS-KORD-GN-OHUVSEG                                      
007900       END-PERFORM                                                        
008000     END-IF                                                               
008100                                                                          
008200     IF KDORDSTA-KLAR                                                     
008300         CONTINUE                                                         
008400      ELSE                                                                
008500         PERFORM S03A-TA-FRAM-KDORDSTA                                    
008600     END-IF                                                               
008700                                                                          
008800     IF WS-KDORDSTA            = WS-DC-KDORDSTA                           
008900        CONTINUE                                                          
009000     ELSE                                                                 
009100        IF GOOD-DC                                                        
009200           MOVE +1               TO ORDSTA-IX                             
009300           PERFORM UNTIL ORDSTA-IX > +3                                   
009400             IF OHUV-IDDC-CLEAR(ORDSTA-IX) = ARB-SKEPP-IDDC               
009500                MOVE WS-KDORDSTA TO OHUV-KDORDSTA(ORDSTA-IX)              
009600                ADD +3           TO ORDSTA-IX                             
009700             ELSE                                                         
009800                IF ORDSTA-IX = +3                                         
009900                   MOVE 'RADENS DC SAKNAS I DC-MATRISEN' TO               
010000                        FELTEXT                                           
010100                   CALL ABEND USING RKOD-ABEND-MED-DUMP                   
010200                ELSE                                                      
010300                  ADD +1         TO ORDSTA-IX                             
010400                END-IF                                                    
010500             END-IF                                                       
010600           END-PERFORM                                                    
010700           PERFORM IMS-ORQI-REPL-OHUVSEG                                  
010800        ELSE                                                              
010900           MOVE WS-KDORDSTA      TO DIRL-KDORDSTA                         
011000           PERFORM IMS-ORQI-REPL-DIRLSEG                                  
011100        END-IF                                                            
011200     END-IF                                                               
011300     .                                                                    
011400     EJECT                                                                
011500 S03A-TA-FRAM-KDORDSTA SECTION.                                           
011600                                                                          
011700     EVALUATE TRUE                                                        
011800       WHEN W-KVKOLLI-LAST     NOT = W-KVKOLLI AND                        
011900            W-KVKOLLI-FAKT     NOT = W-KVKOLLI                            
012000             MOVE 'P*'         TO WS-KDORDSTA                             
012100                                                                          
012200       WHEN W-KVKOLLI-LAST     = W-KVKOLLI      AND                       
012300            W-KVKOLLI-FAKT     NOT = W-KVKOLLI  AND                       
012400            W-KVKOLLI-FAKT     > ZERO                                     
012500             MOVE 'L*'         TO WS-KDORDSTA                             
012600                                                                          
012700       WHEN W-KVKOLLI-FAKT     = W-KVKOLLI      AND                       
012800            W-KVKOLLI-LAST     NOT = W-KVKOLLI  AND                       
012900            W-KVKOLLI-LAST     > ZERO                                     
013000             MOVE 'F*'         TO WS-KDORDSTA                             
013100                                                                          
013200       WHEN W-KVKOLLI-FAKT     = W-KVKOLLI      AND                       
013300            W-KVKOLLI-LAST     = W-KVKOLLI                                
013400             MOVE 'FL'        TO WS-KDORDSTA                              
013500                                                                          
013600       WHEN W-KVKOLLI-FAKT     = W-KVKOLLI      AND                       
013700            W-KVKOLLI-LAST     NOT = W-KVKOLLI  AND                       
013800            W-KVKOLLI-LAST     = ZERO                                     
013900             MOVE 'F '         TO WS-KDORDSTA                             
014000                                                                          
014100       WHEN W-KVKOLLI-LAST     = W-KVKOLLI      AND                       
014200            W-KVKOLLI-FAKT     NOT = W-KVKOLLI  AND                       
014300            W-KVKOLLI-FAKT     = ZERO                                     
014400             MOVE 'L '        TO WS-KDORDSTA                              
014500     END-EVALUATE                                                         
014600     .                                                                    
014700     EJECT                                                                
