000100 01  MID-W2I40601.                                                        
000200*                                 MID-COPYTEXT                            
000300*                                 F÷R W2I406                              
000400     03 MID-IDARTNR-IN       PIC X(9).                                    
000500*                                 ARTIKELNUMMER                           
000600     03 MID-IDARTNR-UT       PIC X(9).                                    
000700*                                 ARTIKELNUMMER                           
000800     03 MID-IDDC-IN          PIC X(2).                                    
000900*                                 IDENTIFIERARE LAGER                     
001000     03 MID-IDDC-UT          PIC X(2).                                    
001100*                                 IDENTIFIERARE LAGER                     
001200     03 MID-IDLEVNR-IN       PIC X(5).                                    
001300*                                 LEVERANT÷RNUMMER                        
001400     03 MID-IDLEVNR-UT       PIC X(5).                                    
001500*                                 LEVERANT÷RNUMMER                        
001600     03 MID-UPDATE-LINE-GRP.                                              
001700        05 MID-UPDATE-LINE   OCCURS 8 TIMES.                              
001800           07 MID-KDCMD-UPD  PIC X.                                       
001900*                                 RAD-UPPDATERINGSKOMMANDO                
002000*                                  BLANK  = INGENTING                     
002100*                                  D , B  = DELETE                        
002200*                                  R , ƒ  = REPLACE                       
002300*                                  I,N,A  = INSERT                        
002400*                                  S , V  = SELECT                        
002500*                                  P , P  = PRINT                         
002600*                                  C , K  = COPY                          
002700           07 MID-DALEVBSK-AVS-UPD                                        
002800                             PIC X(5).                                    
002900*                                 ≈R - VECKA - DAG   (≈≈VVD)              
003000           07 MID-KVAVIS-UPD PIC X(7).                                    
003100*                                 AVISERAT ANTAL                          
003200           07 MID-TILEVBSK-INL-UPD                                        
003300                             PIC X(5).                                    
003400*                                 ≈R - VECKA - DAG   (≈≈VVD)              
003500     03 MID-UPDATE-IN        OCCURS 8 TIMES.                              
003600        05 MID-DALEVBSK-AVS  PIC X(5).                                    
003700*                                 ≈R - VECKA - DAG   (≈≈VVD)              
003800        05 MID-FLFORAVI      PIC X.                                       
003900*                                 F÷RAVISERAD INLEVERANS                  
004000        05 MID-IDLEVNR       PIC X(5).                                    
004100*                                 LEVERANT÷RNUMMER                        
004200     03 MID-KDCMD-NY         PIC X.                                       
004300*                                 RAD-UPPDATERINGSKOMMANDO                
004400*                                  BLANK  = INGENTING                     
004500*                                  D , B  = DELETE                        
004600*                                  R , ƒ  = REPLACE                       
004700*                                  I,N,A  = INSERT                        
004800*                                  S , V  = SELECT                        
004900*                                  P , P  = PRINT                         
005000*                                  C , K  = COPY                          
005100     03 MID-INSERT-LINE.                                                  
005200        05 MID-DALEVBSK-AVS-NY                                            
005300                             PIC X(5).                                    
005400*                                 ≈R - VECKA - DAG   (≈≈VVD)              
005500        05 MID-KVAVIS-NY     PIC X(7).                                    
005600*                                 AVISERAT ANTAL                          
005700        05 MID-TILEVBSK-INL-NY                                            
005800                             PIC X(5).                                    
005900*                                 ≈R - VECKA - DAG   (≈≈VVD)              
006000        05 MID-IDLEVNR-NY    PIC X(5).                                    
006100*                                 LEVERANT÷RNUMMER                        
006200        05 MID-IDLEVNR-SHIP-NY                                            
006300                             PIC X(5).                                    
006400*                                 SKEPPANDE LEVERANT÷R                    
006500     03 MID-TEXT.                                                         
006600        05 MID-TELEVBSK-TEXT1                                             
006700                             PIC X(80).                                   
006800*                                 LEVERANSBESKEDSINFORMATION              
006900        05 MID-TELEVBSK-TEXT2                                             
007000                             PIC X(80).                                   
007100*                                 LEVERANSBESKEDSINFORMATION              
007200        05 MID-TELEVBSK-TEXT3                                             
007300                             PIC X(80).                                   
007400*                                 LEVERANSBESKEDSINFORMATION              
007500        05 MID-TELEVBSK-TEXT4                                             
007600                             PIC X(80).                                   
007700*                                 LEVERANSBESKEDSINFORMATION              
007800        05 MID-TIBORT        PIC X(5).                                    
007900*                                 ≈R - VECKA - DAG   (≈≈VVD)              
008000*** END OF VILMAII-COPY LENGTH= 617 BYTES                                 
