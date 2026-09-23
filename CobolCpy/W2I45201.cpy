000100 01  MID-W2I45201-CTX.                                                    
000200*                                 MID-COPYTEXT FÖR W2045200               
000300     03 MID-KDPRODSL-IN      PIC X(2).                                    
000400*                                 PRODUKTSLAG                             
000500     03 MID-KDPRODSL-UT      PIC X(2).                                    
000600*                                 PRODUKTSLAG                             
000700     03 MID-IDLANDX2-IN      PIC X(2).                                    
000800*                                 2-STÄLLIG LANDSBETECKNINGSKOD           
000900     03 MID-IDLANDX2-UT      PIC X(2).                                    
001000*                                 2-STÄLLIG LANDSBETECKNINGSKOD           
001100     03 MID-W2I45201-001-GRP.                                             
001200*                                 INPUT FOR UPDATE                        
001300        05 MID-IDFKNGRP-FOM-IN                                            
001400                             PIC 9(4).                                    
001500*                                 FUNKTIONSGRUPP                          
001600        05 MID-IDFKNGRP-TOM-IN                                            
001700                             PIC 9(4).                                    
001800*                                 FUNKTIONSGRUPP                          
001900        05 MID-IDANSK-IN     PIC 9(3).                                    
002000*                                 ANSKAFFARNUMMER                         
002100        05 MID-KDCMD-IN      PIC X.                                       
002200*                                 RAD-UPPDATERINGSKOMMANDO                
002300*                                  BLANK  = INGENTING                     
002400*                                  D , B  = DELETE                        
002500*                                  R , Ä  = REPLACE                       
002600*                                  I,N,A  = INSERT                        
002700*                                  S , V  = SELECT                        
002800*                                  P , P  = PRINT                         
002900*                                  C , K  = COPY                          
003000*** END OF VILMAII-COPY LENGTH= 20 BYTES                                  
