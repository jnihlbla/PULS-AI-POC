000100 01  MID-W4I45701.                                                        
000200*                                                                         
000300     03 MID-IDDC-IN          PIC X(2).                                    
000400*                                 IDENTIFIERARE LAGER                     
000500     03 MID-IDDISTR-IN       PIC X(4).                                    
000600*                                 DISTRIKTNUMMER                          
000700     03 MID-INPUT.                                                        
000800        05 MID-KDCMD         OCCURS 3 TIMES                               
000900                             PIC X.                                       
001000*                                 RAD-UPPDATERINGSKOMMANDO                
001100*                                  BLANK  = INGENTING                     
001200*                                  D , B  = DELETE                        
001300*                                  R , Ä  = REPLACE                       
001400*                                  I,N,A  = INSERT                        
001500*                                  S , V  = SELECT                        
001600*                                  P , P  = PRINT                         
001700*                                  C , K  = COPY                          
001800     03 MID-UPD.                                                          
001900        05 MID-IDDISTR-UPD   PIC 9(4).                                    
002000*                                 DISTRIKTNUMMER                          
002100        05 MID-RETRPFAC-AIR-UPD                                           
002200                             PIC X(6).                                    
002300        05 MID-PRWEIGHT-AIR-UPD                                           
002400                             PIC 9(5).                                    
002500        05 MID-PRHAZMAT-AIR-UPD                                           
002600                             PIC 9(5).                                    
002700        05 MID-RETRPFAC-BOAT-UPD                                          
002800                             PIC X(6).                                    
002900        05 MID-PRWEIGHT-BOAT-UPD                                          
003000                             PIC 9(5).                                    
003100        05 MID-PRHAZMAT-BOAT-UPD                                          
003200                             PIC 9(5).                                    
003300        05 MID-RETRPFAC-ROAD-UPD                                          
003400                             PIC X(6).                                    
003500        05 MID-PRWEIGHT-ROAD-UPD                                          
003600                             PIC 9(5).                                    
003700        05 MID-PRHAZMAT-ROAD-UPD                                          
003800                             PIC 9(5).                                    
003900        05 MID-REINSFAC-AIR-UPD2                                          
004000                             PIC 9(7).                                    
004100        05 MID-REINSFAC-BOAT-UPD2                                         
004200                             PIC 9(7).                                    
004300        05 MID-REINSFAC-ROAD-UPD2                                         
004400                             PIC 9(7).                                    
004500*** END OF VILMAII-COPY LENGTH= 82 BYTES                                  
