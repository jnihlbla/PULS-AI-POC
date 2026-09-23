000100 01  MID-W2I35701.                                                        
000200*                                 MIDCOPYTEXT 2357                        
000300     03 MID-IDARTNR-IN       PIC X(9).                                    
000400*                                 ARTIKELNUMMER                           
000500     03 MID-IDARTNR-UT       PIC X(9).                                    
000600*                                 ARTIKELNUMMER                           
000700     03 MID-IDDC-IN          PIC X(2).                                    
000800*                                 IDENTIFIERARE LAGER                     
000900     03 MID-IDDC-UT          PIC X(2).                                    
001000*                                 IDENTIFIERARE LAGER                     
001100     03 MID-INPUT-GRP.                                                    
001200*                                 MID-INDATA 2357                         
001300        05 MID-INPUT         OCCURS 4 TIMES.                              
001400*                                 MID-INDATA 2357                         
001500           07 MID-KDCMD      PIC X.                                       
001600*                                 RAD-UPPDATERINGSKOMMANDO                
001700*                                  BLANK  = INGENTING                     
001800*                                  D , B  = DELETE                        
001900*                                  R , Ä  = REPLACE                       
002000*                                  I,N,A  = INSERT                        
002100*                                  S , V  = SELECT                        
002200*                                  P , P  = PRINT                         
002300*                                  C , K  = COPY                          
002400           07 MID-FLORDSP-EJRO                                            
002500                             PIC X.                                       
002600*                                 ORDERSPÄRR EJ RESTNOTERING              
002700           07 MID-IDPSN-DC   PIC 9(3).                                    
002800*                                 PROPER SHIPPING NAME PER XDC            
002900           07 MID-DAPUBL     PIC 9(5).                                    
003000*                                 ÅR - VECKA - DAG   (ÅÅVVD)              
003100*** END OF VILMAII-COPY LENGTH= 62 BYTES                                  
