000100 01  MID-W2I37701.                                                        
000200*                                 MIDCOPYTEXT 2377                        
000300     03 MID-IDARTNR-IN       PIC X(9).                                    
000400*                                 ARTIKELNUMMER                           
000500     03 MID-IDARTNR-UT       PIC X(9).                                    
000600*                                 ARTIKELNUMMER                           
000700     03 MID-IDDC-IN          PIC X(2).                                    
000800*                                 IDENTIFIERARE LAGER                     
000900     03 MID-IDDC-UT          PIC X(2).                                    
001000*                                 IDENTIFIERARE LAGER                     
001100     03 MID-KDCMD            PIC X.                                       
001200*                                 RAD-UPPDATERINGSKOMMANDO                
001300*                                  BLANK  = INGENTING                     
001400*                                  D , B  = DELETE                        
001500*                                  R , Ä  = REPLACE                       
001600*                                  I,N,A  = INSERT                        
001700*                                  S , V  = SELECT                        
001800*                                  P , P  = PRINT                         
001900*                                  C , K  = COPY                          
002000     03 MID-TIAAVVD-DC       PIC X(5).                                    
002100*                                 ÅR - VECKA - DAG   (ÅÅVVD)              
002200     03 MID-FLORDSP-EJRO     PIC X.                                       
002300*                                 ORDERSPÄRR EJ RESTNOTERING              
002400     03 MID-KDARTURS-DC      PIC X(2).                                    
002500*                                 ARTIKELURSPRUNGSKOD                     
002600     03 MID-IDPSN-DC         PIC X(3).                                    
002700*                                 PROPER SHIPPING NAME PER XDC            
002800*** END OF VILMAII-COPY LENGTH= 34 BYTES                                  
