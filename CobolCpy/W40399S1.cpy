000100 01  MID-W4I39901.                                                        
000200*                                 KOMMENTAR (SVENSKA)                     
000300*                                 31 POS / RAD                            
000400     03 MID-IDANSTNR         PIC X(5).                                    
000500*                                 ANSTÄLLNINGSNUMMER                      
000600     03 MID-IDDISTR          PIC X(4).                                    
000700*                                 DISTRIKTNUMMER                          
000800     03 MID-IDKUNDNR         PIC X(6).                                    
000900*                                 KUNDNUMMER                              
001000     03 MID-IDORDNR          PIC X(5).                                    
001100*                                 ORDERNUMMER UTGÅR PD90                  
001200     03 MID-IDPRODNR         PIC X(7).                                    
001300*                                 PRODUKTIONSNUMMER                       
001400     03 MID-IDDC             PIC X(2).                                    
001500*                                 IDENTIFIERARE LAGER                     
001600     03 MID-IDFAKT-GNB       PIC X(8).                                    
001700*                                 FAKTURANUMMER GNB                       
001800     03 MID-IDSHIPM          PIC 9(7).                                    
001900*                                 SKEPPNINGSNUMMER                        
002000     03 MID-RAD              OCCURS 90 TIMES.                             
002100        05 MID-IDRADNR       PIC X(4).                                    
002200*                                 RADNUMMER                               
002300        05 MID-KVLEVART      PIC X(6).                                    
002400*                                 LEVERERAT ANTAL STYCK                   
002500*** END OF VILMAII-COPY LENGTH= 944 BYTES                                 
