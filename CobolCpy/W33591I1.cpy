000100 01  MID-W33591I1.                                                        
000200*                                 MID-COPYTEXT FÖR W3359100               
000300     03 MID-IDDISTR          PIC 9(4).                                    
000400*                                 DISTRIKTNUMMER                          
000500     03 MID-IDKUNDNR         PIC 9(7).                                    
000600*                                 KUNDNUMMER                              
000700     03 MID-IDBUNDLE-GRP.                                                 
000800*                                 QUERY  REFERENS (ORDERID/RAPPN)         
000900        05 MID-IDBUNDLE      PIC X(15).                                   
001000*                                 BUNDLE ID                               
001100        05 MID-IDORDNR7-FILLER REDEFINES MID-IDBUNDLE.                    
001200           07 MID-IDORDNR7   PIC 9(7).                                    
001300*                                 ORDERNUMMER                             
001400           07 FILLER         PIC X(8).                                    
001500        05 MID-IDRAPPNR-FILLER REDEFINES MID-IDBUNDLE.                    
001600           07 MID-IDRAPPNR   PIC 9(7).                                    
001700*                                 RAPPORT NUMMER                          
001800           07 FILLER         PIC X(8).                                    
001900     03 MID-IDPRQUES-GRP.                                                 
002000*                                 PRISFRÅGENUMMER                         
002100        05 MID-IDPRQORD      PIC S9(7)           COMP-3.                  
002200*                                 PRISFRÅGA NR PER ORDER                  
002300        05 MID-IDPRQRAD      PIC S9(5)           COMP-3.                  
002400*                                 PRISFRÅGA NR ORAD                       
002500*** END OF VILMAII-COPY LENGTH= 33 BYTES                                  
