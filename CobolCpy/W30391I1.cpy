000100 01  MID-W30391I1.                                                        
000200*                                 MID-COPYTEXT FÖR W3039100               
000300     03 MID-IDDISTR          PIC 9(4).                                    
000400*                                 DISTRIKTNUMMER                          
000500     03 MID-IDKUNDNR         PIC 9(6).                                    
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
001900        05 MID-IDORDER-FILLER REDEFINES MID-IDBUNDLE.                     
002000           07 MID-IDORDER    PIC X(7).                                    
002100*                                 VOLVO PARTS ORDERNUMMER                 
002200           07 FILLER         PIC X(8).                                    
002300     03 MID-IDPRQUES         PIC 9(7).                                    
002400*                                 PRISFRÅGA NR                            
002500*** END OF VILMAII-COPY LENGTH= 32 BYTES                                  
