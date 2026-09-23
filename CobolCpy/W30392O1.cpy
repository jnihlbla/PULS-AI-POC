000100 01  MOD-W30392O1.                                                        
000200*                                 MOD-COPYTEXT FÖR W3039200               
000300     03 MOD-IDDISTR          PIC Z(3)9.                                   
000400*                                 DISTRIKTNUMMER                          
000500     03 MOD-IDKUNDNR         PIC Z(5)9.                                   
000600*                                 KUNDNUMMER                              
000700     03 MOD-IDBUNDLE-GRP.                                                 
000800*                                 QUERY  REFERENS (ORDERID/RAPPN)         
000900        05 MOD-IDBUNDLE      PIC X(15).                                   
001000*                                 BUNDLE ID                               
001100        05 MOD-IDORDNR7-FILLER REDEFINES MOD-IDBUNDLE.                    
001200           07 MOD-IDORDNR7   PIC Z(6)9.                                   
001300*                                 ORDERNUMMER                             
001400           07 FILLER         PIC X(8).                                    
001500        05 MOD-IDRAPPNR-FILLER REDEFINES MOD-IDBUNDLE.                    
001600           07 MOD-IDRAPPNR   PIC Z(6)9.                                   
001700*                                 RAPPORT NUMMER                          
001800           07 FILLER         PIC X(8).                                    
001900     03 MOD-IDPRQUES         PIC Z(6)9.                                   
002000*                                 PRISFRÅGA NR                            
002100*** END OF VILMAII-COPY LENGTH= 32 BYTES                                  
