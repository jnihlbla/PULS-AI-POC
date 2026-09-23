000100 01  MOD-W30391O1.                                                        
000200*                                 MODCOPYTEXT TILL W3039100.              
000300     03 MOD-IDDISTR          PIC 9(4).                                    
000400*                                 DISTRIKTNUMMER                          
000500     03 MOD-IDKUNDNR         PIC 9(6).                                    
000600*                                 KUNDNUMMER                              
000700     03 MOD-IDBUNDLE-GRP.                                                 
000800*                                 QUERY  REFERENS (ORDERID/RAPPN)         
000900        05 MOD-IDBUNDLE      PIC X(15).                                   
001000*                                 BUNDLE ID                               
001100        05 MOD-IDORDNR7-FILLER REDEFINES MOD-IDBUNDLE.                    
001200           07 MOD-IDORDNR7   PIC 9(7).                                    
001300*                                 ORDERNUMMER                             
001400           07 FILLER         PIC X(8).                                    
001500        05 MOD-IDRAPPNR-FILLER REDEFINES MOD-IDBUNDLE.                    
001600           07 MOD-IDRAPPNR   PIC 9(7).                                    
001700*                                 RAPPORT NUMMER                          
001800           07 FILLER         PIC X(8).                                    
001900     03 MOD-IDPRQUES         PIC 9(7).                                    
002000*                                 PRISFRÅGA NR                            
002100     03 MOD-IDARTNR          PIC 9(8).                                    
002200*                                 ARTIKELNUMMER                           
002300     03 MOD-KDORDKL          PIC 9.                                       
002400*                                 ORDERKLASS                              
002500     03 MOD-KVBEART          PIC 9(6).                                    
002600*                                 BESTÄLLT ANTAL STYCKEN                  
002700*** END OF VILMAII-COPY LENGTH= 47 BYTES                                  
