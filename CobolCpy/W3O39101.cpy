000100 01  MOD-W3O39101.                                                        
000200*                                 MODCOPYTEXT TILL W3039100.              
000300     03 MOD-IDDISTR          PIC 9(4).                                    
000400*                                 DISTRIKTNUMMER                          
000500     03 MOD-IDKUNDNR         PIC 9(7).                                    
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
001900     03 MOD-IDPRQUES-GRP.                                                 
002000*                                 PRISFRÅGENUMMER                         
002100        05 MOD-IDPRQORD      PIC S9(7)           COMP-3.                  
002200*                                 PRISFRÅGA NR PER ORDER                  
002300        05 MOD-IDPRQRAD      PIC S9(5)           COMP-3.                  
002400*                                 PRISFRÅGA NR ORAD                       
002500     03 MOD-IDARTNR          PIC 9(9).                                    
002600*                                 ARTIKELNUMMER                           
002700     03 MOD-KDORDKL          PIC X.                                       
002800*                                 ORDERKLASS                              
002900     03 MOD-KVBEART          PIC 9(7).                                    
003000*                                 BESTÄLLT ANTAL STYCKEN                  
003100*** END OF VILMAII-COPY LENGTH= 50 BYTES                                  
