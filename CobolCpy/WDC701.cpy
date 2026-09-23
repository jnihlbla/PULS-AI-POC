000100 01  PRQ-WDC701.                                                          
000200*                                 DDI PRIS FRÅGA                          
000300*                                 BUNT ID                                 
000400*                                 FYSISK NYCKEL: WDC701KY                 
000500*                                 (IDDISTR IDKUNDNR IDBUNDLE-GRP)         
000600     03 PRQ-IDDISTR          PIC 9(4).                                    
000700*                                 DISTRIKTNUMMER                          
000800*                                 DISTRICT NUMBER                         
000900     03 PRQ-IDKUNDNR         PIC 9(7).                                    
001000*                                 KUNDNUMMER                              
001100*                                 CUSTOMER NO                             
001200     03 PRQ-IDBUNDLE-GRP.                                                 
001300*                                 QUERY  REFERENS (ORDERID/RAPPN)         
001400*                                 QUERY REFERENCE (ORDERID/REPNO)         
001500        05 PRQ-IDBUNDLE      PIC X(15).                                   
001600*                                 BUNDLE ID                               
001700*                                 BUNDLE ID                               
001800        05 PRQ-IDORDNR7-FILLER REDEFINES PRQ-IDBUNDLE.                    
001900           07 PRQ-IDORDNR7   PIC 9(7).                                    
002000*                                 ORDERNUMMER                             
002100*                                 ORDER NUMBER                            
002200           07 FILLER         PIC X(8).                                    
002300        05 PRQ-IDRAPPNR-FILLER REDEFINES PRQ-IDBUNDLE.                    
002400           07 PRQ-IDRAPPNR   PIC 9(7).                                    
002500*                                 RAPPORT NUMMER                          
002600*                                 DISCREPANCY REPORT NUMBER               
002700           07 FILLER         PIC X(8).                                    
002800        05 PRQ-IDORDER-FILLER REDEFINES PRQ-IDBUNDLE.                     
002900           07 PRQ-IDORDER    PIC 9(7).                                    
003000*                                 VOLVO PARTS ORDERNUMMER                 
003100*                                 VOLVO PARTS ORDER NUMBER                
003200           07 FILLER         PIC X(8).                                    
003300*** END OF VILMAII-COPY LENGTH= 26 BYTES                                  
