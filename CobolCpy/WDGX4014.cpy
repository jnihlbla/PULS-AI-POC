000100 01  4014-WDGX4014.                                                       
000200*                                 VIPS ORDERBEKRÄFTELSERADER              
000300*                                 FYSISK NYCKEL: IDGMTREF                 
000400     03 4014-IDGMTREF.                                                    
000500*                                 GODSMOTTAGAREREFERENS                   
000600*                                 GOODS RECEIVER REFERENS                 
000700        05 4014-IDDISTR      PIC S9(5)           COMP-3.                  
000800*                                 DISTRIKTNUMMER                          
000900*                                 DISTRICT NUMBER                         
001000        05 4014-IDKUNDNR     PIC S9(7)           COMP-3.                  
001100*                                 KUNDNUMMER                              
001200*                                 CUSTOMER NO                             
001300        05 4014-IDKUNDRF-GRP.                                             
001400*                                 KUNDENS REFERENS (ORDERID)              
001500*                                 CUSTOMER REFERENCE (ORDER ID)           
001600           07 4014-IDKUNDRF  PIC X(10).                                   
001700*                                 KUNDENS REFERENS (ORDERID)              
001800*                                 CUSTOMER REFERENCE (ORDER ID)           
001900           07 4014-IDORDNR5-FILLER REDEFINES 4014-IDKUNDRF.               
002000              09 4014-IDORDNR5                                            
002100                             PIC 9(5).                                    
002200*                                 ORDERNUMMER                             
002300*                                 ORDER NUMBER                            
002400              09 FILLER      PIC X(5).                                    
002500           07 4014-IDORDNR7-FILLER REDEFINES 4014-IDKUNDRF.               
002600              09 4014-IDORDNR7                                            
002700                             PIC 9(7).                                    
002800*                                 ORDERNUMMER                             
002900*                                 ORDER NUMBER                            
003000              09 FILLER      PIC X(3).                                    
003100*** END OF VILMAII-COPY LENGTH= 17 BYTES                                  
