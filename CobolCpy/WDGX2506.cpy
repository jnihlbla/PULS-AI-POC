000100 01  2506-WDGX2506.                                                       
000200*                                 REFILL BYPASS ORDER                     
000300     03 2506-IDUSER          PIC X(8).                                    
000400*                                 ANVÄNDARENS SÄKERHETS ID                
000500*                                 USER SECURITY-IDENTITY                  
000600     03 2506-IDDISTR         PIC S9(5)           COMP-3.                  
000700*                                 DISTRIKTNUMMER                          
000800*                                 DISTRICT NUMBER                         
000900     03 2506-IDKUNDNR        PIC S9(7)           COMP-3.                  
001000*                                 KUNDNUMMER                              
001100*                                 CUSTOMER NO                             
001200     03 2506-IDKUNDRF        PIC X(10).                                   
001300*                                 KUNDENS REFERENS (ORDERID)              
001400*                                 CUSTOMER REFERENCE (ORDER ID)           
001500     03 2506-IDORDNR7-FILLER REDEFINES 2506-IDKUNDRF.                     
001600        05 2506-IDORDNR7     PIC 9(7).                                    
001700*                                 ORDERNUMMER                             
001800*                                 ORDER NUMBER                            
001900        05 FILLER            PIC X(3).                                    
002000     03 2506-IDORDNR5-FILLER REDEFINES 2506-IDKUNDRF.                     
002100        05 2506-IDORDNR5     PIC 9(5).                                    
002200*                                 ORDERNUMMER                             
002300*                                 ORDER NUMBER                            
002400        05 FILLER            PIC X(5).                                    
002500     03 FILLER               PIC X(5).                                    
002600*** END OF VILMAII-COPY LENGTH= 30 BYTES                                  
