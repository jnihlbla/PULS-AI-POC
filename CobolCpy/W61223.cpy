000100 01  W61223.                                                              
000200*                                 LEVERANSANMÄRKNING                      
000300*                                 REFILL TRANSFER                         
000400     03 IDDC-SEND            PIC X(2).                                    
000500*                                 SÄNDANDE LAGER                          
000600     03 IDDC-REC             PIC X(2).                                    
000700*                                 MOTTAGANDE LAGER                        
000800     03 IDDISTR              PIC S9(5)           COMP-3.                  
000900*                                 DISTRIKTNUMMER                          
001000     03 IDKUNDNR             PIC S9(7)           COMP-3.                  
001100*                                 KUNDNUMMER                              
001200     03 IDFAKT               PIC S9(7)           COMP-3.                  
001300*                                 FAKTURANUMMER                           
001400     03 IDKUNDRF-GRP.                                                     
001500*                                 KUNDENS REFERENS (ORDERID)              
001600        05 IDKUNDRF          PIC X(10).                                   
001700*                                 KUNDENS REFERENS (ORDERID)              
001800        05 IDORDNR5-FILLER REDEFINES IDKUNDRF.                            
001900           07 IDORDNR5       PIC 9(5).                                    
002000*                                 ORDERNUMMER                             
002100           07 FILLER         PIC X(5).                                    
002200        05 IDORDNR7-FILLER REDEFINES IDKUNDRF.                            
002300           07 IDORDNR7       PIC 9(7).                                    
002400*                                 ORDERNUMMER                             
002500           07 FILLER         PIC X(3).                                    
002600     03 IDKOLLI              PIC S9(5)           COMP-3.                  
002700*                                 KOLLINUMMER                             
002800     03 IDARTNR              PIC S9(9)           COMP-3.                  
002900*                                 ARTIKELNUMMER                           
003000     03 KVLEVANM             PIC S9(7)           COMP-3.                  
003100*                                 LEVERANSANMÄRKNINGSANTAL                
003200     03 KDANMORS             PIC X(2).                                    
003300*                                 ORSAK TILL LEVERANSANMÄRKNING           
003400     03 TIFAKT               PIC S9(7)           COMP-3.                  
003500*                                 FAKTURERINGSDATUM (ÅÅMMDD)              
003600*** END OF VILMAII-COPY LENGTH= 43 BYTES                                  
