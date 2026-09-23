000100 01  BUM-W414BUMA.                                                        
000200*                                 BUMPERS                                 
000300*                                 SKAPAS VID BUMPERSTRYNING               
000400*                                 DIRLEV/DC-21                            
000500*                                 ANVÄNDS VID TRANSAKTION-                
000600*                                 SKAPANDE TILL BUMPERSERVICE-UPP         
000700*                                 F.                                      
000800     03 BUM-IDLEVNR          PIC X(5).                                    
000900*                                 LEVERANTÖRNUMMER                        
001000     03 BUM-IDDISTR          PIC S9(5)           COMP-3.                  
001100*                                 DISTRIKTNUMMER                          
001200     03 BUM-IDKUNDNR         PIC S9(7)           COMP-3.                  
001300*                                 KUNDNUMMER                              
001400     03 BUM-IDKUNDRF         PIC X(10).                                   
001500*                                 KUNDENS REFERENS (ORDERID)              
001600     03 BUM-IDARTNR          PIC S9(9)           COMP-3.                  
001700*                                 ARTIKELNUMMER                           
001800     03 BUM-KVBEART          PIC S9(7)           COMP-3.                  
001900*                                 BESTÄLLT ANTAL STYCKEN                  
002000     03 BUM-IDDC             PIC X(2).                                    
002100*                                 IDENTIFIERARE LAGER                     
002200     03 BUM-IDDC-STEER       PIC X(2).                                    
002300*                                 IDENTIFIERARE LAGER                     
002400*** END OF VILMAII-COPY LENGTH= 35 BYTES                                  
