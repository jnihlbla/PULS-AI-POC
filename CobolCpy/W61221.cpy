000100 01  W61221.                                                              
000200*                                 OUTPUT FILE WITH CONTAINERS             
000300*                                 TO SEND TO PROJECT44.                   
000400     03 DABERANK             PIC 9(8).                                    
000500*                                 BERÄKNAD ANKOMSTDATUM                   
000600     03 IDFAKT               PIC S9(7)           COMP-3.                  
000700*                                 FAKTURANUMMER                           
000800     03 IDDISTR              PIC S9(5)           COMP-3.                  
000900*                                 DISTRIKTNUMMER                          
001000     03 IDKUNDNR             PIC S9(7)           COMP-3.                  
001100*                                 KUNDNUMMER                              
001200     03 IDKUNDRF             PIC X(10).                                   
001300*                                 KUNDENS REFERENS (ORDERID)              
001400     03 IDDC-REC             PIC X(2).                                    
001500*                                 MOTTAGANDE LAGER                        
001600     03 IDDC-SEND            PIC X(2).                                    
001700*                                 SÄNDANDE LAGER                          
001800     03 IDLBBET              PIC X(12).                                   
001900*                                 LASTBÄRARBETECKNING                     
002000     03 IDBOKN               PIC X(15).                                   
002100*                                 BOKNINGSNUMMER                          
002200     03 BETRPFIR             PIC X(15).                                   
002300*                                 TRANSPORTFIRMANS NAMN                   
002400     03 IDSUBSCR             PIC S9(11)          COMP-3.                  
002500*                                 SUBSCRIPTION ID FROM PROJECT44          
002600*** END OF VILMAII-COPY LENGTH= 81 BYTES                                  
