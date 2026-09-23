000100 01  002-W4766402.                                                        
000200*                                 CASE   FÖR FAKTURATRANSAR FRÅN          
000300*                                 BILL-IT                                 
000400     03 002-IDPTYP           PIC X(3).                                    
000500*                                 POSTTYP                                 
000600     03 002-IDFAKT           PIC S9(7)           COMP-3.                  
000700*                                 FAKTURANUMMER                           
000800     03 002-IDDISTR          PIC S9(5)           COMP-3.                  
000900*                                 DISTRIKTNUMMER                          
001000     03 002-IDKUNDNR         PIC S9(7)           COMP-3.                  
001100*                                 KUNDNUMMER                              
001200     03 002-IDDC             PIC X(2).                                    
001300*                                 IDENTIFIERARE LAGER                     
001400     03 002-IDKUNDRF         PIC X(10).                                   
001500*                                 KUNDENS REFERENS (ORDERID)              
001600     03 002-IDKOLLI          PIC S9(5)           COMP-3.                  
001700*                                 KOLLINUMMER                             
001800     03 002-KDKOLLI          PIC X(8).                                    
001900*                                 KOLLIKOD                                
002000     03 002-IDLBBET          PIC X(12).                                   
002100*                                 LASTBÄRARBETECKNING                     
002200*** END OF VILMAII-COPY LENGTH= 49 BYTES                                  
