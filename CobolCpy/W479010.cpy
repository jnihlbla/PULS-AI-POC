000100 01  W479010.                                                             
000200*                                 RENSNINGSPOSTER WDE4 + WDQ2,3           
000300     03 IDPTYP               PIC X(3).                                    
000400*                                 POSTTYP                                 
000500     03 IDORDER              PIC S9(7)           COMP-3.                  
000600*                                 VOLVO PARTS ORDERNUMMER                 
000700     03 IDDC                 PIC X(2).                                    
000800*                                 IDENTIFIERARE LAGER                     
000900     03 IDPRODNR             PIC S9(7)           COMP-3.                  
001000*                                 PRODUKTIONSNUMMER                       
001100     03 IDPLKLST             PIC S9(3)           COMP-3.                  
001200*                                 PLOCKLISTNUMMER                         
001300     03 IDGMTREF.                                                         
001400*                                 GODSMOTTAGAREREFERENS                   
001500        05 IDDISTR           PIC S9(5)           COMP-3.                  
001600*                                 DISTRIKTNUMMER                          
001700        05 IDKUNDNR          PIC S9(7)           COMP-3.                  
001800*                                 KUNDNUMMER                              
001900        05 IDKUNDRF-GRP.                                                  
002000*                                 KUNDENS REFERENS (ORDERID)              
002100           07 IDKUNDRF       PIC X(10).                                   
002200*                                 KUNDENS REFERENS (ORDERID)              
002300           07 IDORDNR5-FILLER REDEFINES IDKUNDRF.                         
002400              09 IDORDNR5    PIC 9(5).                                    
002500*                                 ORDERNUMMER                             
002600              09 FILLER      PIC X(5).                                    
002700           07 IDORDNR7-FILLER REDEFINES IDKUNDRF.                         
002800              09 IDORDNR7    PIC 9(7).                                    
002900*                                 ORDERNUMMER                             
003000              09 FILLER      PIC X(3).                                    
003100     03 IDLEVNR              PIC X(5).                                    
003200*                                 LEVERANTÖRNUMMER                        
003300*** END OF VILMAII-COPY LENGTH= 37 BYTES                                  
