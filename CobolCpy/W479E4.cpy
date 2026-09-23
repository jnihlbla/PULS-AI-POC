000100 01  W479E4.                                                              
000200*                                 RENSNINGSPOSTER WDE4                    
000300*                                 RANDOMKEY SORTERADE                     
000400     03 IDRANDOM             PIC X(4).                                    
000500*                                 RANDOMISERINGSNYCKEL                    
000600     03 IDGMTREF.                                                         
000700*                                 GODSMOTTAGAREREFERENS                   
000800        05 IDDISTR           PIC S9(5)           COMP-3.                  
000900*                                 DISTRIKTNUMMER                          
001000        05 IDKUNDNR          PIC S9(7)           COMP-3.                  
001100*                                 KUNDNUMMER                              
001200        05 IDKUNDRF-GRP.                                                  
001300*                                 KUNDENS REFERENS (ORDERID)              
001400           07 IDKUNDRF       PIC X(10).                                   
001500*                                 KUNDENS REFERENS (ORDERID)              
001600           07 IDORDNR5-FILLER REDEFINES IDKUNDRF.                         
001700              09 IDORDNR5    PIC 9(5).                                    
001800*                                 ORDERNUMMER                             
001900              09 FILLER      PIC X(5).                                    
002000           07 IDORDNR7-FILLER REDEFINES IDKUNDRF.                         
002100              09 IDORDNR7    PIC 9(7).                                    
002200*                                 ORDERNUMMER                             
002300              09 FILLER      PIC X(3).                                    
002400     03 IDPRODNR             PIC S9(7)           COMP-3.                  
002500*                                 PRODUKTIONSNUMMER                       
002600     03 IDPLKLST             PIC S9(3)           COMP-3.                  
002700*                                 PLOCKLISTNUMMER                         
002800*** END OF VILMAII-COPY LENGTH= 27 BYTES                                  
