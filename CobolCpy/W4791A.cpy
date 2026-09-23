000100 01  W4791A.                                                              
000200*                                 RENSNINGSPOSTER WDA5                    
000300     03 IDGMTREF.                                                         
000400*                                 GODSMOTTAGAREREFERENS                   
000500        05 IDDISTR           PIC S9(5)           COMP-3.                  
000600*                                 DISTRIKTNUMMER                          
000700        05 IDKUNDNR          PIC S9(7)           COMP-3.                  
000800*                                 KUNDNUMMER                              
000900        05 IDKUNDRF-GRP.                                                  
001000*                                 KUNDENS REFERENS (ORDERID)              
001100           07 IDKUNDRF       PIC X(10).                                   
001200*                                 KUNDENS REFERENS (ORDERID)              
001300           07 IDORDNR5-FILLER REDEFINES IDKUNDRF.                         
001400              09 IDORDNR5    PIC 9(5).                                    
001500*                                 ORDERNUMMER                             
001600              09 FILLER      PIC X(5).                                    
001700           07 IDORDNR7-FILLER REDEFINES IDKUNDRF.                         
001800              09 IDORDNR7    PIC 9(7).                                    
001900*                                 ORDERNUMMER                             
002000              09 FILLER      PIC X(3).                                    
002100     03 IDARTNR              PIC S9(9)           COMP-3.                  
002200*                                 ARTIKELNUMMER                           
002300     03 IDLOPNR              PIC S9(3)           COMP-3.                  
002400*                                 LÖPNUMMER                               
002500*** END OF VILMAII-COPY LENGTH= 24 BYTES                                  
