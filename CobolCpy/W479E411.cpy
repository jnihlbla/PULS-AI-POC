000100 01  W479E411.                                                            
000200*                                 SEKVENSELLKOPIA WDE411                  
000300     03 IDSEGM               PIC X(6).                                    
000400*                                 SEGMENT                                 
000500     03 IDGMTREF.                                                         
000600*                                 GODSMOTTAGAREREFERENS                   
000700        05 IDDISTR           PIC S9(5)           COMP-3.                  
000800*                                 DISTRIKTNUMMER                          
000900        05 IDKUNDNR          PIC S9(7)           COMP-3.                  
001000*                                 KUNDNUMMER                              
001100        05 IDKUNDRF-GRP.                                                  
001200*                                 KUNDENS REFERENS (ORDERID)              
001300           07 IDKUNDRF       PIC X(10).                                   
001400*                                 KUNDENS REFERENS (ORDERID)              
001500           07 IDORDNR5-FILLER REDEFINES IDKUNDRF.                         
001600              09 IDORDNR5    PIC 9(5).                                    
001700*                                 ORDERNUMMER                             
001800              09 FILLER      PIC X(5).                                    
001900           07 IDORDNR7-FILLER REDEFINES IDKUNDRF.                         
002000              09 IDORDNR7    PIC 9(7).                                    
002100*                                 ORDERNUMMER                             
002200              09 FILLER      PIC X(3).                                    
002300     03 IDPRODNR             PIC S9(7)           COMP-3.                  
002400*                                 PRODUKTIONSNUMMER                       
002500     03 IDARTNR              PIC S9(9)           COMP-3.                  
002600*                                 ARTIKELNUMMER                           
002700     03 IDKUNDRF-RO          PIC X(10).                                   
002800*                                 KUND REF PÅ RO                          
002900     03 IDLOPNR-RO           PIC S9(3)           COMP-3.                  
003000*                                 LÖPNUMMER RO/TPO                        
003100*** END OF VILMAII-COPY LENGTH= 44 BYTES                                  
