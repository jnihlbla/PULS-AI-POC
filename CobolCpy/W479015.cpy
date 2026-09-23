000100 01  W479015-CTX.                                                         
000200*                                 OAVSLUTADE ORDER TILL LISTA             
000300     03 IDPTYP               PIC X(3).                                    
000400*                                 POSTTYP                                 
000500     03 IDORDER              PIC S9(7)           COMP-3.                  
000600*                                 VOLVO PARTS ORDERNUMMER                 
000700     03 IDGMTREF.                                                         
000800*                                 GODSMOTTAGAREREFERENS                   
000900        05 IDDISTR           PIC S9(5)           COMP-3.                  
001000*                                 DISTRIKTNUMMER                          
001100        05 IDKUNDNR          PIC S9(7)           COMP-3.                  
001200*                                 KUNDNUMMER                              
001300        05 IDKUNDRF          PIC X(10).                                   
001400*                                 KUNDENS REFERENS (ORDERID)              
001500        05 IDORDNR5-FILLER REDEFINES IDKUNDRF.                            
001600           07 IDORDNR5       PIC 9(5).                                    
001700*                                 ORDERNUMMER                             
001800           07 FILLER         PIC X(5).                                    
001900        05 IDORDNR7-FILLER REDEFINES IDKUNDRF.                            
002000           07 IDORDNR7       PIC 9(7).                                    
002100*                                 ORDERNUMMER                             
002200           07 FILLER         PIC X(3).                                    
002300     03 IDSYSTEM             PIC X(4).                                    
002400*                                 SKAPANDE SYSTEMNUMMER                   
002500     03 IDUSER               PIC X(8).                                    
002600*                                 ANVÄNDARENS SÄKERHETS ID                
002700     03 KDORDKL              PIC S9              COMP-3.                  
002800*                                 ORDERKLASS                              
002900     03 TIREGDAT             PIC S9(7)           COMP-3.                  
003000*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
003100     03 IDDC                 PIC X(2).                                    
003200*                                 IDENTIFIERARE LAGER                     
003300*** END OF VILMAII-COPY LENGTH= 43 BYTES                                  
