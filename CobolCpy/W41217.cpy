000100 01  W41217.                                                              
000200*                                 SAMTLIGA EJ LÖSTA KLASS 0               
000300*                                 RADER.                                  
000400*                                 (FÖR VOR UPPFÖLJNING, FVB               
000500*                                  TILL VIOS).                            
000600     03 TIUPPFV              PIC S9(5)           COMP-3.                  
000700*                                 ÅR - VECKA  (ÅÅVV)                      
000800     03 IDARTNR              PIC S9(9)           COMP-3.                  
000900*                                 ARTIKELNUMMER                           
001000     03 IDGMTREF.                                                         
001100*                                 GODSMOTTAGAREREFERENS                   
001200        05 IDDISTR           PIC S9(5)           COMP-3.                  
001300*                                 DISTRIKTNUMMER                          
001400        05 IDKUNDNR          PIC S9(7)           COMP-3.                  
001500*                                 KUNDNUMMER                              
001600        05 IDKUNDRF-GRP.                                                  
001700*                                 KUNDENS REFERENS (ORDERID)              
001800           07 IDKUNDRF       PIC X(10).                                   
001900*                                 KUNDENS REFERENS (ORDERID)              
002000           07 IDORDNR5-FILLER REDEFINES IDKUNDRF.                         
002100              09 IDORDNR5    PIC 9(5).                                    
002200*                                 ORDERNUMMER                             
002300              09 FILLER      PIC X(5).                                    
002400           07 IDORDNR7-FILLER REDEFINES IDKUNDRF.                         
002500              09 IDORDNR7    PIC 9(7).                                    
002600*                                 ORDERNUMMER                             
002700              09 FILLER      PIC X(3).                                    
002800     03 KVBEART              PIC S9(7)           COMP-3.                  
002900*                                 BESTÄLLT ANTAL STYCKEN                  
003000     03 IDANSK               PIC S9(3)           COMP-3.                  
003100*                                 ANSKAFFARNUMMER                         
003200     03 IDLEVNR              PIC X(5).                                    
003300*                                 LEVERANTÖRNUMMER                        
003400     03 TIREGDAT             PIC S9(7)           COMP-3.                  
003500*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
003600*** END OF VILMAII-COPY LENGTH= 40 BYTES                                  
