000100 01  W440702.                                                             
000200*                                 ORDERBEKRÄFTELSER FÖR REFILLDIS         
000300*                                 TRIKT (WDQ1)                            
000400     03 IDGMTREF.                                                         
000500*                                 GODSMOTTAGAREREFERENS                   
000600        05 IDDISTR           PIC S9(5)           COMP-3.                  
000700*                                 DISTRIKTNUMMER                          
000800        05 IDKUNDNR          PIC S9(7)           COMP-3.                  
000900*                                 KUNDNUMMER                              
001000        05 IDKUNDRF-GRP.                                                  
001100*                                 KUNDENS REFERENS (ORDERID)              
001200           07 IDKUNDRF       PIC X(10).                                   
001300*                                 KUNDENS REFERENS (ORDERID)              
001400           07 IDORDNR5-FILLER REDEFINES IDKUNDRF.                         
001500              09 IDORDNR5    PIC 9(5).                                    
001600*                                 ORDERNUMMER                             
001700              09 FILLER      PIC X(5).                                    
001800           07 IDORDNR7-FILLER REDEFINES IDKUNDRF.                         
001900              09 IDORDNR7    PIC 9(7).                                    
002000*                                 ORDERNUMMER                             
002100              09 FILLER      PIC X(3).                                    
002200     03 IDARTNR              PIC S9(9)           COMP-3.                  
002300*                                 ARTIKELNUMMER                           
002400     03 KDORDBEK             PIC 9(2).                                    
002500*                                 ORDERBEKRÄFTELSEKOD                     
002600     03 KVBEART              PIC S9(7)           COMP-3.                  
002700*                                 BESTÄLLT ANTAL STYCKEN                  
002800     03 KVBEART-Q            PIC S9(7)           COMP-3.                  
002900*                                 BESTÄLLT KVANTANPASSAT ANTAL            
003000     03 KVANNANT             PIC S9(7)           COMP-3.                  
003100*                                 ANNULLERAT ANTAL ARTIKLAR               
003200     03 TIORDREG-STA         PIC S9(7)           COMP-3.                  
003300*                                 ORDERREGISTRERINGSDATUM  ÅÅMMDD         
003400     03 TIORDREG-STO         PIC S9(7)           COMP-3.                  
003500*                                 ORDERREGISTRERINGSDATUM  ÅÅMMDD         
003600*** END OF VILMAII-COPY LENGTH= 44 BYTES                                  
