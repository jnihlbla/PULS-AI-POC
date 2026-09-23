000100 01  W61212.                                                              
000200*                                 URVAL FRÅN WDL6                         
000300*                                 INLEVERANS HISTORIK SDC AVISERA         
000400*                                 T ANTAL INTE LIKA MED MOTTAGET          
000500*                                 ANTAL                                   
000600     03 IDDC                 PIC X(2).                                    
000700*                                 IDENTIFIERARE LAGER                     
000800     03 IDARTNR              PIC S9(9)           COMP-3.                  
000900*                                 ARTIKELNUMMER                           
001000     03 IDFAKT               PIC S9(7)           COMP-3.                  
001100*                                 FAKTURANUMMER                           
001200     03 KVANTMOT             PIC S9(7)           COMP-3.                  
001300*                                 ANTAL MOTTAGET                          
001400     03 KVAVIS               PIC S9(7)           COMP-3.                  
001500*                                 AVISERAT ANTAL                          
001600     03 TIINLINL             PIC S9(7)           COMP-3.                  
001700*                                 RAPPORTERINGSDATUM INLAGD (R32)         
001800     03 IDKOLLI              PIC S9(5)           COMP-3.                  
001900*                                 KOLLINUMMER                             
002000     03 IDGMTREF.                                                         
002100*                                 GODSMOTTAGAREREFERENS                   
002200        05 IDDISTR           PIC S9(5)           COMP-3.                  
002300*                                 DISTRIKTNUMMER                          
002400        05 IDKUNDNR          PIC S9(7)           COMP-3.                  
002500*                                 KUNDNUMMER                              
002600        05 IDKUNDRF-GRP.                                                  
002700*                                 KUNDENS REFERENS (ORDERID)              
002800           07 IDKUNDRF       PIC X(10).                                   
002900*                                 KUNDENS REFERENS (ORDERID)              
003000           07 IDORDNR5-FILLER REDEFINES IDKUNDRF.                         
003100              09 IDORDNR5    PIC 9(5).                                    
003200*                                 ORDERNUMMER                             
003300              09 FILLER      PIC X(5).                                    
003400           07 IDORDNR7-FILLER REDEFINES IDKUNDRF.                         
003500              09 IDORDNR7    PIC 9(7).                                    
003600*                                 ORDERNUMMER                             
003700              09 FILLER      PIC X(3).                                    
003800     03 KDFRAKT              PIC S9(3)           COMP-3.                  
003900*                                 FRAKTSÄTT DC TILL KUND                  
004000*** END OF VILMAII-COPY LENGTH= 45 BYTES                                  
