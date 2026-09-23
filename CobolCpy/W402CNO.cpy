000100 01  CNO-W402CNO.                                                         
000200*                                 LAYOUT FÖR ORDERINFO KINA               
000300*                                 TILL TACDIS                             
000400*                                 ANVÄNDS I W40293 SOM SKICKAR            
000500*                                 ORDERINFO. TILL TACDIS VIA              
000600*                                 D & P                                   
000700     03 CNO-IDPTYP           PIC X(3).                                    
000800*                                 POSTTYP                                 
000900     03 CNO-IDDISTR          PIC 9(4).                                    
001000*                                 DISTRIKTNUMMER                          
001100     03 CNO-IDKUNDNR         PIC 9(6).                                    
001200*                                 KUNDNUMMER                              
001300     03 CNO-IDORDNR          PIC 9(5).                                    
001400*                                 ORDERNUMMER UTGÅR PD90                  
001500     03 CNO-TIREGDAT         PIC 9(6).                                    
001600*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
001700     03 CNO-IDARTNR          PIC 9(8).                                    
001800*                                 ARTIKELNUMMER                           
001900     03 CNO-KVBEART          PIC 9(7).                                    
002000*                                 BESTÄLLT ANTAL STYCKEN                  
002100     03 CNO-KVPRERO          PIC 9(7).                                    
002200*                                 PRELIMINÄR RO-KVANT                     
002300     03 CNO-KDFRAKT          PIC 9(2).                                    
002400*                                 FRAKTSÄTT DC TILL KUND                  
002500*** END OF VILMAII-COPY LENGTH= 48 BYTES                                  
