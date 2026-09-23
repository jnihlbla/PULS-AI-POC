000100 01  W61227.                                                              
000200*                                 BILL-IT TRANSAR                         
000300     03 DAREGDAT             PIC 9(8).                                    
000400*                                 REGISTRERINGSDATUM (≈≈≈≈MMDD)           
000500     03 IDARTNR              PIC S9(9)           COMP-3.                  
000600*                                 ARTIKELNUMMER                           
000700     03 IDDISTR              PIC S9(5)           COMP-3.                  
000800*                                 DISTRIKTNUMMER                          
000900     03 IDKUNDNR             PIC S9(7)           COMP-3.                  
001000*                                 KUNDNUMMER                              
001100     03 IDKUNDRF             PIC X(10).                                   
001200*                                 KUNDENS REFERENS (ORDERID)              
001300     03 IDKOLLI              PIC S9(5)           COMP-3.                  
001400*                                 KOLLINUMMER                             
001500     03 IDFAKT               PIC S9(7)           COMP-3.                  
001600*                                 FAKTURANUMMER                           
001700     03 IDDC-REC             PIC X(2).                                    
001800*                                 MOTTAGANDE LAGER                        
001900     03 IDDC-SEND            PIC X(2).                                    
002000*                                 SƒNDANDE LAGER                          
002100     03 KVAVIS               PIC S9(7)           COMP-3.                  
002200*                                 AVISERAT ANTAL                          
002300     03 KVANTMOT             PIC S9(7)           COMP-3.                  
002400*                                 ANTAL MOTTAGET                          
002500*** END OF VILMAII-COPY LENGTH= 49 BYTES                                  
