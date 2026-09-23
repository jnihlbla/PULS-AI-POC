000100 01  W61262.                                                              
000200*                                 FÖRVÄNTADE LEVERANSER                   
000300*                                 FRÅN CDC                                
000400*                                                                         
000500     03 IDDC                 PIC X(2).                                    
000600*                                 IDENTIFIERARE LAGER                     
000700     03 IDFAKT               PIC S9(7)           COMP-3.                  
000800*                                 FAKTURANUMMER                           
000900     03 IDORDNR5             PIC 9(5).                                    
001000*                                 ORDERNUMMER                             
001100     03 IDKOLLI              PIC S9(5)           COMP-3.                  
001200*                                 KOLLINUMMER                             
001300     03 IDKUNDNR             PIC S9(7)           COMP-3.                  
001400*                                 KUNDNUMMER                              
001500     03 IDARTNR              PIC S9(9)           COMP-3.                  
001600*                                 ARTIKELNUMMER                           
001700     03 PRARTNTO             PIC S9(7)V9(2)      COMP-3.                  
001800*                                 ARTIKELPRIS NETTO                       
001900     03 PRKURS               PIC S9(6)V9(5)      COMP-3.                  
002000*                                 VALUTAKURS                              
002100     03 KDVALISO             PIC X(3).                                    
002200*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
002300     03 KVAVIS               PIC S9(7)           COMP-3.                  
002400*                                 AVISERAT ANTAL                          
002500     03 TIBERANK             PIC 9(6).                                    
002600*                                 BERÄKNAD ANKOMSTDATUM                   
002700     03 FLNYART              PIC X.                                       
002800*                                 NY ARTIKEL                              
002900*** END OF VILMAII-COPY LENGTH= 48 BYTES                                  
