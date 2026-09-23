000100 01  SHIST-W61254.                                                        
000200*                                 FÖRVÄNTADE FLYGLEVERANSER               
000300*                                 FRÅN CDC                                
000400*                                                                         
000500     03 SHIST-IDDC           PIC X(2).                                    
000600*                                 IDENTIFIERARE LAGER                     
000700     03 SHIST-IDFAKT         PIC S9(7)           COMP-3.                  
000800*                                 FAKTURANUMMER                           
000900     03 SHIST-IDKOLLI        PIC S9(5)           COMP-3.                  
001000*                                 KOLLINUMMER                             
001100     03 SHIST-IDKUNDNR       PIC S9(7)           COMP-3.                  
001200*                                 KUNDNUMMER                              
001300     03 SHIST-IDORDNR5       PIC 9(5).                                    
001400*                                 ORDERNUMMER                             
001500     03 SHIST-IDARTNR        PIC S9(9)           COMP-3.                  
001600*                                 ARTIKELNUMMER                           
001700     03 SHIST-PRARTNTO       PIC S9(7)V9(2)      COMP-3.                  
001800*                                 ARTIKELPRIS NETTO                       
001900     03 SHIST-PRKURS         PIC S9(6)V9(5)      COMP-3.                  
002000*                                 VALUTAKURS                              
002100     03 SHIST-KDVALISO       PIC X(3).                                    
002200*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
002300     03 SHIST-KVAVIS         PIC S9(7)           COMP-3.                  
002400*                                 AVISERAT ANTAL                          
002500     03 SHIST-TIBERANK       PIC 9(6).                                    
002600*                                 BERÄKNAD ANKOMSTDATUM                   
002700     03 SHIST-FLNYART        PIC X.                                       
002800*                                 NY ARTIKEL                              
002900     03 SHIST-ADCITY         PIC X(25).                                   
003000*                                 BENÄMNING PÅ STAD                       
003100*** END OF VILMAII-COPY LENGTH= 73 BYTES                                  
