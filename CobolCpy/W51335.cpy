000100 01  W51335.                                                              
000200*                                 LISTPOST FÖR JUSTERINGSLISTA            
000300*                                 INVENTERINGEN.                          
000400*                                                                         
000500     03 IDDC                 PIC X(2).                                    
000600*                                 IDENTIFIERARE LAGER                     
000700     03 IDARTNR              PIC S9(9)           COMP-3.                  
000800*                                 ARTIKELNUMMER                           
000900     03 SORTVAERDE           PIC S9(9)V9(2)      COMP-3.                  
001000*                                 VÄRDE PÅ JUSTERAT ANTAL (KR)            
001100     03 IDANSKNR             PIC S9(3)           COMP-3.                  
001200*                                 ANSKAFFARNUMMER                         
001300     03 IDLEVNR              PIC X(5).                                    
001400*                                 LEVERANTÖRNUMMER                        
001500     03 ADLAGOMR             PIC S9(3)           COMP-3.                  
001600*                                 LAGEROMRÅDE                             
001700     03 KDPRODSL             PIC S9(3)           COMP-3.                  
001800*                                 PRODUKTSLAG                             
001900     03 KDINVKAT             PIC S9(3)           COMP-3.                  
002000*                                 INVENTERINGSKATEGORI                    
002100     03 KVLS                 PIC S9(7)           COMP-3.                  
002200*                                 LAGERSALDO                              
002300     03 KVJUSTKV             PIC S9(7)           COMP-3.                  
002400*                                 JUSTERAD KVANTITET                      
002500     03 SUARTSTD-JUST        PIC S9(9)V9(2)      COMP-3.                  
002600*                                 VÄRDE PÅ JUSTERAT ANTAL (KR)            
002700     03 BEART                PIC X(15).                                   
002800*                                 ARTIKELBENÄMNING      BEART-002         
002900*** END OF VILMAII-COPY LENGTH= 55 BYTES                                  
