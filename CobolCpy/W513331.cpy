000100 01  W513331.                                                             
000200*                                 COPYTEXT TILL ADJUSTMENT REPORT         
000300*                                  LDC                                    
000400     03 IDAFPRCD             PIC X(10).                                   
000500*                                 AFP-BLANKETT POSTTYP                    
000600     03 IDDC                 PIC X(2).                                    
000700*                                 IDENTIFIERARE LAGER                     
000800     03 IDARTNR              PIC Z(7)9.                                   
000900*                                 ARTIKELNUMMER                           
001000     03 BEART                PIC X(25).                                   
001100*                                 ARTIKELBENÄMNING                        
001200     03 IDLEVNR              PIC X(5).                                    
001300*                                 LEVERANTÖRNUMMER                        
001400     03 ADLAGOMR             PIC 9(2).                                    
001500*                                 LAGEROMRÅDE                             
001600     03 KDPRODSL             PIC Z9.                                      
001700*                                 PRODUKTSLAG                             
001800     03 KDINVKAT             PIC Z9.                                      
001900*                                 INVENTERINGSKATEGORI                    
002000     03 IDANSKNR             PIC 9(3).                                    
002100*                                 ANSKAFFARNUMMER                         
002200     03 KVLS                 PIC -(7)9.                                   
002300*                                 LAGERSALDO                              
002400     03 KVJUSTKV             PIC -(7)9.                                   
002500*                                 JUSTERAD KVANTITET                      
002600     03 SUARTSTD             PIC Z(10).Z(2)-.                             
002700*                                 SUMMA STANDARDPRIS RADVÄRDE             
002800*** END OF VILMAII-COPY LENGTH= 89 BYTES                                  
