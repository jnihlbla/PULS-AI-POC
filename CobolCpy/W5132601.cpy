000100 01  W5132601.                                                            
000200*                                 POSTEN ANVÄNDES FÖR REDOVIS             
000300*                                 NING AV PERIODENS INVENTE               
000400*                                 RINGSDIFFERENSER.                       
000500*                                                                         
000600     03 SORTFLT.                                                          
000700        05 IDLISTA           PIC S9(3)           COMP-3.                  
000800*                                 LISTNUMMER                              
000900        05 IDDC              PIC X(2).                                    
001000*                                 IDENTIFIERARE LAGER                     
001100        05 ADLAGOMR          PIC S9(3)           COMP-3.                  
001200*                                 LAGEROMRÅDE                             
001300        05 FILLER            PIC 9(2).                                    
001400        05 KDPRODSL          PIC S9(3)           COMP-3.                  
001500*                                 PRODUKTSLAG                             
001600        05 SORT-BLANK        PIC X(10).                                   
001700        05 IDARTNR           PIC S9(9)           COMP-3.                  
001800*                                 ARTIKELNUMMER                           
001900     03 TIO-BLANKA           PIC X(10).                                   
002000     03 KVJUSTKV             PIC S9(7)           COMP-3.                  
002100*                                 JUSTERAD KVANTITET                      
002200     03 PRARTSTD             PIC S9(7)V9(2)      COMP-3.                  
002300*                                 ARTIKELSTANDARDPRIS                     
002400     03 JUST-VAERDE          PIC S9(9)V9(2)      COMP-3.                  
002500*                                 VÄRDE PÅ JUSTERAT ANTAL (KR)            
002600     03 TIM-INV              PIC S9(7)           COMP-3.                  
002700*                                 DATUM FÖR INV. ANMODAN (ÅÅMMDD)         
002800     03 KDERS                PIC S9(3)           COMP-3.                  
002900*                                 ERSÄTTNINGSKOD                          
003000     03 BEART-ENG            PIC X(15).                                   
003100*                                 ENGELSK           BEART-ENG-002         
003200*                                 ARTIKELBENÄMNING                        
003300     03 IDANSK               PIC S9(3)           COMP-3.                  
003400*                                 ANSKAFFARNUMMER                         
003500     03 KVLS                 PIC S9(7)           COMP-3.                  
003600*                                 LAGERSALDO                              
003700*** END OF VILMAII-COPY LENGTH= 77 BYTES                                  
