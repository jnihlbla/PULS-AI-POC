000100 01  W51322R1.                                                            
000200*                                 POSTEN ANVÄNDS  FÖR REDOVIS             
000300*                                 NING AV PERIODENS INVENTE               
000400*                                 RINGSDIFFERENSER  NDC                   
000500*                                                                         
000600     03 SORTFLT.                                                          
000700        05 IDLISTA           PIC S9(3)           COMP-3.                  
000800*                                 LISTNUMMER                              
000900        05 IDDC              PIC X(2).                                    
001000*                                 IDENTIFIERARE LAGER                     
001100        05 ADLAGOMR          PIC S9(3)           COMP-3.                  
001200*                                 LAGEROMRÅDE                             
001300        05 SORT-PRODKOD      PIC S9(3)           COMP-3.                  
001400*                                 PRODUKTKOD REDOVISN. (0/1 + PP)         
001500        05 IDARTNR           PIC S9(9)           COMP-3.                  
001600*                                 ARTIKELNUMMER                           
001700     03 KVJUSTKV             PIC S9(7)           COMP-3.                  
001800*                                 JUSTERAD KVANTITET                      
001900     03 PRAVCOST             PIC S9(7)V9(2)      COMP-3.                  
002000*                                 MEDELVÄRDESKOSTNAD I UTL.VALUTA         
002100     03 JUST-VAERDE          PIC S9(9)V9(2)      COMP-3.                  
002200*                                 VÄRDE PÅ JUSTERAT ANTAL (KR)            
002300     03 TIM-INV              PIC S9(7)           COMP-3.                  
002400*                                 DATUM FÖR INV. ANMODAN (ÅÅMMDD)         
002500     03 KDERS                PIC S9(3)           COMP-3.                  
002600*                                 ERSÄTTNINGSKOD                          
002700     03 BEART-ENG            PIC X(15).                                   
002800*                                 ENGELSK           BEART-ENG-002         
002900*                                 ARTIKELBENÄMNING                        
003000     03 IDANSK               PIC S9(3)           COMP-3.                  
003100*                                 ANSKAFFARNUMMER                         
003200     03 KVLS                 PIC S9(7)           COMP-3.                  
003300*                                 LAGERSALDO                              
003400*** END OF VILMAII-COPY LENGTH= 55 BYTES                                  
