000100 01  W5132602.                                                            
000200*                                 POSTEN ANVÄNDES FÖR REDOVISNING         
000300*                                 AV ANTALSMÄSSIGA JUSTERINGAR            
000400*                                 VID INVENTERING I SYSTEM W513           
000500*                                                                         
000600     03 SORTFLT.                                                          
000700        05 IDLISTA           PIC S9(3)           COMP-3.                  
000800*                                 LISTNUMMER                              
000900        05 IDDC              PIC X(2).                                    
001000*                                 IDENTIFIERARE LAGER                     
001100        05 IDARTNR           PIC S9(9)           COMP-3.                  
001200*                                 ARTIKELNUMMER                           
001300        05 SORT-BLANK        PIC X(10).                                   
001400     03 KVJUSTKV             PIC S9(7)           COMP-3.                  
001500*                                 JUSTERAD KVANTITET                      
001600     03 TIM-INV              PIC S9(7)           COMP-3.                  
001700*                                 DATUM FÖR INV. ANMODAN (ÅÅMMDD)         
001800     03 KDERS                PIC S9(3)           COMP-3.                  
001900*                                 ERSÄTTNINGSKOD                          
002000     03 BEART-ENG            PIC X(15).                                   
002100*                                 ENGELSK           BEART-ENG-002         
002200*                                 ARTIKELBENÄMNING                        
002300     03 PRARTSTD             PIC S9(7)V9(2)      COMP-3.                  
002400*                                 ARTIKELSTANDARDPRIS                     
002500     03 IDLKTO               PIC S9(7)           COMP-3.                  
002600*                                 LAGERKONTO (FFHHHUU)                    
002700*** END OF VILMAII-COPY LENGTH= 53 BYTES                                  
