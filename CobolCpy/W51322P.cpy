000100 01  W51322P.                                                             
000200*                                 INFORMATION TILL                        
000300*                                 JUSTERINGSLISTA INVENTERING NDC         
000400*                                                                         
000500     03 W51322P-SORTDEL.                                                  
000600        05 IDLISTA           PIC S9(3)           COMP-3.                  
000700*                                 LISTNUMMER                              
000800        05 IDDC              PIC X(2).                                    
000900*                                 IDENTIFIERARE LAGER                     
001000        05 IDFTG             PIC 9(2).                                    
001100*                                 FÖRETAGSID EKONOM REDOVISNING           
001200        05 SORTVAERDE        PIC S9(9)V9(2)      COMP-3.                  
001300*                                 VÄRDE PÅ JUSTERAT ANTAL (KR)            
001400        05 FILLER            PIC X.                                       
001500        05 IDARTNR           PIC S9(9)           COMP-3.                  
001600*                                 ARTIKELNUMMER                           
001700        05 SORTDEL1          PIC S9(11)          COMP-3.                  
001800     03 IDANSKNR             PIC S9(3)           COMP-3.                  
001900*                                 ANSKAFFARNUMMER                         
002000     03 IDLEVNR              PIC X(5).                                    
002100*                                 LEVERANTÖRNUMMER                        
002200     03 ADLAGOMR             PIC S9(3)           COMP-3.                  
002300*                                 LAGEROMRÅDE                             
002400     03 KDPSLLOC             PIC 9(2).                                    
002500*                                 PRODUKTSLAG LOKALT                      
002600     03 IDKONTO              PIC 9(10).                                   
002700*                                 KONTO                                   
002800     03 KVBR                 PIC S9(7)           COMP-3.                  
002900*                                 BESTÄLLNINGSREST                        
003000     03 KVLS                 PIC S9(7)           COMP-3.                  
003100*                                 LAGERSALDO                              
003200     03 KVJUSTKV             PIC S9(7)           COMP-3.                  
003300*                                 JUSTERAD KVANTITET                      
003400     03 JUST-VAERDE          PIC S9(9)V9(2)      COMP-3.                  
003500*                                 VÄRDE PÅ JUSTERAT ANTAL (KR)            
003600     03 BEART                PIC X(15).                                   
003700*                                 ARTIKELBENÄMNING      BEART-002         
003800     03 KDSORT               PIC X(2).                                    
003900*                                 SORT-KOD                                
004000*** END OF VILMAII-COPY LENGTH= 80 BYTES                                  
