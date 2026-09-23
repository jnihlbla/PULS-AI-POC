000100 01  W2613801.                                                            
000200*                                 LISTFIL TILL W26138-001                 
000300*                                                                         
000400     03 IDLISTA              PIC S9(3)           COMP-3.                  
000500*                                 LISTNUMMER                              
000600     03 IDARTNR              PIC S9(9)           COMP-3.                  
000700*                                 ARTIKELNUMMER                           
000800     03 IDANSK               PIC S9(3)           COMP-3.                  
000900*                                 ANSKAFFARNUMMER                         
001000     03 PRARTSTD             PIC S9(7)V9(2)      COMP-3.                  
001100*                                 ARTIKELSTANDARDPRIS                     
001200     03 IDLEVNR              PIC X(5).                                    
001300*                                 LEVERANTÖRNUMMER                        
001400     03 KVBR                 PIC S9(7)           COMP-3.                  
001500*                                 BESTÄLLNINGSREST                        
001600     03 BEART-SVE            PIC X(25).                                   
001700*                                 SVENSK ARTIKELBENÄMNING                 
001800     03 KDVVKL               PIC S9              COMP-3.                  
001900*                                 VOLYMVÄRDESKLASS                        
002000     03 KVOI-TOT             PIC S9(7)           COMP-3.                  
002100*                                 ORDERINGÅNG TOTAL 8 PER                 
002200     03 C-LAGERDEL           OCCURS 2 TIMES.                              
002300        05 KDERS             PIC S9(3)           COMP-3.                  
002400*                                 ERSÄTTNINGSKOD                          
002500        05 KVLS              PIC S9(7)           COMP-3.                  
002600*                                 LAGERSALDO                              
002700        05 KVRESS            PIC S9(7)           COMP-3.                  
002800*                                 RESERVERAT ANTAL ARTIKLAR               
002900        05 KVOKS-BULK        PIC S9(7)           COMP-3.                  
003000*                                 ORDERKÖSALDO, KLASS 2-4                 
003100        05 KVOKS-DAG         PIC S9(7)           COMP-3.                  
003200*                                 ORDERKÖSALDO, KLASS 1                   
003300        05 KVOKS-VOR         PIC S9(7)           COMP-3.                  
003400*                                 ORDERKÖSALDO, VOR                       
003500        05 SUTPO-TOT         PIC S9(7)           COMP-3.                  
003600*                                 TPO-KVANTITET, TOTAL                    
003700        05 KVPB-SEP          PIC S9(6)V9(1)      COMP-3.                  
003800*                                 SEPARAT PERIODBEHOV                     
003900        05 KVPB-SATS         PIC S9(6)V9(1)      COMP-3.                  
004000*                                 SATS-PERIODBEHOV                        
004100        05 KVSLAGER          PIC S9(7)           COMP-3.                  
004200*                                 SÄKERHETSLAGER                          
004300        05 KVAKS             PIC S9(7)           COMP-3.                  
004400*                                 ANKOMSTSALDO                            
004500     03 KVBUFF               PIC S9(7)           COMP-3.                  
004600*                                 FÖRÄDLAT BUFFERSALDO                    
004700*** END OF VILMAII-COPY LENGTH= 141 BYTES                                 
