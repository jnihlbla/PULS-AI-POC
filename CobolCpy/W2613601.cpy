000100 01  W2613601.                                                            
000200*                                 LISTFIL TILL W26136-001                 
000300*                                                                         
000400     03 IDLISTA              PIC S9(3)           COMP-3.                  
000500*                                 LISTNUMMER                              
000600     03 IDARTNR              PIC S9(9)           COMP-3.                  
000700*                                 ARTIKELNUMMER                           
000800     03 IDANSK               PIC S9(3)           COMP-3.                  
000900*                                 ANSKAFFARNUMMER                         
001000     03 KDLTK                PIC S9              COMP-3.                  
001100*                                 LAGERTILLHÖRIGHETSKOD                   
001200     03 PRARTSTD             PIC S9(7)V9(2)      COMP-3.                  
001300*                                 ARTIKELSTANDARDPRIS                     
001400     03 IDLEVNR              PIC X(5).                                    
001500*                                 LEVERANTÖRNUMMER                        
001600     03 KVBR                 PIC S9(7)           COMP-3.                  
001700*                                 BESTÄLLNINGSREST                        
001800     03 BEART-SVE            PIC X(25).                                   
001900*                                 SVENSK ARTIKELBENÄMNING                 
002000     03 C-LAGERDEL           OCCURS 2 TIMES.                              
002100        05 KDERS             PIC S9(3)           COMP-3.                  
002200*                                 ERSÄTTNINGSKOD                          
002300        05 KVLS              PIC S9(7)           COMP-3.                  
002400*                                 LAGERSALDO                              
002500        05 KVRESS            PIC S9(7)           COMP-3.                  
002600*                                 RESERVERAT ANTAL ARTIKLAR               
002700        05 KVAKS             PIC S9(7)           COMP-3.                  
002800*                                 ANKOMSTSALDO                            
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
004100     03 KVBUFF               PIC S9(7)           COMP-3.                  
004200*                                 FÖRÄDLAT BUFFERSALDO                    
004300*** END OF VILMAII-COPY LENGTH= 129 BYTES                                 
