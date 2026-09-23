000100 01  W261LI02.                                                            
000200*                                 LISTPOSTER LAGERBALANSERING             
000300     03 IDLISTA              PIC S9(3)           COMP-3.                  
000400*                                 LISTNUMMER                              
000500     03 IDANSK               PIC S9(3)           COMP-3.                  
000600*                                 ANSKAFFARNUMMER                         
000700     03 IDLEVNR              PIC X(5).                                    
000800*                                 LEVERANTÖRNUMMER                        
000900     03 IDARTNR              PIC S9(9)           COMP-3.                  
001000*                                 ARTIKELNUMMER                           
001100     03 TIFINLV              PIC S9(5)           COMP-3.                  
001200*                                 PUBLICERINGSVECKA, (ÅÅVVD  D=1)         
001300     03 PRARTSTD             PIC S9(7)V9(2)      COMP-3.                  
001400*                                 ARTIKELSTANDARDPRIS                     
001500     03 KVSKKNST             PIC S9(3)           COMP-3.                  
001600*                                 SKROT KONSTANT                          
001700     03 TIURPROD             PIC S9(5)           COMP-3.                  
001800*                                 DATUM UTGÅTT UR PROD   (ÅÅVV)           
001900     03 KVBR                 PIC S9(7)           COMP-3.                  
002000*                                 BESTÄLLNINGSREST                        
002100     03 FLJANEJ-C2           PIC X.                                       
002200*                                 JA/NEJ-FLAGGA                           
002300     03 BEART-SVE            PIC X(25).                                   
002400*                                 SVENSK ARTIKELBENÄMNING                 
002500     03 C-LAGERDEL           OCCURS 2 TIMES.                              
002600        05 KVLS              PIC S9(7)           COMP-3.                  
002700*                                 LAGERSALDO                              
002800        05 KVRESS            PIC S9(7)           COMP-3.                  
002900*                                 RESERVERAT ANTAL ARTIKLAR               
003000        05 KVAKS             PIC S9(7)           COMP-3.                  
003100*                                 ANKOMSTSALDO                            
003200        05 KVOKS-BULK        PIC S9(7)           COMP-3.                  
003300*                                 ORDERKÖSALDO, KLASS 2-4                 
003400        05 KVOKS-DAG         PIC S9(7)           COMP-3.                  
003500*                                 ORDERKÖSALDO, KLASS 1                   
003600        05 KVOKS-VOR         PIC S9(7)           COMP-3.                  
003700*                                 ORDERKÖSALDO, VOR                       
003800        05 SUTPO-TOT         PIC S9(7)           COMP-3.                  
003900*                                 TPO-KVANTITET, TOTAL                    
004000        05 KVROS             PIC S9(7)           COMP-3.                  
004100*                                 RESTORDERSALDO                          
004200        05 KVPB-SEP          PIC S9(6)V9(1)      COMP-3.                  
004300*                                 SEPARAT PERIODBEHOV                     
004400        05 KVPB-SATS         PIC S9(6)V9(1)      COMP-3.                  
004500*                                 SATS-PERIODBEHOV                        
004600        05 KDERS             PIC S9(3)           COMP-3.                  
004700*                                 ERSÄTTNINGSKOD                          
004800     03 ORDERINGANGSDEL      OCCURS 8 TIMES.                              
004900*                                 ORDERINGÅNG I STYCK                     
005000        05 KVOI-C1KUND-PROGNOS                                            
005100                             PIC S9(7)           COMP-3.                  
005200*                                 PROGNOSPÅVERKANDE ORDERINGÅNG           
005300        05 KVOI-C1KUND-DIVERSE                                            
005400                             PIC S9(7)           COMP-3.                  
005500*                                 ORDERINGÅNG DIVERSE OCH TPO             
005600        05 KVOI-C1KUND-SATS  PIC S9(7)           COMP-3.                  
005700*                                 ORDERINGÅNG SATSFÖRBRUKNING             
005800        05 KVOI-C2KUND-PROGNOS                                            
005900                             PIC S9(7)           COMP-3.                  
006000*                                 PROGNOSPÅVERKANDE ORDERINGÅNG           
006100        05 KVOI-C2KUND-DIVERSE                                            
006200                             PIC S9(7)           COMP-3.                  
006300*                                 ORDERINGÅNG DIVERSE OCH TPO             
006400        05 KVOI-C2KUND-SATS  PIC S9(7)           COMP-3.                  
006500*                                 ORDERINGÅNG SATSFÖRBRUKNING             
006600     03 KVBUFF               PIC S9(7)           COMP-3.                  
006700*                                 FÖRÄDLAT BUFFERSALDO                    
006800*** END OF VILMAII-COPY LENGTH= 337 BYTES                                 
