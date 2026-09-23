000100 01  W261LI01-CTX.                                                        
000200*                                 LISTPOSTER LTK-UPPFÖLJNING              
000300     03 IDLISTA              PIC S9(3)           COMP-3.                  
000400*                                 LISTNUMMER                              
000500     03 IDANSK               PIC S9(3)           COMP-3.                  
000600*                                 ANSKAFFARNUMMER                         
000700     03 KDLTK                PIC S9              COMP-3.                  
000800*                                 LAGERTILLHÖRIGHETSKOD                   
000900     03 IDARTNR              PIC S9(9)           COMP-3.                  
001000*                                 ARTIKELNUMMER                           
001100     03 BEART-SVE            PIC X(25).                                   
001200*                                 SVENSK ARTIKELBENÄMNING                 
001300     03 IDLEVNR              PIC X(5).                                    
001400*                                 LEVERANTÖRNUMMER                        
001500     03 KDGK                 PIC S9              COMP-3.                  
001600*                                 GODSMOTTAGAREKOD                        
001700     03 FLMANGK              PIC X.                                       
001800*                                 FLAGGA MANUELL GODSMOTTAGARKOD          
001900     03 TIFINLV              PIC S9(5)           COMP-3.                  
002000*                                 PUBLICERINGSVECKA, (ÅÅVVD  D=1)         
002100     03 W261LI01-001-GRP     OCCURS 16 TIMES.                             
002200*                                 ANTAL ORDERTRÄFF                        
002300        05 KVOT-C2KUND-PROGNOS                                            
002400                             PIC S9(7)           COMP-3.                  
002500*                                 ORDERTRÄFF, PROGNOSPÅVERKANDE           
002600        05 KVOT-C2KUND-DIVERSE                                            
002700                             PIC S9(7)           COMP-3.                  
002800*                                 ORDERTRÄFF, DIVERSE                     
002900        05 KVOT-C2KUND-SATS  PIC S9(7)           COMP-3.                  
003000*                                 ORDERTRÄFF, SATS                        
003100     03 W261LI01-002-GRP     OCCURS 8 TIMES.                              
003200*                                 ORDERINGÅNG   STYCK                     
003300        05 KVOI-C1KUND-PROGNOS                                            
003400                             PIC S9(7)           COMP-3.                  
003500*                                 PROGNOSPÅVERKANDE ORDERINGÅNG           
003600        05 KVOI-C2KUND-PROGNOS                                            
003700                             PIC S9(7)           COMP-3.                  
003800*                                 PROGNOSPÅVERKANDE ORDERINGÅNG           
003900     03 W261LI01-003-GRP     OCCURS 2 TIMES.                              
004000        05 KVLS              PIC S9(7)           COMP-3.                  
004100*                                 LAGERSALDO                              
004200        05 KVRESS            PIC S9(7)           COMP-3.                  
004300*                                 RESERVERAT ANTAL ARTIKLAR               
004400        05 KVOKS-BULK        PIC S9(7)           COMP-3.                  
004500*                                 ORDERKÖSALDO, KLASS 2-4                 
004600        05 KVOKS-DAG         PIC S9(7)           COMP-3.                  
004700*                                 ORDERKÖSALDO, KLASS 1                   
004800        05 KVOKS-VOR         PIC S9(7)           COMP-3.                  
004900*                                 ORDERKÖSALDO, VOR                       
005000        05 KVPB-SEP          PIC S9(6)V9(1)      COMP-3.                  
005100*                                 SEPARAT PERIODBEHOV                     
005200        05 KVPB-SATS         PIC S9(6)V9(1)      COMP-3.                  
005300*                                 SATS-PERIODBEHOV                        
005400        05 KDERS             PIC S9(3)           COMP-3.                  
005500*                                 ERSÄTTNINGSKOD                          
005600*** END OF VILMAII-COPY LENGTH= 361 BYTES                                 
