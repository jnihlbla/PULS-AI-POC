000100 01  W4O10401.                                                            
000200*                                 COPYTEXT FÖR MOD W4O10401               
000300*                                                                         
000400     03 IDTRANS              PIC X(4).                                    
000500*                                 BILDNUMMER                              
000600     03 TEMFSFEL             PIC X(40).                                   
000700*                                 MFS FELMEDDELANDE                       
000800     03 IDARTNR-IN           PIC X(9).                                    
000900*                                 ARTIKELNUMMER                           
001000     03 IDARTNR-UT           PIC X(9).                                    
001100*                                 ARTIKELNUMMER                           
001200     03 STRECK               PIC X.                                       
001300     03 REKSIFFR             PIC 9.                                       
001400*                                 KONTROLLSIFFRA                          
001500     03 KVLEVBSK             PIC 9(3).                                    
001600*                                 ANTAL LEVERANSBESKED                    
001700     03 KVLEVBSK-TEXT        PIC 9(3).                                    
001800*                                 ANTAL LEVERANSBESKED                    
001900     03 AREA.                                                             
002000*                                 AREA SOM NOLLSTÄLLS MELLAN              
002100*                                 VARVEN                                  
002200*                                                                         
002300        05 KVOKS-DAG         PIC -(6)9.                                   
002400*                                 ORDERKÖSALDO, KLASS 1                   
002500        05 KVOKS-BULK        PIC -(6)9.                                   
002600*                                 ORDERKÖSALDO, KLASS 2-4                 
002700        05 IDFKNGRP          PIC Z(4)9.                                   
002800*                                 FUNKTIONSGRUPP                          
002900        05 KDPRODSL          PIC Z(2)9.                                   
003000*                                 PRODUKTSLAG                             
003100        05 KDPRORED          PIC Z(2)9.                                   
003200*                                 PRODUKTKOD REDOVISN. (0/1 + PP)         
003300        05 KDSORT            PIC X(2).                                    
003400*                                 SORT-KOD                                
003500        05 VKART             PIC Z(6)9.                                   
003600*                                 ARTIKELVIKT (G)                         
003700        05 VLARTNTO          PIC Z(7)9.9.                                 
003800*                                 ARTIKELVOLYM NETTO (CM3)                
003900        05 IDSTATNR          OCCURS 6 TIMES                               
004000                             PIC Z(8).                                    
004100*                                 STATISTISKT NUMMER                      
004200*                                 1 = NORSKT                              
004300*                                 2 = ENGELSKT                            
004400*                                 3 = BELGISKT                            
004500*                                 4 = PERUANSKT                           
004600*                                 5 = SVENSKT                             
004700*                                 6 =                                     
004800        05 IDVAGN            OCCURS 3 TIMES                               
004900                             PIC X(5).                                    
005000*                                                      IDVAGN-002         
005100        05 KDLTK             PIC X.                                       
005200*                                 LAGERTILLHÖRIGHETSKOD                   
005300        05 KDGK              PIC X.                                       
005400*                                 GODSMOTTAGAREKOD                        
005500        05 SUTPO-TOT         PIC -(6)9.                                   
005600*                                 TPO-KVANTITET, TOTAL                    
005700        05 KVEFRS            PIC Z(5)9.                                   
005800*                                 EJ FAKTURERAT ANTAL STYCK               
005900        05 KVAVIS            OCCURS 4 TIMES                               
006000                             PIC Z(5)9.                                   
006100*                                 AVISERAT ANTAL                          
006200        05 TILEVBSK          OCCURS 4 TIMES                               
006300                             PIC Z(4).                                    
006400*                                 LEVERANSBESKEDSVECKA   (ÅÅVV)           
006500        05 TILEVBSK-TEXT     PIC X(27).                                   
006600        05 TELEVBSK-EXT      PIC X(80).                                   
006700*                                 LEVERANSBESKED FÖR EXTERNT              
006800        05 TELEVBSK-EXT2     PIC X(80).                                   
006900*                                 LEVERANSBESKED FÖR EXTERNT              
007000        05 TEMFSINF          PIC X(55).                                   
007100*                                 INFORMATIONSMEDDELANDE                  
007200*** END OF VILMAII-COPY LENGTH= 474 BYTES                                 
