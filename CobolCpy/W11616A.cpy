000100 01  W11616A.                                                             
000200*                                 PARTSINFO VIA VIPS                      
000300*                                                                         
000400     03 IDARTNR              PIC 9(9).                                    
000500*                                 ARTIKELNUMMER                           
000600     03 IDLANDX2             PIC X(2).                                    
000700*                                 2-STÄLLIG LANDSBETECKNINGSKOD           
000800     03 RAD.                                                              
000900*                                 PARTSINFO TILL VIPS                     
001000        05 IDFKNGRP          PIC 9(4).                                    
001100*                                 FUNKTIONSGRUPP                          
001200        05 KDSRA             PIC 9.                                       
001300*                                 EG/EFTA-KOD           KDSRA-002         
001400        05 KVQPACK-0         PIC 9(5).                                    
001500*                                 ANTAL I Q0 FÖRPACKNING                  
001600        05 KDARTURS-NUM      PIC 9(2).                                    
001700*                                 ARTIKELURSPRUNGSKOD NUMERISK            
001800        05 KDPRODSL          PIC 9(2).                                    
001900*                                 PRODUKTSLAG                             
002000        05 VLARTNTO          PIC 9(8)V9(1).                               
002100*                                 ARTIKELVOLYM (CM3)                      
002200        05 VKART             PIC 9(7).                                    
002300*                                 ARTIKELVIKT (G)                         
002400        05 KDVSOP            PIC 9(3).                                    
002500*                                 VSOP-KOD                                
002600        05 IDSTATNR          PIC 9(9).                                    
002700*                                 STATISTISKT NUMMER                      
002800*                                 1 = NORSKT                              
002900*                                 2 = ENGELSKT                            
003000*                                 3 = BELGISKT                            
003100*                                 4 = PERUANSKT                           
003200*                                 5 = SVENSKT                             
003300*                                 6 =                                     
003400        05 KDSORT            PIC X(2).                                    
003500*                                 SORT-KOD                                
003600        05 KDERS             PIC 9(2).                                    
003700*                                 ERSÄTTNINGSKOD                          
003800        05 KDBPSR            PIC 9.                                       
003900*                                 BASLAGERFÖRSLAGSNIVÅ                    
004000        05 KDBBCL            PIC 9.                                       
004100*                                 RETURNERBAR ARTIKEL                     
004200        05 IDLEVNR           PIC X(5).                                    
004300*                                 LEVERANTÖRNUMMER                        
004400        05 KDAGE             PIC X.                                       
004500*                                 AGE-CODE                                
004600        05 IDPROJ            PIC X(4).                                    
004700*                                 PARTS PROJEKTIDENTITET                  
004800        05 PRARTSJK          PIC 9(7)V9(2).                               
004900*                                 ARTIKELNS SJÄLVKOSTNAD                  
005000        05 PRARTSTD          PIC 9(7)V9(2).                               
005100*                                 ARTIKELSTANDARDPRIS                     
005200        05 BEART-L1          PIC X(25).                                   
005300*                                 ARTIKELBENÄMNING                        
005400        05 BEART-L2          PIC X(25).                                   
005500*                                 ARTIKELBENÄMNING                        
005600        05 KDPSLLOC          PIC 9(2).                                    
005700*                                 PRODUKTSLAG LOKALT                      
005800        05 IDLEVNR-LOC       PIC X(5).                                    
005900*                                 LEVERANTÖRNUMMER                        
006000        05 KDSTANAUTG        PIC X.                                       
006100*                                 ERSÄTTNINGSKOD USA/CAN                  
006200        05 FLOVRLAG          PIC X.                                       
006300*                                 ÖVERLAGER                               
006400        05 FLSOFTWARE        PIC X.                                       
006500*                                 ALLMÄN FLAGGA                           
006600     03 IDLEVNR-DUBLETT      PIC X(5).                                    
006700*                                 LEVERANTÖRNUMMER                        
006800     03 IDLEVNR-LOC-DUBLETT  PIC X(5).                                    
006900*                                 LEVERANTÖRNUMMER                        
007000     03 DADATUM              PIC 9(8).                                    
007100*                                 REGISTRERINGSDATUM (ÅÅÅÅMMDD)           
007200     03 KDTIPPR              PIC 9.                                       
007300*                                 TIPPAT PRIS KOD                         
007400     03 IDKAT                OCCURS 3 TIMES                               
007500                             PIC X(5).                                    
007600*                                 KATALOGBETECKNING                       
007700     03 BELEVART             PIC X(30).                                   
007800*                                 LEVERANTÖRENS ARTIKELBENÄMNING          
007900     03 FLGEMFMC             PIC X.                                       
008000*                                 GEMENSAM FORD/MPNR ARTIKEL              
008100     03 IDPROJUP             PIC X(8).                                    
008200*                                 PROJEKTUPPDRAG                          
008300     03 TIURPROD             PIC 9(4).                                    
008400*                                 DATUM UTGÅTT UR PROD   (ÅÅVV)           
008500     03 BEARTEXT             PIC X(100).                                  
008600*                                 UTÖKAD ARTIKELBENÄMNING                 
008700*** END OF VILMAII-COPY LENGTH= 324 BYTES                                 
