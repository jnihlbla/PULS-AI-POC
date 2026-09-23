000100 01  W11620.                                                              
000200*                                 PARTSINFO VIA VIPS TILL USA/CAN         
000300*                                                                         
000400*                                                                         
000500     03 IDPTYP               PIC X(3).                                    
000600*                                 POSTTYP                                 
000700     03 IDARTNR20            PIC X(20).                                   
000800*                                 20-STÄLLIGT ARTIKELNUMMER FÖR A         
000900*                                 S400 (VIPS)                             
001000*                                 FORMATET ÄR HÖGERJUSTERAT MED I         
001100*                                 NLEDANDE                                
001200*                                 BLANKTECKEN, OCH UTAN INLEDANDE         
001300*                                  NOLLOR.                                
001400     03 IDLANDX2             PIC X(2).                                    
001500*                                 2-STÄLLIG LANDSBETECKNINGSKOD           
001600     03 RAD.                                                              
001700*                                 PARTSINFO TILL VIPS                     
001800        05 IDFKNGRP          PIC 9(4).                                    
001900*                                 FUNKTIONSGRUPP                          
002000        05 KDSRA             PIC 9.                                       
002100*                                 EG/EFTA-KOD           KDSRA-002         
002200        05 KVQPACK-0         PIC 9(5).                                    
002300*                                 ANTAL I Q0 FÖRPACKNING                  
002400        05 KDARTURS-NUM      PIC 9(2).                                    
002500*                                 ARTIKELURSPRUNGSKOD NUMERISK            
002600        05 KDPRODSL          PIC 9(2).                                    
002700*                                 PRODUKTSLAG                             
002800        05 VLARTNTO          PIC 9(8)V9(1).                               
002900*                                 ARTIKELVOLYM (CM3)                      
003000        05 VKART             PIC 9(7).                                    
003100*                                 ARTIKELVIKT (G)                         
003200        05 KDVSOP            PIC 9(3).                                    
003300*                                 VSOP-KOD                                
003400        05 IDSTATNR          PIC 9(9).                                    
003500*                                 STATISTISKT NUMMER                      
003600*                                 1 = NORSKT                              
003700*                                 2 = ENGELSKT                            
003800*                                 3 = BELGISKT                            
003900*                                 4 = PERUANSKT                           
004000*                                 5 = SVENSKT                             
004100*                                 6 =                                     
004200        05 KDSORT            PIC X(2).                                    
004300*                                 SORT-KOD                                
004400        05 KDERS             PIC 9(2).                                    
004500*                                 ERSÄTTNINGSKOD                          
004600        05 KDBPSR            PIC 9.                                       
004700*                                 BASLAGERFÖRSLAGSNIVÅ                    
004800        05 KDBBCL            PIC 9.                                       
004900*                                 RETURNERBAR ARTIKEL                     
005000        05 IDLEVNR           PIC X(5).                                    
005100*                                 LEVERANTÖRNUMMER                        
005200        05 KDAGE             PIC X.                                       
005300*                                 AGE-CODE                                
005400        05 IDPROJ            PIC X(4).                                    
005500*                                 PARTS PROJEKTIDENTITET                  
005600        05 PRARTSJK          PIC 9(7)V9(2).                               
005700*                                 ARTIKELNS SJÄLVKOSTNAD                  
005800        05 PRARTSTD          PIC 9(7)V9(2).                               
005900*                                 ARTIKELSTANDARDPRIS                     
006000        05 BEART-L1          PIC X(25).                                   
006100*                                 ARTIKELBENÄMNING                        
006200        05 BEART-L2          PIC X(25).                                   
006300*                                 ARTIKELBENÄMNING                        
006400        05 KDPSLLOC          PIC 9(2).                                    
006500*                                 PRODUKTSLAG LOKALT                      
006600        05 IDLEVNR-LOC       PIC X(5).                                    
006700*                                 LEVERANTÖRNUMMER                        
006800        05 KDSTANAUTG        PIC X.                                       
006900*                                 ERSÄTTNINGSKOD USA/CAN                  
007000        05 FLOVRLAG          PIC X.                                       
007100*                                 ÖVERLAGER                               
007200        05 FLSOFTWARE        PIC X.                                       
007300*                                 ALLMÄN FLAGGA                           
007400     03 DADATUM              PIC 9(8).                                    
007500*                                 REGISTRERINGSDATUM (ÅÅÅÅMMDD)           
007600     03 IDLEVNR-DUBLETT      PIC X(5).                                    
007700*                                 LEVERANTÖRNUMMER                        
007800     03 IDLEVNR-LOC-DUBLETT  PIC X(5).                                    
007900*                                 LEVERANTÖRNUMMER                        
008000     03 KDTIPPR              PIC 9.                                       
008100*                                 TIPPAT PRIS KOD                         
008200     03 IDKAT                OCCURS 3 TIMES                               
008300                             PIC X(5).                                    
008400*                                 KATALOGBETECKNING                       
008500     03 BELEVART             PIC X(30).                                   
008600*                                 LEVERANTÖRENS ARTIKELBENÄMNING          
008700     03 FLGEMFMC             PIC X.                                       
008800*                                 GEMENSAM FORD/MPNR ARTIKEL              
008900     03 IDPROJUP             PIC X(8).                                    
009000*                                 PROJEKTUPPDRAG                          
009100     03 BEARTEXT             PIC X(100).                                  
009200*                                 UTÖKAD ARTIKELBENÄMNING                 
009300     03 TIURPROD             PIC 9(4).                                    
009400*                                 DATUM UTGÅTT UR PROD   (ÅÅVV)           
009500*** END OF VILMAII-COPY LENGTH= 338 BYTES                                 
