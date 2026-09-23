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
001400     03 RAD.                                                              
001500*                                 PARTSINFO TILL VIPS                     
001600        05 IDFKNGRP          PIC 9(4).                                    
001700*                                 FUNKTIONSGRUPP                          
001800        05 KDSRA             PIC 9.                                       
001900*                                 EG/EFTA-KOD           KDSRA-002         
002000        05 KVQPACK-0         PIC 9(5).                                    
002100*                                 ANTAL I Q0 FÖRPACKNING                  
002200        05 KDARTURS-NUM      PIC 9(2).                                    
002300*                                 ARTIKELURSPRUNGSKOD NUMERISK            
002400        05 KDPRODSL          PIC 9(2).                                    
002500*                                 PRODUKTSLAG                             
002600        05 VLARTNTO          PIC 9(8)V9(1).                               
002700*                                 ARTIKELVOLYM NETTO (CM3)                
002800        05 VKART             PIC 9(7).                                    
002900*                                 ARTIKELVIKT (G)                         
003000        05 KDVSOP            PIC 9(3).                                    
003100*                                 VSOP-KOD                                
003200        05 IDSTATNR          PIC 9(9).                                    
003300*                                 STATISTISKT NUMMER                      
003400*                                 1 = NORSKT                              
003500*                                 2 = ENGELSKT                            
003600*                                 3 = BELGISKT                            
003700*                                 4 = PERUANSKT                           
003800*                                 5 = SVENSKT                             
003900*                                 6 =                                     
004000        05 KDSORT            PIC X(2).                                    
004100*                                 SORT-KOD                                
004200        05 KDERS             PIC 9(2).                                    
004300*                                 ERSÄTTNINGSKOD                          
004400        05 KDBPSR            PIC 9.                                       
004500*                                 BASLAGERFÖRSLAGSNIVÅ                    
004600        05 KDBBCL            PIC 9.                                       
004700*                                 RETURNERBAR ARTIKEL                     
004800        05 IDLEVNR           PIC X(5).                                    
004900*                                 LEVERANTÖRNUMMER                        
005000        05 KDAGE             PIC X.                                       
005100*                                 AGE-CODE                                
005200        05 IDPROJ            PIC X(4).                                    
005300*                                 PARTS PROJEKTIDENTITET                  
005400        05 PRARTSJK          PIC 9(7)V9(2).                               
005500*                                 ARTIKELNS SJÄLVKOSTNAD                  
005600        05 PRARTSTD          PIC 9(7)V9(2).                               
005700*                                 ARTIKELSTANDARDPRIS                     
005800        05 BEART-L1          PIC X(25).                                   
005900*                                 ARTIKELBENÄMNING                        
006000        05 BEART-L2          PIC X(25).                                   
006100*                                 ARTIKELBENÄMNING                        
006200        05 KDPSLLOC          PIC 9(2).                                    
006300*                                 PRODUKTSLAG LOKALT                      
006400        05 IDLEVNR-LOC       PIC X(5).                                    
006500*                                 LEVERANTÖRNUMMER                        
006600        05 KDSTANAUTG        PIC X.                                       
006700*                                 ERSÄTTNINGSKOD USA/CAN                  
006800        05 FLOVRLAG          PIC X.                                       
006900*                                 ÖVERLAGER                               
007000        05 FLSOFTWARE        PIC X.                                       
007100*                                 ALLMÄN FLAGGA                           
007200     03 DADATUM              PIC 9(8).                                    
007300*                                 REGISTRERINGSDATUM (ÅÅÅÅMMDD)           
007400     03 IDLEVNR-DUBLETT      PIC X(5).                                    
007500*                                 LEVERANTÖRNUMMER                        
007600     03 IDLEVNR-LOC-DUBLETT  PIC X(5).                                    
007700*                                 LEVERANTÖRNUMMER                        
007800     03 KDTIPPR              PIC 9.                                       
007900*                                 TIPPAT PRIS KOD                         
008000     03 IDKAT                OCCURS 3 TIMES                               
008100                             PIC X(5).                                    
008200*                                 KATALOGBETECKNING                       
008300     03 BELEVART             PIC X(30).                                   
008400*                                 LEVERANTÖRENS ARTIKELBENÄMNING          
008500     03 FLGEMFMC             PIC X.                                       
008600*                                 GEMENSAM FORD/MPNR ARTIKEL              
008700     03 IDPROJUP             PIC X(8).                                    
008800*                                 PROJEKTUPPDRAG                          
008900     03 BEARTEXT             PIC X(100).                                  
009000*                                 UTÖKAD ARTIKELBENÄMNING                 
009100     03 TIURPROD             PIC 9(4).                                    
009200*                                 DATUM UTGÅTT UR PROD   (ÅÅVV)           
009300*** END OF VILMAII-COPY LENGTH= 336 BYTES                                 
