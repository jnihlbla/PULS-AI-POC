000100 01  W11620B.                                                             
000200*                                 PARTSINFO VIA VIPS TILL                 
000300*                                 ÖVRIGA VÄRLDEN                          
000400     03 IDPTYP               PIC X(3).                                    
000500*                                 POSTTYP                                 
000600     03 IDARTNR20            PIC X(20).                                   
000700*                                 20-STÄLLIGT ARTIKELNUMMER FÖR A         
000800*                                 S400 (VIPS)                             
000900*                                 FORMATET ÄR HÖGERJUSTERAT MED I         
001000*                                 NLEDANDE                                
001100*                                 BLANKTECKEN, OCH UTAN INLEDANDE         
001200*                                  NOLLOR.                                
001300     03 IDLANDX2             PIC X(2).                                    
001400*                                 2-STÄLLIG LANDSBETECKNINGSKOD           
001500     03 IDLOPNR              PIC 9(2).                                    
001600*                                 LÖPNUMMER                               
001700     03 RAD.                                                              
001800*                                 PARTSINFO TILL VIPS                     
001900        05 IDFKNGRP          PIC 9(4).                                    
002000*                                 FUNKTIONSGRUPP                          
002100        05 KDSRA             PIC 9.                                       
002200*                                 EG/EFTA-KOD           KDSRA-002         
002300        05 KVQPACK-0         PIC 9(5).                                    
002400*                                 ANTAL I Q0 FÖRPACKNING                  
002500        05 KDARTURS-NUM      PIC 9(2).                                    
002600*                                 ARTIKELURSPRUNGSKOD NUMERISK            
002700        05 KDPRODSL          PIC 9(2).                                    
002800*                                 PRODUKTSLAG                             
002900        05 VLARTNTO          PIC 9(8)V9(1).                               
003000*                                 ARTIKELVOLYM (CM3)                      
003100        05 VKART             PIC 9(7).                                    
003200*                                 ARTIKELVIKT (G)                         
003300        05 KDVSOP            PIC 9(3).                                    
003400*                                 VSOP-KOD                                
003500        05 IDSTATNR          PIC 9(9).                                    
003600*                                 STATISTISKT NUMMER                      
003700*                                 1 = NORSKT                              
003800*                                 2 = ENGELSKT                            
003900*                                 3 = BELGISKT                            
004000*                                 4 = PERUANSKT                           
004100*                                 5 = SVENSKT                             
004200*                                 6 =                                     
004300        05 KDSORT            PIC X(2).                                    
004400*                                 SORT-KOD                                
004500        05 KDERS             PIC 9(2).                                    
004600*                                 ERSÄTTNINGSKOD                          
004700        05 KDBPSR            PIC 9.                                       
004800*                                 BASLAGERFÖRSLAGSNIVÅ                    
004900        05 KDBBCL            PIC 9.                                       
005000*                                 RETURNERBAR ARTIKEL                     
005100        05 IDLEVNR           PIC X(5).                                    
005200*                                 LEVERANTÖRNUMMER                        
005300        05 KDAGE             PIC X.                                       
005400*                                 AGE-CODE                                
005500        05 IDPROJ            PIC X(4).                                    
005600*                                 PARTS PROJEKTIDENTITET                  
005700        05 PRARTSJK          PIC 9(7)V9(2).                               
005800*                                 ARTIKELNS SJÄLVKOSTNAD                  
005900        05 PRARTSTD          PIC 9(7)V9(2).                               
006000*                                 ARTIKELSTANDARDPRIS                     
006100        05 BEART-L1          PIC X(25).                                   
006200*                                 ARTIKELBENÄMNING                        
006300        05 BEART-L2          PIC X(25).                                   
006400*                                 ARTIKELBENÄMNING                        
006500        05 KDPSLLOC          PIC 9(2).                                    
006600*                                 PRODUKTSLAG LOKALT                      
006700        05 IDLEVNR-LOC       PIC X(5).                                    
006800*                                 LEVERANTÖRNUMMER                        
006900        05 KDSTANAUTG        PIC X.                                       
007000*                                 ERSÄTTNINGSKOD USA/CAN                  
007100        05 FLOVRLAG          PIC X.                                       
007200*                                 ÖVERLAGER                               
007300        05 FLSOFTWARE        PIC X.                                       
007400*                                 ALLMÄN FLAGGA                           
007500     03 DADATUM              PIC 9(8).                                    
007600*                                 REGISTRERINGSDATUM (ÅÅÅÅMMDD)           
007700     03 IDLEVNR-DUBLETT      PIC X(5).                                    
007800*                                 LEVERANTÖRNUMMER                        
007900     03 IDLEVNR-LOC-DUBLETT  PIC X(5).                                    
008000*                                 LEVERANTÖRNUMMER                        
008100     03 KDTIPPR              PIC 9.                                       
008200*                                 TIPPAT PRIS KOD                         
008300     03 IDKAT                OCCURS 3 TIMES                               
008400                             PIC X(5).                                    
008500*                                 KATALOGBETECKNING                       
008600     03 BELEVART             PIC X(30).                                   
008700*                                 LEVERANTÖRENS ARTIKELBENÄMNING          
008800     03 FLGEMFMC             PIC X.                                       
008900*                                 GEMENSAM FORD/MPNR ARTIKEL              
009000     03 IDPROJUP             PIC X(8).                                    
009100*                                 PROJEKTUPPDRAG                          
009200     03 BEARTEXT             PIC X(100).                                  
009300*                                 UTÖKAD ARTIKELBENÄMNING                 
009400     03 TIURPROD             PIC 9(4).                                    
009500*                                 DATUM UTGÅTT UR PROD   (ÅÅVV)           
009600*** END OF VILMAII-COPY LENGTH= 340 BYTES                                 
