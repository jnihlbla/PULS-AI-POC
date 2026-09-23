000100 01  W11610A.                                                             
000200*                                 PARTSINFO TO VIPS FROM PULS             
000300*                                                                         
000400*                                                                         
000500     03 IDARTNR              PIC 9(9).                                    
000600*                                 ARTIKELNUMMER                           
000700     03 IDDC                 PIC X(2).                                    
000800*                                 IDENTIFIERARE LAGER                     
000900     03 RAD.                                                              
001000*                                 PARTSINFO TILL VIPS                     
001100        05 IDFKNGRP          PIC 9(4).                                    
001200*                                 FUNKTIONSGRUPP                          
001300        05 KDSRA             PIC 9.                                       
001400*                                 EG/EFTA-KOD           KDSRA-002         
001500        05 KVQPACK-0         PIC 9(5).                                    
001600*                                 ANTAL I Q0 FÖRPACKNING                  
001700        05 KDARTURS          PIC X(2).                                    
001800*                                 ARTIKELURSPRUNGSKOD                     
001900        05 KDPRODSL          PIC 9(2).                                    
002000*                                 PRODUKTSLAG                             
002100        05 VLARTNTO          PIC 9(8)V9(1).                               
002200*                                 ARTIKELVOLYM (CM3)                      
002300        05 VKART             PIC 9(7).                                    
002400*                                 ARTIKELVIKT (G)                         
002500        05 KDVSOP            PIC 9(3).                                    
002600*                                 VSOP-KOD                                
002700        05 IDSTATNR          PIC 9(9).                                    
002800*                                 STATISTISKT NUMMER                      
002900*                                 1 = NORSKT                              
003000*                                 2 = ENGELSKT                            
003100*                                 3 = BELGISKT                            
003200*                                 4 = PERUANSKT                           
003300*                                 5 = SVENSKT                             
003400*                                 6 =                                     
003500        05 KDSORT            PIC X(2).                                    
003600*                                 SORT-KOD                                
003700        05 KDERS             PIC 9(2).                                    
003800*                                 ERSÄTTNINGSKOD                          
003900        05 KDBPSR            PIC 9.                                       
004000*                                 BASLAGERFÖRSLAGSNIVÅ                    
004100        05 KDBBCL            PIC 9.                                       
004200*                                 RETURNERBAR ARTIKEL                     
004300        05 IDLEVNR           PIC X(5).                                    
004400*                                 LEVERANTÖRNUMMER                        
004500        05 KDAGE             PIC X.                                       
004600*                                 AGE-CODE                                
004700        05 IDPROJ            PIC X(4).                                    
004800*                                 PARTS PROJEKTIDENTITET                  
004900        05 PRARTSJK          PIC 9(7)V9(2).                               
005000*                                 ARTIKELNS SJÄLVKOSTNAD                  
005100        05 PRARTSTD          PIC 9(7)V9(2).                               
005200*                                 ARTIKELSTANDARDPRIS                     
005300        05 BEART-L1          PIC X(25).                                   
005400*                                 ARTIKELBENÄMNING                        
005500        05 BEART-L2          PIC X(25).                                   
005600*                                 ARTIKELBENÄMNING                        
005700        05 KDPSLLOC          PIC 9(2).                                    
005800*                                 PRODUKTSLAG LOKALT                      
005900        05 IDLEVNR-LOC       PIC X(5).                                    
006000*                                 LEVERANTÖRNUMMER                        
006100        05 KDSTANAUTG        PIC X.                                       
006200*                                 ERSÄTTNINGSKOD USA/CAN                  
006300        05 FLOVRLAG          PIC X.                                       
006400*                                 ÖVERLAGER                               
006500        05 FLSOFTWARE        PIC X.                                       
006600*                                 ALLMÄN FLAGGA                           
006700     03 TIFINLV              PIC 9(5).                                    
006800*                                 PUBLICERINGSVECKA, (ÅÅVVD  D=1)         
006900     03 KDTIPPR              PIC 9.                                       
007000*                                 TIPPAT PRIS KOD                         
007100     03 IDKAT                OCCURS 3 TIMES                               
007200                             PIC X(5).                                    
007300*                                 KATALOGBETECKNING                       
007400     03 BELEVART             PIC X(30).                                   
007500*                                 LEVERANTÖRENS ARTIKELBENÄMNING          
007600     03 FLGEMFMC             PIC X.                                       
007700*                                 GEMENSAM FORD/MPNR ARTIKEL              
007800     03 IDPROJUP             PIC X(8).                                    
007900*                                 PROJEKTUPPDRAG                          
008000     03 TIURPROD             PIC 9(4).                                    
008100*                                 DATUM UTGÅTT UR PROD   (ÅÅVV)           
008200     03 BEARTEXT             PIC X(100).                                  
008300*                                 UTÖKAD ARTIKELBENÄMNING                 
008400*** END OF VILMAII-COPY LENGTH= 311 BYTES                                 
