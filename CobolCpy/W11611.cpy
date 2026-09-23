000100 01  W11611.                                                              
000200*                                 PARTSINFO VIA VIPS TILL USA/CAN         
000300*                                                                         
000400*                                                                         
000500     03 IDARTNR              PIC 9(9).                                    
000600*                                 ARTIKELNUMMER                           
000700     03 RAD.                                                              
000800*                                 PARTSINFO TILL VIPS                     
000900        05 IDFKNGRP          PIC 9(4).                                    
001000*                                 FUNKTIONSGRUPP                          
001100        05 KDSRA             PIC 9.                                       
001200*                                 EG/EFTA-KOD           KDSRA-002         
001300        05 KVQPACK-0         PIC 9(5).                                    
001400*                                 ANTAL I Q0 FÖRPACKNING                  
001500        05 KDARTURS          PIC X(2).                                    
001600*                                 ARTIKELURSPRUNGSKOD                     
001700        05 KDPRODSL          PIC 9(2).                                    
001800*                                 PRODUKTSLAG                             
001900        05 VLARTNTO          PIC 9(8)V9(1).                               
002000*                                 ARTIKELVOLYM NETTO (CM3)                
002100        05 VKART             PIC 9(7).                                    
002200*                                 ARTIKELVIKT (G)                         
002300        05 KDVSOP            PIC 9(3).                                    
002400*                                 VSOP-KOD                                
002500        05 IDSTATNR          PIC 9(9).                                    
002600*                                 STATISTISKT NUMMER                      
002700*                                 1 = NORSKT                              
002800*                                 2 = ENGELSKT                            
002900*                                 3 = BELGISKT                            
003000*                                 4 = PERUANSKT                           
003100*                                 5 = SVENSKT                             
003200*                                 6 =                                     
003300        05 KDSORT            PIC X(2).                                    
003400*                                 SORT-KOD                                
003500        05 KDERS             PIC 9(2).                                    
003600*                                 ERSÄTTNINGSKOD                          
003700        05 KDBPSR            PIC 9.                                       
003800*                                 BASLAGERFÖRSLAGSNIVÅ                    
003900        05 KDBBCL            PIC 9.                                       
004000*                                 RETURNERBAR ARTIKEL                     
004100        05 IDLEVNR           PIC X(5).                                    
004200*                                 LEVERANTÖRNUMMER                        
004300        05 KDAGE             PIC X.                                       
004400*                                 AGE-CODE                                
004500        05 IDPROJ            PIC X(4).                                    
004600*                                 PARTS PROJEKTIDENTITET                  
004700        05 PRARTSJK          PIC 9(7)V9(2).                               
004800*                                 ARTIKELNS SJÄLVKOSTNAD                  
004900        05 PRARTSTD          PIC 9(7)V9(2).                               
005000*                                 ARTIKELSTANDARDPRIS                     
005100        05 BEART-L1          PIC X(25).                                   
005200*                                 ARTIKELBENÄMNING                        
005300        05 BEART-L2          PIC X(25).                                   
005400*                                 ARTIKELBENÄMNING                        
005500        05 KDPSLLOC          PIC 9(2).                                    
005600*                                 PRODUKTSLAG LOKALT                      
005700        05 IDLEVNR-LOC       PIC X(5).                                    
005800*                                 LEVERANTÖRNUMMER                        
005900        05 KDSTANAUTG        PIC X.                                       
006000*                                 ERSÄTTNINGSKOD USA/CAN                  
006100        05 FLOVRLAG          PIC X.                                       
006200*                                 ÖVERLAGER                               
006300        05 FLSOFTWARE        PIC X.                                       
006400*                                 ALLMÄN FLAGGA                           
006500     03 TIFINLV              PIC 9(5).                                    
006600*                                 PUBLICERINGSVECKA, (ÅÅVVD  D=1)         
006700     03 IDLEVNR-MOTSV        PIC X(5).                                    
006800*                                 LEVERANTÖRNUMMER                        
006900     03 IDLEVNR-LOC-MOTSV    PIC X(5).                                    
007000*                                 LEVERANTÖRNUMMER                        
007100     03 KDTIPPR              PIC 9.                                       
007200*                                 TIPPAT PRIS KOD                         
007300*** END OF VILMAII-COPY LENGTH= 161 BYTES                                 
