000100 01  W11614-CTX.                                                          
000200*                                 PARTSINFO VIA VIPS (EJ USA/CAN)         
000300*                                                                         
000400     03 IDARTNR              PIC 9(9).                                    
000500*                                 ARTIKELNUMMER                           
000600     03 W11614-001-GRP.                                                   
000700*                                 PARTSINFO TILL VIPS                     
000800        05 FLOVRLAG          PIC X.                                       
000900*                                 ÖVERLAGER                               
001000        05 FLSOFTWARE        PIC X.                                       
001100*                                 ALLMÄN FLAGGA                           
001200        05 IDFKNGRP          PIC S9(5)           COMP-3.                  
001300*                                 FUNKTIONSGRUPP                          
001400        05 IDSTATNR          PIC S9(9)           COMP-3.                  
001500*                                 STATISTISKT NUMMER                      
001600*                                 1 = NORSKT                              
001700*                                 2 = ENGELSKT                            
001800*                                 3 = BELGISKT                            
001900*                                 4 = PERUANSKT                           
002000*                                 5 = SVENSKT                             
002100*                                 6 =                                     
002200        05 IDPROJ            PIC X(4).                                    
002300*                                 PARTS PROJEKTIDENTITET                  
002400        05 IDLEVNR           PIC X(5).                                    
002500*                                 LEVERANTÖRNUMMER                        
002600        05 IDLEVNR-LOC       PIC X(5).                                    
002700*                                 LEVERANTÖRNUMMER                        
002800        05 KDSRA             PIC S9(3)           COMP-3.                  
002900*                                 SRA-KOD                                 
003000        05 KVQPACK-0         PIC S9(5)           COMP-3.                  
003100*                                 ANTAL I Q0 FÖRPACKNING                  
003200        05 KDARTURS          PIC X(2).                                    
003300*                                 ARTIKELURSPRUNGSKOD                     
003400        05 KDPRODSL          PIC S9(3)           COMP-3.                  
003500*                                 PRODUKTSLAG                             
003600        05 KDVSOP            PIC S9(3)           COMP-3.                  
003700*                                 VSOP-KOD                                
003800        05 KDSORT            PIC X(2).                                    
003900*                                 SORT-KOD                                
004000        05 KDERS             PIC S9(3)           COMP-3.                  
004100*                                 ERSÄTTNINGSKOD                          
004200        05 KDERS-UTG         PIC S9(3)           COMP-3.                  
004300*                                 ERSÄTTNINGSKOD UTGÅNGEN ARTIKEL         
004400        05 KDBPSR            PIC S9              COMP-3.                  
004500*                                 BASLAGERFÖRSLAGSNIVÅ                    
004600        05 KDBBCL            PIC 9.                                       
004700*                                 RETURNERBAR ARTIKEL                     
004800        05 KDAGE             PIC X.                                       
004900*                                 AGE-CODE                                
005000        05 KDPSLLOC          PIC 9(2).                                    
005100*                                 PRODUKTSLAG LOKALT                      
005200        05 KDSTANAUTG        PIC X.                                       
005300*                                 ERSÄTTNINGSKOD USA/CAN                  
005400        05 KDUART            PIC X.                                       
005500*                                 UNDANTAGSARTIKEL                        
005600        05 PRARTSJK          PIC S9(7)V9(2)      COMP-3.                  
005700*                                 ARTIKELNS SJÄLVKOSTNAD                  
005800        05 PRARTSTD          PIC S9(7)V9(2)      COMP-3.                  
005900*                                 ARTIKELSTANDARDPRIS                     
006000        05 TIFINLV           PIC S9(5)           COMP-3.                  
006100*                                 PUBLICERINGSVECKA, (ÅÅVVD  D=1)         
006200        05 VLARTNTO          PIC S9(8)V9(1)      COMP-3.                  
006300*                                 ARTIKELVOLYM NETTO (CM3)                
006400        05 VKART             PIC S9(7)           COMP-3.                  
006500*                                 ARTIKELVIKT (G)                         
006600        05 W11614-002-GRP    OCCURS 10 TIMES.                             
006700*                                 SPRÅK-RAD                               
006800           07 BEART          PIC X(25).                                   
006900*                                 ARTIKELBENÄMNING                        
007000     03 KDTIPPR              PIC 9.                                       
007100*                                 TIPPAT PRIS KOD                         
007200     03 IDKAT                OCCURS 3 TIMES                               
007300                             PIC X(5).                                    
007400*                                 KATALOGBETECKNING                       
007500     03 BELEVART             PIC X(30).                                   
007600*                                 LEVERANTÖRENS ARTIKELBENÄMNING          
007700     03 FLGEMFMC             PIC X.                                       
007800*                                 GEMENSAM FORD/MPNR ARTIKEL              
007900     03 IDPROJUP             PIC X(8).                                    
008000*                                 PROJEKTUPPDRAG                          
008100     03 TIERSDAT             PIC 9(5).                                    
008200*                                 ERSÄTTNINGSDATUM  (ÅÅVVD)               
008300     03 TIURPROD             PIC 9(4).                                    
008400*                                 DATUM UTGÅTT UR PROD   (ÅÅVV)           
008500*** END OF VILMAII-COPY LENGTH= 393 BYTES                                 
