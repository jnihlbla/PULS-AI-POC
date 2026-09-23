000100 01  W11606-CTX.                                                          
000200*                                 ARTIKELDATA FÖR VIPSSÄNDNIG             
000300     03 IDARTNR              PIC S9(9)           COMP-3.                  
000400*                                 ARTIKELNUMMER                           
000500     03 W11606-ART-GRP.                                                   
000600*                                 PARTSINFO                               
000700        05 IDFKNGRP          PIC S9(5)           COMP-3.                  
000800*                                 FUNKTIONSGRUPP                          
000900        05 IDSTATNR          OCCURS 5 TIMES                               
001000                             PIC S9(9)           COMP-3.                  
001100*                                 STATISTISKT NUMMER                      
001200*                                 1 = NORSKT                              
001300*                                 2 = ENGELSKT                            
001400*                                 3 = BELGISKT                            
001500*                                 4 = PERUANSKT                           
001600*                                 5 = SVENSKT                             
001700*                                 6 =                                     
001800        05 IDPROJ            PIC X(4).                                    
001900*                                 PARTS PROJEKTIDENTITET                  
002000        05 IDLEVNR           PIC X(5).                                    
002100*                                 LEVERANTÖRNUMMER                        
002200        05 KDAGE             PIC X.                                       
002300*                                 AGE-CODE                                
002400        05 KDARTURS          PIC X(2).                                    
002500*                                 ARTIKELURSPRUNGSKOD                     
002600        05 KDBBCL            PIC 9.                                       
002700*                                 RETURNERBAR ARTIKEL                     
002800        05 KDBPSR            PIC S9              COMP-3.                  
002900*                                 BASLAGERFÖRSLAGSNIVÅ                    
003000        05 KDERS             PIC S9(3)           COMP-3.                  
003100*                                 ERSÄTTNINGSKOD                          
003200        05 KDERS-UTG         PIC S9(3)           COMP-3.                  
003300*                                 ERSÄTTNINGSKOD UTGÅNGEN ARTIKEL         
003400        05 KDPRODSL          PIC S9(3)           COMP-3.                  
003500*                                 PRODUKTSLAG                             
003600        05 KDPSLLOC          PIC 9(2).                                    
003700*                                 PRODUKTSLAG LOKALT                      
003800        05 KDSORT            PIC X(2).                                    
003900*                                 SORT-KOD                                
004000        05 KDSRA             PIC S9(3)           COMP-3.                  
004100*                                 SRA-KOD                                 
004200        05 KDVSOP            PIC S9(3)           COMP-3.                  
004300*                                 VSOP-KOD                                
004400        05 KVQPACK-0         PIC S9(5)           COMP-3.                  
004500*                                 ANTAL I Q0 FÖRPACKNING                  
004600        05 PRARTSJK          PIC S9(7)V9(2)      COMP-3.                  
004700*                                 ARTIKELNS SJÄLVKOSTNAD                  
004800        05 PRARTSTD          PIC S9(7)V9(2)      COMP-3.                  
004900*                                 ARTIKELSTANDARDPRIS                     
005000        05 TIFINLV           PIC S9(5)           COMP-3.                  
005100*                                 PUBLICERINGSVECKA, (ÅÅVVD  D=1)         
005200        05 VLARTNTO          PIC S9(8)V9(1)      COMP-3.                  
005300*                                 ARTIKELVOLYM NETTO (CM3)                
005400        05 VKART             PIC S9(7)           COMP-3.                  
005500*                                 ARTIKELVIKT (G)                         
005600        05 KDUART            PIC X.                                       
005700*                                 UNDANTAGSARTIKEL                        
005800     03 W11606-BEN-GRP       OCCURS 10 TIMES.                             
005900        05 IDSKYLT           PIC X(3).                                    
006000*                                 NATIONALITETSTECKEN                     
006100*                                 SPRÅKIDENTIFIKATION                     
006200        05 BEART             PIC X(25).                                   
006300*                                 ARTIKELBENÄMNING                        
006400     03 KDTIPPR              PIC S9              COMP-3.                  
006500*                                 TIPPAT PRIS KOD                         
006600     03 IDKAT                OCCURS 3 TIMES                               
006700                             PIC X(5).                                    
006800*                                 KATALOGBETECKNING                       
006900     03 BELEVART             PIC X(30).                                   
007000*                                 LEVERANTÖRENS ARTIKELBENÄMNING          
007100     03 FLGEMFMC             PIC X.                                       
007200*                                 GEMENSAM FORD/MPNR ARTIKEL              
007300     03 IDPROJUP             PIC X(8).                                    
007400*                                 PROJEKTUPPDRAG                          
007500     03 TIERSDAT             PIC S9(5)           COMP-3.                  
007600*                                 ERSÄTTNINGSDATUM  (ÅÅVVD)               
007700     03 TIURPROD             PIC S9(5)           COMP-3.                  
007800*                                 DATUM UTGÅTT UR PROD   (ÅÅVV)           
007900*** END OF VILMAII-COPY LENGTH= 428 BYTES                                 
