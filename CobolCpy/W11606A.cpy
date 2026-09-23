000100 01  W11606A-CTX.                                                         
000200*                                 ARTIKELDATA FÖR VIPSSÄNDNIG             
000300     03 IDARTNR              PIC S9(9)           COMP-3.                  
000400*                                 ARTIKELNUMMER                           
000500     03 IDDC                 PIC X(2).                                    
000600*                                 IDENTIFIERARE LAGER                     
000700     03 W11606-ART-GRP.                                                   
000800*                                 PARTSINFO                               
000900        05 IDFKNGRP          PIC S9(5)           COMP-3.                  
001000*                                 FUNKTIONSGRUPP                          
001100        05 IDSTATNR          OCCURS 5 TIMES                               
001200                             PIC S9(9)           COMP-3.                  
001300*                                 STATISTISKT NUMMER                      
001400*                                 1 = NORSKT                              
001500*                                 2 = ENGELSKT                            
001600*                                 3 = BELGISKT                            
001700*                                 4 = PERUANSKT                           
001800*                                 5 = SVENSKT                             
001900*                                 6 =                                     
002000        05 IDPROJ            PIC X(4).                                    
002100*                                 PARTS PROJEKTIDENTITET                  
002200        05 IDLEVNR           PIC X(5).                                    
002300*                                 LEVERANTÖRNUMMER                        
002400        05 KDAGE             PIC X.                                       
002500*                                 AGE-CODE                                
002600        05 KDARTURS          PIC X(2).                                    
002700*                                 ARTIKELURSPRUNGSKOD                     
002800        05 KDBBCL            PIC 9.                                       
002900*                                 RETURNERBAR ARTIKEL                     
003000        05 KDBPSR            PIC S9              COMP-3.                  
003100*                                 BASLAGERFÖRSLAGSNIVÅ                    
003200        05 KDERS             PIC S9(3)           COMP-3.                  
003300*                                 ERSÄTTNINGSKOD                          
003400        05 KDERS-UTG         PIC S9(3)           COMP-3.                  
003500*                                 ERSÄTTNINGSKOD UTGÅNGEN ARTIKEL         
003600        05 KDPRODSL          PIC S9(3)           COMP-3.                  
003700*                                 PRODUKTSLAG                             
003800        05 KDPSLLOC          PIC 9(2).                                    
003900*                                 PRODUKTSLAG LOKALT                      
004000        05 KDSORT            PIC X(2).                                    
004100*                                 SORT-KOD                                
004200        05 KDSRA             PIC S9(3)           COMP-3.                  
004300*                                 SRA-KOD                                 
004400        05 KDVSOP            PIC S9(3)           COMP-3.                  
004500*                                 VSOP-KOD                                
004600        05 KVQPACK-0         PIC S9(5)           COMP-3.                  
004700*                                 ANTAL I Q0 FÖRPACKNING                  
004800        05 PRARTSJK          PIC S9(7)V9(2)      COMP-3.                  
004900*                                 ARTIKELNS SJÄLVKOSTNAD                  
005000        05 PRARTSTD          PIC S9(7)V9(2)      COMP-3.                  
005100*                                 ARTIKELSTANDARDPRIS                     
005200        05 TIFINLV           PIC S9(5)           COMP-3.                  
005300*                                 PUBLICERINGSVECKA, (ÅÅVVD  D=1)         
005400        05 VLARTNTO          PIC S9(8)V9(1)      COMP-3.                  
005500*                                 ARTIKELVOLYM (CM3)                      
005600        05 VKART             PIC S9(7)           COMP-3.                  
005700*                                 ARTIKELVIKT (G)                         
005800        05 KDUART            PIC X.                                       
005900*                                 UNDANTAGSARTIKEL                        
006000     03 W11606-BEN-GRP       OCCURS 10 TIMES.                             
006100        05 IDSKYLT           PIC X(3).                                    
006200*                                 NATIONALITETSTECKEN                     
006300*                                 SPRÅKIDENTIFIKATION                     
006400        05 BEART             PIC X(25).                                   
006500*                                 ARTIKELBENÄMNING                        
006600     03 KDTIPPR              PIC S9              COMP-3.                  
006700*                                 TIPPAT PRIS KOD                         
006800     03 IDKAT                OCCURS 3 TIMES                               
006900                             PIC X(5).                                    
007000*                                 KATALOGBETECKNING                       
007100     03 BELEVART             PIC X(30).                                   
007200*                                 LEVERANTÖRENS ARTIKELBENÄMNING          
007300     03 FLGEMFMC             PIC X.                                       
007400*                                 GEMENSAM FORD/MPNR ARTIKEL              
007500     03 IDPROJUP             PIC X(8).                                    
007600*                                 PROJEKTUPPDRAG                          
007700     03 TIERSDAT             PIC S9(5)           COMP-3.                  
007800*                                 ERSÄTTNINGSDATUM  (ÅÅVVD)               
007900     03 TIURPROD             PIC S9(5)           COMP-3.                  
008000*                                 DATUM UTGÅTT UR PROD   (ÅÅVV)           
008100*** END OF VILMAII-COPY LENGTH= 430 BYTES                                 
