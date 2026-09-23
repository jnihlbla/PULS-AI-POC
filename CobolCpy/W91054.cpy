000100 01  W91054.                                                              
000200*                                 FIL MED INFO TILL VR                    
000300*                                                                         
000400     03 IDARTNR              PIC S9(9)           COMP-3.                  
000500*                                 ARTIKELNUMMER                           
000600     03 BEART-ENG            PIC X(25).                                   
000700*                                 ENGELSK ARTIKELBENÄMNING                
000800     03 BEART-FRA            PIC X(25).                                   
000900*                                 FRANSK ARTIKELBENÄMNING                 
001000     03 BEART-SPA            PIC X(25).                                   
001100*                                 SPANSK ARTIKELBENÄMNING                 
001200     03 BEART-SVE            PIC X(25).                                   
001300*                                 SVENSK ARTIKELBENÄMNING                 
001400     03 BEART-TYS            PIC X(25).                                   
001500*                                 TYSK ARTIKELBENÄMNING                   
001600     03 FLABORT-UTG          PIC S9              COMP-3.                  
001700*                                 MÄRKNING ATT ARTIKELN UTGÅTT            
001800     03 FLTPO1               PIC X.                                       
001900*                                 ARTIKELN GODKÄND FÖR TPO1               
002000     03 IDFKNGRP             PIC S9(5)           COMP-3.                  
002100*                                 FUNKTIONSGRUPP                          
002200     03 IDSTATNR-TABELL      OCCURS 6 TIMES.                              
002300        05 IDSTATNR          PIC S9(9)           COMP-3.                  
002400*                                 STATISTISKT NUMMER                      
002500*                                 1 = NORSKT                              
002600*                                 2 = ENGELSKT                            
002700*                                 3 = BELGISKT                            
002800*                                 4 = PERUANSKT                           
002900*                                 5 = SVENSKT                             
003000*                                 6 =                                     
003100     03 KDAGE                PIC X.                                       
003200*                                 AGE-CODE                                
003300     03 KDARTURS             PIC X(2).                                    
003400*                                 ARTIKELURSPRUNGSKOD                     
003500     03 KDERS                PIC S9(3)           COMP-3.                  
003600*                                 ERSÄTTNINGSKOD                          
003700     03 KDFARLIG             PIC S9              COMP-3.                  
003800*                                 KOD FÖR FARLIGT GODS                    
003900     03 KDLTK                PIC S9              COMP-3.                  
004000*                                 LAGERTILLHÖRIGHETSKOD                   
004100     03 KDPRODSL             PIC S9(3)           COMP-3.                  
004200*                                 PRODUKTSLAG                             
004300     03 KDPRTILL             PIC S9              COMP-3.                  
004400*                                 PRISTILLÄMPNINGSKOD                     
004500     03 KDSORT               PIC X(2).                                    
004600*                                 SORT-KOD                                
004700     03 KDSRA                PIC S9(3)           COMP-3.                  
004800*                                 SRA-KOD                                 
004900     03 KDVSOP               PIC S9(3)           COMP-3.                  
005000*                                 VSOP-KOD                                
005100     03 KDVVKL               PIC S9              COMP-3.                  
005200*                                 VOLYMVÄRDESKLASS                        
005300     03 KVFRYSTI             PIC S9(3)           COMP-3.                  
005400*                                 FRYSTID FÖR TPO-ORDER                   
005500     03 KVQPACK-0            PIC S9(5)           COMP-3.                  
005600*                                 ANTAL I Q0 FÖRPACKNING                  
005700     03 KVQPACK-1            PIC S9(5)           COMP-3.                  
005800*                                 ANTAL I Q1 FÖRPACKNING                  
005900     03 KVQPACK-2            PIC S9(5)           COMP-3.                  
006000*                                 ANTAL I Q2 FÖRPACKNING                  
006100     03 KVQPACK-3            PIC S9(5)           COMP-3.                  
006200*                                 ANTAL I Q3 FÖRPACKNING                  
006300     03 KVQPACK-4            PIC S9(5)           COMP-3.                  
006400*                                 ANTAL I Q4 FÖRPACKNING                  
006500     03 PRARTBTO-EXP         PIC S9(7)V9(2)      COMP-3.                  
006600*                                 BRUTTOPRIS EXPORT (FOB-PRIS)            
006700     03 VKART                PIC S9(7)           COMP-3.                  
006800*                                 ARTIKELVIKT (G)                         
006900     03 VLARTNTO             PIC S9(8)V9(1)      COMP-3.                  
007000*                                 ARTIKELVOLYM NETTO (CM3)                
007100     03 TIFINLV              PIC S9(5)           COMP-3.                  
007200*                                 PUBLICERINGSVECKA, (ÅÅVVD  D=1)         
007300     03 ADLAGOMR             PIC S9(3)           COMP-3.                  
007400*                                 LAGEROMRÅDE                             
007500     03 IDLEVNR-DDGS         PIC X(5).                                    
007600*                                 LEVERANTÖRNUMMER                        
007700     03 KDUART               PIC X.                                       
007800*                                 UNDANTAGSARTIKEL                        
007900     03 TIURPROD             PIC S9(5)           COMP-3.                  
008000*                                 DATUM UTGÅTT UR PROD   (ÅÅVV)           
008100*** END OF VILMAII-COPY LENGTH= 227 BYTES                                 
