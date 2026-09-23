000100 01  W61530.                                                              
000200*                                 LAGERPLATSFREKVENS                      
000300     03 IDARTNR              PIC S9(9)           COMP-3.                  
000400*                                 ARTIKELNUMMER                           
000500     03 BEART-ENG            PIC X(25).                                   
000600*                                 ARTIKELBENÄMNING                        
000700     03 IDDC                 PIC X(2).                                    
000800*                                 IDENTIFIERARE LAGER                     
000900     03 ADLAGOMR             PIC 9(2).                                    
001000*                                 LAGEROMRÅDE                             
001100     03 ADGANG               PIC S9(3)           COMP-3.                  
001200*                                 GÅNG                                    
001300     03 ADPLATS              PIC S9(5)           COMP-3.                  
001400*                                 LAGERPLATSNUMMER                        
001500     03 KDSTOR               PIC X(3).                                    
001600*                                 STORAGE CODE                            
001700     03 KDFREQ               PIC X(2).                                    
001800*                                 FREQUENCY CODE                          
001900     03 KDTECKEN             PIC X.                                       
002000*                                 PLUS ELLER MINUS (+ -)                  
002100     03 KVPB-FOM             PIC S9(6)V9(1)      COMP-3.                  
002200*                                 PERIODBEHOV FOM (PROGNOS)               
002300     03 KVPB-TOM             PIC S9(6)V9(1)      COMP-3.                  
002400*                                 PERIODBEHOV TOM (PROGNOS)               
002500     03 RELOCFAC             PIC 9V9(2).                                  
002600*                                 RELOCTION FACTOR                        
002700     03 KVPB-TOT             PIC S9(6)V9(1)      COMP-3.                  
002800*                                 TOTALT PERIODBEHOV                      
002900     03 FLSEASON             PIC X.                                       
003000*                                 JA/NEJ-FLAGGA                           
003100     03 ADLAGOMR-OLD         PIC S9(3)           COMP-3.                  
003200*                                 LAGEROMRÅDE                             
003300     03 ADGANG-OLD           PIC S9(3)           COMP-3.                  
003400*                                 GÅNG                                    
003500     03 ADPLATS-OLD          PIC S9(5)           COMP-3.                  
003600*                                 LAGERPLATSNUMMER                        
003700     03 TIRELOC-DATE         PIC S9(7)           COMP-3.                  
003800*                                 RAPPORTERINGSDATUM INLAGD (R32)         
003900     03 KVQPACK-3            PIC S9(5)           COMP-3.                  
004000*                                 ANTAL I Q3 FÖRPACKNING                  
004100     03 FLCDART              PIC X.                                       
004200*                                 CROSS-DOCKING PART                      
004300     03 VKART                PIC S9(7)           COMP-3.                  
004400*                                 ARTIKELVIKT (G)                         
004500*** END OF VILMAII-COPY LENGTH= 80 BYTES                                  
