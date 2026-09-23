000100 01  ART-W4610502.                                                        
000200*                                 ARTIKELINFORMATION TILL NOAC            
000300*                                 PT-050 VER 2                            
000400     03 ART-IDPTYP           PIC X(3).                                    
000500*                                 POSTTYP                                 
000600     03 ART-IDARTNR          PIC S9(9)           COMP-3.                  
000700*                                 ARTIKELNUMMER                           
000800     03 ART-REKSIFFR         PIC S9              COMP-3.                  
000900*                                 KONTROLLSIFFRA                          
001000     03 ART-IDFKNGRP         PIC S9(5)           COMP-3.                  
001100*                                 FUNKTIONSGRUPP                          
001200     03 ART-KDSRA            PIC S9(3)           COMP-3.                  
001300*                                 SRA-KOD                                 
001400     03 ART-KVQPACK-1        PIC S9(5)           COMP-3.                  
001500*                                 ANTAL I Q1 FÖRPACKNING                  
001600     03 ART-KDCLAGER         PIC S9              COMP-3.                  
001700*                                 CENTRALLAGERKOD                         
001800     03 ART-KDARTURS         PIC S9(3)           COMP-3.                  
001900*                                 ARTIKELURSPRUNGSKOD                     
002000     03 ART-KDPRODSL         PIC S9(3)           COMP-3.                  
002100*                                 PRODUKTSLAG                             
002200     03 ART-VLARTNTO         PIC S9(8)V9(1)      COMP-3.                  
002300*                                 ARTIKELVOLYM NETTO (CM3)                
002400     03 ART-VKART            PIC S9(7)           COMP-3.                  
002500*                                 ARTIKELVIKT (G)                         
002600     03 ART-KDVSOP           PIC S9(3)           COMP-3.                  
002700*                                 VSOP-KOD                                
002800     03 ART-PRARTBTO-EXP     PIC S9(7)V9(2)      COMP-3.                  
002900*                                 BRUTTOPRIS EXPORT (FOB-PRIS)            
003000     03 ART-IDSTATNR         OCCURS 8 TIMES                               
003100                             PIC S9(9)           COMP-3.                  
003200*                                 STATISTISKT NUMMER                      
003300*                                 1 = NORSKT                              
003400*                                 2 = ENGELSKT                            
003500*                                 3 = BELGISKT                            
003600*                                 4 = PERUANSKT                           
003700*                                 5 = SVENSKT                             
003800*                                 6 = VENEZUELANSKT                       
003900*                                 1 = FRANSKT                             
004000*                                 1 = FUNKTIONSGRUPP                      
004100     03 ART-KDUART           PIC X.                                       
004200*                                 UNDANTAGSARTIKEL                        
004300     03 ART-KDSORT           PIC X(2).                                    
004400*                                 SORT-KOD                                
004500     03 ART-KDERS            PIC S9(3)           COMP-3.                  
004600*                                 ERSÄTTNINGSKOD                          
004700     03 ART-KDBPSR           PIC S9              COMP-3.                  
004800*                                 BASLAGERFÖRSLAGSNIVÅ                    
004900     03 ART-IDLEVNR          PIC S9(5)           COMP-3.                  
005000*                                 LEVERANTÖRNUMMER                        
005100     03 ART-KDBBCL           PIC 9.                                       
005200*                                 RETURNERBAR ARTIKEL                     
005300     03 ART-KDRABATT         PIC 9(3).                                    
005400*                                 RABATTKOD                               
005500     03 FILLER               PIC X(9).                                    
005600*** END COPY W4610502    LENGTH=100                                       
