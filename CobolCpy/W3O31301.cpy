000100 01  MOD-W3O31301.                                                        
000200*                                 MOD-COPYTEXT                            
000300*                                 FÖR W3O313                              
000400     03 MOD-IDTRANS          PIC X(4).                                    
000500*                                 BILDNUMMER                              
000600     03 MOD-TEMFSFEL         PIC X(40).                                   
000700*                                 MFS FELMEDDELANDE                       
000800     03 MOD-IDPROMR-IN.                                                   
000900*                                 PRISOMRÅDE (RABATTSTRUKTUR)             
001000        05 MOD-IDMARKBO      PIC X.                                       
001100*                                 MARKNADSBOLAGSKOD                       
001200        05 MOD-IDPROMRN      PIC X(2).                                    
001300*                                 PRISOMRÅDE LÖPNUMMER                    
001400     03 MOD-IDPROMR-UT.                                                   
001500*                                 PRISOMRÅDE (RABATTSTRUKTUR)             
001600        05 MOD-IDMARKBO      PIC X.                                       
001700*                                 MARKNADSBOLAGSKOD                       
001800        05 MOD-IDPROMRN      PIC X(2).                                    
001900*                                 PRISOMRÅDE LÖPNUMMER                    
002000     03 MOD-IDARTNR-IN       PIC X(9).                                    
002100*                                 ARTIKELNUMMER                           
002200     03 MOD-IDARTNR-UT       PIC X(9).                                    
002300*                                 ARTIKELNUMMER                           
002400     03 MOD-IDDISTR-IN       PIC X(4).                                    
002500*                                 DISTRIKTNUMMER                          
002600     03 MOD-IDDISTR-UT       PIC X(4).                                    
002700*                                 DISTRIKTNUMMER                          
002800     03 MOD-FLMRKVAL-IN      PIC X.                                       
002900*                                 FLAGGA FÖR MARKNADSVALUTA               
003000     03 MOD-FLMRKVAL-UT      PIC X.                                       
003100*                                 FLAGGA FÖR MARKNADSVALUTA               
003200     03 MOD-BEART            PIC X(25).                                   
003300*                                 ARTIKELBENÄMNING                        
003400     03 MOD-IDFKNGRP         PIC Z(3)9.                                   
003500*                                 FUNKTIONSGRUPP                          
003600     03 MOD-KDPRODSL         PIC Z9.                                      
003700*                                 PRODUKTSLAG                             
003800     03 MOD-RETAIL-PRIS      PIC Z(6)9.9(2).                              
003900*                                 BRUTTOPRIS PER MARKNAD (FOB)            
004000     03 MOD-PRARTSTD         PIC Z(6)9.9(2).                              
004100*                                 ARTIKELSTANDARDPRIS                     
004200     03 MOD-KDVALISO         PIC X(3).                                    
004300*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
004400     03 MOD-KDARTKAM         PIC Z(4)9.                                   
004500*                                 TRANSFER KOD                            
004600     03 MOD-ARTIKEL-RAB-FINNS                                             
004700                             PIC X(3).                                    
004800     03 MOD-MO-RAB-PRIS      PIC Z(6)9.9(2).                              
004900*                                 ARTIKELPRIS NETTO                       
005000     03 MOD-DO-RAB-PRIS      PIC Z(6)9.9(2).                              
005100*                                 ARTIKELPRIS NETTO                       
005200     03 MOD-MO-RAB           PIC Z9.9(2).                                 
005300*                                 ARTIKELRABATT BULKORDER                 
005400     03 MOD-DO-RAB           PIC Z9.9(2).                                 
005500*                                 ARTIKELRABATT DAGORDER                  
005600     03 MOD-NORMAL-RABATT    PIC 9(2).                                    
005700*                                 RABATTKOD (ARTIKELPRIS)                 
005800     03 MOD-MO-NOR-PRIS      PIC Z(6)9.9(2).                              
005900*                                 ARTIKELPRIS NETTO                       
006000     03 MOD-DO-NOR-PRIS      PIC Z(6)9.9(2).                              
006100*                                 ARTIKELPRIS NETTO                       
006200     03 MOD-MO-NOR-RAB       PIC Z9.9(2).                                 
006300*                                 ARTIKELRABATT BULKORDER                 
006400     03 MOD-DO-NOR-RAB       PIC Z9.9(2).                                 
006500*                                 ARTIKELRABATT DAGORDER                  
006600     03 MOD-TEMFSINF         PIC X(55).                                   
006700*                                 INFORMATIONSMEDDELANDE                  
006800*** END OF VILMAII-COPY LENGTH= 257 BYTES                                 
