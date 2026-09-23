000100 01  MOD-W3O15401.                                                        
000200*                                 MOD-COPYTEXT FÖR BILD 3154              
000300*                                 CORE ALARM                              
000400     03 MOD-IDTRANS          PIC X(4).                                    
000500*                                 BILDNUMMER                              
000600     03 MOD-TEMFSFEL         PIC X(40).                                   
000700*                                 MFS FELMEDDELANDE                       
000800     03 MOD-TIAAVV-FOM-IN    PIC X(2).                                    
000900*                                 MFS BEHANDLING AV INPUTFÄLT             
001000     03 MOD-TIAAVV-TOM-IN    PIC X(2).                                    
001100*                                 MFS BEHANDLING AV INPUTFÄLT             
001200     03 MOD-IDDISTR-IN       PIC X(2).                                    
001300*                                 MFS BEHANDLING AV INPUTFÄLT             
001400     03 MOD-IDFKNGRP-IN      PIC X(2).                                    
001500*                                 MFS BEHANDLING AV INPUTFÄLT             
001600     03 MOD-IDARTNR-IN       PIC X(2).                                    
001700*                                 MFS BEHANDLING AV INPUTFÄLT             
001800     03 MOD-TIAAVV-FOM-UT    PIC X(4).                                    
001900*                                 ÅR - VECKA  (ÅÅVV)                      
002000     03 MOD-TIAAVV-TOM-UT    PIC X(4).                                    
002100*                                 ÅR - VECKA  (ÅÅVV)                      
002200     03 MOD-IDDISTR-UT       PIC X(4).                                    
002300*                                 DISTRIKTNUMMER                          
002400     03 MOD-IDFKNGRP-UT      PIC X(4).                                    
002500*                                 FUNKTIONSGRUPP                          
002600     03 MOD-IDARTNR-UT       PIC X(8).                                    
002700*                                 ARTIKELNUMMER                           
002800     03 MOD-DATA-UT          OCCURS 15 TIMES.                             
002900*                                 RAPPORTERINGS-FÄLT                      
003000        05 MOD-CMD-ATTR      PIC X(2).                                    
003100*                                 MFS ATTRIBUTFÄLT                        
003200        05 MOD-CMD           PIC X.                                       
003300        05 MOD-TAB-IDARTNR   PIC Z(7)9.                                   
003400*                                 ARTIKELNUMMER                           
003500        05 MOD-TAB-BEART-ENG PIC X(25).                                   
003600*                                 ENGELSK ARTIKELBENÄMNING                
003700        05 MOD-TAB-OUTLEV    PIC -(7)9.                                   
003800*                                 ANTAL                                   
003900        05 MOD-TAB-INLEV     PIC -(7)9.                                   
004000*                                 ANTAL                                   
004100        05 MOD-TAB-DIFF      PIC -(7)9.                                   
004200*                                 ANTAL                                   
004300        05 MOD-TAB-REPROCENT PIC -(3)9.                                   
004400*                                 ALLMÄNT PROCENTTALSFÄLT                 
004500     03 MOD-KVANTAL          PIC Z(5)9.                                   
004600*                                 ANTAL                                   
004700     03 MOD-TEMFSINF         PIC X(55).                                   
004800*                                 INFORMATIONSMEDDELANDE                  
004900*** END OF VILMAII-COPY LENGTH= 1099 BYTES                                
