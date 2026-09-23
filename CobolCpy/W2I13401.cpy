000100 01  MID-W2I13401.                                                        
000200*                                 MID-COPYTEXT FÖR W2013400               
000300     03 MID-IDARTNR-IN       PIC X(9).                                    
000400*                                 ARTIKELNUMMER                           
000500     03 MID-IDARTNR-UT       PIC X(9).                                    
000600*                                 ARTIKELNUMMER                           
000700     03 MID-INPUT.                                                        
000800*                                 INDATA FÖR UPPDATERING                  
000900        05 MID-KVPB-TPO-C1-UPP                                            
001000                             PIC X(8).                                    
001100*                                 PERIODBEHOV FÖR TPO1 OCH TPO2           
001200        05 MID-FLRADREF-UPP  PIC X.                                       
001300*                                 KOMPLETTERANDE INFO. KRÄVS              
001400        05 MID-FLTPO1-UPP    PIC X.                                       
001500*                                 ARTIKELN GODKÄND FÖR TPO1               
001600        05 MID-KVFRYSTI-UPP  PIC 9(2).                                    
001700*                                 FRYSTID FÖR TPO-ORDER                   
001800        05 MID-KDOPPLAN-UPP  PIC X.                                       
001900*                                 OPTIMAL PLAN INOM FRYSTID               
002000        05 MID-KDUART-UPP    PIC X.                                       
002100*                                 UNDANTAGSARTIKEL                        
002200        05 MID-FLMANOSK-UPP  PIC X.                                       
002300*                                 MANUELLT SATT ORDERSÄRKOSTNAD           
002400        05 MID-FLAUTREL-UPP  PIC X.                                       
002500*                                 AUT. SPÄRR FÖR VOR RELEASE              
002600        05 MID-PRORDSK-UPP   PIC X(8).                                    
002700*                                 ORDERSÄRKOSTNAD                         
002800*** END OF VILMAII-COPY LENGTH= 42 BYTES                                  
