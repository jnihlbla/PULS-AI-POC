000100 01  SEQA-WDT1A1-CTX.                                                     
000200*                                 SEKUNDÄRT INDEX TILL WDT1               
000300*                                 PÅFYLLNING AV PLOCKSATS                 
000400*                                 ADLAGOMR-FOM INGÅNG                     
000500*                                 FYSISK NYCKEL = WDT1A1KY                
000600*                                 (IDDC + ADLAGOMR-FOM + KDSTAPF          
000700*                                       + KDPRIO-PF + TIORDTIME)          
000800*                                 SEKUNDÄR NYCKEL: WDT1ASEQ               
000900*                                 (IDDC + ADLAGOMR-FOM + KDSTAPF)         
001000     03 SEQA-IDDC            PIC X(2).                                    
001100*                                 IDENTIFIERARE LAGER                     
001200*                                 WAREHOUSE IDENTIFIER                    
001300     03 SEQA-ADLAGOMR-FOM    PIC S9(3)           COMP-3.                  
001400*                                 LAGEROMRÅDE FRÅN OCH MED                
001500*                                 AREA ADDRESS FROM                       
001600     03 SEQA-KDSTAPF         PIC X.                                       
001700*                                 STATUS PÅ PÅFYLLNING                    
001800*                                 STATUS FOR FILLING                      
001900     03 SEQA-KDPRIO-PF       PIC S9              COMP-3.                  
002000*                                 PRIORITETSKOD PÅFYLLNAD                 
002100*                                 PRIORITY FILLUP PICKING ADDRESS         
002200     03 SEQA-TIORDTIME       PIC 9(12).                                   
002300*                                 ORDER DATE AND TIME                     
002400*                                 (YYMMDDHHMM[SS])                        
002500     03 SEQA-IDWDT101        PIC X(19).                                   
002600*                                 NYCKEL TILL WDT101                      
002700*                                 KEY TO WDT101                           
002800*** END OF VILMAII-COPY LENGTH= 37 BYTES                                  
