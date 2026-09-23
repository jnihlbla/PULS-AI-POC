000100 01  SEQI-WDT1I1-CTX.                                                     
000200*                                 SEKUNDÄRT INDEX TILL WDT1               
000300*                                 PÅFYLLNING AV PLOCKSATS                 
000400*                                 LAGEROMR-FOM INGÅNG                     
000500*                                 FYSISK NYCKEL = WDT1I1KY                
000600*                                 (IDDC+ADLAGOMR-F+ADLAGOMR-T +           
000700*                                  KDPRIO+ADGANG-F+ADPLATS-F +            
000800*                                                                         
000900*                                  KDSTAPF + TIORDTIME)                   
001000*                                 SEKUNDÄR NYCKEL: WDT1ISEQ               
001100*                                 (IDDC+ADLAGOMR-F+ADLAGOMR-T +           
001200*                                  KDPRIO)                                
001300     03 SEQI-IDDC            PIC X(2).                                    
001400*                                 IDENTIFIERARE LAGER                     
001500*                                 WAREHOUSE IDENTIFIER                    
001600     03 SEQI-ADLAGOMR-FOM    PIC S9(3)           COMP-3.                  
001700*                                 LAGEROMRÅDE FRÅN OCH MED                
001800*                                 AREA ADDRESS FROM                       
001900     03 SEQI-ADLAGOMR-TOM    PIC S9(3)           COMP-3.                  
002000*                                 LAGEROMRÅDE TILL OCH MED                
002100*                                 AREA ADDRESS TO                         
002200     03 SEQI-KDPRIO-PF       PIC S9              COMP-3.                  
002300*                                 PRIORITETSKOD PÅFYLLNAD                 
002400*                                 PRIORITY FILLUP PICKING ADDRESS         
002500     03 SEQI-ADGANG-FOM      PIC S9(3)           COMP-3.                  
002600*                                 GÅNG FRÅN OCH MED                       
002700*                                 AISLE ADDRESS FROM                      
002800     03 SEQI-ADPLATS-FOM     PIC S9(5)           COMP-3.                  
002900*                                 LAGERPLATS FRÅN OCH MED                 
003000*                                 LOCATION ADDRESS FROM                   
003100     03 SEQI-KDSTAPF         PIC X.                                       
003200*                                 STATUS PÅ PÅFYLLNING                    
003300*                                 STATUS FOR FILLING                      
003400     03 SEQI-TIORDTIME       PIC 9(12).                                   
003500*                                 ORDER DATE AND TIME                     
003600*                                 (YYMMDDHHMM[SS])                        
003700     03 SEQI-IDWDT101        PIC X(19).                                   
003800*                                 NYCKEL TILL WDT101                      
003900*                                 KEY TO WDT101                           
004000*** END OF VILMAII-COPY LENGTH= 44 BYTES                                  
