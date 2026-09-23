000100 01  SEQH-WDT1H1-CTX.                                                     
000200*                                 SEKUNDÄRT INDEX TILL WDT1               
000300*                                 PÅFYLLNING AV PLOCKSATS                 
000400*                                 KDPRIO TOM PLATS INGÅNG                 
000500*                                 FYSISK NYCKEL = WDT1H1KY                
000600*                                 (IDDC + KDPRIO + ADLAGOMR-T +           
000700*                                  ADGANG-T + ADPLATS-T +                 
000800*                                  KDSTAPF + TIORDTIME)                   
000900*                                 SEKUNDÄR NYCKEL: WDT1HSEQ               
001000*                                 (IDDC + KDPRIO + ADLAGOMR-T)            
001100     03 SEQH-IDDC            PIC X(2).                                    
001200*                                 IDENTIFIERARE LAGER                     
001300*                                 WAREHOUSE IDENTIFIER                    
001400     03 SEQH-KDPRIO-PF       PIC S9              COMP-3.                  
001500*                                 PRIORITETSKOD PÅFYLLNAD                 
001600*                                 PRIORITY FILLUP PICKING ADDRESS         
001700     03 SEQH-ADLAGOMR-TOM    PIC S9(3)           COMP-3.                  
001800*                                 LAGEROMRÅDE TILL OCH MED                
001900*                                 AREA ADDRESS TO                         
002000     03 SEQH-ADGANG-TOM      PIC S9(3)           COMP-3.                  
002100*                                 GÅNG TILL OCH MED                       
002200*                                 AISLE ADDRESS TO                        
002300     03 SEQH-ADPLATS-TOM     PIC S9(5)           COMP-3.                  
002400*                                 LAGERPLATS TILL OCH MED                 
002500*                                 LOCATION ADDRESS TO                     
002600     03 SEQH-KDSTAPF         PIC X.                                       
002700*                                 STATUS PÅ PÅFYLLNING                    
002800*                                 STATUS FOR FILLING                      
002900     03 SEQH-TIORDTIME       PIC 9(12).                                   
003000*                                 ORDER DATE AND TIME                     
003100*                                 (YYMMDDHHMM[SS])                        
003200     03 SEQH-IDWDT101        PIC X(19).                                   
003300*                                 NYCKEL TILL WDT101                      
003400*                                 KEY TO WDT101                           
003500*** END OF VILMAII-COPY LENGTH= 42 BYTES                                  
