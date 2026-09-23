000100 01  SEQG-WDT1G1-CTX.                                                     
000200*                                 SEKUNDÄRT INDEX TILL WDT1               
000300*                                 PÅFYLLNING AV PLOCKSATS                 
000400*                                 KDPRIO-PF FOM PLATS INGÅNG              
000500*                                 FYSISK NYCKEL = WDT1G1KY                
000600*                                 (IDDC + KDPRIO + ADLAGOMR-F +           
000700*                                  ADGANG-F + ADPLATS-F +                 
000800*                                  KDSTAPF + TIORDTIME)                   
000900*                                 SEKUNDÄR NYCKEL: WDT1GSEQ               
001000*                                 (IDDC + KDPRIO + ADLAGOMR-F)            
001100     03 SEQG-IDDC            PIC X(2).                                    
001200*                                 IDENTIFIERARE LAGER                     
001300*                                 WAREHOUSE IDENTIFIER                    
001400     03 SEQG-KDPRIO-PF       PIC S9              COMP-3.                  
001500*                                 PRIORITETSKOD PÅFYLLNAD                 
001600*                                 PRIORITY FILLUP PICKING ADDRESS         
001700     03 SEQG-ADLAGOMR-FOM    PIC S9(3)           COMP-3.                  
001800*                                 LAGEROMRÅDE FRÅN OCH MED                
001900*                                 AREA ADDRESS FROM                       
002000     03 SEQG-ADGANG-FOM      PIC S9(3)           COMP-3.                  
002100*                                 GÅNG FRÅN OCH MED                       
002200*                                 AISLE ADDRESS FROM                      
002300     03 SEQG-ADPLATS-FOM     PIC S9(5)           COMP-3.                  
002400*                                 LAGERPLATS FRÅN OCH MED                 
002500*                                 LOCATION ADDRESS FROM                   
002600     03 SEQG-KDSTAPF         PIC X.                                       
002700*                                 STATUS PÅ PÅFYLLNING                    
002800*                                 STATUS FOR FILLING                      
002900     03 SEQG-TIORDTIME       PIC 9(12).                                   
003000*                                 ORDER DATE AND TIME                     
003100*                                 (YYMMDDHHMM[SS])                        
003200     03 SEQG-IDWDT101        PIC X(19).                                   
003300*                                 NYCKEL TILL WDT101                      
003400*                                 KEY TO WDT101                           
003500*** END OF VILMAII-COPY LENGTH= 42 BYTES                                  
