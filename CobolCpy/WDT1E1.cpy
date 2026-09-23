000100 01  SEQE-WDT1E1-CTX.                                                     
000200*                                 SEKUNDÄRT INDEX TILL WDT1               
000300*                                 PÅFYLLNING AV PLOCKSATS                 
000400*                                 IDUSER FOM PLATS INGÅNG                 
000500*                                 FYSISK NYCKEL = WDT1E1KY                
000600*                                 (IDDC + IDUSER + ADLAGOMR-F +           
000700*                                  ADGANG-F + ADPLATS-F +                 
000800*                                  KDSTAPF + TIORDTIME)                   
000900*                                 SEKUNDÄR NYCKEL: WDT1ESEQ               
001000*                                 (IDDC + IDUSER + ADLAGOMR-F)            
001100     03 SEQE-IDDC            PIC X(2).                                    
001200*                                 IDENTIFIERARE LAGER                     
001300*                                 WAREHOUSE IDENTIFIER                    
001400     03 SEQE-IDUSER          PIC X(8).                                    
001500*                                 ANVÄNDARENS SÄKERHETS ID                
001600*                                 USER SECURITY-IDENTITY                  
001700     03 SEQE-ADLAGOMR-FOM    PIC S9(3)           COMP-3.                  
001800*                                 LAGEROMRÅDE FRÅN OCH MED                
001900*                                 AREA ADDRESS FROM                       
002000     03 SEQE-ADGANG-FOM      PIC S9(3)           COMP-3.                  
002100*                                 GÅNG FRÅN OCH MED                       
002200*                                 AISLE ADDRESS FROM                      
002300     03 SEQE-ADPLATS-FOM     PIC S9(5)           COMP-3.                  
002400*                                 LAGERPLATS FRÅN OCH MED                 
002500*                                 LOCATION ADDRESS FROM                   
002600     03 SEQE-KDSTAPF         PIC X.                                       
002700*                                 STATUS PÅ PÅFYLLNING                    
002800*                                 STATUS FOR FILLING                      
002900     03 SEQE-TIORDTIME       PIC 9(12).                                   
003000*                                 ORDER DATE AND TIME                     
003100*                                 (YYMMDDHHMM[SS])                        
003200     03 SEQE-IDWDT101        PIC X(19).                                   
003300*                                 NYCKEL TILL WDT101                      
003400*                                 KEY TO WDT101                           
003500*** END OF VILMAII-COPY LENGTH= 49 BYTES                                  
