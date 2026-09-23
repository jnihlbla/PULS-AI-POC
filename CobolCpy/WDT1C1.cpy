000100 01  SEQC-WDT1C1-CTX.                                                     
000200*                                 SEKUNDÄRT INDEX TILL WDT1               
000300*                                 PÅFYLLNING AV PLOCKSATS                 
000400*                                 ADPLATS-FOM INGÅNG                      
000500*                                 FYSISK NYCKEL = WDT1C1KY                
000600*                                 (IDDC + ADLAGOMR-F + ADGANG-F +         
000700*                                  ADPLATS-F + TIORDTIME)                 
000800*                                 SEKUNDÄR NYCKEL: WDT1CSEQ               
000900*                                 (IDDC + ADLAGOMR-F + ADGANG-F)          
001000     03 SEQC-IDDC            PIC X(2).                                    
001100*                                 IDENTIFIERARE LAGER                     
001200*                                 WAREHOUSE IDENTIFIER                    
001300     03 SEQC-ADLAGOMR-FOM    PIC S9(3)           COMP-3.                  
001400*                                 LAGEROMRÅDE FRÅN OCH MED                
001500*                                 AREA ADDRESS FROM                       
001600     03 SEQC-ADGANG-FOM      PIC S9(3)           COMP-3.                  
001700*                                 GÅNG FRÅN OCH MED                       
001800*                                 AISLE ADDRESS FROM                      
001900     03 SEQC-ADPLATS-FOM     PIC S9(5)           COMP-3.                  
002000*                                 LAGERPLATS FRÅN OCH MED                 
002100*                                 LOCATION ADDRESS FROM                   
002200     03 SEQC-TIORDTIME       PIC 9(12).                                   
002300*                                 ORDER DATE AND TIME                     
002400*                                 (YYMMDDHHMM[SS])                        
002500     03 SEQC-KDSTAPF         PIC X.                                       
002600*                                 STATUS PÅ PÅFYLLNING                    
002700*                                 STATUS FOR FILLING                      
002800     03 SEQC-IDWDT101        PIC X(19).                                   
002900*                                 NYCKEL TILL WDT101                      
003000*                                 KEY TO WDT101                           
003100*** END OF VILMAII-COPY LENGTH= 41 BYTES                                  
