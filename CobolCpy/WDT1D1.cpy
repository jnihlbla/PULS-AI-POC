000100 01  SEQD-WDT1D1-CTX.                                                     
000200*                                 SEKUNDÄRT INDEX TILL WDT1               
000300*                                 PÅFYLLNING AV PLOCKSATS                 
000400*                                 ADPLATS-TOM INGÅNG                      
000500*                                 FYSISK NYCKEL = WDT1D1KY                
000600*                                 (IDDC + ADLAGOMR-T + ADGANG-T +         
000700*                                  ADPLATS-T + TIORDTIME)                 
000800*                                 SEKUNDÄR NYCKEL: WDT1DSEQ               
000900*                                 (IDDC + ADLAGOMR-T + ADGANG-T)          
001000     03 SEQD-IDDC            PIC X(2).                                    
001100*                                 IDENTIFIERARE LAGER                     
001200*                                 WAREHOUSE IDENTIFIER                    
001300     03 SEQD-ADLAGOMR-TOM    PIC S9(3)           COMP-3.                  
001400*                                 LAGEROMRÅDE TILL OCH MED                
001500*                                 AREA ADDRESS TO                         
001600     03 SEQD-ADGANG-TOM      PIC S9(3)           COMP-3.                  
001700*                                 GÅNG TILL OCH MED                       
001800*                                 AISLE ADDRESS TO                        
001900     03 SEQD-ADPLATS-TOM     PIC S9(5)           COMP-3.                  
002000*                                 LAGERPLATS TILL OCH MED                 
002100*                                 LOCATION ADDRESS TO                     
002200     03 SEQD-TIORDTIME       PIC 9(12).                                   
002300*                                 ORDER DATE AND TIME                     
002400*                                 (YYMMDDHHMM[SS])                        
002500     03 SEQD-KDSTAPF         PIC X.                                       
002600*                                 STATUS PÅ PÅFYLLNING                    
002700*                                 STATUS FOR FILLING                      
002800     03 SEQD-IDWDT101        PIC X(19).                                   
002900*                                 NYCKEL TILL WDT101                      
003000*                                 KEY TO WDT101                           
003100*** END OF VILMAII-COPY LENGTH= 41 BYTES                                  
