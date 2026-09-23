000100 01  PF-WDT101.                                                           
000200*                                 ARTIKLAR UTTAGNA FÖR PÅFYLLNIG          
000300*                                 AV PLOCKPLATS                           
000400*                                 FYSISK NYCKEL: WDT101KY                 
000500*                                 (IDDC + IDARTNR + TIORDTIME)            
000600     03 PF-IDDC              PIC X(2).                                    
000700*                                 IDENTIFIERARE LAGER                     
000800*                                 WAREHOUSE IDENTIFIER                    
000900     03 PF-IDARTNR           PIC S9(9)           COMP-3.                  
001000*                                 ARTIKELNUMMER                           
001100*                                 PART NUMBER                             
001200     03 PF-TIORDTIME         PIC 9(12).                                   
001300*                                 ORDER DATE AND TIME                     
001400*                                 (YYMMDDHHMM[SS])                        
001500     03 PF-KDPRIO-PF         PIC S9              COMP-3.                  
001600*                                 PRIORITETSKOD PÅFYLLNAD                 
001700*                                 PRIORITY FILLUP PICKING ADDRESS         
001800     03 PF-KDSTAPF           PIC X.                                       
001900*                                 STATUS PÅ PÅFYLLNING                    
002000*                                 STATUS FOR FILLING                      
002100     03 PF-ADART-FOM.                                                     
002200*                                 ARTIKELADR VARIFRÅN DEN FLYTTAS         
002300*                                 PARTS-ADRESS WHERE TO MOVE FROM         
002400        05 PF-ADLAGOMR-FOM   PIC S9(3)           COMP-3.                  
002500*                                 LAGEROMRÅDE FRÅN OCH MED                
002600*                                 AREA ADDRESS FROM                       
002700        05 PF-ADGANG-FOM     PIC S9(3)           COMP-3.                  
002800*                                 GÅNG FRÅN OCH MED                       
002900*                                 AISLE ADDRESS FROM                      
003000        05 PF-ADPLATS-FOM    PIC S9(5)           COMP-3.                  
003100*                                 LAGERPLATS FRÅN OCH MED                 
003200*                                 LOCATION ADDRESS FROM                   
003300     03 PF-ADART-TOM.                                                     
003400*                                 ARTIKELADRESSS VART DEN FLYTTAS         
003500*                                 PARTS-ADRESS WHERE TO MOVE IT           
003600        05 PF-ADLAGOMR-TOM   PIC S9(3)           COMP-3.                  
003700*                                 LAGEROMRÅDE TILL OCH MED                
003800*                                 AREA ADDRESS TO                         
003900        05 PF-ADGANG-TOM     PIC S9(3)           COMP-3.                  
004000*                                 GÅNG TILL OCH MED                       
004100*                                 AISLE ADDRESS TO                        
004200        05 PF-ADPLATS-TOM    PIC S9(5)           COMP-3.                  
004300*                                 LAGERPLATS TILL OCH MED                 
004400*                                 LOCATION ADDRESS TO                     
004500     03 PF-KVBEST            PIC S9(7)           COMP-3.                  
004600*                                 BESTÄLLT ANTAL                          
004700*                                 ORDERD QUANTITY                         
004800     03 PF-KVBEST-ANDR       PIC S9(7)           COMP-3.                  
004900*                                 ÄNDRAT ANTAL FRÅN BESTÄLLT              
005000*                                 CHANGED QTY FROM THE ORDERED            
005100     03 PF-TIAVSL            PIC 9(12).                                   
005200*                                 FINISHING DATE AND TIME                 
005300*                                 (YYMMDDHHMM[SS])                        
005400     03 PF-TIHOTIME          PIC 9(12).                                   
005500*                                 HAND OVER DATE AND TIME                 
005600*                                 (YYMMDDHHMM[SS])                        
005700     03 PF-IDUSER            PIC X(8).                                    
005800*                                 ANVÄNDARENS SÄKERHETS ID                
005900*                                 USER SECURITY-IDENTITY                  
006000     03 PF-FILLER            PIC X(10).                                   
006100*** END OF VILMAII-COPY LENGTH= 85 BYTES                                  
