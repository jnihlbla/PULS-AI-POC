000100 01  ARB-WDQ212.                                                          
000200*                                 ORDERHUVUDSREGISTER KÖ                  
000300*                                 ARBETSTABELL                            
000400*                                 FYSISK NYCKEL: IDDC                     
000500     03 ARB-IDDC             PIC X(2).                                    
000600*                                 IDENTIFIERARE LAGER                     
000700*                                 WAREHOUSE IDENTIFIER                    
000800     03 ARB-BEGMRK.                                                       
000900*                                 GODSMÄRKE                               
001000*                                 GOODS MARKING                           
001100        05 ARB-BEGMRK-RAD1   PIC X(30).                                   
001200*                                 GODSMÄRKE  RAD1                         
001300*                                 GOODS MARKING  LINE1                    
001400        05 ARB-BEGMRK-RAD2   PIC X(30).                                   
001500*                                 GODSMÄRKE  RAD2                         
001600*                                 GOODS MARKING  LINE2                    
001700     03 ARB-FLODELUT         PIC X.                                       
001800*                                 ORDERDEL HELT UTSKRIVEN                 
001900*                                 ORDERPART FULLY PRINTED                 
002000     03 ARB-IDRADNR-SISTA    PIC S9(5)           COMP-3.                  
002100*                                 RADNUMMER                               
002200*                                 LINE NUMBER                             
002300     03 ARB-IDTRP.                                                        
002400*                                 TRANSPORTIDENTITET                      
002500*                                 TRANSPORTIDENTITY                       
002600        05 ARB-IDTRPLOS      PIC X(3).                                    
002700*                                 TRANSPORTLÖSNING                        
002800*                                 TRANSPORTSOLUTION                       
002900        05 ARB-IDTRPVAR      PIC X(2).                                    
003000*                                 TRANSPORTLÖSNINGSGRUPP                  
003100*                                 TRANSPORTSOLUTIONGROUP                  
003200     03 ARB-IDTRP-ALT.                                                    
003300*                                 TRANSPORT-ID ALTERNATIV                 
003400*                                 TRANSPORT ID ALTERNATIVELY              
003500        05 ARB-IDTRPLOS-ALT  PIC X(3).                                    
003600*                                 TRANSPORTLÖSNING                        
003700*                                 TRANSPORTSOLUTION                       
003800        05 ARB-IDTRPVAR-ALT  PIC X(2).                                    
003900*                                 TRANSPORTLÖSNINGSGRUPP                  
004000*                                 TRANSPORTSOLUTIONGROUP                  
004100     03 ARB-IDPLKLST-SISTA   PIC S9(3)           COMP-3.                  
004200*                                 PLOCKLISTNUMMER                         
004300*                                 PICKING LIST NUMBER                     
004400     03 ARB-KDFDKRAV         PIC S9(3)           COMP-3.                  
004500*                                 TRANSPORTFÖRPACKNINGSKOD                
004600*                                 PACKING CODE                            
004700     03 ARB-KDFRAKT          PIC S9(3)           COMP-3.                  
004800*                                 FRAKTSÄTT DC TILL KUND                  
004900*                                 FREIGHT CODE                            
005000     03 ARB-KDROPACK         PIC X.                                       
005100*                                 FRISLÄPPNINGSKOD RO/DO                  
005200*                                 CONSOLIDATION BO/DO                     
005300     03 ARB-KDTRPKAT         PIC X.                                       
005400*                                 TRANSPORTKATEGORI                       
005500*                                 TRANSPORT CATEGORY                      
005600     03 ARB-KVSEMBRA         PIC S9(3)           COMP-3.                  
005700*                                 ANTAL SPECIALEMBALLAGERADER             
005800*                                 NUMBER OF SPECIAL PACKING LINES         
005900     03 ARB-TIRFS            PIC S9(11)          COMP-3.                  
006000*                                 KLART FÖR TRANSPORT ÅÅMMDDTTMM          
006100*                                 READY FOR SHIPMENT  YYMMDDHHMM          
006200     03 ARB-DATRPAVT.                                                     
006300*                                 TRANSPORTAVGÅNGSTIDPUNKT                
006400*                                 TRANSPORT DEPARTURE                     
006500*                                 YYYYMMDD+HHMM                           
006600        05 ARB-DATRPAVD      PIC 9(8).                                    
006700*                                 TRANSPORTAVGÅNGSDATUM                   
006800*                                 TRANSPORT DEPARTURE DATE                
006900        05 ARB-TIHHMM        PIC S9(5)           COMP-3.                  
007000*                                 KLOCKSLAG (TIMMAR OCH MINUTER)          
007100*                                 TIME IN HOUR AND MINUTE                 
007200     03 ARB-KDORDSTA         PIC X(2).                                    
007300*                                 VOLVOORDERSTATUS                        
007400*                                 VOLVO ORDER STATUS                      
007500     03 ARB-KDORDSTA-O       PIC X(2).                                    
007600*                                 VOLVOORDERSTATUS FÖREGÅENDE             
007700*                                 VOLVO ORDER STATUS OLD                  
007800*** END OF VILMAII-COPY LENGTH= 107 BYTES                                 
