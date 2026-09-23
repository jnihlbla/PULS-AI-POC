000100 01  DAG-WDAGAREA.                                                        
000200*                                 PARAMETRAR TILL WDAGKONV FÖR            
000300*                                 BERÄKNING AV KALENDERDAGAR.             
000400*                                 -------------------------------         
000500*                                 PARAMETERS TO WDAGKONV FOR              
000600*                                 COMPUTATION OF CALENDER DAYS.           
000700*                                 EXAMPLE OF CALL:                        
000800*                                 COMPUTE NBR OF CALENDER DAYS            
000900*                                 BETWEEN TIAAMMDD-FOM AND -TOM           
001000*                                 MOVE 001 TO DAG-KDCALL.                 
001100*                                 MOVE F-DAY TO DAG-TIAAMMDD-FOM          
001200*                                 MOVE T-DAY TO DAG-TIAAMMDD-TOM          
001300*                                 CALL WDAGKONV USING                     
001400*                                      DAG-KDCALL,                        
001500*                                      DAG-DATUM-AREA,                    
001600*                                      DAG-KDSVAR                         
001700*                                 RESULTAT IS PLACED IN                   
001800*                                 DAG-DATUM-AREA.                         
001900*                                 RETURN CODE IN DAG-KDSVAR               
002000*                                 SPACE IS OK                             
002100*                                 F     IS WRONG                          
002200*                                 -------------------------------         
002300     03 DAG-KDCALL           PIC 9(3).                                    
002400*                                 TYPE OF COMPUTATION IN WDAGKONV         
002500*                                 KDCALL = :                              
002600*                                 001= COMPUTE DAYS = TOM - FOM           
002700*                                            (1 DAY IF FOM = TOM)         
002800*                                 002= COMPUTE TOM = FOM + DAYS           
002900*                                      (1 DAY MAKES TOM = FOM)            
003000*                                 003= COMPUTE FOM = TOM - DAYS           
003100*                                      (1 DAY MAKES TOM = FOM)            
003200*                                                                         
003300*                                  THE FIELDS NEEDED FOR THE              
003400*                                  COMPUTATION MUST BE ASSIGNED           
003500*                                  BEFORE THE CALL.                       
003600     03 DAG-DATUM-AREA.                                                   
003700        05 DAG-TISEKEL-FOM   PIC 9(2).                                    
003800*                                 SEKEL I ÅRTALET                         
003900*                                 CENTURY                                 
004000        05 DAG-TIAAMMDD-FOM  PIC 9(6).                                    
004100*                                 ÅR - MÅNAD - DAG  (ÅÅMMDD)              
004200*                                 YEAR - MONTH - DAY  (YYMMDD)            
004300        05 DAG-TISEKEL-TOM   PIC 9(2).                                    
004400*                                 SEKEL I ÅRTALET                         
004500*                                 CENTURY                                 
004600        05 DAG-TIAAMMDD-TOM  PIC 9(6).                                    
004700*                                 ÅR - MÅNAD - DAG  (ÅÅMMDD)              
004800*                                 YEAR - MONTH - DAY  (YYMMDD)            
004900        05 DAG-KVKALDAG      PIC 9(5).                                    
005000*                                 ANTAL KALENDERDAGAR                     
005100*                                 NUMBER OF KALENDERDAYS                  
005200     03 DAG-KDSVAR           PIC X.                                       
005300*                                 SVAR FRÅN PROGRAM ELLER SKÄRM           
005400*                                 RETURN CODE FROM PROGRAM                
005500*** END OF VILMAII-COPY LENGTH= 25 BYTES                                  
