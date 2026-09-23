000100 01  WORK-WORKAREA.                                                       
000200*                                 PARAMETERS TO WORKDAY FOR               
000300*                                 COMPUTATION OF WORKING DAYS.            
000400*                                 EXAMPLE OF CALL:                        
000500*                                 COMPUTE NO. OF WORKING DAYS             
000600*                                 BETWEEN TIAAMMDD-FOM AND -TOM           
000700*                                 MOVE 001  TO WORK-KDCALL                
000800*                                 MOVE 21   TO WORK-IDDC                  
000900*                                 MOVE DAY1 TO WORK-TIAAMMDD-FOM          
001000*                                 MOVE DAY2 TO WORK-TIAAMMDD-TOM          
001100*                                 CALL WORKDAY USING                      
001200*                                      WORK-KDCALL                        
001300*                                      WORK-DATE-AREA                     
001400*                                      WORK-KDSVAR                        
001500*                                 RESULT IS PLACED IN                     
001600*                                 WORK-DATE-AREA                          
001700*                                 RETURN CODE IN WORK-KDSVAR              
001800*                                 -------------------------------         
001900     03 WORK-KDCALL          PIC 9(3).                                    
002000*                                 TYPE OF COMPUTATION IN WORKDAY          
002100*                                 KDCALL:                                 
002200*                                  1=  COMPUTE WORKD  =                   
002300*                                                TOM MINUS FOM            
002400*                                      (1 DAY IF FOM = TOM)               
002500*                                  2=  COMPUTE TOM =                      
002600*                                                FOM PLUS WORKD           
002700*                                     (1 WORKDAY MAKES TOM = FOM)         
002800*                                  3=  COMPUTE FOM =                      
002900*                                                TOM MINUS WORKD          
003000*                                     (1 WORKDAY MAKES TOM = FOM)         
003100*                                                                         
003200*                                  THE FIELDS NEEDED FOR THE              
003300*                                  COMPUTATION MUST BE ASSIGNED           
003400*                                  BEFORE THE CALL.                       
003500*                                                                         
003600     03 WORK-DATE-AREA.                                                   
003700        05 WORK-IDDC         PIC X(2).                                    
003800*                                 IDENTIFIERARE LAGER                     
003900*                                 WAREHOUSE IDENTIFIER                    
004000        05 WORK-TIAAMMDD-FOM PIC 9(6).                                    
004100*                                 ≈R - M≈NAD - DAG  (≈≈MMDD)              
004200*                                 YEAR - MONTH - DAY  (YYMMDD)            
004300        05 WORK-TIAAMMDD-TOM PIC 9(6).                                    
004400*                                 ≈R - M≈NAD - DAG  (≈≈MMDD)              
004500*                                 YEAR - MONTH - DAY  (YYMMDD)            
004600        05 WORK-KVWORKD      PIC 9(3).                                    
004700*                                 ANTAL ARBETSDAGAR                       
004800*                                 NUMBER OF WORKING DAYS                  
004900        05 WORK-TIAAMMDD-NEXT-WORKDAY                                     
005000                             PIC 9(6).                                    
005100*                                 ≈R - M≈NAD - DAG  (≈≈MMDD)              
005200*                                 YEAR - MONTH - DAY  (YYMMDD)            
005300        05 WORK-TIAAMMDD-NEXT-WEEK                                        
005400                             PIC 9(6).                                    
005500*                                 ≈R - M≈NAD - DAG  (≈≈MMDD)              
005600*                                 YEAR - MONTH - DAY  (YYMMDD)            
005700     03 WORK-KDSVAR          PIC X.                                       
005800      88 WORK-KDSVAR-OK      VALUE ' '.                                   
005900      88 WORK-KDSVAR-FEL     VALUE 'F'.                                   
006000*                                                       KDSVAR-88         
006100*                                 SVARSKOD FR≈N SUBPROGRAM                
006200*                                                       KDSVAR-88         
006300*                                 RETURN CODE FROM SUBPROGRAM             
006400*** END OF VILMAII-COPY LENGTH= 33 BYTES                                  
