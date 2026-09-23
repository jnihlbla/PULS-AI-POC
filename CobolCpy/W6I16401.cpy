000100 01  MID-W6I16401.                                                        
000200*                                 COPYTEXT FOR MID W6I16401               
000300     03 MID-ADLAGOMR-FOM-IN  PIC X(2).                                    
000400*                                 AREA ADDRESS FROM                       
000500     03 MID-ADGANG-FOM-IN    PIC X(2).                                    
000600*                                 AISLE ADDRESS FROM                      
000700     03 MID-ADLAGOMR-TOM-IN  PIC X(2).                                    
000800*                                 AREA ADDRESS TO                         
000900     03 MID-ADGANG-TOM-IN    PIC X(2).                                    
001000*                                 AISLE ADDRESS TO                        
001100     03 MID-KDSTAPF-IN       PIC X.                                       
001200*                                 STATUS FOR FILLING                      
001300     03 MID-IDARTNR-IN       PIC X(9).                                    
001400*                                 PART NUMBER                             
001500     03 MID-IDUSER-IN        PIC X(7).                                    
001600*                                 USER SECURITY-IDENTITY                  
001700     03 MID-KDPRIO-PF-IN     PIC X.                                       
001800*                                 PRIORITY CODE                           
001900     03 MID-ADLAGOMR-FOM-UT  PIC X(2).                                    
002000*                                 AREA ADDRESS FROM                       
002100     03 MID-ADGANG-FOM-UT    PIC X(2).                                    
002200*                                 AISLE ADDRESS FROM                      
002300     03 MID-ADLAGOMR-TOM-UT  PIC X(2).                                    
002400*                                 AREA ADDRESS TO                         
002500     03 MID-ADGANG-TOM-UT    PIC X(2).                                    
002600*                                 AISLE ADDRESS TO                        
002700     03 MID-KDSTAPF-UT       PIC X.                                       
002800*                                 STATUS FOR FILLING                      
002900     03 MID-IDARTNR-UT       PIC X(9).                                    
003000*                                 PART NUMBER                             
003100     03 MID-IDUSER-UT        PIC X(7).                                    
003200*                                 USER SECURITY-IDENTITY                  
003300     03 MID-KDPRIO-PF-UT     PIC X.                                       
003400*                                 PRIORITY CODE                           
003500     03 MID-ADLAGOMR-FOM-NEW PIC 9(2).                                    
003600*                                 AREA ADDRESS FROM                       
003700     03 MID-ADGANG-FOM-NEW   PIC 9(2).                                    
003800*                                 AISLE ADDRESS FROM                      
003900     03 MID-ADPLATS-FOM-NEW  PIC 9(5).                                    
004000*                                 LOCATION ADDRESS FROM                   
004100     03 MID-KVBEST-ANDR-NEW  PIC 9(6).                                    
004200*                                 CHANGED QTY FROM THE ORDERED            
004300     03 MID-INPUT            OCCURS 13 TIMES.                             
004400*                                 OCCURS CLAUSE FOR W6I16401 COPY         
004500*                                 TEXT                                    
004600        05 MID-KDCMDVAL      PIC X.                                       
004700*                                 GENERAL COMMAND-CODE                    
004800        05 MID-IDARTNR       PIC 9(9).                                    
004900*                                 PART NUMBER                             
005000        05 MID-ADLAGOMR-FOM  PIC 9(2).                                    
005100*                                 AREA ADDRESS FROM                       
005200        05 MID-ADGANG-FOM    PIC 9(2).                                    
005300*                                 AISLE ADDRESS FROM                      
005400        05 MID-ADPLATS-FOM   PIC 9(5).                                    
005500*                                 LOCATION ADDRESS FROM                   
005600        05 MID-KVBEST        PIC 9(6).                                    
005700*                                 ORDERD QUANTITY                         
005800        05 MID-ADLAGOMR-TOM  PIC 9(2).                                    
005900*                                 AREA ADDRESS TO                         
006000        05 MID-ADGANG-TOM    PIC 9(2).                                    
006100*                                 AISLE ADDRESS TO                        
006200        05 MID-ADPLATS-TOM   PIC 9(5).                                    
006300*                                 LOCATION ADDRESS TO                     
006400        05 MID-KVBEST-ANDR   PIC 9(6).                                    
006500*                                 CHANGED QTY FROM THE ORDERED            
006600        05 MID-IDUSER        PIC X(7).                                    
006700*                                 USER SECURITY-IDENTITY                  
006800        05 MID-KDSTAPF       PIC X.                                       
006900*                                 STATUS FOR FILLING                      
007000        05 MID-KDPRIO        PIC X.                                       
007100*                                 PRIORITY CODE                           
007200        05 MID-KVQPACK-3     PIC 9(5).                                    
007300*                                 QUANTITY IN BULK PACK Q3                
007400        05 MID-TIORDTIME     PIC 9(12).                                   
007500*                                 ORDER DATE AND TIME                     
007600*                                 (YYMMDDHHMM[SS])                        
007700     03 MID-IDPRTLST         PIC X(8).                                    
007800*                                 LOGICAL PRINTER+LIST IDENTITY           
007900*** END OF VILMAII-COPY LENGTH= 933 BYTES                                 
