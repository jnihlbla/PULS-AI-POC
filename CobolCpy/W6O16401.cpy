000100 01  MOD-W6O16401.                                                        
000200*                                 MOD COPYTEXT FOR SCREEN 6164            
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 SCREEN NUMBER                           
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS ERROR MESSAGE                       
000700     03 MOD-ADLAGOMR-FOM-IN  PIC X(2).                                    
000800*                                 AREA ADDRESS FROM                       
000900     03 MOD-ADGANG-FOM-IN    PIC X(2).                                    
001000*                                 AISLE ADDRESS FROM                      
001100     03 MOD-ADLAGOMR-TOM-IN  PIC X(2).                                    
001200*                                 AREA ADDRESS TO                         
001300     03 MOD-ADGANG-TOM-IN    PIC X(2).                                    
001400*                                 AISLE ADDRESS TO                        
001500     03 MOD-KDSTAPF-IN       PIC X.                                       
001600*                                 STATUS FOR FILLING                      
001700     03 MOD-IDARTNR-IN       PIC X(9).                                    
001800*                                 PART NUMBER                             
001900     03 MOD-IDUSER-IN        PIC X(7).                                    
002000*                                 USER SECURITY-IDENTITY                  
002100     03 MOD-KDPRIO-PF-IN     PIC X.                                       
002200*                                 PRIORITY CODE                           
002300     03 MOD-ADLAGOMR-FOM-UT  PIC X(2).                                    
002400*                                 AREA ADDRESS FROM                       
002500     03 MOD-ADGANG-FOM-UT    PIC X(2).                                    
002600*                                 AISLE ADDRESS FROM                      
002700     03 MOD-ADLAGOMR-TOM-UT  PIC X(2).                                    
002800*                                 AREA ADDRESS TO                         
002900     03 MOD-ADGANG-TOM-UT    PIC X(2).                                    
003000*                                 AISLE ADDRESS TO                        
003100     03 MOD-KDSTAPF-UT       PIC X.                                       
003200*                                 STATUS FOR FILLING                      
003300     03 MOD-IDARTNR-UT       PIC X(9).                                    
003400*                                 PART NUMBER                             
003500     03 MOD-IDUSER-UT        PIC X(7).                                    
003600*                                 USER SECURITY-IDENTITY                  
003700     03 MOD-KDPRIO-PF-UT     PIC X.                                       
003800*                                 PRIORITY CODE                           
003900     03 MOD-ADLAGOMR-FOM-N-ATTR                                           
004000                             PIC X(2).                                    
004100     03 MOD-ADLAGOMR-FOM-N   PIC 9(2).                                    
004200*                                 AREA ADDRESS FROM                       
004300     03 MOD-ADGANG-FOM-N-ATTR                                             
004400                             PIC X(2).                                    
004500     03 MOD-ADGANG-FOM-N     PIC 9(2).                                    
004600*                                 AISLE ADDRESS FROM                      
004700     03 MOD-ADPLATS-FOM-N-ATTR                                            
004800                             PIC X(2).                                    
004900     03 MOD-ADPLATS-FOM-N    PIC 9(5).                                    
005000*                                 LOCATION ADDRESS FROM                   
005100     03 MOD-KVBEST-ANDR-N-ATTR                                            
005200                             PIC X(2).                                    
005300     03 MOD-KVBEST-ANDR-N    PIC Z(5)9.                                   
005400*                                 CHANGED QTY FROM THE ORDERED            
005500     03 MOD-OUTPUT           OCCURS 13 TIMES.                             
005600*                                 OCCURS CLAUSE FOR W6O16401 COPY         
005700*                                 TEXT                                    
005800        05 MOD-KDCMDVAL-ATTR PIC X(2).                                    
005900        05 MOD-KDCMDVAL      PIC X.                                       
006000*                                 GENERAL COMMAND-CODE                    
006100        05 MOD-IDARTNR       PIC Z(9).                                    
006200*                                 PART NUMBER                             
006300        05 MOD-ADLAGOMR-FOM  PIC 9(2).                                    
006400*                                 AREA ADDRESS FROM                       
006500        05 MOD-ADGANG-FOM    PIC 9(2).                                    
006600*                                 AISLE ADDRESS FROM                      
006700        05 MOD-ADPLATS-FOM   PIC 9(5).                                    
006800*                                 LOCATION ADDRESS FROM                   
006900        05 MOD-KVBEST        PIC Z(5)9.                                   
007000*                                 ORDERD QUANTITY                         
007100        05 MOD-ADLAGOMR-TOM  PIC 9(2).                                    
007200*                                 AREA ADDRESS TO                         
007300        05 MOD-ADGANG-TOM    PIC 9(2).                                    
007400*                                 AISLE ADDRESS TO                        
007500        05 MOD-ADPLATS-TOM   PIC 9(5).                                    
007600*                                 LOCATION ADDRESS TO                     
007700        05 MOD-KVBEST-ANDR   PIC Z(5)9.                                   
007800*                                 CHANGED QTY FROM THE ORDERED            
007900        05 MOD-IDUSER        PIC X(7).                                    
008000*                                 USER SECURITY-IDENTITY                  
008100        05 MOD-KDSTAPF       PIC X.                                       
008200*                                 STATUS FOR FILLING                      
008300        05 MOD-KDPRIO-ATTR   PIC X(2).                                    
008400        05 MOD-KDPRIO        PIC X.                                       
008500*                                 PRIORITY CODE                           
008600        05 MOD-KVQPACK-3-ATTR                                             
008700                             PIC X(2).                                    
008800        05 MOD-KVQPACK-3     PIC Z(4)9.                                   
008900*                                 QUANTITY IN BULK PACK Q3                
009000        05 MOD-KVREFBER      PIC Z(6).                                    
009100*                                 CALCULATED REFILLING QUANTITY           
009200        05 MOD-TIORDTIME     PIC 9(12).                                   
009300*                                 ORDER DATE AND TIME                     
009400*                                 (YYMMDDHHMM[SS])                        
009500     03 MOD-IDPRTLST-ATTR    PIC X(2).                                    
009600     03 MOD-IDPRTLST         PIC X(8).                                    
009700*                                 LOGICAL PRINTER+LIST IDENTITY           
009800     03 MOD-TEMFSINF         PIC X(55).                                   
009900*                                 INFORMATION MESSAGE                     
010000*** END OF VILMAII-COPY LENGTH= 1198 BYTES                                
