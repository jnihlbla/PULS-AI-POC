000100 01  MOD-W5O13701.                                                        
000200     03 MOD-IDTRANS          PIC X(4).                                    
000300*                                 SCREEN NUMBER                           
000400     03 MOD-TEMFSFEL         PIC X(40).                                   
000500*                                 MFS ERROR MESSAGE                       
000600     03 MOD-IDFKNGRP-IN      PIC X(4).                                    
000700*                                 FUNCTION GROUP                          
000800     03 MOD-IDFKNGRP-UT      PIC X(4).                                    
000900*                                 FUNCTION GROUP                          
001000     03 MOD-IDFTG            PIC 9(2).                                    
001100*                                 COMPANY IDENTITY ACCOUNTING             
001200     03 MOD-LINES            OCCURS 12 TIMES.                             
001300        05 MOD-IDFKNGRP      PIC X(4).                                    
001400*                                 FUNCTION GROUP                          
001500        05 MOD-RELANDCO-EOCF-FROM                                         
001600                             PIC Z(2)9.9(3).                              
001700*                                 LANDING COST/FG EO AFTER TITULF         
001800        05 MOD-RELANDCO-EITX-TO                                           
001900                             PIC Z(2)9.9(3).                              
002000*                                 LANDING COST/FG EI UNTIL TITULF         
002100        05 MOD-RELANDCO-EITX-FROM                                         
002200                             PIC Z(2)9.9(3).                              
002300*                                 LANDING COST/FG EI AFTER TITULF         
002400        05 MOD-RELANDCO-EGTX-TO                                           
002500                             PIC Z(2)9.9(3).                              
002600*                                 LANDING COST/FG EG UNTIL TITULF         
002700        05 MOD-RELANDCO-EGTX-FROM                                         
002800                             PIC Z(2)9.9(3).                              
002900*                                 LANDING COST/FG EG AFTER TITULF         
003000        05 MOD-RELANDCO-FG-FROM                                           
003100                             PIC Z(2)9.9(3).                              
003200*                                 LANDING COST/FG AFTER TITULF            
003300        05 MOD-TIUPPDAT      PIC 9(6).                                    
003400*                                 UPDATING DATE     (YYMMDD)              
003500        05 MOD-TILANDCO      PIC 9(6).                                    
003600*                                 START DATE LANDING COST FACTOR          
003700        05 MOD-IDUSER        PIC X(8).                                    
003800*                                 USER SECURITY-IDENTITY                  
003900     03 MOD-KDCMD-UPD-ATTR   PIC X(2).                                    
004000     03 MOD-KDCMD-UPD        PIC X.                                       
004100*                                 LINE UPDATE COMMAND                     
004200     03 MOD-IDFKNGRP-UPD-ATTR                                             
004300                             PIC X(2).                                    
004400     03 MOD-IDFKNGRP-UPD     PIC X(4).                                    
004500*                                 FUNCTION GROUP                          
004600     03 MOD-RELANDCO-EOF-FROM-UPD-ATTR                                    
004700                             PIC X(2).                                    
004800     03 MOD-RELANDCO-EOCF-FROM-UPD                                        
004900                             PIC Z(2)9.9(3).                              
005000*                                 LANDING COST/FG EO AFTER TITULF         
005100     03 MOD-RELANDCO-EIX-FROM-UPD-ATTR                                    
005200                             PIC X(2).                                    
005300     03 MOD-RELANDCO-EITX-FROM-UPD                                        
005400                             PIC Z(2)9.9(3).                              
005500*                                 LANDING COST/FG EI AFTER TITULF         
005600     03 MOD-RELANDCO-EGX-FROM-UPD-ATTR                                    
005700                             PIC X(2).                                    
005800     03 MOD-RELANDCO-EGTX-FROM-UPD                                        
005900                             PIC Z(2)9.9(3).                              
006000*                                 LANDING COST/FG EG AFTER TITULF         
006100     03 MOD-TILANDCO-UPD-ATTR                                             
006200                             PIC X(2).                                    
006300     03 MOD-TILANDCO-UPD     PIC 9(6).                                    
006400*                                 START DATE LANDING COST FACTOR          
006500     03 MOD-TEMFSINF         PIC X(55).                                   
006600*                                 INFORMATION MESSAGE                     
006700*** END OF VILMAII-COPY LENGTH= 945 BYTES                                 
