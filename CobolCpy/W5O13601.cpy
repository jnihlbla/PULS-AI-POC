000100 01  MOD-W5O13601.                                                        
000200     03 MOD-IDTRANS          PIC X(4).                                    
000300*                                 SCREEN NUMBER                           
000400     03 MOD-TEMFSFEL         PIC X(40).                                   
000500*                                 MFS ERROR MESSAGE                       
000600     03 MOD-KDPRODSL-IN      PIC X(2).                                    
000700*                                 PRODUCT GROUP                           
000800     03 MOD-KDPRODSL-UT      PIC X(2).                                    
000900*                                 PRODUCT GROUP                           
001000     03 MOD-IDFTG            PIC 9(2).                                    
001100*                                 COMPANY IDENTITY ACCOUNTING             
001200     03 MOD-LINES            OCCURS 11 TIMES.                             
001300        05 MOD-KDPRODSL      PIC X(2).                                    
001400*                                 PRODUCT GROUP                           
001500        05 MOD-RELANDCO-PG-TO                                             
001600                             PIC Z(2)9.9(3).                              
001700*                                 LANDING COST/PG UNTIL TITULF            
001800        05 MOD-TILANDCO      PIC 9(6).                                    
001900*                                 START DATE LANDING COST FACTOR          
002000        05 MOD-RELANDCO-PG-FROM                                           
002100                             PIC Z(2)9.9(3).                              
002200*                                 LANDING COST/PG AFTER TITULF            
002300        05 MOD-TIUPPDAT      PIC 9(6).                                    
002400*                                 UPDATING DATE     (YYMMDD)              
002500        05 MOD-IDUSER        PIC X(8).                                    
002600*                                 USER SECURITY-IDENTITY                  
002700     03 MOD-KDCMD-UPD-ATTR   PIC X(2).                                    
002800     03 MOD-KDCMD-UPD        PIC X.                                       
002900*                                 LINE UPDATE COMMAND                     
003000     03 MOD-KDPRODSL-UPD-ATTR                                             
003100                             PIC X(2).                                    
003200     03 MOD-KDPRODSL-UPD     PIC X(2).                                    
003300*                                 PRODUCT GROUP                           
003400     03 MOD-RELANDCO-PG-FROM-UPD-ATTR                                     
003500                             PIC X(2).                                    
003600     03 MOD-RELANDCO-PG-FROM-UPD                                          
003700                             PIC Z(2)9.9(3).                              
003800*                                 LANDING COST/PG AFTER TITULF            
003900     03 MOD-TILANDCO-UPD-ATTR                                             
004000                             PIC X(2).                                    
004100     03 MOD-TILANDCO-UPD     PIC 9(6).                                    
004200*                                 START DATE LANDING COST FACTOR          
004300     03 MOD-TEMFSINF         PIC X(55).                                   
004400*                                 INFORMATION MESSAGE                     
004500*** END OF VILMAII-COPY LENGTH= 525 BYTES                                 
