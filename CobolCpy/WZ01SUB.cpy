000100 01  SUB-WZ01SUB.                                                         
000200*                                 PARAMETERS TO THE DISPATCHER            
000300*                                 SUBPROGRAM WZ01SUB.                     
000400*                                 CALLS:                                  
000500*                                 1. - KDFUNC="GETARG"                    
000600*                                    USING SUB-CONTROL-AREA               
000700*                                          SUB-KVDLEN                     
000800*                                          SUB-DATA                       
000900*                                      ADDISPABS IS SET TO THE            
001000*                                      ADDRESS OF THE RECEIVER.           
001100*                                      KVDLEN IS SET OT THE MAX           
001200*                                      ACCEPTED LENGTH OF THE             
001300*                                      RETURNED ARGUMENT.                 
001400*                                      RC: 0 = OK                         
001500*                                          10 = INVALID ADDRESS           
001600*                                          20 = NO DATA EXISTS            
001700*                                          21 = DATA TRUNCATED            
001800*                                                                         
001900*                                 2. - KDFUNC="RETURN"                    
002000*                                    USING SUB-CONTROL-AREA               
002100*                                          SUB-KVDLEN                     
002200*                                          SUB-DATA                       
002300*                                      KVDLEN IS SET OT THE               
002400*                                      LENGTH OF THE RETURNED             
002500*                                      DATA.                              
002600*                                      RC: 0 = OK                         
002700*                                          20 = NO PREVIOUS               
002800*                                          GETARG CALL                    
002900*                                                                         
003000*                                     SUB-DATA IS NOT DEFINED             
003100*                                     IN THIS COPYTEXT. IT SHOULD         
003200*                                     BE DECLARED IN THE PROGRAM          
003300*                                     AND CAN BE OF ANY LENGTH.           
003400*                                                                         
003500     03 SUB-CONTROL-AREA.                                                 
003600        05 SUB-KDFUNC        PIC X(10).                                   
003700*                                 FUNCTION CODE                           
003800        05 SUB-KDRC          PIC S9(9)           COMP.                    
003900*                                 RETURN CODE                             
004000        05 SUB-ADDISPABS     PIC X(50).                                   
004100*                                 ABSTRACT ADDRESS                        
004200        05 SUB-KDTRANS       PIC X(8).                                    
004300*                                 TRANSACTION CODE                        
004400     03 SUB-KVDLEN           PIC S9(9)           COMP.                    
004500*                                 LENGTH OF DATA                          
004600*** END OF VILMAII-COPY LENGTH= 76 BYTES                                  
