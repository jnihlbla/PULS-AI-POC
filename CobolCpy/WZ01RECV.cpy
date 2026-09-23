000100 01  RECV-WZ01RECV.                                                       
000200*                                 PARAMETERS TO THE DISPATCHER            
000300*                                 SUBPROGRAM WZ01RECV.                    
000400*                                 CALLS:                                  
000500*                                 1. - KDFUNC="OPEN"                      
000600*                                    USING RECV-CONTROL-AREA              
000700*                                          RECV-OPEN-AREA                 
000800*                                      ESTABLISHES A CONNECTION           
000900*                                      WITH THE SENDER.                   
001000*                                      ADDISPABS IS SET TO THE            
001100*                                      ADDRESS OF THE RECEIVER.           
001200*                                      (=THIS PROGRAM)                    
001300*                                                                         
001400*                                      IDCOM IS RETURNED, AND             
001500*                                      USED BY THE OTHER CALLS.           
001600*                                      RC: 0 = OK                         
001700*                                          10 = INVALID ADDRESS           
001800*                                          11 = ALREADY OPEN              
001900*                                          20 = NO DATA EXISTS            
002000*                                      ADDISPABS-RETURN IS ALSO           
002100*                                      RETURNED AND CONTAINS THE          
002200*                                      ADDRESS WHERE ERROR OR             
002300*                                      RECEIPT MESSAGES SHOULD            
002400*                                      BE SENT.                           
002500*                                                                         
002600*                                 2. - KDFUNC="GET"                       
002700*                                    USING RECV-CONTROL-AREA              
002800*                                          RECV-KVDLEN                    
002900*                                          RECV-DATA                      
003000*                                     RECEIVES A RECORD OF DATA           
003100*                                     FROM THE SENDER.                    
003200*                                     KVDLEN SHOULD BE SET TO             
003300*                                     MAX ACCEPTED LENGTH BEFORE          
003400*                                     EACH GET.                           
003500*                                     RECV-DATA IS NOT DEFINED            
003600*                                     IN THIS COPYTEXT. IT SHOULD         
003700*                                     BE DECLARED IN THE PROGRAM          
003800*                                     AND CAN BE OF ANY LENGTH.           
003900*                                     RC: 0 = OK                          
004000*                                         1  = END-OF-DATA                
004100*                                         11 = NOT OPEN                   
004200*                                         21 = DATA TRUNCATED             
004300*                                                                         
004400*                                 3. - KDFUNC="CLOSE"                     
004500*                                    USING RECV-CONTROL-AREA              
004600*                                     RC: 0 = OK                          
004700*                                         11 = NOT OPEN                   
004800*                                                                         
004900     03 RECV-CONTROL-AREA.                                                
005000*                                 USED IN ALL TYPES OF CALLS              
005100*                                                                         
005200        05 RECV-KDFUNC       PIC X(10).                                   
005300*                                 FUNCTION CODE                           
005400        05 RECV-KDRC         PIC S9(9)           COMP.                    
005500*                                 RETURN CODE                             
005600        05 RECV-IDCOM        PIC S9(9)           COMP.                    
005700*                                 ID OF THE TRANSMISSION                  
005800        05 RECV-KDTRANS      PIC X(8).                                    
005900*                                 TRANSACTION CODE                        
006000     03 RECV-OPEN-AREA.                                                   
006100*                                 USED IN THE OPEN CALL                   
006200*                                                                         
006300        05 RECV-ADDISPABS    PIC X(50).                                   
006400*                                 ABSTRACT ADDRESS                        
006500        05 RECV-ADDISPABS-RETURN                                          
006600                             PIC X(50).                                   
006700*                                 ABSTRACT RETURN ADDRESS                 
006800        05 RECV-ADDISPXTRA   PIC X(20).                                   
006900*                                 ADDRESS EXTENTION                       
007000     03 RECV-CLOSE-AREA.                                                  
007100*                                 USED IN THE CLOSE CALL                  
007200*                                                                         
007300        05 RECV-KDRC-PULS    PIC X(10).                                   
007400*                                 PULS SPECIFIC STATUS CODES              
007500*                                                                         
007600        05 RECV-KDRC-HTTP    PIC X(10).                                   
007700*                                 HTTP STATUS CODE RETURNED FROM          
007800*                                 API CALL                                
007900        05 RECV-KDKOMSTA     PIC X.                                       
008000*                                 COMMUNICATION STATUS                    
008100        05 RECV-MESSAGE-KVDLEN                                            
008200                             PIC S9(9)           COMP.                    
008300*                                 LENGTH OF DATA                          
008400        05 RECV-MESSAGE      PIC X(1024).                                 
008500     03 RECV-KVDLEN          PIC S9(9)           COMP.                    
008600*                                 LENGTH OF DATA                          
008700*** END OF VILMAII-COPY LENGTH= 1199 BYTES                                
