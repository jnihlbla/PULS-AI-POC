000100 01  SEND-WZ01SEND.                                                       
000200*                                 PARAMETERS TO THE DISPATCHER            
000300*                                 SUBPROGRAM WZ01SEND.                    
000400*                                 CALLS:                                  
000500*                                 1. - KDFUNC="OPEN"                      
000600*                                    USING SEND-CONTROL-AREA              
000700*                                          SEND-OPEN-AREA                 
000800*                                      ESTABLISHES A CONNECTION           
000900*                                      WITH THE RECEIVER.                 
001000*                                      ADDISPABS IS SET TO THE            
001100*                                      ADDRESS OF THE RECEIVER            
001200*                                      AND ADDISPABS-RETURN TO            
001300*                                      THE ADDRESS WHERE ERROR            
001400*                                      OR RECEIPT MESSAGES                
001500*                                      SHOULD BE SENT.                    
001600*                                      IDAUTH AND KDPASSW MAY             
001700*                                      NORMALLY BE BLANK.                 
001800*                                      IDCOM IS RETURNED, AND             
001900*                                      USED BY THE OTHER CALLS.           
002000*                                      RC: 0 = OK                         
002100*                                          10 = INVALID ADDRESS           
002200*                                          11 = ALREADY OPEN              
002300*                                                                         
002400*                                 2. - KDFUNC="PUT"                       
002500*                                    USING SEND-CONTROL-AREA              
002600*                                          SEND-KVDLEN                    
002700*                                          SEND-DATA                      
002800*                                     SENDS A RECORD OF DATA              
002900*                                     TO THE RECEIVER                     
003000*                                     KVDLEN SHOULD BE SET THE            
003100*                                     CORRECT LENGTH BEFORE EACH          
003200*                                     PUT.                                
003300*                                     SEND-DATA IS NOT DEFINED            
003400*                                     IN THIS COPYTEXT. IT SHOULD         
003500*                                     BE DECLARED IN THE PROGRAM          
003600*                                     AND CAN BE OF ANY LENGTH.           
003700*                                     RC: 0 = OK                          
003800*                                         11 = NOT OPEN                   
003900*                                                                         
004000*                                 3. - KDFUNC="CLOSE"                     
004100*                                    USING SEND-CONTROL-AREA              
004200*                                     RC: 0 = OK                          
004300*                                         11 = NOT OPEN                   
004400*                                                                         
004500     03 SEND-CONTROL-AREA.                                                
004600*                                 USED IN ALL TYPES OF CALLS              
004700*                                                                         
004800        05 SEND-KDFUNC       PIC X(10).                                   
004900*                                 FUNCTION CODE                           
005000        05 SEND-KDRC         PIC S9(9)           COMP.                    
005100*                                 RETURN CODE                             
005200        05 SEND-IDCOM        PIC S9(9)           COMP.                    
005300*                                 ID OF THE TRANSMISSION                  
005400     03 SEND-OPEN-AREA.                                                   
005500*                                 USED IN THE OPEN CALL                   
005600*                                                                         
005700        05 SEND-ADDISPABS    PIC X(50).                                   
005800*                                 ABSTRACT ADDRESS                        
005900        05 SEND-ADDISPABS-RETURN                                          
006000                             PIC X(50).                                   
006100*                                 ABSTRACT RETURN ADDRESS                 
006200        05 SEND-ADDISPXTRA   PIC X(20).                                   
006300*                                 ADDRESS EXTENTION                       
006400        05 SEND-IDAUTH       PIC X(20).                                   
006500*                                 AUTHENTIFICATION ID                     
006600        05 SEND-KDPASSW      PIC X(20).                                   
006700*                                 PASSWORD                                
006800        05 SEND-RESTART-INFO.                                             
006900           07 SEND-FLRESTART PIC X.                                       
007000*                                 FLAG INDICATING RESTART                 
007100*                                 TRANSACTION                             
007200           07 SEND-ADDISPABS-RESTART                                      
007300                             PIC X(50).                                   
007400*                                 ABSTRACT ADDRESS                        
007500           07 SEND-TIDATETIME-DB2                                         
007600                             PIC X(26).                                   
007700*                                 DATE AND TIME DB2 FORMAT                
007800     03 SEND-KVDLEN          PIC S9(9)           COMP.                    
007900*                                 LENGTH OF DATA                          
008000*** END OF VILMAII-COPY LENGTH= 259 BYTES                                 
