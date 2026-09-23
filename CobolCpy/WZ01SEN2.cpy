000100 01  SEND-OPEN-AREA.                                                      
000200*                                 USED IN THE OPEN CALL                   
000300*                                                                         
000400     03 SEND-ADDISPABS       PIC X(50).                                   
000500*                                 ABSTRACT ADDRESS                        
000600     03 SEND-ADDISPABS-RETURN                                             
000700                             PIC X(50).                                   
000800*                                 ABSTRACT RETURN ADDRESS                 
000900     03 SEND-ADDISPXTRA      PIC X(20).                                   
001000*                                 ADDRESS EXTENTION                       
001100     03 SEND-IDAUTH          PIC X(20).                                   
001200*                                 AUTHENTIFICATION ID                     
001300     03 SEND-KDPASSW         PIC X(20).                                   
001400*                                 PASSWORD                                
001500     03 SEND-RESTART-INFO.                                                
001600        05 SEND-FLRESTART    PIC X.                                       
001700*                                 FLAG INDICATING RESTART                 
001800*                                 TRANSACTION                             
001900        05 SEND-ADDISPABS-RESTART                                         
002000                             PIC X(50).                                   
002100*                                 ABSTRACT ADDRESS                        
002200        05 SEND-TIDATETIME-DB2                                            
002300                             PIC X(26).                                   
002400*                                 DATE AND TIME DB2 FORMAT                
002500*** END OF VILMAII-COPY LENGTH= 237 BYTES                                 
