000100 01  CALL-WZ01CALL.                                                       
000200*                                 PARAMETERS TO THE DISPATCHER            
000300*                                 SUBPROGRAM WZ01CALL.                    
000400*                                 CALL FORMAT:                            
000500*                                    USING CALL-CONTROL-AREA              
000600*                                          CALL-KVDLEN-IN                 
000700*                                          CALL-DATA-IN                   
000800*                                          CALL-KVDLEN-OUT                
000900*                                          CALL-DATA-OUT                  
001000*                                     SENDS A RECORD OF DATA              
001100*                                     TO THE RECEIVER AND ACCEPTS         
001200*                                     ANOTHER RECORD OF DATA IS           
001300*                                     RETURNED AS A RESULT.               
001400*                                                                         
001500*                                     KVDLEN-IN SHOULD BE SET TO          
001600*                                     THE CORRECT LENGTH BEFORE           
001700*                                     EACH CALL, AND KVDLEN-OUT           
001800*                                     SHOULD BE SET TO THE MAX            
001900*                                     LENGTH ACCEPTED FOR THE             
002000*                                     RETURNED RESULT.                    
002100*                                     DATA-IN AND DATA-OUT ARE            
002200*                                     NOT DEFINED IN THIS COPY-           
002300*                                     TEXT. THEY SHOULD BE                
002400*                                     DECLARED IN THE PROGRAM             
002500*                                     AND CAN BE OF ANY LENGTH.           
002600*                                     RC: 0 = OK                          
002700*                                         10 = INVALID ADDRESS.           
002800*                                         12 = CALLED FUNCTION            
002900*                                         COULD NOT BE ACTIVATED.         
003000*                                                                         
003100*                                 NOTE: AFTER THE LAST USE OF             
003200*                                  THIS SUBROUTINE, AND EXTRA             
003300*                                  "DUMMY" CALL WITH ADDISPABS            
003400*                                  = SPACE SHOULD BE PERFORMED            
003500*                                  TO CLEAN UP ANY REMAINING              
003600*                                  CONNECTIONS.                           
003700     03 CALL-CONTROL-AREA.                                                
003800        05 CALL-KDRC         PIC S9(9)           COMP.                    
003900*                                 RETURN CODE                             
004000        05 CALL-ADDISPABS    PIC X(50).                                   
004100*                                 ABSTRACT ADDRESS                        
004200        05 CALL-IDAUTH       PIC X(20).                                   
004300*                                 AUTHENTIFICATION ID                     
004400        05 CALL-KDPASSW      PIC X(20).                                   
004500*                                 PASSWORD                                
004600        05 CALL-WAPIINFO.                                                 
004700           07 CALL-IDAPI     PIC X(255).                                  
004800*                                 NAME OF THE API                         
004900           07 CALL-IDAPI-LEN PIC S9(9)           COMP SYNC.               
005000*                                 LENGTH OF DATA                          
005100           07 CALL-IDPATH-API                                             
005200                             PIC X(255).                                  
005300           07 CALL-IDPATH-API-LEN                                         
005400                             PIC S9(9)           COMP.                    
005500*                                 LENGTH OF DATA                          
005600           07 CALL-IDPTYP-API                                             
005700                             PIC X(255).                                  
005800           07 CALL-IDPTYP-API-LEN                                         
005900                             PIC S9(9)           COMP.                    
006000*                                 LENGTH OF DATA                          
006100     03 CALL-KVDLEN-IN       PIC S9(9)           COMP.                    
006200*                                 LENGTH OF DATA                          
006300     03 CALL-KVDLEN-OUT      PIC S9(9)           COMP.                    
006400*                                 LENGTH OF DATA                          
006500*** END OF VILMAII-COPY LENGTH= 882 BYTES                                 
