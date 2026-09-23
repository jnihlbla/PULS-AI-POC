000100 01  CALL-CONTROL-AREA.                                                   
000200     03 CALL-KDRC            PIC S9(9)           COMP.                    
000300*                                 RETURN CODE                             
000400     03 CALL-ADDISPABS       PIC X(50).                                   
000500*                                 ABSTRACT ADDRESS                        
000600     03 CALL-IDAUTH          PIC X(20).                                   
000700*                                 AUTHENTIFICATION ID                     
000800     03 CALL-KDPASSW         PIC X(20).                                   
000900*                                 PASSWORD                                
001000     03 CALL-WAPIINFO.                                                    
001100        05 CALL-IDAPI        PIC X(255).                                  
001200*                                 NAME OF THE API                         
001300        05 CALL-IDAPI-LEN    PIC S9(9)           COMP SYNC.               
001400*                                 LENGTH OF DATA                          
001500        05 CALL-IDPATH-API   PIC X(255).                                  
001600        05 CALL-IDPATH-API-LEN                                            
001700                             PIC S9(9)           COMP.                    
001800*                                 LENGTH OF DATA                          
001900        05 CALL-IDPTYP-API   PIC X(255).                                  
002000        05 CALL-IDPTYP-API-LEN                                            
002100                             PIC S9(9)           COMP.                    
002200*                                 LENGTH OF DATA                          
002300*** END OF VILMAII-COPY LENGTH= 874 BYTES                                 
