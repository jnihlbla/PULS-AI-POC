000100 01  RESP-WZ01RES2.                                                       
000200*                                 THE FIRST FIELDS IN AN RESPONSE         
000300*                                 SENT AS A RESULT OF A REQUEST           
000400*                                 FROM ONE SYSTEM COMPONENT TO            
000500*                                 ANOTHER.                                
000600*                                 !!! SECOND VERSION / IDRESVER =         
000700*                                  002 !!!                                
000800     03 RESP-IDRESVER        PIC 9(3).                                    
000900*                                 VERSION NUMBER OF RESPONSE HEAD         
001000*                                 ER MESSAGE                              
001100     03 RESP-IDMSG-INFO      PIC X(3).                                    
001200*                                 INFORMATION MESSAGE ID                  
001300     03 RESP-IDMSG-ERROR     PIC X(3).                                    
001400*                                 ERROR MESSAGE ID                        
001500     03 RESP-IDELMT-ERROR    PIC X(16).                                   
001600*                                 DATA ITEM NAME                          
001700     03 RESP-KDSTATUS-API    PIC 9(3).                                    
001800*                                 HTTP STATUS CODE FOR API CALL           
001900     03 RESP-MESSAGES        OCCURS 2 TIMES.                              
002000        05 RESP-IDMSG        PIC X(10).                                   
002100*                                 MESSAGE NUMBER                          
002200*                                                                         
002300        05 RESP-MESSAGE      PIC X(100).                                  
002400     03 RESP-FILLER          PIC X(220).                                  
002500*** END OF VILMAII-COPY LENGTH= 468 BYTES                                 
