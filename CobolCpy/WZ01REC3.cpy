000100 01  RECV-CLOSE-AREA.                                                     
000200*                                 USED IN THE CLOSE CALL                  
000300*                                                                         
000400     03 RECV-KDRC-PULS       PIC X(10).                                   
000500*                                 PULS SPECIFIC STATUS CODES              
000600*                                                                         
000700     03 RECV-KDRC-HTTP       PIC X(10).                                   
000800*                                 HTTP STATUS CODE RETURNED FROM          
000900*                                 API CALL                                
001000     03 RECV-KDKOMSTA        PIC X.                                       
001100*                                 COMMUNICATION STATUS                    
001200     03 RECV-MESSAGE-KVDLEN  PIC S9(9)           COMP.                    
001300*                                 LENGTH OF DATA                          
001400     03 RECV-MESSAGE         PIC X(1024).                                 
001500*** END OF VILMAII-COPY LENGTH= 1049 BYTES                                
