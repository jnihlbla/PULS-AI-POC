000100 01  TIME-WZ20TIME.                                                       
000200*                                                                         
000300*                                 CONVERT DATE + TIME TO NBR OF           
000400*                                 SECONDS SINCE OCT 14, 1582              
000500*                                                                         
000600*                                 CALL FORMAT:                            
000700*                                    USING TIME-WZ20TIME                  
000800*                                 WHERE WZ20TIME CONTAINS THE             
000900*                                 FOLLOWING FIELDS:                       
001000*                                    TIME-TIDATETIME                      
001100*                                    TIME-TISECONDS                       
001200*                                    TIME-KDRC                            
001300*                                                                         
001400*                                 TIDATETIME - A STRING CONTAI-           
001500*                                     NING A VALUE IN THE FORMAT          
001600*                                     YYYYMMDDHHMMSS.                     
001700*                                                                         
001800*                                 TISECONDS - NUMBER OF SECONDS           
001900*                                                                         
002000*                                 KDRC = 0 IF THE DATE-TIME IS            
002100*                                     VALID, 8 OTHERWISE.                 
002200*                                                                         
002300     03 TIME-TIDATETIME      PIC X(14).                                   
002400*                                 DATE AND TIME YYYYMMDDHHMMSS            
002500     03 TIME-TISECONDS       PIC S9(11)          COMP-3.                  
002600*                                 NBR OF SECONDS                          
002700     03 TIME-KDRC            PIC S9(9)           COMP.                    
002800*                                 RETURN CODE                             
002900*** END OF VILMAII-COPY LENGTH= 24 BYTES                                  
