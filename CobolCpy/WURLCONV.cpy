000100 01  URL-WURLCONV.                                                        
000200*                                 PARAMETERS TO THE WURLCONV              
000300*                                 SUBPROGRAM.                             
000400*                                 CALL FORMAT:                            
000500*                                    USING URL-CONTROL-AREA               
000600*                                          URL-KVDLEN-IN                  
000700*                                          URL-DATA-IN                    
000800*                                          URL-KVDLEN-OUT                 
000900*                                          URL-DATA-OUT                   
001000*                                     ENCODE USING PERCENT                
001100*                                     ENCODING.                           
001200*                                                                         
001300*                                     KVDLEN-IN SHOULD BE SET TO          
001400*                                     THE CORRECT LENGTH BEFORE           
001500*                                     EACH CALL.                          
001600*                                     KVDLEN-OUT WILL BE SET              
001700*                                     AFTER EXECUTION.                    
001800*                                     DATA-IN AND DATA-OUT ARE            
001900*                                     NOT DEFINED IN THIS COPY-           
002000*                                     TEXT. THEY SHOULD BE                
002100*                                     DECLARED IN THE PROGRAM             
002200*                                     AND CAN BE OF VARIABLE              
002300*                                     LENGTH - MAX 3000                   
002400*                                     RC: 0 = OK                          
002500*                                         10 = INVALID CALL.              
002600*                                                                         
002700     03 URL-CONTROL-AREA.                                                 
002800        05 URL-KDCALL        PIC S9(3)           COMP-3.                  
002900*                                 CALL TYPE                               
003000        05 URL-KDRC          PIC S9(9)           COMP.                    
003100*                                 RETURN CODE                             
003200        05 URL-MESSAGE       PIC X(100).                                  
003300     03 URL-KVDLEN-IN        PIC S9(9)           COMP.                    
003400*                                 LENGTH OF DATA                          
003500     03 URL-KVDLEN-OUT       PIC S9(9)           COMP.                    
003600*                                 LENGTH OF DATA                          
003700*** END OF VILMAII-COPY LENGTH= 114 BYTES                                 
