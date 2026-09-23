000100 01  B64-WB64CONV.                                                        
000200*                                 PARAMETERS TO THE WB64CONV              
000300*                                 SUBPROGRAM.                             
000400*                                 CALL FORMAT:                            
000500*                                    USING B64-CONTROL-AREA               
000600*                                          B64-KVDLEN-IN                  
000700*                                          B64-DATA-IN                    
000800*                                          B64-KVDLEN-OUT                 
000900*                                          B64-DATA-OUT                   
001000*                                     DECODES BASE64 ENCONDED             
001100*                                     DATA.                               
001200*                                     DATA RETURNED IS IN UTF-8           
001300*                                     CHARSET.                            
001400*                                                                         
001500*                                     KVDLEN-IN SHOULD BE SET TO          
001600*                                     THE CORRECT LENGTH BEFORE           
001700*                                     EACH CALL.                          
001800*                                     KVDLEN-OUT WILL BE SET              
001900*                                     AFTER EXECUTION.                    
002000*                                     DATA-IN AND DATA-OUT ARE            
002100*                                     NOT DEFINED IN THIS COPY-           
002200*                                     TEXT. THEY SHOULD BE                
002300*                                     DECLARED IN THE PROGRAM             
002400*                                     AND CAN BE OF ANY LENGTH,           
002500*                                     MAX 1MB.                            
002600*                                     RC: 0 = OK                          
002700*                                         10 = INVALID ADDRESS.           
002800*                                                                         
002900     03 B64-CONTROL-AREA.                                                 
003000        05 B64-KDCALL        PIC S9(3)           COMP-3.                  
003100*                                 CALL TYPE                               
003200        05 B64-KDRC          PIC S9(9)           COMP.                    
003300*                                 RETURN CODE                             
003400        05 B64-MESSAGE       PIC X(100).                                  
003500     03 B64-KVDLEN-IN        PIC S9(9)           COMP.                    
003600*                                 LENGTH OF DATA                          
003700     03 B64-KVDLEN-OUT       PIC S9(9)           COMP.                    
003800*                                 LENGTH OF DATA                          
003900*** END OF VILMAII-COPY LENGTH= 114 BYTES                                 
