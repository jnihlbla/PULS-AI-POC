000100 01  KNTL-WDP111.                                                         
000200*                                 SECURITY-SEGM I SUBMIT-DATABAS          
000300*                                 FYSISK NYCKEL WDP111KY                  
000400*                                              (IDOWNER + IDUSER)         
000500*                                 SÖKBEGREPP    IDOWNER, IDUSER           
000600     03 KNTL-IDOWNER         PIC X(8).                                    
000700*                                 ÄGAREIDENTITET I RACF                   
000800     03 KNTL-IDUSER          PIC X(8).                                    
000900*                                 ANVÄNDARIDENTITET I RACF                
001000     03 KNTL-TIREGDAT        PIC S9(7)           COMP-3.                  
001100*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
001200*** END COPY WDP111CCC0  LENGTH=20                                        
