000100 01  2223-WDGX2223.                                                       
000200*                                 LARMKÖ                                  
000300*                                 FYSISK NYCKEL WDGXKEY:                  
000400*                                 (IDHTYP + IDANSK +                      
000500*                                  LOW-VALUE)                             
000600     03 2223-IDHTYP          PIC X(4).                                    
000700*                                 HÄNDELSETYP                             
000800     03 2223-IDANSK          PIC S9(3)           COMP-3.                  
000900*                                 ANSKAFFARNUMMER                         
001000*                                 PROCURER NO.                            
001100     03 2223-LOW-VALUE       PIC X(24).                                   
001200*** END COPY WDGX2223C0  LENGTH=30                                        
