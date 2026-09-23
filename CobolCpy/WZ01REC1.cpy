000100 01  RECV-CONTROL-AREA.                                                   
000200*                                 USED IN ALL TYPES OF CALLS              
000300*                                                                         
000400     03 RECV-KDFUNC          PIC X(10).                                   
000500*                                 FUNCTION CODE                           
000600     03 RECV-KDRC            PIC S9(9)           COMP.                    
000700*                                 RETURN CODE                             
000800     03 RECV-IDCOM           PIC S9(9)           COMP.                    
000900*                                 ID OF THE TRANSMISSION                  
001000     03 RECV-KDTRANS         PIC X(8).                                    
001100*                                 TRANSACTION CODE                        
001200*** END OF VILMAII-COPY LENGTH= 26 BYTES                                  
