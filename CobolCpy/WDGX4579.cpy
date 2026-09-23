000100 01  4579-WDGX4579-CTX.                                                   
000200*                                 ÅTERSTARTS REGISTER                     
000300*                                 ROTSEGMENT TO ROT SKALL FINNAS          
000400*                                 NYCKEL WDGXKEY =(IDHYP + IDPGM          
000500*                                 + LOW-VALUE)                            
000600     03 4579-IDHTYP          PIC X(4).                                    
000700*                                 HÄNDELSETYP                             
000800     03 4579-IDPGM           PIC X(8).                                    
000900*                                 PROGRAM IDENTITET                       
001000*                                 PROGRAM INTENTITY                       
001100     03 4579-LOW-VALUE       PIC X(18).                                   
001200*** END OF VILMAII-COPY LENGTH= 30 BYTES                                  
