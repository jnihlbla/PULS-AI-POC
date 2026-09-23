000100 01  4535-WDGX4535.                                                       
000200*                                 FÖRRÅDSDATATEXT                         
000300*                                 FYSISK NYCKEL WDGXKEY:                  
000400*                                   (IDHTYP + KDFDKRAV +                  
000500*                                    LOW-VALUE)                           
000600     03 4535-IDHTYP          PIC X(4).                                    
000700*                                 HÄNDELSETYP                             
000800     03 4535-KDFDKRAV        PIC S9(3)           COMP-3.                  
000900*                                 TRANSPORTFÖRPACKNINGSKOD                
001000*                                 PACKING CODE                            
001100     03 4535-LOW-VALUE       PIC X(24).                                   
001200*** END COPY WDGX4535C0  LENGTH=30                                        
