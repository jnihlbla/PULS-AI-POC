000100 01  4825-WDGX4825.                                                       
000200*                                 KVALITE                                 
000300*                                 FELKODSREGISTER                         
000400*                                 FYSISK NYCKEL WDGXKEY:                  
000500*                                   (IDHTYP + IDSKYLT +                   
000600*                                    LOW-VALUE)                           
000700     03 4825-IDHTYP          PIC X(4).                                    
000800*                                 HÄNDELSETYP                             
000900     03 4825-IDSKYLT         PIC X(3).                                    
001000*                                 NATIONALITETSTECKEN                     
001100*                                 NATIONALITY SIGN                        
001200     03 4825-LOW-VALUE       PIC X(23).                                   
001300*** END COPY WDGX4825C0  LENGTH=30                                        
