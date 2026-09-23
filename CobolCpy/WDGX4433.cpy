000100 01  4433-WDGX4433.                                                       
000200*                                 TRANSPORTAVGÅNGS TABELL                 
000300*                                 FYSISK NYCKEL WDGXKEY:                  
000400*                                   (IDHTYP + IDDC +                      
000500*                                    LOW-VALUE)                           
000600     03 4433-IDHTYP          PIC X(4).                                    
000700*                                 HÄNDELSETYP                             
000800     03 4433-IDDC            PIC X(2).                                    
000900*                                 IDENTIFIERARE LAGER                     
001000*                                 WAREHOUSE IDENTIFIER                    
001100     03 4433-LOW-VALUE       PIC X(24).                                   
