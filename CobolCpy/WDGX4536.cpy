000100 01  4536-WDGX4536.                                                       
000200*                                 FÖRRÅDSDATATEXTER                       
000300*                                 FYSISK NYCKEL WDGXKEY:                  
000400*                                 (IDSKYLT + LOW-VALUE)                   
000500     03 4536-IDSKYLT         PIC X(3).                                    
000600*                                 NATIONALITETSTECKEN                     
000700*                                 NATIONALITY SIGN                        
000800     03 4536-LOW-VALUE       PIC X(2).                                    
000900     03 4536-BEFDKRAV        PIC X(40).                                   
001000*                                 FÖRRÅDSDATAKRAV                         
001100     03 4536-KDEMBAL         PIC X.                                       
001200*                                 KOD FÖR ATT TALA OM VILKEN TYP          
001300*                                 AV                                      
001400*                                 EMBALLAGE SOM SKALL ANVÄNDAS            
001500*                                 CODE TO DESCRIBE WHAT KIND OF           
001600*                                 EMBALLAGE TO USE                        
001700     03 4536-FILLER          PIC X(14).                                   
001800*** END COPY WDGX4536C0  LENGTH=60                                        
