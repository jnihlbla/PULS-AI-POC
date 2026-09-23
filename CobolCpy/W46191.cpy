000100 01  LOGG-W46191.                                                         
000200*                                 ANVÄNDS VID LOGGNING AV ANTAL           
000300*                                 TRANSAR SOM SKICKAS TILL                
000400*                                 IMPORTÖR                                
000500     03 LOGG-IDDISTR         PIC S9(5)           COMP-3.                  
000600*                                 DISTRIKTNUMMER                          
000700     03 LOGG-TIFILDAT        PIC S9(7)           COMP-3.                  
000800*                                 DATUM NÄR EN FIL SKAPATS ÅÅMMDD         
000900     03 LOGG-IDGEN           PIC S9(5)           COMP-3.                  
001000*                                 GENERATIONSNUMMER                       
001100     03 LOGG-TIHHMMSS        PIC S9(7)           COMP-3.                  
001200*                                 TIM - MIN - SEK   (HHMMSS)              
001300     03 LOGG-KVTRANS         PIC S9(7)           COMP-3.                  
001400*                                 ANTAL TRANSAKTIONER                     
001500*** END COPY W46191CCC0  LENGTH=18                                        
