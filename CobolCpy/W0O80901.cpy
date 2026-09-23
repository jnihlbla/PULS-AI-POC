000100 01  W0O80901.                                                            
000200*                                 COPYTEXT FÖR MOD                        
000300*                                 W0O80901                                
000400     03 IDTRANS              PIC X(4).                                    
000500*                                 BILDNUMMER                              
000600     03 TEMFSFEL             PIC X(40).                                   
000700*                                 MFS FELMEDDELANDE                       
000800     03 KDLEVVIL-IN          PIC 9.                                       
000900*                                 LEVERANSVILLKOR                         
001000     03 UPPDAT-IN            PIC X.                                       
001100*                                 UPPDATERINGSTYP                         
001200     03 KDLEVVIL-UT          PIC 9.                                       
001300*                                 LEVERANSVILLKOR                         
001400     03 UPPDAT-UT            PIC X.                                       
001500*                                 UPPDATERINGSTYP                         
001600     03 RAD                  OCCURS 6 TIMES.                              
001700        05 BELEVVIL-ATTR     PIC X(2).                                    
001800        05 BELEVVIL          PIC X(35).                                   
001900*                                 LEVERANSVILLKOR                         
002000     03 TEMFSINF             PIC X(55).                                   
002100*                                 INFORMATIONSMEDDELANDE                  
002200*** END OF VILMAII-COPY LENGTH= 325 BYTES                                 
