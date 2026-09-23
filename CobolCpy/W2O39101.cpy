000100 01  MOD-W2O39101.                                                        
000200*                                 MOD-COPYTEXT FOR W2039100               
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-IDTYPE-IN        PIC X.                                       
000800     03 MOD-IDDC-REF-IN      PIC X(2).                                    
000900*                                 SÄNDANDE LAGER FÖR REFILL               
001000     03 MOD-IDTYPE-UT        PIC X(5).                                    
001100     03 MOD-IDDC-REF-UT      PIC X(2).                                    
001200*                                 SÄNDANDE LAGER FÖR REFILL               
001300     03 MOD-GRP              OCCURS 13 TIMES.                             
001400        05 MOD-SELECT        PIC X.                                       
001500        05 MOD-IDPERSON-BUY-ATTR                                          
001600                             PIC X(2).                                    
001700*                                 MFS ATTRIBUTFÄLT                        
001800        05 MOD-IDPERSON-BUY  PIC Z(2)9.                                   
001900*                                 PERSONKOD REFILLANSVARIG                
002000        05 MOD-TO-REVIEW     PIC X(6).                                    
002100     03 MOD-TEMFSINF         PIC X(55).                                   
002200*                                 INFORMATIONSMEDDELANDE                  
002300*** END OF VILMAII-COPY LENGTH= 265 BYTES                                 
