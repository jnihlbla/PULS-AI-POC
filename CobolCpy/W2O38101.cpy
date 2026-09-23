000100 01  MOD-W2O38101.                                                        
000200*                                 MOD-COPYTEXT FÖR W2038100               
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-IDTYPE-IN        PIC X.                                       
000800     03 MOD-IDTYPE-UT        PIC X(5).                                    
000900     03 MOD-GRP              OCCURS 13 TIMES.                             
001000        05 MOD-SELECT        PIC X.                                       
001100        05 MOD-TO-REVIEW     PIC Z(5)9.                                   
001200        05 MOD-IDDC-2381     OCCURS 6 TIMES                               
001300                             PIC X(2).                                    
001400*                                 IDENTIFIERARE LAGER                     
001500     03 MOD-TEMFSINF         PIC X(55).                                   
001600*                                 INFORMATIONSMEDDELANDE                  
001700*** END OF VILMAII-COPY LENGTH= 352 BYTES                                 
