000100 01  MOD-W6O16701.                                                        
000200*                                 MOD COPYTEXT FÖR W6O16700               
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-IDARTNR-IN       PIC X(9).                                    
000800*                                 ARTIKELNUMMER                           
000900     03 MOD-IDARTNR-UT       PIC X(9).                                    
001000*                                 ARTIKELNUMMER                           
001100     03 MOD-TEETIK-EXT-GRP   OCCURS 12 TIMES.                             
001200        05 MOD-TEETIK-EXT-ATTR                                            
001300                             PIC X(2).                                    
001400*                                 MFS ATTRIBUTFÄLT                        
001500        05 MOD-TEETIK-EXT    PIC X(60).                                   
001600*                                 ETIKETT SPECIAL INFO                    
001700     03 MOD-IDUSER           PIC X(8).                                    
001800*                                 ANVÄNDARENS SÄKERHETS ID                
001900     03 MOD-TIUPPDAT         PIC 9(6).                                    
002000*                                 UPPDATERINGSDATUM  (ÅÅMMDD)             
002100     03 MOD-DAREGDAT         PIC 9(8).                                    
002200*                                 REGISTRERINGSDATUM (ÅÅÅÅMMDD)           
002300     03 MOD-TEMFSINF         PIC X(55).                                   
002400*                                 INFORMATIONSMEDDELANDE                  
002500*** END OF VILMAII-COPY LENGTH= 883 BYTES                                 
