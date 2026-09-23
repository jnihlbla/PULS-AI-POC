000100 01  MOD-W4O37101.                                                        
000200*                                 MOD-COPYTEXT FÖR BILD 4354              
000300*                                 FRÅGA PÅ PRODUKTIONSKANAL               
000400     03 MOD-IDTRANS          PIC X(4).                                    
000500*                                 BILDNUMMER                              
000600     03 MOD-TEMFSFEL         PIC X(40).                                   
000700*                                 MFS FELMEDDELANDE                       
000800     03 MOD-KDCMD            OCCURS 13 TIMES                              
000900                             PIC X.                                       
001000*                                 RAD-UPPDATERINGSKOMMANDO                
001100     03 MOD-TEMFSINF         PIC X(61).                                   
001200*                                 INFORMATIONSMEDDELANDE                  
001300*** END COPY W4O37101C0  LENGTH=118                                       
