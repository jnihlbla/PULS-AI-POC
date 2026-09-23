000100 01  MOD-W9O20201.                                                        
000200*                                 MODCOPYTEXT TILL W9020200.              
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-IDMFSFEL         PIC X(3).                                    
000600*                                 MFS FELMEDDELANDE NUMMER                
000700     03 MOD-SERIENR          PIC X(10).                                   
000800     03 MOD-IDARTNR          PIC 9(9).                                    
000900*                                 ARTIKELNUMMER                           
001000     03 MOD-APPARATTYP       PIC X(11).                                   
001100     03 MOD-SKYDDSKOD        OCCURS 3 TIMES                               
001200                             PIC X(6).                                    
001300*** END OF VILMAII-COPY LENGTH= 55 BYTES                                  
