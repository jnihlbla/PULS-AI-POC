000100 01  MOD-W3O16201.                                                        
000200*                                 MOD-COPYTEXT FÖR W3016200               
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-BELEV-IN         PIC X(2).                                    
000800*                                 MFS BEHANDLING AV INPUTFÄLT             
000900     03 MOD-BELEV-UT         PIC X(30).                                   
001000*                                 LEVERANTÖRENS ARTIKELBENÄMNING          
001100     03 MOD-BELEV-LO         PIC X(30).                                   
001200*                                 LEVERANTÖRENS ARTIKELBENÄMNING          
001300     03 MOD-BELEV-HI         PIC X(30).                                   
001400*                                 LEVERANTÖRENS ARTIKELBENÄMNING          
001500     03 MOD-INFO-RAD         OCCURS 14 TIMES.                             
001600*                                 RADINFORMATION                          
001700        05 MOD-BELEV         PIC X(30).                                   
001800*                                 LEVERANTÖRENS ARTIKELBENÄMNING          
001900        05 FILLER            PIC X(4).                                    
002000        05 MOD-IDARTNR       PIC Z(9).                                    
002100*                                 ARTIKELNUMMER                           
002200        05 FILLER            PIC X(2).                                    
002300        05 MOD-BEART-SVE     PIC X(25).                                   
002400*                                 SVENSK ARTIKELBENÄMNING                 
002500     03 MOD-TEMFSINF         PIC X(61).                                   
002600*                                 INFORMATIONSMEDDELANDE                  
002700*** END COPY W3O16201C0  LENGTH=1177                                      
