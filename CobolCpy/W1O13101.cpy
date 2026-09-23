000100 01  MOD-W1O13101-CTX.                                                    
000200     03 MOD-IDTRANS          PIC X(4).                                    
000300*                                 BILDNUMMER                              
000400     03 MOD-TEMFSFEL         PIC X(40).                                   
000500*                                 MFS FELMEDDELANDE                       
000600     03 MOD-IDBENNR-IN       PIC X(7).                                    
000700*                                 BENÄMNINGSNUMMER                        
000800     03 MOD-IDBENNR-UT       PIC X(7).                                    
000900*                                 BENÄMNINGSNUMMER                        
001000     03 MOD-IDARTNR-SPAR     PIC 9(9).                                    
001100*                                 ARTIKELNUMMER                           
001200     03 MOD-BEART            PIC X(25).                                   
001300*                                 ARTIKELBENÄMNING                        
001400     03 MOD-IDBENNR-NYTT-ATTR                                             
001500                             PIC X(2).                                    
001600*                                 MFS ATTRIBUTFÄLT                        
001700     03 MOD-IDBENNR-NYTT     PIC X(7).                                    
001800*                                 BENÄMNINGSNUMMER                        
001900     03 MOD-BEART-NYTT       PIC X(25).                                   
002000*                                 ARTIKELBENÄMNING                        
002100     03 MOD-W1O13101-001-GRP OCCURS 72 TIMES.                             
002200        05 MOD-IDARTNR       PIC Z(8)9.                                   
002300*                                 ARTIKELNUMMER                           
002400     03 MOD-TEMFSINF         PIC X(55).                                   
002500*                                 INFORMATIONSMEDDELANDE                  
002600*** END OF VILMAII-COPY LENGTH= 829 BYTES                                 
