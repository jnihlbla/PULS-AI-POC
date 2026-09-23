000100 01  MOD-W4O67201.                                                        
000200*                                 MOD-COPYTEXT FÖR W4067200               
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 FELMEDDELANDEFÄLT                       
000700     03 MOD-BEROUTE-IN       PIC X(2).                                    
000800*                                 MFS BEHANDLING AV INPUTFÄLT             
000900     03 MOD-BEROUTE-UT       PIC X(25).                                   
001000*                                 FÄRDVÄG, DESTINATION                    
001100     03 MOD-IDTRANSP-NAMN-IN PIC X(2).                                    
001200*                                 MFS BEHANDLING AV INPUTFÄLT             
001300     03 MOD-IDTRANSP-NAMN-UT PIC X(15).                                   
001400*                                 TRANSPORTMEDEL NAMN                     
001500     03 MOD-KVKOLLI-TRPT     PIC Z(3)9.                                   
001600*                                 ANTAL KOLLI TRANSPORT                   
001700     03 MOD-VKORDBTO-TRPT    PIC Z(5)9.9.                                 
001800*                                 BRUTTOVIKT TRANSPORT                    
001900     03 MOD-VLORDBTO-TRPT    PIC Z(3)9.9(3).                              
002000*                                 BRUTTOVOLYM TRANSPORT                   
002100     03 MOD-ADFLGEO          PIC X(3).                                    
002200*                                 GEOGRAFISKT OMRÅDE FÄRDIGLAGER          
002300     03 MOD-ADFLOMR          PIC Z(2)9.                                   
002400*                                 LASTNINGSOMR/STÄLL FÄRDIGLAGER          
002500     03 MOD-ADRUTNIV         PIC Z(2)9.                                   
002600*                                 RUTA/NIVÅ I FÄRDIGLAGRET                
002700     03 MOD-TEMFSINF         PIC X(61).                                   
002800*                                 INFORMATIONSMEDDELANDE                  
002900*** END COPY W4O67201C0  LENGTH=178                                       
