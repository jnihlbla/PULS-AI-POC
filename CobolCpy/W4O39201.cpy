000100 01  MOD-W4O39201.                                                        
000200*                                 COPYTEXT FÖR MOD W4O39201               
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-IDPRODNR-UT      PIC X(7).                                    
000800*                                 PRODUKTIONSNUMMER                       
000900     03 MOD-IDDISTR-UT       PIC X(4).                                    
001000*                                 DISTRIKTNUMMER                          
001100     03 MOD-IDKUNDNR-UT      PIC X(6).                                    
001200*                                 KUNDNUMMER                              
001300     03 MOD-KDFRAKT-UT       PIC X(2).                                    
001400*                                 FRAKTSÄTT C1-C2 TILL KUND               
001500     03 MOD-IDORDNR-UT       PIC X(5).                                    
001600*                                 ORDERNUMMER                             
001700     03 MOD-KDORDKL-UT       PIC X.                                       
001800*                                 ORDERKLASS                              
001900     03 MOD-IDDC-UT          PIC X(2).                                    
002000*                                 IDENTIFIERARE LAGER                     
002100     03 MOD-IDKOLLI-SENAST   PIC Z(4)9.                                   
002200*                                 KOLLINUMMER                             
002300     03 MOD-RAD              OCCURS 13 TIMES.                             
002400*                                 COPYTEXT FÖR MOD W4O39201               
002500        05 MOD-IDKOLLI       PIC 9(5).                                    
002600*                                 KOLLINUMMER                             
002700        05 MOD-KDKOLLI-ATTR  PIC X(2).                                    
002800*                                 MFS ATTRIBUTFÄLT                        
002900        05 MOD-KDKOLLI       PIC X(2).                                    
003000*                                 MFS BEHANDLING AV INPUTFÄLT             
003100        05 MOD-VKORDBTO-KOLLI-ATTR                                        
003200                             PIC X(2).                                    
003300*                                 MFS ATTRIBUTFÄLT                        
003400        05 MOD-VKORDBTO-KOLLI                                             
003500                             PIC X(2).                                    
003600*                                 MFS BEHANDLING AV INPUTFÄLT             
003700        05 MOD-KDEMBTYP-ATTR PIC X(2).                                    
003800*                                 MFS ATTRIBUTFÄLT                        
003900        05 MOD-KDEMBTYP      PIC X(2).                                    
004000*                                 MFS BEHANDLING AV INPUTFÄLT             
004100     03 MOD-TEMFSINF         PIC X(55).                                   
004200*                                 INFORMATIONSMEDDELANDE                  
004300*** END OF VILMAII-COPY LENGTH= 352 BYTES                                 
