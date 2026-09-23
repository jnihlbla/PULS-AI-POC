000100 01  MOD-W5O30601.                                                        
000200*                                 COPYTEXT F÷R MOD                        
000300*                                 W5O30601                                
000400*                                                                         
000500     03 MOD-IDTRANS          PIC X(4).                                    
000600*                                 BILDNUMMER                              
000700     03 MOD-TEMFSFEL         PIC X(40).                                   
000800*                                 MFS FELMEDDELANDE                       
000900     03 MOD-IDARTNR-IN       PIC X(9).                                    
001000*                                 ARTIKELNUMMER                           
001100     03 MOD-IDARTNR-UT       PIC X(9).                                    
001200*                                 ARTIKELNUMMER                           
001300     03 MOD-IDDC-IN          PIC X(2).                                    
001400*                                 IDENTIFIERARE LAGER                     
001500     03 MOD-IDDC-UT          PIC X(2).                                    
001600*                                 IDENTIFIERARE LAGER                     
001700     03 MOD-TIUPPDAT         PIC 9(6).                                    
001800*                                 UPPDATERINGSDATUM  (≈≈MMDD)             
001900     03 MOD-TIUPPTID         PIC 9(2)B9(2)B9(2)B9(2).                     
002000*                                 UPPDATERINGSTID  (TTMMSSTH)             
002100     03 MOD-IDPRODNR         PIC Z(7).                                    
002200*                                 PRODUKTIONSNUMMER                       
002300     03 MOD-KDORDKL          PIC X.                                       
002400*                                 ORDERKLASS                              
002500     03 MOD-IDPW             PIC X(8).                                    
002600*                                 PASSWORD   (L÷SENORD)                   
002700     03 MOD-TEMFSINF         PIC X(55).                                   
002800*                                 INFORMATIONSMEDDELANDE                  
