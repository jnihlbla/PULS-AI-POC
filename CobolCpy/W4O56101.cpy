000100 01  MOD-W4O56101.                                                        
000200*                                 MOD-COPYTEXT                            
000300*                                 FÖR W40561                              
000400     03 MOD-IDTRANS          PIC X(4).                                    
000500*                                 BILDNUMMER                              
000600     03 MOD-TEMFSFEL         PIC X(40).                                   
000700*                                 MFS FELMEDDELANDE                       
000800     03 MOD-IDPRODNR-IN      PIC X(7).                                    
000900*                                 PRODUKTIONSNUMMER                       
001000     03 MOD-IDPRODNR-UT      PIC X(7).                                    
001100*                                 PRODUKTIONSNUMMER                       
001200     03 MOD-IDKOLLI-IN       PIC X(5).                                    
001300*                                 KOLLINUMMER                             
001400     03 MOD-IDKOLLI-UT       PIC X(5).                                    
001500*                                 KOLLINUMMER                             
001600     03 MOD-VKORDBTO-KOLLI-UT                                             
001700                             PIC Z(5)9.9.                                 
001800*                                 ORDERVIKT BRUTTO PER KOLLI              
001900     03 MOD-VKORDBTO-KOLLI-IN-ATTR                                        
002000                             PIC X(2).                                    
002100*                                 MFS ATTRIBUTFÄLT                        
002200     03 MOD-VKORDBTO-KOLLI-IN                                             
002300                             PIC X(8).                                    
002400*                                 ORDERVIKT BRUTTO PER KOLLI              
002500     03 MOD-VLORDBTO-KOLLI-UT                                             
002600                             PIC Z(3)9.9(3).                              
002700*                                 ORDERVOLYM BRUTTO KOLLI                 
002800     03 MOD-VLORDBTO-KOLLI-IN-ATTR                                        
002900                             PIC X(2).                                    
003000*                                 MFS ATTRIBUTFÄLT                        
003100     03 MOD-VLORDBTO-KOLLI-IN                                             
003200                             PIC X(8).                                    
003300*                                 ORDERVOLYM BRUTTO KOLLI                 
003400     03 MOD-TEMFSINF         PIC X(55).                                   
003500*                                 INFORMATIONSMEDDELANDE                  
003600*** END OF VILMAII-COPY LENGTH= 159 BYTES                                 
