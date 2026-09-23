000100 01  MOD-W5O28501.                                                        
000200*                                 MOD-COPYTEXT FÖR W5028500               
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-TIREGDAT-IN      PIC X(6).                                    
000800*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
000900     03 MOD-TIREGTID-IN      PIC X(6).                                    
001000*                                 REGISTRERINGSTID                        
001100     03 MOD-IDUSER-IN        PIC X(8).                                    
001200*                                 ANVÄNDARENS SÄKERHETS ID                
001300     03 MOD-TIREGDAT-UT      PIC X(6).                                    
001400*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
001500     03 MOD-TIREGTID-UT      PIC X(6).                                    
001600*                                 REGISTRERINGSTID                        
001700     03 MOD-IDUSER-UT        PIC X(8).                                    
001800*                                 ANVÄNDARENS SÄKERHETS ID                
001900     03 MOD-W5O28501-GRP     OCCURS 13 TIMES.                             
002000        05 MOD-ADLAGOMR      PIC Z9.                                      
002100*                                 LAGEROMRÅDE                             
002200        05 MOD-ADGANG        PIC Z9.                                      
002300*                                 GÅNG                                    
002400        05 MOD-ADPLATS       PIC Z(4)9.                                   
002500*                                 LAGERPLATSNUMMER                        
002600        05 MOD-IDARTNR       PIC Z(7)9.                                   
002700*                                 ARTIKELNUMMER                           
002800        05 MOD-KVLS-ATTR     PIC X(2).                                    
002900*                                 MFS ATTRIBUTFÄLT                        
003000        05 MOD-KVLS          PIC X(7).                                    
003100*                                 LAGERSALDO                              
003200        05 MOD-BEART         PIC X(25).                                   
003300*                                 ARTIKELBENÄMNING                        
003400     03 MOD-TEMFSINF         PIC X(55).                                   
003500*                                 INFORMATIONSMEDDELANDE                  
003600*** END OF VILMAII-COPY LENGTH= 802 BYTES                                 
