000100 01  MOD-W5O14501.                                                        
000200*                                 COPYTEXT FÖR MOD W5O14501               
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-IDFTG            PIC 9(2).                                    
000800*                                 FÖRETAGSID EKONOM REDOVISNING           
000900     03 MOD-REEMBINF-NEW-ATTR                                             
001000                             PIC X(2).                                    
001100*                                 MFS ATTRIBUTFÄLT                        
001200     03 MOD-REEMBINF-NEW     PIC -(2)9.9(2).                              
001300*                                 INFLATIONSFAKTOR EMBALAGE               
001400     03 MOD-REEMBINF-ACTUAL  PIC -(2)9.9(2).                              
001500*                                 INFLATIONSFAKTOR EMBALAGE               
001600     03 MOD-SULSNIV-MIN-NEW-ATTR                                          
001700                             PIC X(2).                                    
001800*                                 MFS ATTRIBUTFÄLT                        
001900     03 MOD-SULSNIV-MIN-NEW  PIC -(5)9.                                   
002000*                                 LÄGSTA GRÄNS FÖR LS ÄNDRING             
002100     03 MOD-SULSNIV-MAX-NEW-ATTR                                          
002200                             PIC X(2).                                    
002300*                                 MFS ATTRIBUTFÄLT                        
002400     03 MOD-SULSNIV-MAX-NEW  PIC -(5)9.                                   
002500*                                 HÖGSTA GRÄNS FÖR LS ÄNDRING             
002600     03 MOD-SULSNIV-MIN-ACTUAL                                            
002700                             PIC -(5)9.                                   
002800*                                 LÄGSTA GRÄNS FÖR LS ÄNDRING             
002900     03 MOD-SULSNIV-MAX-ACTUAL                                            
003000                             PIC -(5)9.                                   
003100*                                 HÖGSTA GRÄNS FÖR LS ÄNDRING             
003200     03 MOD-IDUSER           PIC X(8).                                    
003300*                                 ANVÄNDARENS SÄKERHETS ID                
003400     03 MOD-TIUPPDAT         PIC 9(6).                                    
003500*                                 UPPDATERINGSDATUM  (ÅÅMMDD)             
003600     03 MOD-TEMFSINF         PIC X(55).                                   
003700*                                 INFORMATIONSMEDDELANDE                  
003800*** END OF VILMAII-COPY LENGTH= 157 BYTES                                 
