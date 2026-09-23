000100 01  W0O80501.                                                            
000200*                                 COPYTEXT FÖR MOD                        
000300*                                 W0O80501                                
000400*                                                                         
000500     03 IDTRANS              PIC X(4).                                    
000600*                                 BILDNUMMER                              
000700     03 TEMFSFEL             PIC X(40).                                   
000800*                                 MFS FELMEDDELANDE                       
000900     03 KDFDKRAV-IN          PIC X(3).                                    
001000*                                 TRANSPORTFÖRPACKNINGSKOD                
001100     03 KDFDKRAV-UT          PIC X(3).                                    
001200*                                 TRANSPORTFÖRPACKNINGSKOD                
001300     03 IDSKYLT-S            PIC X(3).                                    
001400*                                 NATIONALITETSTECKEN                     
001500     03 IDSKYLT-GB           PIC X(3).                                    
001600*                                 NATIONALITETSTECKEN                     
001700     03 BEFDKRAV-S-ATTR      PIC X(2).                                    
001800*                                 MFS ATTRIBUTFÄLT                        
001900     03 BEFDKRAV-S           PIC X(40).                                   
002000*                                 FÖRRÅDSDATAKRAV                         
002100     03 KDEMBAL-S-ATTR       PIC X(2).                                    
002200*                                 MFS ATTRIBUTFÄLT                        
002300     03 KDEMBAL-S            PIC X.                                       
002400*                                 KOD FÖR ATT TALA OM VILKEN TYP          
002500*                                 AV EMBALLAGE SOM SKALL ANVÄNDAS         
002600     03 BEFDKRAV-GB-ATTR     PIC X(2).                                    
002700*                                 MFS ATTRIBUTFÄLT                        
002800     03 BEFDKRAV-GB          PIC X(40).                                   
002900*                                 FÖRRÅDSDATAKRAV                         
003000     03 KDEMBAL-GB-ATTR      PIC X(2).                                    
003100*                                 MFS ATTRIBUTFÄLT                        
003200     03 KDEMBAL-GB           PIC X.                                       
003300*                                 KOD FÖR ATT TALA OM VILKEN TYP          
003400*                                 AV EMBALLAGE SOM SKALL ANVÄNDAS         
003500     03 TEMFSINF             PIC X(61).                                   
003600*                                 INFORMATIONSMEDDELANDE                  
003700*** END COPY W0O80501C0  LENGTH=207                                       
