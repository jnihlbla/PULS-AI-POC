000100 01  MOD-W5O30301.                                                        
000200*                                 MOD-COPYTEXT FÖR BILD                   
000300*                                 INVENTERING                             
000400     03 MOD-IDTRANS          PIC X(4).                                    
000500*                                 BILDNUMMER                              
000600     03 MOD-TEMFSFEL         PIC X(40).                                   
000700*                                 MFS FELMEDDELANDE                       
000800     03 MOD-IDDC-IN          PIC X(2).                                    
000900*                                 IDENTIFIERARE LAGER                     
001000     03 MOD-IDDC-UT          PIC X(2).                                    
001100*                                 IDENTIFIERARE LAGER                     
001200     03 MOD-KDVVKL-FORSTA    PIC X.                                       
001300*                                 VOLYMVÄRDESKLASS                        
001400     03 MOD-ADLAGOMR-FORSTA  PIC X(2).                                    
001500*                                 LAGEROMRÅDE                             
001600     03 MOD-KDVVKL-NASTA     PIC X.                                       
001700*                                 VOLYMVÄRDESKLASS                        
001800     03 MOD-ADLAGOMR-NASTA   PIC X(2).                                    
001900*                                 LAGEROMRÅDE                             
002000     03 MOD-OMR-VVKL-GRP     OCCURS 27 TIMES.                             
002100*                                 VOLYMVÄRDESKLASS PER OMRÅDE             
002200        05 MOD-KDVVKL        PIC 9.                                       
002300*                                 VOLYMVÄRDESKLASS                        
002400        05 MOD-ADLAGOMR      PIC Z9.                                      
002500*                                 LAGEROMRÅDE                             
002600        05 MOD-KVANTAL       PIC Z(5)9.                                   
002700*                                 ANTAL ALLMÄNT                           
002800     03 MOD-KVANTAL-UTSKR    PIC Z(5)9.                                   
002900*                                 ANTAL ALLMÄNT                           
003000     03 MOD-KVANTAL-EJUTSKR  PIC Z(5)9.                                   
003100*                                 ANTAL ALLMÄNT                           
003200     03 MOD-TEMFSINF         PIC X(55).                                   
003300*                                 INFORMATIONSMEDDELANDE                  
