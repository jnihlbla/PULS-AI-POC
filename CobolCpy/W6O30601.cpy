000100 01  MOD-W6O30601.                                                        
000200*                                 COPYTEXT FÖR MOD                        
000300*                                 W6O30601                                
000400     03 MOD-IDTRANS          PIC X(4).                                    
000500*                                 BILDNUMMER                              
000600     03 MOD-TEMFSFEL         PIC X(40).                                   
000700*                                 MFS FELMEDDELANDE                       
000800     03 MOD-ADLAGOMR-IN      PIC Z(2)9.                                   
000900*                                 LAGEROMRÅDE                             
001000     03 MOD-ADLAGOMR-UT      PIC Z(2)9.                                   
001100*                                 LAGEROMRÅDE                             
001200     03 MOD-ADGANG-IN        PIC Z(2)9.                                   
001300*                                 GÅNG                                    
001400     03 MOD-ADGANG-UT        PIC Z(2)9.                                   
001500*                                 GÅNG                                    
001600     03 MOD-ADPLATS-IN       PIC Z(4)9.                                   
001700*                                 LAGERPLATSNUMMER                        
001800     03 MOD-ADPLATS-UT       PIC Z(4)9.                                   
001900*                                 LAGERPLATSNUMMER                        
002000     03 MOD-IDDC-IN          PIC X(2).                                    
002100*                                 IDENTIFIERARE LAGER                     
002200     03 MOD-IDDC-UT          PIC X(2).                                    
002300*                                 IDENTIFIERARE LAGER                     
002400     03 MOD-IDARTNR-ENTER    PIC Z(7)9.                                   
002500*                                 ARTIKELNUMMER                           
002600     03 MOD-IDARTNR-NEXT     PIC Z(7)9.                                   
002700*                                 ARTIKELNUMMER                           
002800     03 MOD-IDARTNR          OCCURS 14 TIMES                              
002900                             PIC Z(7)9.                                   
003000*                                 ARTIKELNUMMER                           
003100     03 MOD-BEART-ENG        OCCURS 14 TIMES                              
003200                             PIC X(25).                                   
003300*                                 ENGELSK ARTIKELBENÄMNING                
003400     03 MOD-TEMFSINF         PIC X(55).                                   
003500*                                 INFORMATIONSMEDDELANDE                  
