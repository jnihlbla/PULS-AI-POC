000100 01  MID-W6I30901.                                                        
000200*                                                                         
000300     03 MID-IDFAKT-IN        PIC X(7).                                    
000400*                                 FAKTURANUMMER                           
000500     03 MID-IDFAKT-UT        PIC X(7).                                    
000600*                                 FAKTURANUMMER                           
000700     03 MID-IDORDNR-IN       PIC X(5).                                    
000800*                                 ORDERNUMMER UTGÅR PD90                  
000900     03 MID-IDORDNR-UT       PIC X(5).                                    
001000*                                 ORDERNUMMER UTGÅR PD90                  
001100     03 MID-IDKUNDNR-IN      PIC X(6).                                    
001200*                                 KUNDNUMMER                              
001300     03 MID-IDKUNDNR-UT      PIC X(6).                                    
001400*                                 KUNDNUMMER                              
001500     03 MID-IDKOLLI-IN       PIC X(5).                                    
001600*                                 KOLLINUMMER                             
001700     03 MID-IDKOLLI-UT       PIC X(5).                                    
001800*                                 KOLLINUMMER                             
001900     03 MID-IDDC-SEND-IN     PIC X(2).                                    
002000*                                 IDENTIFIERARE LAGER                     
002100     03 MID-IDDC-SEND-UT     PIC X(2).                                    
002200*                                 IDENTIFIERARE LAGER                     
002300     03 MID-IDDC-REC-IN      PIC X(2).                                    
002400*                                 IDENTIFIERARE LAGER                     
002500     03 MID-IDDC-REC-UT      PIC X(2).                                    
002600*                                 IDENTIFIERARE LAGER                     
002700     03 MID-FLSLUT-BEKR      PIC X.                                       
002800     03 MID-INPUT.                                                        
002900*                                                                         
003000        05 MID-TABELL        OCCURS 13 TIMES.                             
003100*                                                                         
003200           07 MID-IDARTNR    PIC X(9).                                    
003300*                                 ARTIKELNUMMER                           
003400           07 MID-KVANTMOT   PIC X(6).                                    
003500*                                 ANTAL MOTTAGET                          
003600           07 MID-KVSKROT    PIC X(7).                                    
003700*                                 ANTAL SENASTE SKROTORDER                
003800           07 MID-ADLAGOMR   PIC X(2).                                    
003900*                                 LAGEROMRÅDE                             
004000           07 MID-ADGANG     PIC X(2).                                    
004100*                                 GÅNG                                    
004200           07 MID-ADPLATS    PIC X(5).                                    
004300*                                 LAGERPLATSNUMMER                        
004400        05 MID-KOLLI-KLAR    PIC X.                                       
004500        05 MID-IDUSER-003    PIC X(5).                                    
004600*                                 ANSVARIGT USERID INLÄGGN.(R32)          
004700*** END OF VILMAII-COPY LENGTH= 464 BYTES                                 
