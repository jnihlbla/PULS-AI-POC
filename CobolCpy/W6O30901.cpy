000100 01  MOD-W6O30901.                                                        
000200*                                                                         
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-IDFAKT-IN        PIC X(7).                                    
000800*                                 FAKTURANUMMER                           
000900     03 MOD-IDFAKT-UT        PIC X(7).                                    
001000*                                 FAKTURANUMMER                           
001100     03 MOD-IDORDNR-IN       PIC X(5).                                    
001200*                                 ORDERNUMMER UTGÅR PD90                  
001300     03 MOD-IDORDNR-UT       PIC X(5).                                    
001400*                                 ORDERNUMMER UTGÅR PD90                  
001500     03 MOD-IDKUNDNR-IN      PIC X(6).                                    
001600*                                 KUNDNUMMER                              
001700     03 MOD-IDKUNDNR-UT      PIC X(6).                                    
001800*                                 KUNDNUMMER                              
001900     03 MOD-IDKOLLI-IN       PIC X(5).                                    
002000*                                 KOLLINUMMER                             
002100     03 MOD-IDKOLLI-UT       PIC X(5).                                    
002200*                                 KOLLINUMMER                             
002300     03 MOD-IDDC-SEND-IN     PIC X(2).                                    
002400*                                 IDENTIFIERARE LAGER                     
002500     03 MOD-IDDC-SEND-UT     PIC X(2).                                    
002600*                                 IDENTIFIERARE LAGER                     
002700     03 MOD-IDDC-REC-IN      PIC X(2).                                    
002800*                                 IDENTIFIERARE LAGER                     
002900     03 MOD-IDDC-REC-UT      PIC X(2).                                    
003000*                                 IDENTIFIERARE LAGER                     
003100     03 MOD-FLSLUT-BEKR      PIC X.                                       
003200     03 MOD-TAB              OCCURS 13 TIMES.                             
003300*                                 GRUPP MED TABELLRADER INPUT             
003400        05 MOD-IDARTNR-ATTR  PIC X(2).                                    
003500*                                 MFS ATTRIBUTFÄLT                        
003600        05 MOD-IDARTNR       PIC X(9).                                    
003700*                                 ARTIKELNUMMER                           
003800        05 MOD-KVANTMOT-ATTR PIC X(2).                                    
003900*                                 MFS ATTRIBUTFÄLT                        
004000        05 MOD-KVANTMOT      PIC X(6).                                    
004100*                                 ANTAL MOTTAGET                          
004200        05 MOD-KVSKROT-ATTR  PIC X(2).                                    
004300*                                 MFS ATTRIBUTFÄLT                        
004400        05 MOD-KVSKROT       PIC X(7).                                    
004500*                                 ANTAL SENASTE SKROTORDER                
004600        05 MOD-ADLAGOMR-ATTR PIC X(2).                                    
004700*                                 MFS ATTRIBUTFÄLT                        
004800        05 MOD-ADLAGOMR      PIC X(2).                                    
004900*                                 LAGEROMRÅDE                             
005000        05 MOD-ADGANG-ATTR   PIC X(2).                                    
005100*                                 MFS ATTRIBUTFÄLT                        
005200        05 MOD-ADGANG        PIC X(2).                                    
005300*                                 GÅNG                                    
005400        05 MOD-ADPLATS-ATTR  PIC X(2).                                    
005500*                                 MFS ATTRIBUTFÄLT                        
005600        05 MOD-ADPLATS       PIC X(5).                                    
005700*                                 LAGERPLATSNUMMER                        
005800     03 MOD-KOLLI-KLAR-ATTR  PIC X(2).                                    
005900*                                 MFS ATTRIBUTFÄLT                        
006000     03 MOD-KOLLI-KLAR       PIC X.                                       
006100     03 MOD-IDUSER-003-ATTR  PIC X(2).                                    
006200*                                 MFS ATTRIBUTFÄLT                        
006300     03 MOD-IDUSER-003       PIC X(5).                                    
006400*                                 ANSVARIGT USERID INLÄGGN.(R32)          
006500     03 MOD-TEMFSINF         PIC X(55).                                   
006600*                                 INFORMATIONSMEDDELANDE                  
006700*** END OF VILMAII-COPY LENGTH= 723 BYTES                                 
