000100 01  MOD-W5O10101.                                                        
000200*                                 MOD-COPYTEXT FÖR W5015100               
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-FROM-IDARTNR-IN  PIC Z(9).                                    
000800*                                 ARTIKELNUMMER                           
000900     03 MOD-FROM-IDARTNR-UT  PIC Z(9).                                    
001000*                                 ARTIKELNUMMER                           
001100     03 MOD-TO-IDARTNR-IN    PIC Z(9).                                    
001200*                                 ARTIKELNUMMER                           
001300     03 MOD-TO-IDARTNR-UT    PIC Z(9).                                    
001400*                                 ARTIKELNUMMER                           
001500     03 MOD-IDDC-IN          PIC X(2).                                    
001600*                                 IDENTIFIERARE LAGER                     
001700     03 MOD-IDDC-UT          PIC X(2).                                    
001800*                                 IDENTIFIERARE LAGER                     
001900     03 MOD-FROM-BEART       PIC X(25).                                   
002000*                                 ARTIKELBENÄMNING                        
002100     03 MOD-TO-BEART         PIC X(25).                                   
002200*                                 ARTIKELBENÄMNING                        
002300     03 MOD-FROM-KVLS-BEFORE PIC Z(6)9.                                   
002400*                                 LAGERSALDO                              
002500     03 MOD-TO-KVLS-BEFORE   PIC -(6)9.                                   
002600*                                 LAGERSALDO                              
002700     03 MOD-FROM-KVLS-AFTER  PIC Z(6)9.                                   
002800*                                 LAGERSALDO                              
002900     03 MOD-TO-KVLS-AFTER    PIC -(6)9.                                   
003000*                                 LAGERSALDO                              
003100     03 MOD-KVANTAL-ATTR     PIC X(2).                                    
003200*                                 MFS ATTRIBUTFÄLT                        
003300     03 MOD-KVANTAL          PIC Z(6).                                    
003400*                                 DATAELEMENT                             
003500     03 MOD-IDAVINR-ATTR     PIC X(2).                                    
003600*                                 MFS ATTRIBUTFÄLT                        
003700     03 MOD-IDAVINR          PIC Z(6).                                    
003800*                                 AVI-NUMMER                              
003900     03 MOD-IDKONTO-ATTR     PIC X(2).                                    
004000*                                 MFS ATTRIBUTFÄLT                        
004100     03 MOD-IDKONTO          PIC Z(10).                                   
004200*                                 KONTO                                   
004300     03 MOD-IDANALYS-ATTR    PIC X(2).                                    
004400*                                 MFS ATTRIBUTFÄLT                        
004500     03 MOD-IDANALYS         PIC Z(12).                                   
004600*                                 ANALYSNUMMER                            
004700     03 MOD-TEMFSINF         PIC X(55).                                   
004800*                                 INFORMATIONSMEDDELANDE                  
004900*** END OF VILMAII-COPY LENGTH= 259 BYTES                                 
