000100 01  MOD-W6O12401.                                                        
000200*                                 COPYTEXT FOR MOD W6O12401               
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-IDINLVGN-IN      PIC X(3).                                    
000800*                                 VAGNSIDENTITET                          
000900     03 MOD-ADINLOMR-IN      PIC X(4).                                    
001000*                                 INLEVERANSOMRÅDE                        
001100     03 MOD-ADINLOMR-NXT-IN  PIC X(4).                                    
001200*                                 INLEVERANSOMRÅDE NÄSTA                  
001300     03 MOD-IDDC-IN          PIC X(2).                                    
001400*                                 IDENTIFIERARE LAGER                     
001500     03 MOD-IDINLVGN-UT      PIC X(3).                                    
001600*                                 VAGNSIDENTITET                          
001700     03 MOD-ADINLOMR-UT      PIC X(4).                                    
001800*                                 INLEVERANSOMRÅDE                        
001900     03 MOD-ADINLOMR-NXT-UT  PIC X(4).                                    
002000*                                 INLEVERANSOMRÅDE NÄSTA                  
002100     03 MOD-IDDC-UT          PIC X(2).                                    
002200*                                 IDENTIFIERARE LAGER                     
002300     03 MOD-SPAR-IDINLVGN    PIC 9(3).                                    
002400*                                 VAGNSIDENTITET                          
002500     03 MOD-SPAR-ADINLOMR    PIC X(4).                                    
002600*                                 INLEVERANSOMRÅDE                        
002700     03 MOD-SPAR-ADINLOMR-NXT                                             
002800                             PIC X(4).                                    
002900*                                 INLEVERANSOMRÅDE NÄSTA                  
003000     03 MOD-FLPRIO-ATTR      PIC X(2).                                    
003100*                                 MFS ATTRIBUTFÄLT                        
003200     03 MOD-FLPRIO           PIC X.                                       
003300*                                 PRIORITERAD                             
003400     03 MOD-FLSATS-ATTR      PIC X(2).                                    
003500*                                 MFS ATTRIBUTFÄLT                        
003600     03 MOD-FLSATS           PIC X.                                       
003700*                                 SATSARTIKEL                             
003800     03 MOD-RAD              OCCURS 12 TIMES.                             
003900*                                 LINES                                   
004000        05 MOD-IDARTNR-ATTR  PIC X(2).                                    
004100*                                 MFS ATTRIBUTFÄLT                        
004200        05 MOD-IDARTNR       PIC X(9).                                    
004300*                                 ARTIKELNUMMER                           
004400        05 MOD-KVINLART-ATTR PIC X(2).                                    
004500*                                 MFS ATTRIBUTFÄLT                        
004600        05 MOD-KVINLART      PIC X(6).                                    
004700*                                 ANTAL I PARTIRAD                        
004800        05 MOD-IDLEVNR-ATTR  PIC X(2).                                    
004900*                                 MFS ATTRIBUTFÄLT                        
005000        05 MOD-IDLEVNR       PIC X(5).                                    
005100*                                 LEVERANTÖRNUMMER                        
005200        05 MOD-IDOKOLLI-ATTR PIC X(2).                                    
005300*                                 MFS ATTRIBUTFÄLT                        
005400        05 MOD-IDOKOLLI      PIC X(9).                                    
005500*                                 ODETTE KOLLINUMMER                      
005600        05 MOD-TEMFSMED      PIC X(20).                                   
005700*                                 INFO-MEDDELANDE FÖR FÄLT/RAD            
005800     03 MOD-TEMFSINF         PIC X(55).                                   
005900*                                 INFORMATIONSMEDDELANDE                  
