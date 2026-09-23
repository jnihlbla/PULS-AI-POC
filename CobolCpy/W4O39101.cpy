000100 01  MOD-W4O39101.                                                        
000200*                                 COPYTEXT FÖR MOD W4O39101               
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-IDPRODNR-IN      PIC X(7).                                    
000800*                                 PRODUKTIONSNUMMER                       
000900     03 MOD-IDPRODNR-UT      PIC X(7).                                    
001000*                                 PRODUKTIONSNUMMER                       
001100     03 MOD-IDDISTR-UT       PIC X(4).                                    
001200*                                 DISTRIKTNUMMER                          
001300     03 MOD-IDKUNDNR-UT      PIC X(6).                                    
001400*                                 KUNDNUMMER                              
001500     03 MOD-KDFRAKT-UT       PIC X(2).                                    
001600*                                 FRAKTSÄTT DC TILL KUND                  
001700     03 MOD-IDORDNR-UT       PIC X(5).                                    
001800*                                 ORDERNUMMER                             
001900     03 MOD-KDORDKL-UT       PIC X.                                       
002000*                                 ORDERKLASS                              
002100     03 MOD-IDDC-UT          PIC X(2).                                    
002200*                                 IDENTIFIERARE LAGER                     
002300     03 MOD-PRTVAL-ADRESSFL  PIC X(2).                                    
002400*                                 PRINTER-VAL KOD                         
002500     03 MOD-IDRADNR-SENAST   PIC X(4).                                    
002600*                                 RADNUMMER                               
002700     03 MOD-RAD              OCCURS 13 TIMES.                             
002800*                                 COPYTEXT FÖR MOD W4O39101               
002900        05 MOD-IDRADNR-ATTR  PIC X(2).                                    
003000*                                 MFS ATTRIBUTFÄLT                        
003100        05 MOD-IDRADNR       PIC X(2).                                    
003200*                                 MFS BEHANDLING AV INPUTFÄLT             
003300        05 MOD-KVLEVART-ATTR PIC X(2).                                    
003400*                                 MFS ATTRIBUTFÄLT                        
003500        05 MOD-KVLEVART      PIC X(2).                                    
003600*                                 MFS BEHANDLING AV INPUTFÄLT             
003700     03 MOD-TEMFSINF         PIC X(55).                                   
003800*                                 INFORMATIONSMEDDELANDE                  
003900*** END OF VILMAII-COPY LENGTH= 243 BYTES                                 
