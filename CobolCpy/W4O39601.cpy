000100 01  MOD-W4O39601.                                                        
000200*                                 COPYTEXT FÖR MOD W4O39601               
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
001800*                                 ORDERNUMMER UTGÅR PD90                  
001900     03 MOD-KDORDKL-UT       PIC X.                                       
002000*                                 ORDERKLASS                              
002100     03 MOD-IDDC-UT          PIC X(2).                                    
002200*                                 IDENTIFIERARE LAGER                     
002300     03 MOD-PRTVAL-ADRESSFL  PIC X(2).                                    
002400*                                 PRINTER-VAL KOD                         
002500     03 MOD-IDKOLLI-SENAST   PIC Z(4)9.                                   
002600*                                 KOLLINUMMER                             
002700     03 MOD-RAD              OCCURS 13 TIMES.                             
002800*                                 COPYTEXT FÖR MOD W4O39601               
002900        05 MOD-IDKOLLI       PIC 9(5).                                    
003000*                                 KOLLINUMMER                             
003100        05 MOD-KDKOLLI-ATTR  PIC X(2).                                    
003200*                                 MFS ATTRIBUTFÄLT                        
003300        05 MOD-KDKOLLI       PIC X(2).                                    
003400*                                 MFS BEHANDLING AV INPUTFÄLT             
003500        05 MOD-VKORDBTO-KOLLI-ATTR                                        
003600                             PIC X(2).                                    
003700*                                 MFS ATTRIBUTFÄLT                        
003800        05 MOD-VKORDBTO-KOLLI                                             
003900                             PIC X(2).                                    
004000*                                 MFS BEHANDLING AV INPUTFÄLT             
004100        05 MOD-ADRUTHYL-ATTR PIC X(2).                                    
004200*                                 MFS ATTRIBUTFÄLT                        
004300        05 MOD-ADRUTHYL      PIC X(2).                                    
004400*                                 MFS BEHANDLING AV INPUTFÄLT             
004500        05 MOD-ADFLGEO-ATTR  PIC X(2).                                    
004600*                                 MFS ATTRIBUTFÄLT                        
004700        05 MOD-ADFLGEO       PIC X(2).                                    
004800*                                 MFS BEHANDLING AV INPUTFÄLT             
004900        05 MOD-KDEMBTYP-ATTR PIC X(2).                                    
005000*                                 MFS ATTRIBUTFÄLT                        
005100        05 MOD-KDEMBTYP      PIC X(2).                                    
005200*                                 MFS BEHANDLING AV INPUTFÄLT             
005300        05 MOD-DIKOLLIL-ATTR PIC X(2).                                    
005400*                                 MFS ATTRIBUTFÄLT                        
005500        05 MOD-DIKOLLIL      PIC X(2).                                    
005600*                                 MFS BEHANDLING AV INPUTFÄLT             
005700        05 MOD-DIKOLLIB-ATTR PIC X(2).                                    
005800*                                 MFS ATTRIBUTFÄLT                        
005900        05 MOD-DIKOLLIB      PIC X(2).                                    
006000*                                 MFS BEHANDLING AV INPUTFÄLT             
006100        05 MOD-DIKOLLIH-ATTR PIC X(2).                                    
006200*                                 MFS ATTRIBUTFÄLT                        
006300        05 MOD-DIKOLLIH      PIC X(2).                                    
006400*                                 MFS BEHANDLING AV INPUTFÄLT             
006500     03 MOD-TEMFSINF         PIC X(55).                                   
006600*                                 INFORMATIONSMEDDELANDE                  
006700*** END OF VILMAII-COPY LENGTH= 621 BYTES                                 
