000100 01  MOD-W4O39501.                                                        
000200*                                 COPYTEXT FÖR MOD W4O39501               
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
001700     03 MOD-PRTVAL-ADRESSFL  PIC X(2).                                    
001800*                                 PRINTER-VAL KOD                         
001900     03 MOD-IDORDNR-UT       PIC X(5).                                    
002000*                                 ORDERNUMMER UTGÅR PD90                  
002100     03 MOD-KDORDKL-UT       PIC X.                                       
002200*                                 ORDERKLASS                              
002300     03 MOD-IDDC-UT          PIC X(2).                                    
002400*                                 IDENTIFIERARE LAGER                     
002500     03 MOD-FLMAN-RAPP       PIC X.                                       
002600*                                 ALLMÄN FLAGGA                           
002700     03 MOD-FLSIDA1          PIC X.                                       
002800*                                 ALLMÄN FLAGGA                           
002900     03 MOD-IDRADNR-SENAST   PIC Z(3)9.                                   
003000*                                 RADNUMMER                               
003100     03 MOD-SUM              PIC 9(6).                                    
003200*                                 LEVERERAT ANTAL STYCK                   
003300     03 MOD-IDRADNR-ATTR     PIC X(2).                                    
003400*                                 MFS ATTRIBUTFÄLT                        
003500     03 MOD-IDRADNR          PIC X(4).                                    
003600*                                 RADNUMMER                               
003700     03 MOD-KVLEVART         PIC 9(6).                                    
003800*                                 LEVERERAT ANTAL STYCK                   
003900     03 MOD-KVLEVART-DELAT1-ATTR                                          
004000                             PIC X(2).                                    
004100*                                 MFS ATTRIBUTFÄLT                        
004200     03 MOD-KVLEVART-DELAT1  PIC X(2).                                    
004300*                                 MFS BEHANDLING AV INPUTFÄLT             
004400     03 MOD-IDKOLLI-FOM-ATTR PIC X(2).                                    
004500*                                 MFS ATTRIBUTFÄLT                        
004600     03 MOD-IDKOLLI-FOM      PIC X(2).                                    
004700*                                 MFS BEHANDLING AV INPUTFÄLT             
004800     03 MOD-IDKOLLI-TOM-ATTR PIC X(2).                                    
004900*                                 MFS ATTRIBUTFÄLT                        
005000     03 MOD-IDKOLLI-TOM      PIC X(2).                                    
005100*                                 MFS BEHANDLING AV INPUTFÄLT             
005200     03 MOD-RAD              OCCURS 13 TIMES.                             
005300*                                 COPYTEXT FÖR MOD W4O39501               
005400        05 MOD-KVLEVART-DELAT2-ATTR                                       
005500                             PIC X(2).                                    
005600*                                 MFS ATTRIBUTFÄLT                        
005700        05 MOD-KVLEVART-DELAT2                                            
005800                             PIC X(2).                                    
005900*                                 MFS BEHANDLING AV INPUTFÄLT             
006000        05 MOD-IDKOLLI-ATTR  PIC X(2).                                    
006100*                                 MFS ATTRIBUTFÄLT                        
006200        05 MOD-IDKOLLI       PIC X(2).                                    
006300*                                 MFS BEHANDLING AV INPUTFÄLT             
006400     03 MOD-TEMFSINF         PIC X(55).                                   
006500*                                 INFORMATIONSMEDDELANDE                  
006600*** END OF VILMAII-COPY LENGTH= 275 BYTES                                 
