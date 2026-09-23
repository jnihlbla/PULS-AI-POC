000100 01  MOD-W4O33801.                                                        
000200*                                 MOD-COPYTEXT FÖR W4033800               
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-IDDISTR-IN       PIC X(2).                                    
000800*                                 MFS BEHANDLING AV INPUTFÄLT             
000900     03 MOD-IDDISTR-UT       PIC X(4).                                    
001000*                                 DISTRIKTNUMMER                          
001100     03 MOD-IDKUNDNR-IN      PIC X(2).                                    
001200*                                 MFS BEHANDLING AV INPUTFÄLT             
001300     03 MOD-IDKUNDNR-UT      PIC X(6).                                    
001400*                                 KUNDNUMMER                              
001500     03 MOD-IDORDNR-IN       PIC X(2).                                    
001600*                                 MFS BEHANDLING AV INPUTFÄLT             
001700     03 MOD-IDORDNR-UT       PIC X(5).                                    
001800*                                 ORDERNUMMER UTGÅR PD90                  
001900     03 MOD-IDKOLLI-IN       PIC X(2).                                    
002000*                                 MFS BEHANDLING AV INPUTFÄLT             
002100     03 MOD-IDKOLLI-UT       PIC X(5).                                    
002200*                                 KOLLINUMMER                             
002300     03 MOD-IDDC-IN          PIC X(2).                                    
002400*                                 MFS BEHANDLING AV INPUTFÄLT             
002500     03 MOD-IDDC-UT          PIC X(2).                                    
002600*                                 IDENTIFIERARE LAGER                     
002700     03 MOD-IDPRODNR-IN      PIC X(2).                                    
002800*                                 MFS BEHANDLING AV INPUTFÄLT             
002900     03 MOD-IDPRODNR-UT      PIC 9(7).                                    
003000*                                 PRODUKTIONSNUMMER                       
003100     03 MOD-KDPRTVAL-AF-ATTR PIC X(2).                                    
003200*                                 MFS ATTRIBUTFÄLT                        
003300     03 MOD-KDPRTVAL-AF      PIC X(2).                                    
003400*                                 PRINTER-VAL KOD                         
003500     03 MOD-KDPRTVAL-FS-ATTR PIC X(2).                                    
003600*                                 MFS ATTRIBUTFÄLT                        
003700     03 MOD-KDPRTVAL-FS      PIC X(2).                                    
003800*                                 PRINTER-VAL KOD                         
003900     03 MOD-KDKOLLI-ATTR     PIC X(2).                                    
004000*                                 MFS ATTRIBUTFÄLT                        
004100     03 MOD-KDKOLLI          PIC X(8).                                    
004200*                                 KOLLIKOD                                
004300     03 MOD-KDEMBTYP-ATTR    PIC X(2).                                    
004400*                                 MFS ATTRIBUTFÄLT                        
004500     03 MOD-KDEMBTYP         PIC 9.                                       
004600*                                 EMBALLAGETYP                            
004700     03 MOD-DIKOLLIL-ATTR    PIC X(2).                                    
004800*                                 MFS ATTRIBUTFÄLT                        
004900     03 MOD-DIKOLLIL         PIC Z(3)9.                                   
005000*                                 KOLLI-LÄNGD                             
005100     03 MOD-DIKOLLIB-ATTR    PIC X(2).                                    
005200*                                 MFS ATTRIBUTFÄLT                        
005300     03 MOD-DIKOLLIB         PIC Z(2)9.                                   
005400*                                 KOLLI-BREDD                             
005500     03 MOD-DIKOLLIH-ATTR    PIC X(2).                                    
005600*                                 MFS ATTRIBUTFÄLT                        
005700     03 MOD-DIKOLLIH         PIC Z(2)9.                                   
005800*                                 KOLLI-HÖJD                              
005900     03 MOD-TEMFSINF         PIC X(55).                                   
006000*                                 INFORMATIONSMEDDELANDE                  
006100*** END OF VILMAII-COPY LENGTH= 177 BYTES                                 
