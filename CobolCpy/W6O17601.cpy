000100 01  MOD-W6O17601.                                                        
000200*                                 MOD-COPYTEXT FÖR BILD 6176              
000300*                                 PRIME COUNT SELECTION                   
000400     03 MOD-IDTRANS          PIC X(4).                                    
000500*                                 BILDNUMMER                              
000600     03 MOD-TEMFSFEL         PIC X(40).                                   
000700*                                 MFS FELMEDDELANDE                       
000800     03 MOD-IDLEVNR-IN       PIC X(2).                                    
000900*                                 MFS BEHANDLING AV INPUTFÄLT             
001000     03 MOD-IDLEVNR-UT       PIC X(5).                                    
001100*                                 LEVERANTÖRNUMMER                        
001200     03 MOD-TISUPREF-IN      PIC X(2).                                    
001300*                                 MFS BEHANDLING AV INPUTFÄLT             
001400     03 MOD-TISUPREF-UT      PIC X(6).                                    
001500*                                 ÅR - MÅNAD - DAG  (ÅÅMMDD)              
001600     03 MOD-IDSUPREF-IN      PIC X(2).                                    
001700*                                 MFS BEHANDLING AV INPUTFÄLT             
001800     03 MOD-IDSUPREF-UT      PIC X(10).                                   
001900*                                 LEVERANTöRSREF.                         
002000     03 MOD-IDDISTR-IN       PIC X(2).                                    
002100*                                 MFS BEHANDLING AV INPUTFÄLT             
002200     03 MOD-IDDISTR-UT       PIC X(4).                                    
002300*                                 DISTRIKTNUMMER                          
002400     03 MOD-KVKOLLI-NOT-RECIVED                                           
002500                             PIC Z(4).                                    
002600*                                 ANTAL KOLLI                             
002700     03 MOD-KVKOLLI-RECIVED  PIC Z(4).                                    
002800*                                 ANTAL KOLLI                             
002900     03 MOD-UPDRAD           OCCURS 26 TIMES.                             
003000*                                                                         
003100        05 MOD-IDDISTR-UPD-ATTR                                           
003200                             PIC X(2).                                    
003300*                                 MFS ATTRIBUTFÄLT                        
003400        05 MOD-IDDISTR-UPD   PIC X(4).                                    
003500*                                 DISTRIKTNUMMER                          
003600        05 MOD-IDKUNDNR-UPD-ATTR                                          
003700                             PIC X(2).                                    
003800*                                 MFS ATTRIBUTFÄLT                        
003900        05 MOD-IDKUNDNR-UPD  PIC X(6).                                    
004000*                                 KUNDNUMMER                              
004100        05 MOD-IDORDNR7-UPD-ATTR                                          
004200                             PIC X(2).                                    
004300*                                 MFS ATTRIBUTFÄLT                        
004400        05 MOD-IDORDNR7-UPD  PIC X(7).                                    
004500*                                 ORDERNUMMER                             
004600        05 MOD-IDKOLLI-UPD-ATTR                                           
004700                             PIC X(2).                                    
004800*                                 MFS ATTRIBUTFÄLT                        
004900        05 MOD-IDKOLLI-UPD   PIC X(5).                                    
005000*                                 KOLLINUMMER                             
005100        05 MOD-NOTIFICATION  PIC X(4).                                    
005200     03 MOD-TEMFSINF         PIC X(55).                                   
005300*                                 INFORMATIONSMEDDELANDE                  
005400*** END OF VILMAII-COPY LENGTH= 1024 BYTES                                
