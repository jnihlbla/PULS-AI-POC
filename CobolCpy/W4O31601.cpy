000100 01  MOD-W4O31601.                                                        
000200*                                 MOD-COPYTEXT PGM W40316                 
000300*                                 KOLLI RÄTTNING                          
000400     03 MOD-IDTRANS          PIC X(4).                                    
000500*                                 BILDNUMMER                              
000600     03 MOD-TEMFSFEL         PIC X(40).                                   
000700*                                 MFS FELMEDDELANDE                       
000800     03 MOD-IDANSTNR-IN      PIC X(2).                                    
000900*                                 MFS BEHANDLING AV INPUTFÄLT             
001000     03 MOD-IDANSTNR-UT      PIC X(5).                                    
001100*                                 ANSTÄLLNINGSNUMMER                      
001200     03 MOD-IDDISTR-IN       PIC X(2).                                    
001300*                                 MFS BEHANDLING AV INPUTFÄLT             
001400     03 MOD-IDDISTR-UT       PIC X(4).                                    
001500*                                 DISTRIKTNUMMER                          
001600     03 MOD-IDKUNDNR-IN      PIC X(2).                                    
001700*                                 MFS BEHANDLING AV INPUTFÄLT             
001800     03 MOD-IDKUNDNR-UT      PIC X(6).                                    
001900*                                 KUNDNUMMER                              
002000     03 MOD-IDORDNR-IN       PIC X(2).                                    
002100*                                 MFS BEHANDLING AV INPUTFÄLT             
002200     03 MOD-IDORDNR-UT       PIC X(5).                                    
002300*                                 ORDERNUMMER                             
002400     03 MOD-IDKOLLI-IN       PIC X(2).                                    
002500*                                 MFS BEHANDLING AV INPUTFÄLT             
002600     03 MOD-IDKOLLI-UT       PIC X(5).                                    
002700*                                 KOLLINUMMER                             
002800     03 MOD-IDPRODNR-IN      PIC X(2).                                    
002900*                                 MFS BEHANDLING AV INPUTFÄLT             
003000     03 MOD-IDPRODNR-UT      PIC X(7).                                    
003100*                                 PRODUKTIONSNUMMER                       
003200     03 MOD-IDDC-IN          PIC X(2).                                    
003300*                                 MFS BEHANDLING AV INPUTFÄLT             
003400     03 MOD-IDDC-UT          PIC X(2).                                    
003500*                                 IDENTIFIERARE LAGER                     
003600     03 MOD-IDRADNR-SPAR     PIC 9(4).                                    
003700*                                 RADNUMMER                               
003800     03 MOD-KVORDRAD-SPAR    PIC X(5).                                    
003900*                                 ANTAL ORDERRADER                        
004000     03 MOD-IDRADNR-START-ATTR                                            
004100                             PIC X(2).                                    
004200*                                 MFS ATTRIBUTFÄLT                        
004300     03 MOD-IDRADNR-START    PIC X(2).                                    
004400*                                 MFS BEHANDLING AV INPUTFÄLT             
004500     03 MOD-FLBACKA-ALLA-ATTR                                             
004600                             PIC X(2).                                    
004700*                                 MFS ATTRIBUTFÄLT                        
004800     03 MOD-FLBACKA-ALLA     PIC X.                                       
004900*                                 ALLMÄN SVARSFLAGGA                      
005000     03 MOD-RAD              OCCURS 13 TIMES.                             
005100        05 MOD-IDRADNR       PIC X(4).                                    
005200*                                 RADNUMMER                               
005300        05 MOD-KVLEVART      PIC X(6).                                    
005400*                                 LEVERERAT ANTAL STYCK                   
005500        05 MOD-FLBACKA-ATTR  PIC X(2).                                    
005600*                                 MFS ATTRIBUTFÄLT                        
005700        05 MOD-FLBACKA       PIC X.                                       
005800*                                 ALLMÄN SVARSFLAGGA                      
005900     03 MOD-TEMFSINF         PIC X(55).                                   
006000*                                 INFORMATIONSMEDDELANDE                  
006100*** END OF VILMAII-COPY LENGTH= 332 BYTES                                 
