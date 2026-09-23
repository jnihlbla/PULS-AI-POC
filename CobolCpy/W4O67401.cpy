000100 01  W4O67401.                                                            
000200*                                 COPYTEXT FÖR MID W4O67401               
000300*                                                                         
000400     03 TRANS-NUMMER         PIC X(4).                                    
000500*                                 BILDNUMMER                              
000600     03 TEMFSFEL             PIC X(40).                                   
000700*                                 MFS FELMEDDELANDE                       
000800     03 IDTRPTNR             PIC X(3).                                    
000900*                                 TRANSPORTIDENTITET                      
001000     03 IDLBBET-HUV          PIC X(12).                                   
001100*                                 LASTBÄRARBETECKNING                     
001200     03 IDDC                 PIC X(2).                                    
001300*                                 IDENTIFIERARE LAGER                     
001400     03 IDSHIPM              PIC Z(7).                                    
001500*                                 SKEPPNINGSNUMMER                        
001600     03 FRAN-IDTRANS         PIC X(4).                                    
001700*                                 BILDNUMMER                              
001800     03 BEROUTE-ATTR         PIC X(2).                                    
001900*                                 MFS ATTRIBUTFÄLT                        
002000     03 BEROUTE              PIC X(25).                                   
002100*                                 FÄRDVÄG, DESTINATION                    
002200     03 IDBOKN-ATTR          PIC X(2).                                    
002300*                                 MFS ATTRIBUTFÄLT                        
002400     03 IDBOKN               PIC X(15).                                   
002500*                                 BOKNINGSNUMMER                          
002600     03 IDTRANSP-NAMN-ATTR   PIC X(2).                                    
002700*                                 MFS ATTRIBUTFÄLT                        
002800     03 IDTRANSP-NAMN        PIC X(15).                                   
002900*                                 TRANSPORTMEDEL NAMN                     
003000     03 GRP.                                                              
003100        05 TIAVGANG-ATTR     PIC X(2).                                    
003200*                                 MFS ATTRIBUTFÄLT                        
003300        05 TIAVGANG          PIC X(2).                                    
003400*                                 MFS BEHANDLING AV INPUTFÄLT             
003500        05 BETEXT-NEDK-ATTR  PIC X(2).                                    
003600*                                 MFS ATTRIBUTFÄLT                        
003700        05 BETEXT-NEDK       PIC X(2).                                    
003800*                                 MFS BEHANDLING AV INPUTFÄLT             
003900        05 TINEDK-ATTR       PIC X(2).                                    
004000*                                 MFS ATTRIBUTFÄLT                        
004100        05 TINEDK            PIC X(2).                                    
004200*                                 MFS BEHANDLING AV INPUTFÄLT             
004300        05 BETEXT-HAEMT-ATTR PIC X(2).                                    
004400*                                 MFS ATTRIBUTFÄLT                        
004500        05 BETEXT-HAEMT      PIC X(2).                                    
004600*                                 MFS BEHANDLING AV INPUTFÄLT             
004700        05 TIHAEMT-ATTR      PIC X(2).                                    
004800*                                 MFS ATTRIBUTFÄLT                        
004900        05 TIHAEMT           PIC X(2).                                    
005000*                                 MFS BEHANDLING AV INPUTFÄLT             
005100        05 LB-GRP            OCCURS 6 TIMES.                              
005200           07 IDLBTYP-ATTR   PIC X(2).                                    
005300*                                 MFS ATTRIBUTFÄLT                        
005400           07 IDLBTYP        PIC X(2).                                    
005500*                                 MFS BEHANDLING AV INPUTFÄLT             
005600           07 IDLBBET-ATTR   PIC X(2).                                    
005700*                                 MFS ATTRIBUTFÄLT                        
005800           07 IDLBBET        PIC X(2).                                    
005900*                                 MFS BEHANDLING AV INPUTFÄLT             
006000        05 OEVR-GRP          OCCURS 3 TIMES.                              
006100           07 BETEXT-OEVR-ATTR                                            
006200                             PIC X(2).                                    
006300*                                 MFS ATTRIBUTFÄLT                        
006400           07 BETEXT-OEVR    PIC X(2).                                    
006500*                                 MFS BEHANDLING AV INPUTFÄLT             
006600        05 TRDOK-GRP         OCCURS 3 TIMES.                              
006700           07 FLTRDOK-ATTR   PIC X(2).                                    
006800*                                 MFS ATTRIBUTFÄLT                        
006900           07 FLTRDOK        PIC X(2).                                    
007000*                                 MFS BEHANDLING AV INPUTFÄLT             
007100     03 FILLER REDEFINES GRP.                                             
007200        05 FILLER            OCCURS 23 TIMES.                             
007300           07 ATTRIB         PIC X(2).                                    
007400*                                 MFS ATTRIBUTFÄLT                        
007500           07 FAELT          PIC X(2).                                    
007600*                                 MFS BEHANDLING AV INPUTFÄLT             
007700     03 TEMFSINF             PIC X(55).                                   
007800*                                 INFORMATIONSMEDDELANDE                  
007900*** END OF VILMAII-COPY LENGTH= 280 BYTES                                 
