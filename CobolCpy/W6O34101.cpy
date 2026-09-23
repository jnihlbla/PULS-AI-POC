000100 01  MOD-W6O34101.                                                        
000200*                                 COPYTEXT FÖR MOD                        
000300*                                 W6O34101                                
000400     03 MOD-IDTRANS          PIC X(4).                                    
000500*                                 BILDNUMMER                              
000600     03 MOD-TEMFSFEL         PIC X(40).                                   
000700*                                 MFS FELMEDDELANDE                       
000800     03 MOD-IDDC-IN          PIC X(2).                                    
000900*                                 IDENTIFIERARE LAGER                     
001000     03 MOD-IDDC-UT          PIC X(2).                                    
001100*                                 IDENTIFIERARE LAGER                     
001200     03 MOD-KDFREQ-IN        PIC X(2).                                    
001300*                                 FREQUENCY CODE                          
001400     03 MOD-KDFREQ-UT        PIC X(2).                                    
001500*                                 FREQUENCY CODE                          
001600     03 MOD-KDFREQ-SPAR      PIC X(2).                                    
001700*                                 FREQUENCY CODE                          
001800     03 MOD-KDFREQ-ATTR      PIC X(2).                                    
001900*                                 MFS ATTRIBUTFÄLT                        
002000     03 MOD-KDFREQ-MAIN      PIC X(2).                                    
002100*                                 MFS BEHANDLING AV INPUTFÄLT             
002200     03 MOD-TEFREQ-ATTR      PIC X(2).                                    
002300*                                 MFS ATTRIBUTFÄLT                        
002400     03 MOD-TEFREQ-MAIN      PIC X(2).                                    
002500*                                 MFS BEHANDLING AV INPUTFÄLT             
002600     03 MOD-KVPB-FOM-ATTR    PIC X(2).                                    
002700*                                 MFS ATTRIBUTFÄLT                        
002800     03 MOD-KVPB-FOM-MAIN    PIC X(2).                                    
002900*                                 MFS BEHANDLING AV INPUTFÄLT             
003000     03 MOD-KVPB-TOM-ATTR    PIC X(2).                                    
003100*                                 MFS ATTRIBUTFÄLT                        
003200     03 MOD-KVPB-TOM-MAIN    PIC X(2).                                    
003300*                                 MFS BEHANDLING AV INPUTFÄLT             
003400     03 MOD-RELOCFAC-ATTR    PIC X(2).                                    
003500*                                 MFS ATTRIBUTFÄLT                        
003600     03 MOD-RELOCFAC-MAIN    PIC X(2).                                    
003700*                                 MFS BEHANDLING AV INPUTFÄLT             
003800     03 MOD-KDANDR-ATTR      PIC X(2).                                    
003900*                                 MFS ATTRIBUTFÄLT                        
004000     03 MOD-KDANDR-MAIN      PIC X(2).                                    
004100*                                 MFS BEHANDLING AV INPUTFÄLT             
004200     03 MOD-INDATA           OCCURS 13 TIMES.                             
004300*                                 FREQUENCY TABEL INFO                    
004400        05 MOD-KDFREQ-LINE   PIC X(2).                                    
004500*                                 FREQUENCY CODE                          
004600        05 MOD-TEFREQ-LINE   PIC X(10).                                   
004700*                                 FREQUENCY TYPE INFORMATION              
004800        05 MOD-KVPB-FOM-LINE PIC Z(5)9.9.                                 
004900*                                 PERIODBEHOV FOM (PROGNOS)               
005000        05 MOD-KVPB-TOM-LINE PIC Z(5)9.9.                                 
005100*                                 PERIODBEHOV TOM (PROGNOS)               
005200        05 MOD-RELOCFAC-LINE PIC 9.9(2).                                  
005300*                                 RELOCTION FACTOR                        
005400     03 MOD-TEMFSINF         PIC X(55).                                   
005500*                                 INFORMATIONSMEDDELANDE                  
005600*** END OF VILMAII-COPY LENGTH= 549 BYTES                                 
