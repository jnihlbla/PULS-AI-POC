000100 01  MOD-W4O27101.                                                        
000200*                                 MOD-COPYTEXT FÖR W4027100               
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-IDROLL-IN        PIC X(2).                                    
000800*                                 MFS BEHANDLING AV INPUTFÄLT             
000900     03 MOD-IDROLL           PIC X(5).                                    
001000*                                 VOR ROLL ID                             
001100     03 MOD-RAD              OCCURS 24 TIMES                              
001200                             INDEXED MOD-IX.                              
001300*                                                                         
001400        05 MOD-IDDISTR-FOM   PIC Z(3)9.                                   
001500*                                 LÄGSTA DISTRIKTNR I INTERVALL           
001600        05 MOD-IDDISTR-TOM   PIC Z(3)9.                                   
001700*                                 HÖGSTA DISTRIKTNR I INTERVALL           
002200     03 MOD-KDBEHX-ATTR      PIC X(2).                                    
002300*                                 MFS ATTRIBUTFÄLT                        
002400     03 MOD-KDBEHX           PIC X.                                       
002500*                                 BEHANDLINGSKOD-X                        
002600     03 MOD-IDDISTR-FOM-ATTR PIC X(2).                                    
002700*                                 MFS ATTRIBUTFÄLT                        
002800     03 MOD-IDDISTR-FOM-UPD  PIC Z(3)9.                                   
002900*                                 LÄGSTA DISTRIKTNR I INTERVALL           
003000     03 MOD-IDDISTR-TOM-ATTR PIC X(2).                                    
003100*                                 MFS ATTRIBUTFÄLT                        
003200     03 MOD-IDDISTR-TOM-UPD  PIC Z(3)9.                                   
003300*                                 HÖGSTA DISTRIKTNR I INTERVALL           
004200     03 MOD-TEMFSINF         PIC X(55).                                   
004300*                                 INFORMATIONSMEDDELANDE                  
004400*** END OF VILMAII-COPY LENGTH= 467 BYTES                                 
