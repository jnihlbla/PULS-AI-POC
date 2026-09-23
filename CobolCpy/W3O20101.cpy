000100 01  MOD-W3O20101.                                                        
000200*                                 MOD-COPYTEXT FÖR W3020100               
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-IDFSGURV-IN      PIC X(2).                                    
000800*                                 MFS BEHANDLING AV INPUTFÄLT             
000900     03 MOD-IDUSER-IN        PIC X(2).                                    
001000*                                 MFS BEHANDLING AV INPUTFÄLT             
001100     03 MOD-IDFSGURV-UT      PIC X(8).                                    
001200*                                 URVALS IDENTITET                        
001300     03 MOD-IDUSER-UT        PIC X(8).                                    
001400*                                 ANVÄNDARENS SÄKERHETS ID                
001500     03 MOD-IDTRANS-LO       PIC X(4).                                    
001600*                                 BILDNUMMER                              
001700     03 MOD-IDTRANS-HI       PIC X(4).                                    
001800*                                 BILDNUMMER                              
001900     03 MOD-DAREGDAT-LO      PIC 9(8).                                    
002000*                                 REGISTRERINGSDATUM (ÅÅÅÅMMDD)           
002100     03 MOD-DAREGDAT-HI      PIC 9(8).                                    
002200*                                 REGISTRERINGSDATUM (ÅÅÅÅMMDD)           
002300     03 MOD-TIREGTID-LO      PIC 9(6).                                    
002400*                                 REGISTRERINGSTID                        
002500     03 MOD-TIREGTID-HI      PIC 9(6).                                    
002600*                                 REGISTRERINGSTID                        
002700     03 MOD-IDFSGURV-LO      PIC X(8).                                    
002800*                                 URVALS IDENTITET                        
002900     03 MOD-IDFSGURV-HI      PIC X(8).                                    
003000*                                 URVALS IDENTITET                        
003100     03 MOD-INFO-RAD         OCCURS 28 TIMES.                             
003200*                                 RADINFORMATION                          
003300        05 MOD-SELECT-URVAL-ATTR                                          
003400                             PIC X(2).                                    
003500*                                 MFS ATTRIBUTFÄLT                        
003600        05 MOD-SELECT-URVAL  PIC X(2).                                    
003700*                                 MFS BEHANDLING AV INPUTFÄLT             
003800        05 MOD-IDFSGURV-VISA-ATTR                                         
003900                             PIC X(2).                                    
004000*                                 MFS ATTRIBUTFÄLT                        
004100        05 MOD-IDFSGURV-VISA PIC X(8).                                    
004200*                                 URVALS IDENTITET                        
004300        05 MOD-DAREGDAT-VISA-ATTR                                         
004400                             PIC X(2).                                    
004500*                                 MFS ATTRIBUTFÄLT                        
004600        05 MOD-DAREGDAT-VISA PIC 9(8).                                    
004700*                                 REGISTRERINGSDATUM (ÅÅÅÅMMDD)           
004800        05 MOD-TIREGTID-VISA-ATTR                                         
004900                             PIC X(2).                                    
005000*                                 MFS ATTRIBUTFÄLT                        
005100        05 MOD-TIREGTID-VISA PIC 9(6).                                    
005200*                                 REGISTRERINGSTID                        
005300        05 MOD-IDTRANS-VISA-ATTR                                          
005400                             PIC X(2).                                    
005500*                                 MFS ATTRIBUTFÄLT                        
005600        05 MOD-IDTRANS-VISA  PIC X(4).                                    
005700*                                 BILDNUMMER                              
005800     03 MOD-TEMFSINF         PIC X(55).                                   
005900*                                 INFORMATIONSMEDDELANDE                  
006000*** END OF VILMAII-COPY LENGTH= 1235 BYTES                                
