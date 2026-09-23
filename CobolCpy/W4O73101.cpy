000100 01  W4O73101.                                                            
000200*                                 MODCOPYTEXT TILL W40731.                
000300     03 IDTRANS              PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 TEMFSFEL             PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 KDRETSTA-IN          PIC X.                                       
000800*                                 STATUS RETURER                          
000900     03 IDRT-IN              PIC X(3).                                    
001000*                                 RETURTERMINAL                           
001100     03 IDRTLOP-IN           PIC X(3).                                    
001200*                                 RETUR TERMINAL LÖPNUMMER                
001300     03 KDRETSTA-UT          PIC X.                                       
001400*                                 STATUS RETURER                          
001500     03 IDRT-UT              PIC X(3).                                    
001600*                                 RETURTERMINAL                           
001700     03 IDRTLOP-UT           PIC X(3).                                    
001800*                                 RETUR TERMINAL LÖPNUMMER                
001900     03 INPUT.                                                            
002000*                                                                         
002100        05 IDANSTNR-ATTR     PIC X(2).                                    
002200*                                 MFS ATTRIBUTFÄLT                        
002300        05 IDANSTNR-UPD      PIC X(2).                                    
002400*                                 MFS BEHANDLING AV INPUTFÄLT             
002500        05 ADINLOMR-ATTR     PIC X(2).                                    
002600*                                 MFS ATTRIBUTFÄLT                        
002700        05 ADINLOMR-UPD      PIC X(2).                                    
002800*                                 MFS BEHANDLING AV INPUTFÄLT             
002900     03 RADER                OCCURS 11 TIMES.                             
003000*                                                                         
003100        05 KDCMD-ATTR        PIC X(2).                                    
003200*                                 MFS ATTRIBUTFÄLT                        
003300        05 KDCMD             PIC X(4).                                    
003400        05 IDRT              PIC X(3).                                    
003500*                                 RETURTERMINAL                           
003600        05 IDRTLOP           PIC X(3).                                    
003700*                                 RETUR TERMINAL LÖPNUMMER                
003800        05 TISNDDAT          PIC 9(6).                                    
003900*                                 SÄNDNINGSDATUM     (ÅÅMMDD)             
004000        05 KVKOLLI-SND       PIC Z(3)9.                                   
004100*                                 ANTAL KOLLI                             
004200        05 TILOSSN           PIC 9(6).                                    
004300*                                 LOSSNINGSDATUM                          
004400        05 ADINLOMR-LOSS     PIC X(4).                                    
004500*                                 INLEVERANSOMRÅDE                        
004600        05 IDANSTNR-LOSS     PIC Z(4)9.                                   
004700*                                 ANSTÄLLNINGSNUMMER                      
004800        05 KVKOLLI-LOSS      PIC Z(3)9.                                   
004900*                                 ANTAL KOLLI                             
005000        05 TIINLMOT          PIC 9(6).                                    
005100*                                 MOTTAGNINGSDATUM   (ÅÅMMDD)             
005200        05 ADINLOMR-MOT      PIC X(4).                                    
005300*                                 INLEVERANSOMRÅDE                        
005400        05 IDANSTNR-MOT      PIC Z(4)9.                                   
005500*                                 ANSTÄLLNINGSNUMMER                      
005600        05 KVKOLLI-MOT       PIC Z(3)9.                                   
005700*                                 ANTAL KOLLI                             
005800     03 TEMFSINF             PIC X(55).                                   
005900*                                 INFORMATIONSMEDDELANDE                  
006000*** END OF VILMAII-COPY LENGTH= 781 BYTES                                 
