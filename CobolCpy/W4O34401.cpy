000100 01  MOD-W4O34401.                                                        
000200*                                 MODCOPYTEXT TILL W40344.                
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-IDDISTR-IN       PIC X(4).                                    
000800*                                 DISTRIKTNUMMER                          
000900     03 MOD-IDKUNDNR-IN      PIC X(6).                                    
001000*                                 KUNDNUMMER                              
001100     03 MOD-IDORDNR7-IN      PIC X(7).                                    
001200*                                 ORDERNUMMER                             
001300     03 MOD-IDKOLLI-IN       PIC X(5).                                    
001400*                                 KOLLINUMMER                             
001500     03 MOD-IDKOLLI-SAMP-IN  PIC X(5).                                    
001600*                                 KOLLINUMMER                             
001700     03 MOD-IDDC-IN          PIC X(2).                                    
001800*                                 IDENTIFIERARE LAGER                     
001900     03 MOD-IDDISTR-UT       PIC X(4).                                    
002000*                                 DISTRIKTNUMMER                          
002100     03 MOD-IDKUNDNR-UT      PIC X(6).                                    
002200*                                 KUNDNUMMER                              
002300     03 MOD-IDORDNR7-UT      PIC X(7).                                    
002400*                                 ORDERNUMMER                             
002500     03 MOD-IDKOLLI-UT       PIC X(5).                                    
002600*                                 KOLLINUMMER                             
002700     03 MOD-IDKOLLI-SAMP-UT  PIC X(5).                                    
002800*                                 KOLLINUMMER                             
002900     03 MOD-IDDC-UT          PIC X(2).                                    
003000*                                 IDENTIFIERARE LAGER                     
003100     03 MOD-IDKOLLI-SAMP     PIC Z(4)9.                                   
003200*                                 KOLLINUMMER                             
003300     03 MOD-IDDC             PIC X(2).                                    
003400*                                 IDENTIFIERARE LAGER                     
003500     03 MOD-RADER            OCCURS 14 TIMES.                             
003600*                                                                         
003700        05 MOD-IDDISTR       PIC Z(3)9.                                   
003800*                                 DISTRIKTNUMMER                          
003900        05 MOD-IDKUNDNR      PIC Z(5)9.                                   
004000*                                 KUNDNUMMER                              
004100        05 MOD-IDORDNR7      PIC Z(6)9.                                   
004200*                                 ORDERNUMMER                             
004300        05 MOD-IDKOLLI       PIC Z(4)9.                                   
004400*                                 KOLLINUMMER                             
004500     03 MOD-FLREOPENED-ATTR  PIC X(2).                                    
004600*                                 MFS ATTRIBUTFÄLT                        
004700     03 MOD-FLREOPENED       PIC X.                                       
004800*                                 ALLMÄN FLAGGA                           
004900     03 MOD-REOPEN-RADER     OCCURS 7 TIMES.                              
005000*                                                                         
005100        05 MOD-IDKOLLI-SAMP-R                                             
005200                             PIC X(5).                                    
005300*                                 SAMPACKNINGSKOLLINUMMER                 
005400        05 MOD-IDDC-R        PIC X(2).                                    
005500*                                 IDENTIFIERARE LAGER                     
005600        05 MOD-TIREGDAT-R    PIC 9(6).                                    
005700*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
005800     03 MOD-TEMFSINF         PIC X(55).                                   
005900*                                 INFORMATIONSMEDDELANDE                  
006000*** END OF VILMAII-COPY LENGTH= 566 BYTES                                 
