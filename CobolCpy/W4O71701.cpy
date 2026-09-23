000100 01  MOD-W4O71701.                                                        
000200*                                 MOD-COPYTEXT FOR PGM W4071700           
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
001500     03 MOD-IDRAPPNR-IN      PIC X(2).                                    
001600*                                 MFS BEHANDLING AV INPUTFÄLT             
001700     03 MOD-IDRAPPNR-UT      PIC X(7).                                    
001800*                                 RAPPORT NUMMER                          
001900     03 MOD-IDUSER-IN        PIC X(2).                                    
002000*                                 MFS BEHANDLING AV INPUTFÄLT             
002100     03 MOD-IDUSER-UT        PIC X(8).                                    
002200*                                 ANVÄNDARENS SÄKERHETS ID                
002300     03 MOD-FLTOT-IN         PIC X(2).                                    
002400*                                 MFS BEHANDLING AV INPUTFÄLT             
002500     03 MOD-FLTOT-UT         PIC X.                                       
002600*                                 INDIKERAR TOTALINFO VISNING             
002700     03 MOD-FLGODK-ATTR      PIC X(2).                                    
002800*                                 MFS ATTRIBUTFÄLT                        
002900     03 MOD-FLGODK           PIC X.                                       
003000*                                 GODKÄNT?  JA/NEJ                        
003100     03 MOD-RADER            OCCURS 14 TIMES.                             
003200*                                 RADINFORMATION                          
003300        05 MOD-KDCMD-ATTR    PIC X(2).                                    
003400*                                 MFS ATTRIBUTFÄLT                        
003500        05 MOD-KDCMD         PIC X(2).                                    
003600*                                 MFS BEHANDLING AV INPUTFÄLT             
003700        05 MOD-IDDISTR       PIC Z(3)9.                                   
003800*                                 DISTRIKTNUMMER                          
003900        05 MOD-IDKUNDNR      PIC Z(5)9.                                   
004000*                                 KUNDNUMMER                              
004100        05 MOD-IDRAPPNR      PIC Z(6)9.                                   
004200*                                 RAPPORT NUMMER                          
004300        05 MOD-IDDC          PIC X(2).                                    
004400*                                 IDENTIFIERARE LAGER                     
004500        05 MOD-KDKRENOT      PIC X(2).                                    
004600*                                 TYP AV KREDITERING                      
004700        05 MOD-SUKRENOT      PIC Z(6)9.9(2).                              
004800*                                 KREDITNOTASUMMA                         
004900        05 MOD-BEANST        PIC X(9).                                    
005000*                                 ANSTÄLLDS NAMN                          
005100        05 MOD-IDUSER        PIC X(8).                                    
005200*                                 ANVÄNDARENS SÄKERHETS ID                
005300        05 MOD-BEANST-GODK   PIC X(8).                                    
005400*                                 GODKÄNNARES NAMN                        
005500        05 MOD-IDANSTNR-GODK PIC X(5).                                    
005600*                                 ANSTÄLLNINGSNUMMER                      
005700        05 MOD-TIMMDD        PIC 9(4).                                    
005800*                                 MÅNAD/DAG (MMDD)                        
005900*                                                                         
006000        05 MOD-FLKLAR        PIC X.                                       
006100*                                 AVSLUTNINGSMARKERING                    
006200     03 MOD-TEMFSINF         PIC X(55).                                   
006300*                                 INFORMATIONSMEDDELANDE                  
006400*** END OF VILMAII-COPY LENGTH= 1118 BYTES                                
