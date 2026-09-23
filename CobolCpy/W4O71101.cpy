000100 01  MOD-W4O71101.                                                        
000200*                                 MOD-COPYTEXT FÖR W4071100               
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-IDDISTR-IN       PIC X(4).                                    
000800*                                 DISTRIKTNUMMER                          
000900     03 MOD-IDDISTR-UT       PIC X(4).                                    
001000*                                 DISTRIKTNUMMER                          
001100     03 MOD-IDKUNDNR-IN      PIC X(6).                                    
001200*                                 KUNDNUMMER                              
001300     03 MOD-IDKUNDNR-UT      PIC X(6).                                    
001400*                                 KUNDNUMMER                              
001500     03 MOD-KDLEVANM-IN      PIC X.                                       
001600*                                 STATUS LEVERANSANMÄRKNING               
001700     03 MOD-KDLEVANM-UT      PIC X.                                       
001800*                                 STATUS LEVERANSANMÄRKNING               
001900     03 MOD-IDDISTR-ENTER    PIC 9(4).                                    
002000*                                 DISTRIKTNUMMER                          
002100     03 MOD-IDKUNDNR-ENTER   PIC 9(6).                                    
002200*                                 KUNDNUMMER                              
002300     03 MOD-IDRAPPNR-ENTER   PIC X(7).                                    
002400*                                 RAPPORT NUMMER                          
002500     03 MOD-KDLEVANM-ENTER   PIC X.                                       
002600*                                 STATUS LEVERANSANMÄRKNING               
002700     03 MOD-IDDISTR-NEXT     PIC 9(4).                                    
002800*                                 DISTRIKTNUMMER                          
002900     03 MOD-IDKUNDNR-NEXT    PIC 9(6).                                    
003000*                                 KUNDNUMMER                              
003100     03 MOD-IDRAPPNR-NEXT    PIC X(7).                                    
003200*                                 RAPPORT NUMMER                          
003300     03 MOD-KDLEVANM-NEXT    PIC X.                                       
003400*                                 STATUS LEVERANSANMÄRKNING               
003500     03 MOD-RADER            OCCURS 11 TIMES.                             
003600*                                 RADINFORMATION                          
003700        05 MOD-KDCMD-ATTR    PIC X(2).                                    
003800*                                 MFS ATTRIBUTFÄLT                        
003900        05 MOD-KDCMDVAL      PIC X(3).                                    
004000*                                 GENERELL KOMMANDOKOD                    
004100        05 MOD-IDDISTR-ATTR  PIC X(2).                                    
004200*                                 MFS ATTRIBUTFÄLT                        
004300        05 MOD-IDDISTR       PIC Z(3)9.                                   
004400*                                 DISTRIKTNUMMER                          
004500        05 MOD-IDKUNDNR-ATTR PIC X(2).                                    
004600*                                 MFS ATTRIBUTFÄLT                        
004700        05 MOD-IDKUNDNR      PIC Z(5)9.                                   
004800*                                 KUNDNUMMER                              
004900        05 MOD-IDRAPPNR-ATTR PIC X(2).                                    
005000*                                 MFS ATTRIBUTFÄLT                        
005100        05 MOD-IDRAPPNR      PIC Z(6)9.                                   
005200*                                 RAPPORT NUMMER                          
005300        05 MOD-KVRADER-TOT   PIC Z(4)9.                                   
005400*                                 ANTAL RADER                             
005500        05 MOD-KVRADER-BEH   PIC Z(4)9.                                   
005600*                                 ANTAL RADER                             
005700        05 MOD-TILEVANM      PIC 9(6).                                    
005800*                                 DATUM LEVERANSANMÄRKNING                
005900        05 MOD-KDLEVANM      PIC X.                                       
006000*                                 STATUS LEVERANSANMÄRKNING               
006100        05 MOD-TIRETILL      PIC 9(6).                                    
006200*                                 RETURTILLSTÅNDSDATUM                    
006300        05 MOD-IDUSER        PIC X(8).                                    
006400*                                 ANVÄNDARENS SÄKERHETS ID                
006500     03 MOD-INDATA.                                                       
006600*                                 INDATA                                  
006700        05 MOD-IDDISTR-ANN-ATTR                                           
006800                             PIC X(2).                                    
006900*                                 MFS ATTRIBUTFÄLT                        
007000        05 MOD-IDDISTR-ANN   PIC Z(3)9.                                   
007100*                                 DISTRIKTNUMMER                          
007200        05 MOD-IDKUNDNR-ANN-ATTR                                          
007300                             PIC X(2).                                    
007400*                                 MFS ATTRIBUTFÄLT                        
007500        05 MOD-IDKUNDNR-ANN  PIC Z(5)9.                                   
007600*                                 KUNDNUMMER                              
007700        05 MOD-IDRAPPNR-ANN-ATTR                                          
007800                             PIC X(2).                                    
007900*                                 MFS ATTRIBUTFÄLT                        
008000        05 MOD-IDRAPPNR-ANN  PIC X(7).                                    
008100*                                 RAPPORT NUMMER                          
008200        05 MOD-FLSVAR-ANN-ATTR                                            
008300                             PIC X(2).                                    
008400*                                 MFS ATTRIBUTFÄLT                        
008500        05 MOD-FLSVAR-ANN    PIC X(2).                                    
008600*                                 MFS BEHANDLING AV INPUTFÄLT             
008700        05 MOD-FLSVAR-GODKANN-ATTR                                        
008800                             PIC X(2).                                    
008900*                                 MFS ATTRIBUTFÄLT                        
009000        05 MOD-FLSVAR-GODKANN                                             
009100                             PIC X(2).                                    
009200*                                 MFS BEHANDLING AV INPUTFÄLT             
009300     03 MOD-TEMFSINF         PIC X(55).                                   
009400*                                 INFORMATIONSMEDDELANDE                  
009500*** END OF VILMAII-COPY LENGTH= 837 BYTES                                 
