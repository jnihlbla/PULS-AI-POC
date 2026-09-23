000100 01  MOD-W4O90201.                                                        
000200*                                 MOD-COPYTEXT F÷R FR≈GE-BILD             
000300*                                 FYSISKA AVVIKELSER                      
000400     03 MOD-IDTRANS          PIC X(4).                                    
000500*                                 BILDNUMMER                              
000600     03 MOD-TEMFSFEL         PIC X(40).                                   
000700*                                 MFS FELMEDDELANDE                       
000800     03 MOD-IDDISTR-IN       PIC X(4).                                    
000900*                                 DISTRIKTNUMMER                          
001000     03 MOD-IDDISTR-UT       PIC X(4).                                    
001100*                                 DISTRIKTNUMMER                          
001200     03 MOD-IDKUNDNR-IN      PIC X(6).                                    
001300*                                 KUNDNUMMER                              
001400     03 MOD-IDKUNDNR-UT      PIC X(6).                                    
001500*                                 KUNDNUMMER                              
001600     03 MOD-TIREGDAT-IN      PIC X(6).                                    
001700*                                 REGISTRERINGSDATUM (≈≈MMDD)             
001800     03 MOD-TIREGDAT-UT      PIC X(6).                                    
001900*                                 REGISTRERINGSDATUM (≈≈MMDD)             
002000     03 MOD-SPAR-TIREGDAT    PIC 9(6).                                    
002100*                                 REGISTRERINGSDATUM (≈≈MMDD)             
002200     03 MOD-SPAR-IDDISTR     PIC 9(4).                                    
002300*                                 DISTRIKTNUMMER                          
002400     03 MOD-SPAR-IDKUNDNR    PIC 9(6).                                    
002500*                                 KUNDNUMMER                              
002600     03 MOD-SPAR-IDORDNR     PIC 9(5).                                    
002700*                                 ORDERNUMMER                             
002800     03 MOD-SPAR-IDARTNR     PIC 9(9).                                    
002900*                                 ARTIKELNUMMER                           
003000     03 MOD-RAD              OCCURS 14 TIMES.                             
003100*                                 TABELL INNEH≈LLANDE RADER.              
003200        05 MOD-IDORDNR       PIC Z(4)9.                                   
003300*                                 ORDERNUMMER                             
003400        05 MOD-IDKUNDNR      PIC Z(5)9.                                   
003500*                                 KUNDNUMMER                              
003600        05 MOD-BEVARREF      PIC X(10).                                   
003700*                                 V≈R REFERENS                            
003800        05 MOD-IDARTNR       PIC Z(8)9.                                   
003900*                                 ARTIKELNUMMER                           
004000        05 MOD-STRECK        PIC X.                                       
004100        05 MOD-REKSIFFR      PIC 9.                                       
004200*                                 KONTROLLSIFFRA                          
004300        05 MOD-KVAVBART      PIC Z(5)9.                                   
004400*                                 AVBOKAT ANTAL ARTIKLAR                  
004500        05 MOD-KVLEVART      PIC Z(5)9.                                   
004600*                                 LEVERERAT ANTAL STYCK                   
004700        05 MOD-IDPRODNR      PIC Z(6)9.                                   
004800*                                 PRODUKTIONSNUMMER                       
004900        05 MOD-KDORDKL       PIC 9.                                       
005000*                                 ORDERKLASS                              
005100        05 MOD-IDDC          PIC X(2).                                    
005200*                                 IDENTIFIERARE LAGER                     
005300        05 MOD-BEART         PIC X(25).                                   
005400*                                 ARTIKELBENƒMNING                        
005500     03 MOD-TEMFSINF         PIC X(55).                                   
005600*                                 INFORMATIONSMEDDELANDE                  
005700*** END OF VILMAII-COPY LENGTH= 1267 BYTES                                
