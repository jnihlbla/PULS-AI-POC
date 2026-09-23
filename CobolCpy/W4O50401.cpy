000100 01  MOD-W4O50401.                                                        
000200*                                 MOD-COPYTEXT FÖR W4050400               
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
001500     03 MOD-IDKUNDRF-IN      PIC X(2).                                    
001600*                                 MFS BEHANDLING AV INPUTFÄLT             
001700     03 MOD-IDKUNDRF-UT      PIC X(7).                                    
001800*                                 ORDERNUMMER                             
001900     03 MOD-IDARTNR-IN       PIC X(2).                                    
002000*                                 MFS BEHANDLING AV INPUTFÄLT             
002100     03 MOD-IDARTNR-UT       PIC X(9).                                    
002200*                                 ARTIKELNUMMER                           
002300     03 MOD-IDKOLLI-IN       PIC X(2).                                    
002400*                                 MFS BEHANDLING AV INPUTFÄLT             
002500     03 MOD-IDKOLLI-UT       PIC X(5).                                    
002600*                                 KOLLINUMMER                             
002700     03 MOD-IDPRODNR-IN      PIC X(2).                                    
002800*                                 MFS BEHANDLING AV INPUTFÄLT             
002900     03 MOD-IDPRODNR-UT      PIC X(7).                                    
003000*                                 PRODUKTIONSNUMMER                       
003100     03 MOD-IDDC-IN          PIC X(2).                                    
003200*                                 IDENTIFIERARE LAGER                     
003300     03 MOD-IDDC-UT          PIC X(2).                                    
003400*                                 IDENTIFIERARE LAGER                     
003500     03 MOD-IDPRT-IN-ATTR    PIC X(2).                                    
003600*                                 MFS ATTRIBUTFÄLT                        
003700     03 MOD-IDPRT-IN         PIC X(3).                                    
003800*                                 LOGISK PRINTERIDENTITET                 
003900     03 MOD-BEKUNDRF         PIC X(15).                                   
004000*                                 KUNDENS REFERENS                        
004100     03 MOD-IDKAMPRF         PIC Z(6)9.                                   
004200*                                 KAMPANJREFERENS                         
004300     03 MOD-TIREGDAT         PIC 9(6).                                    
004400*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
004500     03 MOD-TIAAMMDD-RFS     PIC X(6).                                    
004600*                                 ÅR - MÅNAD - DAG  (ÅÅMMDD)              
004700     03 MOD-IDTRP.                                                        
004800*                                 TRANSPORTIDENTITET                      
004900        05 MOD-IDTRPLOS      PIC X(3).                                    
005000*                                 TRANSPORTLÖSNING                        
005100        05 MOD-IDTRPVAR      PIC X(2).                                    
005200*                                 TRANSPORTLÖSNINGSGRUPP                  
005300     03 MOD-TIAAMMDD-TRP     PIC X(6).                                    
005400*                                 ÅR - MÅNAD - DAG  (ÅÅMMDD)              
005500     03 MOD-TIHHMM           PIC X(5).                                    
005600*                                 KLOCKSLAG (TIMMAR OCH MINUTER)          
005700     03 MOD-BEKOPARE.                                                     
005800*                                 KÖPARNAMN                               
005900        05 MOD-BEKOPARE-RAD1 PIC X(35).                                   
006000*                                 KÖPARE NAMN RAD 1                       
006100        05 MOD-BEKOPARE-RAD2 PIC X(35).                                   
006200*                                 KÖPARE NAMN RAD 2                       
006300     03 MOD-ADKOPARE.                                                     
006400*                                 KÖPARADRESS                             
006500        05 MOD-ADKOPARE-RAD1 PIC X(35).                                   
006600*                                 KÖPARE ADRESS RAD 1                     
006700        05 MOD-ADKOPARE-RAD2 PIC X(35).                                   
006800*                                 KÖPARE ADRESS RAD 2                     
006900     03 MOD-BEGMT-RAD1       PIC X(35).                                   
007000*                                 GODSMOTTAGARNAMN RAD 1                  
007100     03 MOD-BEBET-RAD1       PIC X(35).                                   
007200*                                 DEL AV BETALNINGSANSVARIGS NAMN         
007300     03 MOD-BEGMT-RAD2       PIC X(35).                                   
007400*                                 GODSMOTTAGARNAMN RAD 2                  
007500     03 MOD-BEBET-RAD2       PIC X(35).                                   
007600*                                 DEL AV BETALNINGSANSVARIGS NAMN         
007700     03 MOD-ADGMT-RAD1       PIC X(35).                                   
007800*                                 GODSMOTTAGARADRESS RAD 1                
007900     03 MOD-ADBET-RAD1       PIC X(35).                                   
008000*                                 ADRESSRAD BETALNINGSANSVARIG            
008100     03 MOD-ADGMT-RAD2       PIC X(35).                                   
008200*                                 GODSMOTTAGARADRESS RAD 2                
008300     03 MOD-ADBET-RAD2       PIC X(35).                                   
008400*                                 ADRESSRAD BETALNINGSANSVARIG            
008500     03 MOD-BEGDSMRK-GRP.                                                 
008600*                                 GODSMÄRKE                               
008700*                                                                         
008800        05 MOD-BEGDSMRK-DEL1 PIC X(30).                                   
008900*                                 DEL AV GODSMÄRKE                        
009000        05 MOD-BEGDSMRK-DEL2 PIC X(30).                                   
009100*                                 DEL AV GODSMÄRKE                        
009200     03 MOD-TEMFSINF         PIC X(55).                                   
009300*                                 INFORMATIONSMEDDELANDE                  
009400*** END OF VILMAII-COPY LENGTH= 688 BYTES                                 
