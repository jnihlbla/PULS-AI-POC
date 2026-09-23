000100 01  MOD-W4O71901.                                                        
000200*                                 MOD-COPYTEXT FOR PGM W4071900           
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-IDPARTNR-IN      PIC X(2).                                    
000800*                                 MFS BEHANDLING AV INPUTFÄLT             
000900     03 MOD-IDPARTNR-UT      PIC X(9).                                    
001000*                                 PARTNERNUMMER                           
001100     03 MOD-IDFTG-IN         PIC X(2).                                    
001200*                                 MFS BEHANDLING AV INPUTFÄLT             
001300     03 MOD-IDFTG-UT         PIC X(2).                                    
001400*                                 FÖRETAGSID EKONOM REDOVISNING           
001500     03 MOD-IDDISTR-IN       PIC X(2).                                    
001600*                                 MFS BEHANDLING AV INPUTFÄLT             
001700     03 MOD-IDDISTR-UT       PIC X(4).                                    
001800*                                 DISTRIKTNUMMER                          
001900     03 MOD-IDKUNDNR-IN      PIC X(2).                                    
002000*                                 MFS BEHANDLING AV INPUTFÄLT             
002100     03 MOD-IDKUNDNR-UT      PIC X(6).                                    
002200*                                 KUNDNUMMER                              
002300     03 MOD-SUARTBTO-MIN     PIC Z(6)9.9(2).                              
002400*                                 SUMMA FÖRSÄLJNINGSVÄRDE                 
002500     03 MOD-KDVALISO-MIN     PIC X(3).                                    
002600*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
002700     03 MOD-RADER            OCCURS 8 TIMES.                              
002800*                                 RADINFORMATION                          
002900        05 MOD-KDBEHX-ATTR   PIC X(2).                                    
003000*                                 MFS ATTRIBUTFÄLT                        
003100        05 MOD-KDBEHX        PIC X(2).                                    
003200*                                 MFS BEHANDLING AV INPUTFÄLT             
003300        05 MOD-KDANMORS      PIC X(2).                                    
003400*                                 ORSAK TILL LEVERANSANMÄRKNING           
003500        05 MOD-IDDISTR       PIC X(4).                                    
003600*                                 DISTRIKTNUMMER                          
003700        05 MOD-IDKUNDNR      PIC X(6).                                    
003800*                                 KUNDNUMMER                              
003900        05 MOD-IDREF         PIC X(15).                                   
004000*                                 REFERENS ID                             
004100        05 MOD-SUARTBTO      PIC Z(6)9.9(2).                              
004200*                                 SUMMA FÖRSÄLJNINGSVÄRDE                 
004300        05 MOD-KDVALISO      PIC X(3).                                    
004400*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
004500        05 MOD-DAREGDAT      PIC X(8).                                    
004600*                                 REGISTRERINGSDATUM (ÅÅÅÅMMDD)           
004700     03 MOD-INDATA.                                                       
004800*                                 INDATA BILD 4719                        
004900        05 MOD-KDBEHX-UPD-ATTR                                            
005000                             PIC X(2).                                    
005100*                                 MFS ATTRIBUTFÄLT                        
005200        05 MOD-KDBEHX-UPD    PIC X(2).                                    
005300*                                 MFS BEHANDLING AV INPUTFÄLT             
005400        05 MOD-SUARTBTO-ATTR PIC X(2).                                    
005500*                                 MFS ATTRIBUTFÄLT                        
005600        05 MOD-SUARTBTO-UPD  PIC X(2).                                    
005700*                                 MFS BEHANDLING AV INPUTFÄLT             
005800        05 MOD-KDVALISO-ATTR PIC X(2).                                    
005900*                                 MFS ATTRIBUTFÄLT                        
006000        05 MOD-KDVALISO-UPD  PIC X(2).                                    
006100*                                 MFS BEHANDLING AV INPUTFÄLT             
006200     03 MOD-SUARTBTO-RET     PIC Z(6)9.9(2).                              
006300*                                 SUMMA FÖRSÄLJNINGSVÄRDE FÖR RET         
006400*                                 URER                                    
006500     03 MOD-SUARTBTO-72      PIC Z(6)9.9(2).                              
006600*                                 SUMMA FÖRSÄLJNINGSVÄRDE                 
006700     03 MOD-KDVALISO-72      PIC X(3).                                    
006800*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
006900     03 MOD-SUARTBTO-98      PIC Z(6)9.9(2).                              
007000*                                 SUMMA FÖRSÄLJNINGSVÄRDE                 
007100     03 MOD-KDVALISO-98      PIC X(3).                                    
007200*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
007300     03 MOD-SUARTBTO-RR      PIC Z(6)9.9(2).                              
007400*                                 SUMMA FÖRSÄLJNINGSVÄRDE                 
007500     03 MOD-KDVALISO-RR      PIC X(3).                                    
007600*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
007700     03 MOD-TEMFSINF         PIC X(55).                                   
007800*                                 INFORMATIONSMEDDELANDE                  
007900*** END OF VILMAII-COPY LENGTH= 618 BYTES                                 
