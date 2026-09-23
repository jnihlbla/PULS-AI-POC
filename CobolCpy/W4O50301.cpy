000100 01  MOD-W4O50301.                                                        
000200*                                 MOD-COPYTEXT FÖR W4050300               
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
001500     03 MOD-IDORDNR7-IN      PIC X(2).                                    
001600*                                 MFS BEHANDLING AV INPUTFÄLT             
001700     03 MOD-IDORDNR7-UT      PIC X(7).                                    
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
003500     03 MOD-TEDDI            PIC X(11).                                   
003600*                                 TEXTFÄLT DDI                            
003700     03 MOD-BEKUNDRF         PIC X(15).                                   
003800*                                 KUNDENS REFERENS                        
003900     03 MOD-IDKAMPRF         PIC Z(6)9.                                   
004000*                                 KAMPANJREFERENS                         
004100     03 MOD-TIREGDAT         PIC 9(6).                                    
004200*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
004300     03 MOD-TIHHMM-REG       PIC X(5).                                    
004400*                                 KLOCKSLAG (TIMMAR OCH MINUTER)          
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
005500     03 MOD-TIHHMM-TRP       PIC X(5).                                    
005600*                                 KLOCKSLAG (TIMMAR OCH MINUTER)          
005700     03 MOD-TIREGDAT-STO     PIC 9(6).                                    
005800*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
005900     03 MOD-TIHHMM-REG-STO   PIC X(5).                                    
006000*                                 KLOCKSLAG (TIMMAR OCH MINUTER)          
006100     03 MOD-TIREPDAT         PIC 9(6).                                    
006200*                                 REPAIR DATE                             
006300     03 MOD-IDLEVNR          PIC X(5).                                    
006400*                                 LEVERANTÖRNUMMER                        
006500     03 MOD-KDORDTYP-LDC     PIC X(2).                                    
006600*                                 ORDERTYP HOS DEALER                     
006700     03 MOD-KDVALISO         PIC X(3).                                    
006800*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
006900     03 MOD-SUORDV-TOT       PIC Z(8)9.9(2).                              
007000*                                 SUMMA ORDERVÄRDE                        
007100     03 MOD-ASTERIX1         PIC X.                                       
007200*                                 ASTERISK                                
007300     03 MOD-KVRADER-TOT      PIC Z(4)9.                                   
007400*                                 ANTAL RADER                             
007500     03 MOD-SUORDV-U         PIC Z(8)9.9(2).                              
007600*                                 SUMMA ORDERVÄRDE                        
007700     03 MOD-ASTERIX2         PIC X.                                       
007800*                                 ASTERISK                                
007900     03 MOD-SUORDV-P         PIC Z(8)9.9(2).                              
008000*                                 SUMMA ORDERVÄRDE                        
008100     03 MOD-ASTERIX3         PIC X.                                       
008200*                                 ASTERISK                                
008300     03 MOD-KVRADER-P        PIC Z(4)9.                                   
008400*                                 ANTAL RADER                             
008500     03 MOD-SUORDV-F         PIC Z(8)9.9(2).                              
008600*                                 SUMMA ORDERVÄRDE                        
008700     03 MOD-ASTERIX4         PIC X.                                       
008800*                                 ASTERISK                                
008900     03 MOD-KVRADER-F        PIC Z(4)9.                                   
009000*                                 ANTAL RADER                             
009100     03 MOD-SUORDV-L         PIC Z(8)9.9(2).                              
009200*                                 SUMMA ORDERVÄRDE                        
009300     03 MOD-ASTERIX5         PIC X.                                       
009400*                                 ASTERISK                                
009500     03 MOD-KVRADER-L        PIC Z(4)9.                                   
009600*                                 ANTAL RADER                             
009700     03 MOD-FAKTURA-GRP      OCCURS 12 TIMES.                             
009800*                                 FAKTURANUMMER OCH FAKTURADATUM          
009900*                                                                         
010000        05 MOD-IDFAKT        PIC Z(7).                                    
010100*                                 FAKTURANUMMER                           
010200        05 MOD-TIFAKT        PIC X(6).                                    
010300*                                 FAKTURERINGSDATUM (ÅÅMMDD)              
010400     03 MOD-TEMFSINF         PIC X(55).                                   
010500*                                 INFORMATIONSMEDDELANDE                  
010600*** END OF VILMAII-COPY LENGTH= 487 BYTES                                 
