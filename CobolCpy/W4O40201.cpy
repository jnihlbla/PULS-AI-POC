000100 01  MOD-W4O40201.                                                        
000200*                                                                         
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-IDDC-IN          PIC X(2).                                    
000800*                                 IDENTIFIERARE LAGER                     
000900     03 MOD-IDDC-UT          PIC X(2).                                    
001000*                                 IDENTIFIERARE LAGER                     
001100     03 MOD-FLNYSEG-ATTR     PIC X(2).                                    
001200*                                 MFS ATTRIBUTFÄLT                        
001300     03 MOD-FLNYSEG          PIC X.                                       
001400*                                 NYTT SEGMENT                            
001500     03 MOD-BEGMT-RAD1-ATTR  PIC X(2).                                    
001600*                                 MFS ATTRIBUTFÄLT                        
001700     03 MOD-BEGMT-RAD1       PIC X(35).                                   
001800*                                 GODSMOTTAGARNAMN RAD 1                  
001900     03 MOD-BEGMT-RAD2-ATTR  PIC X(2).                                    
002000*                                 MFS ATTRIBUTFÄLT                        
002100     03 MOD-BEGMT-RAD2       PIC X(35).                                   
002200*                                 GODSMOTTAGARNAMN RAD 2                  
002300     03 MOD-ADGMT-GATA-ATTR  PIC X(2).                                    
002400*                                 MFS ATTRIBUTFÄLT                        
002500     03 MOD-ADGMT-GATA       PIC X(35).                                   
002600*                                 GODSMOTTAGARADRESS GATA                 
002700     03 MOD-ADPOSTNR-ATTR    PIC X(2).                                    
002800*                                 MFS ATTRIBUTFÄLT                        
002900     03 MOD-ADPOSTNR         PIC X(10).                                   
003000*                                 POSTNUMMER I ADRESS                     
003100     03 MOD-ADCITY-ATTR      PIC X(2).                                    
003200*                                 MFS ATTRIBUTFÄLT                        
003300     03 MOD-ADCITY           PIC X(20).                                   
003400     03 MOD-ADGMT-LAND-ATTR  PIC X(2).                                    
003500*                                 MFS ATTRIBUTFÄLT                        
003600     03 MOD-ADGMT-LAND       PIC X(35).                                   
003700*                                 GODSMOTTAGARADRESS LAND                 
003800     03 MOD-BEGMT-RAD1-INV-ATTR                                           
003900                             PIC X(2).                                    
004000*                                 MFS ATTRIBUTFÄLT                        
004100     03 MOD-BEGMT-RAD1-INV   PIC X(35).                                   
004200*                                 GODSMOTTAGARNAMN RAD 1                  
004300     03 MOD-BEGMT-RAD2-INV-ATTR                                           
004400                             PIC X(2).                                    
004500*                                 MFS ATTRIBUTFÄLT                        
004600     03 MOD-BEGMT-RAD2-INV   PIC X(35).                                   
004700*                                 GODSMOTTAGARNAMN RAD 2                  
004800     03 MOD-ADGMT-GATA-INV-ATTR                                           
004900                             PIC X(2).                                    
005000*                                 MFS ATTRIBUTFÄLT                        
005100     03 MOD-ADGMT-GATA-INV   PIC X(35).                                   
005200*                                 GODSMOTTAGARADRESS GATA                 
005300     03 MOD-ADPOSTNR-INV-ATTR                                             
005400                             PIC X(2).                                    
005500*                                 MFS ATTRIBUTFÄLT                        
005600     03 MOD-ADPOSTNR-INV     PIC X(10).                                   
005700*                                 POSTNUMMER I ADRESS                     
005800     03 MOD-ADCITY-INV-ATTR  PIC X(2).                                    
005900*                                 MFS ATTRIBUTFÄLT                        
006000     03 MOD-ADCITY-INV       PIC X(20).                                   
006100     03 MOD-ADGMT-LAND-INV-ATTR                                           
006200                             PIC X(2).                                    
006300*                                 MFS ATTRIBUTFÄLT                        
006400     03 MOD-ADGMT-LAND-INV   PIC X(35).                                   
006500*                                 GODSMOTTAGARADRESS LAND                 
006600     03 MOD-KDDC             PIC X(2).                                    
006700*                                 TYP AV DISTR. LAGER                     
006800     03 MOD-KDDC-IN-ATTR     PIC X(2).                                    
006900*                                 MFS ATTRIBUTFÄLT                        
007000     03 MOD-KDDC-IN          PIC X(2).                                    
007100*                                 TYP AV DISTR. LAGER                     
007200     03 MOD-IDLANDX2         PIC X(2).                                    
007300*                                 2-STÄLLIG LANDSBETECKNINGSKOD           
007400     03 MOD-IDLANDX2-IN-ATTR PIC X(2).                                    
007500*                                 MFS ATTRIBUTFÄLT                        
007600     03 MOD-IDLANDX2-IN      PIC X(2).                                    
007700*                                 2-STÄLLIG LANDSBETECKNINGSKOD           
007800     03 MOD-IDLEVNR          PIC X(5).                                    
007900*                                 LEVERANTÖRNUMMER                        
008000     03 MOD-IDLEVNR-IN-ATTR  PIC X(2).                                    
008100*                                 MFS ATTRIBUTFÄLT                        
008200     03 MOD-IDLEVNR-IN       PIC X(5).                                    
008300*                                 LEVERANTÖRNUMMER                        
008400     03 MOD-FLWEBDC          PIC X.                                       
008500*                                 DC MED WEB GRÄNSSNITT                   
008600     03 MOD-FLWEBDC-IN-ATTR  PIC X(2).                                    
008700*                                 MFS ATTRIBUTFÄLT                        
008800     03 MOD-FLWEBDC-IN       PIC X.                                       
008900*                                 DC MED WEB GRÄNSSNITT                   
009000     03 MOD-IDTIDZON         PIC 9(2).                                    
009100*                                 TIDZONER PÅ JORDEN.                     
009200     03 MOD-IDTIDZON-IN-ATTR PIC X(2).                                    
009300*                                 MFS ATTRIBUTFÄLT                        
009400     03 MOD-IDTIDZON-IN      PIC 9(2).                                    
009500*                                 TIDZONER PÅ JORDEN.                     
009600     03 MOD-KDVALISO         PIC X(3).                                    
009700*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
009800     03 MOD-KDVALISO-IN-ATTR PIC X(2).                                    
009900*                                 MFS ATTRIBUTFÄLT                        
010000     03 MOD-KDVALISO-IN      PIC X(3).                                    
010100*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
010200     03 MOD-IDVAT-ATTR       PIC X(2).                                    
010300*                                 MFS ATTRIBUTFÄLT                        
010400     03 MOD-IDVAT            PIC X(17).                                   
010500*                                 MOMSREGISTRERINGSNUMMER                 
010600     03 MOD-TIHHMM-START     PIC 9(4).                                    
010700*                                 KLOCKSLAG (TIMMAR/MIN.) START           
010800     03 MOD-TIHHMM-START-IN-ATTR                                          
010900                             PIC X(2).                                    
011000*                                 MFS ATTRIBUTFÄLT                        
011100     03 MOD-TIHHMM-START-IN  PIC 9(4).                                    
011200*                                 KLOCKSLAG (TIMMAR/MIN.) START           
011300     03 MOD-TIHHMM-READY     PIC 9(4).                                    
011400*                                 KLOCKSLAG (TIMMAR OCH MINUTER)          
011500*                                 AVSLUTNING                              
011600     03 MOD-TIHHMM-READY-IN-ATTR                                          
011700                             PIC X(2).                                    
011800*                                 MFS ATTRIBUTFÄLT                        
011900     03 MOD-TIHHMM-READY-IN  PIC 9(4).                                    
012000*                                 KLOCKSLAG (TIMMAR OCH MINUTER)          
012100*                                 AVSLUTNING                              
012200     03 MOD-FLINVACS         PIC X.                                       
012300*                                 ACS-INVENTERING?                        
012400     03 MOD-FLINVACS-IN-ATTR PIC X(2).                                    
012500*                                 MFS ATTRIBUTFÄLT                        
012600     03 MOD-FLINVACS-IN      PIC X.                                       
012700*                                 ACS-INVENTERING?                        
012800     03 MOD-IDFTG            PIC 9(2).                                    
012900*                                 FÖRETAGSID EKONOM REDOVISNING           
013000     03 MOD-IDFTG-IN-ATTR    PIC X(2).                                    
013100*                                 MFS ATTRIBUTFÄLT                        
013200     03 MOD-IDFTG-IN         PIC 9(2).                                    
013300*                                 FÖRETAGSID EKONOM REDOVISNING           
013400     03 MOD-FLMAINDC         PIC X.                                       
013500*                                 FLAGGA FÖR HUVUD DC PER IDFTG           
013600     03 MOD-FLMAINDC-IN-ATTR PIC X(2).                                    
013700*                                 MFS ATTRIBUTFÄLT                        
013800     03 MOD-FLMAINDC-IN      PIC X.                                       
013900*                                 FLAGGA FÖR HUVUD DC PER IDFTG           
014000     03 MOD-IDLEVNR-EMB      PIC X(5).                                    
014100*                                 ALT.LEV PER DC FÖR EMBALLAGE            
014200     03 MOD-IDLEVNR-EMB-IN-ATTR                                           
014300                             PIC X(2).                                    
014400*                                 MFS ATTRIBUTFÄLT                        
014500     03 MOD-IDLEVNR-EMB-IN   PIC X(5).                                    
014600*                                 ALT.LEV PER DC FÖR EMBALLAGE            
014700     03 MOD-IDPARTNR         PIC X(9).                                    
014800*                                 FINANCIELL KUND                         
014900     03 MOD-IDPARTNR-IN-ATTR PIC X(2).                                    
015000*                                 MFS ATTRIBUTFÄLT                        
015100     03 MOD-IDPARTNR-IN      PIC X(9).                                    
015200*                                 FINANCIELL KUND                         
015300     03 MOD-KDTRADP          PIC X(4).                                    
015400*                                 TRADING PARTNER                         
015500     03 MOD-KDTRADP-IN-ATTR  PIC X(2).                                    
015600*                                 MFS ATTRIBUTFÄLT                        
015700     03 MOD-KDTRADP-IN       PIC X(4).                                    
015800*                                 TRADING PARTNER                         
015900     03 MOD-IDLEGSEL         PIC X(4).                                    
016000*                                 FAKTURERANDE FÖRETAG TEX VCCS           
016100     03 MOD-IDLEGSEL-IN-ATTR PIC X(2).                                    
016200*                                 MFS ATTRIBUTFÄLT                        
016300     03 MOD-IDLEGSEL-IN      PIC X(4).                                    
016400*                                 FAKTURERANDE FÖRETAG TEX VCCS           
016500     03 MOD-FLSTOREF         PIC X.                                       
016600*                                 STOPP REFILL FLAGGA                     
016700*                                                                         
016800     03 MOD-FLSTOREF-IN-ATTR PIC X(2).                                    
016900*                                 MFS ATTRIBUTFÄLT                        
017000     03 MOD-FLSTOREF-IN      PIC X.                                       
017100*                                 STOPP REFILL FLAGGA                     
017200*                                                                         
017300     03 MOD-IDSKYLT-DB       PIC X(3).                                    
017400*                                 NATIONALITETSTECKEN                     
017500*                                 SPRÅKIDENTIFIKATION                     
017600     03 MOD-IDSKYLT-IN-ATTR  PIC X(2).                                    
017700*                                 MFS ATTRIBUTFÄLT                        
017800     03 MOD-IDSKYLT-IN       PIC X(3).                                    
017900*                                 NATIONALITETSTECKEN                     
018000*                                 SPRÅKIDENTIFIKATION                     
018100     03 MOD-FLSTOFC          PIC X.                                       
018200*                                 STOPP FC OMRÄKNING FLAGGA               
018300*                                                                         
018400     03 MOD-FLSTOFC-IN-ATTR  PIC X(2).                                    
018500*                                 MFS ATTRIBUTFÄLT                        
018600     03 MOD-FLSTOFC-IN       PIC X.                                       
018700*                                 STOPP FC OMRÄKNING FLAGGA               
018800*                                                                         
018900     03 MOD-FLLPO            PIC X.                                       
019000*                                 DC MED LOKALANSKAFFNING REFILL          
019100     03 MOD-FLLPO-IN-ATTR    PIC X(2).                                    
019200*                                 MFS ATTRIBUTFÄLT                        
019300     03 MOD-FLLPO-IN         PIC X.                                       
019400*                                 DC MED LOKALANSKAFFNING REFILL          
019500     03 MOD-TEMFSINF         PIC X(55).                                   
019600*                                 INFORMATIONSMEDDELANDE                  
019700*** END OF VILMAII-COPY LENGTH= 637 BYTES                                 
