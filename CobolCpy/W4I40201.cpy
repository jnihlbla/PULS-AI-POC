000100 01  MID-W4I40201.                                                        
000200*                                                                         
000300     03 MID-IDDC-IN          PIC X(2).                                    
000400*                                 IDENTIFIERARE LAGER                     
000500     03 MID-FLNYSEG          PIC X.                                       
000600*                                 NYTT SEGMENT                            
000700     03 MID-INPUT.                                                        
000800        05 MID-BEGMT-RAD1    PIC X(35).                                   
000900*                                 GODSMOTTAGARNAMN RAD 1                  
001000        05 MID-BEGMT-RAD2    PIC X(35).                                   
001100*                                 GODSMOTTAGARNAMN RAD 2                  
001200        05 MID-ADGMT-GATA    PIC X(35).                                   
001300*                                 GODSMOTTAGARADRESS GATA                 
001400        05 MID-ADPOSTNR      PIC X(10).                                   
001500*                                 POSTNUMMER I ADRESS                     
001600        05 MID-ADCITY        PIC X(20).                                   
001700        05 MID-ADGMT-LAND    PIC X(35).                                   
001800*                                 GODSMOTTAGARADRESS LAND                 
001900        05 MID-BEGMT-RAD1-INV                                             
002000                             PIC X(35).                                   
002100*                                 GODSMOTTAGARNAMN RAD 1                  
002200        05 MID-BEGMT-RAD2-INV                                             
002300                             PIC X(35).                                   
002400*                                 GODSMOTTAGARNAMN RAD 2                  
002500        05 MID-ADGMT-GATA-INV                                             
002600                             PIC X(35).                                   
002700*                                 GODSMOTTAGARADRESS GATA                 
002800        05 MID-ADPOSTNR-INV  PIC X(10).                                   
002900*                                 POSTNUMMER I ADRESS                     
003000        05 MID-ADCITY-INV    PIC X(20).                                   
003100        05 MID-ADGMT-LAND-INV                                             
003200                             PIC X(35).                                   
003300*                                 GODSMOTTAGARADRESS LAND                 
003400        05 MID-KDDC-IN       PIC X(2).                                    
003500*                                 TYP AV DISTR. LAGER                     
003600        05 MID-IDLANDX2-IN   PIC X(2).                                    
003700*                                 2-STÄLLIG LANDSBETECKNINGSKOD           
003800        05 MID-IDLEVNR-IN    PIC X(5).                                    
003900*                                 LEVERANTÖRNUMMER                        
004000        05 MID-FLWEBDC-IN    PIC X.                                       
004100*                                 DC MED WEB GRÄNSSNITT                   
004200        05 MID-IDTIDZON-IN   PIC 9(2).                                    
004300*                                 TIDZONER PÅ JORDEN.                     
004400        05 MID-KDVALISO-IN   PIC X(3).                                    
004500*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
004600        05 MID-IDVAT         PIC X(17).                                   
004700*                                 MOMSREGISTRERINGSNUMMER                 
004800        05 MID-TIHHMM-START-IN                                            
004900                             PIC 9(4).                                    
005000*                                 KLOCKSLAG (TIMMAR/MIN.) START           
005100        05 MID-TIHHMM-READY-IN                                            
005200                             PIC 9(4).                                    
005300*                                 KLOCKSLAG (TIMMAR OCH MINUTER)          
005400*                                 AVSLUTNING                              
005500        05 MID-FLINVACS-IN   PIC X.                                       
005600*                                 ACS-INVENTERING?                        
005700        05 MID-IDFTG-IN      PIC 9(2).                                    
005800*                                 FÖRETAGSID EKONOM REDOVISNING           
005900        05 MID-FLMAINDC-IN   PIC X.                                       
006000*                                 FLAGGA FÖR HUVUD DC PER IDFTG           
006100        05 MID-IDLEVNR-EMB-IN                                             
006200                             PIC X(5).                                    
006300*                                 ALT.LEV PER DC FÖR EMBALLAGE            
006400        05 MID-IDPARTNR-IN   PIC X(9).                                    
006500*                                 FINANCIELL KUND                         
006600        05 MID-KDTRADP-IN    PIC X(4).                                    
006700*                                 TRADING PARTNER                         
006800        05 MID-IDLEGSEL-IN   PIC X(4).                                    
006900*                                 FAKTURERANDE FÖRETAG TEX VCCS           
007000        05 MID-FLSTOREF-IN   PIC X.                                       
007100*                                 STOPP REFILL FLAGGA                     
007200*                                                                         
007300        05 MID-IDSKYLT-IN    PIC X(3).                                    
007400*                                 NATIONALITETSTECKEN                     
007500*                                 SPRÅKIDENTIFIKATION                     
007600        05 MID-FLSTOFC-IN    PIC X.                                       
007700*                                 STOPP FC OMRÄKNING FLAGGA               
007800*                                                                         
007900        05 MID-FLLPO-IN      PIC X.                                       
008000*                                 DC MED LOKALANSKAFFNING REFILL          
008100*** END OF VILMAII-COPY LENGTH= 415 BYTES                                 
