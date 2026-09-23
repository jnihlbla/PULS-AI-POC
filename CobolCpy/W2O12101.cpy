000100 01  MOD-W2O12101.                                                        
000200*                                 MOD COPYTEXT FÖR W2012100               
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-IDARTNR-IN       PIC X(2).                                    
000800*                                 MFS BEHANDLING AV INPUTFÄLT             
000900     03 MOD-IDARTNR-UT       PIC X(9).                                    
001000*                                 ARTIKELNUMMER                           
001100     03 MOD-BINDESTRECK      PIC X.                                       
001200     03 MOD-REKSIFFR         PIC 9.                                       
001300*                                 KONTROLLSIFFRA                          
001400     03 MOD-SW-633           PIC X.                                       
001500     03 MOD-IDBEST-SW        PIC 9(12).                                   
001600*                                 BESTÄLLNINGS-ID (PPPBBBBBBSSS)          
001700*                                 PPP   = (PREFIX) INKÖPARNR              
001800*                                 BBBBBB= BESTÄLLARNR                     
001900*                                 SSS   = (SUFFIX) GODSM/PROD.KOD         
002000     03 MOD-TEST-AVTNR-1     PIC Z(11)9.                                  
002100*                                 AVTALSIDENTITET  (PPPBBBBBSSS)          
002200*                                 PPP   = INKÖPARNR (PREFIX)              
002300*                                 BBBBB = BESTÄLLARNR                     
002400*                                 SSS   = SUFFIX                          
002500     03 MOD-TEST-AVTNR-2     PIC Z(11)9.                                  
002600*                                 AVTALSIDENTITET  (PPPBBBBBSSS)          
002700*                                 PPP   = INKÖPARNR (PREFIX)              
002800*                                 BBBBB = BESTÄLLARNR                     
002900*                                 SSS   = SUFFIX                          
003000     03 MOD-TEST-IDLEVNR-AVT-1                                            
003100                             PIC X(5).                                    
003200*                                 LEVERANTÖRNUMMER                        
003300     03 MOD-TEST-IDLEVNR-AVT-2                                            
003400                             PIC X(5).                                    
003500*                                 LEVERANTÖRNUMMER                        
003600     03 MOD-UTRAD            OCCURS 7 TIMES.                              
003700*                                 VISNINGSFAELT                           
003800        05 MOD-IDBEST-BEST   PIC X(12).                                   
003900        05 MOD-IDLEVNR-BEST  PIC X(5).                                    
004000*                                 LEVERANTÖRNUMMER                        
004100        05 MOD-TIBEST-BEST   PIC 9(6).                                    
004200*                                 BESTÄLLNINGSDATUM (ÅÅMMDD)              
004300        05 MOD-KVBEST-BEST   PIC Z(6)9.                                   
004400*                                 BESTÄLLT ANTAL                          
004500        05 MOD-KVBEST-BEKR-BEST                                           
004600                             PIC Z(6)9.                                   
004700*                                 BEKRÄFTAT BESTÄLLT ANTAL                
004800        05 MOD-MARKING-BEST-BEST                                          
004900                             PIC X(11).                                   
005000     03 MOD-IDBEST-IN-ATTR   PIC X(2).                                    
005100*                                 MFS ATTRIBUTFÄLT                        
005200     03 MOD-IDBEST-IN        PIC X(2).                                    
005300*                                 MFS BEHANDLING AV INPUTFÄLT             
005400     03 MOD-IDLEVNR-IN-ATTR  PIC X(2).                                    
005500*                                 MFS ATTRIBUTFÄLT                        
005600     03 MOD-IDLEVNR-IN       PIC X(2).                                    
005700*                                 MFS BEHANDLING AV INPUTFÄLT             
005800     03 MOD-JUST-KVANT-IN-ATTR                                            
005900                             PIC X(2).                                    
006000*                                 MFS ATTRIBUTFÄLT                        
006100     03 MOD-JUST-KVANT-IN    PIC X(2).                                    
006200*                                 MFS BEHANDLING AV INPUTFÄLT             
006300     03 MOD-U-N-A-IN-ATTR    PIC X(2).                                    
006400*                                 MFS ATTRIBUTFÄLT                        
006500     03 MOD-U-N-A-IN         PIC X.                                       
006600     03 MOD-BEF-TOT-BR-ATTR  PIC X(2).                                    
006700*                                 MFS ATTRIBUTFÄLT                        
006800     03 MOD-BEF-TOT-BR       PIC Z(6)9.                                   
006900*                                 BESTÄLLT ANTAL                          
007000     03 MOD-N-BEST-REST-ATTR PIC X(2).                                    
007100*                                 MFS ATTRIBUTFÄLT                        
007200     03 MOD-N-BESTREST       PIC Z(6)9.                                   
007300*                                 BESTÄLLT ANTAL                          
007400     03 MOD-VISNRAD          OCCURS 2 TIMES.                              
007500*                                 VISNINGSFAELT                           
007600        05 MOD-IDAVTAL-AVT   PIC X(12).                                   
007700        05 MOD-IDLEVNR-AVT   PIC X(5).                                    
007800*                                 LEVERANTÖR ENLIGT AVTAL                 
007900        05 MOD-IDLEVNR-SHIP  PIC X(5).                                    
008000*                                 SKEPPANDE LEVERANTÖR                    
008100        05 MOD-DATUM-AVT     PIC 9(6).                                    
008200*                                 AVTALSDATUM  (ÅÅMMDD)                   
008300        05 MOD-ARSANTAL-AVT  PIC Z(6)9.                                   
008400*                                 ÅRSANTAL AVTAL                          
008500        05 MOD-KDFPKPRI      PIC X.                                       
008600*                                 OM FÖRPACKNING INGÅR I ARTPRIS          
008700     03 MOD-PRINK            PIC Z(6)9.9(2).                              
008800*                                 INKÖPSPRIS                              
008900     03 MOD-PRARTSTD         PIC Z(6)9.9(2).                              
009000*                                 ARTIKELSTANDARDPRIS                     
009100     03 MOD-PRARTBES         PIC Z(6)9.9(2).                              
009200*                                 BESTÄLLNINGSPRIS I KRONOR               
009300     03 MOD-FLMANBK          PIC X.                                       
009400*                                 MANUELL BESTÄLLNINGSKVANTITET ?         
009500     03 MOD-KVAL             PIC 9.                                       
009600*                                 ANTAL LEVERANTÖRER                      
009700     03 MOD-TEMFSINF         PIC X(55).                                   
009800*                                 INFORMATIONSMEDDELANDE                  
009900*** END OF VILMAII-COPY LENGTH= 632 BYTES                                 
