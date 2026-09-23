000100 01  W5O11101.                                                            
000200*                                 COPYTEXT FÖR MOD W5O11101               
000300     03 IDTRANS              PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 TEMFSFEL             PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 IDARTNR-IN           PIC X(9).                                    
000800*                                 ARTIKELNUMMER                           
000900     03 IDARTNR-UT           PIC X(9).                                    
001000*                                 ARTIKELNUMMER                           
001100     03 IDLEVNR-IN           PIC X(5).                                    
001200*                                 LEVERANTÖRNUMMER                        
001300     03 IDLEVNR-UT           PIC X(5).                                    
001400*                                 LEVERANTÖRNUMMER                        
001500     03 KDPRBEH-IN           PIC X.                                       
001600*                                 PRIS BEHANDLAD ARTIKEL                  
001700     03 KDPRBEH-UT           PIC X.                                       
001800*                                 PRIS BEHANDLAD ARTIKEL                  
001900     03 REAENDR-IN           PIC Z(3)9.9.                                 
002000*                                 ÄNDRINGSPROCENT                         
002100     03 REAENDR-UT           PIC Z(3)9.9.                                 
002200*                                 ÄNDRINGSPROCENT                         
002300     03 RETULF-LEV           PIC 9(3)V9(4).                               
002400*                                 TULLFAKTOR                              
002500     03 AREA.                                                             
002600        05 BEART             PIC X(25).                                   
002700*                                 ARTIKELBENÄMNING                        
002800        05 PRINK             PIC Z(6)9.9(2).                              
002900*                                 INKÖPSPRIS                              
003000        05 PRARTSTD          PIC Z(6)9.9(2).                              
003100*                                 ARTIKELSTANDARDPRIS                     
003200        05 PRARTBES-ATTR     PIC X(2).                                    
003300*                                 MFS ATTRIBUTFÄLT                        
003400        05 PRARTBES          PIC Z(6)9.9(2).                              
003500*                                 BESTÄLLNINGSPRIS I KRONOR               
003600        05 PRARTSJK-ATTR     PIC X(2).                                    
003700*                                 MFS ATTRIBUTFÄLT                        
003800        05 PRARTSJK          PIC Z(6)9.9(2).                              
003900*                                 ARTIKELNS SJÄLVKOSTNAD                  
004000        05 KDTIPPR-ATTR      PIC X(2).                                    
004100*                                 MFS ATTRIBUTFÄLT                        
004200        05 KDTIPPR           PIC X.                                       
004300*                                 TIPPAT PRIS KOD                         
004400        05 IDANSK            PIC Z(2)9.                                   
004500*                                 ANSKAFFARNUMMER                         
004600        05 IDINK             PIC X(4).                                    
004700*                                 INKÖPARNUMMER                           
004800        05 TIURPROD          PIC 9(4).                                    
004900*                                 DATUM UTGÅTT UR PROD   (ÅÅVV)           
005000        05 IDLEVNR           PIC X(5).                                    
005100*                                 LEVERANTÖRNUMMER                        
005200        05 KVLS-TOT          PIC -(7)9.                                   
005300*                                 LAGERSALDO                              
005400        05 KVPB-TOT          PIC Z(6)9.9.                                 
005500*                                 PERIODBEHOV (PROGNOS)                   
005600        05 IDLEVNR-SEN       PIC X(5).                                    
005700*                                 LEVERANTÖRNUMMER                        
005800        05 TIAVIDAT-SEN      PIC 9(6).                                    
005900*                                 AVISERINGSDATUM (YYMMDD)                
006000        05 PRIS-BEST         OCCURS 5 TIMES.                              
006100           07 BEST-PRIS-ATTR PIC X(2).                                    
006200*                                 MFS ATTRIBUTFÄLT                        
006300           07 BEST-PRIS.                                                  
006400              09 KDPRURSP-PR PIC X.                                       
006500*                                 PRISHÄRSTAMNING BESTÄLLNING             
006600              09 FILLER      PIC X.                                       
006700              09 TIPRLIST-PR PIC 9(6).                                    
006800*                                 PRISLISTEDATUM (AAMMDD)                 
006900              09 FILLER      PIC X.                                       
007000              09 IDLEVNR-PR  PIC X(5).                                    
007100*                                 LEVERANTÖRNUMMER                        
007200              09 FILLER      PIC X.                                       
007300              09 PRARTBES-PR PIC Z(6)9.9(2).                              
007400*                                 BESTÄLLNINGSPRIS I KRONOR               
007500              09 FILLER      PIC X.                                       
007600              09 PRARTBEL-PR PIC Z(7)9.9(5).                              
007700*                                 BESTPRIS LEVERANTÖRENS VALUTA           
007800              09 FILLER      PIC X(2).                                    
007900              09 KDSTATUS-PR PIC X(5).                                    
008000              09 FILLER      PIC X.                                       
008100              09 KDVALISO-PR PIC X(3).                                    
008200*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
008300              09 FILLER      PIC X.                                       
008400              09 KDFPKPRI-PR PIC X.                                       
008500*                                 OM FÖRPACKNING INGÅR I ARTPRIS          
008600        05 BESTALLNING       OCCURS 2 TIMES.                              
008700           07 IDBESTNR       PIC Z9(12).                                  
008800*                                 BESTÄLLNINGSNUMMER                      
008900           07 FILLER         PIC X(3).                                    
009000           07 IDLEVNR-B      PIC X(5).                                    
009100*                                 LEVERANTÖRNUMMER                        
009200           07 FILLER         PIC X.                                       
009300           07 TIBEST         PIC 9(6).                                    
009400*                                 BESTÄLLNINGSDATUM (ÅÅMMDD)              
009500           07 FILLER         PIC X.                                       
009600           07 KVBEST         PIC Z(6)9.                                   
009700*                                 BESTÄLLT ANTAL                          
009800        05 TEARTNOT-ATTR     PIC X(2).                                    
009900*                                 MFS ATTRIBUTFÄLT                        
010000        05 TEARTNOT          PIC X(40).                                   
010100*                                 ARTIKEL NOTERING                        
010200        05 FLART-RETULF-ATTR PIC X(2).                                    
010300*                                 MFS ATTRIBUTFÄLT                        
010400        05 FLART-RETULF      PIC X.                                       
010500        05 RETULF-ATTR       PIC X(2).                                    
010600*                                 MFS ATTRIBUTFÄLT                        
010700        05 RETULF            PIC Z(2)9.9(4).                              
010800*                                 TULLFAKTOR                              
010900        05 KDVALISO          PIC X(3).                                    
011000*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
011100        05 PRLFKST-ATTR      PIC X(2).                                    
011200*                                 MFS ATTRIBUTFÄLT                        
011300        05 PRLFKST           PIC Z(2)9.9(2).                              
011400*                                 LEVERANTÖRENS FÖRPACKN. KOSTNAD         
011500        05 REDIRLEV          PIC 9.9(2).                                  
011600*                                 DIREKTLEVERANSANDEL                     
011700        05 UPPDAT-RAD.                                                    
011800           07 KDPRURSP-U-ATTR                                             
011900                             PIC X(2).                                    
012000*                                 MFS ATTRIBUTFÄLT                        
012100           07 KDPRURSP-U     PIC X.                                       
012200*                                 PRISHÄRSTAMNING BESTÄLLNING             
012300           07 TIPRLIST-U-ATTR                                             
012400                             PIC X(2).                                    
012500*                                 MFS ATTRIBUTFÄLT                        
012600           07 TIPRLIST-U     PIC X(2).                                    
012700*                                 MFS BEHANDLING AV INPUTFÄLT             
012800           07 PRARTBEL-U-ATTR                                             
012900                             PIC X(2).                                    
013000*                                 MFS ATTRIBUTFÄLT                        
013100           07 PRARTBEL-U     PIC X(2).                                    
013200*                                 MFS BEHANDLING AV INPUTFÄLT             
013300           07 KDVALISO-U-ATTR                                             
013400                             PIC X(2).                                    
013500*                                 MFS ATTRIBUTFÄLT                        
013600           07 KDVALISO-U     PIC X(2).                                    
013700*                                 MFS BEHANDLING AV INPUTFÄLT             
013800           07 PRLFKST-U-ATTR PIC X(2).                                    
013900*                                 MFS ATTRIBUTFÄLT                        
014000           07 PRLFKST-U      PIC X(2).                                    
014100*                                 MFS BEHANDLING AV INPUTFÄLT             
014200           07 FLPRIBES-U-ATTR                                             
014300                             PIC X(2).                                    
014400*                                 MFS ATTRIBUTFÄLT                        
014500           07 FLPRIBES-U     PIC X.                                       
014600*                                 SKAPA EJ BESTÄLLNINGSPRIS-INFO          
014700           07 IDLEVNR-U-ATTR PIC X(2).                                    
014800*                                 MFS ATTRIBUTFÄLT                        
014900           07 IDLEVNR-U      PIC X(5).                                    
015000*                                 LEVERANTÖRNUMMER                        
015100           07 TEARTNOT-U-ATTR                                             
015200                             PIC X(2).                                    
015300*                                 MFS ATTRIBUTFÄLT                        
015400           07 TEARTNOT-U     PIC X(2).                                    
015500*                                 MFS BEHANDLING AV INPUTFÄLT             
015600           07 SVAR-U         PIC X(4).                                    
015700           07 FLPRIGO-U-ATTR PIC X(2).                                    
015800*                                 MFS ATTRIBUTFÄLT                        
015900           07 FLPRIGO-U      PIC X(2).                                    
016000*                                 MFS BEHANDLING AV INPUTFÄLT             
016100           07 KDFPKPRI-U-ATTR                                             
016200                             PIC X(2).                                    
016300*                                 MFS ATTRIBUTFÄLT                        
016400           07 KDFPKPRI-U     PIC X.                                       
016500*                                 OM FÖRPACKNING INGÅR I ARTPRIS          
016600     03 TEMFSINF             PIC X(55).                                   
016700*                                 INFORMATIONSMEDDELANDE                  
016800*** END OF VILMAII-COPY LENGTH= 725 BYTES                                 
