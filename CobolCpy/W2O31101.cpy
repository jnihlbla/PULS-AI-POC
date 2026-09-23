000100 01  MOD-W2O31101-CTX.                                                    
000200*                                 MOD-COPYTEXT TILL W2031100              
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-IDKAMPRF-IN      PIC X(7).                                    
000800*                                 KAMPANJREFERENS                         
000900     03 MOD-IDKAMPRF-UT      PIC X(7).                                    
001000*                                 KAMPANJREFERENS                         
001100     03 MOD-IDDC-IN          PIC X(2).                                    
001200*                                 IDENTIFIERARE LAGER                     
001300     03 MOD-IDDC-UT          PIC X(2).                                    
001400*                                 IDENTIFIERARE LAGER                     
001500     03 MOD-IDARTNR-IN       PIC X(9).                                    
001600*                                 ARTIKELNUMMER                           
001700     03 MOD-IDARTNR-UT       PIC X(9).                                    
001800*                                 ARTIKELNUMMER                           
001900     03 MOD-TISTADAT-IN-ATTR PIC X(2).                                    
002000*                                 MFS ATTRIBUTFÄLT                        
002100     03 MOD-TISTADAT-IN      PIC 9(6).                                    
002200*                                 GENERELLT STARTDATUM                    
002300     03 MOD-TISTADAT-UT      PIC 9(6).                                    
002400*                                 GENERELLT STARTDATUM                    
002500     03 MOD-TISTODAT-IN-ATTR PIC X(2).                                    
002600*                                 MFS ATTRIBUTFÄLT                        
002700     03 MOD-TISTODAT-IN      PIC 9(6).                                    
002800*                                 GENERELLT STOPPDATUM                    
002900     03 MOD-TISTODAT-UT      PIC 9(6).                                    
003000*                                 GENERELLT STOPPDATUM                    
003100     03 MOD-CMD-UPD-RAD1-ATTR                                             
003200                             PIC X(2).                                    
003300*                                 MFS ATTRIBUTFÄLT                        
003400     03 MOD-CMD-UPD-RAD1     PIC X.                                       
003500     03 MOD-IDARTNR-RAD1-ATTR                                             
003600                             PIC X(2).                                    
003700*                                 MFS ATTRIBUTFÄLT                        
003800     03 MOD-IDARTNR-RAD1     PIC Z(7)9.                                   
003900*                                 ARTIKELNUMMER                           
004000     03 MOD-IDANSK-RAD1-ATTR PIC X(2).                                    
004100*                                 MFS ATTRIBUTFÄLT                        
004200     03 MOD-IDANSK-RAD1      PIC Z(2)9.                                   
004300*                                 ANSKAFFARNUMMER                         
004400     03 MOD-IDLEVNR-RAD1-ATTR                                             
004500                             PIC X(2).                                    
004600*                                 MFS ATTRIBUTFÄLT                        
004700     03 MOD-IDLEVNR-RAD1     PIC X(5).                                    
004800*                                 LEVERANTÖRNUMMER                        
004900     03 MOD-KVBEART-KAMP-RAD1-ATTR                                        
005000                             PIC X(2).                                    
005100*                                 MFS ATTRIBUTFÄLT                        
005200     03 MOD-KVBEART-KAMP-RAD1                                             
005300                             PIC Z(5)9.                                   
005400*                                 BESTÄLLT ANTAL FÖR KAMPANJEN            
005500     03 MOD-TIRES-RAD1-ATTR  PIC X(2).                                    
005600*                                 MFS ATTRIBUTFÄLT                        
005700     03 MOD-TIRES-RAD1       PIC 9(6).                                    
005800*                                 RESERVATIONSDATUM                       
005900     03 MOD-KVRESS-KAMP-RAD1-ATTR                                         
006000                             PIC X(2).                                    
006100*                                 MFS ATTRIBUTFÄLT                        
006200     03 MOD-KVRESS-KAMP-RAD1 PIC Z(6)9.                                   
006300*                                 TOTALT RESERVERAT FÖR KAMPANJ           
006400     03 MOD-KVBEART-KUND-RAD1-ATTR                                        
006500                             PIC X(2).                                    
006600*                                 MFS ATTRIBUTFÄLT                        
006700     03 MOD-KVBEART-KUND-RAD1                                             
006800                             PIC Z(5)9.                                   
006900*                                 AV KUND BESTÄLLT KVANTITET              
007000     03 MOD-W2O31101-001-GRP OCCURS 9 TIMES.                              
007100        05 MOD-CMD-UPD-ATTR  PIC X(2).                                    
007200*                                 MFS ATTRIBUTFÄLT                        
007300        05 MOD-CMD-UPD       PIC X.                                       
007400        05 MOD-IDARTNR-RAD   PIC Z(7)9.                                   
007500*                                 ARTIKELNUMMER                           
007600        05 MOD-IDANSK-RAD    PIC Z(2)9.                                   
007700*                                 ANSKAFFARNUMMER                         
007800        05 MOD-IDLEVNR-RAD   PIC X(5).                                    
007900*                                 LEVERANTÖRNUMMER                        
008000        05 MOD-KVBEART-KAMP-RAD                                           
008100                             PIC Z(5)9.                                   
008200*                                 BESTÄLLT ANTAL FÖR KAMPANJEN            
008300        05 MOD-TIRES-RAD     PIC 9(6).                                    
008400*                                 RESERVATIONSDATUM                       
008500        05 MOD-KVRESS-KAMP-RAD                                            
008600                             PIC Z(6)9.                                   
008700*                                 TOTALT RESERVERAT FÖR KAMPANJ           
008800        05 MOD-KVBEART-KUND-RAD                                           
008900                             PIC Z(5)9.                                   
009000*                                 AV KUND BESTÄLLT KVANTITET              
009100     03 MOD-IDARTNR-ATTR     PIC X(2).                                    
009200*                                 MFS ATTRIBUTFÄLT                        
009300     03 MOD-IDARTNR          PIC X(9).                                    
009400*                                 ARTIKELNUMMER                           
009500     03 MOD-KVBEART-KAMP-ATTR                                             
009600                             PIC X(2).                                    
009700*                                 MFS ATTRIBUTFÄLT                        
009800     03 MOD-KVBEART-KAMP     PIC Z(5)9.                                   
009900*                                 BESTÄLLT ANTAL FÖR KAMPANJEN            
010000     03 MOD-TIRES-ATTR       PIC X(2).                                    
010100*                                 MFS ATTRIBUTFÄLT                        
010200     03 MOD-TIRES            PIC 9(6).                                    
010300*                                 RESERVATIONSDATUM                       
010400     03 MOD-TEMFSINF         PIC X(55).                                   
010500*                                 INFORMATIONSMEDDELANDE                  
010600*** END OF VILMAII-COPY LENGTH= 644 BYTES                                 
