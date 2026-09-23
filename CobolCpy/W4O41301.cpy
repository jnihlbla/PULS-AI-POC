000100 01  MOD-W4O41301.                                                        
000200*                                                                         
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
001500     03 MOD-IDDC-RET-ATTR    PIC X(2).                                    
001600*                                 MFS ATTRIBUTFÄLT                        
001700     03 MOD-IDDC-RET         PIC X(2).                                    
001800*                                 MOTTAGANDE LAGER FÖR RETURER            
001900     03 MOD-FLRETFG-ATTR     PIC X(2).                                    
002000*                                 MFS ATTRIBUTFÄLT                        
002100     03 MOD-FLRETFG          PIC X.                                       
002200*                                 FARLIGT GODS RETUR FLAGGA               
002300     03 MOD-FLOKFAK-G-ATTR   PIC X(2).                                    
002400*                                 MFS ATTRIBUTFÄLT                        
002500     03 MOD-FLOKFAK-G        PIC X.                                       
002600*                                 FLAGGA FAKTURATYP G GODKÄND             
002700     03 MOD-FLOKFAK-K-ATTR   PIC X(2).                                    
002800*                                 MFS ATTRIBUTFÄLT                        
002900     03 MOD-FLOKFAK-K        PIC X.                                       
003000*                                 FLAGGA FAKTURATYP K GODKÄND             
003100     03 MOD-FLOKFAK-N-ATTR   PIC X(2).                                    
003200*                                 MFS ATTRIBUTFÄLT                        
003300     03 MOD-FLOKFAK-N        PIC X.                                       
003400*                                 FLAGGA FAKTURATYP N GODKÄND             
003500     03 MOD-FLOKFAK-R-ATTR   PIC X(2).                                    
003600*                                 MFS ATTRIBUTFÄLT                        
003700     03 MOD-FLOKFAK-R        PIC X.                                       
003800*                                 FLAGGA FAKTURATYP R GODKÄND             
003900     03 MOD-KDGENFAK-ATTR    PIC X(2).                                    
004000*                                 MFS ATTRIBUTFÄLT                        
004100     03 MOD-KDGENFAK         PIC X.                                       
004200*                                 NORMAL FAKTURATYP                       
004300     03 MOD-FLLDCKND-ATTR    PIC X(2).                                    
004400*                                 MFS ATTRIBUTFÄLT                        
004500     03 MOD-FLLDCKND         PIC X.                                       
004600*                                 FL LDC-KUND                             
004700     03 MOD-KVDAGAR-SDC-ATTR PIC X(2).                                    
004800*                                 MFS ATTRIBUTFÄLT                        
004900     03 MOD-KVDAGAR-SDC      PIC Z9.                                      
005000*                                 SDC-DAGAR                               
005100     03 MOD-KVDAGAR-CDC-ATTR PIC X(2).                                    
005200*                                 MFS ATTRIBUTFÄLT                        
005300     03 MOD-KVDAGAR-CDC      PIC Z9.                                      
005400*                                 CDC-DAGAR                               
005500     03 MOD-FLRETUR-ATTR     PIC X(2).                                    
005600*                                 MFS ATTRIBUTFÄLT                        
005700     03 MOD-FLRETUR          PIC X.                                       
005800*                                 FLAGGA RETUR OK.                        
005900     03 MOD-RETDC-GRP        OCCURS 3 TIMES.                              
006000*                                 RECEIVING DC FOR 72-RETURNS             
006100        05 MOD-IDDC-RET72-ATTR                                            
006200                             PIC X(2).                                    
006300*                                 MFS ATTRIBUTFÄLT                        
006400        05 MOD-IDDC-RET72    PIC X(2).                                    
006500*                                 MOTTAGANDE LAGER FÖR 72-RETURER         
006600     03 MOD-RFSDC-GRP        OCCURS 4 TIMES.                              
006700*                                 RFS DAY CALC FOR DIFFERENT DC           
006800        05 MOD-IDDC-RFS-ATTR PIC X(2).                                    
006900*                                 MFS ATTRIBUTFÄLT                        
007000        05 MOD-IDDC-RFS      PIC X(2).                                    
007100*                                 DC FÖR RFS DAGAR FÖRE REPDAG            
007200        05 MOD-KVDAGAR-RFS-ATTR                                           
007300                             PIC X(2).                                    
007400*                                 MFS ATTRIBUTFÄLT                        
007500        05 MOD-KVDAGAR-RFS   PIC 9.                                       
007600*                                 ANT DGR FÖR RFS/DC FÖRE REPDAT          
007700     03 MOD-KVDAGAR-RFS-DEF-ATTR                                          
007800                             PIC X(2).                                    
007900*                                 MFS ATTRIBUTFÄLT                        
008000     03 MOD-KVDAGAR-RFS-DEF  PIC 9.                                       
008100*                                 DEFAULT DAGAR RFS FÖRE REPDAT           
008200     03 MOD-FLKVBRYT-ORDKL1-ATTR                                          
008300                             PIC X(2).                                    
008400*                                 MFS ATTRIBUTFÄLT                        
008500     03 MOD-FLKVBRYT-ORDKL1  PIC X.                                       
008600*                                 KVANTITET BRYTES                        
008700     03 MOD-FLKVBRYT-ORDKL2-ATTR                                          
008800                             PIC X(2).                                    
008900*                                 MFS ATTRIBUTFÄLT                        
009000     03 MOD-FLKVBRYT-ORDKL2  PIC X.                                       
009100*                                 KVANTITET BRYTES                        
009200     03 MOD-FLKVBRYT-ORDKL3-ATTR                                          
009300                             PIC X(2).                                    
009400*                                 MFS ATTRIBUTFÄLT                        
009500     03 MOD-FLKVBRYT-ORDKL3  PIC X.                                       
009600*                                 KVANTITET BRYTES                        
009700     03 MOD-FLKVBRYT-ORDKL4-ATTR                                          
009800                             PIC X(2).                                    
009900*                                 MFS ATTRIBUTFÄLT                        
010000     03 MOD-FLKVBRYT-ORDKL4  PIC X.                                       
010100*                                 KVANTITET BRYTES                        
010200     03 MOD-FLCOD            PIC X.                                       
010300*                                 KONTANTBETALANDE KUND                   
010400     03 MOD-TISTADAT-COD     PIC 9(6).                                    
010500*                                 STARTDATUM FÖR KONTANTBETALANDE         
010600*                                 KUND                                    
010700     03 MOD-TISTATID-COD     PIC X(6).                                    
010800*                                 STARTTIDPUNKT FÖR                       
010900*                                 KONTANTBETALANDE KUND                   
011000     03 MOD-TISTODAT-COD     PIC 9(6).                                    
011100*                                 STOPPDATUM FÖR KONTANTBETALANDE         
011200*                                 KUND                                    
011300     03 MOD-TISTOTID-COD     PIC X(6).                                    
011400*                                 STOPPTIDPUNKT FÖR                       
011500*                                 KONTANTBETALANDE KUND                   
011600     03 MOD-TIFAKT           PIC 9(6).                                    
011700*                                 FAKTURERINGSDATUM (ÅÅMMDD)              
011800     03 MOD-TISTADAT         PIC 9(6).                                    
011900*                                 GENERELLT STARTDATUM                    
012000     03 MOD-TISTODAT         PIC 9(6).                                    
012100*                                 GENERELLT STOPPDATUM                    
012200     03 MOD-FLAUTREM-ATTR    PIC X(2).                                    
012300*                                 MFS ATTRIBUTFÄLT                        
012400     03 MOD-FLAUTREM         PIC X.                                       
012500*                                 AUTOMATISK REMISS (Y/N)                 
012600     03 MOD-TEMFSINF-ATTR    PIC X(2).                                    
012700*                                 MFS ATTRIBUTFÄLT                        
012800     03 MOD-TEMFSINF         PIC X(55).                                   
012900*                                 INFORMATIONSMEDDELANDE                  
013000*** END OF VILMAII-COPY LENGTH= 258 BYTES                                 
