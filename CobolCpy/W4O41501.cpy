000100 01  MOD-W4O41501.                                                        
000200*                                                                         
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-IDDC-IN          PIC X(2).                                    
000800*                                 IDENTIFIERARE LAGER                     
000900     03 MOD-IDDC-UT          PIC X(2).                                    
001000*                                 IDENTIFIERARE LAGER                     
001100     03 MOD-IDDISTR-IN       PIC X(4).                                    
001200*                                 DISTRIKTNUMMER                          
001300     03 MOD-IDDISTR-UT       PIC X(4).                                    
001400*                                 DISTRIKTNUMMER                          
001500     03 MOD-IDKUNDNR-IN      PIC X(6).                                    
001600*                                 KUNDNUMMER                              
001700     03 MOD-IDKUNDNR-UT      PIC X(6).                                    
001800*                                 KUNDNUMMER                              
001900     03 MOD-INFO-RAD         OCCURS 11 TIMES.                             
002000*                                 RADINFORMATION                          
002100        05 MOD-IDKUNDNR      PIC Z(5)9.                                   
002200*                                 KUNDNUMMER                              
002300        05 MOD-IDGMTOMR      PIC Z(3)9.                                   
002400*                                 GODSMOTTAGAREOMRÅDE                     
002500        05 MOD-KVDAGAR-TRP-DAY                                            
002600                             PIC Z9.                                      
002700*                                 TRP DAGAR DC TILL KUND (DAG)            
002800        05 MOD-IDPKLTAB      PIC X(2).                                    
002900*                                 PRODUKTIONSKLASSTABELLSID               
003000        05 MOD-IDPRCTAB      PIC Z9.                                      
003100*                                 PRCTABELLIDENTITET                      
003200        05 MOD-KDGENFRA-VOR  PIC Z9.                                      
003300*                                 NORMAL FRAKT VOR-ORDER                  
003400        05 MOD-KDGENFRA-DO   PIC Z9.                                      
003500*                                 NORMAL FRAKT DAGORDER                   
003600        05 MOD-KDGENFRA-MO   PIC Z9.                                      
003700*                                 NORMAL FRAKT MÅNADSORDER KL 2-4         
003800        05 MOD-KVLEDTIM-0    PIC Z(2)9.9(2).                              
003900*                                 LEDTID KL 0                             
004000        05 MOD-KVLEDTIM-1    PIC Z(2)9.9(2).                              
004100*                                 LEDTID KL 1                             
004200        05 MOD-KVLEDTIM-2    PIC Z(2)9.9(2).                              
004300*                                 LEDTID KL 2                             
004400        05 MOD-KVLEDTIM-3    PIC Z(2)9.9(2).                              
004500*                                 LEDTID KL 3                             
004600        05 MOD-KVLEDTIM-4    PIC Z(2)9.9(2).                              
004700*                                 LEDTID KL 4                             
004800        05 MOD-KDFORSKN      PIC 9(3).                                    
004900*                                 FÖRSÄKRANSKOD                           
005000        05 MOD-KDSPFKTK      PIC 9.                                       
005100*                                                    KDSPFKTK-002         
005200*                                 INSTRUKTION SPECIALFAKTURA              
005300        05 MOD-KDTULLVE      PIC 9.                                       
005400*                                 TYP AV PRIS PÅ TULLFAKTURA              
005500        05 MOD-KDMOMSIN      PIC 9.                                       
005600*                                 MOMSINSTRUKTION                         
005700        05 MOD-KDROPACK-DAG  PIC X.                                       
005800*                                 FRISLÄPPNINGSKOD RO/DO DAGORDER         
005900        05 MOD-KDROPACK-BULK PIC X.                                       
006000*                                 FRISLÄPPNINGSKOD RO/DO BULK ORD         
006100     03 MOD-IDGMTOMR-UT      PIC Z(3)9.                                   
006200*                                 GODSMOTTAGAREOMRÅDE                     
006300     03 MOD-KVDAGAR-TRP-DAY-UT                                            
006400                             PIC Z9.                                      
006500*                                 TRP DAGAR DC TILL KUND (DAG)            
006600     03 MOD-IDPKLTAB-UT      PIC X(2).                                    
006700*                                 PRODUKTIONSKLASSTABELLSID               
006800     03 MOD-IDPRCTAB-UT      PIC Z9.                                      
006900*                                 PRCTABELLIDENTITET                      
007000     03 MOD-KDGENFRA-VOR-UT  PIC Z9.                                      
007100*                                 NORMAL FRAKT VOR-ORDER                  
007200     03 MOD-KDGENFRA-DO-UT   PIC Z9.                                      
007300*                                 NORMAL FRAKT DAGORDER                   
007400     03 MOD-KDGENFRA-MO-UT   PIC Z9.                                      
007500*                                 NORMAL FRAKT MÅNADSORDER KL 2-4         
007600     03 MOD-KVLEDTIM-0-UT    PIC Z(2)9.9(2).                              
007700*                                 LEDTID KL 0                             
007800     03 MOD-KVLEDTIM-1-UT    PIC Z(2)9.9(2).                              
007900*                                 LEDTID KL 1                             
008000     03 MOD-KVLEDTIM-2-UT    PIC Z(2)9.9(2).                              
008100*                                 LEDTID KL 2                             
008200     03 MOD-KVLEDTIM-3-UT    PIC Z(2)9.9(2).                              
008300*                                 LEDTID KL 3                             
008400     03 MOD-KVLEDTIM-4-UT    PIC Z(2)9.9(2).                              
008500*                                 LEDTID KL 4                             
008600     03 MOD-KDFORSKN-UT      PIC 9(3).                                    
008700*                                 FÖRSÄKRANSKOD                           
008800     03 MOD-KDSPFKTK-UT      PIC 9.                                       
008900*                                                    KDSPFKTK-002         
009000*                                 INSTRUKTION SPECIALFAKTURA              
009100     03 MOD-KDTULLVE-UT      PIC 9.                                       
009200*                                 TYP AV PRIS PÅ TULLFAKTURA              
009300     03 MOD-KDMOMSIN-UT      PIC 9.                                       
009400*                                 MOMSINSTRUKTION                         
009500     03 MOD-KDROPACK-DAG-UT  PIC X.                                       
009600*                                 FRISLÄPPNINGSKOD RO/DO DAGORDER         
009700     03 MOD-KDROPACK-BULK-UT PIC X.                                       
009800*                                 FRISLÄPPNINGSKOD RO/DO BULK ORD         
009900     03 MOD-KDCMD-IN-ATTR    PIC X(2).                                    
010000*                                 MFS ATTRIBUTFÄLT                        
010100     03 MOD-KDCMD-IN         PIC X.                                       
010200*                                 RAD-UPPDATERINGSKOMMANDO                
010300*                                  BLANK  = INGENTING                     
010400*                                  D , B  = DELETE                        
010500*                                  R , Ä  = REPLACE                       
010600*                                  I,N,A  = INSERT                        
010700*                                  S , V  = SELECT                        
010800*                                  P , P  = PRINT                         
010900*                                  C , K  = COPY                          
011000     03 MOD-KUNDNR-IN-ATTR   PIC X(2).                                    
011100*                                 MFS ATTRIBUTFÄLT                        
011200     03 MOD-KUNDNR-IN        PIC X(6).                                    
011300*                                 KUNDNUMMER                              
011400     03 MOD-IDGMTOMR-IN-ATTR PIC X(2).                                    
011500*                                 MFS ATTRIBUTFÄLT                        
011600     03 MOD-IDGMTOMR-IN      PIC X(4).                                    
011700*                                 GODSMOTTAGAREOMRÅDE                     
011800     03 MOD-KVDAGAR-TRP-DAY-IN-ATTR                                       
011900                             PIC X(2).                                    
012000*                                 MFS ATTRIBUTFÄLT                        
012100     03 MOD-KVDAGAR-TRP-DAY-IN                                            
012200                             PIC Z9.                                      
012300*                                 TRP DAGAR DC TILL KUND (DAG)            
012400     03 MOD-IDPKLTAB-IN-ATTR PIC X(2).                                    
012500*                                 MFS ATTRIBUTFÄLT                        
012600     03 MOD-IDPKLTAB-IN      PIC X(2).                                    
012700*                                 PRODUKTIONSKLASSTABELLSID               
012800     03 MOD-IDPRCTAB-IN-ATTR PIC X(2).                                    
012900*                                 MFS ATTRIBUTFÄLT                        
013000     03 MOD-IDPRCTAB-IN      PIC X(2).                                    
013100*                                 PRCTABELLIDENTITET                      
013200     03 MOD-KDGENFRA-VOR-IN-ATTR                                          
013300                             PIC X(2).                                    
013400*                                 MFS ATTRIBUTFÄLT                        
013500     03 MOD-KDGENFRA-VOR-IN  PIC X(2).                                    
013600*                                 NORMAL FRAKT VOR-ORDER                  
013700     03 MOD-KDGENFRA-DO-IN-ATTR                                           
013800                             PIC X(2).                                    
013900*                                 MFS ATTRIBUTFÄLT                        
014000     03 MOD-KDGENFRA-DO-IN   PIC X(2).                                    
014100*                                 NORMAL FRAKT DAGORDER                   
014200     03 MOD-KDGENFRA-MO-IN-ATTR                                           
014300                             PIC X(2).                                    
014400*                                 MFS ATTRIBUTFÄLT                        
014500     03 MOD-KDGENFRA-MO-IN   PIC X(2).                                    
014600*                                 NORMAL FRAKT MÅNADSORDER KL 2-4         
014700     03 MOD-KVLEDTIM-0-IN-ATTR                                            
014800                             PIC X(2).                                    
014900*                                 MFS ATTRIBUTFÄLT                        
015000     03 MOD-KVLEDTIM-0-IN    PIC Z(2)9.9(2).                              
015100*                                 LEDTID KL 0                             
015200     03 MOD-KVLEDTIM-1-IN-ATTR                                            
015300                             PIC X(2).                                    
015400*                                 MFS ATTRIBUTFÄLT                        
015500     03 MOD-KVLEDTIM-1-IN    PIC Z(2)9.9(2).                              
015600*                                 LEDTID KL 1                             
015700     03 MOD-KVLEDTIM-2-IN-ATTR                                            
015800                             PIC X(2).                                    
015900*                                 MFS ATTRIBUTFÄLT                        
016000     03 MOD-KVLEDTIM-2-IN    PIC Z(2)9.9(2).                              
016100*                                 LEDTID KL 2                             
016200     03 MOD-KVLEDTIM-3-IN-ATTR                                            
016300                             PIC X(2).                                    
016400*                                 MFS ATTRIBUTFÄLT                        
016500     03 MOD-KVLEDTIM-3-IN    PIC Z(2)9.9(2).                              
016600*                                 LEDTID KL 3                             
016700     03 MOD-KVLEDTIM-4-IN-ATTR                                            
016800                             PIC X(2).                                    
016900*                                 MFS ATTRIBUTFÄLT                        
017000     03 MOD-KVLEDTIM-4-IN    PIC Z(2)9.9(2).                              
017100*                                 LEDTID KL 4                             
017200     03 MOD-KDFORSKN-IN-ATTR PIC X(2).                                    
017300*                                 MFS ATTRIBUTFÄLT                        
017400     03 MOD-KDFORSKN-IN      PIC X(3).                                    
017500*                                 FÖRSÄKRANSKOD                           
017600     03 MOD-KDSPFKTK-IN-ATTR PIC X(2).                                    
017700*                                 MFS ATTRIBUTFÄLT                        
017800     03 MOD-KDSPFKTK-IN      PIC 9.                                       
017900*                                                    KDSPFKTK-002         
018000*                                 INSTRUKTION SPECIALFAKTURA              
018100     03 MOD-KDTULLVE-IN-ATTR PIC X(2).                                    
018200*                                 MFS ATTRIBUTFÄLT                        
018300     03 MOD-KDTULLVE-IN      PIC 9.                                       
018400*                                 TYP AV PRIS PÅ TULLFAKTURA              
018500     03 MOD-KDMOMSIN-IN-ATTR PIC X(2).                                    
018600*                                 MFS ATTRIBUTFÄLT                        
018700     03 MOD-KDMOMSIN-IN      PIC X.                                       
018800*                                 MOMSINSTRUKTION                         
018900     03 MOD-KDROPACK-DAG-IN-ATTR                                          
019000                             PIC X(2).                                    
019100*                                 MFS ATTRIBUTFÄLT                        
019200     03 MOD-KDROPACK-DAG-IN  PIC X.                                       
019300*                                 FRISLÄPPNINGSKOD RO/DO DAGORDER         
019400     03 MOD-KDROPACK-BULK-IN-ATTR                                         
019500                             PIC X(2).                                    
019600*                                 MFS ATTRIBUTFÄLT                        
019700     03 MOD-KDROPACK-BULK-IN PIC X.                                       
019800*                                 FRISLÄPPNINGSKOD RO/DO BULK ORD         
019900     03 MOD-TEMFSINF         PIC X(55).                                   
020000*                                 INFORMATIONSMEDDELANDE                  
020100*** END OF VILMAII-COPY LENGTH= 938 BYTES                                 
