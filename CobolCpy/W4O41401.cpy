000100 01  MOD-W4O41401.                                                        
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
001500     03 MOD-TABELL-A         OCCURS 8 TIMES.                              
001600*                                 RADINFORMATION                          
001700        05 MOD-IDDC-BULK-ATTR                                             
001800                             PIC X(2).                                    
001900*                                 MFS ATTRIBUTFÄLT                        
002000        05 MOD-IDDC-BULK     PIC X(2).                                    
002100*                                 IDENTIFIERARE BULKORDERLAGER            
002200        05 MOD-KDGENFRA-MO   PIC Z9.                                      
002300*                                 NORMAL FRAKT MÅNADSORDER KL 2-4         
002400        05 MOD-IDDC-DAY-ATTR PIC X(2).                                    
002500*                                 MFS ATTRIBUTFÄLT                        
002600        05 MOD-IDDC-DAY      PIC X(2).                                    
002700*                                 IDENTIFIERARE DAGORDERLAGER             
002800        05 MOD-KDGENFRA-DO   PIC Z9.                                      
002900*                                 NORMAL FRAKT DAGORDER                   
003000        05 MOD-KVDAGAR-DAY   PIC Z9.                                      
003100*                                 ANTAL DAGAR                             
003200        05 MOD-IDDC-VOR-ATTR PIC X(2).                                    
003300*                                 MFS ATTRIBUTFÄLT                        
003400        05 MOD-IDDC-VOR      PIC X(2).                                    
003500*                                 IDENTIFIERARE VORORDERLAGER             
003600        05 MOD-KDGENFRA-VOR  PIC Z9.                                      
003700*                                 NORMAL FRAKT VOR-ORDER                  
003800     03 MOD-TABELL-B         OCCURS 7 TIMES.                              
003900*                                 RADINFORMATION                          
004000        05 MOD-IDDC-BULK-B-ATTR                                           
004100                             PIC X(2).                                    
004200*                                 MFS ATTRIBUTFÄLT                        
004300        05 MOD-IDDC-BULK-B   PIC X(2).                                    
004400*                                 IDENTIFIERARE BULKORDERLAGER            
004500        05 MOD-KDGENFRA-MO-B PIC Z9.                                      
004600*                                 NORMAL FRAKT MÅNADSORDER KL 2-4         
004700        05 MOD-IDDC-DAY-B-ATTR                                            
004800                             PIC X(2).                                    
004900*                                 MFS ATTRIBUTFÄLT                        
005000        05 MOD-IDDC-DAY-B    PIC X(2).                                    
005100*                                 IDENTIFIERARE DAGORDERLAGER             
005200        05 MOD-KDGENFRA-DO-B PIC Z9.                                      
005300*                                 NORMAL FRAKT DAGORDER                   
005400        05 MOD-KVDAGAR-DAY-B PIC Z9.                                      
005500*                                 ANTAL DAGAR                             
005600        05 MOD-IDDC-VOR-B-ATTR                                            
005700                             PIC X(2).                                    
005800*                                 MFS ATTRIBUTFÄLT                        
005900        05 MOD-IDDC-VOR-B    PIC X(2).                                    
006000*                                 IDENTIFIERARE VORORDERLAGER             
006100        05 MOD-KDGENFRA-VOR-B                                             
006200                             PIC Z9.                                      
006300*                                 NORMAL FRAKT VOR-ORDER                  
006400     03 MOD-TABELL-DDGS      OCCURS 7 TIMES.                              
006500*                                 RADINFORMATION                          
006600        05 MOD-IDDC-DDGS-ATTR                                             
006700                             PIC X(2).                                    
006800*                                 MFS ATTRIBUTFÄLT                        
006900        05 MOD-IDDC-DDGS     PIC X(2).                                    
007000*                                 IDENTIFIERARE LAGER                     
007100        05 MOD-KDGENFRA-MO-DDGS                                           
007200                             PIC Z9.                                      
007300*                                 NORMAL FRAKT MÅNADSORDER KL 2-4         
007400        05 MOD-KVDAGAR-DDGS  PIC Z9.                                      
007500*                                 ANTAL DAGAR                             
007600     03 MOD-KVDAGAR-DOW-ATTR PIC X(2).                                    
007700*                                 MFS ATTRIBUTFÄLT                        
007800     03 MOD-KVDAGAR-DOW      PIC Z9.                                      
007900*                                 ANTAL DAGAR FÖRE DC CLEARING            
008000     03 MOD-IDUSER-DCUPD     PIC X(8).                                    
008100*                                 USER SOM UPPDATERAT DC-STYRNING         
008200     03 MOD-TIAAMMDD-DCUPD   PIC 9(6).                                    
008300*                                 DATUM FÖR SISTA DC-STYR UPPDAT          
008400     03 MOD-TIFAKT           PIC 9(6).                                    
008500*                                 FAKTURERINGSDATUM (ÅÅMMDD)              
008600     03 MOD-IDDC-DAY-ALT-ATTR                                             
008700                             PIC X(2).                                    
008800*                                 MFS ATTRIBUTFÄLT                        
008900     03 MOD-IDDC-DAY-ALT     PIC X(2).                                    
009000*                                 DAG DC BARA UNDER GIVNA TIDER           
009100     03 MOD-TIHHMM-START-ATTR                                             
009200                             PIC X(2).                                    
009300*                                 MFS ATTRIBUTFÄLT                        
009400     03 MOD-TIHHMM-START     PIC 9(4).                                    
009500*                                 KLOCKSLAG (TIMMAR/MIN.) START           
009600     03 MOD-TIHHMM-STOP-ATTR PIC X(2).                                    
009700*                                 MFS ATTRIBUTFÄLT                        
009800     03 MOD-TIHHMM-STOP      PIC 9(4).                                    
009900*                                 KLOCKSLAG (TIMMAR/MIN.) STOP            
010000     03 MOD-TISTADAT-ATTR    PIC X(2).                                    
010100*                                 MFS ATTRIBUTFÄLT                        
010200     03 MOD-TISTADAT         PIC 9(6).                                    
010300*                                 GENERELLT STARTDATUM                    
010400     03 MOD-TISTODAT-ATTR    PIC X(2).                                    
010500*                                 MFS ATTRIBUTFÄLT                        
010600     03 MOD-TISTODAT         PIC 9(6).                                    
010700*                                 GENERELLT STOPPDATUM                    
010800     03 MOD-TEMFSINF-ATTR    PIC X(2).                                    
010900*                                 MFS ATTRIBUTFÄLT                        
011000     03 MOD-TEMFSINF         PIC X(55).                                   
011100*                                 INFORMATIONSMEDDELANDE                  
011200*** END OF VILMAII-COPY LENGTH= 533 BYTES                                 
