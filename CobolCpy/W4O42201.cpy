000100 01  MOD-W4O42201.                                                        
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
001500     03 MOD-IDDC-IN          PIC X(2).                                    
001600*                                 IDENTIFIERARE LAGER                     
001700     03 MOD-IDDC-UT          PIC X(2).                                    
001800*                                 IDENTIFIERARE LAGER                     
001900     03 MOD-KDFRAKT-IN       PIC X(2).                                    
002000*                                 FRAKTSÄTT DC TILL KUND                  
002100     03 MOD-KDFRAKT-UT       PIC X(2).                                    
002200*                                 FRAKTSÄTT DC TILL KUND                  
002300     03 MOD-BEGMT-RAD1       PIC X(35).                                   
002400*                                 GODSMOTTAGARNAMN RAD 1                  
002500     03 MOD-BEGMT-RAD2       PIC X(35).                                   
002600*                                 GODSMOTTAGARNAMN RAD 2                  
002700     03 MOD-ADGMT-GATA       PIC X(35).                                   
002800*                                 GODSMOTTAGARADRESS GATA                 
002900     03 MOD-ADGMT-PADR       PIC X(35).                                   
003000*                                 GODSMOTTAGARADRESS POSTADRESS           
003100     03 MOD-ADGMT-LAND       PIC X(35).                                   
003200*                                 GODSMOTTAGARADRESS LAND                 
003300     03 MOD-KDFDKRAV         PIC Z(2)9.                                   
003400*                                 TRANSPORTFÖRPACKNINGSKOD                
003500     03 MOD-BEFDKRAV         PIC X(30).                                   
003600     03 MOD-KDBETVIL         PIC X(4).                                    
003700*                                 BETALNINGSVILLKOR SAP                   
003800     03 MOD-BEBETVIL-X27     PIC X(27).                                   
003900     03 MOD-KDTRPKAT         PIC X.                                       
004000*                                 TRANSPORTKATEGORI                       
004100     03 MOD-IDTRP-0.                                                      
004200*                                 TRANSPORTIDENTITET KLASS 0              
004300        05 MOD-IDTRPLOS-0    PIC X(3).                                    
004400*                                 TRANSPORTLÖSNING                        
004500        05 MOD-IDTRPVAR-0    PIC X(2).                                    
004600*                                 TRANSPORTLÖSNINGSGRUPP                  
004700     03 MOD-BETRPFIR-0       PIC X(15).                                   
004800*                                 TRANSPORTFIRMANS NAMN                   
004900     03 MOD-IDTRP-0-ALT.                                                  
005000*                                 TRANSPORT-ID ALTERNATIV                 
005100        05 MOD-IDTRPLOS-ALT  PIC X(3).                                    
005200*                                 TRANSPORTLÖSNING                        
005300        05 MOD-IDTRPVAR-ALT  PIC X(2).                                    
005400*                                 TRANSPORTLÖSNINGSGRUPP                  
005500     03 MOD-BETRPFIR-0-ALT   PIC X(15).                                   
005600*                                 TRANSPORTFIRMANS NAMN                   
005700     03 MOD-IDTRP-1.                                                      
005800*                                 TRANSPORTIDENTITET KLASS 1              
005900        05 MOD-IDTRPLOS-1    PIC X(3).                                    
006000*                                 TRANSPORTLÖSNING                        
006100        05 MOD-IDTRPVAR-1    PIC X(2).                                    
006200*                                 TRANSPORTLÖSNINGSGRUPP                  
006300     03 MOD-BETRPFIR-1       PIC X(15).                                   
006400*                                 TRANSPORTFIRMANS NAMN                   
006500     03 MOD-IDTRP-1-ALT.                                                  
006600*                                 TRANSPORT-ID ALTERNATIV                 
006700        05 MOD-IDTRPLOS-ALT  PIC X(3).                                    
006800*                                 TRANSPORTLÖSNING                        
006900        05 MOD-IDTRPVAR-ALT  PIC X(2).                                    
007000*                                 TRANSPORTLÖSNINGSGRUPP                  
007100     03 MOD-BETRPFIR-1-ALT   PIC X(15).                                   
007200*                                 TRANSPORTFIRMANS NAMN                   
007300     03 MOD-IDTRP-2.                                                      
007400*                                 TRANSPORTIDENTITET KLASS 2              
007500        05 MOD-IDTRPLOS-2    PIC X(3).                                    
007600*                                 TRANSPORTLÖSNING                        
007700        05 MOD-IDTRPVAR-2    PIC X(2).                                    
007800*                                 TRANSPORTLÖSNINGSGRUPP                  
007900     03 MOD-BETRPFIR-2       PIC X(15).                                   
008000*                                 TRANSPORTFIRMANS NAMN                   
008100     03 MOD-IDTRP-2-ALT.                                                  
008200*                                 TRANSPORT-ID ALTERNATIV                 
008300        05 MOD-IDTRPLOS-ALT  PIC X(3).                                    
008400*                                 TRANSPORTLÖSNING                        
008500        05 MOD-IDTRPVAR-ALT  PIC X(2).                                    
008600*                                 TRANSPORTLÖSNINGSGRUPP                  
008700     03 MOD-BETRPFIR-2-ALT   PIC X(15).                                   
008800*                                 TRANSPORTFIRMANS NAMN                   
008900     03 MOD-IDTRP-3.                                                      
009000*                                 TRANSPORTIDENTITET KLASS 3              
009100        05 MOD-IDTRPLOS-3    PIC X(3).                                    
009200*                                 TRANSPORTLÖSNING                        
009300        05 MOD-IDTRPVAR-3    PIC X(2).                                    
009400*                                 TRANSPORTLÖSNINGSGRUPP                  
009500     03 MOD-BETRPFIR-3       PIC X(15).                                   
009600*                                 TRANSPORTFIRMANS NAMN                   
009700     03 MOD-IDTRP-3-ALT.                                                  
009800*                                 TRANSPORT-ID ALTERNATIV                 
009900        05 MOD-IDTRPLOS-ALT  PIC X(3).                                    
010000*                                 TRANSPORTLÖSNING                        
010100        05 MOD-IDTRPVAR-ALT  PIC X(2).                                    
010200*                                 TRANSPORTLÖSNINGSGRUPP                  
010300     03 MOD-BETRPFIR-3-ALT   PIC X(15).                                   
010400*                                 TRANSPORTFIRMANS NAMN                   
010500     03 MOD-IDTRP-4.                                                      
010600*                                 TRANSPORTIDENTITET KLASS 4              
010700        05 MOD-IDTRPLOS-4    PIC X(3).                                    
010800*                                 TRANSPORTLÖSNING                        
010900        05 MOD-IDTRPVAR-4    PIC X(2).                                    
011000*                                 TRANSPORTLÖSNINGSGRUPP                  
011100     03 MOD-BETRPFIR-4       PIC X(15).                                   
011200*                                 TRANSPORTFIRMANS NAMN                   
011300     03 MOD-IDTRP-4-ALT.                                                  
011400*                                 TRANSPORT-ID ALTERNATIV                 
011500        05 MOD-IDTRPLOS-ALT  PIC X(3).                                    
011600*                                 TRANSPORTLÖSNING                        
011700        05 MOD-IDTRPVAR-ALT  PIC X(2).                                    
011800*                                 TRANSPORTLÖSNINGSGRUPP                  
011900     03 MOD-BETRPFIR-4-ALT   PIC X(15).                                   
012000*                                 TRANSPORTFIRMANS NAMN                   
012100     03 MOD-INFO-RAD         OCCURS 7 TIMES.                              
012200*                                 RADINFORMATION                          
012300        05 MOD-IDDC-BULK     PIC X(2).                                    
012400*                                 IDENTIFIERARE BULKORDERLAGER            
012500        05 MOD-KDGENFRA-MO   PIC Z9.                                      
012600*                                 NORMAL FRAKT MÅNADSORDER KL 2-4         
012700        05 MOD-IDDC-DAY      PIC X(2).                                    
012800*                                 IDENTIFIERARE DAGORDERLAGER             
012900        05 MOD-KDGENFRA-DO   PIC Z9.                                      
013000*                                 NORMAL FRAKT DAGORDER                   
013100        05 MOD-IDDC-VOR      PIC X(2).                                    
013200*                                 IDENTIFIERARE VORORDERLAGER             
013300        05 MOD-KDGENFRA-VOR  PIC Z9.                                      
013400*                                 NORMAL FRAKT VOR-ORDER                  
013500     03 MOD-TEMFSINF         PIC X(55).                                   
013600*                                 INFORMATIONSMEDDELANDE                  
013700*** END OF VILMAII-COPY LENGTH= 651 BYTES                                 
