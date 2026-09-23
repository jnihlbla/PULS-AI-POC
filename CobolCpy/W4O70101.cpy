000100 01  MOD-W4O70101.                                                        
000200*                                 MOD-COPYTEXT FÖR W4070100               
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-KDARBTYP-IN      PIC X(8).                                    
000800*                                 TYP AV ARBETE                           
000900     03 MOD-KDARBTYP-UT      PIC X(8).                                    
001000*                                 TYP AV ARBETE                           
001100     03 MOD-KDANMORS-IN      PIC X(2).                                    
001200*                                 ORSAK TILL LEVERANSANMÄRKNING           
001300     03 MOD-KDANMORS-UT      PIC X(2).                                    
001400*                                 ORSAK TILL LEVERANSANMÄRKNING           
001500     03 MOD-IDDISTR-IN       PIC X(4).                                    
001600*                                 DISTRIKTNUMMER                          
001700     03 MOD-IDDISTR-UT       PIC X(4).                                    
001800*                                 DISTRIKTNUMMER                          
001900     03 MOD-IDDC-IN          PIC X(2).                                    
002000*                                 IDENTIFIERARE LAGER                     
002100     03 MOD-IDDC-UT          PIC X(2).                                    
002200*                                 IDENTIFIERARE LAGER                     
002300     03 MOD-KDARBTYP-ENTER   PIC X(8).                                    
002400*                                 TYP AV ARBETE                           
002500     03 MOD-KDANMORS-ENTER   PIC X(2).                                    
002600*                                 ORSAK TILL LEVERANSANMÄRKNING           
002700     03 MOD-IDRADNR-ENTER    PIC 9(3).                                    
002800*                                 RADNUMMER                               
002900     03 MOD-KDARBTYP-NEXT    PIC X(8).                                    
003000*                                 TYP AV ARBETE                           
003100     03 MOD-KDANMORS-NEXT    PIC X(2).                                    
003200*                                 ORSAK TILL LEVERANSANMÄRKNING           
003300     03 MOD-IDRADNR-NEXT     PIC 9(3).                                    
003400*                                 RADNUMMER                               
003500     03 MOD-IDDC-ENTER       PIC X(2).                                    
003600*                                 IDENTIFIERARE LAGER                     
003700     03 MOD-IDDC-NEXT        PIC X(2).                                    
003800*                                 IDENTIFIERARE LAGER                     
003900     03 MOD-ANSVARIG-RAD1.                                                
004000*                                 ANSVARIGA FÖR LEVERANSANM.              
004100        05 MOD-IDRADNR-RAD1-ATTR                                          
004200                             PIC X(2).                                    
004300*                                 MFS ATTRIBUTFÄLT                        
004400        05 MOD-IDRADNR-RAD1  PIC 9(3).                                    
004500*                                 RADNUMMER                               
004600        05 MOD-KDANMORS-RAD1-ATTR                                         
004700                             PIC X(2).                                    
004800*                                 MFS ATTRIBUTFÄLT                        
004900        05 MOD-KDANMORS-RAD1 PIC X(2).                                    
005000*                                 ORSAK TILL LEVERANSANMÄRKNING           
005100        05 MOD-IDDC-RAD1-ATTR                                             
005200                             PIC X(2).                                    
005300*                                 MFS ATTRIBUTFÄLT                        
005400        05 MOD-IDDC-RAD1     PIC X(2).                                    
005500*                                 IDENTIFIERARE LAGER                     
005600        05 MOD-KDORDKL-RAD1-ATTR                                          
005700                             PIC X(2).                                    
005800*                                 MFS ATTRIBUTFÄLT                        
005900        05 MOD-KDORDKL-RAD1  PIC 9.                                       
006000*                                 ORDERKLASS                              
006100        05 MOD-ADLAGOMR-RAD1-ATTR                                         
006200                             PIC X(2).                                    
006300*                                 MFS ATTRIBUTFÄLT                        
006400        05 MOD-ADLAGOMR-RAD1 PIC Z9.                                      
006500*                                 LAGEROMRÅDE                             
006600        05 MOD-IDDISTR-FOM-RAD1-ATTR                                      
006700                             PIC X(2).                                    
006800*                                 MFS ATTRIBUTFÄLT                        
006900        05 MOD-IDDISTR-FOM-RAD1                                           
007000                             PIC Z(3)9.                                   
007100*                                 DISTRIKTNUMMER                          
007200        05 MOD-IDDISTR-TOM-RAD1-ATTR                                      
007300                             PIC X(2).                                    
007400*                                 MFS ATTRIBUTFÄLT                        
007500        05 MOD-IDDISTR-TOM-RAD1                                           
007600                             PIC Z(3)9.                                   
007700*                                 DISTRIKTNUMMER                          
007800        05 MOD-IDKUNDNR-FOM-RAD1-ATTR                                     
007900                             PIC X(2).                                    
008000*                                 MFS ATTRIBUTFÄLT                        
008100        05 MOD-IDKUNDNR-FOM-RAD1                                          
008200                             PIC Z(5)9.                                   
008300*                                 KUNDNUMMER                              
008400        05 MOD-IDKUNDNR-TOM-RAD1-ATTR                                     
008500                             PIC X(2).                                    
008600*                                 MFS ATTRIBUTFÄLT                        
008700        05 MOD-IDKUNDNR-TOM-RAD1                                          
008800                             PIC Z(5)9.                                   
008900*                                 KUNDNUMMER                              
009000        05 MOD-ARB-PERS-RAD1-ATTR                                         
009100                             PIC X(2).                                    
009200*                                 MFS ATTRIBUTFÄLT                        
009300        05 MOD-ARB-PERS-RAD1 PIC X(6).                                    
009400        05 MOD-IDMAIL30-RAD1-ATTR                                         
009500                             PIC X(2).                                    
009600*                                 MFS ATTRIBUTFÄLT                        
009700        05 MOD-IDMAIL30-RAD1 PIC X(30).                                   
009800     03 MOD-ANSVARIG-TABELL  OCCURS 9 TIMES.                              
009900*                                 ANSVARIGA FÖR LEVERANSANM.              
010000        05 MOD-IDRADNR       PIC 9(3).                                    
010100*                                 RADNUMMER                               
010200        05 MOD-KDANMORS      PIC X(2).                                    
010300*                                 ORSAK TILL LEVERANSANMÄRKNING           
010400        05 MOD-IDDC          PIC X(2).                                    
010500*                                 IDENTIFIERARE LAGER                     
010600        05 MOD-KDORDKL       PIC 9.                                       
010700*                                 ORDERKLASS                              
010800        05 MOD-ADLAGOMR      PIC Z9.                                      
010900*                                 LAGEROMRÅDE                             
011000        05 MOD-IDDISTR-FOM   PIC Z(3)9.                                   
011100*                                 DISTRIKTNUMMER                          
011200        05 MOD-IDDISTR-TOM   PIC Z(3)9.                                   
011300*                                 DISTRIKTNUMMER                          
011400        05 MOD-IDKUNDNR-FOM  PIC Z(5)9.                                   
011500*                                 KUNDNUMMER                              
011600        05 MOD-IDKUNDNR-TOM  PIC Z(5)9.                                   
011700*                                 KUNDNUMMER                              
011800        05 MOD-KDARBTYP      PIC X(3).                                    
011900        05 MOD-IDPERSON      PIC X(3).                                    
012000*                                 PERSONKOD                               
012100        05 MOD-IDMAIL30      PIC X(30).                                   
012200     03 MOD-UPPD-RAD.                                                     
012300*                                 UPDDATERINGSRAD                         
012400        05 MOD-IDRADNR-UPPD-ATTR                                          
012500                             PIC X(2).                                    
012600*                                 MFS ATTRIBUTFÄLT                        
012700        05 MOD-IDRADNR-UPPD  PIC 9(3).                                    
012800*                                 RADNUMMER                               
012900        05 MOD-KDANMORS-UPPD-ATTR                                         
013000                             PIC X(2).                                    
013100*                                 MFS ATTRIBUTFÄLT                        
013200        05 MOD-KDANMORS-UPPD PIC X(2).                                    
013300*                                 ORSAK TILL LEVERANSANMÄRKNING           
013400        05 MOD-IDDC-UPPD-ATTR                                             
013500                             PIC X(2).                                    
013600*                                 MFS ATTRIBUTFÄLT                        
013700        05 MOD-IDDC-UPPD     PIC X(2).                                    
013800*                                 IDENTIFIERARE LAGER                     
013900        05 MOD-KDORDKL-UPPD-ATTR                                          
014000                             PIC X(2).                                    
014100*                                 MFS ATTRIBUTFÄLT                        
014200        05 MOD-KDORDKL-UPPD  PIC 9.                                       
014300*                                 ORDERKLASS                              
014400        05 MOD-ADLAGOMR-UPPD-ATTR                                         
014500                             PIC X(2).                                    
014600*                                 MFS ATTRIBUTFÄLT                        
014700        05 MOD-ADLAGOMR-UPPD PIC Z9.                                      
014800*                                 LAGEROMRÅDE                             
014900        05 MOD-IDDISTR-FOM-UPPD-ATTR                                      
015000                             PIC X(2).                                    
015100*                                 MFS ATTRIBUTFÄLT                        
015200        05 MOD-IDDISTR-FOM-UPPD                                           
015300                             PIC Z(3)9.                                   
015400*                                 DISTRIKTNUMMER                          
015500        05 MOD-IDDISTR-TOM-UPPD-ATTR                                      
015600                             PIC X(2).                                    
015700*                                 MFS ATTRIBUTFÄLT                        
015800        05 MOD-IDDISTR-TOM-UPPD                                           
015900                             PIC Z(3)9.                                   
016000*                                 DISTRIKTNUMMER                          
016100        05 MOD-IDKUNDNR-FOM-UPPD-ATTR                                     
016200                             PIC X(2).                                    
016300*                                 MFS ATTRIBUTFÄLT                        
016400        05 MOD-IDKUNDNR-FOM-UPPD                                          
016500                             PIC Z(5)9.                                   
016600*                                 KUNDNUMMER                              
016700        05 MOD-IDKUNDNR-TOM-UPPD-ATTR                                     
016800                             PIC X(2).                                    
016900*                                 MFS ATTRIBUTFÄLT                        
017000        05 MOD-IDKUNDNR-TOM-UPPD                                          
017100                             PIC Z(5)9.                                   
017200*                                 KUNDNUMMER                              
017300        05 MOD-KDARB-IDPERS-UPD-ATTR                                      
017400                             PIC X(2).                                    
017500*                                 MFS ATTRIBUTFÄLT                        
017600        05 MOD-KDARB-IDPERS-UPD                                           
017700                             PIC X(6).                                    
017800        05 MOD-FLBORT-UPPD-ATTR                                           
017900                             PIC X(2).                                    
018000*                                 MFS ATTRIBUTFÄLT                        
018100        05 MOD-FLBORT-UPPD   PIC X.                                       
018200*                                 ALLMÄN FLAGGA                           
018300     03 MOD-TEMFSINF         PIC X(55).                                   
018400*                                 INFORMATIONSMEDDELANDE                  
018500*** END OF VILMAII-COPY LENGTH= 902 BYTES                                 
