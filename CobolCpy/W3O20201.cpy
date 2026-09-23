000100 01  MOD-W3O20201.                                                        
000200*                                 MOD-COPYTEXT FÖR W3020200               
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-IDFSGURV-IN      PIC X(2).                                    
000800*                                 MFS BEHANDLING AV INPUTFÄLT             
000900     03 MOD-IDUSER-IN-ATTR   PIC X(2).                                    
001000*                                 MFS ATTRIBUTFÄLT                        
001100     03 MOD-IDUSER-IN        PIC X(2).                                    
001200*                                 MFS BEHANDLING AV INPUTFÄLT             
001300     03 MOD-IDFSGURV-UT      PIC X(8).                                    
001400*                                 URVALS IDENTITET                        
001500     03 MOD-IDUSER-UT        PIC X(8).                                    
001600*                                 ANVÄNDARENS SÄKERHETS ID                
001700     03 MOD-IDFKNGRP-FOM-LO  PIC 9(4).                                    
001800*                                 FUNKTIONSGRUPP                          
001900     03 MOD-IDFKNGRP-TOM-LO  PIC 9(4).                                    
002000*                                 FUNKTIONSGRUPP                          
002100     03 MOD-IDFKNGRP-FOM-HI  PIC 9(4).                                    
002200*                                 FUNKTIONSGRUPP                          
002300     03 MOD-IDFKNGRP-TOM-HI  PIC 9(4).                                    
002400*                                 FUNKTIONSGRUPP                          
002500     03 MOD-DAREGDAT-DOLD    PIC 9(8).                                    
002600*                                 REGISTRERINGSDATUM (ÅÅÅÅMMDD)           
002700     03 MOD-TIREGTID-DOLD    PIC 9(6).                                    
002800*                                 REGISTRERINGSTID                        
002900     03 MOD-TIFSGVV-FOM-ATTR PIC X(2).                                    
003000*                                 MFS ATTRIBUTFÄLT                        
003100     03 MOD-TIFSGVV-FOM      PIC 9(4).                                    
003200*                                 FÖRSÄLJNINGSVECKA ARTIKEL               
003300     03 MOD-IDFSGURV-ATTR    PIC X(2).                                    
003400*                                 MFS ATTRIBUTFÄLT                        
003500     03 MOD-IDFSGURV         PIC X(8).                                    
003600*                                 URVALS IDENTITET                        
003700     03 MOD-KDBORT-ATTR      PIC X(2).                                    
003800*                                 MFS ATTRIBUTFÄLT                        
003900     03 MOD-KDBORT           PIC X(2).                                    
004000*                                 MFS BEHANDLING AV INPUTFÄLT             
004100     03 MOD-IDPTYP-ATTR      PIC X(2).                                    
004200*                                 MFS ATTRIBUTFÄLT                        
004300     03 MOD-IDPTYP           PIC X(3).                                    
004400*                                 POSTTYP                                 
004500     03 MOD-TIFSGVV-TOM-ATTR PIC X(2).                                    
004600*                                 MFS ATTRIBUTFÄLT                        
004700     03 MOD-TIFSGVV-TOM      PIC 9(4).                                    
004800*                                 FÖRSÄLJNINGSVECKA ARTIKEL               
004900     03 MOD-KDPRTYPG-ATTR    PIC X(2).                                    
005000*                                 MFS ATTRIBUTFÄLT                        
005100     03 MOD-KDPRTYPG         PIC X(2).                                    
005200*                                 MFS BEHANDLING AV INPUTFÄLT             
005300     03 MOD-INFO-RAD         OCCURS 5 TIMES.                              
005400*                                 RADINFORMATION                          
005500        05 MOD-KDVVKL-ATTR   PIC X(2).                                    
005600*                                 MFS ATTRIBUTFÄLT                        
005700        05 MOD-KDVVKL        PIC X(2).                                    
005800*                                 MFS BEHANDLING AV INPUTFÄLT             
005900     03 MOD-KDNIVA-ATTR      PIC X(2).                                    
006000*                                 MFS ATTRIBUTFÄLT                        
006100     03 MOD-KDNIVA           PIC 9(2).                                    
006200*                                 NIVÅ NUMMER                             
006300     03 MOD-INFO-RAD         OCCURS 7 TIMES.                              
006400*                                 RADINFORMATION                          
006500        05 MOD-KDPRODSL-ATTR PIC X(2).                                    
006600*                                 MFS ATTRIBUTFÄLT                        
006700        05 MOD-KDPRODSL      PIC Z9.                                      
006800*                                 PRODUKTSLAG                             
006900     03 MOD-KDSVAR-ATTR      PIC X(2).                                    
007000*                                 MFS ATTRIBUTFÄLT                        
007100     03 MOD-KDSVAR           PIC X.                                       
007200*                                 SVAR FRÅN PROGRAM ELLER SKÄRM           
007300     03 MOD-INFO-RAD         OCCURS 8 TIMES.                              
007400*                                 RADINFORMATION                          
007500        05 MOD-IDKONCNR-ATTR PIC X(2).                                    
007600*                                 MFS ATTRIBUTFÄLT                        
007700        05 MOD-IDKONCNR      PIC Z(2)9.                                   
007800*                                 KONCERNNUMMER                           
007900     03 MOD-INFO-RAD         OCCURS 8 TIMES.                              
008000*                                 RADINFORMATION                          
008100        05 MOD-IDLEVNR-ATTR  PIC X(2).                                    
008200*                                 MFS ATTRIBUTFÄLT                        
008300        05 MOD-IDLEVNR       PIC X(5).                                    
008400*                                 LEVERANTÖRNUMMER                        
008500     03 MOD-INFO-RAD         OCCURS 8 TIMES.                              
008600*                                 RADINFORMATION                          
008700        05 MOD-IDLKTO-ATTR   PIC X(2).                                    
008800*                                 MFS ATTRIBUTFÄLT                        
008900        05 MOD-IDLKTO        PIC 9(7).                                    
009000*                                 LAGERKONTO (FFHHHUU)                    
009100     03 MOD-INFO-RAD         OCCURS 8 TIMES.                              
009200*                                 RADINFORMATION                          
009300        05 MOD-KDMARK-FOM-ATTR                                            
009400                             PIC X(2).                                    
009500*                                 MFS ATTRIBUTFÄLT                        
009600        05 MOD-KDMARK-FOM    PIC Z(3).                                    
009700*                                 MARKNADSKOD                             
009800        05 MOD-KDMARK-TOM-ATTR                                            
009900                             PIC X(2).                                    
010000*                                 MFS ATTRIBUTFÄLT                        
010100        05 MOD-KDMARK-TOM    PIC Z(3).                                    
010200*                                 MARKNADSKOD                             
010300     03 MOD-INFO-RAD         OCCURS 4 TIMES.                              
010400*                                 RADINFORMATION                          
010500        05 MOD-IDDISTR-FOM-ATTR                                           
010600                             PIC X(2).                                    
010700*                                 MFS ATTRIBUTFÄLT                        
010800        05 MOD-IDDISTR-FOM   PIC Z(3)9.                                   
010900*                                 DISTRIKTNUMMER                          
011000        05 MOD-IDDISTR-TOM-ATTR                                           
011100                             PIC X(2).                                    
011200*                                 MFS ATTRIBUTFÄLT                        
011300        05 MOD-IDDISTR-TOM   PIC Z(3)9.                                   
011400*                                 DISTRIKTNUMMER                          
011500     03 MOD-INFO-RAD         OCCURS 4 TIMES.                              
011600*                                 RADINFORMATION                          
011700        05 MOD-IDANSK-FOM-ATTR                                            
011800                             PIC X(2).                                    
011900*                                 MFS ATTRIBUTFÄLT                        
012000        05 MOD-IDANSK-FOM    PIC Z(2)9.                                   
012100*                                 ANSKAFFARNUMMER                         
012200        05 MOD-IDANSK-TOM-ATTR                                            
012300                             PIC X(2).                                    
012400*                                 MFS ATTRIBUTFÄLT                        
012500        05 MOD-IDANSK-TOM    PIC Z(2)9.                                   
012600*                                 ANSKAFFARNUMMER                         
012700     03 MOD-INFO-RAD         OCCURS 8 TIMES.                              
012800*                                 RADINFORMATION                          
012900        05 MOD-IDFKNGRP-FOM-ATTR                                          
013000                             PIC X(2).                                    
013100*                                 MFS ATTRIBUTFÄLT                        
013200        05 MOD-IDFKNGRP-FOM  PIC Z(3)9.                                   
013300*                                 FUNKTIONSGRUPP                          
013400        05 MOD-IDFKNGRP-TOM-ATTR                                          
013500                             PIC X(2).                                    
013600*                                 MFS ATTRIBUTFÄLT                        
013700        05 MOD-IDFKNGRP-TOM  PIC Z(3)9.                                   
013800*                                 FUNKTIONSGRUPP                          
013900     03 MOD-KDCMD-ATTR       PIC X(2).                                    
014000*                                 MFS ATTRIBUTFÄLT                        
014100     03 MOD-KDCMD            PIC X(2).                                    
014200*                                 MFS BEHANDLING AV INPUTFÄLT             
014300     03 MOD-INFO-RAD         OCCURS 4 TIMES.                              
014400*                                 RADINFORMATION                          
014500        05 MOD-IDFKNGRP-FOM-IN-ATTR                                       
014600                             PIC X(2).                                    
014700*                                 MFS ATTRIBUTFÄLT                        
014800        05 MOD-IDFKNGRP-FOM-IN                                            
014900                             PIC Z(3)9.                                   
015000*                                 FUNKTIONSGRUPP                          
015100        05 MOD-IDFKNGRP-TOM-IN-ATTR                                       
015200                             PIC X(2).                                    
015300*                                 MFS ATTRIBUTFÄLT                        
015400        05 MOD-IDFKNGRP-TOM-IN                                            
015500                             PIC Z(3)9.                                   
015600*                                 FUNKTIONSGRUPP                          
015700     03 MOD-TEMFSINF         PIC X(55).                                   
015800*                                 INFORMATIONSMEDDELANDE                  
015900*** END OF VILMAII-COPY LENGTH= 725 BYTES                                 
