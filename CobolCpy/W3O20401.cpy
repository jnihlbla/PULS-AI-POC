000100 01  MOD-W3O20401.                                                        
000200*                                 MOD-COPYTEXT FÖR W3020400               
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-IDFSGURV-IN      PIC X(2).                                    
000800*                                 MFS BEHANDLING AV INPUTFÄLT             
000900     03 MOD-IDUSER-IN        PIC X(2).                                    
001000*                                 MFS BEHANDLING AV INPUTFÄLT             
001100     03 MOD-IDFSGURV-UT      PIC X(8).                                    
001200*                                 URVALS IDENTITET                        
001300     03 MOD-IDUSER-UT        PIC X(8).                                    
001400*                                 ANVÄNDARENS SÄKERHETS ID                
001500     03 MOD-KVART-ATTR       PIC X(2).                                    
001600*                                 MFS ATTRIBUTFÄLT                        
001700     03 MOD-KVART            PIC Z(6)9.                                   
001800*                                 ANTAL ARTNR PER BRYTBEGREPP             
001900     03 MOD-IDPTYP-ATTR      PIC X(2).                                    
002000*                                 MFS ATTRIBUTFÄLT                        
002100     03 MOD-IDPTYP           PIC X(3).                                    
002200*                                 POSTTYP                                 
002300     03 MOD-INFO-RAD         OCCURS 4 TIMES.                              
002400*                                 RADINFORMATION                          
002500        05 MOD-KDPRODSL-ATTR PIC X(2).                                    
002600*                                 MFS ATTRIBUTFÄLT                        
002700        05 MOD-KDPRODSL      PIC Z9.                                      
002800*                                 PRODUKTSLAG                             
002900     03 MOD-INFO-RAD         OCCURS 8 TIMES.                              
003000*                                 RADINFORMATION                          
003100        05 MOD-IDKONCNR-ATTR PIC X(2).                                    
003200*                                 MFS ATTRIBUTFÄLT                        
003300        05 MOD-IDKONCNR      PIC Z(2)9.                                   
003400*                                 KONCERNNUMMER                           
003500     03 MOD-INFO-RAD         OCCURS 8 TIMES.                              
003600*                                 RADINFORMATION                          
003700        05 MOD-IDLEVNR-ATTR  PIC X(2).                                    
003800*                                 MFS ATTRIBUTFÄLT                        
003900        05 MOD-IDLEVNR       PIC X(5).                                    
004000*                                 LEVERANTÖRNUMMER                        
004100     03 MOD-IDFKNGRP-FOM-ATTR                                             
004200                             PIC X(2).                                    
004300*                                 MFS ATTRIBUTFÄLT                        
004400     03 MOD-IDFKNGRP-FOM     PIC Z(3)9.                                   
004500*                                 FUNKTIONSGRUPP                          
004600     03 MOD-IDFKNGRP-TOM-ATTR                                             
004700                             PIC X(2).                                    
004800*                                 MFS ATTRIBUTFÄLT                        
004900     03 MOD-IDFKNGRP-TOM     PIC Z(3)9.                                   
005000*                                 FUNKTIONSGRUPP                          
005100     03 MOD-INFO-RAD         OCCURS 4 TIMES.                              
005200*                                 RADINFORMATION                          
005300        05 MOD-KDMARK-FOM-ATTR                                            
005400                             PIC X(2).                                    
005500*                                 MFS ATTRIBUTFÄLT                        
005600        05 MOD-KDMARK-FOM    PIC Z(3).                                    
005700*                                 MARKNADSKOD                             
005800        05 MOD-KDMARK-TOM-ATTR                                            
005900                             PIC X(2).                                    
006000*                                 MFS ATTRIBUTFÄLT                        
006100        05 MOD-KDMARK-TOM    PIC Z(3).                                    
006200*                                 MARKNADSKOD                             
006300     03 MOD-INFO-RAD         OCCURS 4 TIMES.                              
006400*                                 RADINFORMATION                          
006500        05 MOD-IDDISTR-FOM-ATTR                                           
006600                             PIC X(2).                                    
006700*                                 MFS ATTRIBUTFÄLT                        
006800        05 MOD-IDDISTR-FOM   PIC Z(3)9.                                   
006900*                                 DISTRIKTNUMMER                          
007000        05 MOD-IDDISTR-TOM-ATTR                                           
007100                             PIC X(2).                                    
007200*                                 MFS ATTRIBUTFÄLT                        
007300        05 MOD-IDDISTR-TOM   PIC Z(3)9.                                   
007400*                                 DISTRIKTNUMMER                          
007500     03 MOD-INFO-RAD         OCCURS 4 TIMES.                              
007600*                                 RADINFORMATION                          
007700        05 MOD-IDANSK-FOM-ATTR                                            
007800                             PIC X(2).                                    
007900*                                 MFS ATTRIBUTFÄLT                        
008000        05 MOD-IDANSK-FOM    PIC Z(2)9.                                   
008100*                                 ANSKAFFARNUMMER                         
008200        05 MOD-IDANSK-TOM-ATTR                                            
008300                             PIC X(2).                                    
008400*                                 MFS ATTRIBUTFÄLT                        
008500        05 MOD-IDANSK-TOM    PIC Z(2)9.                                   
008600*                                 ANSKAFFARNUMMER                         
008700     03 MOD-TEMFSINF         PIC X(55).                                   
008800*                                 INFORMATIONSMEDDELANDE                  
008900*** END OF VILMAII-COPY LENGTH= 385 BYTES                                 
