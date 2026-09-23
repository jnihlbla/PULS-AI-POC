000100 01  W4O73301.                                                            
000200*                                 MODCOPYTEXT TILL W40733.                
000300     03 IDTRANS              PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 TEMFSFEL             PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 IDRT-IN              PIC X(3).                                    
000800*                                 RETURTERMINAL                           
000900     03 IDRTLOP-IN           PIC X(3).                                    
001000*                                 RETUR TERMINAL LÖPNUMMER                
001100     03 IDRT-UT              PIC X(3).                                    
001200*                                 RETURTERMINAL                           
001300     03 IDRTLOP-UT           PIC X(3).                                    
001400*                                 RETUR TERMINAL LÖPNUMMER                
001500     03 OUTPUT.                                                           
001600*                                                                         
001700        05 KVKOLLI-LOSS      PIC Z(3)9.                                   
001800*                                 ANTAL KOLLI                             
001900        05 IDANSTNR-LOSS     PIC Z(4)9.                                   
002000*                                 ANSTÄLLNINGSNUMMER                      
002100        05 ADINLOMR-LOSS     PIC X(4).                                    
002200*                                 INLEVERANSOMRÅDE                        
002300        05 IDFRASED-LOSS     PIC X(15).                                   
002400*                                 FRAKTSEDELSNUMMER                       
002500        05 KVKOLLI-MOT       PIC Z(3)9.                                   
002600*                                 ANTAL KOLLI                             
002700        05 IDANSTNR-MOT      PIC Z(4)9.                                   
002800*                                 ANSTÄLLNINGSNUMMER                      
002900        05 ADINLOMR-MOT      PIC X(4).                                    
003000*                                 INLEVERANSOMRÅDE                        
003100        05 IDFRASED-MOT      PIC X(15).                                   
003200*                                 FRAKTSEDELSNUMMER                       
003300     03 INPUT.                                                            
003400*                                                                         
003500        05 SND.                                                           
003600*                                                                         
003700           07 FLNYSND-ATTR   PIC X(2).                                    
003800*                                 MFS ATTRIBUTFÄLT                        
003900           07 FLNYSND        PIC X.                                       
004000*                                 ALLMÄN FLAGGA                           
004100           07 KVKOLLI-LOSS-ATTR                                           
004200                             PIC X(2).                                    
004300*                                 MFS ATTRIBUTFÄLT                        
004400           07 KVKOLLI-LOSS-UPD                                            
004500                             PIC X(4).                                    
004600*                                 ANTAL KOLLI                             
004700           07 IDANSTNR-LOSS-ATTR                                          
004800                             PIC X(2).                                    
004900*                                 MFS ATTRIBUTFÄLT                        
005000           07 IDANSTNR-LOSS-UPD                                           
005100                             PIC X(5).                                    
005200*                                 ANSTÄLLNINGSNUMMER                      
005300           07 ADINLOMR-LOSS-ATTR                                          
005400                             PIC X(2).                                    
005500*                                 MFS ATTRIBUTFÄLT                        
005600           07 ADINLOMR-LOSS-UPD                                           
005700                             PIC X(4).                                    
005800*                                 INLEVERANSOMRÅDE                        
005900           07 IDFRASED-LOSS-ATTR                                          
006000                             PIC X(2).                                    
006100*                                 MFS ATTRIBUTFÄLT                        
006200           07 IDFRASED-LOSS-UPD                                           
006300                             PIC X(15).                                   
006400*                                 FRAKTSEDELSNUMMER                       
006500           07 KVKOLLI-MOT-ATTR                                            
006600                             PIC X(2).                                    
006700*                                 MFS ATTRIBUTFÄLT                        
006800           07 KVKOLLI-MOT-UPD                                             
006900                             PIC X(4).                                    
007000*                                 ANTAL KOLLI                             
007100           07 IDANSTNR-MOT-ATTR                                           
007200                             PIC X(2).                                    
007300*                                 MFS ATTRIBUTFÄLT                        
007400           07 IDANSTNR-MOT-UPD                                            
007500                             PIC X(5).                                    
007600*                                 ANSTÄLLNINGSNUMMER                      
007700           07 ADINLOMR-MOT-ATTR                                           
007800                             PIC X(2).                                    
007900*                                 MFS ATTRIBUTFÄLT                        
008000           07 ADINLOMR-MOT-UPD                                            
008100                             PIC X(4).                                    
008200*                                 INLEVERANSOMRÅDE                        
008300           07 IDFRASED-MOT-ATTR                                           
008400                             PIC X(2).                                    
008500*                                 MFS ATTRIBUTFÄLT                        
008600           07 IDFRASED-MOT-UPD                                            
008700                             PIC X(15).                                   
008800*                                 FRAKTSEDELSNUMMER                       
008900        05 KOLLI.                                                         
009000*                                                                         
009100           07 IDKOLLI-ATTR   PIC X(2).                                    
009200*                                 MFS ATTRIBUTFÄLT                        
009300           07 IDKOLLI        PIC X(5).                                    
009400*                                 KOLLINUMMER                             
009500           07 IDDISTR-ATTR   PIC X(2).                                    
009600*                                 MFS ATTRIBUTFÄLT                        
009700           07 IDDISTR        PIC X(4).                                    
009800*                                 DISTRIKTNUMMER                          
009900           07 IDKOLLI-FOM-ATTR                                            
010000                             PIC X(2).                                    
010100*                                 MFS ATTRIBUTFÄLT                        
010200           07 IDKOLLI-FOM    PIC X(5).                                    
010300*                                 KOLLINUMMER                             
010400           07 IDKOLLI-TOM-ATTR                                            
010500                             PIC X(2).                                    
010600*                                 MFS ATTRIBUTFÄLT                        
010700           07 IDKOLLI-TOM    PIC X(5).                                    
010800*                                 KOLLINUMMER                             
010900        05 RADER             OCCURS 8 TIMES.                              
011000*                                                                         
011100           07 IDKUNDNR-ATTR  PIC X(2).                                    
011200*                                 MFS ATTRIBUTFÄLT                        
011300           07 IDKUNDNR       PIC X(6).                                    
011400*                                 KUNDNUMMER                              
011500           07 IDRAPPNR-ATTR  PIC X(2).                                    
011600*                                 MFS ATTRIBUTFÄLT                        
011700           07 IDRAPPNR       PIC X(7).                                    
011800*                                 RAPPORT NUMMER                          
011900           07 KVKOLLI-ATTR   PIC X(2).                                    
012000*                                 MFS ATTRIBUTFÄLT                        
012100           07 KVKOLLI        PIC X(4).                                    
012200*                                 ANTAL KOLLI                             
012300           07 IDFRASED-ATTR  PIC X(2).                                    
012400*                                 MFS ATTRIBUTFÄLT                        
012500           07 IDFRASED       PIC X(15).                                   
012600*                                 FRAKTSEDELSNUMMER                       
012700           07 TERETNOT-ATTR  PIC X(2).                                    
012800*                                 MFS ATTRIBUTFÄLT                        
012900           07 TERETNOT       PIC X(20).                                   
013000*                                 FRI NOTERING RETURER                    
013100     03 TEMFSINF             PIC X(55).                                   
013200*                                 INFORMATIONSMEDDELANDE                  
013300*** END OF VILMAII-COPY LENGTH= 765 BYTES                                 
