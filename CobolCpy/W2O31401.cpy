000100 01  MOD-W2O31401.                                                        
000200*                                 MOD-COPYTEXT TILL W2031400              
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-IDKAMP-IN        PIC X(7).                                    
000800*                                 SERVICEKAMPANJ                          
000900     03 MOD-IDKAMP-UT        PIC X(7).                                    
001000*                                 SERVICEKAMPANJ                          
001100     03 MOD-IDKAMP-GRP-IN    PIC X(7).                                    
001200*                                 ID FÖR KAMPANJGRUPPER                   
001300     03 MOD-IDKAMP-GRP-UT    PIC X(7).                                    
001400*                                 ID FÖR KAMPANJGRUPPER                   
001500     03 MOD-VISA-UNIK-IN-ATTR                                             
001600                             PIC X(2).                                    
001700*                                 MFS ATTRIBUTFÄLT                        
001800     03 MOD-VISA-UNIK-IN     PIC X(2).                                    
001900*                                 MFS BEHANDLING AV INPUTFÄLT             
002000     03 MOD-VISA-UNIK-UT     PIC X.                                       
002100     03 MOD-VISA-DEF-ERS-IN-ATTR                                          
002200                             PIC X(2).                                    
002300*                                 MFS ATTRIBUTFÄLT                        
002400     03 MOD-VISA-DEF-ERS-IN  PIC X(2).                                    
002500*                                 MFS BEHANDLING AV INPUTFÄLT             
002600     03 MOD-VISA-DEF-ERS-UT  PIC X.                                       
002700     03 MOD-RAD              OCCURS 12 TIMES.                             
002800        05 MOD-CMD-ATTR      PIC X(2).                                    
002900*                                 MFS ATTRIBUTFÄLT                        
003000        05 MOD-CMD           PIC X(2).                                    
003100*                                 MFS BEHANDLING AV INPUTFÄLT             
003200        05 MOD-IDARTNR       PIC Z(8)9.                                   
003300*                                 ARTIKELNUMMER                           
003400        05 MOD-KVREPANT      PIC X(6).                                    
003500*                                 ANTAL PER REPARATION OCH BIL            
003600        05 MOD-KVKAMP-LAUNCH-ATTR                                         
003700                             PIC X(2).                                    
003800*                                 MFS ATTRIBUTFÄLT                        
003900        05 MOD-KVKAMP-LAUNCH PIC Z(6)9.                                   
004000*                                                                         
004100        05 MOD-KVKAMP-FIRST-ATTR                                          
004200                             PIC X(2).                                    
004300*                                 MFS ATTRIBUTFÄLT                        
004400        05 MOD-KVKAMP-FIRST  PIC Z(6)9.                                   
004500*                                                                         
004600        05 MOD-KVKAMP-TOTAL-ATTR                                          
004700                             PIC X(2).                                    
004800*                                 MFS ATTRIBUTFÄLT                        
004900        05 MOD-KVKAMP-TOTAL  PIC Z(6)9.                                   
005000*                                 ANTAL I KAMPANJ                         
005100        05 MOD-FLKVKAMP-TOTAL-ATTR                                        
005200                             PIC X(2).                                    
005300*                                 MFS ATTRIBUTFÄLT                        
005400        05 MOD-FLKVKAMP-TOTAL                                             
005500                             PIC X.                                       
005600*                                 MANUELLT ELLER MASKINELLT BERÄK         
005700*                                 NAD KVANTITET                           
005800        05 MOD-IDANSK        PIC X(3).                                    
005900*                                 ANSKAFFARNUMMER                         
006000        05 MOD-KDERS         PIC X(2).                                    
006100*                                 ERSÄTTNINGSKOD                          
006200        05 MOD-RERESPRT-ATTR PIC X(2).                                    
006300*                                 MFS ATTRIBUTFÄLT                        
006400        05 MOD-RERESPRT      PIC X(3).                                    
006500*                                                                         
006600        05 MOD-KOMMENTAR     PIC X(19).                                   
006700     03 MOD-NY-IDARTNR-ATTR  PIC X(2).                                    
006800*                                 MFS ATTRIBUTFÄLT                        
006900     03 MOD-NY-IDARTNR       PIC X(9).                                    
007000*                                 ARTIKELNUMMER                           
007100     03 MOD-NY-KVKAMP-LAUNCH-ATTR                                         
007200                             PIC X(2).                                    
007300*                                 MFS ATTRIBUTFÄLT                        
007400     03 MOD-NY-KVKAMP-LAUNCH PIC X(7).                                    
007500*                                                                         
007600     03 MOD-NY-KVKAMP-FIRST-ATTR                                          
007700                             PIC X(2).                                    
007800*                                 MFS ATTRIBUTFÄLT                        
007900     03 MOD-NY-KVKAMP-FIRST  PIC X(7).                                    
008000*                                                                         
008100     03 MOD-NY-KVKAMP-TOTAL-ATTR                                          
008200                             PIC X(2).                                    
008300*                                 MFS ATTRIBUTFÄLT                        
008400     03 MOD-NY-KVKAMP-TOTAL  PIC X(7).                                    
008500*                                 ANTAL I KAMPANJ                         
008600     03 MOD-KDKAMP           PIC X.                                       
008700*                                                                         
008800     03 MOD-TEMFSINF         PIC X(55).                                   
008900*                                 INFORMATIONSMEDDELANDE                  
009000*** END OF VILMAII-COPY LENGTH= 1112 BYTES                                
