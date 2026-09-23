000100 01  MOD-W3O17201.                                                        
000200*                                 COPYTEXT FÖR MOD                        
000300*                                 W3O17201                                
000400     03 MOD-IDTRANS          PIC X(4).                                    
000500*                                 BILDNUMMER                              
000600     03 MOD-TEMFSFEL         PIC X(40).                                   
000700*                                 MFS FELMEDDELANDE                       
000800     03 MOD-IDDISTR-IN       PIC X(4).                                    
000900*                                 DISTRIKTNUMMER                          
001000     03 MOD-IDDISTR-UT       PIC X(4).                                    
001100*                                 DISTRIKTNUMMER                          
001200     03 MOD-IDBYTRAP-IN      PIC X(7).                                    
001300*                                 RAPPORTNUMMER  BYTES                    
001400     03 MOD-IDBYTRAP-UT      PIC X(7).                                    
001500*                                 RAPPORTNUMMER  BYTES                    
001600     03 MOD-KDPRT-ATTR       PIC X(2).                                    
001700*                                 MFS ATTRIBUTFÄLT                        
001800     03 MOD-KDPRT            PIC X(3).                                    
001900*                                 PRINTERKOD                              
002000     03 MOD-FLAGGA           PIC X.                                       
002100*                                 ALLMÄN FLAGGA                           
002200     03 MOD-IDKUNDNR         PIC Z(7).                                    
002300*                                 KUNDNUMMER                              
002400     03 MOD-IDFAKTNR         PIC Z(6)9.                                   
002500*                                 FAKTURANUMMER                           
002600     03 MOD-KDBYTSTA-RAPP    PIC X.                                       
002700*                                 STATUSKOD BYTESOBJEKT                   
002800     03 MOD-IDUSER-GODK      PIC X(8).                                    
002900*                                 ANVÄNDARENS SÄKERHETS ID                
003000     03 MOD-DISTR-GRUPP      OCCURS 9 TIMES.                              
003100        05 MOD-SELECT-URVAL-ATTR                                          
003200                             PIC X(2).                                    
003300*                                 MFS ATTRIBUTFÄLT                        
003400        05 MOD-SELECT-URVAL  PIC X.                                       
003500        05 MOD-IDARTNR-OBJ   PIC Z(9).                                    
003600*                                 ARTIKELNUMMER                           
003700        05 MOD-KVPOINT       PIC Z(6).                                    
003800*                                 POINT VALUE                             
003900        05 MOD-KVRETUR-URSP  PIC Z(3).                                    
004000        05 MOD-KVRETUR-GODK-ATTR                                          
004100                             PIC X(2).                                    
004200*                                 MFS ATTRIBUTFÄLT                        
004300        05 MOD-KVRETUR-GODK  PIC Z(2)9.                                   
004400        05 MOD-KDBYTSTA-RAD  PIC X.                                       
004500*                                 STATUSKOD BYTESOBJEKT                   
004600        05 MOD-ANMARK-IN-UT-ATTR                                          
004700                             PIC X(2).                                    
004800*                                 MFS ATTRIBUTFÄLT                        
004900        05 MOD-ANMARK-IN-UT  PIC X(3).                                    
005000        05 MOD-FLSKROT-IN-UT PIC X.                                       
005100*                                 SKROTNINGSMARKERING                     
005200        05 MOD-BEART         PIC X(20).                                   
005300*                                 BENÄMNING            BEART-003          
005400        05 MOD-IDBYTRAD      PIC 9(5).                                    
005500*                                 RADNUMMER                               
005600        05 MOD-BERADREF      PIC X(10).                                   
005700*                                 KUNDENS RADREFERENS                     
005800        05 MOD-FLAGGA-LOC    PIC X.                                       
005900*                                 ALLMÄN FLAGGA                           
006000        05 MOD-IDTABNR       PIC Z(3).                                    
006100*                                 TABELLNUMMER                            
006200     03 MOD-KVRETUR-TOTU     PIC Z(3)9.                                   
006300     03 MOD-KVRETUR-TOTG     PIC Z(3)9.                                   
006400     03 MOD-TEXTRAD          PIC X(30).                                   
006500     03 MOD-OBJNR-SPAERR-ATTR                                             
006600                             PIC X(2).                                    
006700*                                 MFS ATTRIBUTFÄLT                        
006800     03 MOD-OBJNR-SPAERR     PIC Z(8)9.                                   
006900*                                 OBJEKTNUMMER                            
007000     03 MOD-IDBYTRAD-3173    PIC 9(5).                                    
007100*                                 RADNUMMER                               
007200     03 MOD-ANTAL-IN-ATTR    PIC X(2).                                    
007300*                                 MFS ATTRIBUTFÄLT                        
007400     03 MOD-ANTAL-IN         PIC Z(2)9.                                   
007500     03 MOD-ANMARK-IN-ATTR   PIC X(2).                                    
007600*                                 MFS ATTRIBUTFÄLT                        
007700     03 MOD-ANMARK-IN        PIC X(3).                                    
007800     03 MOD-FLSKROT-IN-ATTR  PIC X(2).                                    
007900*                                 MFS ATTRIBUTFÄLT                        
008000     03 MOD-FLSKROT-IN       PIC X.                                       
008100*                                 SKROTNINGSMARKERING                     
008200     03 MOD-GODK-IN-ATTR     PIC X(2).                                    
008300*                                 MFS ATTRIBUTFÄLT                        
008400     03 MOD-GODK-IN          PIC X.                                       
008500     03 MOD-TEMFSINF         PIC X(55).                                   
008600*                                 INFORMATIONSMEDDELANDE                  
008700*** END OF VILMAII-COPY LENGTH= 868 BYTES                                 
