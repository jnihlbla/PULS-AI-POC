000100 01  RESP-W30172O1.                                                       
000200*                                 COPYTEXT FÖR MOD                        
000300*                                 W3O17201                                
000400     03 RESP-KDPRT-ATTR      PIC X(2).                                    
000500*                                 MFS ATTRIBUTFÄLT                        
000600     03 RESP-KDPRT           PIC X(3).                                    
000700*                                 PRINTERKOD                              
000800     03 RESP-IDBYTRAD-START  PIC 9(5).                                    
000900*                                 RADNUMMER                               
001000     03 RESP-IDBYTRAD-NEXT   PIC 9(5).                                    
001100*                                 RADNUMMER                               
001200     03 RESP-FLAGGA          PIC X.                                       
001300*                                 ALLMÄN FLAGGA                           
001400     03 RESP-IDKUNDNR        PIC Z(7).                                    
001500*                                 KUNDNUMMER                              
001600     03 RESP-IDFAKT          PIC Z(6)9.                                   
001700*                                 FAKTURANUMMER                           
001800     03 RESP-KDBYTSTA-RAPP   PIC X.                                       
001900*                                 STATUSKOD BYTESOBJEKT                   
002000     03 RESP-IDUSER-GODK     PIC X(8).                                    
002100*                                 ANVÄNDARENS SÄKERHETS ID                
002200     03 RESP-KVRETUR-TOTU    PIC Z(4)9.                                   
002300*                                 ANTAL I RETUR                           
002400     03 RESP-KVRETUR-TOTG    PIC Z(4)9.                                   
002500*                                 ANTAL I RETUR                           
002600     03 RESP-IDMSG-VIPS      PIC X(3).                                    
002700*                                 MEDDELANDE NUMMER                       
002800     03 RESP-IDARTNR-OBJ-SPAERR-ATTR                                      
002900                             PIC X(2).                                    
003000*                                 MFS ATTRIBUTFÄLT                        
003100     03 RESP-IDARTNR-OBJ-SPAERR                                           
003200                             PIC Z(8)9.                                   
003300*                                 OBJEKTNUMMER                            
003400     03 RESP-IDBYTRAD-3173   PIC 9(5).                                    
003500*                                 RADNUMMER                               
003600     03 RESP-KVRETUR-IN-ATTR PIC X(2).                                    
003700*                                 MFS ATTRIBUTFÄLT                        
003800     03 RESP-KVRETUR-IN      PIC Z(4)9.                                   
003900*                                 ANTAL I RETUR                           
004000     03 RESP-KDBYTREF-IN-ATTR                                             
004100                             PIC X(2).                                    
004200*                                 MFS ATTRIBUTFÄLT                        
004300     03 RESP-KDBYTREF-IN     PIC X(3).                                    
004400*                                 CENTRAL REFERENS                        
004500     03 RESP-FLSKROT-IN-ATTR PIC X(2).                                    
004600*                                 MFS ATTRIBUTFÄLT                        
004700     03 RESP-FLSKROT-IN      PIC X.                                       
004800*                                 SKROTNINGSMARKERING                     
004900     03 RESP-FLGODK-IN-ATTR  PIC X(2).                                    
005000*                                 MFS ATTRIBUTFÄLT                        
005100     03 RESP-FLGODK-IN       PIC X.                                       
005200*                                 GODKÄNT?  JA/NEJ                        
005300     03 RESP-KVRADER         PIC 9(5).                                    
005400*                                 ANTAL RADER                             
005500     03 RESP-RADINFO         OCCURS 300 TIMES.                            
005600        05 RESP-KDCMD-LINE-ATTR                                           
005700                             PIC X(2).                                    
005800*                                 MFS ATTRIBUTFÄLT                        
005900        05 RESP-KDCMD-LINE   PIC X.                                       
006000*                                 RAD-UPPDATERINGSKOMMANDO                
006100*                                  BLANK  = INGENTING                     
006200*                                  D , B  = DELETE                        
006300*                                  R , Ä  = REPLACE                       
006400*                                  I,N,A  = INSERT                        
006500*                                  S , V  = SELECT                        
006600*                                  P , P  = PRINT                         
006700*                                  C , K  = COPY                          
006800        05 RESP-IDARTNR-OBJ-LINE                                          
006900                             PIC Z(7)9.                                   
007000*                                 ARTIKELNUMMER                           
007100        05 RESP-KVPOINT-LINE PIC Z(6).                                    
007200*                                 POINT VALUE                             
007300        05 RESP-KVRETUR-URSP-LINE                                         
007400                             PIC Z(5).                                    
007500*                                 ANTAL I RETUR                           
007600        05 RESP-KVRETUR-GODK-LINE-ATTR                                    
007700                             PIC X(2).                                    
007800*                                 MFS ATTRIBUTFÄLT                        
007900        05 RESP-KVRETUR-GODK-LINE                                         
008000                             PIC Z(5).                                    
008100*                                 ANTAL I RETUR                           
008200        05 RESP-KDBYTSTA-LINE                                             
008300                             PIC X.                                       
008400*                                 STATUSKOD BYTESOBJEKT                   
008500        05 RESP-KDBYTREF-LINE-ATTR                                        
008600                             PIC X(2).                                    
008700*                                 MFS ATTRIBUTFÄLT                        
008800        05 RESP-KDBYTREF-LINE                                             
008900                             PIC X(3).                                    
009000*                                 CENTRAL REFERENS                        
009100        05 RESP-FLSKROT-LINE PIC X.                                       
009200*                                 SKROTNINGSMARKERING                     
009300        05 RESP-BEART-LINE   PIC X(25).                                   
009400*                                 ARTIKELBENÄMNING                        
009500        05 RESP-IDBYTRAD-LINE                                             
009600                             PIC 9(5).                                    
009700*                                 RADNUMMER                               
009800        05 RESP-BERADREF-LINE                                             
009900                             PIC X(10).                                   
010000*                                 KUNDENS RADREFERENS                     
010100        05 RESP-FLAGGA-LOC-LINE                                           
010200                             PIC X.                                       
010300*                                 ALLMÄN FLAGGA                           
010400        05 RESP-IDTABNR-LINE PIC 9(3).                                    
010500*                                 TABELLNUMMER                            
010600*** END OF VILMAII-COPY LENGTH= 24091 BYTES                               
