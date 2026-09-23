000100 01  W2O12901.                                                            
000200     03 IDTRANS              PIC X(4).                                    
000300*                                 BILDNUMMER                              
000400     03 TEMFSFEL             PIC X(40).                                   
000500*                                 MFS FELMEDDELANDE                       
000600     03 IDARTNR-IN           PIC X(9).                                    
000700*                                 ARTIKELNUMMER                           
000800     03 IDARTNR-UT           PIC X(9).                                    
000900*                                 ARTIKELNUMMER                           
001000     03 AREA.                                                             
001100        05 C2FAELT-SW        PIC X.                                       
001200*                                 FLAGGA OM C2 FINNS                      
001300        05 BEN               PIC X(25).                                   
001400*                                 ARTIKELBENÄMNING                        
001500        05 KVOKS-C1          PIC -(7)9.                                   
001600*                                 ORDERKÖSALDO                            
001700        05 KVOKS-C2          PIC -(7)9.                                   
001800*                                 ORDERKÖSALDO                            
001900        05 KVDISP-C1         PIC -(7)9.                                   
002000*                                 DISPONIBELT LAGER                       
002100        05 KVDISP-C2         PIC -(7)9.                                   
002200*                                 DISPONIBELT LAGER                       
002300        05 KVAK-C1           PIC -(7)9.                                   
002400*                                 ANKOMSTSALDO                            
002500        05 KVAK-C2           PIC -(7)9.                                   
002600*                                 ANKOMSTSALDO                            
002700        05 SUTPO-TOT-C1      PIC -(7)9.                                   
002800*                                 TPO-KVANTITET, TOTAL                    
002900        05 SUTPO-TOT-C2      PIC -(7)9.                                   
003000*                                 TPO-KVANTITET, TOTAL                    
003100        05 KV-SPANT-C1       PIC -(7)9.                                   
003200*                                 SPÄRRAT ANTAL                           
003300        05 KV-SPANT-C2       PIC -(7)9.                                   
003400*                                 SPÄRRAT ANTAL                           
003500        05 KVOFFERT-C1       PIC -(6)9.                                   
003600*                                 OFFERTSALDO                             
003700        05 KVOFFERT-C2       PIC -(6)9.                                   
003800*                                 OFFERTSALDO                             
003900        05 KDLTK             PIC 9.                                       
004000*                                 LAGERTILLHÖRIGHETSKOD                   
004100        05 KDUART            PIC X.                                       
004200*                                 UNDANTAGSARTIKEL                        
004300        05 KVSLUTKP-VISN     PIC Z(6)9.                                   
004400*                                 SLUTKÖPSSALDO                           
004500        05 TISLUTKP-VISN     PIC 9(6).                                    
004600        05 KDERS-C1          PIC Z9.                                      
004700*                                 ERSÄTTNINGSKOD                          
004800        05 KDERS-C2          PIC Z9.                                      
004900*                                 ERSÄTTNINGSKOD                          
005000        05 FLSKRSP-C1-VISN-ATTR                                           
005100                             PIC X(2).                                    
005200*                                 MFS ATTRIBUTFÄLT                        
005300        05 FLSKRSP-C1-VISN   PIC X(2).                                    
005400*                                 MFS BEHANDLING AV INPUTFÄLT             
005500        05 FLSKRSP-C2-VISN-ATTR                                           
005600                             PIC X(2).                                    
005700*                                 MFS ATTRIBUTFÄLT                        
005800        05 FLSKRSP-C2-VISN   PIC X(2).                                    
005900*                                 MFS BEHANDLING AV INPUTFÄLT             
006000        05 TISKROT-BEORD-C1  PIC 9(6).                                    
006100*                                 SKROTNINGSDATUM                         
006200        05 TISKROT-BEORD-C2  PIC 9(6).                                    
006300*                                 SKROTNINGSDATUM                         
006400        05 FLSKROT-AUTO      PIC X.                                       
006500*                                 SKROTNING AUTOMATISKT BEORDRAD          
006600        05 KVSKROT-C1        PIC -(7)9.                                   
006700*                                 ANTAL SENASTE SKROTORDER                
006800        05 KVSKROT-C2        PIC -(7)9.                                   
006900*                                 ANTAL SENASTE SKROTORDER                
007000        05 TISKROT-C1        PIC 9(6).                                    
007100*                                 SKROTNINGSDATUM                         
007200        05 TISKROT-C2        PIC 9(6).                                    
007300*                                 SKROTNINGSDATUM                         
007400        05 TISKROT-AUTO-UT   PIC 9(6).                                    
007500*                                 STOPDATE AUTO-SKROTNING                 
007600        05 TISKROT-AUTO-IN-ATTR                                           
007700                             PIC X(2).                                    
007800*                                 MFS ATTRIBUTFÄLT                        
007900        05 TISKROT-AUTO-IN   PIC 9(6).                                    
008000*                                 STOPDATE AUTO-SKROTNING                 
008100        05 KDERS-ATTR        PIC X(2).                                    
008200*                                 MFS ATTRIBUTFÄLT                        
008300        05 KDERS             PIC X(2).                                    
008400*                                 ERSÄTTNINGSKOD                          
008500        05 KDCLAGER-ATTR     PIC X(2).                                    
008600*                                 MFS ATTRIBUTFÄLT                        
008700        05 KDCLAGER          PIC X.                                       
008800*                                 CENTRALLAGERKOD                         
008900        05 KVKVAR-ATTR       PIC X(2).                                    
009000*                                 MFS ATTRIBUTFÄLT                        
009100        05 KVKVAR            PIC X(7).                                    
009200*                                 KVARLIGGANDE ANTAL                      
009300        05 KVSKRANT          PIC Z(6)9.                                   
009400*                                 ANTAL SENASTE SKROTORDER                
009500        05 IDKONTO-ATTR      PIC X(2).                                    
009600*                                 MFS ATTRIBUTFÄLT                        
009700        05 IDKONTO           PIC X(10).                                   
009800*                                 KONTO                                   
009900        05 IDANALYS-ATTR     PIC X(2).                                    
010000*                                 MFS ATTRIBUTFÄLT                        
010100        05 IDANALYS          PIC X(12).                                   
010200*                                 ANALYSNUMMER                            
010300        05 FLSKROT-CLASS-ATTR                                             
010400                             PIC X(2).                                    
010500*                                 MFS ATTRIBUTFÄLT                        
010600        05 FLSKROT-CLASS     PIC X.                                       
010700*                                 SKROTNINGEN TILL CLASSIC                
010800        05 BELAGINS30-ATTR   PIC X(2).                                    
010900*                                 MFS ATTRIBUTFÄLT                        
011000        05 BELAGINS30        PIC X(30).                                   
011100     03 LINE23.                                                           
011200        05 TEMFSINF          PIC X(55).                                   
011300*                                 INFORMATIONSMEDDELANDE                  
011400*** END OF VILMAII-COPY LENGTH= 403 BYTES                                 
