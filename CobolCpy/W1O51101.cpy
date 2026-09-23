000100 01  MOD-W1O51101.                                                        
000200*                                 MOD-COPYTEXT FÖR BILD                   
000300*                                 FRÅGA KATALOGRADER                      
000400     03 MOD-IDTRANS          PIC X(4).                                    
000500*                                 BILDNUMMER                              
000600     03 MOD-TEMFSFEL         PIC X(40).                                   
000700*                                 MFS FELMEDDELANDE                       
000800     03 MOD-IDCATNR-IN       PIC X(5).                                    
000900*                                 KATALOG-ID                              
001000     03 MOD-IDCATNR-UT       PIC X(5).                                    
001100*                                 KATALOG-ID                              
001200     03 MOD-IDCATGRP-IN      PIC X(2).                                    
001300*                                 KATALOG-GRUPP                           
001400     03 MOD-IDCATGRP-UT      PIC X(2).                                    
001500*                                 KATALOG-GRUPP                           
001600     03 MOD-IDCATAVS-IN      PIC X(4).                                    
001700*                                 KATALOG-AVSNITT                         
001800     03 MOD-IDCATAVS-UT      PIC X(4).                                    
001900*                                 KATALOG-AVSNITT                         
002000     03 MOD-IDSKYLT-IN       PIC X(3).                                    
002100*                                 NATIONALITETSTECKEN                     
002200*                                 SPRÅKIDENTIFIKATION                     
002300     03 MOD-IDSKYLT-UT       PIC X(3).                                    
002400*                                 NATIONALITETSTECKEN                     
002500*                                 SPRÅKIDENTIFIKATION                     
002600     03 MOD-IDCATRAD-IN      PIC X(4).                                    
002700*                                 RADNUMMER                               
002800     03 MOD-IDCATRAD-UT      PIC X(4).                                    
002900*                                 RADNUMMER                               
003000     03 MOD-KDCATPUB-R-FOM-IN                                             
003100                             PIC X(3).                                    
003200*                                 3 HÖGRASTE TECKNEN I KDCATPUB           
003300     03 MOD-KDCATPUB-R-FOM-UT                                             
003400                             PIC X(3).                                    
003500*                                 3 HÖGRASTE TECKNEN I KDCATPUB           
003600     03 MOD-IDCATRAD-ENTER   PIC Z(3)9.                                   
003700*                                 RADNUMMER                               
003800     03 MOD-IDCATRAD-NEXT    PIC Z(3)9.                                   
003900*                                 RADNUMMER                               
004000     03 MOD-KDFORDON         PIC X(2).                                    
004100*                                 FORDONSSLAG                             
004200     03 MOD-KDCATPUB-R-MIN   PIC X(3).                                    
004300*                                 3 HÖGRASTE TECKNEN I KDCATPUB           
004400     03 MOD-KDCATPUB-R-MAX   PIC X(3).                                    
004500*                                 3 HÖGRASTE TECKNEN I KDCATPUB           
004600     03 MOD-TEKOL            OCCURS 5 TIMES                               
004700                             PIC X(25).                                   
004800*                                 KOLUMNTEXT                              
004900     03 MOD-BERUBTXT         OCCURS 3 TIMES                               
005000                             PIC X(30).                                   
005100*                                 RUBRIKTEXT                              
005200     03 MOD-IDILLU           PIC Z(5).                                    
005300*                                 ILLUSTRATIONENS NR                      
005400     03 MOD-IDVERS           PIC Z(2)9.                                   
005500*                                 UTGÅVA                                  
005600     03 MOD-FLAVSTVAD        PIC X.                                       
005700*                                 AVSNITTET SKICKAS TILL VADIS?           
005800     03 MOD-FLAVSUST         PIC X.                                       
005900*                                 AVSNITTET ÄNDRAT?                       
006000     03 MOD-BERUBTEXT.                                                    
006100        05 MOD-BERUBTEXT-1   PIC X(30).                                   
006200*                                 RUBRIKTEXT                              
006300        05 MOD-BERUBTEXT-2   OCCURS 2 TIMES                               
006400                             PIC X(30).                                   
006500*                                 RUBRIKTEXT                              
006600     03 FILLER               OCCURS 12 TIMES.                             
006700        05 MOD-IDCATRAD      PIC Z(3)9.                                   
006800*                                 RADNUMMER                               
006900        05 MOD-KDFBX         PIC X.                                       
007000*                                 FBX-KOD                                 
007100        05 MOD-IDCATPOS      PIC X(3).                                    
007200*                                 POSITIONSNUMMER                         
007300        05 MOD-IDARTNR       PIC Z(9).                                    
007400*                                 ARTIKELNUMMER                           
007500        05 MOD-KVKOL-GRP.                                                 
007600           07 MOD-KVKOL      OCCURS 5 TIMES                               
007700                             PIC X(3).                                    
007800*                                 ANTAL AV ARTIKEL I RESP KOLUMN          
007900        05 MOD-KDPS          PIC X(2).                                    
008000*                                 ARTIKELSTATUS                           
008100        05 MOD-KVPUNKT       PIC Z.                                       
008200*                                 ANTAL INDRAGNINGSPUNKTER                
008300        05 MOD-BEART         PIC X(30).                                   
008400*                                 RUBRIKTEXT                              
008500        05 MOD-TEKATANM      PIC X(23).                                   
008600*                                 ANMÄRKNINGSTEXT                         
008700        05 MOD-KDRADST       PIC X.                                       
008800*                                 RAD-     L = LÅNAD.                     
008900*                                 STATUS   Ä = ÄNDRAD.                    
009000*                                          N = NYREGISTRERAD.             
009100*                                          C = L,Ä,N EFTER OMBRYT         
009200*                                              FRAM TILL VADGEN.          
009300*                                      SPACE = OFÖRÄNDRAD.                
009400        05 MOD-FLNOTE        PIC X.                                       
009500*                                 NOTERINGSFLAGGA                         
009600        05 MOD-KDERS         PIC Z(2).                                    
009700*                                 ERSÄTTNINGSKOD                          
009800        05 MOD-FLH-REFERENS  PIC X.                                       
009900*                                 FINNS HÄNVISNING ?                      
010000        05 MOD-FLH           PIC X.                                       
010100*                                 FINNS HÄNVISNING ?                      
010200     03 MOD-TEMFSINF         PIC X(55).                                   
010300*                                 INFORMATIONSMEDDELANDE                  
010400*** END OF VILMAII-COPY LENGTH= 1600 BYTES                                
