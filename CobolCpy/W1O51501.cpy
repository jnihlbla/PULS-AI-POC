000100 01  MOD-W1O51501.                                                        
000200*                                 MOD-COPYTEXT FÖR BILD 1515              
000300*                                 VADIS/AVSNITT SÖKBEGREPP                
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
003600     03 MOD-FLAVSTVAD-ATTR   PIC X(2).                                    
003700*                                 MFS ATTRIBUTFÄLT                        
003800     03 MOD-FLAVSTVAD        PIC X.                                       
003900*                                 AVSNITTET SKICKAS TILL VADIS?           
004000     03 MOD-KDCATPUB-R-TOM   PIC X(3).                                    
004100*                                 3 HÖGRASTE TECKNEN I KDCATPUB           
004200     03 MOD-RAD              OCCURS 10 TIMES.                             
004300*                                 UTDATA-RADER FÖR VADIS                  
004400*                                 SÖKBEGREPP PÅ AVSNITT                   
004500        05 MOD-IDKOL-ATTR    PIC X(2).                                    
004600*                                 MFS ATTRIBUTFÄLT                        
004700        05 MOD-IDKOL         PIC X.                                       
004800*                                 KOLUMN-ID (A-E)                         
004900        05 MOD-FLEXCL-ATTR   PIC X(2).                                    
005000*                                 MFS ATTRIBUTFÄLT                        
005100        05 MOD-FLEXCL        PIC X.                                       
005200*                                 NYCKELVÄRDEN EXCLUDERAS?                
005300        05 MOD-IDMODELL-ATTR PIC X(2).                                    
005400*                                 MFS ATTRIBUTFÄLT                        
005500        05 MOD-IDMODELL      PIC X(3).                                    
005600*                                 BILENS NUMERISKA MODELLBET.             
005700        05 MOD-TIMODAAR-STA-ATTR                                          
005800                             PIC X(2).                                    
005900*                                 MFS ATTRIBUTFÄLT                        
006000        05 MOD-TIMODAAR-STA  PIC Z(4).                                    
006100*                                 MODELLÅR (ÅÅÅÅ) STARTÅR                 
006200        05 MOD-TIMODAAR-STO-ATTR                                          
006300                             PIC X(2).                                    
006400*                                 MFS ATTRIBUTFÄLT                        
006500        05 MOD-TIMODAAR-STO  PIC Z(4).                                    
006600*                                 MODELLÅR (ÅÅÅÅ) STOPPÅR                 
006700        05 MOD-IDVARIANT-ATTR                                             
006800                             PIC X(2).                                    
006900*                                 MFS ATTRIBUTFÄLT                        
007000        05 MOD-IDVARIANT     PIC X(15).                                   
007100*                                 BILVARIANT                              
007200        05 MOD-IDVARIANT-2-ATTR                                           
007300                             PIC X(2).                                    
007400*                                 MFS ATTRIBUTFÄLT                        
007500        05 MOD-IDVARIANT-2   PIC X(15).                                   
007600*                                 ASSOCIERAD (2:A) BILVARIANT             
007700        05 MOD-KDCHATYP-ATTR PIC X(2).                                    
007800*                                 MFS ATTRIBUTFÄLT                        
007900        05 MOD-KDCHATYP      PIC Z.                                       
008000*                                 CHASSINUMMER-TYP                        
008100        05 MOD-IDCHASSI-STA-ATTR                                          
008200                             PIC X(2).                                    
008300*                                 MFS ATTRIBUTFÄLT                        
008400        05 MOD-IDCHASSI-STA  PIC Z(6).                                    
008500*                                 CHASSINUMMER START                      
008600        05 MOD-IDCHASSI-STO-ATTR                                          
008700                             PIC X(2).                                    
008800*                                 MFS ATTRIBUTFÄLT                        
008900        05 MOD-IDCHASSI-STO  PIC Z(6).                                    
009000*                                 CHASSINUMMER STOPP                      
009100        05 MOD-IDRADNR       PIC 9(4).                                    
009200*                                 RADNUMMER                               
009300     03 MOD-TEMFSINF         PIC X(55).                                   
009400*                                 INFORMATIONSMEDDELANDE                  
009500*** END OF VILMAII-COPY LENGTH= 947 BYTES                                 
