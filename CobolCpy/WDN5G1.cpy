000100 01  AVSG-WDN5G1.                                                         
000200*                                 KATALOG AVSNITTSREGISTER                
000300*                                 SECONDARY INDEX PÅ                      
000400*                                           HÄNVISNING                    
000500*                                 FYSISK NYCKEL:  WDN5G1KY                
000600*                                 (IDCATNR + IDCATGRP                     
000700*                                  + IDCATAVS + IDCATRAD                  
000800*                                  + IDCATPUB + IDWDN512)                 
000900*                                 SECONDARY NYCKEL: WDN5GSEQ              
001000*                                 (IDCATNR + IDCATGRP                     
001100*                                  + IDCATAVS + IDCATRAD                  
001200*                                  + IDCATPUB)                            
001300     03 AVSG-IDCATRKY.                                                    
001400*                                 NYCKEL TILL KATALOGRAD                  
001500        05 AVSG-IDCATNR      PIC 9(5).                                    
001600*                                 KATALOG-ID                              
001700*                                 CATALOG-ID                              
001800        05 AVSG-IDCATGRP     PIC 9(2).                                    
001900*                                 KATALOG-GRUPP                           
002000*                                 CATALOG-GROUP                           
002100        05 AVSG-IDCATAVS     PIC 9(4).                                    
002200*                                 KATALOG-AVSNITT                         
002300*                                 CATALOG TEXT BLOCK                      
002400        05 AVSG-IDCATRAD     PIC 9(4).                                    
002500*                                 RADNUMMER                               
002600*                                 ROW NUMBER IN TEXT BLOCK                
002700        05 AVSG-KDCATPUB-FOM PIC X(6).                                    
002800*                                 PUBLICERINGS TIDKOD, F.O.M.             
002900*                                 RELEASE TIME CODE, FROM                 
003000     03 AVSG-IDWDN512        PIC X(21).                                   
003100*                                 NYCKEL TILL WDN512                      
003200*                                 KEY TO WDN512                           
003300     03 AVSG-KDHAEN          PIC X.                                       
003400*                                 HÄNVISNINGSKODER VÄRDEN:                
003500*                                  A = HÄNVISNING I ANM. FÄLTET           
003600*                                  B = HÄNVISNING I BEN. FÄLTET           
003700*                                  * = HÄNVISNING, EJ KLAR DEST.          
003800*                                  SPACE = INGEN HÄNVISNING               
003900*                                 REFERENCE CODE   VALUES:                
004000*                                  A = REFERENCE IN NOTE-FIELD            
004100*                                  B = REFERENCE IN DESCR-FIELD           
004200*                                  * = REFERENCE DURING EDITING           
004300*                                  SPACE = NO REFERENCE                   
004400*** END OF VILMAII-COPY LENGTH= 43 BYTES                                  
