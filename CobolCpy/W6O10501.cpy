000100 01  MOD-W6O10501.                                                        
000200*                                 COPYTEXT FOR MOD W6O10501               
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-ADINLOMR-IN      PIC X(4).                                    
000800*                                 INLEVERANSOMRÅDE                        
000900     03 MOD-IDDC-IN          PIC X(2).                                    
001000*                                 IDENTIFIERARE LAGER                     
001100     03 MOD-ADINLOMR-UT      PIC X(4).                                    
001200*                                 INLEVERANSOMRÅDE                        
001300     03 MOD-IDDC-UT          PIC X(2).                                    
001400*                                 IDENTIFIERARE LAGER                     
001500     03 MOD-IDARTNR-ENTER    PIC X(9).                                    
001600*                                 ARTIKELNUMMER                           
001700     03 MOD-IDARTNR-NEXT     PIC X(9).                                    
001800*                                 ARTIKELNUMMER                           
001900     03 MOD-IDFKNGRP-ENTER   PIC X(4).                                    
002000*                                 FUNKTIONSGRUPP                          
002100     03 MOD-IDFKNGRP-NEXT    PIC X(4).                                    
002200*                                 FUNKTIONSGRUPP                          
002300     03 MOD-IDLEVNR-ENTER    PIC X(5).                                    
002400*                                 LEVERANTÖRNUMMER                        
002500     03 MOD-IDLEVNR-NEXT     PIC X(5).                                    
002600*                                 LEVERANTÖRNUMMER                        
002700     03 MOD-BEFT-ENTER       PIC X(2).                                    
002800*                                 FÖRPACKNINGSTYP                         
002900     03 MOD-BEFT-NEXT        PIC X(2).                                    
003000*                                 FÖRPACKNINGSTYP                         
003100     03 MOD-RAD              OCCURS 12 TIMES.                             
003200*                                 LINES                                   
003300        05 MOD-KDCMD-RAD-ATTR                                             
003400                             PIC X(2).                                    
003500*                                 MFS ATTRIBUTFÄLT                        
003600        05 MOD-KDCMD-RAD     PIC X.                                       
003700*                                 RAD-UPPDATERINGSKOMMANDO                
003800*                                  BLANK  = INGENTING                     
003900*                                  D , B  = DELETE                        
004000*                                  R , Ä  = REPLACE                       
004100*                                  I , N  = INSERT                        
004200*                                  S , V  = SELECT                        
004300        05 MOD-ADINLOMR-RAD-ATTR                                          
004400                             PIC X(2).                                    
004500*                                 MFS ATTRIBUTFÄLT                        
004600        05 MOD-ADINLOMR-RAD  PIC X(4).                                    
004700*                                 INLEVERANSOMRÅDE                        
004800        05 MOD-IDARTNR-FOM-ATTR                                           
004900                             PIC X(2).                                    
005000*                                 MFS ATTRIBUTFÄLT                        
005100        05 MOD-IDARTNR-FOM   PIC Z(7)9.                                   
005200*                                 ARTIKELNUMMER FRÅN OCH MED              
005300        05 MOD-IDARTNR-TOM-ATTR                                           
005400                             PIC X(2).                                    
005500*                                 MFS ATTRIBUTFÄLT                        
005600        05 MOD-IDARTNR-TOM   PIC Z(7)9.                                   
005700*                                 ARTIKELNUMMER TILL OCH MED              
005800        05 MOD-IDFKNGRP-FOM-ATTR                                          
005900                             PIC X(2).                                    
006000*                                 MFS ATTRIBUTFÄLT                        
006100        05 MOD-IDFKNGRP-FOM  PIC Z(3)9.                                   
006200*                                 FUNKTIONSGRUPP-FROM                     
006300        05 MOD-IDFKNGRP-TOM-ATTR                                          
006400                             PIC X(2).                                    
006500*                                 MFS ATTRIBUTFÄLT                        
006600        05 MOD-IDFKNGRP-TOM  PIC Z(3)9.                                   
006700*                                 FUNKTIONSGRUPP-TOM                      
006800        05 MOD-IDLEVNR-ATTR  PIC X(2).                                    
006900*                                 MFS ATTRIBUTFÄLT                        
007000        05 MOD-IDLEVNR       PIC X(5).                                    
007100*                                 LEVERANTÖRNUMMER                        
007200        05 MOD-BEFT-FOM-ATTR PIC X(2).                                    
007300*                                 MFS ATTRIBUTFÄLT                        
007400        05 MOD-BEFT-FOM      PIC Z9.                                      
007500*                                 FÖRPACKNINGSTYP FR O M                  
007600        05 MOD-BEFT-TOM-ATTR PIC X(2).                                    
007700*                                 MFS ATTRIBUTFÄLT                        
007800        05 MOD-BEFT-TOM      PIC Z9.                                      
007900*                                 FÖRPACKNINGSTYP T O M                   
008000     03 MOD-INPUT.                                                        
008100*                                 LINES                                   
008200        05 MOD-ADINLOMR-INPUT-ATTR                                        
008300                             PIC X(2).                                    
008400*                                 MFS ATTRIBUTFÄLT                        
008500        05 MOD-ADINLOMR-INPUT                                             
008600                             PIC X(4).                                    
008700*                                 INLEVERANSOMRÅDE                        
008800        05 MOD-IDARTNR-FOM-INPUT-ATTR                                     
008900                             PIC X(2).                                    
009000*                                 MFS ATTRIBUTFÄLT                        
009100        05 MOD-IDARTNR-FOM-INPUT                                          
009200                             PIC X(8).                                    
009300*                                 ARTIKELNUMMER FRÅN OCH MED              
009400        05 MOD-IDARTNR-TOM-INPUT-ATTR                                     
009500                             PIC X(2).                                    
009600*                                 MFS ATTRIBUTFÄLT                        
009700        05 MOD-IDARTNR-TOM-INPUT                                          
009800                             PIC X(8).                                    
009900*                                 ARTIKELNUMMER TILL OCH MED              
010000        05 MOD-IDFKNGRP-FOM-INPUT-ATTR                                    
010100                             PIC X(2).                                    
010200*                                 MFS ATTRIBUTFÄLT                        
010300        05 MOD-IDFKNGRP-FOM-INPUT                                         
010400                             PIC X(4).                                    
010500*                                 FUNKTIONSGRUPP-FROM                     
010600        05 MOD-IDFKNGRP-TOM-INPUT-ATTR                                    
010700                             PIC X(2).                                    
010800*                                 MFS ATTRIBUTFÄLT                        
010900        05 MOD-IDFKNGRP-TOM-INPUT                                         
011000                             PIC X(4).                                    
011100*                                 FUNKTIONSGRUPP-TOM                      
011200        05 MOD-IDLEVNR-INPUT-ATTR                                         
011300                             PIC X(2).                                    
011400*                                 MFS ATTRIBUTFÄLT                        
011500        05 MOD-IDLEVNR-INPUT PIC X(5).                                    
011600*                                 LEVERANTÖRNUMMER                        
011700        05 MOD-BEFT-FOM-INPUT-ATTR                                        
011800                             PIC X(2).                                    
011900*                                 MFS ATTRIBUTFÄLT                        
012000        05 MOD-BEFT-FOM-INPUT                                             
012100                             PIC X(2).                                    
012200*                                 FÖRPACKNINGSTYP FR O M                  
012300        05 MOD-BEFT-TOM-INPUT-ATTR                                        
012400                             PIC X(2).                                    
012500*                                 MFS ATTRIBUTFÄLT                        
012600        05 MOD-BEFT-TOM-INPUT                                             
012700                             PIC X(2).                                    
012800*                                 FÖRPACKNINGSTYP T O M                   
012900     03 MOD-TEMFSINF         PIC X(55).                                   
013000*                                 INFORMATIONSMEDDELANDE                  
013100*** END OF VILMAII-COPY LENGTH= 876 BYTES                                 
