000100 01  W4266301.                                                            
000200*                                 KVALITET KONTROLLKOD                    
000300*                                 UPPDATERING AV W6D2                     
000400     03 IDARTNR              PIC S9(9)           COMP-3.                  
000500*                                 ARTIKELNUMMER                           
000600     03 ADKVAULG             PIC X(2).                                    
000700*                                 PLATS UNDERLAG KVAL.KONTROLL            
000800     03 BEART                PIC X(25).                                   
000900*                                 ARTIKELBENÄMNING                        
001000     03 IDBERED              PIC S9(3)           COMP-3.                  
001100*                                 BEREDARENUMMER                          
001200     03 IDFKNGRP             PIC S9(5)           COMP-3.                  
001300*                                 FUNKTIONSGRUPP                          
001400     03 IDLEVNR-OLD          PIC X(5).                                    
001500*                                 LEVERANTÖRNUMMER                        
001600     03 IDLEVNR-NEW          PIC X(5).                                    
001700*                                 LEVERANTÖRNUMMER                        
001800     03 KDERS                PIC S9(3)           COMP-3.                  
001900*                                 ERSÄTTNINGSKOD                          
002000     03 KDFARLIG             PIC S9              COMP-3.                  
002100*                                 KOD FÖR FARLIGT GODS                    
002200     03 BEFT                 PIC S9(3)           COMP-3.                  
002300*                                 FÖRPACKNINGSTYP                         
002400     03 KDPRODSL             PIC S9(3)           COMP-3.                  
002500*                                 PRODUKTSLAG                             
002600     03 KDKVAKTL.                                                         
002700*                                 KVALITETSKONTROLL KOD                   
002800        05 KDKVATYP          PIC X.                                       
002900*                                 NORMAL/VERIFIKATIONS KVAL.KONTR         
003000        05 IDPROVPL-PRI      PIC X.                                       
003100*                                 PROVTAGNINGSPLAN PRIM.KONTROLL          
003200        05 IDPROVPL-SEK      PIC X.                                       
003300*                                 PROVTAGNINGSPLAN SEK.KONTROLL           
003400        05 KDKVAULG          PIC X.                                       
003500*                                 UNDERLAG FÖR KVALITETSKONTROLL          
003600     03 KDVVKL               PIC S9              COMP-3.                  
003700*                                 VOLYMVÄRDESKLASS                        
003800     03 KDYTBEH              PIC S9(3)           COMP-3.                  
003900*                                 YTBEHANDLINGSKOD                        
004000     03 TEXT                 PIC X(11).                                   
004100     03 PRARTSTD             PIC S9(7)V9(2)      COMP-3.                  
004200*                                 ARTIKELSTANDARDPRIS                     
004300     03 TIFINLV              PIC S9(5)           COMP-3.                  
004400*                                 PUBLICERINGSVECKA, (ÅÅVVD  D=1)         
004500     03 TIKVASAK             PIC S9(7)           COMP-3.                  
004600*                                 DATUM KVALITETSSÄKRAD                   
004700     03 IDANSK               PIC S9(3)           COMP-3.                  
004800*                                 ANSKAFFARNUMMER                         
004900*** END OF VILMAII-COPY LENGTH= 86 BYTES                                  
