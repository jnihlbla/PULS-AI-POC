000100 01  W4O10501.                                                            
000200*                                 COPYTEXT FÖR MOD W4O10501.              
000300*                                                                         
000400     03 IDTRANS              PIC X(4).                                    
000500*                                 BILDNUMMER                              
000600     03 MESSAGE-RAD1         PIC X(40).                                   
000700*                                 MEDDELANDEFÄLT PÅ RAD 1                 
000800     03 KVERS-SKIP           PIC 9(3).                                    
000900*                                 ANTAL ERS SOM LÄSES FÖRBI               
001000     03 IDARTNR-IN           PIC X(9).                                    
001100*                                 ARTIKELNUMMER                           
001200     03 IDARTNR-UT           PIC X(9).                                    
001300*                                 ARTIKELNUMMER                           
001400     03 STRECK               PIC X.                                       
001500     03 AREA.                                                             
001600*                                 AREA NOLLSTÄLLS MELLAN VARVEN.          
001700*                                                                         
001800        05 REKSIFFR          PIC X.                                       
001900*                                 KONTROLLSIFFRA                          
002000        05 FLPCOO            PIC X.                                       
002100*                                 FLAGGA OM FÖRMÅNSAVTAL URS.LAND         
002200        05 BEART-SVE         PIC X(25).                                   
002300*                                 SVENSK ARTIKELBENÄMNING                 
002400        05 BEART-ENG         PIC X(25).                                   
002500*                                 ENGELSK ARTIKELBENÄMNING                
002600        05 VLARTNTO          PIC Z(7)9.9.                                 
002700*                                 ARTIKELVOLYM NETTO (CM3)                
002800        05 KDGK              PIC 9.                                       
002900*                                 GODSMOTTAGAREKOD                        
003000        05 TYP1.                                                          
003100           07 FILLER         OCCURS 12 TIMES.                             
003200              09 IDARTNR-TILLK                                            
003300                             PIC Z(8)9.                                   
003400*                                 TILLKOMMANDE ARTIKELNUMMER              
003500              09 FILLER      PIC X(2).                                    
003600              09 DIERS-TILLK PIC Z(3)9.9(3).                              
003700*                                 TILLKOMMANDE ARTIKELANTAL               
003800              09 FILLER      PIC X.                                       
003900        05 TYP2 REDEFINES TYP1.                                           
004000           07 FILLER         OCCURS 12 TIMES.                             
004100              09 BEERS       PIC X(20).                                   
004200*                                 ERSÄTTNINGSTEXT                         
004300        05 VKART             PIC Z(6)9.                                   
004400*                                 ARTIKELVIKT (G)                         
004500        05 KDLTK             PIC 9.                                       
004600*                                 LAGERTILLHÖRIGHETSKOD                   
004700        05 KDSORT            PIC X(2).                                    
004800*                                 SORT-KOD                                
004900        05 KDVVKL            PIC 9.                                       
005000*                                 VOLYMVÄRDESKLASS                        
005100        05 IDFKNGRP          PIC Z(4)9.                                   
005200*                                 FUNKTIONSGRUPP                          
005300        05 KDVSOP            PIC X(3).                                    
005400*                                 VSOP-KOD                                
005500        05 IDANSK            PIC Z(2)9.                                   
005600*                                 ANSKAFFARNUMMER                         
005700        05 KDARTURS          PIC X(2).                                    
005800*                                 ARTIKELURSPRUNGSKOD                     
005900        05 IDLEVNR           PIC X(5).                                    
006000*                                 LEVERANTÖRNUMMER                        
006100        05 IDBERED           PIC Z(2)9.                                   
006200*                                 BEREDARENUMMER                          
006300        05 KDERS             PIC Z(2)9.                                   
006400*                                 ERSÄTTNINGSKOD                          
006500        05 KDSPARR           PIC X.                                       
006600*                                 SPÄRRKOD                                
006700        05 DIERS-ERS         PIC Z(3)9.9(3).                              
006800*                                 ERSATT ARTIKELANTAL                     
006900        05 FLLSRDEL          PIC X(3).                                    
007000*                                                    FLLSRDEL-002         
007100*                                 LEVERERAS SOM RESDEL.                   
007200        05 TIERSDAT          PIC Z(4)9.                                   
007300*                                 ERSÄTTNINGSDATUM  (ÅÅVVD)               
007400        05 KDLEVSP           PIC Z(2)9.                                   
007500*                                 SPÄRRKOD LEVERANS                       
007600        05 MESSAGE-RAD23     PIC X(79).                                   
007700*                                 MEDDELANDEFÄLT PÅ RAD 23                
007800*** END OF VILMAII-COPY LENGTH= 503 BYTES                                 
