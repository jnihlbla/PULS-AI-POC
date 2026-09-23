000100 01  W3O10101.                                                            
000200     03 TRANS-NUMMER.                                                     
000300        05 TRANS-SIFF-1      PIC X.                                       
000400        05 TRANS-SIFF-2      PIC X.                                       
000500        05 TRANS-SIFF-3      PIC X.                                       
000600        05 TRANS-SIFF-4      PIC X.                                       
000700     03 MESSAGE              PIC X(36).                                   
000800*                                 MEDDELANDE         TEMFSFEL-002         
000900     03 IDARTNR-IN           PIC X(9).                                    
001000*                                 ARTIKELNUMMER                           
001100     03 IDARTNR-UT           PIC X(9).                                    
001200*                                 ARTIKELNUMMER                           
001300     03 BINDESTRECK          PIC X.                                       
001400     03 REKSIFFR             PIC 9.                                       
001500*                                 KONTROLLSIFFRA                          
001600     03 AREA.                                                             
001700        05 KDSORT            PIC X(2).                                    
001800*                                 SORT-KOD                                
001900        05 IDFKNGRP          PIC Z(4)9.                                   
002000*                                 FUNKTIONSGRUPP                          
002100        05 KDPRODSL          PIC Z(2)9.                                   
002200*                                 PRODUKTSLAG                             
002300        05 KDPRTILL          PIC 9.                                       
002400*                                 PRISTILLÄMPNINGSKOD                     
002500        05 BEART-ENG         PIC X(25).                                   
002600*                                 ENGELSK ARTIKELBENÄMNING                
002700        05 BEART-SVE         PIC X(25).                                   
002800*                                 SVENSK ARTIKELBENÄMNING                 
002900        05 TIFINLV           PIC Z(4)9.                                   
003000*                                 PUBLICERINGSVECKA, (ÅÅVVD  D=1)         
003100        05 VKART             PIC -(7)9.                                   
003200*                                 ARTIKELVIKT (G)                         
003300        05 FLSPECPR          PIC 9.                                       
003400*                                 SPECIALPRISFLAGGA                       
003500        05 KDVVKL            PIC 9.                                       
003600*                                 VOLYMVÄRDESKLASS                        
003700        05 IDANSK            PIC Z(2)9.                                   
003800*                                 ANSKAFFARNUMMER                         
003900        05 KDPSLLOC          PIC 9(2).                                    
004000*                                 PRODUKTSLAG LOKALT                      
004100        05 IDLEVNR           PIC X(5).                                    
004200*                                 LEVERANTÖRNUMMER                        
004300        05 KDLTK             PIC 9.                                       
004400*                                 LAGERTILLHÖRIGHETSKOD                   
004500        05 KDIART            PIC X.                                       
004600*                                 INGÅR I SATS                            
004700        05 KDHF              PIC 9.                                       
004800*                                 HUVUDFÖRRÅDSMÄRKNING                    
004900        05 KDSRA             PIC Z(2)9.                                   
005000*                                 SRA-KOD                                 
005100        05 KDERS             PIC Z(2)9.                                   
005200*                                 ERSÄTTNINGSKOD                          
005300        05 PRARTSJK          PIC -(7)9.9(2).                              
005400*                                 ARTIKELNS SJÄLVKOSTNAD                  
005500        05 KDTIPPR           PIC 9.                                       
005600*                                 TIPPAT PRIS KOD                         
005700        05 PRARTBES          PIC -(7)9.9(2).                              
005800*                                 BESTÄLLNINGSPRIS I KRONOR               
005900        05 PRARTSTD          PIC -(7)9.9(2).                              
006000*                                 ARTIKELSTANDARDPRIS                     
006100        05 PRDIRLON          PIC -(4)9.9(3).                              
006200*                                 DIREKT LÖN                              
006300        05 PRDMTRL           PIC -(6)9.9(3).                              
006400*                                 DIREKT MATERIAL                         
006500        05 PROVRPAL          PIC -(4)9.9(3).                              
006600*                                 ÖVRIGA OMKOSTNADER PÅLÄGG               
006700        05 FILLER            PIC X.                                       
006800        05 PRHEMTAG          PIC Z(6)9.9(2).                              
006900*                                 HEMTAGNINGSKOSTNAD                      
007000        05 KDVTH             PIC 9.                                       
007100*                                 KOD FÖR OMKOSTNADSBÄRANDE AVD.          
007200        05 KVPB-SEP          PIC Z(6)9.9.                                 
007300*                                 SEPARAT PERIODBEHOV                     
007400        05 TIERSDAT          PIC Z(4)9.                                   
007500*                                 ERSÄTTNINGSDATUM  (ÅÅVVD)               
007600        05 KDSPARR           PIC X.                                       
007700*                                 SPÄRRKOD                                
007800        05 KVQPACK-1         PIC -(5)9.                                   
007900*                                 ANTAL I Q1 FÖRPACKNING                  
008000        05 IDAO1             PIC X(10).                                   
008100*                                 ÄNDRINGSORDERNUMMER                     
008200        05 FILLER            PIC X.                                       
008300        05 IDAO2             PIC X(10).                                   
008400*                                 ÄNDRINGSORDERNUMMER                     
008500        05 TIAVIDAT-SEN      PIC -(7)9.                                   
008600*                                 AVISERINGSDATUM (YYMMDD)                
008700*** END OF VILMAII-COPY LENGTH= 280 BYTES                                 
