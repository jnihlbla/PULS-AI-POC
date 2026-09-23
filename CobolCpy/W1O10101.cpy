000100 01  W1O10101.                                                            
000200*                                 COPYTEXT FÖR MOD W1O10101               
000300     03 IDTRANS              PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 TEMFSFEL             PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 IDARTNR-IN           PIC X(9).                                    
000800*                                 ARTIKELNUMMER                           
000900     03 IDARTNR-UT           PIC X(9).                                    
001000*                                 ARTIKELNUMMER                           
001100     03 STRECK               PIC X.                                       
001200     03 REKSIFFR             PIC 9.                                       
001300*                                 KONTROLLSIFFRA                          
001400     03 BLADDRING-ANT        PIC X(3).                                    
001500*                                                                         
001600     03 AREA.                                                             
001700        05 IDANSK            PIC Z(2)9.                                   
001800*                                 ANSKAFFARNUMMER                         
001900        05 IDLEVNR           PIC X(5).                                    
002000*                                 LEVERANTÖRNUMMER                        
002100        05 KDPRODSL          PIC Z9.                                      
002200*                                 PRODUKTSLAG                             
002300        05 KDUART            PIC X.                                       
002400*                                 UNDANTAGSARTIKEL                        
002500        05 KDPSLLOC          PIC 9(2).                                    
002600*                                 PRODUKTSLAG LOKALT                      
002700        05 KDIART            PIC X.                                       
002800*                                 INGÅR I SATS                            
002900        05 IDSKYLT-TILLV     PIC X(3).                                    
003000*                                 TILLVERKNINGSLAND                       
003100        05 FLGEMFMC          PIC X.                                       
003200*                                 GEMENSAM FORD/MPNR ARTIKEL              
003300        05 PRARTSTD          PIC Z(6)9.9(2).                              
003400*                                 ARTIKELSTANDARDPRIS                     
003500        05 IDPROJ            PIC X(4).                                    
003600*                                 PARTS PROJEKTIDENTITET                  
003700        05 IDPROJUP          PIC X(8).                                    
003800*                                 PROJEKTUPPDRAG                          
003900        05 IDRITN            PIC X(10).                                   
004000*                                 RITNINGSNUMMER                          
004100        05 ERS-KOD.                                                       
004200           07 KDERS-ATTR     PIC X(2).                                    
004300*                                 MFS ATTRIBUTFÄLT                        
004400           07 KDERS          PIC Z9(2).                                   
004500*                                 ERSÄTTNINGSKOD                          
004600        05 KVDISP            PIC -(6)9.                                   
004700*                                 DISPONIBELT LAGER                       
004800        05 IDFKNGRP          PIC Z(3)9.                                   
004900*                                 FUNKTIONSGRUPP                          
005000        05 KDSORT            PIC X(2).                                    
005100*                                 SORT-KOD                                
005200        05 KDBPSR            PIC 9.                                       
005300*                                 BASLAGERFÖRSLAGSNIVÅ                    
005400        05 IDBERED           PIC Z(2)9.                                   
005500*                                 BEREDARENUMMER                          
005600        05 FLGEMART          PIC X.                                       
005700*                                 FLAGGA GEMENSAM ARTIKEL                 
005800        05 FLLSRDEL          PIC X.                                       
005900*                                 LEVERERAS SOM RESDEL                    
006000        05 BEART-SVE         PIC X(25).                                   
006100*                                 SVENSK ARTIKELBENÄMNING                 
006200        05 BEART-ENG         PIC X(25).                                   
006300*                                 ENGELSK ARTIKELBENÄMNING                
006400        05 BEUPPDSU          PIC X(35).                                   
006500*                                 SU-UPPDRAG BENÄMNING                    
006600        05 TIREGDAT          PIC 9(6).                                    
006700*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
006800        05 TISOP             PIC 9(5).                                    
006900*                                 PRODUKTIONSSTART, (ÅÅVVD  D=1)          
007000        05 TIERSDAT          PIC 9(5).                                    
007100*                                 ERSÄTTNINGSDATUM  (ÅÅVVD)               
007200        05 IDAO              OCCURS 5 TIMES                               
007300                             INDEXED IX                                   
007400                             PIC X(10).                                   
007500*                                 ÄNDRINGSORDERNUMMER                     
007600        05 IDKAT-1           PIC X(5).                                    
007700*                                 KATALOGBETECKNING                       
007800        05 IDKAT-2           PIC X(5).                                    
007900*                                 KATALOGBETECKNING                       
008000        05 IDKAT-3           PIC X(5).                                    
008100*                                 KATALOGBETECKNING                       
008200        05 KAT-BEEMBLEM      OCCURS 14 TIMES                              
008300                             INDEXED IZ                                   
008400                             PIC X(5).                                    
008500*                                 KATALOGBETECKNING                       
008600        05 IDPROENH          OCCURS 3 TIMES                               
008700                             INDEXED IY                                   
008800                             PIC X(8).                                    
008900*                                 PRODUKTIONSENHET                        
009000        05 TEARTNOT-3-ATTR   PIC X(2).                                    
009100*                                 MFS ATTRIBUTFÄLT                        
009200        05 TEARTNOT-3        PIC X(40).                                   
009300*                                 ARTIKEL NOTERING                        
009400        05 TEARTNOT-7-ATTR   PIC X(2).                                    
009500*                                 MFS ATTRIBUTFÄLT                        
009600        05 TEARTNOT-7        PIC X(40).                                   
009700*                                 ARTIKEL NOTERING                        
009800     03 TEMFSINF             PIC X(72).                                   
009900*                                 INFORMATIONSMEDDELANDE                  
010000*** END OF VILMAII-COPY LENGTH= 557 BYTES                                 
