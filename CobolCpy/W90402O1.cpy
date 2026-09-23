000100 01  W90402O1.                                                            
000200*                                 COPYTEXT FÖR MOD W90402O1               
000300     03 IDTRANS              PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 TEMFSFEL             PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 FILLER               PIC X(9).                                    
000800     03 IDARTNR-UT           PIC X(9).                                    
000900*                                 ARTIKELNUMMER                           
001000     03 FILLER               PIC X.                                       
001100     03 FILLER               PIC 9.                                       
001200     03 BLADDRING-ANT        PIC X(3).                                    
001300*                                                                         
001400     03 AREA.                                                             
001500        05 IDANSK            PIC Z(2)9.                                   
001600*                                 ANSKAFFARNUMMER                         
001700        05 IDLEVNR           PIC X(5).                                    
001800*                                 LEVERANTÖRNUMMER                        
001900        05 KDPRODSL          PIC Z9.                                      
002000*                                 PRODUKTSLAG                             
002100        05 KDUART            PIC X.                                       
002200*                                 UNDANTAGSARTIKEL                        
002300        05 KDPSLLOC          PIC 9(2).                                    
002400*                                 PRODUKTSLAG LOKALT                      
002500        05 KDIART            PIC X.                                       
002600*                                 INGÅR I SATS                            
002700        05 FILLER            PIC X(3).                                    
002800        05 FLGEMFMC          PIC X.                                       
002900*                                 GEMENSAM FORD/MPNR ARTIKEL              
003000        05 PRARTSTD          PIC Z(6)9.9(2).                              
003100*                                 ARTIKELSTANDARDPRIS                     
003200        05 FILLER            PIC X(4).                                    
003300        05 IDPROJUP          PIC X(8).                                    
003400*                                 PROJEKTUPPDRAG                          
003500        05 IDRITN            PIC X(10).                                   
003600*                                 RITNINGSNUMMER                          
003700        05 ERS-KOD           OCCURS 2 TIMES.                              
003800           07 FILLER         PIC X(2).                                    
003900           07 KDERS          PIC Z9(2).                                   
004000*                                 ERSÄTTNINGSKOD                          
004100        05 KVDISP            OCCURS 2 TIMES                               
004200                             PIC -(6)9.                                   
004300*                                 DISPONIBELT LAGER                       
004400        05 KVPB-TOT          OCCURS 2 TIMES                               
004500                             PIC Z(5)9.9.                                 
004600*                                 TOTALT PERIODBEHOV                      
004700        05 IDFKNGRP          PIC Z(3)9.                                   
004800*                                 FUNKTIONSGRUPP                          
004900        05 KDSORT            PIC X(2).                                    
005000*                                 SORT-KOD                                
005100        05 KDBPSR            PIC 9.                                       
005200*                                 BASLAGERFÖRSLAGSNIVÅ                    
005300        05 IDBERED           PIC Z(2)9.                                   
005400*                                 BEREDARENUMMER                          
005500        05 FLGEMART          PIC X.                                       
005600*                                 FLAGGA GEMENSAM ARTIKEL                 
005700        05 FLLSRDEL          PIC X.                                       
005800*                                 LEVERERAS SOM RESDEL                    
005900        05 BEART-SVE         PIC X(25).                                   
006000*                                 SVENSK ARTIKELBENÄMNING                 
006100        05 BEART-ENG         PIC X(25).                                   
006200*                                 ENGELSK ARTIKELBENÄMNING                
006300        05 TIREGDAT          PIC 9(6).                                    
006400*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
006500        05 TISOP             PIC 9(5).                                    
006600*                                 PRODUKTIONSSTART, (ÅÅVVD  D=1)          
006700        05 TIERSDAT          PIC 9(5).                                    
006800*                                 ERSÄTTNINGSDATUM  (ÅÅVVD)               
006900        05 IDAO              OCCURS 5 TIMES                               
007000                             INDEXED IX                                   
007100                             PIC X(10).                                   
007200*                                 ÄNDRINGSORDERNUMMER                     
007300        05 IDKAT-1           PIC X(5).                                    
007400*                                 KATALOGBETECKNING                       
007500        05 IDKAT-2           PIC X(5).                                    
007600*                                 KATALOGBETECKNING                       
007700        05 IDKAT-3           PIC X(5).                                    
007800*                                 KATALOGBETECKNING                       
007900        05 KAT-BEEMBLEM      OCCURS 14 TIMES                              
008000                             INDEXED IZ                                   
008100                             PIC X(5).                                    
008200*                                 KATALOGBETECKNING                       
008300        05 FILLER            OCCURS 3 TIMES                               
008400                             INDEXED IY                                   
008500                             PIC X(8).                                    
008600        05 TEARTNOT-3-ATTR   PIC X(2).                                    
008700*                                 MFS ATTRIBUTFÄLT                        
008800        05 TEARTNOT-3        PIC X(40).                                   
008900*                                 ARTIKEL NOTERING                        
009000        05 TEARTNOT-7-ATTR   PIC X(2).                                    
009100*                                 MFS ATTRIBUTFÄLT                        
009200        05 TEARTNOT-7        PIC X(40).                                   
009300*                                 ARTIKEL NOTERING                        
009400     03 TEMFSINF             PIC X(72).                                   
009500*                                 INFORMATIONSMEDDELANDE                  
009600*** END OF VILMAII-COPY LENGTH= 550 BYTES                                 
