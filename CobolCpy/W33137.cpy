000100 01  ART-W33137.                                                          
000200*                                 ARTIKELINFORMATION                      
000300     03 ART-IDPTYP           PIC X(3).                                    
000400*                                 POSTTYP                                 
000500     03 ART-IDARTNR          PIC S9(9)           COMP-3.                  
000600*                                 ARTIKELNUMMER                           
000700     03 ART-IDFKNGRP         PIC S9(5)           COMP-3.                  
000800*                                 FUNKTIONSGRUPP                          
000900     03 ART-KDSRA            PIC S9(3)           COMP-3.                  
001000*                                 SRA-KOD                                 
001100     03 ART-KVQPACK-1        PIC S9(5)           COMP-3.                  
001200*                                 ANTAL I Q1 FÖRPACKNING                  
001300     03 ART-KDARTURS         PIC X(2).                                    
001400*                                 ARTIKELURSPRUNGSKOD                     
001500     03 ART-KDPRODSL         PIC S9(3)           COMP-3.                  
001600*                                 PRODUKTSLAG                             
001700     03 ART-VLARTNTO         PIC S9(8)V9(1)      COMP-3.                  
001800*                                 ARTIKELVOLYM NETTO (CM3)                
001900     03 ART-VKART            PIC S9(7)           COMP-3.                  
002000*                                 ARTIKELVIKT (G)                         
002100     03 ART-IDSTATNR         PIC S9(9)           COMP-3.                  
002200*                                 STATISTISKT NUMMER                      
002300*                                 1 = NORSKT                              
002400*                                 2 = ENGELSKT                            
002500*                                 3 = BELGISKT                            
002600*                                 4 = PERUANSKT                           
002700*                                 5 = SVENSKT                             
002800*                                 6 =                                     
002900     03 ART-KDVSOP           PIC S9(3)           COMP-3.                  
003000*                                 VSOP-KOD                                
003100     03 ART-KDSORT           PIC X(2).                                    
003200*                                 SORT-KOD                                
003300     03 ART-KDERS            PIC S9(3)           COMP-3.                  
003400*                                 ERSÄTTNINGSKOD                          
003500     03 ART-TIERSDAT         PIC S9(5)           COMP-3.                  
003600*                                 ERSÄTTNINGSDATUM  (ÅÅVVD)               
003700     03 ART-KDBPSR           PIC S9              COMP-3.                  
003800*                                 BASLAGERFÖRSLAGSNIVÅ                    
003900     03 ART-KDBBCL           PIC 9.                                       
004000*                                 RETURNERBAR ARTIKEL                     
004100     03 ART-IDLEVNR          PIC X(5).                                    
004200*                                 LEVERANTÖRNUMMER                        
004300     03 ART-PRARTSJK         PIC S9(7)V9(2)      COMP-3.                  
004400*                                 ARTIKELNS SJÄLVKOSTNAD                  
004500     03 ART-PRARTSTD         PIC S9(7)V9(2)      COMP-3.                  
004600*                                 ARTIKELSTANDARDPRIS                     
004700     03 ART-FLIART           PIC X.                                       
004800*                                 ARTIKELN INGÅR I SATS                   
004900     03 ART-IDPROJ           PIC X(4).                                    
005000*                                 PARTS PROJEKTIDENTITET                  
005100     03 ART-IDAO             OCCURS 2 TIMES                               
005200                             PIC X(10).                                   
005300*                                 ÄNDRINGSORDERNUMMER                     
005400     03 ART-TIFINLV          PIC S9(5)           COMP-3.                  
005500*                                 PUBLICERINGSVECKA, (ÅÅVVD  D=1)         
005600     03 ART-IDANSK           PIC S9(3)           COMP-3.                  
005700*                                 ANSKAFFARNUMMER                         
005800     03 ART-KDVVKL           PIC S9              COMP-3.                  
005900*                                 VOLYMVÄRDESKLASS                        
006000     03 ART-KDTIPPR          PIC S9              COMP-3.                  
006100*                                 TIPPAT PRIS KOD                         
006200     03 ART-IDINK            PIC X(4).                                    
006300*                                 INKÖPARNUMMER                           
006400     03 ART-KDUART           PIC X.                                       
006500*                                 UNDANTAGSARTIKEL                        
006600     03 ART-IDARTNR-MOTSV    PIC S9(9)           COMP-3.                  
006700*                                 MOTSVARANDE ARTIKEL                     
006800     03 ART-PRINK            PIC S9(7)V9(2)      COMP-3.                  
006900*                                 INKÖPSPRIS                              
007000     03 ART-IDRITN           PIC X(10).                                   
007100*                                 RITNINGSNUMMER                          
007200     03 ART-PRHANTK          PIC S9(5)V9(2)      COMP-3.                  
007300*                                 DIR LÖN  + DIR MTRL + ÖVR PÅL           
007400*                                 PRDIRLON + PRDMTRL  + PROVRPAL          
007500*                                                                         
007600*                                                                         
007700*                                 DIR LÖN AVSER KALKYLERADE OMKOS         
007800*                                 TNADER I KR PER ST AV EN AR-            
007900*                                 TIKEL FÖR FÖRPACKNINGSVERKSAMHE         
008000*                                 T I SAMBAND MED INLEVERANS.             
008100*                                 SORT: KR                                
008200*                                                                         
008300*                                 DIR MTRL AVSER KALKYLERADE KOST         
008400*                                 NADER I KR PER ST AV EN AR-             
008500*                                 TIKEL FÖR FÖRPACKNINGSMATERIAL          
008600*                                 I SAMBAND MED INLEVERANS.               
008700*                                 RANS.                                   
008800*                                 UPPDATERAS PÅ LB ENDAST VIA KT          
008900*                                 24.                                     
009000*                                 SORT: KR                                
009100*                                                                         
009200*                                 OVR PAL AVSER KALKYLERADE OMKOS         
009300*                                 TNADER I KR PER ENHET FÖR IN-           
009400*                                  DIREKTA ÖVRIGA OMKOSTNADSPÅLÄG         
009500*                                 G AVSER KALKYLERADE   KOST-             
009600*                                  NADER FÖR ÖVR DIREKTA LÖNER OC         
009700*                                 H FÖRBRUKNINGSMATERIAL I RS             
009800*                                  FÖRPACKNINGSVERKSAMHET VID INL         
009900*                                 EVERANS.                                
010000*                                 UPPDATERAS PÅ LB ENDAST VIA TT          
010100*                                 R24 PRISÄNDRING.                        
010200*                                 SORT: KR                                
010300     03 ART-TEORSAK          PIC X(50).                                   
010400*                                 INFO OM SLAG AV ÅTGÄRD                  
010500     03 ART-TEARTNOT         PIC X(40).                                   
010600*                                 ARTIKEL NOTERING                        
010700     03 ART-BEART            OCCURS 3 TIMES                               
010800                             PIC X(25).                                   
010900*                                 ARTIKELBENÄMNING                        
011000     03 ART-KDAGE            PIC X.                                       
011100*                                 AGE-CODE                                
011200     03 ART-KDPSLLOC         PIC 9(2).                                    
011300*                                 PRODUKTSLAG LOKALT                      
011400     03 ART-IDKAT            OCCURS 3 TIMES                               
011500                             PIC X(5).                                    
011600*                                 KATALOGBETECKNING                       
011700     03 ART-FLLSRDEL         PIC X.                                       
011800*                                 LEVERERAS SOM RESDEL                    
011900     03 ART-IDPROJUP         PIC X(8).                                    
012000*                                 PROJEKTUPPDRAG                          
012100     03 ART-FLGEMFMC         PIC X.                                       
012200*                                 GEMENSAM FORD/MPNR ARTIKEL              
012300*** END OF VILMAII-COPY LENGTH= 314 BYTES                                 
