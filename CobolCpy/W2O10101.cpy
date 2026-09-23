000100 01  W2O10101.                                                            
000200*                                 COPYTEXT FÖR MOD W2010100               
000300     03 IDTRANS              PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 TEMFSFEL             PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 IDARTNR-IN           PIC X(9).                                    
000800*                                 ARTIKELNUMMER                           
000900     03 BLADDRING-ANT        PIC X(3).                                    
001000*                                                                         
001100     03 IDARTNR-UT           PIC X(9).                                    
001200*                                 ARTIKELNUMMER                           
001300     03 STRECK               PIC X.                                       
001400     03 REKSIFFR             PIC 9.                                       
001500*                                 KONTROLLSIFFRA                          
001600     03 BEART-SVE            PIC X(25).                                   
001700*                                 ARTIKELBENÄMNING                        
001800     03 AREA.                                                             
001900        05 IDANSK            PIC Z(2)9.                                   
002000*                                 ANSKAFFARNUMMER                         
002100        05 IDLEVNR-SHIP      PIC X(5).                                    
002200*                                 SKEPPANDE LEVERANTÖR                    
002300        05 IDFKNGRP          PIC Z(3)9.                                   
002400*                                 FUNKTIONSGRUPP                          
002500        05 IDPROJ            PIC X(4).                                    
002600*                                 PARTS PROJEKTIDENTITET                  
002700        05 IDBERED           PIC Z9.                                      
002800*                                 BEREDARENUMMER                          
002900        05 KDSORT            PIC X(2).                                    
003000*                                 SORT-KOD                                
003100        05 IDPLANGR-LEV      PIC 9.                                       
003200*                                 PLANERINGSGRUPP HOS LEVERANTÖR          
003300        05 IDPLANGR-AG       PIC 9.                                       
003400*                                 PLANERINGSGRUPP ANSKAFFARE              
003500        05 KDAVT             PIC 9.                                       
003600*                                 AVTALSMÄRKNING                          
003700        05 KVAP              PIC Z(6)9.                                   
003800*                                 ANNULLATIONSPUNKT                       
003900        05 IDINK             PIC X(4).                                    
004000*                                 INKÖPARNUMMER                           
004100        05 TIFINLV           PIC 9(5).                                    
004200*                                 PUBLICERINGSVECKA, (ÅÅVVD  D=1)         
004300        05 KDVVKL            PIC 9.                                       
004400*                                 VOLYMVÄRDESKLASS                        
004500        05 KVAL              PIC 9.                                       
004600*                                 ANTAL LEVERANTÖRER                      
004700        05 KVEOQ             PIC Z(6)9.                                   
004800*                                 BER. OPTIMAL HEMTAGNINGSKVANTIT         
004900*                                 ET                                      
005000        05 TIURPROD          PIC 9(4).                                    
005100*                                 DATUM UTGÅTT UR PROD   (ÅÅVV)           
005200        05 KVAARLF           PIC Z9.                                      
005300*                                 ANTAL ÅR LAGERFÖRING EFTER EOP          
005400        05 KDERS-UTG         PIC Z9.                                      
005500*                                 ERSÄTTNINGSKOD UTGÅNGEN ARTIKEL         
005600        05 KDSRA             PIC Z9.                                      
005700*                                 SRA-KOD                                 
005800        05 KVQ               PIC Z(6)9.                                   
005900*                                 EKONOMISK HEMTAGNINGSKVANTITET          
006000        05 FLMANQ            PIC X.                                       
006100*                                 MANUELL HEMTAGNINGSKVANTITET            
006200        05 KVVECKOR-LT       PIC Z9.                                      
006300*                                 ANTAL VECKOR LEDTID                     
006400        05 FLMANLT           PIC X.                                       
006500*                                 MANUELLT SATT LEDTID ?                  
006600        05 KDHF              PIC 9.                                       
006700*                                 HUVUDFÖRRÅDSMÄRKNING                    
006800        05 KDARTURS          PIC X(2).                                    
006900*                                 ARTIKELURSPRUNGSKOD                     
007000        05 KVQ-JUST          PIC Z(6)9.                                   
007100*                                 NY EKON HEMTAGNINGSKVANTITET            
007200        05 TIQJUST           PIC 9(4).                                    
007300*                                 DATUM NY HEMTAGN KVANT (ÅÅVV)           
007400        05 KVVECKOR-FT       PIC Z9.                                      
007500*                                 ANTAL VECKOR FRYSNINGSTID               
007600        05 FLAVRART          PIC X.                                       
007700*                                 AVROPSARTIKEL                           
007800        05 FLLTKSP           PIC X.                                       
007900*                                 SPÄRR UTLEVERANS C2-LAGER               
008000        05 KVKP              PIC Z(6)9.                                   
008100*                                 KÖPPUNKT                                
008200        05 FLMANKP           PIC X.                                       
008300*                                 MANUELL FRAMTAGEN KÖPPUNKT ?            
008400        05 KVVECKOR-AT       PIC Z9.                                      
008500*                                 ANTAL VECKOR ANSKAFFNINGSTID            
008600        05 FLMANAT           PIC X.                                       
008700*                                 MANUELLT SATT ANSKAFFNINGSTID ?         
008800        05 FLLSRDEL          PIC X.                                       
008900*                                 LEVERERAS SOM RESDEL                    
009000        05 KDUART            PIC X.                                       
009100*                                 UNDANTAGSARTIKEL                        
009200        05 KDKSP             PIC 9.                                       
009300*                                 KÖPSPÄRR                                
009400        05 KVVECKOR-BT       PIC Z9.                                      
009500*                                 ANTAL VECKOR BESTÄLLNINGSTID            
009600        05 FLFSP             OCCURS 2 TIMES                               
009700                             PIC X.                                       
009800*                                 FÖRDELNINGSSPÄRR                        
009900        05 KVBK              PIC Z(6)9.                                   
010000*                                 EKONOMISK BESTÄLLNINGSKVANTITET         
010100        05 FLMANBK           PIC X.                                       
010200*                                 MANUELL BESTÄLLNINGSKVANTITET ?         
010300        05 KVDAGAR-INLEV     PIC Z9.                                      
010400*                                 INLEVERANSTID     (ANTAL DAGAR)         
010500        05 KDLEVSP-C1        PIC X.                                       
010600        05 KDLEVSP-C2        PIC X.                                       
010700        05 KVPALL            PIC Z(6)9.                                   
010800*                                 ANTAL I PALL                            
010900        05 FLJIT             PIC X.                                       
011000*                                 JUST-IN-TIME FLAGGA                     
011100        05 KVDAGAR-TT        PIC Z9.                                      
011200*                                 DAGAR TULL- OCH TRANSPORT-TID           
011300        05 KDFARLIG          PIC 9.                                       
011400*                                 KOD FÖR FARLIGT GODS                    
011500        05 KVULOAD           PIC Z(6)9.                                   
011600*                                 MIN ENHETSLAST FRÅN LEVERANTÖR          
011700        05 FLCDART           PIC X.                                       
011800*                                 CROSS-DOCKING PART                      
011900        05 KVOVERF           PIC Z(6)9.                                   
012000*                                 ÖVERFÖRINGSSALDO                        
012100        05 IDKAT-1           PIC X(5).                                    
012200*                                 KATALOGBETECKNING                       
012300        05 IDKAT-2           PIC X(5).                                    
012400*                                 KATALOGBETECKNING                       
012500        05 IDKAT-3           PIC X(5).                                    
012600*                                 KATALOGBETECKNING                       
012700        05 IDPROENH          OCCURS 3 TIMES                               
012800                             INDEXED IY                                   
012900                             PIC X(8).                                    
013000*                                 PRODUKTIONSENHET                        
013100        05 IDKAT             OCCURS 11 TIMES                              
013200                             INDEXED IZ                                   
013300                             PIC X(5).                                    
013400*                                 KATALOGBETECKNING                       
013500        05 IDAO              OCCURS 5 TIMES                               
013600                             INDEXED IX                                   
013700                             PIC X(10).                                   
013800*                                 ÄNDRINGSORDERNUMMER                     
013900        05 VARNOT            PIC X(40).                                   
014000*                                 ARTIKEL NOTERING                        
014100        05 TEARTNOT          OCCURS 2 TIMES                               
014200                             PIC X(40).                                   
014300*                                 ARTIKEL NOTERING                        
014400        05 BEUPPDSU          PIC X(35).                                   
014500*                                 SU-UPPDRAG BENÄMNING                    
014600     03 TEMFSINF             PIC X(55).                                   
014700*                                 INFORMATIONSMEDDELANDE                  
014800*** END OF VILMAII-COPY LENGTH= 589 BYTES                                 
