000100 01  ARTINFO.                                                             
000200*                                 ARTIKELINFORMATION TILL                 
000300*                                 MARKNADSBOLAG                           
000400*                                 PARTNO. INFORM.  TRANS TO               
000500*                                 MARKET COMPANY                          
000600*                                 IDPTYP =401                             
000700     03 IDPTYP               PIC X(3).                                    
000800*                                 POSTTYP                                 
000900*                                 RECORD TYPE                             
001000     03 IDVTYP               PIC X.                                       
001100*                                 POSTTYPSVERSION                         
001200*                                 RECORD TYPE VERSION                     
001300     03 IDARTNR              PIC S9(9)           COMP-3.                  
001400*                                 ARTIKELNUMMER                           
001500*                                 PART NUMBER                             
001600     03 IDFKNGRP             PIC S9(5)           COMP-3.                  
001700*                                 FUNKTIONSGRUPP                          
001800*                                 FUNCTION GROUP                          
001900     03 KDSRA                PIC S9(3)           COMP-3.                  
002000*                                 SRA-KOD                                 
002100*                                 SRA CODE                                
002200     03 KVQPACK-0            PIC S9(5)           COMP-3.                  
002300*                                 ANTAL I Q0 FÖRPACKNING                  
002400     03 KDARTURS-NUM         PIC S9(3)           COMP-3.                  
002500*                                 ARTIKELURSPRUNGSKOD NUMERISK            
002600*                                 COUNTRY OF ORIGIN NUMERIC               
002700     03 KDPRODSL             PIC S9(3)           COMP-3.                  
002800*                                 PRODUKTSLAG                             
002900*                                 PRODUCT GROUP                           
003000     03 VLARTNTO             PIC S9(8)V9(1)      COMP-3.                  
003100*                                 ARTIKELVOLYM NETTO (CM3)                
003200*                                 PART NET VOLUME    (CM3)                
003300     03 VKART                PIC S9(7)           COMP-3.                  
003400*                                 ARTIKELVIKT (G)                         
003500*                                 PART WEIGHT (G)                         
003600     03 KDVSOP               PIC S9(3)           COMP-3.                  
003700*                                 VSOP-KOD                                
003800*                                 VSOP-CODE                               
003900     03 IDSTATNR             PIC S9(9)           COMP-3.                  
004000*                                 STATISTISKT NUMMER                      
004100*                                 1 = NORSKT                              
004200*                                 2 = ENGELSKT                            
004300*                                 3 = BELGISKT                            
004400*                                 4 = PERUANSKT                           
004500*                                 5 = SVENSKT                             
004600*                                 6 =                                     
004700*                                 STATISTICAL NO.                         
004800     03 KDSORT               PIC X(2).                                    
004900*                                 SORT-KOD                                
005000*                                 UNIT OF MEASURE                         
005100     03 KDERS                PIC S9(3)           COMP-3.                  
005200*                                 ERSÄTTNINGSKOD                          
005300*                                 SUPERSESSION CODE                       
005400     03 KDBPSR               PIC S9              COMP-3.                  
005500*                                 BASLAGERFÖRSLAGSNIVÅ                    
005600*                                 BASIC PART STOCK RECOMMENDATION         
005700     03 KDBBCL               PIC 9.                                       
005800*                                 RETURNERBAR ARTIKEL                     
005900*                                 RETURNABLE PART                         
006000     03 IDLEVNR              PIC X(5).                                    
006100*                                 LEVERANTÖRNUMMER                        
006200*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
006300     03 PRARTVNA             PIC S9(7)V9(2)      COMP-3.                  
006400*                                 SJÄLVKOST ELLER BESTPRIS                
006500*                                 COST-OF-S OR SUPPLIER PRICE             
006600     03 PRARTSTD             PIC S9(7)V9(2)      COMP-3.                  
006700*                                 ARTIKELSTANDARDPRIS                     
006800*                                 STANDARD PRICE                          
006900     03 FLIART               PIC X.                                       
007000*                                 ARTIKELN INGÅR I SATS                   
007100*                                 PART IN KIT                             
007200     03 IDPROJ               PIC X(4).                                    
007300*                                 PARTS PROJEKTIDENTITET                  
007400*                                 PARTS PROJECT IDENTITY                  
007500     03 IDAO                 OCCURS 2 TIMES                               
007600                             PIC X(10).                                   
007700*                                 ÄNDRINGSORDERNUMMER                     
007800*                                 DESIGN CHANGE NOTICE                    
007900     03 TIFINLEV             PIC S9(7)           COMP-3.                  
008000*                                 PUBLICERINGSDATUM  (AAMMDD)             
008100*                                 DATE 1:ST GOODS REC (YYMMDD)            
008200     03 IDANSK               PIC S9(3)           COMP-3.                  
008300*                                 ANSKAFFARNUMMER                         
008400*                                 PROCURER NO.                            
008500     03 KVPB                 PIC S9(6)V9(1)      COMP-3.                  
008600*                                 PERIODBEHOV (PROGNOS)                   
008700*                                 PERIOD REQUIREMENTS                     
008800     03 KDVVKL               PIC S9              COMP-3.                  
008900*                                 VOLYMVÄRDESKLASS                        
009000*                                 VOLUME VALUE CLASS                      
009100     03 BELEVART             PIC X(30).                                   
009200*                                 LEVERANTÖRENS ARTIKELBENÄMNING          
009300*                                 SUPPLIER PART DESCRIPTION               
009400     03 KDTIPPR              PIC S9              COMP-3.                  
009500*                                 TIPPAT PRIS KOD                         
009600*                                 ESTIMATED PRICE CODE                    
009700     03 IDINK                PIC X(4).                                    
009800*                                 INKÖPARNUMMER                           
009900*                                 PURCHASE IDENTIFICATION NUMBER          
010000     03 TIREGDAT             PIC S9(7)           COMP-3.                  
010100*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
010200*                                 REGISTRATION DATE (YYMMDD)              
010300     03 KDUART               PIC X.                                       
010400*                                 UNDANTAGSARTIKEL                        
010500*                                 EXECPTION PARTS                         
010600     03 IDARTNR-MOTSV        PIC S9(9)           COMP-3.                  
010700*                                 MOTSVARANDE ARTIKEL                     
010800*                                 CORRESPONDING PART NO                   
010900     03 PRINK                PIC S9(7)V9(2)      COMP-3.                  
011000*                                 INKÖPSPRIS                              
011100*                                 PURCHASE PRICE                          
011200     03 IDRITN               PIC X(10).                                   
011300*                                 RITNINGSNUMMER                          
011400*                                 DRAWING NUMBER                          
011500     03 PRHANTK              PIC S9(5)V9(2)      COMP-3.                  
011600*                                 DIR LÖN  + DIR MTRL + ÖVR PÅL           
011700*                                 PRDIRLON + PRDMTRL  + PROVRPAL          
011800*                                                                         
011900*                                                                         
012000*                                 DIR LÖN AVSER KALKYLERADE OMKOS         
012100*                                 TNADER I KR PER ST AV EN AR-            
012200*                                 TIKEL FÖR FÖRPACKNINGSVERKSAMHE         
012300*                                 T I SAMBAND MED INLEVERANS.             
012400*                                 SORT: KR                                
012500*                                                                         
012600*                                 DIR MTRL AVSER KALKYLERADE KOST         
012700*                                 NADER I KR PER ST AV EN AR-             
012800*                                 TIKEL FÖR FÖRPACKNINGSMATERIAL          
012900*                                 I SAMBAND MED INLEVERANS.               
013000*                                 RANS.                                   
013100*                                 UPPDATERAS PÅ LB ENDAST VIA KT          
013200*                                 24.                                     
013300*                                 SORT: KR                                
013400*                                                                         
013500*                                 OVR PAL AVSER KALKYLERADE OMKOS         
013600*                                 TNADER I KR PER ENHET FÖR IN-           
013700*                                  DIREKTA ÖVRIGA OMKOSTNADSPÅLÄG         
013800*                                 G AVSER KALKYLERADE   KOST-             
013900*                                  NADER FÖR ÖVR DIREKTA LÖNER OC         
014000*                                 H FÖRBRUKNINGSMATERIAL I RS             
014100*                                  FÖRPACKNINGSVERKSAMHET VID INL         
014200*                                 EVERANS.                                
014300*                                 UPPDATERAS PÅ LB ENDAST VIA TT          
014400*                                 R24 PRISÄNDRING.                        
014500*                                 SORT: KR                                
014600*                                 SURCHARGE COSTS +                       
014700*                                 CALCULATED COST +                       
014800*                                 OVR PAL                                 
014900     03 KDAGE                PIC X.                                       
015000*                                 AGE-CODE                                
015100*                                 AGE-CODE                                
015200     03 KDPSLLOC             PIC 9(2).                                    
015300*                                 PRODUKTSLAG LOKALT                      
015400*                                 PRODUCT GROUP LOCAL                     
015500     03 SLAG-IDLEVNR         PIC X(5).                                    
015600*                                 LEVERANTÖRNUMMER                        
015700*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
015800     03 IDKAT                OCCURS 3 TIMES                               
015900                             PIC X(5).                                    
016000*                                 KATALOGBETECKNING                       
016100     03 KDRAB                PIC X(3).                                    
016200*                                 RABATTKOD                               
016300     03 PRARTBEL             PIC S9(7)V9(2)      COMP-3.                  
016400*                                 BESTÄLLNINGSPRIS   PRARTBEL-002         
016500*                                 I LEVERANTÖRS VALUTA                    
016600     03 FLLSRDEL             PIC X.                                       
016700*                                 LEVERERAS SOM RESDEL                    
016800     03 IDPROJUP             PIC X(8).                                    
016900*                                 PROJEKTUPPDRAG                          
017000*                                 PROJECT ASSIGNMENT                      
017100     03 FLGEMFMC             PIC X.                                       
017200*                                 GEMENSAM FORD/MPNR ARTIKEL              
017300*                                 COMMON FMC/MPNR PARTNO.                 
017400     03 TIURPROD             PIC S9(5)           COMP-3.                  
017500*                                 DATUM UTGÅTT UR PROD   (ÅÅVV)           
017600*                                 OUT OF PRODUCTION DATE (YYWW)           
017700*** END OF VILMAII-COPY LENGTH= 202 BYTES                                 
