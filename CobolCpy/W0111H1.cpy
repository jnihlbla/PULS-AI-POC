000100 01  W011100.                                                             
000200*                                 W011100 = LAGERBAND DAG 9-KANAL         
000300     03 IDARTNR              PIC S9(9)           COMP-3.                  
000400*                                 ARTIKELNUMMER                           
000500*                                 PART NUMBER                             
000600     03 KDCLAGER             PIC S9              COMP-3.                  
000700*                                 CENTRALLAGERKOD                         
000800*                                 CENTRAL WAREHOUSE CODE                  
000900     03 IDANSK               PIC S9(3)           COMP-3.                  
001000*                                 ANSKAFFARNUMMER                         
001100*                                 PROCURER NO.                            
001200     03 IDARTNR-EMBQ1        PIC S9(9)           COMP-3.                  
001300*                                 EMBALLAGEARTIKELNR FÖR Q1               
001400     03 IDFKNGRP             PIC S9(5)           COMP-3.                  
001500*                                 FUNKTIONSGRUPP                          
001600*                                 FUNCTION GROUP                          
001700     03 IDLEVNR              PIC S9(5)           COMP-3.                  
001800*                                 LEVERANTÖRNUMMER                        
001900*                                 SUPPLIER NUMBER (VENDORNUMBER)          
002000     03 IDLKTO               PIC S9(7)           COMP-3.                  
002100*                                 LAGERKONTO (FFHHHUU)                    
002200*                                 STOCK ACCOUNT (CCMMMSS)                 
002300     03 ADLAGOMR             PIC S9(3)           COMP-3.                  
002400*                                 LAGEROMRÅDE                             
002500*                                 AREA                                    
002600     03 ADPLATS              PIC S9(5)           COMP-3.                  
002700*                                 LAGERPLATSNUMMER                        
002800*                                 LOCATION                                
002900     03 BEART-ENG            PIC X(25).                                   
003000*                                 ENGELSK ARTIKELBENÄMNING                
003100     03 BEART-FRA            PIC X(25).                                   
003200*                                 FRANSK ARTIKELBENÄMNING                 
003300     03 BEART-SPA            PIC X(25).                                   
003400*                                 SPANSK ARTIKELBENÄMNING                 
003500     03 BEART-SVE            PIC X(25).                                   
003600*                                 SVENSK ARTIKELBENÄMNING                 
003700     03 BEART-TYS            PIC X(25).                                   
003800*                                 TYSK ARTIKELBENÄMNING                   
003900     03 BEFT                 PIC S9(3)           COMP-3.                  
004000*                                 FÖRPACKNINGSTYP                         
004100*                                 PACKAGING TYPE                          
004200     03 KDAGENT              PIC S9              COMP-3.                  
004300     03 KDARTHNT             PIC S9(7)           COMP-3.                  
004400*                                 HANTERINGSKOD                           
004500*                                 HANDLING CODE                           
004600     03 KDARTURS             PIC X(2).                                    
004700*                                 ARTIKELURSPRUNGSKOD                     
004800*                                 COUNTRY OF ORIGIN                       
004900     03 KDBPSR               PIC S9              COMP-3.                  
005000*                                 BASLAGERFÖRSLAGSNIVÅ                    
005100*                                 BASIC PART STOCK RECOMMENDATION         
005200     03 KDCLPOST             PIC S9              COMP-3.                  
005300*                                 CENTRALLAGERPOST                        
005400     03 KDEMBKOD-0           PIC S9(3)           COMP-3.                  
005500*                                 EMBALLAGEKOD 0                          
005600     03 KDEMBKOD-1           PIC S9(3)           COMP-3.                  
005700*                                 EMBALLAGEKOD 1                          
005800     03 KDEMBKOD-2           PIC S9(3)           COMP-3.                  
005900*                                 EMBALLAGEKOD 2                          
006000     03 KDERS                PIC S9(3)           COMP-3.                  
006100*                                 ERSÄTTNINGSKOD                          
006200*                                 SUPERSESSION CODE                       
006300     03 KDFORP               PIC S9(5)           COMP-3.                  
006400*                                 FÖRPACKNINGSKOD                         
006500*                                 PACKAGING CODE                          
006600     03 ADGANG               PIC S9(3)           COMP-3.                  
006700*                                 GÅNG                                    
006800*                                 AISLE                                   
006900     03 KDGK                 PIC S9              COMP-3.                  
007000*                                 GODSMOTTAGAREKOD                        
007100*                                 GOODS RECEIVING WAREHOUSE CODE          
007200     03 KDHF                 PIC S9              COMP-3.                  
007300*                                 HUVUDFÖRRÅDSMÄRKNING                    
007400*                                 CODE MAIN STORAGE                       
007500     03 KDIART               PIC S9              COMP-3.                  
007600*                                 INGÅR I SATS                            
007700     03 KDKG                 PIC S9              COMP-3.                  
007800*                                 KURANSGRUPP                             
007900*                                 TURNOVER CODE                           
008000     03 KDKONTR              PIC S9(5)           COMP-3.                  
008100*                                 KVAL.KONTR.KOD DATAELEMENT UTG.         
008200     03 KDLEVSP              PIC S9(3)           COMP-3.                  
008300*                                 SPÄRRKOD LEVERANS                       
008400*                                 DELIVERY BLOCKING CODE                  
008500     03 FLLSRDEL             PIC X.                                       
008600*                                 LEVERERAS SOM RESDEL                    
008700     03 KDLTK                PIC S9              COMP-3.                  
008800*                                 LAGERTILLHÖRIGHETSKOD                   
008900*                                 STOCK BELONGING CODE                    
009000     03 IDINK                PIC S9(3)           COMP-3.                  
009100*                                 INKÖPARNUMMER                           
009200*                                 PURCHASE IDENTIFICATION NUMBER          
009300     03 KDPRODSL             PIC S9(3)           COMP-3.                  
009400*                                 PRODUKTSLAG                             
009500*                                 PRODUCT GROUP                           
009600     03 KDPRTILL             PIC S9              COMP-3.                  
009700*                                 PRISTILLÄMPNINGSKOD                     
009800*                                 PRICE ADAPTION CODE                     
009900     03 KDRABATT             PIC S9(3)           COMP-3.                  
010000     03 KDUART               PIC X.                                       
010100*                                 UNDANTAGSARTIKEL                        
010200*                                 EXECPTION PARTS                         
010300     03 KDSORT               PIC X(2).                                    
010400*                                 SORT-KOD                                
010500*                                 UNIT OF MEASURE                         
010600     03 FLLTKSP              PIC X.                                       
010700*                                 SPÄRR UTLEVERANS C2-LAGER               
010800     03 KDSRA                PIC S9(3)           COMP-3.                  
010900*                                 SRA-KOD                                 
011000*                                 SRA CODE                                
011100     03 KDTIPPR              PIC S9              COMP-3.                  
011200*                                 TIPPAT PRIS KOD                         
011300*                                 ESTIMATED PRICE CODE                    
011400     03 KDVVKL               PIC S9              COMP-3.                  
011500*                                 VOLYMVÄRDESKLASS                        
011600*                                 VOLUME VALUE CLASS                      
011700     03 KVAKS                PIC S9(7)           COMP-3.                  
011800*                                 ANKOMSTSALDO                            
011900*                                 ADVICED,NOT BINNED,QTY                  
012000     03 KVAKS-E              PIC S9(7)           COMP-3.                  
012100*                                 DEL AV EFR TILL ANDRA CLAGRET           
012200*                                 PART OF EFR TO OTHER WAREHOUSE          
012300     03 KVAKS-F              PIC S9(7)           COMP-3.                  
012400*                                 DEL AV AKS TILL ANDRA CLAGRET           
012500*                                 PART OF AKS TO OTHER WAREHOUSE          
012600     03 SUTPO-TOT            PIC S9(7)           COMP-3.                  
012700*                                 TPO-KVANTITET, TOTAL                    
012800*                                 TPO-QUANTITY, TOTAL                     
012900     03 KVEFRS               PIC S9(7)           COMP-3.                  
013000*                                 EJ FAKTURERAT ANTAL STYCK               
013100*                                 ORDERED NOT INVOICED QTY                
013200     03 KVSPANT              PIC S9(7)           COMP-3.                  
013300*                                 SPÄRRAT ANTAL                           
013400*                                 BLOCKED QTY                             
013500     03 KVLS                 PIC S9(7)           COMP-3.                  
013600*                                 LAGERSALDO                              
013700*                                 STOCK BALANCE                           
013800     03 FILLER               PIC S9(7)           COMP-3.                  
013900     03 KVPB-SATS            PIC S9(6)V9(1)      COMP-3.                  
014000*                                 SATS-PERIODBEHOV                        
014100*                                 KIT PERIOD REQUIREMENTS                 
014200     03 KVPB-SEP             PIC S9(6)V9(1)      COMP-3.                  
014300*                                 SEPARAT PERIODBEHOV                     
014400*                                 SEPARATE PERIOD REQUIREMENTS            
014500     03 KVQPACK-0            PIC S9(5)           COMP-3.                  
014600*                                 ANTAL I Q0 FÖRPACKNING                  
014700     03 KVQPACK-1            PIC S9(5)           COMP-3.                  
014800*                                 ANTAL I Q1 FÖRPACKNING                  
014900*                                 QUANTITY IN BULK PACK Q1                
015000     03 KVQPACK-2            PIC S9(5)           COMP-3.                  
015100*                                 ANTAL I Q2 FÖRPACKNING                  
015200*                                 QUANTITY IN BULK PACK Q2                
015300     03 KVQPACK-3            PIC S9(5)           COMP-3.                  
015400*                                 ANTAL I Q3 FÖRPACKNING                  
015500*                                 QUANTITY IN BULK PACK Q3                
015600     03 KVQPACK-4            PIC S9(5)           COMP-3.                  
015700*                                 ANTAL I Q4 FÖRPACKNING                  
015800*                                 QUANTITY IN BULK PACK Q4                
015900     03 KVRESS               PIC S9(7)           COMP-3.                  
016000*                                 RESERVERAT ANTAL ARTIKLAR               
016100*                                 QUANTITY RESERVED ITEMS                 
016200     03 KVROS                PIC S9(7)           COMP-3.                  
016300*                                 RESTORDERSALDO                          
016400*                                 BACKORDER QTY                           
016500     03 KVSLAGER             PIC S9(7)           COMP-3.                  
016600*                                 SÄKERHETSLAGER                          
016700*                                 SAFETY STOCK                            
016800     03 PRARTBES             PIC S9(7)V9(2)      COMP-3.                  
016900*                                 BESTÄLLNINGSPRIS I KRONOR               
017000*                                 ORDER PRICE SWEDISH CURRENCY            
017100     03 PRARTBTO-SVE         PIC S9(7)V9(2)      COMP-3.                  
017200     03 PRARTBTO-EXP         PIC S9(7)V9(2)      COMP-3.                  
017300*                                 BRUTTOPRIS EXPORT (FOB-PRIS)            
017400*                                 GROSS-PRICE EXPORT                      
017500*                                  (FOB-GROSS)                            
017600     03 PRARTSJK             PIC S9(7)V9(2)      COMP-3.                  
017700*                                 ARTIKELNS SJÄLVKOSTNAD                  
017800*                                 COST OF SALES                           
017900     03 PRARTSTD             PIC S9(7)V9(2)      COMP-3.                  
018000*                                 ARTIKELSTANDARDPRIS                     
018100*                                 STANDARD PRICE                          
018200     03 KVUTRS               PIC S9(7)           COMP-3.                  
018300*                                 UTREDNINGSSALDO                         
018400*                                 INVESTIG.BALANCE                        
018500     03 PRINK                PIC S9(7)V9(2)      COMP-3.                  
018600*                                 INKÖPSPRIS                              
018700*                                 PURCHASE PRICE                          
018800     03 FILLER               PIC X(10).                                   
018900     03 REF                  PIC S9V9(2)         COMP-3.                  
019000*                                 FÖRDELNINGSFAKTOR                       
019100*                                 SPLIT FACTOR                            
019200     03 REKSIFFR             PIC S9              COMP-3.                  
019300*                                 KONTROLLSIFFRA                          
019400*                                 PART NO CHECK DIGIT                     
019500     03 REOMRTAL-DO          PIC S9(2)V9(3)      COMP-3.                  
019600*                                 OMRÄKNINGSTAL DAGORDER                  
019700*                                 PRICE CONV FACTOR                       
019800     03 REOMRTAL-MO          PIC S9(2)V9(3)      COMP-3.                  
019900     03 REPROCFP             PIC S9V9(2)         COMP-3.                  
020000*                                 PROCENT-FÖRPACKNING                     
020100*                                 PERCENTAGE TO PREPACK                   
020200     03 FILLER               PIC X(26).                                   
020300     03 TIFINLV              PIC S9(5)           COMP-3.                  
020400*                                 PUBLICERINGSVECKA, (ÅÅVVD  D=1)         
020500*                                 DATE 1:ST GOODS REC,(YYWWD D=1)         
020600     03 TIINVDAT             PIC S9(5)           COMP-3.                  
020700*                                 INVENTERINGSDATUM                       
020800*                                 STOCKTAKING DATE                        
020900     03 TIRODAT              PIC S9(5)           COMP-3.                  
021000*                                 RESTORDERDATUM      TIRODAT-002         
021100*                                 (AAVVD)                                 
021200     03 VKART                PIC S9(7)           COMP-3.                  
021300*                                 ARTIKELVIKT (G)                         
021400*                                 PART WEIGHT (G)                         
021500     03 VLARTNTO             PIC S9(8)V9(1)      COMP-3.                  
021600*                                 ARTIKELVOLYM NETTO (CM3)                
021700*                                 PART NET VOLUME    (CM3)                
021800     03 KDFARLIG             PIC S9              COMP-3.                  
021900*                                 KOD FÖR FARLIGT GODS                    
022000*                                 DANGEROUS GOODS CODE                    
022100*** END OF VILMAII-COPY LENGTH= 370 BYTES                                 
