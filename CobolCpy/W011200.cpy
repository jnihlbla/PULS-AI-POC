000100 01  W011200.                                                             
000200*                                 W011200  = ART.REG. VECKA               
000300*                                 ANVÄNDS AV PERIODSYSTEM OCH EZT         
000400     03 IDARTNR              PIC S9(9)           COMP-3.                  
000500*                                 ARTIKELNUMMER                           
000600*                                 PART NUMBER                             
000700     03 BEART-ENG            PIC X(25).                                   
000800*                                 ENGELSK ARTIKELBENÄMNING                
000900     03 BEART-SVE            PIC X(25).                                   
001000*                                 SVENSK ARTIKELBENÄMNING                 
001100     03 BEFT                 PIC S9(3)           COMP-3.                  
001200*                                 FÖRPACKNINGSTYP                         
001300*                                 PACKAGING TYPE                          
001400     03 FLAVRART             PIC X.                                       
001500*                                 AVROPSARTIKEL                           
001600     03 FLFSP                PIC X.                                       
001700*                                 FÖRDELNINGSSPÄRR                        
001800*                                 BLOCKED FOR SPLIT                       
001900     03 FLIART               PIC X.                                       
002000*                                 ARTIKELN INGÅR I SATS                   
002100*                                 PART IN KIT                             
002200     03 FLINFART             PIC S9              COMP-3.                  
002300*                                 INFORMATION ARTIKELMÄRKNING             
002400     03 FLLSRDEL             PIC X.                                       
002500*                                 LEVERERAS SOM RESDEL                    
002600     03 FLMANAT              PIC X.                                       
002700*                                 MANUELLT SATT ANSKAFFNINGSTID ?         
002800     03 FLMANBK              PIC X.                                       
002900*                                 MANUELL BESTÄLLNINGSKVANTITET ?         
003000     03 FLMANKP              PIC X.                                       
003100*                                 MANUELL FRAMTAGEN KÖPPUNKT ?            
003200     03 FLMANLT              PIC X.                                       
003300*                                 MANUELLT SATT LEDTID ?                  
003400     03 FLMANQ               PIC X.                                       
003500*                                 MANUELL HEMTAGNINGSKVANTITET            
003600     03 FLMPB-C1             PIC X.                                       
003700*                                 MASKINELLT SATT PROGNOS C1              
003800     03 FLMPB-C2             PIC X.                                       
003900*                                 MASKINELLT SATT PROGNOS C2              
004000     03 FLPRRAPP             PIC S9              COMP-3.                  
004100     03 FLSPECPR             PIC S9              COMP-3.                  
004200*                                 SPECIALPRISFLAGGA                       
004300*                                 SPECIAL PRICE FLAG                      
004400     03 FLTOPP               PIC X.                                       
004500*                                 TOPP-200-ARTIKEL                        
004600*                                 TOP 200 PART                            
004700     03 IDAETNR              PIC S9(3)           COMP-3.                  
004800*                                 ÄNDRINGSTILLFÄLLENUMMER                 
004900*                                 AMENDMENT OCCASION NUMBER               
005000     03 IDANSK               PIC S9(3)           COMP-3.                  
005100*                                 ANSKAFFARNUMMER                         
005200*                                 PROCURER NO.                            
005300     03 IDARTNR-EMBQ0        PIC S9(9)           COMP-3.                  
005400*                                 EMBALLAGEARTIKELNR FÖR Q0               
005500     03 IDARTNR-EMBQ1        PIC S9(9)           COMP-3.                  
005600*                                 EMBALLAGEARTIKELNR FÖR Q1               
005700     03 IDARTNR-EMBQ2        PIC S9(9)           COMP-3.                  
005800*                                 EMBALLAGEARTIKELNR FÖR Q2               
005900     03 IDARTNR-EMBQ3        PIC S9(9)           COMP-3.                  
006000*                                 EMBALLAGEARTIKELNR FÖR Q3               
006100     03 IDARTNR-EMBQ4        PIC S9(9)           COMP-3.                  
006200*                                 EMBALLAGEARTIKELNR FÖR Q4               
006300     03 IDBERED              PIC S9(3)           COMP-3.                  
006400*                                 BEREDARENUMMER                          
006500     03 IDFKNGRP             PIC S9(5)           COMP-3.                  
006600*                                 FUNKTIONSGRUPP                          
006700*                                 FUNCTION GROUP                          
006800     03 IDINK                PIC X(4).                                    
006900*                                 INKÖPARNUMMER                           
007000*                                 PURCHASE IDENTIFICATION NUMBER          
007100     03 IDLEVNR              PIC X(5).                                    
007200*                                 LEVERANTÖRNUMMER                        
007300*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
007400     03 IDLKTO               PIC S9(7)           COMP-3.                  
007500*                                 LAGERKONTO (FFHHHUU)                    
007600*                                 STOCK ACCOUNT (CCMMMSS)                 
007700     03 IDPLANGR-AG          PIC S9              COMP-3.                  
007800*                                 PLANERINGSGRUPP ANSKAFFARE              
007900     03 IDPLANGR-LEV         PIC S9              COMP-3.                  
008000*                                 PLANERINGSGRUPP HOS LEVERANTÖR          
008100     03 IDPROD               PIC S9(3)           COMP-3.                  
008200*                                 PRODUKTKOD, DEL AV PRODUKTSLAG          
008300*                                 PRODUCT CODE PART OF PROD.GRP.          
008400     03 KDAGENT              PIC S9              COMP-3.                  
008500     03 KDARTHNT             PIC S9(7)           COMP-3.                  
008600*                                 HANTERINGSKOD                           
008700*                                 HANDLING CODE                           
008800     03 KDARTURS             PIC X(2).                                    
008900*                                 ARTIKELURSPRUNGSKOD                     
009000*                                 COUNTRY OF ORIGIN                       
009100     03 KDAVT                PIC S9              COMP-3.                  
009200*                                 AVTALSMÄRKNING                          
009300*                                 AGREEMENT CODE                          
009400     03 KDBPSR               PIC S9              COMP-3.                  
009500*                                 BASLAGERFÖRSLAGSNIVÅ                    
009600*                                 BASIC PART STOCK RECOMMENDATION         
009700     03 KDEMBKOD-0           PIC S9(3)           COMP-3.                  
009800*                                 EMBALLAGEKOD 0                          
009900     03 KDEMBKOD-1           PIC S9(3)           COMP-3.                  
010000*                                 EMBALLAGEKOD 1                          
010100     03 KDEMBKOD-2           PIC S9(3)           COMP-3.                  
010200*                                 EMBALLAGEKOD 2                          
010300     03 KDERS                PIC S9(3)           COMP-3.                  
010400*                                 ERSÄTTNINGSKOD                          
010500*                                 SUPERSESSION CODE                       
010600     03 KDFARLIG             PIC S9              COMP-3.                  
010700*                                 KOD FÖR FARLIGT GODS                    
010800*                                 DANGEROUS GOODS CODE                    
010900     03 KDFORP               PIC S9(5)           COMP-3.                  
011000*                                 FÖRPACKNINGSKOD                         
011100*                                 PACKAGING CODE                          
011200     03 KDGK                 PIC S9              COMP-3.                  
011300*                                 GODSMOTTAGAREKOD                        
011400*                                 GOODS RECEIVING WAREHOUSE CODE          
011500     03 KDHF                 PIC S9              COMP-3.                  
011600*                                 HUVUDFÖRRÅDSMÄRKNING                    
011700*                                 CODE MAIN STORAGE                       
011800     03 KDKG                 PIC S9              COMP-3.                  
011900*                                 KURANSGRUPP                             
012000*                                 TURNOVER CODE                           
012100     03 KDKONTR              PIC S9(5)           COMP-3.                  
012200*                                 KVAL.KONTR.KOD DATAELEMENT UTG.         
012300     03 KDKSP                PIC S9              COMP-3.                  
012400*                                 KÖPSPÄRR                                
012500*                                 PURCHASE BLOCKING CODE                  
012600     03 KDLEVSP              PIC S9(3)           COMP-3.                  
012700*                                 SPÄRRKOD LEVERANS                       
012800*                                 DELIVERY BLOCKING CODE                  
012900     03 KDLPSP               PIC S9              COMP-3.                  
013000*                                 LEVERANSPLANESPÄRR                      
013100     03 KDLTK                PIC S9              COMP-3.                  
013200*                                 LAGERTILLHÖRIGHETSKOD                   
013300*                                 STOCK BELONGING CODE                    
013400     03 KDPRIO               PIC S9              COMP-3.                  
013500*                                 PRIORITETSKOD                           
013600*                                 PRIORITY CODE                           
013700     03 KDPRODSL             PIC S9(3)           COMP-3.                  
013800*                                 PRODUKTSLAG                             
013900*                                 PRODUCT GROUP                           
014000     03 KDPRTILL             PIC S9              COMP-3.                  
014100*                                 PRISTILLÄMPNINGSKOD                     
014200*                                 PRICE ADAPTION CODE                     
014300     03 KDRABATT             PIC S9(3)           COMP-3.                  
014400     03 KDSORT               PIC X(2).                                    
014500*                                 SORT-KOD                                
014600*                                 UNIT OF MEASURE                         
014700     03 KDSRA                PIC S9(3)           COMP-3.                  
014800*                                 SRA-KOD                                 
014900*                                 SRA CODE                                
015000     03 KDTIPPR              PIC S9              COMP-3.                  
015100*                                 TIPPAT PRIS KOD                         
015200*                                 ESTIMATED PRICE CODE                    
015300     03 KDUART               PIC X.                                       
015400*                                 UNDANTAGSARTIKEL                        
015500*                                 EXECPTION PARTS                         
015600     03 KDVOLART             PIC S9              COMP-3.                  
015700     03 KDVSOP               PIC S9(3)           COMP-3.                  
015800*                                 VSOP-KOD                                
015900*                                 VSOP-CODE                               
016000     03 KDVTH                PIC S9              COMP-3.                  
016100*                                 KOD FÖR OMKOSTNADSBÄRANDE AVD.          
016200*                                 CODE FOR COST RESPONSIBILITY            
016300     03 KDVVKL               PIC S9              COMP-3.                  
016400*                                 VOLYMVÄRDESKLASS                        
016500*                                 VOLUME VALUE CLASS                      
016600     03 KVAKS                PIC S9(7)           COMP-3.                  
016700*                                 ANKOMSTSALDO                            
016800*                                 ADVICED,NOT BINNED,QTY                  
016900     03 KVAKS-E              PIC S9(7)           COMP-3.                  
017000*                                 DEL AV EFR TILL ANDRA CLAGRET           
017100*                                 PART OF EFR TO OTHER WAREHOUSE          
017200     03 KVAL                 PIC S9              COMP-3.                  
017300*                                 ANTAL LEVERANTÖRER                      
017400*                                 NUMBER OF SUPPLIERS                     
017500     03 KVBK                 PIC S9(7)           COMP-3.                  
017600*                                 EKONOMISK BESTÄLLNINGSKVANTITET         
017700     03 KVBR-TOT             PIC S9(7)           COMP-3.                  
017800*                                 TOT BEST REST                           
017900     03 SUTPO-TOT            PIC S9(7)           COMP-3.                  
018000*                                 TPO-KVANTITET, TOTAL                    
018100*                                 TPO-QUANTITY, TOTAL                     
018200     03 KVEFRS               PIC S9(7)           COMP-3.                  
018300*                                 EJ FAKTURERAT ANTAL STYCK               
018400*                                 ORDERED NOT INVOICED QTY                
018500     03 KVKP                 PIC S9(7)           COMP-3.                  
018600*                                 KÖPPUNKT                                
018700     03 KVLAAN               PIC S9(7)           COMP-3.                  
018800*                                 LÅNESALDO                               
018900     03 KVLS                 PIC S9(7)           COMP-3.                  
019000*                                 LAGERSALDO                              
019100*                                 STOCK BALANCE                           
019200     03 KVMP                 PIC S9(7)           COMP-3.                  
019300*                                 MAXPUNKT                                
019400*                                 MAXIMUM POINT                           
019500     03 KVOVERF              PIC S9(7)           COMP-3.                  
019600*                                 ÖVERFÖRINGSSALDO                        
019700     03 KVPALL               PIC S9(7)           COMP-3.                  
019800*                                 ANTAL I PALL                            
019900*                                 QUANTITY IN PALLET                      
020000     03 KVPB-SATS            PIC S9(6)V9(1)      COMP-3.                  
020100*                                 SATS-PERIODBEHOV                        
020200*                                 KIT PERIOD REQUIREMENTS                 
020300     03 KVPB-TOT             PIC S9(6)V9(1)      COMP-3.                  
020400*                                 TOTALT PERIODBEHOV                      
020500     03 KVQ                  PIC S9(7)           COMP-3.                  
020600*                                 EKONOMISK HEMTAGNINGSKVANTITET          
020700     03 KVQPACK-0            PIC S9(5)           COMP-3.                  
020800*                                 ANTAL I Q0 FÖRPACKNING                  
020900     03 KVQPACK-1            PIC S9(5)           COMP-3.                  
021000*                                 ANTAL I Q1 FÖRPACKNING                  
021100*                                 QUANTITY IN BULK PACK Q1                
021200     03 KVQPACK-2            PIC S9(5)           COMP-3.                  
021300*                                 ANTAL I Q2 FÖRPACKNING                  
021400*                                 QUANTITY IN BULK PACK Q2                
021500     03 KVQPACK-3            PIC S9(5)           COMP-3.                  
021600*                                 ANTAL I Q3 FÖRPACKNING                  
021700*                                 QUANTITY IN BULK PACK Q3                
021800     03 KVQPACK-4            PIC S9(5)           COMP-3.                  
021900*                                 ANTAL I Q4 FÖRPACKNING                  
022000*                                 QUANTITY IN BULK PACK Q4                
022100     03 KVRESS               PIC S9(7)           COMP-3.                  
022200*                                 RESERVERAT ANTAL ARTIKLAR               
022300*                                 QUANTITY RESERVED ITEMS                 
022400     03 KVROS                PIC S9(7)           COMP-3.                  
022500*                                 RESTORDERSALDO                          
022600*                                 BACKORDER QTY                           
022700     03 KVSLAGER             PIC S9(7)           COMP-3.                  
022800*                                 SÄKERHETSLAGER                          
022900*                                 SAFETY STOCK                            
023000     03 KVSLUTKP             PIC S9(7)           COMP-3.                  
023100*                                 SLUTKÖPSSALDO                           
023200     03 KVVECKOR-AT          PIC S9(3)           COMP-3.                  
023300*                                 ANTAL VECKOR ANSKAFFNINGSTID            
023400     03 KVVECKOR-BT          PIC S9(3)           COMP-3.                  
023500*                                 ANTAL VECKOR BESTÄLLNINGSTID            
023600     03 KVVECKOR-FT          PIC S9(3)           COMP-3.                  
023700*                                 ANTAL VECKOR FRYSNINGSTID               
023800     03 KVVECKOR-LT          PIC S9(3)           COMP-3.                  
023900*                                 ANTAL VECKOR LEDTID                     
024000     03 PRARTBES             PIC S9(7)V9(2)      COMP-3.                  
024100*                                 BESTÄLLNINGSPRIS I KRONOR               
024200*                                 ORDER PRICE SWEDISH CURRENCY            
024300     03 PRARTBTO-SVE         PIC S9(7)V9(2)      COMP-3.                  
024400     03 PRARTBTO-UTL         PIC S9(7)V9(2)      COMP-3.                  
024500*                                 EXPORTPRIS                              
024600     03 PRARTSJK             PIC S9(7)V9(2)      COMP-3.                  
024700*                                 ARTIKELNS SJÄLVKOSTNAD                  
024800*                                 COST OF SALES                           
024900     03 PRARTSTD             PIC S9(7)V9(2)      COMP-3.                  
025000*                                 ARTIKELSTANDARDPRIS                     
025100*                                 STANDARD PRICE                          
025200     03 PRINK                PIC S9(7)V9(2)      COMP-3.                  
025300*                                 INKÖPSPRIS                              
025400*                                 PURCHASE PRICE                          
025500     03 REF                  PIC S9V9(2)         COMP-3.                  
025600*                                 FÖRDELNINGSFAKTOR                       
025700*                                 SPLIT FACTOR                            
025800     03 REKSIFFR             PIC S9              COMP-3.                  
025900*                                 KONTROLLSIFFRA                          
026000*                                 PART NO CHECK DIGIT                     
026100     03 REOMRTAL-DO          PIC S9(2)V9(3)      COMP-3.                  
026200*                                 OMRÄKNINGSTAL DAGORDER                  
026300*                                 PRICE CONV FACTOR                       
026400     03 REOMRTAL-MO          PIC S9(2)V9(3)      COMP-3.                  
026500     03 REPROCFP             PIC S9V9(2)         COMP-3.                  
026600*                                 PROCENT-FÖRPACKNING                     
026700*                                 PERCENTAGE TO PREPACK                   
026800     03 RESLJUST-C1          PIC S9(2)V9(1)      COMP-3.                  
026900*                                 SÄKERHETSLAGER JUST C1                  
027000*                                 SAFETY STOCK ADJUST C1                  
027100     03 RESLJUST-C2          PIC S9(2)V9(1)      COMP-3.                  
027200*                                 SÄKERHETSLAGER JUST C2                  
027300*                                 SAFETY STOCK ADJUST C2                  
027400     03 TIAAVVD-REG          PIC S9(5)           COMP-3.                  
027500*                                 REGISTRERINGSDATUM                      
027600     03 TIERSDAT             PIC S9(5)           COMP-3.                  
027700*                                 ERSÄTTNINGSDATUM  (ÅÅVVD)               
027800*                                 DATE OF SUPERSESSION (YYWWD)            
027900     03 TIFINLV              PIC S9(5)           COMP-3.                  
028000*                                 PUBLICERINGSVECKA, (ÅÅVVD  D=1)         
028100*                                 DATE 1:ST GOODS REC,(YYWWD D=1)         
028200     03 TILPSP               PIC S9(5)           COMP-3.                  
028300*                                 DATUM LEVERANSPLAN-SPÄRR (ÅÅVV)         
028400     03 TISLJUST-C1          PIC S9(5)           COMP-3.                  
028500*                                 DAT SÄK-LAG-JUST-FAKT C1                
028600     03 TISLJUST-C2          PIC S9(5)           COMP-3.                  
028700*                                 DAT SÄK-LAG-JUST-FAKT C2                
028800     03 TIURPROD             PIC S9(5)           COMP-3.                  
028900*                                 DATUM UTGÅTT UR PROD   (ÅÅVV)           
029000     03 VKART                PIC S9(7)           COMP-3.                  
029100*                                 ARTIKELVIKT (G)                         
029200*                                 PART WEIGHT (G)                         
029300     03 VLARTNTO             PIC S9(8)V9(1)      COMP-3.                  
029400*                                 ARTIKELVOLYM NETTO (CM3)                
029500*                                 PART NET VOLUME    (CM3)                
029600     03 PRARTBTO-N           PIC S9(7)V9(2)      COMP-3.                  
029700     03 PRARTBTO-SVE-N       PIC S9(7)V9(2)      COMP-3.                  
029800     03 IDPROJ               PIC X(4).                                    
029900*                                 PARTS PROJEKTIDENTITET                  
030000*                                 PARTS PROJECT IDENTITY                  
030100     03 IDPROJUP             PIC X(8).                                    
030200*                                 PROJEKTUPPDRAG                          
030300*                                 PROJECT ASSIGNMENT                      
030400     03 KDYTBEH              PIC S9(3)           COMP-3.                  
030500*                                 YTBEHANDLINGSKOD                        
030600*                                                                         
030700     03 PRARTBES-PR          PIC S9(7)V9(2)      COMP-3.                  
030800*                                 DETTA BESTÄLLNINGSPRIS (KR)             
030900     03 PRARTBEL-PR          PIC S9(8)V9(3)      COMP-3.                  
031000*                                 DETTA BESTÄLLNINGSPRIS                  
031100*                                 (I LEVERANTÖRENS VALUTA)                
031200*** END OF VILMAII-COPY LENGTH= 382 BYTES                                 
