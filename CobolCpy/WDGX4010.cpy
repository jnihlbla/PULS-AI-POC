000100 01  4010-WDGX4010.                                                       
000200*                                 PLOCKSATSER UNDER UTSKRIFT              
000300*                                 PACKUNDERLAG                            
000400*                                 FYSISK NYCKEL: KY4010                   
000500*                                 (IDORDER,  KDPRT,   KDSS-PU,            
000600*                                  ADLAGOMR, ADGANG,  ADPLATS,            
000700*                                  IDARTNR,  IDLOPNR)                     
000800     03 4010-IDORDER         PIC S9(7)           COMP-3.                  
000900*                                 VOLVO PARTS ORDERNUMMER                 
001000*                                 VOLVO PARTS ORDER NUMBER                
001100     03 4010-KDPRT           PIC X(3).                                    
001200*                                 PRINTERKOD                              
001300*                                 PRINTERCODE                             
001400     03 4010-KDSS-PU         PIC X.                                       
001500*                                 SIDOSKIPSKOD                            
001600*                                 CODE FOR PAGESKIP                       
001700     03 4010-ADLAGOMR        PIC S9(3)           COMP-3.                  
001800*                                 LAGEROMRÅDE                             
001900*                                 AREA                                    
002000     03 4010-ADGANG          PIC S9(3)           COMP-3.                  
002100*                                 GÅNG                                    
002200*                                 AISLE                                   
002300     03 4010-ADPLATS         PIC S9(5)           COMP-3.                  
002400*                                 LAGERPLATSNUMMER                        
002500*                                 LOCATION                                
002600     03 4010-IDARTNR         PIC S9(9)           COMP-3.                  
002700*                                 ARTIKELNUMMER                           
002800*                                 PART NUMBER                             
002900     03 4010-IDLOPNR         PIC S9(3)           COMP-3.                  
003000*                                 LÖPNUMMER                               
003100*                                 SEQUENCE NUMBER                         
003200     03 4010-ADLAGOMR-ORD    PIC S9(3)           COMP-3.                  
003300*                                 LAGEROMRÅDE                             
003400*                                 AREA                                    
003500     03 4010-ADPLATS-ORD     PIC S9(5)           COMP-3.                  
003600*                                 LAGERPLATSNUMMER                        
003700*                                 LOCATION                                
003800     03 4010-BEART           PIC X(25).                                   
003900*                                 ARTIKELBENÄMNING                        
004000*                                 PART DESCRIPTION                        
004100     03 4010-FLAKPLOC        PIC X.                                       
004200*                                 ORDERRADEN SKA PLOCKAS PÅ AK            
004300*                                 ORDER LINE FROM "AK" QUEUE              
004400     03 4010-FLSDCLEV        PIC X.                                       
004500*                                 LEVERANSSTYRNING SDC                    
004600*                                 DELIVERY ONLY SDC                       
004700     03 4010-IDBORD          PIC X(3).                                    
004800*                                 PACK-BORD                               
004900*                                 PACKING TABLE                           
005000     03 4010-IDKUNDRF        PIC X(10).                                   
005100*                                 KUNDENS REFERENS (ORDERID)              
005200*                                 CUSTOMER REFERENCE (ORDER ID)           
005300     03 4010-IDKUNDRF-RO     PIC X(10).                                   
005400*                                 KUND REF PÅ RO                          
005500*                                 CUST REF RO                             
005600     03 4010-IDLOPNR-ORD     PIC S9(3)           COMP-3.                  
005700*                                 ORDERNS ORDNINGSNUMMER INOM             
005800*                                 EN PLOCKSATS                            
005900*                                 SEQUENCE-NUMBER FOR AN ORDER            
006000*                                 WITHIN A PICKING UNIT                   
006100     03 4010-IDLOPNR-PL      PIC S9(3)           COMP-3.                  
006200*                                 PLOCKSATSENS LÖPNUMMER INOM             
006300*                                 PRC-GRUPP                               
006400*                                 SEQUENCE-NUMBER FOR THE                 
006500*                                 PICKING UNIT WITHIN PRC-GROUP           
006600     03 4010-IDPRC.                                                       
006700*                                 PRODUKTIONSKANAL                        
006800*                                 PRODUCTION CHANNEL                      
006900        05 4010-IDPRCBAS     PIC X(3).                                    
007000*                                 PRC-BAS                                 
007100*                                 PRC-BASIC                               
007200        05 4010-IDPRCVAR     PIC X.                                       
007300*                                 PRC-VARIANT                             
007400*                                 PRC-VARIANT                             
007500     03 4010-IDPRODNR        PIC S9(7)           COMP-3.                  
007600*                                 PRODUKTIONSNUMMER                       
007700*                                 PRODUCTION NUMBER                       
007800     03 4010-IDPURAD         PIC S9(5)           COMP-3.                  
007900*                                 RADNUMMER PÅ PACKUNDERLAG               
008000*                                 LINENO IN PACKINGDOCUMENT               
008100     03 4010-IDSPECEMB       PIC 9(4).                                    
008200*                                 SPECIALEMBALLAGEID                      
008300*                                 SPECIAL PACKING ID                      
008400     03 4010-IDUSER          PIC X(8).                                    
008500*                                 ANVÄNDARENS SÄKERHETS ID                
008600*                                 USER SECURITY-IDENTITY                  
008700     03 4010-KDARTURS        PIC X(2).                                    
008800*                                 ARTIKELURSPRUNGSKOD                     
008900*                                 COUNTRY OF ORIGIN                       
009000     03 4010-IDDC            PIC X(2).                                    
009100*                                 IDENTIFIERARE LAGER                     
009200*                                 WAREHOUSE IDENTIFIER                    
009300     03 4010-KDFDKRAV        PIC S9(3)           COMP-3.                  
009400*                                 TRANSPORTFÖRPACKNINGSKOD                
009500*                                 PACKING CODE                            
009600     03 4010-KVAVBART        PIC S9(7)           COMP-3.                  
009700*                                 AVBOKAT ANTAL ARTIKLAR                  
009800*                                 ALLOCATED QUANTITY                      
009900     03 4010-KVBEART-Q       PIC S9(7)           COMP-3.                  
010000*                                 BESTÄLLT KVANTANPASSAT ANTAL            
010100*                                 ORDERED QUANTITY ADAPTED                
010200*                                  ITEMS                                  
010300     03 4010-KVHANTTI        PIC S9(7)           COMP-3.                  
010400*                                 HANTERINGSKOD TID                       
010500*                                 PIECEWORK TIME LINES                    
010600     03 4010-REKSIFFR        PIC S9              COMP-3.                  
010700*                                 KONTROLLSIFFRA                          
010800*                                 PART NO CHECK DIGIT                     
010900     03 4010-TILST           PIC S9(11)          COMP-3.                  
011000*                                 SENASTE STARTTIDPUNKT                   
011100*                                 LATEST START-TIME                       
011200     03 4010-TIREGDAT        PIC S9(7)           COMP-3.                  
011300*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
011400*                                 REGISTRATION DATE (YYMMDD)              
011500     03 4010-TIREGTID        PIC S9(7)           COMP-3.                  
011600*                                 REGISTRERINGSTID                        
011700*                                 GENERAL REGISTRATION TIME               
011800     03 4010-TIRFS           PIC S9(11)          COMP-3.                  
011900*                                 KLART FÖR TRANSPORT ÅÅMMDDTTMM          
012000*                                 READY FOR SHIPMENT  YYMMDDHHMM          
012100     03 4010-TIUTSKR         PIC S9(7)           COMP-3.                  
012200*                                 UTSKRIFTDATUM  (ÅÅMMDD)                 
012300*                                 PRINTING DATE  (YYMMDD)                 
012400     03 4010-TIUTSTID        PIC S9(7)           COMP-3.                  
012500*                                 UTSKRIFTSTID (TTMMSS)                   
012600*                                 TIME OF PRINTING (HHMMSS)               
012700     03 4010-VKORDNTO        PIC S9(6)V9(1)      COMP-3.                  
012800*                                 ORDERVIKT NETTO (KG)                    
012900*                                 WEIGHT PER ORDER NETTO (KG)             
013000     03 4010-VLORDNTO        PIC S9(4)V9(3)      COMP-3.                  
013100*                                 ORDERVOLYM NETTO (M3)                   
013200*                                 NET VOLUME PER ORDER (M3)               
013300     03 4010-ORDER.                                                       
013400        05 4010-BERADREF     PIC X(10).                                   
013500*                                 KUNDENS RADREFERENS                     
013600*                                 CUSTOMERS ITEM REF.                     
013700        05 4010-BEVOLREF     PIC X(10).                                   
013800*                                 VOLVO REFERENS                          
013900*                                 VOLVO REFERENCE                         
014000        05 4010-FLCOD        PIC X.                                       
014100*                                 KONTANTBETALANDE KUND                   
014200*                                 CASH ON DELIVERY CUSTOMER               
014300        05 4010-FLINVEST     PIC X.                                       
014400*                                 BYTES INVENTERINGSFLAGGA                
014500*                                 EXCHANGE INVESTMENT FLAG                
014600        05 4010-FLPRTILL     PIC X.                                       
014700*                                 PRISTILLÄGGS FLAGGA                     
014800*                                 PRICE PENALTY FLAG                      
014900        05 4010-FLRESTN      PIC X.                                       
015000*                                 RESTNOTERING ?                          
015100*                                 BACKORDERED ?                           
015200        05 4010-FLTILLK      PIC X.                                       
015300*                                 TILLKOMMANDE ARTIKEL ?                  
015400*                                 REPLACEMENT PART FLAG                   
015500        05 4010-IDDC-RO      PIC X(2).                                    
015600*                                 LAGER DÄR RESTORDER FÅR SKE             
015700*                                 WAREHOUSE FOR BACKORDERS                
015800        05 4010-IDKAMPRF     PIC S9(7)           COMP-3.                  
015900*                                 KAMPANJREFERENS                         
016000*                                 CAMPAIGN REFERENCE                      
016100        05 4010-IDKONTO      PIC S9(11)          COMP-3.                  
016200*                                 KONTO                                   
016300*                                 ACCOUNT                                 
016400        05 4010-IDKST        PIC X(10).                                   
016500*                                 KOSTNADSSTÄLLE                          
016600*                                 COST CENTRE                             
016700        05 4010-IDLEVNR      PIC X(5).                                    
016800*                                 LEVERANTÖRNUMMER                        
016900*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
017000        05 4010-IDLOPNR-RO   PIC S9(3)           COMP-3.                  
017100*                                 LÖPNUMMER RO/TPO                        
017200*                                 SEQUENCE NUMBER BO/TPO                  
017300        05 4010-IDPLKLST     PIC S9(3)           COMP-3.                  
017400*                                 PLOCKLISTNUMMER                         
017500*                                 PICKING LIST NUMBER                     
017600        05 4010-IDPSN        PIC 9(3).                                    
017700*                                 PROPER SHIPPING NAME                    
017800*                                 PROPER SHIPPING NAME                    
017900        05 4010-IDSYSTEM     PIC X(4).                                    
018000*                                 VOLVO VCCS SYSTEMNUMMER                 
018100*                                 VOLVO VCCS SYSTEM NUMBER                
018200        05 4010-IDZON        PIC X(2).                                    
018300*                                 TRANSPORTVÄG (RUTT,ZON)                 
018400*                                 TRANSPORT ROUTE (ZONE)                  
018500        05 4010-KDDSP        PIC S9              COMP-3.                  
018600*                                 PÅVERKAN PÅ DSP                         
018700*                                 AFFECT ON DSP                           
018800        05 4010-KDFARLIG     PIC S9              COMP-3.                  
018900*                                 KOD FÖR FARLIGT GODS                    
019000*                                 DANGEROUS GOODS CODE                    
019100        05 4010-KDKVBRYT     PIC S9              COMP-3.                  
019200*                                 KOD OM KVANTFÖRP SKALL BRYTAS           
019300*                                 BREAK BULKPACK CODE                     
019400        05 4010-KDOI         PIC X(2).                                    
019500*                                 ORDERINGÅNGSTYP                         
019600*                                 TYPE OF INCOMING ORDER                  
019700        05 4010-KDORDING     PIC S9              COMP-3.                  
019800*                                 UPPDATERING ORDERINGÅNG                 
019900*                                 ORDER STATISTICS                        
020000        05 4010-KDORDKL      PIC S9              COMP-3.                  
020100*                                 ORDERKLASS                              
020200*                                 ORDER CLASS                             
020300        05 4010-KDPRODSL     PIC S9(3)           COMP-3.                  
020400*                                 PRODUKTSLAG                             
020500*                                 PRODUCT GROUP                           
020600        05 4010-KDPRTYP      PIC X.                                       
020700*                                 TYP AV PRISTILLÄMPNING                  
020800*                                 TYPE OF PRICING                         
020900        05 4010-KDVRINFO     PIC S9              COMP-3.                  
021000*                                 PÅVERKAN I VR/DSP SYSTEM                
021100*                                 VR/DSP UP-DATE                          
021200        05 4010-KVSLATT      PIC S9(7)           COMP-3.                  
021300*                                 BERÄKNAD SLATTGRÄNS                     
021400*                                                                         
021500        05 4010-PRARTNTO     PIC S9(7)V9(2)      COMP-3.                  
021600*                                 ARTIKELPRIS NETTO                       
021700*                                 NET PRICE EACH   (FOB NET)              
021800        05 4010-SUEQFG       PIC S9(3)V9(4)      COMP-3.                  
021900*                                 EQ-VÄRDE FARLIGT GODS                   
022000*                                 EQ VALUE DANGEROUS GODS                 
022100        05 4010-TIPRIS       PIC S9(7)           COMP-3.                  
022200*                                 PRISTILLÄMPNINGSDATUM  (ÅÅMMDD)         
022300*                                 PRICE ADAPTION DATE    (YYMMDD)         
022400        05 4010-TIRODAT      PIC S9(7)           COMP-3.                  
022500*                                 RESTORDERDATUM         (ÅÅMMDD)         
022600*                                 BACK ORDER DATE        (YYMMDD)         
022700        05 4010-VKART        PIC S9(7)           COMP-3.                  
022800*                                 ARTIKELVIKT (G)                         
022900*                                 PART WEIGHT (G)                         
023000        05 4010-VKART-NTO    PIC S9(9)           COMP-3.                  
023100*                                 ARTIKELNS NETTOVIKT                     
023200*                                 PART NET WEIGHT                         
023300        05 4010-VKART-FG     PIC S9(7)           COMP-3.                  
023400*                                 NETTOVIKT EXPLOSIVA ÄMNEN               
023500*                                 NET WEIGHT EXPLOSIVES                   
023600        05 4010-VLARTNTO     PIC S9(8)V9(1)      COMP-3.                  
023700*                                 ARTIKELVOLYM (CM3)                      
023800*                                 PART VOLUME    (CM3)                    
023900        05 4010-VLFG         PIC S9(4)V9(3)      COMP-3.                  
024000*                                 VOLYM FARLIGT GODS                      
024100*                                 VOLUME DANGEROUS GOODS                  
024200        05 4010-IDBIL.                                                    
024300*                                 BILIDENTITET                            
024400*                                 CAR IDENTITY                            
024500           07 4010-IDBILTYP  PIC X(3).                                    
024600*                                 BILTYP                                  
024700*                                 CAR TYPE                                
024800           07 4010-TIAAAA    PIC X(4).                                    
024900*                                 ÅRTAL (ÅÅÅÅ)                            
025000*                                 YEAR  (YYYY)                            
025100           07 4010-IDCHASSI-PIE                                           
025200                             PIC X(6).                                    
025300*                                 CHASSINUMMER PIE                        
025400*                                 CHASSI NUMBER PIE                       
025500        05 4010-IDANALYS     PIC X(12).                                   
025600*                                 ANALYSNUMMER                            
025700*                                 ANALYSIS NUMBER                         
025800        05 4010-IDARBREF     PIC X(10).                                   
025900*                                 ARBETSORDER SOFT/DÖSKALLE               
026000*                                 WORK ORDER SOFT/DUMMY ORDERHEAD         
026100        05 4010-IDKLIENT     PIC X(10).                                   
026200*                                 VADIS KLIENT                            
026300*                                 VADIS CLIENT                            
026400        05 4010-IDVIN        PIC X(17).                                   
026500*                                 VIN ID FORDON                           
026600*                                 VEHICLE VIN ID                          
026700        05 4010-KDORDTYP-LDC PIC X(2).                                    
026800*                                 ORDERTYP HOS DEALER                     
026900        05 4010-IDKUNDRF-WIP PIC X(10).                                   
027000*                                 REPARATIONS ORDERNR, LDC KUND           
027100*                                 WORK ORDER NUMBER, LDC DEALER           
027200        05 4010-TIREPDAT     PIC S9(7)           COMP-3.                  
027300*                                 REPAIR DATE                             
027400*                                 REPAIR DATE                             
027500     03 4010-DEAL-PR-LINE.                                                
027600*                                 DEALERPRIS (RAD)                        
027700        05 4010-IDPRQUES     PIC 9(7).                                    
027800*                                 PRISFRÅGA NR                            
027900*                                 PRICE QUESTION NO                       
028000        05 4010-PRARTNTO-LOC PIC S9(7)V9(2)      COMP-3.                  
028100*                                 ARTIKELPRIS NETTO LOKAL VALUTA          
028200*                                 NET PRICE EACH LOCAL CURRENCY           
028300        05 4010-PRARTNTO-LOCPREL                                          
028400                             PIC S9(7)V9(2)      COMP-3.                  
028500*                                 PREL NETTO SLUTKUNDSPRIS I              
028600*                                 LOKAL VALUTA                            
028700*                                 PREL NET PRICE - LOCAL CURRENCY         
028800        05 4010-PRARTBTO-LOC PIC S9(7)V9(2)      COMP-3.                  
028900*                                 PRIS I LOKAL VALUTA                     
029000*                                 LOCAL GROSS SALES PRICE                 
029100        05 4010-KDVALISO     PIC X(3).                                    
029200*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
029300*                                 CURRENCY CODE BY ISO-STANDARD.          
029400        05 4010-KDVAT        PIC X(2).                                    
029500*                                 MOMSKOD                                 
029600*                                 VAT CODE                                
029700        05 4010-RERAB        PIC S9(2)V9(1)      COMP-3.                  
029800*                                 RABATTSATS (PROCENT)                    
029900        05 4010-KDRAB        PIC X(5).                                    
030000*                                 RABATTKOD                               
030100        05 4010-BEART-VIPS   PIC X(25).                                   
030200*                                 VIPS ARTIKELBENÄMNING                   
030300*                                 PÅ DEALERNS SPRÅK                       
030400     03 4010-CLEARGROUP.                                                  
030500*                                 CLEARINGAREA FÖR ORDERINGÅNG            
030600        05 4010-CLEARAREA    OCCURS 7 TIMES.                              
030700*                                 CLEARINGAREA FÖR ORDERINGÅNG            
030800           07 4010-IDDC-CLEAR                                             
030900                             PIC X(2).                                    
031000*                                 LAGERPRIORITERING VID                   
031100*                                 ORDERCLEARING                           
031200*                                 WAREHOUSE PRIORITY FOR ORDER            
031300*                                 CLEARING                                
031400           07 4010-FLLF      PIC X.                                       
031500*                                 ARTIKEL LAGERFÖRES                      
031600*                                 PART IN STOCK                           
031700           07 4010-FLCLEAR   PIC X.                                       
031800*                                 ORDERRAD CLEAR FLAGGA                   
031900*                                 CLEARING FLAG FOR ORDER LINE            
032000     03 4010-PRAVCOST        PIC S9(7)V9(2)      COMP-3.                  
032100*                                 MEDELVÄRDESKOSTNAD I UTL.VALUTA         
032200*                                 AVERAGE COST FOREIGN CURRENCY           
032300     03 4010-KVQPACK-3       PIC S9(5)           COMP-3.                  
032400*                                 ANTAL I Q3 FÖRPACKNING                  
032500*                                 QUANTITY IN BULK PACK Q3                
032600*** END OF VILMAII-COPY LENGTH= 451 BYTES                                 
