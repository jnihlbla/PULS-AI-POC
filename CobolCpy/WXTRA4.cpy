000100 01  WXTRA4.                                                              
000200*                                 INFORMATION, FAKTURERADE RADER          
000300*                                 INFORMATION, INVOICED LINES             
000400     03 IDDISTR              PIC 9(4).                                    
000500*                                 DISTRIKTNUMMER                          
000600*                                 DISTRICT NUMBER                         
000700     03 IDKUNDNR             PIC 9(6).                                    
000800*                                 KUNDNUMMER                              
000900*                                 CUSTOMER NO                             
001000     03 IDPRODNR             PIC 9(7).                                    
001100*                                 PRODUKTIONSNUMMER                       
001200*                                 PRODUCTION NUMBER                       
001300     03 IDORDER              PIC 9(7).                                    
001400*                                 VOLVO PARTS ORDERNUMMER                 
001500*                                 VOLVO PARTS ORDER NUMBER                
001600     03 IDDC                 PIC X(2).                                    
001700*                                 IDENTIFIERARE LAGER                     
001800*                                 WAREHOUSE IDENTIFIER                    
001900     03 IDARTNR              PIC 9(8).                                    
002000*                                 ARTIKELNUMMER                           
002100*                                 PART NUMBER                             
002200     03 ADLAGOMR-DLB         PIC 9(2).                                    
002300*                                 LAGEROMRÅDE                             
002400*                                 AREA                                    
002500     03 ADLAGOMR-VERKLIGT    PIC 9(2).                                    
002600*                                 LAGEROMRÅDE                             
002700*                                 AREA                                    
002800     03 DASUPREF             PIC 9(8).                                    
002900*                                 SÄNDNINGSDATUM DIREKTLEVERANTÖR         
003000*                                 SHIPPING DATE DIRECT SUPPLIER           
003100     03 DATRPAVD             PIC 9(6).                                    
003200*                                 AVSÄNDNINGSDATUM                        
003300*                                 DATE WHEN A TRANSPORT WAS REPOR         
003400*                                 TED AS LOADED                           
003500     03 TIHHMM               PIC 9(4).                                    
003600*                                 KLOCKSLAG (TIMMAR OCH MINUTER)          
003700*                                 TIME IN HOUR AND MINUTE                 
003800     03 FLDIRLEV             PIC X.                                       
003900*                                 DIREKTLEVERANS ?                        
004000*                                 DIRECT DELIVERY ?                       
004100     03 FLLDCKND             PIC X.                                       
004200*                                 FL LDC-KUND                             
004300*                                 FL LDC CUSTOMER                         
004400     03 IDFAKT               PIC 9(7).                                    
004500*                                 FAKTURANUMMER                           
004600*                                 INVOICE NO.                             
004700     03 IDFKNGRP             PIC 9(4).                                    
004800*                                 FUNKTIONSGRUPP                          
004900*                                 FUNCTION GROUP                          
005000     03 IDKOLLI              PIC 9(5).                                    
005100*                                 KOLLINUMMER                             
005200*                                 CASE NUMBER                             
005300     03 IDLBBET              PIC X(12).                                   
005400*                                 LASTBÄRARBETECKNING                     
005500*                                 TRAILER NUMBER                          
005600     03 IDLEVNR              PIC X(5).                                    
005700*                                 LEVERANTÖRNUMMER                        
005800*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
005900     03 IDORDNR5             PIC 9(5).                                    
006000*                                 ORDERNUMMER                             
006100*                                 ORDER NUMBER                            
006200     03 IDPLKLST             PIC 9(3).                                    
006300*                                 PLOCKLISTNUMMER                         
006400*                                 PICKING LIST NUMBER                     
006500     03 IDPLOCK              PIC 9(6).                                    
006600*                                 PLOCKARE                                
006700*                                 PICKER                                  
006800     03 IDPRC.                                                            
006900*                                 PRODUKTIONSKANAL                        
007000*                                 PRODUCTION CHANNEL                      
007100        05 IDPRCBAS          PIC X(3).                                    
007200*                                 PRC-BAS                                 
007300*                                 PRC-BASIC                               
007400        05 IDPRCVAR          PIC X.                                       
007500*                                 PRC-VARIANT                             
007600*                                 PRC-VARIANT                             
007700     03 IDPSN                PIC 9(3).                                    
007800*                                 PROPER SHIPPING NAME                    
007900*                                 PROPER SHIPPING NAME                    
008000     03 IDPSN-DC             PIC 9(3).                                    
008100*                                 PROPER SHIPPING NAME PER XDC            
008200*                                 PROPER SHIPPING NAME XDC                
008300     03 IDRONR               PIC X(10).                                   
008400*                                 KUND REF PÅ RO                          
008500*                                 CUST REF RO                             
008600     03 IDSHIPM              PIC 9(7).                                    
008700*                                 SKEPPNINGSNUMMER                        
008800*                                 SHIPMENT NO                             
008900     03 IDSKIFT              PIC X.                                       
009000*                                 SHIFT IDENTITET                         
009100*                                 SHIFT IDENTITY                          
009200     03 IDUSER-OREG          PIC X(8).                                    
009300*                                 ANVÄNDARENS SÄKERHETS ID                
009400*                                 USER SECURITY-IDENTITY                  
009500     03 IDSYSTEM             PIC X(4).                                    
009600*                                 VOLVO VCCS SYSTEMNUMMER                 
009700*                                 VOLVO VCCS SYSTEM NUMBER                
009800     03 IDTRP.                                                            
009900*                                 TRANSPORTIDENTITET                      
010000*                                 TRANSPORTIDENTITY                       
010100        05 IDTRPLOS          PIC X(3).                                    
010200*                                 TRANSPORTLÖSNING                        
010300*                                 TRANSPORTSOLUTION                       
010400        05 IDTRPVAR          PIC X(2).                                    
010500*                                 TRANSPORTLÖSNINGSGRUPP                  
010600*                                 TRANSPORTSOLUTIONGROUP                  
010700     03 IDUSER-PACK          PIC X(8).                                    
010800*                                 ANVÄNDARENS SÄKERHETS ID                
010900*                                 USER SECURITY-IDENTITY                  
011000     03 KDARTURS             PIC X(2).                                    
011100*                                 ARTIKELURSPRUNGSKOD                     
011200*                                 COUNTRY OF ORIGIN                       
011300     03 KDFAKTYP             PIC X.                                       
011400*                                 FAKTURATYP                              
011500*                                 INVOICE TYPE                            
011600     03 KDFARLIG             PIC 9.                                       
011700*                                 KOD FÖR FARLIGT GODS                    
011800*                                 DANGEROUS GOODS CODE                    
011900     03 KDFRAKT              PIC 9(2).                                    
012000*                                 FRAKTSÄTT DC TILL KUND                  
012100*                                 FREIGHT CODE                            
012200     03 KDORDKL              PIC 9.                                       
012300*                                 ORDERKLASS                              
012400*                                 ORDER CLASS                             
012500     03 KDPRODSL             PIC 9(2).                                    
012600*                                 PRODUKTSLAG                             
012700*                                 PRODUCT GROUP                           
012800     03 KDKOLLI              PIC X(8).                                    
012900*                                 KOLLIKOD                                
013000*                                 KOLLI CODE                              
013100     03 KDPRCGRP             PIC X(5).                                    
013200*                                 PRODUKTIONSKANALSGRUPP                  
013300*                                 GROUP OF PRODUCTION CHANNELS            
013400     03 KDSORT               PIC X(2).                                    
013500*                                 SORT-KOD                                
013600*                                 UNIT OF MEASURE                         
013700     03 KDVIA                PIC X(2).                                    
013800*                                 KOD FöR LEVERANS VIA                    
013900*                                 CODE FOR DELIVERY VIA                   
014000     03 KVBEART              PIC 9(6).                                    
014100*                                 BESTÄLLT ANTAL STYCKEN                  
014200*                                 ORDERED QUANTITY                        
014300     03 KVLEVART             PIC 9(7).                                    
014400*                                 LEVERERAT ANTAL STYCK                   
014500*                                 DELIVERED QUANTITY                      
014600     03 KVLEVART2            PIC 9(7).                                    
014700*                                 FAKTISKT LEVERERAT ANTAL I KOLL         
014800*                                 IT                                      
014900*                                 DELIVERED QUANTITY IN ONE CASE          
015000     03 KVQPACK-0            PIC 9(5).                                    
015100*                                 ANTAL I Q0 FÖRPACKNING                  
015200     03 KVQPACK-1            PIC 9(5).                                    
015300*                                 ANTAL I Q1 FÖRPACKNING                  
015400*                                 QUANTITY IN BULK PACK Q1                
015500     03 KVQPACK-2            PIC 9(5).                                    
015600*                                 ANTAL I Q2 FÖRPACKNING                  
015700*                                 QUANTITY IN BULK PACK Q2                
015800     03 KVQPACK-3            PIC 9(5).                                    
015900*                                 ANTAL I Q3 FÖRPACKNING                  
016000*                                 QUANTITY IN BULK PACK Q3                
016100     03 KVQPACK-4            PIC 9(5).                                    
016200*                                 ANTAL I Q4 FÖRPACKNING                  
016300*                                 QUANTITY IN BULK PACK Q4                
016400     03 KVULOAD              PIC 9(7).                                    
016500*                                 MIN ENHETSLAST FRÅN LEVERANTÖR          
016600*                                 MIN LOAD FROM SUPPLIER                  
016700     03 PRARTNTO             PIC 9(7)V9(2).                               
016800*                                 ARTIKELPRIS NETTO                       
016900*                                 NET PRICE EACH   (FOB NET)              
017000     03 PRARTNTO-LOC         PIC 9(7)V9(2).                               
017100*                                 ARTIKELPRIS NETTO LOKAL VALUTA          
017200*                                 NET PRICE EACH LOCAL CURRENCY           
017300     03 PRARTSTD             PIC 9(7)V9(2).                               
017400*                                 ARTIKELSTANDARDPRIS                     
017500*                                 STANDARD PRICE                          
017600     03 TIBEGPAC             PIC 9(6).                                    
017700*                                 BEGÄRD PACKNINGSDAG    (ÅÅMMDD)         
017800*                                 REQUESTED PACKING DATE (YYMMDD)         
017900     03 TIBEGTID             PIC 9(4).                                    
018000     03 TIFAKT               PIC 9(6).                                    
018100*                                 FAKTURERINGSDATUM (ÅÅMMDD)              
018200*                                 INVOICING DATE   (YYMMDD)               
018300     03 TIFAKTID             PIC 9(4).                                    
018400*                                 FAKTURERINGSTID                         
018500     03 TILASTID             PIC 9(4).                                    
018600*                                 LASTNINGSTID                            
018700     03 TILASTN              PIC 9(6).                                    
018800*                                 LASTNINGSDATUM         (ÅÅMMDD)         
018900*                                 LOADING DATE           (YYMMDD)         
019000     03 TIORDREG             PIC 9(6).                                    
019100*                                 ORDERREGISTRERINGSDATUM  ÅÅMMDD         
019200*                                 ORDER REGISTRATION DATE  YYMMDD         
019300     03 TIPACKN              PIC 9(6).                                    
019400*                                 PACKNINGSDATUM         (ÅÅMMDD)         
019500*                                 PACKING DATE           (YYMMDD)         
019600     03 TIPACTID             PIC 9(4).                                    
019700*                                 PACKNINGSTID  TTMMSS                    
019800*                                 PACKING TIME  HHMMSS                    
019900     03 TIREGTID             PIC 9(4).                                    
020000*                                 REGISTRERINGSTID                        
020100*                                 GENERAL REGISTRATION TIME               
020200     03 TIREPDAT             PIC 9(6).                                    
020300*                                 REPAIR DATE                             
020400*                                 REPAIR DATE                             
020500     03 TISKEPPN             PIC 9(6).                                    
020600*                                 SKEPPNINGSDATUM  (ÅÅMMDD)               
020700*                                 SHIPPING DATE    (YYMMDD)               
020800     03 TISKPTID             PIC 9(4).                                    
020900*                                 SKEPPNINGSTID                           
021000     03 TISUPTID             PIC 9(4).                                    
021100*                                 SÄNDNINGSTID DIREKTLEVERANTÖR           
021200*                                 SHIPPING TIME DIRECT SUPPLIER           
021300     03 TIUTSKR              PIC 9(6).                                    
021400*                                 UTSKRIFTDATUM  (ÅÅMMDD)                 
021500*                                 PRINTING DATE  (YYMMDD)                 
021600     03 TIUTSTID             PIC 9(4).                                    
021700*                                 UTSKRIFTSTID (TTMMSS)                   
021800*                                 TIME OF PRINTING (HHMMSS)               
021900     03 VKARTNTO             PIC 9(4)V9(3).                               
022000*                                 ARTIKELVIKT NETTO (KG)                  
022100*                                 PART NET WEIGHT (KG)                    
022200     03 VLARTNTO             PIC 9(8)V9(1).                               
022300*                                 ARTIKELVOLYM NETTO (CM3)                
022400*                                 PART NET VOLUME    (CM3)                
022500     03 IDSTATNR-3           PIC 9(9).                                    
022600*                                 STATISTISKT NUMMER                      
022700*                                 1 = NORSKT                              
022800*                                 2 = ENGELSKT                            
022900*                                 3 = BELGISKT                            
023000*                                 4 = PERUANSKT                           
023100*                                 5 = SVENSKT                             
023200*                                 6 =                                     
023300*                                 STATISTICAL NO.                         
023400     03 IDVIN                PIC X(17).                                   
023500*                                 VIN ID FORDON                           
023600*                                 VEHICLE VIN ID                          
023700*** END OF VILMAII-COPY LENGTH= 385 BYTES                                 
