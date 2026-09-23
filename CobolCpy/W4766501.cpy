000100 01  BILL-W4766501.                                                       
000200*                                 RAD FÖR FAKTURATRANSAR FRÅN BIL         
000300*                                 L-IT                                    
000400     03 BILL-IDARTNR         PIC S9(9)           COMP-3.                  
000500*                                 ARTIKELNUMMER                           
000600     03 BILL-IDKUNDRF        PIC X(10).                                   
000700*                                 KUNDENS REFERENS (ORDERID)              
000800     03 BILL-IDPRODNR        PIC S9(7)           COMP-3.                  
000900*                                 PRODUKTIONSNUMMER                       
001000     03 BILL-IDKOLLI         PIC S9(5)           COMP-3.                  
001100*                                 KOLLINUMMER                             
001200     03 BILL-IDPURAD         PIC S9(5)           COMP-3.                  
001300*                                 RADNUMMER PÅ PACKUNDERLAG               
001400     03 BILL-IDSHIPM         PIC 9(7).                                    
001500*                                 SKEPPNINGSNUMMER                        
001600     03 BILL-IDTULL.                                                      
001700*                                 IDENTITET TULL SÄNDNING                 
001800        05 BILL-IDTULFTG     PIC X(2).                                    
001900*                                 IDENTIFIERARE TULLANDE FÖRETAG          
002000*                                                                         
002100        05 BILL-IDTULLNR     PIC 9(7).                                    
002200*                                 NUMMERSERIE INGÅENDE I TULLID           
002300*                                                                         
002400        05 BILL-RETULKS      PIC 9.                                       
002500*                                 KONTROLLSIFFRA TULLID                   
002600     03 BILL-BEART           PIC X(25).                                   
002700*                                 ARTIKELBENÄMNING                        
002800     03 BILL-DAFINDOC        PIC 9(8).                                    
002900*                                 DOKUMENT DATUM (ÅÅÅÅMMDD)               
003000     03 BILL-FLORDSPE        PIC X.                                       
003100*                                 SPECIALORDERFLAGGA                      
003200     03 BILL-FLOVRLEV        PIC X.                                       
003300*                                 ÖVERLEVERANS                            
003400     03 BILL-IDDC            PIC X(2).                                    
003500*                                 IDENTIFIERARE LAGER                     
003600     03 BILL-IDDISTR         PIC S9(5)           COMP-3.                  
003700*                                 DISTRIKTNUMMER                          
003800     03 BILL-IDKUNDNR        PIC S9(7)           COMP-3.                  
003900*                                 KUNDNUMMER                              
004000     03 BILL-IDORDNR7        PIC S9(7)           COMP-3.                  
004100*                                 ORDERNUMMER                             
004200     03 BILL-IDFAKT          PIC S9(7)           COMP-3.                  
004300*                                 FAKTURANUMMER                           
004400     03 BILL-IDPARTNR        PIC X(9).                                    
004500*                                 FINANCIELL KUND                         
004600     03 BILL-KDFAKTYP        PIC X.                                       
004700*                                 FAKTURATYP                              
004800     03 BILL-KDFRAKT         PIC S9(3)           COMP-3.                  
004900*                                 FRAKTSÄTT DC TILL KUND                  
005000     03 BILL-KDORDKL         PIC S9              COMP-3.                  
005100*                                 ORDERKLASS                              
005200     03 BILL-KDTULLVE        PIC S9              COMP-3.                  
005300*                                 TYP AV PRIS PÅ TULLFAKTURA              
005400     03 BILL-KDVALISO        PIC X(3).                                    
005500*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
005600     03 BILL-KVLEVART        PIC S9(7)           COMP-3.                  
005700*                                 LEVERERAT ANTAL STYCK                   
005800     03 BILL-PRARTNTO        PIC S9(7)V9(2)      COMP-3.                  
005900*                                 ARTIKELPRIS NETTO                       
006000     03 BILL-PRARTNTO-LOC    PIC S9(7)V9(2)      COMP-3.                  
006100*                                 ARTIKELPRIS NETTO LOKAL VALUTA          
006200     03 BILL-VKARTNTO        PIC S9(4)V9(3)      COMP-3.                  
006300*                                 ARTIKELVIKT NETTO (KG) MED EMB          
006400     03 BILL-FLLSBOK         PIC X.                                       
006500*                                 LAGERAVBOKNING                          
006600     03 BILL-VKART-NTO-KG    PIC S9(4)V9(3)      COMP-3.                  
006700*                                 ART. NETTOVIKT I KG UTAN EMB            
006800     03 BILL-FLSTUDSUP       PIC X.                                       
006900*                                 UPPDATERING AV STUDS J/N                
007000     03 BILL-SUNTO-PART-LOC  PIC S9(11)V9(2)     COMP-3.                  
007100*                                 TOTAL SALES AMOUNT PARTS EXCL.          
007200*                                 VAT                                     
007300*                                 NET TOT FOR PART DNI                    
007400*                                                                         
007500     03 BILL-SUBTO-TOT-PART-LOC                                           
007600                             PIC S9(11)V9(2)     COMP-3.                  
007700*                                 TOTAL SALES AMOUNT INCL. VAT            
007800*                                 NET TOT DNI AMOUNT INCL. VAT            
007900     03 BILL-SUNTO-TOT-LOC   PIC S9(11)V9(2)     COMP-3.                  
008000*                                 TOTAL SALES AMOUNT EXCL. VAT            
008100*                                 NET TOT DNI                             
008200     03 BILL-SUVAT-BILLIT-TOT-LOC                                         
008300                             PIC S9(11)V9(2)     COMP-3.                  
008400*                                 SUMMERAT MOMSVÄRDE LOC                  
008500*                                 NET TOT VAT DNI                         
008600     03 BILL-SUBTO-TOT-LOC   PIC S9(11)V9(2)     COMP-3.                  
008700*                                 TOTAL SALES AMOUNT INCL. VAT            
008800*                                 BRUTTO TOT DNI                          
008900     03 BILL-KDVALISO-LOC    PIC X(3).                                    
009000*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
009100*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
009200     03 BILL-PRKURS-LOC      PIC S9(6)V9(5)      COMP-3.                  
009300*                                 VALUTAKURS                              
009400     03 BILL-KDTECKEN-LOC    PIC X.                                       
009500*                                 PLUS ELLER MINUS (+ -)                  
009600     03 BILL-SUNTO-LOCC      PIC S9(11)V9(2)     COMP-3.                  
009700*                                 TOTAL SALES AMOUNT EXCL. VAT            
009800*                                 CURRENCY CONVERSION AMOUNT DNI          
009900     03 BILL-SUNTO-PART-RECALC                                            
010000                             PIC S9(11)V9(2)     COMP-3.                  
010100*                                 TOTAL SALES AMOUNT PARTS EXCL.          
010200*                                 VAT                                     
010300*                                 NET TOT FOR PART RECALCULATION          
010400*                                 VAT                                     
010500     03 BILL-SUBTO-TOT-PART-RECALC                                        
010600                             PIC S9(11)V9(2)     COMP-3.                  
010700*                                 TOTAL SALES AMOUNT INCL. VAT            
010800*                                  BRUTTO TOT FOR PART RECALCULAT         
010900*                                 ION                                     
011000     03 BILL-SUNTO-TOT-RECALC                                             
011100                             PIC S9(11)V9(2)     COMP-3.                  
011200*                                 TOTAL SALES AMOUNT EXCL. VAT            
011300*                                 NET TOT RECALCULATION EXCL.VAT          
011400     03 BILL-SUBTO-TOT-RECALC                                             
011500                             PIC S9(11)V9(2)     COMP-3.                  
011600*                                 TOTAL SALES AMOUNT INCL. VAT            
011700*                                 BRUTTO TOT RECALCULATION                
011800     03 BILL-KDVALISO-RECALC PIC X(3).                                    
011900*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
012000     03 BILL-PRKURS-RECALC   PIC S9(6)V9(5)      COMP-3.                  
012100*                                 VALUTAKURS                              
012200     03 BILL-KDTECKEN-RECALC PIC X.                                       
012300*                                 PLUS ELLER MINUS (+ -)                  
012400     03 BILL-SUNTO-LOCC-RECALC                                            
012500                             PIC S9(11)V9(2)     COMP-3.                  
012600*                                 TOTAL SALES AMOUNT EXCL. VAT            
012700*                                 CURRENCY CONVERSION AMOUNT RECA         
012800*                                 LCULATION                               
012900     03 BILL-PRAVCOST-BILLIT PIC S9(7)V9(2)      COMP-3.                  
013000*                                 MEDELVÄRDESKOSTNAD I UTL.VALUTA         
013100     03 BILL-KDVALISO-AVC    PIC X(3).                                    
013200*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
013300     03 BILL-KDVALISO-NTO    PIC X(3).                                    
013400*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
013500*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
013600     03 BILL-IDDC-BILLIT     PIC X(2).                                    
013700*                                 IDENTIFIERARE LAGER                     
013800     03 BILL-KDARTURS        PIC X(2).                                    
013900*                                 ARTIKELURSPRUNGSKOD                     
014000     03 BILL-FLPCOO          PIC X.                                       
014100*                                 FLAGGA OM FÖRMÅNSAVTAL URS.LAND         
014200     03 BILL-IDUSER-OREG     PIC X(8).                                    
014300*                                 ANSVARIGT USERID ORDERREG.              
014400     03 BILL-FLDIRLEV        PIC X.                                       
014500*                                 DIREKTLEVERANS ?                        
014600     03 BILL-IDKONTO         PIC S9(11)          COMP-3.                  
014700*                                 KONTO                                   
014800     03 BILL-IDKST           PIC X(10).                                   
014900*                                 KOSTNADSSTÄLLE                          
015000     03 BILL-IDANALYS        PIC X(12).                                   
015100*                                 ANALYSNUMMER                            
015200     03 BILL-KVBEART         PIC S9(7)           COMP-3.                  
015300*                                 BESTÄLLT ANTAL STYCKEN                  
015400     03 BILL-KVAVBART        PIC S9(7)           COMP-3.                  
015500*                                 AVBOKAT ANTAL ARTIKLAR                  
015600     03 BILL-IDLEVNR         PIC X(5).                                    
015700*                                 LEVERANTÖRNUMMER                        
015800     03 BILL-KDVAT           PIC X(2).                                    
015900*                                 MOMSKOD                                 
016000     03 BILL-ADLAGOMR        PIC S9(3)           COMP-3.                  
016100*                                 LAGEROMRÅDE                             
016200     03 BILL-IDUSER-PACK     PIC X(8).                                    
016300*                                 ANSVARIGT USERID PACKARE                
016400     03 BILL-KVORDRAD        PIC S9(5)           COMP-3.                  
016500*                                 ANTAL ORDERRADER                        
016600     03 BILL-KDKOLLI         PIC X(8).                                    
016700*                                 KOLLIKOD                                
016800     03 BILL-TIFAKT          PIC S9(7)           COMP-3.                  
016900*                                 FAKTURERINGSDATUM (ÅÅMMDD)              
017000     03 BILL-TIREGDAT        PIC S9(7)           COMP-3.                  
017100*                                 REGISTRERINGSDATUM (ÅÅMMDD OR Å         
017200*                                 ÅÅÅ-MM-DD)                              
017300     03 BILL-VKORDBTO-KOLLI  PIC S9(6)V9(1)      COMP-3.                  
017400*                                 ORDERVIKT BRUTTO PER KOLLI              
017500     03 BILL-VKORDNTO-KOLLI  PIC S9(6)V9(1)      COMP-3.                  
017600*                                 ORDERVIKT NETTO PER KOLLI               
017700     03 BILL-TISKEPPN        PIC S9(7)           COMP-3.                  
017800*                                 SKEPPNINGSDATUM  (ÅÅMMDD)               
017900     03 BILL-SUBTO-LINE      PIC S9(11)V9(2)     COMP-3.                  
018000*                                 TOTAL SALES AMOUNT PER LINE INC         
018100*                                 L. VAT                                  
018200     03 BILL-SUBTO-TOT       PIC S9(11)V9(2)     COMP-3.                  
018300*                                 TOTAL SALES AMOUNT INCL. VAT            
018400     03 BILL-SUNTO-LINE      PIC S9(11)V9(2)     COMP-3.                  
018500*                                 TOTAL SALES AMOUNT PER LINE EXC         
018600*                                 L. VAT                                  
018700     03 BILL-SUNTO-TOT       PIC S9(11)V9(2)     COMP-3.                  
018800*                                 TOTAL SALES AMOUNT EXCL. VAT            
018900     03 BILL-SUVAT-LINE      PIC S9(11)V9(2)     COMP-3.                  
019000*                                 MOMSVÄRDE PER FAKTURARAD                
019100     03 BILL-IDFKNGRP        PIC S9(5)           COMP-3.                  
019200*                                 FUNKTIONSGRUPP                          
019300     03 BILL-KDARTRAB        PIC 9(2).                                    
019400*                                 RABATTKOD (ARTIKELPRIS)                 
019500     03 BILL-KDPRODSL        PIC S9(3)           COMP-3.                  
019600*                                 PRODUKTSLAG                             
019700     03 BILL-PRAVCOST-CORE   PIC S9(7)V9(2)      COMP-3.                  
019800*                                 OBJEKTETS MEDELVÄRDESKOSTNAD I          
019900*                                 UTL.VALUTA                              
020000     03 BILL-PRKURS-BET      PIC S9(6)V9(5)      COMP-3.                  
020100*                                 VALUTAKURS                              
020200     03 BILL-PRKURS-FAKT     PIC S9(6)V9(5)      COMP-3.                  
020300*                                 VALUTAKURS                              
020400     03 BILL-PRKURS-FIKTIV   PIC S9(6)V9(5)      COMP-3.                  
020500*                                 VALUTAKURS                              
020600     03 BILL-KDFAKSTA-EXP    PIC X.                                       
020700*                                 DUBBELFAKTURERING STATUS                
020800     03 BILL-KDVALISO-BET    PIC X(3).                                    
020900*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
021000     03 BILL-IDBORD          PIC X(3).                                    
021100*                                 PACK-BORD                               
021200*** END OF VILMAII-COPY LENGTH= 413 BYTES                                 
