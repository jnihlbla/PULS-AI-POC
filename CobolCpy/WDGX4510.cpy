000100 01  4510-WDGX4510.                                                       
000200*                                 DDI FAKTURA TRANSAR TILL VIPS           
000300*                                 ORDERRADER                              
000400*                                 FYSISK NYCKEL KY4510:                   
000500*                                 (IDPRODNR+IDKOLLI+IDPURAD)              
000600     03 4510-IDPRODNR        PIC S9(7)           COMP-3.                  
000700*                                 PRODUKTIONSNUMMER                       
000800*                                 PRODUCTION NUMBER                       
000900     03 4510-IDKOLLI         PIC S9(5)           COMP-3.                  
001000*                                 KOLLINUMMER                             
001100*                                 CASE NUMBER                             
001200     03 4510-IDPURAD         PIC S9(5)           COMP-3.                  
001300*                                 RADNUMMER PÅ PACKUNDERLAG               
001400*                                 LINENO IN PACKINGDOCUMENT               
001500     03 4510-BEART           PIC X(25).                                   
001600*                                 ARTIKELBENÄMNING                        
001700*                                 PART DESCRIPTION                        
001800     03 4510-DAFINDOC        PIC 9(8).                                    
001900*                                 DOKUMENT DATUM (ÅÅÅÅMMDD)               
002000*                                 INVOICING DATE   (YYYYMMDD)             
002100     03 4510-IDDC            PIC X(2).                                    
002200*                                 IDENTIFIERARE LAGER                     
002300*                                 WAREHOUSE IDENTIFIER                    
002400     03 4510-IDDISTR         PIC S9(5)           COMP-3.                  
002500*                                 DISTRIKTNUMMER                          
002600*                                 DISTRICT NUMBER                         
002700     03 4510-IDKUNDNR        PIC S9(7)           COMP-3.                  
002800*                                 KUNDNUMMER                              
002900*                                 CUSTOMER NO                             
003000     03 4510-IDORDNR5        PIC S9(5)           COMP-3.                  
003100*                                 ORDERNUMMER                             
003200*                                 ORDER NUMBER                            
003300     03 4510-IDFKNGRP        PIC S9(5)           COMP-3.                  
003400*                                 FUNKTIONSGRUPP                          
003500*                                 FUNCTION GROUP                          
003600     03 4510-IDPARTNR        PIC X(9).                                    
003700*                                 FINANCIELL KUND                         
003800*                                 FINANCIAL CUST                          
003900     03 4510-IDSHIPM         PIC 9(7).                                    
004000*                                 SKEPPNINGSNUMMER                        
004100*                                 SHIPMENT NO                             
004200     03 4510-KDARTRAB        PIC 9(2).                                    
004300*                                 RABATTKOD (ARTIKELPRIS)                 
004400*                                 PURCHASE DISCOUNT CODE                  
004500     03 4510-KDPRODSL        PIC S9(3)           COMP-3.                  
004600*                                 PRODUKTSLAG                             
004700*                                 PRODUCT GROUP                           
004800     03 4510-KDVALISO        PIC X(3).                                    
004900*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
005000*                                 CURRENCY CODE BY ISO-STANDARD.          
005100     03 4510-PRAVCOST        PIC S9(7)V9(2)      COMP-3.                  
005200*                                 MEDELVÄRDESKOSTNAD I UTL.VALUTA         
005300*                                 AVERAGE COST FOREIGN CURRENCY           
005400     03 4510-PRAVCOST-CORE   PIC S9(7)V9(2)      COMP-3.                  
005500*                                 OBJEKTETS MEDELVÄRDESKOSTNAD I          
005600*                                 UTL.VALUTA                              
005700*                                 AV. COST PRICE OF THE CORE IN F         
005800*                                 OR. CURR.                               
005900     03 4510-SUBTO-LINE      PIC S9(11)V9(2)     COMP-3.                  
006000*                                 TOTAL SALES AMOUNT PER LINE INC         
006100*                                 L. VAT                                  
006200     03 4510-SUBTO-TOT       PIC S9(11)V9(2)     COMP-3.                  
006300*                                 TOTAL SALES AMOUNT INCL. VAT            
006400     03 4510-SUNTO-LINE      PIC S9(11)V9(2)     COMP-3.                  
006500*                                 TOTAL SALES AMOUNT PER LINE EXC         
006600*                                 L. VAT                                  
006700     03 4510-SUNTO-TOT       PIC S9(11)V9(2)     COMP-3.                  
006800*                                 TOTAL SALES AMOUNT EXCL. VAT            
006900     03 4510-SUVAT-FAKT      PIC S9(11)V9(2)     COMP-3.                  
007000*                                 TOTALT MOMSVÄRDE PER FAKTURA/KR         
007100*                                 EDITNOTA                                
007200*                                 TOTAL VAT VALUE PER INVOICE/CRE         
007300*                                 DIT                                     
007400     03 4510-SUVAT-LINE      PIC S9(11)V9(2)     COMP-3.                  
007500*                                 MOMSVÄRDE PER FAKTURARAD                
007600*                                 VAT VALUE PER INVOICE LINE              
007700     03 4510-TIFINDOC        PIC S9(7)           COMP-3.                  
007800*                                 DOKUMENT KLOCKSLAG (TTMMSS)             
007900*                                 DOCUMENT TIME (HHMMSS)                  
008000     03 4510-TISKEPPN        PIC S9(7)           COMP-3.                  
008100*                                 SKEPPNINGSDATUM  (ÅÅMMDD)               
008200*                                 SHIPPING DATE    (YYMMDD)               
008300     03 4510-VKARTNTO        PIC S9(4)V9(3)      COMP-3.                  
008400*                                 ARTIKELVIKT NETTO (KG) MED EMB          
008500*                                 PART NET WEIGHT (KG) W/ PACKAGE         
008600     03 4510-KDLEVVIL        PIC S9              COMP-3.                  
008700*                                 LEVERANSVILLKOR                         
008800*                                 TERMS OF DELIVERY                       
008900     03 4510-KDVALISO-BET    PIC X(3).                                    
009000*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
009100*                                 CURRENCY CODE BY ISO-STANDARD.          
009200     03 4510-PRKURS-BET      PIC S9(6)V9(5)      COMP-3.                  
009300*                                 VALUTAKURS                              
009400*                                 CURRENCY EXCHANGE RATE                  
009500     03 4510-PRKURS-FAKT     PIC S9(6)V9(5)      COMP-3.                  
009600*                                 VALUTAKURS                              
009700*                                 CURRENCY EXCHANGE RATE                  
009800     03 4510-PRKURS-FIKTIV   PIC S9(6)V9(5)      COMP-3.                  
009900*                                 VALUTAKURS                              
010000*                                 CURRENCY EXCHANGE RATE                  
010100     03 4510-PRKURS-SND      PIC S9(6)V9(5)      COMP-3.                  
010200*                                 VALUTAKURS SÄNDANDE DC                  
010300*                                 CURRENCY OF SENDING DC                  
010400     03 4510-KDVALISO-SND    PIC X(3).                                    
010500*                                 SÄNDANDE LANDS VALUTAKOD                
010600*                                 CURRENCY OF SENDING COUNTRY             
010700     03 4510-FLCOD           PIC X.                                       
010800*                                 KONTANTBETALANDE KUND                   
010900*                                 CASH ON DELIVERY CUSTOMER               
011000     03 4510-IDDC-LEV        PIC X(2).                                    
011100*                                 LEVERERANDE DC I EXPORTFLÖDET           
011200*                                 DELIVERY DC IN EXPORT FLOW              
011300     03 4510-KDFAKSTA-EXP    PIC X.                                       
011400*                                 DUBBELFAKTURERING STATUS                
011500*                                 STATUS CODE DOUBLE INVOICING            
011600     03 4510-SUNTO-PART-LOC  PIC S9(11)V9(2)     COMP-3.                  
011700*                                 TOTAL SALES AMOUNT PARTS EXCL.          
011800*                                 VAT                                     
011900*                                 NET TOT FOR PART DNI                    
012000*                                                                         
012100     03 4510-SUVAT-BILLIT-TOT-PART-L                                      
012200                             PIC S9(11)V9(2)     COMP-3.                  
012300*                                 SUMMERAT NET TOT VAT FÖR ARTIKE         
012400*                                 L DNI                                   
012500*                                 TOTAL VAT VALUE PER DNI                 
012600*                                 NET TOT VAT FOR PART DNI                
012700     03 4510-SUBTO-TOT-PART-LOC                                           
012800                             PIC S9(11)V9(2)     COMP-3.                  
012900*                                 TOTAL SALES AMOUNT INCL. VAT            
013000*                                 NET TOT DNI AMOUNT INCL. VAT            
013100     03 4510-SUNTO-TOT-LOC   PIC S9(11)V9(2)     COMP-3.                  
013200*                                 TOTAL SALES AMOUNT EXCL. VAT            
013300*                                 NET TOT DNI                             
013400     03 4510-SUVAT-BILLIT-TOT-LOC                                         
013500                             PIC S9(11)V9(2)     COMP-3.                  
013600*                                 SUMMERAT MOMSVÄRDE LOC                  
013700*                                 NET TOT VAT DNI                         
013800*                                 TOTAL VAT VALUE                         
013900     03 4510-SUBTO-TOT-LOC   PIC S9(11)V9(2)     COMP-3.                  
014000*                                 TOTAL SALES AMOUNT INCL. VAT            
014100*                                 BRUTTO TOT DNI                          
014200     03 4510-KDVALISO-LOC    PIC X(3).                                    
014300*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
014400*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
014500*                                 CURRENCY CODE BY ISO-STANDARD.          
014600     03 4510-PRKURS-LOC      PIC S9(6)V9(5)      COMP-3.                  
014700*                                 VALUTAKURS                              
014800*                                 CURRENCY EXCHANGE RATE                  
014900*                                 CURRENCY RATE DNI                       
015000     03 4510-KDTECKEN-LOC    PIC X.                                       
015100*                                 PLUS ELLER MINUS (+ -)                  
015200*                                 PLUS OR MINUS                           
015300     03 4510-SUNTO-LOCC      PIC S9(11)V9(2)     COMP-3.                  
015400*                                 TOTAL SALES AMOUNT EXCL. VAT            
015500*                                 CURRENCY CONVERSION AMOUNT DNI          
015600     03 4510-SUNTO-PART-RECALC                                            
015700                             PIC S9(11)V9(2)     COMP-3.                  
015800*                                 TOTAL SALES AMOUNT PARTS EXCL.          
015900*                                 VAT                                     
016000*                                 NET TOT FOR PART RECALCULATION          
016100*                                 VAT                                     
016200     03 4510-SUVAT-BILLIT-TOT-PART-R                                      
016300                             PIC S9(11)V9(2)     COMP-3.                  
016400*                                 SUMMERAT MOMSVÄRDE                      
016500*                                 TOTAL VAT VALUE                         
016600*                                 NET TOT VAT FOR PART RECALCULAT         
016700*                                 ION                                     
016800     03 4510-SUBTO-TOT-PART-RECALC                                        
016900                             PIC S9(11)V9(2)     COMP-3.                  
017000*                                 TOTAL SALES AMOUNT INCL. VAT            
017100*                                  BRUTTO TOT FOR PART RECALCULAT         
017200*                                 ION                                     
017300     03 4510-SUNTO-TOT-RECALC                                             
017400                             PIC S9(11)V9(2)     COMP-3.                  
017500*                                 TOTAL SALES AMOUNT EXCL. VAT            
017600*                                 NET TOT RECALCULATION EXCL.VAT          
017700     03 4510-SUVAT-BILLIT-TOT-RECALC                                      
017800                             PIC S9(11)V9(2)     COMP-3.                  
017900*                                 SUMMERAT MOMSVÄRDE                      
018000*                                 TOTAL VAT VALUE                         
018100     03 4510-SUBTO-TOT-RECALC                                             
018200                             PIC S9(11)V9(2)     COMP-3.                  
018300*                                 TOTAL SALES AMOUNT INCL. VAT            
018400*                                 BRUTTO TOT RECALCULATION                
018500     03 4510-KDVALISO-RECALC PIC X(3).                                    
018600*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
018700*                                 CURRENCY CODE BY ISO-STANDARD.          
018800     03 4510-PRKURS-RECALC   PIC S9(6)V9(5)      COMP-3.                  
018900*                                 VALUTAKURS                              
019000*                                 CURRENCY EXCHANGE RATE                  
019100*                                 CURRENCY RATE RECALCULATION             
019200     03 4510-KDTECKEN-RECALC PIC X.                                       
019300*                                 PLUS ELLER MINUS (+ -)                  
019400*                                 PLUS OR MINUS                           
019500*                                 CURRENCY CONVERSION SIGN RECALC         
019600*                                 ULATION                                 
019700     03 4510-SUNTO-LOCC-RECALC                                            
019800                             PIC S9(11)V9(2)     COMP-3.                  
019900*                                 TOTAL SALES AMOUNT EXCL. VAT            
020000*                                 CURRENCY CONVERSION AMOUNT RECA         
020100*                                 LCULATION                               
020200     03 4510-PRAVCOST-BILLIT PIC S9(7)V9(2)      COMP-3.                  
020300*                                 MEDELVÄRDESKOSTNAD I UTL.VALUTA         
020400*                                 AVERAGE COST FOREIGN CURRENCY           
020500     03 4510-KDVALISO-AVC    PIC X(3).                                    
020600*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
020700*                                 CURRENCY CODE BY ISO-STANDARD.          
020800*                                 AVERAGE COST CURRENCY                   
020900     03 4510-PRARTNTO        PIC S9(7)V9(2)      COMP-3.                  
021000*                                 ARTIKELPRIS NETTO                       
021100*                                 NET PRICE EACH   (FOB NET)              
021200     03 4510-PRARTNTO-LOC    PIC S9(7)V9(2)      COMP-3.                  
021300*                                 ARTIKELPRIS NETTO LOKAL VALUTA          
021400*                                 NET PRICE EACH LOCAL CURRENCY           
021500     03 4510-KDVALISO-NTO    PIC X(3).                                    
021600*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
021700*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
021800*                                 CURRENCY CODE BY ISO-STANDARD.          
021900     03 4510-IDDC-BILLIT     PIC X(2).                                    
022000*                                 IDENTIFIERARE LAGER                     
022100*                                 WAREHOUSE IDENTIFIER                    
022200     03 4510-KVLEVART        PIC S9(7)           COMP-3.                  
022300*                                 LEVERERAT ANTAL STYCK                   
022400*                                 DELIVERED QUANTITY                      
022500     03 4510-KDARTURS        PIC X(2).                                    
022600*                                 ARTIKELURSPRUNGSKOD                     
022700*                                 COUNTRY OF ORIGIN                       
022800     03 4510-FLPCOO          PIC X.                                       
022900*                                 FLAGGA OM FÖRMÅNSAVTAL URS.LAND         
023000*                                 FLAG PREF.AGREM. COUNTRY ORIGIN         
023100*** END OF VILMAII-COPY LENGTH= 328 BYTES                                 
