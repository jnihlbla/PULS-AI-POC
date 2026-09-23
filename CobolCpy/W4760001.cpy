000100 01  BILL-W4760001.                                                       
000200*                                 RAD FÖR FAKTURATRANSAR FRÅN BIL         
000300*                                 L-IT                                    
000400     03 BILL-IDPRODNR        PIC S9(7)           COMP-3.                  
000500*                                 PRODUKTIONSNUMMER                       
000600     03 BILL-IDKOLLI         PIC S9(5)           COMP-3.                  
000700*                                 KOLLINUMMER                             
000800     03 BILL-IDPURAD         PIC S9(5)           COMP-3.                  
000900*                                 RADNUMMER PÅ PACKUNDERLAG               
001000     03 BILL-BEART           PIC X(25).                                   
001100*                                 ARTIKELBENÄMNING                        
001200     03 BILL-DAFINDOC        PIC 9(8).                                    
001300*                                 DOKUMENT DATUM (ÅÅÅÅMMDD)               
001400     03 BILL-IDDC            PIC X(2).                                    
001500*                                 IDENTIFIERARE LAGER                     
001600     03 BILL-IDDISTR         PIC S9(5)           COMP-3.                  
001700*                                 DISTRIKTNUMMER                          
001800     03 BILL-IDKUNDNR        PIC S9(7)           COMP-3.                  
001900*                                 KUNDNUMMER                              
002000     03 BILL-IDORDNR7        PIC S9(7)           COMP-3.                  
002100*                                 ORDERNUMMER                             
002200     03 BILL-IDFAKT          PIC S9(7)           COMP-3.                  
002300*                                 FAKTURANUMMER                           
002400     03 BILL-IDFKNGRP        PIC S9(5)           COMP-3.                  
002500*                                 FUNKTIONSGRUPP                          
002600     03 BILL-IDPARTNR        PIC X(9).                                    
002700*                                 FINANCIELL KUND                         
002800     03 BILL-IDSHIPM         PIC 9(7).                                    
002900*                                 SKEPPNINGSNUMMER                        
003000     03 BILL-KDARTRAB        PIC 9(2).                                    
003100*                                 RABATTKOD (ARTIKELPRIS)                 
003200     03 BILL-KDPRODSL        PIC S9(3)           COMP-3.                  
003300*                                 PRODUKTSLAG                             
003400     03 BILL-KDVALISO-FAKT   PIC X(3).                                    
003500*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
003600     03 BILL-PRAVCOST        PIC S9(7)V9(2)      COMP-3.                  
003700*                                 MEDELVÄRDESKOSTNAD I UTL.VALUTA         
003800     03 BILL-PRAVCOST-CORE   PIC S9(7)V9(2)      COMP-3.                  
003900*                                 OBJEKTETS MEDELVÄRDESKOSTNAD I          
004000*                                 UTL.VALUTA                              
004100     03 BILL-SUBTO-LINE      PIC S9(11)V9(2)     COMP-3.                  
004200*                                 TOTAL SALES AMOUNT PER LINE INC         
004300*                                 L. VAT                                  
004400     03 BILL-SUBTO-TOT       PIC S9(11)V9(2)     COMP-3.                  
004500*                                 TOTAL SALES AMOUNT INCL. VAT            
004600     03 BILL-SUNTO-LINE      PIC S9(11)V9(2)     COMP-3.                  
004700*                                 TOTAL SALES AMOUNT PER LINE EXC         
004800*                                 L. VAT                                  
004900     03 BILL-SUNTO-TOT       PIC S9(11)V9(2)     COMP-3.                  
005000*                                 TOTAL SALES AMOUNT EXCL. VAT            
005100     03 BILL-SUVAT-FAKT      PIC S9(11)V9(2)     COMP-3.                  
005200*                                 TOTALT MOMSVÄRDE PER FAKTURA/KR         
005300*                                 EDITNOTA                                
005400     03 BILL-SUVAT-LINE      PIC S9(11)V9(2)     COMP-3.                  
005500*                                 MOMSVÄRDE PER FAKTURARAD                
005600     03 BILL-TIFINDOC        PIC S9(7)           COMP-3.                  
005700*                                 DOKUMENT KLOCKSLAG (TTMMSS)             
005800     03 BILL-TISKEPPN        PIC S9(7)           COMP-3.                  
005900*                                 SKEPPNINGSDATUM  (ÅÅMMDD)               
006000     03 BILL-VKARTNTO        PIC S9(4)V9(3)      COMP-3.                  
006100*                                 ARTIKELVIKT NETTO (KG) MED EMB          
006200     03 BILL-KDLEVVIL        PIC S9              COMP-3.                  
006300*                                 LEVERANSVILLKOR                         
006400     03 BILL-KDVALISO-BET    PIC X(3).                                    
006500*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
006600     03 BILL-PRKURS-BET      PIC S9(6)V9(5)      COMP-3.                  
006700*                                 VALUTAKURS                              
006800     03 BILL-PRKURS-FAKT     PIC S9(6)V9(5)      COMP-3.                  
006900*                                 VALUTAKURS                              
007000     03 BILL-PRKURS-FIKTIV   PIC S9(6)V9(5)      COMP-3.                  
007100*                                 VALUTAKURS                              
007200     03 BILL-FLCOD           PIC X.                                       
007300*                                 KONTANTBETALANDE KUND                   
007400     03 BILL-IDLEVNR-ART     PIC X(5).                                    
007500*                                 LEVERANTÖRNR PÅ ARTIKEL                 
007600     03 BILL-KDFAKSTA-EXP    PIC X.                                       
007700*                                 DUBBELFAKTURERING STATUS                
007800     03 BILL-IDVAT-LEG       PIC X(17).                                   
007900*                                 MOMSREGISTRERINGSNUMMER LEGAL S         
008000*                                 ÄLJARE                                  
008100     03 BILL-IDVAT-RESP      PIC X(17).                                   
008200*                                 MOMSREGISTRERINGSNUMMER ANSVARI         
008300*                                 G AVD                                   
008400     03 BILL-IDVAT-BET       PIC X(17).                                   
008500*                                 MOMSREGISTRERINGSNUMMER BETALAR         
008600*                                 E                                       
008700     03 BILL-IDVAT-AGENT     PIC X(17).                                   
008800*                                 MOMSREGISTRERINGSNUMMER AGENT           
008900     03 BILL-IDVAT-DDGS-RESP PIC X(17).                                   
009000*                                 MOMSREGISTRERINGSNUMMER ANSVARI         
009100*                                 G AVD                                   
009200     03 BILL-SUNTO-PART-LOC  PIC S9(11)V9(2)     COMP-3.                  
009300*                                 TOTAL SALES AMOUNT PARTS EXCL.          
009400*                                 VAT                                     
009500*                                 NET TOT FOR PART DNI                    
009600*                                                                         
009700     03 BILL-SUVAT-BILLIT-TOT-PART-L                                      
009800                             PIC S9(11)V9(2)     COMP-3.                  
009900*                                 SUMMERAT NET TOT VAT FÖR ARTIKE         
010000*                                 L DNI                                   
010100     03 BILL-SUBTO-TOT-PART-LOC                                           
010200                             PIC S9(11)V9(2)     COMP-3.                  
010300*                                 TOTAL SALES AMOUNT INCL. VAT            
010400*                                 NET TOT DNI AMOUNT INCL. VAT            
010500     03 BILL-SUNTO-TOT-LOC   PIC S9(11)V9(2)     COMP-3.                  
010600*                                 TOTAL SALES AMOUNT EXCL. VAT            
010700*                                 NET TOT DNI                             
010800     03 BILL-SUVAT-BILLIT-TOT-LOC                                         
010900                             PIC S9(11)V9(2)     COMP-3.                  
011000*                                 SUMMERAT MOMSVÄRDE LOC                  
011100*                                 NET TOT VAT DNI                         
011200     03 BILL-SUBTO-TOT-LOC   PIC S9(11)V9(2)     COMP-3.                  
011300*                                 TOTAL SALES AMOUNT INCL. VAT            
011400*                                 BRUTTO TOT DNI                          
011500     03 BILL-KDVALISO-LOC    PIC X(3).                                    
011600*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
011700*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
011800     03 BILL-PRKURS-LOC      PIC S9(6)V9(5)      COMP-3.                  
011900*                                 VALUTAKURS                              
012000     03 BILL-KDTECKEN-LOC    PIC X.                                       
012100*                                 PLUS ELLER MINUS (+ -)                  
012200     03 BILL-SUNTO-LOCC      PIC S9(11)V9(2)     COMP-3.                  
012300*                                 TOTAL SALES AMOUNT EXCL. VAT            
012400*                                 CURRENCY CONVERSION AMOUNT DNI          
012500     03 BILL-SUNTO-PART-RECALC                                            
012600                             PIC S9(11)V9(2)     COMP-3.                  
012700*                                 TOTAL SALES AMOUNT PARTS EXCL.          
012800*                                 VAT                                     
012900*                                 NET TOT FOR PART RECALCULATION          
013000*                                 VAT                                     
013100     03 BILL-SUVAT-BILLIT-TOT-PART-R                                      
013200                             PIC S9(11)V9(2)     COMP-3.                  
013300*                                 SUMMERAT MOMSVÄRDE                      
013400     03 BILL-SUBTO-TOT-PART-RECALC                                        
013500                             PIC S9(11)V9(2)     COMP-3.                  
013600*                                 TOTAL SALES AMOUNT INCL. VAT            
013700*                                  BRUTTO TOT FOR PART RECALCULAT         
013800*                                 ION                                     
013900     03 BILL-SUNTO-TOT-RECALC                                             
014000                             PIC S9(11)V9(2)     COMP-3.                  
014100*                                 TOTAL SALES AMOUNT EXCL. VAT            
014200*                                 NET TOT RECALCULATION EXCL.VAT          
014300     03 BILL-SUVAT-BILLIT-TOT-RECALC                                      
014400                             PIC S9(11)V9(2)     COMP-3.                  
014500*                                 SUMMERAT MOMSVÄRDE                      
014600     03 BILL-SUBTO-TOT-RECALC                                             
014700                             PIC S9(11)V9(2)     COMP-3.                  
014800*                                 TOTAL SALES AMOUNT INCL. VAT            
014900*                                 BRUTTO TOT RECALCULATION                
015000     03 BILL-KDVALISO-RECALC PIC X(3).                                    
015100*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
015200     03 BILL-PRKURS-RECALC   PIC S9(6)V9(5)      COMP-3.                  
015300*                                 VALUTAKURS                              
015400     03 BILL-KDTECKEN-RECALC PIC X.                                       
015500*                                 PLUS ELLER MINUS (+ -)                  
015600     03 BILL-SUNTO-LOCC-RECALC                                            
015700                             PIC S9(11)V9(2)     COMP-3.                  
015800*                                 TOTAL SALES AMOUNT EXCL. VAT            
015900*                                 CURRENCY CONVERSION AMOUNT RECA         
016000*                                 LCULATION                               
016100     03 BILL-PRAVCOST-BILLIT PIC S9(7)V9(2)      COMP-3.                  
016200*                                 MEDELVÄRDESKOSTNAD I UTL.VALUTA         
016300     03 BILL-KDVALISO-AVC    PIC X(3).                                    
016400*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
016500     03 BILL-PRARTNTO        PIC S9(7)V9(2)      COMP-3.                  
016600*                                 ARTIKELPRIS NETTO                       
016700     03 BILL-PRARTNTO-LOC    PIC S9(7)V9(2)      COMP-3.                  
016800*                                 ARTIKELPRIS NETTO LOKAL VALUTA          
016900     03 BILL-KDVALISO-NTO    PIC X(3).                                    
017000*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
017100*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
017200     03 BILL-IDDC-BILLIT     PIC X(2).                                    
017300*                                 IDENTIFIERARE LAGER                     
017400     03 BILL-KVLEVART        PIC S9(7)           COMP-3.                  
017500*                                 LEVERERAT ANTAL STYCK                   
017600     03 BILL-KDARTURS        PIC X(2).                                    
017700*                                 ARTIKELURSPRUNGSKOD                     
017800     03 BILL-FLPCOO          PIC X.                                       
017900*                                 FLAGGA OM FÖRMÅNSAVTAL URS.LAND         
018000*** END OF VILMAII-COPY LENGTH= 412 BYTES                                 
