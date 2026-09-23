000100 01  FAK-WDL501.                                                          
000200*                                 INVOICE HISTORY                         
000300*                                 INVOICE HEAD                            
000400*                                 FYSISK NYCKEL: IDFAKT                   
000500     03 FAK-IDFAKT           PIC S9(7)           COMP-3.                  
000600*                                 FAKTURANUMMER                           
000700*                                 INVOICE NO.                             
000800     03 FAK-IDDC             PIC X(2).                                    
000900*                                 IDENTIFIERARE LAGER                     
001000*                                 WAREHOUSE IDENTIFIER                    
001100     03 FAK-IDDC-LEV         PIC X(2).                                    
001200*                                 LEVERERANDE DC I EXPORTFLÖDET           
001300*                                 DELIVERY DC IN EXPORT FLOW              
001400     03 FAK-FLDIRLEV         PIC X.                                       
001500*                                 DIREKTLEVERANS ?                        
001600*                                 DIRECT DELIVERY ?                       
001700     03 FAK-KDFAKTYP         PIC X.                                       
001800*                                 FAKTURATYP                              
001900*                                 INVOICE TYPE                            
002000     03 FAK-KDVALISO         PIC X(3).                                    
002100*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
002200*                                 CURRENCY CODE BY ISO-STANDARD.          
002300     03 FAK-KDVAT            PIC X(2).                                    
002400*                                 MOMSKOD                                 
002500*                                 VAT CODE                                
002600     03 FAK-IDANALYS         PIC X(12).                                   
002700*                                 ANALYSNUMMER                            
002800*                                 ANALYSIS NUMBER                         
002900     03 FAK-IDKONTO          PIC S9(11)          COMP-3.                  
003000*                                 KONTO                                   
003100*                                 ACCOUNT                                 
003200     03 FAK-IDKST            PIC X(10).                                   
003300*                                 KOSTNADSSTÄLLE                          
003400*                                 COST CENTRE                             
003500     03 FAK-IDPARTNR         PIC X(9).                                    
003600*                                 FINANCIELL KUND                         
003700*                                 FINANCIAL CUST                          
003800     03 FAK-IDDISTR          PIC S9(5)           COMP-3.                  
003900*                                 DISTRIKTNUMMER                          
004000*                                 DISTRICT NUMBER                         
004100     03 FAK-FLSTUDSUP        PIC X.                                       
004200*                                 UPPDATERING AV STUDS J/N                
004300*                                 UPDATING BOUNCE Y/N                     
004400     03 FAK-SUBTO-TOT        PIC S9(11)V9(2)     COMP-3.                  
004500*                                 TOTAL SALES AMOUNT INCL. VAT            
004600     03 FAK-SUNTO-TOT        PIC S9(11)V9(2)     COMP-3.                  
004700*                                 TOTAL SALES AMOUNT EXCL. VAT            
004800     03 FAK-SUNTO-PART-LOC   PIC S9(11)V9(2)     COMP-3.                  
004900*                                 TOTAL SALES AMOUNT PARTS EXCL.          
005000*                                 VAT                                     
005100*                                 NET TOT FOR PART DNI                    
005200*                                                                         
005300     03 FAK-SUBTO-TOT-PART-LOC                                            
005400                             PIC S9(11)V9(2)     COMP-3.                  
005500*                                 TOTAL SALES AMOUNT INCL. VAT            
005600*                                 NET TOT DNI AMOUNT INCL. VAT            
005700     03 FAK-SUNTO-TOT-LOC    PIC S9(11)V9(2)     COMP-3.                  
005800*                                 TOTAL SALES AMOUNT EXCL. VAT            
005900*                                 NET TOT DNI                             
006000     03 FAK-SUVAT-BILLIT-TOT-LOC                                          
006100                             PIC S9(11)V9(2)     COMP-3.                  
006200*                                 SUMMERAT MOMSVÄRDE LOC                  
006300*                                 NET TOT VAT DNI                         
006400*                                 TOTAL VAT VALUE                         
006500     03 FAK-SUBTO-TOT-LOC    PIC S9(11)V9(2)     COMP-3.                  
006600*                                 TOTAL SALES AMOUNT INCL. VAT            
006700*                                 BRUTTO TOT DNI                          
006800     03 FAK-KDVALISO-LOC     PIC X(3).                                    
006900*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
007000*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
007100*                                 CURRENCY CODE BY ISO-STANDARD.          
007200     03 FAK-PRKURS-LOC       PIC S9(6)V9(5)      COMP-3.                  
007300*                                 VALUTAKURS                              
007400*                                 CURRENCY EXCHANGE RATE                  
007500*                                 CURRENCY RATE DNI                       
007600     03 FAK-KDTECKEN         PIC X.                                       
007700*                                 PLUS ELLER MINUS (+ -)                  
007800*                                 PLUS OR MINUS                           
007900     03 FAK-SUFKTTILL        PIC S9(7)V9(2)      COMP-3.                  
008000*                                 PRISTILLÄGG (KR)                        
008100*                                 ADDITIONAL COSTS (SEK)                  
008200     03 FAK-SUNTO-LOCC       PIC S9(11)V9(2)     COMP-3.                  
008300*                                 TOTAL SALES AMOUNT EXCL. VAT            
008400*                                 CURRENCY CONVERSION AMOUNT DNI          
008500     03 FAK-SUNTO-PART-RECALC                                             
008600                             PIC S9(11)V9(2)     COMP-3.                  
008700*                                 TOTAL SALES AMOUNT PARTS EXCL.          
008800*                                 VAT                                     
008900*                                 NET TOT FOR PART RECALCULATION          
009000*                                 VAT                                     
009100     03 FAK-SUBTO-TOT-PART-RECALC                                         
009200                             PIC S9(11)V9(2)     COMP-3.                  
009300*                                 TOTAL SALES AMOUNT INCL. VAT            
009400*                                  BRUTTO TOT FOR PART RECALCULAT         
009500*                                 ION                                     
009600     03 FAK-SUNTO-TOT-RECALC PIC S9(11)V9(2)     COMP-3.                  
009700*                                 TOTAL SALES AMOUNT EXCL. VAT            
009800*                                 NET TOT RECALCULATION EXCL.VAT          
009900     03 FAK-SUBTO-TOT-RECALC PIC S9(11)V9(2)     COMP-3.                  
010000*                                 TOTAL SALES AMOUNT INCL. VAT            
010100*                                 BRUTTO TOT RECALCULATION                
010200     03 FAK-KDVALISO-RECALC  PIC X(3).                                    
010300*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
010400*                                 CURRENCY CODE BY ISO-STANDARD.          
010500     03 FAK-PRKURS-RECALC    PIC S9(6)V9(5)      COMP-3.                  
010600*                                 VALUTAKURS                              
010700*                                 CURRENCY EXCHANGE RATE                  
010800*                                 CURRENCY RATE RECALCULATION             
010900     03 FAK-KDTECKEN-RECALC  PIC X.                                       
011000*                                 PLUS ELLER MINUS (+ -)                  
011100*                                 PLUS OR MINUS                           
011200*                                 CURRENCY CONVERSION SIGN RECALC         
011300*                                 ULATION                                 
011400     03 FAK-SUNTO-LOCC-RECALC                                             
011500                             PIC S9(11)V9(2)     COMP-3.                  
011600*                                 TOTAL SALES AMOUNT EXCL. VAT            
011700*                                 CURRENCY CONVERSION AMOUNT RECA         
011800*                                 LCULATION                               
011900*** END OF VILMAII-COPY LENGTH= 172 BYTES                                 
