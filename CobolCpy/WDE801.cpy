000100 01  PHUV-WDE801.                                                         
000200*                                 PROFORMAREGISTER                        
000300*                                 PROFORMA HUVUD                          
000400*                                 FYSISK NYCKEL: WDE801KY                 
000500*                                 (IDGMTREF)                              
000600     03 PHUV-IDGMTREF.                                                    
000700*                                 GODSMOTTAGAREREFERENS                   
000800*                                 GOODS RECEIVER REFERENS                 
000900        05 PHUV-IDDISTR      PIC S9(5)           COMP-3.                  
001000*                                 DISTRIKTNUMMER                          
001100*                                 DISTRICT NUMBER                         
001200        05 PHUV-IDKUNDNR     PIC S9(7)           COMP-3.                  
001300*                                 KUNDNUMMER                              
001400*                                 CUSTOMER NO                             
001500        05 PHUV-IDKUNDRF-GRP.                                             
001600*                                 KUNDENS REFERENS (ORDERID)              
001700*                                 CUSTOMER REFERENCE (ORDER ID)           
001800           07 PHUV-IDKUNDRF  PIC X(10).                                   
001900*                                 KUNDENS REFERENS (ORDERID)              
002000*                                 CUSTOMER REFERENCE (ORDER ID)           
002100           07 PHUV-IDORDNR5-FILLER REDEFINES PHUV-IDKUNDRF.               
002200              09 PHUV-IDORDNR5                                            
002300                             PIC 9(5).                                    
002400*                                 ORDERNUMMER                             
002500*                                 ORDER NUMBER                            
002600              09 FILLER      PIC X(5).                                    
002700           07 PHUV-IDORDNR7-FILLER REDEFINES PHUV-IDKUNDRF.               
002800              09 PHUV-IDORDNR7                                            
002900                             PIC 9(7).                                    
003000*                                 ORDERNUMMER                             
003100*                                 ORDER NUMBER                            
003200              09 FILLER      PIC X(3).                                    
003300     03 PHUV-ADBET.                                                       
003400*                                 BETALNINGSANSVARIG ADRESS               
003500*                                 ADDRESS OF PAYER                        
003600        05 PHUV-ADBETRAD-1   PIC X(35).                                   
003700*                                 ADRESSRAD BETALNINGSANSVARIG            
003800*                                 PART OF FINANCIAL CUSTOMER ADDR         
003900*                                 ESS                                     
004000        05 PHUV-ADBETRAD-2   PIC X(35).                                   
004100*                                 ADRESSRAD BETALNINGSANSVARIG            
004200*                                 PART OF FINANCIAL CUSTOMER ADDR         
004300*                                 ESS                                     
004400        05 PHUV-ADBETRAD-3   PIC X(35).                                   
004500*                                 ADRESSRAD BETALNINGSANSVARIG            
004600*                                 PART OF FINANCIAL CUSTOMER ADDR         
004700*                                 ESS                                     
004800     03 PHUV-ADGMT.                                                       
004900*                                 GODSMOTTAGARADRESS                      
005000*                                 GOODS RECEIVER ADDRESS                  
005100        05 PHUV-ADGMT-GATA   PIC X(35).                                   
005200*                                 GODSMOTTAGARADRESS GATA                 
005300*                                 GOODS RECEIVER ADDRESS STREET           
005400        05 PHUV-ADGMT-PADR   PIC X(35).                                   
005500*                                 GODSMOTTAGARADRESS POSTADRESS           
005600*                                 GOODS RECEIVER ADDRESS TOWN             
005700        05 PHUV-ADPOST-PNRORT REDEFINES PHUV-ADGMT-PADR.                  
005800*                                 POSTNUMMER + ORT                        
005900*                                 POSTAL CODE + CITY                      
006000           07 PHUV-ADPOSTNR  PIC X(10).                                   
006100*                                 POSTNUMMER I ADRESS                     
006200*                                 POSTAL CODE IN ADDRESS                  
006300           07 PHUV-ADCITY    PIC X(25).                                   
006400*                                 BENÄMNING PÅ STAD                       
006500*                                 CITY                                    
006600        05 PHUV-ADPOST-ORTPNR REDEFINES PHUV-ADGMT-PADR.                  
006700*                                 ORT + POSTNUMMER                        
006800*                                 CITY + POSTAL CODE                      
006900           07 PHUV-ADCITY    PIC X(25).                                   
007000*                                 BENÄMNING PÅ STAD                       
007100*                                 CITY                                    
007200           07 PHUV-ADPOSTNR  PIC X(10).                                   
007300*                                 POSTNUMMER I ADRESS                     
007400*                                 POSTAL CODE IN ADDRESS                  
007500        05 PHUV-ADGMT-LAND   PIC X(35).                                   
007600*                                 GODSMOTTAGARADRESS LAND                 
007700*                                 GOODS RECEIVER ADDRESS COUNTRY          
007800     03 PHUV-BEBET.                                                       
007900*                                 BETALNINGSANSVARIG NAMN                 
008000*                                 NAME OF PAYER                           
008100        05 PHUV-BEBETRAD-1   PIC X(35).                                   
008200*                                 DEL AV BETALNINGSANSVARIGS NAMN         
008300*                                 PART OF FINANCIAL CUSTOMER NAME         
008400        05 PHUV-BEBETRAD-2   PIC X(35).                                   
008500*                                 DEL AV BETALNINGSANSVARIGS NAMN         
008600*                                 PART OF FINANCIAL CUSTOMER NAME         
008700     03 PHUV-BEGMT.                                                       
008800*                                 GODSMOTTAGARNAMN                        
008900*                                 GOODS RECEIVER NAME                     
009000        05 PHUV-BEGMT-RAD1   PIC X(35).                                   
009100*                                 GODSMOTTAGARNAMN RAD 1                  
009200*                                 GOODS RECEIVER NAME LINE 1              
009300        05 PHUV-BEGMT-RAD2   PIC X(35).                                   
009400*                                 GODSMOTTAGARNAMN RAD 2                  
009500*                                 GOODS RECEIVER NAME LINE 2              
009600     03 PHUV-BEKUNDRF        PIC X(15).                                   
009700*                                 KUNDENS REFERENS                        
009800*                                 CUSTOMERS REFERENCE                     
009900     03 PHUV-BELOSORT        PIC X(17).                                   
010000*                                 LOSSNININGSORT NAMN                     
010100*                                 UNLOADING PLACE NAME                    
010200     03 PHUV-BEVARREF        PIC X(10).                                   
010300*                                 VÅR REFERENS                            
010400*                                 OUR REFERENCE                           
010500     03 PHUV-FLBORT          PIC X.                                       
010600*                                 BORTTAGNINGSFLAGGA                      
010700     03 PHUV-FLRESTN         PIC X.                                       
010800*                                 RESTNOTERING ?                          
010900*                                 BACKORDERED ?                           
011000     03 PHUV-IDANALYS        PIC X(12).                                   
011100*                                 ANALYSNUMMER                            
011200*                                 ANALYSIS NUMBER                         
011300     03 PHUV-IDKONTO         PIC S9(11)          COMP-3.                  
011400*                                 KONTO                                   
011500*                                 ACCOUNT                                 
011600     03 PHUV-IDKST           PIC X(10).                                   
011700*                                 KOSTNADSSTÄLLE                          
011800*                                 COST CENTRE                             
011900     03 PHUV-IDORDER         PIC S9(7)           COMP-3.                  
012000*                                 VOLVO PARTS ORDERNUMMER                 
012100*                                 VOLVO PARTS ORDER NUMBER                
012200     03 PHUV-IDSKYLT         PIC X(3).                                    
012300*                                 NATIONALITETSTECKEN                     
012400*                                 SPRÅKIDENTIFIKATION                     
012500*                                 NATIONALITY SIGN                        
012600*                                 LANGUAGE IDENTIFIER                     
012700     03 PHUV-IDSYSTEM        PIC X(4).                                    
012800*                                 VOLVO VCCS SYSTEMNUMMER                 
012900*                                 VOLVO VCCS SYSTEM NUMBER                
013000     03 PHUV-IDUSER          PIC X(8).                                    
013100*                                 ANVÄNDARENS SÄKERHETS ID                
013200*                                 USER SECURITY-IDENTITY                  
013300     03 PHUV-IDFTG           PIC 9(2).                                    
013400*                                 FÖRETAGSID EKONOM REDOVISNING           
013500*                                 COMPANY IDENTITY ACCOUNTING             
013600     03 PHUV-IDKUNDRF-ING    OCCURS 10 TIMES                              
013700                             PIC X(10).                                   
013800*                                 KUNDENS REFERENS (ORDERID)              
013900*                                 CUSTOMER REFERENCE (ORDER ID)           
014000     03 PHUV-KDLEVVIL        PIC S9              COMP-3.                  
014100*                                 LEVERANSVILLKOR                         
014200*                                 TERMS OF DELIVERY                       
014300     03 PHUV-KDFAKTYP        PIC X.                                       
014400*                                 FAKTURATYP                              
014500*                                 INVOICE TYPE                            
014600     03 PHUV-KDFRAKT         PIC S9(3)           COMP-3.                  
014700*                                 FRAKTSÄTT DC TILL KUND                  
014800*                                 FREIGHT CODE                            
014900     03 PHUV-KDMOMSIN        PIC S9              COMP-3.                  
015000*                                 MOMSINSTRUKTION                         
015100*                                 TVA-INSTRUCTION                         
015200     03 PHUV-KDORDING        PIC S9              COMP-3.                  
015300*                                 UPPDATERING ORDERINGÅNG                 
015400*                                 ORDER STATISTICS                        
015500     03 PHUV-KDORDKL         PIC S9              COMP-3.                  
015600*                                 ORDERKLASS                              
015700*                                 ORDER CLASS                             
015800     03 PHUV-KDPROTYP        PIC X.                                       
015900*                                 TYP AV PROFORMA                         
016000*                                 TYPE OF PRO FORMA                       
016100     03 PHUV-KDVALUTA        PIC S9(3)           COMP-3.                  
016200*                                 VALUTAKOD                               
016300*                                 CURRENCY CODE                           
016400     03 PHUV-KVKAROSS        PIC S9(3)           COMP-3.                  
016500*                                 ANTAL RADER MED KAROSSARTIKLAR          
016600*                                 QUANTITY LINES COACHWORK ARTICL         
016700*                                 ES                                      
016800     03 PHUV-KVKVBRYT        PIC S9(3)           COMP-3.                  
016900*                                 ANTAL BRUTNA KVANTER                    
017000*                                 NO OF BREAK-BULKS                       
017100     03 PHUV-KVMOTOR         PIC S9(3)           COMP-3.                  
017200*                                 ANTAL RADER MOTORARTIKLAR               
017300*                                 QTY LINES WITH ENGINE ARTICLES          
017400     03 PHUV-PRAVDRAG        PIC S9(7)V9(2)      COMP-3.                  
017500*                                 AVDRAGSBELOPP                           
017600*                                 DEDUCTION                               
017700     03 PHUV-PREMBHNT        PIC S9(7)V9(2)      COMP-3.                  
017800*                                 EMBALLAGE O HANTERINGSKOST              
017900*                                 PACKING O HANDL COSTS                   
018000     03 PHUV-PRFOERS         PIC S9(7)V9(2)      COMP-3.                  
018100*                                 FÖRSÄKRINGSPREMIE                       
018200*                                 INSURANCE FEE                           
018300     03 PHUV-PRFRAKT         PIC S9(7)V9(2)      COMP-3.                  
018400*                                 FRAKTKOSTNAD                            
018500*                                 FREIGHT COST                            
018600     03 PHUV-PRKURS          PIC S9(6)V9(5)      COMP-3.                  
018700*                                 VALUTAKURS                              
018800*                                 CURRENCY EXCHANGE RATE                  
018900     03 PHUV-PRLEGKST        PIC S9(7)V9(2)      COMP-3.                  
019000*                                 LEGALISERINSKOSTNAD                     
019100*                                 LEGALIZATION FEE                        
019200     03 PHUV-REAVDRAG        PIC S9(2)V9(1)      COMP-3.                  
019300*                                 AVDRAGSPROCENT                          
019400*                                 DEDUCTION PERCENT                       
019500     03 PHUV-REEMBHNT        PIC S9(2)V9(1)      COMP-3.                  
019600*                                 EMB OCH HANTERINGSKOST (%)              
019700*                                 PACKING AND HANDLING (%)                
019800     03 PHUV-REFOERS         PIC S9(2)V9(3)      COMP-3.                  
019900*                                 FÖRSÄKRINGKOSTNADSFAKTOR                
020000*                                 INSURANCE COSTS                         
020100     03 PHUV-REOMRTAL        PIC S9(2)V9(3)      COMP-3.                  
020200*                                 OMRÄKNINGSTAL                           
020300*                                 CONVERSION FACTOR                       
020400     03 PHUV-REOVKOFF        PIC S9(2)V9(1)      COMP-3.                  
020500*                                 ÖVERFÖRSÄKRINGSKOEFFICIENT              
020600*                                 OVER INSURANCE COEFFICIENT              
020700     03 PHUV-SUORDV          PIC S9(9)V9(2)      COMP-3.                  
020800*                                 SUMMA ORDERVÄRDE                        
020900*                                 TOTAL ORDER VALUE                       
021000     03 PHUV-TEBANK          OCCURS 2 TIMES                               
021100                             PIC X(72).                                   
021200*                                 BANKINFORMATION                         
021300*                                 BANKINGINFORMATION                      
021400     03 PHUV-TEBANKTO        PIC X(20).                                   
021500*                                 BANKKONTO                               
021600*                                 BANKACCOUNT                             
021700     03 PHUV-TEBETVIL        OCCURS 4 TIMES                               
021800                             PIC X(72).                                   
021900*                                 BETALNINGSVILLKOR                       
022000*                                 PAYMENTCONDITIONS                       
022100     03 PHUV-TEGILTIG        OCCURS 4 TIMES                               
022200                             PIC X(72).                                   
022300*                                 GILTIGHETSVILLKOR                       
022400*                                 TERMS OF VALIDITY                       
022500     03 PHUV-TEFRITT         OCCURS 4 TIMES                               
022600                             PIC X(72).                                   
022700*                                 ÖVRIGA VILLKOR                          
022800*                                 OTHER TERMS                             
022900     03 PHUV-TELEVVIL        OCCURS 3 TIMES                               
023000                             PIC X(72).                                   
023100*                                 LEVERANSVILLKOR                         
023200*                                 DELIVERYCONDITIONS                      
023300     03 PHUV-TEPACK          PIC X(72).                                   
023400*                                 PACKNINGSVILLKOR                        
023500*                                 PACKINGCONDITIONS                       
023600     03 PHUV-TIFORDAT        PIC S9(7)           COMP-3.                  
023700*                                 FÖRFALLODATUM                           
023800*                                                                         
023900     03 PHUV-TIGILTIG        PIC S9(7)           COMP-3.                  
024000*                                 GILTIGHETSDATUM (ÅÅMMDD)                
024100*                                 DATE OF VALIDITY (YYMMDD)               
024200     03 PHUV-TIORDDAT        PIC S9(7)           COMP-3.                  
024300*                                 ORDERDATUM                              
024400     03 PHUV-TIREGDAT        PIC S9(7)           COMP-3.                  
024500*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
024600*                                 REGISTRATION DATE (YYMMDD)              
024700     03 PHUV-TIREGTID        PIC S9(7)           COMP-3.                  
024800*                                 REGISTRERINGSTID                        
024900*                                 GENERAL REGISTRATION TIME               
025000     03 PHUV-TIUPPDAT        PIC S9(7)           COMP-3.                  
025100*                                 UPPDATERINGSDATUM  (ÅÅMMDD)             
025200*                                 UPDATING DATE     (YYMMDD)              
025300     03 PHUV-TIUPPTID        PIC S9(9)           COMP-3.                  
025400*                                 UPPDATERINGSTID  (TTMMSSTH)             
025500*                                 UPDATING TIME    (HHMMSSTH)             
025600     03 PHUV-VKORDBTO        PIC S9(6)V9(1)      COMP-3.                  
025700*                                 ORDERVIKT BRUTTO (KG)                   
025800*                                 GROSS WEIGHT (KG)                       
025900     03 PHUV-VKORDNTO        PIC S9(6)V9(1)      COMP-3.                  
026000*                                 ORDERVIKT NETTO (KG)                    
026100*                                 WEIGHT PER ORDER NETTO (KG)             
026200     03 PHUV-VLORDBTO        PIC S9(4)V9(3)      COMP-3.                  
026300*                                 ORDERVOLYM BRUTTO (M3)                  
026400*                                 GROSS VOLUME PER ORDER (M3)             
026500     03 PHUV-DEAL-PR-SUM.                                                 
026600*                                 DEALERPRIS (HUVUD)                      
026700        05 PHUV-SUORDV-LOC   PIC S9(9)V9(2)      COMP-3.                  
026800*                                 ORDERVÄRDE SLUTKUNDPRIS                 
026900*                                 I LOKAL VALUTA                          
027000*                                 ORDER VALUE, CUSTOMER PRICE             
027100*                                 IN LOCAL CURRENCY                       
027200        05 PHUV-SUORDV-LOCPREL                                            
027300                             PIC S9(9)V9(2)      COMP-3.                  
027400*                                 ORDERVÄRDE PREL SLUT-                   
027500*                                 KUNDPRIS, LOKAL VALUTA                  
027600*                                 ORDER VALUE, PREL CUSTOMER              
027700*                                 PRICE IN LOCAL CURRENCY                 
027800        05 PHUV-KDVALISO     PIC X(3).                                    
027900*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
028000*                                 CURRENCY CODE BY ISO-STANDARD.          
028100*** END OF VILMAII-COPY LENGTH= 1997 BYTES                                
