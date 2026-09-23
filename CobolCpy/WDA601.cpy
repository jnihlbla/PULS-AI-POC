000100 01  VOR-WDA601.                                                          
000200*                                 VOR-REGISTER                            
000300*                                 VOR-RDERRADER                           
000400*                                 FYSISK NYCKEL: WDA601KY                 
000500*                                 (IDDISTR + IDKUNDNR + IDKUNDRF)         
000600*                                 (TIREGDAT+ IDARTNR  + TIREGTID)         
000700*                                 (TIREGDAV+ TIREGTIV)                    
000800     03 VOR-IDDISTR          PIC S9(5)           COMP-3.                  
000900*                                 DISTRIKTNUMMER                          
001000*                                 DISTRICT NUMBER                         
001100     03 VOR-IDKUNDNR         PIC S9(7)           COMP-3.                  
001200*                                 KUNDNUMMER                              
001300*                                 CUSTOMER NO                             
001400     03 VOR-IDKUNDRF-GRP.                                                 
001500*                                 KUNDENS REFERENS (ORDERID)              
001600*                                 CUSTOMER REFERENCE (ORDER ID)           
001700        05 VOR-IDKUNDRF      PIC X(10).                                   
001800*                                 KUNDENS REFERENS (ORDERID)              
001900*                                 CUSTOMER REFERENCE (ORDER ID)           
002000        05 VOR-IDORDNR5-FILLER REDEFINES VOR-IDKUNDRF.                    
002100           07 VOR-IDORDNR5   PIC 9(5).                                    
002200*                                 ORDERNUMMER                             
002300*                                 ORDER NUMBER                            
002400           07 FILLER         PIC X(5).                                    
002500        05 VOR-IDORDNR7-FILLER REDEFINES VOR-IDKUNDRF.                    
002600           07 VOR-IDORDNR7   PIC 9(7).                                    
002700*                                 ORDERNUMMER                             
002800*                                 ORDER NUMBER                            
002900           07 FILLER         PIC X(3).                                    
003000     03 VOR-TIREGDAT-URSP    PIC S9(7)           COMP-3.                  
003100*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
003200*                                 REGISTRATION DATE (YYMMDD)              
003300     03 VOR-IDARTNR          PIC S9(9)           COMP-3.                  
003400*                                 ARTIKELNUMMER                           
003500*                                 PART NUMBER                             
003600     03 VOR-TIREGTID-URSP    PIC S9(9)           COMP-3.                  
003700*                                 KLOCKSLAG (TTMMSSTH)                    
003800*                                 TIME OF DAY (HHMMSSTH)                  
003900     03 VOR-TIREGDAT-AVV     PIC S9(7)           COMP-3.                  
004000*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
004100*                                 REGISTRATION DATE (YYMMDD)              
004200     03 VOR-TIREGTID-AVV     PIC S9(9)           COMP-3.                  
004300*                                 KLOCKSLAG (TTMMSSTH)                    
004400*                                 TIME OF DAY (HHMMSSTH)                  
004500     03 VOR-TIREGDAT-AVV9    PIC S9(7)           COMP-3.                  
004600*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
004700*                                 REGISTRATION DATE (YYMMDD)              
004800     03 VOR-TIREGTID-AVV9    PIC S9(9)           COMP-3.                  
004900*                                 KLOCKSLAG (TTMMSSTH)                    
005000*                                 TIME OF DAY (HHMMSSTH)                  
005100     03 VOR-IDKUNDRF-LEV-GRP.                                             
005200*                                 KUNDENS REF (ORDERID) LEVERANS          
005300*                                 CUSTOMER REF (ORDER ID) DELIV.          
005400        05 VOR-IDKUNDRF-LEV  PIC X(10).                                   
005500*                                 KUND REF PÅ LEVERANSORDERN              
005600*                                 CUST REF ON DELIVERY ORDER              
005700        05 VOR-IDORDNR5-LEV-FILLER REDEFINES VOR-IDKUNDRF-LEV.            
005800           07 VOR-IDORDNR5-LEV                                            
005900                             PIC 9(5).                                    
006000*                                 ORDERNUMMER                             
006100*                                 ORDER NUMBER                            
006200           07 FILLER         PIC X(5).                                    
006300        05 VOR-IDORDNR7-LEV-FILLER REDEFINES VOR-IDKUNDRF-LEV.            
006400           07 VOR-IDORDNR7-LEV                                            
006500                             PIC 9(7).                                    
006600*                                 LEVERANSORDERNUMMER                     
006700           07 FILLER         PIC X(3).                                    
006800     03 VOR-TIREGDAT-LEV     PIC S9(7)           COMP-3.                  
006900*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
007000*                                 REGISTRATION DATE (YYMMDD)              
007100     03 VOR-TIREGTID-LEV     PIC S9(9)           COMP-3.                  
007200*                                 KLOCKSLAG (TTMMSSTH)                    
007300*                                 TIME OF DAY (HHMMSSTH)                  
007400     03 VOR-IDROLL           PIC X(5).                                    
007500*                                 VOR ROLL ID                             
007600*                                 VOR ROLE ID                             
007700     03 VOR-IDANSK           PIC S9(3)           COMP-3.                  
007800*                                 ANSKAFFARNUMMER                         
007900*                                 PROCURER NO.                            
008000     03 VOR-IDLEVNR          PIC X(5).                                    
008100*                                 LEVERANTÖRNUMMER                        
008200*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
008300     03 VOR-BERADREF         PIC X(10).                                   
008400*                                 KUNDENS RADREFERENS                     
008500*                                 CUSTOMERS ITEM REF.                     
008600     03 VOR-KVBEART-URSP     PIC S9(7)           COMP-3.                  
008700*                                 BESTÄLLT ANTAL STYCKEN                  
008800*                                 ORDERED QUANTITY                        
008900     03 VOR-KVBEART          PIC S9(7)           COMP-3.                  
009000*                                 BESTÄLLT ANTAL STYCKEN                  
009100*                                 ORDERED QUANTITY                        
009200     03 VOR-KVBEART-Q        PIC S9(7)           COMP-3.                  
009300*                                 BESTÄLLT KVANTANPASSAT ANTAL            
009400*                                 ORDERED QUANTITY ADAPTED                
009500*                                  ITEMS                                  
009600     03 VOR-KVPREAVB         PIC S9(7)           COMP-3.                  
009700*                                 PREL-AVB KVANT                          
009800*                                 PREL-RES QUANT                          
009900     03 VOR-IDDC             PIC X(2).                                    
010000*                                 IDENTIFIERARE LAGER                     
010100*                                 WAREHOUSE IDENTIFIER                    
010200     03 VOR-IDUSER           PIC X(8).                                    
010300*                                 ANVÄNDARENS SÄKERHETS ID                
010400*                                 USER SECURITY-IDENTITY                  
010500     03 VOR-KDORDBEK         PIC 9(2).                                    
010600*                                 ORDERBEKRÄFTELSEKOD                     
010700*                                 ORDERCONFIMATIONCODE                    
010800     03 VOR-KDPRTYP          PIC X.                                       
010900*                                 TYP AV PRISTILLÄMPNING                  
011000*                                 TYPE OF PRICING                         
011100     03 VOR-KDVORATG         PIC X.                                       
011200*                                 TYP AV ÅTGÄRD FÖR POST VOR-KÖN          
011300*                                 TYPE OF HANDLE ON VOR-QUE               
011400     03 VOR-PRARTNTO         PIC S9(7)V9(2)      COMP-3.                  
011500*                                 ARTIKELPRIS NETTO                       
011600*                                 NET PRICE EACH   (FOB NET)              
011700     03 VOR-TEVORMRK         PIC X(2).                                    
011800*                                 MÄRKNINGSTEXT FÖR                       
011900*                                 VOR-KÖN                                 
012000     03 VOR-TIKLAR           PIC S9(7)           COMP-3.                  
012100*                                 KLARDATUM          (ÅÅMMDD)             
012200*                                 READY DATE        (YYMMDD)              
012300     03 VOR-TIKLATID         PIC S9(7)           COMP-3.                  
012400*                                 KLARTID                                 
012500*                                 GENERAL READY TIME                      
012600     03 VOR-TIUPPDAT         PIC S9(7)           COMP-3.                  
012700*                                 UPPDATERINGSDATUM  (ÅÅMMDD)             
012800*                                 UPDATING DATE     (YYMMDD)              
012900     03 VOR-TIUPPTID         PIC S9(9)           COMP-3.                  
013000*                                 UPPDATERINGSTID  (TTMMSSTH)             
013100*                                 UPDATING TIME    (HHMMSSTH)             
013200     03 VOR-DEAL-PR-LINE.                                                 
013300*                                 DEALERPRIS (RAD)                        
013400        05 VOR-IDPRQUES      PIC 9(7).                                    
013500*                                 PRISFRÅGA NR                            
013600*                                 PRICE QUESTION NO                       
013700        05 VOR-PRARTNTO-LOC  PIC S9(7)V9(2)      COMP-3.                  
013800*                                 ARTIKELPRIS NETTO LOKAL VALUTA          
013900*                                 NET PRICE EACH LOCAL CURRENCY           
014000        05 VOR-PRARTNTO-LOCPREL                                           
014100                             PIC S9(7)V9(2)      COMP-3.                  
014200*                                 PREL NETTO SLUTKUNDSPRIS I              
014300*                                 LOKAL VALUTA                            
014400*                                 PREL NET PRICE - LOCAL CURRENCY         
014500        05 VOR-PRARTBTO-LOC  PIC S9(7)V9(2)      COMP-3.                  
014600*                                 PRIS I LOKAL VALUTA                     
014700*                                 LOCAL GROSS SALES PRICE                 
014800        05 VOR-KDVALISO      PIC X(3).                                    
014900*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
015000*                                 CURRENCY CODE BY ISO-STANDARD.          
015100        05 VOR-KDVAT         PIC X(2).                                    
015200*                                 MOMSKOD                                 
015300*                                 VAT CODE                                
015400        05 VOR-RERAB         PIC S9(2)V9(1)      COMP-3.                  
015500*                                 RABATTSATS (PROCENT)                    
015600        05 VOR-KDRAB         PIC X(5).                                    
015700*                                 RABATTKOD                               
015800        05 VOR-BEART-VIPS    PIC X(25).                                   
015900*                                 VIPS ARTIKELBENÄMNING                   
016000*                                 PÅ DEALERNS SPRÅK                       
016100     03 VOR-TEVORMRK-SC      PIC X(2).                                    
016200*                                 SÄLBOLAG MÄRKNINGSTEXT VOR-KÖ           
016300*                                 MESSAGE TEXT FOR SALE COMPANY           
016400     03 VOR-FLVORFK          PIC X.                                       
016500*                                 VOR-FRAKTKOD FRÅN KLASS 1               
016600*                                 VOR-FREIGHTCODE FROM CLASS 1            
016700     03 VOR-FILLER           PIC X(19).                                   
016800*** END OF VILMAII-COPY LENGTH= 225 BYTES                                 
