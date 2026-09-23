000100 01  4542-WDGX4542.                                                       
000200*                                 VOR-KÖ                                  
000300*                                 FYSISK NYCKEL KY4542:                   
000400*                                 (IDDISTR, IDANSK, IDARTNR,              
000500*                                  IDLOPNR, IDORDER)                      
000600     03 4542-IDDISTR         PIC S9(5)           COMP-3.                  
000700*                                 DISTRIKTNUMMER                          
000800*                                 DISTRICT NUMBER                         
000900     03 4542-IDANSK          PIC S9(3)           COMP-3.                  
001000*                                 ANSKAFFARNUMMER                         
001100*                                 PROCURER NO.                            
001200     03 4542-IDARTNR         PIC S9(9)           COMP-3.                  
001300*                                 ARTIKELNUMMER                           
001400*                                 PART NUMBER                             
001500     03 4542-IDLOPNR         PIC S9(3)           COMP-3.                  
001600*                                 LÖPNUMMER                               
001700*                                 SEQUENCE NUMBER                         
001800     03 4542-IDORDER         PIC S9(7)           COMP-3.                  
001900*                                 VOLVO PARTS ORDERNUMMER                 
002000*                                 VOLVO PARTS ORDER NUMBER                
002100     03 4542-BERADREF        PIC X(10).                                   
002200*                                 KUNDENS RADREFERENS                     
002300*                                 CUSTOMERS ITEM REF.                     
002400     03 4542-IDDC            PIC X(2).                                    
002500*                                 IDENTIFIERARE LAGER                     
002600*                                 WAREHOUSE IDENTIFIER                    
002700     03 4542-IDKUNDNR        PIC S9(7)           COMP-3.                  
002800*                                 KUNDNUMMER                              
002900*                                 CUSTOMER NO                             
003000     03 4542-IDKUNDRF-GRP.                                                
003100*                                 KUNDENS REFERENS (ORDERID)              
003200*                                 CUSTOMER REFERENCE (ORDER ID)           
003300        05 4542-IDKUNDRF     PIC X(10).                                   
003400*                                 KUNDENS REFERENS (ORDERID)              
003500*                                 CUSTOMER REFERENCE (ORDER ID)           
003600        05 4542-IDORDNR5-FILLER REDEFINES 4542-IDKUNDRF.                  
003700           07 4542-IDORDNR5  PIC 9(5).                                    
003800*                                 ORDERNUMMER                             
003900*                                 ORDER NUMBER                            
004000           07 FILLER         PIC X(5).                                    
004100        05 4542-IDORDNR7-FILLER REDEFINES 4542-IDKUNDRF.                  
004200           07 4542-IDORDNR7  PIC 9(7).                                    
004300*                                 ORDERNUMMER                             
004400*                                 ORDER NUMBER                            
004500           07 FILLER         PIC X(3).                                    
004600     03 4542-IDLEVNR         PIC X(5).                                    
004700*                                 LEVERANTÖRNUMMER                        
004800*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
004900     03 4542-IDUSER          PIC X(8).                                    
005000*                                 ANVÄNDARENS SÄKERHETS ID                
005100*                                 USER SECURITY-IDENTITY                  
005200     03 4542-KDORDBEK        PIC 9(2).                                    
005300*                                 ORDERBEKRÄFTELSEKOD                     
005400*                                 ORDERCONFIMATIONCODE                    
005500     03 4542-KDPRTYP         PIC X.                                       
005600*                                 TYP AV PRISTILLÄMPNING                  
005700*                                 TYPE OF PRICING                         
005800     03 4542-KDVORATG        PIC X.                                       
005900*                                 TYP AV ÅTGÄRD FÖR POST VOR-KÖN          
006000*                                 TYPE OF HANDLE ON VOR-QUE               
006100     03 4542-KVBEART         PIC S9(7)           COMP-3.                  
006200*                                 BESTÄLLT ANTAL STYCKEN                  
006300*                                 ORDERED QUANTITY                        
006400     03 4542-KVBEART-Q       PIC S9(7)           COMP-3.                  
006500*                                 BESTÄLLT KVANTANPASSAT ANTAL            
006600*                                 ORDERED QUANTITY ADAPTED                
006700*                                  ITEMS                                  
006800     03 4542-KVPREAVB        PIC S9(7)           COMP-3.                  
006900*                                 PREL-AVB KVANT                          
007000*                                 PREL-RES QUANT                          
007100     03 4542-PRARTNTO        PIC S9(7)V9(2)      COMP-3.                  
007200*                                 ARTIKELPRIS NETTO                       
007300*                                 NET PRICE EACH   (FOB NET)              
007400     03 4542-TEVORMRK        PIC X(2).                                    
007500*                                 MÄRKNINGSTEXT FÖR                       
007600*                                 VOR-KÖN                                 
007700     03 4542-TIREGDAT        PIC S9(7)           COMP-3.                  
007800*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
007900*                                 REGISTRATION DATE (YYMMDD)              
008000     03 4542-TIREGTID        PIC S9(7)           COMP-3.                  
008100*                                 REGISTRERINGSTID                        
008200*                                 GENERAL REGISTRATION TIME               
008300     03 4542-TIUPPDAT        PIC S9(7)           COMP-3.                  
008400*                                 UPPDATERINGSDATUM  (ÅÅMMDD)             
008500*                                 UPDATING DATE     (YYMMDD)              
008600     03 4542-TIUPPTID        PIC S9(9)           COMP-3.                  
008700*                                 UPPDATERINGSTID  (TTMMSSTH)             
008800*                                 UPDATING TIME    (HHMMSSTH)             
008900     03 4542-DEAL-PR-LINE.                                                
009000*                                 DEALERPRIS (RAD)                        
009100        05 4542-IDPRQUES     PIC 9(7).                                    
009200*                                 PRISFRÅGA NR                            
009300*                                 PRICE QUESTION NO                       
009400        05 4542-PRARTNTO-LOC PIC S9(7)V9(2)      COMP-3.                  
009500*                                 ARTIKELPRIS NETTO LOKAL VALUTA          
009600*                                 NET PRICE EACH LOCAL CURRENCY           
009700        05 4542-PRARTNTO-LOCPREL                                          
009800                             PIC S9(7)V9(2)      COMP-3.                  
009900*                                 PREL NETTO SLUTKUNDSPRIS I              
010000*                                 LOKAL VALUTA                            
010100*                                 PREL NET PRICE - LOCAL CURRENCY         
010200        05 4542-PRARTBTO-LOC PIC S9(7)V9(2)      COMP-3.                  
010300*                                 PRIS I LOKAL VALUTA                     
010400*                                 LOCAL GROSS SALES PRICE                 
010500        05 4542-KDVALISO     PIC X(3).                                    
010600*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
010700*                                 CURRENCY CODE BY ISO-STANDARD.          
010800        05 4542-KDVAT        PIC X(2).                                    
010900*                                 MOMSKOD                                 
011000*                                 VAT CODE                                
011100        05 4542-RERAB        PIC S9(2)V9(1)      COMP-3.                  
011200*                                 RABATTSATS (PROCENT)                    
011300        05 4542-KDRAB        PIC X(5).                                    
011400*                                 RABATTKOD                               
011500        05 4542-BEART-VIPS   PIC X(25).                                   
011600*                                 VIPS ARTIKELBENÄMNING                   
011700*                                 PÅ DEALERNS SPRÅK                       
011800*** END OF VILMAII-COPY LENGTH= 154 BYTES                                 
