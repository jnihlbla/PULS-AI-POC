000100 01  VOR-W4406B.                                                          
000200*                                 EXTRACT OF VOR BACKORDER REGIST         
000300*                                 ER                                      
000400     03 VOR-IDDISTR          PIC S9(5)           COMP-3.                  
000500*                                 DISTRIKTNUMMER                          
000600*                                 DISTRICT NUMBER                         
000700     03 VOR-IDKUNDNR         PIC S9(7)           COMP-3.                  
000800*                                 KUNDNUMMER                              
000900*                                 CUSTOMER NO                             
001000     03 VOR-IDKUNDRF         PIC X(10).                                   
001100*                                 KUNDENS REFERENS (ORDERID)              
001200*                                 CUSTOMER REFERENCE (ORDER ID)           
001300     03 VOR-TIREGDAT-URSP    PIC S9(7)           COMP-3.                  
001400*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
001500*                                 REGISTRATION DATE (YYMMDD)              
001600     03 VOR-IDARTNR          PIC S9(9)           COMP-3.                  
001700*                                 ARTIKELNUMMER                           
001800*                                 PART NUMBER                             
001900     03 VOR-TIREGTID-URSP    PIC S9(9)           COMP-3.                  
002000*                                 KLOCKSLAG (TTMMSSTH)                    
002100*                                 TIME OF DAY (HHMMSSTH)                  
002200     03 VOR-TIREGDAT-AVV     PIC S9(7)           COMP-3.                  
002300*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
002400*                                 REGISTRATION DATE (YYMMDD)              
002500     03 VOR-TIREGTID-AVV     PIC S9(9)           COMP-3.                  
002600*                                 KLOCKSLAG (TTMMSSTH)                    
002700*                                 TIME OF DAY (HHMMSSTH)                  
002800     03 VOR-IDKUNDRF-LEV     PIC X(10).                                   
002900*                                 KUND REF PÅ LEVERANSORDERN              
003000*                                 CUST REF ON DELIVERY ORDER              
003100     03 VOR-TIREGDAT-LEV     PIC S9(7)           COMP-3.                  
003200*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
003300*                                 REGISTRATION DATE (YYMMDD)              
003400     03 VOR-TIREGTID-LEV     PIC S9(9)           COMP-3.                  
003500*                                 KLOCKSLAG (TTMMSSTH)                    
003600*                                 TIME OF DAY (HHMMSSTH)                  
003700     03 VOR-IDROLL           PIC X(5).                                    
003800*                                 VOR ROLL ID                             
003900*                                 VOR ROLE ID                             
004000     03 VOR-IDANSK           PIC S9(3)           COMP-3.                  
004100*                                 ANSKAFFARNUMMER                         
004200*                                 PROCURER NO.                            
004300     03 VOR-IDLEVNR          PIC X(5).                                    
004400*                                 LEVERANTÖRNUMMER                        
004500*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
004600     03 VOR-BERADREF         PIC X(10).                                   
004700*                                 KUNDENS RADREFERENS                     
004800*                                 CUSTOMERS ITEM REF.                     
004900     03 VOR-KVBEART-URSP     PIC S9(7)           COMP-3.                  
005000*                                 BESTÄLLT ANTAL STYCKEN                  
005100*                                 ORDERED QUANTITY                        
005200     03 VOR-KVBEART          PIC S9(7)           COMP-3.                  
005300*                                 BESTÄLLT ANTAL STYCKEN                  
005400*                                 ORDERED QUANTITY                        
005500     03 VOR-KVBEART-Q        PIC S9(7)           COMP-3.                  
005600*                                 BESTÄLLT KVANTANPASSAT ANTAL            
005700*                                 ORDERED QUANTITY ADAPTED                
005800*                                  ITEMS                                  
005900     03 VOR-KVPREAVB         PIC S9(7)           COMP-3.                  
006000*                                 PREL-AVB KVANT                          
006100*                                 PREL-RES QUANT                          
006200     03 VOR-IDDC             PIC X(2).                                    
006300*                                 IDENTIFIERARE LAGER                     
006400*                                 WAREHOUSE IDENTIFIER                    
006500     03 VOR-IDUSER           PIC X(8).                                    
006600*                                 ANVÄNDARENS SÄKERHETS ID                
006700*                                 USER SECURITY-IDENTITY                  
006800     03 VOR-KDORDBEK         PIC 9(2).                                    
006900*                                 ORDERBEKRÄFTELSEKOD                     
007000*                                 ORDERCONFIMATIONCODE                    
007100     03 VOR-KDPRTYP          PIC X.                                       
007200*                                 TYP AV PRISTILLÄMPNING                  
007300*                                 TYPE OF PRICING                         
007400     03 VOR-KDVORATG         PIC X.                                       
007500*                                 TYP AV ÅTGÄRD FÖR POST VOR-KÖN          
007600*                                 TYPE OF HANDLE ON VOR-QUE               
007700     03 VOR-PRARTNTO         PIC S9(7)V9(2)      COMP-3.                  
007800*                                 ARTIKELPRIS NETTO                       
007900*                                 NET PRICE EACH   (FOB NET)              
008000     03 VOR-TEVORMRK         PIC X(2).                                    
008100*                                 MÄRKNINGSTEXT FÖR                       
008200*                                 VOR-KÖN                                 
008300     03 VOR-TIKLAR           PIC S9(7)           COMP-3.                  
008400*                                 KLARDATUM          (ÅÅMMDD)             
008500*                                 READY DATE        (YYMMDD)              
008600     03 VOR-TIKLATID         PIC S9(7)           COMP-3.                  
008700*                                 KLARTID                                 
008800*                                 GENERAL READY TIME                      
008900     03 VOR-TIUPPDAT         PIC S9(7)           COMP-3.                  
009000*                                 UPPDATERINGSDATUM  (ÅÅMMDD)             
009100*                                 UPDATING DATE     (YYMMDD)              
009200     03 VOR-TIUPPTID         PIC S9(9)           COMP-3.                  
009300*                                 UPPDATERINGSTID  (TTMMSSTH)             
009400*                                 UPDATING TIME    (HHMMSSTH)             
009500     03 VOR-DEAL-PR-LINE.                                                 
009600*                                 DEALERPRIS (RAD)                        
009700        05 VOR-IDPRQUES      PIC 9(7).                                    
009800*                                 PRISFRÅGA NR                            
009900*                                 PRICE QUESTION NO                       
010000        05 VOR-PRARTNTO-LOC  PIC S9(7)V9(2)      COMP-3.                  
010100*                                 ARTIKELPRIS NETTO LOKAL VALUTA          
010200*                                 NET PRICE EACH LOCAL CURRENCY           
010300        05 VOR-PRARTNTO-LOCPREL                                           
010400                             PIC S9(7)V9(2)      COMP-3.                  
010500*                                 PREL NETTO SLUTKUNDSPRIS I              
010600*                                 LOKAL VALUTA                            
010700*                                 PREL NET PRICE - LOCAL CURRENCY         
010800        05 VOR-PRARTBTO-LOC  PIC S9(7)V9(2)      COMP-3.                  
010900*                                 PRIS I LOKAL VALUTA                     
011000*                                 LOCAL GROSS SALES PRICE                 
011100        05 VOR-KDVALISO      PIC X(3).                                    
011200*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
011300*                                 CURRENCY CODE BY ISO-STANDARD.          
011400        05 VOR-KDVAT         PIC X(2).                                    
011500*                                 MOMSKOD                                 
011600*                                 VAT CODE                                
011700        05 VOR-RERAB         PIC S9(2)V9(1)      COMP-3.                  
011800*                                 RABATTSATS (PROCENT)                    
011900        05 VOR-KDRAB         PIC X(5).                                    
012000*                                 RABATTKOD                               
012100        05 VOR-BEART-VIPS    PIC X(25).                                   
012200*                                 VIPS ARTIKELBENÄMNING                   
012300*                                 PÅ DEALERNS SPRÅK                       
012400     03 VOR-TEVORMRK-SC      PIC X(2).                                    
012500*                                 SÄLBOLAG MÄRKNINGSTEXT VOR-KÖ           
012600*                                 MESSAGE TEXT FOR SALE COMPANY           
012700     03 VOR-FLVORFK          PIC X.                                       
012800*                                 VOR-FRAKTKOD FRÅN KLASS 1               
012900*                                 VOR-FREIGHTCODE FROM CLASS 1            
013000     03 VOR-TEVORINT         PIC X(230).                                  
013100*                                 VOR MESSAGE I                           
013200*                                 VOR MESSAGE I                           
013300     03 VOR-FLLAEST-DEL      PIC X.                                       
013400*                                 JA/NEJ-FLAGGA                           
013500     03 VOR-TEVOREXT         PIC X(310).                                  
013600*                                 VOR MESSAGE E                           
013700*                                 VOR MESSAGE E                           
013800     03 VOR-FLLAEST-SC       PIC X.                                       
013900*                                 JA/NEJ-FLAGGA                           
014000     03 VOR-TEVORSC          PIC X(310).                                  
014100*                                 VOR MESS. SC                            
014200*                                 VOR MESS. SC                            
014300     03 VOR-TEVORDEL         PIC X(310).                                  
014400*                                 VOR MESS. DEL                           
014500*                                 VOR MESS. DEL                           
014600*** END OF VILMAII-COPY LENGTH= 1359 BYTES                                
