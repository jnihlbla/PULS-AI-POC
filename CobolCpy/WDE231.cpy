000100 01  BRAD-WDE231.                                                         
000200*                                 TRANSPORTRELEASEREGISTER                
000300*                                 FAKTURA -> BILL-IT                      
000400*                                 RADINFO                                 
000500*                                 FYSISK NYCKEL: IDPURAD                  
000600     03 BRAD-IDPURAD         PIC S9(5)           COMP-3.                  
000700*                                 RADNUMMER PÅ PACKUNDERLAG               
000800*                                 LINENO IN PACKINGDOCUMENT               
000900     03 BRAD-DEAL-PR-LINE.                                                
001000*                                 DEALERPRIS (RAD)                        
001100        05 BRAD-IDPRQUES     PIC 9(7).                                    
001200*                                 PRISFRÅGA NR                            
001300*                                 PRICE QUESTION NO                       
001400        05 BRAD-PRARTNTO-LOC PIC S9(7)V9(2)      COMP-3.                  
001500*                                 ARTIKELPRIS NETTO LOKAL VALUTA          
001600*                                 NET PRICE EACH LOCAL CURRENCY           
001700        05 BRAD-PRARTNTO-LOCPREL                                          
001800                             PIC S9(7)V9(2)      COMP-3.                  
001900*                                 PREL NETTO SLUTKUNDSPRIS I              
002000*                                 LOKAL VALUTA                            
002100*                                 PREL NET PRICE - LOCAL CURRENCY         
002200        05 BRAD-PRARTBTO-LOC PIC S9(7)V9(2)      COMP-3.                  
002300*                                 PRIS I LOKAL VALUTA                     
002400*                                 LOCAL GROSS SALES PRICE                 
002500        05 BRAD-KDVALISO     PIC X(3).                                    
002600*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
002700*                                 CURRENCY CODE BY ISO-STANDARD.          
002800        05 BRAD-KDVAT        PIC X(2).                                    
002900*                                 MOMSKOD                                 
003000*                                 VAT CODE                                
003100        05 BRAD-RERAB        PIC S9(2)V9(1)      COMP-3.                  
003200*                                 RABATTSATS (PROCENT)                    
003300        05 BRAD-KDRAB        PIC X(5).                                    
003400*                                 RABATTKOD                               
003500        05 BRAD-BEART-VIPS   PIC X(25).                                   
003600*                                 VIPS ARTIKELBENÄMNING                   
003700*                                 PÅ DEALERNS SPRÅK                       
003800     03 BRAD-IDARTNR         PIC S9(9)           COMP-3.                  
003900*                                 ARTIKELNUMMER                           
004000*                                 PART NUMBER                             
004100     03 BRAD-REKSIFFR        PIC S9              COMP-3.                  
004200*                                 KONTROLLSIFFRA                          
004300*                                 PART NO CHECK DIGIT                     
004400     03 BRAD-IDSTATNR        PIC S9(9)           COMP-3.                  
004500*                                 STATISTISKT NUMMER                      
004600*                                 1 = NORSKT                              
004700*                                 2 = ENGELSKT                            
004800*                                 3 = BELGISKT                            
004900*                                 4 = PERUANSKT                           
005000*                                 5 = SVENSKT                             
005100*                                 6 =                                     
005200*                                 STATISTICAL NO.                         
005300     03 BRAD-KDARTRAB        PIC 9(2).                                    
005400*                                 RABATTKOD (ARTIKELPRIS)                 
005500*                                 PURCHASE DISCOUNT CODE                  
005600     03 BRAD-KDARTURS        PIC X(2).                                    
005700*                                 ARTIKELURSPRUNGSKOD                     
005800*                                 COUNTRY OF ORIGIN                       
005900     03 BRAD-KDPRTYP         PIC X.                                       
006000*                                 TYP AV PRISTILLÄMPNING                  
006100*                                 TYPE OF PRICING                         
006200     03 BRAD-KDPRODSL        PIC S9(3)           COMP-3.                  
006300*                                 PRODUKTSLAG                             
006400*                                 PRODUCT GROUP                           
006500     03 BRAD-KDSOFT          PIC S9              COMP-3.                  
006600*                                 0 NORMAL ORDER                          
006700*                                 1 VCEM SOFTWARE ORDER                   
006800*                                 2 VADIS SOFTWARE ORDER                  
006900*                                 3 OTHER SOFTWARE ORDER                  
007000     03 BRAD-KVBEART         PIC S9(7)           COMP-3.                  
007100*                                 BESTÄLLT ANTAL STYCKEN                  
007200*                                 ORDERED QUANTITY                        
007300     03 BRAD-KVLEVART        PIC S9(7)           COMP-3.                  
007400*                                 LEVERERAT ANTAL STYCK                   
007500*                                 DELIVERED QUANTITY                      
007600     03 BRAD-IDANALYS        PIC X(12).                                   
007700*                                 ANALYSNUMMER                            
007800*                                 ANALYSIS NUMBER                         
007900     03 BRAD-IDKONTO         PIC S9(11)          COMP-3.                  
008000*                                 KONTO                                   
008100*                                 ACCOUNT                                 
008200     03 BRAD-IDKST           PIC X(10).                                   
008300*                                 KOSTNADSSTÄLLE                          
008400*                                 COST CENTRE                             
008500     03 BRAD-BERADREF        PIC X(10).                                   
008600*                                 KUNDENS RADREFERENS                     
008700*                                 CUSTOMERS ITEM REF.                     
008800     03 BRAD-PRARTBTO-EXP    PIC S9(7)V9(2)      COMP-3.                  
008900*                                 BRUTTOPRIS EXPORT (FOB-PRIS)            
009000*                                 GROSS-PRICE EXPORT                      
009100*                                  (FOB-GROSS)                            
009200     03 BRAD-PRARTNTO        PIC S9(7)V9(2)      COMP-3.                  
009300*                                 ARTIKELPRIS NETTO                       
009400*                                 NET PRICE EACH   (FOB NET)              
009500     03 BRAD-VKARTNTO        PIC S9(4)V9(3)      COMP-3.                  
009600*                                 ARTIKELVIKT NETTO (KG) MED EMB          
009700*                                 PART NET WEIGHT (KG) W/ PACKAGE         
009800     03 BRAD-IDLEVNR-ART     PIC X(5).                                    
009900*                                 LEVERANTÖRNR PÅ ARTIKEL                 
010000*                                 PART SUPPLIER NUMBER                    
010100     03 BRAD-VKART-NTO-KG    PIC S9(4)V9(3)      COMP-3.                  
010200*                                 ART. NETTOVIKT I KG UTAN EMB            
010300*                                 PART NET WEIGHT KG NO PACKAGING         
010400*** END OF VILMAII-COPY LENGTH= 150 BYTES                                 
