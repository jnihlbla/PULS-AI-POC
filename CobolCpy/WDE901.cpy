000100 01  PRAD-WDE901.                                                         
000200*                                 PROFORMAREGISTER                        
000300*                                 PROFORMA RADER                          
000400*                                 FYSISK NYCKEL: WDE901KY                 
000500*                                 (IDORDER  + IDARTNR +                   
000600*                                  IDLOPNR)                               
000700     03 PRAD-IDORDER         PIC S9(7)           COMP-3.                  
000800*                                 VOLVO PARTS ORDERNUMMER                 
000900*                                 VOLVO PARTS ORDER NUMBER                
001000     03 PRAD-IDARTNR         PIC S9(9)           COMP-3.                  
001100*                                 ARTIKELNUMMER                           
001200*                                 PART NUMBER                             
001300     03 PRAD-IDLOPNR         PIC S9(3)           COMP-3.                  
001400*                                 LÖPNUMMER                               
001500*                                 SEQUENCE NUMBER                         
001600     03 PRAD-BEART           PIC X(25).                                   
001700*                                 ARTIKELBENÄMNING                        
001800*                                 PART DESCRIPTION                        
001900     03 PRAD-BERADREF        PIC X(10).                                   
002000*                                 KUNDENS RADREFERENS                     
002100*                                 CUSTOMERS ITEM REF.                     
002200     03 PRAD-FLINVEST        PIC X.                                       
002300*                                 BYTES INVENTERINGSFLAGGA                
002400*                                 EXCHANGE INVESTMENT FLAG                
002500     03 PRAD-FLPRTILL        PIC X.                                       
002600*                                 PRISTILLÄGGS FLAGGA                     
002700*                                 PRICE PENALTY FLAG                      
002800     03 PRAD-FLRESTN         PIC X.                                       
002900*                                 RESTNOTERING ?                          
003000*                                 BACKORDERED ?                           
003100     03 PRAD-IDLEVNR         PIC X(5).                                    
003200*                                 LEVERANTÖRNUMMER                        
003300*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
003400     03 PRAD-IDSPECEMB       PIC 9(4).                                    
003500*                                 SPECIALEMBALLAGEID                      
003600*                                 SPECIAL PACKING ID                      
003700     03 PRAD-IDSYSTEM        PIC X(4).                                    
003800*                                 VOLVO VCCS SYSTEMNUMMER                 
003900*                                 VOLVO VCCS SYSTEM NUMBER                
004000     03 PRAD-KDARTURS        PIC X(2).                                    
004100*                                 ARTIKELURSPRUNGSKOD                     
004200*                                 COUNTRY OF ORIGIN                       
004300     03 PRAD-KDKVBRYT        PIC S9              COMP-3.                  
004400*                                 KOD OM KVANTFÖRP SKALL BRYTAS           
004500*                                 BREAK BULKPACK CODE                     
004600     03 PRAD-KDORDING        PIC S9              COMP-3.                  
004700*                                 UPPDATERING ORDERINGÅNG                 
004800*                                 ORDER STATISTICS                        
004900     03 PRAD-KDPRODSL        PIC S9(3)           COMP-3.                  
005000*                                 PRODUKTSLAG                             
005100*                                 PRODUCT GROUP                           
005200     03 PRAD-KDPRTYP         PIC X.                                       
005300*                                 TYP AV PRISTILLÄMPNING                  
005400*                                 TYPE OF PRICING                         
005500     03 PRAD-KDSPEEMB        PIC 9.                                       
005600*                                 SPECIALEMBALLAGEKOD                     
005700*                                 SPECIAL PACKING CODE                    
005800     03 PRAD-KDTPOTYP        PIC S9              COMP-3.                  
005900*                                 TYP AV TIDPLANERAD ORDER                
006000*                                 TYPE OF TIME PLANNED ORDER              
006100     03 PRAD-KVBEART         PIC S9(7)           COMP-3.                  
006200*                                 BESTÄLLT ANTAL STYCKEN                  
006300*                                 ORDERED QUANTITY                        
006400     03 PRAD-KVBEART-Q       PIC S9(7)           COMP-3.                  
006500*                                 BESTÄLLT KVANTANPASSAT ANTAL            
006600*                                 ORDERED QUANTITY ADAPTED                
006700*                                  ITEMS                                  
006800     03 PRAD-KVVECKOR-TPO5   PIC S9(3)           COMP-3.                  
006900*                                 ANTAL VECKOR FÖR TPO5                   
007000*                                 AMOUNT WEEKS FOR TPO5                   
007100     03 PRAD-PRARTNTO        PIC S9(7)V9(2)      COMP-3.                  
007200*                                 ARTIKELPRIS NETTO                       
007300*                                 NET PRICE EACH   (FOB NET)              
007400     03 PRAD-PRBPRIS         PIC S9(7)V9(2)      COMP-3.                  
007500*                                 BASPRIS                                 
007600     03 PRAD-REKSIFFR        PIC S9              COMP-3.                  
007700*                                 KONTROLLSIFFRA                          
007800*                                 PART NO CHECK DIGIT                     
007900     03 PRAD-TIPRIS          PIC S9(7)           COMP-3.                  
008000*                                 PRISTILLÄMPNINGSDATUM  (ÅÅMMDD)         
008100*                                 PRICE ADAPTION DATE    (YYMMDD)         
008200     03 PRAD-TIREGDAT        PIC S9(7)           COMP-3.                  
008300*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
008400*                                 REGISTRATION DATE (YYMMDD)              
008500     03 PRAD-TIREGTID        PIC S9(7)           COMP-3.                  
008600*                                 REGISTRERINGSTID                        
008700*                                 GENERAL REGISTRATION TIME               
008800     03 PRAD-VKART           PIC S9(7)           COMP-3.                  
008900*                                 ARTIKELVIKT (G)                         
009000*                                 PART WEIGHT (G)                         
009100     03 PRAD-VLARTNTO        PIC S9(8)V9(1)      COMP-3.                  
009200*                                 ARTIKELVOLYM NETTO (CM3)                
009300*                                 PART NET VOLUME    (CM3)                
009400     03 PRAD-DEAL-PR-LINE.                                                
009500*                                 DEALERPRIS (RAD)                        
009600        05 PRAD-IDPRQUES     PIC 9(7).                                    
009700*                                 PRISFRÅGA NR                            
009800*                                 PRICE QUESTION NO                       
009900        05 PRAD-PRARTNTO-LOC PIC S9(7)V9(2)      COMP-3.                  
010000*                                 ARTIKELPRIS NETTO LOKAL VALUTA          
010100*                                 NET PRICE EACH LOCAL CURRENCY           
010200        05 PRAD-PRARTNTO-LOCPREL                                          
010300                             PIC S9(7)V9(2)      COMP-3.                  
010400*                                 PREL NETTO SLUTKUNDSPRIS I              
010500*                                 LOKAL VALUTA                            
010600*                                 PREL NET PRICE - LOCAL CURRENCY         
010700        05 PRAD-PRARTBTO-LOC PIC S9(7)V9(2)      COMP-3.                  
010800*                                 PRIS I LOKAL VALUTA                     
010900*                                 LOCAL GROSS SALES PRICE                 
011000        05 PRAD-KDVALISO     PIC X(3).                                    
011100*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
011200*                                 CURRENCY CODE BY ISO-STANDARD.          
011300        05 PRAD-KDVAT        PIC X(2).                                    
011400*                                 MOMSKOD                                 
011500*                                 VAT CODE                                
011600        05 PRAD-RERAB        PIC S9(2)V9(1)      COMP-3.                  
011700*                                 RABATTSATS (PROCENT)                    
011800        05 PRAD-KDRAB        PIC X(5).                                    
011900*                                 RABATTKOD                               
012000        05 PRAD-BEART-VIPS   PIC X(25).                                   
012100*                                 VIPS ARTIKELBENÄMNING                   
012200*                                 PÅ DEALERNS SPRÅK                       
012300     03 PRAD-FILLER          PIC X(8).                                    
012400*** END OF VILMAII-COPY LENGTH= 180 BYTES                                 
