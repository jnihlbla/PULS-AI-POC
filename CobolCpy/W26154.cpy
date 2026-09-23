000100 01  ORAD-W26154.                                                         
000200*                                 EXTRAKT FRÅN WDQ4                       
000300*                                 ORDERRADSREGISTER KÖ                    
000400*                                 KOMPLETTERAD MED ANSKAFFARINFO          
000500     03 ORAD-IDORDER         PIC S9(7)           COMP-3.                  
000600*                                 VOLVO PARTS ORDERNUMMER                 
000700*                                 VOLVO PARTS ORDER NUMBER                
000800     03 ORAD-IDDC            PIC X(2).                                    
000900*                                 IDENTIFIERARE LAGER                     
001000*                                 WAREHOUSE IDENTIFIER                    
001100     03 ORAD-ADLAGOMR        PIC S9(3)           COMP-3.                  
001200*                                 LAGEROMRÅDE                             
001300*                                 AREA                                    
001400     03 ORAD-ADGANG          PIC S9(3)           COMP-3.                  
001500*                                 GÅNG                                    
001600*                                 AISLE                                   
001700     03 ORAD-ADPLATS         PIC S9(5)           COMP-3.                  
001800*                                 LAGERPLATSNUMMER                        
001900*                                 LOCATION                                
002000     03 ORAD-IDARTNR         PIC S9(9)           COMP-3.                  
002100*                                 ARTIKELNUMMER                           
002200*                                 PART NUMBER                             
002300     03 ORAD-IDLOPNR         PIC S9(3)           COMP-3.                  
002400*                                 LÖPNUMMER                               
002500*                                 SEQUENCE NUMBER                         
002600     03 ORAD-BERADREF        PIC X(10).                                   
002700*                                 KUNDENS RADREFERENS                     
002800*                                 CUSTOMERS ITEM REF.                     
002900     03 ORAD-BEVOLREF        PIC X(10).                                   
003000*                                 VOLVO REFERENS                          
003100*                                 VOLVO REFERENCE                         
003200     03 ORAD-FLAKPLOC        PIC X.                                       
003300*                                 ORDERRADEN SKA PLOCKAS PÅ AK            
003400*                                 ORDER LINE FROM "AK" QUEUE              
003500     03 ORAD-FLINVEST        PIC X.                                       
003600*                                 BYTES INVENTERINGSFLAGGA                
003700*                                 EXCHANGE INVESTMENT FLAG                
003800     03 ORAD-FLOBTRAN        PIC X.                                       
003900*                                 ORDERBEKRÄFTELSETRANSAKTION             
004000*                                 ORDERCONFIRMATIONTRANSACTION            
004100     03 ORAD-FLPRTILL        PIC X.                                       
004200*                                 PRISTILLÄGGS FLAGGA                     
004300*                                 PRICE PENALTY FLAG                      
004400     03 ORAD-FLRESTN         PIC X.                                       
004500*                                 RESTNOTERING ?                          
004600*                                 BACKORDERED ?                           
004700     03 ORAD-FLSDCLEV        PIC X.                                       
004800*                                 LEVERANSSTYRNING SDC                    
004900*                                 DELIVERY ONLY SDC                       
005000     03 ORAD-FLTILLK         PIC X.                                       
005100*                                 TILLKOMMANDE ARTIKEL ?                  
005200*                                 REPLACEMENT PART FLAG                   
005300     03 ORAD-IDDC-RO         PIC X(2).                                    
005400*                                 LAGER DÄR RESTORDER FÅR SKE             
005500*                                 WAREHOUSE FOR BACKORDERS                
005600     03 ORAD-IDGMTREF.                                                    
005700*                                 GODSMOTTAGAREREFERENS                   
005800*                                 GOODS RECEIVER REFERENS                 
005900        05 ORAD-IDDISTR      PIC S9(5)           COMP-3.                  
006000*                                 DISTRIKTNUMMER                          
006100*                                 DISTRICT NUMBER                         
006200        05 ORAD-IDKUNDNR     PIC S9(7)           COMP-3.                  
006300*                                 KUNDNUMMER                              
006400*                                 CUSTOMER NO                             
006500        05 ORAD-IDKUNDRF-GRP.                                             
006600*                                 KUNDENS REFERENS (ORDERID)              
006700*                                 CUSTOMER REFERENCE (ORDER ID)           
006800           07 ORAD-IDKUNDRF  PIC X(10).                                   
006900*                                 KUNDENS REFERENS (ORDERID)              
007000*                                 CUSTOMER REFERENCE (ORDER ID)           
007100           07 ORAD-IDORDNR5-FILLER REDEFINES ORAD-IDKUNDRF.               
007200              09 ORAD-IDORDNR5                                            
007300                             PIC 9(5).                                    
007400*                                 ORDERNUMMER                             
007500*                                 ORDER NUMBER                            
007600              09 FILLER      PIC X(5).                                    
007700           07 ORAD-IDORDNR7-FILLER REDEFINES ORAD-IDKUNDRF.               
007800              09 ORAD-IDORDNR7                                            
007900                             PIC 9(7).                                    
008000*                                 ORDERNUMMER                             
008100*                                 ORDER NUMBER                            
008200              09 FILLER      PIC X(3).                                    
008300     03 ORAD-IDKAMPRF        PIC S9(7)           COMP-3.                  
008400*                                 KAMPANJREFERENS                         
008500*                                 CAMPAIGN REFERENCE                      
008600     03 ORAD-IDLEVNR         PIC X(5).                                    
008700*                                 LEVERANTÖRNUMMER                        
008800*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
008900     03 ORAD-IDLOPNR-RO      PIC S9(3)           COMP-3.                  
009000*                                 LÖPNUMMER RO/TPO                        
009100*                                 SEQUENCE NUMBER BO/TPO                  
009200     03 ORAD-IDKUNDRF-RO     PIC X(10).                                   
009300*                                 KUND REF PÅ RO                          
009400*                                 CUST REF RO                             
009500     03 ORAD-IDSPECEMB       PIC 9(4).                                    
009600*                                 SPECIALEMBALLAGEID                      
009700*                                 SPECIAL PACKING ID                      
009800     03 ORAD-IDSYSTEM        PIC X(4).                                    
009900*                                 VOLVO VCCS SYSTEMNUMMER                 
010000*                                 VOLVO VCCS SYSTEM NUMBER                
010100     03 ORAD-KDARTURS        PIC X(2).                                    
010200*                                 ARTIKELURSPRUNGSKOD                     
010300*                                 COUNTRY OF ORIGIN                       
010400     03 ORAD-KDDSP           PIC S9              COMP-3.                  
010500*                                 PÅVERKAN PÅ DSP                         
010600*                                 AFFECT ON DSP                           
010700     03 ORAD-KDFARLIG        PIC S9              COMP-3.                  
010800*                                 KOD FÖR FARLIGT GODS                    
010900*                                 DANGEROUS GOODS CODE                    
011000     03 ORAD-KDKVBRYT        PIC S9              COMP-3.                  
011100*                                 KOD OM KVANTFÖRP SKALL BRYTAS           
011200*                                 BREAK BULKPACK CODE                     
011300     03 ORAD-KDOI            PIC X(2).                                    
011400*                                 ORDERINGÅNGSTYP                         
011500*                                 TYPE OF INCOMING ORDER                  
011600     03 ORAD-KDORDING        PIC S9              COMP-3.                  
011700*                                 UPPDATERING ORDERINGÅNG                 
011800*                                 ORDER STATISTICS                        
011900     03 ORAD-KDORDKL         PIC S9              COMP-3.                  
012000*                                 ORDERKLASS                              
012100*                                 ORDER CLASS                             
012200     03 ORAD-KDPRODSL        PIC S9(3)           COMP-3.                  
012300*                                 PRODUKTSLAG                             
012400*                                 PRODUCT GROUP                           
012500     03 ORAD-KDPRTYP         PIC X.                                       
012600*                                 TYP AV PRISTILLÄMPNING                  
012700*                                 TYPE OF PRICING                         
012800     03 ORAD-KDSPEEMB        PIC 9.                                       
012900*                                 SPECIALEMBALLAGEKOD                     
013000*                                 SPECIAL PACKING CODE                    
013100     03 ORAD-KDTPOTYP        PIC S9              COMP-3.                  
013200*                                 TYP AV TIDPLANERAD ORDER                
013300*                                 TYPE OF TIME PLANNED ORDER              
013400     03 ORAD-KDVRINFO        PIC S9              COMP-3.                  
013500*                                 PÅVERKAN I VR/DSP SYSTEM                
013600*                                 VR/DSP UP-DATE                          
013700     03 ORAD-KVBEART         PIC S9(7)           COMP-3.                  
013800*                                 BESTÄLLT ANTAL STYCKEN                  
013900*                                 ORDERED QUANTITY                        
014000     03 ORAD-KVBEART-Q       PIC S9(7)           COMP-3.                  
014100*                                 BESTÄLLT KVANTANPASSAT ANTAL            
014200*                                 ORDERED QUANTITY ADAPTED                
014300*                                  ITEMS                                  
014400     03 ORAD-KVPREAVB        PIC S9(7)           COMP-3.                  
014500*                                 PREL-AVB KVANT                          
014600*                                 PREL-RES QUANT                          
014700     03 ORAD-KVPRERO         PIC S9(7)           COMP-3.                  
014800*                                 PRELIMINÄR RO-KVANT                     
014900*                                 PRELIMINARY BO-QUANT                    
015000     03 ORAD-KVSLATT         PIC S9(7)           COMP-3.                  
015100*                                 BERÄKNAD SLATTGRÄNS                     
015200*                                                                         
015300     03 ORAD-PRARTNTO        PIC S9(7)V9(2)      COMP-3.                  
015400*                                 ARTIKELPRIS NETTO                       
015500*                                 NET PRICE EACH   (FOB NET)              
015600     03 ORAD-PRBPRIS         PIC S9(7)V9(2)      COMP-3.                  
015700*                                 BASPRIS                                 
015800     03 ORAD-REKSIFFR        PIC S9              COMP-3.                  
015900*                                 KONTROLLSIFFRA                          
016000*                                 PART NO CHECK DIGIT                     
016100     03 ORAD-RERF-RAD        PIC S9V9(4)         COMP-3.                  
016200*                                 RANSONERINGSFAKTOR PÅ ORDERRAD          
016300*                                 RATIONING FACTOR ON ORDERLINE           
016400     03 ORAD-TIPRIS          PIC S9(7)           COMP-3.                  
016500*                                 PRISTILLÄMPNINGSDATUM  (ÅÅMMDD)         
016600*                                 PRICE ADAPTION DATE    (YYMMDD)         
016700     03 ORAD-TIREGDAT        PIC S9(7)           COMP-3.                  
016800*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
016900*                                 REGISTRATION DATE (YYMMDD)              
017000     03 ORAD-TIREGTID        PIC S9(7)           COMP-3.                  
017100*                                 REGISTRERINGSTID                        
017200*                                 GENERAL REGISTRATION TIME               
017300     03 ORAD-TIRODAT         PIC S9(7)           COMP-3.                  
017400*                                 RESTORDERDATUM         (ÅÅMMDD)         
017500*                                 BACK ORDER DATE        (YYMMDD)         
017600     03 ORAD-TITPO           PIC S9(7)           COMP-3.                  
017700*                                 PLANERAD ORDERDATUM                     
017800*                                 PLANNED ORDER DATE                      
017900     03 ORAD-VKART           PIC S9(7)           COMP-3.                  
018000*                                 ARTIKELVIKT (G)                         
018100*                                 PART WEIGHT (G)                         
018200     03 ORAD-VLARTNTO        PIC S9(8)V9(1)      COMP-3.                  
018300*                                 ARTIKELVOLYM NETTO (CM3)                
018400*                                 PART NET VOLUME    (CM3)                
018500     03 ORAD-IDBIL.                                                       
018600*                                 BILIDENTITET                            
018700*                                 CAR IDENTITY                            
018800        05 ORAD-IDBILTYP     PIC X(3).                                    
018900*                                 BILTYP                                  
019000*                                 CAR TYPE                                
019100        05 ORAD-TIAAAA       PIC X(4).                                    
019200*                                 ÅRTAL (ÅÅÅÅ)                            
019300*                                 YEAR  (YYYY)                            
019400        05 ORAD-IDCHASSI-PIE PIC X(6).                                    
019500*                                 CHASSINUMMER PIE                        
019600*                                 CHASSI NUMBER PIE                       
019700     03 ORAD-IDARBREF        PIC X(10).                                   
019800*                                 ARBETSORDER VADIS                       
019900*                                 WORK ORDER VADIS                        
020000     03 ORAD-IDKLIENT        PIC X(10).                                   
020100*                                 VADIS KLIENT                            
020200*                                 VADIS CLIENT                            
020300     03 ORAD-IDVIN           PIC X(17).                                   
020400*                                 VIN ID FORDON                           
020500*                                 VEHICLE VIN ID                          
020600     03 ORAD-IDANSK          PIC S9(3)           COMP-3.                  
020700*                                 ANSKAFFARNUMMER                         
020800*                                 PROCURER NO.                            
020900     03 ORAD-GRP-INT         PIC X(7).                                    
021000     03 ORAD-FLSKROT-AUTO    PIC X.                                       
021100*                                 SKROTNING AUTOMATISKT BEORDRAD          
021200*                                 SCRAPPING AUTOMATICALLY ORDERED         
021300*** END OF VILMAII-COPY LENGTH= 233 BYTES                                 
