000100 01  RESP-WF0278O1.                                                       
000200*                                 RESPONS-COPYTEXT FÖR PGM WF0278         
000300*                                 ERRONEUS BUNDLES DETAIL                 
000400     03 RESP-IDLEGSEL-KEY    PIC X(4).                                    
000500*                                 FAKTURERANDE FÖRETAG TEX VCCS           
000600*                                 LEGAL SELLER IDENTITY                   
000700     03 RESP-IDBUNDLE-KEY    PIC X(15).                                   
000800*                                 BUNDLE ID                               
000900*                                 BUNDLE ID                               
001000     03 RESP-DAREGDAT-KEY    PIC X(8).                                    
001100*                                 REGISTRERINGSDATUM (ÅÅÅÅMMDD)           
001200*                                 REGISTRATION DATE (YYYYMMDD)            
001300     03 RESP-TIREGTID-KEY    PIC 9(10).                                   
001400*                                 REGISTRERINGSTID                        
001500*                                 GENERAL REGISTRATION TIME               
001600     03 RESP-IDREF-KEY       PIC X(15).                                   
001700*                                 REFERENS ID                             
001800*                                 REFERENCE ID                            
001900     03 RESP-DAREFDAT-KEY    PIC X(8).                                    
002000*                                 REFERENSDATUM (ÅÅÅÅMMDD)                
002100*                                 REFERENCE DATE(YYYYMMDD)                
002200     03 RESP-IDREFRAD-KEY    PIC 9(5).                                    
002300*                                 REFERENSRADSNR                          
002400*                                 REFERENCE LINE NUMBER                   
002500     03 RESP-BEVOLREF        PIC X(10).                                   
002600*                                 VOLVO REFERENS                          
002700*                                 VOLVO REFERENCE                         
002800     03 RESP-IDLANDX3-SEND   PIC X(3).                                    
002900*                                 LANDKOD SÄNDANDE LAND                   
003000*                                 COUNTRY CODE SENDING COUNTRY            
003100     03 RESP-IDLANDX3-REC    PIC X(3).                                    
003200*                                 LANDKOD MOTTAGANDE LAND                 
003300*                                 COUNTRY CODE RECEIVING COUNTRY          
003400     03 RESP-IDPARTNR        PIC X(9).                                    
003500*                                 PARTNERNUMMER                           
003600*                                 PARTNER NO                              
003700     03 RESP-IDEXCUST        OCCURS 3 TIMES                               
003800                             PIC X(15).                                   
003900*                                 EXTERNT KUNDID                          
004000*                                 EXTERNAL CUSTOMER ID                    
004100     03 RESP-IDOPTION        OCCURS 5 TIMES                               
004200                             PIC X(15).                                   
004300*                                 BRYTBEGREPP                             
004400*                                 BREAK ID                                
004500     03 RESP-IDARTNR-FINANCE PIC X(50).                                   
004600*                                 ARTIKELNUMMER FÖR FINANSIELL BR         
004700*                                 UK                                      
004800*                                 PART NUMBER FOR FINANCIAL USE           
004900     03 RESP-IDSTATNR        PIC Z(8)9.                                   
005000*                                 STATISTISKT NUMMER                      
005100*                                 1 = NORSKT                              
005200*                                 2 = ENGELSKT                            
005300*                                 3 = BELGISKT                            
005400*                                 4 = PERUANSKT                           
005500*                                 5 = SVENSKT                             
005600*                                 6 =                                     
005700*                                 STATISTICAL NO.                         
005800     03 RESP-VKARTNTO        PIC Z(3)9.9(3).                              
005900*                                 ARTIKELVIKT NETTO (KG)                  
006000*                                 PART NET WEIGHT (KG)                    
006100     03 RESP-PRARTBTO        PIC Z(6)9.9(2).                              
006200*                                 FÖRSÄLJNINGSPRIS BRUTTO (KR)            
006300*                                 GROSS SALES PRICE (SEK)                 
006400     03 RESP-PRARTNTO        PIC Z(6)9.9(2).                              
006500*                                 ARTIKELPRIS NETTO                       
006600*                                 NET PRICE EACH   (FOB NET)              
006700     03 RESP-REARTRAB        PIC Z9.9(2).                                 
006800*                                 ARTIKELRABATT                           
006900*                                 PARTS DISCOUNT PERCENT                  
007000     03 RESP-KVBEART         PIC Z(6)9.                                   
007100*                                 BESTÄLLT ANTAL STYCKEN                  
007200*                                 ORDERED QUANTITY                        
007300     03 RESP-KVLEVART        PIC Z(6)9.                                   
007400*                                 LEVERERAT ANTAL STYCK                   
007500*                                 DELIVERED QUANTITY                      
007600     03 RESP-BEART           PIC X(25).                                   
007700*                                 ARTIKELBENÄMNING                        
007800*                                 PART DESCRIPTION                        
007900     03 RESP-FLSOFT          PIC X.                                       
008000*                                 FLAGGA SOFTVARA                         
008100*                                 SOFTWARE MARK                           
008200     03 RESP-FLSPECPR        PIC X.                                       
008300*                                 SPECIALPRISFLAGGA                       
008400*                                 SPECIAL PRICE FLAG                      
008500     03 RESP-FLFREE          PIC X.                                       
008600*                                 GRATISFATURA                            
008700*                                 FREE INVOICE                            
008800     03 RESP-KDVAT           PIC X(2).                                    
008900*                                 MOMSKOD                                 
009000*                                 VAT CODE                                
009100     03 RESP-KDVALISO        PIC X(3).                                    
009200*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
009300*                                 CURRENCY CODE BY ISO-STANDARD.          
009400     03 RESP-KDINVFRQ        PIC X(4).                                    
009500*                                 FAKTURERINGSFREKVENS                    
009600*                                 INVOICE FREQUENCE                       
009700     03 RESP-KDFINDOC        PIC X(4).                                    
009800*                                 TYP FINANSIELLT DOKUMENT                
009900*                                 FINANCIAL DOCUMENT TYPE                 
010000     03 RESP-IDBREAK         OCCURS 2 TIMES                               
010100                             PIC X(8).                                    
010200*                                 BRYTVÄRDE                               
010300*                                 BREAK VALUE                             
010400     03 RESP-IDSEQ           OCCURS 3 TIMES                               
010500                             PIC X(8).                                    
010600*                                 SEKVENSVÄRDE                            
010700*                                 SEQUENCE VALUE                          
010800     03 RESP-KDARTURS        PIC X(2).                                    
010900*                                 ARTIKELURSPRUNGSKOD                     
011000*                                 COUNTRY OF ORIGIN                       
011100     03 RESP-KDANMORS        PIC X(2).                                    
011200*                                 ORSAK TILL LEVERANSANMÄRKNING           
011300*                                 DISCREPANCY REPORT REASON CODE          
011400     03 RESP-IDFAKREF        PIC 9(9).                                    
011500*                                 URSPRUNGLIGT FAKTURANUMMER              
011600*                                 ORIGINAL INVOICE NUMBER                 
011700     03 RESP-DAFAKREF        PIC 9(8).                                    
011800*                                 URSPRUNGLIGT FAKTURADATUM (ÅÅÅÅ         
011900*                                 MMDD)                                   
012000*                                 ORIGINAL INVOICING DATE (YYYYMM         
012100*                                 DD)                                     
012200     03 RESP-IDDC            PIC X(2).                                    
012300*                                 IDENTIFIERARE LAGER                     
012400*                                 WAREHOUSE IDENTIFIER                    
012500     03 RESP-IDACCNT         OCCURS 4 TIMES                               
012600                             PIC X(15).                                   
012700*                                 KONTOFÄLT                               
012800*                                 ACCOUNT FIELD                           
012900     03 RESP-VKORDBTO-KOLLI  PIC Z(5)9.9.                                 
013000*                                 ORDERVIKT BRUTTO PER KOLLI              
013100*                                 ORDER WEIGHT GROSS PER CASE             
013200     03 RESP-IDLEVNR         PIC X(5).                                    
013300*                                 LEVERANTÖRNUMMER                        
013400*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
013500     03 RESP-KDFRAKT         PIC Z9.                                      
013600*                                 FRAKTSÄTT DC TILL KUND                  
013700*                                 FREIGHT CODE                            
013800     03 RESP-BELEVVIL        PIC X(35).                                   
013900*                                 LEVERANSVILLKOR                         
014000*                                 DELIVERY TERMS                          
014100     03 RESP-IDSYSTEM-SEND   PIC X(4).                                    
014200*                                 VOLVO SÄNDANDE SYSTEM                   
014300*                                 VOLVO SENDING SYSTEM                    
014400     03 RESP-IDSYSTEM-REC    PIC X(4).                                    
014500*                                 VOLVO MOTTAGANDE SYSTEM                 
014600*                                 VOLVO RECEIVING SYSTEM                  
014700     03 RESP-BETEXT          PIC X(100).                                  
014800     03 RESP-IDFELKOD        PIC X(3).                                    
014900*                                 FELKOD                                  
015000*                                 ERROR CODE                              
015100     03 RESP-BEFEL           PIC X(50).                                   
015200*                                 FELTEXT                                 
015300*                                 ERROR TEXT                              
015400     03 RESP-IDAPPEND        PIC X(8).                                    
015500*                                 APPENDIXVÄRDE                           
015600*                                 APPENDIX ITEM                           
015700     03 RESP-BEANST          PIC X(25).                                   
015800*                                 ANSTÄLLDS NAMN                          
015900*                                 NAME OF EMPLOYED                        
016000     03 RESP-IDUSER          PIC X(8).                                    
016100*                                 ANVÄNDARENS SÄKERHETS ID                
016200*                                 USER SECURITY-IDENTITY                  
016300     03 RESP-BELEGRAD-1      PIC X(35).                                   
016400*                                 DEL AV LEGAL SELLER NAMN                
016500*                                 PART OF LEGAL SELLER NAME               
016600     03 RESP-FLRESTBUN       PIC X.                                       
016700*                                 ÅTERSTARTBAR BUNT ?                     
016800*                                 RESTARTABLE BUNDLE ?                    
016900     03 RESP-KVFELBUN        PIC Z(4)9.                                   
017000*                                 ANTAL FEL I BUNT                        
017100*                                 NUMBER OF ERRORS IN BUNDLE              
017200     03 RESP-IDFELBUN        PIC Z(4)9.                                   
017300*                                 ORDNINGSNR PÅ FEL I BUNT                
017400*                                 SEQUENC NBR OF ERROR IN BUNDLE          
017500*** END OF VILMAII-COPY LENGTH= 778 BYTES                                 
