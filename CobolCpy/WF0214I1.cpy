000100 01  WF0214I1.                                                            
000200*                                 GENERAL ENTRANCE FOR LINES TO B         
000300*                                 E INVOICED/CREDITED                     
000400     03 IDLEGSEL             PIC X(4).                                    
000500*                                 FAKTURERANDE FÖRETAG TEX VCCS           
000600*                                 LEGAL SELLER IDENTITY                   
000700     03 IDBUNDLE             PIC X(15).                                   
000800*                                 BUNDLE ID                               
000900*                                 BUNDLE ID                               
001000     03 BEVOLREF             PIC X(10).                                   
001100*                                 VOLVO REFERENS                          
001200*                                 VOLVO REFERENCE                         
001300     03 IDREF                PIC X(15).                                   
001400*                                 REFERENS ID                             
001500*                                 REFERENCE ID                            
001600     03 DAREFDAT             PIC 9(8).                                    
001700*                                 REFERENSDATUM (ÅÅÅÅMMDD)                
001800*                                 REFERENCE DATE(YYYYMMDD)                
001900     03 IDREFRAD             PIC 9(5).                                    
002000*                                 REFERENSRADSNR                          
002100*                                 REFERENCE LINE NUMBER                   
002200     03 IDLANDX3-SEND        PIC X(3).                                    
002300*                                 LANDKOD SÄNDANDE LAND                   
002400*                                 COUNTRY CODE SENDING COUNTRY            
002500     03 IDLANDX3-REC         PIC X(3).                                    
002600*                                 LANDKOD MOTTAGANDE LAND                 
002700*                                 COUNTRY CODE RECEIVING COUNTRY          
002800     03 IDPARTNR             PIC X(9).                                    
002900*                                 PARTNERNUMMER                           
003000*                                 PARTNER NO                              
003100     03 IDEXCUST             OCCURS 3 TIMES                               
003200                             PIC X(15).                                   
003300*                                 EXTERNT KUNDID                          
003400*                                 EXTERNAL CUSTOMER ID                    
003500     03 IDOPTION             OCCURS 5 TIMES                               
003600                             PIC X(15).                                   
003700*                                 BRYTBEGREPP                             
003800*                                 OPTIONAL ID                             
003900     03 IDAPPEND             PIC X(8).                                    
004000*                                 APPENDIXVÄRDE                           
004100*                                 APPENDIX ITEM                           
004200     03 IDARTNR-FINANCE      PIC X(50).                                   
004300*                                 ARTIKELNUMMER FÖR FINANSIELL BR         
004400*                                 UK                                      
004500*                                 PART NUMBER FOR FINANCIAL USE           
004600     03 IDSTATNR             PIC 9(9).                                    
004700*                                 STATISTISKT NUMMER                      
004800*                                 1 = NORSKT                              
004900*                                 2 = ENGELSKT                            
005000*                                 3 = BELGISKT                            
005100*                                 4 = PERUANSKT                           
005200*                                 5 = SVENSKT                             
005300*                                 6 =                                     
005400*                                 STATISTICAL NO.                         
005500     03 VKORDBTO-KOLLI       PIC 9(6)V9(1).                               
005600*                                 ORDERVIKT BRUTTO PER KOLLI              
005700*                                 ORDER WEIGHT GROSS PER CASE             
005800     03 VKARTNTO             PIC 9(4)V9(3).                               
005900*                                 ARTIKELVIKT NETTO (KG)                  
006000*                                 PART NET WEIGHT (KG)                    
006100     03 PRARTBTO             PIC 9(7)V9(2).                               
006200*                                 FÖRSÄLJNINGSPRIS BRUTTO (KR)            
006300*                                 GROSS SALES PRICE (SEK)                 
006400     03 PRARTNTO             PIC 9(7)V9(2).                               
006500*                                 ARTIKELPRIS NETTO                       
006600*                                 NET PRICE EACH   (FOB NET)              
006700     03 REARTRAB             PIC 9(2)V9(2).                               
006800*                                 ARTIKELRABATT                           
006900*                                 PARTS DISCOUNT PERCENT                  
007000     03 KVBEART              PIC 9(6).                                    
007100*                                 BESTÄLLT ANTAL STYCKEN                  
007200*                                 ORDERED QUANTITY                        
007300     03 KVLEVART             PIC 9(7).                                    
007400*                                 LEVERERAT ANTAL STYCK                   
007500*                                 DELIVERED QUANTITY                      
007600     03 BEART                PIC X(25).                                   
007700*                                 ARTIKELBENÄMNING                        
007800*                                 PART DESCRIPTION                        
007900     03 FLSOFT               PIC X.                                       
008000*                                 FLAGGA SOFTVARA                         
008100*                                 SOFTWARE MARK                           
008200     03 FLSPECPR             PIC X.                                       
008300*                                 SPECIALPRISFLAGGA                       
008400*                                 SPECIAL PRICE FLAG                      
008500     03 FLFREE               PIC X.                                       
008600*                                 GRATISFATURA                            
008700*                                 FREE INVOICE                            
008800     03 FLPRIV               PIC X.                                       
008900*                                 KÖPARE ÄR EN PRIVATPERSON               
009000*                                 PURCHASER IS A PRIVATE PERSON           
009100     03 KDVAT                PIC X(2).                                    
009200*                                 MOMSKOD                                 
009300*                                 VAT CODE                                
009400     03 KDVALISO             PIC X(3).                                    
009500*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
009600*                                 CURRENCY CODE BY ISO-STANDARD.          
009700     03 KDINVFRQ             PIC X(4).                                    
009800*                                 FAKTURERINGSFREKVENS                    
009900*                                 INVOICE FREQUENCE                       
010000     03 KDFINDOC             PIC X(4).                                    
010100*                                 TYP FINANSIELLT DOKUMENT                
010200*                                 FINANCIAL DOCUMENT TYPE                 
010300     03 IDBREAK              OCCURS 2 TIMES                               
010400                             PIC X(8).                                    
010500*                                 BRYTVÄRDE                               
010600*                                 BREAK VALUE                             
010700     03 IDSEQ                OCCURS 3 TIMES                               
010800                             PIC X(8).                                    
010900*                                 SEKVENSVÄRDE                            
011000*                                 SEQUENCE VALUE                          
011100     03 KDARTURS             PIC X(2).                                    
011200*                                 ARTIKELURSPRUNGSKOD                     
011300*                                 COUNTRY OF ORIGIN                       
011400     03 KDANMORS             PIC X(2).                                    
011500*                                 ORSAK TILL LEVERANSANMÄRKNING           
011600*                                 DISCREPANCY REPORT REASON CODE          
011700     03 IDFAKREF             PIC 9(9).                                    
011800*                                 URSPRUNGLIGT FAKTURANUMMER              
011900*                                 ORIGINAL INVOICE NUMBER                 
012000     03 DAFAKREF             PIC 9(8).                                    
012100*                                 URSPRUNGLIGT FAKTURADATUM (ÅÅÅÅ         
012200*                                 MMDD)                                   
012300*                                 ORIGINAL INVOICING DATE (YYYYMM         
012400*                                 DD)                                     
012500     03 IDDC                 PIC X(2).                                    
012600*                                 IDENTIFIERARE LAGER                     
012700*                                 WAREHOUSE IDENTIFIER                    
012800     03 IDLEVNR              PIC X(5).                                    
012900*                                 LEVERANTÖRNUMMER                        
013000*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
013100     03 KDFRAKT              PIC 9(2).                                    
013200*                                 FRAKTSÄTT DC TILL KUND                  
013300*                                 FREIGHT CODE                            
013400     03 BELEVVIL             PIC X(35).                                   
013500*                                 LEVERANSVILLKOR                         
013600*                                 DELIVERY TERMS                          
013700     03 IDACCNT              OCCURS 4 TIMES                               
013800                             PIC X(15).                                   
013900*                                 KONTOFÄLT                               
014000*                                 ACCOUNT FIELD                           
014100     03 IDSYSTEM-SEND        PIC X(4).                                    
014200*                                 VOLVO SÄNDANDE SYSTEM                   
014300*                                 VOLVO SENDING SYSTEM                    
014400     03 IDSYSTEM-REC         PIC X(4).                                    
014500*                                 VOLVO MOTTAGANDE SYSTEM                 
014600*                                 VOLVO RECEIVING SYSTEM                  
014700     03 BEANST               PIC X(25).                                   
014800*                                 ANSTÄLLDS NAMN                          
014900*                                 NAME OF EMPLOYED                        
015000     03 IDUSER               PIC X(8).                                    
015100*                                 ANVÄNDARENS SÄKERHETS ID                
015200*                                 USER SECURITY-IDENTITY                  
015300     03 BETEXT               PIC X(100).                                  
015400     03 BETEXT-CRE           PIC X(100).                                  
015500*** END OF VILMAII-COPY LENGTH= 756 BYTES                                 
