000100 01  DP-FIPH-WF2101.                                                      
000200*                                 DOCUMENT DATA FIPH                      
000300     03 DP-FIPH-IDAFPRCD     PIC X(10).                                   
000400*                                 AFP-BLANKETT POSTTYP                    
000500*                                 AFP FORMS RECORD TYPE                   
000600     03 DP-FIPH-IDEXCUST-1   PIC X(15).                                   
000700*                                 EXTERNT KUNDID                          
000800*                                 EXTERNAL CUSTOMER ID                    
000900     03 DP-FIPH-IDEXCUST-2   PIC X(15).                                   
001000*                                 EXTERNT KUNDID                          
001100*                                 EXTERNAL CUSTOMER ID                    
001200     03 DP-FIPH-IDBUNDLE     PIC X(15).                                   
001300*                                 BUNDLE ID                               
001400*                                 BUNDLE ID                               
001500     03 DP-FIPH-IDOPTION-1   PIC X(15).                                   
001600*                                 BRYTBEGREPP                             
001700*                                 OPTIONAL ID                             
001800     03 DP-FIPH-IDOPTION-2   PIC X(15).                                   
001900*                                 BRYTBEGREPP                             
002000*                                 OPTIONAL ID                             
002100     03 DP-FIPH-IDOPTION-3   PIC X(15).                                   
002200*                                 BRYTBEGREPP                             
002300*                                 OPTIONAL ID                             
002400     03 DP-FIPH-IDREF        PIC X(15).                                   
002500*                                 REFERENS ID                             
002600*                                 REFERENCE ID                            
002700     03 DP-FIPH-BEVOLREF     PIC X(10).                                   
002800*                                 VOLVO REFERENS                          
002900*                                 VOLVO REFERENCE                         
003000     03 DP-FIPH-BERADREF     PIC X(10).                                   
003100     03 DP-FIPH-IDARTNR-FINANCE                                           
003200                             PIC X(50).                                   
003300*                                 ARTIKELNUMMER FÖR FINANSIELL BR         
003400*                                 UK                                      
003500*                                 PART NUMBER FOR FINANCIAL USE           
003600     03 DP-FIPH-BEART        PIC X(25).                                   
003700*                                 ARTIKELBENÄMNING                        
003800*                                 PART DESCRIPTION                        
003900     03 DP-FIPH-KVBEART      PIC Z(6).                                    
004000*                                 BESTÄLLT ANTAL STYCKEN                  
004100*                                 ORDERED QUANTITY                        
004200     03 DP-FIPH-KVLEVART     PIC Z(6)9.                                   
004300*                                 LEVERERAT ANTAL STYCK                   
004400*                                 DELIVERED QUANTITY                      
004500     03 DP-FIPH-PRARTBTO     PIC Z(6)9.9(2).                              
004600*                                 FÖRSÄLJNINGSPRIS BRUTTO (KR)            
004700*                                 GROSS SALES PRICE (SEK)                 
004800     03 DP-FIPH-PRARTNTO     PIC Z(6)9.9(2).                              
004900*                                 ARTIKELPRIS NETTO                       
005000*                                 NET PRICE EACH   (FOB NET)              
005100     03 DP-FIPH-REARTRAB     PIC Z9.9(2).                                 
005200*                                 ARTIKELRABATT                           
005300*                                 PARTS DISCOUNT PERCENT                  
005400     03 DP-FIPH-FLSPECPR     PIC X.                                       
005500*                                 SPECIALPRISFLAGGA                       
005600*                                 SPECIAL PRICE FLAG                      
005700     03 DP-FIPH-SUNTO        PIC Z(10)9.9(2).                             
005800*                                 TOTAL SALES AMOUNT EXCL. VAT            
005900     03 DP-FIPH-REVAT        PIC Z(2)9.9(2).                              
006000*                                 MULTIPLIKATIONSFAKTOR FÖR MOMS          
006100*                                 VAT FACTOR                              
006200     03 DP-FIPH-SUVAT-BILLIT PIC Z(10)9.9(2).                             
006300*                                 SUMMERAT MOMSVÄRDE PER RAD              
006400*                                 TOTAL VAT VALUE PER LINE                
006500     03 DP-FIPH-SUBTO        PIC Z(10)9.9(2).                             
006600*                                 TOTAL SALES AMOUNT INCL. VAT            
006700     03 DP-FIPH-VKARTNTO     PIC Z(3)9.9(3).                              
006800*                                 ARTIKELVIKT NETTO (KG)                  
006900*                                 PART NET WEIGHT (KG)                    
007000     03 DP-FIPH-VKORDBTO-KOLLI                                            
007100                             PIC Z(5)9.9.                                 
007200*                                 ORDERVIKT BRUTTO PER KOLLI              
007300*                                 ORDER WEIGHT GROSS PER CASE             
007400     03 DP-FIPH-KDARTURS     PIC X(2).                                    
007500*                                 ARTIKELURSPRUNGSKOD                     
007600*                                 COUNTRY OF ORIGIN                       
007700     03 DP-FIPH-IDSTATNR     PIC Z(9).                                    
007800*                                 STATISTISKT NUMMER                      
007900*                                 1 = NORSKT                              
008000*                                 2 = ENGELSKT                            
008100*                                 3 = BELGISKT                            
008200*                                 4 = PERUANSKT                           
008300*                                 5 = SVENSKT                             
008400*                                 6 =                                     
008500*                                 STATISTICAL NO.                         
008600     03 DP-FIPH-KDFRAKT      PIC Z9.                                      
008700*                                 FRAKTSÄTT DC TILL KUND                  
008800*                                 FREIGHT CODE                            
008900     03 DP-FIPH-IDACCNT-1    PIC X(15).                                   
009000*                                 KONTOFÄLT                               
009100*                                 ACCOUNT FIELD                           
009200     03 DP-FIPH-IDACCNT-2    PIC X(15).                                   
009300*                                 KONTOFÄLT                               
009400*                                 ACCOUNT FIELD                           
009500     03 DP-FIPH-IDACCNT-3    PIC X(15).                                   
009600*                                 KONTOFÄLT                               
009700*                                 ACCOUNT FIELD                           
009800     03 DP-FIPH-KDANMORS     PIC X(2).                                    
009900*                                 ORSAK TILL LEVERANSANMÄRKNING           
010000*                                 DISCREPANCY REPORT REASON CODE          
010100     03 DP-FIPH-IDFAKREF     PIC Z(9).                                    
010200*                                 URSPRUNGLIGT FAKTURANUMMER              
010300*                                 ORIGINAL INVOICE NUMBER                 
010400     03 DP-FIPH-DAFAKREF     PIC Z(8).                                    
010500*                                 URSPRUNGLIGT FAKTURADATUM (ÅÅÅÅ         
010600*                                 MMDD)                                   
010700*                                 ORIGINAL INVOICING DATE (YYYYMM         
010800*                                 DD)                                     
010900     03 DP-FIPH-DAREFDAT     PIC 9(8).                                    
011000*                                 REFERENSDATUM (ÅÅÅÅMMDD)                
011100*                                 REFERENCE DATE(YYYYMMDD)                
011200     03 DP-FIPH-IDDC         PIC X(2).                                    
011300*                                 IDENTIFIERARE LAGER                     
011400*                                 WAREHOUSE IDENTIFIER                    
011500     03 DP-FIPH-BELEVVIL     PIC X(35).                                   
011600*                                 LEVERANSVILLKOR                         
011700*                                 DELIVERY TERMS                          
011800     03 DP-FIPH-IDOPTION-5   PIC X(15).                                   
011900*                                 BRYTBEGREPP                             
012000*                                 OPTIONAL ID                             
012100     03 DP-FIPH-BEANST       PIC X(25).                                   
012200*                                 ANSTÄLLDS NAMN                          
012300*                                 NAME OF EMPLOYED                        
012400     03 DP-FIPH-IDUSER       PIC X(8).                                    
012500*                                 ANVÄNDARENS SÄKERHETS ID                
012600*                                 USER SECURITY-IDENTITY                  
012700     03 DP-FIPH-REF-MINUS    PIC X.                                       
012800*** END OF VILMAII-COPY LENGTH= 484 BYTES                                 
