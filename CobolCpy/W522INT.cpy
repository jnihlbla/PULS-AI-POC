000100 01  W522INT.                                                             
000200*                                 INTRASTAT DATA FOR DISTRIBUTION         
000300     03 W522INT-001.                                                      
000400        05 TIAA              PIC 9(2).                                    
000500*                                 ÅR    (ÅÅ)                              
000600*                                 YEAR  (YY)                              
000700        05 TIRP              PIC 9(2).                                    
000800*                                 REDOVISNINGSPERIOD                      
000900*                                 12 PER ÅR                               
001000*                                 ACCOUNTING PERIOD                       
001100*                                 12 PER YEAR                             
001200        05 IDLANDX3-BET      PIC X(3).                                    
001300*                                 LANDKOD BETALANDE KUND ETC              
001400*                                 COUNTRY CODE PAYING CUSTOMER ET         
001500*                                 C                                       
001600        05 IDLANDX3-SEND     PIC X(3).                                    
001700*                                 LANDKOD SÄNDANDE LAND                   
001800*                                 COUNTRY CODE SENDING COUNTRY            
001900        05 IDLANDX3-REC      PIC X(3).                                    
002000*                                 LANDKOD MOTTAGANDE LAND                 
002100*                                 COUNTRY CODE RECEIVING COUNTRY          
002200        05 KDVALISO          PIC X(3).                                    
002300*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
002400*                                 CURRENCY CODE BY ISO-STANDARD.          
002500        05 PRKURS            PIC 9(6)V9(5).                               
002600*                                 VALUTAKURS                              
002700*                                 CURRENCY EXCHANGE RATE                  
002800        05 IDPARTNR          PIC X(9).                                    
002900*                                 FINANCIELL KUND                         
003000*                                 FINANCIAL CUST                          
003100        05 KDFINDOC          PIC X(4).                                    
003200*                                 TYP FINANSIELLT DOKUMENT                
003300*                                 FINANCIAL DOCUMENT TYPE                 
003400        05 DAFINDOC          PIC 9(8).                                    
003500*                                 DOKUMENT DATUM (ÅÅÅÅMMDD)               
003600*                                 INVOICING DATE   (YYYYMMDD)             
003700        05 IDFINDOC          PIC 9(9).                                    
003800*                                 FINANSIELLT DOKUMENT ID                 
003900*                                 FINANCIAL DOCUMENT ID                   
004000        05 IDDISTR           PIC 9(5).                                    
004100*                                 DISTRIKTNUMMER                          
004200*                                 DISTRICT NUMBER                         
004300        05 IDKUNDNR          PIC 9(7).                                    
004400*                                 KUNDNUMMER                              
004500*                                 CUSTOMER NO                             
004600        05 IDARTNR           PIC 9(9).                                    
004700*                                 ARTIKELNUMMER                           
004800*                                 PART NUMBER                             
004900        05 BEART             PIC X(25).                                   
005000*                                 ARTIKELBENÄMNING                        
005100*                                 PART DESCRIPTION                        
005200        05 IDSTATNR          PIC 9(9).                                    
005300*                                 STATISTISKT NUMMER                      
005400*                                 1 = NORSKT                              
005500*                                 2 = ENGELSKT                            
005600*                                 3 = BELGISKT                            
005700*                                 4 = PERUANSKT                           
005800*                                 5 = SVENSKT                             
005900*                                 6 =                                     
006000*                                 STATISTICAL NO.                         
006100        05 VKORDNTO          PIC 9(6)V9(1).                               
006200*                                 ORDERVIKT NETTO (KG)                    
006300*                                 WEIGHT PER ORDER NETTO (KG)             
006400        05 KVLEVART          PIC 9(7).                                    
006500*                                 LEVERERAT ANTAL STYCK                   
006600*                                 DELIVERED QUANTITY                      
006700        05 KDARTURS          PIC X(2).                                    
006800*                                 ARTIKELURSPRUNGSKOD                     
006900*                                 COUNTRY OF ORIGIN                       
007000        05 KDFRAKT           PIC 9(2).                                    
007100*                                 FRAKTSÄTT DC TILL KUND                  
007200*                                 FREIGHT CODE                            
007300        05 BELEVVIL          PIC X(35).                                   
007400*                                 LEVERANSVILLKOR                         
007500*                                 DELIVERY TERMS                          
007600        05 SUNTO             PIC 9(11)V9(2).                              
007700*                                 TOTAL SALES AMOUNT EXCL. VAT            
007800        05 KDVALISO-SEND     PIC X(3).                                    
007900*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
008000*                                 CURRENCY CODE BY ISO-STANDARD.          
008100        05 PRKURS-SEND       PIC 9(6)V9(5).                               
008200*                                 VALUTAKURS                              
008300*                                 CURRENCY EXCHANGE RATE                  
008400     03 W522INT-002.                                                      
008500        05 IDVAT-LEG         PIC X(17).                                   
008600*                                 MOMSREGISTRERINGSNUMMER LEGAL S         
008700*                                 ÄLJARE                                  
008800*                                 VAT REGISTRATION LEGAL PAYER            
008900        05 IDVAT-BET         PIC X(17).                                   
009000*                                 MOMSREGISTRERINGSNUMMER BETALAR         
009100*                                 E                                       
009200*                                 VAT REGISTRATION NUMBER PAYER           
009300        05 IDVAT-RESP        PIC X(17).                                   
009400*                                 MOMSREGISTRERINGSNUMMER ANSVARI         
009500*                                 G AVD                                   
009600*                                 VAT REGISTRATION RESPONSIBLE DP         
009700*                                 T                                       
009800        05 IDVAT-AGENT       PIC X(17).                                   
009900*                                 MOMSREGISTRERINGSNUMMER AGENT           
010000*                                 VAT REGISTRATION VAT AGENT              
010100        05 VKORDNTO-3DEC     PIC 9(5)V9(3).                               
010200*                                 ORDERVIKT NETTO (KG)                    
010300*                                 WEIGHT PER ORDER NETTO (KG)             
010400*** END OF VILMAII-COPY LENGTH= 268 BYTES                                 
