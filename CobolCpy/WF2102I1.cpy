000100 01  WF2102I1.                                                            
000200*                                 FEEDBACK DATA TO SYSTEM PULS-IN         
000300*                                 TRASTAT                                 
000400     03 DAEXDAT              PIC 9(8).                                    
000500*                                 EXEKVERINGSDATUM (ÅÅÅÅMMDD)             
000600*                                 EXECUTION DATE (YYYYMMDD)               
000700     03 TIEXTID              PIC 9(6).                                    
000800*                                 EXEKVERINGSTIDPUNKT                     
000900*                                 EXECUTION TIME                          
001000     03 IDPTYP               PIC X(3).                                    
001100*                                 POSTTYP                                 
001200*                                 RECORD TYPE                             
001300     03 INTRASTAT-DATA.                                                   
001400        05 IDLANDX3-BET      PIC X(3).                                    
001500*                                 LANDKOD BETALANDE KUND ETC              
001600*                                 COUNTRY CODE PAYING CUSTOMER ET         
001700*                                 C                                       
001800        05 IDLANDX3-SEND     PIC X(3).                                    
001900*                                 LANDKOD SÄNDANDE LAND                   
002000*                                 COUNTRY CODE SENDING COUNTRY            
002100        05 IDLANDX3-REC      PIC X(3).                                    
002200*                                 LANDKOD MOTTAGANDE LAND                 
002300*                                 COUNTRY CODE RECEIVING COUNTRY          
002400        05 KDVALISO          PIC X(3).                                    
002500*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
002600*                                 CURRENCY CODE BY ISO-STANDARD.          
002700        05 PRKURS            PIC 9(6)V9(5).                               
002800*                                 VALUTAKURS                              
002900*                                 CURRENCY EXCHANGE RATE                  
003000        05 IDPARTNR          PIC X(9).                                    
003100*                                 FINANCIELL KUND                         
003200*                                 FINANCIAL CUST                          
003300        05 KDFINDOC          PIC X(4).                                    
003400*                                 TYP FINANSIELLT DOKUMENT                
003500*                                 FINANCIAL DOCUMENT TYPE                 
003600        05 DAFINDOC          PIC 9(8).                                    
003700*                                 DOKUMENT DATUM (ÅÅÅÅMMDD)               
003800*                                 INVOICING DATE   (YYYYMMDD)             
003900        05 IDFINDOC          PIC 9(9).                                    
004000*                                 FINANSIELLT DOKUMENT ID                 
004100*                                 FINANCIAL DOCUMENT ID                   
004200        05 IDEXCUST-1        PIC X(15).                                   
004300*                                 EXTERNT KUNDID                          
004400*                                 EXTERNAL CUSTOMER ID                    
004500        05 IDEXCUST-2        PIC X(15).                                   
004600*                                 EXTERNT KUNDID                          
004700*                                 EXTERNAL CUSTOMER ID                    
004800        05 IDARTNR-FINANCE   PIC X(50).                                   
004900*                                 ARTIKELNUMMER FÖR FINANSIELL BR         
005000*                                 UK                                      
005100*                                 PART NUMBER FOR FINANCIAL USE           
005200        05 BEART             PIC X(25).                                   
005300*                                 ARTIKELBENÄMNING                        
005400*                                 PART DESCRIPTION                        
005500        05 IDSTATNR          PIC 9(9).                                    
005600*                                 STATISTISKT NUMMER                      
005700*                                 1 = NORSKT                              
005800*                                 2 = ENGELSKT                            
005900*                                 3 = BELGISKT                            
006000*                                 4 = PERUANSKT                           
006100*                                 5 = SVENSKT                             
006200*                                 6 =                                     
006300*                                 STATISTICAL NO.                         
006400        05 VKORDNTO          PIC 9(6)V9(1).                               
006500*                                 ORDERVIKT NETTO (KG)                    
006600*                                 WEIGHT PER ORDER NETTO (KG)             
006700        05 KVLEVART          PIC 9(7).                                    
006800*                                 LEVERERAT ANTAL STYCK                   
006900*                                 DELIVERED QUANTITY                      
007000        05 KDARTURS          PIC X(2).                                    
007100*                                 ARTIKELURSPRUNGSKOD                     
007200*                                 COUNTRY OF ORIGIN                       
007300        05 KDFRAKT           PIC 9(2).                                    
007400*                                 FRAKTSÄTT DC TILL KUND                  
007500*                                 FREIGHT CODE                            
007600        05 BELEVVIL          PIC X(35).                                   
007700*                                 LEVERANSVILLKOR                         
007800*                                 DELIVERY TERMS                          
007900        05 SUNTO             PIC 9(11)V9(2).                              
008000*                                 TOTAL SALES AMOUNT EXCL. VAT            
008100        05 KDVALISO-SEND     PIC X(3).                                    
008200*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
008300*                                 CURRENCY CODE BY ISO-STANDARD.          
008400        05 PRKURS-SEND       PIC 9(6)V9(5).                               
008500*                                 VALUTAKURS                              
008600*                                 CURRENCY EXCHANGE RATE                  
008700        05 IDVAT-LEG         PIC X(17).                                   
008800*                                 MOMSREGISTRERINGSNUMMER LEGAL S         
008900*                                 ÄLJARE                                  
009000*                                 VAT REGISTRATION LEGAL PAYER            
009100        05 IDVAT-BET         PIC X(17).                                   
009200*                                 MOMSREGISTRERINGSNUMMER BETALAR         
009300*                                 E                                       
009400*                                 VAT REGISTRATION NUMBER PAYER           
009500        05 IDVAT-RESP        PIC X(17).                                   
009600*                                 MOMSREGISTRERINGSNUMMER ANSVARI         
009700*                                 G AVD                                   
009800*                                 VAT REGISTRATION RESPONSIBLE DP         
009900*                                 T                                       
010000        05 IDVAT-AGENT       PIC X(17).                                   
010100*                                 MOMSREGISTRERINGSNUMMER AGENT           
010200*                                 VAT REGISTRATION VAT AGENT              
010300        05 VKORDNTO-3DEC     PIC 9(5)V9(3).                               
010400*                                 ORDERVIKT NETTO (KG)                    
010500*                                 WEIGHT PER ORDER NETTO (KG)             
010600*** END OF VILMAII-COPY LENGTH= 340 BYTES                                 
