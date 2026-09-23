000100 01  INT-WF2012.                                                          
000200*                                 INTRASTAT DATA                          
000300*                                                                         
000400     03 INT-IDLEGSEL         PIC X(4).                                    
000500*                                 FAKTURERANDE FÖRETAG TEX VCCS           
000600*                                 LEGAL SELLER IDENTITY                   
000700     03 INT-DAEXDAT          PIC 9(8).                                    
000800*                                 EXEKVERINGSDATUM (ÅÅÅÅMMDD)             
000900*                                 EXECUTION DATE (YYYYMMDD)               
001000     03 INT-TIEXTID          PIC S9(7)           COMP-3.                  
001100*                                 EXEKVERINGSTIDPUNKT                     
001200*                                 EXECUTION TIME                          
001300     03 INT-IDLANDX3-SEND    PIC X(3).                                    
001400*                                 LANDKOD SÄNDANDE LAND                   
001500*                                 COUNTRY CODE SENDING COUNTRY            
001600     03 INT-IDLANDX3-BET     PIC X(3).                                    
001700*                                 LANDKOD BETALANDE KUND ETC              
001800*                                 COUNTRY CODE PAYING CUSTOMER ET         
001900*                                 C                                       
002000     03 INT-IDLANDX3-REC     PIC X(3).                                    
002100*                                 LANDKOD MOTTAGANDE LAND                 
002200*                                 COUNTRY CODE RECEIVING COUNTRY          
002300     03 INT-KDVALISO         PIC X(3).                                    
002400*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
002500*                                 CURRENCY CODE BY ISO-STANDARD.          
002600     03 INT-PRKURS           PIC S9(6)V9(5)      COMP-3.                  
002700*                                 VALUTAKURS                              
002800*                                 CURRENCY EXCHANGE RATE                  
002900     03 INT-KDFINDOC         PIC X(4).                                    
003000*                                 TYP FINANSIELLT DOKUMENT                
003100*                                 FINANCIAL DOCUMENT TYPE                 
003200     03 INT-DAFINDOC         PIC 9(8).                                    
003300*                                 DOKUMENT DATUM (ÅÅÅÅMMDD)               
003400*                                 INVOICING DATE   (YYYYMMDD)             
003500     03 INT-IDFINDOC         PIC S9(9)           COMP-3.                  
003600*                                 FINANSIELLT DOKUMENT ID                 
003700*                                 FINANCIAL DOCUMENT ID                   
003800     03 INT-IDPARTNR         PIC X(9).                                    
003900*                                 FINANCIELL KUND                         
004000*                                 FINANCIAL CUST                          
004100     03 INT-IDEXCUST-1       PIC X(15).                                   
004200*                                 EXTERNT KUNDID                          
004300*                                 EXTERNAL CUSTOMER ID                    
004400     03 INT-IDEXCUST-2       PIC X(15).                                   
004500*                                 EXTERNT KUNDID                          
004600*                                 EXTERNAL CUSTOMER ID                    
004700     03 INT-IDEXCUST-3       PIC X(15).                                   
004800*                                 EXTERNT KUNDID                          
004900*                                 EXTERNAL CUSTOMER ID                    
005000     03 INT-IDARTNR-FINANCE  PIC X(50).                                   
005100*                                 ARTIKELNUMMER FÖR FINANSIELL BR         
005200*                                 UK                                      
005300*                                 PART NUMBER FOR FINANCIAL USE           
005400     03 INT-BEART            PIC X(25).                                   
005500*                                 ARTIKELBENÄMNING                        
005600*                                 PART DESCRIPTION                        
005700     03 INT-IDSTATNR         PIC S9(9)           COMP-3.                  
005800*                                 STATISTISKT NUMMER                      
005900*                                 1 = NORSKT                              
006000*                                 2 = ENGELSKT                            
006100*                                 3 = BELGISKT                            
006200*                                 4 = PERUANSKT                           
006300*                                 5 = SVENSKT                             
006400*                                 6 =                                     
006500*                                 STATISTICAL NO.                         
006600     03 INT-VKARTNTO         PIC S9(4)V9(3)      COMP-3.                  
006700*                                 ARTIKELVIKT NETTO (KG) MED EMB          
006800*                                 PART NET WEIGHT (KG) W/ PACKAGE         
006900     03 INT-KVLEVART         PIC S9(7)           COMP-3.                  
007000*                                 LEVERERAT ANTAL STYCK                   
007100*                                 DELIVERED QUANTITY                      
007200     03 INT-KDARTURS         PIC X(2).                                    
007300*                                 ARTIKELURSPRUNGSKOD                     
007400*                                 COUNTRY OF ORIGIN                       
007500     03 INT-KDFRAKT          PIC S9(3)           COMP-3.                  
007600*                                 FRAKTSÄTT DC TILL KUND                  
007700*                                 FREIGHT CODE                            
007800     03 INT-BELEVVIL         PIC X(35).                                   
007900*                                 LEVERANSVILLKOR                         
008000*                                 DELIVERY TERMS                          
008100     03 INT-SUNTO            PIC S9(11)V9(2)     COMP-3.                  
008200*                                 TOTAL SALES AMOUNT EXCL. VAT            
008300     03 INT-KDVALISO-SEND    PIC X(3).                                    
008400*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
008500*                                 CURRENCY CODE BY ISO-STANDARD.          
008600     03 INT-PRKURS-SEND      PIC S9(6)V9(5)      COMP-3.                  
008700*                                 VALUTAKURS                              
008800*                                 CURRENCY EXCHANGE RATE                  
008900     03 INT-IDVAT-LEG        PIC X(17).                                   
009000*                                 MOMSREGISTRERINGSNUMMER LEGAL S         
009100*                                 ÄLJARE                                  
009200*                                 VAT REGISTRATION LEGAL PAYER            
009300     03 INT-IDVAT-RESP       PIC X(17).                                   
009400*                                 MOMSREGISTRERINGSNUMMER ANSVARI         
009500*                                 G AVD                                   
009600*                                 VAT REGISTRATION RESPONSIBLE DP         
009700*                                 T                                       
009800     03 INT-IDVAT-BET        PIC X(17).                                   
009900*                                 MOMSREGISTRERINGSNUMMER BETALAR         
010000*                                 E                                       
010100*                                 VAT REGISTRATION NUMBER PAYER           
010200     03 INT-IDVAT-AGENT      PIC X(17).                                   
010300*                                 MOMSREGISTRERINGSNUMMER AGENT           
010400*                                 VAT REGISTRATION VAT AGENT              
010500*** END OF VILMAII-COPY LENGTH= 316 BYTES                                 
