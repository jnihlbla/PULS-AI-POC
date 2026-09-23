000100 01  CUS-WF2018T.                                                         
000200*                                 INTRASTAT DATA                          
000300*                                                                         
000400     03 CUS-IDLEGSEL         OCCURS 100 TIMES                             
000500                             PIC X(4).                                    
000600*                                 FAKTURERANDE FÖRETAG TEX VCCS           
000700*                                 LEGAL SELLER IDENTITY                   
000800     03 CUS-DAEXDAT          OCCURS 100 TIMES                             
000900                             PIC 9(8).                                    
001000*                                 EXEKVERINGSDATUM (ÅÅÅÅMMDD)             
001100*                                 EXECUTION DATE (YYYYMMDD)               
001200     03 CUS-TIEXTID          OCCURS 100 TIMES                             
001300                             PIC S9(7)           COMP-3.                  
001400*                                 EXEKVERINGSTIDPUNKT                     
001500*                                 EXECUTION TIME                          
001600     03 CUS-IDLANDX3-SEND    OCCURS 100 TIMES                             
001700                             PIC X(3).                                    
001800*                                 LANDKOD SÄNDANDE LAND                   
001900*                                 COUNTRY CODE SENDING COUNTRY            
002000     03 CUS-IDLANDX3-BET     OCCURS 100 TIMES                             
002100                             PIC X(3).                                    
002200*                                 LANDKOD BETALANDE KUND ETC              
002300*                                 COUNTRY CODE PAYING CUSTOMER ET         
002400*                                 C                                       
002500     03 CUS-IDLANDX3-REC     OCCURS 100 TIMES                             
002600                             PIC X(3).                                    
002700*                                 LANDKOD MOTTAGANDE LAND                 
002800*                                 COUNTRY CODE RECEIVING COUNTRY          
002900     03 CUS-KDVALISO         OCCURS 100 TIMES                             
003000                             PIC X(3).                                    
003100*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
003200*                                 CURRENCY CODE BY ISO-STANDARD.          
003300     03 CUS-PRKURS           OCCURS 100 TIMES                             
003400                             PIC S9(6)V9(5)      COMP-3.                  
003500*                                 VALUTAKURS                              
003600*                                 CURRENCY EXCHANGE RATE                  
003700     03 CUS-KDFINDOC         OCCURS 100 TIMES                             
003800                             PIC X(4).                                    
003900*                                 TYP FINANSIELLT DOKUMENT                
004000*                                 FINANCIAL DOCUMENT TYPE                 
004100     03 CUS-DAFINDOC         OCCURS 100 TIMES                             
004200                             PIC 9(8).                                    
004300*                                 DOKUMENT DATUM (ÅÅÅÅMMDD)               
004400*                                 INVOICING DATE   (YYYYMMDD)             
004500     03 CUS-IDFINDOC         OCCURS 100 TIMES                             
004600                             PIC S9(9)           COMP-3.                  
004700*                                 FINANSIELLT DOKUMENT ID                 
004800*                                 FINANCIAL DOCUMENT ID                   
004900     03 CUS-IDPARTNR         OCCURS 100 TIMES                             
005000                             PIC X(9).                                    
005100*                                 PARTNERNUMMER                           
005200*                                 PARTNER NO                              
005300     03 CUS-IDEXCUST-1       OCCURS 100 TIMES                             
005400                             PIC X(15).                                   
005500*                                 EXTERNT KUNDID                          
005600*                                 EXTERNAL CUSTOMER ID                    
005700     03 CUS-IDEXCUST-2       OCCURS 100 TIMES                             
005800                             PIC X(15).                                   
005900*                                 EXTERNT KUNDID                          
006000*                                 EXTERNAL CUSTOMER ID                    
006100     03 CUS-IDEXCUST-3       OCCURS 100 TIMES                             
006200                             PIC X(15).                                   
006300*                                 EXTERNT KUNDID                          
006400*                                 EXTERNAL CUSTOMER ID                    
006500     03 CUS-IDARTNR-FINANCE  OCCURS 100 TIMES                             
006600                             PIC X(50).                                   
006700*                                 ARTIKELNUMMER FÖR FINANSIELL BR         
006800*                                 UK                                      
006900*                                 PART NUMBER FOR FINANCIAL USE           
007000     03 CUS-BEART            OCCURS 100 TIMES                             
007100                             PIC X(25).                                   
007200*                                 ARTIKELBENÄMNING                        
007300*                                 PART DESCRIPTION                        
007400     03 CUS-IDSTATNR         OCCURS 100 TIMES                             
007500                             PIC S9(9)           COMP-3.                  
007600*                                 STATISTISKT NUMMER                      
007700*                                 1 = NORSKT                              
007800*                                 2 = ENGELSKT                            
007900*                                 3 = BELGISKT                            
008000*                                 4 = PERUANSKT                           
008100*                                 5 = SVENSKT                             
008200*                                 6 =                                     
008300*                                 STATISTICAL NO.                         
008400     03 CUS-VKARTNTO         OCCURS 100 TIMES                             
008500                             PIC S9(4)V9(3)      COMP-3.                  
008600*                                 ARTIKELVIKT NETTO (KG)                  
008700*                                 PART NET WEIGHT (KG)                    
008800     03 CUS-KVLEVART         OCCURS 100 TIMES                             
008900                             PIC S9(7)           COMP-3.                  
009000*                                 LEVERERAT ANTAL STYCK                   
009100*                                 DELIVERED QUANTITY                      
009200     03 CUS-KDARTURS         OCCURS 100 TIMES                             
009300                             PIC X(2).                                    
009400*                                 ARTIKELURSPRUNGSKOD                     
009500*                                 COUNTRY OF ORIGIN                       
009600     03 CUS-KDFRAKT          OCCURS 100 TIMES                             
009700                             PIC S9(3)           COMP-3.                  
009800*                                 FRAKTSÄTT DC TILL KUND                  
009900*                                 FREIGHT CODE                            
010000     03 CUS-BELEVVIL         OCCURS 100 TIMES                             
010100                             PIC X(35).                                   
010200*                                 LEVERANSVILLKOR                         
010300*                                 DELIVERY TERMS                          
010400     03 CUS-SUNTO            OCCURS 100 TIMES                             
010500                             PIC S9(11)V9(2)     COMP-3.                  
010600*                                 TOTAL SALES AMOUNT EXCL. VAT            
010700     03 CUS-KDVALISO-SEND    OCCURS 100 TIMES                             
010800                             PIC X(3).                                    
010900*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
011000*                                 CURRENCY CODE BY ISO-STANDARD.          
011100     03 CUS-PRKURS-SEND      OCCURS 100 TIMES                             
011200                             PIC S9(6)V9(5)      COMP-3.                  
011300*                                 VALUTAKURS                              
011400*                                 CURRENCY EXCHANGE RATE                  
011500*** END OF VILMAII-COPY LENGTH= 24800 BYTES                               
