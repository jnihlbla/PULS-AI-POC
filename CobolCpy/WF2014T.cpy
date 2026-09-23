000100 01  GL-WF2014T.                                                          
000200*                                 GENERAL LEDGER DATA                     
000300     03 GL-IDLEGSEL          OCCURS 100 TIMES                             
000400                             PIC X(4).                                    
000500*                                 FAKTURERANDE FÖRETAG TEX VCCS           
000600*                                 LEGAL SELLER IDENTITY                   
000700     03 GL-KDVALISO          OCCURS 100 TIMES                             
000800                             PIC X(3).                                    
000900*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
001000*                                 CURRENCY CODE BY ISO-STANDARD.          
001100     03 GL-PRKURS            OCCURS 100 TIMES                             
001200                             PIC S9(6)V9(5)      COMP-3.                  
001300*                                 VALUTAKURS                              
001400*                                 CURRENCY EXCHANGE RATE                  
001500     03 GL-IDLANDX3-SEND     OCCURS 100 TIMES                             
001600                             PIC X(3).                                    
001700*                                 LANDKOD SÄNDANDE LAND                   
001800*                                 COUNTRY CODE SENDING COUNTRY            
001900     03 GL-IDLANDX3-BET      OCCURS 100 TIMES                             
002000                             PIC X(3).                                    
002100*                                 LANDKOD BETALANDE KUND ETC              
002200*                                 COUNTRY CODE PAYING CUSTOMER ET         
002300*                                 C                                       
002400     03 GL-KDPARTTY          OCCURS 100 TIMES                             
002500                             PIC X(3).                                    
002600*                                 TYP AV BETALARE                         
002700*                                 TYPE OF FIN.CUSTOMER                    
002800     03 GL-KDPARTGR          OCCURS 100 TIMES                             
002900                             PIC X(15).                                   
003000*                                 GRUPP AV BETALARE                       
003100*                                 FIN.CUSTOMER GROUP                      
003200     03 GL-IDPARTNR          OCCURS 100 TIMES                             
003300                             PIC X(9).                                    
003400*                                 PARTNERNUMMER                           
003500*                                 PARTNER NO                              
003600     03 GL-KDFINDOC          OCCURS 100 TIMES                             
003700                             PIC X(4).                                    
003800*                                 TYP FINANSIELLT DOKUMENT                
003900*                                 FINANCIAL DOCUMENT TYPE                 
004000     03 GL-FLSOFT            OCCURS 100 TIMES                             
004100                             PIC X.                                       
004200*                                 FLAGGA SOFTVARA                         
004300*                                 SOFTWARE MARK                           
004400     03 GL-FLFREE            OCCURS 100 TIMES                             
004500                             PIC X.                                       
004600*                                 GRATISFATURA                            
004700*                                 FREE INVOICE                            
004800     03 GL-DAFINDOC          OCCURS 100 TIMES                             
004900                             PIC 9(8).                                    
005000*                                 DOKUMENT DATUM (ÅÅÅÅMMDD)               
005100*                                 INVOICING DATE   (YYYYMMDD)             
005200     03 GL-IDFINDOC          OCCURS 100 TIMES                             
005300                             PIC S9(9)           COMP-3.                  
005400*                                 FINANSIELLT DOKUMENT ID                 
005500*                                 FINANCIAL DOCUMENT ID                   
005600     03 GL-SUNTO-SERV        OCCURS 100 TIMES                             
005700                             PIC S9(11)V9(2)     COMP-3.                  
005800*                                 TOTAL SALES AMOUNT SERVICES EXC         
005900*                                 L. VAT                                  
006000     03 GL-SUNTO-PART        OCCURS 100 TIMES                             
006100                             PIC S9(11)V9(2)     COMP-3.                  
006200*                                 TOTAL SALES AMOUNT PARTS EXCL.          
006300*                                 VAT                                     
006400     03 GL-SUBTO-SERV        OCCURS 100 TIMES                             
006500                             PIC S9(11)V9(2)     COMP-3.                  
006600*                                 TOTAL SALES AMOUNT SERVICES INC         
006700*                                 L. VAT                                  
006800     03 GL-SUBTO-PART        OCCURS 100 TIMES                             
006900                             PIC S9(11)V9(2)     COMP-3.                  
007000*                                 TOTAL SALES AMOUNT PARTS INCL.          
007100*                                 VAT                                     
007200     03 GL-SUNTO-TOT         OCCURS 100 TIMES                             
007300                             PIC S9(11)V9(2)     COMP-3.                  
007400*                                 TOTAL SALES AMOUNT EXCL. VAT            
007500     03 GL-SUBTO-TOT         OCCURS 100 TIMES                             
007600                             PIC S9(11)V9(2)     COMP-3.                  
007700*                                 TOTAL SALES AMOUNT INCL. VAT            
007800     03 GL-SUVAT-BILLIT-TOT  OCCURS 100 TIMES                             
007900                             PIC S9(11)V9(2)     COMP-3.                  
008000*                                 SUMMERAT MOMSVÄRDE                      
008100*                                 TOTAL VAT VALUE                         
008200     03 GL-KDTRADP           OCCURS 100 TIMES                             
008300                             PIC X(4).                                    
008400*                                 TRADING PARTNER                         
008500*                                 TRADING PARTNER                         
008600     03 GL-IDBUNDLE          OCCURS 100 TIMES                             
008700                             PIC X(15).                                   
008800*                                 BUNDLE ID                               
008900*                                 BUNDLE ID                               
009000     03 GL-IDREF             OCCURS 100 TIMES                             
009100                             PIC X(15).                                   
009200*                                 REFERENS ID                             
009300*                                 REFERENCE ID                            
009400     03 GL-DAREFDAT          OCCURS 100 TIMES                             
009500                             PIC 9(8).                                    
009600*                                 REFERENSDATUM (ÅÅÅÅMMDD)                
009700*                                 REFERENCE DATE(YYYYMMDD)                
009800     03 GL-IDARTNR-FINANCE   OCCURS 100 TIMES                             
009900                             PIC X(50).                                   
010000*                                 ARTIKELNUMMER FÖR FINANSIELL BR         
010100*                                 UK                                      
010200*                                 PART NUMBER FOR FINANCIAL USE           
010300     03 GL-PRARTBTO          OCCURS 100 TIMES                             
010400                             PIC S9(7)V9(2)      COMP-3.                  
010500*                                 FÖRSÄLJNINGSPRIS BRUTTO (KR)            
010600*                                 GROSS SALES PRICE (SEK)                 
010700     03 GL-PRARTNTO          OCCURS 100 TIMES                             
010800                             PIC S9(7)V9(2)      COMP-3.                  
010900*                                 ARTIKELPRIS NETTO                       
011000*                                 NET PRICE EACH   (FOB NET)              
011100     03 GL-REARTRAB          OCCURS 100 TIMES                             
011200                             PIC S9(2)V9(2)      COMP-3.                  
011300*                                 ARTIKELRABATT                           
011400*                                 PARTS DISCOUNT PERCENT                  
011500     03 GL-REVAT             OCCURS 100 TIMES                             
011600                             PIC S9(3)V9(2)      COMP-3.                  
011700*                                 MULTIPLIKATIONSFAKTOR FÖR MOMS          
011800*                                 VAT FACTOR                              
011900     03 GL-KVLEVART          OCCURS 100 TIMES                             
012000                             PIC S9(7)           COMP-3.                  
012100*                                 LEVERERAT ANTAL STYCK                   
012200*                                 DELIVERED QUANTITY                      
012300     03 GL-FLSPECPR          OCCURS 100 TIMES                             
012400                             PIC X.                                       
012500*                                 SPECIALPRISFLAGGA                       
012600*                                 SPECIAL PRICE FLAG                      
012700     03 GL-KDANMORS          OCCURS 100 TIMES                             
012800                             PIC X(2).                                    
012900*                                 ORSAK TILL LEVERANSANMÄRKNING           
013000*                                 DISCREPANCY REPORT REASON CODE          
013100     03 GL-IDDC              OCCURS 100 TIMES                             
013200                             PIC X(2).                                    
013300*                                 IDENTIFIERARE LAGER                     
013400*                                 WAREHOUSE IDENTIFIER                    
013500     03 GL-SUNTO             OCCURS 100 TIMES                             
013600                             PIC S9(11)V9(2)     COMP-3.                  
013700*                                 TOTAL SALES AMOUNT EXCL. VAT            
013800     03 GL-SUBTO             OCCURS 100 TIMES                             
013900                             PIC S9(11)V9(2)     COMP-3.                  
014000*                                 TOTAL SALES AMOUNT INCL. VAT            
014100     03 GL-SUVAT-BILLIT      OCCURS 100 TIMES                             
014200                             PIC S9(11)V9(2)     COMP-3.                  
014300*                                 SUMMERAT MOMSVÄRDE PER RAD              
014400*                                 TOTAL VAT VALUE PER LINE                
014500     03 GL-KDFRAKT           OCCURS 100 TIMES                             
014600                             PIC S9(3)           COMP-3.                  
014700*                                 FRAKTSÄTT DC TILL KUND                  
014800*                                 FREIGHT CODE                            
014900     03 GL-KDVAT             OCCURS 100 TIMES                             
015000                             PIC X(2).                                    
015100*                                 MOMSKOD                                 
015200*                                 VAT CODE                                
015300     03 GL-IDSYSTEM-SEND     OCCURS 100 TIMES                             
015400                             PIC X(4).                                    
015500*                                 VOLVO SÄNDANDE SYSTEM                   
015600*                                 VOLVO SENDING SYSTEM                    
015700     03 GL-KDBETALV          OCCURS 100 TIMES                             
015800                             PIC X(4).                                    
015900*                                 BETALNINGSVILLKOR KUNDRESKONTRA         
016000*                                 TERMS OF PAYMENT                        
016100     03 GL-REVALUTA          OCCURS 100 TIMES                             
016200                             PIC S9(5)           COMP-3.                  
016300*                                 OMRÄKNINGSTAL FÖR VALUTA                
016400*                                 CONVERT VALUE FOR CURRENCY CODE         
016500     03 GL-KDVALISO-INV      OCCURS 100 TIMES                             
016600                             PIC X(3).                                    
016700*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
016800*                                 CURRENCY CODE BY ISO-STANDARD.          
016900     03 GL-PRKURS-INV        OCCURS 100 TIMES                             
017000                             PIC S9(6)V9(5)      COMP-3.                  
017100*                                 VALUTAKURS                              
017200*                                 CURRENCY EXCHANGE RATE                  
017300     03 GL-REVALUTA-INV      OCCURS 100 TIMES                             
017400                             PIC S9(5)           COMP-3.                  
017500*                                 OMRÄKNINGSTAL FÖR VALUTA                
017600*                                 CONVERT VALUE FOR CURRENCY CODE         
017700     03 GL-TABELLRAD         OCCURS 100 TIMES.                            
017800*                                 GRUPP MED TABELLRADER                   
017900        05 GL-IDEXCUST       OCCURS 3 TIMES                               
018000                             PIC X(15).                                   
018100*                                 EXTERNT KUNDID                          
018200*                                 EXTERNAL CUSTOMER ID                    
018300        05 GL-IDOPTION       OCCURS 5 TIMES                               
018400                             PIC X(15).                                   
018500*                                 BRYTBEGREPP                             
018600*                                 OPTIONAL ID                             
018700        05 GL-IDACCNT        OCCURS 4 TIMES                               
018800                             PIC X(15).                                   
018900*                                 KONTOFÄLT                               
019000*                                 ACCOUNT FIELD                           
019100*** END OF VILMAII-COPY LENGTH= 45900 BYTES                               
