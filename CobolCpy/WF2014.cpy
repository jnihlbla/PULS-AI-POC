000100 01  GL-WF2014.                                                           
000200*                                 GENERAL LEDGER DATA                     
000300     03 GL-IDLEGSEL          PIC X(4).                                    
000400*                                 FAKTURERANDE FÖRETAG TEX VCCS           
000500*                                 LEGAL SELLER IDENTITY                   
000600     03 GL-KDVALISO          PIC X(3).                                    
000700*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
000800*                                 CURRENCY CODE BY ISO-STANDARD.          
000900     03 GL-PRKURS            PIC S9(6)V9(5)      COMP-3.                  
001000*                                 VALUTAKURS                              
001100*                                 CURRENCY EXCHANGE RATE                  
001200     03 GL-IDLANDX3-SEND     PIC X(3).                                    
001300*                                 LANDKOD SÄNDANDE LAND                   
001400*                                 COUNTRY CODE SENDING COUNTRY            
001500     03 GL-IDLANDX3-BET      PIC X(3).                                    
001600*                                 LANDKOD BETALANDE KUND ETC              
001700*                                 COUNTRY CODE PAYING CUSTOMER ET         
001800*                                 C                                       
001900     03 GL-KDPARTTY          PIC X(3).                                    
002000*                                 TYP AV BETALARE                         
002100*                                 TYPE OF FIN.CUSTOMER                    
002200     03 GL-KDPARTGR          PIC X(15).                                   
002300*                                 GRUPP AV BETALARE                       
002400*                                 FIN.CUSTOMER GROUP                      
002500     03 GL-IDPARTNR          PIC X(9).                                    
002600*                                 PARTNERNUMMER                           
002700*                                 PARTNER NO                              
002800     03 GL-KDFINDOC          PIC X(4).                                    
002900*                                 TYP FINANSIELLT DOKUMENT                
003000*                                 FINANCIAL DOCUMENT TYPE                 
003100     03 GL-FLSOFT            PIC X.                                       
003200*                                 FLAGGA SOFTVARA                         
003300*                                 SOFTWARE MARK                           
003400     03 GL-FLFREE            PIC X.                                       
003500*                                 GRATISFATURA                            
003600*                                 FREE INVOICE                            
003700     03 GL-DAFINDOC          PIC 9(8).                                    
003800*                                 DOKUMENT DATUM (ÅÅÅÅMMDD)               
003900*                                 INVOICING DATE   (YYYYMMDD)             
004000     03 GL-IDFINDOC          PIC S9(9)           COMP-3.                  
004100*                                 FINANSIELLT DOKUMENT ID                 
004200*                                 FINANCIAL DOCUMENT ID                   
004300     03 GL-SUNTO-SERV        PIC S9(11)V9(2)     COMP-3.                  
004400*                                 TOTAL SALES AMOUNT SERVICES EXC         
004500*                                 L. VAT                                  
004600     03 GL-SUNTO-PART        PIC S9(11)V9(2)     COMP-3.                  
004700*                                 TOTAL SALES AMOUNT PARTS EXCL.          
004800*                                 VAT                                     
004900     03 GL-SUBTO-SERV        PIC S9(11)V9(2)     COMP-3.                  
005000*                                 TOTAL SALES AMOUNT SERVICES INC         
005100*                                 L. VAT                                  
005200     03 GL-SUBTO-PART        PIC S9(11)V9(2)     COMP-3.                  
005300*                                 TOTAL SALES AMOUNT PARTS INCL.          
005400*                                 VAT                                     
005500     03 GL-SUNTO-TOT         PIC S9(11)V9(2)     COMP-3.                  
005600*                                 TOTAL SALES AMOUNT EXCL. VAT            
005700     03 GL-SUBTO-TOT         PIC S9(11)V9(2)     COMP-3.                  
005800*                                 TOTAL SALES AMOUNT INCL. VAT            
005900     03 GL-SUVAT-BILLIT-TOT  PIC S9(11)V9(2)     COMP-3.                  
006000*                                 SUMMERAT MOMSVÄRDE                      
006100*                                 TOTAL VAT VALUE                         
006200     03 GL-KDTRADP           PIC X(4).                                    
006300*                                 TRADING PARTNER                         
006400*                                 TRADING PARTNER                         
006500     03 GL-IDBUNDLE          PIC X(15).                                   
006600*                                 BUNDLE ID                               
006700*                                 BUNDLE ID                               
006800     03 GL-IDREF             PIC X(15).                                   
006900*                                 REFERENS ID                             
007000*                                 REFERENCE ID                            
007100     03 GL-DAREFDAT          PIC 9(8).                                    
007200*                                 REFERENSDATUM (ÅÅÅÅMMDD)                
007300*                                 REFERENCE DATE(YYYYMMDD)                
007400     03 GL-IDEXCUST          OCCURS 3 TIMES                               
007500                             PIC X(15).                                   
007600*                                 EXTERNT KUNDID                          
007700*                                 EXTERNAL CUSTOMER ID                    
007800     03 GL-IDOPTION          OCCURS 5 TIMES                               
007900                             PIC X(15).                                   
008000*                                 BRYTBEGREPP                             
008100*                                 OPTIONAL ID                             
008200     03 GL-IDARTNR-FINANCE   PIC X(50).                                   
008300*                                 ARTIKELNUMMER FÖR FINANSIELL BR         
008400*                                 UK                                      
008500*                                 PART NUMBER FOR FINANCIAL USE           
008600     03 GL-PRARTBTO          PIC S9(7)V9(2)      COMP-3.                  
008700*                                 FÖRSÄLJNINGSPRIS BRUTTO (KR)            
008800*                                 GROSS SALES PRICE (SEK)                 
008900     03 GL-PRARTNTO          PIC S9(7)V9(2)      COMP-3.                  
009000*                                 ARTIKELPRIS NETTO                       
009100*                                 NET PRICE EACH   (FOB NET)              
009200     03 GL-REARTRAB          PIC S9(2)V9(2)      COMP-3.                  
009300*                                 ARTIKELRABATT                           
009400*                                 PARTS DISCOUNT PERCENT                  
009500     03 GL-REVAT             PIC S9(3)V9(2)      COMP-3.                  
009600*                                 MULTIPLIKATIONSFAKTOR FÖR MOMS          
009700*                                 VAT FACTOR                              
009800     03 GL-KVLEVART          PIC S9(7)           COMP-3.                  
009900*                                 LEVERERAT ANTAL STYCK                   
010000*                                 DELIVERED QUANTITY                      
010100     03 GL-FLSPECPR          PIC X.                                       
010200*                                 SPECIALPRISFLAGGA                       
010300*                                 SPECIAL PRICE FLAG                      
010400     03 GL-KDANMORS          PIC X(2).                                    
010500*                                 ORSAK TILL LEVERANSANMÄRKNING           
010600*                                 DISCREPANCY REPORT REASON CODE          
010700     03 GL-IDDC              PIC X(2).                                    
010800*                                 IDENTIFIERARE LAGER                     
010900*                                 WAREHOUSE IDENTIFIER                    
011000     03 GL-SUNTO             PIC S9(11)V9(2)     COMP-3.                  
011100*                                 TOTAL SALES AMOUNT EXCL. VAT            
011200     03 GL-SUBTO             PIC S9(11)V9(2)     COMP-3.                  
011300*                                 TOTAL SALES AMOUNT INCL. VAT            
011400     03 GL-SUVAT-BILLIT      PIC S9(11)V9(2)     COMP-3.                  
011500*                                 SUMMERAT MOMSVÄRDE PER RAD              
011600*                                 TOTAL VAT VALUE PER LINE                
011700     03 GL-IDACCNT           OCCURS 4 TIMES                               
011800                             PIC X(15).                                   
011900*                                 KONTOFÄLT                               
012000*                                 ACCOUNT FIELD                           
012100     03 GL-KDFRAKT           PIC S9(3)           COMP-3.                  
012200*                                 FRAKTSÄTT DC TILL KUND                  
012300*                                 FREIGHT CODE                            
012400     03 GL-KDVAT             PIC X(2).                                    
012500*                                 MOMSKOD                                 
012600*                                 VAT CODE                                
012700     03 GL-IDSYSTEM-SEND     PIC X(4).                                    
012800*                                 VOLVO SÄNDANDE SYSTEM                   
012900*                                 VOLVO SENDING SYSTEM                    
013000     03 GL-KDBETALV          PIC X(4).                                    
013100*                                 BETALNINGSVILLKOR KUNDRESKONTRA         
013200*                                 TERMS OF PAYMENT                        
013300     03 GL-REVALUTA          PIC S9(5)           COMP-3.                  
013400*                                 OMRÄKNINGSTAL FÖR VALUTA                
013500*                                 CONVERT VALUE FOR CURRENCY CODE         
013600     03 GL-KDVALISO-INV      PIC X(3).                                    
013700*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
013800*                                 CURRENCY CODE BY ISO-STANDARD.          
013900     03 GL-PRKURS-INV        PIC S9(6)V9(5)      COMP-3.                  
014000*                                 VALUTAKURS                              
014100*                                 CURRENCY EXCHANGE RATE                  
014200     03 GL-REVALUTA-INV      PIC S9(5)           COMP-3.                  
014300*                                 OMRÄKNINGSTAL FÖR VALUTA                
014400*                                 CONVERT VALUE FOR CURRENCY CODE         
014500*** END OF VILMAII-COPY LENGTH= 459 BYTES                                 
