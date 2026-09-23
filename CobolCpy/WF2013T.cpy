000100 01  VAT-WF2013T.                                                         
000200*                                 VAT DATA                                
000300     03 VAT-IDLEGSEL         OCCURS 100 TIMES                             
000400                             PIC X(4).                                    
000500*                                 FAKTURERANDE FÖRETAG TEX VCCS           
000600*                                 LEGAL SELLER IDENTITY                   
000700     03 VAT-DAEXDAT          OCCURS 100 TIMES                             
000800                             PIC 9(8).                                    
000900*                                 EXEKVERINGSDATUM (ÅÅÅÅMMDD)             
001000*                                 EXECUTION DATE (YYYYMMDD)               
001100     03 VAT-TIEXTID          OCCURS 100 TIMES                             
001200                             PIC S9(7)           COMP-3.                  
001300*                                 EXEKVERINGSTIDPUNKT                     
001400*                                 EXECUTION TIME                          
001500     03 VAT-IDVAT-LEG        OCCURS 100 TIMES                             
001600                             PIC X(17).                                   
001700*                                 MOMSREGISTRERINGSNUMMER LEGAL S         
001800*                                 ÄLJARE                                  
001900*                                 VAT REGISTRATION LEGAL PAYER            
002000     03 VAT-IDVAT-RESP       OCCURS 100 TIMES                             
002100                             PIC X(17).                                   
002200*                                 MOMSREGISTRERINGSNUMMER ANSVARI         
002300*                                 G AVD                                   
002400*                                 VAT REGISTRATION RESPONSIBLE DP         
002500*                                 T                                       
002600     03 VAT-IDVAT-AGENT      OCCURS 100 TIMES                             
002700                             PIC X(17).                                   
002800*                                 MOMSREGISTRERINGSNUMMER AGENT           
002900*                                 VAT REGISTRATION VAT AGENT              
003000     03 VAT-IDVAT-BET        OCCURS 100 TIMES                             
003100                             PIC X(17).                                   
003200*                                 MOMSREGISTRERINGSNUMMER BETALAR         
003300*                                 E                                       
003400*                                 VAT REGISTRATION NUMBER PAYER           
003500     03 VAT-IDLANDX3-SEND    OCCURS 100 TIMES                             
003600                             PIC X(3).                                    
003700*                                 LANDKOD SÄNDANDE LAND                   
003800*                                 COUNTRY CODE SENDING COUNTRY            
003900     03 VAT-IDLANDX3-BET     OCCURS 100 TIMES                             
004000                             PIC X(3).                                    
004100*                                 LANDKOD BETALANDE KUND ETC              
004200*                                 COUNTRY CODE PAYING CUSTOMER ET         
004300*                                 C                                       
004400     03 VAT-KDVALISO         OCCURS 100 TIMES                             
004500                             PIC X(3).                                    
004600*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
004700*                                 CURRENCY CODE BY ISO-STANDARD.          
004800     03 VAT-PRKURS           OCCURS 100 TIMES                             
004900                             PIC S9(6)V9(5)      COMP-3.                  
005000*                                 VALUTAKURS                              
005100*                                 CURRENCY EXCHANGE RATE                  
005200     03 VAT-KDFINDOC         OCCURS 100 TIMES                             
005300                             PIC X(4).                                    
005400*                                 TYP FINANSIELLT DOKUMENT                
005500*                                 FINANCIAL DOCUMENT TYPE                 
005600     03 VAT-DAFINDOC         OCCURS 100 TIMES                             
005700                             PIC 9(8).                                    
005800*                                 DOKUMENT DATUM (ÅÅÅÅMMDD)               
005900*                                 INVOICING DATE   (YYYYMMDD)             
006000     03 VAT-IDFINDOC         OCCURS 100 TIMES                             
006100                             PIC S9(9)           COMP-3.                  
006200*                                 FINANSIELLT DOKUMENT ID                 
006300*                                 FINANCIAL DOCUMENT ID                   
006400     03 VAT-IDPARTNR         OCCURS 100 TIMES                             
006500                             PIC X(9).                                    
006600*                                 PARTNERNUMMER                           
006700*                                 PARTNER NO                              
006800     03 VAT-SUNTO-TOT        OCCURS 100 TIMES                             
006900                             PIC S9(11)V9(2)     COMP-3.                  
007000*                                 TOTAL SALES AMOUNT EXCL. VAT            
007100     03 VAT-SUVAT-BILLIT-TOT OCCURS 100 TIMES                             
007200                             PIC S9(11)V9(2)     COMP-3.                  
007300*                                 SUMMERAT MOMSVÄRDE                      
007400*                                 TOTAL VAT VALUE                         
007500     03 VAT-IDEXCUST-1       OCCURS 100 TIMES                             
007600                             PIC X(15).                                   
007700*                                 EXTERNT KUNDID                          
007800*                                 EXTERNAL CUSTOMER ID                    
007900     03 VAT-IDEXCUST-2       OCCURS 100 TIMES                             
008000                             PIC X(15).                                   
008100*                                 EXTERNT KUNDID                          
008200*                                 EXTERNAL CUSTOMER ID                    
008300     03 VAT-IDEXCUST-3       OCCURS 100 TIMES                             
008400                             PIC X(15).                                   
008500*                                 EXTERNT KUNDID                          
008600*                                 EXTERNAL CUSTOMER ID                    
008700*** END OF VILMAII-COPY LENGTH= 18400 BYTES                               
