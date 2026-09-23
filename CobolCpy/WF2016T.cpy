000100 01  AR-WF2016T.                                                          
000200*                                 ACCOUNTS RECEIVABLE DATA                
000300     03 AR-IDLEGSEL          OCCURS 100 TIMES                             
000400                             PIC X(4).                                    
000500*                                 FAKTURERANDE FÖRETAG TEX VCCS           
000600*                                 LEGAL SELLER IDENTITY                   
000700     03 AR-KDVALISO          OCCURS 100 TIMES                             
000800                             PIC X(3).                                    
000900*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
001000*                                 CURRENCY CODE BY ISO-STANDARD.          
001100     03 AR-PRKURS            OCCURS 100 TIMES                             
001200                             PIC S9(6)V9(5)      COMP-3.                  
001300*                                 VALUTAKURS                              
001400*                                 CURRENCY EXCHANGE RATE                  
001500     03 AR-IDLANDX3-SEND     OCCURS 100 TIMES                             
001600                             PIC X(3).                                    
001700*                                 LANDKOD SÄNDANDE LAND                   
001800*                                 COUNTRY CODE SENDING COUNTRY            
001900     03 AR-IDLANDX3-BET      OCCURS 100 TIMES                             
002000                             PIC X(3).                                    
002100*                                 LANDKOD BETALANDE KUND ETC              
002200*                                 COUNTRY CODE PAYING CUSTOMER ET         
002300*                                 C                                       
002400     03 AR-IDPARTNR          OCCURS 100 TIMES                             
002500                             PIC X(9).                                    
002600*                                 PARTNERNUMMER                           
002700*                                 PARTNER NO                              
002800     03 AR-KDFINDOC          OCCURS 100 TIMES                             
002900                             PIC X(4).                                    
003000*                                 TYP FINANSIELLT DOKUMENT                
003100*                                 FINANCIAL DOCUMENT TYPE                 
003200     03 AR-DAFINDOC          OCCURS 100 TIMES                             
003300                             PIC 9(8).                                    
003400*                                 DOKUMENT DATUM (ÅÅÅÅMMDD)               
003500*                                 INVOICING DATE   (YYYYMMDD)             
003600     03 AR-IDFINDOC          OCCURS 100 TIMES                             
003700                             PIC S9(9)           COMP-3.                  
003800*                                 FINANSIELLT DOKUMENT ID                 
003900*                                 FINANCIAL DOCUMENT ID                   
004000     03 AR-SUNTO-SERV        OCCURS 100 TIMES                             
004100                             PIC S9(11)V9(2)     COMP-3.                  
004200*                                 TOTAL SALES AMOUNT SERVICES EXC         
004300*                                 L. VAT                                  
004400     03 AR-SUNTO-PART        OCCURS 100 TIMES                             
004500                             PIC S9(11)V9(2)     COMP-3.                  
004600*                                 TOTAL SALES AMOUNT PARTS EXCL.          
004700*                                 VAT                                     
004800     03 AR-SUBTO-SERV        OCCURS 100 TIMES                             
004900                             PIC S9(11)V9(2)     COMP-3.                  
005000*                                 TOTAL SALES AMOUNT SERVICES INC         
005100*                                 L. VAT                                  
005200     03 AR-SUBTO-PART        OCCURS 100 TIMES                             
005300                             PIC S9(11)V9(2)     COMP-3.                  
005400*                                 TOTAL SALES AMOUNT PARTS INCL.          
005500*                                 VAT                                     
005600     03 AR-SUNTO-TOT         OCCURS 100 TIMES                             
005700                             PIC S9(11)V9(2)     COMP-3.                  
005800*                                 TOTAL SALES AMOUNT EXCL. VAT            
005900     03 AR-SUBTO-TOT         OCCURS 100 TIMES                             
006000                             PIC S9(11)V9(2)     COMP-3.                  
006100*                                 TOTAL SALES AMOUNT INCL. VAT            
006200     03 AR-SUVAT-BILLIT-TOT  OCCURS 100 TIMES                             
006300                             PIC S9(11)V9(2)     COMP-3.                  
006400*                                 SUMMERAT MOMSVÄRDE                      
006500*                                 TOTAL VAT VALUE                         
006600*** END OF VILMAII-COPY LENGTH= 9400 BYTES                                
