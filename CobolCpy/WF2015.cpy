000100 01  AP-WF2015.                                                           
000200*                                 ACCOUNTS PAYABLE DATA                   
000300     03 AP-IDLEGSEL          PIC X(4).                                    
000400*                                 FAKTURERANDE FÖRETAG TEX VCCS           
000500*                                 LEGAL SELLER IDENTITY                   
000600     03 AP-KDVALISO          PIC X(3).                                    
000700*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
000800*                                 CURRENCY CODE BY ISO-STANDARD.          
000900     03 AP-PRKURS            PIC S9(6)V9(5)      COMP-3.                  
001000*                                 VALUTAKURS                              
001100*                                 CURRENCY EXCHANGE RATE                  
001200     03 AP-IDLANDX3-SEND     PIC X(3).                                    
001300*                                 LANDKOD SÄNDANDE LAND                   
001400*                                 COUNTRY CODE SENDING COUNTRY            
001500     03 AP-IDLANDX3-BET      PIC X(3).                                    
001600*                                 LANDKOD BETALANDE KUND ETC              
001700*                                 COUNTRY CODE PAYING CUSTOMER ET         
001800*                                 C                                       
001900     03 AP-IDPARTNR          PIC X(9).                                    
002000*                                 PARTNERNR                               
002100*                                 PARTNER NO                              
002200     03 AP-KDFINDOC          PIC X(4).                                    
002300*                                 TYP FINANSIELLT DOKUMENT                
002400*                                 FINANCIAL DOCUMENT TYPE                 
002500     03 AP-DAFINDOC          PIC 9(8).                                    
002600*                                 DOKUMENT DATUM (ÅÅÅÅMMDD)               
002700*                                 INVOICING DATE   (YYYYMMDD)             
002800     03 AP-IDFINDOC          PIC S9(9)           COMP-3.                  
002900*                                 FINANSIELLT DOKUMENT                    
003000*                                 FINANCIAL DOCUMENT                      
003100     03 AP-SUNTO-SERV        PIC S9(11)V9(2)     COMP-3.                  
003200*                                 TOTAL SALES AMOUNT SERVICES EXC         
003300*                                 L. VAT                                  
003400     03 AP-SUNTO-PART        PIC S9(11)V9(2)     COMP-3.                  
003500*                                 TOTAL SALES AMOUNT PARTS EXCL.          
003600*                                 VAT                                     
003700     03 AP-SUBTO-SERV        PIC S9(11)V9(2)     COMP-3.                  
003800*                                 TOTAL SALES AMOUNT SERVICES INC         
003900*                                 L. VAT                                  
004000     03 AP-SUBTO-PART        PIC S9(11)V9(2)     COMP-3.                  
004100*                                 TOTAL SALES AMOUNT PARTS INCL.          
004200*                                 VAT                                     
004300     03 AP-SUNTO-TOT         PIC S9(11)V9(2)     COMP-3.                  
004400*                                 TOTAL SALES AMOUNT EXCL. VAT            
004500     03 AP-SUBTO-TOT         PIC S9(11)V9(2)     COMP-3.                  
004600*                                 TOTAL SALES AMOUNT INCL. VAT            
004700     03 AP-SUVAT-BILLIT-TOT  PIC S9(11)V9(2)     COMP-3.                  
004800*                                 SUMMERAT MOMSVÄRDE                      
004900*                                 TOTAL VAT VALUE                         
005000*** END OF VILMAII-COPY LENGTH= 94 BYTES                                  
