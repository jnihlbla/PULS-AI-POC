000100 01  VAT-WF2013.                                                          
000200*                                 VAT DATA                                
000300     03 VAT-IDLEGSEL         PIC X(4).                                    
000400*                                 FAKTURERANDE FÖRETAG TEX VCCS           
000500*                                 LEGAL SELLER IDENTITY                   
000600     03 VAT-DAEXDAT          PIC 9(8).                                    
000700*                                 EXEKVERINGSDATUM (ÅÅÅÅMMDD)             
000800*                                 EXECUTION DATE (YYYYMMDD)               
000900     03 VAT-TIEXTID          PIC S9(7)           COMP-3.                  
001000*                                 EXEKVERINGSTIDPUNKT                     
001100*                                 EXECUTION TIME                          
001200     03 VAT-IDVAT-LEG        PIC X(17).                                   
001300*                                 MOMSREGISTRERINGSNUMMER LEGAL S         
001400*                                 ÄLJARE                                  
001500*                                 VAT REGISTRATION LEGAL PAYER            
001600     03 VAT-IDVAT-RESP       PIC X(17).                                   
001700*                                 MOMSREGISTRERINGSNUMMER ANSVARI         
001800*                                 G AVD                                   
001900*                                 VAT REGISTRATION RESPONSIBLE DP         
002000*                                 T                                       
002100     03 VAT-IDVAT-AGENT      PIC X(17).                                   
002200*                                 MOMSREGISTRERINGSNUMMER AGENT           
002300*                                 VAT REGISTRATION VAT AGENT              
002400     03 VAT-IDVAT-BET        PIC X(17).                                   
002500*                                 MOMSREGISTRERINGSNUMMER BETALAR         
002600*                                 E                                       
002700*                                 VAT REGISTRATION NUMBER PAYER           
002800     03 VAT-IDLANDX3-SEND    PIC X(3).                                    
002900*                                 LANDKOD SÄNDANDE LAND                   
003000*                                 COUNTRY CODE SENDING COUNTRY            
003100     03 VAT-IDLANDX3-BET     PIC X(3).                                    
003200*                                 LANDKOD BETALANDE KUND ETC              
003300*                                 COUNTRY CODE PAYING CUSTOMER ET         
003400*                                 C                                       
003500     03 VAT-KDVALISO         PIC X(3).                                    
003600*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
003700*                                 CURRENCY CODE BY ISO-STANDARD.          
003800     03 VAT-PRKURS           PIC S9(6)V9(5)      COMP-3.                  
003900*                                 VALUTAKURS                              
004000*                                 CURRENCY EXCHANGE RATE                  
004100     03 VAT-KDFINDOC         PIC X(4).                                    
004200*                                 TYP FINANSIELLT DOKUMENT                
004300*                                 FINANCIAL DOCUMENT TYPE                 
004400     03 VAT-DAFINDOC         PIC 9(8).                                    
004500*                                 DOKUMENT DATUM (ÅÅÅÅMMDD)               
004600*                                 INVOICING DATE   (YYYYMMDD)             
004700     03 VAT-IDFINDOC         PIC S9(9)           COMP-3.                  
004800*                                 FINANSIELLT DOKUMENT ID                 
004900*                                 FINANCIAL DOCUMENT ID                   
005000     03 VAT-IDPARTNR         PIC X(9).                                    
005100*                                 PARTNERNUMMER                           
005200*                                 PARTNER NO                              
005300     03 VAT-SUNTO-TOT        PIC S9(11)V9(2)     COMP-3.                  
005400*                                 TOTAL SALES AMOUNT EXCL. VAT            
005500     03 VAT-SUVAT-BILLIT-TOT PIC S9(11)V9(2)     COMP-3.                  
005600*                                 SUMMERAT MOMSVÄRDE                      
005700*                                 TOTAL VAT VALUE                         
005800     03 VAT-IDEXCUST-1       PIC X(15).                                   
005900*                                 EXTERNT KUNDID                          
006000*                                 EXTERNAL CUSTOMER ID                    
006100     03 VAT-IDEXCUST-2       PIC X(15).                                   
006200*                                 EXTERNT KUNDID                          
006300*                                 EXTERNAL CUSTOMER ID                    
006400     03 VAT-IDEXCUST-3       PIC X(15).                                   
006500*                                 EXTERNT KUNDID                          
006600*                                 EXTERNAL CUSTOMER ID                    
006700*** END OF VILMAII-COPY LENGTH= 184 BYTES                                 
