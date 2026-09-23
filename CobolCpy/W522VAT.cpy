000100 01  W522VAT.                                                             
000200*                                 VAT DATA FOR DISTRIBUTION               
000300     03 TIAA                 PIC 9(2).                                    
000400*                                 ÅR    (ÅÅ)                              
000500*                                 YEAR  (YY)                              
000600     03 TIRP                 PIC 9(2).                                    
000700*                                 REDOVISNINGSPERIOD                      
000800*                                 12 PER ÅR                               
000900*                                 ACCOUNTING PERIOD                       
001000*                                 12 PER YEAR                             
001100     03 IDLANDX3-BET         PIC X(3).                                    
001200*                                 LANDKOD BETALANDE KUND ETC              
001300*                                 COUNTRY CODE PAYING CUSTOMER ET         
001400*                                 C                                       
001500     03 IDLANDX3-SEND        PIC X(3).                                    
001600*                                 LANDKOD SÄNDANDE LAND                   
001700*                                 COUNTRY CODE SENDING COUNTRY            
001800     03 KDVALISO             PIC X(3).                                    
001900*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
002000*                                 CURRENCY CODE BY ISO-STANDARD.          
002100     03 PRKURS               PIC 9(6)V9(5).                               
002200*                                 VALUTAKURS                              
002300*                                 CURRENCY EXCHANGE RATE                  
002400     03 IDVAT-LEG            PIC X(17).                                   
002500*                                 MOMSREGISTRERINGSNUMMER LEGAL S         
002600*                                 ÄLJARE                                  
002700*                                 VAT REGISTRATION LEGAL PAYER            
002800     03 IDVAT-RESP           PIC X(17).                                   
002900*                                 MOMSREGISTRERINGSNUMMER ANSVARI         
003000*                                 G AVD                                   
003100*                                 VAT REGISTRATION RESPONSIBLE DP         
003200*                                 T                                       
003300     03 IDVAT-BET            PIC X(17).                                   
003400*                                 MOMSREGISTRERINGSNUMMER BETALAR         
003500*                                 E                                       
003600*                                 VAT REGISTRATION NUMBER PAYER           
003700     03 IDPARTNR             PIC X(9).                                    
003800*                                 PARTNERNUMMER                           
003900*                                 PARTNER NO                              
004000     03 KDFINDOC             PIC X(4).                                    
004100*                                 TYP FINANSIELLT DOKUMENT                
004200*                                 FINANCIAL DOCUMENT TYPE                 
004300     03 DAFINDOC             PIC 9(8).                                    
004400*                                 DOKUMENT DATUM (ÅÅÅÅMMDD)               
004500*                                 INVOICING DATE   (YYYYMMDD)             
004600     03 IDFINDOC             PIC 9(9).                                    
004700*                                 FINANSIELLT DOKUMENT ID                 
004800*                                 FINANCIAL DOCUMENT ID                   
004900     03 SUNTO-TOT            PIC 9(11)V9(2).                              
005000*                                 TOTAL SALES AMOUNT EXCL. VAT            
005100     03 SUVAT-BILLIT-TOT     PIC 9(11)V9(2).                              
005200*                                 SUMMERAT MOMSVÄRDE                      
005300*                                 TOTAL VAT VALUE                         
005400     03 KDVALISO-SEND        PIC X(3).                                    
005500*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
005600*                                 CURRENCY CODE BY ISO-STANDARD.          
005700     03 PRKURS-SEND          PIC 9(6)V9(5).                               
005800*                                 VALUTAKURS                              
005900*                                 CURRENCY EXCHANGE RATE                  
006000     03 IDDISTR              PIC 9(5).                                    
006100*                                 DISTRIKTNUMMER                          
006200*                                 DISTRICT NUMBER                         
006300     03 IDKUNDNR             PIC 9(7).                                    
006400*                                 KUNDNUMMER                              
006500*                                 CUSTOMER NO                             
006600*** END OF VILMAII-COPY LENGTH= 157 BYTES                                 
