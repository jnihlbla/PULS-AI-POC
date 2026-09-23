000100 01  WF2103I1.                                                            
000200*                                 FEEDBACK DATA TO SYSTEM PULS-VA         
000300*                                 T                                       
000400     03 DAEXDAT              PIC 9(8).                                    
000500*                                 EXEKVERINGSDATUM (ÅÅÅÅMMDD)             
000600*                                 EXECUTION DATE (YYYYMMDD)               
000700     03 TIEXTID              PIC 9(6).                                    
000800*                                 EXEKVERINGSTIDPUNKT                     
000900*                                 EXECUTION TIME                          
001000     03 IDPTYP               PIC X(3).                                    
001100*                                 POSTTYP                                 
001200*                                 RECORD TYPE                             
001300     03 VAT-DATA.                                                         
001400        05 IDLANDX3-BET      PIC X(3).                                    
001500*                                 LANDKOD BETALANDE KUND ETC              
001600*                                 COUNTRY CODE PAYING CUSTOMER ET         
001700*                                 C                                       
001800        05 IDLANDX3-SEND     PIC X(3).                                    
001900*                                 LANDKOD SÄNDANDE LAND                   
002000*                                 COUNTRY CODE SENDING COUNTRY            
002100        05 KDVALISO          PIC X(3).                                    
002200*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
002300*                                 CURRENCY CODE BY ISO-STANDARD.          
002400        05 PRKURS            PIC 9(6)V9(5).                               
002500*                                 VALUTAKURS                              
002600*                                 CURRENCY EXCHANGE RATE                  
002700        05 IDVAT-LEG         PIC X(17).                                   
002800*                                 MOMSREGISTRERINGSNUMMER LEGAL S         
002900*                                 ÄLJARE                                  
003000*                                 VAT REGISTRATION LEGAL PAYER            
003100        05 IDVAT-RESP        PIC X(17).                                   
003200*                                 MOMSREGISTRERINGSNUMMER ANSVARI         
003300*                                 G AVD                                   
003400*                                 VAT REGISTRATION RESPONSIBLE DP         
003500*                                 T                                       
003600        05 IDVAT-BET         PIC X(17).                                   
003700*                                 MOMSREGISTRERINGSNUMMER BETALAR         
003800*                                 E                                       
003900*                                 VAT REGISTRATION NUMBER PAYER           
004000        05 IDPARTNR          PIC X(9).                                    
004100*                                 PARTNERNUMMER                           
004200*                                 PARTNER NO                              
004300        05 KDFINDOC          PIC X(4).                                    
004400*                                 TYP FINANSIELLT DOKUMENT                
004500*                                 FINANCIAL DOCUMENT TYPE                 
004600        05 DAFINDOC          PIC 9(8).                                    
004700*                                 DOKUMENT DATUM (ÅÅÅÅMMDD)               
004800*                                 INVOICING DATE   (YYYYMMDD)             
004900        05 IDFINDOC          PIC 9(9).                                    
005000*                                 FINANSIELLT DOKUMENT                    
005100*                                 FINANCIAL DOCUMENT                      
005200        05 SUNTO-TOT         PIC 9(11)V9(2).                              
005300*                                 TOTAL SALES AMOUNT EXCL. VAT            
005400        05 SUVAT-BILLIT-TOT  PIC 9(11)V9(2).                              
005500*                                 SUMMERAT MOMSVÄRDE                      
005600*                                 TOTAL VAT VALUE                         
005700        05 IDEXCUST-1        PIC X(15).                                   
005800*                                 EXTERNT KUNDID                          
005900*                                 EXTERNAL CUSTOMER ID                    
006000        05 IDEXCUST-2        PIC X(15).                                   
006100*                                 EXTERNT KUNDID                          
006200*                                 EXTERNAL CUSTOMER ID                    
006300*** END OF VILMAII-COPY LENGTH= 174 BYTES                                 
