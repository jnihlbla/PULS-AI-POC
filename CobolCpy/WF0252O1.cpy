000100 01  RESP-WF0252O1.                                                       
000200*                                 RESPONS-COPYTEXT FÖR PGM WF0252         
000300*                                 FINANCIAL CUSTOMER MAINTENANCE          
000400     03 RESP-IDLEGSEL-KEY    PIC X(4).                                    
000500*                                 FAKTURERANDE FÖRETAG TEX VCCS           
000600*                                 LEGAL SELLER IDENTITY                   
000700     03 RESP-IDPARTNR-KEY    PIC X(9).                                    
000800*                                 FINANCIELL KUND                         
000900*                                 FINANCIAL CUST                          
001000     03 RESP-KDSTATUS-KEY    PIC 9(3).                                    
001100*                                 STATUSKOD          KDSTATUS-002         
001200     03 RESP-BELEGRAD-1      PIC X(35).                                   
001300*                                 DEL AV LEGAL SELLER NAMN                
001400*                                 PART OF LEGAL SELLER NAME               
001500     03 RESP-FLCOMING        PIC X.                                       
001600*                                 ALLMÄN FLAGGA                           
001700*                                 GENERAL FLAG                            
001800     03 RESP-BEBET-NAME1     PIC X(35).                                   
001900*                                 DEL AV BETALNINGSANSVARIGS NAMN         
002000*                                 PART OF FINANCIAL CUSTOMER NAME         
002100     03 RESP-BEBET-NAME2     PIC X(35).                                   
002200*                                 DEL AV BETALNINGSANSVARIGS NAMN         
002300*                                 PART OF FINANCIAL CUSTOMER NAME         
002400     03 RESP-BEBET-NAME3     PIC X(35).                                   
002500*                                 DEL AV BETALNINGSANSVARIGS NAMN         
002600*                                 PART OF FINANCIAL CUSTOMER NAME         
002700     03 RESP-BEBET-NAME4     PIC X(35).                                   
002800*                                 DEL AV BETALNINGSANSVARIGS NAMN         
002900*                                 PART OF FINANCIAL CUSTOMER NAME         
003000     03 RESP-ADBET-STREET    PIC X(35).                                   
003100*                                 BETALARENS GATUADRESS                   
003200*                                 PAYER ADDRESS STREET                    
003300     03 RESP-ADBET-BOX       PIC X(10).                                   
003400*                                 BOXADRESS BETALNINGSANSVARIG            
003500*                                 PAYER BOX ADDRESS                       
003600     03 RESP-ADBET-PCODE     PIC X(10).                                   
003700*                                 BETALARENS STADSADRESS POSTNR           
003800*                                 PAYER ADDRESS POSTAL CODE               
003900     03 RESP-ADBET-CITY      PIC X(35).                                   
004000*                                 BETALARENS STADSADRESS                  
004100*                                 PAYER ADDRESS CITY                      
004200     03 RESP-IDLANDX3        PIC X(3).                                    
004300*                                 3-STÄLLIG LANDSBETECKNINGSKOD           
004400*                                 3-LETTER CODE FOR COUNTRY.              
004500     03 RESP-IDSPRAK         PIC X(2).                                    
004600*                                 2-STÄLLIG ISO SPRÅKKOD                  
004700*                                 2-LETTER ISO LANGUAGE CODE              
004800     03 RESP-IDTFN           PIC X(20).                                   
004900*                                 TELEFONNUMMER EXTERNT                   
005000*                                 TELEPHONE NUMBER  EXTERNAL              
005100     03 RESP-IDTFX           PIC X(20).                                   
005200*                                 TELEFAXNUMMER                           
005300*                                 FAXNUMBER                               
005400     03 RESP-IDMAIL          PIC X(60).                                   
005500*                                 MAIL ADRESS                             
005600*                                 MAIL ADDRESS                            
005700     03 RESP-IDLEVNR-AP      PIC X(10).                                   
005800*                                 LEVERANTÖRNUMMER                        
005900*                                 SUPPLIER NUMBER (VENDORNUMBER)          
006000     03 RESP-IDVAT           PIC X(17).                                   
006100*                                 MOMSREGISTRERINGSNUMMER                 
006200*                                 VAT REGISTRATION NUMBER                 
006300     03 RESP-KDVALISO        PIC X(3).                                    
006400*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
006500*                                 CURRENCY CODE BY ISO-STANDARD.          
006600     03 RESP-FLRATE          PIC X.                                       
006700*                                 A RATE BETWEEN TWO LOCAL CURREN         
006800*                                 CIES                                    
006900*                                 A RATE BETWEEN TWO LOCAL CURREN         
007000*                                 CIES                                    
007100     03 RESP-FLLOCCUR        PIC X.                                       
007200*                                 ANGER ATT FAKTURAN RÄKNAS               
007300*                                 OM TILL KUNDENS VALUTA (FRÅN RA         
007400*                                 D-VALUTA)                               
007500*                                 GET INVOICE IN LOCAL CURRENCY           
007600     03 RESP-KDTRADP         PIC X(4).                                    
007700*                                 TRADING PARTNER                         
007800*                                 TRADING PARTNER                         
007900     03 RESP-KDBETALV        PIC X(4).                                    
008000*                                 BETALNINGSVILLKOR KUNDRESKONTRA         
008100*                                 TERMS OF PAYMENT                        
008200     03 RESP-KDKREDSP        PIC X.                                       
008300*                                 KREDITSPÄRR PÅ BETALARE                 
008400*                                 CREDIT STOP FINANCIAL CUSTOMER          
008500     03 RESP-KDPARTTY        PIC X(3).                                    
008600*                                 TYP AV BETALARE                         
008700*                                 TYPE OF FIN.CUSTOMER                    
008800     03 RESP-KDPARTGR        PIC X(15).                                   
008900*                                 GRUPP AV BETALARE                       
009000*                                 FIN.CUSTOMER GROUP                      
009100     03 RESP-DAREGDAT        PIC Z(8).                                    
009200*                                 REGISTRERINGSDATUM (ÅÅÅÅMMDD)           
009300*                                 REGISTRATION DATE (YYYYMMDD)            
009400     03 RESP-DAUPPDAT        PIC Z(8).                                    
009500*                                 UPPDATERINGSDATUM  (ÅÅÅÅMMDD)           
009600*                                 UPDATING DATE     (YYYYMMDD)            
009700     03 RESP-DADELDAT        PIC Z(8).                                    
009800*                                 BORTTAGSDATUM      (ÅÅÅÅMMDD)           
009900*                                 DELETION DATE     (YYYYMMDD)            
010000     03 RESP-IDUSER          PIC X(8).                                    
010100*                                 ANVÄNDARENS SÄKERHETS ID                
010200*                                 USER SECURITY-IDENTITY                  
010300     03 RESP-FLFINFIL        PIC X.                                       
010400*                                 FINACIELL INFO FIL TILL KUND            
010500*                                 FINACIAL INFO FILE TO FINANCIAL         
010600*                                  CUSTOMER                               
010700     03 RESP-FLSAPBLK        PIC X.                                       
010800*                                 FLAGGA BLOCK SAP UPPDATERING            
010900*                                 BLOCK SAP UPDATING FLAG                 
011000     03 RESP-FLDECIMAL       PIC X.                                       
011100*                                 ANGER OM DECIMAL ANGES                  
011200*                                 INDICATES IF DECIMALS USED              
011300     03 RESP-FLCURINF        PIC X.                                       
011400*                                 ANGER OM VALUTA INFO VISAS              
011500*                                 INDICATES IF CURRENCY CONSIDERS         
011600     03 RESP-FLCURRND        PIC X.                                       
011700*                                 ANGER OM BELOPP AVRUNDAS                
011800*                                 INDICATES IF AMOUNT ROUNDED             
011900     03 RESP-KDVALTYP        PIC X.                                       
012000*                                 KURSENS PER A=ÅR/M=MÅNAD/D=DAG          
012100*                                 CURRENCY PER YEAR/MONTH/DAY             
012200     03 RESP-FLDIRVAT        PIC X.                                       
012300*                                 OM VAT FÖR DIRLEV                       
012400*                                 IF VAT FOR DDGS                         
012500*** END OF VILMAII-COPY LENGTH= 485 BYTES                                 
