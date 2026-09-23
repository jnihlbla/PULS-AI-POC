000100 01  REQU-WF0252I1.                                                       
000200*                                 REQUEST-COPYTEXT PGM WF0252             
000300*                                 FINANCIAL CUSTOMER MAINTENANCE          
000400     03 REQU-IDLEGSEL-KEY    PIC X(4).                                    
000500*                                 FAKTURERANDE FÖRETAG TEX VCCS           
000600*                                 LEGAL SELLER IDENTITY                   
000700     03 REQU-IDPARTNR-KEY    PIC X(9).                                    
000800*                                 FINANCIELL KUND                         
000900*                                 FINANCIAL CUST                          
001000     03 REQU-KDSTATUS-KEY    PIC 9(3).                                    
001100*                                 STATUSKOD          KDSTATUS-002         
001200     03 REQU-FLCOMING        PIC X.                                       
001300*                                 ALLMÄN FLAGGA                           
001400*                                 GENERAL FLAG                            
001500     03 REQU-BEBET-NAME1     PIC X(35).                                   
001600*                                 DEL AV BETALNINGSANSVARIGS NAMN         
001700*                                 PART OF FINANCIAL CUSTOMER NAME         
001800     03 REQU-BEBET-NAME2     PIC X(35).                                   
001900*                                 DEL AV BETALNINGSANSVARIGS NAMN         
002000*                                 PART OF FINANCIAL CUSTOMER NAME         
002100     03 REQU-BEBET-NAME3     PIC X(35).                                   
002200*                                 DEL AV BETALNINGSANSVARIGS NAMN         
002300*                                 PART OF FINANCIAL CUSTOMER NAME         
002400     03 REQU-BEBET-NAME4     PIC X(35).                                   
002500*                                 DEL AV BETALNINGSANSVARIGS NAMN         
002600*                                 PART OF FINANCIAL CUSTOMER NAME         
002700     03 REQU-ADBET-STREET    PIC X(35).                                   
002800*                                 BETALARENS GATUADRESS                   
002900*                                 PAYER ADDRESS STREET                    
003000     03 REQU-ADBET-BOX       PIC X(10).                                   
003100*                                 BOXADRESS BETALNINGSANSVARIG            
003200*                                 PAYER BOX ADDRESS                       
003300     03 REQU-ADBET-PCODE     PIC X(10).                                   
003400*                                 BETALARENS STADSADRESS POSTNR           
003500*                                 PAYER ADDRESS POSTAL CODE               
003600     03 REQU-ADBET-CITY      PIC X(35).                                   
003700*                                 BETALARENS STADSADRESS                  
003800*                                 PAYER ADDRESS CITY                      
003900     03 REQU-IDLANDX3        PIC X(3).                                    
004000*                                 3-STÄLLIG LANDSBETECKNINGSKOD           
004100*                                 3-LETTER CODE FOR COUNTRY.              
004200     03 REQU-IDSPRAK         PIC X(2).                                    
004300*                                 2-STÄLLIG ISO SPRÅKKOD                  
004400*                                 2-LETTER ISO LANGUAGE CODE              
004500     03 REQU-IDTFN           PIC X(20).                                   
004600*                                 TELEFONNUMMER EXTERNT                   
004700*                                 TELEPHONE NUMBER  EXTERNAL              
004800     03 REQU-IDTFX           PIC X(20).                                   
004900*                                 TELEFAXNUMMER                           
005000*                                 FAXNUMBER                               
005100     03 REQU-IDMAIL          PIC X(60).                                   
005200*                                 MAIL ADRESS                             
005300*                                 MAIL ADDRESS                            
005400     03 REQU-IDLEVNR-AP      PIC X(10).                                   
005500*                                 LEVERANTÖRNUMMER                        
005600*                                 SUPPLIER NUMBER (VENDORNUMBER)          
005700     03 REQU-IDVAT           PIC X(17).                                   
005800*                                 MOMSREGISTRERINGSNUMMER                 
005900*                                 VAT REGISTRATION NUMBER                 
006000     03 REQU-KDVALISO        PIC X(3).                                    
006100*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
006200*                                 CURRENCY CODE BY ISO-STANDARD.          
006300     03 REQU-FLRATE          PIC X.                                       
006400*                                 A RATE BETWEEN TWO LOCAL CURREN         
006500*                                 CIES                                    
006600*                                 A RATE BETWEEN TWO LOCAL CURREN         
006700*                                 CIES                                    
006800     03 REQU-FLLOCCUR        PIC X.                                       
006900*                                 ANGER ATT FAKTURAN RÄKNAS               
007000*                                 OM TILL KUNDENS VALUTA (FRÅN RA         
007100*                                 D-VALUTA)                               
007200*                                 GET INVOICE IN LOCAL CURRENCY           
007300     03 REQU-KDTRADP         PIC X(4).                                    
007400*                                 TRADING PARTNER                         
007500*                                 TRADING PARTNER                         
007600     03 REQU-KDBETALV        PIC X(4).                                    
007700*                                 BETALNINGSVILLKOR KUNDRESKONTRA         
007800*                                 TERMS OF PAYMENT                        
007900     03 REQU-KDKREDSP        PIC X.                                       
008000*                                 KREDITSPÄRR PÅ BETALARE                 
008100*                                 CREDIT STOP FINANCIAL CUSTOMER          
008200     03 REQU-KDPARTTY        PIC X(3).                                    
008300*                                 TYP AV BETALARE                         
008400*                                 TYPE OF FIN.CUSTOMER                    
008500     03 REQU-KDPARTGR        PIC X(15).                                   
008600*                                 GRUPP AV BETALARE                       
008700*                                 FIN.CUSTOMER GROUP                      
008800     03 REQU-DAUPPDAT        PIC X(8).                                    
008900*                                 UPPDATERINGSDATUM  (ÅÅÅÅMMDD)           
009000*                                 UPDATING DATE     (YYYYMMDD)            
009100     03 REQU-DADELDAT        PIC X(8).                                    
009200*                                 BORTTAGSDATUM      (ÅÅÅÅMMDD)           
009300*                                 DELETION DATE     (YYYYMMDD)            
009400     03 REQU-FLFINFIL        PIC X.                                       
009500*                                 FINACIELL INFO FIL TILL KUND            
009600*                                 FINACIAL INFO FILE TO FINANCIAL         
009700*                                  CUSTOMER                               
009800     03 REQU-FLSAPBLK        PIC X.                                       
009900*                                 FLAGGA BLOCK SAP UPPDATERING            
010000*                                 BLOCK SAP UPDATING FLAG                 
010100     03 REQU-FLDECIMAL       PIC X.                                       
010200*                                 ANGER OM DECIMAL ANGES                  
010300*                                 INDICATES IF DECIMALS USED              
010400     03 REQU-FLCURINF        PIC X.                                       
010500*                                 ANGER OM VALUTA INFO VISAS              
010600*                                 INDICATES IF CURRENCY CONSIDERS         
010700     03 REQU-FLCURRND        PIC X.                                       
010800*                                 ANGER OM BELOPP AVRUNDAS                
010900*                                 INDICATES IF AMOUNT ROUNDED             
011000     03 REQU-KDVALTYP        PIC X.                                       
011100*                                 KURSENS PER A=ÅR/M=MÅNAD/D=DAG          
011200*                                 CURRENCY PER YEAR/MONTH/DAY             
011300     03 REQU-FLDIRVAT        PIC X.                                       
011400*                                 OM VAT FÖR DIRLEV                       
011500*                                 IF VAT FOR DDGS                         
011600*** END OF VILMAII-COPY LENGTH= 434 BYTES                                 
