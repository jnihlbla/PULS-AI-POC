000100 01  BET-WDB101.                                                          
000200*                                 KUNDREGISTER                            
000300*                                 BETALARINFORMATION                      
000400*                                 FYSISK NYCKEL: WDB101KY                 
000500*                                 (IDPARTNR + IDFTG)                      
000600     03 BET-IDPARTNR         PIC X(9).                                    
000700*                                 FINANCIELL KUND                         
000800*                                 FINANCIAL CUST                          
000900     03 BET-IDFTG            PIC 9(2).                                    
001000*                                 FÖRETAGSID EKONOM REDOVISNING           
001100*                                 COMPANY IDENTITY ACCOUNTING             
001200     03 BET-IDLANDX2         PIC X(2).                                    
001300*                                 2-STÄLLIG LANDSBETECKNINGSKOD           
001400*                                 2-LETTER CODE FOR COUNTRY               
001500     03 BET-IDVAT            PIC X(17).                                   
001600*                                 MOMSREGISTRERINGSNUMMER                 
001700*                                 VAT REGISTRATION NUMBER                 
001800     03 BET-KDBETVIL         PIC X(4).                                    
001900*                                 BETALNINGSVILLKOR SAP                   
002000*                                 TERMS OF PAYMENT                        
002100     03 BET-KDKREDSP         PIC X.                                       
002200*                                 KREDITSPÄRR PÅ BETALARE                 
002300*                                 CREDIT STOP FINANCIAL CUSTOMER          
002400     03 BET-KDTRADP          PIC X(4).                                    
002500*                                 TRADING PARTNER                         
002600*                                 TRADING PARTNER                         
002700     03 BET-KDVALISO         PIC X(3).                                    
002800*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
002900*                                 CURRENCY CODE BY ISO-STANDARD.          
003000     03 BET-TISTADAT         PIC S9(7)           COMP-3.                  
003100*                                 GENERELLT STARTDATUM                    
003200*                                 GENERAL START DATE                      
003300     03 BET-TIUPPDAT         PIC S9(7)           COMP-3.                  
003400*                                 UPPDATERINGSDATUM  (ÅÅMMDD)             
003500*                                 UPDATING DATE     (YYMMDD)              
003600     03 BET-TISTODAT         PIC S9(7)           COMP-3.                  
003700*                                 GENERELLT STOPPDATUM                    
003800*                                 GENERAL STOP DATE YYMMDD                
003900     03 BET-IDUSER           PIC X(8).                                    
004000*                                 ANVÄNDARENS SÄKERHETS ID                
004100*                                 USER SECURITY-IDENTITY                  
004200     03 BET-RELANDCO         PIC S9(3)V9(2)      COMP-3.                  
004300*                                 LANDING COST PROCENT                    
004400*                                 LANDING COST PERCENT                    
004500     03 BET-IDPROMR.                                                      
004600*                                 PRISOMRÅDE (RABATTSTRUKTUR)             
004700*                                 PRICE AREA                              
004800        05 BET-IDMARKBO      PIC X.                                       
004900*                                 MARKNADSBOLAGSKOD                       
005000*                                 MARKET COMPANY CODE                     
005100        05 BET-IDPROMRN      PIC X(2).                                    
005200*                                 PRISOMRÅDE LÖPNUMMER                    
005300*                                 PRICE AREA SERIALNUMBER                 
005400     03 BET-ADBETRAD-1       PIC X(35).                                   
005500*                                 ADRESSRAD BETALNINGSANSVARIG            
005600*                                 PART OF FINANCIAL CUSTOMER ADDR         
005700*                                 ESS                                     
005800     03 BET-ADBETRAD-2       PIC X(35).                                   
005900*                                 ADRESSRAD BETALNINGSANSVARIG            
006000*                                 PART OF FINANCIAL CUSTOMER ADDR         
006100*                                 ESS                                     
006200     03 BET-BELAND-SVE       PIC X(35).                                   
006300*                                 SVENSK LANDSBETECKNING                  
006400*                                 SWEDISH NAME OF COUNTRY                 
006500     03 BET-BEBETRAD-1       PIC X(35).                                   
006600*                                 DEL AV BETALNINGSANSVARIGS NAMN         
006700*                                 PART OF FINANCIAL CUSTOMER NAME         
006800     03 BET-BEBETRAD-2       PIC X(35).                                   
006900*                                 DEL AV BETALNINGSANSVARIGS NAMN         
007000*                                 PART OF FINANCIAL CUSTOMER NAME         
007100     03 BET-BEBETVIL         PIC X(30).                                   
007200*                                 BETALNINGSVILLKORSTEXT                  
007300*                                 TERMS OF PAYMENT TEXT                   
007400     03 BET-FLLOCCUR         PIC X.                                       
007500*                                 ANGER ATT FAKTURAN RÄKNAS               
007600*                                 OM TILL KUNDENS VALUTA (FRÅN RA         
007700*                                 D-VALUTA)                               
007800*                                 GET INVOICE IN LOCAL CURRENCY           
007900     03 BET-FLRATE           PIC X.                                       
008000*                                 A RATE BETWEEN TWO LOCAL CURREN         
008100*                                 CIES                                    
008200*                                 A RATE BETWEEN TWO LOCAL CURREN         
008300*                                 CIES                                    
008400     03 BET-KDVALTYP         PIC X.                                       
008500*                                 KURSENS PER A=ÅR/M=MÅNAD/D=DAG          
008600*                                 CURRENCY PER YEAR/MONTH/DAY             
008700     03 BET-RESERAVG         PIC S9(2)V9(1)      COMP-3.                  
008800*                                 SERVICE AVGIFT (%)                      
008900*                                 SERVICE FEE (%)                         
009000     03 BET-FLDIRVAT         PIC X.                                       
009100*                                 OM VAT FÖR DIRLEV                       
009200*                                 IF VAT FOR DDGS                         
009300     03 BET-FLINVGRP         PIC X.                                       
009400*                                 FLAGGA FÖR GRUPPFAKTURA/INKÖP           
009500*                                 FLAG FOR GROUP INVOICING/PO             
009600     03 BET-FLARTRAB         PIC X.                                       
009700*                                 ALTERNATIV RABATTKOD ART.PRIS           
009800*                                 ALTERNATE DISC. CODE FOR PART           
009900     03 BET-IDLEVNR-FIN      PIC X(5).                                    
010000*                                 LEVERANTÖRENS FINANSIELLA KOD           
010100*                                 SUPPLIERS FINANCIAL CODE                
010200     03 BET-FILLER           PIC X(4).                                    
010300*** END OF VILMAII-COPY LENGTH= 290 BYTES                                 
