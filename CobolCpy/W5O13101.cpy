000100 01  MOD-W5O13101.                                                        
000200*                                 COPYTEXT FÖR MID W5O13101               
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500*                                 SCREEN NUMBER                           
000600     03 MOD-TEMFSFEL         PIC X(40).                                   
000700*                                 MFS FELMEDDELANDE                       
000800*                                 MFS ERROR MESSAGE                       
000900     03 MOD-IDPARTNR-IN      PIC X(9).                                    
001000*                                 FINANCIELL KUND                         
001100*                                 FINANCIAL CUST                          
001200     03 MOD-IDPARTNR-UT      PIC X(9).                                    
001300*                                 FINANCIELL KUND                         
001400*                                 FINANCIAL CUST                          
001500     03 MOD-IDFTG-IN         PIC X(2).                                    
001600*                                 FÖRETAGSID EKONOM REDOVISNING           
001700*                                 COMPANY IDENTITY ACCOUNTING             
001800     03 MOD-IDFTG-UT         PIC X(2).                                    
001900*                                 FÖRETAGSID EKONOM REDOVISNING           
002000*                                 COMPANY IDENTITY ACCOUNTING             
002100     03 MOD-BEBETRAD-1       PIC X(35).                                   
002200*                                 DEL AV BETALNINGSANSVARIGS NAMN         
002300*                                 PART OF FINANCIAL CUSTOMER NAME         
002400     03 MOD-TISTADAT         PIC 9(6).                                    
002500*                                 GENERELLT STARTDATUM                    
002600*                                 GENERAL START DATE                      
002700     03 MOD-BEBETRAD-2       PIC X(35).                                   
002800*                                 DEL AV BETALNINGSANSVARIGS NAMN         
002900*                                 PART OF FINANCIAL CUSTOMER NAME         
003000     03 MOD-TIUPPDAT         PIC 9(6).                                    
003100*                                 UPPDATERINGSDATUM  (ÅÅMMDD)             
003200*                                 UPDATING DATE     (YYMMDD)              
003300     03 MOD-ADBETRAD-1       PIC X(35).                                   
003400*                                 ADRESSRAD BETALNINGSANSVARIG            
003500*                                 PART OF FINANCIAL CUSTOMER ADDR         
003600*                                 ESS                                     
003700     03 MOD-TIBETUPH         PIC 9(6).                                    
003800*                                 KUND/BETALARE UPPHÖR DATUM              
003900*                                 CUSTOMER CEASES DATE                    
004000     03 MOD-ADBETRAD-2       PIC X(35).                                   
004100*                                 ADRESSRAD BETALNINGSANSVARIG            
004200*                                 PART OF FINANCIAL CUSTOMER ADDR         
004300*                                 ESS                                     
004400     03 MOD-IDUSER           PIC X(8).                                    
004500*                                 ANVÄNDARENS SÄKERHETS ID                
004600*                                 USER SECURITY-IDENTITY                  
004700     03 MOD-BELAND-SVE       PIC X(35).                                   
004800*                                 SVENSK LANDSBETECKNING                  
004900*                                 SWEDISH NAME OF COUNTRY                 
005000     03 MOD-IDLANDX2         PIC X(2).                                    
005100*                                 2-STÄLLIG LANDSBETECKNINGSKOD           
005200*                                 2-LETTER CODE FOR COUNTRY               
005300     03 MOD-KDVALISO         PIC X(3).                                    
005400*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
005500*                                 CURRENCY CODE BY ISO-STANDARD.          
005600     03 MOD-KDVALTYP         PIC X.                                       
005700*                                 KURSENS PER A=ÅR/M=MÅNAD/D=DAG          
005800*                                 CURRENCY PER YEAR/MONTH/DAY             
005900     03 MOD-KDTRADP          PIC X(4).                                    
006000*                                 TRADING PARTNER                         
006100*                                 TRADING PARTNER                         
006200     03 MOD-RESERAVG-IN-ATTR PIC X(2).                                    
006300*                                 MFS ATTRIBUTFÄLT                        
006400     03 MOD-RESERAVG-IN      PIC Z9.9.                                    
006500*                                 SERVICE AVGIFT (%)                      
006600*                                 SERVICE FEE (%)                         
006700     03 MOD-KDKREDSP         PIC X.                                       
006800*                                 KREDITSPÄRR PÅ BETALARE                 
006900*                                 CREDIT STOP FINANCIAL CUSTOMER          
007000     03 MOD-FLDIRVAT-IN-ATTR PIC X(2).                                    
007100*                                 MFS ATTRIBUTFÄLT                        
007200     03 MOD-FLDIRVAT-IN      PIC X.                                       
007300*                                 OM VAT FÖR DIRLEV                       
007400*                                 IF VAT FOR DDGS                         
007500     03 MOD-FLINVGRP-IN-ATTR PIC X(2).                                    
007600*                                 MFS ATTRIBUTFÄLT                        
007700     03 MOD-FLINVGRP-IN      PIC X.                                       
007800*                                 FLAGGA FÖR GRUPPFAKTURA/INKÖP           
007900*                                 FLAG FOR GROUP INVOICING/PO             
008000     03 MOD-IDLEVNR-FIN-IN-ATTR                                           
008100                             PIC X(2).                                    
008200*                                 MFS ATTRIBUTFÄLT                        
008300     03 MOD-IDLEVNR-FIN-IN   PIC X(5).                                    
008400*                                 LEVERANTÖRENS FINANSIELLA KOD           
008500*                                 SUPPLIERS FINANCIAL CODE                
008600     03 MOD-IDVAT            PIC X(17).                                   
008700*                                 MOMSREGISTRERINGSNUMMER                 
008800*                                 VAT REGISTRATION NUMBER                 
008900     03 MOD-KDBETVIL         PIC X(4).                                    
009000*                                 BETALNINGSVILLKOR SAP                   
009100*                                 TERMS OF PAYMENT                        
009200     03 MOD-BEBETVIL         PIC X(30).                                   
009300*                                 BETALNINGSVILLKORSTEXT                  
009400*                                 TERMS OF PAYMENT TEXT                   
009500     03 MOD-RELANDCO         PIC Z(2)9.9(2).                              
009600*                                 LANDING COST PROCENT                    
009700*                                 LANDING COST PERCENT                    
009800     03 MOD-IDPROMR.                                                      
009900*                                 PRISOMRÅDE (RABATTSTRUKTUR)             
010000*                                 PRICE AREA                              
010100        05 MOD-IDMARKBO      PIC X.                                       
010200*                                 MARKNADSBOLAGSKOD                       
010300*                                 MARKET COMPANY CODE                     
010400        05 MOD-IDPROMRN      PIC X(2).                                    
010500*                                 PRISOMRÅDE LÖPNUMMER                    
010600*                                 PRICE AREA SERIALNUMBER                 
010700     03 MOD-KDVALIS2         PIC X(3).                                    
010800*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
010900*                                 CURRENCY CODE BY ISO-STANDARD.          
011000     03 MOD-TEMFSINF         PIC X(55).                                   
011100*                                 INFORMATIONSMEDDELANDE                  
011200*                                 INFORMATION MESSAGE                     
011300*** END OF VILMAII-COPY LENGTH= 415 BYTES                                 
