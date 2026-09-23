000100 01  WF10CUST.                                                            
000200*                                 EXTERN FINANSIELL KUNDINFO FRÅN         
000300*                                  SAP R/3                                
000400     03 IDLEGSEL             PIC X(4).                                    
000500*                                 FAKTURERANDE FÖRETAG TEX VCCS           
000600*                                 LEGAL SELLER IDENTITY                   
000700     03 IDPARTNR             PIC X(9).                                    
000800*                                 PARTNERNUMMER                           
000900*                                 PARTNER NO                              
001000     03 KDSTATUS             PIC S9(3)           COMP-3.                  
001100*                                 STATUSKOD          KDSTATUS-002         
001200     03 IDALPHA              PIC X(10).                                   
001300*                                 ALFANUMERISK SÖKNYCKEL                  
001400*                                 ALPHANUMERICAL SEARCH KEY               
001500     03 BEBET.                                                            
001600*                                 BETALNINGSANSVARIG NAMN                 
001700*                                 NAME OF PAYER                           
001800        05 BEBET-NAME1       PIC X(35).                                   
001900*                                 DEL AV BETALNINGSANSVARIGS NAMN         
002000*                                 PART OF FINANCIAL CUSTOMER NAME         
002100        05 BEBET-NAME2       PIC X(35).                                   
002200*                                 DEL AV BETALNINGSANSVARIGS NAMN         
002300*                                 PART OF FINANCIAL CUSTOMER NAME         
002400        05 BEBET-NAME3       PIC X(35).                                   
002500*                                 DEL AV BETALNINGSANSVARIGS NAMN         
002600*                                 PART OF FINANCIAL CUSTOMER NAME         
002700        05 BEBET-NAME4       PIC X(35).                                   
002800*                                 DEL AV BETALNINGSANSVARIGS NAMN         
002900*                                 PART OF FINANCIAL CUSTOMER NAME         
003000     03 ADBET-STREET         PIC X(35).                                   
003100*                                 BETALARENS GATUADRESS                   
003200*                                 PAYER ADDRESS STREET                    
003300     03 ADBET-BOX            PIC X(10).                                   
003400*                                 BOXADRESS BETALNINGSANSVARIG            
003500*                                 PAYER BOX ADDRESS                       
003600     03 ADBET-CITY           PIC X(35).                                   
003700*                                 BETALARENS STADSADRESS                  
003800*                                 PAYER ADDRESS CITY                      
003900     03 ADBET-PCODE          PIC X(10).                                   
004000*                                 BETALARENS STADSADRESS POSTNR           
004100*                                 PAYER ADDRESS POSTAL CODE               
004200     03 IDLANDX3             PIC X(3).                                    
004300*                                 3-STÄLLIG LANDSBETECKNINGSKOD           
004400*                                 3-LETTER CODE FOR COUNTRY.              
004500     03 IDSPRAK              PIC X(2).                                    
004600*                                 2-STÄLLIG ISO SPRÅKKOD                  
004700*                                 2-LETTER ISO LANGUAGE CODE              
004800     03 IDTFN                PIC X(20).                                   
004900*                                 TELEFONNUMMER EXTERNT                   
005000*                                 TELEPHONE NUMBER  EXTERNAL              
005100     03 IDTFX                PIC X(20).                                   
005200*                                 TELEFAXNUMMER                           
005300*                                 FAXNUMBER                               
005400     03 IDMAIL               PIC X(60).                                   
005500*                                 MAIL ADRESS                             
005600*                                 MAIL ADDRESS                            
005700     03 IDLEVNR-AP           PIC X(10).                                   
005800*                                 LEVERANTÖRNUMMER                        
005900*                                 SUPPLIER NUMBER (VENDORNUMBER)          
006000     03 IDVAT                PIC X(17).                                   
006100*                                 MOMSREGISTRERINGSNUMMER                 
006200*                                 VAT REGISTRATION NUMBER                 
006300     03 KDVALISO             PIC X(3).                                    
006400*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
006500*                                 CURRENCY CODE BY ISO-STANDARD.          
006600     03 KDTRADP              PIC X(4).                                    
006700*                                 TRADING PARTNER                         
006800*                                 TRADING PARTNER                         
006900     03 KDBETALV             PIC X(4).                                    
007000*                                 BETALNINGSVILLKOR KUNDRESKONTRA         
007100*                                 TERMS OF PAYMENT                        
007200     03 KDKREDSP             PIC X.                                       
007300*                                 KREDITSPÄRR PÅ BETALARE                 
007400*                                 CREDIT STOP FINANCIAL CUSTOMER          
007500     03 KDPARTTY             PIC X(3).                                    
007600*                                 TYP AV BETALARE                         
007700*                                 TYPE OF FIN.CUSTOMER                    
007800     03 KDPARTGR             PIC X(15).                                   
007900*                                 GRUPP AV BETALARE                       
008000*                                 FIN.CUSTOMER GROUP                      
008100     03 DAREGDAT             PIC 9(8).                                    
008200*                                 REGISTRERINGSDATUM (ÅÅÅÅMMDD)           
008300*                                 REGISTRATION DATE (YYYYMMDD)            
008400     03 DAUPPDAT             PIC 9(8).                                    
008500*                                 UPPDATERINGSDATUM  (ÅÅÅÅMMDD)           
008600*                                                                         
008700*                                 UPDATING DATE     (YYYYMMDD)            
008800*                                                                         
008900     03 DADELDAT             PIC 9(8).                                    
009000*                                 BORTTAGSDATUM      (ÅÅÅÅMMDD)           
009100*                                 DELETION DATE     (YYYYMMDD)            
009200     03 IDUSER               PIC X(8).                                    
009300*                                 ANVÄNDARENS SÄKERHETS ID                
009400*                                 USER SECURITY-IDENTITY                  
009500     03 FLRATE               PIC X.                                       
009600*                                 IND. A RATE BETWEEN TWO LOCAL C         
009700*                                 URRENCIES                               
009800*                                 IND. A RATE BETWEEN TWO LOCAL C         
009900*                                 URRENCIES                               
010000*** END OF VILMAII-COPY LENGTH= 450 BYTES                                 
