000100 01  WF1070.                                                              
000200*                                 FINANSIELL KUNDINFO FRÅN BILLIT         
000300*                                  TILL VSS                               
000400     03 IDLEGSEL             PIC X(4).                                    
000500*                                 FAKTURERANDE FÖRETAG TEX VCCS           
000600*                                 LEGAL SELLER IDENTITY                   
000700     03 IDPARTNR             PIC X(9).                                    
000800*                                 PARTNERNUMMER                           
000900*                                 PARTNER NO                              
001000     03 IDALPHA              PIC X(10).                                   
001100*                                 ALFANUMERISK SÖKNYCKEL                  
001200*                                 ALPHANUMERICAL SEARCH KEY               
001300     03 BEBET.                                                            
001400*                                 BETALNINGSANSVARIG NAMN                 
001500*                                 NAME OF PAYER                           
001600        05 BEBET-NAME1       PIC X(35).                                   
001700*                                 DEL AV BETALNINGSANSVARIGS NAMN         
001800*                                 PART OF FINANCIAL CUSTOMER NAME         
001900        05 BEBET-NAME2       PIC X(35).                                   
002000*                                 DEL AV BETALNINGSANSVARIGS NAMN         
002100*                                 PART OF FINANCIAL CUSTOMER NAME         
002200        05 BEBET-NAME3       PIC X(35).                                   
002300*                                 DEL AV BETALNINGSANSVARIGS NAMN         
002400*                                 PART OF FINANCIAL CUSTOMER NAME         
002500        05 BEBET-NAME4       PIC X(35).                                   
002600*                                 DEL AV BETALNINGSANSVARIGS NAMN         
002700*                                 PART OF FINANCIAL CUSTOMER NAME         
002800     03 ADBET-STREET         PIC X(35).                                   
002900*                                 BETALARENS GATUADRESS                   
003000*                                 PAYER ADDRESS STREET                    
003100     03 ADBET-BOX            PIC X(10).                                   
003200*                                 BOXADRESS BETALNINGSANSVARIG            
003300*                                 PAYER BOX ADDRESS                       
003400     03 ADBET-CITY           PIC X(35).                                   
003500*                                 BETALARENS STADSADRESS                  
003600*                                 PAYER ADDRESS CITY                      
003700     03 ADBET-PCODE          PIC X(10).                                   
003800*                                 BETALARENS STADSADRESS POSTNR           
003900*                                 PAYER ADDRESS POSTAL CODE               
004000     03 IDLANDX3             PIC X(3).                                    
004100*                                 3-STÄLLIG LANDSBETECKNINGSKOD           
004200*                                 3-LETTER CODE FOR COUNTRY.              
004300     03 IDSPRAK              PIC X(2).                                    
004400*                                 2-STÄLLIG ISO SPRÅKKOD                  
004500*                                 2-LETTER ISO LANGUAGE CODE              
004600     03 IDTFN                PIC X(20).                                   
004700*                                 TELEFONNUMMER EXTERNT                   
004800*                                 TELEPHONE NUMBER  EXTERNAL              
004900     03 IDTFX                PIC X(20).                                   
005000*                                 TELEFAXNUMMER                           
005100*                                 FAXNUMBER                               
005200     03 IDMAIL               PIC X(60).                                   
005300*                                 MAIL ADRESS                             
005400*                                 MAIL ADDRESS                            
005500     03 IDLEVNR-AP           PIC X(10).                                   
005600*                                 LEVERANTÖRNUMMER                        
005700*                                 SUPPLIER NUMBER (VENDORNUMBER)          
005800     03 IDVAT                PIC X(17).                                   
005900*                                 MOMSREGISTRERINGSNUMMER                 
006000*                                 VAT REGISTRATION NUMBER                 
006100     03 KDVALISO             PIC X(3).                                    
006200*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
006300*                                 CURRENCY CODE BY ISO-STANDARD.          
006400     03 KDTRADP              PIC X(4).                                    
006500*                                 TRADING PARTNER                         
006600*                                 TRADING PARTNER                         
006700     03 KDBETALV             PIC X(4).                                    
006800*                                 BETALNINGSVILLKOR KUNDRESKONTRA         
006900*                                 TERMS OF PAYMENT                        
007000     03 KDKREDSP             PIC X.                                       
007100*                                 KREDITSPÄRR PÅ BETALARE                 
007200*                                 CREDIT STOP FINANCIAL CUSTOMER          
007300     03 KDPARTTY             PIC X(3).                                    
007400*                                 TYP AV BETALARE                         
007500*                                 TYPE OF FIN.CUSTOMER                    
007600     03 KDPARTGR             PIC X(15).                                   
007700*                                 GRUPP AV BETALARE                       
007800*                                 FIN.CUSTOMER GROUP                      
007900     03 DAREGDAT             PIC 9(8).                                    
008000*                                 REGISTRERINGSDATUM (ÅÅÅÅMMDD)           
008100*                                 REGISTRATION DATE (YYYYMMDD)            
008200     03 DAUPPDAT             PIC 9(8).                                    
008300*                                 UPPDATERINGSDATUM  (ÅÅÅÅMMDD)           
008400*                                                                         
008500*                                 UPDATING DATE     (YYYYMMDD)            
008600*                                                                         
008700     03 DADELDAT             PIC 9(8).                                    
008800*                                 BORTTAGSDATUM      (ÅÅÅÅMMDD)           
008900*                                 DELETION DATE     (YYYYMMDD)            
009000     03 IDUSER               PIC X(8).                                    
009100*                                 ANVÄNDARENS SÄKERHETS ID                
009200*                                 USER SECURITY-IDENTITY                  
009300     03 BEBETVIL             PIC X(30).                                   
009400*                                 BETALNINGSVILLKORSTEXT                  
009500*                                 TERMS OF PAYMENT TEXT                   
009600*** END OF VILMAII-COPY LENGTH= 477 BYTES                                 
