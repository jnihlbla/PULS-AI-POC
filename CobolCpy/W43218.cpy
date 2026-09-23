000100 01  W43218.                                                              
000200*                                 KUNDREGISTERINFO FRÅN VIPS              
000300*                                 DEALER INFO FROM VIPS                   
000400     03 IDPTYP               PIC X(3).                                    
000500*                                 POSTTYP                                 
000600     03 IDDISTR              PIC 9(5).                                    
000700*                                 DISTRIKTSNUMMER                         
000800     03 IDKUNDNR             PIC 9(6).                                    
000900*                                 KUNDNUMMER                              
001000     03 IDDEALER-VIPS        PIC X(6).                                    
001100*                                 VIPS ÅTERFÖRSÄLJARE                     
001200     03 IDDEALER-VIPSINV     PIC X(6).                                    
001300*                                 FINANSIELL ÅTERFÖRSÄLJARE               
001400     03 BEDEALER-VIPSINV     PIC X(30).                                   
001500*                                 VIPS ÅTERFÖRSÄLJARNAMN                  
001600     03 ADDEALER-INVRAD1     PIC X(30).                                   
001700*                                 KUNDADRESS 1                            
001800     03 ADDEALER-INVRAD2     PIC X(30).                                   
001900*                                 KUNDADRESS 2                            
002000     03 ADPOSTNR-INV         PIC X(8).                                    
002100*                                 POSTNUMMER I ADRESS                     
002200     03 ADCITY-INV           PIC X(20).                                   
002300*                                 FAKTURAMOTTAGERENS STAD                 
002400     03 BEDEALER-VIPSGMT     PIC X(30).                                   
002500*                                 VIPS ÅTERFÖRSÄLJARNAMN                  
002600     03 ADDEALER-GMTRAD1     PIC X(30).                                   
002700*                                 GODSMOTTAGANDE KUND 1                   
002800     03 ADDEALER-GMTRAD2     PIC X(30).                                   
002900*                                 GODSMOTTAGANDE KUND 2                   
003000     03 ADPOSTNR-GMT         PIC X(8).                                    
003100*                                 POSTNUMMER I ADRESS                     
003200     03 ADCITY-GMT           PIC X(20).                                   
003300*                                 GODSMOTTAGERENS STAD                    
003400     03 KDKNDSTA             PIC X.                                       
003500*                                 KUNDSTATUSKOD                           
003600     03 DAREGDAT             PIC 9(8).                                    
003700*                                 REGISTRERINGSDATUM (ÅÅÅÅMMDD)           
003800     03 DASTODAT             PIC 9(8).                                    
003900*                                 GENERELLT STOPPDATUM                    
004000     03 KDCREDIT             PIC X.                                       
004100*                                 KREDITVÄRDIGHETSKOD                     
004200     03 KDKNDKAT             PIC X(2).                                    
004300*                                 KUNDKATEGORIKOD                         
004400     03 IDLANDX2             PIC X(2).                                    
004500*                                 2-STÄLLIG LANDSBETECKNINGSKOD           
004600     03 DATRADAT             PIC 9(8).                                    
004700*                                 TRANSAKTIONSDATUM (ÅÅÅÅMMDD)            
004800     03 TITRATID             PIC 9(6).                                    
004900*                                 TRANSAKTIONSKLOCKSLAG HHMMSS            
005000     03 FLDIRAFF             PIC X.                                       
005100*                                 DIRECT BUSINESS?                        
005200*** END OF VILMAII-COPY LENGTH= 299 BYTES                                 
