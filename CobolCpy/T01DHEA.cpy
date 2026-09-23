000100* GENERATION OF COBOL HOST STRUCTURE FROM T01DHEA-TAB                     
000200  01 T01DHEA.                                                             
000300*              T01DHEA                                                    
000400   03 IDLEGSEL                          PIC X(4).                         
000500*              FAKTURERANDE FÖRETAG TEX VCCS                              
000600   03 DAEXDAT                           PIC X(8).                         
000700*              EXEKVERINGSDATUM (ÅÅÅÅMMDD)                                
000800   03 TIEXTID                           PIC S9(7) COMP-3.                 
000900*              EXEKVERINGSTIDPUNKT                                        
001000   03 KDVALISO                          PIC X(3).                         
001100*              VALUTAKOD ENLIGT ISO-STANDARD.                             
001200   03 IDLANDX3-SEND                     PIC X(3).                         
001300*              LANDKOD SÄNDANDE LAND                                      
001400   03 IDLEVNR                           PIC X(5).                         
001500*              LEVERANTÖRNUMMER                                           
001600   03 IDPARTNR                          PIC X(9).                         
001700*              PARTNERNUMMER                                              
001800   03 KDFINDOC                          PIC X(4).                         
001900*              TYP FINANSIELLT DOKUMENT                                   
002000   03 FLSOFT                            PIC X(1).                         
002100*              FLAGGA SOFTVARA                                            
002200   03 FLFREE                            PIC X(1).                         
002300*              GRATISFATURA                                               
002400   03 FLPRIV                            PIC X(1).                         
002500*              KÖPARE ÄR EN PRIVATPERSON                                  
002600   03 IDBREAK-1                         PIC X(8).                         
002700*              BRYTVÄRDE                                                  
002800   03 IDBREAK-2                         PIC X(8).                         
002900*              BRYTVÄRDE                                                  
003000   03 IDFINDOC                          PIC S9(9) COMP-3.                 
003100*              FINANSIELLT DOKUMENT ID                                    
003200   03 DAFINDOC                          PIC X(8).                         
003300*              DOKUMENT DATUM (ÅÅÅÅMMDD)                                  
003400   03 IDSYSTEM-SEND                     PIC X(4).                         
003500*              VOLVO SÄNDANDE SYSTEM                                      
003600   03 IDSYSTEM-REC                      PIC X(4).                         
003700*              VOLVO MOTTAGANDE SYSTEM                                    
003800   03 IDSPRAK                           PIC X(2).                         
003900*              2-STÄLLIG ISO SPRÅKKOD                                     
004000   03 KDBETALV                          PIC X(4).                         
004100*              BETALNINGSVILLKOR KUNDRESKONTRA                            
004200   03 BEBETVIL                          PIC X(30).                        
004300*              BETALNINGSVILLKORSTEXT                                     
004400   03 BELEGRAD-1                        PIC X(35).                        
004500*              DEL AV LEGAL SELLER NAMN                                   
004600   03 BELEGRAD-2                        PIC X(35).                        
004700*              DEL AV LEGAL SELLER NAMN                                   
004800   03 ADLEG-STREET                      PIC X(35).                        
004900*              LEGAL SELLER GATUADRESS                                    
005000   03 ADLEG-BOX                         PIC X(10).                        
005100*              BOXADRESS LEGAL SELLER                                     
005200   03 ADLEG-CITY                        PIC X(35).                        
005300*              LEGAL SÄLJARES ADRESS STAD                                 
005400   03 ADLEG-PCODE                       PIC X(10).                        
005500*              LEGAL SELLER ADRESS POSTNR                                 
005600   03 IDLANDX3-LEG                      PIC X(3).                         
005700*              LANDKOD LEGAL SÄLJARE                                      
005800   03 IDTFN-LEG                         PIC X(20).                        
005900*              TELEFONNUMMER EXTERNT LEGAL SÄLJARE                        
006000   03 IDTFX-LEG                         PIC X(20).                        
006100*              FAXNUMMER LEGAL SÄLJARE                                    
006200   03 IDMAIL-LEG                        PIC X(60).                        
006300*              MAIL ADRESS LEGAL SÄLJARE                                  
006400   03 BECONT-LEG                        PIC X(35).                        
006500*              KONTAKTPERSON LEGAL SÄLJARE                                
006600   03 IDVAT-LEG                         PIC X(17).                        
006700*              MOMSREGISTRERINGSNUMMER LEGAL SÄLJARE                      
006800   03 IDBG-LEG                          PIC X(15).                        
006900*              BANKGIRO LEGAL SÄLJARE                                     
007000   03 IDPG-LEG                          PIC X(15).                        
007100*              POSTGIRO LEGAL SÄLJARE                                     
007200   03 BERESPRA-1                        PIC X(35).                        
007300*              DEL AV ANSV AVDELNINGS NAMN                                
007400   03 BERESPRA-2                        PIC X(35).                        
007500*              DEL AV ANSV AVDELNINGS NAMN                                
007600   03 ADRESP-STREET                     PIC X(35).                        
007700*              ANSVARIG AVDELNINGS GATUADRESS                             
007800   03 ADRESP-BOX                        PIC X(10).                        
007900*              BOX ADRESS ANSVARIG AVDELNING                              
008000   03 ADRESP-CITY                       PIC X(35).                        
008100*              ANSVARIG AVDELNINGS STAD (ELLER LIKNANDE)                  
008200   03 ADRESP-PCODE                      PIC X(10).                        
008300*              ANSVARIG AVDELNINGS POSTNUMMER                             
008400   03 IDLANDX3-RESP                     PIC X(3).                         
008500*              LANDKOD ANSVARIG AVD ETC                                   
008600   03 IDTFN-RESP                        PIC X(20).                        
008700*              TELEFONNUMMER EXTERNT ANSVARIG AVD                         
008800   03 IDTFX-RESP                        PIC X(20).                        
008900*              FAXNUMMER ANSVARIG AVD                                     
009000   03 IDMAIL-RESP                       PIC X(60).                        
009100*              MAIL ADRESS ANSVARIG AVD                                   
009200   03 BECONT-RESP                       PIC X(35).                        
009300*              KONTAKTPERSON ANSVARIG AVD                                 
009400   03 IDVAT-RESP                        PIC X(17).                        
009500*              MOMSREGISTRERINGSNUMMER ANSVARIG AVD                       
009600   03 IDBG-RESP                         PIC X(15).                        
009700*              BANKGIRO ANSVARIG AVD                                      
009800   03 IDPG-RESP                         PIC X(15).                        
009900*              POSTGIRO ANSVARIG AVD                                      
010000   03 BEBET-NAME1                       PIC X(35).                        
010100*              DEL AV BETALNINGSANSVARIGS NAMN                            
010200   03 BEBET-NAME2                       PIC X(35).                        
010300*              DEL AV BETALNINGSANSVARIGS NAMN                            
010400   03 ADBET-STREET                      PIC X(35).                        
010500*              BETALARENS GATUADRESS                                      
010600   03 ADBET-BOX                         PIC X(10).                        
010700*              BOXADRESS BETALNINGSANSVARIG                               
010800   03 ADBET-CITY                        PIC X(35).                        
010900*              BETALARENS STADSADRESS                                     
011000   03 ADBET-PCODE                       PIC X(10).                        
011100*              BETALARENS STADSADRESS POSTNR                              
011200   03 IDLANDX3-BET                      PIC X(3).                         
011300*              LANDKOD BETALANDE KUND ETC                                 
011400   03 IDVAT-BET                         PIC X(17).                        
011500*              MOMSREGISTRERINGSNUMMER BETALARE                           
011600   03 SUNTO-PART                        PIC S9(11)V9(2) COMP-3.           
011700*              TOTAL SALES AMOUNT PARTS EXCL. VAT                         
011800   03 SUNTO-SERV                        PIC S9(11)V9(2) COMP-3.           
011900*              TOTAL SALES AMOUNT SERVICES EXCL. VAT                      
012000   03 SUBTO-PART                        PIC S9(11)V9(2) COMP-3.           
012100*              TOTAL SALES AMOUNT PARTS INCL. VAT                         
012200   03 SUBTO-SERV                        PIC S9(11)V9(2) COMP-3.           
012300*              TOTAL SALES AMOUNT SERVICES INCL. VAT                      
012400   03 SUNTO-TOT                         PIC S9(11)V9(2) COMP-3.           
012500*              TOTAL SALES AMOUNT EXCL. VAT                               
012600   03 SUVAT-BILLIT-TOT                  PIC S9(11)V9(2) COMP-3.           
012700*              SUMMERAT MOMSVÄRDE                                         
012800   03 SUBTO-TOT                         PIC S9(11)V9(2) COMP-3.           
012900*              TOTAL SALES AMOUNT INCL. VAT                               
013000   03 KDTRADP                           PIC X(4).                         
013100*              TRADING PARTNER                                            
013200   03 PRKURS                            PIC S9(6)V9(5) COMP-3.            
013300*              VALUTAKURS                                                 
013400   03 BEANST                            PIC X(25).                        
013500*              ANSTÄLLDS NAMN                                             
013600   03 IDUSER                            PIC X(8).                         
013700*              ANVÄNDARENS SÄKERHETS ID                                   
013800   03 BETEXT-1                          PIC X(50).                        
013900   03 BETEXT-2                          PIC X(50).                        
014000   03 BETEXT-3                          PIC X(50).                        
014100   03 BETEXT-4                          PIC X(50).                        
014200   03 IDVAT-AGENT                       PIC X(17).                        
014300*              MOMSREGISTRERINGSNUMMER AGENT                              
014400   03 BETEXT                            PIC X(125).                       
014500   03 BETEXT-CRE                        PIC X(100).                       
014600*                                                                         
014700*** END OF VILMAII-COPY LENGTH= 1520 OLD LENGTH=                          
