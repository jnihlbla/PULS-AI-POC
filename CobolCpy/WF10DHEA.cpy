000100 01  WF10DLIN.                                                            
000200*                                 NEDLÄSNING R30 WDL6                     
000300     03 IDLEGSEL             PIC X(4).                                    
000400*                                 FAKTURERANDE FÖRETAG TEX VCCS           
000500     03 DAEXDAT              PIC X(8).                                    
000600*                                 EXEKVERINGSDATUM (ÅÅÅÅMMDD)             
000700     03 TIEXTID              PIC 9(8).                                    
000800*                                 EXEKVERINGSTIDPUNKT                     
000900     03 KDVALISO             PIC X(3).                                    
001000*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
001100     03 IDLANDX3-SEND        PIC X(3).                                    
001200*                                 LANDKOD SÄNDANDE LAND                   
001300     03 IDLEVNR              PIC X(5).                                    
001400*                                 LEVERANTÖRNUMMER                        
001500     03 IDPARTNR             PIC X(9).                                    
001600*                                 FINANCIELL KUND                         
001700     03 KDFINDOC             PIC X(4).                                    
001800*                                 TYP FINANSIELLT DOKUMENT                
001900     03 FLSOFT               PIC X.                                       
002000*                                 FLAGGA SOFTVARA                         
002100     03 FLFREE               PIC X.                                       
002200*                                 GRATISFATURA                            
002300     03 FLPRIV               PIC X.                                       
002400*                                 KÖPARE ÄR EN PRIVATPERSON               
002500     03 IDBREAK              OCCURS 2 TIMES                               
002600                             PIC X(8).                                    
002700*                                 BRYTVÄRDE                               
002800     03 IDFINDOC             PIC Z(8)9.                                   
002900*                                 FINANSIELLT DOKUMENT ID                 
003000     03 DAFINDOC             PIC X(8).                                    
003100*                                 DOKUMENT DATUM (ÅÅÅÅMMDD)               
003200     03 IDSYSTEM-SEND        PIC X(4).                                    
003300*                                 VOLVO SÄNDANDE SYSTEM                   
003400     03 IDSYSTEM-REC         PIC X(4).                                    
003500*                                 VOLVO MOTTAGANDE SYSTEM                 
003600     03 IDSPRAK              PIC X(2).                                    
003700*                                 2-STÄLLIG ISO SPRÅKKOD                  
003800     03 KDBETALV             PIC X(4).                                    
003900*                                 BETALNINGSVILLKOR KUNDRESKONTRA         
004000     03 BEBETVIL             PIC X(30).                                   
004100*                                 BETALNINGSVILLKORSTEXT                  
004200     03 BELEGRAD             OCCURS 2 TIMES                               
004300                             PIC X(35).                                   
004400*                                 DEL AV LEGAL SELLER NAMN                
004500     03 ADLEG-STREET         PIC X(35).                                   
004600*                                 LEGAL SELLER GATUADRESS                 
004700     03 ADLEG-BOX            PIC X(10).                                   
004800*                                 BOXADRESS LEGAL SELLER                  
004900     03 ADLEG-CITY           PIC X(35).                                   
005000*                                 LEGAL SÄLJARES ADRESS STAD              
005100     03 ADLEG-PCODE          PIC X(10).                                   
005200*                                 LEGAL SELLER ADRESS POSTNR              
005300     03 IDLANDX3-LEG         PIC X(3).                                    
005400*                                 LANDKOD LEGAL SÄLJARE                   
005500     03 IDTFN-LEG            PIC X(20).                                   
005600*                                 TELEFONNUMMER EXTERNT LEGAL SÄL         
005700*                                 JARE                                    
005800     03 IDTFX-LEG            PIC X(20).                                   
005900*                                 FAXNUMMER LEGAL SÄLJARE                 
006000     03 IDMAIL-LEG           PIC X(60).                                   
006100*                                 MAIL ADRESS LEGAL SÄLJARE               
006200     03 BECONT-LEG           PIC X(35).                                   
006300*                                 KONTAKTPERSON LEGAL SÄLJARE             
006400     03 IDVAT-LEG            PIC X(17).                                   
006500*                                 MOMSREGISTRERINGSNUMMER LEGAL S         
006600*                                 ÄLJARE                                  
006700     03 IDBG-LEG             PIC X(15).                                   
006800*                                 BANKGIRO LEGAL SÄLJARE                  
006900     03 IDPG-LEG             PIC X(15).                                   
007000*                                 POSTGIRO LEGAL SÄLJARE                  
007100     03 BERESPRA             OCCURS 2 TIMES                               
007200                             PIC X(35).                                   
007300*                                 DEL AV ANSV AVDELNINGS NAMN             
007400     03 ADRESP-STREET        PIC X(35).                                   
007500*                                 ANSVARIG AVDELNINGS GATUADRESS          
007600     03 ADRESP-BOX           PIC X(10).                                   
007700*                                 BOX ADRESS ANSVARIG AVDELNING           
007800     03 ADRESP-CITY          PIC X(35).                                   
007900*                                 ANSVARIG AVDELNINGS STAD (ELLER         
008000*                                  LIKNANDE)                              
008100     03 ADRESP-PCODE         PIC X(10).                                   
008200*                                 ANSVARIG AVDELNINGS POSTNUMMER          
008300     03 IDLANDX3-RESP        PIC X(3).                                    
008400*                                 LANDKOD ANSVARIG AVD ETC                
008500     03 IDTFN-RESP           PIC X(20).                                   
008600*                                 TELEFONNUMMER EXTERNT ANSVARIG          
008700*                                 AVD                                     
008800     03 IDTFX-RESP           PIC X(20).                                   
008900*                                 FAXNUMMER ANSVARIG AVD                  
009000     03 IDMAIL-RESP          PIC X(60).                                   
009100*                                 MAIL ADRESS ANSVARIG AVD                
009200     03 BECONT-RESP          PIC X(35).                                   
009300*                                 KONTAKTPERSON ANSVARIG AVD              
009400     03 IDVAT-RESP           PIC X(17).                                   
009500*                                 MOMSREGISTRERINGSNUMMER ANSVARI         
009600*                                 G AVD                                   
009700     03 IDBG-RESP            PIC X(15).                                   
009800*                                 BANKGIRO ANSVARIG AVD                   
009900     03 IDPG-RESP            PIC X(15).                                   
010000*                                 POSTGIRO ANSVARIG AVD                   
010100     03 BEBET-NAME1          PIC X(35).                                   
010200*                                 DEL AV BETALNINGSANSVARIGS NAMN         
010300     03 BEBET-NAME2          PIC X(35).                                   
010400*                                 DEL AV BETALNINGSANSVARIGS NAMN         
010500     03 ADBET-STREET         PIC X(35).                                   
010600*                                 BETALARENS GATUADRESS                   
010700     03 ADBET-BOX            PIC X(10).                                   
010800*                                 BOXADRESS BETALNINGSANSVARIG            
010900     03 ADBET-CITY           PIC X(35).                                   
011000*                                 BETALARENS STADSADRESS                  
011100     03 ADBET-PCODE          PIC X(10).                                   
011200*                                 BETALARENS STADSADRESS POSTNR           
011300     03 IDLANDX3-BET         PIC X(3).                                    
011400*                                 LANDKOD BETALANDE KUND ETC              
011500     03 IDVAT-BET            PIC X(17).                                   
011600*                                 MOMSREGISTRERINGSNUMMER BETALAR         
011700*                                 E                                       
011800     03 SUNTO-PART           PIC Z(10)9.9(2).                             
011900*                                 TOTAL SALES AMOUNT PARTS EXCL.          
012000*                                 VAT                                     
012100     03 SUNTO-SERV           PIC Z(10)9.9(2).                             
012200*                                 TOTAL SALES AMOUNT SERVICES EXC         
012300*                                 L. VAT                                  
012400     03 SUBTO-PART           PIC Z(10)9.9(2).                             
012500*                                 TOTAL SALES AMOUNT PARTS INCL.          
012600*                                 VAT                                     
012700     03 SUBTO-SERV           PIC Z(10)9.9(2).                             
012800*                                 TOTAL SALES AMOUNT SERVICES INC         
012900*                                 L. VAT                                  
013000     03 SUNTO-TOT            PIC Z(10)9.9(2).                             
013100*                                 TOTAL SALES AMOUNT EXCL. VAT            
013200     03 SUVAT-BILLIT-TOT     PIC Z(10)9.9(2).                             
013300*                                 SUMMERAT MOMSVÄRDE                      
013400     03 SUBTO-TOT            PIC Z(10)9.9(2).                             
013500*                                 TOTAL SALES AMOUNT INCL. VAT            
013600     03 KDTRADP              PIC X(4).                                    
013700*                                 TRADING PARTNER                         
013800     03 PRKURS               PIC Z(5)9.9(5).                              
013900*                                 VALUTAKURS                              
014000     03 BEANST               PIC X(25).                                   
014100*                                 ANSTÄLLDS NAMN                          
014200     03 IDUSER               PIC X(8).                                    
014300*                                 ANVÄNDARENS SÄKERHETS ID                
014400     03 BETEXT               OCCURS 4 TIMES                               
014500                             PIC X(50).                                   
014600     03 IDVAT-AGENT          PIC X(17).                                   
014700*                                 MOMSREGISTRERINGSNUMMER AGENT           
014800     03 BETEXT-5             PIC X(125).                                  
014900     03 BETEXT-CRE           PIC X(100).                                  
015000*** END OF VILMAII-COPY LENGTH= 1583 BYTES                                
