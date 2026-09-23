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
002800     03 IDLOPNR              PIC Z(4)9.                                   
002900*                                 LÖPNUMMER          IDLOPNR-002          
003000     03 IDLANDX3-REC         PIC X(3).                                    
003100*                                 LANDKOD MOTTAGANDE LAND                 
003200     03 IDEXCUST             OCCURS 3 TIMES                               
003300                             PIC X(15).                                   
003400*                                 EXTERNT KUNDID                          
003500     03 IDBUNDLE             PIC X(15).                                   
003600*                                 BUNDLE ID                               
003700     03 IDREF                PIC X(15).                                   
003800*                                 REFERENS ID                             
003900     03 IDREFRAD             PIC Z(4)9.                                   
004000*                                 REFERENSRADSNR                          
004100     03 DAREFDAT             PIC X(8).                                    
004200*                                 REFERENSDATUM (ÅÅÅÅMMDD)                
004300     03 BEVOLREF             PIC X(10).                                   
004400*                                 VOLVO REFERENS                          
004500     03 IDOPTION             OCCURS 5 TIMES                               
004600                             PIC X(15).                                   
004700*                                 BRYTBEGREPP                             
004800     03 IDARTNR-FINANCE      PIC X(50).                                   
004900*                                 ARTIKELNUMMER FÖR FINANSIELL BR         
005000*                                 UK                                      
005100     03 BEART                PIC X(25).                                   
005200*                                 ARTIKELBENÄMNING                        
005300     03 IDSTATNR             PIC Z(8)9.                                   
005400*                                 STATISTISKT NUMMER                      
005500*                                 1 = NORSKT                              
005600*                                 2 = ENGELSKT                            
005700*                                 3 = BELGISKT                            
005800*                                 4 = PERUANSKT                           
005900*                                 5 = SVENSKT                             
006000*                                 6 =                                     
006100     03 VKORDBTO-KOLLI       PIC Z(5)9.9.                                 
006200*                                 ORDERVIKT BRUTTO PER KOLLI              
006300     03 VKARTNTO             PIC Z(3)9.9(3).                              
006400*                                 ARTIKELVIKT NETTO (KG) MED EMB          
006500     03 KDARTURS             PIC X(2).                                    
006600*                                 ARTIKELURSPRUNGSKOD                     
006700     03 KVBEART              PIC Z(6)9.                                   
006800*                                 BESTÄLLT ANTAL STYCKEN                  
006900     03 KVLEVART             PIC Z(6)9.                                   
007000*                                 LEVERERAT ANTAL STYCK                   
007100     03 PRARTBTO             PIC Z(6)9.9(2).                              
007200*                                 FÖRSÄLJNINGSPRIS BRUTTO (KR)            
007300     03 PRARTNTO             PIC Z(6)9.9(2).                              
007400*                                 ARTIKELPRIS NETTO                       
007500     03 REARTRAB             PIC Z9.9(2).                                 
007600*                                 ARTIKELRABATT                           
007700     03 KDVAT                PIC X(2).                                    
007800*                                 MOMSKOD                                 
007900     03 FLSPECPR             PIC X.                                       
008000*                                 SPECIALPRISFLAGGA                       
008100     03 KDANMORS             PIC X(2).                                    
008200*                                 ORSAK TILL LEVERANSANMÄRKNING           
008300     03 IDFAKREF             PIC Z(9).                                    
008400*                                 URSPRUNGLIGT FAKTURANUMMER              
008500     03 DAFAKREF             PIC X(8).                                    
008600*                                 URSPRUNGLIGT FAKTURADATUM (ÅÅÅÅ         
008700*                                 MMDD)                                   
008800     03 IDDC                 PIC X(2).                                    
008900*                                 IDENTIFIERARE LAGER                     
009000     03 KDFRAKT              PIC Z(3).                                    
009100*                                 FRAKTSÄTT DC TILL KUND                  
009200     03 BELEVVIL             PIC X(35).                                   
009300*                                 LEVERANSVILLKOR                         
009400     03 IDACCNT              OCCURS 4 TIMES                               
009500                             PIC X(15).                                   
009600*                                 KONTOFÄLT                               
009700     03 FILLER               PIC X(100).                                  
009800     03 SUNTO                PIC Z(10)9.9(2).                             
009900*                                 TOTAL SALES AMOUNT EXCL. VAT            
010000     03 REVAT                PIC Z(2)9.9(2).                              
010100*                                 MULTIPLIKATIONSFAKTOR FÖR MOMS          
010200     03 SUVAT-BILLIT         PIC Z(10)9.9(2).                             
010300*                                 SUMMERAT MOMSVÄRDE PER RAD              
010400     03 SUBTO                PIC Z(10)9.9(2).                             
010500*                                 TOTAL SALES AMOUNT INCL. VAT            
010600     03 BEVAT                PIC X(50).                                   
010700*                                 MOMSKODSBENÄMNING R3                    
010800     03 IDAPPEND             PIC X(8).                                    
010900*                                 APPENDIXVÄRDE                           
011000     03 IDARTNR-CNTRL        PIC X(2).                                    
011100*                                 KONTROLLSIFFRA FÖR ARTIKELNUMME         
011200*                                 R                                       
011300     03 FLPCOO               PIC X.                                       
011400*                                 FLAGGA OM FÖRMÅNSAVTAL URS.LAND         
011500     03 IDLEVNR-ART          PIC X(5).                                    
011600*                                 LEVERANTÖRNR PÅ ARTIKEL                 
011700     03 IDTRACK              OCCURS 5 TIMES                               
011800                             PIC X(25).                                   
011900*                                 TRACKING ID FROM CUSTOMS                
012000     03 KVANT-TRACK          OCCURS 5 TIMES                               
012100                             PIC Z(6)9.                                   
012200*                                 TILLHÖR AV EN VISS ARTIKEL              
012300*** END OF VILMAII-COPY LENGTH= 881 BYTES                                 
