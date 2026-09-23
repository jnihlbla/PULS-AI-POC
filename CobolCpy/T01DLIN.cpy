000100* GENERATION OF COBOL HOST STRUCTURE FROM T01DLIN-TAB                     
000200  01 T01DLIN.                                                             
000300*              T01DLIN                                                    
000400   03 IDLEGSEL          PIC X(4).                                         
000500*              FAKTURERANDE FÖRETAG TEX VCCS                              
000600   03 DAEXDAT           PIC X(8).                                         
000700*              EXEKVERINGSDATUM (ÅÅÅÅMMDD)                                
000800   03 TIEXTID           PIC S9(7) COMP-3.                                 
000900*              EXEKVERINGSTIDPUNKT                                        
001000   03 KDVALISO          PIC X(3).                                         
001100*              VALUTAKOD ENLIGT ISO-STANDARD.                             
001200   03 IDLANDX3-SEND     PIC X(3).                                         
001300*              LANDKOD SÄNDANDE LAND                                      
001400   03 IDLEVNR           PIC X(5).                                         
001500*              LEVERANTÖRNUMMER                                           
001600   03 IDPARTNR          PIC X(9).                                         
001700*              FINANCIELL KUND                                            
001800   03 KDFINDOC          PIC X(4).                                         
001900*              TYP FINANSIELLT DOKUMENT                                   
002000   03 FLSOFT            PIC X(1).                                         
002100*              FLAGGA SOFTVARA                                            
002200   03 FLFREE            PIC X(1).                                         
002300*              GRATISFATURA                                               
002400   03 FLPRIV            PIC X(1).                                         
002500*              KÖPARE ÄR EN PRIVATPERSON                                  
002600   03 IDBREAK-1         PIC X(8).                                         
002700*              BRYTVÄRDE                                                  
002800   03 IDBREAK-2         PIC X(8).                                         
002900*              BRYTVÄRDE                                                  
003000   03 IDLOPNR           PIC S9(5) COMP-3.                                 
003100*              LÖPNUMMER          IDLOPNR-002                             
003200   03 IDLANDX3-REC      PIC X(3).                                         
003300*              LANDKOD MOTTAGANDE LAND                                    
003400   03 IDEXCUST-1        PIC X(15).                                        
003500*              EXTERNT KUNDID                                             
003600   03 IDEXCUST-2        PIC X(15).                                        
003700*              EXTERNT KUNDID                                             
003800   03 IDEXCUST-3        PIC X(15).                                        
003900*              EXTERNT KUNDID                                             
004000   03 IDBUNDLE          PIC X(15).                                        
004100*              BUNDLE ID                                                  
004200   03 IDREF             PIC X(15).                                        
004300*              REFERENS ID                                                
004400   03 IDREFRAD          PIC S9(5) COMP-3.                                 
004500*              REFERENSRADSNR                                             
004600   03 DAREFDAT          PIC X(8).                                         
004700*              REFERENSDATUM (ÅÅÅÅMMDD)                                   
004800   03 BEVOLREF          PIC X(10).                                        
004900*              VOLVO REFERENS                                             
005000   03 IDOPTION-1        PIC X(15).                                        
005100*              BRYTBEGREPP                                                
005200   03 IDOPTION-2        PIC X(15).                                        
005300*              BRYTBEGREPP                                                
005400   03 IDOPTION-3        PIC X(15).                                        
005500*              BRYTBEGREPP                                                
005600   03 IDOPTION-4        PIC X(15).                                        
005700*              BRYTBEGREPP                                                
005800   03 IDOPTION-5        PIC X(15).                                        
005900*              BRYTBEGREPP                                                
006000   03 IDARTNR-FINANCE   PIC X(50).                                        
006100*              ARTIKELNUMMER FÖR FINANSIELL BRUK                          
006200   03 BEART             PIC X(25).                                        
006300*              ARTIKELBENÄMNING                                           
006400   03 IDSTATNR          PIC S9(9) COMP-3.                                 
006500*              STATISTISKT NUMMER                                         
006600*              1 = NORSKT                                                 
006700*              2 = ENGELSKT                                               
006800*              3 = BELGISKT                                               
006900*              4 = PERUANSKT                                              
007000*              5 = SVENSKT                                                
007100*              6 =                                                        
007200   03 VKORDBTO-KOLLI    PIC S9(6)V9(1) COMP-3.                            
007300*              ORDERVIKT BRUTTO PER KOLLI                                 
007400   03 VKARTNTO          PIC S9(4)V9(3) COMP-3.                            
007500*              ARTIKELVIKT NETTO (KG) MED EMB                             
007600   03 KDARTURS          PIC X(2).                                         
007700*              ARTIKELURSPRUNGSKOD                                        
007800   03 KVBEART           PIC S9(7) COMP-3.                                 
007900*              BESTÄLLT ANTAL STYCKEN                                     
008000   03 KVLEVART          PIC S9(7) COMP-3.                                 
008100*              LEVERERAT ANTAL STYCK                                      
008200   03 PRARTBTO          PIC S9(7)V9(2) COMP-3.                            
008300*              FÖRSÄLJNINGSPRIS BRUTTO (KR)                               
008400   03 PRARTNTO          PIC S9(7)V9(2) COMP-3.                            
008500*              ARTIKELPRIS NETTO                                          
008600   03 REARTRAB          PIC S9(2)V9(2) COMP-3.                            
008700*              ARTIKELRABATT                                              
008800   03 KDVAT             PIC X(2).                                         
008900*              MOMSKOD                                                    
009000   03 FLSPECPR          PIC X(1).                                         
009100*              SPECIALPRISFLAGGA                                          
009200   03 KDANMORS          PIC X(2).                                         
009300*              ORSAK TILL LEVERANSANMÄRKNING                              
009400   03 IDFAKREF          PIC S9(9) COMP-3.                                 
009500*              URSPRUNGLIGT FAKTURANUMMER                                 
009600   03 DAFAKREF          PIC X(8).                                         
009700*              URSPRUNGLIGT FAKTURADATUM (ÅÅÅÅMMDD)                       
009800   03 IDDC              PIC X(2).                                         
009900*              IDENTIFIERARE LAGER                                        
010000   03 KDFRAKT           PIC S9(3) COMP-3.                                 
010100*              FRAKTSÄTT DC TILL KUND                                     
010200   03 BELEVVIL          PIC X(35).                                        
010300*              LEVERANSVILLKOR                                            
010400   03 IDACCNT-1         PIC X(15).                                        
010500*              KONTOFÄLT                                                  
010600   03 IDACCNT-2         PIC X(15).                                        
010700*              KONTOFÄLT                                                  
010800   03 IDACCNT-3         PIC X(15).                                        
010900*              KONTOFÄLT                                                  
011000   03 IDACCNT-4         PIC X(15).                                        
011100*              KONTOFÄLT                                                  
011200   03 FILLER            PIC X(100).                                       
011300   03 SUNTO             PIC S9(11)V9(2) COMP-3.                           
011400*              TOTAL SALES AMOUNT EXCL. VAT                               
011500   03 REVAT             PIC S9(3)V9(2) COMP-3.                            
011600*              MULTIPLIKATIONSFAKTOR FÖR MOMS                             
011700   03 SUVAT-BILLIT      PIC S9(11)V9(2) COMP-3.                           
011800*              SUMMERAT MOMSVÄRDE PER RAD                                 
011900   03 SUBTO             PIC S9(11)V9(2) COMP-3.                           
012000*              TOTAL SALES AMOUNT INCL. VAT                               
012100   03 BEVAT             PIC X(50).                                        
012200*              MOMSKODSBENÄMNING R3                                       
012300   03 IDAPPEND          PIC X(8).                                         
012400*              APPENDIXVÄRDE                                              
012500   03 IDARTNR-CNTRL     PIC X(2).                                         
012600*              KONTROLLSIFFRA FÖR ARTIKELNUMMER                           
012700   03 FLPCOO            PIC X(1).                                         
012800*              FLAGGA OM FÖRMÅNSAVTAL URS.LAND                            
012900   03 IDLEVNR-ART       PIC X(5).                                         
013000*              LEVERANTÖRNR PÅ ARTIKEL                                    
013100   03 IDTRACK-1         PIC X(25).                                        
013200*              TRACKING ID FROM CUSTOMS                                   
013300   03 IDTRACK-2         PIC X(25).                                        
013400*              TRACKING ID FROM CUSTOMS                                   
013500   03 IDTRACK-3         PIC X(25).                                        
013600*              TRACKING ID FROM CUSTOMS                                   
013700   03 IDTRACK-4         PIC X(25).                                        
013800*              TRACKING ID FROM CUSTOMS                                   
013900   03 IDTRACK-5         PIC X(25).                                        
014000*              TRACKING ID FROM CUSTOMS                                   
014100   03 KVANT-TRACK-1     PIC S9(7) COMP-3.                                 
014200*              TILLHÖR AV EN VISS ARTIKEL                                 
014300   03 KVANT-TRACK-2     PIC S9(7) COMP-3.                                 
014400*              TILLHÖR AV EN VISS ARTIKEL                                 
014500   03 KVANT-TRACK-3     PIC S9(7) COMP-3.                                 
014600*              TILLHÖR AV EN VISS ARTIKEL                                 
014700   03 KVANT-TRACK-4     PIC S9(7) COMP-3.                                 
014800*              TILLHÖR AV EN VISS ARTIKEL                                 
014900   03 KVANT-TRACK-5     PIC S9(7) COMP-3.                                 
015000*              TILLHÖR AV EN VISS ARTIKEL                                 
015100   03 KDPRMOD           PIC X(2).                                         
015200*              VILKEN PRISMODELL SOM ANVÄNDS                              
015300*                                                                         
015400*** END OF VILMAII-COPY LENGTH= 801 OLD LENGTH=                           
