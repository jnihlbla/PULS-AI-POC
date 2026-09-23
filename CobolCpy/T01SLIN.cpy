000100* GENERATION OF COBOL HOST STRUCTURE FROM T01SLIN-TAB                     
000200  01 T01SLIN.                                                             
000300*              T01SLIN                                                    
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
003200   03 IDAPPEND          PIC X(8).                                         
003300*              APPENDIXVÄRDE                                              
003400   03 IDREF             PIC X(15).                                        
003500*              REFERENS ID                                                
003600   03 DAREFDAT          PIC X(8).                                         
003700*              REFERENSDATUM (ÅÅÅÅMMDD)                                   
003800   03 IDREFRAD          PIC S9(5) COMP-3.                                 
003900*              REFERENSRADSNR                                             
004000   03 BEVOLREF          PIC X(10).                                        
004100*              VOLVO REFERENS                                             
004200   03 IDSEQ-1           PIC X(8).                                         
004300*              SEKVENSVÄRDE                                               
004400   03 IDSEQ-2           PIC X(8).                                         
004500*              SEKVENSVÄRDE                                               
004600   03 IDSEQ-3           PIC X(8).                                         
004700*              SEKVENSVÄRDE                                               
004800   03 IDLANDX3-REC      PIC X(3).                                         
004900*              LANDKOD MOTTAGANDE LAND                                    
005000   03 IDEXCUST-1        PIC X(15).                                        
005100*              EXTERNT KUNDID                                             
005200   03 IDEXCUST-2        PIC X(15).                                        
005300*              EXTERNT KUNDID                                             
005400   03 IDEXCUST-3        PIC X(15).                                        
005500*              EXTERNT KUNDID                                             
005600   03 IDBUNDLE          PIC X(15).                                        
005700*              BUNDLE ID                                                  
005800   03 IDOPTION-1        PIC X(15).                                        
005900*              BRYTBEGREPP                                                
006000   03 IDOPTION-2        PIC X(15).                                        
006100*              BRYTBEGREPP                                                
006200   03 IDOPTION-3        PIC X(15).                                        
006300*              BRYTBEGREPP                                                
006400   03 IDOPTION-4        PIC X(15).                                        
006500*              BRYTBEGREPP                                                
006600   03 IDOPTION-5        PIC X(15).                                        
006700*              BRYTBEGREPP                                                
006800   03 IDARTNR-FINANCE   PIC X(50).                                        
006900*              ARTIKELNUMMER FÖR FINANSIELL BRUK                          
007000   03 BEART             PIC X(25).                                        
007100*              ARTIKELBENÄMNING                                           
007200   03 IDSTATNR          PIC S9(9) COMP-3.                                 
007300*              STATISTISKT NUMMER                                         
007400*              1 = NORSKT                                                 
007500*              2 = ENGELSKT                                               
007600*              3 = BELGISKT                                               
007700*              4 = PERUANSKT                                              
007800*              5 = SVENSKT                                                
007900*              6 =                                                        
008000   03 KDVAT             PIC X(2).                                         
008100*              MOMSKOD                                                    
008200   03 FLSPECPR          PIC X(1).                                         
008300*              SPECIALPRISFLAGGA                                          
008400   03 PRARTBTO          PIC S9(7)V9(2) COMP-3.                            
008500*              FÖRSÄLJNINGSPRIS BRUTTO (KR)                               
008600   03 PRARTNTO          PIC S9(7)V9(2) COMP-3.                            
008700*              ARTIKELPRIS NETTO                                          
008800   03 REARTRAB          PIC S9(2)V9(2) COMP-3.                            
008900*              ARTIKELRABATT                                              
009000   03 KVBEART           PIC S9(7) COMP-3.                                 
009100*              BESTÄLLT ANTAL STYCKEN                                     
009200   03 KVLEVART          PIC S9(7) COMP-3.                                 
009300*              LEVERERAT ANTAL STYCK                                      
009400   03 KDANMORS          PIC X(2).                                         
009500*              ORSAK TILL LEVERANSANMÄRKNING                              
009600   03 IDFAKREF          PIC S9(9) COMP-3.                                 
009700*              URSPRUNGLIGT FAKTURANUMMER                                 
009800   03 DAFAKREF          PIC X(8).                                         
009900*              URSPRUNGLIGT FAKTURADATUM (ÅÅÅÅMMDD)                       
010000   03 IDDC              PIC X(2).                                         
010100*              IDENTIFIERARE LAGER                                        
010200   03 KDFRAKT           PIC S9(3) COMP-3.                                 
010300*              FRAKTSÄTT DC TILL KUND                                     
010400   03 BELEVVIL          PIC X(35).                                        
010500*              LEVERANSVILLKOR                                            
010600   03 IDACCNT-1         PIC X(15).                                        
010700*              KONTOFÄLT                                                  
010800   03 IDACCNT-2         PIC X(15).                                        
010900*              KONTOFÄLT                                                  
011000   03 IDACCNT-3         PIC X(15).                                        
011100*              KONTOFÄLT                                                  
011200   03 IDACCNT-4         PIC X(15).                                        
011300*              KONTOFÄLT                                                  
011400   03 IDSYSTEM-SEND     PIC X(4).                                         
011500*              VOLVO SÄNDANDE SYSTEM                                      
011600   03 IDSYSTEM-REC      PIC X(4).                                         
011700*              VOLVO MOTTAGANDE SYSTEM                                    
011800   03 VKORDBTO-KOLLI    PIC S9(6)V9(1) COMP-3.                            
011900*              ORDERVIKT BRUTTO PER KOLLI                                 
012000   03 VKARTNTO          PIC S9(4)V9(3) COMP-3.                            
012100*              ARTIKELVIKT NETTO (KG) MED EMB                             
012200   03 KDARTURS          PIC X(2).                                         
012300*              ARTIKELURSPRUNGSKOD                                        
012400   03 FILLER            PIC X(100).                                       
012500   03 BEANST            PIC X(25).                                        
012600*              ANSTÄLLDS NAMN                                             
012700   03 IDUSER            PIC X(8).                                         
012800*              ANVÄNDARENS SÄKERHETS ID                                   
012900   03 BETEXT            PIC X(125).                                       
013000   03 BETEXT-CRE        PIC X(100).                                       
013100   03 IDARTNR-CNTRL     PIC X(2).                                         
013200*              KONTROLLSIFFRA FÖR ARTIKELNUMMER                           
013300   03 FLPCOO            PIC X(1).                                         
013400*              FLAGGA OM FÖRMÅNSAVTAL URS.LAND                            
013500   03 IDLEVNR-ART       PIC X(5).                                         
013600*              LEVERANTÖRNR PÅ ARTIKEL                                    
013700   03 IDTRACK-1         PIC X(25).                                        
013800*              TRACKING ID FROM CUSTOMS                                   
013900   03 IDTRACK-2         PIC X(25).                                        
014000*              TRACKING ID FROM CUSTOMS                                   
014100   03 IDTRACK-3         PIC X(25).                                        
014200*              TRACKING ID FROM CUSTOMS                                   
014300   03 IDTRACK-4         PIC X(25).                                        
014400*              TRACKING ID FROM CUSTOMS                                   
014500   03 IDTRACK-5         PIC X(25).                                        
014600*              TRACKING ID FROM CUSTOMS                                   
014700   03 KVANT-TRACK-1     PIC S9(7) COMP-3.                                 
014800*              TILLHÖR AV EN VISS ARTIKEL                                 
014900   03 KVANT-TRACK-2     PIC S9(7) COMP-3.                                 
015000*              TILLHÖR AV EN VISS ARTIKEL                                 
015100   03 KVANT-TRACK-3     PIC S9(7) COMP-3.                                 
015200*              TILLHÖR AV EN VISS ARTIKEL                                 
015300   03 KVANT-TRACK-4     PIC S9(7) COMP-3.                                 
015400*              TILLHÖR AV EN VISS ARTIKEL                                 
015500   03 KVANT-TRACK-5     PIC S9(7) COMP-3.                                 
015600*              TILLHÖR AV EN VISS ARTIKEL                                 
015700   03 KDPRMOD           PIC X(2).                                         
015800*              VILKEN PRISMODELL SOM ANVÄNDS                              
015900*                                                                         
016000*** END OF VILMAII-COPY LENGTH= 1017 OLD LENGTH=                          
