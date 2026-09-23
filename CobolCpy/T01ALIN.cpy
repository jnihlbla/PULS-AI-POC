000100* GENERATION OF COBOL HOST STRUCTURE FROM T01ALIN-TAB                     
000200  01 T01ALIN.                                                             
000300*              T01ALIN                                                    
000400   03 IDLEGSEL          PIC X(4).                                         
000500*              FAKTURERANDE FÖRETAG TEX VCCS                              
000600   03 IDBUNDLE          PIC X(15).                                        
000700*              BUNDLE ID                                                  
000800   03 DAREGDAT          PIC X(8).                                         
000900*              REGISTRERINGSDATUM (ÅÅÅÅMMDD)                              
001000   03 TIREGTID          PIC S9(10) COMP-3.                                
001100*              REGISTRERINGSTID                                           
001200   03 IDREF             PIC X(15).                                        
001300*              REFERENS ID                                                
001400   03 DAREFDAT          PIC X(8).                                         
001500*              REFERENSDATUM (ÅÅÅÅMMDD)                                   
001600   03 IDREFRAD          PIC S9(5) COMP-3.                                 
001700*              REFERENSRADSNR                                             
001800   03 BEVOLREF          PIC X(10).                                        
001900*              VOLVO REFERENS                                             
002000   03 IDAPPEND          PIC X(8).                                         
002100*              APPENDIXVÄRDE                                              
002200   03 IDLANDX3-SEND     PIC X(3).                                         
002300*              LANDKOD SÄNDANDE LAND                                      
002400   03 IDLANDX3-REC      PIC X(3).                                         
002500*              LANDKOD MOTTAGANDE LAND                                    
002600   03 IDLEVNR           PIC X(5).                                         
002700*              LEVERANTÖRNUMMER                                           
002800   03 IDPARTNR          PIC X(9).                                         
002900*              FINANCIELL KUND                                            
003000   03 IDEXCUST-1        PIC X(15).                                        
003100*              EXTERNT KUNDID                                             
003200   03 IDEXCUST-2        PIC X(15).                                        
003300*              EXTERNT KUNDID                                             
003400   03 IDEXCUST-3        PIC X(15).                                        
003500*              EXTERNT KUNDID                                             
003600   03 IDOPTION-1        PIC X(15).                                        
003700*              BRYTBEGREPP                                                
003800   03 IDOPTION-2        PIC X(15).                                        
003900*              BRYTBEGREPP                                                
004000   03 IDOPTION-3        PIC X(15).                                        
004100*              BRYTBEGREPP                                                
004200   03 IDOPTION-4        PIC X(15).                                        
004300*              BRYTBEGREPP                                                
004400   03 IDOPTION-5        PIC X(15).                                        
004500*              BRYTBEGREPP                                                
004600   03 IDARTNR-FINANCE   PIC X(50).                                        
004700*              ARTIKELNUMMER FÖR FINANSIELL BRUK                          
004800   03 IDSTATNR          PIC S9(9) COMP-3.                                 
004900*              STATISTISKT NUMMER                                         
005000*              1 = NORSKT                                                 
005100*              2 = ENGELSKT                                               
005200*              3 = BELGISKT                                               
005300*              4 = PERUANSKT                                              
005400*              5 = SVENSKT                                                
005500*              6 =                                                        
005600   03 VKORDBTO-KOLLI    PIC S9(6)V9(1) COMP-3.                            
005700*              ORDERVIKT BRUTTO PER KOLLI                                 
005800   03 VKARTNTO          PIC S9(4)V9(3) COMP-3.                            
005900*              ARTIKELVIKT NETTO (KG) MED EMB                             
006000   03 PRARTBTO          PIC S9(7)V9(2) COMP-3.                            
006100*              FÖRSÄLJNINGSPRIS BRUTTO (KR)                               
006200   03 PRARTNTO          PIC S9(7)V9(2) COMP-3.                            
006300*              ARTIKELPRIS NETTO                                          
006400   03 REARTRAB          PIC S9(2)V9(2) COMP-3.                            
006500*              ARTIKELRABATT                                              
006600   03 KVBEART           PIC S9(7) COMP-3.                                 
006700*              BESTÄLLT ANTAL STYCKEN                                     
006800   03 KVLEVART          PIC S9(7) COMP-3.                                 
006900*              LEVERERAT ANTAL STYCK                                      
007000   03 BEART             PIC X(25).                                        
007100*              ARTIKELBENÄMNING                                           
007200   03 FLSOFT            PIC X(1).                                         
007300*              FLAGGA SOFTVARA                                            
007400   03 FLSPECPR          PIC X(1).                                         
007500*              SPECIALPRISFLAGGA                                          
007600   03 FLFREE            PIC X(1).                                         
007700*              GRATISFATURA                                               
007800   03 FLPRIV            PIC X(1).                                         
007900*              KÖPARE ÄR EN PRIVATPERSON                                  
008000   03 KDVAT             PIC X(2).                                         
008100*              MOMSKOD                                                    
008200   03 KDVALISO          PIC X(3).                                         
008300*              VALUTAKOD ENLIGT ISO-STANDARD.                             
008400   03 KDINVFRQ          PIC X(4).                                         
008500*              FAKTURERINGSFREKVENS                                       
008600   03 KDFINDOC          PIC X(4).                                         
008700*              TYP FINANSIELLT DOKUMENT                                   
008800   03 IDBREAK-1         PIC X(8).                                         
008900*              BRYTVÄRDE                                                  
009000   03 IDBREAK-2         PIC X(8).                                         
009100*              BRYTVÄRDE                                                  
009200   03 IDSEQ-1           PIC X(8).                                         
009300*              SEKVENSVÄRDE                                               
009400   03 IDSEQ-2           PIC X(8).                                         
009500*              SEKVENSVÄRDE                                               
009600   03 IDSEQ-3           PIC X(8).                                         
009700*              SEKVENSVÄRDE                                               
009800   03 KDARTURS          PIC X(2).                                         
009900*              ARTIKELURSPRUNGSKOD                                        
010000   03 KDANMORS          PIC X(2).                                         
010100*              ORSAK TILL LEVERANSANMÄRKNING                              
010200   03 IDFAKREF          PIC S9(9) COMP-3.                                 
010300*              URSPRUNGLIGT FAKTURANUMMER                                 
010400   03 DAFAKREF          PIC X(8).                                         
010500*              URSPRUNGLIGT FAKTURADATUM (ÅÅÅÅMMDD)                       
010600   03 IDDC              PIC X(2).                                         
010700*              IDENTIFIERARE LAGER                                        
010800   03 KDFRAKT           PIC S9(3) COMP-3.                                 
010900*              FRAKTSÄTT DC TILL KUND                                     
011000   03 BELEVVIL          PIC X(35).                                        
011100*              LEVERANSVILLKOR                                            
011200   03 IDACCNT-1         PIC X(15).                                        
011300*              KONTOFÄLT                                                  
011400   03 IDACCNT-2         PIC X(15).                                        
011500*              KONTOFÄLT                                                  
011600   03 IDACCNT-3         PIC X(15).                                        
011700*              KONTOFÄLT                                                  
011800   03 IDACCNT-4         PIC X(15).                                        
011900*              KONTOFÄLT                                                  
012000   03 IDSYSTEM-SEND     PIC X(4).                                         
012100*              VOLVO SÄNDANDE SYSTEM                                      
012200   03 IDSYSTEM-REC      PIC X(4).                                         
012300*              VOLVO MOTTAGANDE SYSTEM                                    
012400   03 DASTADAT          PIC X(8).                                         
012500*              GENERELLT STARTDATUM                                       
012600   03 FILLER            PIC X(100).                                       
012700   03 BEANST            PIC X(25).                                        
012800*              ANSTÄLLDS NAMN                                             
012900   03 IDUSER            PIC X(8).                                         
013000*              ANVÄNDARENS SÄKERHETS ID                                   
013100   03 BETEXT            PIC X(125).                                       
013200   03 BETEXT-CRE        PIC X(100).                                       
013300   03 IDARTNR-CNTRL     PIC X(2).                                         
013400*              KONTROLLSIFFRA FÖR ARTIKELNUMMER                           
013500   03 FLPCOO            PIC X(1).                                         
013600*              FLAGGA OM FÖRMÅNSAVTAL URS.LAND                            
013700   03 IDLEVNR-ART       PIC X(5).                                         
013800*              LEVERANTÖRNR PÅ ARTIKEL                                    
013900   03 IDTRACK-1         PIC X(25).                                        
014000*              TRACKING ID FROM CUSTOMS                                   
014100   03 IDTRACK-2         PIC X(25).                                        
014200*              TRACKING ID FROM CUSTOMS                                   
014300   03 IDTRACK-3         PIC X(25).                                        
014400*              TRACKING ID FROM CUSTOMS                                   
014500   03 IDTRACK-4         PIC X(25).                                        
014600*              TRACKING ID FROM CUSTOMS                                   
014700   03 IDTRACK-5         PIC X(25).                                        
014800*              TRACKING ID FROM CUSTOMS                                   
014900   03 KVANT-TRACK-1     PIC S9(7) COMP-3.                                 
015000*              TILLHÖR AV EN VISS ARTIKEL                                 
015100   03 KVANT-TRACK-2     PIC S9(7) COMP-3.                                 
015200*              TILLHÖR AV EN VISS ARTIKEL                                 
015300   03 KVANT-TRACK-3     PIC S9(7) COMP-3.                                 
015400*              TILLHÖR AV EN VISS ARTIKEL                                 
015500   03 KVANT-TRACK-4     PIC S9(7) COMP-3.                                 
015600*              TILLHÖR AV EN VISS ARTIKEL                                 
015700   03 KVANT-TRACK-5     PIC S9(7) COMP-3.                                 
015800*              TILLHÖR AV EN VISS ARTIKEL                                 
015900   03 KDPRMOD           PIC X(2).                                         
016000*              VILKEN PRISMODELL SOM ANVÄNDS                              
016100*                                                                         
016200*** END OF VILMAII-COPY LENGTH= 1028 OLD LENGTH=                          
