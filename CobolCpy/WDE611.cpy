000100 01  KOLLI-WDE611.                                                        
000200*                                 KOLLIREGISTER                           
000300*                                 KOLLISEGMENT                            
000400*                                 FYSISK NYCKEL: IDKOLLI                  
000500*                                 SÖKBEGREPP:                             
000600*                                 KDKOLSTA (KOLLISTATUS)                  
000700*                                 IDFAKLOP (FAKTURA LÖPNUMMER)            
000800     03 KOLLI-IDKOLLI        PIC S9(5)           COMP-3.                  
000900*                                 KOLLINUMMER                             
001000     03 KOLLI-IDKOLLI-FLER   PIC S9(5)           COMP-3.                  
001100*                                 SAMPACKNINGSKOLLINUMMER                 
001200     03 KOLLI-IDTRPTNR       PIC S9(3)           COMP-3.                  
001300*                                 TRANSPORTIDENTITET                      
001400     03 KOLLI-ADKOLLI.                                                    
001500*                                 KOLLI-ADRESS                            
001600        05 KOLLI-ADCLGEO.                                                 
001700*                                 IDDC + GEO ADRESS                       
001800           07 KOLLI-IDDC     PIC X(2).                                    
001900*                                 IDENTIFIERARE LAGER                     
002000           07 KOLLI-ADFLGEO  PIC X(3).                                    
002100*                                 GEOGRAFISKT OMRÅDE FÄRDIGLAGER          
002200        05 KOLLI-ADFLOMR     PIC S9(3)           COMP-3.                  
002300*                                 LASTNINGSOMR/STÄLL FÄRDIGLAGER          
002400        05 KOLLI-ADRUTNIV    PIC S9(3)           COMP-3.                  
002500*                                 RUTA/NIVÅ I FÄRDIGLAGRET                
002600        05 KOLLI-ADVMODUL    PIC S9(3)           COMP-3.                  
002700*                                 VÄNSTER-MODUL                           
002800     03 KOLLI-DIDMODUL       PIC S9(3)           COMP-3.                  
002900*                                 MODUL-DJUP                              
003000     03 KOLLI-DIHMODUL       PIC S9(3)           COMP-3.                  
003100*                                 MODUL-HÖJD                              
003200     03 KOLLI-FLUTLAST       PIC X.                                       
003300*                                 KOLLI I UTLASTNINGSLAGER                
003400     03 KOLLI-FLBANDST       PIC X.                                       
003500*                                 BANDSTATIONSFLAGGA                      
003600     03 KOLLI-DIKOLLIL       PIC S9(5)           COMP-3.                  
003700*                                 KOLLI-LÄNGD                             
003800     03 KOLLI-DIKOLLIB       PIC S9(3)           COMP-3.                  
003900*                                 KOLLI-BREDD                             
004000     03 KOLLI-DIKOLLIH       PIC S9(3)           COMP-3.                  
004100*                                 KOLLI-HÖJD                              
004200     03 KOLLI-FLTULLG        PIC X.                                       
004300*                                 FINNS TULLGRUPP ?                       
004400     03 KOLLI-IDFAKLOP       PIC S9(3)           COMP-3.                  
004500*                                 FAKTURALÖPNUMMER                        
004600     03 KOLLI-IDFAKT         PIC S9(7)           COMP-3.                  
004700*                                 FAKTURANUMMER                           
004800     03 KOLLI-IDLBBET        PIC X(12).                                   
004900*                                 LASTBÄRARBETECKNING                     
005000     03 KOLLI-IDPLOCK        PIC S9(7)           COMP-3.                  
005100*                                 PLOCKARE                                
005200     03 KOLLI-KDARTURS-KOLLI PIC X(2).                                    
005300*                                 ARTIKELURSRPRUNGSKOD FÖR KOLLI          
005400     03 KOLLI-KDEMBTYP       PIC S9              COMP-3.                  
005500*                                 EMBALLAGETYP                            
005600     03 KOLLI-KVFALRAD       PIC S9(5)           COMP-3.                  
005700*                                 ANTAL RADER MED FARLIGT GODS            
005800     03 KOLLI-KDKOLLI        PIC X(8).                                    
005900*                                 KOLLIKOD                                
006000     03 KOLLI-KDKOLSTA       PIC S9              COMP-3.                  
006100*                                 KOLLISTATUS                             
006200     03 KOLLI-KVFLAMP-KOLLI  PIC S9(2)V9(1)      COMP-3.                  
006300*                                 KOLLITS FLAMPUNKT                       
006400     03 KOLLI-KVORDRAD       PIC S9(5)           COMP-3.                  
006500*                                 ANTAL ORDERRADER                        
006600     03 KOLLI-TIAAVVD-PATR   PIC S9(5)           COMP-3.                  
006700*                                 SÄDN.DATUM AV PACKNINGSTRANS            
006800     03 KOLLI-SUORDV-KOLLI   PIC S9(9)V9(2)      COMP-3.                  
006900*                                 VARUVÄRDE PER KOLLI                     
007000     03 KOLLI-TIFAKT         PIC S9(7)           COMP-3.                  
007100*                                 FAKTURERINGSDATUM (ÅÅMMDD)              
007200     03 KOLLI-TIFAKTID       PIC S9(7)           COMP-3.                  
007300*                                 FAKTURERINGSTID                         
007400     03 KOLLI-TILASTN        PIC S9(7)           COMP-3.                  
007500*                                 LASTNINGSDATUM         (ÅÅMMDD)         
007600     03 KOLLI-TILASTID       PIC S9(7)           COMP-3.                  
007700*                                 LASTNINGSTID                            
007800     03 KOLLI-TIPACKN        PIC S9(7)           COMP-3.                  
007900*                                 PACKNINGSDATUM         (ÅÅMMDD)         
008000     03 KOLLI-TIPACTID       PIC S9(7)           COMP-3.                  
008100*                                 PACKNINGSTID  TTMMSS                    
008200     03 KOLLI-VKORDNTO-KOLLI PIC S9(6)V9(1)      COMP-3.                  
008300*                                 ORDERVIKT NETTO PER KOLLI               
008400     03 KOLLI-IDDISTR        PIC S9(5)           COMP-3.                  
008500*                                 DISTRIKTNUMMER                          
008600     03 KOLLI-IDKUNDNR       PIC S9(7)           COMP-3.                  
008700*                                 KUNDNUMMER                              
008800     03 KOLLI-ADHMODUL       PIC S9(3)           COMP-3.                  
008900*                                 HÖGER-MODUL                             
009000     03 KOLLI-KDFARLIG-KOLLI PIC S9              COMP-3.                  
009100*                                 KOD FÖR FARLIGT GODS I KOLLI            
009200     03 KOLLI-VKORDBTO-KOLLI PIC S9(6)V9(1)      COMP-3.                  
009300*                                 ORDERVIKT BRUTTO PER KOLLI              
009400     03 KOLLI-VLORDBTO-KOLLI PIC S9(4)V9(3)      COMP-3.                  
009500*                                 ORDERVOLYM BRUTTO KOLLI                 
009600     03 KOLLI-KDORDKL        PIC S9              COMP-3.                  
009700*                                 ORDERKLASS                              
009800     03 KOLLI-FLAUTFAK       PIC X.                                       
009900*                                 AUTOMATFAKTURERING ?                    
010000     03 KOLLI-IDLASTN        PIC S9(7)           COMP-3.                  
010100*                                 LASTNINGSNUMMER                         
010200     03 KOLLI-FLFRSUTS       PIC X.                                       
010300*                                 FLAGGA FRAKTSEDEL UTSKRIVEN             
010400     03 KOLLI-FG-PSN         OCCURS 10 TIMES.                             
010500        05 KOLLI-IDPSN       PIC 9(3).                                    
010600*                                 PROPER SHIPPING NAME                    
010700        05 KOLLI-VKART-FG    PIC S9(7)           COMP-3.                  
010800*                                 NETTOVIKT EXPLOSIVA ÄMNEN               
010900        05 KOLLI-VLFG        PIC S9(4)V9(3)      COMP-3.                  
011000*                                 VOLYM FARLIGT GODS                      
011100     03 KOLLI-SUEQFG         PIC S9(3)V9(4)      COMP-3.                  
011200*                                 EQ-VÄRDE FARLIGT GODS                   
011300     03 KOLLI-DARFS          PIC 9(12).                                   
011400*                                 KLART FÖR TRANSPORT                     
011500     03 KOLLI-IDKOLLI-SAMP   PIC S9(5)           COMP-3.                  
011600*                                 SAMPACKNINGSKOLLINUMMER                 
011700     03 KOLLI-IDTRP.                                                      
011800*                                 TRANSPORTIDENTITET                      
011900        05 KOLLI-IDTRPLOS    PIC X(3).                                    
012000*                                 TRANSPORTLÖSNING                        
012100        05 KOLLI-IDTRPVAR    PIC X(2).                                    
012200*                                 TRANSPORTLÖSNINGSGRUPP                  
012300     03 KOLLI-IDSUPREF       PIC X(10).                                   
012400*                                 LEVERANTöRSREF.                         
012500     03 KOLLI-DASUPREF       PIC 9(8).                                    
012600*                                 SÄNDNINGSDATUM DIREKTLEVERANTÖR         
012700     03 KOLLI-IDLEVNR        PIC X(5).                                    
012800*                                 LEVERANTÖRNUMMER                        
012900     03 KOLLI-KDVIA          PIC X(2).                                    
013000*                                 KOD FöR LEVERANS VIA                    
013100     03 KOLLI-TISUPTID       PIC S9(5)           COMP-3.                  
013200*                                 SÄNDNINGSTID DIREKTLEVERANTÖR           
013300     03 KOLLI-IDTULL.                                                     
013400*                                 IDENTITET TULL SÄNDNING                 
013500        05 KOLLI-IDTULFTG    PIC X(2).                                    
013600*                                 IDENTIFIERARE TULLANDE FÖRETAG          
013700*                                                                         
013800        05 KOLLI-IDTULLNR    PIC 9(7).                                    
013900*                                 NUMMERSERIE INGÅENDE I TULLID           
014000*                                                                         
014100        05 KOLLI-RETULKS     PIC 9.                                       
014200*                                 KONTROLLSIFFRA TULLID                   
014300     03 KOLLI-DEAL-PR-SUM.                                                
014400*                                 DEALERPRIS (HUVUD)                      
014500        05 KOLLI-SUORDV-LOC  PIC S9(9)V9(2)      COMP-3.                  
014600*                                 ORDERVÄRDE SLUTKUNDPRIS                 
014700*                                 I LOKAL VALUTA                          
014800        05 KOLLI-SUORDV-LOCPREL                                           
014900                             PIC S9(9)V9(2)      COMP-3.                  
015000*                                 ORDERVÄRDE PREL SLUT-                   
015100*                                 KUNDPRIS, LOKAL VALUTA                  
015200        05 KOLLI-KDVALISO    PIC X(3).                                    
015300*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
015400     03 KOLLI-IDSHIPM        PIC 9(7).                                    
015500*                                 SKEPPNINGSNUMMER                        
015600     03 KOLLI-KDSTASKLI      PIC X.                                       
015700*                                 STATUS SAMLINGSKOLLI                    
015800     03 KOLLI-TIFAKT-EXP     PIC S9(7)           COMP-3.                  
015900*                                 FAKTDAT EXPORT MED TVÅ FAKTUROR         
016000     03 KOLLI-TIFAKTID-EXP   PIC S9(7)           COMP-3.                  
016100*                                 FAKTTID EXPORT MED TVÅ FAKTUROR         
016200     03 KOLLI-SUORDV-KLI-EXP PIC S9(9)V9(2)      COMP-3.                  
016300*                                 ORDERVÄRDE/KOLLI EXPORTFLÖDE            
016400     03 KOLLI-KDVALISO-EXP   PIC X(3).                                    
016500*                                 VALUTAKOD I EXP.FLÖDE(LOK. VAL)         
016600     03 KOLLI-IDFAKT-EXP     PIC S9(7)           COMP-3.                  
016700*                                 FAKTNR NR.1 I EXPORTFLÖDET              
016800     03 KOLLI-FILLERX2       PIC X(2).                                    
016900*** END OF VILMAII-COPY LENGTH= 355 BYTES                                 
