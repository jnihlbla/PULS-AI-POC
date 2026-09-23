000100 01  W61215.                                                              
000200*                                 RENSADE SEGMENT FRÅN WDL6               
000300*                                 ÄLDRE ÄN 1 ÅR                           
000400     03 IDARTNR              PIC S9(9)           COMP-3.                  
000500*                                 ARTIKELNUMMER                           
000600*                                 PART NUMBER                             
000700     03 WDL611.                                                           
000800*                                 INLEVERANS HISTORIK SDC                 
000900*                                 INLEVERANSENS IDENTITET                 
001000*                                 R30 FAKTURERAT I CDC                    
001100*                                 310 MOTTAGET I SDC/NDC                  
001200*                                 R32 RAPPORTERAD                         
001300*                                 FYSISK NYCKEL: DAINLEV                  
001400*                                 SÖKBEGREPP IDDC                         
001500*                                            IDPTYP                       
001600        05 DAINLEV           PIC 9(16).                                   
001700*                                 INLEVERANS NUMMER                       
001800*                                 CONSIGNMENT IDENTITY                    
001900*                                 (YYYYMMDD+HHMMSSTH)                     
002000        05 ADART.                                                         
002100*                                 ARTIKELADRESS I LAGRET                  
002200*                                 PARTS-ADRESS                            
002300           07 ADLAGOMR       PIC S9(3)           COMP-3.                  
002400*                                 LAGEROMRÅDE                             
002500*                                 AREA                                    
002600           07 ADGANG         PIC S9(3)           COMP-3.                  
002700*                                 GÅNG                                    
002800*                                 AISLE                                   
002900           07 ADPLATS        PIC S9(5)           COMP-3.                  
003000*                                 LAGERPLATSNUMMER                        
003100*                                 LOCATION                                
003200        05 FLMAKUL           PIC X.                                       
003300*                                 FLAGGA MAKULERAT KOLLI                  
003400*                                 CASE CANCELLATION FLAG                  
003500        05 FLPRIO            PIC X.                                       
003600*                                 PRIORITERAD                             
003700*                                 HAS PRIORITY                            
003800        05 FLSKAKOL          PIC X.                                       
003900*                                 FLAGGA SKADAT KOLLI.                    
004000*                                 DAMAGED CASE FLAG                       
004100        05 IDDC              PIC X(2).                                    
004200*                                 IDENTIFIERARE LAGER                     
004300*                                 WAREHOUSE IDENTIFIER                    
004400        05 IDLEVNR           PIC X(5).                                    
004500*                                 LEVERANTÖRNUMMER                        
004600*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
004700        05 IDLOPNRM          PIC S9(9)           COMP-3.                  
004800*                                 LÖPNUMMER MOTTAGNINGSRAPPORT            
004900*                                 (0VVDLLLLK)                             
005000*                                 SERIAL NO RECEIVING REPORT              
005100*                                 (0WWDLLLLC)                             
005200        05 IDFAKT            PIC S9(7)           COMP-3.                  
005300*                                 FAKTURANUMMER                           
005400*                                 INVOICE NO.                             
005500        05 IDGMTREF.                                                      
005600*                                 GODSMOTTAGAREREFERENS                   
005700*                                 GOODS RECEIVER REFERENS                 
005800           07 IDDISTR        PIC S9(5)           COMP-3.                  
005900*                                 DISTRIKTNUMMER                          
006000*                                 DISTRICT NUMBER                         
006100           07 IDKUNDNR       PIC S9(7)           COMP-3.                  
006200*                                 KUNDNUMMER                              
006300*                                 CUSTOMER NO                             
006400           07 IDKUNDRF-GRP.                                               
006500*                                 KUNDENS REFERENS (ORDERID)              
006600*                                 CUSTOMER REFERENCE (ORDER ID)           
006700              09 IDKUNDRF    PIC X(10).                                   
006800*                                 KUNDENS REFERENS (ORDERID)              
006900*                                 CUSTOMER REFERENCE (ORDER ID)           
007000              09 IDORDNR5-FILLER REDEFINES IDKUNDRF.                      
007100                 11 IDORDNR5 PIC 9(5).                                    
007200*                                 ORDERNUMMER                             
007300*                                 ORDER NUMBER                            
007400                 11 FILLER   PIC X(5).                                    
007500              09 IDORDNR7-FILLER REDEFINES IDKUNDRF.                      
007600                 11 IDORDNR7 PIC 9(7).                                    
007700*                                 ORDERNUMMER                             
007800*                                 ORDER NUMBER                            
007900                 11 FILLER   PIC X(3).                                    
008000        05 IDKOLLI           PIC S9(5)           COMP-3.                  
008100*                                 KOLLINUMMER                             
008200*                                 CASE NUMBER                             
008300        05 IDPTYP            PIC X(3).                                    
008400*                                 POSTTYP                                 
008500*                                 RECORD TYPE                             
008600        05 KDFRAKT           PIC S9(3)           COMP-3.                  
008700*                                 FRAKTSÄTT DC TILL KUND                  
008800*                                 FREIGHT CODE                            
008900        05 KDKOLLI           PIC X(8).                                    
009000*                                 KOLLIKOD                                
009100*                                 KOLLI CODE                              
009200        05 KDRT              PIC S9(3)           COMP-3.                  
009300*                                 REDOVISNINGSTYP                         
009400*                                 TYPE OF ACCOUNTING                      
009500        05 KDVALISO          PIC X(3).                                    
009600*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
009700*                                 CURRENCY CODE BY ISO-STANDARD.          
009800        05 KVANTMOT          PIC S9(7)           COMP-3.                  
009900*                                 ANTAL MOTTAGET                          
010000*                                 QUANTITY RECEIVED                       
010100        05 KVART-SKROT       PIC S9(7)           COMP-3.                  
010200*                                 ANTAL SKROTADE ARTIKLAR                 
010300*                                 QUANTITY INSPECTED PARTS                
010400        05 KVAVIS            PIC S9(7)           COMP-3.                  
010500*                                 AVISERAT ANTAL                          
010600*                                 QUANTITY NOTIFIED                       
010700        05 PRARTNTO          PIC S9(7)V9(2)      COMP-3.                  
010800*                                 ARTIKELPRIS NETTO                       
010900*                                 NET PRICE EACH   (FOB NET)              
011000        05 PRKURS            PIC S9(6)V9(5)      COMP-3.                  
011100*                                 VALUTAKURS                              
011200*                                 CURRENCY EXCHANGE RATE                  
011300        05 TIBERANK          PIC 9(6).                                    
011400*                                 BERÄKNAD ANKOMSTDATUM                   
011500*                                 ESTIMATED RECEIVING DATE                
011600        05 TIINLMOT          PIC S9(7)           COMP-3.                  
011700*                                 MOTTAGNINGSDATUM   (ÅÅMMDD)             
011800*                                 RECEIVING DATE    (YYMMDD)              
011900        05 TIINLMTI          PIC 9(4).                                    
012000*                                 MOTTAGNINGSTID     (TTMM)               
012100*                                 RECEIVING HOUR    (HHMM)                
012200        05 TIINLINL          PIC S9(7)           COMP-3.                  
012300*                                 RAPPORTERINGSDATUM INLAGD (R32)         
012400*                                 DATE OF REPORTED IN STOCK (R32)         
012500        05 TIINLITI          PIC 9(4).                                    
012600*                                 RAPPORTERINGSTID   INLAGD (R32)         
012700*                                 HOUR REPORTED IN STOCK (R32)            
012800        05 ADINLOMR          PIC X(4).                                    
012900*                                 INLEVERANSOMRÅDE                        
013000*                                 RECEIVING AREA                          
013100        05 IDANALYS          PIC X(12).                                   
013200*                                 ANALYSNUMMER                            
013300*                                 ANALYSIS NUMBER                         
013400        05 IDKONTO           PIC S9(11)          COMP-3.                  
013500*                                 KONTO                                   
013600*                                 ACCOUNT                                 
013700        05 IDKST             PIC X(10).                                   
013800*                                 KOSTNADSSTÄLLE                          
013900*                                 COST CENTRE                             
014000        05 IDUSER-003        PIC X(5).                                    
014100*                                 ANSVARIGT USERID INLÄGGN.(R32)          
014200        05 TILASTN-BOAT      PIC S9(7)           COMP-3.                  
014300*                                 LASTNINGSDATUM         (ÅÅMMDD)         
014400*                                 FÖR BÅT                                 
014500*                                 LOADING DATE           (YYMMDD)         
014600*                                 OF BOAT                                 
014700        05 TILOSSN-BOAT      PIC S9(7)           COMP-3.                  
014800*                                 LOSSNINGSDATUM                          
014900*                                 DATE OF UNLOADING OF BOAT               
015000        05 KVRETUR           PIC S9(7)           COMP-3.                  
015100*                                 ANTAL I RETUR                           
015200*                                 QUANTITY IN RETURN                      
015300        05 KDAVVANT          PIC S9              COMP-3.                  
015400*                                 AVVIKELSEANTAL KOD                      
015500*                                 0=INGEN ANM.   1=AVVIKELSE              
015600*                                 2=MAKULERING AV MOTT.RAPPORT            
015700*                                 QUANTITY DEVIATION  CODE                
015800*                                 0=NO DEV.    1=DEVIATION                
015900*                                 2=CANCELLING OF REC. REPORT             
016000        05 TIAVIDAT          PIC S9(7)           COMP-3.                  
016100*                                 AVISERINGSDATUM (YYMMDD)                
016200*                                 ADVICE NOTE DATE                        
016300        05 FILLER            PIC X(6).                                    
016400*** END OF VILMAII-COPY LENGTH= 190 BYTES                                 
