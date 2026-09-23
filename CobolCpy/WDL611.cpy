000100 01  INL-WDL611.                                                          
000200*                                 INLEVERANS HISTORIK SDC                 
000300*                                 INLEVERANSENS IDENTITET                 
000400*                                 R30 FAKTURERAT I CDC                    
000500*                                 310 MOTTAGET I SDC/NDC                  
000600*                                 R32 RAPPORTERAD                         
000700*                                 FYSISK NYCKEL: DAINLEV                  
000800*                                 SÖKBEGREPP IDDC                         
000900*                                            IDPTYP                       
001000     03 INL-DAINLEV          PIC 9(16).                                   
001100*                                 INLEVERANS NUMMER                       
001200*                                 CONSIGNMENT IDENTITY                    
001300*                                 (YYYYMMDD+HHMMSSTH)                     
001400     03 INL-ADART.                                                        
001500*                                 ARTIKELADRESS I LAGRET                  
001600*                                 PARTS-ADRESS                            
001700        05 INL-ADLAGOMR      PIC S9(3)           COMP-3.                  
001800*                                 LAGEROMRÅDE                             
001900*                                 AREA                                    
002000        05 INL-ADGANG        PIC S9(3)           COMP-3.                  
002100*                                 GÅNG                                    
002200*                                 AISLE                                   
002300        05 INL-ADPLATS       PIC S9(5)           COMP-3.                  
002400*                                 LAGERPLATSNUMMER                        
002500*                                 LOCATION                                
002600     03 INL-FLMAKUL          PIC X.                                       
002700*                                 FLAGGA MAKULERAT KOLLI                  
002800*                                 CASE CANCELLATION FLAG                  
002900     03 INL-FLPRIO           PIC X.                                       
003000*                                 PRIORITERAD                             
003100*                                 HAS PRIORITY                            
003200     03 INL-FLSKAKOL         PIC X.                                       
003300*                                 FLAGGA SKADAT KOLLI.                    
003400*                                 DAMAGED CASE FLAG                       
003500     03 INL-IDDC             PIC X(2).                                    
003600*                                 IDENTIFIERARE LAGER                     
003700*                                 WAREHOUSE IDENTIFIER                    
003800     03 INL-IDLEVNR          PIC X(5).                                    
003900*                                 LEVERANTÖRNUMMER                        
004000*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
004100     03 INL-IDLOPNRM         PIC S9(9)           COMP-3.                  
004200*                                 LÖPNUMMER MOTTAGNINGSRAPPORT            
004300*                                 (0VVDLLLLK)                             
004400*                                 SERIAL NO RECEIVING REPORT              
004500*                                 (0WWDLLLLC)                             
004600     03 INL-IDFAKT           PIC S9(7)           COMP-3.                  
004700*                                 FAKTURANUMMER                           
004800*                                 INVOICE NO.                             
004900     03 INL-IDGMTREF.                                                     
005000*                                 GODSMOTTAGAREREFERENS                   
005100*                                 GOODS RECEIVER REFERENS                 
005200        05 INL-IDDISTR       PIC S9(5)           COMP-3.                  
005300*                                 DISTRIKTNUMMER                          
005400*                                 DISTRICT NUMBER                         
005500        05 INL-IDKUNDNR      PIC S9(7)           COMP-3.                  
005600*                                 KUNDNUMMER                              
005700*                                 CUSTOMER NO                             
005800        05 INL-IDKUNDRF-GRP.                                              
005900*                                 KUNDENS REFERENS (ORDERID)              
006000*                                 CUSTOMER REFERENCE (ORDER ID)           
006100           07 INL-IDKUNDRF   PIC X(10).                                   
006200*                                 KUNDENS REFERENS (ORDERID)              
006300*                                 CUSTOMER REFERENCE (ORDER ID)           
006400           07 INL-IDORDNR5-FILLER REDEFINES INL-IDKUNDRF.                 
006500              09 INL-IDORDNR5                                             
006600                             PIC 9(5).                                    
006700*                                 ORDERNUMMER                             
006800*                                 ORDER NUMBER                            
006900              09 FILLER      PIC X(5).                                    
007000           07 INL-IDORDNR7-FILLER REDEFINES INL-IDKUNDRF.                 
007100              09 INL-IDORDNR7                                             
007200                             PIC 9(7).                                    
007300*                                 ORDERNUMMER                             
007400*                                 ORDER NUMBER                            
007500              09 FILLER      PIC X(3).                                    
007600     03 INL-IDKOLLI          PIC S9(5)           COMP-3.                  
007700*                                 KOLLINUMMER                             
007800*                                 CASE NUMBER                             
007900     03 INL-IDPTYP           PIC X(3).                                    
008000*                                 POSTTYP                                 
008100*                                 RECORD TYPE                             
008200     03 INL-KDFRAKT          PIC S9(3)           COMP-3.                  
008300*                                 FRAKTSÄTT DC TILL KUND                  
008400*                                 FREIGHT CODE                            
008500     03 INL-KDKOLLI          PIC X(8).                                    
008600*                                 KOLLIKOD                                
008700*                                 KOLLI CODE                              
008800     03 INL-KDRT             PIC S9(3)           COMP-3.                  
008900*                                 REDOVISNINGSTYP                         
009000*                                 TYPE OF ACCOUNTING                      
009100     03 INL-KDVALISO         PIC X(3).                                    
009200*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
009300*                                 CURRENCY CODE BY ISO-STANDARD.          
009400     03 INL-KVANTMOT         PIC S9(7)           COMP-3.                  
009500*                                 ANTAL MOTTAGET                          
009600*                                 QUANTITY RECEIVED                       
009700     03 INL-KVART-SKROT      PIC S9(7)           COMP-3.                  
009800*                                 ANTAL SKROTADE ARTIKLAR                 
009900*                                 QUANTITY INSPECTED PARTS                
010000     03 INL-KVAVIS           PIC S9(7)           COMP-3.                  
010100*                                 AVISERAT ANTAL                          
010200*                                 QUANTITY NOTIFIED                       
010300     03 INL-PRARTNTO         PIC S9(7)V9(2)      COMP-3.                  
010400*                                 ARTIKELPRIS NETTO                       
010500*                                 NET PRICE EACH   (FOB NET)              
010600     03 INL-PRKURS           PIC S9(6)V9(5)      COMP-3.                  
010700*                                 VALUTAKURS                              
010800*                                 CURRENCY EXCHANGE RATE                  
010900     03 INL-TIBERANK         PIC 9(6).                                    
011000*                                 BERÄKNAD ANKOMSTDATUM                   
011100*                                 ESTIMATED RECEIVING DATE                
011200     03 INL-TIINLMOT         PIC S9(7)           COMP-3.                  
011300*                                 MOTTAGNINGSDATUM   (ÅÅMMDD)             
011400*                                 RECEIVING DATE    (YYMMDD)              
011500     03 INL-TIINLMTI         PIC 9(4).                                    
011600*                                 MOTTAGNINGSTID     (TTMM)               
011700*                                 RECEIVING HOUR    (HHMM)                
011800     03 INL-TIINLINL         PIC S9(7)           COMP-3.                  
011900*                                 RAPPORTERINGSDATUM INLAGD (R32)         
012000*                                 DATE OF REPORTED IN STOCK (R32)         
012100     03 INL-TIINLITI         PIC 9(4).                                    
012200*                                 RAPPORTERINGSTID   INLAGD (R32)         
012300*                                 HOUR REPORTED IN STOCK (R32)            
012400     03 INL-ADINLOMR         PIC X(4).                                    
012500*                                 INLEVERANSOMRÅDE                        
012600*                                 RECEIVING AREA                          
012700     03 INL-IDANALYS         PIC X(12).                                   
012800*                                 ANALYSNUMMER                            
012900*                                 ANALYSIS NUMBER                         
013000     03 INL-IDKONTO          PIC S9(11)          COMP-3.                  
013100*                                 KONTO                                   
013200*                                 ACCOUNT                                 
013300     03 INL-IDKST            PIC X(10).                                   
013400*                                 KOSTNADSSTÄLLE                          
013500*                                 COST CENTRE                             
013600     03 INL-IDUSER-003       PIC X(5).                                    
013700*                                 ANSVARIGT USERID INLÄGGN.(R32)          
013800     03 INL-FLTULLST         PIC X.                                       
013900*                                 FLAGGA FÖR ART STOPPAD I TULLEN         
014000*                                 FLAG FOR PART HOLD AT CUSTOM            
014100     03 INL-FILLER           PIC X(3).                                    
014200     03 INL-KVTULRET         PIC S9(7)           COMP-3.                  
014300*                                 ANTAL SOM TULLEN RETURNERAT             
014400*                                 RETURNED QUANTITY FROM CUSTOM           
014500     03 INL-KVRETUR          PIC S9(7)           COMP-3.                  
014600*                                 ANTAL I RETUR                           
014700*                                 QUANTITY IN RETURN                      
014800     03 INL-KDAVVANT         PIC S9              COMP-3.                  
014900*                                 AVVIKELSEANTAL KOD                      
015000*                                 0=INGEN ANM.   1=AVVIKELSE              
015100*                                 2=MAKULERING AV MOTT.RAPPORT            
015200*                                 QUANTITY DEVIATION  CODE                
015300*                                 0=NO DEV.    1=DEVIATION                
015400*                                 2=CANCELLING OF REC. REPORT             
015500     03 INL-TIAVIDAT         PIC S9(7)           COMP-3.                  
015600*                                 AVISERINGSDATUM (YYMMDD)                
015700*                                 ADVICE NOTE DATE                        
015800     03 INL-IDDC-LEV         PIC X(2).                                    
015900*                                 LEVERERANDE DC I EXPORTFLÖDET           
016000*                                 DELIVERY DC IN EXPORT FLOW              
016100*** END OF VILMAII-COPY LENGTH= 181 BYTES                                 
