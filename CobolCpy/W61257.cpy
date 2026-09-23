000100 01  W61257.                                                              
000200*                                 INLEVERANS HISTORIK SDC                 
000300*                                 INLEVERANSENS IDENTITET                 
000400*                                 R30 FAKTURERAT I CDC                    
000500*                                 310 MOTTAGET I SDC/NDC                  
000600*                                 R32 RAPPORTERAD                         
000700*                                 FYSISK NYCKEL: DAINLEV                  
000800*                                 SÖKBEGREPP IDDC                         
000900*                                            IDPTYP                       
001000     03 IDARTNR              PIC 9(9).                                    
001100*                                 ARTIKELNUMMER                           
001200*                                 PART NUMBER                             
001300     03 DAINLEV              PIC 9(16).                                   
001400*                                 INLEVERANS NUMMER                       
001500*                                 CONSIGNMENT IDENTITY                    
001600*                                 (YYYYMMDD+HHMMSSTH)                     
001700     03 ADLAGOMR             PIC 9(2).                                    
001800*                                 LAGEROMRÅDE                             
001900*                                 AREA                                    
002000     03 ADGANG               PIC 9(2).                                    
002100*                                 GÅNG                                    
002200*                                 AISLE                                   
002300     03 ADPLATS              PIC 9(5).                                    
002400*                                 LAGERPLATSNUMMER                        
002500*                                 LOCATION                                
002600     03 FLMAKUL              PIC X.                                       
002700*                                 FLAGGA MAKULERAT KOLLI                  
002800*                                 CASE CANCELLATION FLAG                  
002900     03 FLPRIO               PIC X.                                       
003000*                                 PRIORITERAD                             
003100*                                 HAS PRIORITY                            
003200     03 FLSKAKOL             PIC X.                                       
003300*                                 FLAGGA SKADAT KOLLI.                    
003400*                                 DAMAGED CASE FLAG                       
003500     03 IDDC                 PIC X(2).                                    
003600*                                 IDENTIFIERARE LAGER                     
003700*                                 WAREHOUSE IDENTIFIER                    
003800     03 IDLEVNR              PIC X(5).                                    
003900*                                 LEVERANTÖRNUMMER                        
004000*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
004100     03 IDLOPNRM             PIC 9(9).                                    
004200*                                 LÖPNUMMER MOTTAGNINGSRAPPORT            
004300*                                 (0VVDLLLLK)                             
004400*                                 SERIAL NO RECEIVING REPORT              
004500*                                 (0WWDLLLLC)                             
004600     03 IDFAKT               PIC X(7).                                    
004700*                                 FAKTURANUMMER                           
004800*                                 INVOICE NO.                             
004900     03 IDKUNDRF             PIC X(10).                                   
005000*                                 KUNDENS REFERENS (ORDERID)              
005100*                                 CUSTOMER REFERENCE (ORDER ID)           
005200     03 IDKOLLI              PIC 9(5).                                    
005300*                                 KOLLINUMMER                             
005400*                                 CASE NUMBER                             
005500     03 IDPTYP               PIC X(3).                                    
005600*                                 POSTTYP                                 
005700*                                 RECORD TYPE                             
005800     03 KDFRAKT              PIC 9(3).                                    
005900*                                 FRAKTSÄTT DC TILL KUND                  
006000*                                 FREIGHT CODE                            
006100     03 KDKOLLI              PIC X(8).                                    
006200*                                 KOLLIKOD                                
006300*                                 KOLLI CODE                              
006400     03 KDRT                 PIC 9(3).                                    
006500     03 KDVALISO             PIC X(3).                                    
006600*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
006700*                                 CURRENCY CODE BY ISO-STANDARD.          
006800     03 KVANTMOT             PIC -(6)9.                                   
006900*                                 ANTAL MOTTAGET                          
007000*                                 QUANTITY RECEIVED                       
007100     03 KVART-SKROT          PIC 9(7).                                    
007200*                                 ANTAL SKROTADE ARTIKLAR                 
007300*                                 QUANTITY INSPECTED PARTS                
007400     03 KVAVIS               PIC 9(7).                                    
007500*                                 AVISERAT ANTAL                          
007600*                                 QUANTITY NOTIFIED                       
007700     03 PRARTNTO             PIC 9(7)V9(2).                               
007800*                                 ARTIKELPRIS NETTO                       
007900*                                 NET PRICE EACH   (FOB NET)              
008000     03 PRKURS               PIC 9(6)V9(5).                               
008100*                                 VALUTAKURS                              
008200*                                 CURRENCY EXCHANGE RATE                  
008300     03 TIBERANK             PIC 9(6).                                    
008400*                                 BERÄKNAD ANKOMSTDATUM                   
008500*                                 ESTIMATED RECEIVING DATE                
008600     03 TIINLMOT             PIC 9(6).                                    
008700*                                 MOTTAGNINGSDATUM   (ÅÅMMDD)             
008800*                                 RECEIVING DATE    (YYMMDD)              
008900     03 TIINLMTI             PIC 9(4).                                    
009000*                                 MOTTAGNINGSTID     (TTMM)               
009100*                                 RECEIVING HOUR    (HHMM)                
009200     03 TIINLINL             PIC 9(6).                                    
009300*                                 RAPPORTERINGSDATUM INLAGD (R32)         
009400*                                 DATE OF REPORTED IN STOCK (R32)         
009500     03 TIINLITI             PIC 9(4).                                    
009600*                                 RAPPORTERINGSTID   INLAGD (R32)         
009700*                                 HOUR REPORTED IN STOCK (R32)            
009800     03 ADINLOMR             PIC X(4).                                    
009900*                                 INLEVERANSOMRÅDE                        
010000*                                 RECEIVING AREA                          
010100     03 IDANALYS             PIC X(12).                                   
010200*                                 ANALYSNUMMER                            
010300*                                 ANALYSIS NUMBER                         
010400     03 IDKONTO              PIC 9(10).                                   
010500*                                 KONTO                                   
010600*                                 ACCOUNT                                 
010700     03 IDKST                PIC X(10).                                   
010800*                                 KOSTNADSSTÄLLE                          
010900*                                 COST CENTRE                             
011000     03 IDUSER-003           PIC X(5).                                    
011100*                                 ANSVARIGT USERID INLÄGGN.(R32)          
011200     03 KVRETUR              PIC 9(7).                                    
011300*                                 ANTAL I RETUR                           
011400*                                 QUANTITY IN RETURN                      
011500     03 KDAVVANT             PIC 9.                                       
011600*                                 AVVIKELSEANTAL KOD                      
011700*                                 0=INGEN ANM.   1=AVVIKELSE              
011800*                                 2=MAKULERING AV MOTT.RAPPORT            
011900*                                 QUANTITY DEVIATION  CODE                
012000*                                 0=NO DEV.    1=DEVIATION                
012100*                                 2=CANCELLING OF REC. REPORT             
012200     03 TIAVIDAT             PIC 9(6).                                    
012300*                                 AVISERINGSDATUM (YYMMDD)                
012400*                                 ADVICE NOTE DATE                        
012500     03 IDDC-LEV             PIC X(2).                                    
012600*                                 LEVERERANDE DC I EXPORTFLÖDET           
012700*                                 DELIVERY DC IN EXPORT FLOW              
012800     03 IDDISTR              PIC 9(5).                                    
012900*                                 DISTRIKTNUMMER                          
013000*                                 DISTRICT NUMBER                         
013100     03 IDKUNDNR             PIC 9(7).                                    
013200*                                 KUNDNUMMER                              
013300*                                 CUSTOMER NO                             
013400     03 KVTULRET             PIC Z(6)9.                                   
013500*                                 ANTAL SOM TULLEN RETURNERAT             
013600*                                 RETURNED QUANTITY FROM CUSTOM           
013700     03 FLTULLST             PIC X.                                       
013800*                                 FLAGGA FÖR ART STOPPAD I TULLEN         
013900*                                 FLAG FOR PART HOLD AT CUSTOM            
014000*** END OF VILMAII-COPY LENGTH= 239 BYTES                                 
