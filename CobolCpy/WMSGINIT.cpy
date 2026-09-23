000100 01  MSGI-WMSGINIT.                                                       
000200*                                 BESKRIVNING AV GENERELL                 
000300*                                 INIT-IO-AREA                            
000400*                                 INFO PÅ W.PROQ.TEXT(W005INIT)           
000500     03 MSGI-KDCALL          PIC X(3).                                    
000600*                                 ANROPSTYP                               
000700*                                 CALL TYPE                               
000800     03 MSGI-IDUSER          PIC X(8).                                    
000900*                                 ANVÄNDARENS SÄKERHETS ID                
001000*                                 USER SECURITY-IDENTITY                  
001100     03 MSGI-BEANST          PIC X(25).                                   
001200*                                 ANSTÄLLDS NAMN                          
001300*                                 NAME OF EMPLOYED                        
001400     03 MSGI-IDAVD           PIC X(5).                                    
001500*                                 DEN ANSTÄLLDES AVDELNING/               
001600*                                 KOSTNADSSTÄLLE                          
001700*                                 DEPARTMENT OF EMPLOYED/                 
001800*                                 COST CENTER                             
001900     03 MSGI-IDCSS           PIC X(8).                                    
002000*                                 CSS-STILMALL                            
002100*                                 CASCADING STYLESHEET                    
002200     03 MSGI-IDDC            PIC X(2).                                    
002300*                                 IDENTIFIERARE LAGER                     
002400*                                 WAREHOUSE IDENTIFIER                    
002500     03 MSGI-IDFTG           PIC X(2).                                    
002600*                                 FÖRETAGSID EKONOM REDOVISNING           
002700*                                 COMPANY IDENTITY ACCOUNTING             
002800     03 MSGI-IDLAND-SPR      PIC X(2).                                    
002900*                                 2-STÄLLIG LANDSKOD FÖR SPRÅK            
003000*                                 2-CHAR COUNTRY CODE FOR LANG.           
003100     03 MSGI-IDLTERM         PIC X(8).                                    
003200*                                 LOGISKT TERMINALNAMN                    
003300*                                 IDENTITY OF LOGICAL TERMINAL            
003400     03 MSGI-IDLTERM-USER    PIC X(8).                                    
003500*                                 LOGISKT TERMINALNAMN (USER > 1)         
003600*                                 IDENTITY OF LOGICAL TERMINAL            
003700     03 MSGI-IDNODE          PIC X(8).                                    
003800*                                 VTAM NODE-NAMN                          
003900*                                 VTAM NODE NAME                          
004000     03 MSGI-IDRT-KEY        PIC X(3).                                    
004100*                                 RETURTERMINAL                           
004200*                                 RETURN TERMINAL                         
004300     03 MSGI-IDSPRAK         PIC X(2).                                    
004400*                                 2-STÄLLIG ISO SPRÅKKOD                  
004500*                                 2-LETTER ISO LANGUAGE CODE              
004600     03 MSGI-IDTFN           PIC X(20).                                   
004700*                                 TELEFONNUMMER EXTERNT                   
004800*                                 TELEPHONE NUMBER  EXTERNAL              
004900     03 MSGI-IDTFX           PIC X(20).                                   
005000*                                 TELEFAXNUMMER                           
005100*                                 FAXNUMBER                               
005200     03 MSGI-IDTIDZON        PIC X(2).                                    
005300*                                 TIDZONER PÅ JORDEN.                     
005400*                                 TIME ZONE ON EARTH                      
005500     03 MSGI-IDTRANS         PIC X(4).                                    
005600*                                 BILDNUMMER                              
005700*                                 SCREEN NUMBER                           
005800     03 MSGI-IDTRANS-OLD     PIC X(4).                                    
005900*                                 BILDNUMMER FÖREGÅENDE                   
006000*                                 SCREEN NUMBER OLD                       
006100     03 MSGI-KDMATT          PIC X.                                       
006200*                                 MÅTTKOD                                 
006300*                                 MEASUREMENT CODE                        
006400     03 MSGI-KDMFSFOR        PIC X.                                       
006500*                                 TYP AV MFS-FORMAT                       
006600*                                 1 = W-FORMAT  2 = N-FORMAT              
006700*                                 TYPE OF MFS FORMAT                      
006800     03 MSGI-KDSVAR          PIC X.                                       
006900*                                 SVAR FRÅN PROGRAM ELLER SKÄRM           
007000*                                 RETURN CODE FROM PROGRAM                
007100     03 MSGI-TILOKDAT        PIC X(6).                                    
007200*                                 DATUM FÖR LOKAL TID     AAMMDD          
007300*                                 DATE FOR LOCAL TIME     YYMMDD          
007400     03 MSGI-TILOKTID        PIC X(4).                                    
007500*                                 TID (KLOCKAN) FÖR LOKAL TID             
007600*                                 LOCAL TIME AS  HHMM                     
007700     03 MSGI-RESERVINFO      PIC X(25).                                   
007800     03 MSGI-NYCKLAR.                                                     
007900*                                 SPARADE NYCKLAR FRÅN MID                
008000*                                 SAVED KEYS FROM MID                     
008100        05 MSGI-ADGANG-FOM   PIC X(2).                                    
008200*                                 GÅNG FRÅN OCH MED                       
008300*                                 AISLE ADDRESS FROM                      
008400        05 MSGI-ADGANG-TOM   PIC X(2).                                    
008500*                                 GÅNG TILL OCH MED                       
008600*                                 AISLE ADDRESS TO                        
008700        05 MSGI-ADINLOMR     PIC X(4).                                    
008800*                                 INLEVERANSOMRÅDE                        
008900*                                 RECEIVING AREA                          
009000        05 MSGI-ADINLOMR-NXT PIC X(4).                                    
009100*                                 INLEVERANSOMRÅDE NÄSTA                  
009200*                                 RECEIVING AREA NEXT                     
009300        05 MSGI-ADINLOMR-PRT PIC X(4).                                    
009400*                                 PRINTERPLACERING                        
009500*                                 PLACE OF A PRINTER                      
009600        05 MSGI-ADLAGOMR     PIC X(2).                                    
009700*                                 LAGEROMRÅDE                             
009800*                                 AREA                                    
009900        05 MSGI-ADLAGOMR-FOM PIC X(2).                                    
010000*                                 LAGEROMRÅDE FRÅN OCH MED                
010100*                                 AREA ADDRESS FROM                       
010200        05 MSGI-ADLAGOMR-TOM PIC X(2).                                    
010300*                                 LAGEROMRÅDE TILL OCH MED                
010400*                                 AREA ADDRESS TO                         
010500        05 MSGI-ADLEV-FOM    PIC X(2).                                    
010600*                                 LAGERPLATSNUMMER FOM                    
010700*                                 LEVEL FROM                              
010800        05 MSGI-ADLEV-TOM    PIC X(2).                                    
010900*                                 LAGERPLATSNUMMER THRU                   
011000*                                 LEVEL THRU                              
011100        05 MSGI-ADPLATS      PIC X(5).                                    
011200*                                 LAGERPLATSNUMMER                        
011300*                                 LOCATION                                
011400        05 MSGI-ADSEC-FOM    PIC X(2).                                    
011500*                                 LAGERPLATSNUMMER FOM                    
011600*                                 SECTION FROM                            
011700        05 MSGI-ADSEC-TOM    PIC X(2).                                    
011800*                                 LAGERPLATSNUMMER THRU                   
011900*                                 SECTION THRU                            
012000        05 MSGI-ADSEQ-FOM    PIC X.                                       
012100*                                 LAGERPLATSNUMMER  FRÅN                  
012200*                                                                         
012300*                                 SEQUENCE POSITION NUMBER FROM           
012400        05 MSGI-ADSEQ-TOM    PIC X.                                       
012500*                                 LAGERPLATSNUMMER  TILL                  
012600*                                 SEQUENCE POSITION NUMBER TO             
012700        05 MSGI-BEFT         PIC X(2).                                    
012800*                                 FÖRPACKNINGSTYP                         
012900*                                 PACKAGING TYPE                          
013000        05 MSGI-DASUPREF     PIC X(8).                                    
013100*                                 SÄNDNINGSDATUM DIREKTLEVERANTÖR         
013200*                                 SHIPPING DATE DIRECT SUPPLIER           
013300        05 MSGI-FLINLFB      PIC X.                                       
013400*                                 VALD TILL FÖRBEHANDLING                 
013500*                                 SELECTED FOR PRETREATEMENT              
013600        05 MSGI-FLINLI       PIC X.                                       
013700*                                 INLAGD RAD, PARTI ELLER KOLLI           
013800*                                 STORED  LINE                            
013900        05 MSGI-FLLDCKND     PIC X.                                       
014000*                                 FL LDC-KUND                             
014100*                                 FL LDC CUSTOMER                         
014200        05 MSGI-FLMRKVAL     PIC X.                                       
014300*                                 FLAGGA FÖR MARKNADSVALUTA               
014400*                                 FLAG FOR MARKET CURRENCY                
014500        05 MSGI-FLORDLEV     PIC X.                                       
014600*                                 FLAGGA LEVERANSORDERNUMMER              
014700*                                 DELIVERY ORDER FLAG                     
014800        05 MSGI-FLVISA       PIC X.                                       
014900*                                 ALLMÄN FLAGGA FÖR DATAVISNING           
015000*                                 GENERAL FLAG FOR SHOWING INFO           
015100        05 MSGI-IDANSK       PIC X(3).                                    
015200*                                 ANSKAFFARNUMMER                         
015300*                                 PROCURER NO.                            
015400        05 MSGI-IDANSK-FOM   PIC X(3).                                    
015500*                                 LÄGSTA ANSKAFFARNR I INTERVALL          
015600*                                 LOWEST PURCHASE PLANNER NUMBER          
015700        05 MSGI-IDANSK-TOM   PIC X(3).                                    
015800*                                 HÖGSTA ANSKAFFARNR I INTERVALL          
015900*                                 HIGHEST PURCHASE PLANNER NO             
016000        05 MSGI-IDANSTNR     PIC X(5).                                    
016100*                                 ANSTÄLLNINGSNUMMER                      
016200*                                 IDENTIFICATION NO EMPLOYEE              
016300        05 MSGI-IDARTNR      PIC X(9).                                    
016400*                                 ARTIKELNUMMER                           
016500*                                 PART NUMBER                             
016600        05 MSGI-IDBYTKOL     PIC X(3).                                    
016700*                                 BYTES KOLLINUMMER                       
016800*                                 EXCHANGE CASE NUMBER                    
016900        05 MSGI-IDBYTRAP     PIC X(7).                                    
017000*                                 RAPPORTNUMMER  BYTES                    
017100*                                 REPORTNUMBER   EXCHANGE                 
017200        05 MSGI-IDCATAVS     PIC X(4).                                    
017300*                                 KATALOG-AVSNITT                         
017400*                                 CATALOG TEXT BLOCK                      
017500        05 MSGI-IDCATGRP     PIC X(2).                                    
017600*                                 KATALOG-GRUPP                           
017700*                                 CATALOG-GROUP                           
017800        05 MSGI-IDCATNR      PIC X(5).                                    
017900*                                 KATALOG-ID                              
018000*                                 CATALOG-ID                              
018100        05 MSGI-IDCATRAD     PIC X(4).                                    
018200*                                 RADNUMMER                               
018300*                                 ROW NUMBER IN TEXT BLOCK                
018400        05 MSGI-IDDC-BULK    PIC X(2).                                    
018500*                                 IDENTIFIERARE BULKORDERLAGER            
018600*                                 WAREHOUSE IDENTIFIER BULK               
018700*                                 ORDERS                                  
018800        05 MSGI-IDDC-DAY     PIC X(2).                                    
018900*                                 IDENTIFIERARE DAGORDERLAGER             
019000*                                 WAREHOUSE IDENTIFIER DAILY              
019100*                                 ORDERS                                  
019200        05 MSGI-IDDC-KEY     PIC X(2).                                    
019300*                                 DC FÖR HOPP MELLAN BILDER               
019400*                                 SAVED KEY BETWEEN SCREENS               
019500        05 MSGI-IDDC-REC     PIC X(2).                                    
019600*                                 MOTTAGANDE LAGER                        
019700*                                 RECEIVING WAREHOUSE                     
019800        05 MSGI-IDDC-SEND    PIC X(2).                                    
019900*                                 SÄNDANDE LAGER                          
020000*                                 SENDING WAREHOUSE                       
020100        05 MSGI-IDDIRGRP     PIC X(10).                                   
020200*                                 DIREKTLEVERANSGRUPP                     
020300*                                 DIREKTLEVERANSGRUPP                     
020400        05 MSGI-IDDISTR      PIC X(4).                                    
020500*                                 DISTRIKTNUMMER                          
020600*                                 DISTRICT NUMBER                         
020700        05 MSGI-IDDISTR-FOM  PIC X(4).                                    
020800*                                 LÄGSTA DISTRIKTNR I INTERVALL           
020900*                                 LOWEST DISTRICT NUMBER                  
021000        05 MSGI-IDDISTR-TOM  PIC X(4).                                    
021100*                                 HÖGSTA DISTRIKTNR I INTERVALL           
021200*                                 HIGHEST DISTRICT NUMBER                 
021300        05 MSGI-IDFAKT       PIC X(7).                                    
021400*                                 FAKTURANUMMER                           
021500*                                 INVOICE NO.                             
021600        05 MSGI-IDFKNGRP     PIC X(4).                                    
021700*                                 FUNKTIONSGRUPP                          
021800*                                 FUNCTION GROUP                          
021900        05 MSGI-IDFOTNR      PIC X(5).                                    
022000*                                 FOTNOTSNUMMER                           
022100*                                 FOOT NOTE ID NUMBER                     
022200        05 MSGI-IDFPINST     PIC X(7).                                    
022300*                                 FÖRPACKNINGSINSTRUKTION NR              
022400*                                 PACKAGE INSTRUCTION NUMBER              
022500        05 MSGI-IDFS         PIC X(8).                                    
022600*                                 FÖLJESEDELSNUMMER ENL ODETTE            
022700*                                 ADVICE NOTE NUMBER ODETTE               
022800        05 MSGI-IDILIRAD     PIC X(5).                                    
022900*                                 INLÄGGNINGSLISTERADNUMMER               
023000*                                 REPORTINGLISTLINENUMBER                 
023100        05 MSGI-IDILIST      PIC X(5).                                    
023200*                                 INLÄGGNINGSLISTEIDENTITET               
023300*                                 REPORTINGLIST-IDENTITY                  
023400        05 MSGI-IDILLU       PIC X(5).                                    
023500*                                 ILLUSTRATIONENS NR                      
023600*                                 ILLUSTRATION NUMBER                     
023700        05 MSGI-IDINLVGN     PIC X(3).                                    
023800*                                 VAGNSIDENTITET                          
023900*                                 INTERNAL CARRIER ID                     
024000        05 MSGI-IDKAMP       PIC X(7).                                    
024100*                                 SERVICEKAMPANJ                          
024200*                                 SERVICE CAMPAIGN                        
024300        05 MSGI-IDKAMP-GRP   PIC X(7).                                    
024400*                                 ID FÖR KAMPANJGRUPPER                   
024500*                                 ID OF CAMPAIGNGROUPS                    
024600        05 MSGI-IDKOLLI      PIC X(5).                                    
024700*                                 KOLLINUMMER                             
024800*                                 CASE NUMBER                             
024900        05 MSGI-IDKONCNR     PIC X(3).                                    
025000*                                 KONCERNNUMMER                           
025100*                                 CONCERN NO                              
025200        05 MSGI-IDKONTO      PIC X(10).                                   
025300*                                 KONTO                                   
025400*                                 ACCOUNT                                 
025500        05 MSGI-IDKR         PIC X(5).                                    
025600*                                 KONTROLLRAPPORT NUMMER                  
025700*                                 INSPECTION REPORT NUMBER                
025800        05 MSGI-IDKUNDNR     PIC X(6).                                    
025900*                                 KUNDNUMMER                              
026000*                                 CUSTOMER NO                             
026100        05 MSGI-IDKUNDRF-GRP.                                             
026200*                                 KUNDENS REFERENS (ORDERID)              
026300*                                 CUSTOMER REFERENCE (ORDER ID)           
026400           07 MSGI-IDKUNDRF  PIC X(10).                                   
026500*                                 KUNDENS REFERENS (ORDERID)              
026600*                                 CUSTOMER REFERENCE (ORDER ID)           
026700           07 MSGI-IDORDNR5-FILLER REDEFINES MSGI-IDKUNDRF.               
026800              09 MSGI-IDORDNR5                                            
026900                             PIC 9(5).                                    
027000*                                 ORDERNUMMER                             
027100*                                 ORDER NUMBER                            
027200              09 FILLER      PIC X(5).                                    
027300           07 MSGI-IDORDNR7-FILLER REDEFINES MSGI-IDKUNDRF.               
027400              09 MSGI-IDORDNR7                                            
027500                             PIC 9(7).                                    
027600*                                 ORDERNUMMER                             
027700*                                 ORDER NUMBER                            
027800              09 FILLER      PIC X(3).                                    
027900        05 MSGI-IDLANDX2     PIC X(2).                                    
028000*                                 2-STÄLLIG LANDSBETECKNINGSKOD           
028100*                                 2-LETTER CODE FOR COUNTRY               
028200        05 MSGI-IDLASTN      PIC X(7).                                    
028300*                                 LASTNINGSNUMMER                         
028400*                                 LOADING NO.                             
028500        05 MSGI-IDLBBET      PIC X(12).                                   
028600*                                 LASTBÄRARBETECKNING                     
028700*                                 TRAILER NUMBER                          
028800        05 MSGI-IDLBFIKT     PIC X(3).                                    
028900*                                 FIKTIVT TRAILERNUMMER                   
029000*                                 FICTIVE TRAILER NUMBER                  
029100        05 MSGI-IDLEVNR      PIC X(5).                                    
029200*                                 LEVERANTÖRNUMMER                        
029300*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
029400        05 MSGI-IDLOPNRM     PIC X(8).                                    
029500*                                 LÖPNUMMER MOTTAGNINGSRAPPORT            
029600*                                 (0VVDLLLLK)                             
029700*                                 SERIAL NO RECEIVING REPORT              
029800*                                 (0WWDLLLLC)                             
029900        05 MSGI-IDOKOLLI     PIC X(9).                                    
030000*                                 ODETTE KOLLINUMMER                      
030100*                                 ODETTE CASE NUMBER                      
030200        05 MSGI-IDPARTNR     PIC X(9).                                    
030300*                                 FINANCIELL KUND                         
030400*                                 FINANCIAL CUST                          
030500        05 MSGI-IDPERSON     PIC X(3).                                    
030600*                                 PERSONKOD                               
030700*                                 STAFF CODE                              
030800        05 MSGI-IDPERSON-CDC PIC X(3).                                    
030900*                                 PERSONKOD CDC                           
031000*                                 CDC STAFF CODE                          
031100        05 MSGI-IDPERSON-QUAL                                             
031200                             PIC X(3).                                    
031300*                                 PERSONKOD QUALITY                       
031400*                                 STAFF CODE QUALITY                      
031500        05 MSGI-IDPRC.                                                    
031600*                                 PRODUKTIONSKANAL                        
031700*                                 PRODUCTION CHANNEL                      
031800           07 MSGI-IDPRCBAS  PIC X(3).                                    
031900*                                 PRC-BAS                                 
032000*                                 PRC-BASIC                               
032100           07 MSGI-IDPRCVAR  PIC X.                                       
032200*                                 PRC-VARIANT                             
032300*                                 PRC-VARIANT                             
032400        05 MSGI-IDPRCTR      PIC X(10).                                   
032500*                                 PROFIT CENTER                           
032600*                                 PROFIT CENTER                           
032700        05 MSGI-IDPRODNR     PIC X(7).                                    
032800*                                 PRODUKTIONSNUMMER                       
032900*                                 PRODUCTION NUMBER                       
033000        05 MSGI-IDPROJ       PIC X(4).                                    
033100*                                 PARTS PROJEKTIDENTITET                  
033200*                                 PARTS PROJECT IDENTITY                  
033300        05 MSGI-IDPROMR.                                                  
033400*                                 PRISOMRÅDE (RABATTSTRUKTUR)             
033500*                                 PRICE AREA                              
033600           07 MSGI-IDMARKBO  PIC X.                                       
033700*                                 MARKNADSBOLAGSKOD                       
033800*                                 MARKET COMPANY CODE                     
033900           07 MSGI-IDPROMRN  PIC X(2).                                    
034000*                                 PRISOMRÅDE LÖPNUMMER                    
034100*                                 PRICE AREA SERIALNUMBER                 
034200        05 MSGI-IDPTYP       PIC X(3).                                    
034300*                                 POSTTYP                                 
034400*                                 RECORD TYPE                             
034500        05 MSGI-IDRADNR      PIC X(4).                                    
034600*                                 RADNUMMER                               
034700*                                 LINE NO                                 
034800        05 MSGI-IDRAPP       PIC X(10).                                   
034900*                                 RAPPORT ID                              
035000*                                 REPORT ID                               
035100        05 MSGI-IDRAPPNR     PIC X(7).                                    
035200*                                 RAPPORT NUMMER                          
035300*                                 DISCREPANCY REPORT NUMBER               
035400        05 MSGI-IDROLL       PIC X(5).                                    
035500*                                 VOR ROLL ID                             
035600*                                 VOR ROLE ID                             
035700        05 MSGI-IDRT         PIC X(3).                                    
035800*                                 RETURTERMINAL                           
035900*                                 RETURN TERMINAL                         
036000        05 MSGI-IDRTLOP      PIC X(3).                                    
036100*                                 RETUR TERMINAL LÖPNUMMER                
036200*                                 RETURN TERMINAL SEQUENCE NUMBER         
036300        05 MSGI-IDRUBNR      PIC X(5).                                    
036400*                                 RUBRIKNUMMER                            
036500*                                 HEADLINE ID NUMBER                      
036600        05 MSGI-IDSHIPM      PIC X(7).                                    
036700*                                 SKEPPNINGSNUMMER                        
036800*                                 SHIPMENT NO                             
036900        05 MSGI-IDSKEPPN     PIC X(7).                                    
037000*                                 SKEPPNINGSNUMMER                        
037100*                                 SHIPMENT NO                             
037200        05 MSGI-IDSNDJOB     PIC X(8).                                    
037300*                                 SÄNDANDE JOB IDENTITET                  
037400*                                 IDENTITY OF SENDING JOB                 
037500        05 MSGI-IDSNDNOD     PIC X(8).                                    
037600*                                 SÄNDANDE NODE IDENTITET                 
037700*                                 IDENTITY OF SENDING NODE                
037800        05 MSGI-IDSUPREF     PIC X(10).                                   
037900*                                 LEVERANTöRSREF.                         
038000*                                 SUPPLIER REF.                           
038100        05 MSGI-IDSYSMOT     PIC X(6).                                    
038200*                                 PULS MOTTAGANDE SYSTEMNAMN              
038300*                                 PULS RECEIVING SYSTEM NAME              
038400        05 MSGI-IDTABNR      PIC X(3).                                    
038500*                                 TABELLNUMMER                            
038600*                                 TABELNUMBER                             
038700        05 MSGI-IDTRPTNR     PIC X(3).                                    
038800*                                 TRANSPORTIDENTITET                      
038900*                                 TRANSPORT IDENTITY                      
039000        05 MSGI-IDTTEXNR     PIC X(5).                                    
039100*                                 TILLÄGGSTEXT-NR                         
039200*                                 ADDITIONAL TEXT, ID NUMBER              
039300        05 MSGI-IDUSER-KEY   PIC X(8).                                    
039400*                                 USER FÖR HOPP MELLAN BILDER             
039500*                                 USER ID ON SCREENS KEY                  
039600        05 MSGI-KDANMORS     PIC X(2).                                    
039700*                                 ORSAK TILL LEVERANSANMÄRKNING           
039800*                                 DISCREPANCY REPORT REASON CODE          
039900        05 MSGI-KDARBVAL     PIC X.                                       
040000*                                 ARBETSTYPSVAL KOD                       
040100*                                 CATEGORY OF WORK CODE                   
040200        05 MSGI-KDARBTYP     PIC X(8).                                    
040300*                                 TYP AV ARBETE                           
040400*                                 CATEGORY OF WORK                        
040500        05 MSGI-KDARTKAM     PIC X(5).                                    
040600*                                 TRANSFER KOD                            
040700*                                 TRANSFER CONDITION CODE                 
040800        05 MSGI-KDAVROP      PIC X.                                       
040900*                                 AVROPSKOD                               
041000*                                 CALLED                                  
041100        05 MSGI-KDBYTSTA     PIC X.                                       
041200*                                 STATUSKOD BYTESOBJEKT                   
041300*                                 STATUSCODE EXCH CORES                   
041400        05 MSGI-KDEKHHT      PIC X(3).                                    
041500*                                 EKONOMISK HUVUDHÄNDELSE                 
041600*                                 ECONOMIC MAIN EVENT                     
041700        05 MSGI-KDEKNIVA     PIC X(5).                                    
041800*                                 EKONOMISK HÄNDELSENIVÅ                  
041900*                                 ECONOMICAL EVENT LEVEL                  
042000        05 MSGI-KDEKSHT      PIC X(3).                                    
042100*                                 EKONOMISK SUBHÄNDELSE                   
042200*                                 ECONOMIC SUB EVENT                      
042300        05 MSGI-KDEXCHA      PIC X(3).                                    
042400*                                 EXCHANGE ACCOUNT CODE                   
042500        05 MSGI-KDFRAKT      PIC X(2).                                    
042600*                                 FRAKTSÄTT DC TILL KUND                  
042700*                                 FREIGHT CODE                            
042800        05 MSGI-KDFREQ       PIC X(2).                                    
042900*                                 FREQUENCY CODE                          
043000*                                 FREQUENCY CODE                          
043100        05 MSGI-KDKOMSTA     PIC X.                                       
043200*                                 KOMMUNIKATIONSSTATUS                    
043300*                                 COMMUNICATION STATUS                    
043400        05 MSGI-KDKRENOT     PIC X(2).                                    
043500*                                 TYP AV KREDITERING                      
043600*                                 CREDIT NOTE TYPE                        
043700        05 MSGI-KDLARM       PIC X(3).                                    
043800*                                 LARMORSAKSKOD                           
043900*                                 ALARM REASON CODE                       
044000        05 MSGI-KDLEVANM     PIC X.                                       
044100*                                 STATUS LEVERANSANMÄRKNING               
044200*                                 STATUS DISCREPANCY                      
044300        05 MSGI-KDLEVANM-FOM PIC X.                                       
044400*                                 MIN STATUS LEVERANSANMÄRKNING           
044500*                                 MIN STATUS DISCREPANCY                  
044600        05 MSGI-KDLEVANM-TOM PIC X.                                       
044700*                                 MAX STATUS LEVERANSANMÄRKNING           
044800*                                 MAX STATUS DISCREPANCY                  
044900        05 MSGI-KDLEVPLF     PIC X.                                       
045000*                                 KOD FÖR LEVPLAN-GODKÄNNANDE             
045100*                                 CODE FOR APPROVAL OF SCHEDULE P         
045200*                                 ROPOSAL                                 
045300        05 MSGI-KDLPORS      PIC X(2).                                    
045400*                                 LEVERANSPLANEORSAK                      
045500        05 MSGI-KDPRIO-PF    PIC X.                                       
045600*                                 PRIORITETSKOD PÅFYLLNAD                 
045700*                                 PRIORITY FILLUP PICKING ADDRESS         
045800        05 MSGI-KDPRODSL     PIC X(2).                                    
045900*                                 PRODUKTSLAG                             
046000*                                 PRODUCT GROUP                           
046100        05 MSGI-KDPRT        PIC X(3).                                    
046200*                                 PRINTERKOD                              
046300*                                 PRINTERCODE                             
046400        05 MSGI-KDRETSTA     PIC X.                                       
046500*                                 STATUS RETURER                          
046600*                                 RETURN STATUS                           
046700        05 MSGI-KDSORT       PIC X(2).                                    
046800*                                 SORT-KOD                                
046900*                                 UNIT OF MEASURE                         
047000        05 MSGI-KDSTAPF      PIC X.                                       
047100*                                 STATUS PÅ PÅFYLLNING                    
047200*                                 STATUS FOR FILLING                      
047300        05 MSGI-KDSTARAD     PIC X.                                       
047400*                                 RADSTATUSKOD                            
047500*                                 LINE STATUS CODE                        
047600        05 MSGI-KDSTOR       PIC X(3).                                    
047700*                                 STORAGE CODE                            
047800*                                 STORAGE CODE                            
047900        05 MSGI-KDTRPDOCT    PIC X.                                       
048000*                                 TYP AV TRANSPORTDOKUMENT                
048100*                                 TYPE OF TRANSPORT DOCUMENT              
048200        05 MSGI-KDURVAL      PIC X.                                       
048300*                                 KOD FÖR BEGRÄNSNING AV URVAL            
048400*                                 LIMIT SELECTION TREATMENT               
048500        05 MSGI-TEVORMRK     PIC X(2).                                    
048600*                                 MÄRKNINGSTEXT FÖR                       
048700*                                 VOR-KÖN                                 
048800        05 MSGI-TIAAVV-FOM   PIC X(4).                                    
048900*                                 ÅR - VECKA  (ÅÅVV)                      
049000*                                 YEAR - WEEK  (YYWW)                     
049100        05 MSGI-TIAAVV-TOM   PIC X(4).                                    
049200*                                 ÅR - VECKA  (ÅÅVV)                      
049300*                                 YEAR - WEEK  (YYWW)                     
049400        05 MSGI-TIAVIDAT     PIC X(6).                                    
049500*                                 AVISERINGSDATUM (YYMMDD)                
049600*                                 ADVICE NOTE DATE                        
049700        05 MSGI-TIFINLV-FOM  PIC X(5).                                    
049800*                                 PUBL.VECKA-FOM, (ÅÅVVD  D=1)            
049900*                                 DATE GOODS REC-FROM,(YYWWD D=1)         
050000        05 MSGI-TIFINLV-TOM  PIC X(5).                                    
050100*                                 PUBL.VECKA-FOM, (ÅÅVVD  D=1)            
050200*                                 DATE GOODS REC-UNTIL(YYWWD D=1)         
050300        05 MSGI-TIKLOCK      PIC X(8).                                    
050400*                                 KLOCKSLAG (TTMMSSTH)                    
050500*                                 TIME OF DAY (HHMMSSTH)                  
050600        05 MSGI-TIREGDAT     PIC X(6).                                    
050700*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
050800*                                 REGISTRATION DATE (YYMMDD)              
050900        05 MSGI-TISKEPPN     PIC X(6).                                    
051000*                                 SKEPPNINGSDATUM  (ÅÅMMDD)               
051100*                                 SHIPPING DATE    (YYMMDD)               
051200        05 MSGI-TISKROT-BEORD                                             
051300                             PIC X(6).                                    
051400*                                 BEORDRAD SKROTNINGSDATUM                
051500*                                 DATE OF SCRAPPING DECISION              
051600        05 MSGI-KDPRTVAL-ADR PIC X(2).                                    
051700*                                 PRINTER-VAL KOD ADRESS FLAGGA           
051800*                                 PRINTER SEL. CODE ADDRESS FLAG          
051900        05 MSGI-KDPRTVAL-FS  PIC X(2).                                    
052000*                                 PRINTER-VAL KOD FÖLJESEDEL              
052100*                                 PRINTER DEL. NOTE SELECT CODE           
052200        05 MSGI-IDSPRAK-KEY  PIC X(2).                                    
052300*                                 SPRÅK FÖR HOPP MELLAN BILDER            
052400*                                 LANGUAGE KEY BETWEEN SCREENS            
052500        05 MSGI-IDKOLLI-SAMP PIC X(5).                                    
052600*                                 SAMPACKNINGSKOLLINUMMER                 
052700*                                 MIXED PACKING CASE NUMBER               
052800        05 MSGI-IDVO         PIC X(2).                                    
052900*                                 VERKSAMHETSOMRÅDE SAMLINGSKOLLI         
053000*                                 AREA OF OPERATIONS MIX CASES            
053100        05 MSGI-IDKAMPRF     PIC X(7).                                    
053200*                                 KAMPANJREFERENS                         
053300*                                 CAMPAIGN REFERENCE                      
053400        05 MSGI-BEWEBSCR     PIC X(15).                                   
053500*                                 VALD BILD I PULS WEBBEN                 
053600*                                 CHOSEN SCREEN ON PULS WEB               
053700        05 MSGI-FLSORT       PIC X.                                       
053800*                                 SORTERINGSFLAGGA (J/N)                  
053900        05 MSGI-TIREGTID     PIC X(6).                                    
054000*                                 REGISTRERINGSTID                        
054100*                                 GENERAL REGISTRATION TIME               
054200        05 MSGI-KDVALISO     PIC X(3).                                    
054300*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
054400*                                 CURRENCY CODE BY ISO-STANDARD.          
054500        05 MSGI-IDFTG-KEY    PIC X(2).                                    
054600*                                 FTG FÖR HOPP MELLAN BILDER              
054700*                                 SAVED KEY BETWEEN SCREENS               
054800        05 MSGI-KDMAIL       PIC X(4).                                    
054900*                                 TYP AV MAIL UTSKICK                     
055000*                                 TYPE OF MAIL SENDNINGS                  
055100        05 MSGI-IDPRCTAB     PIC X(2).                                    
055200*                                 PRCTABELLIDENTITET                      
055300*                                 PRC TABLE IDENTITY                      
055400        05 MSGI-KDVALTYP     PIC X.                                       
055500*                                 KURSENS PER A=ÅR/M=MÅNAD/D=DAG          
055600*                                 CURRENCY PER YEAR/MONTH/DAY             
055700        05 MSGI-KDOTFREK     PIC X.                                       
055800*                                 ORDERTRÄFF FREKVENSEN ARTIKEL           
055900*                                 ORDER HIT FREQUENCY FOR PART            
056000        05 MSGI-TIAAVV       PIC X(4).                                    
056100*                                 ÅR - VECKA  (ÅÅVV)                      
056200*                                 YEAR - WEEK  (YYWW)                     
056300        05 MSGI-KDFARLIG     PIC X.                                       
056400*                                 KOD FÖR FARLIGT GODS                    
056500*                                 DANGEROUS GOODS CODE                    
056600        05 MSGI-TIRFSDAT-CDC PIC X(6).                                    
056700*                                 KLART FÖR TRANSPORT CDC ÅÅMMDD          
056800*                                 READY FOR SHIPMENT CDC YYMMDD           
056900        05 MSGI-KVBEART      PIC X(6).                                    
057000*                                 BESTÄLLT ANTAL STYCKEN                  
057100*                                 ORDERED QUANTITY                        
057200        05 MSGI-KDORDKL      PIC X.                                       
057300*                                 ORDERKLASS                              
057400*                                 ORDER CLASS                             
057500        05 MSGI-IDDC-REF     PIC X(2).                                    
057600*                                 SÄNDANDE LAGER FÖR REFILL               
057700*                                 SENDING WAREHOUSE FOR REFILL            
057800        05 MSGI-RESERV-KEY   PIC X(55).                                   
057900     03 MSGI-SECURITY.                                                    
058000*                                 INFO FÖR SECURITY                       
058100*                                 INFO FOR SECURITY                       
058200        05 MSGI-KDARBTYP-SEC PIC X(8).                                    
058300*                                 TYP AV ARBETE FÖR SECURITY              
058400*                                 TYPE OF WORK FOR SECURITY               
058500        05 MSGI-KDARBTYP-SEC-IDLEV                                        
058600                             PIC X(8).                                    
058700*                                 ARBETSTYP FÖR SÄKERHET PÅ LEV           
058800*                                 WORK CATEG. FOR SUPPL. SECURITY         
058900        05 MSGI-KDARBTYP-SEC-4352                                         
059000                             PIC X(8).                                    
059100*                                 TYP AV ARBETE FÖR SECURITY-4352         
059200*                                 TYPE OF WORK FOR SECURITY-4352          
059300        05 MSGI-RESERV-SEC   PIC X(126).                                  
059400     03 MSGI-SPAR-AREA       PIC X(1000).                                 
059500*** END OF VILMAII-COPY LENGTH= 2000 BYTES                                
