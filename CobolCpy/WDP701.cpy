000100 01  INIT-WDP701.                                                         
000200*                                 BESKRIVNING AV GENERELL                 
000300*                                 INIT-IO-AREA                            
000400     03 INIT-USER.                                                        
000500*                                 USER-INFORMATION                        
000600*                                 USER INFORMATION                        
000700        05 INIT-IDUSER       PIC X(8).                                    
000800*                                 ANVÄNDARENS SÄKERHETS ID                
000900*                                 USER SECURITY-IDENTITY                  
001000        05 INIT-BEANST       PIC X(25).                                   
001100*                                 ANSTÄLLDS NAMN                          
001200*                                 NAME OF EMPLOYED                        
001300        05 INIT-IDAVD        PIC X(5).                                    
001400*                                 DEN ANSTÄLLDES AVDELNING/               
001500*                                 KOSTNADSSTÄLLE                          
001600*                                 DEPARTMENT OF EMPLOYED/                 
001700*                                 COST CENTER                             
001800        05 INIT-IDCSS        PIC X(8).                                    
001900*                                 CSS-STILMALL                            
002000*                                 CASCADING STYLESHEET                    
002100        05 INIT-IDDC         PIC X(2).                                    
002200*                                 IDENTIFIERARE LAGER                     
002300*                                 WAREHOUSE IDENTIFIER                    
002400        05 INIT-IDFTG        PIC 9(2).                                    
002500*                                 FÖRETAGSID EKONOM REDOVISNING           
002600*                                 COMPANY IDENTITY ACCOUNTING             
002700        05 INIT-IDLAND-SPR   PIC X(2).                                    
002800*                                 2-STÄLLIG LANDSKOD FÖR SPRÅK            
002900*                                 2-CHAR COUNTRY CODE FOR LANG.           
003000        05 INIT-IDLTERM      PIC X(8).                                    
003100*                                 LOGISKT TERMINALNAMN                    
003200*                                 IDENTITY OF LOGICAL TERMINAL            
003300        05 INIT-IDNODE       PIC X(8).                                    
003400*                                 VTAM NODE-NAMN                          
003500*                                 VTAM NODE NAME                          
003600        05 INIT-IDRT-KEY     PIC X(3).                                    
003700*                                 RETURTERMINAL                           
003800*                                 RETURN TERMINAL                         
003900        05 INIT-IDSPRAK      PIC X(2).                                    
004000*                                 2-STÄLLIG ISO SPRÅKKOD                  
004100*                                 2-LETTER ISO LANGUAGE CODE              
004200        05 INIT-IDTFN        PIC X(20).                                   
004300*                                 TELEFONNUMMER EXTERNT                   
004400*                                 TELEPHONE NUMBER  EXTERNAL              
004500        05 INIT-IDTFX        PIC X(20).                                   
004600*                                 TELEFAXNUMMER                           
004700*                                 FAXNUMBER                               
004800        05 INIT-IDTRANS      PIC X(4).                                    
004900*                                 BILDNUMMER                              
005000*                                 SCREEN NUMBER                           
005100        05 INIT-IDTIDZON     PIC X(2).                                    
005200*                                 TIDZONER PÅ JORDEN.                     
005300*                                 TIME ZONE ON EARTH                      
005400        05 INIT-KDMATT       PIC X.                                       
005500*                                 MÅTTKOD                                 
005600*                                 MEASUREMENT CODE                        
005700        05 INIT-KDMFSFOR     PIC X.                                       
005800*                                 TYP AV MFS-FORMAT                       
005900*                                 1 = W-FORMAT  2 = N-FORMAT              
006000*                                 TYPE OF MFS FORMAT                      
006100        05 INIT-KDSVAR       PIC X.                                       
006200*                                 SVAR FRÅN PROGRAM ELLER SKÄRM           
006300*                                 RETURN CODE FROM PROGRAM                
006400        05 INIT-TIREGDAT-MPP PIC X(6).                                    
006500*                                 REGISTRERINGSDATUM I IMS-MPP            
006600*                                 REGISTRATION DATE IN IMS-MPP            
006700        05 INIT-TIUPPDAT     PIC X(6).                                    
006800*                                 UPPDATERINGSDATUM  (ÅÅMMDD)             
006900*                                 UPDATING DATE     (YYMMDD)              
007000        05 INIT-TIUPPTID     PIC X(8).                                    
007100*                                 UPPDATERINGSTID  (TTMMSSTH)             
007200*                                 UPDATING TIME    (HHMMSSTH)             
007300        05 INIT-RESERVINFO   PIC X(30).                                   
007400     03 INIT-NYCKLAR.                                                     
007500*                                 SPARADE NYCKLAR FRÅN MID                
007600*                                 SAVED KEYS FROM MID                     
007700        05 INIT-ADGANG-FOM   PIC X(2).                                    
007800*                                 GÅNG FRÅN OCH MED                       
007900*                                 AISLE ADDRESS FROM                      
008000        05 INIT-ADGANG-TOM   PIC X(2).                                    
008100*                                 GÅNG TILL OCH MED                       
008200*                                 AISLE ADDRESS TO                        
008300        05 INIT-ADINLOMR     PIC X(4).                                    
008400*                                 INLEVERANSOMRÅDE                        
008500*                                 RECEIVING AREA                          
008600        05 INIT-ADINLOMR-NXT PIC X(4).                                    
008700*                                 INLEVERANSOMRÅDE NÄSTA                  
008800*                                 RECEIVING AREA NEXT                     
008900        05 INIT-ADINLOMR-PRT PIC X(4).                                    
009000*                                 PRINTERPLACERING                        
009100*                                 PLACE OF A PRINTER                      
009200        05 INIT-ADLAGOMR     PIC X(2).                                    
009300*                                 LAGEROMRÅDE                             
009400*                                 AREA                                    
009500        05 INIT-ADLAGOMR-FOM PIC X(2).                                    
009600*                                 LAGEROMRÅDE FRÅN OCH MED                
009700*                                 AREA ADDRESS FROM                       
009800        05 INIT-ADLAGOMR-TOM PIC X(2).                                    
009900*                                 LAGEROMRÅDE TILL OCH MED                
010000*                                 AREA ADDRESS TO                         
010100        05 INIT-ADLEV-FOM    PIC X(2).                                    
010200*                                 LAGERPLATSNUMMER FOM                    
010300*                                 LEVEL FROM                              
010400        05 INIT-ADLEV-TOM    PIC X(2).                                    
010500*                                 LAGERPLATSNUMMER THRU                   
010600*                                 LEVEL THRU                              
010700        05 INIT-ADPLATS      PIC X(5).                                    
010800*                                 LAGERPLATSNUMMER                        
010900*                                 LOCATION                                
011000        05 INIT-ADSEC-FOM    PIC X(2).                                    
011100*                                 LAGERPLATSNUMMER FOM                    
011200*                                 SECTION FROM                            
011300        05 INIT-ADSEC-TOM    PIC X(2).                                    
011400*                                 LAGERPLATSNUMMER THRU                   
011500*                                 SECTION THRU                            
011600        05 INIT-ADSEQ-FOM    PIC X.                                       
011700*                                 LAGERPLATSNUMMER  FRÅN                  
011800*                                                                         
011900*                                 SEQUENCE POSITION NUMBER FROM           
012000        05 INIT-ADSEQ-TOM    PIC X.                                       
012100*                                 LAGERPLATSNUMMER  TILL                  
012200*                                 SEQUENCE POSITION NUMBER TO             
012300        05 INIT-BEFT         PIC X(2).                                    
012400*                                 FÖRPACKNINGSTYP                         
012500*                                 PACKAGING TYPE                          
012600        05 INIT-DASUPREF     PIC X(8).                                    
012700*                                 SÄNDNINGSDATUM DIREKTLEVERANTÖR         
012800*                                 SHIPPING DATE DIRECT SUPPLIER           
012900        05 INIT-FLINLFB      PIC X.                                       
013000*                                 VALD TILL FÖRBEHANDLING                 
013100*                                 SELECTED FOR PRETREATEMENT              
013200        05 INIT-FLINLI       PIC X.                                       
013300*                                 INLAGD RAD, PARTI ELLER KOLLI           
013400*                                 STORED  LINE                            
013500        05 INIT-FLLDCKND     PIC X.                                       
013600*                                 FL LDC-KUND                             
013700*                                 FL LDC CUSTOMER                         
013800        05 INIT-FLMRKVAL     PIC X.                                       
013900*                                 FLAGGA FÖR MARKNADSVALUTA               
014000*                                 FLAG FOR MARKET CURRENCY                
014100        05 INIT-FLORDLEV     PIC X.                                       
014200*                                 FLAGGA LEVERANSORDERNUMMER              
014300*                                 DELIVERY ORDER FLAG                     
014400        05 INIT-FLVISA       PIC X.                                       
014500*                                 ALLMÄN FLAGGA FÖR DATAVISNING           
014600*                                 GENERAL FLAG FOR SHOWING INFO           
014700        05 INIT-IDANSK       PIC X(3).                                    
014800*                                 ANSKAFFARNUMMER                         
014900*                                 PROCURER NO.                            
015000        05 INIT-IDANSK-FOM   PIC X(3).                                    
015100*                                 LÄGSTA ANSKAFFARNR I INTERVALL          
015200*                                 LOWEST PURCHASE PLANNER NUMBER          
015300        05 INIT-IDANSK-TOM   PIC X(3).                                    
015400*                                 HÖGSTA ANSKAFFARNR I INTERVALL          
015500*                                 HIGHEST PURCHASE PLANNER NO             
015600        05 INIT-IDANSTNR     PIC X(5).                                    
015700*                                 ANSTÄLLNINGSNUMMER                      
015800*                                 IDENTIFICATION NO EMPLOYEE              
015900        05 INIT-IDARTNR      PIC X(9).                                    
016000*                                 ARTIKELNUMMER                           
016100*                                 PART NUMBER                             
016200        05 INIT-IDBYTKOL     PIC X(3).                                    
016300*                                 BYTES KOLLINUMMER                       
016400*                                 EXCHANGE CASE NUMBER                    
016500        05 INIT-IDBYTRAP     PIC X(7).                                    
016600*                                 RAPPORTNUMMER  BYTES                    
016700*                                 REPORTNUMBER   EXCHANGE                 
016800        05 INIT-IDCATAVS     PIC X(4).                                    
016900*                                 KATALOG-AVSNITT                         
017000*                                 CATALOG TEXT BLOCK                      
017100        05 INIT-IDCATGRP     PIC X(2).                                    
017200*                                 KATALOG-GRUPP                           
017300*                                 CATALOG-GROUP                           
017400        05 INIT-IDCATNR      PIC X(5).                                    
017500*                                 KATALOG-ID                              
017600*                                 CATALOG-ID                              
017700        05 INIT-IDCATRAD     PIC X(4).                                    
017800*                                 RADNUMMER                               
017900*                                 ROW NUMBER IN TEXT BLOCK                
018000        05 INIT-IDDC-BULK    PIC X(2).                                    
018100*                                 IDENTIFIERARE BULKORDERLAGER            
018200*                                 WAREHOUSE IDENTIFIER BULK               
018300*                                 ORDERS                                  
018400        05 INIT-IDDC-DAY     PIC X(2).                                    
018500*                                 IDENTIFIERARE DAGORDERLAGER             
018600*                                 WAREHOUSE IDENTIFIER DAILY              
018700*                                 ORDERS                                  
018800        05 INIT-IDDC-KEY     PIC X(2).                                    
018900*                                 DC FÖR HOPP MELLAN BILDER               
019000*                                 SAVED KEY BETWEEN SCREENS               
019100        05 INIT-IDDC-REC     PIC X(2).                                    
019200*                                 MOTTAGANDE LAGER                        
019300*                                 RECEIVING WAREHOUSE                     
019400        05 INIT-IDDC-SEND    PIC X(2).                                    
019500*                                 SÄNDANDE LAGER                          
019600*                                 SENDING WAREHOUSE                       
019700        05 INIT-IDDIRGRP     PIC X(10).                                   
019800*                                 DIREKTLEVERANSGRUPP                     
019900*                                 DIREKTLEVERANSGRUPP                     
020000        05 INIT-IDDISTR      PIC X(4).                                    
020100*                                 DISTRIKTNUMMER                          
020200*                                 DISTRICT NUMBER                         
020300        05 INIT-IDDISTR-FOM  PIC X(4).                                    
020400*                                 LÄGSTA DISTRIKTNR I INTERVALL           
020500*                                 LOWEST DISTRICT NUMBER                  
020600        05 INIT-IDDISTR-TOM  PIC X(4).                                    
020700*                                 HÖGSTA DISTRIKTNR I INTERVALL           
020800*                                 HIGHEST DISTRICT NUMBER                 
020900        05 INIT-IDFAKT       PIC X(7).                                    
021000*                                 FAKTURANUMMER                           
021100*                                 INVOICE NO.                             
021200        05 INIT-IDFKNGRP     PIC X(4).                                    
021300*                                 FUNKTIONSGRUPP                          
021400*                                 FUNCTION GROUP                          
021500        05 INIT-IDFOTNR      PIC X(5).                                    
021600*                                 FOTNOTSNUMMER                           
021700*                                 FOOT NOTE ID NUMBER                     
021800        05 INIT-IDFPINST     PIC X(7).                                    
021900*                                 FÖRPACKNINGSINSTRUKTION NR              
022000*                                 PACKAGE INSTRUCTION NUMBER              
022100        05 INIT-IDFS         PIC X(8).                                    
022200*                                 FÖLJESEDELSNUMMER ENL ODETTE            
022300*                                 ADVICE NOTE NUMBER ODETTE               
022400        05 INIT-IDILIRAD     PIC X(5).                                    
022500*                                 INLÄGGNINGSLISTERADNUMMER               
022600*                                 REPORTINGLISTLINENUMBER                 
022700        05 INIT-IDILIST      PIC X(5).                                    
022800*                                 INLÄGGNINGSLISTEIDENTITET               
022900*                                 REPORTINGLIST-IDENTITY                  
023000        05 INIT-IDILLU       PIC X(5).                                    
023100*                                 ILLUSTRATIONENS NR                      
023200*                                 ILLUSTRATION NUMBER                     
023300        05 INIT-IDINLVGN     PIC X(3).                                    
023400*                                 VAGNSIDENTITET                          
023500*                                 INTERNAL CARRIER ID                     
023600        05 INIT-IDKAMP       PIC X(7).                                    
023700*                                 SERVICEKAMPANJ                          
023800*                                 SERVICE CAMPAIGN                        
023900        05 INIT-IDKAMP-GRP   PIC X(7).                                    
024000*                                 ID FÖR KAMPANJGRUPPER                   
024100*                                 ID OF CAMPAIGNGROUPS                    
024200        05 INIT-IDKOLLI      PIC X(5).                                    
024300*                                 KOLLINUMMER                             
024400*                                 CASE NUMBER                             
024500        05 INIT-IDKONCNR     PIC X(3).                                    
024600*                                 KONCERNNUMMER                           
024700*                                 CONCERN NO                              
024800        05 INIT-IDKONTO      PIC X(10).                                   
024900*                                 KONTO                                   
025000*                                 ACCOUNT                                 
025100        05 INIT-IDKR         PIC X(5).                                    
025200*                                 KONTROLLRAPPORT NUMMER                  
025300*                                 INSPECTION REPORT NUMBER                
025400        05 INIT-IDKUNDNR     PIC X(6).                                    
025500*                                 KUNDNUMMER                              
025600*                                 CUSTOMER NO                             
025700        05 INIT-IDKUNDRF     PIC X(10).                                   
025800*                                 KUNDENS REFERENS (ORDERID)              
025900*                                 CUSTOMER REFERENCE (ORDER ID)           
026000        05 INIT-IDLANDX2     PIC X(2).                                    
026100*                                 2-STÄLLIG LANDSBETECKNINGSKOD           
026200*                                 2-LETTER CODE FOR COUNTRY               
026300        05 INIT-IDLASTN      PIC X(7).                                    
026400*                                 LASTNINGSNUMMER                         
026500*                                 LOADING NO.                             
026600        05 INIT-IDLBBET      PIC X(12).                                   
026700*                                 LASTBÄRARBETECKNING                     
026800*                                 TRAILER NUMBER                          
026900        05 INIT-IDLBFIKT     PIC X(3).                                    
027000*                                 FIKTIVT TRAILERNUMMER                   
027100*                                 FICTIVE TRAILER NUMBER                  
027200        05 INIT-IDLEVNR      PIC X(5).                                    
027300*                                 LEVERANTÖRNUMMER                        
027400*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
027500        05 INIT-IDLOPNRM     PIC X(8).                                    
027600*                                 LÖPNUMMER MOTTAGNINGSRAPPORT            
027700*                                 (0VVDLLLLK)                             
027800*                                 SERIAL NO RECEIVING REPORT              
027900*                                 (0WWDLLLLC)                             
028000        05 INIT-IDOKOLLI     PIC X(9).                                    
028100*                                 ODETTE KOLLINUMMER                      
028200*                                 ODETTE CASE NUMBER                      
028300        05 INIT-IDPARTNR     PIC X(9).                                    
028400*                                 FINANCIELL KUND                         
028500*                                 FINANCIAL CUST                          
028600        05 INIT-IDPERSON     PIC X(3).                                    
028700*                                 PERSONKOD                               
028800*                                 STAFF CODE                              
028900        05 INIT-IDPERSON-CDC PIC X(3).                                    
029000*                                 PERSONKOD CDC                           
029100*                                 CDC STAFF CODE                          
029200        05 INIT-IDPERSON-QUAL                                             
029300                             PIC X(3).                                    
029400*                                 PERSONKOD QUALITY                       
029500*                                 STAFF CODE QUALITY                      
029600        05 INIT-IDPRC.                                                    
029700*                                 PRODUKTIONSKANAL                        
029800*                                 PRODUCTION CHANNEL                      
029900           07 INIT-IDPRCBAS  PIC X(3).                                    
030000*                                 PRC-BAS                                 
030100*                                 PRC-BASIC                               
030200           07 INIT-IDPRCVAR  PIC X.                                       
030300*                                 PRC-VARIANT                             
030400*                                 PRC-VARIANT                             
030500        05 INIT-IDPRCTR      PIC X(10).                                   
030600*                                 PROFIT CENTER                           
030700*                                 PROFIT CENTER                           
030800        05 INIT-IDPRODNR     PIC X(7).                                    
030900*                                 PRODUKTIONSNUMMER                       
031000*                                 PRODUCTION NUMBER                       
031100        05 INIT-IDPROJ       PIC X(4).                                    
031200*                                 PARTS PROJEKTIDENTITET                  
031300*                                 PARTS PROJECT IDENTITY                  
031400        05 INIT-IDPROMR.                                                  
031500*                                 PRISOMRÅDE (RABATTSTRUKTUR)             
031600*                                 PRICE AREA                              
031700           07 INIT-IDMARKBO  PIC X.                                       
031800*                                 MARKNADSBOLAGSKOD                       
031900*                                 MARKET COMPANY CODE                     
032000           07 INIT-IDPROMRN  PIC X(2).                                    
032100*                                 PRISOMRÅDE LÖPNUMMER                    
032200*                                 PRICE AREA SERIALNUMBER                 
032300        05 INIT-IDPTYP       PIC X(3).                                    
032400*                                 POSTTYP                                 
032500*                                 RECORD TYPE                             
032600        05 INIT-IDRADNR      PIC X(4).                                    
032700*                                 RADNUMMER                               
032800*                                 LINE NO                                 
032900        05 INIT-IDRAPP       PIC X(10).                                   
033000*                                 RAPPORT ID                              
033100*                                 REPORT ID                               
033200        05 INIT-IDRAPPNR     PIC X(7).                                    
033300*                                 RAPPORT NUMMER                          
033400*                                 DISCREPANCY REPORT NUMBER               
033500        05 INIT-IDROLL       PIC X(5).                                    
033600*                                 VOR ROLL ID                             
033700*                                 VOR ROLE ID                             
033800        05 INIT-IDRT         PIC X(3).                                    
033900*                                 RETURTERMINAL                           
034000*                                 RETURN TERMINAL                         
034100        05 INIT-IDRTLOP      PIC X(3).                                    
034200*                                 RETUR TERMINAL LÖPNUMMER                
034300*                                 RETURN TERMINAL SEQUENCE NUMBER         
034400        05 INIT-IDRUBNR      PIC X(5).                                    
034500*                                 RUBRIKNUMMER                            
034600*                                 HEADLINE ID NUMBER                      
034700        05 INIT-IDSHIPM      PIC X(7).                                    
034800*                                 SKEPPNINGSNUMMER                        
034900*                                 SHIPMENT NO                             
035000        05 INIT-IDSKEPPN     PIC X(7).                                    
035100*                                 SKEPPNINGSNUMMER                        
035200*                                 SHIPMENT NO                             
035300        05 INIT-IDSNDJOB     PIC X(8).                                    
035400*                                 SÄNDANDE JOB IDENTITET                  
035500*                                 IDENTITY OF SENDING JOB                 
035600        05 INIT-IDSNDNOD     PIC X(8).                                    
035700*                                 SÄNDANDE NODE IDENTITET                 
035800*                                 IDENTITY OF SENDING NODE                
035900        05 INIT-IDSUPREF     PIC X(10).                                   
036000*                                 LEVERANTöRSREF.                         
036100*                                 SUPPLIER REF.                           
036200        05 INIT-IDSYSMOT     PIC X(6).                                    
036300*                                 PULS MOTTAGANDE SYSTEMNAMN              
036400*                                 PULS RECEIVING SYSTEM NAME              
036500        05 INIT-IDTABNR      PIC X(3).                                    
036600*                                 TABELLNUMMER                            
036700*                                 TABELNUMBER                             
036800        05 INIT-IDTRPTNR     PIC X(3).                                    
036900*                                 TRANSPORTIDENTITET                      
037000*                                 TRANSPORT IDENTITY                      
037100        05 INIT-IDTTEXNR     PIC X(5).                                    
037200*                                 TILLÄGGSTEXT-NR                         
037300*                                 ADDITIONAL TEXT, ID NUMBER              
037400        05 INIT-IDUSER-KEY   PIC X(8).                                    
037500*                                 USER FÖR HOPP MELLAN BILDER             
037600*                                 USER ID ON SCREENS KEY                  
037700        05 INIT-KDANMORS     PIC X(2).                                    
037800*                                 ORSAK TILL LEVERANSANMÄRKNING           
037900*                                 DISCREPANCY REPORT REASON CODE          
038000        05 INIT-KDARBVAL     PIC X.                                       
038100*                                 ARBETSTYPSVAL KOD                       
038200*                                 CATEGORY OF WORK CODE                   
038300        05 INIT-KDARBTYP     PIC X(8).                                    
038400*                                 TYP AV ARBETE                           
038500*                                 CATEGORY OF WORK                        
038600        05 INIT-KDARTKAM     PIC X(5).                                    
038700*                                 TRANSFER KOD                            
038800*                                 TRANSFER CONDITION CODE                 
038900        05 INIT-KDAVROP      PIC X.                                       
039000*                                 AVROPSKOD                               
039100*                                 CALLED                                  
039200        05 INIT-KDBYTSTA     PIC X.                                       
039300*                                 STATUSKOD BYTESOBJEKT                   
039400*                                 STATUSCODE EXCH CORES                   
039500        05 INIT-KDEKHHT      PIC X(3).                                    
039600*                                 EKONOMISK HUVUDHÄNDELSE                 
039700*                                 ECONOMIC MAIN EVENT                     
039800        05 INIT-KDEKNIVA     PIC X(5).                                    
039900*                                 EKONOMISK HÄNDELSENIVÅ                  
040000*                                 ECONOMICAL EVENT LEVEL                  
040100        05 INIT-KDEKSHT      PIC X(3).                                    
040200*                                 EKONOMISK SUBHÄNDELSE                   
040300*                                 ECONOMIC SUB EVENT                      
040400        05 INIT-KDEXCHA      PIC X(3).                                    
040500*                                 EXCHANGE ACCOUNT CODE                   
040600        05 INIT-KDFRAKT      PIC X(2).                                    
040700*                                 FRAKTSÄTT DC TILL KUND                  
040800*                                 FREIGHT CODE                            
040900        05 INIT-KDFREQ       PIC X(2).                                    
041000*                                 FREQUENCY CODE                          
041100*                                 FREQUENCY CODE                          
041200        05 INIT-KDKOMSTA     PIC X.                                       
041300*                                 KOMMUNIKATIONSSTATUS                    
041400*                                 COMMUNICATION STATUS                    
041500        05 INIT-KDKRENOT     PIC X(2).                                    
041600*                                 TYP AV KREDITERING                      
041700*                                 CREDIT NOTE TYPE                        
041800        05 INIT-KDLARM       PIC X(3).                                    
041900*                                 LARMORSAKSKOD                           
042000*                                 ALARM REASON CODE                       
042100        05 INIT-KDLEVANM     PIC X.                                       
042200*                                 STATUS LEVERANSANMÄRKNING               
042300*                                 STATUS DISCREPANCY                      
042400        05 INIT-KDLEVANM-FOM PIC X.                                       
042500*                                 MIN STATUS LEVERANSANMÄRKNING           
042600*                                 MIN STATUS DISCREPANCY                  
042700        05 INIT-KDLEVANM-TOM PIC X.                                       
042800*                                 MAX STATUS LEVERANSANMÄRKNING           
042900*                                 MAX STATUS DISCREPANCY                  
043000        05 INIT-KDLEVPLF     PIC X.                                       
043100*                                 KOD FÖR LEVPLAN-GODKÄNNANDE             
043200*                                 CODE FOR APPROVAL OF SCHEDULE P         
043300*                                 ROPOSAL                                 
043400        05 INIT-KDLPORS      PIC X(2).                                    
043500*                                 LEVERANSPLANEORSAK                      
043600        05 INIT-KDPRIO-PF    PIC X.                                       
043700*                                 PRIORITETSKOD PÅFYLLNAD                 
043800*                                 PRIORITY FILLUP PICKING ADDRESS         
043900        05 INIT-KDPRODSL     PIC X(2).                                    
044000*                                 PRODUKTSLAG                             
044100*                                 PRODUCT GROUP                           
044200        05 INIT-KDPRT        PIC X(3).                                    
044300*                                 PRINTERKOD                              
044400*                                 PRINTERCODE                             
044500        05 INIT-KDRETSTA     PIC X.                                       
044600*                                 STATUS RETURER                          
044700*                                 RETURN STATUS                           
044800        05 INIT-KDSORT       PIC X(2).                                    
044900*                                 SORT-KOD                                
045000*                                 UNIT OF MEASURE                         
045100        05 INIT-KDSTAPF      PIC X.                                       
045200*                                 STATUS PÅ PÅFYLLNING                    
045300*                                 STATUS FOR FILLING                      
045400        05 INIT-KDSTARAD     PIC X.                                       
045500*                                 RADSTATUSKOD                            
045600*                                 LINE STATUS CODE                        
045700        05 INIT-KDSTOR       PIC X(3).                                    
045800*                                 STORAGE CODE                            
045900*                                 STORAGE CODE                            
046000        05 INIT-KDTRPDOCT    PIC X.                                       
046100*                                 TYP AV TRANSPORTDOKUMENT                
046200*                                 TYPE OF TRANSPORT DOCUMENT              
046300        05 INIT-KDURVAL      PIC X.                                       
046400*                                 KOD FÖR BEGRÄNSNING AV URVAL            
046500*                                 LIMIT SELECTION TREATMENT               
046600        05 INIT-TEVORMRK     PIC X(2).                                    
046700*                                 MÄRKNINGSTEXT FÖR                       
046800*                                 VOR-KÖN                                 
046900        05 INIT-TIAAVV-FOM   PIC X(4).                                    
047000*                                 ÅR - VECKA  (ÅÅVV)                      
047100*                                 YEAR - WEEK  (YYWW)                     
047200        05 INIT-TIAAVV-TOM   PIC X(4).                                    
047300*                                 ÅR - VECKA  (ÅÅVV)                      
047400*                                 YEAR - WEEK  (YYWW)                     
047500        05 INIT-TIAVIDAT     PIC X(6).                                    
047600*                                 AVISERINGSDATUM (YYMMDD)                
047700*                                 ADVICE NOTE DATE                        
047800        05 INIT-TIFINLV-FOM  PIC X(5).                                    
047900*                                 PUBL.VECKA-FOM, (ÅÅVVD  D=1)            
048000*                                 DATE GOODS REC-FROM,(YYWWD D=1)         
048100        05 INIT-TIFINLV-TOM  PIC X(5).                                    
048200*                                 PUBL.VECKA-FOM, (ÅÅVVD  D=1)            
048300*                                 DATE GOODS REC-UNTIL(YYWWD D=1)         
048400        05 INIT-TIKLOCK      PIC X(8).                                    
048500*                                 KLOCKSLAG (TTMMSSTH)                    
048600*                                 TIME OF DAY (HHMMSSTH)                  
048700        05 INIT-TIREGDAT     PIC X(6).                                    
048800*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
048900*                                 REGISTRATION DATE (YYMMDD)              
049000        05 INIT-TISKEPPN     PIC X(6).                                    
049100*                                 SKEPPNINGSDATUM  (ÅÅMMDD)               
049200*                                 SHIPPING DATE    (YYMMDD)               
049300        05 INIT-TISKROT-BEORD                                             
049400                             PIC X(6).                                    
049500*                                 BEORDRAD SKROTNINGSDATUM                
049600*                                 DATE OF SCRAPPING DECISION              
049700        05 INIT-KDPRTVAL-ADR PIC X(2).                                    
049800*                                 PRINTER-VAL KOD ADRESS FLAGGA           
049900*                                 PRINTER SEL. CODE ADDRESS FLAG          
050000        05 INIT-KDPRTVAL-FS  PIC X(2).                                    
050100*                                 PRINTER-VAL KOD FÖLJESEDEL              
050200*                                 PRINTER DEL. NOTE SELECT CODE           
050300        05 INIT-IDSPRAK-KEY  PIC X(2).                                    
050400*                                 SPRÅK FÖR HOPP MELLAN BILDER            
050500*                                 LANGUAGE KEY BETWEEN SCREENS            
050600        05 INIT-IDKOLLI-SAMP PIC X(5).                                    
050700*                                 SAMPACKNINGSKOLLINUMMER                 
050800*                                 MIXED PACKING CASE NUMBER               
050900        05 INIT-IDVO         PIC X(2).                                    
051000*                                 VERKSAMHETSOMRÅDE SAMLINGSKOLLI         
051100*                                 AREA OF OPERATIONS MIX CASES            
051200        05 INIT-IDKAMPRF     PIC X(7).                                    
051300*                                 KAMPANJREFERENS                         
051400*                                 CAMPAIGN REFERENCE                      
051500        05 INIT-BEWEBSCR     PIC X(15).                                   
051600*                                 VALD BILD I PULS WEBBEN                 
051700*                                 CHOSEN SCREEN ON PULS WEB               
051800        05 INIT-FLSORT       PIC X.                                       
051900*                                 SORTERINGSFLAGGA (J/N)                  
052000        05 INIT-TIREGTID     PIC X(6).                                    
052100*                                 REGISTRERINGSTID                        
052200*                                 GENERAL REGISTRATION TIME               
052300        05 INIT-KDVALISO     PIC X(3).                                    
052400*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
052500*                                 CURRENCY CODE BY ISO-STANDARD.          
052600        05 INIT-IDFTG-KEY    PIC X(2).                                    
052700*                                 FTG FÖR HOPP MELLAN BILDER              
052800*                                 SAVED KEY BETWEEN SCREENS               
052900        05 INIT-KDMAIL       PIC X(4).                                    
053000*                                 TYP AV MAIL UTSKICK                     
053100*                                 TYPE OF MAIL SENDNINGS                  
053200        05 INIT-IDPRCTAB     PIC 9(2).                                    
053300*                                 PRCTABELLIDENTITET                      
053400*                                 PRC TABLE IDENTITY                      
053500        05 INIT-KDVALTYP     PIC X.                                       
053600*                                 KURSENS PER A=ÅR/M=MÅNAD/D=DAG          
053700*                                 CURRENCY PER YEAR/MONTH/DAY             
053800        05 INIT-KDOTFREK     PIC X.                                       
053900*                                 ORDERTRÄFF FREKVENSEN ARTIKEL           
054000*                                 ORDER HIT FREQUENCY FOR PART            
054100        05 INIT-TIAAVV       PIC X(4).                                    
054200*                                 ÅR - VECKA  (ÅÅVV)                      
054300*                                 YEAR - WEEK  (YYWW)                     
054400        05 INIT-KDFARLIG     PIC X.                                       
054500*                                 KOD FÖR FARLIGT GODS                    
054600*                                 DANGEROUS GOODS CODE                    
054700        05 INIT-TIRFSDAT-CDC PIC X(6).                                    
054800*                                 KLART FÖR TRANSPORT CDC ÅÅMMDD          
054900*                                 READY FOR SHIPMENT CDC YYMMDD           
055000        05 INIT-KVBEART      PIC X(6).                                    
055100*                                 BESTÄLLT ANTAL STYCKEN                  
055200*                                 ORDERED QUANTITY                        
055300        05 INIT-KDORDKL      PIC X.                                       
055400*                                 ORDERKLASS                              
055500*                                 ORDER CLASS                             
055600        05 INIT-IDDC-REF     PIC X(2).                                    
055700*                                 SÄNDANDE LAGER FÖR REFILL               
055800*                                 SENDING WAREHOUSE FOR REFILL            
055900        05 INIT-RESERV-KEY   PIC X(55).                                   
056000     03 INIT-SECURITY.                                                    
056100*                                 INFO FÖR SECURITY                       
056200*                                 INFO FOR SECURITY                       
056300        05 INIT-KDARBTYP-SEC PIC X(8).                                    
056400*                                 TYP AV ARBETE FÖR SECURITY              
056500*                                 TYPE OF WORK FOR SECURITY               
056600        05 INIT-KDARBTYP-SEC-IDLEV                                        
056700                             PIC X(8).                                    
056800*                                 ARBETSTYP FÖR SÄKERHET PÅ LEV           
056900*                                 WORK CATEG. FOR SUPPL. SECURITY         
057000        05 INIT-KDARBTYP-SEC-4352                                         
057100                             PIC X(8).                                    
057200*                                 TYP AV ARBETE FÖR SECURITY-4352         
057300*                                 TYPE OF WORK FOR SECURITY-4352          
057400        05 INIT-RESERV-SEC   PIC X(126).                                  
057500     03 INIT-SPAR-AREA       PIC X(1000).                                 
057600*** END OF VILMAII-COPY LENGTH= 2000 BYTES                                
