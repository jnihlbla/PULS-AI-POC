000100 01  W41842X.                                                             
000200*                                 INFO FROM WDA2 - TO CREATE WXTR         
000300*                                  FILE IN EDITABLE FORMAT                
000400     03 IDLEVANM.                                                         
000500*                                 LEVERANSANMÄRKNINGSIDENTITET            
000600*                                 DISCREPANCY REPORT IDENTITY             
000700        05 IDDISTR           PIC Z(3)9                                    
000800                             VALUE ZEROS.                                 
000900*                                 DISTRIKTNUMMER                          
001000*                                 DISTRICT NUMBER                         
001100        05 IDKUNDNR          PIC Z(5)9                                    
001200                             VALUE ZEROS.                                 
001300*                                 KUNDNUMMER                              
001400*                                 CUSTOMER NO                             
001500        05 IDRAPPNR          PIC Z(6)9                                    
001600                             VALUE ZEROS.                                 
001700*                                 RAPPORT NUMMER                          
001800*                                 DISCREPANCY REPORT NUMBER               
001900     03 IDFTG-ANM            PIC 9(2)                                     
002000                             VALUE ZEROS.                                 
002100*                                 FÖRETAGSID EKONOM REDOVISNING           
002200*                                 COMPANY IDENTITY ACCOUNTING             
002300     03 IDPERSON-ANM         PIC Z(2)9                                    
002400                             VALUE ZEROS.                                 
002500*                                 PERSONKOD                               
002600*                                 STAFF CODE                              
002700     03 IDUSER               PIC X(8)                                     
002800                             VALUE SPACES.                                
002900*                                 ANVÄNDARENS SÄKERHETS ID                
003000*                                 USER SECURITY-IDENTITY                  
003100     03 KDARBTYP-ANM         PIC X(8)                                     
003200                             VALUE SPACES.                                
003300*                                 TYP AV ARBETE                           
003400*                                 CATEGORY OF WORK                        
003500     03 KDLEVANM             PIC X                                        
003600                             VALUE SPACE.                                 
003700*                                 STATUS LEVERANSANMÄRKNING               
003800*                                 STATUS DISCREPANCY                      
003900     03 KVRADER-OBEH         PIC Z(4)9                                    
004000                             VALUE ZEROS.                                 
004100*                                 ANTAL OBEHANDLADE RADER                 
004200*                                 NUMBER OF NOT TREATED LINES             
004300     03 KVRADER-RT           PIC Z(4)9                                    
004400                             VALUE ZEROS.                                 
004500*                                 ANTAL RADER RETURTILLSTÅND              
004600*                                 NUMBER OF LINES RETURNPERMIT            
004700     03 PRFOERS              PIC Z(6)9.9(2)                               
004800                             VALUE ZEROS.                                 
004900*                                 FÖRSÄKRINGSPREMIE                       
005000*                                 INSURANCE FEE                           
005100     03 PRFRAKT-ANM          PIC Z(6)9.9(2)                               
005200                             VALUE ZEROS.                                 
005300*                                 FRAKTKOSTNAD                            
005400*                                 FREIGHT COST                            
005500     03 PRLEGKST             PIC Z(6)9.9(2)                               
005600                             VALUE ZEROS.                                 
005700*                                 LEGALISERINSKOSTNAD                     
005800*                                 LEGALIZATION FEE                        
005900     03 REEMBHNT             PIC Z9.9                                     
006000                             VALUE ZEROS.                                 
006100*                                 EMB OCH HANTERINGSKOST (%)              
006200*                                 PACKING AND HANDLING (%)                
006300     03 RELANDCO             PIC Z(2)9.9(2)                               
006400                             VALUE ZEROS.                                 
006500*                                 LANDING COST PROCENT                    
006600*                                 LANDING COST PERCENT                    
006700     03 DALEVANM-ANM         PIC 9(8)                                     
006800                             VALUE ZEROS.                                 
006900*                                 DATUM LEV.ANMÄRKNING(YYYYMMDD)          
007000*                                 DISCREPANCY REPORT DATE                 
007100     03 DARETANK             PIC 9(8)                                     
007200                             VALUE ZEROS.                                 
007300*                                 ANKOMSTDATUM (ÅÅÅÅMMDD)                 
007400*                                 DATE GOODS RECEIVING(YYYYMMDD)          
007500     03 DARETILL             PIC 9(8)                                     
007600                             VALUE ZEROS.                                 
007700*                                 RETURTILLSTÅNDSDATUM (AAAAMMDD)         
007800*                                 DATE RETURNPERMIT    (YYYYMMDD)         
007900     03 FLFARLIG             PIC X                                        
008000                             VALUE SPACE.                                 
008100*                                 FARLIGT GODS-FLAGGA                     
008200*                                 DENGEROUS GOODS FLAG                    
008300     03 DARTPMN              PIC 9(8)                                     
008400                             VALUE ZEROS.                                 
008500*                                 PÅMINNELSE RETURTILLSTÅNDSDAT.          
008600*                                 (YYYYMMDD)                              
008700*                                 DATE RETURNPERMIT REMINDER              
008800     03 KDLEVANM-UPD         PIC X                                        
008900                             VALUE SPACE.                                 
009000*                                 STATUS LEVERANSANMÄRKNING               
009100*                                 STATUS DISCREPANCY                      
009200     03 KDVALISO             PIC X(3)                                     
009300                             VALUE SPACES.                                
009400*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
009500*                                 CURRENCY CODE BY ISO-STANDARD.          
009600     03 BEANST               PIC X(25)                                    
009700                             VALUE SPACES.                                
009800*                                 ANSTÄLLDS NAMN                          
009900*                                 NAME OF EMPLOYED                        
010000     03 IDUSER-ADM           PIC X(8)                                     
010100                             VALUE SPACES.                                
010200*                                 ANVÄNDAR-ID ADMINISTRATIV KONTR         
010300*                                 USER ID ADMINISTRATIVE INSPEC.          
010400     03 KDLEVATT             PIC 9                                        
010500                             VALUE ZEROS.                                 
010600*                                 ATTESTERING KOD LEVERANSANM.            
010700*                                 ATTEST CODE DISCREPANCY REPORT          
010800     03 IDDC-RET-ANM         PIC X(2)                                     
010900                             VALUE SPACES.                                
011000*                                 MOTTAGANDE LAGER FÖR RETURER            
011100*                                 RECEIVING WAREHOUSE FOR RETURNS         
011200     03 IXDCCLEAR            PIC 9                                        
011300                             VALUE ZERO.                                  
011400*                                 CLEARING DC SEKVENS                     
011500*                                 POSITION FOR CLEARING DC                
011600     03 IDSYSTEM             PIC X(4)                                     
011700                             VALUE SPACES.                                
011800*                                 VOLVO VCCS SYSTEMNUMMER                 
011900*                                 VOLVO VCCS SYSTEM NUMBER                
012000     03 IDARTNR              PIC Z(7)9                                    
012100                             VALUE ZEROS.                                 
012200*                                 ARTIKELNUMMER                           
012300*                                 PART NUMBER                             
012400     03 IDRADNR              PIC Z(3)9                                    
012500                             VALUE ZEROS.                                 
012600*                                 RADNUMMER                               
012700*                                 LINE NO                                 
012800     03 ADGANG               PIC Z9                                       
012900                             VALUE ZEROS.                                 
013000*                                 GÅNG                                    
013100*                                 AISLE                                   
013200     03 ADLAGOMR             PIC Z9                                       
013300                             VALUE ZEROS.                                 
013400*                                 LAGEROMRÅDE                             
013500*                                 AREA                                    
013600     03 ADPLATS              PIC Z(4)9                                    
013700                             VALUE ZEROS.                                 
013800*                                 LAGERPLATSNUMMER                        
013900*                                 LOCATION                                
014000     03 FLANLYSF             PIC X                                        
014100                             VALUE SPACE.                                 
014200*                                 FEL ANALYSNUMMER?                       
014300*                                 INCORRECT ANALYSISNUMBER?               
014400     03 FLANNULL             PIC X                                        
014500                             VALUE SPACE.                                 
014600*                                 ANNULLATION                             
014700*                                 CANCELLATION                            
014800     03 FLAUTKRE             PIC X                                        
014900                             VALUE SPACE.                                 
015000*                                 AUTOMATISK KREDITERING                  
015100*                                 AUTOMATIC DISCREPENCY                   
015200     03 FLDIRLEV             PIC X                                        
015300                             VALUE SPACE.                                 
015400*                                 DIREKTLEVERANS ?                        
015500*                                 DIRECT DELIVERY ?                       
015600     03 FLSKROT              PIC X                                        
015700                             VALUE SPACE.                                 
015800*                                 SKROTNINGSMARKERING                     
015900*                                 SCRAPPING FLAG                          
016000     03 FLSVAR               PIC X                                        
016100                             VALUE SPACE.                                 
016200*                                 ALLMÄN SVARSFLAGGA                      
016300*                                 GENERAL REPLY FLAG                      
016400     03 FLTEXT               PIC X                                        
016500                             VALUE SPACE.                                 
016600*                                 FINNS TEXTINFORMATION ?                 
016700     03 IDANALYS             PIC X(12)                                    
016800                             VALUE SPACES.                                
016900*                                 ANALYSNUMMER                            
017000*                                 ANALYSIS NUMBER                         
017100     03 IDANSTNR-RET         PIC Z(4)9                                    
017200                             VALUE ZEROS.                                 
017300*                                 ANSTÄLLNINGSNUMMER RETURAVDELN.         
017400*                                 EMPLOYEE NUMBER RETURNDEPT.             
017500     03 IDDC                 PIC X(2)                                     
017600                             VALUE SPACES.                                
017700*                                 IDENTIFIERARE LAGER                     
017800*                                 WAREHOUSE IDENTIFIER                    
017900     03 IDDC-RET-LEV         PIC X(2)                                     
018000                             VALUE SPACES.                                
018100*                                 MOTTAGANDE LAGER FÖR RETURER            
018200*                                 RECEIVING WAREHOUSE FOR RETURNS         
018300     03 IDFAKT               PIC Z(6)9                                    
018400                             VALUE ZEROS.                                 
018500*                                 FAKTURANUMMER                           
018600*                                 INVOICE NO.                             
018700     03 IDFAKT-LOC           PIC Z(6)9                                    
018800                             VALUE ZEROS.                                 
018900*                                 FAKTURANR LOKALT                        
019000*                                 LOCAL INVOICE NO                        
019100     03 IDFTG-LEV            PIC 9(2)                                     
019200                             VALUE ZEROS.                                 
019300*                                 FÖRETAGSID EKONOM REDOVISNING           
019400*                                 COMPANY IDENTITY ACCOUNTING             
019500     03 IDILIST              PIC 9(5)                                     
019600                             VALUE ZEROS.                                 
019700*                                 INLÄGGNINGSLISTEIDENTITET               
019800*                                 REPORTINGLIST-IDENTITY                  
019900     03 IDKNOTNR             PIC Z(6)9                                    
020000                             VALUE ZEROS.                                 
020100*                                 KREDITNOTANUMMER                        
020200*                                 CREDIT NOTE NUMBER                      
020300     03 IDKOLLI              PIC Z(4)9                                    
020400                             VALUE ZEROS.                                 
020500*                                 KOLLINUMMER                             
020600*                                 CASE NUMBER                             
020700     03 IDKONTO              PIC Z(9)9                                    
020800                             VALUE ZEROS.                                 
020900*                                 KONTO                                   
021000*                                 ACCOUNT                                 
021100     03 IDKST                PIC X(10)                                    
021200                             VALUE SPACES.                                
021300*                                 KOSTNADSSTÄLLE                          
021400*                                 COST CENTRE                             
021500     03 IDKUNDRF             PIC X(10)                                    
021600                             VALUE SPACES.                                
021700*                                 KUNDENS REFERENS (ORDERID)              
021800*                                 CUSTOMER REFERENCE (ORDER ID)           
021900     03 IDLOPNRM             PIC Z(7)9                                    
022000                             VALUE ZEROS.                                 
022100*                                 LÖPNUMMER MOTTAGNINGSRAPPORT            
022200*                                 (0VVDLLLLK)                             
022300*                                 SERIAL NO RECEIVING REPORT              
022400*                                 (0WWDLLLLC)                             
022500     03 IDPERSON-LEV         PIC Z(2)9                                    
022600                             VALUE ZEROS.                                 
022700*                                 PERSONKOD                               
022800*                                 STAFF CODE                              
022900     03 IDPERSON-REM         PIC Z(2)9                                    
023000                             VALUE ZEROS.                                 
023100*                                 PERSONKOD REMISS                        
023200*                                 STAFF CODE CONSIDERATION                
023300     03 IDUSER-PACK          PIC X(8)                                     
023400                             VALUE SPACES.                                
023500*                                 ANSVARIGT USERID PACKARE                
023600*                                 RESPONSIBLE USERID PACKER               
023700     03 KDANMORS             PIC X(2)                                     
023800                             VALUE SPACES.                                
023900*                                 ORSAK TILL LEVERANSANMÄRKNING           
024000*                                 DISCREPANCY REPORT REASON CODE          
024100     03 KDARBTYP-LEV         PIC X(8)                                     
024200                             VALUE SPACES.                                
024300*                                 TYP AV ARBETE                           
024400*                                 CATEGORY OF WORK                        
024500     03 KDARBTYP-REM         PIC X(8)                                     
024600                             VALUE SPACES.                                
024700*                                 REMISSANSVARIG LEVANM                   
024800*                                 PERSON WHO CONSIDERED DISCR.            
024900     03 KDEMBLEV             PIC 9                                        
025000                             VALUE ZERO.                                  
025100*                                 EMBALLAGEKOD PÅ LEVERANSANMÄRKN         
025200*                                 PACK CODE DISCREP                       
025300     03 KDFAKTYP             PIC X                                        
025400                             VALUE SPACE.                                 
025500*                                 FAKTURATYP                              
025600*                                 INVOICE TYPE                            
025700     03 KDFAKTYP-KNOT        PIC X                                        
025800                             VALUE SPACE.                                 
025900*                                 FAKTURATYP KREDITNOTA                   
026000     03 KDFRAKT              PIC Z9                                       
026100                             VALUE ZEROS.                                 
026200*                                 FRAKTSÄTT DC TILL KUND                  
026300*                                 FREIGHT CODE                            
026400     03 KDKREBEH             PIC X(3)                                     
026500                             VALUE SPACES.                                
026600*                                 BEHANDLINGSSTATUS                       
026700*                                 TREATMENT STATUS                        
026800     03 KDORDKL              PIC 9                                        
026900                             VALUE ZERO.                                  
027000*                                 ORDERKLASS                              
027100*                                 ORDER CLASS                             
027200     03 KVANTAL-ILI          PIC Z(5)9                                    
027300                             VALUE ZEROS.                                 
027400*                                 ANTAL PÅ INLÄGGNINGSLISTA               
027500*                                                                         
027600     03 KVAVV-KVAL           PIC Z(6)9                                    
027700                             VALUE ZEROS.                                 
027800*                                 ANTALSAVVIKELSE KVALITET                
027900*                                 QUANTITYDEVIATION QUALITY               
028000     03 KVAVV-KVANT          PIC Z(6)9                                    
028100                             VALUE ZEROS.                                 
028200*                                 ANTALSAVVIKELSE KVANTITET               
028300*                                 QUANTITYDEVIATION QUANTITY              
028400     03 KVLEVANM             PIC Z(5)9                                    
028500                             VALUE ZEROS.                                 
028600*                                 LEVERANSANMÄRKNINGSANTAL                
028700*                                 DISCREPANCY REPORT QTY                  
028800     03 KVLEVANM-BEKR        PIC Z(5)9                                    
028900                             VALUE ZEROS.                                 
029000*                                 BEKRÄFTAT RETURANTAL                    
029100     03 KVRETINL             PIC Z(5)9                                    
029200                             VALUE ZEROS.                                 
029300*                                 INLAGT ANTAL VID RETUR                  
029400*                                 RECEIVED QUANTITY ON RETURN             
029500     03 KVRETINL-SKR         PIC Z(5)9                                    
029600                             VALUE ZEROS.                                 
029700*                                 INRPT ANTAL SOM SKROTATS                
029800*                                 REPORTED QTY SCRAPPED                   
029900     03 PRARTBTO             PIC Z(6)9.9(2)                               
030000                             VALUE ZEROS.                                 
030100*                                 FÖRSÄLJNINGSPRIS BRUTTO (KR)            
030200*                                 GROSS SALES PRICE (SEK)                 
030300     03 PRARTBTO-LOC         PIC Z(6)9.9(2)                               
030400                             VALUE ZEROS.                                 
030500*                                 PRIS I LOKAL VALUTA                     
030600*                                 LOCAL GROSS SALES PRICE                 
030700     03 PRFRAKT-LEV          PIC Z(6)9.9(2)                               
030800                             VALUE ZEROS.                                 
030900*                                 FRAKTKOSTNAD                            
031000*                                 FREIGHT COST                            
031100     03 TIFAKT               PIC 9(6)                                     
031200                             VALUE ZEROS.                                 
031300*                                 FAKTURERINGSDATUM (ÅÅMMDD)              
031400*                                 INVOICING DATE   (YYMMDD)               
031500     03 TIFAKT-LOC           PIC 9(6)                                     
031600                             VALUE ZEROS.                                 
031700*                                 FAKTURADATUM LOKALT                     
031800*                                 LOCAL INVOICING DATE                    
031900     03 TIINLINL             PIC 9(6)                                     
032000                             VALUE ZEROS.                                 
032100*                                 RAPPORTERINGSDATUM INLAGD (R32)         
032200*                                 DATE OF REPORTED IN STOCK (R32)         
032300     03 TIKNOTA              PIC 9(6)                                     
032400                             VALUE ZEROS.                                 
032500*                                 KREDITNOTADATUM                         
032600*                                 DATE OF CREDIT NOTE                     
032700     03 DALEVANM-LEV         PIC 9(8)                                     
032800                             VALUE ZEROS.                                 
032900*                                 DATUM LEV.ANMÄRKNING(YYYYMMDD)          
033000*                                 DISCREPANCY REPORT DATE                 
033100     03 TIREMISS-IN          PIC 9(6)                                     
033200                             VALUE ZEROS.                                 
033300*                                 REMISSVARSDATUM                         
033400*                                 DATE OF CONSIDERATION                   
033500     03 TIREMISS-UT          PIC 9(6)                                     
033600                             VALUE ZEROS.                                 
033700*                                 DATUM FÖR REMISSFRÅGA                   
033800*                                 DATE OF CONSIDERATION ASKED             
033900     03 TIUTSKR              PIC 9(6)                                     
034000                             VALUE ZEROS.                                 
034100*                                 UTSKRIFTDATUM  (ÅÅMMDD)                 
034200*                                 PRINTING DATE  (YYMMDD)                 
034300     03 IDARTNR-DEL          PIC Z(7)9                                    
034400                             VALUE ZEROS.                                 
034500*                                 LEVERERAD ARTIKEL                       
034600*                                 DELIVERED PART NO                       
034700     03 TIUPPDAT-ILI         PIC 9(6)                                     
034800                             VALUE ZEROS.                                 
034900*                                 UPPD.DATUM PÅ INLÄGGNINGSLISTA          
035000     03 FLLSBOK              PIC X                                        
035100                             VALUE SPACE.                                 
035200*                                 LAGERAVBOKNING                          
035300*                                 STOCKUPDATING                           
035400     03 KDAVVTYP             PIC 9                                        
035500                             VALUE ZERO.                                  
035600*                                 AVVIKELSETYP                            
035700*                                 1=POSITIV.  2=NEGATIV                   
035800*                                 TYPE OF DISCREPANCY                     
035900*                                 1=POSITIVE. 2=NEGATIVE                  
036000     03 FLINVUPD             PIC X                                        
036100                             VALUE SPACE.                                 
036200*                                 INVENTERINGSUPPDAT                      
036300*                                 STOCKTAKING UPDATE                      
036400     03 FLPRQUES             PIC X                                        
036500                             VALUE SPACE.                                 
036600*                                 PRISSÄTTNINGSFLAGGA                     
036700*                                 PRICING  FLAG                           
036800     03 IDPRQUES             PIC Z(6)9                                    
036900                             VALUE ZEROS.                                 
037000*                                 PRISFRÅGA NR                            
037100*                                 PRICE QUESTION NO                       
037200     03 KDVAT                PIC X(2)                                     
037300                             VALUE SPACES.                                
037400*                                 MOMSKOD                                 
037500*                                 VAT CODE                                
037600     03 BEART-VIPS           PIC X(25)                                    
037700                             VALUE SPACES.                                
037800*                                 VIPS ARTIKELBENÄMNING                   
037900*                                 PÅ DEALERNS SPRÅK                       
038000     03 PRARTSTD             PIC Z(6)9.9(2)                               
038100                             VALUE ZEROS.                                 
038200*                                 ARTIKELSTANDARDPRIS                     
038300*                                 STANDARD PRICE                          
038400     03 PRARTSJK             PIC Z(6)9.9(2)                               
038500                             VALUE ZEROS.                                 
038600*                                 ARTIKELNS SJÄLVKOSTNAD                  
038700*                                 COST OF SALES                           
038800     03 PRARTBTO-LOCINV      PIC Z(6)9.9(2)                               
038900                             VALUE ZEROS.                                 
039000*                                 FÖRSÄLJNINGSPRIS LOKAL FAKTURA          
039100*                                 GROSS SALES PRICE LOCAL INVOICE         
039200     03 FLRETUR              PIC X                                        
039300                             VALUE SPACE.                                 
039400*                                 FLAGGA RETUR OK.                        
039500*                                 RETURN PART FLAG                        
039600     03 IDANSTNR-ILIU        PIC Z(4)9                                    
039700                             VALUE ZEROS.                                 
039800*                                 ANSTÄLLNINGSNUMMER I-LIST UPPD          
039900*                                 EMPLOYEE NUMBER REP-LIST UPDATE         
040000     03 TEANMNOT-REG         OCCURS 3 TIMES                               
040100                             PIC X(70)                                    
040200                             VALUE SPACES.                                
040300*                                 FRI TEXT FRÅN REGISTRERINGEN            
040400*                                 FREE TEXT FROM REGISTRATION             
040500     03 TEANMNOT-ADM         OCCURS 3 TIMES                               
040600                             PIC X(70)                                    
040700                             VALUE SPACES.                                
040800*                                 FRI TEXT FRÅN ADMINISTRATION            
040900*                                 FREE TEXT FROM ADMINISTRATION           
041000     03 TEANMNOT-REM         OCCURS 3 TIMES                               
041100                             PIC X(70)                                    
041200                             VALUE SPACES.                                
041300*                                 FRI TEXT FRÅN REMISSINSTANS             
041400*                                 TEXT FROM PERS. WHO CONSIDERED          
041500     03 TEANMNOT-RET         OCCURS 3 TIMES                               
041600                             PIC X(70)                                    
041700                             VALUE SPACES.                                
041800*                                 FRI TEXT FRÅN RETURAVDELNINGEN          
041900*                                 FREE TEXT FROM RETURNDEPARTMENT         
042000     03 TEANMNOT-DLR         OCCURS 3 TIMES                               
042100                             PIC X(70)                                    
042200                             VALUE SPACES.                                
042300*                                 FRI TEXT FRÅN ADM. TILL DEALER          
042400*                                 FREE TEXT FROM ADM. TO DEALER           
042500*** END OF VILMAII-COPY LENGTH= 1590 BYTES                                
