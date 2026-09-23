000100 01  W41842X1.                                                            
000200*                                 LEVERANSANMÄRKNING                      
000300     03 IDLEVANM.                                                         
000400*                                 LEVERANSANMÄRKNINGSIDENTITET            
000500*                                 DISCREPANCY REPORT IDENTITY             
000600        05 IDDISTR           PIC S9(5)           COMP-3.                  
000700*                                 DISTRIKTNUMMER                          
000800*                                 DISTRICT NUMBER                         
000900        05 IDKUNDNR          PIC S9(7)           COMP-3.                  
001000*                                 KUNDNUMMER                              
001100*                                 CUSTOMER NO                             
001200        05 IDRAPPNR          PIC 9(7).                                    
001300*                                 RAPPORT NUMMER                          
001400*                                 DISCREPANCY REPORT NUMBER               
001500     03 IDFTG-ANM            PIC 9(2).                                    
001600*                                 FÖRETAGSID EKONOM REDOVISNING           
001700*                                 COMPANY IDENTITY ACCOUNTING             
001800     03 IDPERSON-ANM         PIC S9(3)           COMP-3.                  
001900*                                 PERSONKOD                               
002000*                                 STAFF CODE                              
002100     03 IDUSER               PIC X(8).                                    
002200*                                 ANVÄNDARENS SÄKERHETS ID                
002300*                                 USER SECURITY-IDENTITY                  
002400     03 KDARBTYP-ANM         PIC X(8).                                    
002500*                                 TYP AV ARBETE                           
002600*                                 CATEGORY OF WORK                        
002700     03 KDLEVANM             PIC X.                                       
002800*                                 STATUS LEVERANSANMÄRKNING               
002900*                                 STATUS DISCREPANCY                      
003000     03 KVRADER-OBEH         PIC S9(5)           COMP-3.                  
003100*                                 ANTAL OBEHANDLADE RADER                 
003200*                                 NUMBER OF NOT TREATED LINES             
003300     03 KVRADER-RT           PIC S9(5)           COMP-3.                  
003400*                                 ANTAL RADER RETURTILLSTÅND              
003500*                                 NUMBER OF LINES RETURNPERMIT            
003600     03 PRFOERS              PIC S9(7)V9(2)      COMP-3.                  
003700*                                 FÖRSÄKRINGSPREMIE                       
003800*                                 INSURANCE FEE                           
003900     03 PRFRAKT-ANM          PIC S9(7)V9(2)      COMP-3.                  
004000*                                 FRAKTKOSTNAD                            
004100*                                 FREIGHT COST                            
004200     03 PRLEGKST             PIC S9(7)V9(2)      COMP-3.                  
004300*                                 LEGALISERINSKOSTNAD                     
004400*                                 LEGALIZATION FEE                        
004500     03 REEMBHNT             PIC S9(2)V9(1)      COMP-3.                  
004600*                                 EMB OCH HANTERINGSKOST (%)              
004700*                                 PACKING AND HANDLING (%)                
004800     03 RELANDCO             PIC S9(3)V9(2)      COMP-3.                  
004900*                                 LANDING COST PROCENT                    
005000*                                 LANDING COST PERCENT                    
005100     03 DALEVANM-ANM         PIC 9(8).                                    
005200*                                 DATUM LEV.ANMÄRKNING(YYYYMMDD)          
005300*                                 DISCREPANCY REPORT DATE                 
005400     03 DARETANK             PIC 9(8).                                    
005500*                                 ANKOMSTDATUM (ÅÅÅÅMMDD)                 
005600*                                 DATE GOODS RECEIVING(YYYYMMDD)          
005700     03 DARETILL             PIC 9(8).                                    
005800*                                 RETURTILLSTÅNDSDATUM (AAAAMMDD)         
005900*                                 DATE RETURNPERMIT    (YYYYMMDD)         
006000     03 FLFARLIG             PIC X.                                       
006100*                                 FARLIGT GODS-FLAGGA                     
006200*                                 DENGEROUS GOODS FLAG                    
006300     03 DARTPMN              PIC 9(8).                                    
006400*                                 PÅMINNELSE RETURTILLSTÅNDSDAT.          
006500*                                 (YYYYMMDD)                              
006600*                                 DATE RETURNPERMIT REMINDER              
006700     03 KDLEVANM-UPD         PIC X.                                       
006800*                                 STATUS LEVERANSANMÄRKNING               
006900*                                 STATUS DISCREPANCY                      
007000     03 KDVALISO             PIC X(3).                                    
007100*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
007200*                                 CURRENCY CODE BY ISO-STANDARD.          
007300     03 BEANST               PIC X(25).                                   
007400*                                 ANSTÄLLDS NAMN                          
007500*                                 NAME OF EMPLOYED                        
007600     03 IDUSER-ADM           PIC X(8).                                    
007700*                                 ANVÄNDAR-ID ADMINISTRATIV KONTR         
007800*                                 USER ID ADMINISTRATIVE INSPEC.          
007900     03 KDLEVATT             PIC 9.                                       
008000*                                 ATTESTERING KOD LEVERANSANM.            
008100*                                 ATTEST CODE DISCREPANCY REPORT          
008200     03 IDDC-RET-ANM         PIC X(2).                                    
008300*                                 MOTTAGANDE LAGER FÖR RETURER            
008400*                                 RECEIVING WAREHOUSE FOR RETURNS         
008500     03 IXDCCLEAR            PIC 9.                                       
008600*                                 CLEARING DC SEKVENS                     
008700*                                 POSITION FOR CLEARING DC                
008800     03 IDSYSTEM             PIC X(4).                                    
008900*                                 VOLVO VCCS SYSTEMNUMMER                 
009000*                                 VOLVO VCCS SYSTEM NUMBER                
009100     03 IDARTNR              PIC S9(9)           COMP-3.                  
009200*                                 ARTIKELNUMMER                           
009300*                                 PART NUMBER                             
009400     03 IDRADNR              PIC S9(5)           COMP-3.                  
009500*                                 RADNUMMER                               
009600*                                 LINE NO                                 
009700     03 ADGANG               PIC S9(3)           COMP-3.                  
009800*                                 GÅNG                                    
009900*                                 AISLE                                   
010000     03 ADLAGOMR             PIC S9(3)           COMP-3.                  
010100*                                 LAGEROMRÅDE                             
010200*                                 AREA                                    
010300     03 ADPLATS              PIC S9(5)           COMP-3.                  
010400*                                 LAGERPLATSNUMMER                        
010500*                                 LOCATION                                
010600     03 FLANLYSF             PIC X.                                       
010700*                                 FEL ANALYSNUMMER?                       
010800*                                 INCORRECT ANALYSISNUMBER?               
010900     03 FLANNULL             PIC X.                                       
011000*                                 ANNULLATION                             
011100*                                 CANCELLATION                            
011200     03 FLAUTKRE             PIC X.                                       
011300*                                 AUTOMATISK KREDITERING                  
011400*                                 AUTOMATIC DISCREPENCY                   
011500     03 FLDIRLEV             PIC X.                                       
011600*                                 DIREKTLEVERANS ?                        
011700*                                 DIRECT DELIVERY ?                       
011800     03 FLSKROT              PIC X.                                       
011900*                                 SKROTNINGSMARKERING                     
012000*                                 SCRAPPING FLAG                          
012100     03 FLSVAR               PIC X.                                       
012200*                                 ALLMÄN SVARSFLAGGA                      
012300*                                 GENERAL REPLY FLAG                      
012400     03 FLTEXT               PIC X.                                       
012500*                                 FINNS TEXTINFORMATION ?                 
012600     03 IDANALYS             PIC X(12).                                   
012700*                                 ANALYSNUMMER                            
012800*                                 ANALYSIS NUMBER                         
012900     03 IDANSTNR-RET         PIC S9(5)           COMP-3.                  
013000*                                 ANSTÄLLNINGSNUMMER RETURAVDELN.         
013100*                                 EMPLOYEE NUMBER RETURNDEPT.             
013200     03 IDDC                 PIC X(2).                                    
013300*                                 IDENTIFIERARE LAGER                     
013400*                                 WAREHOUSE IDENTIFIER                    
013500     03 IDDC-RET-LEV         PIC X(2).                                    
013600*                                 MOTTAGANDE LAGER FÖR RETURER            
013700*                                 RECEIVING WAREHOUSE FOR RETURNS         
013800     03 IDFAKT               PIC S9(7)           COMP-3.                  
013900*                                 FAKTURANUMMER                           
014000*                                 INVOICE NO.                             
014100     03 IDFAKT-LOC           PIC S9(7)           COMP-3.                  
014200*                                 FAKTURANR LOKALT                        
014300*                                 LOCAL INVOICE NO                        
014400     03 IDFTG-LEV            PIC 9(2).                                    
014500*                                 FÖRETAGSID EKONOM REDOVISNING           
014600*                                 COMPANY IDENTITY ACCOUNTING             
014700     03 IDILIST              PIC 9(5).                                    
014800*                                 INLÄGGNINGSLISTEIDENTITET               
014900*                                 REPORTINGLIST-IDENTITY                  
015000     03 IDKNOTNR             PIC S9(7)           COMP-3.                  
015100*                                 KREDITNOTANUMMER                        
015200*                                 CREDIT NOTE NUMBER                      
015300     03 IDKOLLI              PIC S9(5)           COMP-3.                  
015400*                                 KOLLINUMMER                             
015500*                                 CASE NUMBER                             
015600     03 IDKONTO              PIC S9(11)          COMP-3.                  
015700*                                 KONTO                                   
015800*                                 ACCOUNT                                 
015900     03 IDKST                PIC X(10).                                   
016000*                                 KOSTNADSSTÄLLE                          
016100*                                 COST CENTRE                             
016200     03 IDKUNDRF             PIC X(10).                                   
016300*                                 KUNDENS REFERENS (ORDERID)              
016400*                                 CUSTOMER REFERENCE (ORDER ID)           
016500     03 IDLOPNRM             PIC S9(9)           COMP-3.                  
016600*                                 LÖPNUMMER MOTTAGNINGSRAPPORT            
016700*                                 (0VVDLLLLK)                             
016800*                                 SERIAL NO RECEIVING REPORT              
016900*                                 (0WWDLLLLC)                             
017000     03 IDPERSON-LEV         PIC S9(3)           COMP-3.                  
017100*                                 PERSONKOD                               
017200*                                 STAFF CODE                              
017300     03 IDPERSON-REM         PIC S9(3)           COMP-3.                  
017400*                                 PERSONKOD REMISS                        
017500*                                 STAFF CODE CONSIDERATION                
017600     03 IDUSER-PACK          PIC X(8).                                    
017700*                                 ANSVARIGT USERID PACKARE                
017800*                                 RESPONSIBLE USERID PACKER               
017900     03 KDANMORS             PIC X(2).                                    
018000*                                 ORSAK TILL LEVERANSANMÄRKNING           
018100*                                 DISCREPANCY REPORT REASON CODE          
018200     03 KDARBTYP-LEV         PIC X(8).                                    
018300*                                 TYP AV ARBETE                           
018400*                                 CATEGORY OF WORK                        
018500     03 KDARBTYP-REM         PIC X(8).                                    
018600*                                 REMISSANSVARIG LEVANM                   
018700*                                 PERSON WHO CONSIDERED DISCR.            
018800     03 KDEMBLEV             PIC S9              COMP-3.                  
018900*                                 EMBALLAGEKOD PÅ LEVERANSANMÄRKN         
019000*                                 PACK CODE DISCREP                       
019100     03 KDFAKTYP             PIC X.                                       
019200*                                 FAKTURATYP                              
019300*                                 INVOICE TYPE                            
019400     03 KDFAKTYP-KNOT        PIC X.                                       
019500*                                 FAKTURATYP KREDITNOTA                   
019600     03 KDFRAKT              PIC S9(3)           COMP-3.                  
019700*                                 FRAKTSÄTT DC TILL KUND                  
019800*                                 FREIGHT CODE                            
019900     03 KDKREBEH             PIC X(3).                                    
020000*                                 BEHANDLINGSSTATUS                       
020100*                                 TREATMENT STATUS                        
020200     03 KDORDKL              PIC S9              COMP-3.                  
020300*                                 ORDERKLASS                              
020400*                                 ORDER CLASS                             
020500     03 KVANTAL-ILI          PIC S9(7)           COMP-3.                  
020600*                                 ANTAL PÅ INLÄGGNINGSLISTA               
020700*                                                                         
020800     03 KVAVV-KVAL           PIC S9(7)           COMP-3.                  
020900*                                 ANTALSAVVIKELSE KVALITET                
021000*                                 QUANTITYDEVIATION QUALITY               
021100     03 KVAVV-KVANT          PIC S9(7)           COMP-3.                  
021200*                                 ANTALSAVVIKELSE KVANTITET               
021300*                                 QUANTITYDEVIATION QUANTITY              
021400     03 KVLEVANM             PIC S9(7)           COMP-3.                  
021500*                                 LEVERANSANMÄRKNINGSANTAL                
021600*                                 DISCREPANCY REPORT QTY                  
021700     03 KVLEVANM-BEKR        PIC S9(7)           COMP-3.                  
021800*                                 BEKRÄFTAT RETURANTAL                    
021900     03 KVRETINL             PIC S9(7)           COMP-3.                  
022000*                                 INLAGT ANTAL VID RETUR                  
022100*                                 RECEIVED QUANTITY ON RETURN             
022200     03 KVRETINL-SKR         PIC S9(7)           COMP-3.                  
022300*                                 INRPT ANTAL SOM SKROTATS                
022400*                                 REPORTED QTY SCRAPPED                   
022500     03 PRARTBTO             PIC S9(7)V9(2)      COMP-3.                  
022600*                                 FÖRSÄLJNINGSPRIS BRUTTO (KR)            
022700*                                 GROSS SALES PRICE (SEK)                 
022800     03 PRARTBTO-LOC         PIC S9(7)V9(2)      COMP-3.                  
022900*                                 PRIS I LOKAL VALUTA                     
023000*                                 LOCAL GROSS SALES PRICE                 
023100     03 PRFRAKT-LEV          PIC S9(7)V9(2)      COMP-3.                  
023200*                                 FRAKTKOSTNAD                            
023300*                                 FREIGHT COST                            
023400     03 TIFAKT               PIC S9(7)           COMP-3.                  
023500*                                 FAKTURERINGSDATUM (ÅÅMMDD)              
023600*                                 INVOICING DATE   (YYMMDD)               
023700     03 TIFAKT-LOC           PIC S9(7)           COMP-3.                  
023800*                                 FAKTURADATUM LOKALT                     
023900*                                 LOCAL INVOICING DATE                    
024000     03 TIINLINL             PIC S9(7)           COMP-3.                  
024100*                                 RAPPORTERINGSDATUM INLAGD (R32)         
024200*                                 DATE OF REPORTED IN STOCK (R32)         
024300     03 TIKNOTA              PIC S9(7)           COMP-3.                  
024400*                                 KREDITNOTADATUM                         
024500*                                 DATE OF CREDIT NOTE                     
024600     03 DALEVANM-LEV         PIC 9(8).                                    
024700*                                 DATUM LEV.ANMÄRKNING(YYYYMMDD)          
024800*                                 DISCREPANCY REPORT DATE                 
024900     03 TIREMISS-IN          PIC S9(7)           COMP-3.                  
025000*                                 REMISSVARSDATUM                         
025100*                                 DATE OF CONSIDERATION                   
025200     03 TIREMISS-UT          PIC S9(7)           COMP-3.                  
025300*                                 DATUM FÖR REMISSFRÅGA                   
025400*                                 DATE OF CONSIDERATION ASKED             
025500     03 TIUTSKR              PIC S9(7)           COMP-3.                  
025600*                                 UTSKRIFTDATUM  (ÅÅMMDD)                 
025700*                                 PRINTING DATE  (YYMMDD)                 
025800     03 IDARTNR-DEL          PIC S9(9)           COMP-3.                  
025900*                                 LEVERERAD ARTIKEL                       
026000*                                 DELIVERED PART NO                       
026100     03 TIUPPDAT-ILI         PIC S9(7)           COMP-3.                  
026200*                                 UPPD.DATUM PÅ INLÄGGNINGSLISTA          
026300     03 FLLSBOK              PIC X.                                       
026400*                                 LAGERAVBOKNING                          
026500*                                 STOCKUPDATING                           
026600     03 KDAVVTYP             PIC S9              COMP-3.                  
026700*                                 AVVIKELSETYP                            
026800*                                 1=POSITIV.  2=NEGATIV                   
026900*                                 TYPE OF DISCREPANCY                     
027000*                                 1=POSITIVE. 2=NEGATIVE                  
027100     03 FLINVUPD             PIC X.                                       
027200*                                 INVENTERINGSUPPDAT                      
027300*                                 STOCKTAKING UPDATE                      
027400     03 FLPRQUES             PIC X.                                       
027500*                                 PRISSÄTTNINGSFLAGGA                     
027600*                                 PRICING  FLAG                           
027700     03 IDPRQUES             PIC 9(7).                                    
027800*                                 PRISFRÅGA NR                            
027900*                                 PRICE QUESTION NO                       
028000     03 KDVAT                PIC X(2).                                    
028100*                                 MOMSKOD                                 
028200*                                 VAT CODE                                
028300     03 BEART-VIPS           PIC X(25).                                   
028400*                                 VIPS ARTIKELBENÄMNING                   
028500*                                 PÅ DEALERNS SPRÅK                       
028600     03 PRARTSTD             PIC S9(7)V9(2)      COMP-3.                  
028700*                                 ARTIKELSTANDARDPRIS                     
028800*                                 STANDARD PRICE                          
028900     03 PRARTSJK             PIC S9(7)V9(2)      COMP-3.                  
029000*                                 ARTIKELNS SJÄLVKOSTNAD                  
029100*                                 COST OF SALES                           
029200     03 PRARTBTO-LOCINV      PIC S9(7)V9(2)      COMP-3.                  
029300*                                 FÖRSÄLJNINGSPRIS LOKAL FAKTURA          
029400*                                 GROSS SALES PRICE LOCAL INVOICE         
029500     03 FLRETUR              PIC X.                                       
029600*                                 FLAGGA RETUR OK.                        
029700*                                 RETURN PART FLAG                        
029800     03 IDANSTNR-ILIU        PIC S9(5)           COMP-3.                  
029900*                                 ANSTÄLLNINGSNUMMER I-LIST UPPD          
030000*                                 EMPLOYEE NUMBER REP-LIST UPDATE         
030100     03 TEANMNOT-REG         OCCURS 3 TIMES                               
030200                             PIC X(70).                                   
030300*                                 FRI TEXT FRÅN REGISTRERINGEN            
030400*                                 FREE TEXT FROM REGISTRATION             
030500     03 TEANMNOT-ADM         OCCURS 3 TIMES                               
030600                             PIC X(70).                                   
030700*                                 FRI TEXT FRÅN ADMINISTRATION            
030800*                                 FREE TEXT FROM ADMINISTRATION           
030900     03 TEANMNOT-REM         OCCURS 3 TIMES                               
031000                             PIC X(70).                                   
031100*                                 FRI TEXT FRÅN REMISSINSTANS             
031200*                                 TEXT FROM PERS. WHO CONSIDERED          
031300     03 TEANMNOT-RET         OCCURS 3 TIMES                               
031400                             PIC X(70).                                   
031500*                                 FRI TEXT FRÅN RETURAVDELNINGEN          
031600*                                 FREE TEXT FROM RETURNDEPARTMENT         
031700     03 TEANMNOT-DLR         OCCURS 3 TIMES                               
031800                             PIC X(70).                                   
031900*                                 FRI TEXT FRÅN ADM. TILL DEALER          
032000*                                 FREE TEXT FROM ADM. TO DEALER           
032100*** END OF VILMAII-COPY LENGTH= 1467 BYTES                                
