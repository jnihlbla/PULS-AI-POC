000100 01  W4184411.                                                            
000200*                                 RAD LEVERANSANMÄRKNING                  
000300     03 IDPTYP               PIC X(3).                                    
000400*                                 POSTTYP                                 
000500*                                 RECORD TYPE                             
000600     03 IDLEVANM.                                                         
000700*                                 LEVERANSANMÄRKNINGSIDENTITET            
000800*                                 DISCREPANCY REPORT IDENTITY             
000900        05 IDDISTR           PIC S9(5)           COMP-3.                  
001000*                                 DISTRIKTNUMMER                          
001100*                                 DISTRICT NUMBER                         
001200        05 IDKUNDNR          PIC S9(7)           COMP-3.                  
001300*                                 KUNDNUMMER                              
001400*                                 CUSTOMER NO                             
001500        05 IDRAPPNR          PIC 9(7).                                    
001600*                                 RAPPORT NUMMER                          
001700*                                 DISCREPANCY REPORT NUMBER               
001800     03 WDA211.                                                           
001900*                                 RAD LEVERANSANMÄRKNING                  
002000*                                 FYSISK NYCKEL: WDA211KY                 
002100*                                  (IDARTNR + IDRADNR)                    
002200        05 IDARTNR           PIC S9(9)           COMP-3.                  
002300*                                 ARTIKELNUMMER                           
002400*                                 PART NUMBER                             
002500        05 IDRADNR           PIC S9(5)           COMP-3.                  
002600*                                 RADNUMMER                               
002700*                                 LINE NO                                 
002800        05 ADGANG            PIC S9(3)           COMP-3.                  
002900*                                 GÅNG                                    
003000*                                 AISLE                                   
003100        05 ADLAGOMR          PIC S9(3)           COMP-3.                  
003200*                                 LAGEROMRÅDE                             
003300*                                 AREA                                    
003400        05 ADPLATS           PIC S9(5)           COMP-3.                  
003500*                                 LAGERPLATSNUMMER                        
003600*                                 LOCATION                                
003700        05 FLANLYSF          PIC X.                                       
003800*                                 FEL ANALYSNUMMER?                       
003900*                                 INCORRECT ANALYSISNUMBER?               
004000        05 FLANNULL          PIC X.                                       
004100*                                 ANNULLATION                             
004200*                                 CANCELLATION                            
004300        05 FLAUTKRE          PIC X.                                       
004400*                                 AUTOMATISK KREDITERING                  
004500*                                 AUTOMATIC DISCREPENCY                   
004600        05 FLDIRLEV          PIC X.                                       
004700*                                 DIREKTLEVERANS ?                        
004800*                                 DIRECT DELIVERY ?                       
004900        05 FLSKROT           PIC X.                                       
005000*                                 SKROTNINGSMARKERING                     
005100*                                 SCRAPPING FLAG                          
005200        05 FLSVAR            PIC X.                                       
005300*                                 ALLMÄN SVARSFLAGGA                      
005400*                                 GENERAL REPLY FLAG                      
005500        05 FLTEXT            PIC X.                                       
005600*                                 FINNS TEXTINFORMATION ?                 
005700        05 IDANALYS          PIC X(12).                                   
005800*                                 ANALYSNUMMER                            
005900*                                 ANALYSIS NUMBER                         
006000        05 IDANSTNR-RET      PIC S9(5)           COMP-3.                  
006100*                                 ANSTÄLLNINGSNUMMER RETURAVDELN.         
006200*                                 EMPLOYEE NUMBER RETURNDEPT.             
006300        05 IDDC              PIC X(2).                                    
006400*                                 IDENTIFIERARE LAGER                     
006500*                                 WAREHOUSE IDENTIFIER                    
006600        05 IDDC-RET          PIC X(2).                                    
006700*                                 MOTTAGANDE LAGER FÖR RETURER            
006800*                                 RECEIVING WAREHOUSE FOR RETURNS         
006900        05 IDFAKT            PIC S9(7)           COMP-3.                  
007000*                                 FAKTURANUMMER                           
007100*                                 INVOICE NO.                             
007200        05 IDFAKT-LOC        PIC S9(7)           COMP-3.                  
007300*                                 FAKTURANR LOKALT                        
007400*                                 LOCAL INVOICE NO                        
007500        05 IDFTG             PIC 9(2).                                    
007600*                                 FÖRETAGSID EKONOM REDOVISNING           
007700*                                 COMPANY IDENTITY ACCOUNTING             
007800        05 IDILIST           PIC 9(5).                                    
007900*                                 INLÄGGNINGSLISTEIDENTITET               
008000*                                 REPORTINGLIST-IDENTITY                  
008100        05 IDKNOTNR          PIC S9(7)           COMP-3.                  
008200*                                 KREDITNOTANUMMER                        
008300*                                 CREDIT NOTE NUMBER                      
008400        05 IDKOLLI           PIC S9(5)           COMP-3.                  
008500*                                 KOLLINUMMER                             
008600*                                 CASE NUMBER                             
008700        05 IDKONTO           PIC S9(11)          COMP-3.                  
008800*                                 KONTO                                   
008900*                                 ACCOUNT                                 
009000        05 IDKST             PIC X(10).                                   
009100*                                 KOSTNADSSTÄLLE                          
009200*                                 COST CENTRE                             
009300        05 IDKUNDRF-GRP.                                                  
009400*                                 KUNDENS REFERENS (ORDERID)              
009500*                                 CUSTOMER REFERENCE (ORDER ID)           
009600           07 IDKUNDRF       PIC X(10).                                   
009700*                                 KUNDENS REFERENS (ORDERID)              
009800*                                 CUSTOMER REFERENCE (ORDER ID)           
009900           07 IDORDNR5-FILLER REDEFINES IDKUNDRF.                         
010000              09 IDORDNR5    PIC 9(5).                                    
010100*                                 ORDERNUMMER                             
010200*                                 ORDER NUMBER                            
010300              09 FILLER      PIC X(5).                                    
010400           07 IDORDNR7-FILLER REDEFINES IDKUNDRF.                         
010500              09 IDORDNR7    PIC 9(7).                                    
010600*                                 ORDERNUMMER                             
010700*                                 ORDER NUMBER                            
010800              09 FILLER      PIC X(3).                                    
010900        05 IDLOPNRM          PIC S9(9)           COMP-3.                  
011000*                                 LÖPNUMMER MOTTAGNINGSRAPPORT            
011100*                                 (0VVDLLLLK)                             
011200*                                 SERIAL NO RECEIVING REPORT              
011300*                                 (0WWDLLLLC)                             
011400        05 IDPERSON          PIC S9(3)           COMP-3.                  
011500*                                 PERSONKOD                               
011600*                                 STAFF CODE                              
011700        05 IDPERSON-REM      PIC S9(3)           COMP-3.                  
011800*                                 PERSONKOD REMISS                        
011900*                                 STAFF CODE CONSIDERATION                
012000        05 IDUSER-PACK       PIC X(8).                                    
012100*                                 ANSVARIGT USERID PACKARE                
012200*                                 RESPONSIBLE USERID PACKER               
012300        05 KDANMORS          PIC X(2).                                    
012400*                                 ORSAK TILL LEVERANSANMÄRKNING           
012500*                                 DISCREPANCY REPORT REASON CODE          
012600        05 KDARBTYP          PIC X(8).                                    
012700*                                 TYP AV ARBETE                           
012800*                                 CATEGORY OF WORK                        
012900        05 KDARBTYP-REM      PIC X(8).                                    
013000*                                 REMISSANSVARIG LEVANM                   
013100*                                 PERSON WHO CONSIDERED DISCR.            
013200        05 KDEMBLEV          PIC S9              COMP-3.                  
013300*                                 EMBALLAGEKOD PÅ LEVERANSANMÄRKN         
013400*                                 PACK CODE DISCREP                       
013500        05 KDFAKTYP          PIC X.                                       
013600*                                 FAKTURATYP                              
013700*                                 INVOICE TYPE                            
013800        05 KDFAKTYP-KNOT     PIC X.                                       
013900*                                 FAKTURATYP KREDITNOTA                   
014000        05 KDFRAKT           PIC S9(3)           COMP-3.                  
014100*                                 FRAKTSÄTT DC TILL KUND                  
014200*                                 FREIGHT CODE                            
014300        05 KDKREBEH          PIC X(3).                                    
014400*                                 BEHANDLINGSSTATUS                       
014500*                                 TREATMENT STATUS                        
014600        05 KDORDKL           PIC S9              COMP-3.                  
014700*                                 ORDERKLASS                              
014800*                                 ORDER CLASS                             
014900        05 KVANTAL-ILI       PIC S9(7)           COMP-3.                  
015000*                                 ANTAL PÅ INLÄGGNINGSLISTA               
015100*                                                                         
015200        05 KVAVV-KVAL        PIC S9(7)           COMP-3.                  
015300*                                 ANTALSAVVIKELSE KVALITET                
015400*                                 QUANTITYDEVIATION QUALITY               
015500        05 KVAVV-KVANT       PIC S9(7)           COMP-3.                  
015600*                                 ANTALSAVVIKELSE KVANTITET               
015700*                                 QUANTITYDEVIATION QUANTITY              
015800        05 KVLEVANM          PIC S9(7)           COMP-3.                  
015900*                                 LEVERANSANMÄRKNINGSANTAL                
016000*                                 DISCREPANCY REPORT QTY                  
016100        05 KVLEVANM-BEKR     PIC S9(7)           COMP-3.                  
016200*                                 BEKRÄFTAT RETURANTAL                    
016300        05 KVRETINL          PIC S9(7)           COMP-3.                  
016400*                                 INLAGT ANTAL VID RETUR                  
016500*                                 RECEIVED QUANTITY ON RETURN             
016600        05 KVRETINL-SKR      PIC S9(7)           COMP-3.                  
016700*                                 INRPT ANTAL SOM SKROTATS                
016800*                                 REPORTED QTY SCRAPPED                   
016900        05 PRARTBTO          PIC S9(7)V9(2)      COMP-3.                  
017000*                                 FÖRSÄLJNINGSPRIS BRUTTO (KR)            
017100*                                 GROSS SALES PRICE (SEK)                 
017200        05 PRARTBTO-LOC      PIC S9(7)V9(2)      COMP-3.                  
017300*                                 PRIS I LOKAL VALUTA                     
017400*                                 LOCAL GROSS SALES PRICE                 
017500        05 PRFRAKT           PIC S9(7)V9(2)      COMP-3.                  
017600*                                 FRAKTKOSTNAD                            
017700*                                 FREIGHT COST                            
017800        05 TIFAKT            PIC S9(7)           COMP-3.                  
017900*                                 FAKTURERINGSDATUM (ÅÅMMDD)              
018000*                                 INVOICING DATE   (YYMMDD)               
018100        05 TIFAKT-LOC        PIC S9(7)           COMP-3.                  
018200*                                 FAKTURADATUM LOKALT                     
018300*                                 LOCAL INVOICING DATE                    
018400        05 TIINLINL          PIC S9(7)           COMP-3.                  
018500*                                 RAPPORTERINGSDATUM INLAGD (R32)         
018600*                                 DATE OF REPORTED IN STOCK (R32)         
018700        05 TIKNOTA           PIC S9(7)           COMP-3.                  
018800*                                 KREDITNOTADATUM                         
018900*                                 DATE OF CREDIT NOTE                     
019000        05 DALEVANM          PIC 9(8).                                    
019100*                                 DATUM LEV.ANMÄRKNING(YYYYMMDD)          
019200*                                 DISCREPANCY REPORT DATE                 
019300        05 TIREMISS-IN       PIC S9(7)           COMP-3.                  
019400*                                 REMISSVARSDATUM                         
019500*                                 DATE OF CONSIDERATION                   
019600        05 TIREMISS-UT       PIC S9(7)           COMP-3.                  
019700*                                 DATUM FÖR REMISSFRÅGA                   
019800*                                 DATE OF CONSIDERATION ASKED             
019900        05 TIUTSKR           PIC S9(7)           COMP-3.                  
020000*                                 UTSKRIFTDATUM  (ÅÅMMDD)                 
020100*                                 PRINTING DATE  (YYMMDD)                 
020200        05 IDARTNR-DEL       PIC S9(9)           COMP-3.                  
020300*                                 LEVERERAD ARTIKEL                       
020400*                                 DELIVERED PART NO                       
020500        05 TIUPPDAT-ILI      PIC S9(7)           COMP-3.                  
020600*                                 UPPD.DATUM PÅ INLÄGGNINGSLISTA          
020700        05 FLLSBOK           PIC X.                                       
020800*                                 LAGERAVBOKNING                          
020900*                                 STOCKUPDATING                           
021000        05 KDAVVTYP          PIC S9              COMP-3.                  
021100*                                 AVVIKELSETYP                            
021200*                                 1=POSITIV.  2=NEGATIV                   
021300*                                 TYPE OF DISCREPANCY                     
021400*                                 1=POSITIVE. 2=NEGATIVE                  
021500        05 FLINVUPD          PIC X.                                       
021600*                                 INVENTERINGSUPPDAT                      
021700*                                 STOCKTAKING UPDATE                      
021800        05 FLPRQUES          PIC X.                                       
021900*                                 PRISSÄTTNINGSFLAGGA                     
022000*                                 PRICING  FLAG                           
022100        05 IDPRQUES          PIC 9(7).                                    
022200*                                 PRISFRÅGA NR                            
022300*                                 PRICE QUESTION NO                       
022400        05 KDVAT             PIC X(2).                                    
022500*                                 MOMSKOD                                 
022600*                                 VAT CODE                                
022700        05 BEART-VIPS        PIC X(25).                                   
022800*                                 VIPS ARTIKELBENÄMNING                   
022900*                                 PÅ DEALERNS SPRÅK                       
023000        05 PRARTSTD          PIC S9(7)V9(2)      COMP-3.                  
023100*                                 ARTIKELSTANDARDPRIS                     
023200*                                 STANDARD PRICE                          
023300        05 PRARTSJK          PIC S9(7)V9(2)      COMP-3.                  
023400*                                 ARTIKELNS SJÄLVKOSTNAD                  
023500*                                 COST OF SALES                           
023600        05 PRARTBTO-LOCINV   PIC S9(7)V9(2)      COMP-3.                  
023700*                                 FÖRSÄLJNINGSPRIS LOKAL FAKTURA          
023800*                                 GROSS SALES PRICE LOCAL INVOICE         
023900        05 FLRETUR           PIC X.                                       
024000*                                 FLAGGA RETUR OK.                        
024100*                                 RETURN PART FLAG                        
024200        05 IDANSTNR-ILIU     PIC S9(5)           COMP-3.                  
024300*                                 ANSTÄLLNINGSNUMMER I-LIST UPPD          
024400*                                 EMPLOYEE NUMBER REP-LIST UPDATE         
024500        05 FILLER            PIC X(10).                                   
024600*** END OF VILMAII-COPY LENGTH= 305 BYTES                                 
