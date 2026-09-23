000100 01  LEV-WDA211.                                                          
000200*                                 RAD LEVERANSANMÄRKNING                  
000300*                                 FYSISK NYCKEL: WDA211KY                 
000400*                                  (IDARTNR + IDRADNR)                    
000500     03 LEV-IDARTNR          PIC S9(9)           COMP-3.                  
000600*                                 ARTIKELNUMMER                           
000700*                                 PART NUMBER                             
000800     03 LEV-IDRADNR          PIC S9(5)           COMP-3.                  
000900*                                 RADNUMMER                               
001000*                                 LINE NO                                 
001100     03 LEV-ADGANG           PIC S9(3)           COMP-3.                  
001200*                                 GÅNG                                    
001300*                                 AISLE                                   
001400     03 LEV-ADLAGOMR         PIC S9(3)           COMP-3.                  
001500*                                 LAGEROMRÅDE                             
001600*                                 AREA                                    
001700     03 LEV-ADPLATS          PIC S9(5)           COMP-3.                  
001800*                                 LAGERPLATSNUMMER                        
001900*                                 LOCATION                                
002000     03 LEV-FLANLYSF         PIC X.                                       
002100*                                 FEL ANALYSNUMMER?                       
002200*                                 INCORRECT ANALYSISNUMBER?               
002300     03 LEV-FLANNULL         PIC X.                                       
002400*                                 ANNULLATION                             
002500*                                 CANCELLATION                            
002600     03 LEV-FLAUTKRE         PIC X.                                       
002700*                                 AUTOMATISK KREDITERING                  
002800*                                 AUTOMATIC DISCREPENCY                   
002900     03 LEV-FLDIRLEV         PIC X.                                       
003000*                                 DIREKTLEVERANS ?                        
003100*                                 DIRECT DELIVERY ?                       
003200     03 LEV-FLSKROT          PIC X.                                       
003300*                                 SKROTNINGSMARKERING                     
003400*                                 SCRAPPING FLAG                          
003500     03 LEV-FLSVAR           PIC X.                                       
003600*                                 ALLMÄN SVARSFLAGGA                      
003700*                                 GENERAL REPLY FLAG                      
003800     03 LEV-FLTEXT           PIC X.                                       
003900*                                 FINNS TEXTINFORMATION ?                 
004000     03 LEV-IDANALYS         PIC X(12).                                   
004100*                                 ANALYSNUMMER                            
004200*                                 ANALYSIS NUMBER                         
004300     03 LEV-IDANSTNR-RET     PIC S9(5)           COMP-3.                  
004400*                                 ANSTÄLLNINGSNUMMER RETURAVDELN.         
004500*                                 EMPLOYEE NUMBER RETURNDEPT.             
004600     03 LEV-IDDC             PIC X(2).                                    
004700*                                 IDENTIFIERARE LAGER                     
004800*                                 WAREHOUSE IDENTIFIER                    
004900     03 LEV-IDDC-RET         PIC X(2).                                    
005000*                                 MOTTAGANDE LAGER FÖR RETURER            
005100*                                 RECEIVING WAREHOUSE FOR RETURNS         
005200     03 LEV-IDFAKT           PIC S9(7)           COMP-3.                  
005300*                                 FAKTURANUMMER                           
005400*                                 INVOICE NO.                             
005500     03 LEV-IDFAKT-LOC       PIC S9(7)           COMP-3.                  
005600*                                 FAKTURANR LOKALT                        
005700*                                 LOCAL INVOICE NO                        
005800     03 LEV-IDFTG            PIC 9(2).                                    
005900*                                 FÖRETAGSID EKONOM REDOVISNING           
006000*                                 COMPANY IDENTITY ACCOUNTING             
006100     03 LEV-IDILIST          PIC 9(5).                                    
006200*                                 INLÄGGNINGSLISTEIDENTITET               
006300*                                 REPORTINGLIST-IDENTITY                  
006400     03 LEV-IDKNOTNR         PIC S9(7)           COMP-3.                  
006500*                                 KREDITNOTANUMMER                        
006600*                                 CREDIT NOTE NUMBER                      
006700     03 LEV-IDKOLLI          PIC S9(5)           COMP-3.                  
006800*                                 KOLLINUMMER                             
006900*                                 CASE NUMBER                             
007000     03 LEV-IDKONTO          PIC S9(11)          COMP-3.                  
007100*                                 KONTO                                   
007200*                                 ACCOUNT                                 
007300     03 LEV-IDKST            PIC X(10).                                   
007400*                                 KOSTNADSSTÄLLE                          
007500*                                 COST CENTRE                             
007600     03 LEV-IDKUNDRF-GRP.                                                 
007700*                                 KUNDENS REFERENS (ORDERID)              
007800*                                 CUSTOMER REFERENCE (ORDER ID)           
007900        05 LEV-IDKUNDRF      PIC X(10).                                   
008000*                                 KUNDENS REFERENS (ORDERID)              
008100*                                 CUSTOMER REFERENCE (ORDER ID)           
008200        05 LEV-IDORDNR5-FILLER REDEFINES LEV-IDKUNDRF.                    
008300           07 LEV-IDORDNR5   PIC 9(5).                                    
008400*                                 ORDERNUMMER                             
008500*                                 ORDER NUMBER                            
008600           07 FILLER         PIC X(5).                                    
008700        05 LEV-IDORDNR7-FILLER REDEFINES LEV-IDKUNDRF.                    
008800           07 LEV-IDORDNR7   PIC 9(7).                                    
008900*                                 ORDERNUMMER                             
009000*                                 ORDER NUMBER                            
009100           07 FILLER         PIC X(3).                                    
009200     03 LEV-IDLOPNRM         PIC S9(9)           COMP-3.                  
009300*                                 LÖPNUMMER MOTTAGNINGSRAPPORT            
009400*                                 (0VVDLLLLK)                             
009500*                                 SERIAL NO RECEIVING REPORT              
009600*                                 (0WWDLLLLC)                             
009700     03 LEV-IDPERSON         PIC S9(3)           COMP-3.                  
009800*                                 PERSONKOD                               
009900*                                 STAFF CODE                              
010000     03 LEV-IDPERSON-REM     PIC S9(3)           COMP-3.                  
010100*                                 PERSONKOD REMISS                        
010200*                                 STAFF CODE CONSIDERATION                
010300     03 LEV-IDUSER-PACK      PIC X(8).                                    
010400*                                 ANSVARIGT USERID PACKARE                
010500*                                 RESPONSIBLE USERID PACKER               
010600     03 LEV-KDANMORS         PIC X(2).                                    
010700*                                 ORSAK TILL LEVERANSANMÄRKNING           
010800*                                 DISCREPANCY REPORT REASON CODE          
010900     03 LEV-KDARBTYP         PIC X(8).                                    
011000*                                 TYP AV ARBETE                           
011100*                                 CATEGORY OF WORK                        
011200     03 LEV-KDARBTYP-REM     PIC X(8).                                    
011300*                                 REMISSANSVARIG LEVANM                   
011400*                                 PERSON WHO CONSIDERED DISCR.            
011500     03 LEV-KDEMBLEV         PIC S9              COMP-3.                  
011600*                                 EMBALLAGEKOD PÅ LEVERANSANMÄRKN         
011700*                                 PACK CODE DISCREP                       
011800     03 LEV-KDFAKTYP         PIC X.                                       
011900*                                 FAKTURATYP                              
012000*                                 INVOICE TYPE                            
012100     03 LEV-KDFAKTYP-KNOT    PIC X.                                       
012200*                                 FAKTURATYP KREDITNOTA                   
012300     03 LEV-KDFRAKT          PIC S9(3)           COMP-3.                  
012400*                                 FRAKTSÄTT DC TILL KUND                  
012500*                                 FREIGHT CODE                            
012600     03 LEV-KDKREBEH         PIC X(3).                                    
012700*                                 BEHANDLINGSSTATUS                       
012800*                                 TREATMENT STATUS                        
012900     03 LEV-KDORDKL          PIC S9              COMP-3.                  
013000*                                 ORDERKLASS                              
013100*                                 ORDER CLASS                             
013200     03 LEV-KVANTAL-ILI      PIC S9(7)           COMP-3.                  
013300*                                 ANTAL PÅ INLÄGGNINGSLISTA               
013400*                                                                         
013500     03 LEV-KVAVV-KVAL       PIC S9(7)           COMP-3.                  
013600*                                 ANTALSAVVIKELSE KVALITET                
013700*                                 QUANTITYDEVIATION QUALITY               
013800     03 LEV-KVAVV-KVANT      PIC S9(7)           COMP-3.                  
013900*                                 ANTALSAVVIKELSE KVANTITET               
014000*                                 QUANTITYDEVIATION QUANTITY              
014100     03 LEV-KVLEVANM         PIC S9(7)           COMP-3.                  
014200*                                 LEVERANSANMÄRKNINGSANTAL                
014300*                                 DISCREPANCY REPORT QTY                  
014400     03 LEV-KVLEVANM-BEKR    PIC S9(7)           COMP-3.                  
014500*                                 BEKRÄFTAT RETURANTAL                    
014600     03 LEV-KVRETINL         PIC S9(7)           COMP-3.                  
014700*                                 INLAGT ANTAL VID RETUR                  
014800*                                 RECEIVED QUANTITY ON RETURN             
014900     03 LEV-KVRETINL-SKR     PIC S9(7)           COMP-3.                  
015000*                                 INRPT ANTAL SOM SKROTATS                
015100*                                 REPORTED QTY SCRAPPED                   
015200     03 LEV-PRARTBTO         PIC S9(7)V9(2)      COMP-3.                  
015300*                                 FÖRSÄLJNINGSPRIS BRUTTO (KR)            
015400*                                 GROSS SALES PRICE (SEK)                 
015500     03 LEV-PRARTBTO-LOC     PIC S9(7)V9(2)      COMP-3.                  
015600*                                 PRIS I LOKAL VALUTA                     
015700*                                 LOCAL GROSS SALES PRICE                 
015800     03 LEV-PRFRAKT          PIC S9(7)V9(2)      COMP-3.                  
015900*                                 FRAKTKOSTNAD                            
016000*                                 FREIGHT COST                            
016100     03 LEV-TIFAKT           PIC S9(7)           COMP-3.                  
016200*                                 FAKTURERINGSDATUM (ÅÅMMDD)              
016300*                                 INVOICING DATE   (YYMMDD)               
016400     03 LEV-TIFAKT-LOC       PIC S9(7)           COMP-3.                  
016500*                                 FAKTURADATUM LOKALT                     
016600*                                 LOCAL INVOICING DATE                    
016700     03 LEV-TIINLINL         PIC S9(7)           COMP-3.                  
016800*                                 RAPPORTERINGSDATUM INLAGD (R32)         
016900*                                 DATE OF REPORTED IN STOCK (R32)         
017000     03 LEV-TIKNOTA          PIC S9(7)           COMP-3.                  
017100*                                 KREDITNOTADATUM                         
017200*                                 DATE OF CREDIT NOTE                     
017300     03 LEV-DALEVANM         PIC 9(8).                                    
017400*                                 DATUM LEV.ANMÄRKNING(YYYYMMDD)          
017500*                                 DISCREPANCY REPORT DATE                 
017600     03 LEV-TIREMISS-IN      PIC S9(7)           COMP-3.                  
017700*                                 REMISSVARSDATUM                         
017800*                                 DATE OF CONSIDERATION                   
017900     03 LEV-TIREMISS-UT      PIC S9(7)           COMP-3.                  
018000*                                 DATUM FÖR REMISSFRÅGA                   
018100*                                 DATE OF CONSIDERATION ASKED             
018200     03 LEV-TIUTSKR          PIC S9(7)           COMP-3.                  
018300*                                 UTSKRIFTDATUM  (ÅÅMMDD)                 
018400*                                 PRINTING DATE  (YYMMDD)                 
018500     03 LEV-IDARTNR-DEL      PIC S9(9)           COMP-3.                  
018600*                                 LEVERERAD ARTIKEL                       
018700*                                 DELIVERED PART NO                       
018800     03 LEV-TIUPPDAT-ILI     PIC S9(7)           COMP-3.                  
018900*                                 UPPD.DATUM PÅ INLÄGGNINGSLISTA          
019000     03 LEV-FLLSBOK          PIC X.                                       
019100*                                 LAGERAVBOKNING                          
019200*                                 STOCKUPDATING                           
019300     03 LEV-KDAVVTYP         PIC S9              COMP-3.                  
019400*                                 AVVIKELSETYP                            
019500*                                 1=POSITIV.  2=NEGATIV                   
019600*                                 TYPE OF DISCREPANCY                     
019700*                                 1=POSITIVE. 2=NEGATIVE                  
019800     03 LEV-FLINVUPD         PIC X.                                       
019900*                                 INVENTERINGSUPPDAT                      
020000*                                 STOCKTAKING UPDATE                      
020100     03 LEV-FLPRQUES         PIC X.                                       
020200*                                 PRISSÄTTNINGSFLAGGA                     
020300*                                 PRICING  FLAG                           
020400     03 LEV-IDPRQUES         PIC 9(7).                                    
020500*                                 PRISFRÅGA NR                            
020600*                                 PRICE QUESTION NO                       
020700     03 LEV-KDVAT            PIC X(2).                                    
020800*                                 MOMSKOD                                 
020900*                                 VAT CODE                                
021000     03 LEV-BEART-VIPS       PIC X(25).                                   
021100*                                 VIPS ARTIKELBENÄMNING                   
021200*                                 PÅ DEALERNS SPRÅK                       
021300     03 LEV-PRARTSTD         PIC S9(7)V9(2)      COMP-3.                  
021400*                                 ARTIKELSTANDARDPRIS                     
021500*                                 STANDARD PRICE                          
021600     03 LEV-PRARTSJK         PIC S9(7)V9(2)      COMP-3.                  
021700*                                 ARTIKELNS SJÄLVKOSTNAD                  
021800*                                 COST OF SALES                           
021900     03 LEV-PRARTBTO-LOCINV  PIC S9(7)V9(2)      COMP-3.                  
022000*                                 FÖRSÄLJNINGSPRIS LOKAL FAKTURA          
022100*                                 GROSS SALES PRICE LOCAL INVOICE         
022200     03 LEV-FLRETUR          PIC X.                                       
022300*                                 FLAGGA RETUR OK.                        
022400*                                 RETURN PART FLAG                        
022500     03 LEV-IDANSTNR-ILIU    PIC S9(5)           COMP-3.                  
022600*                                 ANSTÄLLNINGSNUMMER I-LIST UPPD          
022700*                                 EMPLOYEE NUMBER REP-LIST UPDATE         
022800     03 LEV-FILLER           PIC X(10).                                   
022900*** END OF VILMAII-COPY LENGTH= 288 BYTES                                 
