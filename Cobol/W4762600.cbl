000200                                                                          
000300 ID DIVISION.                                                             
000400 PROGRAM-ID.     W4762600.                                                
000500 AUTHOR.         MOGREN STINA.                                            
000600 DATE-WRITTEN.   02/06/28.                                                
000700 DATE-COMPILED.                                                           
000800                                                                          
000900*    FUNKTION:                                                            
001000*        SKAPAR FIL FÖR EDI-ÖVERFÖRING AV KOLLIDATA TILL DANZAS,          
001100*        NORGE (DANZAS)                                                   
001200*                                                                         
001201*        RUTIN W476D3                                                     
001202*                                                                         
001210*        PROGRAMMET DELVIS KOPIERAT FRÅN W4755200                         
001220*                                                                         
001230*        PROGRAMMET LÄSER      W47623-FIL                                 
001300*        PROGRAMMET LÄSER      WDE6                                       
001400*        PROGRAMMET LÄSER      WDB2                                       
001500*        PROGRAMMET LÄSER      WDGX4738 (WDR1)                            
001600*                                                                         
001700*    ABENDKODER:                                                          
001800*        U0016 -  . . . .                                                 
001900*        U1000 -  . . . .                                                 
002000*                                                                         
002100                                                                          
002200     SKIP3                                                                
002300 ENVIRONMENT DIVISION.                                                    
002400     SKIP2                                                                
002500 INPUT-OUTPUT SECTION.                                                    
002600                                                                          
002700 FILE-CONTROL.                                                            
002800     SKIP2                                                                
003200*          --- FIL FÖR ÖVERSÄTTNING TILL EDI-FORMAT                       
003300     SELECT W47623                     ASSIGN TO W47626D1.                
003400     SKIP2                                                                
003410*          --- EDI-POSTER                                                 
003420     SELECT W47626                     ASSIGN TO W47626D2.                
003430     SKIP2                                                                
003500 DATA DIVISION.                                                           
003600     SKIP2                                                                
003700 FILE SECTION.                                                            
003800     SKIP3                                                                
004800 FD  W47623                                                               
004900     RECORDING       F                                                    
005000     BLOCK CONTAINS  0.                                                   
005100                                                                          
005300*01  POST -COPY W4762501  -PRE  IN-  -L.                                  
005310       EJECT                                                              
005311 FD  W47626                                                               
005320     RECORDING       V                                                    
005330     BLOCK CONTAINS  0.                                                   
005340                                                                          
005341 01  EDI-POST                    PIC X(531).                              
005370*                                                                         
005380     EJECT                                                                
005400 WORKING-STORAGE SECTION.                                                 
005500                                                                          
005600                                                                          
005700*    -- CHECKED BY WY2000                                                 
005800 77  IDPGM                       PIC X(8)    VALUE 'W4762600'.            
005900 77  JA                          PIC X       VALUE 'J'.                   
006000 77  NEJ                         PIC X       VALUE 'N'.                   
006100 77  PUNKT                       PIC X       VALUE '.'.                   
006200                                                                          
006300 77  W47623-EOF-SW               PIC X       VALUE 'N'.                   
006400     88  END-OF-W47623                       VALUE 'J'.                   
006500                                                                          
006510 77  TOM-FIL-SW                  PIC X       VALUE 'N'.                   
006520     88  TOM-FIL                             VALUE 'J'.                   
006530                                                                          
006600 77  FIRST-POST-SW               PIC X       VALUE 'J'.                   
006700     88  FIRST-POST                          VALUE 'J'.                   
006701                                                                          
006702 77  UTSKRIFT-SW                 PIC X       VALUE 'J'.                   
006703     88  SKRIV-FAKTURA                       VALUE 'J'.                   
006704                                                                          
006710 77  SKRIV-DGS-SW                PIC X       VALUE 'N'.                   
006720     88  SKRIV-DGS                           VALUE 'J'.                   
006730                                                                          
006800     EJECT                                                                
006900                                                                          
007000                                                                          
007100 01  ARBETSFALT.                                                          
007110     03 WW-IDSHIPM                  PIC 9(7)  VALUE ZERO COMP-3.          
007120     03 WW-IDPRODNR                 PIC S9(7) VALUE ZERO COMP-3.          
007130     03 WW-IDKOLLI                  PIC S9(5) VALUE ZERO COMP-3.          
007200                                                                          
007300     03 WS-NUMBER-OF-SEGMENTS       PIC 9(06) VALUE ZERO.                 
007400     03 WS-MESSAGE-REF-NO           PIC 9(06) VALUE ZERO.                 
007500                                                                          
007600                                                                          
007700     03 WS-FAKTURANUMMER.                                                 
007800        05 WS-FAKT-KDFAKTYP         PIC X(01) VALUE SPACE.                
007900        05 WS-FAKT-IDFAKT           PIC 9(07) VALUE ZERO.                 
008000                                                                          
008100                                                                          
008110     03 W-ANT-KOLLI                 PIC S9(5) VALUE ZERO COMP-3.          
008200     03 WS-KOLLIRAK                 PIC 9(03) VALUE ZERO.                 
008300                                                                          
008400     03 IN-IDPTYP                   PIC X(03) VALUE SPACE.                
008500     03 EDI-IDPTYP                  PIC X(03) VALUE SPACE.                
008600                                                                          
008700     03 WS-PRODNR-KOLLI.                                                  
008800        05 WS-IDPRODNR              PIC 9(07) VALUE ZERO.                 
008900        05 WS-IDKOLLI               PIC 9(05) VALUE ZERO.                 
009000                                                                          
009010     03 WS-DISTR-KUND.                                                    
009020        05 WS-IDDISTR               PIC 9(03) VALUE ZERO.                 
009030        05 WS-IDKUNDNR              PIC 9(05) VALUE ZERO.                 
009040                                                                          
009100                                                                          
011500 01  KONSTANTER.                                                          
011600                                                                          
011700     03 001-UPPGIFTER.                                                    
011800                                                                          
011900       05 WC-001-IDPTYP                 PIC X(03) VALUE '001'.            
012000       05 WC-001-LENGTH                 PIC 9(03) VALUE  072 .            
012100       05 WC-001-VOLVO-PARTS            PIC X(04) VALUE 'VPAR'.           
012200       05 WC-001-VOLVO-AMTRIX           PIC X(04) VALUE 'VAMP'.           
012300       05 WC-001-IFCSUM93               PIC X(08)                         
012310                                        VALUE 'IFCSUM93'.                 
012600                                                                          
012700                                                                          
012701     03 UNB-UPPGIFTER.                                                    
012702                                                                          
012703       05 WC-UNB-IDPTYP                 PIC X(03) VALUE 'UNB'.            
012704       05 WC-UNB-LENGTH                 PIC 9(03) VALUE  125 .            
012705       05 WC-UNB-UNOA                   PIC X(04) VALUE 'UNOA'.           
012706       05 WC-UNB-2                      PIC X(01) VALUE '2'.              
012707       05 WC-UNB-CDC-CAR-PARTS          PIC X(14)                         
012708                                        VALUE '01441         '.           
012711       05 WC-UNB-DANZAS-NO-PROD         PIC X(14)                         
012712                                        VALUE '100232        '.           
012714                                                                          
012715                                                                          
012716     03 UNH-UPPGIFTER.                                                    
012720                                                                          
012730       05 WC-UNH-IDPTYP                 PIC X(03) VALUE 'UNH'.            
012740       05 WC-UNH-LENGTH                 PIC 9(03) VALUE  072 .            
012750       05 WC-UNH-IFCSUM                 PIC X(06) VALUE 'IFCSUM'.         
012760       05 WC-UNH-STANDARD-MESSAGE       PIC X(03) VALUE 'S  '.            
012770       05 WC-UNH-93A                    PIC X(03) VALUE '93A'.            
012780       05 WC-UNH-UN                     PIC X(02) VALUE 'UN'.             
012790       05 WC-UNH-NIG001                 PIC X(06) VALUE 'NIG001'.         
012791                                                                          
012792                                                                          
012800     03 BGM-UPPGIFTER.                                                    
012900                                                                          
013000       05 WC-BGM-IDPTYP                 PIC X(03) VALUE 'BGM'.            
013100       05 WC-BGM-LENGTH                 PIC 9(03) VALUE  044 .            
013200       05 WC-BGM-CARGO-MANIFEST         PIC X(03) VALUE '785'.            
013300       05 WC-BGM-CANCELATION            PIC X(03) VALUE '1  '.            
013400       05 WC-BGM-REPLACE                PIC X(03) VALUE '5  '.            
013500       05 WC-BGM-ORGINAL                PIC X(03) VALUE '9  '.            
013600                                                                          
013700                                                                          
013800     03 BGM-FTX-UPPGIFTER.                                                
013900                                                                          
014000       05 WC-BGM-FTX-IDPTYP              PIC X(03) VALUE 'FTX'.           
014100       05 WC-BGM-FTX-LENGTH              PIC 9(03) VALUE  353 .           
014200       05 WC-BGM-FTX-GENERAL-INFORMATION PIC X(03) VALUE 'AAI'.           
014300                                                                          
014400                                                                          
014500     03 CNT-UPPGIFTER.                                                    
014600                                                                          
014700       05 WC-CNT-IDPTYP                 PIC X(03) VALUE 'CNT'.            
014800       05 WC-CNT-LENGTH                 PIC 9(03) VALUE  024.             
014900       05 WC-CNT-TOT-GROSS-WEIGHT       PIC X(03) VALUE '7  '.            
015000       05 WC-CNT-TOT-NUM-OF-PACK        PIC X(03) VALUE '11 '.            
015100       05 WC-CNT-KILOGRAM               PIC X(03) VALUE 'KGM'.            
015200       05 WC-CNT-PIECES                 PIC X(03) VALUE 'PCE'.            
015300       05 WC-CNT-CUBIC-METRE            PIC X(03) VALUE 'MTQ'.            
015400                                                                          
015500                                                                          
015600     03 TDT-UPPGIFTER.                                                    
015700                                                                          
015800       05 WC-TDT-IDPTYP                 PIC X(03) VALUE 'TDT'.            
015900       05 WC-TDT-LENGTH                 PIC 9(03) VALUE  113.             
016000       05 WC-TDT-MAIN-CARRIAGE-TRANSP   PIC X(03) VALUE '20 '.            
016100       05 WC-TDT-ROAD-TRANSPORT         PIC X(03) VALUE '3  '.            
016200                                                                          
016300                                                                          
016400     03 LOC-UPPGIFTER.                                                    
016500                                                                          
016600       05 WC-LOC-IDPTYP                 PIC X(03) VALUE 'LOC'.            
016700       05 WC-LOC-LENGTH                 PIC 9(03) VALUE  051.             
016800       05 WC-LOC-PLACE-OF-DESTINATION   PIC X(03) VALUE '8  '.            
016900                                                                          
017000                                                                          
017100     03 DTM-UPPGIFTER.                                                    
017200                                                                          
017300       05 WC-DTM-IDPTYP                 PIC X(03) VALUE 'DTM'.            
017400       05 WC-DTM-LENGTH                 PIC 9(03) VALUE 041.              
017500       05 WC-DTM-DEPARTURE-SCHEDULED    PIC X(03) VALUE '189'.            
017600       05 WC-DTM-CCYYMMDDHHMM           PIC X(03) VALUE '203'.            
017700                                                                          
017800                                                                          
017900     03 NAD-UPPGIFTER.                                                    
018000                                                                          
018100       05 WC-NAD-IDPTYP                 PIC X(03) VALUE 'NAD'.            
018200       05 WC-NAD-LENGTH                 PIC 9(03) VALUE 531.              
018300       05 WC-NAD-DOCUMENT-SENDER        PIC X(03) VALUE 'MS '.            
018400       05 WC-NAD-VOLVO-DEPARTMENT       PIC X(17)                         
018500                                        VALUE '57512            '.        
018600                                                                          
018700                                                                          
018800     03 CNI-UPPGIFTER.                                                    
018900                                                                          
019000       05 WC-CNI-IDPTYP                 PIC X(03) VALUE 'CNI'.            
019100       05 WC-CNI-LENGTH                 PIC 9(03) VALUE 039.              
019200                                                                          
019300                                                                          
019400     03 MOA-UPPGIFTER.                                                    
019500                                                                          
019600       05 WC-MOA-IDPTYP                 PIC X(03) VALUE 'MOA'.            
019700       05 WC-MOA-LENGTH                 PIC 9(03) VALUE 024.              
019800       05 WC-MOA-INVOICE-AMOUNT         PIC X(03) VALUE '77 '.            
019810       05 WC-MOA-TECKEN                 PIC X(01) VALUE '+'.              
019900       05 WC-MOA-SEK                    PIC X(17) VALUE 'SEK'.            
020000                                                                          
020100                                                                          
020200     03 CNI-FTX-UPPGIFTER.                                                
020300                                                                          
020400       05 WC-CNI-FTX-IDPTYP             PIC X(03) VALUE 'FTX'.            
020500       05 WC-CNI-FTX-LENGTH             PIC 9(03) VALUE 353.              
020600       05 WC-CNI-FTX-GENERAL-INFO       PIC X(03) VALUE 'AAI'.            
020700                                                                          
020800                                                                          
020900     03 RFF-UPPGIFTER.                                                    
021000                                                                          
021100       05 WC-RFF-IDPTYP                 PIC X(03) VALUE 'RFF'.            
021200       05 WC-RFF-LENGTH                 PIC 9(03) VALUE 038.              
021300       05 WC-RFF-BUYERS-ORDER-NUMBER    PIC X(03) VALUE 'CO '.            
021400                                                                          
021500                                                                          
021600     03 CNI-NAD-UPPGIFTER.                                                
021700                                                                          
021800       05 WC-CNI-NAD-IDPTYP             PIC X(03) VALUE 'NAD'.            
021900       05 WC-CNI-NAD-LENGTH             PIC 9(03) VALUE 353.              
022000       05 WC-CNI-NAD-CONSIGNEE          PIC X(03) VALUE 'CN '.            
022100                                                                          
022200                                                                          
022300     03 CTA-UPPGIFTER.                                                    
022400                                                                          
022500       05 WC-CTA-IDPTYP                 PIC X(03) VALUE 'CTA'.            
022600       05 WC-CTA-LENGTH                 PIC 9(03) VALUE 055.              
022700       05 WC-CTA-INFO-CONTACT           PIC X(03) VALUE 'IC '.            
022800                                                                          
022900                                                                          
023000     03 COM-UPPGIFTER.                                                    
023100                                                                          
023200       05 WC-COM-IDPTYP                 PIC X(03) VALUE 'COM'.            
023300       05 WC-COM-LENGTH                 PIC 9(03) VALUE 028.              
023400       05 WC-COM-TELEFAX                PIC X(03) VALUE 'FX '.            
023500       05 WC-COM-TELEPHONE              PIC X(03) VALUE 'TE '.            
023600                                                                          
023700                                                                          
023800     03 GID-UPPGIFTER.                                                    
023900                                                                          
024000       05 WC-GID-IDPTYP                 PIC X(03) VALUE 'GID'.            
024100       05 WC-GID-LENGTH                 PIC 9(03) VALUE 155.              
024200                                                                          
024300                                                                          
024400     03 GID-FTX-UPPGIFTER.                                                
024500                                                                          
024600       05 WC-GID-FTX-IDPTYP             PIC X(03) VALUE 'FTX'.            
024700       05 WC-GID-FTX-LENGTH             PIC 9(03) VALUE 353.              
024800       05 WC-GID-FTX-GOODS-DESCR        PIC X(03) VALUE 'AAA'.            
024900       05 WC-GID-FTX-CAR-PARTS          PIC X(20)                         
025000                                        VALUE 'CAR-PARTS   ' .            
025100       05 WC-GID-FTX-FARLIGT-GODS       PIC X(20)                         
025200                                        VALUE 'FARLIGT GODS' .            
025300                                                                          
025400                                                                          
025500     03 MEA-UPPGIFTER.                                                    
025600                                                                          
025700       05 WC-MEA-IDPTYP                 PIC X(03) VALUE 'MEA'.            
025800       05 WC-MEA-LENGTH                 PIC 9(03) VALUE 66.               
025900                                                                          
026000       05 WC-MEA-WEIGHT                 PIC X(03) VALUE 'WT '.            
026100       05 WC-MEA-VOLUME                 PIC X(03) VALUE 'VOL'.            
026200       05 WC-MEA-GROSS-WEIGHT           PIC X(03) VALUE 'G  '.            
026300       05 WC-MEA-NET-WEIGHT             PIC X(03) VALUE 'N  '.            
026400       05 WC-MEA-KILOGRAM               PIC X(03) VALUE 'KGM'.            
026500       05 WC-MEA-CUBIC-METRE            PIC X(03) VALUE 'MTQ'.            
026600       05 WC-MEA-TONNE                  PIC X(03) VALUE 'TNE'.            
026700                                                                          
026800                                                                          
026900     03 DIM-UPPGIFTER.                                                    
027000                                                                          
027100       05 WC-DIM-IDPTYP                 PIC X(03) VALUE 'DIM'.            
027200       05 WC-DIM-LENGTH                 PIC 9(03) VALUE 51.               
027300                                                                          
027400       05 WC-DIM-OFF-STANDARD           PIC X(03) VALUE '9  '.            
027500       05 WC-DIM-METER                  PIC X(03) VALUE 'MTR'.            
027600       05 WC-DIM-DECIMETER              PIC X(03) VALUE 'DTM'.            
027700       05 WC-DIM-CENTIMETER             PIC X(03) VALUE 'CMT'.            
027800                                                                          
027900                                                                          
027910     03 DGS-UPPGIFTER.                                                    
027920                                                                          
027930       05 WC-DGS-IDPTYP                 PIC X(03) VALUE 'DGS'.            
027940       05 WC-DGS-LENGTH                 PIC 9(03) VALUE 97.               
027950                                                                          
027960       05 WC-DGS-ADR                    PIC X(03) VALUE 'ADR'.            
027991                                                                          
027992                                                                          
028000     03 UNT-UPPGIFTER.                                                    
028200       05 WC-UNT-IDPTYP                 PIC X(03) VALUE 'UNT'.            
028300       05 WC-UNT-LENGTH                 PIC 9(03) VALUE 28.               
028400                                                                          
028500                                                                          
028510     03 003-UPPGIFTER.                                                    
028520                                                                          
028530       05 WC-003-IDPTYP                 PIC X(03) VALUE '003'.            
028540       05 WC-003-LENGTH                 PIC 9(03) VALUE 73.               
028550                                                                          
028560                                                                          
028600                                                                          
028700                                                                          
028800                                                                          
028900                                                                          
029000 01  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
029100 01  FILLER REDEFINES DAGENS-DATUM.                                       
029200     03  DAGENS-DATUM-AAR        PIC 9(2).                                
029300     03  DAGENS-DATUM-MAANAD     PIC 9(2).                                
029400     03  DAGENS-DATUM-DAG        PIC 9(2).                                
029500     EJECT                                                                
029600                                                                          
029700 01  WS-CURRENT-DATE-TIME.                                                
029800     03 WS-CURRENT-DATE          PIC 9(8).                                
029900     03 WS-CURRENT-TIME          PIC 9(4).                                
030000                                                                          
030100                                                                          
030200 01  DYNAMISKA-SUBPROGRAM.                                                
030300*                                                                         
030400     03  ABEND                   PIC X(8)    VALUE 'ABEND'.               
030500     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
030600     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
030700     03  POSTSUM                 PIC X(8)    VALUE 'POSTSUM'.             
030800     SKIP2                                                                
030900*    --- PARAMETRAR TILL ABEND                                            
031000                                                                          
031100 77  RKOD-ABEND                  PIC S9(4)   COMP VALUE +0.               
031200 77  RKOD-ABEND-UTAN-DUMP        PIC S9(4)   COMP VALUE +16.              
031300 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   COMP VALUE +1000.            
031400     SKIP2                                                                
031500 01  FELTEXT.                                                             
031600     03  FILLER                  PIC X(8)    VALUE 'FELTEXT'.             
031700     03  FELTEXT-STR             PIC X(72)   VALUE SPACE.                 
031800     EJECT                                                                
031900*    --- PARAMETRAR TILL POSTSUM                                          
032000*                                                                         
032100*01  -COPY W0005   -PRE  POSTSUM-                                         
032200     EJECT                                                                
032300                                                                          
032400                                                                          
032500 01  EDI-AREA-START              PIC X(24)   VALUE                        
032600                                 'EDI-AREA-START  '.                      
032700 01  EDI-AREA.                                                            
032800     03  FILLER                   PIC X(24) VALUE '001-AREA'.             
032900     03  001-AREA.                                                        
033000*       05  -COPY WEDI0013.                                               
033100                                                                          
033110    03  FILLER                   PIC X(24) VALUE 'UNB-AREA'.              
033120    03  UNB-AREA.                                                         
033130*       05  -COPY WEDIUNB3.                                               
033140                                                                          
033150    03  FILLER                   PIC X(24) VALUE 'UNH-AREA'.              
033160    03  UNH-AREA.                                                         
033170*       05  -COPY WEDIUNH3.                                               
033180                                                                          
033200    03  FILLER                   PIC X(24) VALUE 'BGM-AREA'.              
033300    03  BGM-AREA.                                                         
033400*       05  -COPY WEDIBGM3.                                               
033500                                                                          
033600    03  FILLER                   PIC X(24) VALUE 'BGM-FTX-AREA'.          
033700    03  BGM-FTX-AREA.                                                     
033800*       05  -COPY WEDIFTX3 -PRE BGM-.                                     
033900                                                                          
034000    03  FILLER                   PIC X(24) VALUE 'VIKT-CNT-AREA'.         
034100    03  VIKT-CNT-AREA.                                                    
034200*       05  -COPY WEDICNT3 -PRE VIKT- .                                   
034300                                                                          
034400    03  FILLER                   PIC X(24) VALUE 'KOLLI-CNT-AREA'.        
034500    03  KOLLI-CNT-AREA.                                                   
034600*       05  -COPY WEDICNT3 -PRE KOLLI- .                                  
034700                                                                          
034800    03  FILLER                   PIC X(24) VALUE 'TDT-AREA'.              
034900    03  TDT-AREA.                                                         
035000*       05  -COPY WEDITDT3.                                               
035100                                                                          
035200    03  FILLER                   PIC X(24) VALUE 'LOC-AREA'.              
035300    03  LOC-AREA.                                                         
035400*       05  -COPY WEDILOC3.                                               
035500                                                                          
035600    03  FILLER                   PIC X(24) VALUE 'DTM-AREA'.              
035700    03  DTM-AREA.                                                         
035800*       05  -COPY WEDIDTM3.                                               
035900                                                                          
036000    03  FILLER                   PIC X(24) VALUE 'NAD-AREA'.              
036100    03  NAD-AREA.                                                         
036200*       05  -COPY WEDINAD3.                                               
036300                                                                          
036400    03  FILLER                   PIC X(24) VALUE 'CNI-AREA'.              
036500    03  CNI-AREA.                                                         
036600*       05  -COPY WEDICNI3.                                               
036700                                                                          
036800    03  FILLER                   PIC X(24) VALUE 'MOA-AREA'.              
036900    03  MOA-AREA.                                                         
037000*       05  -COPY WEDIMOA3.                                               
037100                                                                          
037200    03  FILLER                   PIC X(24) VALUE 'CNI-FTX-AREA'.          
037300    03  CNI-FTX-AREA.                                                     
037400*       05  -COPY WEDIFTX3 -PRE CNI-.                                     
037500                                                                          
037600    03  FILLER                   PIC X(24) VALUE 'RFF-AREA'.              
037700    03  RFF-AREA.                                                         
037800*       05  -COPY WEDIRFF3.                                               
037900                                                                          
038000    03  FILLER                   PIC X(24) VALUE 'CNI-NAD-AREA'.          
038100    03  CNI-NAD-AREA.                                                     
038200*       05  -COPY WEDINAD4 -PRE CNI-.                                     
038300                                                                          
038400    03  FILLER                   PIC X(24) VALUE 'CTA-AREA'.              
038500    03  CTA-AREA.                                                         
038600*       05  -COPY WEDICTA3.                                               
038700                                                                          
038800    03  FILLER                   PIC X(24) VALUE 'COM-AREA'.              
038900    03  COM-AREA.                                                         
039000*       05  -COPY WEDICOM3.                                               
039100                                                                          
039200    03  FILLER                   PIC X(24) VALUE 'GID-AREA'.              
039300    03  GID-AREA.                                                         
039400*       05  -COPY WEDIGID3.                                               
039500                                                                          
039600    03  FILLER                   PIC X(24) VALUE 'GID-FTX-AREA'.          
039700    03  GID-FTX-AREA.                                                     
039800*       05  -COPY WEDIFTX3 -PRE GID-.                                     
039900                                                                          
040000    03  FILLER                   PIC X(24) VALUE 'MEA-AREA'.              
040100    03  MEA-AREA.                                                         
040200        05 MEA-GRUPP OCCURS 3.                                            
040300*         07  -COPY WEDIMEA3.                                             
040400                                                                          
040500    03  FILLER                   PIC X(24) VALUE 'DIM-AREA'.              
040600    03  DIM-AREA.                                                         
040700*       05  -COPY WEDIDIM3.                                               
040800                                                                          
040900    03  FILLER                   PIC X(24) VALUE 'DGS-AREA'.              
041000    03  DGS-AREA.                                                         
041100*       05  -COPY WEDIDGS3.                                               
041200                                                                          
041300    03  FILLER                   PIC X(24) VALUE 'DGS-FTX-AREA'.          
041400    03  DGS-FTX-AREA.                                                     
041500*       05  -COPY WEDIFTX3 -PRE DGS-.                                     
041600                                                                          
041700    03  FILLER                   PIC X(24) VALUE 'UNT-AREA'.              
041800    03  UNT-AREA.                                                         
041900*       05  -COPY WEDIUNT3.                                               
042000                                                                          
042010    03  FILLER                   PIC X(24) VALUE '003-AREA'.              
042020    03  003-AREA.                                                         
042030*       05  -COPY WEDI0033.                                               
042040                                                                          
042100     EJECT                                                                
042110 01  IN-AREA-START               PIC X(24)   VALUE                        
042120                                 'IN-AREA-START  '.                       
042130 01  IN-AREA.                                                             
042140*    03  -COPY W4762501                                                   
042150                                                                          
042200 01  UT-AREA-START               PIC X(24)   VALUE                        
042300                                 'UT-AREA-START  '.                       
043600                                                                          
043700 01  UT-AREA.                                                             
043800     03  UT-EDI-AREA             PIC X(531).                              
043900                                                                          
043960                                                                          
044000*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
044100*                                                                         
044200     EJECT                                                                
044300 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
044400     SKIP3                                                                
044500 01  NYCKLAR-TILL-DLI.                                                    
044600                                                                          
045200                                                                          
045300     03  W-IDFAKLOP-X.                                                    
045400         05  W-IDFAKLOP          PIC S9(03) COMP-3 VALUE ZERO.            
045401                                                                          
045410     03  W-IDPRODNR-X.                                                    
045420         05  W-IDPRODNR          PIC S9(07) COMP-3 VALUE ZERO.            
045500                                                                          
045600     03  W-IDGMT-X.                                                       
045700         05  W-IDDISTR-GMT       PIC S9(05) COMP-3 VALUE ZERO.            
045800         05  W-IDKUNDNR-GMT      PIC S9(07) COMP-3 VALUE ZERO.            
045900                                                                          
045910     03  W-IDSHIPM-X.                                                     
045920         05  W-IDSHIPM           PIC 9(07)  VALUE ZERO.                   
045930                                                                          
045940     03  W-IDDISTR-X.                                                     
045950         05  W-IDDISTR           PIC S9(5)   VALUE ZERO COMP-3.           
045960         05  W-IDKUNDNR          PIC S9(7)   VALUE ZERO COMP-3.           
046000                                                                          
046100     03  W-WDGXKEY-4738-X.                                                
046200         05  FILLER              PIC X(4)     VALUE '4738'.               
046300         05  W-KDEMBTYP          PIC S9(3)    VALUE +0  COMP-3.           
046400         05  FILLER              PIC X(24)    VALUE LOW-VALUE.            
046500                                                                          
046600     EJECT                                                                
046700*    --- STATUS-KOD FRÅN IMS                                              
046800 01  STATUS-WS                   PIC XX.                                  
046900     88  SEGMENT-FINNS                       VALUE '  '.                  
047000     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
047100     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
047200     SKIP2                                                                
047300 01  GODK-STATUSKODER.                                                    
047400     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
047500     SKIP3                                                                
047600 01  SSA1                        PIC X(64).                               
047700 01  SSA2                        PIC X(64).                               
047800     EJECT                                                                
047900*    --- IMS FUNKTIONSKODER                                               
048000*01  -COPY W0003                                                          
048100     EJECT                                                                
048200*    ---  DLI INPUT-OUTPUT AREA                                           
048300 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDE601'.                      
048400 01  DLI-IO-WDE601.                                                       
048500*    03  -COPY WDE601.                                                    
048600     EJECT                                                                
048700 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDB201'.                      
048800 01  DLI-IO-WDB201.                                                       
048900*    03  -COPY WDB201.                                                    
049000     EJECT                                                                
049100 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDGX4738'.                    
049200 01  DLI-IO-WDGX4738.                                                     
049300*    03  -COPY WDGX4738.                                                  
049400     EJECT                                                                
049410 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDE101'.                      
049420 01  DLI-IO-WDE101.                                                       
049430*    03  -COPY WDE101.                                                    
049440     EJECT                                                                
049450 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDE111'.                      
049460 01  DLI-IO-WDE111.                                                       
049470*    03  -COPY WDE111.                                                    
049480     EJECT                                                                
049490 01  FILLER         PIC X(16) VALUE 'DLI-IO-WDE121'.                      
049491 01  DLI-IO-WDE121.                                                       
049492*    03  -COPY WDE121.                                                    
049493     EJECT                                                                
049500                                                                          
049600                                                                          
049700 LINKAGE SECTION.                                                         
049800                                                                          
050000*01  -COPY W0008  -PRE WDE6-                                              
050100     05  FILLER                  PIC X.                                   
050200                                                                          
050300*01  -COPY W0008  -PRE WDB2-                                              
050400     05  FILLER                  PIC X.                                   
050500                                                                          
050600*01  -COPY W0008  -PRE 4738-                                              
050700     05  FILLER                  PIC X.                                   
050701                                                                          
050710*01  -COPY W0008  -PRE WDE1-                                              
050720     05  FILLER                  PIC X.                                   
050800     EJECT                                                                
050900                                                                          
051100 PROCEDURE DIVISION  USING WDE6-PCB WDB2-PCB                              
051200                           4738-PCB WDE1-PCB.                             
051300 MAIN SECTION.                                                            
051700                                                                          
051800     PERFORM A-INIT                                                       
051900                                                                          
052001     PERFORM S10-LAS-INFIL                                                
052002     IF END-OF-W47623                                                     
052003       MOVE JA             TO TOM-FIL-SW                                  
052004     END-IF                                                               
052005     IF NOT TOM-FIL                                                       
052010       PERFORM S31-SKRIV-001-POST                                         
052020       PERFORM S32-SKRIV-UNB-POST                                         
052030     END-IF                                                               
052110     PERFORM UNTIL  END-OF-W47623                                         
052200                                                                          
052401        MOVE JA TO UTSKRIFT-SW                                            
052402        IF WW-IDSHIPM NOT = SHIP-IDSKEPPN                                 
052440           IF SKRIV-FAKTURA                                               
052500              IF  FIRST-POST                                              
052600                 MOVE NEJ TO FIRST-POST-SW                                
052900              END-IF                                                      
053300              PERFORM B-NOLLSTALL-FAKTURA                                 
053400              PERFORM DA-BEHANDLA-FAKTHUV-INFO                            
053500              PERFORM S21-SKRIV-FAKTHUV-INFO                              
053510           END-IF                                                         
053520        END-IF                                                            
053600                                                                          
053700        IF WW-IDSHIPM  NOT = SHIP-IDSKEPPN OR                             
053701           WW-IDPRODNR NOT = SHIP-IDPRODNR OR                             
053702           WW-IDKOLLI  NOT = SHIP-IDKOLLI                                 
053703           IF SKRIV-FAKTURA                                               
053710             PERFORM C-NOLLSTALL-KOLLI                                    
053900              PERFORM DB-BEHANDLA-KSPEC-INFO                              
053910           END-IF                                                         
053920        END-IF                                                            
054000                                                                          
054110           IF SKRIV-FAKTURA                                               
054300              PERFORM DC-BEHANDLA-KOLLI-INFO                              
054400              PERFORM S22-SKRIV-KOLLI-INFO                                
054410           END-IF                                                         
054500                                                                          
054800                                                                          
054801        MOVE SHIP-IDSKEPPN     TO WW-IDSHIPM                              
054802        MOVE SHIP-IDPRODNR     TO WW-IDPRODNR                             
054803        MOVE SHIP-IDKOLLI      TO WW-IDKOLLI                              
054810       PERFORM S10-LAS-INFIL                                              
055000     END-PERFORM                                                          
055100                                                                          
055200     IF NOT TOM-FIL                                                       
055310       PERFORM S34-SKRIV-003-POST                                         
055320     END-IF                                                               
055400     PERFORM Z-FINIT                                                      
055500                                                                          
055600     MOVE ZERO TO RETURN-CODE                                             
055700     GOBACK                                                               
055800     .                                                                    
055900     EJECT                                                                
056000 A-INIT SECTION.                                                          
056100                                                                          
056200     OPEN INPUT  W47623                                                   
056300                                                                          
056400     OPEN OUTPUT W47626                                                   
056500                                                                          
056600     ACCEPT DAGENS-DATUM  FROM DATE                                       
056700     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
056800     .                                                                    
056900     EJECT                                                                
057000                                                                          
057120 B-NOLLSTALL-FAKTURA SECTION.                                             
057200                                                                          
057210     COMPUTE  WS-MESSAGE-REF-NO =                                         
057220              WS-MESSAGE-REF-NO + 1                                       
057221     MOVE ZERO             TO WS-KOLLIRAK                                 
057222                              WS-NUMBER-OF-SEGMENTS                       
057223                              WS-DISTR-KUND                               
057230                                                                          
057300     INITIALIZE            UNH-AREA                                       
057400                           BGM-AREA                                       
057500                           BGM-FTX-AREA                                   
057600                           VIKT-CNT-AREA                                  
057700                           KOLLI-CNT-AREA                                 
057800                           TDT-AREA                                       
057900                           LOC-AREA                                       
058000                           DTM-AREA                                       
058100                           NAD-AREA                                       
058200                           CNI-AREA                                       
058300                           MOA-AREA                                       
058400                           CNI-FTX-AREA                                   
058500                           RFF-AREA                                       
058600                           CNI-NAD-AREA                                   
058700                           CTA-AREA                                       
058800                           COM-AREA                                       
058900                           GID-AREA                                       
059000                           GID-FTX-AREA                                   
059100                           MEA-AREA                                       
059200                           DIM-AREA                                       
059300                           DGS-AREA                                       
059400                           DGS-FTX-AREA                                   
059500                           UNT-AREA                                       
059600     .                                                                    
059700     EJECT                                                                
059800                                                                          
059900                                                                          
059920 C-NOLLSTALL-KOLLI SECTION.                                               
060100                                                                          
060510     MOVE NEJ              TO SKRIV-DGS-SW                                
060511                                                                          
060520     INITIALIZE            CNI-AREA                                       
060530                           MOA-AREA                                       
060540                           CNI-FTX-AREA                                   
060550                           RFF-AREA                                       
060560                           CNI-NAD-AREA                                   
060570                           CTA-AREA                                       
060580                           COM-AREA                                       
060590                           GID-AREA                                       
060591                           GID-FTX-AREA                                   
060592                           MEA-AREA                                       
060593                           DIM-AREA                                       
060594                           DGS-AREA                                       
060595                           DGS-FTX-AREA                                   
060600     .                                                                    
060700     EJECT                                                                
060800                                                                          
060900                                                                          
061000 DA-BEHANDLA-FAKTHUV-INFO SECTION.                                        
061100                                                                          
061110     MOVE SHIP-IDDISTR TO WS-IDDISTR                                      
061111     PERFORM DAA-RAKNA-KOLLI                                              
061120                                                                          
061200     PERFORM DA1-SKAPA-UNH-POST                                           
061300     PERFORM DA2-SKAPA-BGM-POST                                           
061400     PERFORM DA3-SKAPA-BGM-FTX-POST                                       
061500     PERFORM DA4-SKAPA-CNT-POST                                           
061600     PERFORM DA5-SKAPA-TDT-POST                                           
061700     PERFORM DA6-SKAPA-LOC-POST                                           
061800     PERFORM DA7-SKAPA-DTM-POST                                           
061900     PERFORM DA8-SKAPA-NAD-POST                                           
062000     .                                                                    
062100     EJECT                                                                
062200                                                                          
062300                                                                          
064700 DA1-SKAPA-UNH-POST SECTION.                                              
064800                                                                          
064900     MOVE WC-UNH-IDPTYP          TO UNH3-IDPTYP                           
065000     MOVE WC-UNH-LENGTH          TO UNH3-LENGTH                           
065100                                                                          
065200     MOVE WS-MESSAGE-REF-NO TO                                            
065300                         UNH3-0062-MESSAGE-REFERENCE                      
065400                                                                          
065500     MOVE WC-UNH-IFCSUM               TO                                  
065600                         UNH3-0065-MESSAGE-TYPE-ID                        
065700     MOVE WC-UNH-STANDARD-MESSAGE     TO                                  
065800                         UNH3-0052-MESSAGE-VERSION                        
065900     MOVE WC-UNH-93A                  TO                                  
066000                         UNH3-0054-MESSAGE-RELEASE                        
066100     MOVE WC-UNH-UN                   TO                                  
066200                         UNH3-0051-CONTROLING-AGENCY                      
066300     MOVE WC-UNH-NIG001               TO                                  
066400                         UNH3-0057-ASSOCIATION-CODE                       
066500                                                                          
066600     .                                                                    
066700     EJECT                                                                
066800                                                                          
066900                                                                          
067000 DA2-SKAPA-BGM-POST SECTION.                                              
067100                                                                          
067200     MOVE WC-BGM-IDPTYP          TO BGM3-IDPTYP                           
067300     MOVE WC-BGM-LENGTH          TO BGM3-LENGTH                           
067400     MOVE WC-BGM-CARGO-MANIFEST  TO BGM3-1001-DOCUMENT-NAME               
067500*    MOVE SHIP-KDFAKTYP          TO WS-FAKT-KDFAKTYP                      
067510     MOVE SPACE                  TO WS-FAKT-KDFAKTYP                      
067600*    MOVE SHIP-IDFAKT            TO WS-FAKT-IDFAKT                        
067610     MOVE SHIP-IDSKEPPN          TO WS-FAKT-IDFAKT                        
067700     MOVE WS-FAKTURANUMMER       TO BGM3-1004-DOCUMENT-NUMBER             
067800     MOVE WC-BGM-ORGINAL         TO BGM3-1225-MESSAGE-FUNCTION            
067900                                                                          
068000                                                                          
068100     .                                                                    
068200     EJECT                                                                
068300                                                                          
068400                                                                          
068500 DA3-SKAPA-BGM-FTX-POST SECTION.                                          
068600                                                                          
068700     MOVE WC-BGM-FTX-IDPTYP      TO BGM-FTX3-IDPTYP                       
068800     MOVE WC-BGM-FTX-LENGTH      TO BGM-FTX3-LENGTH                       
068900     MOVE WC-BGM-FTX-GENERAL-INFORMATION TO                               
069000                            BGM-FTX3-4451-SUBJECT-QUAL                    
069100                                                                          
069200                                                                          
069300     .                                                                    
069400     EJECT                                                                
069500                                                                          
069600                                                                          
069700 DA4-SKAPA-CNT-POST SECTION.                                              
069800                                                                          
069900*    VIKT                                                                 
070100     MOVE WC-CNT-IDPTYP           TO VIKT-CNT3-IDPTYP                     
070200     MOVE WC-CNT-LENGTH           TO VIKT-CNT3-LENGTH                     
070201                                                                          
070202     MOVE WC-CNT-TOT-GROSS-WEIGHT TO                                      
070203                              VIKT-CNT3-6069-CONTROL-QUAL                 
070204                                                                          
070205     MOVE SHIP-IDPRODNR        TO W-IDPRODNR                              
070206     PERFORM IMS-GU-E601                                                  
070300     IF VORD-VKORDBTO > ZERO AND SEGMENT-FINNS                            
071110        MOVE VORD-VKORDBTO TO                                             
071120                              VIKT-CNT3-6066-CONTROL-VALUE                
071140     ELSE                                                                 
071150        MOVE 1                    TO                                      
071160                              VIKT-CNT3-6066-CONTROL-VALUE                
071170     END-IF                                                               
071180                                                                          
071200     MOVE WC-CNT-KILOGRAM         TO VIKT-CNT3-6411-MESSURE-UNIT          
071300                                                                          
071400                                                                          
071500*    ANTAL KOLLI                                                          
071700     MOVE WC-CNT-IDPTYP           TO KOLLI-CNT3-IDPTYP                    
071800     MOVE WC-CNT-LENGTH           TO KOLLI-CNT3-LENGTH                    
071900     MOVE WC-CNT-TOT-NUM-OF-PACK  TO                                      
072000                              KOLLI-CNT3-6069-CONTROL-QUAL                
072100                                                                          
072200                                                                          
072810     IF SEGMENT-FINNS                                                     
072811*       MOVE VORD-KVKOLLI-FAKT  TO KOLLI-CNT3-6066-CONTROL-VALUE          
072813        MOVE W-ANT-KOLLI      TO KOLLI-CNT3-6066-CONTROL-VALUE            
072820     ELSE                                                                 
072821        MOVE 0.001            TO KOLLI-CNT3-6066-CONTROL-VALUE            
072830     END-IF                                                               
073300     MOVE WC-CNT-PIECES       TO KOLLI-CNT3-6411-MESSURE-UNIT             
073400                                                                          
073500                                                                          
073600     .                                                                    
073700     EJECT                                                                
073800                                                                          
073900                                                                          
074000 DA5-SKAPA-TDT-POST SECTION.                                              
074100                                                                          
074200     MOVE WC-TDT-IDPTYP          TO TDT3-IDPTYP                           
074300     MOVE WC-TDT-LENGTH          TO TDT3-LENGTH                           
074400     MOVE WC-TDT-MAIN-CARRIAGE-TRANSP TO                                  
074500                              TDT3-8051-TRANSP-STAGE-QUAL                 
074600     MOVE WC-TDT-ROAD-TRANSPORT  TO                                       
074700                              TDT3-8067-MODE-OF-TRANSPORT                 
074800                                                                          
074900                                                                          
075000                                                                          
075100     .                                                                    
075200     EJECT                                                                
075300                                                                          
075400                                                                          
075500 DA6-SKAPA-LOC-POST SECTION.                                              
075600                                                                          
075700     MOVE WC-LOC-IDPTYP          TO LOC3-IDPTYP                           
075800     MOVE WC-LOC-LENGTH          TO LOC3-LENGTH                           
075900     MOVE WC-LOC-PLACE-OF-DESTINATION TO                                  
076000                              LOC3-3227-PLACE-LOC-QUAL                    
076100                                                                          
076200     MOVE SHIP-IDDISTR           TO LOC3-3225-PLACE-LOC-ID                
076300                                                                          
076400     .                                                                    
076500     EJECT                                                                
076600                                                                          
076700                                                                          
076800 DA7-SKAPA-DTM-POST SECTION.                                              
076900                                                                          
077000     MOVE WC-DTM-IDPTYP          TO DTM3-IDPTYP                           
077100     MOVE WC-DTM-LENGTH          TO DTM3-LENGTH                           
077200                                                                          
077300     MOVE WC-DTM-DEPARTURE-SCHEDULED  TO                                  
077400                            DTM3-2005-DATE-TIME-PER-QUAL                  
077500                                                                          
077600     MOVE FUNCTION CURRENT-DATE(1:8) TO WS-CURRENT-DATE                   
077700     MOVE FUNCTION CURRENT-DATE(9:4) TO WS-CURRENT-TIME                   
077800     MOVE WS-CURRENT-DATE-TIME        TO                                  
077900                            DTM3-2380-DATE-TIME-PER                       
078000                                                                          
078100     MOVE WC-DTM-CCYYMMDDHHMM         TO                                  
078200                            DTM3-2379-DATE-TIME-PER-FORMAT                
078300                                                                          
078400     .                                                                    
078500     EJECT                                                                
078600                                                                          
078700                                                                          
078800 DA8-SKAPA-NAD-POST SECTION.                                              
078900                                                                          
079000     MOVE WC-NAD-IDPTYP          TO NAD3-IDPTYP                           
079100     MOVE WC-NAD-LENGTH          TO NAD3-LENGTH                           
079200                                                                          
079300     MOVE WC-NAD-DOCUMENT-SENDER  TO NAD3-3035-PARTY-QUAL                 
079400     MOVE WC-NAD-VOLVO-DEPARTMENT TO                                      
079500                              NAD3-3039-PARTY-ID                          
079600                                                                          
079700     .                                                                    
079800     EJECT                                                                
079810                                                                          
079820 DAA-RAKNA-KOLLI  SECTION.                                                
079830                                                                          
079831     MOVE ZERO                   TO W-ANT-KOLLI                           
079832     MOVE SHIP-IDSKEPPN          TO W-IDSHIPM                             
079833     PERFORM IMS-GU-WDE101                                                
079834     IF SEGMENT-FINNS                                                     
079835       PERFORM IMS-GNP-WDE111                                             
079836                                                                          
079837       PERFORM UNTIL SEGMENT-SAKNAS                                       
079838         MOVE SGMT-IDDISTR       TO W-IDDISTR                             
079839         MOVE SGMT-IDKUNDNR      TO W-IDKUNDNR                            
079840         PERFORM IMS-GNP-WDE121                                           
079841                                                                          
079842         PERFORM UNTIL SEGMENT-SAKNAS                                     
079843           ADD +1                TO W-ANT-KOLLI                           
079844           PERFORM IMS-GNP-WDE121                                         
079845         END-PERFORM                                                      
079846         PERFORM IMS-GNP-WDE111                                           
079847       END-PERFORM                                                        
079848     END-IF                                                               
079849     IF W-ANT-KOLLI = ZERO                                                
079850       MOVE +1                   TO W-ANT-KOLLI                           
079851     END-IF                                                               
079852     .                                                                    
079860     EJECT                                                                
079900                                                                          
080000                                                                          
080010 DB-BEHANDLA-KSPEC-INFO SECTION.                                          
080020                                                                          
080030     PERFORM DB1-SKAPA-CNI-POST                                           
080040     PERFORM DB2-SKAPA-MOA-POST                                           
080050     PERFORM DB3-SKAPA-CNI-FTX-POST                                       
080070     PERFORM DB5-SKAPA-CTA-POST                                           
080080     PERFORM DB6-SKAPA-COM-POST                                           
080090     PERFORM DB7-SKAPA-GID-POST                                           
080091     PERFORM DB8-SKAPA-GID-FTX-POST                                       
080092     PERFORM DB9-SKAPA-MEA-POST                                           
080093     PERFORM DB10-SKAPA-DIM-POST                                          
080094     PERFORM DB11-SKAPA-DGS-POST                                          
080095     .                                                                    
080096     EJECT                                                                
080097                                                                          
080098                                                                          
080110 DB1-SKAPA-CNI-POST SECTION.                                              
080200                                                                          
080300     MOVE WC-CNI-IDPTYP          TO CNI3-IDPTYP                           
080400     MOVE WC-CNI-LENGTH          TO CNI3-LENGTH                           
080500                                                                          
080600     COMPUTE WS-KOLLIRAK =                                                
080700             WS-KOLLIRAK + 1                                              
080800     MOVE WS-KOLLIRAK            TO                                       
080900                        CNI3-1490-CONSOLID-ITEM-NUMBER                    
081000                                                                          
081100     MOVE SHIP-IDPRODNR          TO WS-IDPRODNR                           
081200     MOVE SHIP-IDKOLLI           TO WS-IDKOLLI                            
081300                                                                          
081400     MOVE WS-PRODNR-KOLLI        TO                                       
081500                              CNI3-1004-DOCUMENT-NUMBER                   
081600                                                                          
081700     .                                                                    
081800     EJECT                                                                
081900                                                                          
082000                                                                          
082100 DB2-SKAPA-MOA-POST SECTION.                                              
082200                                                                          
082300     MOVE WC-MOA-IDPTYP          TO MOA3-IDPTYP                           
082400     MOVE WC-MOA-LENGTH          TO MOA3-LENGTH                           
082500                                                                          
082710     MOVE WC-MOA-INVOICE-AMOUNT  TO                                       
082720                        MOA3-5025-MONETARY-AMOUNT-QUAL                    
082800     MOVE WC-MOA-TECKEN          TO                                       
082810                        MOA3-5004-MONETARY-AMOUNT-TKN                     
082900     MOVE SHIP-SUORDV-KOLLI      TO MOA3-5004-MONETARY-AMOUNT             
083500     MOVE SHIP-KDVALISO          TO MOA3-6345-CURRENCY-CODED              
083600                                                                          
083700     .                                                                    
083800     EJECT                                                                
083900                                                                          
085100                                                                          
085200 DB3-SKAPA-CNI-FTX-POST SECTION.                                          
085300                                                                          
085400     MOVE WC-CNI-FTX-IDPTYP      TO CNI-FTX3-IDPTYP                       
085500     MOVE WC-CNI-FTX-LENGTH      TO CNI-FTX3-LENGTH                       
085600                                                                          
085700     MOVE WC-CNI-FTX-GENERAL-INFO TO                                      
085800                        CNI-FTX3-4451-SUBJECT-QUAL                        
085900                                                                          
086000     .                                                                    
086100     EJECT                                                                
086200                                                                          
086300                                                                          
088800                                                                          
088900                                                                          
089000 DB5-SKAPA-CTA-POST SECTION.                                              
089100                                                                          
089200     MOVE WC-CTA-IDPTYP          TO CTA3-IDPTYP                           
089300     MOVE WC-CTA-LENGTH          TO CTA3-LENGTH                           
089400                                                                          
089500     MOVE WC-CTA-INFO-CONTACT    TO CTA3-3139-CONTACT-FUNCTION            
089600     MOVE 'XXXXX'                TO CTA3-3413-DEP-OR-EMP-ID               
089700     MOVE 'YYYYY'                TO CTA3-3412-DEP-OR-EMP                  
089800     .                                                                    
089900     EJECT                                                                
090000                                                                          
090100                                                                          
090200 DB6-SKAPA-COM-POST SECTION.                                              
090300                                                                          
090400     MOVE WC-COM-IDPTYP          TO COM3-IDPTYP                           
090500     MOVE WC-COM-LENGTH          TO COM3-LENGTH                           
090600                                                                          
090700     MOVE '123456'               TO COM3-3148-COMUNICATION-NO             
090800     MOVE WC-COM-TELEFAX         TO COM3-3155-COMUNICATION-QUAL           
090900     .                                                                    
091000     EJECT                                                                
091100                                                                          
091200                                                                          
091300 DB7-SKAPA-GID-POST SECTION.                                              
091400                                                                          
091500     MOVE WC-GID-IDPTYP          TO GID3-IDPTYP                           
091600     MOVE WC-GID-LENGTH          TO GID3-LENGTH                           
091700                                                                          
091800     MOVE 1                      TO GID3-7224-NUMBER-OF-PACKAGES          
091810     IF SHIP-KDEMBTYP > ZERO                                              
091811        MOVE SHIP-KDEMBTYP       TO W-KDEMBTYP                            
091820     ELSE                                                                 
091821        MOVE +1                  TO W-KDEMBTYP                            
091830     END-IF                                                               
092000     PERFORM IMS-GU-WDGX4738                                              
092100     MOVE EMBTYP-BEEMBTYP(2)     TO GID3-7065-TYPE-OF-PACKAGES-ID         
092300                                                                          
092400                                                                          
092500     .                                                                    
092600     EJECT                                                                
092700                                                                          
092800                                                                          
092900 DB8-SKAPA-GID-FTX-POST SECTION.                                          
093000                                                                          
093100     MOVE WC-GID-FTX-IDPTYP      TO GID-FTX3-IDPTYP                       
093200     MOVE WC-GID-FTX-LENGTH      TO GID-FTX3-LENGTH                       
093300     MOVE WC-GID-FTX-GOODS-DESCR TO                                       
093400                             GID-FTX3-4451-SUBJECT-QUAL                   
093700     IF SHIP-KDFARLIG-KOLLI > 3                                           
094000        MOVE WC-GID-FTX-FARLIGT-GODS TO                                   
094100                                GID-FTX3-44401-FREE-TEXT-1                
094110     ELSE                                                                 
094120        MOVE WC-GID-FTX-CAR-PARTS    TO                                   
094130                                GID-FTX3-44401-FREE-TEXT-1                
094200     END-IF                                                               
094300     .                                                                    
094400     EJECT                                                                
094500                                                                          
094600                                                                          
094700 DB9-SKAPA-MEA-POST SECTION.                                              
094800                                                                          
094900     MOVE WC-MEA-IDPTYP          TO MEA3-IDPTYP(1)                        
095000     MOVE WC-MEA-LENGTH          TO MEA3-LENGTH(1)                        
095100                                                                          
095200     MOVE WC-MEA-WEIGHT          TO                                       
095300                         MEA3-6311-MEASURMENT-QUAL(1)                     
095400     MOVE WC-MEA-GROSS-WEIGHT    TO                                       
095500                         MEA3-6313-MEASURMENT-DIM(1)                      
095600     MOVE WC-MEA-KILOGRAM        TO                                       
095700                         MEA3-6411-MEASURE-UNIT-QUAL(1)                   
095910     IF SHIP-VKORDBTO-KOLLI > ZERO                                        
095920        MOVE SHIP-VKORDBTO-KOLLI TO                                       
095951                         MEA3-6314-MEASURMENT-VALUE(1)                    
095952     ELSE                                                                 
095953        MOVE 1                        TO                                  
095954                         MEA3-6314-MEASURMENT-VALUE(1)                    
095955     END-IF                                                               
096000                                                                          
096100                                                                          
096200     MOVE WC-MEA-IDPTYP          TO MEA3-IDPTYP(2)                        
096300     MOVE WC-MEA-LENGTH          TO MEA3-LENGTH(2)                        
096400                                                                          
096500     MOVE WC-MEA-WEIGHT          TO                                       
096600                         MEA3-6311-MEASURMENT-QUAL(2)                     
096700     MOVE WC-MEA-NET-WEIGHT      TO                                       
096800                         MEA3-6313-MEASURMENT-DIM(2)                      
096900     MOVE WC-MEA-KILOGRAM        TO                                       
097000                         MEA3-6411-MEASURE-UNIT-QUAL(2)                   
097100                                                                          
097210     IF SHIP-VKORDNTO-KOLLI > ZERO                                        
097220        MOVE SHIP-VKORDNTO-KOLLI TO                                       
097260                         MEA3-6314-MEASURMENT-VALUE(2)                    
097270     ELSE                                                                 
097271        MOVE MEA3-6314-MEASURMENT-VALUE(1) TO                             
097272                         MEA3-6314-MEASURMENT-VALUE(2)                    
097280     END-IF                                                               
097300                                                                          
097400                                                                          
097500     MOVE WC-MEA-IDPTYP          TO MEA3-IDPTYP(3)                        
097600     MOVE WC-MEA-LENGTH          TO MEA3-LENGTH(3)                        
097700                                                                          
097800     MOVE WC-MEA-VOLUME          TO                                       
097900                         MEA3-6311-MEASURMENT-QUAL(3)                     
098000     MOVE WC-MEA-CUBIC-METRE     TO                                       
098100                         MEA3-6411-MEASURE-UNIT-QUAL(3)                   
098200                                                                          
098300     IF SHIP-VLORDBTO-KOLLI > ZERO                                        
098310        MOVE SHIP-VLORDBTO-KOLLI TO                                       
098360                         MEA3-6314-MEASURMENT-VALUE(3)                    
098370     ELSE                                                                 
098371        MOVE 1                   TO                                       
098372                         MEA3-6314-MEASURMENT-VALUE(3)                    
098380     END-IF                                                               
098400                                                                          
098500     .                                                                    
098600     EJECT                                                                
098700                                                                          
098800                                                                          
098900 DB10-SKAPA-DIM-POST SECTION.                                             
099000                                                                          
099100                                                                          
099200     MOVE WC-DIM-IDPTYP          TO DIM3-IDPTYP                           
099300     MOVE WC-DIM-LENGTH          TO DIM3-LENGTH                           
099400                                                                          
099500     MOVE WC-DIM-OFF-STANDARD    TO                                       
099600                         DIM3-6145-DIMENSION-QUAL                         
099700     MOVE WC-DIM-CENTIMETER      TO                                       
099800                         DIM3-6411-MEASURE-UNIT-QUAL                      
099810                                                                          
099900     IF SHIP-DIKOLLIL > ZERO                                              
099910        MOVE SHIP-DIKOLLIL     TO                                         
100000                         DIM3-6168-LENGTH-DIMENSION                       
100010     ELSE                                                                 
100012        MOVE 100          TO  DIM3-6168-LENGTH-DIMENSION                  
100020     END-IF                                                               
100030                                                                          
100031     IF SHIP-DIKOLLIB > ZERO                                              
100032        MOVE SHIP-DIKOLLIB     TO                                         
100033                         DIM3-6140-WIDTH-DIMENSION                        
100034     ELSE                                                                 
100035        MOVE 100          TO  DIM3-6140-WIDTH-DIMENSION                   
100036     END-IF                                                               
100037                                                                          
100038     IF SHIP-DIKOLLIH > ZERO                                              
100039        MOVE SHIP-DIKOLLIH     TO                                         
100040                         DIM3-6008-HEIGHT-DIMENSION                       
100050     ELSE                                                                 
100060        MOVE 100          TO  DIM3-6008-HEIGHT-DIMENSION                  
100070     END-IF                                                               
100600     .                                                                    
100700     EJECT                                                                
100800                                                                          
100900                                                                          
100901 DB11-SKAPA-DGS-POST SECTION.                                             
100902                                                                          
100903     IF SHIP-KDFARLIG-KOLLI > 3                                           
100904        MOVE WC-DGS-IDPTYP          TO DGS3-IDPTYP                        
100905        MOVE WC-DGS-LENGTH          TO DGS3-LENGTH                        
100906                                                                          
100907        MOVE WC-DGS-ADR             TO                                    
100908                            DGS3-8273-DANGEROUS-GOODS-REG                 
100909        MOVE JA                     TO SKRIV-DGS-SW                       
100910     END-IF                                                               
100911     .                                                                    
100912     EJECT                                                                
100913                                                                          
100914                                                                          
100915 DC-BEHANDLA-KOLLI-INFO SECTION.                                          
100920                                                                          
100930     PERFORM DC1-SKAPA-RFF-POST                                           
100931     PERFORM DC2-SKAPA-CNI-NAD-POST                                       
100940     .                                                                    
100950     EJECT                                                                
100960                                                                          
100970                                                                          
101000 DC1-SKAPA-RFF-POST SECTION.                                              
101100                                                                          
101200     MOVE WC-RFF-IDPTYP          TO RFF3-IDPTYP                           
101300     MOVE WC-RFF-LENGTH          TO RFF3-LENGTH                           
101400                                                                          
101500     MOVE WC-RFF-BUYERS-ORDER-NUMBER TO                                   
101600                        RFF3-1153-REFERENCE-QUAL                          
101700                                                                          
101800     MOVE SHIP-IDKUNDRF(3:5) TO                                           
101900                        RFF3-1154-REFERENCE-NUMBER                        
102000                                                                          
102100     .                                                                    
102200     EJECT                                                                
102300                                                                          
102301                                                                          
102310 DC2-SKAPA-CNI-NAD-POST SECTION.                                          
102320                                                                          
102330     MOVE WC-CNI-NAD-IDPTYP      TO CNI-NAD4-IDPTYP                       
102340     MOVE WC-CNI-NAD-LENGTH      TO CNI-NAD4-LENGTH                       
102350                                                                          
102360     MOVE WC-CNI-NAD-CONSIGNEE   TO                                       
102370                        CNI-NAD4-3035-PARTY-QUAL                          
102380                                                                          
102381*    IF SHIP-IDDEALER = ZERO                                              
102382*       MOVE WS-IDDISTR          TO                                       
102383*                       CNI-NAD4-3039-PARTY-ID                            
102384*    ELSE                                                                 
102386        MOVE SHIP-IDKUNDNR       TO WS-IDKUNDNR                           
102387        MOVE WS-DISTR-KUND       TO                                       
102394                        CNI-NAD4-3039-PARTY-ID                            
102395*    END-IF                                                               
102396                                                                          
102397* ADRESS EJ NÖDVÄNDING ENLIGT ASG, KNEP MED ATT DELA UPP                  
102398*POSTNUMMER                                                               
102399*    MOVE SHIP-IDDISTR           TO W-IDDISTR-GMT                         
102400*    MOVE SHIP-IDDEALER          TO W-IDDEALER-GMT                        
102401*    PERFORM IMS-GU-WDB201                                                
102402*    MOVE GMT-BEGMT-RAD1         TO NAD-31241-NAME-AND-ADRESS             
102403*    MOVE GMT-ADGMT-GATA         TO                                       
102404*                           NAD-31221-STREET-AND-NUMBER-PBOX1             
102405*    MOVE GMT-ADGMT-PADR         TO NAD-3164-CITY-NAME                    
102406*    MOVE GMT-ADGMT-LAND         TO NAD-3164-CITY-NAME                    
102407                                                                          
102408     .                                                                    
102409     EJECT                                                                
102410                                                                          
102423                                                                          
102430 S21-SKRIV-FAKTHUV-INFO SECTION.                                          
102500                                                                          
102600     INITIALIZE UT-EDI-AREA                                               
102700     MOVE WEDIUNH3                    TO UT-EDI-AREA                      
102800     MOVE UT-EDI-AREA(1:3)            TO EDI-IDPTYP                       
102900     PERFORM S11-SKRIV-W47626                                             
103000                                                                          
103100     INITIALIZE UT-EDI-AREA                                               
103200     MOVE WEDIBGM3                    TO UT-EDI-AREA                      
103300     MOVE UT-EDI-AREA(1:3)            TO EDI-IDPTYP                       
103400     PERFORM S11-SKRIV-W47626                                             
103500                                                                          
103600     INITIALIZE UT-EDI-AREA                                               
103700     MOVE BGM-WEDIFTX3                TO UT-EDI-AREA                      
103800     MOVE UT-EDI-AREA(1:3)            TO EDI-IDPTYP                       
103900     PERFORM S11-SKRIV-W47626                                             
104000                                                                          
104100     INITIALIZE UT-EDI-AREA                                               
104200     MOVE VIKT-WEDICNT3               TO UT-EDI-AREA                      
104300     MOVE UT-EDI-AREA(1:3)            TO EDI-IDPTYP                       
104400     PERFORM S11-SKRIV-W47626                                             
104500                                                                          
104600     INITIALIZE UT-EDI-AREA                                               
104700     MOVE KOLLI-WEDICNT3              TO UT-EDI-AREA                      
104800     MOVE UT-EDI-AREA(1:3)            TO EDI-IDPTYP                       
104900     PERFORM S11-SKRIV-W47626                                             
105000                                                                          
105100     INITIALIZE UT-EDI-AREA                                               
105200     MOVE WEDITDT3                    TO UT-EDI-AREA                      
105300     MOVE UT-EDI-AREA(1:3)            TO EDI-IDPTYP                       
105400     PERFORM S11-SKRIV-W47626                                             
105500                                                                          
105600     INITIALIZE UT-EDI-AREA                                               
105700     MOVE WEDILOC3                    TO UT-EDI-AREA                      
105800     MOVE UT-EDI-AREA(1:3)            TO EDI-IDPTYP                       
105900     PERFORM S11-SKRIV-W47626                                             
106000                                                                          
106100     INITIALIZE UT-EDI-AREA                                               
106200     MOVE WEDIDTM3                    TO UT-EDI-AREA                      
106300     MOVE UT-EDI-AREA(1:3)            TO EDI-IDPTYP                       
106400     PERFORM S11-SKRIV-W47626                                             
106500                                                                          
106600     INITIALIZE UT-EDI-AREA                                               
106700     MOVE WEDINAD3                   TO UT-EDI-AREA                       
106800     MOVE UT-EDI-AREA(1:3)           TO EDI-IDPTYP                        
106900     PERFORM S11-SKRIV-W47626                                             
107000     .                                                                    
107100     EJECT                                                                
107200                                                                          
107300                                                                          
107400 S22-SKRIV-KOLLI-INFO SECTION.                                            
107500                                                                          
107600     INITIALIZE UT-EDI-AREA                                               
107700     MOVE WEDICNI3                   TO UT-EDI-AREA                       
107800     MOVE UT-EDI-AREA(1:3)           TO EDI-IDPTYP                        
107900     PERFORM S11-SKRIV-W47626                                             
108000                                                                          
108100     INITIALIZE UT-EDI-AREA                                               
108200     MOVE WEDIMOA3                   TO UT-EDI-AREA                       
108300     MOVE UT-EDI-AREA(1:3)           TO EDI-IDPTYP                        
108400     PERFORM S11-SKRIV-W47626                                             
108500                                                                          
108600     INITIALIZE UT-EDI-AREA                                               
108700     MOVE CNI-WEDIFTX3               TO UT-EDI-AREA                       
108800     MOVE UT-EDI-AREA(1:3)           TO EDI-IDPTYP                        
108900     PERFORM S11-SKRIV-W47626                                             
109000                                                                          
109100     INITIALIZE UT-EDI-AREA                                               
109200     MOVE WEDIRFF3                   TO UT-EDI-AREA                       
109300     MOVE UT-EDI-AREA(1:3)           TO EDI-IDPTYP                        
109400     PERFORM S11-SKRIV-W47626                                             
109500                                                                          
109600     INITIALIZE UT-EDI-AREA                                               
109700     MOVE CNI-WEDINAD4               TO UT-EDI-AREA                       
109800     MOVE UT-EDI-AREA(1:3)           TO EDI-IDPTYP                        
109900     PERFORM S11-SKRIV-W47626                                             
110000                                                                          
110010*ANVÄNDS EJ ASG NORGE                                                     
110100*    INITIALIZE UT-EDI-AREA                                               
110200*    MOVE WEDICTA3                   TO UT-EDI-AREA                       
110300*    MOVE UT-EDI-AREA(1:3)           TO EDI-IDPTYP                        
110400*    PERFORM S11-SKRIV-W47626                                             
110500                                                                          
110510*ANVÄNDS EJ ASG NORGE                                                     
110600*    INITIALIZE UT-EDI-AREA                                               
110700*    MOVE WEDICOM3                   TO UT-EDI-AREA                       
110800*    MOVE UT-EDI-AREA(1:3)           TO EDI-IDPTYP                        
110900*    PERFORM S11-SKRIV-W47626                                             
111000                                                                          
111100     INITIALIZE UT-EDI-AREA                                               
111200     MOVE WEDIGID3                   TO UT-EDI-AREA                       
111300     MOVE UT-EDI-AREA(1:3)           TO EDI-IDPTYP                        
111400     PERFORM S11-SKRIV-W47626                                             
111500                                                                          
111600     INITIALIZE UT-EDI-AREA                                               
111700     MOVE GID-WEDIFTX3            TO UT-EDI-AREA                          
111800     MOVE UT-EDI-AREA(1:3)        TO EDI-IDPTYP                           
111900     PERFORM S11-SKRIV-W47626                                             
112000                                                                          
112100     INITIALIZE UT-EDI-AREA                                               
112200     MOVE MEA-GRUPP(1)               TO UT-EDI-AREA                       
112300     MOVE UT-EDI-AREA(1:3)           TO EDI-IDPTYP                        
112400     PERFORM S11-SKRIV-W47626                                             
112500                                                                          
112600     INITIALIZE UT-EDI-AREA                                               
112700     MOVE MEA-GRUPP(2)               TO UT-EDI-AREA                       
112800     MOVE UT-EDI-AREA(1:3)           TO EDI-IDPTYP                        
112900     PERFORM S11-SKRIV-W47626                                             
113000                                                                          
113100     INITIALIZE UT-EDI-AREA                                               
113200     MOVE MEA-GRUPP(3)               TO UT-EDI-AREA                       
113300     MOVE UT-EDI-AREA(1:3)           TO EDI-IDPTYP                        
113400     PERFORM S11-SKRIV-W47626                                             
113500                                                                          
113600     INITIALIZE UT-EDI-AREA                                               
113700     MOVE DIM-AREA                   TO UT-EDI-AREA                       
113800     MOVE UT-EDI-AREA(1:3)           TO EDI-IDPTYP                        
113900     PERFORM S11-SKRIV-W47626                                             
114000                                                                          
114010     IF SKRIV-DGS                                                         
114100        INITIALIZE UT-EDI-AREA                                            
114200        MOVE DGS-AREA                   TO UT-EDI-AREA                    
114300        MOVE UT-EDI-AREA(1:3)           TO EDI-IDPTYP                     
114400        PERFORM S11-SKRIV-W47626                                          
114410     END-IF                                                               
114500     .                                                                    
114600     EJECT                                                                
114700                                                                          
114800                                                                          
116801 S31-SKRIV-001-POST SECTION.                                              
116802                                                                          
116804     INITIALIZE WEDI0013                                                  
116805                                                                          
116806     MOVE WC-001-IDPTYP          TO 0013-IDPTYP                           
116807     MOVE WC-001-LENGTH          TO 0013-LENGTH                           
116808                                                                          
116810     MOVE WC-001-VOLVO-PARTS     TO 0013-SNODE-SENDER-COMMON-NODE         
116811     MOVE WC-001-VOLVO-AMTRIX    TO 0013-RNODE-RECIVER-COMMON-NODE        
116812     MOVE WC-001-IFCSUM93        TO 0013-VFILE-VIRTUAL-FILE-NAME          
116813                                                                          
116814     MOVE FUNCTION CURRENT-DATE(3:8) TO                                   
116815                              0013-VFDATE-VIRTUAL-FILE-DATE               
116816     MOVE FUNCTION CURRENT-DATE(9:4) TO WS-CURRENT-TIME                   
116817                              0013-VFTIME-VIRTUAL-FILE-TIME               
116822                                                                          
116823     INITIALIZE UT-EDI-AREA                                               
116824     MOVE 001-AREA                   TO UT-EDI-AREA                       
116825     MOVE UT-EDI-AREA(1:3)           TO EDI-IDPTYP                        
116826     PERFORM S11-SKRIV-W47626                                             
116836     .                                                                    
116837     EJECT                                                                
116838                                                                          
116839 S32-SKRIV-UNB-POST SECTION.                                              
116840                                                                          
116841     INITIALIZE WEDIUNB3                                                  
116842                                                                          
116843     MOVE WC-UNB-IDPTYP          TO UNB3-IDPTYP                           
116844     MOVE WC-UNB-LENGTH          TO UNB3-LENGTH                           
116845                                                                          
116852     MOVE WC-UNB-UNOA            TO UNB3-E0001-SYNTAX-IDENTIFIER          
116853     MOVE WC-UNB-2               TO UNB3-E0002-SYNTAX-VERSION-NO          
116854     MOVE WC-UNB-CDC-CAR-PARTS   TO UNB3-E0004-SENDER-ID                  
116859     MOVE WC-UNB-DANZAS-NO-PROD  TO UNB3-E0010-RECIPIENT-ID               
116861                                                                          
116863                                                                          
116882     MOVE FUNCTION CURRENT-DATE(3:8) TO UNB3-E0017-DATE                   
116884     MOVE FUNCTION CURRENT-DATE(9:4) TO UNB3-E0019-TIME                   
116886*                                                                         
116888                                                                          
116889     INITIALIZE UT-EDI-AREA                                               
116890     MOVE UNB-AREA                   TO UT-EDI-AREA                       
116891     MOVE UT-EDI-AREA(1:3)           TO EDI-IDPTYP                        
116892     PERFORM S11-SKRIV-W47626                                             
116893     .                                                                    
116894     EJECT                                                                
116895                                                                          
116896                                                                          
116897*S33-SKRIV-UNT-POST SECTION.                                              
116898*                                                                         
116899*    MOVE WC-UNT-IDPTYP          TO UNT3-IDPTYP                           
116900*    MOVE WC-UNT-LENGTH          TO UNT3-LENGTH                           
116901*                                                                         
116902*    COMPUTE WS-NUMBER-OF-SEGMENTS =                                      
116903*            WS-NUMBER-OF-SEGMENTS + 1                                    
116904*    MOVE WS-NUMBER-OF-SEGMENTS  TO                                       
116905*                        UNT3-0074-NUMBER-OF-SEGM                         
116906*    MOVE WS-MESSAGE-REF-NO      TO                                       
116907*                        UNT3-0062-MESSAGE-REF-NO                         
116908*                                                                         
116909*    INITIALIZE UT-EDI-AREA                                               
116910*    MOVE UNT-AREA                   TO UT-EDI-AREA                       
116911*    MOVE UT-EDI-AREA(1:3)           TO EDI-IDPTYP                        
116912*    PERFORM S11-SKRIV-W47626                                             
116913*    .                                                                    
116914*    EJECT                                                                
116915                                                                          
116916                                                                          
116917 S34-SKRIV-003-POST SECTION.                                              
116918                                                                          
116919     INITIALIZE WEDI0033                                                  
116920     MOVE WC-003-IDPTYP          TO 0033-IDPTYP                           
116921     MOVE WC-003-LENGTH          TO 0033-LENGTH                           
116922                                                                          
116923     INITIALIZE UT-EDI-AREA                                               
116924     MOVE 003-AREA                   TO UT-EDI-AREA                       
116925     MOVE UT-EDI-AREA(1:3)           TO EDI-IDPTYP                        
116926     PERFORM S11-SKRIV-W47626                                             
116927     .                                                                    
116928     EJECT                                                                
116929                                                                          
116930                                                                          
116940 Z-FINIT SECTION.                                                         
117000     CLOSE W47623                                                         
117100           W47626                                                         
117200     SKIP2                                                                
117300     MOVE 'S' TO POSTSUM-OPKOD                                            
117400     CALL POSTSUM USING POSTSUM-PARM                                      
117500     .                                                                    
117600     EJECT                                                                
117700                                                                          
117800                                                                          
117900 S10-LAS-INFIL   SECTION.                                                 
118000     READ W47623 INTO IN-AREA                                             
118100      AT END                                                              
118200         MOVE HIGH-VALUE TO IN-AREA                                       
118300         SET END-OF-W47623 TO TRUE                                        
118400                                                                          
118500      NOT AT END                                                          
118600         MOVE 'W47623'       TO POSTSUM-FDNAMN                            
118700         MOVE 'W47626D1'     TO POSTSUM-DDNAMN2                           
118800         MOVE SPACE          TO POSTSUM-TRANSTYP                          
118900         CALL POSTSUM USING POSTSUM-PARM                                  
119000     END-READ                                                             
119100     .                                                                    
119200     EJECT                                                                
119210                                                                          
119300                                                                          
119400                                                                          
119500 S11-SKRIV-W47626  SECTION.                                               
119600                                                                          
119700     COMPUTE WS-NUMBER-OF-SEGMENTS =                                      
119800             WS-NUMBER-OF-SEGMENTS + 1                                    
119900                                                                          
120000     WRITE EDI-POST FROM UT-EDI-AREA                                      
120100                                                                          
120200     MOVE EDI-IDPTYP    TO POSTSUM-TRANSTYP                               
120300     MOVE 'W47626'      TO POSTSUM-FDNAMN                                 
120400     MOVE 'W47626D2'    TO POSTSUM-DDNAMN2                                
120500     CALL POSTSUM USING POSTSUM-PARM                                      
120600     .                                                                    
120700     EJECT                                                                
120800                                                                          
120895                                                                          
120900                                                                          
121800* --- IMS SEKTIONER ---                                                   
121900                                                                          
122200                                                                          
122300*IMS-GU-B201 SECTION.                                                     
122400*                                                                         
122500*    STRING 'WDB201  (IDGMT    =' W-IDGMT-X ')'                           
122600*         DELIMITED BY SIZE INTO SSA1                                     
122700*    MOVE '  GE' TO GODK-STATUSKODER                                      
122800*    CALL CBLTDLI USING GU WDB2-PCB DLI-IO-WDB201 SSA1                    
122900*    MOVE WDB2-STATUS-CODE TO STATUS-WS                                   
123000*    PERFORM IMS-STATUSKONTROLL                                           
123100*    .                                                                    
123200*    EJECT                                                                
123300                                                                          
123400                                                                          
123500 IMS-GU-E601 SECTION.                                                     
123600                                                                          
123700     STRING 'WDE601  (IDPRODNR =' W-IDPRODNR-X ')'                        
123800          DELIMITED BY SIZE INTO SSA1                                     
124100     MOVE '  GE' TO GODK-STATUSKODER                                      
124200     CALL CBLTDLI USING GU WDE6-PCB DLI-IO-WDE601 SSA1                    
124300     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
124400     PERFORM IMS-STATUSKONTROLL                                           
124500     .                                                                    
124600     EJECT                                                                
124700                                                                          
124710 IMS-GU-WDE101  SECTION.                                                  
124720                                                                          
124730     STRING 'WDE101  (IDSHIPM  =' W-IDSHIPM-X ')'                         
124740          DELIMITED BY SIZE INTO SSA1                                     
124750     MOVE '    ' TO GODK-STATUSKODER                                      
124760     CALL CBLTDLI USING GU WDE1-PCB DLI-IO-WDE101 SSA1                    
124770     MOVE WDE1-STATUS-CODE TO STATUS-WS                                   
124780     PERFORM IMS-STATUSKONTROLL                                           
124790     .                                                                    
124791     SKIP2                                                                
124792 IMS-GNP-WDE111 SECTION.                                                  
124793                                                                          
124794     MOVE 'WDE111 '          TO SSA1                                      
124795     MOVE '  GE' TO GODK-STATUSKODER                                      
124796     CALL CBLTDLI USING GNP WDE1-PCB DLI-IO-WDE111 SSA1                   
124797     MOVE WDE1-STATUS-CODE TO STATUS-WS                                   
124798     PERFORM IMS-STATUSKONTROLL                                           
124799     .                                                                    
124800     EJECT                                                                
124801 IMS-GNP-WDE121  SECTION.                                                 
124802                                                                          
124803     STRING 'WDE111  (WDE111KY =' W-IDDISTR-X ')'                         
124804          DELIMITED BY SIZE INTO SSA1                                     
124805     MOVE 'WDE121 '          TO SSA2                                      
124806     MOVE '  GE' TO GODK-STATUSKODER                                      
124807     CALL CBLTDLI USING GNP WDE1-PCB DLI-IO-WDE121 SSA1 SSA2              
124808     MOVE WDE1-STATUS-CODE TO STATUS-WS                                   
124809     PERFORM IMS-STATUSKONTROLL                                           
124810     .                                                                    
124811     SKIP2                                                                
124820                                                                          
124900 IMS-GU-WDGX4738 SECTION.                                                 
125000                                                                          
125100     STRING 'WDR101  (WDGXKEY  =' W-WDGXKEY-4738-X ')'                    
125200          DELIMITED BY SIZE INTO SSA1                                     
125300     MOVE   'WDGX4738'        TO SSA2                                     
125400     MOVE '  GE' TO GODK-STATUSKODER                                      
125500     CALL CBLTDLI USING GU 4738-PCB DLI-IO-WDGX4738 SSA1 SSA2             
125600     MOVE 4738-STATUS-CODE TO STATUS-WS                                   
125700     PERFORM IMS-STATUSKONTROLL                                           
125800     .                                                                    
125900     EJECT                                                                
126000                                                                          
126100                                                                          
126200 IMS-STATUSKONTROLL SECTION.                                              
126300                                                                          
126400     SET STATUS-IX TO 1                                                   
126500     SEARCH GODK-STATUS                                                   
126600       AT END                                                             
126700         STRING ' FELAKTIG STATUSKOD FRÅN IMS: ' STATUS-WS                
126800           DELIMITED BY SIZE INTO FELTEXT                                 
126900         DISPLAY FELTEXT                                                  
127000         CALL FELLOG                                                      
127100       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS                           
127200         CONTINUE                                                         
127300     END-SEARCH                                                           
127400     .                                                                    
