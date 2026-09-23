000100*COMPOPT STDSUB=YES                                                       
000200 ID DIVISION.                                                             
000400 PROGRAM-ID.     W475DGR.                                                 
000500 AUTHOR.         JAN-ERIK FRANTZEN.                                       
000600 DATE-WRITTEN.   AUGUSTI 1994.                                            
000700*                                                                         
000800*                                                                         
000900*    FUNKTION.                                                            
001000*        PROGRAMMET ÄR ETT SUBPROGRAM TILL W4053900 OCH                   
001100*        SKRIVER BLANKETT DGR FÖR FARLIGT GODS MED HJÄLP                  
001200*        AV PRINTPROGRAM W006PRR1.                                        
001300*                                                                         
001400*        PROGRAMMET LÄSER      WLGMTA (WDB2)                              
001500*        PROGRAMMET LÄSER      WL1165 (WDR2 HTYP 1165/1168)               
001600*        PROGRAMMET LÄSER      WL4513 (WDR4)                              
001700*                                                                         
001800*    UTDATA.                                                              
001900*        BLANKETTER:  DGR                                                 
002000*                                                                         
002010*    IMPORTANT.                                                           
002020*    4633590 - SEND SEAL,UN NUM TO FLS/MIC                                
002030*    IF PSN IS ADDED TO GODK-PSN,IT IS IMPORTANT TO ADD IN                
002040*    GOOD-PSN-DGR IN W4768000.                                            
002050*                                                                         
002100                                                                          
002200     SKIP3                                                                
002300 ENVIRONMENT DIVISION.                                                    
002400 DATA DIVISION.                                                           
002500     EJECT                                                                
002600 WORKING-STORAGE SECTION.                                                 
002700                                                                          
002800*    -- CHECKED BY WY2000                                                 
002900 77  IDPGM                       PIC X(08)   VALUE 'W475DGR '.            
003000 77  WS-ADRESS-DP                PIC X(50)                                
003100         VALUE 'CARPARTS.DAP.DISTRDOC'.                                   
003200 77  KDRC-DISPLAY                PIC Z(5).                                
003300                                                                          
003400*    --- ARBETSFÄLT FÖR FELMEDDELANDEN VID CALL ABEND/FELLOG              
003500 77  FELTEXT                     PIC X(80) VALUE SPACE.                   
003600                                                                          
003700 77  WZ04-001-IDCOM              PIC S9(9)   COMP VALUE +0.               
003800 77  JA                          PIC X       VALUE 'J'.                   
003900 77  NEJ                         PIC X       VALUE 'N'.                   
004000 77  SAMMA-ORDERNR               PIC X       VALUE 'N'.                   
004100 77  STRECK                      PIC X       VALUE '-'.                   
004200 77  IX1                         PIC S9(9)   VALUE +0  COMP SYNC.         
004300 77  IX2                         PIC S9(3)   VALUE +0  COMP-3.            
004400 77  IX3                         PIC S9(3)   VALUE +0  COMP-3.            
004500 77  IX4                         PIC S9(3)   VALUE +0  COMP-3.            
004600 77  TAB-IX                      PIC S9(3)   VALUE +0  COMP-3.            
004700 77  ONR-IX                      PIC S9(3)   VALUE +0  COMP-3.            
004800 77  STR-IX-1                    PIC S9(3)   VALUE +0  COMP-3.            
004900 77  STR-IX-2                    PIC S9(3)   VALUE +0  COMP-3.            
005000 77  MAX-IX                      PIC S9(9)   VALUE +998 COMP SYNC.        
005100 77  RAD-IX                      PIC S9(3)   VALUE +0  COMP-3.            
005200 77  MAX-RAD-IX                  PIC S9(3)   VALUE +17 COMP-3.            
005300 77  MAX-TAB-IX                  PIC S9(3)   VALUE +10 COMP-3.            
005400 77  MAX-ONR-IX                  PIC S9(3)   VALUE +10 COMP-3.            
005500 77  WS-SIDA-TOT                 PIC S9(3)   VALUE +0  COMP-3.            
005600 77  WS-SIDA                     PIC S9(3)   VALUE +0  COMP-3.            
005700 77  WS-RADER                    PIC S9(3)   VALUE +0  COMP-3.            
005800 77  WS-SUEQFG                   PIC 9(3)V9(4).                           
005900 77  WS-FORSTA-IDORDNR7          PIC 9(7)    VALUE ZERO.                  
006000 77  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
006100 77  PRT-EQUAL-53                PIC S9(3)   VALUE +853 COMP-3.           
006200 77  PRT-EQUAL-56                PIC S9(3)   VALUE +856 COMP-3.           
006300 77  PRT-EQUAL-60                PIC S9(3)   VALUE +860 COMP-3.           
006400 77  PRT-EQUAL-61                PIC S9(3)   VALUE +861 COMP-3.           
006500 77  PRT-EQUAL-63                PIC S9(3)   VALUE +863 COMP-3.           
006600 77  WS-PSN-90                   PIC 9(3)    VALUE 90.                    
006700 77  WS-PSN-91                   PIC 9(3)    VALUE 91.                    
006800 77  WS-PSN-93                   PIC 9(3)    VALUE 93.                    
006900 77  WS-FIBREBOARDBOX            PIC X(17)   VALUE                        
007000                                  'fibreboard box X '.                    
007100 77  WS-PLYWOODBOX               PIC X(14)   VALUE                        
007200                                  'plywood box X '.                       
007300 77  WS-ENGINE                   PIC X(09)   VALUE                        
007400                                  'engine X '.                            
007500 77  WS-IDSPRAK-SE               PIC X(2)    VALUE 'SE'.                  
007600 77  WS-IDSPRAK-GB               PIC X(2)    VALUE 'GB'.                  
007700                                                                          
007800 77  DC-FG-SW                    PIC X       VALUE 'N'.                   
007900     88 DC-FG-OK                             VALUE 'J'.                   
008000                                                                          
008100 77  SPEC-TEXT-90-91-SW          PIC X       VALUE 'N'.                   
008200     88 SPEC-TEXT-90-91-OK                   VALUE 'J'.                   
008300                                                                          
008400 EJECT                                                                    
008500*      --- VALID IDDC CODES                                               
008600*                                                                         
008700*01    -COPY WWDC99                                                       
008800       EJECT                                                              
008900 01  WS-VLFG                     PIC 9(4)V9(3).                           
009000 01  FILLER REDEFINES WS-VLFG.                                            
009100     03  WS-VLFG-1-4             PIC 9(4).                                
009200     03  WS-VLFG-5               PIC 9.                                   
009300     03  WS-VLFG-6-7             PIC 99.                                  
009400                                                                          
009500 01  WS-DATUM-AAMMDD             PIC 9(6).                                
009600 01  FILLER REDEFINES WS-DATUM-AAMMDD.                                    
009700     03  WS-AA                   PIC 9(2).                                
009800     03  WS-MM                   PIC 9(2).                                
009900     03  WS-DD                   PIC 9(2).                                
010000                                                                          
010100 01  WS-DATUM-AAMMDD-S           PIC X(10).                               
010200 01  FILLER REDEFINES WS-DATUM-AAMMDD-S.                                  
010300     03  WS-AA1-S                PIC 9(2).                                
010400     03  WS-AA-S                 PIC 9(2).                                
010500     03  WS-STRECK1              PIC X(1).                                
010600     03  WS-MM-S                 PIC 9(2).                                
010700     03  WS-STRECK2              PIC X(1).                                
010800     03  WS-DD-S                 PIC 9(2).                                
010900                                                                          
011000 01  WS-DATUM-DDMMAA             PIC X(6).                                
011100 01  FILLER REDEFINES WS-DATUM-DDMMAA.                                    
011200     03  WS-MM-NA                PIC 9(2).                                
011300     03  WS-DD-NA                PIC 9(2).                                
011400     03  WS-AA-NA                PIC 9(2).                                
011500                                                                          
011600 01  GENERELLA-SUBPROGRAM.                                                
011700     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
011800     03  W006PRR1                PIC X(8)    VALUE 'W006PRR1'.            
011900     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
012000     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
012100     03  WZ01SUB                 PIC X(8)    VALUE 'WZ01SUB '.            
012200     03  WZ01SEND                PIC X(8)    VALUE 'WZ01SEND'.            
012300     03  WL10WBDC                PIC X(8)    VALUE 'WL10WBDC'.            
012400                                                                          
012500                                                                          
012600*    --- PARAMETERS FOR SUBPROGRAM WL10WBDC                               
012700                                                                          
012800 01  FILLER                      PIC X(16)  VALUE 'WL10WBDC AREA'.        
012900*01  -COPY WL10WBDC                                                       
013000                                                                          
013100 77  RKOD-ABEND-MED-DUMP        PIC S9(4)   VALUE +33 COMP SYNC.          
013200                                                                          
013300*    --- POSTBESKRIVNINGAR TILL D&P                                       
013400 01  DOC-HEAD-AREA.                                                       
013500*    03 -COPY W476FG1  -PRE HUVUD-                                        
013600 01  DOC-LINE-AREA.                                                       
013700*    03 -COPY W476FG2  -PRE RAD-                                          
013800 01  DOC-FOOT-AREA.                                                       
013900*    03 -COPY W476FG3  -PRE FOT-                                          
014000                                                                          
014100 01  FILLER                      PIC X(16)   VALUE 'SEND-CONTROL'.        
014200*01  -COPY WZ01SEND                                                       
014300     EJECT                                                                
014400 01  PRINTAREA-START             PIC X(24)   VALUE                        
014500                                 'PRINTAREA-START'.                       
014600 01  HDR-AREA.                                                            
014700*    03  -COPY WZ01REQU  -PRE HDR-                                        
014800*    03  -COPY WZ04HDR                                                    
014900                                                                          
015000     EJECT                                                                
015100*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
015200*                                                                         
015300 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
015400     SKIP3                                                                
015500                                                                          
015600 01  NYCKLAR-TILL-DLI.                                                    
015700*--- TILL WDR4 (WL4513).                                                  
015800                                                                          
015900     03  W-4513-X.                                                        
016000         05  W-IDHTYP            PIC X(4)    VALUE '4513'.                
016100         05  FILLER              PIC X(26)   VALUE LOW-VALUE.             
016200                                                                          
016300     03  W-DASKEPPN-X.                                                    
016400         05  W-DASKEPPN          PIC 9(8)    VALUE ZERO.                  
016500                                                                          
016600     03  W-4516-X.                                                        
016700         05  W-IDDC-4516         PIC X(2)    VALUE SPACE.                 
016800         05  W-IDSKEPPN-4516     PIC S9(7)   VALUE ZERO COMP-3.           
016900         05  W-IDDISTR-4516      PIC S9(5)   VALUE ZERO COMP-3.           
017000         05  W-IDKUNDNR-4516     PIC S9(7)   VALUE ZERO COMP-3.           
017100                                                                          
017200*--- Till WDB2 (WLGMTA).                                                  
017300                                                                          
017400     03  W-IDGMT-X.                                                       
017500         05  W-IDDISTR-WDB2      PIC S9(5)   VALUE ZERO COMP-3.           
017600         05  W-IDKUNDNR-WDB2     PIC S9(7)   VALUE ZERO COMP-3.           
017700                                                                          
017800*--- Till WDR2 (WL1165) Htyp 1165/1168                                    
017900                                                                          
018000     03  W-1165-X.                                                        
018100         05  W-IDHTYP            PIC X(4)    VALUE '1165'.                
018200         05  W-IDPSN             PIC 9(3).                                
018300         05  W-IDSPRAK           PIC X(2).                                
018400         05  W-LOW-VALUE         PIC X(21)   VALUE LOW-VALUE.             
018500                                                                          
018600     03  W-KDFGTRP-X.                                                     
018700         05  W-KDFGTRP           PIC 9(2)    VALUE 1.                     
018800                                                                          
018900*--- Till WDE6                                                            
019000                                                                          
019100     03    W-IDPRODNR-X.                                                  
019200         05    W-IDPRODNR        PIC S9(7)   VALUE ZERO  COMP-3.          
019300                                                                          
019400     03    W-IDKOLLI-X.                                                   
019500         05    W-IDKOLLI         PIC S9(5)   VALUE ZERO  COMP-3.          
019600                                                                          
019700                                                                          
019800*--- Till WDB6                                                            
019900                                                                          
020000     03  W-IDDC-B6-X.                                                     
020100         05 W-IDDC-B6                  PIC X(2).                          
020200     EJECT                                                                
020300*    --- PRINTPARAMETRAR (PRINTNING MED ÅTERSTART)                        
020400*01  FILLER   -COPY W006PRAR                                              
020500     EJECT                                                                
020600                                                                          
020700 01  PRT-AREA.                                                            
020800     03 WS-PRT-IDPRTLST          PIC X(8)  VALUE SPACE.                   
020900     03 WS-PRT-IDLIST.                                                    
021000        05 WS-IDLIST             PIC X(4)  VALUE 'DGR'.                   
021100        05 WS-PRT-IDDISTR        PIC 9(4).                                
021200        05 WS-PRT-KDFRAKT        PIC 9(2).                                
021300     03 WS-WEB-IDLIST.                                                    
021400        05 WS-WEB-IDLIST         PIC X(3)  VALUE 'DGR'.                   
021500        05 WS-WEB-IDDISTR        PIC 9(4).                                
021600        05 WS-WEB-IDTRPTNR       PIC 9(3).                                
021700     03 WS-PRT-LISTRAD.                                                   
021800        05 FILLER                PIC X(2)  VALUE SPACE.                   
021900        05 WS-RAD                PIC X(78).                               
022000     03 WS-PRT-DUMMY             PIC X(1).                                
022100                                                                          
022200 01  FILLER                      PIC X(80)   VALUE ALL 'G'.               
022300*    --- BLANKETT-TABELL                                                  
022400 01  BLANKETT-DATA-TABELL.                                                
022500    03   BLANKETT-DATA OCCURS 1000.                                       
022600        05 BLK-IDKOLLI                PIC S9(5).                          
022700        05 BLK-SUEQFG                 PIC S9(3)V9(4).                     
022800        05 BLK-KDKOLLI                PIC X(8).                           
022900        05 BLK-RADER                  PIC S9(3).                          
023000        05 FILLER OCCURS 10.                                              
023100           07 BLK-IDPSN            PIC S9(3).                             
023200           07 BLK-VKART-FG         PIC S9(7).                             
023300           07 BLK-VLFG             PIC S9(4)V9(3).                        
023400     EJECT                                                                
023500 01  ARB-RAD                      PIC X(90) VALUE SPACE.                  
023600     SKIP2                                                                
023700 01  ARB-RAD-1.                                                           
023800     03   FILLER                  PIC X(2)  VALUE '//'.                   
023900     03   ARB-VLFG-1              PIC Z(2)9.9(1).                         
024000     03   ARB-SORT-1              PIC X(4)  VALUE SPACE.                  
024100     03   FILLER                  PIC X(2)  VALUE '//'.                   
024200                                                                          
024300 01  ARB-RAD-2.                                                           
024400     03   FILLER                  PIC X(5) VALUE SPACE.                   
024500     03   ARB-ANTAL               PIC X(2)  VALUE '1 '.                   
024600     03   ARB-BOX                 PIC X(17)  VALUE SPACE.                 
024700     03   ARB-VLFG-2              PIC Z(2)9.9(1).                         
024800     03   ARB-SORT-2              PIC X(4)  VALUE SPACE.                  
024900                                                                          
025000 01  FILLER                       PIC X(80)   VALUE ALL 'H'.              
025100                                                                          
025200 01  RAD00.                                                               
025300     03   FILLER                  PIC X(51) VALUE SPACE.                  
025400     03   FILLER                  PIC X(24) VALUE                         
025500                                  '*** BOKNINGSUNDERLAG ***'.             
025600 01  RAD0.                                                                
025700     03   FILLER                  PIC X(51) VALUE SPACE.                  
025800     03   DGR-IDFRASED            PIC X(15).                              
025900                                                                          
026000 01  RAD1.                                                                
026100     03   FILLER                  PIC X(43) VALUE SPACE.                  
026200     03   DGR-SIDA                PIC Z(3).                               
026300     03   FILLER                  PIC X(4) VALUE SPACE.                   
026400     03   DGR-SIDA-TOT            PIC Z(3).                               
026500                                                                          
026600 01  RAD2.                                                                
026700     03   FILLER                  PIC X(57) VALUE SPACE.                  
026800     03   DGR-IDORDNR7            PIC Z(7).                               
026900                                                                          
027000 01  RAD3.                                                                
027100     03   FILLER                  PIC X(5) VALUE SPACE.                   
027200     03   DGR-BEGMT-RAD1          PIC X(35).                              
027300                                                                          
027400 01  RAD3A.                                                               
027500     03   FILLER                  PIC X(5) VALUE SPACE.                   
027600     03   DGR-BEGMT-RAD2          PIC X(35).                              
027700                                                                          
027800 01  RAD4.                                                                
027900     03   FILLER                  PIC X(5) VALUE SPACE.                   
028000     03   DGR-ADGMT-GATA          PIC X(35).                              
028100                                                                          
028200 01  RAD5.                                                                
028300     03   FILLER                  PIC X(5) VALUE SPACE.                   
028400     03   DGR-ADGMT-PADR          PIC X(35).                              
028500                                                                          
028600 01  RAD6.                                                                
028700     03   FILLER                  PIC X(5) VALUE SPACE.                   
028800     03   DGR-ADGMT-LAND          PIC X(35).                              
028900                                                                          
029000 01  RAD7.                                                                
029100     03   FILLER                  PIC X(25) VALUE SPACE.                  
029200     03   DGR-AIRPORT             PIC X(12) VALUE 'GOTHENBURG'.           
029300*---- KOLLIRADERNA                                                        
029400 01  RAD8.                                                                
029500     03   FILLER                  PIC X(5)  VALUE SPACE.                  
029600     03   DGR-BEPSN               PIC X(75).                              
029700*---                                                                      
029800 01  RAD9.                                                                
029900     03   FILLER                  PIC X(5)  VALUE SPACE.                  
030000     03   FILLER                  PIC X(18) VALUE                         
030100     'All packed in one '.                                                
030200     03   RAD9-BOX                PIC X(14)  VALUE SPACE.                 
030300                                                                          
030400 01  RAD10.                                                               
030500     03   FILLER                  PIC X(5)  VALUE SPACE.                  
030600     03   FILLER                  PIC X(2)  VALUE 'Q='.                   
030700     03   DGR-SUEQFG              PIC 9(1).9(1).                          
030800                                                                          
030900 01  RAD10NA.                                                             
031000     03   FILLER                  PIC X(5)  VALUE SPACE.                  
031100     03   FILLER                  PIC X(48) VALUE                         
031200     'Safety devices in accordance with 49 CFR 173.166'.                  
031300                                                                          
031400 01  RAD11NA.                                                             
031500     03   FILLER                  PIC X(5)  VALUE SPACE.                  
031600     03   FINALRAD.                                                       
031700       05 FINAL-IDORDNR-GRP OCCURS 10 TIMES.                              
031800         07 FINAL-IDORDNR7        PIC 9(7).                               
031900         07 FILLER                PIC X     VALUE SPACE.                  
032000                                                                          
032100 01  RAD11.                                                               
032200     03   FILLER                  PIC X(5)  VALUE SPACE.                  
032300     03   FILLER                  PIC X(32) VALUE                         
032400     'PREPARED IN ACCORDANCE WITH ICAO'.                                  
032500                                                                          
032600 01  RAD12.                                                               
032700     03   FILLER                  PIC X(5)  VALUE SPACE.                  
032800     03   FILLER                  PIC X(35) VALUE                         
032900     '24-hour emergency contact telephone'.                               
033000     03   FILLER                  PIC X(32) VALUE                         
033100     ' number: +46 31 94 61 10        '.                                  
034000                                                                          
034100 01  RAD13.                                                               
034200     03   FILLER                  PIC X(5)  VALUE SPACE.                  
034300     03   FILLER                  PIC X(35) VALUE SPACE.                  
034400     03   FILLER                  PIC X(32) VALUE SPACE.                  
034500                                                                          
034600 01  RAD14.                                                               
034700     03   FILLER                  PIC X(5)  VALUE SPACE.                  
034800     03   FILLER                  PIC X(28) VALUE                         
034900     'Transport enligt/Carriage in'.                                      
035000     03   FILLER                  PIC X(31) VALUE                         
036000     ' accordance with 1.1.4.2.1, ADR'.                                   
037000                                                                          
037100 01  RAD16-EU.                                                            
037200     03   FILLER                  PIC X(51) VALUE SPACE.                  
037300     03   DGR-SIGN-ORT-SE         PIC X(11) VALUE 'Gothenburg '.          
037400     03   DGR-SIGN-DATUM-SE       PIC X(10).                              
037500                                                                          
037600 01  RAD16A-NAPF.                                                         
037700     03   FILLER                  PIC X(51) VALUE SPACE.                  
037800     03   DGR-SIGN-ORT-OVR        PIC X(20) VALUE SPACE.                  
037900                                                                          
038000 01  RAD16B-NAPF.                                                         
038100     03   FILLER                  PIC X(51) VALUE SPACE.                  
038200     03   DGR-SIGN-DATUM-NA       PIC X(6).                               
038300                                                                          
038400 01  RAD17.                                                               
038500     03   FILLER                  PIC X(5)  VALUE SPACE.                  
038600     03   FILLER                  PIC X(45) VALUE                         
038700          'Re-ship the consignment, at the shippers cost'.                
038800     03   FILLER                  PIC X(17) VALUE                         
038900          ' and risk, if the'.                                            
039000 01  RAD18.                                                               
039100     03   FILLER                  PIC X(5)  VALUE SPACE.                  
039200     03   FILLER                  PIC X(53) VALUE                         
039300          'shipment is not cleared and received by the consignee'.        
039400     03   FILLER                  PIC X(10) VALUE                         
039500          ' within 15'.                                                   
039600 01  RAD19.                                                               
039700     03   FILLER                  PIC X(5)  VALUE SPACE.                  
039800     03   FILLER                  PIC X(49) VALUE                         
039900          'working days from the arrival of the consignment.'.            
040000                                                                          
040100     EJECT                                                                
040200***  OBS  ***  OM MAN LÄGGER TILL GODK-PSN-er (10 TOM 99)                 
040300*              MÅSTE MAN KOLLA ATT DE FINNS UPPLAGDA PÅ                   
040400*              ALLA SPRÅKEN!                                              
040500*              KOLLA PÅ BILD 1132                                         
040600*              IDPSN + SPRÅK (SE, DE, FR, ES, IT, PL, NL, FI)             
040700***  OBS  ***  OBS  *******************************************           
040800 01  GODK-PSN-SW                  PIC 9(3).                               
040900     88  GODK-PSN                           VALUE 010                     
041000                                                  020 021 022 023         
041100                                                  024 025                 
041200                                                  031 032 033 034         
041300                                                  035 036 037 038         
041400                                                  039                     
041500                                                  040 041 042 043         
041600                                                  044 045 046 047         
041700                                                  048 049                 
041800                                                  050 051 052 053         
041900                                                  054 055 056 057         
042000                                                  058 059                 
042100                                                  061 062 063 064         
042200                                                  065 THRU 069            
042300                                                  070 THRU 075            
042400                                                  080 081 088 089         
042500                                                  090 091 092 093         
042600                                                  094 THRU 099            
042700                                                  110 THRU 129            
042800                                                  133                     
042900                                                  135 THRU 138            
043000                                                  140                     
043100                                                  143 THRU 147            
043200                                                  149 THRU 332            
043300                                                  334 THRU 399.           
043400                                                                          
043500 01  SORT-SW                      PIC 9(3).                               
043600     88  SORT-KG                            VALUE 010                     
043700                                                  020 THRU 024            
043800                                                  054                     
043900                                                  080 089                 
044000                                                  090 091 093             
044100                                                  094 096 097             
044200                                                  098 099.                
044300                                                                          
044400 01  KDKOLLI-SW                   PIC X(8).                               
044500     88  FIBREBOARDBOX                       VALUE '1       '             
044600                                                   '27      '             
044700                                                   '0402    '             
044800                                                   '0403    '             
044900                                                   '0408    '             
045000                                                   '2207    '             
045100                                                   '2209    '             
045200                                                   '2231    '             
045300                                                   '2629    '             
045400                                                   '2631    '             
045500                                                   '2636    '             
045600                                                   '2639    '             
045700                                                   '3278    '             
045800                                                   '4454    '             
045900                                                   '4457    '             
046000                                                   '4460    '             
046100                                                   '4464    '             
046200                                                   '4506    '             
046300                                                   '4526    '             
046400                                                   '4548    '             
046500                                                   '5513    '             
046600                                                   '5543    '             
046700                                                   '5586    '             
046800                                                   '6111    '             
046900                                                   '6120    '             
047000                                                   '6255    '             
047100                                                   '6444    '             
047200                                                   '6451    '             
047300                                                   '7135    '             
047400                                                   '7136    '             
047500                                                   '7137    '             
047600                                                   '7138    '             
047700                                                   '7139    '             
047800                                                   '8789    '             
047900                                                   '9702    '             
048000                                                   '9703    '             
048100                                                   '9778    '             
048200                                                   'SP02    '             
048300                                                   'SP10    '.            
048400                                                                          
048500     88  PLYWOODBOX                          VALUE '1130    '             
048600                                                   '1131    '             
048700                                                   '1183    '             
048800                                                   '1635    '.            
048900                                                                          
049000 01  FORTSAETTNING-SW             PIC X.                                  
049100     88  FORTSAETTNING                       VALUE 'J'.                   
049200     SKIP2                                                                
049300                                                                          
049400 01  SKRIV-EQ-VARDE-SW            PIC X.                                  
049500     88  SKRIV-EQ-VARDE                      VALUE 'J'.                   
049600     SKIP2                                                                
049700                                                                          
049800 01  TEST-IDDISTR                 PIC 9(5)   COMP-3.                      
049900     SKIP3                                                                
050000*01  FILLER   -COPY WWDIST07    -RED TEST-IDDISTR.                        
050100     SKIP3                                                                
050200*01  FILLER   -COPY WWDIST32    -RED TEST-IDDISTR.                        
050300     SKIP3                                                                
050400*01  FILLER   -COPY WWDIST35    -RED TEST-IDDISTR.                        
050500     SKIP3                                                                
050600*01  FILLER   -COPY WWDIST66    -RED TEST-IDDISTR.                        
050700     EJECT                                                                
050800*01  FILLER   -COPY WWDIST67    -RED TEST-IDDISTR.                        
050900     EJECT                                                                
051000                                                                          
051100 01  FILLER                      PIC X(08)   VALUE 'FG-KUND'.             
051200*    -COPY WWKUND15.                                                      
051300                                                                          
051400*    --- STATUS-KOD FRÅN IMS                                              
051500 01  STATUS-WS                   PIC XX.                                  
051600     88  SEGMENT-FINNS                       VALUE '  '.                  
051700     88  SEGMENT-FINNS-REDAN                 VALUE 'II'.                  
051800     88  SEGMENT-SAKNAS                      VALUE 'GE'.                  
051900     88  SEGMENT-SLUT                        VALUE 'GB'.                  
052000     SKIP2                                                                
052100 01  GODK-STATUSKODER.                                                    
052200     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
052300     SKIP3                                                                
052400                                                                          
052500 01  SSA1                        PIC X(150).                              
052600 01  SSA2                        PIC X(150).                              
052700                                                                          
052800     EJECT                                                                
052900*    --- IMS FUNKTIONSKODER                                               
053000*01  -COPY W0003                                                          
053100     EJECT                                                                
053200*    ---  DLI INPUT-OUTPUT AREA                                           
053300 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-AREA'.         
053400     SKIP3                                                                
053500 01  DLI-IO-AREA.                                                         
053600     03  IO-AREA                 PIC X(800)  VALUE SPACE.                 
053700     SKIP3                                                                
053800     03  WLWDE611 REDEFINES IO-AREA.                                      
053900*        05  -COPY WDE611                                                 
054000     SKIP3                                                                
054100     03  WL116512 REDEFINES IO-AREA.                                      
054200*        05  -COPY WDGX1168  -PRE 1165-                                   
054300     EJECT                                                                
054700 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-4514'.         
054800 01  DLI-IO-AREA-4514.                                                    
054900     03  WL451311.                                                        
055000*        05  -COPY WDGX4514                                               
055100     SKIP3                                                                
055200 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-4516'.         
055300     SKIP3                                                                
055400 01  DLI-IO-AREA-4516.                                                    
055500     03  WL451321.                                                        
055600*        05  -COPY WDGX4516                                               
055700     SKIP3                                                                
055800 01  FILLER                      PIC X(16)  VALUE 'DLI-IO-B601'.          
055900 01  DLI-IO-AREA-B601.                                                    
056000     03  WDB601.                                                          
056100*        05  -COPY WDB601                                                 
056200     EJECT                                                                
056201 01  FILLER                      PIC X(16)  VALUE 'DLI-IO-B201'.          
056202 01  DLI-IO-AREA-B201.                                                    
056203*    03  -COPY WDB201                                                     
056204     EJECT                                                                
056300 LINKAGE SECTION.                                                         
056400                                                                          
056500*                                                                         
056600*   -COPY W475DGR -PRE LINK-                                              
056700*                                                                         
056800     EJECT                                                                
056900*01  -COPY W0009   -PRE ALT-                                              
057000     EJECT                                                                
057400*01  -COPY W0008  -PRE LISB-                                              
057500     05  FILLER                  PIC X.                                   
057600                                                                          
057700*01  -COPY W0008  -PRE 4513-                                              
057800     05  FILLER                  PIC X.                                   
057900     EJECT                                                                
058000*01  -COPY W0008  -PRE 1165-                                              
058100     05  FILLER                  PIC X.                                   
058200                                                                          
058300*01  -COPY W0008  -PRE GMTA-                                              
058400     05  FILLER                  PIC X.                                   
058500     EJECT                                                                
058600*01  -COPY W0008  -PRE WDE6-                                              
058700     05  FILLER                  PIC X.                                   
058800     EJECT                                                                
058900*01  -COPY W0008  -PRE WDB6-                                              
059000     05  FILLER                  PIC X.                                   
059100     EJECT                                                                
059200 PROCEDURE DIVISION  USING LINK-W475DGR ALT-PCB                           
059300                                       LISB-PCB  4513-PCB                 
059400                                       1165-PCB GMTA-PCB                  
059500                                       WDE6-PCB WDB6-PCB.                 
059600 MAIN SECTION.                                                            
059700     PERFORM A-INIT                                                       
059800     PERFORM B-LAES-DATA                                                  
059900     IF FORTSAETTNING                                                     
060000        IF WBDC-FLWEBDC = JA                                              
060100          PERFORM C-KOLLA-DC                                              
060200                                                                          
060300          IF DC-FG-OK                                                     
060400           PERFORM F-SKRIV-BLANKETT-WEB                                   
060500          END-IF                                                          
060600                                                                          
060700        ELSE                                                              
060800           PERFORM G-SKRIV-BLANKETT                                       
060900        END-IF                                                            
061000     END-IF                                                               
061100                                                                          
061200     MOVE ZERO                     TO RETURN-CODE                         
061300     GOBACK                                                               
061400     .                                                                    
061500     EJECT                                                                
061600 A-INIT SECTION.                                                          
061700                                                                          
061800     PERFORM AA-NOLLA-TABELL                                              
061900     PERFORM AB-KOLLA-WEB-DC                                              
062000                                                                          
062100     MOVE NEJ                      TO SPEC-TEXT-90-91-SW                  
062200                                                                          
062300     MOVE LINK-IDDISTR             TO W-IDDISTR-4516                      
062400                                      W-IDDISTR-WDB2                      
062500                                                                          
062600     MOVE LINK-IDKUNDNR            TO W-IDKUNDNR-4516                     
062700                                      W-IDKUNDNR-WDB2                     
062800                                                                          
062900     MOVE LINK-IDDC                TO W-IDDC-4516                         
063000                                      W-IDDC-B6                           
063100                                      WS-IDDC                             
063200                                                                          
063300     MOVE LINK-IDSKEPPN            TO W-IDSKEPPN-4516                     
063400                                                                          
063500     MOVE LINK-DASKEPPN            TO W-DASKEPPN                          
063600                                                                          
063700     ACCEPT DAGENS-DATUM FROM DATE                                        
063800     MOVE DAGENS-DATUM             TO WS-DATUM-AAMMDD                     
063900                                                                          
064000     MOVE WS-AA                    TO WS-AA-S                             
064100     MOVE WS-MM                    TO WS-MM-S                             
064200     MOVE WS-DD                    TO WS-DD-S                             
064300     MOVE 20                       TO WS-AA1-S                            
064400     MOVE STRECK                   TO WS-STRECK1                          
064500                                      WS-STRECK2                          
064600     MOVE WS-DATUM-AAMMDD-S        TO DGR-SIGN-DATUM-SE                   
064700                                                                          
064800     IF NDC-NA                                                            
064900       MOVE WS-AA                  TO WS-AA-NA                            
065000       MOVE WS-MM                  TO WS-MM-NA                            
065100       MOVE WS-DD                  TO WS-DD-NA                            
065200       MOVE WS-DATUM-DDMMAA        TO DGR-SIGN-DATUM-NA                   
065300       IF NDC-US-RU                                                       
065400         MOVE 'NEW YORK'           TO DGR-AIRPORT                         
065500       ELSE                                                               
065600         IF NDC-US-LA                                                     
065700           MOVE 'LOS ANGELES'      TO DGR-AIRPORT                         
065800         ELSE                                                             
065900           MOVE 'TORONTO'          TO DGR-AIRPORT                         
066000         END-IF                                                           
066100       END-IF                                                             
066200     ELSE                                                                 
066300       IF NDC-JP                                                          
066400         MOVE 'NAGOYA'             TO DGR-AIRPORT                         
066500       ELSE                                                               
066600         IF NDC-AU                                                        
066700           MOVE 'SYDNEY'           TO DGR-AIRPORT                         
066800         ELSE                                                             
066900           IF LDC-FR-3P                                                   
067000             MOVE 'PARIS'          TO DGR-AIRPORT                         
067100           END-IF                                                         
067200         END-IF                                                           
067300       END-IF                                                             
067400     END-IF                                                               
067500     .                                                                    
067600     EJECT                                                                
067700 AA-NOLLA-TABELL SECTION.                                                 
067800                                                                          
067900     MOVE +1                       TO IX1                                 
068000     PERFORM UNTIL IX1 > MAX-IX                                           
068100        MOVE ZERO                  TO BLK-IDKOLLI(IX1)                    
068200                                      BLK-SUEQFG (IX1)                    
068300                                      BLK-RADER  (IX1)                    
068400        MOVE SPACE                 TO BLK-KDKOLLI(IX1)                    
068500        MOVE +1                    TO IX2                                 
068600        PERFORM 10 TIMES                                                  
068700           MOVE ZERO               TO BLK-IDPSN   (IX1, IX2)              
068800                                      BLK-VKART-FG(IX1, IX2)              
068900                                      BLK-VLFG    (IX1, IX2)              
069000           ADD +1                  TO IX2                                 
069100        END-PERFORM                                                       
069200        ADD +1                     TO IX1                                 
069300     END-PERFORM                                                          
069400                                                                          
069500     MOVE +1                       TO TAB-IX                              
069600     PERFORM UNTIL TAB-IX > MAX-TAB-IX                                    
069700        MOVE ZERO                  TO FINAL-IDORDNR7 (TAB-IX)             
069800        ADD +1                     TO TAB-IX                              
069900     END-PERFORM                                                          
070000     MOVE +1                       TO TAB-IX                              
070100     MOVE ZERO                     TO ONR-IX                              
070200                                                                          
070300     .                                                                    
070400     EJECT                                                                
070500 AB-KOLLA-WEB-DC SECTION.                                                 
070600                                                                          
070700     MOVE LINK-IDDC       TO WBDC-IDDC                                    
070800     CALL WL10WBDC USING WBDC-AREA                                        
070900                                                                          
071000     .                                                                    
071100     EJECT                                                                
071200 B-LAES-DATA SECTION.                                                     
071300                                                                          
071400     PERFORM BA-LAES-SKEPP-KOLLI                                          
071500                                                                          
071600     IF FORTSAETTNING                                                     
071700        PERFORM BB-LAES-GMTA                                              
071800     END-IF                                                               
071900     .                                                                    
072000     EJECT                                                                
072100 BA-LAES-SKEPP-KOLLI SECTION.                                             
072200                                                                          
072300     MOVE +1                     TO IX1                                   
072400     MOVE NEJ                    TO FORTSAETTNING-SW                      
072500                                                                          
072600     PERFORM IMS-GU-WL451311                                              
072700     PERFORM IMS-GNP-WL451321                                             
072800                                                                          
072900     PERFORM UNTIL SEGMENT-SAKNAS OR SEGMENT-SLUT                         
073000       MOVE 4516-IDPRODNR        TO W-IDPRODNR                            
073100       MOVE 4516-IDKOLLI         TO W-IDKOLLI                             
073200                                                                          
073300       IF NDC-NA                                                          
073400         PERFORM BAA-LAGG-UT-ORDERNR                                      
073500       END-IF                                                             
073600                                                                          
073700       MOVE SPACE                TO DGR-IDFRASED                          
073800       PERFORM IMS-GET-WDE6                                               
073900                                                                          
074000       IF KOLLI-IDPSN(1)         > ZERO                                   
074100         MOVE +1               TO IX2                                     
074200         PERFORM UNTIL IX2 > 9                                            
074300           IF KOLLI-IDPSN(1)  > ZERO                                      
074400             MOVE KOLLI-IDPSN (IX2)                                       
074500                     TO GODK-PSN-SW                                       
074600             IF GODK-PSN                                                  
074700                MOVE JA TO FORTSAETTNING-SW                               
074800                IF IX2 = 1                                                
074900                   ADD 4  TO BLK-RADER  (IX1)                             
075000                ELSE                                                      
075100                   ADD 4  TO BLK-RADER  (IX1)                             
075200                END-IF                                                    
075300                                                                          
075400                MOVE KOLLI-IDKOLLI                                        
075500                          TO BLK-IDKOLLI(IX1)                             
075600                MOVE KOLLI-KDKOLLI                                        
075700                          TO BLK-KDKOLLI(IX1)                             
075800                MOVE KOLLI-SUEQFG                                         
075900                          TO BLK-SUEQFG (IX1)                             
076000                                                                          
076100                IF BLK-IDPSN (IX1, IX2) = ZERO                            
076200                   MOVE KOLLI-IDPSN (IX2)                                 
076300                          TO BLK-IDPSN(IX1, IX2)                          
076400                   MOVE KOLLI-VKART-FG(IX2)                               
076500                          TO BLK-VKART-FG(IX1, IX2)                       
076600                   MOVE KOLLI-VLFG(IX2)                                   
076700                          TO BLK-VLFG(IX1, IX2)                           
076800                   ADD +1 TO IX1                                          
076900                END-IF                                                    
077000             END-IF                                                       
077100           END-IF                                                         
077200           ADD +1      TO IX2                                             
077300         END-PERFORM                                                      
077400       END-IF                                                             
077500       PERFORM IMS-GNP-WL451321                                           
077600     END-PERFORM                                                          
077700                                                                          
077800     .                                                                    
077900     EJECT                                                                
078000 BAA-LAGG-UT-ORDERNR SECTION.                                             
078100                                                                          
078200     IF ONR-IX = 0                                                        
078300       MOVE 4516-IDKUNDRF(1:7) TO DGR-IDORDNR7                            
078400                                  WS-FORSTA-IDORDNR7                      
078500       ADD +1                  TO ONR-IX                                  
078600     ELSE                                                                 
078700       IF 4516-IDKUNDRF(1:7) NOT = WS-FORSTA-IDORDNR7                     
078800         MOVE +1               TO TAB-IX                                  
078900         MOVE NEJ              TO SAMMA-ORDERNR                           
079000         PERFORM UNTIL TAB-IX > MAX-TAB-IX                                
079100                       OR SAMMA-ORDERNR = JA                              
079200           IF 4516-IDKUNDRF(1:7) = FINAL-IDORDNR7(TAB-IX)                 
079300             MOVE JA           TO SAMMA-ORDERNR                           
079400           END-IF                                                         
079500           ADD +1              TO TAB-IX                                  
079600         END-PERFORM                                                      
079700         IF SAMMA-ORDERNR = NEJ                                           
079800           IF ONR-IX < MAX-ONR-IX + 1                                     
079900             MOVE 4516-IDKUNDRF(1:7)                                      
080000                               TO FINAL-IDORDNR7 (ONR-IX)                 
080100             ADD +1            TO ONR-IX                                  
080200           END-IF                                                         
080300         END-IF                                                           
080400       END-IF                                                             
080500     END-IF                                                               
080600     .                                                                    
080700     EJECT                                                                
080800 BB-LAES-GMTA SECTION.                                                    
080900                                                                          
081000     PERFORM IMS-GET-GMTA01                                               
081100                                                                          
081200*    IF GMT-FLSAMFAK = JA AND CDC                                         
081300*      MOVE SPACE                  TO DGR-BEGMT-RAD1                      
081400*                                     DGR-ADGMT-GATA                      
081500*                                     DGR-ADGMT-PADR                      
081600*                                     DGR-ADGMT-LAND                      
081700*    ELSE                                                                 
081800       MOVE GMT-BEGMT-RAD1         TO DGR-BEGMT-RAD1                      
081900       MOVE SPACE                  TO DGR-BEGMT-RAD2                      
082000       IF GMT-BEGMT-RAD2 > SPACE                                          
082100         MOVE GMT-BEGMT-RAD2       TO DGR-BEGMT-RAD2                      
082200       END-IF                                                             
082300       MOVE GMT-ADGMT-GATA         TO DGR-ADGMT-GATA                      
082400       MOVE GMT-ADGMT-PADR         TO DGR-ADGMT-PADR                      
082500       MOVE GMT-ADGMT-LAND         TO DGR-ADGMT-LAND                      
082600*    END-IF                                                               
082700                                                                          
082800     MOVE LINK-IDDISTR             TO TEST-IDDISTR                        
082900     IF DIST32-SAUDI AND CDC                                              
083000       MOVE SPACE                  TO DGR-BEGMT-RAD1                      
083100                                      DGR-ADGMT-GATA                      
083200                                      DGR-ADGMT-PADR                      
083300                                      DGR-ADGMT-LAND                      
083400     END-IF                                                               
083500     .                                                                    
083600     EJECT                                                                
083700 C-KOLLA-DC  SECTION.                                                     
083800                                                                          
083900     MOVE LINK-IDDISTR              TO TEST-IDDISTR                       
084000     MOVE LINK-IDKUNDNR             TO KUND15-IDKUNDNR                    
084100                                                                          
084200     IF NDC-CA                                                            
084300       MOVE JA                      TO DC-FG-SW                           
084400     ELSE                                                                 
084500       IF LDC-FR          AND                                             
084600          DIST67-FG-FR    AND                                             
084700          KUND15-KUND-FG                                                  
084800         MOVE JA                    TO DC-FG-SW                           
084900       END-IF                                                             
085000     END-IF                                                               
085100     .                                                                    
085200     EJECT                                                                
085300 F-SKRIV-BLANKETT-WEB SECTION.                                            
085400                                                                          
085500     PERFORM FA-SKRIV-HUVUD-WEB                                           
085600     MOVE +99                      TO RAD-IX                              
085700     MOVE +1                       TO IX1                                 
085800     MOVE +1                       TO WS-SIDA                             
085900                                                                          
086000     PERFORM UNTIL IX1 > MAX-IX                                           
086100          OR BLK-IDKOLLI (IX1) = 0                                        
086200                                                                          
086300        PERFORM FB-REDIGERA-BLANKETT                                      
086400        ADD +1                     TO IX1                                 
086500     END-PERFORM                                                          
086600                                                                          
086700     PERFORM FCC-SKRIV-AVSLUTNING                                         
086800     MOVE WZ04-001-IDCOM          TO SEND-IDCOM                           
086900     PERFORM S95-SEND-CLOSE                                               
087000     .                                                                    
087100     EJECT                                                                
087200 FA-SKRIV-HUVUD-WEB     SECTION.                                          
087300                                                                          
087400     MOVE 1                 TO HDR-REQU-IDMSGVER                          
087500     MOVE 'R'               TO HDR-REQU-KDPGMACT                          
087600     MOVE SPACE             TO HDR-REQU-IDUSER                            
087700                                                                          
087800     MOVE SPACE             TO HDR-IDOUTREC                               
087900                               HDR-IDLIST                                 
088000     MOVE 'DANGEROUS-GOODS' TO HDR-IDOUTTYPE                              
088100     MOVE LINK-IDDC         TO HDR-IDOUTREC(1:2)                          
088200     MOVE 'DGR'             TO HDR-IDLIST(1:3)                            
088300     MOVE LINK-IDDISTR      TO WS-WEB-IDDISTR                             
088400     MOVE WS-WEB-IDDISTR    TO HDR-IDLIST(4:4)                            
088500     MOVE LINK-IDTRPTNR     TO WS-WEB-IDTRPTNR                            
088600     MOVE WS-WEB-IDTRPTNR   TO HDR-IDLIST(8:3)                            
088700     PERFORM S90-SEND-OPEN                                                
088800                                                                          
088900     MOVE SEND-IDCOM       TO WZ04-001-IDCOM                              
089000     PERFORM S91-PUT-DOC-HDR                                              
090000                                                                          
100000     PERFORM FAA-RAKNA-UT-ANTAL-SIDOR                                     
110000                                                                          
120000     MOVE '1'              TO HUVUD-IDAFPRCD                              
130000     MOVE DGR-IDFRASED     TO HUVUD-IDFRASED                              
140000     MOVE DGR-SIDA-TOT     TO HUVUD-KVPAGE                                
150000     MOVE DGR-IDORDNR7     TO HUVUD-IDORDNR7                              
160000     MOVE DGR-BEGMT-RAD1   TO HUVUD-BEGMT-RAD1                            
170000     MOVE SPACE            TO HUVUD-BEGMT-RAD2                            
180000     MOVE DGR-ADGMT-GATA   TO HUVUD-ADGMT-GATA                            
190000     MOVE DGR-ADGMT-PADR   TO HUVUD-ADGMT-PADR                            
200000     MOVE DGR-ADGMT-LAND   TO HUVUD-ADGMT-LAND                            
210000     MOVE DGR-AIRPORT      TO HUVUD-AIRPORT                               
220000     MOVE RAD6             TO WS-RAD                                      
230000     MOVE RAD7             TO WS-RAD                                      
231000                                                                          
232000                                                                          
233000     PERFORM IMS-GU-WDB601                                                
234000                                                                          
235000     IF SEGMENT-FINNS                                                     
236000       MOVE DCS-ADGMT-PADR(11:20) TO DGR-SIGN-ORT-OVR                     
237000                                                                          
238000       MOVE DCS-BEGMT-RAD1        TO HUVUD-DCS-BEGMT-RAD1                 
238100       MOVE DCS-BEGMT-RAD2        TO HUVUD-DCS-BEGMT-RAD2                 
238200     END-IF                                                               
238300                                                                          
238400     IF NDC-NA                                                            
238500       MOVE +1                       TO TAB-IX                            
238600       PERFORM UNTIL TAB-IX > MAX-TAB-IX                                  
238700        INSPECT FINAL-IDORDNR7 (TAB-IX)                                   
238800                REPLACING LEADING ZERO BY SPACE                           
238900        ADD +1                     TO TAB-IX                              
239000       END-PERFORM                                                        
239100       MOVE RAD11NA                TO WS-RAD                              
239200                                                                          
239300       IF NDC-US-RU                                                       
239400         MOVE 'Rutherford, NJ      ' TO DGR-SIGN-ORT-OVR                  
239500       END-IF                                                             
239600                                                                          
239700       IF NDC-US-LA                                                       
239800         MOVE 'Carson, CA          ' TO DGR-SIGN-ORT-OVR                  
239900       END-IF                                                             
240000                                                                          
240100       IF NDC-CA                                                          
240200         MOVE 'Missisauga, Ontario ' TO DGR-SIGN-ORT-OVR                  
240300       END-IF                                                             
240400                                                                          
240500       IF NDC-JP                                                          
240600         MOVE 'Nagoya              ' TO DGR-SIGN-ORT-OVR                  
240700       END-IF                                                             
240800                                                                          
240900       IF NDC-AU                                                          
241000         MOVE 'Minto               ' TO DGR-SIGN-ORT-OVR                  
242000       END-IF                                                             
242100                                                                          
242200     END-IF                                                               
242300                                                                          
242400     IF NDC-CN                                                            
242500       IF NDC-CN-71                                                       
242600         MOVE 'DC 71               ' TO DGR-SIGN-ORT-OVR                  
242700       END-IF                                                             
242800                                                                          
242900       IF NDC-CN-72                                                       
243000         MOVE 'DC 72               ' TO DGR-SIGN-ORT-OVR                  
243100       END-IF                                                             
243200                                                                          
243300       IF NDC-CN-73                                                       
243400         MOVE 'DC 73               ' TO DGR-SIGN-ORT-OVR                  
243500       END-IF                                                             
243600                                                                          
243700       IF NDC-CN-74                                                       
243800         MOVE 'DC 74               ' TO DGR-SIGN-ORT-OVR                  
243900       END-IF                                                             
244000     END-IF                                                               
244100                                                                          
244200     MOVE DGR-SIGN-ORT-OVR         TO HUVUD-SIGN-ORT                      
244300     IF NDC-NA                                                            
244400       MOVE DGR-SIGN-DATUM-NA      TO HUVUD-SIGN-DATUM                    
244500     ELSE                                                                 
244600       MOVE DGR-SIGN-DATUM-SE      TO HUVUD-SIGN-DATUM                    
244700     END-IF                                                               
244800                                                                          
244900     PERFORM S92-PUT-DOC-HEAD                                             
245000     .                                                                    
245100     EJECT                                                                
245200 FAA-RAKNA-UT-ANTAL-SIDOR SECTION.                                        
245300                                                                          
245400     MOVE +0                       TO WS-RADER                            
245500     MOVE +1                       TO IX1                                 
245600                                      WS-SIDA-TOT                         
245700     PERFORM UNTIL IX1 > MAX-IX                                           
245800        IF BLK-IDKOLLI (IX1) > 0                                          
245900           ADD BLK-RADER(IX1)      TO WS-RADER                            
246000           IF WS-RADER + BLK-RADER(IX1 + 1) > MAX-RAD-IX                  
246100              ADD +1               TO WS-SIDA-TOT                         
246200              MOVE +0              TO WS-RADER                            
246300           END-IF                                                         
246400        END-IF                                                            
246500        ADD +1                     TO IX1                                 
246600     END-PERFORM                                                          
246700                                                                          
246800     MOVE WS-SIDA-TOT              TO DGR-SIDA-TOT                        
246900                                                                          
247000     .                                                                    
247100     EJECT                                                                
247200 FB-REDIGERA-BLANKETT SECTION.                                            
247300                                                                          
247400     MOVE '2'              TO RAD-IDAFPRCD                                
247500     MOVE NEJ              TO SKRIV-EQ-VARDE-SW                           
247600     MOVE +1               TO IX2                                         
247700     MOVE +0               TO IX4                                         
247800     PERFORM UNTIL IX2 > +9                                               
247900        IF BLK-IDPSN (IX1, IX2) > +0                                      
248000           PERFORM FBA-SKRIV-KOLLI-RADER                                  
248100        END-IF                                                            
248200        ADD +1             TO IX2                                         
248300     END-PERFORM                                                          
248400                                                                          
248500     MOVE SPACE            TO RAD-FG-RAD                                  
248600     PERFORM S93-PUT-DOC-LINE                                             
248700                                                                          
248800     IF SKRIV-EQ-VARDE                                                    
248900        PERFORM FBB-SKRIV-EQ-VARDE                                        
249000     END-IF                                                               
249100     .                                                                    
249200     EJECT                                                                
249300 FBA-SKRIV-KOLLI-RADER SECTION.                                           
249400                                                                          
249500*--- KOLLI RADER                                                          
249600                                                                          
249700     MOVE IX2                      TO IX3                                 
249800     ADD +1                        TO IX3                                 
249900                                                                          
250000     MOVE BLK-IDPSN (IX1, IX2)     TO W-IDPSN                             
250100     MOVE WS-IDSPRAK-GB            TO W-IDSPRAK                           
250200                                                                          
250300     PERFORM IMS-GET-1165-WDR2                                            
250400                                                                          
250500     IF BLK-IDPSN(IX1, IX3) > +0 OR IX2 > +1                              
250600                                                                          
250700        IF 1165-1168-BEPSN (1) NOT = SPACE                                
250800                                                                          
250900           IF W-IDPSN = WS-PSN-90 OR                                      
251000                        WS-PSN-91                                         
251100                                                                          
251200             MOVE JA TO SPEC-TEXT-90-91-SW                                
251300           END-IF                                                         
251400                                                                          
251500           MOVE 1165-1168-BEPSN (1) TO ARB-RAD                            
251600           IF 1165-1168-BEPSN (3)   = SPACE                               
251700             PERFORM FBAA-KOLLA-KOLLI-FLER-PSN                            
251800           END-IF                                                         
251900           MOVE ARB-RAD            TO DGR-BEPSN                           
252000                                                                          
252100           IF 1165-1168-BEPSN (2) NOT = SPACE                             
252200              MOVE RAD8(6:75)          TO RAD-FG-RAD                      
252300              PERFORM S93-PUT-DOC-LINE                                    
252400                                                                          
252500              MOVE 1165-1168-BEPSN (2) TO ARB-RAD                         
252600              IF 1165-1168-BEPSN (3) NOT = SPACE                          
252700                PERFORM FBAA-KOLLA-KOLLI-FLER-PSN                         
252800              END-IF                                                      
252900              MOVE ARB-RAD             TO DGR-BEPSN                       
253000           END-IF                                                         
253100           IF 1165-1168-BEPSN (3) NOT = SPACE                             
253200              MOVE RAD8(6:75)          TO RAD-FG-RAD                      
253300              PERFORM S93-PUT-DOC-LINE                                    
253400                                                                          
253500              MOVE 1165-1168-BEPSN(3)  TO DGR-BEPSN                       
253600           END-IF                                                         
253700           MOVE RAD8(6:75)             TO RAD-FG-RAD                      
253800           PERFORM S93-PUT-DOC-LINE                                       
253900        END-IF                                                            
254000     ELSE                                                                 
254100        PERFORM FBAB-SKRIV-KOLLI-EN-PSN                                   
254200     END-IF                                                               
254300                                                                          
254400     ADD +1                        TO IX4                                 
254500     IF IX4 > +1                                                          
254600        MOVE JA                    TO SKRIV-EQ-VARDE-SW                   
254700     END-IF                                                               
254800     .                                                                    
254900     EJECT                                                                
255000 FBAA-KOLLA-KOLLI-FLER-PSN SECTION.                                       
255100                                                                          
255200     MOVE +1                       TO STR-IX-1                            
255300     MOVE +2                       TO STR-IX-2                            
255400                                                                          
255500     PERFORM UNTIL STR-IX-1 > 75                                          
255600        IF ARB-RAD (STR-IX-1 : STR-IX-2) = SPACE                          
255700           MOVE BLK-VLFG (IX1, IX2)                                       
255800                                TO WS-VLFG                                
255900           IF WS-VLFG-6-7 = 0                                             
256000              CONTINUE                                                    
256100           ELSE                                                           
256200              COMPUTE WS-VLFG = WS-VLFG + 0.1                             
256300           END-IF                                                         
256400           MOVE WS-VLFG            TO ARB-VLFG-1                          
256500           MOVE BLK-IDPSN(IX1, IX2)                                       
256600                                TO SORT-SW                                
256700           IF SORT-KG                                                     
256800              MOVE 'KG  '       TO ARB-SORT-1                             
256900           ELSE                                                           
257000              MOVE 'L   '       TO ARB-SORT-1                             
258000           END-IF                                                         
258100                                                                          
258200           MOVE ARB-RAD-1       TO ARB-RAD(STR-IX-1 : 14)                 
258300           MOVE +99             TO STR-IX-1                               
258400        END-IF                                                            
258500        ADD +1                  TO STR-IX-1                               
258600     END-PERFORM                                                          
258700     .                                                                    
258800     EJECT                                                                
258900 FBAB-SKRIV-KOLLI-EN-PSN SECTION.                                         
259000                                                                          
259100     IF 1165-1168-BEPSN (1) NOT = SPACE                                   
259200                                                                          
259300       IF W-IDPSN = WS-PSN-90 OR                                          
259400                    WS-PSN-91                                             
259500                                                                          
259600         MOVE JA TO SPEC-TEXT-90-91-SW                                    
259700       END-IF                                                             
259800                                                                          
259900        MOVE 1165-1168-BEPSN (1)   TO DGR-BEPSN                           
260000                                                                          
260100        MOVE RAD8(6:75)            TO RAD-FG-RAD                          
260200        PERFORM S93-PUT-DOC-LINE                                          
260300                                                                          
260400        IF 1165-1168-BEPSN (3) = SPACE                                    
260500                                                                          
260600          MOVE BLK-VLFG (IX1, IX2) TO WS-VLFG                             
260700                                                                          
260800          IF WS-VLFG-6-7 = 0                                              
260900             CONTINUE                                                     
261000          ELSE                                                            
261100             COMPUTE WS-VLFG = WS-VLFG + 0.1                              
261200          END-IF                                                          
261300                                                                          
261400          MOVE WS-VLFG             TO ARB-VLFG-2                          
261500          MOVE BLK-IDPSN(IX1, IX2) TO SORT-SW                             
261600          IF SORT-KG                                                      
261700             MOVE 'KG,'            TO ARB-SORT-2                          
261800          ELSE                                                            
261900             MOVE 'L,'             TO ARB-SORT-2                          
262000          END-IF                                                          
262100                                                                          
262200          IF BLK-IDPSN(IX1, IX2) = WS-PSN-93                              
262300            MOVE WS-ENGINE         TO ARB-BOX                             
262400          ELSE                                                            
262500            MOVE BLK-KDKOLLI(IX1)  TO KDKOLLI-SW                          
262600            IF NDC-NA                                                     
262700              MOVE WS-FIBREBOARDBOX TO ARB-BOX                            
262800            ELSE                                                          
262900              IF FIBREBOARDBOX                                            
263000                 MOVE WS-FIBREBOARDBOX TO ARB-BOX                         
263100              ELSE                                                        
263200                 IF PLYWOODBOX                                            
263300                    MOVE WS-PLYWOODBOX TO ARB-BOX                         
263400                 ELSE                                                     
263500                    MOVE WS-FIBREBOARDBOX                                 
263600                                       TO ARB-BOX                         
263700                 END-IF                                                   
263800              END-IF                                                      
263900            END-IF                                                        
264000          END-IF                                                          
264100                                                                          
264200          MOVE ARB-RAD-2           TO RAD8                                
264300          MOVE RAD8(6:75)          TO RAD-FG-RAD                          
264400          PERFORM S93-PUT-DOC-LINE                                        
264500                                                                          
264600        END-IF                                                            
264700                                                                          
264800        IF 1165-1168-BEPSN (2) NOT = SPACE                                
264900           MOVE 1165-1168-BEPSN(2) TO DGR-BEPSN                           
265000                                                                          
265100           MOVE RAD8(6:75)         TO RAD-FG-RAD                          
265200           PERFORM S93-PUT-DOC-LINE                                       
265300                                                                          
265400          IF 1165-1168-BEPSN (3) NOT = SPACE                              
265500            MOVE BLK-VLFG (IX1, IX2) TO WS-VLFG                           
265600                                                                          
265700            IF WS-VLFG-6-7 = 0                                            
265800               CONTINUE                                                   
265900            ELSE                                                          
266000               COMPUTE WS-VLFG = WS-VLFG + 0.1                            
266100            END-IF                                                        
266200                                                                          
266300            MOVE WS-VLFG           TO ARB-VLFG-2                          
266400            MOVE BLK-IDPSN(IX1, IX2) TO SORT-SW                           
266500            IF SORT-KG                                                    
266600               MOVE 'KG,'          TO ARB-SORT-2                          
266700            ELSE                                                          
266800               MOVE 'L,'           TO ARB-SORT-2                          
266900            END-IF                                                        
267000                                                                          
267100            IF BLK-IDPSN(IX1, IX2) = WS-PSN-93                            
267200              MOVE WS-ENGINE       TO ARB-BOX                             
267300            ELSE                                                          
267400              MOVE BLK-KDKOLLI(IX1) TO KDKOLLI-SW                         
267500              IF NDC-NA                                                   
267600                MOVE WS-FIBREBOARDBOX TO ARB-BOX                          
267700              ELSE                                                        
267800                IF FIBREBOARDBOX                                          
267900                   MOVE WS-FIBREBOARDBOX TO ARB-BOX                       
268000                ELSE                                                      
268100                   IF PLYWOODBOX                                          
268200                      MOVE WS-PLYWOODBOX TO ARB-BOX                       
268300                   ELSE                                                   
268400                      MOVE WS-FIBREBOARDBOX                               
268500                                         TO ARB-BOX                       
268600                   END-IF                                                 
268700                END-IF                                                    
268800              END-IF                                                      
268900            END-IF                                                        
269000                                                                          
269100            MOVE ARB-RAD-2         TO RAD8                                
269200            MOVE RAD8              TO WS-RAD                              
269300            MOVE RAD8(6:75)        TO RAD-FG-RAD                          
269400            PERFORM S93-PUT-DOC-LINE                                      
269500          END-IF                                                          
269600        END-IF                                                            
269700                                                                          
269800        IF 1165-1168-BEPSN (3) NOT = SPACE                                
269900           MOVE 1165-1168-BEPSN(3) TO DGR-BEPSN                           
270000                                                                          
270100           MOVE RAD8(6:75)         TO RAD-FG-RAD                          
270200           PERFORM S93-PUT-DOC-LINE                                       
270300                                                                          
270400        END-IF                                                            
270500     END-IF                                                               
270600     .                                                                    
270700     EJECT                                                                
270800 FBB-SKRIV-EQ-VARDE SECTION.                                              
270900                                                                          
271000     MOVE BLK-KDKOLLI(IX1)         TO KDKOLLI-SW                          
271100     IF NDC-NA                                                            
271200       MOVE WS-FIBREBOARDBOX       TO RAD9-BOX                            
271300     ELSE                                                                 
271400       IF FIBREBOARDBOX                                                   
271500          MOVE WS-FIBREBOARDBOX    TO RAD9-BOX                            
271600       ELSE                                                               
271700          IF PLYWOODBOX                                                   
271800             MOVE WS-PLYWOODBOX    TO RAD9-BOX                            
271900          ELSE                                                            
272000             MOVE WS-FIBREBOARDBOX TO RAD9-BOX                            
272100          END-IF                                                          
272200       END-IF                                                             
272300     END-IF                                                               
272400                                                                          
272500     MOVE RAD9(6:32)               TO RAD-FG-RAD                          
272600     PERFORM S93-PUT-DOC-LINE                                             
272700                                                                          
272800     COMPUTE WS-SUEQFG = WS-SUEQFG + (BLK-SUEQFG(IX1) + 0.1)              
272900     MOVE WS-SUEQFG                TO DGR-SUEQFG                          
273000                                                                          
273100     MOVE RAD10(6:5)               TO RAD-FG-RAD                          
273200     PERFORM S93-PUT-DOC-LINE                                             
273300                                                                          
273400     .                                                                    
273500     EJECT                                                                
273600 FCC-SKRIV-AVSLUTNING SECTION.                                            
273700                                                                          
273800     MOVE '3'          TO FOT-IDAFPRCD                                    
273900     MOVE SPACE        TO FOT-FG-FOT                                      
274000     MOVE LINK-IDDISTR TO TEST-IDDISTR                                    
274100                                                                          
274200     IF NDC-NA                                                            
274300       MOVE RAD11NA                TO FOT-FG-FOT                          
274400       PERFORM S94-PUT-DOC-FOOT                                           
274500     END-IF                                                               
274600                                                                          
274700     IF NDC-CN                                                            
274800        MOVE RAD11(6:32)           TO FOT-FG-FOT                          
274900        PERFORM S94-PUT-DOC-FOOT                                          
275000        IF DIST32-SPEC-TEXT                                               
275100                                                                          
275200          MOVE PRT-EQUAL-56        TO PRT-RADSKIP                         
275300          MOVE RAD12               TO WS-RAD                              
275400          PERFORM S02-SKRIV                                               
275500                                                                          
275600          MOVE PRT-AFTER-1         TO PRT-RADSKIP                         
275700          MOVE RAD13               TO WS-RAD                              
275800          PERFORM S02-SKRIV                                               
275900                                                                          
276000          MOVE PRT-AFTER-1         TO PRT-RADSKIP                         
276100          MOVE RAD14               TO WS-RAD                              
276200          PERFORM S02-SKRIV                                               
276300       END-IF                                                             
276400     END-IF                                                               
276500                                                                          
276600     .                                                                    
276700     EJECT                                                                
276800 G-SKRIV-BLANKETT SECTION.                                                
276900                                                                          
277000     PERFORM GA-SKAPA-PRINTER-ID                                          
278000     IF WS-PRT-IDPRTLST NOT = SPACE                                       
279000       PERFORM S01-OPPNA-PRINTER                                          
280000       PERFORM GD-RAKNA-UT-ANTAL-SIDOR                                    
280100       MOVE +99                      TO RAD-IX                            
280200       MOVE +1                       TO IX1                               
280300       MOVE +1                       TO WS-SIDA                           
280400                                                                          
280500       PERFORM UNTIL IX1 > MAX-IX                                         
280600          IF BLK-IDKOLLI (IX1) > 0                                        
280700             IF RAD-IX + BLK-RADER(IX1) > MAX-RAD-IX                      
280800                PERFORM GB-SKRIV-KOPARE-ADRESS                            
280900                MOVE +1              TO RAD-IX                            
281000             END-IF                                                       
281100             PERFORM GC-REDIGERA-BLANKETT                                 
281200          END-IF                                                          
281300          ADD +1                     TO IX1                               
281400       END-PERFORM                                                        
281500                                                                          
281600       PERFORM S10-STAENG-PRINTER                                         
281700     END-IF                                                               
281800     .                                                                    
281900     EJECT                                                                
282000 GA-SKAPA-PRINTER-ID SECTION.                                             
283000                                                                          
284000     MOVE SPACE TO WS-PRT-IDPRTLST                                        
285000                                                                          
286000     IF CDC-SE OR DDC-SE                                                  
286100       MOVE LINK-IDDISTR          TO TEST-IDDISTR                         
286200       IF DIST66-PRINTER-EUROPA2                                          
286300         MOVE 'W40503E2'          TO WS-PRT-IDPRTLST                      
286400       ELSE                                                               
286500         IF DIST66-PRINTER-NORDEN                                         
286600           MOVE 'W40503N '        TO WS-PRT-IDPRTLST                      
286700         ELSE                                                             
286800           IF DIST66-PRINTER-OVERSEAS                                     
286900             MOVE 'W40503O '      TO WS-PRT-IDPRTLST                      
287000           ELSE                                                           
287100             IF DIST66-PRINTER-EUROPA3                                    
287200               MOVE 'W40503E3'    TO WS-PRT-IDPRTLST                      
287300             ELSE                                                         
287400               MOVE 'W40503E2'    TO WS-PRT-IDPRTLST                      
287500             END-IF                                                       
287600           END-IF                                                         
287700         END-IF                                                           
287800       END-IF                                                             
287900       MOVE LINK-IDDISTR          TO WS-PRT-IDDISTR                       
288000       MOVE LINK-KDFRAKT          TO WS-PRT-KDFRAKT                       
288100     ELSE                                                                 
288200*                                                                         
288300*-- BORTKOMMENTERADE LAGER SKA INTE HA DGR-LISTA (JUST NU 2012-10)        
288400*-- I ALLA FALL USA. JAPAN O AUSTRALEN KOMMENTAR SEN TIDIGARE             
288500*-- /KJELL                                                                
288600*                                                                         
288700*-- CANADA BLEV SISTA ATT TA BORT UR LISTAN I SMBAND MED                  
288800*-- MED ATT LAGRET ÅTERÖPPNAS 2014.                                       
288900*-- DÄRFÖR KOMMENTERAS HELA EVALUATE-SATSEN                               
289000*-- /Göran                                                                
289100*                                                                         
289200*      MOVE LINK-IDDC             TO WS-IDDC                              
289300*      EVALUATE TRUE                                                      
289400*        WHEN SDC-NL                                                      
289500*          MOVE 'W4051321'        TO WS-PRT-IDPRTLST                      
289600*        WHEN NDC-US-RU                                                   
289700*          MOVE 'W40541  '        TO WS-PRT-IDPRTLST                      
289800*        WHEN NDC-US-LA                                                   
289900*          MOVE 'W40543  '        TO WS-PRT-IDPRTLST                      
290000*        WHEN NDC-CA                                                      
290100*          MOVE 'W40551  '        TO WS-PRT-IDPRTLST                      
290200*        WHEN NDC-JP                                                      
290300*          MOVE 'W40561  '        TO WS-PRT-IDPRTLST                      
290400*        WHEN NDC-AU                                                      
290500*          MOVE 'W40562  '        TO WS-PRT-IDPRTLST                      
290600*        WHEN OTHER                                                       
290700*          MOVE 'W40503O '        TO WS-PRT-IDPRTLST                      
290800*      END-EVALUATE                                                       
290900                                                                          
291000       MOVE LINK-IDDISTR          TO WS-PRT-IDDISTR                       
292000       MOVE LINK-KDFRAKT          TO WS-PRT-KDFRAKT                       
292100                                                                          
292200     END-IF                                                               
292300     .                                                                    
292400     EJECT                                                                
292500 GB-SKRIV-KOPARE-ADRESS SECTION.                                          
292600                                                                          
292700     IF LINK-IDSYSTEM = '4535' OR '4665' OR '4675'                        
292800                                                                          
292900        MOVE PRT-NYSIDA-RAD5       TO PRT-RADSKIP                         
293000        MOVE SPACE                 TO WS-RAD                              
293100        PERFORM S02-SKRIV                                                 
293200                                                                          
293300        MOVE PRT-AFTER-1           TO PRT-RADSKIP                         
293400        MOVE RAD0                  TO WS-RAD                              
293500        PERFORM S02-SKRIV                                                 
293600     ELSE                                                                 
293700        MOVE PRT-NYSIDA-RAD2       TO PRT-RADSKIP                         
293800        MOVE SPACE                 TO WS-RAD                              
293900        PERFORM S02-SKRIV                                                 
294000                                                                          
294100        MOVE PRT-AFTER-1           TO PRT-RADSKIP                         
294200        MOVE RAD00                 TO WS-RAD                              
294300        PERFORM S02-SKRIV                                                 
294400                                                                          
294500        MOVE PRT-AFTER-3           TO PRT-RADSKIP                         
294600        MOVE RAD0                  TO WS-RAD                              
294700        PERFORM S02-SKRIV                                                 
294800     END-IF                                                               
294900                                                                          
295000     MOVE WS-SIDA                  TO DGR-SIDA                            
295100     MOVE PRT-AFTER-2              TO PRT-RADSKIP                         
295200     MOVE RAD1                     TO WS-RAD                              
295300     PERFORM S02-SKRIV                                                    
295400                                                                          
295500     MOVE PRT-AFTER-2              TO PRT-RADSKIP                         
295600     MOVE RAD2                     TO WS-RAD                              
295700     PERFORM S02-SKRIV                                                    
295800                                                                          
295900     MOVE PRT-AFTER-2              TO PRT-RADSKIP                         
296000     MOVE RAD3                     TO WS-RAD                              
296100     PERFORM S02-SKRIV                                                    
296200                                                                          
296300     IF RAD3A > SPACE                                                     
296400       MOVE PRT-AFTER-1            TO PRT-RADSKIP                         
296500       MOVE RAD3A                  TO WS-RAD                              
296600       PERFORM S02-SKRIV                                                  
296700     END-IF                                                               
296800                                                                          
296900     MOVE PRT-AFTER-1              TO PRT-RADSKIP                         
297000     MOVE RAD4                     TO WS-RAD                              
297100     PERFORM S02-SKRIV                                                    
297200                                                                          
297300     MOVE PRT-AFTER-1              TO PRT-RADSKIP                         
297400     MOVE RAD5                     TO WS-RAD                              
297500     PERFORM S02-SKRIV                                                    
297600                                                                          
297700     MOVE PRT-AFTER-1              TO PRT-RADSKIP                         
297800     MOVE RAD6                     TO WS-RAD                              
297900     PERFORM S02-SKRIV                                                    
298000                                                                          
298100     MOVE PRT-AFTER-9              TO PRT-RADSKIP                         
298200     MOVE RAD7                     TO WS-RAD                              
298300     PERFORM S02-SKRIV                                                    
298400                                                                          
298500     ADD +1                        TO WS-SIDA                             
298600                                                                          
298700     PERFORM GBA-POSITIONERA-KOLLIRADER                                   
298800     .                                                                    
298900     EJECT                                                                
299000 GBA-POSITIONERA-KOLLIRADER SECTION.                                      
299100                                                                          
299200*--- POSITIONERA FRAM TILL FÖRSTA KOLLIRADEN                              
299300                                                                          
299400     MOVE PRT-AFTER-8              TO PRT-RADSKIP                         
299500     MOVE SPACE                    TO WS-RAD                              
299600     PERFORM S02-SKRIV                                                    
299700                                                                          
299800     .                                                                    
299900     EJECT                                                                
300000 GC-REDIGERA-BLANKETT SECTION.                                            
300100                                                                          
300200     MOVE NEJ                      TO SKRIV-EQ-VARDE-SW                   
300300     MOVE +1                       TO IX2                                 
300400     MOVE +0                       TO IX4                                 
300500     PERFORM UNTIL IX2 > +9                                               
300600        IF BLK-IDPSN (IX1, IX2) > +0                                      
300700           PERFORM GCB-SKRIV-KOLLI-RADER                                  
300800        END-IF                                                            
300900        ADD +1                     TO IX2                                 
301000     END-PERFORM                                                          
301100                                                                          
301200     MOVE PRT-AFTER-1              TO PRT-RADSKIP                         
301300     MOVE SPACE                    TO WS-RAD                              
301400     PERFORM S02-SKRIV                                                    
301500     ADD +1                        TO RAD-IX                              
301600                                                                          
301700     IF SKRIV-EQ-VARDE                                                    
301800        PERFORM GCD-SKRIV-EQ-VARDE                                        
301900                                                                          
302000        MOVE PRT-AFTER-1           TO PRT-RADSKIP                         
302100        MOVE SPACE                 TO WS-RAD                              
302200        PERFORM S02-SKRIV                                                 
302300        ADD +1                     TO RAD-IX                              
302400                                                                          
302500     END-IF                                                               
302600                                                                          
302700     IF BLK-IDKOLLI (IX1 + 1) > 0                                         
302800        IF RAD-IX + BLK-RADER(IX1 + 1) > MAX-RAD-IX                       
302900           PERFORM GCC-SKRIV-AVSLUTNING                                   
303000        END-IF                                                            
303100     ELSE                                                                 
303200        PERFORM GCC-SKRIV-AVSLUTNING                                      
303300     END-IF                                                               
303400     .                                                                    
303500     EJECT                                                                
303600 GCB-SKRIV-KOLLI-RADER SECTION.                                           
303700                                                                          
303800*--- KOLLI RADER                                                          
303900                                                                          
304000     MOVE IX2                      TO IX3                                 
304100     ADD +1                        TO IX3                                 
304200                                                                          
304300     MOVE BLK-IDPSN (IX1, IX2)     TO W-IDPSN                             
304400     MOVE WS-IDSPRAK-SE            TO W-IDSPRAK                           
304500                                                                          
304600     PERFORM IMS-GET-1165-WDR2                                            
304700                                                                          
304800     MOVE SPACE                    TO WS-RAD                              
304900                                      RAD8                                
305000                                                                          
305100     IF BLK-IDPSN(IX1, IX3) > +0 OR IX2 > +1                              
305200                                                                          
305300        IF 1165-1168-BEPSN (1) NOT = SPACE                                
305400                                                                          
305500           IF W-IDPSN = WS-PSN-90 OR                                      
305600                        WS-PSN-91                                         
305700                                                                          
305800             MOVE JA               TO SPEC-TEXT-90-91-SW                  
305900           END-IF                                                         
306000                                                                          
306100           MOVE 1165-1168-BEPSN (1) TO ARB-RAD                            
306200           IF 1165-1168-BEPSN (3)   = SPACE                               
306300             PERFORM GCBA-KOLLA-KOLLI-FLER-PSN                            
306400           END-IF                                                         
306500           MOVE ARB-RAD            TO DGR-BEPSN                           
306600                                                                          
306700           IF 1165-1168-BEPSN (2) NOT = SPACE                             
306800              MOVE PRT-AFTER-1     TO PRT-RADSKIP                         
306900              MOVE RAD8            TO WS-RAD                              
307000              PERFORM S02-SKRIV                                           
307100              ADD +1               TO RAD-IX                              
307200                                                                          
307300              MOVE SPACE           TO RAD8                                
307400              MOVE 1165-1168-BEPSN (2) TO ARB-RAD                         
307500              IF 1165-1168-BEPSN (3) NOT = SPACE                          
307600                PERFORM GCBA-KOLLA-KOLLI-FLER-PSN                         
307700              END-IF                                                      
307800              MOVE ARB-RAD          TO DGR-BEPSN                          
307900           END-IF                                                         
308000           IF 1165-1168-BEPSN (3) NOT = SPACE                             
308100              MOVE PRT-AFTER-1     TO PRT-RADSKIP                         
308200              MOVE RAD8            TO WS-RAD                              
308300              PERFORM S02-SKRIV                                           
308400              ADD +1               TO RAD-IX                              
308500                                                                          
308600              MOVE SPACE           TO RAD8                                
308700              MOVE 1165-1168-BEPSN(3) TO DGR-BEPSN                        
308800           END-IF                                                         
308900           MOVE PRT-AFTER-1        TO PRT-RADSKIP                         
309000           MOVE RAD8               TO WS-RAD                              
309100           PERFORM S02-SKRIV                                              
309200           ADD +1                  TO RAD-IX                              
309300        END-IF                                                            
309400     ELSE                                                                 
309500        PERFORM GCBB-SKRIV-KOLLI-EN-PSN                                   
309600     END-IF                                                               
309700                                                                          
309800     ADD +1                        TO IX4                                 
309900     IF IX4 > +1                                                          
310000        MOVE JA                    TO SKRIV-EQ-VARDE-SW                   
310100     END-IF                                                               
310200     .                                                                    
310300     EJECT                                                                
310400 GCBA-KOLLA-KOLLI-FLER-PSN SECTION.                                       
310500                                                                          
310600     MOVE +1                       TO STR-IX-1                            
310700     MOVE +2                       TO STR-IX-2                            
310800                                                                          
310900     PERFORM UNTIL STR-IX-1 > 75                                          
311000        IF ARB-RAD (STR-IX-1 : STR-IX-2) = SPACE                          
311100           MOVE BLK-VLFG (IX1, IX2)                                       
311200                                TO WS-VLFG                                
311300           IF WS-VLFG-6-7 = 0                                             
311400              CONTINUE                                                    
311500           ELSE                                                           
311600              COMPUTE WS-VLFG = WS-VLFG + 0.1                             
311700           END-IF                                                         
311800           MOVE WS-VLFG            TO ARB-VLFG-1                          
311900           MOVE BLK-IDPSN(IX1, IX2)                                       
312000                                TO SORT-SW                                
312100           IF SORT-KG                                                     
312200              MOVE 'KG  '       TO ARB-SORT-1                             
312300           ELSE                                                           
312400              MOVE 'L   '       TO ARB-SORT-1                             
312500           END-IF                                                         
312600                                                                          
312700           MOVE ARB-RAD-1       TO ARB-RAD(STR-IX-1 : 14)                 
312800           MOVE +99             TO STR-IX-1                               
312900        END-IF                                                            
313000        ADD +1                  TO STR-IX-1                               
313100     END-PERFORM                                                          
313200     .                                                                    
313300     EJECT                                                                
313400 GCBB-SKRIV-KOLLI-EN-PSN SECTION.                                         
313500                                                                          
313600     IF 1165-1168-BEPSN (1) NOT = SPACE                                   
313700                                                                          
313800       IF W-IDPSN = WS-PSN-90 OR                                          
313900                    WS-PSN-91                                             
314000                                                                          
314100         MOVE JA                   TO SPEC-TEXT-90-91-SW                  
314200       END-IF                                                             
314300                                                                          
314400        MOVE 1165-1168-BEPSN (1)   TO DGR-BEPSN                           
314500        MOVE PRT-AFTER-1           TO PRT-RADSKIP                         
314600        MOVE RAD8                  TO WS-RAD                              
314700        PERFORM S02-SKRIV                                                 
314800        ADD +1                     TO RAD-IX                              
314900                                                                          
315000        IF 1165-1168-BEPSN (3) = SPACE                                    
315100                                                                          
315200          MOVE BLK-VLFG (IX1, IX2) TO WS-VLFG                             
315300                                                                          
315400          IF WS-VLFG-6-7 = 0                                              
315500             CONTINUE                                                     
315600          ELSE                                                            
315700             COMPUTE WS-VLFG = WS-VLFG + 0.1                              
315800          END-IF                                                          
315900                                                                          
316000          MOVE WS-VLFG             TO ARB-VLFG-2                          
316100          MOVE BLK-IDPSN(IX1, IX2) TO SORT-SW                             
316200          IF SORT-KG                                                      
316300             MOVE 'KG,'            TO ARB-SORT-2                          
316400          ELSE                                                            
316500             MOVE 'L,'             TO ARB-SORT-2                          
316600          END-IF                                                          
316700                                                                          
316800          IF BLK-IDPSN(IX1, IX2) = WS-PSN-93                              
316900            MOVE WS-ENGINE         TO ARB-BOX                             
317000          ELSE                                                            
317100            MOVE BLK-KDKOLLI(IX1)  TO KDKOLLI-SW                          
317200            IF NDC-NA                                                     
317300              MOVE WS-FIBREBOARDBOX TO ARB-BOX                            
317400            ELSE                                                          
317500              IF FIBREBOARDBOX                                            
317600                 MOVE WS-FIBREBOARDBOX TO ARB-BOX                         
317700              ELSE                                                        
317800                 IF PLYWOODBOX                                            
317900                    MOVE WS-PLYWOODBOX TO ARB-BOX                         
318000                 ELSE                                                     
318100                    MOVE WS-FIBREBOARDBOX                                 
318200                                       TO ARB-BOX                         
318300                 END-IF                                                   
318400              END-IF                                                      
318500            END-IF                                                        
318600          END-IF                                                          
318700          MOVE SPACE               TO RAD8                                
318800          MOVE ARB-RAD-2           TO RAD8                                
318900                                                                          
319000          MOVE PRT-AFTER-1         TO PRT-RADSKIP                         
319100          MOVE RAD8                TO WS-RAD                              
319200          PERFORM S02-SKRIV                                               
319300          ADD +1                   TO RAD-IX                              
319400                                                                          
319500        END-IF                                                            
319600                                                                          
319700        IF 1165-1168-BEPSN (2) NOT = SPACE                                
319800           MOVE SPACE              TO RAD8                                
319900           MOVE 1165-1168-BEPSN(2) TO DGR-BEPSN                           
320000                                                                          
320100           MOVE PRT-AFTER-1        TO PRT-RADSKIP                         
320200           MOVE RAD8               TO WS-RAD                              
320300           PERFORM S02-SKRIV                                              
320400           ADD +1                  TO RAD-IX                              
320500                                                                          
320600          IF 1165-1168-BEPSN (3) NOT = SPACE                              
320700            MOVE BLK-VLFG (IX1, IX2) TO WS-VLFG                           
320800                                                                          
320900            IF WS-VLFG-6-7 = 0                                            
321000               CONTINUE                                                   
321100            ELSE                                                          
321200               COMPUTE WS-VLFG = WS-VLFG + 0.1                            
321300            END-IF                                                        
321400                                                                          
321500            MOVE WS-VLFG           TO ARB-VLFG-2                          
321600            MOVE BLK-IDPSN(IX1, IX2) TO SORT-SW                           
321700            IF SORT-KG                                                    
321800               MOVE 'KG,'          TO ARB-SORT-2                          
321900            ELSE                                                          
322000               MOVE 'L,'           TO ARB-SORT-2                          
322100            END-IF                                                        
322200                                                                          
322300            IF BLK-IDPSN(IX1, IX2) = WS-PSN-93                            
322400              MOVE WS-ENGINE       TO ARB-BOX                             
322500            ELSE                                                          
322600              MOVE BLK-KDKOLLI(IX1) TO KDKOLLI-SW                         
322700              IF NDC-NA                                                   
322800                MOVE WS-FIBREBOARDBOX TO ARB-BOX                          
322900              ELSE                                                        
323000                IF FIBREBOARDBOX                                          
323100                   MOVE WS-FIBREBOARDBOX TO ARB-BOX                       
323200                ELSE                                                      
323300                   IF PLYWOODBOX                                          
323400                      MOVE WS-PLYWOODBOX TO ARB-BOX                       
323500                   ELSE                                                   
323600                      MOVE WS-FIBREBOARDBOX                               
323700                                         TO ARB-BOX                       
323800                   END-IF                                                 
323900                END-IF                                                    
324000              END-IF                                                      
324100            END-IF                                                        
324200            MOVE SPACE             TO RAD8                                
324300            MOVE ARB-RAD-2         TO RAD8                                
324400                                                                          
324500            MOVE PRT-AFTER-1       TO PRT-RADSKIP                         
324600            MOVE RAD8              TO WS-RAD                              
324700            PERFORM S02-SKRIV                                             
324800            ADD +1                 TO RAD-IX                              
324900          END-IF                                                          
325000        END-IF                                                            
325100                                                                          
325200        IF 1165-1168-BEPSN (3) NOT = SPACE                                
325300           MOVE SPACE              TO RAD8                                
325400           MOVE 1165-1168-BEPSN(3) TO DGR-BEPSN                           
325500                                                                          
325600           MOVE PRT-AFTER-1        TO PRT-RADSKIP                         
325700           MOVE RAD8               TO WS-RAD                              
325800           PERFORM S02-SKRIV                                              
325900           ADD +1                  TO RAD-IX                              
326000                                                                          
326100        END-IF                                                            
326200     END-IF                                                               
326300     .                                                                    
326400     EJECT                                                                
326500 GCC-SKRIV-AVSLUTNING SECTION.                                            
326600                                                                          
326700     MOVE LINK-IDDISTR TO TEST-IDDISTR                                    
326800                                                                          
326900* En speciell text för refill distr. USA + dealer distr.                  
327000* gäller bara IDPSN=90 och 91                                             
327100                                                                          
327200     IF (CDC OR DDC-SE) AND (DIST35-REFILL-NA        OR                   
327800                             DIST07-USA-RETAILER     OR                   
327900                             DIST07-CAN-RETAILER     OR                   
328000                             DIST32-TEXT-90-91)                           
328100                                                                          
328200       IF SPEC-TEXT-90-91-OK                                              
328300                                                                          
328400         MOVE PRT-EQUAL-53         TO PRT-RADSKIP                         
328500         MOVE RAD10NA              TO WS-RAD                              
328600         PERFORM S02-SKRIV                                                
328700       END-IF                                                             
328800     END-IF                                                               
328900*                                                                         
329000     IF CDC AND                                                           
329100        DIST07-NA-CUSTOMERS OR DIST35-REFILL-NA     OR                    
329200        DIST07-KINA         OR DIST35-REFILL-CN     OR                    
329300        DIST07-INDIEN       OR DIST35-CDC-IN-REFILL OR                    
329400        DIST07-KOREA        OR DIST35-CDC-KR-REFILL OR                    
329500        DIST07-TURKEY       OR DIST35-CDC-TR-REFILL OR                    
329600        DIST07-MALAYSIA     OR DIST35-CDC-MY-REFILL OR                    
329610        DIST07-THAILAND     OR DIST35-CDC-TH-REFILL OR                    
329620        DIST07-TAIWAN       OR DIST35-CDC-TW-REFILL OR                    
329630        DIST07-MEXICO       OR DIST35-CDC-MX-REFILL OR                    
329640        DIST07-BRAZIL       OR DIST35-CDC-BR-REFILL OR                    
329650                               DIST35-CDC-AE-REFILL OR                    
329660        DIST07-S-AFRICA     OR DIST35-CDC-ZA-REFILL                       
329670                                                                          
329800       MOVE PRT-EQUAL-55           TO PRT-RADSKIP                         
329900       MOVE RAD11                  TO WS-RAD                              
330000       PERFORM S02-SKRIV                                                  
330100     END-IF                                                               
330200                                                                          
330300     IF (CDC OR GOOD-DDC) AND (DIST32-SPEC-TEXT)                          
330400                                                                          
330500        MOVE PRT-EQUAL-56          TO PRT-RADSKIP                         
330600        MOVE RAD12                 TO WS-RAD                              
330700        PERFORM S02-SKRIV                                                 
330800                                                                          
330900        MOVE PRT-AFTER-1           TO PRT-RADSKIP                         
331000        MOVE RAD13                 TO WS-RAD                              
331100        PERFORM S02-SKRIV                                                 
331200                                                                          
331300        MOVE PRT-AFTER-1           TO PRT-RADSKIP                         
331400        MOVE RAD14                 TO WS-RAD                              
331500        PERFORM S02-SKRIV                                                 
331600     ELSE                                                                 
331700        IF CDC AND DIST32-SAUDI                                           
331800           MOVE PRT-EQUAL-55       TO PRT-RADSKIP                         
331900           MOVE RAD17              TO WS-RAD                              
332000           PERFORM S02-SKRIV                                              
332100                                                                          
332200           MOVE PRT-AFTER-1        TO PRT-RADSKIP                         
332300           MOVE RAD18              TO WS-RAD                              
332400           PERFORM S02-SKRIV                                              
332500                                                                          
332600           MOVE PRT-AFTER-1        TO PRT-RADSKIP                         
332700           MOVE RAD19              TO WS-RAD                              
332800           PERFORM S02-SKRIV                                              
332900        END-IF                                                            
333000     END-IF                                                               
333100                                                                          
333200     IF NDC-NA                                                            
333300       MOVE PRT-EQUAL-55           TO PRT-RADSKIP                         
333400       MOVE +1                       TO TAB-IX                            
333500       PERFORM UNTIL TAB-IX > MAX-TAB-IX                                  
333600        INSPECT FINAL-IDORDNR7 (TAB-IX)                                   
333700                REPLACING LEADING ZERO BY SPACE                           
333800        ADD +1                     TO TAB-IX                              
333900       END-PERFORM                                                        
334000       MOVE RAD11NA                TO WS-RAD                              
334100       PERFORM S02-SKRIV                                                  
334200                                                                          
334300       MOVE PRT-EQUAL-61           TO PRT-RADSKIP                         
334400                                                                          
334500       IF NDC-US-RU                                                       
334600         MOVE 'Rutherford, NJ      ' TO DGR-SIGN-ORT-OVR                  
334700       END-IF                                                             
334800                                                                          
334900       IF NDC-US-LA                                                       
335000         MOVE 'Carson, CA          ' TO DGR-SIGN-ORT-OVR                  
335100       END-IF                                                             
335200                                                                          
335300       IF NDC-CA                                                          
335400         MOVE 'Missisauga, Ontario ' TO DGR-SIGN-ORT-OVR                  
335500       END-IF                                                             
335600                                                                          
335700       IF NDC-JP                                                          
335800         MOVE 'Nagoya              ' TO DGR-SIGN-ORT-OVR                  
335900       END-IF                                                             
336000                                                                          
336100       IF NDC-AU                                                          
336200         MOVE 'Minto               ' TO DGR-SIGN-ORT-OVR                  
336300       END-IF                                                             
336400                                                                          
336500       MOVE RAD16A-NAPF            TO WS-RAD                              
336600     ELSE                                                                 
336700       MOVE PRT-EQUAL-63           TO PRT-RADSKIP                         
336800       MOVE RAD16-EU               TO WS-RAD                              
336900     END-IF                                                               
337000                                                                          
337100     PERFORM S02-SKRIV                                                    
337200                                                                          
337300     IF NDC-NA                                                            
337400       MOVE PRT-AFTER-1            TO PRT-RADSKIP                         
337500       MOVE RAD16B-NAPF            TO WS-RAD                              
337600       PERFORM S02-SKRIV                                                  
337700     END-IF                                                               
337800     .                                                                    
337900     EJECT                                                                
338000 GCD-SKRIV-EQ-VARDE SECTION.                                              
338100                                                                          
338200     MOVE BLK-KDKOLLI(IX1)         TO KDKOLLI-SW                          
338300     IF NDC-NA                                                            
338400       MOVE WS-FIBREBOARDBOX       TO RAD9-BOX                            
338500     ELSE                                                                 
338600       IF FIBREBOARDBOX                                                   
338700          MOVE WS-FIBREBOARDBOX    TO RAD9-BOX                            
338800       ELSE                                                               
338900          IF PLYWOODBOX                                                   
339000             MOVE WS-PLYWOODBOX    TO RAD9-BOX                            
339100          ELSE                                                            
339200             MOVE WS-FIBREBOARDBOX TO RAD9-BOX                            
339300          END-IF                                                          
339400       END-IF                                                             
339500     END-IF                                                               
339600                                                                          
339700     MOVE PRT-AFTER-1              TO PRT-RADSKIP                         
339800     MOVE RAD9                     TO WS-RAD                              
339900     PERFORM S02-SKRIV                                                    
340000     ADD +1                        TO RAD-IX                              
340100                                                                          
340200     COMPUTE WS-SUEQFG = WS-SUEQFG + (BLK-SUEQFG(IX1) + 0.1)              
340300     MOVE WS-SUEQFG                TO DGR-SUEQFG                          
340400                                                                          
340500     MOVE PRT-AFTER-1              TO PRT-RADSKIP                         
340600     MOVE RAD10                    TO WS-RAD                              
340700     PERFORM S02-SKRIV                                                    
340800     ADD +1                        TO RAD-IX                              
340900                                                                          
341000     .                                                                    
341100     EJECT                                                                
341200 GD-RAKNA-UT-ANTAL-SIDOR SECTION.                                         
341300                                                                          
341400     MOVE +0                       TO WS-RADER                            
341500     MOVE +1                       TO IX1                                 
341600                                      WS-SIDA-TOT                         
341700     PERFORM UNTIL IX1 > MAX-IX                                           
341800        IF BLK-IDKOLLI (IX1) > 0                                          
341900           ADD BLK-RADER(IX1)      TO WS-RADER                            
342000           IF WS-RADER + BLK-RADER(IX1 + 1) > MAX-RAD-IX                  
342100              ADD +1               TO WS-SIDA-TOT                         
342200              MOVE +0              TO WS-RADER                            
342300           END-IF                                                         
342400        END-IF                                                            
342500        ADD +1                     TO IX1                                 
342600     END-PERFORM                                                          
342700                                                                          
342800     MOVE WS-SIDA-TOT              TO DGR-SIDA-TOT                        
342900                                                                          
343000     .                                                                    
343100     EJECT                                                                
343200 S01-OPPNA-PRINTER SECTION.                                               
343300                                                                          
343400     CALL W006PRR1 USING PRT-SPOOL-A4S                                    
343500                         PRT-OPEN                                         
343600                         WS-PRT-IDPRTLST                                  
343700                         ALT-PCB                                          
343800                         LISB-PCB                                         
343900                         WS-PRT-IDLIST                                    
344000                         WS-PRT-DUMMY                                     
344100                         WS-PRT-DUMMY                                     
344200                                                                          
344300     .                                                                    
344400     EJECT                                                                
344500 S02-SKRIV SECTION.                                                       
344600                                                                          
344700     CALL W006PRR1 USING PRT-SPOOL-A4S                                    
344800                         PRT-WRITE                                        
344900                         WS-PRT-IDPRTLST                                  
345000                         ALT-PCB                                          
345100                         LISB-PCB                                         
345200                         WS-PRT-IDLIST                                    
345300                         PRT-RADSKIP                                      
345400                         WS-PRT-LISTRAD                                   
345500     .                                                                    
345600     EJECT                                                                
345700 S10-STAENG-PRINTER SECTION.                                              
345800                                                                          
345900     CALL W006PRR1 USING PRT-SPOOL-A4S                                    
346000                         PRT-CLOSE                                        
346100                         WS-PRT-IDPRTLST                                  
346200                         ALT-PCB                                          
346300                         LISB-PCB                                         
346400                         WS-PRT-IDLIST                                    
346500                         WS-PRT-DUMMY                                     
346600                         WS-PRT-DUMMY                                     
346700                                                                          
346800     .                                                                    
346900     EJECT                                                                
347000                                                                          
347100 S90-SEND-OPEN SECTION.                                                   
347200                                                                          
347300     MOVE WS-ADRESS-DP                    TO SEND-ADDISPABS               
347400     MOVE 'OPEN'                          TO SEND-KDFUNC                  
347500     CALL WZ01SEND USING SEND-CONTROL-AREA                                
347600                         SEND-OPEN-AREA                                   
347700     IF SEND-KDRC > ZERO                                                  
347800       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
347900       STRING 'WZ01SEND OPEN ERROR RC=' KDRC-DISPLAY                      
348000       DELIMITED BY SIZE INTO FELTEXT                                     
348100       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
348200     END-IF                                                               
348300     .                                                                    
348400     SKIP3                                                                
348500 S91-PUT-DOC-HDR SECTION.                                                 
348600                                                                          
348700     MOVE 'PUT'                           TO SEND-KDFUNC                  
348800     MOVE WZ04-001-IDCOM                  TO SEND-IDCOM                   
348900     MOVE LENGTH OF HDR-AREA              TO SEND-KVDLEN                  
349000     CALL WZ01SEND USING SEND-CONTROL-AREA                                
349100                         SEND-KVDLEN                                      
349200                         HDR-AREA                                         
349300     IF SEND-KDRC > ZERO                                                  
349400       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
349500       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
349600       DELIMITED BY SIZE INTO FELTEXT                                     
349700       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
349800     END-IF                                                               
349900     .                                                                    
350000     SKIP3                                                                
350100 S92-PUT-DOC-HEAD SECTION.                                                
350200                                                                          
350300     MOVE 'PUT'                           TO SEND-KDFUNC                  
350400     MOVE WZ04-001-IDCOM                  TO SEND-IDCOM                   
350500     MOVE LENGTH OF DOC-HEAD-AREA         TO SEND-KVDLEN                  
350600     CALL WZ01SEND USING SEND-CONTROL-AREA                                
350700                         SEND-KVDLEN                                      
350800                         DOC-HEAD-AREA                                    
350900     IF SEND-KDRC > ZERO                                                  
351000       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
351100       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
351200       DELIMITED BY SIZE INTO FELTEXT                                     
351300       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
351400     END-IF                                                               
351500     .                                                                    
351600     SKIP3                                                                
351700 S93-PUT-DOC-LINE SECTION.                                                
351800                                                                          
351900     MOVE 'PUT'                           TO SEND-KDFUNC                  
352000     MOVE WZ04-001-IDCOM                  TO SEND-IDCOM                   
352100     MOVE LENGTH OF DOC-LINE-AREA         TO SEND-KVDLEN                  
352200     CALL WZ01SEND USING SEND-CONTROL-AREA                                
352300                         SEND-KVDLEN                                      
352400                         DOC-LINE-AREA                                    
352500     IF SEND-KDRC > ZERO                                                  
352600       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
352700       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
352800       DELIMITED BY SIZE INTO FELTEXT                                     
352900       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
353000     END-IF                                                               
353100     .                                                                    
353200     EJECT                                                                
353300 S94-PUT-DOC-FOOT SECTION.                                                
353400                                                                          
353500     MOVE 'PUT'                           TO SEND-KDFUNC                  
353600     MOVE WZ04-001-IDCOM                  TO SEND-IDCOM                   
353700     MOVE LENGTH OF DOC-LINE-AREA         TO SEND-KVDLEN                  
353800     CALL WZ01SEND USING SEND-CONTROL-AREA                                
353900                         SEND-KVDLEN                                      
354000                         DOC-FOOT-AREA                                    
354100     IF SEND-KDRC > ZERO                                                  
354200       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
354300       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
354400       DELIMITED BY SIZE INTO FELTEXT                                     
354500       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
354600     END-IF                                                               
354700     .                                                                    
354800     EJECT                                                                
354900 S95-SEND-CLOSE SECTION.                                                  
355000                                                                          
355100     MOVE 'CLOSE'                         TO SEND-KDFUNC                  
355200     CALL WZ01SEND USING SEND-CONTROL-AREA                                
355300     .                                                                    
355400     EJECT                                                                
355500* --- IMS SEKTIONER ---                                                   
355600     SKIP3                                                                
355700 IMS-GET-1165-WDR2 SECTION.                                               
355800     STRING 'WL116501(WDGXKEY  =' W-1165-X ')'                            
355900          DELIMITED BY SIZE INTO SSA1                                     
356000     STRING 'WL116512(KDFGTRP  =' W-KDFGTRP-X ')'                         
356100          DELIMITED BY SIZE INTO SSA2                                     
356200     MOVE '    ' TO GODK-STATUSKODER                                      
356300     CALL CBLTDLI USING GU 1165-PCB DLI-IO-AREA SSA1 SSA2                 
356400     MOVE 1165-STATUS-CODE TO STATUS-WS                                   
356500     PERFORM IMS-STATUSKONTROLL                                           
356600     .                                                                    
356700     EJECT                                                                
356800 IMS-GET-GMTA01 SECTION.                                                  
356900                                                                          
357000     STRING 'WLGMTA01(IDGMT   >=' W-IDGMT-X ')'                           
357100          DELIMITED BY SIZE INTO SSA1                                     
357200     MOVE '  '     TO GODK-STATUSKODER                                    
357300     CALL CBLTDLI USING GU GMTA-PCB DLI-IO-AREA-B201 SSA1                 
357400     MOVE GMTA-STATUS-CODE TO STATUS-WS                                   
357500     PERFORM IMS-STATUSKONTROLL                                           
357600     .                                                                    
357700    SKIP3                                                                 
357800 IMS-GET-WDE6 SECTION.                                                    
357900     STRING 'WDE601  (IDPRODNR =' W-IDPRODNR-X ')'                        
358000            DELIMITED BY SIZE INTO SSA1                                   
358100     STRING 'WDE611  (IDKOLLI  =' W-IDKOLLI-X ')'                         
358200            DELIMITED BY SIZE INTO SSA2                                   
358300     MOVE '    ' TO GODK-STATUSKODER                                      
358400     CALL CBLTDLI USING GU    WDE6-PCB DLI-IO-AREA SSA1 SSA2              
358500     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
358600     PERFORM IMS-STATUSKONTROLL                                           
358700     .                                                                    
358800     EJECT                                                                
358900 IMS-GU-WL451311  SECTION.                                                
359000                                                                          
359100     STRING 'WL451301(WDGXKEY  =' W-4513-X ')'                            
359200          DELIMITED BY SIZE INTO SSA1                                     
359300     STRING 'WL451311(DASKEPPN =' W-DASKEPPN-X ')'                        
359400          DELIMITED BY SIZE INTO SSA2                                     
359500     MOVE '  ' TO GODK-STATUSKODER                                        
359600     CALL CBLTDLI USING GU 4513-PCB DLI-IO-AREA-4514 SSA1 SSA2            
359700     MOVE 4513-STATUS-CODE TO STATUS-WS                                   
359800     PERFORM IMS-STATUSKONTROLL                                           
359900     .                                                                    
360000     SKIP2                                                                
360100 IMS-GNP-WL451321 SECTION.                                                
360200                                                                          
360300     STRING 'WL451321(WDGXKEY  =' W-4516-X ')'                            
360400          DELIMITED BY SIZE INTO SSA1                                     
360500     MOVE '  GE' TO GODK-STATUSKODER                                      
360600     CALL CBLTDLI USING GNP 4513-PCB DLI-IO-AREA-4516 SSA1                
360700     MOVE 4513-STATUS-CODE TO STATUS-WS                                   
360800     PERFORM IMS-STATUSKONTROLL                                           
360900     .                                                                    
361000     SKIP3                                                                
361100 IMS-GU-WDB601    SECTION.                                                
361200     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
361300          DELIMITED BY SIZE INTO SSA1                                     
361400     MOVE '  GE' TO GODK-STATUSKODER                                      
361500     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
361600     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
361700     PERFORM IMS-STATUSKONTROLL                                           
361800     .                                                                    
361900     SKIP3                                                                
362000 IMS-STATUSKONTROLL SECTION.                                              
362100                                                                          
362200     SET STATUS-IX TO 1                                                   
362300     SEARCH GODK-STATUS                                                   
362400       AT END                                                             
362500       STRING 'STATUSKOD FRÅN IMS ' STATUS-WS                             
362600       DELIMITED BY SIZE INTO FELTEXT                                     
362700       CALL FELLOG                                                        
362800       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
362900     END-SEARCH                                                           
363000     .                                                                    
