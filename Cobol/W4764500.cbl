000300 ID DIVISION.                                                             
000400 PROGRAM-ID.     W4764500.                                                
000500 AUTHOR.         CAMELIA OLGRENER.                                        
000600 DATE-WRITTEN.   07/04/17.                                                
000700 DATE-COMPILED.                                                           
000800                                                                          
000900*    FUNKTION:                                                            
001000*        SKAPAR FIL FÖR EDI-ÖVERFÖRING AV KOLLIDATA TILL DHL              
001200*                                                                         
001300*        PROGRAMMET LÄSER      WDGX4545 (WDR4)                            
001400*                              WDE6                                       
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
003300     SELECT W47645A                    ASSIGN TO W47645D1.                
003310     SKIP2                                                                
003320*          --- FIL FÖR SKAPANDE AV TOTALLIST TILL DHL-KURIR               
003330     SELECT W47645B                    ASSIGN TO W47645D2.                
003340     SKIP2                                                                
003410*          --- FIL FÖR RENSNING AV WDR4                                   
003420     SELECT W47645C                    ASSIGN TO W47645D3.                
003430     EJECT                                                                
003500 DATA DIVISION.                                                           
003600     SKIP2                                                                
003700 FILE SECTION.                                                            
003800                                                                          
004800 FD  W47645A                                                              
004900     RECORDING       V                                                    
005000     BLOCK CONTAINS  0.                                                   
005100                                                                          
005209 01  UT-001-POST.                                                         
005210*    03  -COPY WEDI001G -PRE UT-                                          
005211                                                                          
005213 01  UT-UNB-POST.                                                         
005214*    03  -COPY WEDIUNBG -PRE UT-                                          
005215                                                                          
005217 01  UT-UNH-POST.                                                         
005218*    03  -COPY WEDIUNHG -PRE UT-.                                         
005219                                                                          
005221 01  UT-BGM-POST.                                                         
005222*    03  -COPY WEDIBGMG -PRE UT-.                                         
005223                                                                          
005225 01  UT-FTX-POST.                                                         
005226*    03  -COPY WEDIFTXG -PRE UT-.                                         
005227                                                                          
005229 01  UT-VIKT-CNT-POST.                                                    
005230*    03  -COPY WEDICNTG -PRE UT-VIKT- .                                   
005231                                                                          
005233 01  UT-KOLLI-CNT-POST.                                                   
005234*    03  -COPY WEDICNTG -PRE UT-KOLLI- .                                  
005235                                                                          
005237 01  UT-VOLYM-CNT-POST.                                                   
005238*    03  -COPY WEDICNTG -PRE UT-VOLYM- .                                  
005239                                                                          
005241 01  UT-TDT-POST.                                                         
005242*    03  -COPY WEDITDTG -PRE UT-.                                         
005243                                                                          
005245 01  UT-TSR-POST.                                                         
005246*    03  -COPY WEDITSRG -PRE UT-.                                         
005247                                                                          
005249 01  UT-LOC-POST.                                                         
005250*    03  -COPY WEDILOCD -PRE UT-.                                         
005251                                                                          
005253 01  UT-DTM-POST.                                                         
005254*    03  -COPY WEDIDTMG -PRE UT-.                                         
005255                                                                          
005257 01  UT-SEN-NAD-POST.                                                     
005258*    03  -COPY WEDINADG -PRE UT-SEN-.                                     
005259                                                                          
005261 01  UT-REC-NAD-POST.                                                     
005262*    03  -COPY WEDINADG -PRE UT-REC-.                                     
005263                                                                          
005265 01  UT-CNI-POST.                                                         
005266*    03  -COPY WEDICNIG -PRE UT-.                                         
005267                                                                          
005269 01  UT-MOA-POST.                                                         
005270*    03  -COPY WEDIMOAG -PRE UT-.                                         
005271                                                                          
005277 01  UT-RFF-POST.                                                         
005278*    03  -COPY WEDIRFFG -PRE UT-.                                         
005279                                                                          
005281 01  UT-CTA-POST.                                                         
005282*    03  -COPY WEDICTAG -PRE UT-.                                         
005283                                                                          
005285 01  UT-COM-POST.                                                         
005286*    03  -COPY WEDICOMG -PRE UT-.                                         
005287                                                                          
005289 01  UT-GID-POST.                                                         
005290*    03  -COPY WEDIGIDG -PRE UT-.                                         
005291                                                                          
005293 01  UT-GID-RFF-POST.                                                     
005294*    03  -COPY WEDIRFFG -PRE UT-GID-.                                     
005295                                                                          
005297 01  UT-MEA-POST.                                                         
005298*    03  -COPY WEDIMEAG -PRE UT-.                                         
005299                                                                          
005301 01  UT-DIM-POST.                                                         
005302*    03  -COPY WEDIDIMG -PRE UT-.                                         
005303                                                                          
005309 01  UT-003-POST.                                                         
005310*    03  -COPY WEDI003G -PRE UT-.                                         
005311                                                                          
005312     EJECT                                                                
005313 FD  W47645B                                                              
005314     RECORDING       F                                                    
005315     BLOCK CONTAINS  0.                                                   
005316                                                                          
005317 01  UT2-POST.                                                            
005318     03  UT2-POST-L      PIC X(86).                                       
005330     EJECT                                                                
005340 FD  W47645C                                                              
005350     RECORDING       F                                                    
005360     BLOCK CONTAINS  0.                                                   
005370                                                                          
005380 01  UT3-POST.                                                            
005390     03  -COPY WDGX4546   -L.                                             
005391     EJECT                                                                
005400 WORKING-STORAGE SECTION.                                                 
005500                                                                          
005600                                                                          
005700*    -- CHECKED BY WY2000                                                 
005800 77  IDPGM                       PIC X(8)    VALUE 'W4764500'.            
005900 77  JA                          PIC X       VALUE 'J'.                   
006000 77  NEJ                         PIC X       VALUE 'N'.                   
006100 77  PUNKT                       PIC X       VALUE '.'.                   
006110 77  WS-KOLLI-RAK                PIC 9(3)    VALUE ZERO.                  
006114 77  WS-VLORDBTO                 PIC 9(4)V9(3) VALUE ZERO.                
006115 77  WS-ANTAL-KOLLI              PIC 9(3)    VALUE 1.                     
006120 77  WS-IDDC                     PIC X(2)    VALUE SPACE.                 
006121 77  WS-IDDISTR                  PIC S9(5)   VALUE ZERO.                  
006130 77  WS-IDKUNDNR                 PIC S9(7)   VALUE ZERO.                  
006150 77  WS-IDKONTO                  PIC 9(9)    VALUE ZERO.                  
006160 77  WS-IDPRODNR                 PIC S9(7)   VALUE ZERO.                  
006200                                                                          
006705 77  FIRST-CASE-SW               PIC X       VALUE 'J'.                   
006706     88  FIRST-CASE                          VALUE 'J'.                   
006707                                                                          
006708 77  KOLLI-SKEPP                 PIC X       VALUE 'N'.                   
006709     88  KOLLI-OK                            VALUE 'J'.                   
006710                                                                          
006800     EJECT                                                                
007100 01  ARBETSFALT.                                                          
007200                                                                          
007300     03 WS-NUMBER-OF-SEGMENTS       PIC 9(06) VALUE ZERO.                 
007400     03 WS-MESSAGE-REF-NO           PIC 9(06) VALUE ZERO.                 
007500                                                                          
008400     03 IN-IDPTYP                   PIC X(03) VALUE SPACE.                
008500     03 EDI-IDPTYP                  PIC X(03) VALUE SPACE.                
008600                                                                          
008700     03 WS-IDAWB.                                                         
008800        05 WS-IDAWB-9               PIC 9(09) VALUE ZERO.                 
008900        05 WS-IDAWB-10              PIC 9(01) VALUE ZERO.                 
009040                                                                          
009050     03 WS-REF-NR.                                                        
009060        05 WS-IDKUNDNR-REF          PIC 9(06) VALUE ZERO.                 
009061        05 WS-TECKEN                PIC X(01) VALUE '*'.                  
009070        05 WS-IDORDNR5              PIC 9(05) VALUE ZERO.                 
009080                                                                          
009090     03 WS-VKORDBTO                 PIC 9(6)V9 VALUE ZERO.                
009091     03 WS-VKORDBTO-ALFA REDEFINES WS-VKORDBTO.                           
009093        05 WS-VKORDBTO-W            PIC X(7).                             
009094                                                                          
009095     03 WS-VKORDBTO-TOT             PIC 9(6)V9 VALUE ZERO.                
009096     03 WS-VKORDBTO-TOT-NUM REDEFINES WS-VKORDBTO-TOT.                    
009097        05 WS-VKORDBTO-TOT-W        PIC 9(7).                             
009098                                                                          
009099     03 WS-VLORDBTO-TOT             PIC 9(4)V9(3) VALUE ZERO.             
009100     03 WS-VLORDBTO-TOT-NUM REDEFINES WS-VLORDBTO-TOT.                    
009101        05 WS-VLORDBTO-TOT-W        PIC 9(7).                             
009102                                                                          
009103     03 WS-SUORDV                   PIC 9(9)V9(2) VALUE ZERO.             
009104     03 WS-SUORDV-NUM REDEFINES WS-SUORDV.                                
009105        05 WS-SUORDV-W              PIC 9(11).                            
009106                                                                          
011500 01  KONSTANTER.                                                          
011600                                                                          
011700     03 001-UPPGIFTER.                                                    
011800                                                                          
011900       05 WC-001-IDPTYP                 PIC X(03) VALUE '001'.            
012000       05 WC-001-LENGTH                 PIC 9(03) VALUE  073 .            
012100       05 WC-001-VOLVO-PARTS            PIC X(04) VALUE 'VPAR'.           
012200       05 WC-001-VOLVO-AMTRIX           PIC X(04) VALUE 'VAMP'.           
012300       05 WC-001-IFCSUM95               PIC X(08)                         
012310                                        VALUE 'IFCSUM95'.                 
012600                                                                          
012700                                                                          
012701     03 UNB-UPPGIFTER.                                                    
012702                                                                          
012703       05 WC-UNB-IDPTYP                 PIC X(03) VALUE 'UNB'.            
012704       05 WC-UNB-LENGTH                 PIC 9(03) VALUE  125 .            
012705       05 WC-UNB-UNOA                   PIC X(04) VALUE 'UNOA'.           
012706       05 WC-UNB-2                      PIC X(01) VALUE '1'.              
012707       05 WC-UNB-DHL                    PIC X(14)                         
012708                                        VALUE '100072        '.           
012709       05 WC-UNB-CDC-CAR-PARTS          PIC X(14)                         
012710                                        VALUE '01441         '.           
012714                                                                          
012715                                                                          
012716     03 UNH-UPPGIFTER.                                                    
012720                                                                          
012730       05 WC-UNH-IDPTYP                 PIC X(03) VALUE 'UNH'.            
012740       05 WC-UNH-LENGTH                 PIC 9(03) VALUE  072 .            
012750       05 WC-UNH-TYP                    PIC X(06) VALUE 'IFCSUM'.         
012760       05 WC-UNH-VERSION-NO             PIC X(03) VALUE 'D  '.            
012770       05 WC-UNH-REL-NO                 PIC X(03) VALUE '95A'.            
012780       05 WC-UNH-AGENCY                 PIC X(02) VALUE 'UN'.             
012791                                                                          
012792                                                                          
012800     03 BGM-UPPGIFTER.                                                    
012900                                                                          
013000       05 WC-BGM-IDPTYP                 PIC X(03) VALUE 'BGM'.            
013100       05 WC-BGM-LENGTH                 PIC 9(03) VALUE  085 .            
013200       05 WC-BGM-CARGO-MANIFEST         PIC X(03) VALUE '703'.            
013600                                                                          
013700                                                                          
013800     03 FTX-UPPGIFTER.                                                    
013900                                                                          
014000       05 WC-FTX-IDPTYP                 PIC X(03) VALUE 'FTX'.            
014100       05 WC-FTX-LENGTH                 PIC 9(03) VALUE  368 .            
014200       05 WC-FTX-GENERAL-INFO           PIC X(03) VALUE 'AAA'.            
014210       05 WC-FTX-FREE-TEXT              PIC X(11) VALUE                   
014300                                                 'SPARE PARTS'.           
014400                                                                          
014500     03 CNT-UPPGIFTER.                                                    
014600                                                                          
014700       05 WC-CNT-IDPTYP                 PIC X(03) VALUE 'CNT'.            
014800       05 WC-CNT-LENGTH                 PIC 9(03) VALUE  024.             
014900       05 WC-CNT-TOT-GROSS-WEIGHT       PIC X(03) VALUE '7  '.            
015000       05 WC-CNT-TOT-NUM-OF-PACK        PIC X(03) VALUE '11 '.            
015010       05 WC-CNT-TOT-VOLUME             PIC X(03) VALUE '15 '.            
015100       05 WC-CNT-KILOGRAM               PIC X(03) VALUE 'KGM'.            
015200       05 WC-CNT-PIECES                 PIC X(03) VALUE 'PCE'.            
015300       05 WC-CNT-CUBIC-METRE            PIC X(03) VALUE 'MTQ'.            
015400                                                                          
015500                                                                          
015600     03 TDT-UPPGIFTER.                                                    
015700                                                                          
015800       05 WC-TDT-IDPTYP                 PIC X(03) VALUE 'TDT'.            
015900       05 WC-TDT-LENGTH                 PIC 9(03) VALUE  205.             
016000       05 WC-TDT-MAIN-CARRIAGE-TRANSP   PIC X(03) VALUE '20 '.            
016200                                                                          
016300                                                                          
016310       05 WC-TSR-IDPTYP                 PIC X(03) VALUE 'TSR'.            
016320       05 WC-TSR-LENGTH                 PIC 9(03) VALUE  045.             
016330       05 WC-TSR-SERVICE-COD            PIC X(03) VALUE 'ECT'.            
016340                                                                          
016350                                                                          
016400     03 LOC-UPPGIFTER.                                                    
016500                                                                          
016600       05 WC-LOC-IDPTYP                 PIC X(03) VALUE 'LOC'.            
016700       05 WC-LOC-LENGTH                 PIC 9(03) VALUE  256.             
016800       05 WC-LOC-PLACE-OF-DESTINATION   PIC X(03) VALUE '8  '.            
016810       05 WC-LOC-CODE-LIST-AGENCY       PIC X(03) VALUE '87 '.            
016900                                                                          
017000                                                                          
017100     03 DTM-UPPGIFTER.                                                    
017200                                                                          
017300       05 WC-DTM-IDPTYP                 PIC X(03) VALUE 'DTM'.            
017400       05 WC-DTM-LENGTH                 PIC 9(03) VALUE 041.              
017500       05 WC-DTM-QUAL                   PIC X(03) VALUE '11 '.            
017600       05 WC-DTM-CCYYMMDDHHMM           PIC X(03) VALUE '203'.            
017700                                                                          
017800                                                                          
017900     03 NAD-UPPGIFTER.                                                    
018000                                                                          
018100       05 WC-NAD-IDPTYP                 PIC X(03) VALUE 'NAD'.            
018200       05 WC-NAD-LENGTH                 PIC 9(03) VALUE 558.              
018300       05 WC-NAD-DOCUMENT-SENDER        PIC X(03) VALUE 'CZ '.            
018310       05 WC-NAD-DOCUMENT-RECEIVER      PIC X(03) VALUE 'CN '.            
018400       05 WC-NAD-VOLVO-ZIP-CODE         PIC X(09) VALUE                   
018500                                                  '40531    '.            
018510       05 WC-NAD-VOLVO-COUNTRY          PIC X(03) VALUE 'SE '.            
018600                                                                          
018700                                                                          
018800     03 CNI-UPPGIFTER.                                                    
018900                                                                          
019000       05 WC-CNI-IDPTYP                 PIC X(03) VALUE 'CNI'.            
019100       05 WC-CNI-LENGTH                 PIC 9(03) VALUE 084.              
019200                                                                          
019300                                                                          
019400     03 MOA-UPPGIFTER.                                                    
019500                                                                          
019600       05 WC-MOA-IDPTYP                 PIC X(03) VALUE 'MOA'.            
019700       05 WC-MOA-LENGTH                 PIC 9(03) VALUE 030.              
019800       05 WC-MOA-INVOICE-AMOUNT         PIC X(03) VALUE '43 '.            
019900       05 WC-MOA-SEK                    PIC X(17) VALUE 'SEK'.            
020000                                                                          
020100                                                                          
020900     03 RFF-UPPGIFTER.                                                    
021000                                                                          
021100       05 WC-RFF-IDPTYP                 PIC X(03) VALUE 'RFF'.            
021200       05 WC-RFF-LENGTH                 PIC 9(03) VALUE 079.              
021300       05 WC-RFF-REF-QUAL               PIC X(03) VALUE 'CU '.            
021400                                                                          
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
024100       05 WC-GID-LENGTH                 PIC 9(03) VALUE 203.              
024200                                                                          
024300                                                                          
025500     03 MEA-UPPGIFTER.                                                    
025600                                                                          
025700       05 WC-MEA-IDPTYP                 PIC X(03) VALUE 'MEA'.            
025800       05 WC-MEA-LENGTH                 PIC 9(03) VALUE 144.              
025900                                                                          
026000       05 WC-MEA-WEIGHT                 PIC X(03) VALUE 'WT '.            
026200       05 WC-MEA-GROSS-WEIGHT           PIC X(03) VALUE 'G  '.            
026400       05 WC-MEA-KILOGRAM               PIC X(03) VALUE 'KGM'.            
026700                                                                          
026800                                                                          
026900     03 DIM-UPPGIFTER.                                                    
027000                                                                          
027100       05 WC-DIM-IDPTYP                 PIC X(03) VALUE 'DIM'.            
027200       05 WC-DIM-LENGTH                 PIC 9(03) VALUE 51.               
027300                                                                          
027400       05 WC-DIM-DIMENTION-QUAL         PIC X(03) VALUE '2  '.            
027700       05 WC-DIM-CENTIMETER             PIC X(03) VALUE 'CMT'.            
027800                                                                          
027810     03 GID-RFF-UPPGIFTER.                                                
027820                                                                          
027830       05 WC-GID-RFF-IDPTYP             PIC X(03) VALUE 'RFF'.            
027840       05 WC-GID-RFF-LENGTH             PIC 9(03) VALUE 079.              
027850       05 WC-GID-RFF-REF-QUAL           PIC X(03) VALUE 'CU '.            
027860                                                                          
028510     03 003-UPPGIFTER.                                                    
028520                                                                          
028530       05 WC-003-IDPTYP                 PIC X(03) VALUE '003'.            
028540       05 WC-003-LENGTH                 PIC 9(03) VALUE 73.               
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
030110 01  TEST-IDDISTR       PIC 9(5)   COMP-3.                                
030120                                                                          
030130*01  FILLER -COPY WWDIST83    -RED TEST-IDDISTR.                          
030140     EJECT                                                                
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
043610 01  FILLER                      PIC X(24)   VALUE                        
043620                                 'UT-AREA-START  '.                       
043900                                                                          
043910 01  FILLER                      PIC X(24) VALUE '001-AREA'.              
043920 01  001-AREA.                                                            
043930*    03  -COPY WEDI001G                                                   
043940                                                                          
043950 01  FILLER                      PIC X(24) VALUE 'UNB-AREA'.              
043960 01  UNB-AREA.                                                            
043970*    03  -COPY WEDIUNBG.                                                  
043980                                                                          
043990 01  FILLER                      PIC X(24) VALUE 'UNH-AREA'.              
043991 01  UNH-AREA.                                                            
043992*    03  -COPY WEDIUNHG.                                                  
043993                                                                          
043994 01  FILLER                      PIC X(24) VALUE 'BGM-AREA'.              
043995 01  BGM-AREA.                                                            
043996*    03  -COPY WEDIBGMG.                                                  
043997                                                                          
043998 01  FILLER                      PIC X(24) VALUE 'FTX-AREA'.              
043999 01  FTX-AREA.                                                            
044000*    03  -COPY WEDIFTXG.                                                  
044001                                                                          
044002 01  FILLER                      PIC X(24) VALUE 'VIKT-CNT-AREA'.         
044003 01  VIKT-CNT-AREA.                                                       
044004*    03  -COPY WEDICNTG -PRE VIKT- .                                      
044005                                                                          
044006 01  FILLER                      PIC X(24) VALUE 'KOLLI-CNT-AREA'.        
044007 01  KOLLI-CNT-AREA.                                                      
044008*    03  -COPY WEDICNTG -PRE KOLLI- .                                     
044009                                                                          
044010 01  FILLER                      PIC X(24) VALUE 'VOLYM-CNT-AREA'.        
044011 01  VOLYM-CNT-AREA.                                                      
044012*    03  -COPY WEDICNTG -PRE VOLYM- .                                     
044013                                                                          
044014 01  FILLER                      PIC X(24) VALUE 'TDT-AREA'.              
044015 01  TDT-AREA.                                                            
044016*    03  -COPY WEDITDTG.                                                  
044017                                                                          
044018 01  FILLER                      PIC X(24) VALUE 'TSR-AREA'.              
044019 01  TSR-AREA.                                                            
044020*    03  -COPY WEDITSRG.                                                  
044021                                                                          
044022 01  FILLER                      PIC X(24) VALUE 'LOC-AREA'.              
044023 01  LOC-AREA.                                                            
044024*    03  -COPY WEDILOCD.                                                  
044025                                                                          
044026 01  FILLER                      PIC X(24) VALUE 'DTM-AREA'.              
044027 01  DTM-AREA.                                                            
044028*    03  -COPY WEDIDTMG.                                                  
044029                                                                          
044030 01  FILLER                      PIC X(24) VALUE 'SEN-NAD-AREA'.          
044031 01  SEN-NAD-AREA.                                                        
044032*    03  -COPY WEDINADG -PRE SEN- .                                       
044033                                                                          
044034 01  FILLER                      PIC X(24) VALUE 'REC-NAD-AREA'.          
044035 01  REC-NAD-AREA.                                                        
044036*    03  -COPY WEDINADG -PRE REC- .                                       
044037                                                                          
044038 01  FILLER                      PIC X(24) VALUE 'CNI-AREA'.              
044039 01  CNI-AREA.                                                            
044040*    03  -COPY WEDICNIG.                                                  
044041                                                                          
044042 01  FILLER                      PIC X(24) VALUE 'MOA-AREA'.              
044043 01  MOA-AREA.                                                            
044044*    03  -COPY WEDIMOAG.                                                  
044045                                                                          
044050 01  FILLER                      PIC X(24) VALUE 'RFF-AREA'.              
044051 01  RFF-AREA.                                                            
044052*    03  -COPY WEDIRFFG.                                                  
044053                                                                          
044054 01  FILLER                      PIC X(24) VALUE 'CTA-AREA'.              
044055 01  CTA-AREA.                                                            
044056*    03  -COPY WEDICTAG.                                                  
044057                                                                          
044058 01  FILLER                      PIC X(24) VALUE 'COM-AREA'.              
044059 01  COM-AREA.                                                            
044060*    03  -COPY WEDICOMG.                                                  
044061                                                                          
044062 01  FILLER                      PIC X(24) VALUE 'GID-AREA'.              
044063 01  GID-AREA.                                                            
044064*    03  -COPY WEDIGIDG.                                                  
044065                                                                          
044066 01  FILLER                      PIC X(24) VALUE 'GID-RFF-AREA'.          
044067 01  GID-RFF-AREA.                                                        
044068*    03  -COPY WEDIRFFG -PRE GID-.                                        
044069                                                                          
044070 01  FILLER                      PIC X(24) VALUE 'MEA-AREA'.              
044071 01  MEA-AREA.                                                            
044072*    03  -COPY WEDIMEAG.                                                  
044073                                                                          
044074 01  FILLER                      PIC X(24) VALUE 'DIM-AREA'.              
044075 01  DIM-AREA.                                                            
044076*    03  -COPY WEDIDIMG.                                                  
044077                                                                          
044082 01  FILLER                      PIC X(24) VALUE '003-AREA'.              
044083 01  003-AREA.                                                            
044084*    03  -COPY WEDI003G.                                                  
044085*                                                                         
044086 01  FILLER                      PIC X(24) VALUE 'UT2-AREA'.              
044087 01  UT2-AREA.                                                            
044089     03  UT2-IDAWB         PIC 9(10) VALUE ZERO.                          
044090     03  UT2-IDKONTO       PIC 9(09).                                     
044091     03  UT2-REF           PIC X(12).                                     
044092     03  UT2-IDCITY        PIC X(03).                                     
044093     03  UT2-ANTAL-KOLLI   PIC 9(03).                                     
044094     03  UT2-VKORDBTO      PIC 9(6)V9(1).                                 
044095     03  UT2-VLORDBTO      PIC 9(4)V9(3).                                 
044097     03  UT2-BEGMT         PIC X(35).                                     
044098     EJECT                                                                
044099 01  FILLER                      PIC X(24) VALUE 'UT3-AREA'.              
044100 01  UT3-AREA.                                                            
044101     03  -COPY WDGX4546  -PRE UT3-.                                       
044109     EJECT                                                                
044110*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
044120*                                                                         
044200     SKIP3                                                                
044300 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
044400     SKIP3                                                                
044500 01  NYCKLAR-TILL-DLI.                                                    
044600                                                                          
044700   03  W-WDGXKEY-4545-X.                                                  
044800     05  W-IDHTYP-4545           PIC X(4)    VALUE '4545'.                
044900     05  FILLER                  PIC X(26)   VALUE LOW-VALUE.             
045000                                                                          
045050   03  W-IDPRODNR-X.                                                      
045060     05  W-IDPRODNR              PIC S9(7)   VALUE ZERO  COMP-3.          
045070*                                                                         
045080   03  W-IDKOLLI-X.                                                       
045090     05  W-IDKOLLI               PIC S9(5)   VALUE ZERO  COMP-3.          
045100*                                                                         
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
047600 01  SSA1                        PIC X(128).                              
047700 01  SSA2                        PIC X(64).                               
047710 01  SSA3                        PIC X(64).                               
047720 01  SSA4                        PIC X(64).                               
047800     EJECT                                                                
047900*    --- IMS FUNKTIONSKODER                                               
048000*01  -COPY W0003                                                          
048100     EJECT                                                                
048200*    ---  DLI INPUT-OUTPUT AREA                                           
048281 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-4546'.         
048290 01  DLI-IO-4546.                                                         
048291*    03  -COPY WDGX4546                                                   
048292     EJECT                                                                
048298 01  FILLER                      PIC X(16)   VALUE 'DLI-IO-E611'.         
048303 01  DLI-IO-WDE611.                                                       
048305*    03  -COPY WDE611                                                     
049600                                                                          
049700 LINKAGE SECTION.                                                         
049800                                                                          
050000*01  -COPY W0008  -PRE 4545-                                              
050100     05  FILLER                  PIC X.                                   
050200                                                                          
050300*01  -COPY W0008  -PRE WDE6-                                              
050400     05  FILLER                  PIC X.                                   
050500                                                                          
050800     EJECT                                                                
051100 PROCEDURE DIVISION  USING 4545-PCB WDE6-PCB.                             
051200                                                                          
051300 MAIN SECTION.                                                            
051400     ENTRY 'DLITCBL' USING 4545-PCB WDE6-PCB.                             
051700                                                                          
051800     PERFORM A-INIT                                                       
051900                                                                          
051910     PERFORM IMS-GU-WDGX4545                                              
051911     PERFORM IMS-GNP-WDGX4546                                             
052030                                                                          
052100     PERFORM UNTIL SEGMENT-SAKNAS                                         
052101       MOVE 4546-IDPRODNR TO W-IDPRODNR                                   
052102       MOVE 4546-IDKOLLI TO W-IDKOLLI                                     
052103       PERFORM IMS-GU-WDE611                                              
052104                                                                          
052105       IF KOLLI-KDKOLSTA > 6                                              
052106         IF FIRST-CASE                                                    
052110           PERFORM S31-SKRIV-001-POST                                     
052120           PERFORM S32-SKRIV-UNB-POST                                     
052130         END-IF                                                           
052200                                                                          
052404         PERFORM C-BEHANDLA-KOLLI-GEN                                     
052407                                                                          
053260         PERFORM D-NOLLSTALL-SEG                                          
053261         MOVE JA TO KOLLI-SKEPP                                           
053270       END-IF                                                             
053750                                                                          
053760       PERFORM IMS-GNP-WDGX4546                                           
054600     END-PERFORM                                                          
054800                                                                          
054810     IF KOLLI-OK                                                          
054900       PERFORM S34-SKRIV-003-POST                                         
055000     END-IF                                                               
055100                                                                          
055400     PERFORM Z-FINIT                                                      
055500                                                                          
055600     MOVE ZERO TO RETURN-CODE                                             
055700     GOBACK                                                               
055800     .                                                                    
055900     EJECT                                                                
056000 A-INIT SECTION.                                                          
056100                                                                          
056300                                                                          
056400     OPEN OUTPUT W47645A                                                  
056500                                                                          
056510     OPEN OUTPUT W47645B                                                  
056520                                                                          
056530     OPEN OUTPUT W47645C                                                  
056540                                                                          
056600     ACCEPT DAGENS-DATUM  FROM DATE                                       
056700     MOVE IDPGM TO POSTSUM-PROGNAMN                                       
056710                                                                          
056740     MOVE JA    TO FIRST-CASE-SW                                          
056741                                                                          
056742     INITIALIZE                UNH-AREA                                   
056743                               BGM-AREA                                   
056744                               DTM-AREA                                   
056745                               MOA-AREA                                   
056746                               FTX-AREA                                   
056747                               VIKT-CNT-AREA                              
056748                               KOLLI-CNT-AREA                             
056749                               VOLYM-CNT-AREA                             
056800     .                                                                    
056900     EJECT                                                                
057000                                                                          
057120 D-NOLLSTALL-SEG SECTION.                                                 
057200                                                                          
057210     COMPUTE  WS-MESSAGE-REF-NO =                                         
057220              WS-MESSAGE-REF-NO + 1                                       
057221     MOVE ZERO              TO WS-KOLLI-RAK                               
057222     MOVE +1                TO WS-NUMBER-OF-SEGMENTS                      
057230                                                                          
057300     INITIALIZE                UNH-AREA                                   
057400                               BGM-AREA                                   
057410                               DTM-AREA                                   
057420                               MOA-AREA                                   
057500                               FTX-AREA                                   
057600                               VIKT-CNT-AREA                              
057700                               KOLLI-CNT-AREA                             
057701                               VOLYM-CNT-AREA                             
059600     .                                                                    
059700     EJECT                                                                
059800 C-BEHANDLA-KOLLI-GEN SECTION.                                            
059810                                                                          
059900     PERFORM CA-NOLLSTALL-KOLLI                                           
059901                                                                          
059902     PERFORM CB-SKAPA-UNH-POST                                            
059903                                                                          
059904     PERFORM CC-SKAPA-DTM-POST                                            
059905                                                                          
059906     PERFORM CD-SKAPA-MOA-POST                                            
059907                                                                          
059908     PERFORM CE-SKAPA-FTX-POST                                            
059909                                                                          
059910     PERFORM CF-SKAPA-CNT-POST                                            
060300                                                                          
060400     PERFORM CG-SKAPA-GEN-KOLLI-INFO                                      
060600                                                                          
060900     PERFORM CH-SKAPA-KOLLI-INFO                                          
060901                                                                          
060910     PERFORM CJ-SKAPA-TOTAL-FIL                                           
060922                                                                          
060923     PERFORM CK-SKAPA-RENSNINGSFIL                                        
060924                                                                          
060925     MOVE ZERO TO WS-VKORDBTO-TOT                                         
060926     MOVE ZERO TO WS-VLORDBTO-TOT                                         
060927     .                                                                    
060928     EJECT                                                                
060929 CA-NOLLSTALL-KOLLI SECTION.                                              
060930                                                                          
060951     INITIALIZE               RFF-AREA                                    
060952                              TDT-AREA                                    
060953                              TSR-AREA                                    
060960                              LOC-AREA                                    
060970                              CNI-AREA                                    
060980                              SEN-NAD-AREA                                
060981                              REC-NAD-AREA                                
060990                              CTA-AREA                                    
060991                              COM-AREA                                    
060992                              RFF-AREA                                    
060993                              GID-AREA                                    
060994                              MEA-AREA                                    
060995                              DIM-AREA                                    
060996                              GID-RFF-AREA                                
060997     .                                                                    
060998     EJECT                                                                
064700 CB-SKAPA-UNH-POST SECTION.                                               
064800                                                                          
064900     MOVE WC-UNH-IDPTYP          TO UNH-IDPTYP                            
065000     MOVE WC-UNH-LENGTH          TO UNH-LENGTH                            
065100                                                                          
065200     MOVE WS-MESSAGE-REF-NO      TO                                       
065300                         UNH-0062-MESSAGE-REFERENCE                       
065400                                                                          
065500     MOVE WC-UNH-TYP             TO                                       
065600                         UNH-0065-MESSAGE-TYPE-ID                         
065700     MOVE WC-UNH-VERSION-NO      TO                                       
065800                         UNH-0052-MESSAGE-VERSION                         
065900     MOVE WC-UNH-REL-NO          TO                                       
066000                         UNH-0054-MESSAGE-RELEASE                         
066100     MOVE WC-UNH-AGENCY          TO                                       
066200                         UNH-0051-CONTROLING-AGENCY                       
066600     .                                                                    
066700     EJECT                                                                
068300 CC-SKAPA-DTM-POST SECTION.                                               
068400                                                                          
068410     MOVE WC-DTM-IDPTYP          TO DTM-IDPTYP                            
068420     MOVE WC-DTM-LENGTH          TO DTM-LENGTH                            
068430     MOVE WC-DTM-QUAL            TO DTM-2005-DATE-TIME-PER-QUAL           
068440                                                                          
068450     MOVE FUNCTION CURRENT-DATE(1:8) TO WS-CURRENT-DATE                   
068460     MOVE FUNCTION CURRENT-DATE(9:4) TO WS-CURRENT-TIME                   
068470     MOVE WS-CURRENT-DATE-TIME        TO                                  
068480                            DTM-2380-DATE-TIME-PER                        
068490                                                                          
068491     MOVE WC-DTM-CCYYMMDDHHMM         TO                                  
068492                            DTM-2379-DATE-TIME-PER-FORM                   
068494     .                                                                    
068495     EJECT                                                                
068496 CD-SKAPA-MOA-POST SECTION.                                               
068497                                                                          
068498     MOVE WC-MOA-IDPTYP          TO MOA-IDPTYP                            
068499     MOVE WC-MOA-LENGTH          TO MOA-LENGTH                            
068500                                                                          
068501     MOVE WC-MOA-INVOICE-AMOUNT  TO                                       
068502                        MOA-5025-MON-AMOUNT-QUAL                          
068505     IF KOLLI-SUORDV-KOLLI > ZERO                                         
068506       MOVE KOLLI-SUORDV-KOLLI   TO WS-SUORDV                             
068507       MOVE WS-SUORDV-W          TO MOA-5004-MONETARY-AMOUNT              
068508       MOVE KOLLI-KDVALISO       TO MOA-6345-CURRENCY-CODED               
068509     ELSE                                                                 
068510       IF KOLLI-SUORDV-LOC > ZERO                                         
068511         MOVE KOLLI-SUORDV-LOC   TO WS-SUORDV                             
068512         MOVE WS-SUORDV-W        TO MOA-5004-MONETARY-AMOUNT              
068513         MOVE KOLLI-KDVALISO     TO MOA-6345-CURRENCY-CODED               
068514       ELSE                                                               
068515         IF KOLLI-SUORDV-LOCPREL > ZERO                                   
068516           MOVE KOLLI-SUORDV-LOCPREL                                      
068517                                 TO WS-SUORDV                             
068518           MOVE WS-SUORDV-W      TO MOA-5004-MONETARY-AMOUNT              
068519           MOVE KOLLI-KDVALISO   TO MOA-6345-CURRENCY-CODED               
068520         END-IF                                                           
068521       END-IF                                                             
068522     END-IF                                                               
068523     .                                                                    
068524     EJECT                                                                
068525 CE-SKAPA-FTX-POST SECTION.                                               
068600                                                                          
068700     MOVE WC-FTX-IDPTYP          TO FTX-IDPTYP                            
068800     MOVE WC-FTX-LENGTH          TO FTX-LENGTH                            
068900     MOVE WC-FTX-GENERAL-INFO    TO FTX-4451-TEXT-SUB-QUAL                
068910     MOVE WC-FTX-FREE-TEXT       TO FTX-4440-FREE-TEXT                    
069300     .                                                                    
069400     EJECT                                                                
069700 CF-SKAPA-CNT-POST SECTION.                                               
069800                                                                          
069900*    VIKT                                                                 
070100     MOVE WC-CNT-IDPTYP           TO VIKT-CNT-IDPTYP                      
070200     MOVE WC-CNT-LENGTH           TO VIKT-CNT-LENGTH                      
070201                                                                          
070203     MOVE WC-CNT-TOT-GROSS-WEIGHT TO VIKT-CNT-6069-CONTR-QUAL             
070204                                                                          
070205     MOVE 4546-VKORDBTO           TO WS-VKORDBTO-TOT                      
070206                                                                          
070300     IF WS-VKORDBTO-TOT > ZERO                                            
071110        MOVE WS-VKORDBTO-TOT-W    TO VIKT-CNT-6066-CONTR-VALUE            
071140     ELSE                                                                 
071150        MOVE 1                    TO WS-VKORDBTO-TOT                      
071160        MOVE WS-VKORDBTO-TOT-W    TO VIKT-CNT-6066-CONTR-VALUE            
071170     END-IF                                                               
071180                                                                          
071200     MOVE WC-CNT-KILOGRAM         TO VIKT-CNT-6411-MEA-UNIT-Q             
071300                                                                          
071400                                                                          
071500*    ANTAL KOLLI                                                          
071700     MOVE WC-CNT-IDPTYP           TO KOLLI-CNT-IDPTYP                     
071800     MOVE WC-CNT-LENGTH           TO KOLLI-CNT-LENGTH                     
071900                                                                          
072000     MOVE WC-CNT-TOT-NUM-OF-PACK  TO KOLLI-CNT-6069-CONTR-QUAL            
072100     MOVE 1                       TO KOLLI-CNT-6066-CONTR-VALUE           
072200                                                                          
073300     MOVE WC-CNT-PIECES           TO KOLLI-CNT-6411-MEA-UNIT-Q            
073400                                                                          
073410*    VOLYM                                                                
073420     MOVE WC-CNT-IDPTYP           TO VOLYM-CNT-IDPTYP                     
073430     MOVE WC-CNT-LENGTH           TO VOLYM-CNT-LENGTH                     
073431                                                                          
073440     MOVE WC-CNT-TOT-VOLUME       TO VOLYM-CNT-6069-CONTR-QUAL            
073460     MOVE 4546-VLORDBTO           TO WS-VLORDBTO-TOT                      
073461     MOVE WS-VLORDBTO-TOT-W       TO VOLYM-CNT-6066-CONTR-VALUE           
073462                                                                          
073470     MOVE WC-CNT-CUBIC-METRE      TO VOLYM-CNT-6411-MEA-UNIT-Q            
073600     .                                                                    
073700     EJECT                                                                
076610 CG-SKAPA-GEN-KOLLI-INFO SECTION.                                         
076700                                                                          
076800     PERFORM CG0-SKAPA-BGM-POST                                           
076900     PERFORM S21-SKRIV-GEN-INFO                                           
077000*                                                                         
080021     PERFORM CG1-SKAPA-RFF-POST                                           
080022     PERFORM CG2-SKAPA-TDT-POST                                           
080023     PERFORM CG3-SKAPA-TSR-POST                                           
080030     PERFORM CG4-SKAPA-LOC-POST                                           
080050     PERFORM CG5-SKAPA-CNI-POST                                           
080070     PERFORM CG6-SKAPA-SEN-NAD-POST                                       
080080     PERFORM CG7-SKAPA-CTA-POST                                           
080090     PERFORM CG8-SKAPA-COM-POST                                           
080091     PERFORM CG9-SKAPA-REC-NAD-POST                                       
080092*    PERFORM CG9-SKAPA-NAD-RFF-POST                                       
080093                                                                          
080094     PERFORM S22-SKRIV-GEN-KOLLI-INFO                                     
080097     .                                                                    
080098     EJECT                                                                
080099 CG0-SKAPA-BGM-POST SECTION.                                              
080100                                                                          
080101     MOVE WC-BGM-IDPTYP          TO BGM-IDPTYP                            
080102     MOVE WC-BGM-LENGTH          TO BGM-LENGTH                            
080103     MOVE WC-BGM-CARGO-MANIFEST  TO BGM-1001-DOCUMENT-NAME                
080104     MOVE 4546-IDAWB             TO WS-IDAWB-9                            
080105     MOVE 4546-REKSIFFR-AWB      TO WS-IDAWB-10                           
080106     MOVE WS-IDAWB               TO BGM-1004-DOCUMENT-NUMBER              
080107     .                                                                    
080108     EJECT                                                                
080109 CG1-SKAPA-RFF-POST SECTION.                                              
080110                                                                          
080111     MOVE WC-RFF-IDPTYP          TO RFF-IDPTYP                            
080112     MOVE WC-RFF-LENGTH          TO RFF-LENGTH                            
080113                                                                          
080114     MOVE WC-RFF-REF-QUAL        TO RFF-1153-REFERENCE-QUAL               
080115                                                                          
080116     MOVE 4546-IDKUNDNR          TO WS-IDKUNDNR-REF                       
080117     MOVE 4546-IDORDNR5          TO WS-IDORDNR5                           
080118     MOVE WS-REF-NR              TO RFF-1154-REFERENCE-NO                 
080119     .                                                                    
080120     EJECT                                                                
080121 CG2-SKAPA-TDT-POST SECTION.                                              
080122                                                                          
080123     MOVE WC-TDT-IDPTYP          TO TDT-IDPTYP                            
080124     MOVE WC-TDT-LENGTH          TO TDT-LENGTH                            
080125     MOVE WC-TDT-MAIN-CARRIAGE-TRANSP                                     
080126                                 TO TDT-8051-TRANSP-STAGE-QUAL            
080127     .                                                                    
080128     EJECT                                                                
080129 CG3-SKAPA-TSR-POST SECTION.                                              
080130                                                                          
080131     MOVE WC-TSR-IDPTYP          TO TSR-IDPTYP                            
080132     MOVE WC-TSR-LENGTH          TO TSR-LENGTH                            
080133     MOVE WC-TSR-SERVICE-COD     TO TSR-7273-SERVICE-REQ-CODED            
080134     .                                                                    
080135     EJECT                                                                
080136 CG4-SKAPA-LOC-POST SECTION.                                              
080137                                                                          
080138     MOVE WC-LOC-IDPTYP          TO LOC-IDPTYP                            
080139     MOVE WC-LOC-LENGTH          TO LOC-LENGTH                            
080140     MOVE WC-LOC-PLACE-OF-DESTINATION TO                                  
080141                              LOC-3227-PLACE-LOC-QUAL                     
080142     MOVE 4546-IDCITY            TO LOC-3225-LOC-ID                       
080143                                                                          
080144     MOVE WC-LOC-CODE-LIST-AGENCY                                         
080145                                 TO LOC-3055-CODE-LIST-AGENCY             
080146     .                                                                    
080147     EJECT                                                                
080150 CG5-SKAPA-CNI-POST SECTION.                                              
080200                                                                          
080300     MOVE WC-CNI-IDPTYP          TO CNI-IDPTYP                            
080400     MOVE WC-CNI-LENGTH          TO CNI-LENGTH                            
080500                                                                          
080800     MOVE WS-KOLLI-RAK           TO                                       
080900                        CNI-1490-CONSOLID-ITEM-NO                         
081700     .                                                                    
081800     EJECT                                                                
081810 CG6-SKAPA-SEN-NAD-POST SECTION.                                          
081820                                                                          
081830     MOVE WC-NAD-IDPTYP          TO SEN-NAD-IDPTYP                        
081840     MOVE WC-NAD-LENGTH          TO SEN-NAD-LENGTH                        
081850                                                                          
081860     MOVE WC-NAD-DOCUMENT-SENDER  TO SEN-NAD-3035-PARTY-QUAL              
081861     MOVE 4546-IDKONTO            TO WS-IDKONTO                           
081862     MOVE WS-IDKONTO              TO SEN-NAD-3039-PARTY-ID                
081870     MOVE WC-NAD-VOLVO-ZIP-CODE   TO                                      
081880                              SEN-NAD-3251-POSTCODE-ID                    
081881     MOVE WC-NAD-VOLVO-COUNTRY    TO                                      
081882                              SEN-NAD-3207-COUNTRY-CODED                  
081883     .                                                                    
081884     EJECT                                                                
081905 CG7-SKAPA-CTA-POST SECTION.                                              
081906                                                                          
081907     MOVE WC-CTA-IDPTYP          TO CTA-IDPTYP                            
081908     MOVE WC-CTA-LENGTH          TO CTA-LENGTH                            
081909                                                                          
081910     MOVE WC-CTA-INFO-CONTACT    TO CTA-3139-CONTACT-FUNCTION             
081911     MOVE 'BJÖRN JENSEN'         TO CTA-3412-DEP-OR-EMP                   
081912     .                                                                    
081913     EJECT                                                                
081919 CG8-SKAPA-COM-POST SECTION.                                              
081920                                                                          
081921     MOVE WC-COM-IDPTYP          TO COM-IDPTYP                            
081922     MOVE WC-COM-LENGTH          TO COM-LENGTH                            
081923                                                                          
081924     MOVE '123456'               TO COM-3148-COMUNICATION-NO              
081925     MOVE WC-COM-TELEFAX         TO COM-3155-COMUNICATION-QUAL            
081926     .                                                                    
081927     EJECT                                                                
081928 CG9-SKAPA-REC-NAD-POST SECTION.                                          
081929                                                                          
081930     MOVE WC-NAD-IDPTYP          TO REC-NAD-IDPTYP                        
081931     MOVE WC-NAD-LENGTH          TO REC-NAD-LENGTH                        
081932                                                                          
081933     MOVE WC-NAD-DOCUMENT-RECEIVER                                        
081934                                 TO REC-NAD-3035-PARTY-QUAL               
081935     MOVE 4546-BEGMT-RAD1        TO                                       
081936                              REC-NAD-3036-PARTY-NAME-1                   
081937     MOVE 4546-ADGMT-GATA         TO                                      
081938                              REC-NAD-3042-STREET-PBOX-1                  
081941     MOVE 4546-ADGMT-PADR         TO                                      
081942                              REC-NAD-3164-CITY-NAME                      
081943     MOVE 4546-ADGMT-LAND         TO                                      
081944                              REC-NAD-3229-COUNTRY-ID                     
081945*                                                                         
081946     PERFORM CG9A-TA-FRAM-POSTNR                                          
081950*                                                                         
081960     MOVE KOLLI-IDDISTR           TO TEST-IDDISTR                         
081974                                                                          
081975     IF DIST83-DHL-FI                                                     
081976       MOVE 'FI '                 TO                                      
081977                              REC-NAD-3207-COUNTRY-CODED                  
081978     END-IF                                                               
081979                                                                          
081980     IF DIST83-DHL-BE                                                     
081981       MOVE 'BE '                 TO                                      
081982                              REC-NAD-3207-COUNTRY-CODED                  
081983     END-IF                                                               
081984                                                                          
081985     IF DIST83-DHL-GB                                                     
081986       MOVE 'GB '                 TO                                      
081987                              REC-NAD-3207-COUNTRY-CODED                  
081988     END-IF                                                               
081989                                                                          
081990     IF DIST83-DHL-FR                                                     
081991       MOVE 'FR '                 TO                                      
081992                              REC-NAD-3207-COUNTRY-CODED                  
081993     END-IF                                                               
081994                                                                          
081995     IF DIST83-DHL-GR                                                     
081996       MOVE 'GR '                 TO                                      
081997                              REC-NAD-3207-COUNTRY-CODED                  
081998     END-IF                                                               
081999                                                                          
082000     IF DIST83-DHL-NL                                                     
082001       MOVE 'NL '                 TO                                      
082002                              REC-NAD-3207-COUNTRY-CODED                  
082003     END-IF                                                               
082004                                                                          
082005     IF DIST83-DHL-IE                                                     
082006       MOVE 'IE '                 TO                                      
082007                              REC-NAD-3207-COUNTRY-CODED                  
082008     END-IF                                                               
082009                                                                          
082010     IF DIST83-DHL-IT                                                     
082011       MOVE 'IT '                 TO                                      
082012                              REC-NAD-3207-COUNTRY-CODED                  
082013     END-IF                                                               
082014                                                                          
082015     IF DIST83-DHL-PT                                                     
082016       MOVE 'PT '                 TO                                      
082017                              REC-NAD-3207-COUNTRY-CODED                  
082018     END-IF                                                               
082019                                                                          
082020     IF DIST83-DHL-ES                                                     
082021       MOVE 'ES '                 TO                                      
082022                              REC-NAD-3207-COUNTRY-CODED                  
082023     END-IF                                                               
082024                                                                          
082025     IF DIST83-DHL-DE                                                     
082026       MOVE 'DE '                 TO                                      
082027                              REC-NAD-3207-COUNTRY-CODED                  
082028     END-IF                                                               
082029                                                                          
082030     IF DIST83-DHL-SI                                                     
082031       MOVE 'SI '                 TO                                      
082032                              REC-NAD-3207-COUNTRY-CODED                  
082033     END-IF                                                               
082034                                                                          
082035     IF DIST83-DHL-HU                                                     
082036       MOVE 'HU '                 TO                                      
082037                              REC-NAD-3207-COUNTRY-CODED                  
082038     END-IF                                                               
082039                                                                          
082040     IF DIST83-DHL-CZ                                                     
082041       MOVE 'CZ '                 TO                                      
082042                              REC-NAD-3207-COUNTRY-CODED                  
082043     END-IF                                                               
082044                                                                          
082045     IF DIST83-DHL-SK                                                     
082046       MOVE 'SK '                 TO                                      
082047                              REC-NAD-3207-COUNTRY-CODED                  
082048     END-IF                                                               
082049                                                                          
082050     IF DIST83-DHL-AT                                                     
082051       MOVE 'AT '                 TO                                      
082052                              REC-NAD-3207-COUNTRY-CODED                  
082053     END-IF                                                               
082054                                                                          
082055     IF DIST83-DHL-BG                                                     
082056       MOVE 'BG '                 TO                                      
082057                              REC-NAD-3207-COUNTRY-CODED                  
082058     END-IF                                                               
082059                                                                          
082060     IF DIST83-DHL-RO                                                     
082061       MOVE 'RO '                 TO                                      
082062                              REC-NAD-3207-COUNTRY-CODED                  
082063     END-IF                                                               
082064                                                                          
082065     IF DIST83-DHL-PL                                                     
082066       MOVE 'PL '                 TO                                      
082067                              REC-NAD-3207-COUNTRY-CODED                  
082068     END-IF                                                               
082069                                                                          
082070     IF DIST83-DHL-MT                                                     
082071       MOVE 'MT '                 TO                                      
082072                              REC-NAD-3207-COUNTRY-CODED                  
082073     END-IF                                                               
082074                                                                          
082075     IF DIST83-DHL-CY                                                     
082076       MOVE 'CY '                 TO                                      
082077                              REC-NAD-3207-COUNTRY-CODED                  
082078     END-IF                                                               
082079     .                                                                    
082080     EJECT                                                                
082081 CG9A-TA-FRAM-POSTNR SECTION.                                             
082082                                                                          
082083     MOVE KOLLI-IDDISTR        TO TEST-IDDISTR                            
082084                                                                          
082085     IF DIST83-DHL-GB                                                     
082086       MOVE 'UNITED KINGDOM'   TO REC-NAD-3229-COUNTRY-ID                 
082087     END-IF                                                               
082088                                                                          
082089     MOVE 4546-ADGMT-PADR(1:9) TO REC-NAD-3251-POSTCODE-ID                
082090     .                                                                    
082100     EJECT                                                                
082108 CH-SKAPA-KOLLI-INFO SECTION.                                             
082109                                                                          
082110     PERFORM CH1-SKAPA-GID-POST                                           
082111     PERFORM CH2-SKAPA-MEA-POST                                           
082112     PERFORM CH3-SKAPA-DIM-POST                                           
082113     PERFORM CH4-SKAPA-GID-RFF-POST                                       
082114                                                                          
082115     PERFORM S23-SKRIV-KOLLI-INFO                                         
082120     .                                                                    
082200     EJECT                                                                
091300 CH1-SKAPA-GID-POST SECTION.                                              
091400                                                                          
091500     MOVE WC-GID-IDPTYP          TO GID-IDPTYP                            
091600     MOVE WC-GID-LENGTH          TO GID-LENGTH                            
091700                                                                          
091800     MOVE WS-ANTAL-KOLLI         TO GID-1496-GOODS-ITEM-NUMBER            
092500     .                                                                    
092600     EJECT                                                                
092700 CH2-SKAPA-MEA-POST SECTION.                                              
092800                                                                          
092810     MOVE WC-MEA-IDPTYP          TO MEA-IDPTYP                            
092820     MOVE WC-MEA-LENGTH          TO MEA-LENGTH                            
092830                                                                          
092840     MOVE WC-MEA-WEIGHT          TO                                       
092850                                 MEA-6311-MEASURE-QUAL                    
092860     MOVE WC-MEA-GROSS-WEIGHT    TO                                       
092870                                 MEA-6313-MEASURE-DIM                     
092880     MOVE WC-MEA-KILOGRAM        TO                                       
092890                                 MEA-6411-MEASURE-UNIT-Q                  
092891     IF 4546-VKORDBTO > ZERO                                              
092892       MOVE 4546-VKORDBTO        TO WS-VKORDBTO                           
092894       MOVE WS-VKORDBTO-W        TO                                       
092895                                 MEA-6314-MEASURE-VALUE                   
092900     ELSE                                                                 
092901        MOVE 1                   TO WS-VKORDBTO                           
092902        MOVE WS-VKORDBTO-W       TO                                       
092903                                 MEA-6314-MEASURE-VALUE                   
092910     END-IF                                                               
092935     .                                                                    
092936     EJECT                                                                
092937 CH3-SKAPA-DIM-POST SECTION.                                              
092938                                                                          
092939                                                                          
092940     MOVE WC-DIM-IDPTYP          TO DIM-IDPTYP                            
092941     MOVE WC-DIM-LENGTH          TO DIM-LENGTH                            
092942                                                                          
092943     MOVE WC-DIM-DIMENTION-QUAL  TO                                       
092944                                 DIM-6145-DIMENSION-QUAL                  
092945     MOVE WC-DIM-CENTIMETER      TO                                       
092946                                 DIM-6411-MEASURE-UNIT-QUAL               
092947                                                                          
092948     IF 4546-DIKOLLIL > ZERO                                              
092949        MOVE 4546-DIKOLLIL       TO DIM-6168-LENGTH-DIMENSION             
092951     ELSE                                                                 
092952        MOVE 100                 TO DIM-6168-LENGTH-DIMENSION             
092953     END-IF                                                               
092954                                                                          
092955     IF 4546-DIKOLLIB > ZERO                                              
092956        MOVE 4546-DIKOLLIB     TO DIM-6140-WIDTH-DIMENSION                
092958     ELSE                                                                 
092959        MOVE 100               TO DIM-6140-WIDTH-DIMENSION                
092960     END-IF                                                               
092961                                                                          
092962     IF 4546-DIKOLLIH > ZERO                                              
092963        MOVE 4546-DIKOLLIH     TO DIM-6008-HEIGHT-DIMENSION               
092965     ELSE                                                                 
092966        MOVE 100               TO DIM-6008-HEIGHT-DIMENSION               
092967     END-IF                                                               
092968     .                                                                    
092969     EJECT                                                                
092970 CH4-SKAPA-GID-RFF-POST SECTION.                                          
093000                                                                          
093100     MOVE WC-GID-RFF-IDPTYP      TO GID-RFF-IDPTYP                        
093200     MOVE WC-GID-RFF-LENGTH      TO GID-RFF-LENGTH                        
093300     MOVE WC-GID-RFF-REF-QUAL    TO GID-RFF-1153-REFERENCE-QUAL           
093700     MOVE 4546-IDKOLLI           TO GID-RFF-1154-REFERENCE-NO             
094300     .                                                                    
094400     EJECT                                                                
102450 S21-SKRIV-GEN-INFO SECTION.                                              
102500                                                                          
102700     WRITE UT-UNH-POST                FROM UNH-AREA                       
102800     MOVE UNH-AREA (1:3)              TO EDI-IDPTYP                       
102900     PERFORM S11-POSTSUM-UTPOST                                           
103000                                                                          
103200     WRITE UT-BGM-POST                FROM BGM-AREA                       
103300     MOVE BGM-AREA(1:3)               TO EDI-IDPTYP                       
103400     PERFORM S11-POSTSUM-UTPOST                                           
103500                                                                          
103700     WRITE UT-DTM-POST                FROM DTM-AREA                       
103800     MOVE DTM-AREA(1:3)               TO EDI-IDPTYP                       
103900     PERFORM S11-POSTSUM-UTPOST                                           
104000                                                                          
104200     WRITE UT-MOA-POST                FROM MOA-AREA                       
104300     MOVE MOA-AREA(1:3)               TO EDI-IDPTYP                       
104400     PERFORM S11-POSTSUM-UTPOST                                           
104500                                                                          
104700     WRITE UT-FTX-POST                FROM FTX-AREA                       
104800     MOVE FTX-AREA(1:3)               TO EDI-IDPTYP                       
104900     PERFORM S11-POSTSUM-UTPOST                                           
105000                                                                          
105200     WRITE UT-VIKT-CNT-POST           FROM VIKT-CNT-AREA                  
105300     MOVE VIKT-CNT-AREA(1:3)          TO EDI-IDPTYP                       
105400     PERFORM S11-POSTSUM-UTPOST                                           
105500                                                                          
105700     WRITE UT-KOLLI-CNT-POST          FROM KOLLI-CNT-AREA                 
105800     MOVE KOLLI-CNT-AREA(1:3)         TO EDI-IDPTYP                       
105900     PERFORM S11-POSTSUM-UTPOST                                           
106000                                                                          
106200     WRITE UT-VOLYM-CNT-POST          FROM VOLYM-CNT-AREA                 
106300     MOVE VOLYM-CNT-AREA(1:3)         TO EDI-IDPTYP                       
106400     PERFORM S11-POSTSUM-UTPOST                                           
107000     .                                                                    
107100     EJECT                                                                
107400 S22-SKRIV-GEN-KOLLI-INFO SECTION.                                        
107410                                                                          
107430     WRITE UT-RFF-POST               FROM RFF-AREA                        
107440     MOVE RFF-AREA(1:3)              TO EDI-IDPTYP                        
107450     PERFORM S11-POSTSUM-UTPOST                                           
107500                                                                          
107700     WRITE UT-TDT-POST               FROM TDT-AREA                        
107800     MOVE TDT-AREA(1:3)              TO EDI-IDPTYP                        
107900     PERFORM S11-POSTSUM-UTPOST                                           
108000                                                                          
108020     WRITE UT-TSR-POST               FROM TSR-AREA                        
108030     MOVE TSR-AREA(1:3)              TO EDI-IDPTYP                        
108040     PERFORM S11-POSTSUM-UTPOST                                           
108050                                                                          
108052     WRITE UT-LOC-POST               FROM LOC-AREA                        
108053     MOVE LOC-AREA(1:3)              TO EDI-IDPTYP                        
108054     PERFORM S11-POSTSUM-UTPOST                                           
108055                                                                          
108070     WRITE UT-CNI-POST               FROM CNI-AREA                        
108080     MOVE CNI-AREA(1:3)              TO EDI-IDPTYP                        
108090     PERFORM S11-POSTSUM-UTPOST                                           
108091                                                                          
108093     WRITE UT-SEN-NAD-POST           FROM SEN-NAD-AREA                    
108094     MOVE SEN-NAD-AREA(1:3)          TO EDI-IDPTYP                        
108095     PERFORM S11-POSTSUM-UTPOST                                           
108096                                                                          
108098     WRITE UT-CTA-POST               FROM CTA-AREA                        
108099     MOVE CTA-AREA(1:3)              TO EDI-IDPTYP                        
108100     PERFORM S11-POSTSUM-UTPOST                                           
108101                                                                          
108103     WRITE UT-COM-POST               FROM COM-AREA                        
108104     MOVE COM-AREA(1:3)              TO EDI-IDPTYP                        
108105     PERFORM S11-POSTSUM-UTPOST                                           
108106                                                                          
108108     WRITE UT-REC-NAD-POST           FROM REC-NAD-AREA                    
108109     MOVE REC-NAD-AREA(1:3)          TO EDI-IDPTYP                        
108110     PERFORM S11-POSTSUM-UTPOST                                           
108410     .                                                                    
108420     EJECT                                                                
108510 S23-SKRIV-KOLLI-INFO SECTION.                                            
108520                                                                          
108700     WRITE UT-GID-POST               FROM GID-AREA                        
108800     MOVE GID-AREA(1:3)              TO EDI-IDPTYP                        
108900     PERFORM S11-POSTSUM-UTPOST                                           
109500                                                                          
109700     WRITE UT-MEA-POST               FROM MEA-AREA                        
109800     MOVE MEA-AREA(1:3)              TO EDI-IDPTYP                        
109900     PERFORM S11-POSTSUM-UTPOST                                           
110000                                                                          
110002     WRITE UT-DIM-POST               FROM DIM-AREA                        
110003     MOVE DIM-AREA(1:3)              TO EDI-IDPTYP                        
110004     PERFORM S11-POSTSUM-UTPOST                                           
110005                                                                          
110007     WRITE UT-GID-RFF-POST           FROM GID-RFF-AREA                    
110008     MOVE GID-RFF-AREA(1:3)          TO EDI-IDPTYP                        
110009     PERFORM S11-POSTSUM-UTPOST                                           
114500     .                                                                    
114600     EJECT                                                                
116801 S31-SKRIV-001-POST SECTION.                                              
116802                                                                          
116804     INITIALIZE 001-AREA                                                  
116805                                                                          
116806     MOVE WC-001-IDPTYP          TO 001-IDPTYP                            
116807     MOVE WC-001-LENGTH          TO 001-LENGTH                            
116808                                                                          
116810     MOVE WC-001-VOLVO-PARTS     TO 001-SNODE-SEN-COMMON-NODE             
116811     MOVE WC-001-VOLVO-AMTRIX    TO 001-RNODE-REC-COMMON-NODE             
116812     MOVE WC-001-IFCSUM95        TO 001-VFILE-VIRTUAL-FILE-NAME           
116813                                                                          
116814     MOVE FUNCTION CURRENT-DATE(3:8) TO                                   
116815                              001-VFDATE-VIR-FILE-DATE                    
116816     MOVE FUNCTION CURRENT-DATE(9:4) TO WS-CURRENT-TIME                   
116817                              001-VFTIME-VIR-FILE-TIME                    
116822*                                                                         
116823*    COMPUTE WS-NUMBER-OF-SEGMENTS =                                      
116824*            WS-NUMBER-OF-SEGMENTS + 1                                    
116825*                                                                         
116826     WRITE UT-001-POST               FROM 001-AREA                        
116827     MOVE 001-AREA(1:3)              TO EDI-IDPTYP                        
116828     PERFORM S11-POSTSUM-UTPOST                                           
116836     .                                                                    
116839     EJECT                                                                
116841 S32-SKRIV-UNB-POST SECTION.                                              
116842                                                                          
116843     INITIALIZE UNB-AREA                                                  
116844                                                                          
116845     MOVE WC-UNB-IDPTYP          TO UNB-IDPTYP                            
116846     MOVE WC-UNB-LENGTH          TO UNB-LENGTH                            
116847                                                                          
116852     MOVE WC-UNB-UNOA            TO UNB-0001-SYNTAX-ID                    
116853     MOVE WC-UNB-2               TO UNB-0002-SYNTAX-VERSION-NO            
116854     MOVE WC-UNB-CDC-CAR-PARTS   TO UNB-0004-SENDER-ID                    
116857     MOVE WC-UNB-DHL             TO UNB-0010-RECIPIENT-ID                 
116861                                                                          
116863                                                                          
116882     MOVE FUNCTION CURRENT-DATE(3:8) TO UNB-0017-DATE                     
116884     MOVE FUNCTION CURRENT-DATE(9:4) TO UNB-0019-TIME                     
116887*                                                                         
116888*    COMPUTE WS-NUMBER-OF-SEGMENTS =                                      
116889*            WS-NUMBER-OF-SEGMENTS + 1                                    
116890*                                                                         
116891     WRITE UT-UNB-POST               FROM UNB-AREA                        
116894     MOVE UNB-AREA(1:3)              TO EDI-IDPTYP                        
116895     PERFORM S11-POSTSUM-UTPOST                                           
116896                                                                          
116897     MOVE NEJ                        TO FIRST-CASE-SW                     
116898     .                                                                    
116899     EJECT                                                                
116921 S34-SKRIV-003-POST SECTION.                                              
116922                                                                          
116923     INITIALIZE 003-AREA                                                  
116924                                                                          
116925     MOVE WC-003-IDPTYP          TO 003-IDPTYP                            
116926     MOVE WC-003-LENGTH          TO 003-LENGTH                            
116927                                                                          
116929     WRITE UT-003-POST           FROM 003-AREA                            
116930     MOVE 003-AREA(1:3)          TO EDI-IDPTYP                            
116931     PERFORM S11-POSTSUM-UTPOST                                           
116932     .                                                                    
116933     EJECT                                                                
116934 CJ-SKAPA-TOTAL-FIL SECTION.                                              
116935                                                                          
116981     MOVE 4546-IDAWB             TO WS-IDAWB-9                            
116982     MOVE 4546-REKSIFFR-AWB      TO WS-IDAWB-10                           
116983                                                                          
116984     IF UT2-IDAWB = WS-IDAWB                                              
116985       CONTINUE                                                           
116986     ELSE                                                                 
116987       MOVE WS-IDAWB           TO UT2-IDAWB                               
116988       MOVE 4546-IDKONTO       TO UT2-IDKONTO                             
116989       MOVE WS-REF-NR          TO UT2-REF                                 
116990       MOVE 4546-IDCITY        TO UT2-IDCITY                              
116991       MOVE WS-ANTAL-KOLLI     TO UT2-ANTAL-KOLLI                         
116992       MOVE WS-VKORDBTO-TOT    TO UT2-VKORDBTO                            
116993       MOVE WS-VLORDBTO-TOT    TO UT2-VLORDBTO                            
116995       MOVE 4546-BEGMT-RAD1    TO UT2-BEGMT                               
116996                                                                          
116997       WRITE UT2-POST          FROM UT2-AREA                              
116998       MOVE 'W47645B'          TO POSTSUM-FDNAMN                          
116999       MOVE 'W47645D2'         TO POSTSUM-DDNAMN2                         
117000       SKIP2                                                              
117001       CALL POSTSUM            USING  POSTSUM-PARM                        
117002     END-IF                                                               
117003     .                                                                    
117004     EJECT                                                                
117005 CK-SKAPA-RENSNINGSFIL SECTION.                                           
117006                                                                          
117008     MOVE 4546-WDGX4546          TO UT3-4546-WDGX4546                     
117009                                                                          
117022     WRITE UT3-POST              FROM UT3-AREA                            
117023     MOVE 'W47645C'              TO POSTSUM-FDNAMN                        
117024     MOVE 'W47645D3'             TO POSTSUM-DDNAMN2                       
117025     SKIP2                                                                
117026     CALL POSTSUM                USING  POSTSUM-PARM                      
117028     .                                                                    
117029     EJECT                                                                
117030 Z-FINIT SECTION.                                                         
117040     CLOSE W47645A                                                        
117100           W47645B                                                        
117110           W47645C                                                        
117200     SKIP2                                                                
117300     MOVE 'S' TO POSTSUM-OPKOD                                            
117400     CALL POSTSUM USING POSTSUM-PARM                                      
117500     .                                                                    
117600     EJECT                                                                
119510 S11-POSTSUM-UTPOST SECTION.                                              
119600                                                                          
120200     MOVE EDI-IDPTYP TO POSTSUM-TRANSTYP                                  
120300     MOVE 'W47645A' TO POSTSUM-FDNAMN                                     
120400     MOVE 'W47645D1' TO POSTSUM-DDNAMN2                                   
120500     CALL POSTSUM USING POSTSUM-PARM                                      
120600     .                                                                    
120700     EJECT                                                                
121800* --- IMS SEKTIONER ---                                                   
121900                                                                          
122000 IMS-GU-WDGX4545 SECTION.                                                 
122100                                                                          
122200     STRING 'WDR401  (WDGXKEY  =' W-WDGXKEY-4545-X ')'                    
122300          DELIMITED BY SIZE INTO SSA1                                     
122800     MOVE '  ' TO GODK-STATUSKODER                                        
122900     CALL CBLTDLI USING GU 4545-PCB DLI-IO-4546 SSA1                      
123100     MOVE 4545-STATUS-CODE TO STATUS-WS                                   
123200     PERFORM IMS-STATUSKONTROLL                                           
123300     .                                                                    
123400     SKIP3                                                                
124811 IMS-GNP-WDGX4546 SECTION.                                                
124812                                                                          
124813     MOVE 'WDGX4546 ' TO SSA1                                             
124814     MOVE '  GE' TO GODK-STATUSKODER                                      
124815     CALL CBLTDLI USING GNP 4545-PCB DLI-IO-4546 SSA1                     
124816     MOVE 4545-STATUS-CODE TO STATUS-WS                                   
124817     PERFORM IMS-STATUSKONTROLL                                           
124818     .                                                                    
124819     SKIP3                                                                
124820 IMS-GU-WDE611 SECTION.                                                   
124830                                                                          
124840     STRING 'WDE601  (IDPRODNR =' W-IDPRODNR-X ')'                        
124850          DELIMITED BY SIZE INTO SSA1                                     
124860     STRING 'WDE611  (IDKOLLI  =' W-IDKOLLI-X ')'                         
124870          DELIMITED BY SIZE INTO SSA2                                     
124880     MOVE '  ' TO GODK-STATUSKODER                                        
124890     CALL CBLTDLI USING GU WDE6-PCB DLI-IO-WDE611 SSA1 SSA2               
124900     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
125000     PERFORM IMS-STATUSKONTROLL                                           
125100     .                                                                    
125200     EJECT                                                                
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
