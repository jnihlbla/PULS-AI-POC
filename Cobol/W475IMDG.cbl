000100*COMPOPT STDSUB=YES                                                       
000300 ID DIVISION.                                                             
000400 PROGRAM-ID.     W475IMDG.                                                
000500 AUTHOR.         JAN-ERIK FRANTZEN.                                       
000600 DATE-WRITTEN.   MAJ 1994.                                                
000700                                                                          
000800*                                                                         
000900*    FUNKTION.                                                            
001000*        PROGRAMMET ÄR ETT SUBPROGRAM TILL W4054000 OCH                   
001100*        SKRIVER BLANKETT IMDG FÖR FARLIGT GODS MED HJÄLP                 
001200*        AV PRINTPROGRAM W006PRR1.                                        
001300*                                                                         
001400*        PROGRAMMET LÄSER      WLGMTA (WDB2)                              
001500*        PROGRAMMET LÄSER      WDB101                                     
001600*        PROGRAMMET LÄSER      WL1165 (WDR2 HTYP 1165/1168)               
001700*        PROGRAMMET LÄSER      WL4513 (WDR4)                              
001800*                                                                         
001900*    UTDATA.                                                              
002000*        BLANKETTER:  IMDG/IMDG-SF                                        
002100*                                                                         
002110*    IMPORTANT.                                                           
002120*    4633590 - SEND SEAL,UN NUM TO FLS/MIC                                
002130*    IF PSN IS ADDED TO GODK-PSN,IT IS IMPORTANT TO ADD IN                
002140*    GOOD-PSN-IMDG IN W4768000.                                           
002150*                                                                         
002200     SKIP3                                                                
002300 ENVIRONMENT DIVISION.                                                    
002400 DATA DIVISION.                                                           
002500     EJECT                                                                
002600 WORKING-STORAGE SECTION.                                                 
002700                                                                          
002800*    -- CHECKED BY WY2000                                                 
002900 77  IDPGM                       PIC X(08)   VALUE 'W475IMDG'.            
003000 77  CURRENT-SECTION             PIC X(16)   VALUE SPACE.                 
003100 77  CURRENT-IMS-SECTION         PIC X(16)   VALUE SPACE.                 
003200 77  WZ04-001-IDCOM              PIC S9(9)   COMP VALUE +0.               
003300 77  WS-ADRESS-DP                PIC X(50)                                
003400         VALUE 'CARPARTS.DAP.DISTRDOC'.                                   
003500 77  KDRC-DISPLAY                PIC Z(5).                                
003600 77  FELTEXT                     PIC X(80)   VALUE SPACE.                 
003700 77  JA                          PIC X       VALUE 'J'.                   
003800 77  NEJ                         PIC X       VALUE 'N'.                   
003900 77  IX                          PIC S9(9)   VALUE +0   COMP-3.           
004000 77  IX2                         PIC S9(9)   VALUE +0   COMP-3.           
004100 77  PSN-IX                      PIC S9(9)   VALUE +0   COMP-3.           
004200 77  TAB2-IX                     PIC S9(9)   VALUE +0   COMP-3.           
004300 77  TAB2-PSN-IX                 PIC S9(9)   VALUE +0   COMP-3.           
004400 77  TAB3-IX                     PIC S9(9)   VALUE +0   COMP-3.           
004500 77  TAB3-PSN-IX                 PIC S9(9)   VALUE +0   COMP-3.           
004600 77  KDKOLLI-IX                  PIC S9(9)   VALUE +0   COMP-3.           
004700 77  SPAR-IX                     PIC S9(9)   VALUE +0   COMP-3.           
004800 77  MAX-IX                      PIC S9(9)   VALUE +999 COMP-3.           
004900 77  RAD-IX                      PIC S9(3)   VALUE +0   COMP-3.           
005000 77  MAX-RAD-IX                  PIC S9(3)   VALUE +16  COMP-3.           
005100 77  WS-IDPSN-IX                 PIC S9(3)   VALUE +0   COMP-3.           
005200 77  WS-ANTAL-PSN                PIC S9(3)   VALUE +0   COMP-3.           
005300 77  WS-KDCLAGER-NUM             PIC 9(1)    VALUE ZERO.                  
005400 77  DAGENS-DATUM                PIC 9(6)    VALUE ZERO.                  
005500 77  PRT-EQUAL-31                PIC S9(3)   VALUE +831 COMP-3.           
005600 77  PRT-EQUAL-53                PIC S9(3)   VALUE +853 COMP-3.           
005700 77  PRT-EQUAL-57                PIC S9(3)   VALUE +857 COMP-3.           
005800 77  PRT-EQUAL-60                PIC S9(3)   VALUE +860 COMP-3.           
005900 77  PRT-EQUAL-61                PIC S9(3)   VALUE +861 COMP-3.           
006000 77  PRT-EQUAL-63                PIC S9(3)   VALUE +863 COMP-3.           
006100 77  WS-KDKOLLI-KOLLI            PIC X(7)    VALUE SPACE.                 
006200 77  WS-KDKOLLI-TAB              PIC X(7)    VALUE SPACE.                 
006300 77  RKOD-ABEND-MED-DUMP         PIC S9(4)   VALUE +33 COMP SYNC.         
006400 77  WS-IDSPRAK-SE               PIC X(2)    VALUE 'SE'.                  
006500 77  WS-IDSPRAK-GB               PIC X(2)    VALUE 'GB'.                  
006600 77  STRECK                      PIC X       VALUE '-'.                   
006700                                                                          
006800 77  DC-FG-SW                    PIC X       VALUE 'N'.                   
006900     88 DC-FG-OK                             VALUE 'J'.                   
007000                                                                          
007100 77  ADRESS-SW                   PIC X       VALUE 'N'.                   
007200     88 ADRESS-FINNS                         VALUE 'J'.                   
007300                                                                          
007400 01  WS-DATUM-AAMMDD             PIC 9(6).                                
007500 01  FILLER REDEFINES WS-DATUM-AAMMDD.                                    
007600     03  WS-AA                   PIC 9(2).                                
007700     03  WS-MM                   PIC 9(2).                                
007800     03  WS-DD                   PIC 9(2).                                
007900                                                                          
008000 01  WS-DATUM-AAMMDD-S           PIC X(10).                               
008100 01  FILLER REDEFINES WS-DATUM-AAMMDD-S.                                  
008200     03  WS-AA1-S                PIC 9(2).                                
008300     03  WS-AA-S                 PIC 9(2).                                
008400     03  WS-STRECK1              PIC X(1).                                
008500     03  WS-MM-S                 PIC 9(2).                                
008600     03  WS-STRECK2              PIC X(1).                                
008700     03  WS-DD-S                 PIC 9(2).                                
008800                                                                          
008900 01  WS-DATUM-DDMMAA             PIC 9(6).                                
009000 01  FILLER REDEFINES WS-DATUM-DDMMAA.                                    
009100     03  WS-MM-NA                PIC 9(2).                                
009200     03  WS-DD-NA                PIC 9(2).                                
009300     03  WS-AA-NA                PIC 9(2).                                
009400                                                                          
009500 01  WS-ORT-DATUM.                                                        
009600     03   IMDG-SIGN-ORT          PIC X(20) VALUE SPACE.                   
009700     03   IMDG-SIGN-DATUM        PIC X(6).                                
009800                                                                          
009900*      --- VALID IDDC CODES                                               
010000*                                                                         
010100*01    -COPY WWDC99                                                       
010200       EJECT                                                              
010300                                                                          
010400 01  WS-VLFG                     PIC 9(4)V9(3).                           
010500 01  FILLER REDEFINES WS-VLFG.                                            
010600     03  WS-VLFG-1-4             PIC 9(4).                                
010700     03  WS-VLFG-5               PIC 9.                                   
010800     03  WS-VLFG-6-7             PIC 99.                                  
010900                                                                          
011000 01  GENE0ELLA-SUBPROGRAM.                                                
011100     03  CBLTDLI                 PIC X(8)    VALUE 'CBLTDLI '.            
011200     03  W006PRR1                PIC X(8)    VALUE 'W006PRR1'.            
011300     03  FELLOG                  PIC X(8)    VALUE 'FELLOG  '.            
011400     03  WZ01SEND                PIC X(8)    VALUE 'WZ01SEND'.            
011500     03  WL10WBDC                PIC X(8)    VALUE 'WL10WBDC'.            
011600     03  ABEND                   PIC X(8)    VALUE 'ABEND   '.            
011700                                                                          
011800*    --- PARAMETERS FOR SUBPROGRAM WL10WBDC                               
011900                                                                          
012000 01  FILLER                      PIC X(16)  VALUE 'WL10WBDC AREA'.        
012100*01  -COPY WL10WBDC                                                       
012200                                                                          
012300                                                                          
012400     EJECT                                                                
012500 01  TEST-IDDISTR                PIC 9(5)   COMP-3.                       
012600*01  FILLER   -COPY WWDIST37    -RED TEST-IDDISTR.                        
012700     EJECT                                                                
012800*01  FILLER   -COPY WWDIST66    -RED TEST-IDDISTR.                        
012900     EJECT                                                                
013000*01  FILLER   -COPY WWDIST67    -RED TEST-IDDISTR.                        
013100     EJECT                                                                
013200 01  FILLER                      PIC X(08)   VALUE 'FG-KUND'.             
013300*    -COPY WWKUND15.                                                      
013400                                                                          
013500*    --- POSTBESKRIVNINGAR TILL D&P                                       
013600 01  DOC-HEAD-AREA.                                                       
013700*    03 -COPY W476FG1  -PRE HUVUD-                                        
013800 01  DOC-LINE-AREA.                                                       
013900*    03 -COPY W476FG2  -PRE RAD-                                          
014000 01  DOC-FOOT-AREA.                                                       
014100*    03 -COPY W476FG3  -PRE FOT-                                          
014200                                                                          
014300 01  FILLER                      PIC X(16)   VALUE 'SEND-CONTROL'.        
014400*01  -COPY WZ01SEND                                                       
014500     EJECT                                                                
014600 01  PRINTAREA-START             PIC X(24)   VALUE                        
014700                                 'PRINTAREA-START'.                       
014800 01  HDR-AREA.                                                            
014900*    03  -COPY WZ01REQU  -PRE HDR-                                        
015000*    03  -COPY WZ04HDR                                                    
015100                                                                          
015200                                                                          
015300*    --- ARBETS-AREOR TILL IMS-SEKTIONERNA                                
015400*                                                                         
015500 01  FILLER                      PIC X(16)   VALUE 'IMS-WS'.              
015600     SKIP3                                                                
015700                                                                          
015800 01  NYCKLAR-TILL-DLI.                                                    
015900*--- TILL WDR4 (WL4513).                                                  
016000                                                                          
016100     03  W-4513-X.                                                        
016200         05  W-IDHTYP            PIC X(4)    VALUE '4513'.                
016300         05  FILLER              PIC X(26)   VALUE LOW-VALUE.             
016400                                                                          
016500     03  W-DASKEPPN-X.                                                    
016600         05  W-DASKEPPN          PIC 9(8)    VALUE ZERO.                  
016700                                                                          
016800     03  W-4516-MIN-X.                                                    
016900         05  W-IDDC-4516-MIN     PIC X(2)    VALUE SPACE.                 
017000         05  W-IDSKEPPN-4516-MIN PIC S9(7)   VALUE ZERO COMP-3.           
017100         05  W-IDDISTR-4516-MIN  PIC S9(5)   VALUE ZERO COMP-3.           
017200         05  W-IDKUNDNR-4516-MIN PIC S9(7)   VALUE ZERO COMP-3.           
017300                                                                          
017400     03  W-4516-MAX-X.                                                    
017500         05  W-IDDC-4516-MAX     PIC X(2)    VALUE SPACE.                 
017600         05  W-IDSKEPPN-4516-MAX PIC S9(7)   VALUE ZERO COMP-3.           
017700         05  W-IDDISTR-4516-MAX  PIC S9(5)   VALUE ZERO COMP-3.           
017800         05  W-IDKUNDNR-4516-MAX PIC S9(7)   VALUE ZERO COMP-3.           
017900                                                                          
018000*--- Till WDB2 (WLGMTA).                                                  
018100                                                                          
018200     03  W-IDGMT-X.                                                       
018300         05  W-IDDISTR-WDB2      PIC S9(5)   VALUE ZERO COMP-3.           
018400         05  W-IDKUNDNR-WDB2     PIC S9(7)   VALUE ZERO COMP-3.           
018500                                                                          
018600*--- Till WDB1 (WDB101).                                                  
018700                                                                          
018800     03  W-WDB1-WDB101KY-X.                                               
018900         05  W-WDB1-IDPARTNR     PIC X(9)    VALUE SPACE.                 
019000         05  W-WDB1-IDFTG        PIC 9(2)    VALUE ZERO.                  
019100                                                                          
019200*--- Till WDR2 (WL1165) Htyp 1165/1168                                    
019300                                                                          
019400     03  W-WDGXKEY-X.                                                     
019500         05  W-IDHTYP            PIC X(4)    VALUE '1165'.                
019600         05  W-IDPSN             PIC 9(3).                                
019700         05  W-IDSPRAK           PIC X(2).                                
019800         05  W-LOW-VALUE         PIC X(21)   VALUE LOW-VALUE.             
019900                                                                          
020000     03  W-KDFGTRP-X.                                                     
020100         05  W-KDFGTRP           PIC 9(2)    VALUE 0.                     
020200                                                                          
020300*--- Till WDE6                                                            
020400                                                                          
020500     03    W-IDPRODNR-X.                                                  
020600         05    W-IDPRODNR        PIC S9(7)   VALUE ZERO  COMP-3.          
020700                                                                          
020800     03    W-IDKOLLI-X.                                                   
020900         05    W-IDKOLLI         PIC S9(5)   VALUE ZERO  COMP-3.          
021000                                                                          
021100                                                                          
021200*--- Till WDB6                                                            
021300                                                                          
021400     03  W-IDDC-B6-X.                                                     
021500         05 W-IDDC-B6                  PIC X(2).                          
021600     EJECT                                                                
021700*    --- PRINTPARAMETRAR (PRINTNING MED ÅTERSTART)                        
021800*01  FILLER   -COPY W006PRAR                                              
021900     EJECT                                                                
022000                                                                          
022100 01  PRT-AREA.                                                            
022200     03 WS-PRT-IDPRTLST          PIC X(8)  VALUE SPACE.                   
022300     03 WS-PRT-IDLIST.                                                    
022400        05 WS-IDLIST             PIC X(4)  VALUE 'IMDG'.                  
022500        05 WS-PRT-IDDISTR        PIC 9(4).                                
022600        05 WS-PRT-KDFRAKT        PIC 9(2).                                
022700     03 WS-WEB-IDLIST.                                                    
022800        05 WS-WEB-IDLIST         PIC X(3)  VALUE 'IMD'.                   
022900        05 WS-WEB-IDDISTR        PIC 9(4).                                
023000        05 WS-WEB-IDTRPTNR       PIC 9(3).                                
023100     03 WS-PRT-LISTRAD.                                                   
023200        05 FILLER                PIC X(2)  VALUE SPACE.                   
023300        05 WS-RAD                PIC X(78).                               
023400     03 WS-PRT-DUMMY             PIC X(1).                                
023500                                                                          
023600 01  FILLER           PIC X(16)   VALUE 'BLK-TAB1-OMIXAD'.                
023700*    --- BLANKETT-TABELL                                                  
023800 01  BLANKETT1-DATA-TABELL.                                               
023900    03   TABELL1-DATA OCCURS 1000.                                        
024000      05 TAB1-IDPSN                   PIC S9(3).                          
024100      05 TABELL1-IDPSN-DATA OCCURS 4.                                     
024200        07 TAB1-KDKOLLI               PIC X(8).                           
024300        07 TAB1-VLFG                  PIC S9(4)V9(3).                     
024400        07 TAB1-VKORDBTO              PIC S9(6)V9(1).                     
024500        07 TAB1-VKART-FG              PIC S9(7).                          
024600        07 TAB1-ANTAL-KOLLIN          PIC  9(3).                          
024700*                                                                         
024800 01  FILLER           PIC X(16)   VALUE 'BLK-TAB2-MIXAD '.                
024900 01  BLANKETT2-DATA-TABELL.                                               
025000    03   TABELL2-DATA OCCURS 100.                                         
025100      05 TAB2-KDKOLLI                 PIC X(8).                           
025200      05 TAB2-ANTAL-PSN               PIC 9(2).                           
025300      05 TAB2-VKORDBTO                PIC S9(6)V9(1).                     
025400      05 TAB2-VKART-FG                PIC S9(7).                          
025500      05 TAB2-ANTAL-KOLLIN            PIC  9(3).                          
025600      05 TAB2-VLFG                    PIC S9(4)V9(3).                     
025700      05 TABELL2-IDPSN OCCURS 9.                                          
025800        07 TAB2-IDPSN                 PIC S9(3).                          
025900*                                                                         
026000 01  FILLER           PIC X(16)   VALUE 'BLK-TAB3-RESTEN'.                
026100 01  BLANKETT3-DATA-TABELL.                                               
026200    03   TABELL3-DATA OCCURS 100.                                         
026300      05 TAB3-KDKOLLI                 PIC X(8).                           
026400      05 TAB3-VKORDBTO                PIC S9(6)V9(1).                     
026500      05 TAB3-VKART-FG                PIC S9(7).                          
026600      05 TAB3-ANTAL-KOLLIN            PIC  9(3).                          
026700      05 TAB3-VLFG                    PIC S9(4)V9(3).                     
026800      05 TABELL3-IDPSN OCCURS 9.                                          
026900        07 TAB3-IDPSN                 PIC S9(3).                          
027000*                                                                         
027100 01  FILLER                       PIC X(80)   VALUE ALL 'H'.              
027200                                                                          
027300 01  RAD00.                                                               
027400     03   FILLER                  PIC X(51) VALUE SPACE.                  
027500     03   FILLER                  PIC X(24) VALUE                         
027600                                  '*** BOKNINGSUNDERLAG ***'.             
027700 01  RAD0.                                                                
027800     03   FILLER                  PIC X(51) VALUE SPACE.                  
027900     03   IMDG-IDBOKN             PIC X(15).                              
028000                                                                          
028100 01  RAD1.                                                                
028200     03   FILLER                  PIC X(5) VALUE SPACE.                   
028300     03   IMDG-BEBETRAD-1         PIC X(35).                              
028400                                                                          
028500 01  RAD2.                                                                
028600     03   FILLER                  PIC X(5) VALUE SPACE.                   
028700     03   IMDG-BEBETRAD-2         PIC X(35).                              
028800                                                                          
028900 01  RAD3.                                                                
029000     03   FILLER                  PIC X(5) VALUE SPACE.                   
029100     03   IMDG-ADBETRAD-1         PIC X(35).                              
029200                                                                          
029300 01  RAD4.                                                                
029400     03   FILLER                  PIC X(5) VALUE SPACE.                   
029500     03   IMDG-ADBETRAD-2         PIC X(35).                              
029600                                                                          
029700 01  RAD5.                                                                
029800     03   FILLER                  PIC X(5) VALUE SPACE.                   
029900     03   IMDG-ADGMTLAND          PIC X(35).                              
030000                                                                          
030100 01  RAD6.                                                                
030200     03   FILLER                  PIC X(5)  VALUE SPACE.                  
030300     03   IMDG-TEXT-3             PIC X(22) VALUE SPACE.                  
030400     03   IMDG-VKART-FG           PIC Z(6)9.                              
030500     03   IMDG-TEXT-4             PIC X(6)  VALUE SPACE.                  
030600                                                                          
030700 01  RAD7.                                                                
030800     03   FILLER                  PIC X(5)  VALUE SPACE.                  
030900     03   IMDG-ANTAL-KOLLIN       PIC Z(3).                               
031000     03   FILLER                  PIC X(1)  VALUE SPACE.                  
031100     03   IMDG-TEXT-1             PIC X(26) VALUE SPACE.                  
031200     03   FILLER                  PIC X(3)  VALUE SPACE.                  
031300     03   IMDG-TEXT-2             PIC X(12) VALUE 'gross weight'.         
031400     03   FILLER                  PIC X(1)  VALUE SPACE.                  
031500     03   IMDG-VKORDBTO           PIC Z(5)9.9.                            
031600     03   FILLER                  PIC X(1)  VALUE SPACE.                  
031700     03   FILLER                  PIC X(2)  VALUE 'kg'.                   
031800                                                                          
031900 01  RAD8.                                                                
032000     03   FILLER                  PIC X(5)  VALUE SPACE.                  
032100     03   IMDG-BEPSN              PIC X(60).                              
032200                                                                          
032300 01  RAD9.                                                                
032400     03   FILLER                  PIC X(5)  VALUE SPACE.                  
032500     03   FILLER                  PIC X(25) VALUE                         
032600     '24-hour emergency contact'.                                         
032700     03   FILLER                  PIC X(38) VALUE                         
032800     ' telephone number: +46 31 94 61 10   '.                             
032900                                                                          
033000 01  RAD9A.                                                               
033100     03   FILLER                  PIC X(5)  VALUE SPACE.                  
033200     03   FILLER                  PIC X(38) VALUE                         
033300     'DGM Sweden Contract number 411       '.                             
033500                                                                          
033600 01  RAD10.                                                               
033700     03   FILLER                  PIC X(5)  VALUE SPACE.                  
033900     03   FILLER                  PIC X(28) VALUE SPACE.                  
034100     03   FILLER                  PIC X(38) VALUE SPACE.                  
034200                                                                          
034300 01  RAD11.                                                               
034400     03   FILLER                  PIC X(5)  VALUE SPACE.                  
034500     03   FILLER                  PIC X(28) VALUE SPACE.                  
034600     03   FILLER                  PIC X(41) VALUE SPACE.                  
034700                                                                          
034800 01  RAD12.                                                               
034900     03   FILLER                  PIC X(19) VALUE SPACE.                  
035000     03   IMDG-DATUM-1            PIC X(10).                              
035100                                                                          
035200 01  RAD12A.                                                              
035300     03   FILLER                  PIC X(19) VALUE SPACE.                  
035400     03   IMDG-DATUM-1A           PIC X(10).                              
035500     03   FILLER                  PIC X(22).                              
035600     03   IMDG-TRAILER            PIC X(7)  VALUE 'TRAILER'.              
035700                                                                          
035800 01  RAD13.                                                               
035900     03   FILLER                  PIC X(51) VALUE SPACE.                  
036000     03   IMDG-ORT                PIC X(11) VALUE 'Gothenburg '.          
036100     03   IMDG-DATUM-2            PIC X(10).                              
036200                                                                          
036300 EJECT                                                                    
036400***  OBS  ***  OM MAN LÄGGER TILL GODK-PSN-er                             
036500*              (10 TOM 99 OCH > 900)                                      
036600*              MÅSTE MAN KOLLA ATT DE FINNS UPPLAGDA PÅ                   
036700*              ALLA SPRÅKEN!                                              
036800*              KOLLA PÅ BILD 1132                                         
036900*              IDPSN + SPRÅK (DE, FR, ES, IT, PL, NL, FI)                 
037000***  OBS  ***  OBS  *******************************************           
037100 01  GODK-PSN-SW                  PIC 9(3).                               
037200     88  GODK-PSN                           VALUE 010                     
037300                                                  020 021 022 023         
037400                                                  024 025                 
037500                                                  030 031 032 033         
037600                                                  034 035 036 037         
037700                                                  038 039                 
037800                                                  040 041 042 043         
037900                                                  044 045 046 047         
038000                                                  049                     
038100                                                  050 051 052 053         
038200                                                  054 055 056 057         
038300                                                  058 059                 
038400                                                  060 061 062 063         
038500                                                  064 THRU 069            
038600                                                  070 THRU 075            
038700                                                  080 081 088 089         
038800                                                  090 091 092 093         
038900                                                  094 095 096 097         
039000                                                  907.                    
039100                                                                          
039200 01  GODK-KDKOLLI-SW              PIC X(8).                               
039300     88  FIBREBOARDBOX                      VALUE '1       '              
039400                                                  '27      '              
039500                                                  '0402    '              
039600                                                  '0403    '              
039700                                                  '0408    '              
039800                                                  '2207    '              
039900                                                  '2209    '              
040000                                                  '2231    '              
040100                                                  '2629    '              
040200                                                  '2631    '              
040300                                                  '2636    '              
040400                                                  '2639    '              
040500                                                  '3278    '              
040600                                                  '4454    '              
040700                                                  '4457    '              
040800                                                  '4460    '              
040900                                                  '4464    '              
041000                                                  '4506    '              
041100                                                  '4526    '              
041200                                                  '4548    '              
041300                                                  '5513    '              
041400                                                  '5543    '              
041500                                                  '5586    '              
041600                                                  '6111    '              
041700                                                  '6120    '              
041800                                                  '6255    '              
041900                                                  '6444    '              
042000                                                  '6451    '              
042100                                                  '7135    '              
042200                                                  '7136    '              
042300                                                  '7137    '              
042400                                                  '7138    '              
042500                                                  '7139    '              
042600                                                  '8789    '              
042700                                                  '9702    '              
042800                                                  '9703    '              
042900                                                  '9778    '              
043000                                                  'SP02    '              
043100                                                  'SP10    '.             
043200                                                                          
043300     88  PLYWOODBOX                         VALUE '1130    '              
043400                                                  '1131    '              
043500                                                  '1183    '              
043600                                                  '1635    '.             
043700                                                                          
043800     88  PLASTICDRUM                        VALUE 'PFAT    '.             
043900                                                                          
044000     88  STEELDRUM                          VALUE 'SFAT    '.             
044100                                                                          
044200     88  OVERPACK                           VALUE 'L1      '              
044300                                                  'L2      '              
044400                                                  'L3      '              
044500                                                  'L4      '              
044600                                                  'L5      '              
044700                                                  'L6      '              
044710                                                  'L34     '              
044720                                                  'L35     '              
044800                                                  'HL1     '              
044900                                                  'HL2     '              
045000                                                  'HL3     '              
045100                                                  'HL4     '              
045200                                                  'HL5     '              
045300                                                  'HL6     '              
045400                                                  'HL7     '              
045500                                                  'HL8     '              
045600                                                  '9057    '              
045700                                                  '9058    '              
045800                                                  '9066    '              
045900                                                  '9067    '              
046000                                                  '9068    '              
046100                                                  '9101    '              
046200                                                  '9102    '              
046300                                                  '9103    '              
046400                                                  '9104    '.             
046500                                                                          
046600 01  FORTSAETTNING-SW             PIC X.                                  
046700     88  FORTSAETTNING                      VALUE 'J'.                    
046800                                                                          
046900 01  RAKNA-UPP-KOLLI-SW           PIC X.                                  
047000     88  RAKNA-UPP-KOLLI                    VALUE 'J'.                    
047100                                                                          
047200 01  NY-SIDA-SW                   PIC X.                                  
047300     88  NY-SIDA-FINNS                      VALUE 'J'.                    
047400                                                                          
047500 01  TAB1-DATA-SW                 PIC X.                                  
047600     88  TAB1-DATA-FINNS                    VALUE 'J'.                    
047700                                                                          
047800 01  MIXAD-PSN-SW                 PIC X.                                  
047900     88  MIXAD-PSN                          VALUE 'J'.                    
048000     88  OMIXAD-PSN                         VALUE 'N'.                    
048100                                                                          
048200 01  PSN-MATCH-SW                 PIC X.                                  
048300     88  PSN-MATCH                          VALUE 'J'.                    
048400                                                                          
048500 01  KDKOLLI-SW                   PIC X.                                  
048600     88  KDKOLLI-OK                         VALUE 'J'.                    
048700                                                                          
048800 01  PSN-OK-SW                    PIC X.                                  
048900     88  PSN-OK                             VALUE 'J'.                    
049000                                                                          
049100     EJECT                                                                
049200*    --- STATUS-KOD FRÅN IMS                                              
049300 01  STATUS-WS                    PIC XX.                                 
049400     88  SEGMENT-FINNS                      VALUE '  '.                   
049500     88  SEGMENT-FINNS-REDAN                VALUE 'II'.                   
049600     88  SEGMENT-SAKNAS                     VALUE 'GE'.                   
049700     88  SEGMENT-SLUT                       VALUE 'GB'.                   
049800     SKIP2                                                                
049900 01  GODK-STATUSKODER.                                                    
050000     03  GODK-STATUS OCCURS 5 INDEXED BY STATUS-IX PIC XX.                
050100     SKIP3                                                                
050200                                                                          
050300 01  SSA1                        PIC X(150).                              
050400 01  SSA2                        PIC X(150).                              
050500                                                                          
050600     EJECT                                                                
050700*    --- IMS FUNKTIONSKODER                                               
050800*01  -COPY W0003                                                          
050900     EJECT                                                                
051000*    ---  DLI INPUT-OUTPUT AREA                                           
051100 01  FILLER                      PIC X(16)  VALUE 'DLI-IO-AREA'.          
051200     SKIP3                                                                
051300 01  DLI-IO-AREA.                                                         
051400     03  IO-AREA                 PIC X(600) VALUE SPACE.                  
051500     SKIP3                                                                
051600     03  WDE611 REDEFINES IO-AREA.                                        
051700*        05  -COPY WDE611                                                 
051800     SKIP3                                                                
051900     03  WL116512 REDEFINES IO-AREA.                                      
052000*        05  -COPY WDGX1168  -PRE 1165-                                   
052100     EJECT                                                                
052200     03  WDB101   REDEFINES IO-AREA.                                      
052300*        05  -COPY WDB101                                                 
052400     EJECT                                                                
052500 01  DLI-IO-AREA-WDB2.                                                    
052600     03  WLGMTA01.                                                        
052700*        05  -COPY WDB201                                                 
052800     EJECT                                                                
052900 01  FILLER                      PIC X(16)  VALUE 'DLI-IO-4514'.          
053000 01  DLI-IO-AREA-4514.                                                    
053100     03  WL451311.                                                        
053200*        05  -COPY WDGX4514                                               
053300     SKIP3                                                                
053400 01  FILLER                      PIC X(16)  VALUE 'DLI-IO-4516'.          
053500     SKIP3                                                                
053600 01  DLI-IO-AREA-4516.                                                    
053700     03  WL451321.                                                        
053800*        05  -COPY WDGX4516                                               
053900     SKIP3                                                                
054000 01  FILLER                      PIC X(16)  VALUE 'DLI-IO-B601'.          
054100 01  DLI-IO-AREA-B601.                                                    
054200     03  WDB601.                                                          
054300*        05  -COPY WDB601                                                 
054400     EJECT                                                                
054500 LINKAGE SECTION.                                                         
054600                                                                          
054700*                                                                         
054800*   -COPY W475IMDG -PRE LINK-                                             
054900*                                                                         
055000     EJECT                                                                
055100*01  -COPY W0009   -PRE ALT-                                              
055200     EJECT                                                                
055600*01  -COPY W0008  -PRE LISB-                                              
055700     05  FILLER                  PIC X.                                   
055800     EJECT                                                                
055900*01  -COPY W0008  -PRE 4513-                                              
056000     05  FILLER                  PIC X.                                   
056100     EJECT                                                                
056200*01  -COPY W0008  -PRE 1165-                                              
056300     05  FILLER                  PIC X.                                   
056400     EJECT                                                                
056500*01  -COPY W0008  -PRE GMTA-                                              
056600     05  FILLER                  PIC X.                                   
056700     EJECT                                                                
056800*01  -COPY W0008  -PRE WDB1-                                              
056900     05  FILLER                  PIC X.                                   
057000     EJECT                                                                
057100*01  -COPY W0008  -PRE WDE6-                                              
057200     05  FILLER                  PIC X.                                   
057300     EJECT                                                                
057400*01  -COPY W0008  -PRE WDB6-                                              
057500     05  FILLER                  PIC X.                                   
057600     EJECT                                                                
057700 PROCEDURE DIVISION  USING LINK-W475IMDG ALT-PCB                          
057800                                       LISB-PCB 4513-PCB                  
057900                                       1165-PCB GMTA-PCB WDB1-PCB         
058000                                       WDE6-PCB WDB6-PCB.                 
058100 MAIN SECTION.                                                            
058200     PERFORM A-INIT                                                       
058300     PERFORM B-LAES-DATA                                                  
058400     IF FORTSAETTNING                                                     
058500       IF WBDC-FLWEBDC = JA                                               
058600         PERFORM D-KOLLA-DC                                               
058700                                                                          
058800         IF DC-FG-OK                                                      
058900           PERFORM C-SKRIV-BLANKETT-WEB                                   
059000         END-IF                                                           
059100                                                                          
059200       ELSE                                                               
059300         PERFORM G-SKRIV-BLANKETT                                         
059400       END-IF                                                             
059500     END-IF                                                               
059600                                                                          
059700     MOVE ZERO                     TO RETURN-CODE                         
059800     GOBACK                                                               
059900     .                                                                    
060000     EJECT                                                                
060100 A-INIT SECTION.                                                          
060200     MOVE 'A-INIT          ' TO CURRENT-SECTION                           
060300                                                                          
060400     MOVE LOW-VALUE                TO W-4516-MIN-X                        
060500     MOVE HIGH-VALUE               TO W-4516-MAX-X                        
060600                                                                          
060700     MOVE LINK-IDDISTR             TO W-IDDISTR-4516-MIN                  
060800                                      W-IDDISTR-4516-MAX                  
060900                                      W-IDDISTR-WDB2                      
061000                                      TEST-IDDISTR                        
061100                                                                          
061200     MOVE LINK-IDKUNDNR            TO W-IDKUNDNR-WDB2                     
061300                                                                          
061400     MOVE LINK-IDDC                TO W-IDDC-4516-MIN                     
061500                                      W-IDDC-4516-MAX                     
061600                                      WS-IDDC                             
061700                                      W-IDDC-B6                           
061800                                                                          
061900     MOVE LINK-IDSKEPPN            TO W-IDSKEPPN-4516-MIN                 
062000                                      W-IDSKEPPN-4516-MAX                 
062100                                                                          
062200     MOVE LINK-DASKEPPN            TO W-DASKEPPN                          
062300                                                                          
062400     ACCEPT DAGENS-DATUM FROM DATE                                        
062500     MOVE DAGENS-DATUM             TO WS-DATUM-AAMMDD                     
062600                                      IMDG-SIGN-DATUM                     
062700     MOVE WS-AA                    TO WS-AA-S                             
062800     MOVE WS-MM                    TO WS-MM-S                             
062900     MOVE WS-DD                    TO WS-DD-S                             
063000     MOVE 20                       TO WS-AA1-S                            
063100     MOVE STRECK                   TO WS-STRECK1                          
063200                                      WS-STRECK2                          
063300                                                                          
063400     MOVE WS-DATUM-AAMMDD-S        TO IMDG-DATUM-1                        
063500                                      IMDG-DATUM-1A                       
063600                                      IMDG-DATUM-2                        
063700                                                                          
063800                                                                          
063900     MOVE +0                       TO WS-VLFG                             
064000     MOVE NEJ                      TO TAB1-DATA-SW                        
064100     PERFORM AA-NOLLA-TAB1                                                
064200     PERFORM AB-NOLLA-TAB2                                                
064300     PERFORM AC-NOLLA-TAB3                                                
064400     PERFORM AD-KOLLA-WEB-DC                                              
064500                                                                          
064600     IF NDC-NA                                                            
064700       MOVE WS-AA                  TO WS-AA-NA                            
064800       MOVE WS-MM                  TO WS-MM-NA                            
064900       MOVE WS-DD                  TO WS-DD-NA                            
065000       MOVE WS-DATUM-DDMMAA        TO IMDG-SIGN-DATUM                     
065100     END-IF                                                               
065200                                                                          
065300     .                                                                    
065400     EJECT                                                                
065500 AA-NOLLA-TAB1 SECTION.                                                   
065600     MOVE 'AA-NOLLA-TAB1   ' TO CURRENT-SECTION                           
065700                                                                          
065800     MOVE +1                       TO IX                                  
065900     PERFORM UNTIL IX > MAX-IX                                            
066000        MOVE ZERO                  TO TAB1-IDPSN (IX)                     
066100        MOVE +1                    TO IX2                                 
066200        PERFORM UNTIL IX2 > 4                                             
066300          MOVE SPACE               TO TAB1-KDKOLLI (IX, IX2)              
066400          MOVE ZERO                TO TAB1-VLFG (IX, IX2)                 
066500          MOVE ZERO                TO TAB1-VKORDBTO (IX, IX2)             
066600          MOVE ZERO                TO TAB1-VKART-FG (IX, IX2)             
066700          MOVE ZERO                TO TAB1-ANTAL-KOLLIN (IX, IX2)         
066800          ADD +1                   TO IX2                                 
066900        END-PERFORM                                                       
067000                                                                          
067100        ADD +1                     TO IX                                  
067200     END-PERFORM                                                          
067300     .                                                                    
067400     EJECT                                                                
067500 AB-NOLLA-TAB2 SECTION.                                                   
067600     MOVE 'AB-NOLLA-TAB2   ' TO CURRENT-SECTION                           
067700                                                                          
067800     MOVE +1                       TO IX                                  
067900     PERFORM UNTIL IX > 100                                               
068000        MOVE SPACE                 TO TAB2-KDKOLLI (IX)                   
068100        MOVE ZERO                  TO TAB2-VLFG (IX)                      
068200        MOVE ZERO                  TO TAB2-VKORDBTO (IX)                  
068300        MOVE ZERO                  TO TAB2-VKART-FG (IX)                  
068400        MOVE ZERO                  TO TAB2-ANTAL-KOLLIN (IX)              
068500        MOVE ZERO                  TO TAB2-ANTAL-PSN (IX)                 
068600                                                                          
068700        MOVE +1                    TO IX2                                 
068800        PERFORM UNTIL IX2 > 9                                             
068900          MOVE ZERO                TO TAB2-IDPSN (IX, IX2)                
069000          ADD +1                   TO IX2                                 
069100        END-PERFORM                                                       
069200                                                                          
069300        ADD +1                     TO IX                                  
069400     END-PERFORM                                                          
069500     .                                                                    
069600     EJECT                                                                
069700 AC-NOLLA-TAB3 SECTION.                                                   
069800     MOVE 'AC-NOLLA-TAB3   ' TO CURRENT-SECTION                           
069900                                                                          
070000     MOVE +1                       TO IX                                  
070100     PERFORM UNTIL IX > 100                                               
070200        MOVE SPACE                 TO TAB3-KDKOLLI (IX)                   
070300        MOVE ZERO                  TO TAB3-VLFG (IX)                      
070400        MOVE ZERO                  TO TAB3-VKORDBTO (IX)                  
070500        MOVE ZERO                  TO TAB3-VKART-FG (IX)                  
070600        MOVE ZERO                  TO TAB3-ANTAL-KOLLIN (IX)              
070700                                                                          
070800        MOVE +1                    TO IX2                                 
070900        PERFORM UNTIL IX2 > 9                                             
071000          MOVE ZERO                TO TAB3-IDPSN (IX, IX2)                
071100          ADD +1                   TO IX2                                 
071200        END-PERFORM                                                       
071300                                                                          
071400        ADD +1                     TO IX                                  
071500     END-PERFORM                                                          
071600     .                                                                    
071700     EJECT                                                                
071800 AD-KOLLA-WEB-DC SECTION.                                                 
071900     MOVE 'AD-KOLLA-WEB-DC ' TO CURRENT-SECTION                           
072000                                                                          
072100     MOVE LINK-IDDC       TO WBDC-IDDC                                    
072200     CALL WL10WBDC USING WBDC-AREA                                        
072300     .                                                                    
072400                                                                          
072500 B-LAES-DATA SECTION.                                                     
072600     MOVE 'B-LAES-DATA     ' TO CURRENT-SECTION                           
072700                                                                          
072800     PERFORM BA-LAES-SKEPP-KOLLI                                          
072900                                                                          
073000     IF FORTSAETTNING                                                     
073100        PERFORM BB-LAES-KUNDREG                                           
073200     END-IF                                                               
073300     .                                                                    
073400     EJECT                                                                
073500 BA-LAES-SKEPP-KOLLI SECTION.                                             
073600     MOVE 'BA-LAES-SKEPP-KOLLI' TO CURRENT-SECTION                        
073700                                                                          
073800     MOVE NEJ                TO FORTSAETTNING-SW                          
073900                                                                          
074000     PERFORM IMS-GU-WL451311                                              
074100                                                                          
074200     IF SEGMENT-FINNS                                                     
074300       MOVE SPACE            TO IMDG-IDBOKN                               
074400       PERFORM UNTIL SEGMENT-SAKNAS OR SEGMENT-SLUT                       
074500          PERFORM IMS-GNP-WL451321                                        
074600                                                                          
074700          IF SEGMENT-FINNS                                                
074800             MOVE 4516-IDPRODNR    TO W-IDPRODNR                          
074900             MOVE 4516-IDKOLLI     TO W-IDKOLLI                           
075000             PERFORM IMS-GET-WDE6                                         
075100                                                                          
075200             PERFORM BAA-KOLLA-KDKOLLI-GODK                               
075300                                                                          
075400             IF FIBREBOARDBOX OR PLYWOODBOX OR                            
075500                PLASTICDRUM OR STEELDRUM                                  
075600               PERFORM BAB-KOLLA-OM-MIXADE-PSN                            
075700                                                                          
075800               IF OMIXAD-PSN                                              
075900                 PERFORM S02-FLYTTA-TILL-OMIXAD-TAB                       
076000               ELSE                                                       
076100                 PERFORM S03-FLYTTA-TILL-MIXAD-TAB                        
076200               END-IF                                                     
076300                                                                          
076400             ELSE                                                         
076500               PERFORM S04-FLYTTA-TILL-EJ-GODK-TAB                        
076600             END-IF                                                       
076700                                                                          
076800          END-IF                                                          
076900       END-PERFORM                                                        
077000     END-IF                                                               
077100     .                                                                    
077200     EJECT                                                                
077300 BAA-KOLLA-KDKOLLI-GODK SECTION.                                          
077400     MOVE 'BAA-KOLLA-KDKOLLI-GODK' TO CURRENT-SECTION                     
077500                                                                          
077600     MOVE KOLLI-KDKOLLI TO GODK-KDKOLLI-SW                                
077700                                                                          
077800     IF FIBREBOARDBOX OR PLYWOODBOX OR                                    
077900        PLASTICDRUM   OR STEELDRUM  OR                                    
078000        OVERPACK                                                          
078100       CONTINUE                                                           
078200                                                                          
078300     ELSE                                                                 
078400       MOVE '1       ' TO KOLLI-KDKOLLI                                   
078500                                                                          
078600     END-IF                                                               
078700                                                                          
078800     IF KOLLI-IDPSN(1) = 53                                               
078900       MOVE 'SFAT    ' TO KOLLI-KDKOLLI                                   
079000                                                                          
079100     ELSE                                                                 
079200       IF KOLLI-IDPSN(1) = 60                                             
079300         MOVE 'PFAT    ' TO KOLLI-KDKOLLI                                 
079400       END-IF                                                             
079500                                                                          
079600     END-IF                                                               
079700                                                                          
079800     MOVE KOLLI-KDKOLLI TO GODK-KDKOLLI-SW                                
079900     .                                                                    
080000     EJECT                                                                
080100 BAB-KOLLA-OM-MIXADE-PSN SECTION.                                         
080200     MOVE 'BAB-KOLLA-OM-MIXADE-PSN' TO CURRENT-SECTION                    
080300                                                                          
080400     MOVE +1 TO IX                                                        
080500     MOVE +0 TO WS-ANTAL-PSN                                              
080600     MOVE NEJ TO MIXAD-PSN-SW                                             
080700                                                                          
080800     PERFORM UNTIL IX > 9                                                 
080900                                                                          
081000       IF KOLLI-IDPSN (IX) > ZERO                                         
081100         MOVE KOLLI-IDPSN (IX) TO GODK-PSN-SW                             
081200                                                                          
081300         IF GODK-PSN                                                      
081400           ADD +1 TO WS-ANTAL-PSN                                         
081500         END-IF                                                           
081600       END-IF                                                             
081700                                                                          
081800       ADD +1 TO IX                                                       
081900     END-PERFORM                                                          
082000                                                                          
082100     IF WS-ANTAL-PSN > 1                                                  
082200       MOVE JA TO MIXAD-PSN-SW                                            
082300     END-IF                                                               
082400     .                                                                    
082500 BB-LAES-KUNDREG SECTION.                                                 
082600     MOVE 'BB-LAES-KUNDREG        ' TO CURRENT-SECTION                    
082700                                                                          
082800     PERFORM IMS-GET-GMTA01                                               
082900                                                                          
083000     MOVE LINK-IDDISTR              TO TEST-IDDISTR                       
083100     MOVE LINK-IDKUNDNR             TO KUND15-IDKUNDNR                    
083800                                                                          
083900     IF GMT-FLSAMFAK = 'J'                                                
084000                                                                          
084100*      DISTR. 778 OCH KUND 433 -> VISBY BEHÖVER HA KUNDENS                
084200*      ADRESS PÅ DOK. (EJ BETALARENS)                                     
084400       IF DIST67-FG-SE                                                    
084600                                                                          
084700         MOVE GMT-BEGMT-RAD1         TO IMDG-BEBETRAD-1                   
084800         MOVE GMT-BEGMT-RAD2         TO IMDG-BEBETRAD-2                   
084900         MOVE GMT-ADGMT-GATA         TO IMDG-ADBETRAD-1                   
085000         MOVE GMT-ADGMT-PADR         TO IMDG-ADBETRAD-2                   
085100         MOVE GMT-ADGMT-LAND         TO IMDG-ADGMTLAND                    
085200                                                                          
085300       ELSE                                                               
085400         MOVE GMT-IDPARTNR           TO W-WDB1-IDPARTNR                   
085500         MOVE GMT-IDFTG              TO W-WDB1-IDFTG                      
085600         PERFORM IMS-GET-WDB101                                           
085700                                                                          
085800         MOVE BET-BEBETRAD-1         TO IMDG-BEBETRAD-1                   
085900         MOVE BET-BEBETRAD-2         TO IMDG-BEBETRAD-2                   
086000         MOVE BET-ADBETRAD-1         TO IMDG-ADBETRAD-1                   
086100         MOVE BET-ADBETRAD-2         TO IMDG-ADBETRAD-2                   
086200         MOVE SPACE                  TO IMDG-ADGMTLAND                    
086300       END-IF                                                             
086400     ELSE                                                                 
086500       MOVE GMT-BEGMT-RAD1           TO IMDG-BEBETRAD-1                   
086600       MOVE GMT-BEGMT-RAD2           TO IMDG-BEBETRAD-2                   
086700       MOVE GMT-ADGMT-GATA           TO IMDG-ADBETRAD-1                   
086800       MOVE GMT-ADGMT-PADR           TO IMDG-ADBETRAD-2                   
086900       MOVE GMT-ADGMT-LAND           TO IMDG-ADGMTLAND                    
087000     END-IF                                                               
087100     .                                                                    
087200     EJECT                                                                
087300 C-SKRIV-BLANKETT-WEB SECTION.                                            
087400                                                                          
087500     MOVE 'C-SKRIV-BLANKETT-WEB   ' TO CURRENT-SECTION                    
087600                                                                          
087700                                                                          
087800     IF TAB1-DATA-FINNS                                                   
087900        PERFORM CA-SKRIV-HUVUD-WEB                                        
088000        PERFORM CB-REDIGERA-BLK-OMIXAD                                    
088100        PERFORM CE-SKRIV-AVSLUTNING                                       
088200                                                                          
088300     END-IF                                                               
088400                                                                          
088500                                                                          
088600     IF TAB2-KDKOLLI(1) > SPACE                                           
088700        PERFORM CA-SKRIV-HUVUD-WEB                                        
088800        PERFORM CC-REDIGERA-BLK-MIXAD                                     
088900        PERFORM CE-SKRIV-AVSLUTNING                                       
089000                                                                          
089100     END-IF                                                               
089200                                                                          
089300                                                                          
089400                                                                          
089500     IF  TAB3-ANTAL-KOLLIN(1) > ZERO                                      
089600     AND TAB3-IDPSN(1, 1) > ZERO                                          
089700        MOVE +1                    TO IX                                  
089800        PERFORM CA-SKRIV-HUVUD-WEB                                        
089900        PERFORM CD-REDIGERA-BLK-EJ-GODK                                   
090000        PERFORM CE-SKRIV-AVSLUTNING                                       
090100                                                                          
090200     END-IF                                                               
090300                                                                          
090400     MOVE WZ04-001-IDCOM          TO SEND-IDCOM                           
090500     PERFORM S95-SEND-CLOSE                                               
090600     .                                                                    
090700     EJECT                                                                
090800                                                                          
090900 CA-SKRIV-HUVUD-WEB     SECTION.                                          
091000                                                                          
091100     MOVE 'CA-SKRIV-HUVUD  ' TO CURRENT-SECTION                           
091200                                                                          
091300     MOVE 1                 TO HDR-REQU-IDMSGVER                          
091400     MOVE 'R'               TO HDR-REQU-KDPGMACT                          
091500     MOVE SPACE             TO HDR-REQU-IDUSER                            
091600                                                                          
091700     MOVE SPACE             TO HDR-IDOUTREC                               
091800                               HDR-IDLIST                                 
091900     MOVE 'DANGEROUS-GOODS' TO HDR-IDOUTTYPE                              
092000     MOVE LINK-IDDC         TO HDR-IDOUTREC(1:2)                          
092100     MOVE SPACE             TO HDR-IDOUTREC(3:8)                          
092200     MOVE 'IMD'             TO HDR-IDLIST(1:3)                            
092300     MOVE LINK-IDDISTR      TO WS-WEB-IDDISTR                             
092400     MOVE WS-WEB-IDDISTR    TO HDR-IDLIST(4:4)                            
092500     MOVE LINK-IDTRPTNR     TO WS-WEB-IDTRPTNR                            
092600     MOVE WS-WEB-IDTRPTNR   TO HDR-IDLIST(8:3)                            
092700     PERFORM S90-SEND-OPEN                                                
092800                                                                          
092900     MOVE SEND-IDCOM       TO WZ04-001-IDCOM                              
093000     PERFORM S91-PUT-DOC-HDR                                              
093100                                                                          
093200                                                                          
093300     MOVE '1'              TO HUVUD-IDAFPRCD                              
093400     MOVE SPACE            TO HUVUD-IDFRASED                              
093500     MOVE ZERO             TO HUVUD-KVPAGE                                
093600     MOVE ZERO             TO HUVUD-IDORDNR7                              
093700     MOVE IMDG-BEBETRAD-1  TO HUVUD-BEGMT-RAD1                            
093800     MOVE IMDG-BEBETRAD-2  TO HUVUD-BEGMT-RAD2                            
093900     MOVE IMDG-ADBETRAD-1  TO HUVUD-ADGMT-GATA                            
094000     MOVE IMDG-ADBETRAD-2  TO HUVUD-ADGMT-PADR                            
094100     MOVE IMDG-ADGMTLAND   TO HUVUD-ADGMT-LAND                            
094200                                                                          
094300     MOVE LINK-IDDISTR             TO TEST-IDDISTR                        
094400     IF DIST37-TRAILER-DISTR                                              
094500       MOVE 'TRAILER'              TO HUVUD-AIRPORT                       
094600     ELSE                                                                 
094700       MOVE SPACE                  TO HUVUD-AIRPORT                       
094800     END-IF                                                               
094900                                                                          
095000                                                                          
095100     IF ADRESS-FINNS                                                      
095200       MOVE DCS-ADGMT-PADR(11:20)  TO IMDG-SIGN-ORT                       
095300       MOVE DCS-BEGMT-RAD1         TO HUVUD-DCS-BEGMT-RAD1                
095400       MOVE DCS-BEGMT-RAD2         TO HUVUD-DCS-BEGMT-RAD2                
095500     ELSE                                                                 
095600       MOVE SPACE                  TO HUVUD-DCS-BEGMT-RAD1                
095700                                      HUVUD-DCS-BEGMT-RAD2                
095800     END-IF                                                               
095900     IF NDC-NA                                                            
096000       IF NDC-US-RU                                                       
096100         MOVE 'RUTHERFORD, NJ      ' TO IMDG-SIGN-ORT                     
096200       END-IF                                                             
096300                                                                          
096400       IF NDC-US-LA                                                       
096500         MOVE 'CARSON, CA          ' TO IMDG-SIGN-ORT                     
096600       END-IF                                                             
096700                                                                          
096800       IF NDC-CA                                                          
096900         MOVE 'MISSISAUGA, ONTARIO ' TO IMDG-SIGN-ORT                     
097000       END-IF                                                             
097100                                                                          
097200       IF NDC-JP                                                          
097300         MOVE 'NAGOYA              ' TO IMDG-SIGN-ORT                     
097400       END-IF                                                             
097500                                                                          
097600       IF NDC-AU                                                          
097700         MOVE 'MINTO               ' TO IMDG-SIGN-ORT                     
097800       END-IF                                                             
097900                                                                          
098000     END-IF                                                               
098100                                                                          
098200     MOVE IMDG-SIGN-ORT              TO HUVUD-SIGN-ORT                    
098300     MOVE IMDG-SIGN-DATUM            TO HUVUD-SIGN-DATUM                  
098400                                                                          
098500                                                                          
098600     PERFORM S92-PUT-DOC-HEAD                                             
098700     .                                                                    
098800 CB-REDIGERA-BLK-OMIXAD SECTION.                                          
098900     MOVE 'CB-REDIGERA-BLK-OMIXAD ' TO CURRENT-SECTION                    
099000                                                                          
099100     MOVE '2'   TO RAD-IDAFPRCD                                           
099200                                                                          
099300     MOVE +1                       TO IX                                  
099400     MOVE +1                       TO IX2                                 
099500                                                                          
099600     PERFORM UNTIL IX > MAX-IX                                            
099700        IF TAB1-IDPSN (IX) > 0                                            
099800           MOVE TAB1-IDPSN (IX)    TO GODK-PSN-SW                         
099900           IF GODK-PSN                                                    
100000                                                                          
100100             PERFORM UNTIL IX2 > 4                                        
100200               IF TAB1-KDKOLLI (IX, IX2) > SPACE                          
100300                 PERFORM CBA-SKRIV-KOLLI-RADER                            
100400               END-IF                                                     
100500               ADD +1 TO IX2                                              
100600             END-PERFORM                                                  
100700           END-IF                                                         
100800        END-IF                                                            
100900        ADD  +1                    TO IX                                  
101000        MOVE +1                    TO IX2                                 
101100     END-PERFORM                                                          
101200                                                                          
101300     MOVE SPACE            TO RAD-FG-RAD                                  
101400     PERFORM S93-PUT-DOC-LINE                                             
101500     .                                                                    
101600     EJECT                                                                
101700 CBA-SKRIV-KOLLI-RADER SECTION.                                           
101800                                                                          
101900     MOVE 'CBA-SKRIV-KOLLI-RADER  ' TO CURRENT-SECTION                    
102000                                                                          
102100*--- KOLLI RADER                                                          
102200                                                                          
102300     MOVE SPACE                    TO WS-RAD                              
102400                                      RAD8                                
102500     MOVE TAB1-IDPSN (IX)          TO W-IDPSN                             
102600     MOVE WS-IDSPRAK-GB            TO W-IDSPRAK                           
102700                                                                          
102800     IF LINK-IDPTYP = '   '                                               
102900        MOVE 2                     TO  W-KDFGTRP                          
103000     ELSE                                                                 
103100        IF W-IDPSN = 32 OR 33 OR 34                                       
103200           MOVE 3                  TO  W-KDFGTRP                          
103300        ELSE                                                              
103400           MOVE 2                  TO  W-KDFGTRP                          
103500        END-IF                                                            
103600     END-IF                                                               
103700                                                                          
103800     PERFORM IMS-GET-1165-WDR2                                            
103900                                                                          
104000     IF 1165-1168-BEPSN (1) NOT = SPACE                                   
104100        MOVE SPACE                 TO RAD8                                
104200        MOVE 1165-1168-BEPSN (1)   TO IMDG-BEPSN                          
104300                                                                          
104400        MOVE RAD8                  TO RAD-FG-RAD                          
104500        PERFORM S93-PUT-DOC-LINE                                          
104600                                                                          
104700     END-IF                                                               
104800                                                                          
104900     IF 1165-1168-BEPSN (2) NOT = SPACE                                   
105000        MOVE SPACE                 TO RAD8                                
105100        MOVE 1165-1168-BEPSN (2)   TO IMDG-BEPSN                          
105200                                                                          
105300        MOVE RAD8                  TO RAD-FG-RAD                          
105400        PERFORM S93-PUT-DOC-LINE                                          
105500                                                                          
105600     END-IF                                                               
105700                                                                          
105800     IF 1165-1168-BEPSN (3) NOT = SPACE                                   
105900        MOVE SPACE                 TO RAD8                                
106000        MOVE 1165-1168-BEPSN(3)    TO IMDG-BEPSN                          
106100                                                                          
106200        MOVE RAD8                  TO RAD-FG-RAD                          
106300        PERFORM S93-PUT-DOC-LINE                                          
106400                                                                          
106500     END-IF                                                               
106600                                                                          
106700     MOVE TAB1-ANTAL-KOLLIN (IX, IX2)      TO IMDG-ANTAL-KOLLIN           
106800                                                                          
106900     MOVE TAB1-KDKOLLI (IX, IX2)           TO GODK-KDKOLLI-SW             
107000     IF FIBREBOARDBOX                                                     
107100       IF TAB1-ANTAL-KOLLIN (IX, IX2) > 1                                 
107200         MOVE 'Fibreboard Boxes          ' TO IMDG-TEXT-1                 
107300       ELSE                                                               
107400         MOVE 'Fibreboard Box            ' TO IMDG-TEXT-1                 
107500       END-IF                                                             
107600     END-IF                                                               
107700                                                                          
107800     IF PLYWOODBOX                                                        
107900       IF TAB1-ANTAL-KOLLIN (IX, IX2) > 1                                 
108000         MOVE 'Plywood Boxes             ' TO IMDG-TEXT-1                 
108100       ELSE                                                               
108200         MOVE 'Plywood Box               ' TO IMDG-TEXT-1                 
108300       END-IF                                                             
108400     END-IF                                                               
108500                                                                          
108600     IF PLASTICDRUM                                                       
108700       IF TAB1-ANTAL-KOLLIN (IX, IX2) > 1                                 
108800         MOVE 'Plastic Drums             ' TO IMDG-TEXT-1                 
108900       ELSE                                                               
109000         MOVE 'Plastic Drum              ' TO IMDG-TEXT-1                 
109100       END-IF                                                             
109200     END-IF                                                               
109300                                                                          
109400     IF STEELDRUM                                                         
109500       IF TAB1-ANTAL-KOLLIN (IX, IX2) > 1                                 
109600         MOVE 'Steel Drums               ' TO IMDG-TEXT-1                 
109700       ELSE                                                               
109800         MOVE 'Steel Drums               ' TO IMDG-TEXT-1                 
109900       END-IF                                                             
110000     END-IF                                                               
110100                                                                          
110200     MOVE TAB1-VKORDBTO (IX, IX2)  TO IMDG-VKORDBTO                       
110300                                                                          
110400     MOVE RAD7                     TO RAD-FG-RAD                          
110500     PERFORM S93-PUT-DOC-LINE                                             
110600                                                                          
110700     MOVE SPACE                    TO RAD-FG-RAD                          
110800     PERFORM S93-PUT-DOC-LINE                                             
110900                                                                          
111000     .                                                                    
111100     EJECT                                                                
111200 CC-REDIGERA-BLK-MIXAD SECTION.                                           
111300                                                                          
111400     MOVE 'CC-REDIGERA-BLK-MIXAD      ' TO CURRENT-SECTION                
111500                                                                          
111600     MOVE '2'   TO RAD-IDAFPRCD                                           
111700                                                                          
111800     MOVE +1                       TO IX                                  
111900     MOVE +1                       TO IX2                                 
112000*                                                                         
112100     PERFORM UNTIL IX > 99                                                
112200        IF TAB2-KDKOLLI (IX) > SPACE                                      
112300           PERFORM UNTIL IX2 > 9                                          
112400             IF TAB2-IDPSN (IX, IX2 ) > 0                                 
112500               MOVE TAB2-IDPSN (IX, IX2) TO GODK-PSN-SW                   
112600               IF GODK-PSN                                                
112700                                                                          
112800                  PERFORM CCA-SKRIV-PSN-RADER                             
112900               END-IF                                                     
113000             END-IF                                                       
113100             ADD +1 TO IX2                                                
113200           END-PERFORM                                                    
113300           PERFORM CCB-SKRIV-KOLLI-RADER                                  
113400        END-IF                                                            
113500        ADD  +1                    TO IX                                  
113600        MOVE +1                    TO IX2                                 
113700     END-PERFORM                                                          
113800                                                                          
113900     .                                                                    
114000     EJECT                                                                
114100 CCA-SKRIV-PSN-RADER SECTION.                                             
114200                                                                          
114300     MOVE 'CCA-SKRIV-PSN-RADER        ' TO CURRENT-SECTION                
114400                                                                          
114500     MOVE SPACE                    TO WS-RAD                              
114600                                      RAD8                                
114700     MOVE TAB2-IDPSN (IX, IX2)     TO W-IDPSN                             
114800     MOVE WS-IDSPRAK-GB            TO W-IDSPRAK                           
114900                                                                          
115000     IF LINK-IDPTYP = '   '                                               
115100        MOVE 2                     TO  W-KDFGTRP                          
115200     ELSE                                                                 
115300        IF W-IDPSN = 32 OR 33 OR 34                                       
115400           MOVE 3                  TO  W-KDFGTRP                          
115500        ELSE                                                              
115600           MOVE 2                  TO  W-KDFGTRP                          
115700        END-IF                                                            
115800     END-IF                                                               
115900                                                                          
116000     PERFORM IMS-GET-1165-WDR2                                            
116100                                                                          
116200     IF 1165-1168-BEPSN (1) NOT = SPACE                                   
116300        MOVE SPACE                 TO RAD8                                
116400        MOVE 1165-1168-BEPSN (1)   TO IMDG-BEPSN                          
116500                                                                          
116600        MOVE RAD8                  TO RAD-FG-RAD                          
116700        PERFORM S93-PUT-DOC-LINE                                          
116800                                                                          
116900     END-IF                                                               
117000                                                                          
117100     IF 1165-1168-BEPSN (2) NOT = SPACE                                   
117200        MOVE SPACE                 TO RAD8                                
117300        MOVE 1165-1168-BEPSN(2)    TO IMDG-BEPSN                          
117400                                                                          
117500        MOVE RAD8                  TO RAD-FG-RAD                          
117600        PERFORM S93-PUT-DOC-LINE                                          
117700     END-IF                                                               
117800                                                                          
117900     IF 1165-1168-BEPSN (3) NOT = SPACE                                   
118000        MOVE SPACE                 TO RAD8                                
118100        MOVE 1165-1168-BEPSN(3)    TO IMDG-BEPSN                          
118200                                                                          
118300        MOVE RAD8                  TO RAD-FG-RAD                          
118400        PERFORM S93-PUT-DOC-LINE                                          
118500                                                                          
118600     END-IF                                                               
118700     .                                                                    
118800     EJECT                                                                
118900 CCB-SKRIV-KOLLI-RADER SECTION.                                           
119000                                                                          
119100     MOVE 'CCB-SKRIV-KOLLI-RADER      ' TO CURRENT-SECTION                
119200                                                                          
119300     MOVE TAB2-ANTAL-KOLLIN (IX) TO IMDG-ANTAL-KOLLIN                     
119400                                                                          
119500     MOVE TAB2-KDKOLLI (IX)     TO GODK-KDKOLLI-SW                        
119600     IF FIBREBOARDBOX                                                     
119700       IF TAB2-ANTAL-KOLLIN (IX) > 1                                      
119800         MOVE 'Fibreboard Boxes          ' TO IMDG-TEXT-1                 
119900       ELSE                                                               
120000         MOVE 'Fibreboard Box            ' TO IMDG-TEXT-1                 
120100       END-IF                                                             
120200     END-IF                                                               
120300                                                                          
120400     IF PLYWOODBOX                                                        
120500       IF TAB2-ANTAL-KOLLIN (IX) > 1                                      
120600         MOVE 'Plywood Boxes             ' TO IMDG-TEXT-1                 
120700       ELSE                                                               
120800         MOVE 'Plywood Box               ' TO IMDG-TEXT-1                 
120900       END-IF                                                             
121000     END-IF                                                               
121100                                                                          
121200     IF PLASTICDRUM                                                       
121300       IF TAB2-ANTAL-KOLLIN (IX) > 1                                      
121400         MOVE 'Plastic Drums             ' TO IMDG-TEXT-1                 
121500       ELSE                                                               
121600         MOVE 'Plastic Drum              ' TO IMDG-TEXT-1                 
121700       END-IF                                                             
121800     END-IF                                                               
121900                                                                          
122000     IF STEElDRUM                                                         
122100       IF TAB2-ANTAL-KOLLIN (IX) > 1                                      
122200         MOVE 'Steel Drums               ' TO IMDG-TEXT-1                 
122300       ELSE                                                               
122400         MOVE 'Steel Drums               ' TO IMDG-TEXT-1                 
122500       END-IF                                                             
122600     END-IF                                                               
122700                                                                          
122800     MOVE TAB2-VKORDBTO (IX)       TO IMDG-VKORDBTO                       
122900                                                                          
123000     MOVE RAD7                     TO RAD-FG-RAD                          
123100     PERFORM S93-PUT-DOC-LINE                                             
123200                                                                          
123300     MOVE SPACE                    TO RAD-FG-RAD                          
123400     PERFORM S93-PUT-DOC-LINE                                             
123500     .                                                                    
123600     EJECT                                                                
123700 CD-REDIGERA-BLK-EJ-GODK SECTION.                                         
123800                                                                          
123900     MOVE 'CD-REDIGERA-BLK-EJ-GODK    ' TO CURRENT-SECTION                
124000                                                                          
124100     MOVE '2'   TO RAD-IDAFPRCD                                           
124200                                                                          
124300     MOVE +1                       TO IX                                  
124400     MOVE +1                       TO IX2                                 
124500*                                                                         
124600                                                                          
124700     PERFORM UNTIL IX > 99                                                
124800       IF TAB3-ANTAL-KOLLIN (IX) > ZERO AND                               
124900          TAB3-IDPSN (IX, IX2)   > ZERO                                   
125000         PERFORM UNTIL IX2 > 9                                            
125100           IF TAB3-IDPSN (IX, IX2) > 0                                    
125200             MOVE TAB3-IDPSN (IX, IX2) TO GODK-PSN-SW                     
125300             IF GODK-PSN                                                  
125400                                                                          
125500                PERFORM CDA-SKRIV-PSN-RADER                               
125600             END-IF                                                       
125700           END-IF                                                         
125800           ADD +1 TO IX2                                                  
125900         END-PERFORM                                                      
126000         PERFORM CDB-SKRIV-KOLLI-RADER                                    
126100       END-IF                                                             
126200       ADD  +1                    TO IX                                   
126300       MOVE +1                    TO IX2                                  
126400     END-PERFORM                                                          
126500                                                                          
126600     MOVE SPACE            TO RAD-FG-RAD                                  
126700     PERFORM S93-PUT-DOC-LINE                                             
126800                                                                          
126900     .                                                                    
127000     EJECT                                                                
127100 CDA-SKRIV-PSN-RADER SECTION.                                             
127200                                                                          
127300     MOVE 'CDA-SKRIV-PSN-RADER        ' TO CURRENT-SECTION                
127400                                                                          
127500                                                                          
127600     MOVE SPACE                    TO WS-RAD                              
127700                                      RAD8                                
127800     MOVE TAB3-IDPSN (IX, IX2)     TO W-IDPSN                             
127900     MOVE WS-IDSPRAK-GB            TO W-IDSPRAK                           
128000                                                                          
128100     IF LINK-IDPTYP = '   '                                               
128200        MOVE 2                     TO  W-KDFGTRP                          
128300     ELSE                                                                 
128400        IF W-IDPSN = 32 OR 33 OR 34                                       
128500           MOVE 3                  TO  W-KDFGTRP                          
128600        ELSE                                                              
128700           MOVE 2                  TO  W-KDFGTRP                          
128800        END-IF                                                            
128900     END-IF                                                               
129000                                                                          
129100     PERFORM IMS-GET-1165-WDR2                                            
129200                                                                          
129300     IF 1165-1168-BEPSN (1) NOT = SPACE                                   
129400                                                                          
129500        MOVE SPACE                 TO RAD8                                
129600        MOVE 1165-1168-BEPSN (1)   TO IMDG-BEPSN                          
129700                                                                          
129800        MOVE RAD8                  TO RAD-FG-RAD                          
129900        PERFORM S93-PUT-DOC-LINE                                          
130000                                                                          
130100     END-IF                                                               
130200                                                                          
130300     IF 1165-1168-BEPSN (2) NOT = SPACE                                   
130400        MOVE SPACE                 TO RAD8                                
130500        MOVE 1165-1168-BEPSN (2)   TO IMDG-BEPSN                          
130600                                                                          
130700        MOVE RAD8                  TO RAD-FG-RAD                          
130800        PERFORM S93-PUT-DOC-LINE                                          
130900                                                                          
131000     END-IF                                                               
131100                                                                          
131200     IF 1165-1168-BEPSN (3) NOT = SPACE                                   
131300        MOVE SPACE                 TO RAD8                                
131400        MOVE 1165-1168-BEPSN(3)    TO IMDG-BEPSN                          
131500                                                                          
131600        MOVE RAD8                  TO RAD-FG-RAD                          
131700        PERFORM S93-PUT-DOC-LINE                                          
131800                                                                          
131900     END-IF                                                               
132000     .                                                                    
132100     EJECT                                                                
132200 CDB-SKRIV-KOLLI-RADER SECTION.                                           
132300                                                                          
132400     MOVE 'CDB-SKRIV-KOLLI-RADER      ' TO CURRENT-SECTION                
132500                                                                          
132600     MOVE TAB3-ANTAL-KOLLIN (IX) TO IMDG-ANTAL-KOLLIN                     
132700                                                                          
132800     MOVE SPACE                    TO IMDG-TEXT-1                         
132900                                                                          
133000     MOVE TAB3-VKORDBTO (IX)       TO IMDG-VKORDBTO                       
133100                                                                          
133200     MOVE RAD7                     TO RAD-FG-RAD                          
133300     PERFORM S93-PUT-DOC-LINE                                             
133400                                                                          
133500     MOVE SPACE                    TO RAD-FG-RAD                          
133600     PERFORM S93-PUT-DOC-LINE                                             
133700                                                                          
133800     .                                                                    
133900     EJECT                                                                
134000 CE-SKRIV-AVSLUTNING SECTION.                                             
134100                                                                          
134200     MOVE 'CE-SKRIV-AVSLUTNING       ' TO CURRENT-SECTION                 
134300                                                                          
134400     MOVE '3'          TO FOT-IDAFPRCD                                    
134500     MOVE SPACE        TO FOT-FG-FOT                                      
134600     MOVE LINK-IDDISTR TO TEST-IDDISTR                                    
134700                                                                          
134800     MOVE RAD9                     TO FOT-FG-FOT                          
134900     PERFORM S94-PUT-DOC-FOOT                                             
135000                                                                          
135100     MOVE RAD10                    TO FOT-FG-FOT                          
135200     PERFORM S94-PUT-DOC-FOOT                                             
135300                                                                          
135400     MOVE RAD11                    TO FOT-FG-FOT                          
135500     PERFORM S94-PUT-DOC-FOOT                                             
135600                                                                          
135700     .                                                                    
135800     EJECT                                                                
135900 G-SKRIV-BLANKETT SECTION.                                                
136000                                                                          
136100     MOVE 'G-SKRIV-BLANKETT           ' TO CURRENT-SECTION                
136200                                                                          
136300     PERFORM S01-OPPNA-PRINTER                                            
136400     PERFORM GA-SKAPA-PRINTER-ID                                          
136500                                                                          
136600     IF TAB1-DATA-FINNS                                                   
136700       MOVE NEJ                    TO NY-SIDA-SW                          
136800       MOVE +20                    TO RAD-IX                              
136900       MOVE +1                     TO IX                                  
137000                                                                          
137100       PERFORM UNTIL IX > MAX-IX                                          
137200          IF RAD-IX > MAX-RAD-IX                                          
137300             PERFORM GB-SKRIV-ADRESS                                      
137400          END-IF                                                          
137500          PERFORM GC-REDIGERA-BLK-OMIXAD                                  
137600       END-PERFORM                                                        
137700     END-IF                                                               
137800                                                                          
137900     IF TAB2-KDKOLLI (1) > SPACE                                          
138000       MOVE NEJ                    TO NY-SIDA-SW                          
138100       MOVE +20                    TO RAD-IX                              
138200       MOVE +1                     TO IX                                  
138300                                                                          
138400       PERFORM UNTIL IX > 99                                              
138500          IF RAD-IX > MAX-RAD-IX                                          
138600             PERFORM GB-SKRIV-ADRESS                                      
138700          END-IF                                                          
138800          PERFORM GD-REDIGERA-BLK-MIXAD                                   
138900       END-PERFORM                                                        
139000     END-IF                                                               
139100                                                                          
139200     IF TAB3-ANTAL-KOLLIN (1) > ZERO AND                                  
139300        TAB3-IDPSN (1, 1)     > ZERO                                      
139400       MOVE NEJ                    TO NY-SIDA-SW                          
139500       MOVE +20                    TO RAD-IX                              
139600       MOVE +1                     TO IX                                  
139700                                                                          
139800       PERFORM UNTIL IX > 99                                              
139900          IF RAD-IX > MAX-RAD-IX                                          
140000             PERFORM GB-SKRIV-ADRESS                                      
140100          END-IF                                                          
140200          PERFORM GE-REDIGERA-BLK-EJ-GODK                                 
140300       END-PERFORM                                                        
140400     END-IF                                                               
140500                                                                          
140600     PERFORM S10-STAENG-PRINTER                                           
140700     .                                                                    
140800     EJECT                                                                
140900 D-KOLLA-DC  SECTION.                                                     
141000                                                                          
141100     MOVE 'D-KOLLA-DC                 ' TO CURRENT-SECTION                
141200                                                                          
141300     MOVE LINK-IDDISTR              TO TEST-IDDISTR                       
141400     MOVE LINK-IDKUNDNR             TO KUND15-IDKUNDNR                    
141500                                                                          
141600     PERFORM IMS-GU-WDB601                                                
141700                                                                          
141800     IF SEGMENT-FINNS                                                     
141900       MOVE JA                      TO ADRESS-SW                          
142000     END-IF                                                               
142100                                                                          
142200     IF NDC-CN-71                                                         
142300       MOVE JA                      TO DC-FG-SW                           
142400     ELSE                                                                 
142500       IF LDC-FR                AND                                       
142600          DIST67-FG-FR          AND                                       
142700          KUND15-KUND-FG                                                  
142800         MOVE JA                    TO DC-FG-SW                           
142900       ELSE                                                               
143000         IF LDC-SE-1C           AND                                       
143100            DIST67-FG-PL                                                  
143200           MOVE JA                  TO DC-FG-SW                           
143300         ELSE                                                             
143400           IF LDC-SE-1A         AND                                       
143500              DIST67-FG-SE                                                
143700             MOVE JA                TO DC-FG-SW                           
143800           ELSE                                                           
143900             IF LDC-FI          AND                                       
144000                DIST67-FG-BL                                              
144100               MOVE JA              TO DC-FG-SW                           
144200             ELSE                                                         
144300               IF SDC-IT        AND                                       
144400                  DIST67-FG-MT                                            
144500                 MOVE JA            TO DC-FG-SW                           
144600               END-IF                                                     
144700             END-IF                                                       
144800           END-IF                                                         
144900         END-IF                                                           
145000       END-IF                                                             
145100     END-IF                                                               
145200     .                                                                    
145300     EJECT                                                                
145400 GA-SKAPA-PRINTER-ID SECTION.                                             
145500                                                                          
145600     MOVE 'GA-SKAPA-PRINTER-ID        ' TO CURRENT-SECTION                
145700                                                                          
145800     IF CDC-SE  OR DDC-SE                                                 
145900       MOVE LINK-IDDISTR             TO TEST-IDDISTR                      
146000       IF DIST66-PRINTER-EUROPA2                                          
146100         MOVE 'W40502E2'             TO WS-PRT-IDPRTLST                   
146200       ELSE                                                               
146300         IF DIST66-PRINTER-NORDEN                                         
146400           MOVE 'W40502N '           TO WS-PRT-IDPRTLST                   
146500         ELSE                                                             
146600           IF DIST66-PRINTER-OVERSEAS                                     
146700             MOVE 'W40502O '         TO WS-PRT-IDPRTLST                   
146800           ELSE                                                           
146900             IF DIST66-PRINTER-EUROPA3                                    
147000               MOVE 'W40502E3'       TO WS-PRT-IDPRTLST                   
147100             ELSE                                                         
147200               MOVE 'W40502E2'       TO WS-PRT-IDPRTLST                   
147300             END-IF                                                       
147400           END-IF                                                         
147500         END-IF                                                           
147600       END-IF                                                             
147700*    ELSE                                                                 
147800*      IF SDC-NL                                                          
147900*        MOVE 'W4051221'             TO WS-PRT-IDPRTLST                   
148000*      END-IF                                                             
148100     END-IF                                                               
148200                                                                          
148300     MOVE LINK-IDDISTR             TO WS-PRT-IDDISTR                      
148400     MOVE LINK-KDFRAKT             TO WS-PRT-KDFRAKT                      
148500                                                                          
148600     .                                                                    
148700     EJECT                                                                
148800 GB-SKRIV-ADRESS SECTION.                                                 
148900                                                                          
149000     MOVE 'GB-SKRIV-ADRESS            ' TO CURRENT-SECTION                
149100                                                                          
149200*--- SKRIVER  NAMN OCH ADRESS PÅ RAD 12 - 16                              
149300                                                                          
149400     IF LINK-IDSYSTEM = '4535' OR '4665' OR '4675'                        
149500                                                                          
149600        MOVE PRT-NYSIDA-RAD5       TO PRT-RADSKIP                         
149700        MOVE SPACE                 TO WS-RAD                              
149800        PERFORM S02-SKRIV                                                 
149900                                                                          
150000        MOVE PRT-AFTER-1           TO PRT-RADSKIP                         
150100        MOVE RAD0                  TO WS-RAD                              
150200        PERFORM S02-SKRIV                                                 
150300     ELSE                                                                 
150400        MOVE PRT-NYSIDA-RAD2       TO PRT-RADSKIP                         
150500        MOVE SPACE                 TO WS-RAD                              
150600        PERFORM S02-SKRIV                                                 
150700                                                                          
150800        MOVE PRT-AFTER-1           TO PRT-RADSKIP                         
150900        MOVE RAD00                 TO WS-RAD                              
151000        PERFORM S02-SKRIV                                                 
151100                                                                          
151200        MOVE PRT-AFTER-3           TO PRT-RADSKIP                         
151300        MOVE RAD0                  TO WS-RAD                              
151400        PERFORM S02-SKRIV                                                 
151500     END-IF                                                               
151600                                                                          
151700     MOVE PRT-AFTER-6              TO PRT-RADSKIP                         
151800     MOVE RAD1                     TO WS-RAD                              
151900     PERFORM S02-SKRIV                                                    
152000                                                                          
152100     MOVE PRT-AFTER-1              TO PRT-RADSKIP                         
152200     MOVE RAD2                     TO WS-RAD                              
152300     PERFORM S02-SKRIV                                                    
152400                                                                          
152500     MOVE PRT-AFTER-1              TO PRT-RADSKIP                         
152600     MOVE RAD3                     TO WS-RAD                              
152700     PERFORM S02-SKRIV                                                    
152800                                                                          
152900     MOVE PRT-AFTER-1              TO PRT-RADSKIP                         
153000     MOVE RAD4                     TO WS-RAD                              
153100     PERFORM S02-SKRIV                                                    
153200                                                                          
153300     MOVE PRT-AFTER-1              TO PRT-RADSKIP                         
153400     MOVE RAD5                     TO WS-RAD                              
153500     PERFORM S02-SKRIV                                                    
153600                                                                          
153700     .                                                                    
153800     EJECT                                                                
153900 GC-REDIGERA-BLK-OMIXAD SECTION.                                          
154000                                                                          
154100     MOVE 'GC-REDIGERA-BLK-OMIXAD     ' TO CURRENT-SECTION                
154200                                                                          
154300                                                                          
154400*--- POSITIONERA FRAM TILL FÖRSTA KOLLIRADEN                              
154500                                                                          
154600     MOVE PRT-AFTER-6              TO PRT-RADSKIP                         
154700     MOVE SPACE                    TO WS-RAD                              
154800     PERFORM S02-SKRIV                                                    
154900                                                                          
155000     MOVE PRT-AFTER-6              TO PRT-RADSKIP                         
155100     MOVE SPACE                    TO WS-RAD                              
155200     PERFORM S02-SKRIV                                                    
155300                                                                          
155400     MOVE PRT-AFTER-2              TO PRT-RADSKIP                         
155500     MOVE SPACE                    TO WS-RAD                              
155600     PERFORM S02-SKRIV                                                    
155700*                                                                         
155800     MOVE +1                       TO RAD-IX                              
155900     IF NY-SIDA-FINNS                                                     
156000       CONTINUE                                                           
156100     ELSE                                                                 
156200       MOVE +1                     TO IX                                  
156300       MOVE +1                     TO IX2                                 
156400     END-IF                                                               
156500                                                                          
156600     PERFORM UNTIL IX > MAX-IX OR RAD-IX > MAX-RAD-IX                     
156700        IF TAB1-IDPSN (IX) > 0                                            
156800           MOVE TAB1-IDPSN (IX)    TO GODK-PSN-SW                         
156900           IF GODK-PSN                                                    
157000                                                                          
157100             PERFORM UNTIL IX2 > 4                                        
157200               IF TAB1-KDKOLLI (IX, IX2) > SPACE                          
157300                 PERFORM GCA-SKRIV-KOLLI-RADER                            
157400               END-IF                                                     
157500               ADD +1 TO IX2                                              
157600             END-PERFORM                                                  
157700           END-IF                                                         
157800        END-IF                                                            
157900        ADD  +1                    TO IX                                  
158000        MOVE +1                    TO IX2                                 
158100     END-PERFORM                                                          
158200                                                                          
158300     IF RAD-IX > MAX-RAD-IX                                               
158400       PERFORM GCB-KOLLA-OM-FLER-PSN-FINNS                                
158500     END-IF                                                               
158600                                                                          
158700     PERFORM GCC-SKRIV-AVSLUTNING                                         
158800     .                                                                    
158900     EJECT                                                                
159000 GCA-SKRIV-KOLLI-RADER SECTION.                                           
159100                                                                          
159200     MOVE 'GCA-SKRIV-KOLLI-RADER      ' TO CURRENT-SECTION                
159300                                                                          
159400*--- KOLLI RADER                                                          
159500                                                                          
159600     MOVE SPACE                    TO WS-RAD                              
159700                                      RAD8                                
159800     MOVE TAB1-IDPSN (IX)          TO W-IDPSN                             
159900     MOVE WS-IDSPRAK-SE            TO W-IDSPRAK                           
160000                                                                          
160100     IF LINK-IDPTYP = '   '                                               
160200        MOVE 2                     TO  W-KDFGTRP                          
160300     ELSE                                                                 
160400        IF W-IDPSN = 32 OR 33 OR 34                                       
160500           MOVE 3                  TO  W-KDFGTRP                          
160600        ELSE                                                              
160700           MOVE 2                  TO  W-KDFGTRP                          
160800        END-IF                                                            
160900     END-IF                                                               
161000                                                                          
161100     PERFORM IMS-GET-1165-WDR2                                            
161200                                                                          
161300     IF 1165-1168-BEPSN (1) NOT = SPACE                                   
161400        MOVE 1165-1168-BEPSN (1)   TO IMDG-BEPSN                          
161500                                                                          
161600        MOVE PRT-AFTER-1           TO PRT-RADSKIP                         
161700        MOVE RAD8                  TO WS-RAD                              
161800        PERFORM S02-SKRIV                                                 
161900                                                                          
162000        ADD +1                     TO RAD-IX                              
162100                                                                          
162200     END-IF                                                               
162300                                                                          
162400     IF 1165-1168-BEPSN (2) NOT = SPACE                                   
162500        MOVE SPACE                 TO RAD8                                
162600        MOVE 1165-1168-BEPSN (2)   TO IMDG-BEPSN                          
162700                                                                          
162800        MOVE PRT-AFTER-1           TO PRT-RADSKIP                         
162900        MOVE RAD8                  TO WS-RAD                              
163000        PERFORM S02-SKRIV                                                 
163100                                                                          
163200        ADD +1                     TO RAD-IX                              
163300                                                                          
163400     END-IF                                                               
163500                                                                          
163600     IF 1165-1168-BEPSN (3) NOT = SPACE                                   
163700        MOVE SPACE                 TO RAD8                                
163800        MOVE 1165-1168-BEPSN(3)    TO IMDG-BEPSN                          
163900                                                                          
164000        MOVE PRT-AFTER-1           TO PRT-RADSKIP                         
164100        MOVE RAD8                  TO WS-RAD                              
164200        PERFORM S02-SKRIV                                                 
164300                                                                          
164400        ADD +1                     TO RAD-IX                              
164500                                                                          
164600     END-IF                                                               
164700                                                                          
164800     MOVE TAB1-ANTAL-KOLLIN (IX, IX2)      TO IMDG-ANTAL-KOLLIN           
164900                                                                          
165000     MOVE TAB1-KDKOLLI (IX, IX2)           TO GODK-KDKOLLI-SW             
165100     IF FIBREBOARDBOX                                                     
165200       IF TAB1-ANTAL-KOLLIN (IX, IX2) > 1                                 
165300         MOVE 'Fibreboard Boxes          ' TO IMDG-TEXT-1                 
165400       ELSE                                                               
165500         MOVE 'Fibreboard Box            ' TO IMDG-TEXT-1                 
165600       END-IF                                                             
165700     END-IF                                                               
165800                                                                          
165900     IF PLYWOODBOX                                                        
166000       IF TAB1-ANTAL-KOLLIN (IX, IX2) > 1                                 
166100         MOVE 'Plywood Boxes             ' TO IMDG-TEXT-1                 
166200       ELSE                                                               
166300         MOVE 'Plywood Box               ' TO IMDG-TEXT-1                 
166400       END-IF                                                             
166500     END-IF                                                               
166600                                                                          
166700     IF PLASTICDRUM                                                       
166800       IF TAB1-ANTAL-KOLLIN (IX, IX2) > 1                                 
166900         MOVE 'Plastic Drums             ' TO IMDG-TEXT-1                 
167000       ELSE                                                               
167100         MOVE 'Plastic Drum              ' TO IMDG-TEXT-1                 
167200       END-IF                                                             
167300     END-IF                                                               
167400                                                                          
167500     IF STEELDRUM                                                         
167600       IF TAB1-ANTAL-KOLLIN (IX, IX2) > 1                                 
167700         MOVE 'Steel Drums               ' TO IMDG-TEXT-1                 
167800       ELSE                                                               
167900         MOVE 'Steel Drums               ' TO IMDG-TEXT-1                 
168000       END-IF                                                             
168100     END-IF                                                               
168200                                                                          
168300     MOVE TAB1-VKORDBTO (IX, IX2)  TO IMDG-VKORDBTO                       
168400                                                                          
168500     MOVE PRT-AFTER-1              TO PRT-RADSKIP                         
168600     MOVE RAD7                     TO WS-RAD                              
168700     PERFORM S02-SKRIV                                                    
168800     ADD +1                        TO RAD-IX                              
168900                                                                          
169000     MOVE PRT-AFTER-1              TO PRT-RADSKIP                         
169100     MOVE SPACE                    TO WS-RAD                              
169200     PERFORM S02-SKRIV                                                    
169300     ADD +1                        TO RAD-IX                              
169400     .                                                                    
169500     EJECT                                                                
169600 GCB-KOLLA-OM-FLER-PSN-FINNS SECTION.                                     
169700                                                                          
169800     MOVE 'GCB-KOLLA-OM-FLER-PSN-FINNS' TO CURRENT-SECTION                
169900                                                                          
170000     MOVE IX                       TO SPAR-IX                             
170100                                                                          
170200     PERFORM UNTIL IX > MAX-IX                                            
170300                                                                          
170400       IF TAB1-IDPSN (IX) > ZERO                                          
170500         MOVE TAB1-IDPSN (IX)      TO GODK-PSN-SW                         
170600         IF GODK-PSN                                                      
170700           MOVE JA                 TO NY-SIDA-SW                          
170800           MOVE 1000               TO IX                                  
170900         END-IF                                                           
171000       END-IF                                                             
171100                                                                          
171200       ADD +1                      TO IX                                  
171300     END-PERFORM                                                          
171400                                                                          
171500     IF NY-SIDA-FINNS                                                     
171600       MOVE SPAR-IX                TO IX                                  
171700     END-IF                                                               
171800     .                                                                    
171900     EJECT                                                                
172000 GCC-SKRIV-AVSLUTNING SECTION.                                            
172100                                                                          
172200     MOVE 'GCC-SKRIV-AVSLUTNING       ' TO CURRENT-SECTION                
172300                                                                          
172400     MOVE PRT-EQUAL-53             TO PRT-RADSKIP                         
172500     MOVE RAD9                     TO WS-RAD                              
172600     PERFORM S02-SKRIV                                                    
172700                                                                          
172800     MOVE PRT-AFTER-1              TO PRT-RADSKIP                         
172900     MOVE RAD9A                    TO WS-RAD                              
173000     PERFORM S02-SKRIV                                                    
173100                                                                          
173200     MOVE PRT-AFTER-1              TO PRT-RADSKIP                         
173300     MOVE RAD11                    TO WS-RAD                              
173400     PERFORM S02-SKRIV                                                    
173500                                                                          
173600     MOVE PRT-EQUAL-61             TO PRT-RADSKIP                         
173700     MOVE LINK-IDDISTR             TO TEST-IDDISTR                        
173800     IF DIST37-TRAILER-DISTR                                              
173900       MOVE RAD12A                 TO WS-RAD                              
174000     ELSE                                                                 
174100       MOVE RAD12                  TO WS-RAD                              
174200     END-IF                                                               
174300     PERFORM S02-SKRIV                                                    
174400                                                                          
174500     MOVE PRT-EQUAL-63             TO PRT-RADSKIP                         
174600     MOVE RAD13                    TO WS-RAD                              
174700     PERFORM S02-SKRIV                                                    
174800     .                                                                    
174900     EJECT                                                                
175000 GD-REDIGERA-BLK-MIXAD SECTION.                                           
175100                                                                          
175200     MOVE 'GD-REDIGERA-BLK-MIXAD      ' TO CURRENT-SECTION                
175300                                                                          
175400*--- POSITIONERA FRAM TILL FÖRSTA KOLLIRADEN                              
175500                                                                          
175600     MOVE PRT-AFTER-6              TO PRT-RADSKIP                         
175700     MOVE SPACE                    TO WS-RAD                              
175800     PERFORM S02-SKRIV                                                    
175900                                                                          
176000     MOVE PRT-AFTER-6              TO PRT-RADSKIP                         
176100     MOVE SPACE                    TO WS-RAD                              
176200     PERFORM S02-SKRIV                                                    
176300                                                                          
176400     MOVE PRT-AFTER-2              TO PRT-RADSKIP                         
176500     MOVE SPACE                    TO WS-RAD                              
176600     PERFORM S02-SKRIV                                                    
176700*                                                                         
176800     MOVE +1                       TO RAD-IX                              
176900     IF NY-SIDA-FINNS                                                     
177000       CONTINUE                                                           
177100     ELSE                                                                 
177200       MOVE +1                     TO IX                                  
177300       MOVE +1                     TO IX2                                 
177400     END-IF                                                               
177500                                                                          
177600     PERFORM UNTIL IX > 99 OR RAD-IX > MAX-RAD-IX                         
177700        IF TAB2-KDKOLLI (IX) > SPACE                                      
177800           PERFORM UNTIL IX2 > 9                                          
177900             IF TAB2-IDPSN (IX, IX2 ) > 0                                 
178000               MOVE TAB2-IDPSN (IX, IX2) TO GODK-PSN-SW                   
178100               IF GODK-PSN                                                
178200                                                                          
178300                  PERFORM GDA-SKRIV-PSN-RADER                             
178400               END-IF                                                     
178500             END-IF                                                       
178600             ADD +1 TO IX2                                                
178700           END-PERFORM                                                    
178800           PERFORM GDB-SKRIV-KOLLI-RADER                                  
178900        END-IF                                                            
179000        ADD  +1                    TO IX                                  
179100        MOVE +1                    TO IX2                                 
179200     END-PERFORM                                                          
179300                                                                          
179400     IF RAD-IX > MAX-RAD-IX                                               
179500       PERFORM GDC-KOLLA-OM-FLER-KDKOLLI                                  
179600     END-IF                                                               
179700                                                                          
179800     PERFORM GDD-SKRIV-AVSLUTNING                                         
179900     .                                                                    
180000     EJECT                                                                
180100 GDA-SKRIV-PSN-RADER SECTION.                                             
180200                                                                          
180300     MOVE 'GDA-SKRIV-PSN-RADER        ' TO CURRENT-SECTION                
180400                                                                          
180500     MOVE SPACE                    TO WS-RAD                              
180600                                      RAD8                                
180700     MOVE TAB2-IDPSN (IX, IX2)     TO W-IDPSN                             
180800     MOVE WS-IDSPRAK-SE            TO W-IDSPRAK                           
180900                                                                          
181000     IF LINK-IDPTYP = '   '                                               
181100        MOVE 2                     TO  W-KDFGTRP                          
181200     ELSE                                                                 
181300        IF W-IDPSN = 32 OR 33 OR 34                                       
181400           MOVE 3                  TO  W-KDFGTRP                          
181500        ELSE                                                              
181600           MOVE 2                  TO  W-KDFGTRP                          
181700        END-IF                                                            
181800     END-IF                                                               
181900                                                                          
182000     PERFORM IMS-GET-1165-WDR2                                            
182100                                                                          
182200     IF 1165-1168-BEPSN (1) NOT = SPACE                                   
182300        MOVE 1165-1168-BEPSN (1)   TO IMDG-BEPSN                          
182400                                                                          
182500        MOVE PRT-AFTER-1           TO PRT-RADSKIP                         
182600        MOVE RAD8                  TO WS-RAD                              
182700        PERFORM S02-SKRIV                                                 
182800                                                                          
182900        ADD +1                     TO RAD-IX                              
183000     END-IF                                                               
183100                                                                          
183200     IF 1165-1168-BEPSN (2) NOT = SPACE                                   
183300        MOVE SPACE                 TO RAD8                                
183400        MOVE 1165-1168-BEPSN(2)    TO IMDG-BEPSN                          
183500                                                                          
183600        MOVE PRT-AFTER-1           TO PRT-RADSKIP                         
183700        MOVE RAD8                  TO WS-RAD                              
183800        PERFORM S02-SKRIV                                                 
183900                                                                          
184000        ADD +1                     TO RAD-IX                              
184100     END-IF                                                               
184200                                                                          
184300     IF 1165-1168-BEPSN (3) NOT = SPACE                                   
184400        MOVE SPACE                 TO RAD8                                
184500        MOVE 1165-1168-BEPSN(3)    TO IMDG-BEPSN                          
184600                                                                          
184700        MOVE PRT-AFTER-1           TO PRT-RADSKIP                         
184800        MOVE RAD8                  TO WS-RAD                              
184900        PERFORM S02-SKRIV                                                 
185000                                                                          
185100        ADD +1                     TO RAD-IX                              
185200                                                                          
185300     END-IF                                                               
185400     .                                                                    
185500     EJECT                                                                
185600 GDB-SKRIV-KOLLI-RADER SECTION.                                           
185700                                                                          
185800     MOVE 'GDB-SKRIV-KOLLI-RADER      ' TO CURRENT-SECTION                
185900                                                                          
186000     MOVE TAB2-ANTAL-KOLLIN (IX) TO IMDG-ANTAL-KOLLIN                     
186100                                                                          
186200     MOVE TAB2-KDKOLLI (IX)     TO GODK-KDKOLLI-SW                        
186300     IF FIBREBOARDBOX                                                     
186400       IF TAB2-ANTAL-KOLLIN (IX) > 1                                      
186500         MOVE 'Fibreboard Boxes          ' TO IMDG-TEXT-1                 
186600       ELSE                                                               
186700         MOVE 'Fibreboard Box            ' TO IMDG-TEXT-1                 
186800       END-IF                                                             
186900     END-IF                                                               
187000                                                                          
187100     IF PLYWOODBOX                                                        
187200       IF TAB2-ANTAL-KOLLIN (IX) > 1                                      
187300         MOVE 'Plywood Boxes             ' TO IMDG-TEXT-1                 
187400       ELSE                                                               
187500         MOVE 'Plywood Box               ' TO IMDG-TEXT-1                 
187600       END-IF                                                             
187700     END-IF                                                               
187800                                                                          
187900     IF PLASTICDRUM                                                       
188000       IF TAB2-ANTAL-KOLLIN (IX) > 1                                      
188100         MOVE 'Plastic Drums             ' TO IMDG-TEXT-1                 
188200       ELSE                                                               
188300         MOVE 'Plastic Drum              ' TO IMDG-TEXT-1                 
188400       END-IF                                                             
188500     END-IF                                                               
188600                                                                          
188700     IF STEElDRUM                                                         
188800       IF TAB2-ANTAL-KOLLIN (IX) > 1                                      
188900         MOVE 'Steel Drums               ' TO IMDG-TEXT-1                 
189000       ELSE                                                               
189100         MOVE 'Steel Drums               ' TO IMDG-TEXT-1                 
189200       END-IF                                                             
189300     END-IF                                                               
189400                                                                          
189500     MOVE TAB2-VKORDBTO (IX)       TO IMDG-VKORDBTO                       
189600                                                                          
189700     MOVE PRT-AFTER-1              TO PRT-RADSKIP                         
189800     MOVE RAD7                     TO WS-RAD                              
189900     PERFORM S02-SKRIV                                                    
190000     ADD +1                        TO RAD-IX                              
190100                                                                          
190200     MOVE PRT-AFTER-1              TO PRT-RADSKIP                         
190300     MOVE SPACE                    TO WS-RAD                              
190400     PERFORM S02-SKRIV                                                    
190500     ADD +1                        TO RAD-IX                              
190600     .                                                                    
190700     EJECT                                                                
190800 GDC-KOLLA-OM-FLER-KDKOLLI SECTION.                                       
190900                                                                          
191000     MOVE 'GDC-KOLLA-OM-FLER-KDKOLLI  ' TO CURRENT-SECTION                
191100                                                                          
191200     MOVE IX                       TO SPAR-IX                             
191300                                                                          
191400     PERFORM UNTIL IX > 99                                                
191500                                                                          
191600       IF TAB2-KDKOLLI(IX) > SPACE                                        
191700         MOVE TAB2-KDKOLLI (IX)    TO GODK-KDKOLLI-SW                     
191800         IF FIBREBOARDBOX OR PLYWOODBOX OR                                
191900            PLASTICDRUM   OR STEELDRUM                                    
192000           MOVE JA                 TO NY-SIDA-SW                          
192100           MOVE 100                TO IX                                  
192200         END-IF                                                           
192300       END-IF                                                             
192400                                                                          
192500       ADD +1                      TO IX                                  
192600     END-PERFORM                                                          
192700                                                                          
192800     IF NY-SIDA-FINNS                                                     
192900       MOVE SPAR-IX                TO IX                                  
193000     END-IF                                                               
193100     .                                                                    
193200     EJECT                                                                
193300 GDD-SKRIV-AVSLUTNING SECTION.                                            
193400                                                                          
193500     MOVE 'GDD-SKRIV-AVSLUTNING       ' TO CURRENT-SECTION                
193600                                                                          
193700                                                                          
193800     MOVE PRT-EQUAL-53             TO PRT-RADSKIP                         
193900     MOVE RAD9                     TO WS-RAD                              
194000     PERFORM S02-SKRIV                                                    
194100                                                                          
194200     MOVE PRT-AFTER-1              TO PRT-RADSKIP                         
194300     MOVE RAD9A                    TO WS-RAD                              
194400     PERFORM S02-SKRIV                                                    
194500                                                                          
194600     MOVE PRT-AFTER-1              TO PRT-RADSKIP                         
194700     MOVE RAD11                    TO WS-RAD                              
194800     PERFORM S02-SKRIV                                                    
194900                                                                          
195000     MOVE PRT-EQUAL-61             TO PRT-RADSKIP                         
195100     MOVE LINK-IDDISTR             TO TEST-IDDISTR                        
195200     IF DIST37-TRAILER-DISTR                                              
195300       MOVE RAD12A                 TO WS-RAD                              
195400     ELSE                                                                 
195500       MOVE RAD12                  TO WS-RAD                              
195600     END-IF                                                               
195700     PERFORM S02-SKRIV                                                    
195800                                                                          
195900     MOVE PRT-EQUAL-63             TO PRT-RADSKIP                         
196000     MOVE RAD13                    TO WS-RAD                              
196100     PERFORM S02-SKRIV                                                    
196200     .                                                                    
196300     EJECT                                                                
196400 GE-REDIGERA-BLK-EJ-GODK SECTION.                                         
196500                                                                          
196600     MOVE 'GE-REDIGERA-BLK-EJ-GODK    ' TO CURRENT-SECTION                
196700                                                                          
196800*--- POSITIONERA FRAM TILL FÖRSTA KOLLIRADEN                              
196900                                                                          
197000     MOVE PRT-AFTER-6              TO PRT-RADSKIP                         
197100     MOVE SPACE                    TO WS-RAD                              
197200     PERFORM S02-SKRIV                                                    
197300                                                                          
197400     MOVE PRT-AFTER-6              TO PRT-RADSKIP                         
197500     MOVE SPACE                    TO WS-RAD                              
197600     PERFORM S02-SKRIV                                                    
197700                                                                          
197800     MOVE PRT-AFTER-2              TO PRT-RADSKIP                         
197900     MOVE SPACE                    TO WS-RAD                              
198000     PERFORM S02-SKRIV                                                    
198100*                                                                         
198200     MOVE +1                       TO RAD-IX                              
198300     IF NY-SIDA-FINNS                                                     
198400       CONTINUE                                                           
198500     ELSE                                                                 
198600       MOVE +1                     TO IX                                  
198700       MOVE +1                     TO IX2                                 
198800     END-IF                                                               
198900                                                                          
199000     PERFORM UNTIL IX > 99 OR RAD-IX > MAX-RAD-IX                         
199100       IF TAB3-ANTAL-KOLLIN (IX) > ZERO AND                               
199200          TAB3-IDPSN (IX, IX2)   > ZERO                                   
199300         PERFORM UNTIL IX2 > 9                                            
199400           IF TAB3-IDPSN (IX, IX2) > 0                                    
199500             MOVE TAB3-IDPSN (IX, IX2) TO GODK-PSN-SW                     
199600             IF GODK-PSN                                                  
199700                                                                          
199800                PERFORM GEA-SKRIV-PSN-RADER                               
199900             END-IF                                                       
200000           END-IF                                                         
200100           ADD +1 TO IX2                                                  
200200         END-PERFORM                                                      
200300         PERFORM GEB-SKRIV-KOLLI-RADER                                    
200400       END-IF                                                             
200500       ADD  +1                    TO IX                                   
200600       MOVE +1                    TO IX2                                  
200700     END-PERFORM                                                          
200800                                                                          
200900     IF RAD-IX > MAX-RAD-IX                                               
201000       PERFORM GEC-KOLLA-OM-FLER-PSN-FINNS                                
201100     END-IF                                                               
201200                                                                          
201300     PERFORM GED-SKRIV-AVSLUTNING                                         
201400     .                                                                    
201500     EJECT                                                                
201600 GEA-SKRIV-PSN-RADER SECTION.                                             
201700                                                                          
201800     MOVE 'GEA-SKRIV-PSN-RADER        ' TO CURRENT-SECTION                
201900                                                                          
202000                                                                          
202100     MOVE SPACE                    TO WS-RAD                              
202200                                      RAD8                                
202300     MOVE TAB3-IDPSN (IX, IX2)     TO W-IDPSN                             
202400     MOVE WS-IDSPRAK-SE            TO W-IDSPRAK                           
202500                                                                          
202600     IF LINK-IDPTYP = '   '                                               
202700        MOVE 2                     TO  W-KDFGTRP                          
202800     ELSE                                                                 
202900        IF W-IDPSN = 32 OR 33 OR 34                                       
203000           MOVE 3                  TO  W-KDFGTRP                          
203100        ELSE                                                              
203200           MOVE 2                  TO  W-KDFGTRP                          
203300        END-IF                                                            
203400     END-IF                                                               
203500                                                                          
203600     PERFORM IMS-GET-1165-WDR2                                            
203700                                                                          
203800     IF 1165-1168-BEPSN (1) NOT = SPACE                                   
203900        MOVE 1165-1168-BEPSN (1)   TO IMDG-BEPSN                          
204000                                                                          
204100        MOVE PRT-AFTER-1           TO PRT-RADSKIP                         
204200        MOVE RAD8                  TO WS-RAD                              
204300        PERFORM S02-SKRIV                                                 
204400                                                                          
204500        ADD +1                     TO RAD-IX                              
204600                                                                          
204700     END-IF                                                               
204800                                                                          
204900     IF 1165-1168-BEPSN (2) NOT = SPACE                                   
205000        MOVE SPACE                 TO RAD8                                
205100        MOVE 1165-1168-BEPSN (2)   TO IMDG-BEPSN                          
205200                                                                          
205300        MOVE PRT-AFTER-1           TO PRT-RADSKIP                         
205400        MOVE RAD8                  TO WS-RAD                              
205500        PERFORM S02-SKRIV                                                 
205600                                                                          
205700        ADD +1                     TO RAD-IX                              
205800                                                                          
205900     END-IF                                                               
206000                                                                          
206100     IF 1165-1168-BEPSN (3) NOT = SPACE                                   
206200        MOVE SPACE                 TO RAD8                                
206300        MOVE 1165-1168-BEPSN(3)    TO IMDG-BEPSN                          
206400                                                                          
206500        MOVE PRT-AFTER-1           TO PRT-RADSKIP                         
206600        MOVE RAD8                  TO WS-RAD                              
206700        PERFORM S02-SKRIV                                                 
206800                                                                          
206900        ADD +1                     TO RAD-IX                              
207000                                                                          
207100     END-IF                                                               
207200     .                                                                    
207300     EJECT                                                                
207400 GEB-SKRIV-KOLLI-RADER SECTION.                                           
207500                                                                          
207600     MOVE 'GEB-SKRIV-KOLLI-RADER      ' TO CURRENT-SECTION                
207700                                                                          
207800     MOVE TAB3-ANTAL-KOLLIN (IX) TO IMDG-ANTAL-KOLLIN                     
207900                                                                          
208000     MOVE SPACE                    TO IMDG-TEXT-1                         
208100                                                                          
208200     MOVE TAB3-VKORDBTO (IX)       TO IMDG-VKORDBTO                       
208300                                                                          
208400     MOVE PRT-AFTER-1              TO PRT-RADSKIP                         
208500     MOVE RAD7                     TO WS-RAD                              
208600     PERFORM S02-SKRIV                                                    
208700     ADD +1                        TO RAD-IX                              
208800                                                                          
208900     MOVE PRT-AFTER-1              TO PRT-RADSKIP                         
209000     MOVE SPACE                    TO WS-RAD                              
209100     PERFORM S02-SKRIV                                                    
209200     ADD +1                        TO RAD-IX                              
209300     .                                                                    
209400     EJECT                                                                
209500 GEC-KOLLA-OM-FLER-PSN-FINNS SECTION.                                     
209600                                                                          
209700     MOVE 'GEC-KOLLA-OM-FLER-PSN-FINNS' TO CURRENT-SECTION                
209800                                                                          
209900     MOVE IX                       TO SPAR-IX                             
210000                                                                          
210100     PERFORM UNTIL IX > 99                                                
210200                                                                          
210300       IF TAB3-VKORDBTO (IX) > ZERO                                       
210400         MOVE JA                   TO NY-SIDA-SW                          
210500         MOVE 100                  TO IX                                  
210600       END-IF                                                             
210700                                                                          
210800       ADD +1                      TO IX                                  
210900     END-PERFORM                                                          
211000                                                                          
211100     IF NY-SIDA-FINNS                                                     
211200       MOVE SPAR-IX                TO IX                                  
211300     END-IF                                                               
211400     .                                                                    
211500     EJECT                                                                
211600 GED-SKRIV-AVSLUTNING SECTION.                                            
211700                                                                          
211800     MOVE 'GED-SKRIV-AVSLUTNING       ' TO CURRENT-SECTION                
211900                                                                          
212000     MOVE PRT-EQUAL-53             TO PRT-RADSKIP                         
212100     MOVE RAD9                     TO WS-RAD                              
212200     PERFORM S02-SKRIV                                                    
212300                                                                          
212400     MOVE PRT-AFTER-1              TO PRT-RADSKIP                         
212500     MOVE RAD9A                    TO WS-RAD                              
212600     PERFORM S02-SKRIV                                                    
212700                                                                          
212800     MOVE PRT-AFTER-1              TO PRT-RADSKIP                         
212900     MOVE RAD11                    TO WS-RAD                              
213000     PERFORM S02-SKRIV                                                    
213100                                                                          
213200     MOVE PRT-EQUAL-61             TO PRT-RADSKIP                         
213300     MOVE LINK-IDDISTR             TO TEST-IDDISTR                        
213400     IF DIST37-TRAILER-DISTR                                              
213500       MOVE RAD12A                 TO WS-RAD                              
213600     ELSE                                                                 
213700       MOVE RAD12                  TO WS-RAD                              
213800     END-IF                                                               
213900     PERFORM S02-SKRIV                                                    
214000                                                                          
214100     MOVE PRT-EQUAL-63             TO PRT-RADSKIP                         
214200     MOVE RAD13                    TO WS-RAD                              
214300     PERFORM S02-SKRIV                                                    
214400     .                                                                    
214500     EJECT                                                                
214600 S01-OPPNA-PRINTER SECTION.                                               
214700                                                                          
214800     CALL W006PRR1 USING PRT-SPOOL-A4S                                    
214900                         PRT-OPEN                                         
215000                         WS-PRT-IDPRTLST                                  
215100                         ALT-PCB                                          
215200                         LISB-PCB                                         
215300                         WS-PRT-IDLIST                                    
215400                         WS-PRT-DUMMY                                     
215500                         WS-PRT-DUMMY                                     
215600                                                                          
215700     .                                                                    
215800     EJECT                                                                
215900 S02-SKRIV SECTION.                                                       
216000                                                                          
216100                                                                          
216200     CALL W006PRR1 USING PRT-SPOOL-A4S                                    
216300                         PRT-WRITE                                        
216400                         WS-PRT-IDPRTLST                                  
216500                         ALT-PCB                                          
216600                         LISB-PCB                                         
216700                         WS-PRT-IDLIST                                    
216800                         PRT-RADSKIP                                      
216900                         WS-PRT-LISTRAD                                   
217000     .                                                                    
217100     EJECT                                                                
217200 S02-FLYTTA-TILL-OMIXAD-TAB SECTION.                                      
217300                                                                          
217400     MOVE +1                 TO IX                                        
217500     MOVE NEJ                TO KDKOLLI-SW                                
217600                                                                          
217700     PERFORM UNTIL IX > 9                                                 
217800       IF KOLLI-IDPSN (IX)    > ZERO                                      
217900         MOVE KOLLI-IDPSN (IX) TO GODK-PSN-SW                             
218000                                                                          
218100         IF GODK-PSN                                                      
218200           MOVE JA      TO FORTSAETTNING-SW                               
218300           MOVE KOLLI-IDPSN (IX)   TO PSN-IX                              
218400                                                                          
218500           IF TAB1-IDPSN (PSN-IX) = ZERO                                  
218600             MOVE KOLLI-IDPSN (IX) TO TAB1-IDPSN (PSN-IX)                 
218700             MOVE JA TO TAB1-DATA-SW                                      
218800           END-IF                                                         
218900                                                                          
219000           MOVE +1 TO KDKOLLI-IX                                          
219100           PERFORM UNTIL KDKOLLI-IX > 4 OR KDKOLLI-OK                     
219200                                                                          
219300             IF TAB1-KDKOLLI (PSN-IX, KDKOLLI-IX) = SPACE                 
219400               MOVE KOLLI-KDKOLLI                                         
219500                        TO TAB1-KDKOLLI (PSN-IX, KDKOLLI-IX)              
219600               MOVE KOLLI-VKART-FG (IX)                                   
219700                        TO TAB1-VKART-FG (PSN-IX, KDKOLLI-IX)             
219800               MOVE KOLLI-VLFG (IX)                                       
219900                        TO TAB1-VLFG (PSN-IX, KDKOLLI-IX)                 
220000               MOVE KOLLI-VKORDBTO-KOLLI                                  
220100                        TO TAB1-VKORDBTO (PSN-IX, KDKOLLI-IX)             
220200                                                                          
220300               ADD +1   TO TAB1-ANTAL-KOLLIN (PSN-IX, KDKOLLI-IX)         
220400               MOVE JA TO KDKOLLI-SW                                      
220500             ELSE                                                         
220600               PERFORM S02A-KOLLA-KDKOLLI                                 
220700                                                                          
220800               IF WS-KDKOLLI-KOLLI = WS-KDKOLLI-TAB                       
220900                                                                          
221000                 ADD KOLLI-VKART-FG (IX)                                  
221100                        TO TAB1-VKART-FG(PSN-IX, KDKOLLI-IX)              
221200                 ADD KOLLI-VLFG (IX)                                      
221300                        TO TAB1-VLFG(PSN-IX, KDKOLLI-IX)                  
221400                 ADD KOLLI-VKORDBTO-KOLLI                                 
221500                        TO TAB1-VKORDBTO(PSN-IX, KDKOLLI-IX)              
221600                 ADD +1 TO TAB1-ANTAL-KOLLIN (PSN-IX, KDKOLLI-IX)         
221700                                                                          
221800                 MOVE JA TO KDKOLLI-SW                                    
221900               END-IF                                                     
222000             END-IF                                                       
222100                                                                          
222200             ADD +1   TO KDKOLLI-IX                                       
222300           END-PERFORM                                                    
222400         END-IF                                                           
222500       END-IF                                                             
222600       ADD +1               TO IX                                         
222700     END-PERFORM                                                          
222800     .                                                                    
222900     EJECT                                                                
223000 S02A-KOLLA-KDKOLLI SECTION.                                              
223100                                                                          
223200     MOVE KOLLI-KDKOLLI                     TO GODK-KDKOLLI-SW            
223300     IF FIBREBOARDBOX                                                     
223400       MOVE 'FIBRE  '                       TO WS-KDKOLLI-KOLLI           
223500     END-IF                                                               
223600     IF PLYWOODBOX                                                        
223700       MOVE 'PLYWOOD'                       TO WS-KDKOLLI-KOLLI           
223800     END-IF                                                               
223900     IF PLASTICDRUM                                                       
224000       MOVE 'PLASTIC'                       TO WS-KDKOLLI-KOLLI           
224100     END-IF                                                               
224200     IF STEELDRUM                                                         
224300       MOVE 'STEEL  '                       TO WS-KDKOLLI-KOLLI           
224400     END-IF                                                               
224500                                                                          
224600     MOVE TAB1-KDKOLLI (PSN-IX, KDKOLLI-IX) TO GODK-KDKOLLI-SW            
224700     IF FIBREBOARDBOX                                                     
224800       MOVE 'FIBRE  '                       TO WS-KDKOLLI-TAB             
224900     END-IF                                                               
225000     IF PLYWOODBOX                                                        
225100       MOVE 'PLYWOOD'                       TO WS-KDKOLLI-TAB             
225200     END-IF                                                               
225300     IF PLASTICDRUM                                                       
225400       MOVE 'PLASTIC'                       TO WS-KDKOLLI-TAB             
225500     END-IF                                                               
225600     IF STEELDRUM                                                         
225700       MOVE 'STEEL  '                       TO WS-KDKOLLI-TAB             
225800     END-IF                                                               
225900     .                                                                    
226000     EJECT                                                                
226100 S03-FLYTTA-TILL-MIXAD-TAB SECTION.                                       
226200                                                                          
226300     MOVE +1                 TO IX                                        
226400     MOVE +1                 TO TAB2-IX                                   
226500     MOVE +1                 TO TAB2-PSN-IX                               
226600     MOVE JA                 TO PSN-MATCH-SW                              
226700                                                                          
226800     PERFORM UNTIL TAB2-IX > 100                                          
226900       PERFORM S03A-KOLLA-OM-SAMMA-PSN                                    
227000                                                                          
227100       IF PSN-MATCH                                                       
227200                                                                          
227300         PERFORM S03B-FLYTTA-DATA1-TAB2                                   
227400         MOVE +100 TO TAB2-IX                                             
227500                                                                          
227600       ELSE                                                               
227700         IF TAB2-KDKOLLI (TAB2-IX)   = SPACE AND                          
227800            TAB2-ANTAL-PSN (TAB2-IX) = ZERO                               
227900                                                                          
228000           PERFORM S03C-FLYTTA-DATA2-TAB2                                 
228100           MOVE +100 TO TAB2-IX                                           
228200         END-IF                                                           
228300       END-IF                                                             
228400                                                                          
228500       ADD +1 TO TAB2-IX                                                  
228600     END-PERFORM                                                          
228700     .                                                                    
228800     EJECT                                                                
228900 S03A-KOLLA-OM-SAMMA-PSN SECTION.                                         
229000                                                                          
229100     PERFORM S03AA-KOLLA-KDKOLLI                                          
229200                                                                          
229300     IF WS-KDKOLLI-KOLLI = WS-KDKOLLI-TAB      AND                        
229400        WS-ANTAL-PSN     = TAB2-ANTAL-PSN (TAB2-IX)                       
229500                                                                          
229600       PERFORM UNTIL IX > 9                                               
229700         IF KOLLI-IDPSN (IX)  > ZERO                                      
229800           MOVE KOLLI-IDPSN (IX) TO GODK-PSN-SW                           
229900                                                                          
230000           IF GODK-PSN                                                    
230100             MOVE JA    TO FORTSAETTNING-SW                               
230200                                                                          
230300             IF TAB2-IDPSN (TAB2-IX, TAB2-PSN-IX) =                       
230400                KOLLI-IDPSN (IX)                                          
230500                                                                          
230600               ADD  +1 TO IX                                              
230700               MOVE +1 TO TAB2-PSN-IX                                     
230800             ELSE                                                         
230900               ADD +1 TO TAB2-PSN-IX                                      
231000               IF TAB2-PSN-IX > 9                                         
231100                 MOVE NEJ TO PSN-MATCH-SW                                 
231200                 ADD +1   TO IX                                           
231300                 MOVE +1 TO TAB2-PSN-IX                                   
231400               END-IF                                                     
231500                                                                          
231600             END-IF                                                       
231700           ELSE                                                           
231800             ADD +1   TO IX                                               
231900           END-IF                                                         
232000         ELSE                                                             
232100           ADD +1 TO IX                                                   
232200         END-IF                                                           
232300                                                                          
232400       END-PERFORM                                                        
232500                                                                          
232600     ELSE                                                                 
232700       MOVE NEJ TO PSN-MATCH-SW                                           
232800     END-IF                                                               
232900     .                                                                    
233000     EJECT                                                                
233100 S03AA-KOLLA-KDKOLLI SECTION.                                             
233200                                                                          
233300     MOVE SPACE                   TO WS-KDKOLLI-KOLLI                     
233400                                     WS-KDKOLLI-TAB                       
233500     MOVE KOLLI-KDKOLLI           TO GODK-KDKOLLI-SW                      
233600                                                                          
233700     IF FIBREBOARDBOX                                                     
233800       MOVE 'FIBRE  '             TO WS-KDKOLLI-KOLLI                     
233900     END-IF                                                               
234000     IF PLYWOODBOX                                                        
234100       MOVE 'PLYWOOD'             TO WS-KDKOLLI-KOLLI                     
234200     END-IF                                                               
234300     IF PLASTICDRUM                                                       
234400       MOVE 'PLASTIC'             TO WS-KDKOLLI-KOLLI                     
234500     END-IF                                                               
234600     IF STEELDRUM                                                         
234700       MOVE 'STEEL  '             TO WS-KDKOLLI-KOLLI                     
234800     END-IF                                                               
234900                                                                          
235000     MOVE TAB2-KDKOLLI (TAB2-IX)  TO GODK-KDKOLLI-SW                      
235100     IF FIBREBOARDBOX                                                     
235200       MOVE 'FIBRE  '             TO WS-KDKOLLI-TAB                       
235300     END-IF                                                               
235400     IF PLYWOODBOX                                                        
235500       MOVE 'PLYWOOD'             TO WS-KDKOLLI-TAB                       
235600     END-IF                                                               
235700     IF PLASTICDRUM                                                       
235800       MOVE 'PLASTIC'             TO WS-KDKOLLI-TAB                       
235900     END-IF                                                               
236000     IF STEELDRUM                                                         
236100       MOVE 'STEEL  '             TO WS-KDKOLLI-TAB                       
236200     END-IF                                                               
236300     .                                                                    
236400     EJECT                                                                
236500 S03B-FLYTTA-DATA1-TAB2 SECTION.                                          
236600                                                                          
236700     MOVE +1 TO IX                                                        
236800     MOVE +1 TO TAB2-PSN-IX                                               
236900                                                                          
237000     ADD +1               TO TAB2-ANTAL-KOLLIN (TAB2-IX)                  
237100     ADD KOLLI-VKORDBTO-KOLLI TO TAB2-VKORDBTO (TAB2-IX)                  
237200                                                                          
237300     PERFORM UNTIL IX > 9                                                 
237400       IF KOLLI-IDPSN (IX)    > ZERO                                      
237500         MOVE KOLLI-IDPSN (IX) TO GODK-PSN-SW                             
237600                                                                          
237700         IF GODK-PSN                                                      
237800           MOVE JA      TO FORTSAETTNING-SW                               
237900                                                                          
238000           ADD KOLLI-VLFG (IX)     TO TAB2-VLFG (TAB2-IX)                 
238100           ADD KOLLI-VKART-FG (IX) TO TAB2-VKART-FG (TAB2-IX)             
238200                                                                          
238300           ADD    +1 TO IX                                                
238400           ADD    +1 TO TAB2-PSN-IX                                       
238500         ELSE                                                             
238600           ADD    +1 TO IX                                                
238700         END-IF                                                           
238800       ELSE                                                               
238900         ADD +1 TO IX                                                     
239000       END-IF                                                             
239100                                                                          
239200     END-PERFORM                                                          
239300     .                                                                    
239400     EJECT                                                                
239500 S03C-FLYTTA-DATA2-TAB2 SECTION.                                          
239600                                                                          
239700     MOVE +1 TO IX                                                        
239800     MOVE +1 TO TAB2-PSN-IX                                               
239900                                                                          
240000     MOVE KOLLI-KDKOLLI       TO TAB2-KDKOLLI (TAB2-IX)                   
240100     ADD +1                   TO TAB2-ANTAL-KOLLIN (TAB2-IX)              
240200     ADD KOLLI-VKORDBTO-KOLLI TO TAB2-VKORDBTO (TAB2-IX)                  
240300     MOVE WS-ANTAL-PSN        TO TAB2-ANTAL-PSN (TAB2-IX)                 
240400                                                                          
240500     PERFORM UNTIL IX > 9                                                 
240600       IF KOLLI-IDPSN (IX)    > ZERO                                      
240700         MOVE KOLLI-IDPSN (IX) TO GODK-PSN-SW                             
240800                                                                          
240900         IF GODK-PSN                                                      
241000           MOVE JA      TO FORTSAETTNING-SW                               
241100                                                                          
241200           MOVE KOLLI-IDPSN (IX)   TO                                     
241300                TAB2-IDPSN (TAB2-IX, TAB2-PSN-IX)                         
241400           ADD KOLLI-VLFG (IX)     TO TAB2-VLFG (TAB2-IX)                 
241500           ADD KOLLI-VKART-FG (IX) TO TAB2-VKART-FG (TAB2-IX)             
241600                                                                          
241700                                                                          
241800           ADD    +1 TO IX                                                
241900           ADD    +1 TO TAB2-PSN-IX                                       
242000         ELSE                                                             
242100           ADD    +1 TO IX                                                
242200         END-IF                                                           
242300       ELSE                                                               
242400         ADD +1 TO IX                                                     
242500       END-IF                                                             
242600                                                                          
242700     END-PERFORM                                                          
242800     .                                                                    
242900     EJECT                                                                
243000 S04-FLYTTA-TILL-EJ-GODK-TAB SECTION.                                     
243100                                                                          
243200     MOVE +1 TO IX                                                        
243300     MOVE +1 TO TAB3-IX                                                   
243400     MOVE +1 TO TAB3-PSN-IX                                               
243500     MOVE NEJ TO PSN-OK-SW                                                
243600                                                                          
243700     PERFORM UNTIL TAB3-IX > +100                                         
243800       IF TAB3-KDKOLLI (TAB3-IX)      = SPACE AND                         
243900          TAB3-ANTAL-KOLLIN (TAB3-IX) = ZERO                              
244000                                                                          
244100         PERFORM UNTIL IX > 9                                             
244200           IF KOLLI-IDPSN (IX) > ZERO                                     
244300             MOVE KOLLI-IDPSN (IX) TO GODK-PSN-SW                         
244400                                                                          
244500             IF GODK-PSN                                                  
244600               MOVE JA  TO FORTSAETTNING-SW                               
244700               MOVE JA  TO PSN-OK-SW                                      
244800                                                                          
244900               MOVE KOLLI-IDPSN (IX)   TO                                 
245000                    TAB3-IDPSN (TAB3-IX, TAB3-PSN-IX)                     
245100               ADD KOLLI-VLFG (IX)     TO TAB3-VLFG (TAB3-IX)             
245200               ADD KOLLI-VKART-FG (IX) TO TAB3-VKART-FG (TAB3-IX)         
245300                                                                          
245400                                                                          
245500               ADD +1 TO IX                                               
245600               ADD +1 TO TAB3-PSN-IX                                      
245700             ELSE                                                         
245800               ADD +1 TO IX                                               
245900             END-IF                                                       
246000           ELSE                                                           
246100             ADD +1 TO IX                                                 
246200           END-IF                                                         
246300                                                                          
246400         END-PERFORM                                                      
246500                                                                          
246600         IF PSN-OK                                                        
246700           MOVE KOLLI-KDKOLLI      TO TAB3-KDKOLLI (TAB3-IX)              
246800           MOVE KOLLI-VKORDBTO-KOLLI                                      
246900                                   TO TAB3-VKORDBTO (TAB3-IX)             
247000           ADD +1                  TO TAB3-ANTAL-KOLLIN (TAB3-IX)         
247100         END-IF                                                           
247200                                                                          
247300         MOVE +100 TO TAB3-IX                                             
247400       END-IF                                                             
247500                                                                          
247600       ADD +1 TO TAB3-IX                                                  
247700     END-PERFORM                                                          
247800     .                                                                    
247900     EJECT                                                                
248000 S10-STAENG-PRINTER SECTION.                                              
248100                                                                          
248200     CALL W006PRR1 USING PRT-SPOOL-A4S                                    
248300                         PRT-CLOSE                                        
248400                         WS-PRT-IDPRTLST                                  
248500                         ALT-PCB                                          
248600                         LISB-PCB                                         
248700                         WS-PRT-IDLIST                                    
248800                         WS-PRT-DUMMY                                     
248900                         WS-PRT-DUMMY                                     
249000                                                                          
249100     .                                                                    
249200 S90-SEND-OPEN SECTION.                                                   
249300                                                                          
249400     MOVE WS-ADRESS-DP                    TO SEND-ADDISPABS               
249500     MOVE 'OPEN'                          TO SEND-KDFUNC                  
249600     CALL WZ01SEND USING SEND-CONTROL-AREA                                
249700                         SEND-OPEN-AREA                                   
249800     IF SEND-KDRC > ZERO                                                  
249900       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
250000       STRING 'WZ01SEND OPEN ERROR RC=' KDRC-DISPLAY                      
250100       DELIMITED BY SIZE INTO FELTEXT                                     
250200       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
250300     END-IF                                                               
250400     .                                                                    
250500                                                                          
250600 S91-PUT-DOC-HDR SECTION.                                                 
250700                                                                          
250800     MOVE 'PUT'                           TO SEND-KDFUNC                  
250900     MOVE WZ04-001-IDCOM                  TO SEND-IDCOM                   
251000     MOVE LENGTH OF HDR-AREA              TO SEND-KVDLEN                  
251100     CALL WZ01SEND USING SEND-CONTROL-AREA                                
251200                         SEND-KVDLEN                                      
251300                         HDR-AREA                                         
251400     IF SEND-KDRC > ZERO                                                  
251500       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
251600       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
251700       DELIMITED BY SIZE INTO FELTEXT                                     
251800       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
251900     END-IF                                                               
252000     .                                                                    
252100     SKIP3                                                                
252200 S92-PUT-DOC-HEAD SECTION.                                                
252300                                                                          
252400     MOVE 'PUT'                           TO SEND-KDFUNC                  
252500     MOVE WZ04-001-IDCOM                  TO SEND-IDCOM                   
252600     MOVE LENGTH OF DOC-HEAD-AREA         TO SEND-KVDLEN                  
252700     CALL WZ01SEND USING SEND-CONTROL-AREA                                
252800                         SEND-KVDLEN                                      
252900                         DOC-HEAD-AREA                                    
253000     IF SEND-KDRC > ZERO                                                  
253100       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
253200       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
253300       DELIMITED BY SIZE INTO FELTEXT                                     
253400       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
253500     END-IF                                                               
253600     .                                                                    
253700     SKIP3                                                                
253800 S93-PUT-DOC-LINE SECTION.                                                
253900                                                                          
254000     MOVE 'PUT'                           TO SEND-KDFUNC                  
254100     MOVE WZ04-001-IDCOM                  TO SEND-IDCOM                   
254200     MOVE LENGTH OF DOC-LINE-AREA         TO SEND-KVDLEN                  
254300     CALL WZ01SEND USING SEND-CONTROL-AREA                                
254400                         SEND-KVDLEN                                      
254500                         DOC-LINE-AREA                                    
254600     IF SEND-KDRC > ZERO                                                  
254700       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
254800       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
254900       DELIMITED BY SIZE INTO FELTEXT                                     
255000       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
255100     END-IF                                                               
255200     .                                                                    
255300     EJECT                                                                
255400 S94-PUT-DOC-FOOT SECTION.                                                
255500                                                                          
255600     MOVE 'PUT'                           TO SEND-KDFUNC                  
255700     MOVE WZ04-001-IDCOM                  TO SEND-IDCOM                   
255800     MOVE LENGTH OF DOC-LINE-AREA         TO SEND-KVDLEN                  
255900     CALL WZ01SEND USING SEND-CONTROL-AREA                                
256000                         SEND-KVDLEN                                      
256100                         DOC-FOOT-AREA                                    
256200     IF SEND-KDRC > ZERO                                                  
256300       MOVE SEND-KDRC                     TO KDRC-DISPLAY                 
256400       STRING 'WZ01SEND PUT ERROR RC=' KDRC-DISPLAY                       
256500       DELIMITED BY SIZE INTO FELTEXT                                     
256600       CALL ABEND USING RKOD-ABEND-MED-DUMP                               
256700     END-IF                                                               
256800     .                                                                    
256900     EJECT                                                                
257000 S95-SEND-CLOSE SECTION.                                                  
257100                                                                          
257200     MOVE 'CLOSE'                         TO SEND-KDFUNC                  
257300     CALL WZ01SEND USING SEND-CONTROL-AREA                                
257400     .                                                                    
257500                                                                          
257600                                                                          
257700     EJECT                                                                
257800* --- IMS SEKTIONER ---                                                   
257900     SKIP3                                                                
258000 IMS-GET-1165-WDR2 SECTION.                                               
258100                                                                          
258200     MOVE 'GET-1165-WDR2   ' TO CURRENT-IMS-SECTION                       
258300                                                                          
258400     STRING 'WL116501(WDGXKEY  =' W-WDGXKEY-X ')'                         
258500          DELIMITED BY SIZE INTO SSA1                                     
258600     STRING 'WL116512(KDFGTRP  =' W-KDFGTRP-X ')'                         
258700          DELIMITED BY SIZE INTO SSA2                                     
258800     MOVE '    ' TO GODK-STATUSKODER                                      
258900     CALL CBLTDLI USING GU 1165-PCB DLI-IO-AREA SSA1 SSA2                 
259000     MOVE 1165-STATUS-CODE TO STATUS-WS                                   
259100     PERFORM IMS-STATUSKONTROLL                                           
259200     .                                                                    
259300     EJECT                                                                
259400 IMS-GET-GMTA01 SECTION.                                                  
259500                                                                          
259600     MOVE 'GET-GMTA01      ' TO CURRENT-IMS-SECTION                       
259700                                                                          
259800     STRING 'WLGMTA01(IDGMT   >=' W-IDGMT-X ')'                           
259900          DELIMITED BY SIZE INTO SSA1                                     
260000     MOVE '  '     TO GODK-STATUSKODER                                    
260100     CALL CBLTDLI USING GU GMTA-PCB DLI-IO-AREA-WDB2 SSA1                 
260200     MOVE GMTA-STATUS-CODE TO STATUS-WS                                   
260300     PERFORM IMS-STATUSKONTROLL                                           
260400     .                                                                    
260500     SKIP3                                                                
260600 IMS-GET-WDB101 SECTION.                                                  
260700                                                                          
260800     MOVE 'GET-WDB101      ' TO CURRENT-IMS-SECTION                       
260900                                                                          
261000     STRING 'WLBETC01(WDB101KY =' W-WDB1-WDB101KY-X ')'                   
261100          DELIMITED BY SIZE INTO SSA1                                     
261200     MOVE '  '     TO GODK-STATUSKODER                                    
261300     CALL CBLTDLI USING GU WDB1-PCB DLI-IO-AREA SSA1                      
261400     MOVE WDB1-STATUS-CODE TO STATUS-WS                                   
261500     PERFORM IMS-STATUSKONTROLL                                           
261600     .                                                                    
261700     EJECT                                                                
261800 IMS-GET-WDE6 SECTION.                                                    
261900                                                                          
262000     MOVE 'GET-WDE6        ' TO CURRENT-IMS-SECTION                       
262100                                                                          
262200     STRING 'WDE601  (IDPRODNR =' W-IDPRODNR-X ')'                        
262300            DELIMITED BY SIZE INTO SSA1                                   
262400     STRING 'WDE611  (IDKOLLI  =' W-IDKOLLI-X ')'                         
262500            DELIMITED BY SIZE INTO SSA2                                   
262600     MOVE '    ' TO GODK-STATUSKODER                                      
262700     CALL CBLTDLI USING GU    WDE6-PCB DLI-IO-AREA SSA1 SSA2              
262800     MOVE WDE6-STATUS-CODE TO STATUS-WS                                   
262900     PERFORM IMS-STATUSKONTROLL                                           
263000     .                                                                    
263100     EJECT                                                                
263200 IMS-GU-WL451311  SECTION.                                                
263300                                                                          
263400     MOVE 'GU-WL451311     ' TO CURRENT-IMS-SECTION                       
263500                                                                          
263600     STRING 'WL451301(WDGXKEY  =' W-4513-X ')'                            
263700          DELIMITED BY SIZE INTO SSA1                                     
263800     STRING 'WL451311(DASKEPPN =' W-DASKEPPN-X ')'                        
263900          DELIMITED BY SIZE INTO SSA2                                     
264000     MOVE '  GE' TO GODK-STATUSKODER                                      
264100     CALL CBLTDLI USING GU 4513-PCB DLI-IO-AREA-4514 SSA1 SSA2            
264200     MOVE 4513-STATUS-CODE TO STATUS-WS                                   
264300     PERFORM IMS-STATUSKONTROLL                                           
264400     .                                                                    
264500     SKIP2                                                                
264600 IMS-GNP-WL451321 SECTION.                                                
264700                                                                          
264800     MOVE 'GNP-WL451321    ' TO CURRENT-IMS-SECTION                       
264900                                                                          
265000     STRING 'WL451321(WDGXKEY >=' W-4516-MIN-X                            
265100            '&WDGXKEY <=' W-4516-MAX-X ')'                                
265200          DELIMITED BY SIZE INTO SSA1                                     
265300     MOVE '  GE' TO GODK-STATUSKODER                                      
265400     CALL CBLTDLI USING GNP 4513-PCB DLI-IO-AREA-4516 SSA1                
265500     MOVE 4513-STATUS-CODE TO STATUS-WS                                   
265600     PERFORM IMS-STATUSKONTROLL                                           
265700     .                                                                    
265800     SKIP3                                                                
265900 IMS-GU-WDB601    SECTION.                                                
266000     STRING 'WDB601  (IDDC     =' W-IDDC-B6-X ')'                         
266100          DELIMITED BY SIZE INTO SSA1                                     
266200     MOVE '  GE' TO GODK-STATUSKODER                                      
266300     CALL CBLTDLI USING GU WDB6-PCB DLI-IO-AREA-B601 SSA1                 
266400     MOVE WDB6-STATUS-CODE    TO STATUS-WS                                
266500     PERFORM IMS-STATUSKONTROLL                                           
266600     .                                                                    
266700     SKIP3                                                                
266800 IMS-STATUSKONTROLL SECTION.                                              
266900                                                                          
267000     SET STATUS-IX TO 1                                                   
267100     SEARCH GODK-STATUS                                                   
267200       AT END                                                             
267300       STRING 'STATUSKOD FRÅN IMS ' STATUS-WS                             
267400       DELIMITED BY SIZE INTO FELTEXT                                     
267500       CALL FELLOG                                                        
267600       WHEN GODK-STATUS (STATUS-IX) = STATUS-WS CONTINUE                  
267700     END-SEARCH                                                           
268000     .                                                                    
